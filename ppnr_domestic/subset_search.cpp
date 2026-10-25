// Reused from the verified Domestic search; ranks the three best subsets per task.
// [[Rcpp::plugins(cpp11)]]
#include <Rcpp.h>
#include <algorithm>
#include <atomic>
#include <chrono>
#include <cmath>
#include <memory>
#include <queue>
#include <thread>
#include <vector>
#include <sys/resource.h>
#ifdef __APPLE__
#include <mach/mach.h>
#endif
using namespace Rcpp;

static double ppnr_rss() {
#ifdef __APPLE__
  mach_task_basic_info_data_t info;
  mach_msg_type_number_t count = MACH_TASK_BASIC_INFO_COUNT;
  if (task_info(mach_task_self(), MACH_TASK_BASIC_INFO,
                (task_info_t)&info, &count) == KERN_SUCCESS)
    return (double)info.resident_size;
#endif
  return 0;
}
static double ppnr_peak() {
  struct rusage usage; getrusage(RUSAGE_SELF, &usage);
#ifdef __APPLE__
  return (double)usage.ru_maxrss;
#else
  return (double)usage.ru_maxrss * 1024;
#endif
}
static double ppnr_choose(int n, int k) {
  if (k < 0 || k > n) return 0;
  double value = 1;
  for (int j = 1; j <= k; ++j) value *= double(n-j+1)/j;
  return std::round(value);
}
struct PPNRFold {
  std::vector<double> gram, cy, future;
  double base, actual;
  bool selection;
};
struct PPNRCandidate {
  double mse;
  std::vector<int> variables;
  std::vector<double> predictions;
  bool operator<(const PPNRCandidate& other) const {
    if (mse != other.mse) return mse < other.mse;
    return variables < other.variables;
  }
};
struct PPNRTask {
  int p, k, nf, top_n, selection_n = 0, check_n = 0;
  double checked = 0, check_sum = 0, check_min = INFINITY, check_max = 0;
  double total, covered = 0, excluded = 0, nodes = 0, max_nodes;
  std::vector<PPNRFold> folds;
  std::vector<int> chosen;
  std::vector<double> L, z, w, predictions;
  std::priority_queue<PPNRCandidate> best;
  std::atomic<double> progress{0};
  std::atomic<bool>* cancel;
  std::string error;

  PPNRTask(List input, int size, int keep, double limit, std::atomic<bool>* stop_flag)
    : k(size), top_n(keep), max_nodes(limit), cancel(stop_flag) {
    nf = input.size(); p = 0;
    for (int f = 0; f < nf; ++f) {
      List entry = input[f]; NumericMatrix G = entry["gram"];
      NumericVector c = entry["cy"], x = entry["future"];
      if (!f) p = G.nrow();
      if (G.nrow() != p || G.ncol() != p || c.size() != p || x.size() != p)
        stop("Inconsistent search matrices");
      PPNRFold fold;
      fold.gram.assign(G.begin(), G.end()); fold.cy.assign(c.begin(), c.end());
      fold.future.assign(x.begin(), x.end());
      fold.base = as<double>(entry["base_prediction"]);
      fold.actual = as<double>(entry["actual"]);
      fold.selection = !entry.containsElementNamed("selection") || as<bool>(entry["selection"]);
      if (fold.selection) ++selection_n; else ++check_n;
      folds.push_back(fold);
    }
    if (k < 1 || k > 8 || k > p || selection_n < 1) stop("Invalid search dimensions");
    total = ppnr_choose(p, k);
    L.assign(nf*k*k, 0); z.assign(nf*k, 0); w.assign(nf*k, 0);
    predictions.assign(nf*(k+1), 0);
    for (int f = 0; f < nf; ++f) predictions[f*(k+1)] = folds[f].base;
  }
  void record(double mse, double check_mse) {
    if (!std::isfinite(mse)) return;
    if (check_n) {
      double rmse = std::sqrt(check_mse / check_n);
      ++checked; check_sum += rmse;
      check_min = std::min(check_min, rmse); check_max = std::max(check_max, rmse);
    }
    if (best.size() == (size_t)top_n && mse >= best.top().mse) return;
    PPNRCandidate item; item.mse = mse; item.variables = chosen;
    for (int f = 0; f < nf; ++f) item.predictions.push_back(predictions[f*(k+1)+k]);
    best.push(item); if (best.size() > (size_t)top_n) best.pop();
  }
  void search(int next, int depth) {
    for (int j = next; j <= p-(k-depth); ++j) {
      if (((unsigned long long)nodes & 4095) == 0) {
        progress.store(covered, std::memory_order_relaxed);
        if (cancel->load() || nodes >= max_nodes) return;
      }
      ++nodes; chosen.push_back(j); bool valid = true; double mse = 0, check_mse = 0;
      for (int f = 0; f < nf; ++f) {
        const PPNRFold& fold = folds[f];
        int li = f*k*k, vi = f*k, pi = f*(k+1);
        double diagonal = fold.gram[j+p*j];
        for (int r = 0; r < depth; ++r) {
          double value = fold.gram[j+p*chosen[r]];
          for (int s = 0; s < r; ++s) value -= L[li+depth*k+s]*L[li+r*k+s];
          value /= L[li+r*k+r]; L[li+depth*k+r] = value;
          diagonal -= value*value;
        }
        if (!std::isfinite(diagonal) || diagonal < 1e-10) { valid = false; break; }
        double yy = fold.cy[j], xx = fold.future[j];
        for (int r = 0; r < depth; ++r) {
          yy -= L[li+depth*k+r]*z[vi+r]; xx -= L[li+depth*k+r]*w[vi+r];
        }
        double root = std::sqrt(diagonal); L[li+depth*k+depth] = root;
        z[vi+depth] = yy/root; w[vi+depth] = xx/root;
        double prediction = predictions[pi+depth] + z[vi+depth]*w[vi+depth];
        predictions[pi+depth+1] = prediction;
        double residual = prediction-fold.actual;
        if (fold.selection) mse += residual*residual; else check_mse += residual*residual;
      }
      if (!valid) {
        double count = ppnr_choose(p-j-1, k-depth-1);
        covered += count; excluded += count;
      } else if (depth+1 == k) {
        ++covered; record(mse/selection_n, check_mse);
      } else search(j+1, depth+1);
      chosen.pop_back();
      if (cancel->load() || nodes >= max_nodes) return;
    }
  }
  void run() { search(0, 0); progress.store(covered); }
};
// All R API calls and progress output stay on the main thread.
// [[Rcpp::export]]
List ppnr_search_round(List inputs, int size, int threads = 6,
                       double seconds = 7200, double memory_bytes = 4294967296.0,
                       double max_nodes = 1e100, bool verbose = true) {
  std::atomic<bool> cancel{false}; std::atomic<int> next{0}, finished{0};
  int ntasks = inputs.size();
  if (!ntasks || threads < 1 || seconds <= 0) stop("Invalid execution limits");
  std::vector<std::unique_ptr<PPNRTask>> tasks;
  for (int i = 0; i < ntasks; ++i)
    tasks.emplace_back(new PPNRTask(as<List>(inputs[i]), size, 3, max_nodes, &cancel));
  std::vector<std::thread> workers;
  auto start = std::chrono::steady_clock::now();
  auto elapsed = [&]() { return std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count(); };
  double peak = ppnr_rss(), last_report = 0;
  std::string status = "complete";
  if (peak > memory_bytes) { status = "memory_limit"; cancel.store(true); }
  try {
    for (int t = 0; t < std::min(threads, ntasks); ++t) workers.emplace_back([&]() {
      while (!cancel.load()) {
        int id = next.fetch_add(1); if (id >= ntasks) break;
        try { tasks[id]->run(); }
        catch (const std::exception& e) { tasks[id]->error = e.what(); cancel.store(true); }
        catch (...) { tasks[id]->error = "Unknown native search error"; cancel.store(true); }
        finished.fetch_add(1);
      }
    });
    while (finished.load() < ntasks && !cancel.load()) {
      std::this_thread::sleep_for(std::chrono::milliseconds(100));
      Rcpp::checkUserInterrupt();
      double rss = ppnr_rss(); peak = std::max(peak, rss);
      if (rss > memory_bytes) { status = "memory_limit"; cancel.store(true); }
      if (elapsed() > seconds) { status = "time_limit"; cancel.store(true); }
      if (verbose && elapsed()-last_report >= 10) {
        last_report = elapsed(); double covered = 0, total = 0;
        for (auto& task : tasks) { covered += task->progress.load(); total += task->total; }
        Rcpp::Rcout << "  Factors " << size << " | " << std::round(1000*covered/total)/10
          << "% | " << std::round(elapsed()) << " s | R " << std::round(rss/1048576)
          << " MiB" << std::endl;
        R_FlushConsole();
      }
    }
  } catch (...) {
    cancel.store(true); for (auto& worker : workers) if (worker.joinable()) worker.join();
    throw;
  }
  for (auto& worker : workers) if (worker.joinable()) worker.join();
  List results(ntasks);
  for (int i = 0; i < ntasks; ++i) {
    auto& task = *tasks[i];
    if (!task.error.empty()) stop(task.error);
    std::vector<PPNRCandidate> ranked;
    while (!task.best.empty()) { ranked.push_back(task.best.top()); task.best.pop(); }
    std::sort(ranked.begin(), ranked.end()); List best(ranked.size());
    for (size_t j = 0; j < ranked.size(); ++j) {
      IntegerVector variables(ranked[j].variables.size());
      for (size_t q = 0; q < ranked[j].variables.size(); ++q) variables[q] = ranked[j].variables[q]+1;
      best[j] = List::create(_["MSE"] = ranked[j].mse, _["variables"] = variables,
                            _["predictions"] = wrap(ranked[j].predictions));
    }
    if (task.covered < task.total && status == "complete") status = "pilot_limit";
    results[i] = List::create(_["covered"] = task.covered, _["excluded"] = task.excluded,
                              _["total"] = task.total, _["top"] = best,
      _["check_models"] = task.checked,
      _["mean_check_RMSE"] = task.checked ? task.check_sum/task.checked : NA_REAL,
      _["min_check_RMSE"] = task.checked ? task.check_min : NA_REAL,
      _["max_check_RMSE"] = task.checked ? task.check_max : NA_REAL);
  }
  return List::create(_["status"] = status, _["seconds"] = elapsed(),
    _["peak_bytes"] = std::max(peak, ppnr_peak()), _["tasks"] = results);
}

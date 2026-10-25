# RStudio Git workflow: Pull -> edit -> Save -> Stage -> Commit -> Push.
# Open Morgan_Stanley_Macroeconomics.Rproj first; update with git pull before working.
# Load repository data and run the Domestic-only diagnostics and trial model.
# The mixed and ARX searches remain opt-in.
source("ppnr_domestic/ppnr_domestic.R", local = TRUE)

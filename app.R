#setwd('W:/NZ-SYSTEMS-TOOL/NZST Phase 3/NZST Phase 3')


admin_mode = FALSE #change to TRUE to run in admin mode
load_from_file = FALSE #change to TRUE to load data from file if not able to connect to SQL database (non-admin only)

devtools::load_all()
run_app(admin_mode=admin_mode,load_from_file=load_from_file)
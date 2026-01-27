#run example 1

source("run_c_and_d_model_fcn.R")
source("error_checks_fcn.R")
wd_subfolder <- "example_1"
run_c_and_d_model(model_subfolder = wd_subfolder)

##run files in the main working directory (not in a subfolder)
#wd_subfolder <- ""
#run_c_and_d_model(model_subfolder = wd_subfolder)

##run code chunks without running the whole function
model_subfolder <- ""
scenario_subfolder<-""
wd = getwd()
model_subfolder <- "example_1"


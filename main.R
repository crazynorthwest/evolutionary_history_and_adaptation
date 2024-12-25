####################################################
##  
## 
## 05/01/2024
####################################################

#Please set repeat times to run the main simulation.
repeat_times_main <- 500
#Please set repeat times to run the retest simulation.
repeat_times_retest <- 500
#Please set the following path to the path where this R script is located.
path_code <- ".main.R"
#Please set the following path to Slim.
path_Slim <- "C:/msys64/mingw64/bin/slim"
#Maximum distance to final optimal point in retest simulation.
parameter_retest_max_distance <- 1
parameter_retest_min_distance <- 0.5
#First two terms for final optimal point before test.
optimal_final_before_test <- c(1, 0)
#Trait number
trait_number <- 2
#Optimal point for the test phase in main simulation.
vector_optimal_main <- c(0.5,0.5,rep(0,trait_number-2)) 
#Number of generations before test phase.
generation_before_test <- 1400
#Number of generations in test phase.
generation_test <- 100


#Build a vector of phenotype names.
phenotype_name <- c(paste0(rep("phenotype", trait_number),(1:trait_number)))
#Setting the main path.
path <- substr(path_code, 1, nchar(path_code)-7)

#Find the number of times it has been reused to avoid new runs overwriting previous data.
file_used_number <- 1
idx_tem <- 1
while (idx_tem > 0){
  fileName <- paste0(path, '/reused_', file_used_number)
  if (file.exists(fileName)){
    file_used_number <- file_used_number + 1
  }
  else{
    dir.create(fileName);
    idx_tem <- 0;
  }
}

#Setting other pathes.
path_main <- paste0(path, '/reused_', file_used_number, '/main')
path_retest <- paste0(path, '/reused_', file_used_number, '/retest')
path_figures <- paste0(path, '/reused_', file_used_number, '/figures')
path_main_test_simulation <- paste0(path, '/reused_', file_used_number, '/main/test_simulation')
path_main_whole_simulation <- paste0(path, '/reused_', file_used_number, '/main/whole_simulation')
path_main_analysis <- paste0(path, '/reused_', file_used_number, '/main/analysis')
path_main_mutation <- paste0(path, '/reused_', file_used_number, '/main/mutation')
path_retest_test_simulation <- paste0(path, '/reused_', file_used_number, '/retest/test_simulation')
path_retest_whole_simulation <- paste0(path, '/reused_', file_used_number, '/retest/whole_simulation')
path_retest_analysis <- paste0(path, '/reused_', file_used_number, '/retest/analysis')
path_retest_mutation <- paste0(path, '/reused_', file_used_number, '/retest/mutation')
if(!file.exists(path_figures)){dir.create(path_figures)}
if(!file.exists(path_retest)){dir.create(path_retest)}
if(!file.exists(path_main)){dir.create(path_main)}
if(!file.exists(path_main_test_simulation)){dir.create(path_main_test_simulation)}
if(!file.exists(path_main_whole_simulation)){dir.create(path_main_whole_simulation)}
if(!file.exists(path_main_analysis)){dir.create(path_main_analysis)}
if(!file.exists(path_main_mutation)){dir.create(path_main_mutation)}
if(!file.exists(path_retest_test_simulation)){dir.create(path_retest_test_simulation)}
if(!file.exists(path_retest_whole_simulation)){dir.create(path_retest_whole_simulation)}
if(!file.exists(path_retest_analysis)){dir.create(path_retest_analysis)}
if(!file.exists(path_retest_mutation)){dir.create(path_retest_mutation)}
path_simulation <- paste0(path, '/simulation.slim')
path_simulation_tem <- paste0(path, '/simulation_tem.slim')
path_runSlim <- paste0(path, '/runSlim.R')
path_organizeData <- paste0(path, '/organizeData.R')
path_analysisData <- paste0(path, '/analysisData.R')
#Run simulations.
source(path_runSlim)
#Re-organize data
source(path_organizeData)
#Analysis data
source(path_analysisData)

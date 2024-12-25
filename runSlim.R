#Function to run Slim with specified code
RunSlim <- function(script)
{
  system2(path_Slim, args = shQuote(script), stdout=T, stderr=T)
}

#Build new Slim code document and run main simulation.
lines_0 <- readLines(path_simulation)
for (idx_tem in 1:repeat_times_main)
{
  if (file.exists(path_simulation_tem)){file.remove(path_simulation_tem)}
  lines_1 <- 'function (string)addPath(void){'
  lines_2 <- paste0('if(!exists("n")){defineConstant("n",', trait_number, ');}')
  lines_3 <- paste0('path_data = "', path_main, '";')
  lines_4 <- paste0('if(!exists("filenamenumber")){defineConstant("filenamenumber",',idx_tem,');}')
  lines_5 <- 'if(!exists("testOptimal")){defineConstant("testOptimal",matrix(c(0.5, 0.5, rep(0,n-2)),nrow=1));}'
  lines_6 <- 'return path_data;'
  lines_7 <- '}'
  lines_final <- c(lines_1, lines_2, lines_3, lines_4, lines_5, lines_6, lines_7, lines_0)
  writeLines(lines_final, con = path_simulation_tem, sep = "\n", useBytes = F)
  RunSlim(path_simulation_tem)
}

#Set and save the first two terms in optimal point for retest simulation.
par_length <- runif(repeat_times_retest,min = parameter_retest_min_distance ,max=parameter_retest_max_distance)
par_angle <- runif(repeat_times_retest)*2*pi
par_1 <- par_length*cos(par_angle) + optimal_final_before_test[1]
par_2 <- par_length*sin(par_angle) + optimal_final_before_test[2]
fileName <- paste0(path_retest_analysis,"/retest_parameter.dat")
par_optimal <- data.frame(par1 = par_1, par2 = par_2)
write.table(par_optimal, file = fileName, row.names = FALSE, col.names = TRUE)
#Build new Slim code document and run retest simulation.
for (idx_tem in 1:repeat_times_retest)
{
  if (file.exists(path_simulation_tem)){file.remove(path_simulation_tem)}
  lines_2 <- rbind('function (string)addPath(void){', 
                   paste0('if(!exists("n")){defineConstant("n",', trait_number, ');}'),
                   'testOptimums = matrix(rep(0.0,n),nrow=1);')
  lines_3 <- paste0('testOptimums[0]=',par_1[idx_tem],';')
  lines_4 <- paste0('testOptimums[1]=',par_2[idx_tem],';')
  lines_5 <- 'if(!exists("testOptimal")){defineConstant("testOptimal", testOptimums);}'
  lines_6 <- paste0('if(!exists("filenamenumber")){defineConstant("filenamenumber",',idx_tem,');}')
  lines_7 <- paste0('path_data = "', path_retest, '";')
  lines_8 <- 'return path_data;'
  lines_9 <- '}'
  lines_final <- c(lines_2, lines_3, lines_4, lines_5, lines_6, lines_7, lines_8, lines_9, lines_0)
  writeLines(lines_final, con = path_simulation_tem, sep = "\n", useBytes = F)
  RunSlim(path_simulation_tem)
}


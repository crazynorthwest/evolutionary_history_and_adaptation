#Input data including each individuals' phenotype in a population and optimal point.
#Then, the function will calculate the angle between the vector from center of population to
#optimal point and the vector of gmax.
pca_cal <- function(subdata, vector_optimal_pca){
  pca_cal_result <- list()
  pca_cal_result_names <- NULL
  subdata_pca <- subdata[phenotype_name]
  pca_result <- prcomp(subdata_pca)
  PC1_vector <- pca_result$rotation[,1]
  names(PC1_vector) <- NULL
  names(pca_result$center) <- NULL
  
  #Calculate the angle between the direction of PC1 and the direction of vector between the center of population and final optimum
  vector_optimal_pca_angle1 <- vector_optimal_pca - pca_result$center
  angle <- acos(sum(vector_optimal_pca_angle1*PC1_vector)/sqrt(sum(vector_optimal_pca_angle1^2)*sum(PC1_vector^2)))
  if (angle > pi/2) {angle <- pi - angle}
  pca_cal_result <- append(pca_cal_result, angle)
  pca_cal_result_names <- c(pca_cal_result_names, "angle")
  
  #Calculate the distance between optimal point and center of population.  
  pca_center <- pca_result$center
  names(pca_center) <- NULL
  pca_distance <- sqrt(sum((pca_center-vector_optimal_pca)^2))
  pca_cal_result <- append(pca_cal_result, pca_distance)
  pca_cal_result_names <- c(pca_cal_result_names, "phenotype_distance")
  
  #Calculate the length of each principal component of PCA
  pca_summary <- summary(pca_result)
  pca_length <- pca_summary$importance[2,]
  pca_length <- pca_length^2
  pca_cal_result <- append(pca_cal_result, pca_length)
  pca_cal_result_names <- c(pca_cal_result_names, c(paste0(rep("PC", trait_number),(1:trait_number),rep("_length", trait_number))))
  names(pca_cal_result) <- pca_cal_result_names
  return(pca_cal_result)
}


population_names <- c("Subpopulation<p1>", "Subpopulation<p2>")

#Reorganize data in test phase in main simulation.
repeat_record <- 1
data_record <- NULL
generation_name <- (generation_before_test + 1):(generation_before_test + generation_test + 1)
while(repeat_record > 0){
  fileName <- paste0(path_main_test_simulation,"/",repeat_record,".dat")
  if (!file.exists(fileName)){
    break
  }
  data_main_test <- read.csv(fileName, header = TRUE, sep = " ")
  for (idx_population in population_names) {
    data_main_test_population <- subset(data_main_test, population == idx_population)
    for (idx_generation in generation_name) {
      data_main_test_generation <- subset(data_main_test_population, generation == idx_generation)
      data_main_test_generation_1 <- subset(data_main_test_population, generation == idx_generation + 1)
      fitness_increase <- mean(data_main_test_generation_1$fitness) - mean(data_main_test_generation$fitness)
      fitness <- mean(data_main_test_generation$fitness)
      pca_result_record <- pca_cal(data_main_test_generation, vector_optimal_main)
      data_record <- rbind(data_record, c(repeat_record, substr(idx_population, 15, 16), idx_generation, fitness_increase, fitness, pca_result_record))
    }
  }
  repeat_record <- repeat_record+1
}
colnames(data_record) <- c("repeat_number", "population", "generation", "fitness_increase", "fitness", "angle", "phenotype_distance", c(paste0(rep("PC", trait_number),(1:trait_number),rep("_length", trait_number))))
fileName <- paste0(path_main_analysis,"/main_test.dat")
write.table(data_record, file = fileName, row.names = FALSE, col.names = TRUE)

#Reorganize data in test phase in retest simulation.
fileName <- paste0(path_retest_analysis,"/retest_parameter.dat")
data_retest_par <- read.csv(fileName, header = TRUE, sep = " ") 
repeat_record <- 1
data_record <- NULL
while(repeat_record > 0){
  fileName <- paste0(path_retest_test_simulation,"/",repeat_record,".dat")
  if (!file.exists(fileName)){
    break
  }
  data_retest_test <- read.csv(fileName, header = TRUE, sep = " ")
  optimal_retest <- c(data_retest_par[repeat_record,1], data_retest_par[repeat_record,2], rep(0,trait_number-2))
  if(data_retest_par[repeat_record,2]>0){
    close_to <- 1
  }else{
    close_to <- 2
  }
  for (idx_population in population_names) {
    data_retest_test_population <- subset(data_retest_test, population == idx_population)
    for (idx_generation in generation_name) {
      data_retest_test_generation <- subset(data_retest_test_population, generation == idx_generation)
      data_retest_test_generation_1 <- subset(data_retest_test_population, generation == idx_generation + 1)
      fitness_increase <- mean(data_retest_test_generation_1$fitness) - mean(data_retest_test_generation$fitness)
      fitness <- mean(data_retest_test_generation$fitness)
      pca_result_record <- pca_cal(data_retest_test_generation, optimal_retest)
      data_record <- rbind(data_record, c(repeat_record, substr(idx_population, 15, 16), idx_generation, fitness_increase, fitness, pca_result_record,close_to))
    }
  }
  repeat_record <- repeat_record+1
}
colnames(data_record) <- c("repeat_number", "population", "generation", "fitness_increase", "fitness", "angle", "phenotype_distance", c(paste0(rep("PC", trait_number),(1:trait_number),rep("_length", trait_number))),"close_to")
fileName <- paste0(path_retest_analysis,"/retest_test.dat")
write.table(data_record, file = fileName, row.names = FALSE, col.names = TRUE)


#Reorganize data about the phenotype distance in main simulation.
repeat_record <- 1;
data_record <- NULL
while(repeat_record > 0){
  fileName <- paste0(path_main_whole_simulation,"/",repeat_record,".dat")
  if (!file.exists(fileName)){
    break
  }
  data_phenotype_average <- read.csv(fileName, header = TRUE, sep = " ")
  data_p1_phenotype_average <- subset(data_phenotype_average, population == "Subpopulation<p1>")
  data_p2_phenotype_average <- subset(data_phenotype_average, population == "Subpopulation<p2>")
  generation_uni <- 1 : (generation_before_test + generation_test + 1)
  for (generation_idx in generation_uni){
    data_p1_phenotype_average_tem <- subset(data_p1_phenotype_average, generation == generation_idx)
    data_p2_phenotype_average_tem <- subset(data_p2_phenotype_average, generation == generation_idx)
    phenotype_average_distance <- sqrt(sum((data_p1_phenotype_average_tem[phenotype_name] - data_p2_phenotype_average_tem[phenotype_name])^2))
    data_record <- rbind(data_record, c(repeat_record, generation_idx, phenotype_average_distance))
  }
  repeat_record <- repeat_record + 1
}
colnames(data_record) <- c("repeat_number", "generation", "phenotype_distance")
fileName <- paste0(path_main_analysis,"/main_whole_phenotype_distance.dat")
write.table(data_record, file = fileName, row.names = FALSE, col.names = TRUE)

#Reorganize data about the population dynamic in main simulation.
repeat_record <- 1;
data_record <- NULL
while(repeat_record > 0){
  fileName <- paste0(path_main_whole_simulation,"/",repeat_record,".dat")
  if (!file.exists(fileName)){
    break
  }
  data_main_whole <- read.csv(fileName, header = TRUE, sep = " ")
  data_main_whole$population[data_main_whole$population == "Subpopulation<p1>"] <- "p1"
  data_main_whole$population[data_main_whole$population == "Subpopulation<p2>"] <- "p2"
  data_main_whole[phenotype_name] <- NULL
  data_main_whole$repeat_number <- rep(repeat_record, 2*(generation_before_test + generation_test + 2))
  data_record <- rbind(data_record, data_main_whole)
  repeat_record <- repeat_record+1
}
fileName <- paste0(path_main_analysis,"/main_whole_population.dat")
write.table(data_record, file = fileName, row.names = FALSE, col.names = TRUE)

#Reorganize data about the population dynamic in retest simulation.
repeat_record <- 1;
data_record <- NULL
while(repeat_record > 0){
  fileName <- paste0(path_retest_whole_simulation,"/",repeat_record,".dat")
  if (!file.exists(fileName)){
    break
  }
  data_retest_whole <- read.csv(fileName, header = TRUE, sep = " ")
  data_retest_whole$population[data_retest_whole$population == "Subpopulation<p1>"] <- "p1"
  data_retest_whole$population[data_retest_whole$population == "Subpopulation<p2>"] <- "p2"
  data_retest_whole[phenotype_name] <- NULL
  data_retest_whole$repeat_number <- rep(repeat_record, 2*(generation_before_test + generation_test + 2))
  data_record <- rbind(data_record, data_retest_whole)
  repeat_record <- repeat_record+1
}
fileName <- paste0(path_retest_analysis,"/retest_whole_population.dat")
write.table(data_record, file = fileName, row.names = FALSE, col.names = TRUE)



library(ggplot2)
library(ggeffects)
library(car)
library(lme4)
library(emmeans)
library(patchwork)

color_mapping <- c("p1" = "red", "p2" = "blue")

fileName <- paste0(path_main_analysis,"/main_test.dat")
data_main <- read.csv(fileName, header = TRUE, sep = " ")
data_main$shape <- data_main$PC2_length / data_main$PC1_length
data_main_p1 <- subset(data_main, population == "p1")
data_main_p2 <- subset(data_main, population == "p2")
fileName <- paste0(path_retest_analysis,"/retest_test.dat")
data_retest <- read.csv(fileName, header = TRUE, sep = " ")
data_retest$shape <- data_retest$PC2_length / data_retest$PC1_length
data_retest_p1 <- subset(data_retest, population == "p1")
data_retest_p2 <- subset(data_retest, population == "p2")

#Figures about population dynamic in main simulation.
fileName <- paste0(path_main_analysis,"/main_whole_population.dat")
data_population_size <- read.csv(fileName, header = TRUE, sep = " ")
data_p1_population_size <- subset(data_population_size, population == "p1")
data_p2_population_size <- subset(data_population_size, population == "p2")
data_p1_population_size_mean  <- aggregate(data_p1_population_size$size, by = list(data_p1_population_size$generation), mean)
data_p2_population_size_mean  <- aggregate(data_p2_population_size$size, by = list(data_p2_population_size$generation), mean)
colnames(data_p1_population_size_mean) <- c("generation", "size")
colnames(data_p2_population_size_mean) <- c("generation", "size")
data_p1_population_size_mean$phase <- " "
data_p1_population_size_mean[1:generation_before_test,3] <- "Pretest Phase"
data_p1_population_size_mean[(1:generation_test)+generation_before_test,3] <- "Test Phase"

fig_2a <- ggplot( ) + 
  geom_point(aes(x = generation, y = size,col= "p1"),data=data_p1_population_size_mean[1:generation_before_test,])+
  geom_point(aes(x = generation, y = size,col= "p2"),data=data_p2_population_size_mean[1:generation_before_test,])+
  labs(x = "Generation", y = "Population Size") + 
  scale_color_manual(name="Population", values = c("p1" = "red", "p2" = "blue")) +
  theme_bw() + 
  theme(legend.position = c(0.7, 0.29), legend.background = element_rect(linetype = 1, color="black", size=0.25), legend.text = element_text(size = 6), legend.title = element_text(size = 8), panel.border = element_rect(colour = "black")) +
  facet_wrap(~ phase, scales = "fixed")
ggsave(filename = paste0(path_figures,"/population_beforetest_average.png"),dpi = 600)

fig_2b <- ggplot( ) + 
  geom_point(aes(x = generation, y = size,col= "p1"),data=data_p1_population_size_mean[(1:generation_test)+generation_before_test,])+
  geom_point(aes(x = generation, y = size,col= "p2"),data=data_p2_population_size_mean[(1:generation_test)+generation_before_test,])+
  labs(x = "Generation", y = "Population Size") + 
  scale_color_manual(name="Population", values = c("p1" = "red", "p2" = "blue")) +
  theme_bw() + 
  theme(legend.position = c(0.5, 0.29), legend.background = element_rect(linetype = 1, color="black", size=0.25), legend.text = element_text(size = 6), legend.title = element_text(size = 8), panel.border = element_rect(colour = "black")) +
  facet_wrap(~ phase, scales = "fixed")
ggsave(filename = paste0(path_figures,"/population_test_average.png"),dpi = 600)

#Figures about phenotype distance in main simulation.
fileName <- paste0(path_main_analysis,"/main_whole_phenotype_distance.dat")
data_phenotype_distance <- read.csv(fileName, header = TRUE, sep = " ")
data_phenotype_distance_mean  <- aggregate(data_phenotype_distance$phenotype_distance, by = list(data_phenotype_distance$generation), mean)
colnames(data_phenotype_distance_mean) <- c("generation", "phenotype_distance")
data_phenotype_distance_mean$phase <- " "
data_phenotype_distance_mean[1:generation_before_test,3] <- "Pretest Phase"
data_phenotype_distance_mean[(1:generation_test)+generation_before_test,3] <- "Test Phase"

fig_s1a <- ggplot( ) + 
  geom_point(aes(x = generation, y = phenotype_distance),data=data_phenotype_distance_mean[1:generation_before_test,])+
  labs(x = "Generation", y = "Phenotype Distance between Populations") + 
  scale_color_manual() +
  theme_bw() + 
  theme(legend.position = c(0.5, 0.29), legend.background = element_rect(linetype = 1, color="black", size=0.25), legend.text = element_text(size = 6), legend.title = element_text(size = 8), panel.border = element_rect(colour = "black")) +
  facet_wrap(~ phase, scales = "fixed")
ggsave(filename = paste0(path_figures,"/phenotype_distance_beforetest_average.png"),dpi = 600)

fig_s1b <- ggplot( ) + 
  geom_point(aes(x = generation, y = phenotype_distance),data=data_phenotype_distance_mean[(1:generation_test)+generation_before_test,])+
  labs(x = "Generation", y = "Phenotype Distance between Populations") + 
  scale_color_manual() +
  theme_bw() + 
  theme(legend.position = c(0.5, 0.29), legend.background = element_rect(linetype = 1, color="black", size=0.25), legend.text = element_text(size = 6), legend.title = element_text(size = 8), panel.border = element_rect(colour = "black")) +
  facet_wrap(~ phase, scales = "fixed")
ggsave(filename = paste0(path_figures,"/phenotype_distance_test_average.png"),dpi = 600)

#Anova and box Plots about the comparison in first generation of test phase in main simulation.

data_main_test_1st <- subset(data_main, generation == generation_before_test + 1)

result_aov_fitness_increase <- aov(fitness_increase ~ population, data = data_main_test_1st)
summary(result_aov_fitness_increase)
result_aov_angle <- aov(angle ~ population, data = data_main_test_1st)
summary(result_aov_angle)
result_aov_fitness <- aov(fitness ~ population, data = data_main_test_1st)
summary(result_aov_fitness)
result_aov_distance <- aov(phenotype_distance ~ population, data = data_main_test_1st)
summary(result_aov_distance)
result_aov_shape <- aov(shape ~ population, data = data_main_test_1st)
summary(result_aov_shape)
result_aov_PC1_length <- aov(PC1_length ~ population, data = data_main_test_1st)
summary(result_aov_PC1_length)

fig_2c <- ggplot(data_main_test_1st, aes(x = population, y = fitness, color=population)) +
  geom_boxplot(fill="grey98", color= "black", outlier.colour = NA) +
  geom_jitter(width = 0.15, size=0.4, alpha=0.9) +
  labs(x = "Population", y = "Fitness") +
  theme_classic() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
        text = element_text(size = 10),
        legend.position = c(0.2, 0.87),
        legend.key.height = unit(0.08, "cm"),
        legend.key.width = unit(0.08, "cm")) +
  scale_shape_manual(name=NULL, values=c(16,8)) +
  scale_color_manual(guide="none",values = c("p1" = "red", "p2" = "blue")) + 
  scale_y_continuous() + 
  scale_x_discrete()
ggsave(filename = paste0(path_figures,"/S_main_test_1st_fitness.png"),dpi = 600)

fig_2d <- ggplot(data_main_test_1st, aes(x = population, y = fitness_increase, color=population)) +
  geom_boxplot(fill="grey98", color= "black", outlier.colour = NA) +
  geom_jitter(width = 0.15, size=0.4, alpha=0.9) +
  labs(x = "Population", y = "Fitness Increase") +
  theme_classic() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
        text = element_text(size = 10),
        legend.position = c(0.2, 0.87),
        legend.key.height = unit(0.08, "cm"),
        legend.key.width = unit(0.08, "cm")) +
  scale_shape_manual(name=NULL, values=c(16,8)) +
  scale_color_manual(guide="none",values = c("p1" = "red", "p2" = "blue")) + 
  scale_y_continuous() + 
  scale_x_discrete()
ggsave(filename = paste0(path_figures,"/S_main_test_1st_fitness_increase.png"),dpi = 600)

fig_2e <- ggplot(data_main_test_1st, aes(x = population, y = phenotype_distance, color=population)) +
  geom_boxplot(fill="grey98", color= "black", outlier.colour = NA) +
  geom_jitter(width = 0.15, size=0.4, alpha=0.9) +
  labs(x = "Population", y = "Distance to Optimum") +
  theme_classic() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
        text = element_text(size = 10),
        legend.position = c(0.2, 0.87),
        legend.key.height = unit(0.08, "cm"),
        legend.key.width = unit(0.08, "cm")) +
  scale_shape_manual(name=NULL, values=c(16,8)) +
  scale_color_manual(guide="none",values = c("p1" = "red", "p2" = "blue")) + 
  scale_y_continuous() + 
  scale_x_discrete()
ggsave(filename = paste0(path_figures,"/S_main_test_1st_distance.png"),dpi = 600)

fig_2f <- ggplot(data_main_test_1st, aes(x = population, y = angle, color=population)) +
  geom_boxplot(fill="grey98", color= "black", outlier.colour = NA) +
  geom_jitter(width = 0.15, size=0.4, alpha=0.9) +
  labs(x = "Population", y = "Deviation Angle (rad)") +
  theme_classic() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
        text = element_text(size = 10),
        legend.position = c(0.2, 0.87),
        legend.key.height = unit(0.08, "cm"),
        legend.key.width = unit(0.08, "cm")) +
  scale_shape_manual(name=NULL, values=c(16,8)) +
  scale_color_manual(guide="none",values = c("p1" = "red", "p2" = "blue")) + 
  scale_y_continuous() + 
  scale_x_discrete()
ggsave(filename = paste0(path_figures,"/S_main_test_1st_angle.png"),dpi = 600)


#Using Linear model to analysis the effects on average fitnessin main simulation.
lm_main_fitness_whole <- lm(fitness ~   angle + phenotype_distance+ shape + PC1_length,  
                            data = data_retest_test_1st, na.action = na.exclude)
Anova (lm_main_fitness_whole)
summary(lm_main_fitness_whole)

pred_lm_main_fitness_phenotype_distance_whole <- ggpredict(lm_main_fitness_whole,terms = c("phenotype_distance[all]"))
fig_4a <-ggplot( ) + 
  geom_line(aes(x = x, y = predicted,group = "1" ),data = pred_lm_main_fitness_phenotype_distance_whole) +
  geom_point(aes(x = phenotype_distance, y = fitness),data_retest_test_1st)+
  labs(x = "Distance to Optimum", y = "Fitness") + 
  theme_bw() + 
  theme(legend.position = c(0.8, 0.7), legend.background = element_rect(linetype = 1, color="black", size=0.25), legend.text = element_text(size = 6), legend.title = element_text(size = 8), panel.border = element_rect(colour = "black")) 

pred_lm_main_fitness_angle_whole <- ggpredict(lm_main_fitness_whole,terms = c("angle[all]"))
fig_4b <-ggplot( ) + 
  geom_line(aes(x = x, y = predicted,group = "1" ),data = pred_lm_main_fitness_angle_whole) +
  geom_point(aes(x = angle, y = fitness),data_retest_test_1st)+
  labs(x = "Deviation Angle (rad)", y = "Fitness") + 
  theme_bw() + 
  theme(legend.position = c(0.2, 0.29), legend.background = element_rect(linetype = 1, color="black", size=0.25), legend.text = element_text(size = 6), legend.title = element_text(size = 8), panel.border = element_rect(colour = "black")) 


pred_lm_main_fitness_shape_whole <- ggpredict(lm_main_fitness_whole,terms = c("shape[all]"))
fig_4c <-ggplot( ) + 
  geom_line(aes(x = x, y = predicted,group ="1" ),data = pred_lm_main_fitness_shape_whole) +
  geom_point(aes(x = shape, y = fitness),data_retest_test_1st)+
  labs(x = "Ratio of Eigenvalue", y = "Fitness") + 
  theme_bw() + 
  theme(legend.position = c(0.8, 0.29), legend.background = element_rect(linetype = 1, color="black", size=0.25), legend.text = element_text(size = 6), legend.title = element_text(size = 8), panel.border = element_rect(colour = "black")) 

pred_lm_main_fitness_PC1_length_whole <- ggpredict(lm_main_fitness_whole,terms = c("PC1_length[all]"))
fig_4d <-ggplot( ) + 
  geom_line(aes(x = x, y = predicted,group = "1"),data = pred_lm_main_fitness_PC1_length_whole) +
  geom_point(aes(x = PC1_length, y = fitness),data_retest_test_1st)+
  labs(x = "Size", y = "Fitness") + 
  theme_bw() + 
  theme(legend.position = c(0.8, 0.69), legend.background = element_rect(linetype = 1, color="black", size=0.25), legend.text = element_text(size = 6), legend.title = element_text(size = 8), panel.border = element_rect(colour = "black")) 

#Using Linear model to analysis the effects on average fitness increase in main simulation.
lm_main_fitness_increase_whole <- lm(fitness_increase ~   angle + phenotype_distance +shape +PC1_length+population,  
                                     data = data_retest_test_1st, na.action = na.exclude)
Anova (lm_main_fitness_increase_whole)
summary(lm_main_fitness_increase_whole)

pred_lm_main_fitness_increase_phenotype_distance_whole <- ggpredict(lm_main_fitness_increase_whole,terms = c("phenotype_distance[all]"))
fig_5a <-ggplot( ) + 
  geom_line(aes(x = x, y = predicted,group = "1"),data = pred_lm_main_fitness_increase_phenotype_distance_whole) +
  geom_point(aes(x = phenotype_distance, y = fitness_increase),data_retest_test_1st)+
  labs(x = "Distance to Optimum", y = "Fitness Increase") + 
  theme_bw() + 
  theme(legend.position = c(0.8, 0.7), legend.background = element_rect(linetype = 1, color="black", size=0.25), legend.text = element_text(size = 6), legend.title = element_text(size = 8), panel.border = element_rect(colour = "black")) 

pred_lm_main_fitness_increase_angle_whole <- ggpredict(lm_main_fitness_increase_whole,terms = c("angle[all]"))
fig_5b <-ggplot( ) + 
  geom_line(aes(x = x, y = predicted,group = "1"),data = pred_lm_main_fitness_increase_angle_whole) +
  geom_point(aes(x = angle, y = fitness_increase),data_retest_test_1st)+
  labs(x = "Deviation Angle (rad)", y = "Fitness Increase") + 
  theme_bw() + 
  theme(legend.position = c(0.8, 0.7), legend.background = element_rect(linetype = 1, color="black", size=0.25), legend.text = element_text(size = 6), legend.title = element_text(size = 8), panel.border = element_rect(colour = "black")) 


pred_lm_main_fitness_increase_shape_whole <- ggpredict(lm_main_fitness_increase_whole,terms = c("shape[all]"))
pred_lm_main_fitness_increase_shape_whole_p1 <- subset(pred_lm_main_fitness_increase_shape_whole, group == "p1")
pred_lm_main_fitness_increase_shape_whole_p2 <- subset(pred_lm_main_fitness_increase_shape_whole, group == "p2")
fig_5c <-ggplot( ) + 
  geom_line(aes(x = x, y = predicted,group = "1"),data = pred_lm_main_fitness_increase_shape_whole) +
  geom_point(aes(x = shape, y = fitness_increase),data_retest_test_1st)+
  labs(x = "Ratio of Eigenvalue", y = "Fitness Increase") + 
  theme_bw() + 
  theme(legend.position = c(0.8, 0.7), legend.background = element_rect(linetype = 1, color="black", size=0.25), legend.text = element_text(size = 6), legend.title = element_text(size = 8), panel.border = element_rect(colour = "black")) 

pred_lm_main_fitness_increase_PC1_length_whole <- ggpredict(lm_main_fitness_increase_whole,terms = c("PC1_length[all]"))
fig_5d <-ggplot( ) + 
  geom_line(aes(x = x, y = predicted,group = "1" ),data = pred_lm_main_fitness_increase_PC1_length_whole) +
  geom_point(aes(x = PC1_length, y = fitness_increase),data_retest_test_1st)+
  labs(x = "Size", y = "Fitness Increase") + 
  theme_bw() + 
  theme(legend.position = c(0.8, 0.3), legend.background = element_rect(linetype = 1, color="black", size=0.25), legend.text = element_text(size = 6), legend.title = element_text(size = 8), panel.border = element_rect(colour = "black")) 

######Dynamic of PC1 length and the shape of genetic variance in main test

PC1_length_main_p1_mean  <- aggregate(data_main_p1$PC1_length, by = list(data_main_p1$generation), mean)
shape_main  <- aggregate(data_main_p1$shape, by = list(data_main_p1$generation), mean)
colnames(shape_main) <- c("generation", "shape_p1")
shape_main$PC1_length_p1 <- PC1_length_main_p1_mean$x
PC1_length_main_p2_mean  <- aggregate(data_main_p2$PC1_length, by = list(data_main_p1$generation), mean)
shape_main$PC1_length_p2 <- PC1_length_main_p2_mean$x
shape_main_p2  <- aggregate(data_main_p2$shape, by = list(data_main_p2$generation), mean)
shape_main$shape_p2 <-   shape_main_p2$x

lme_shape_dynamic_main <- lmer(shape ~ population + factor(generation)  + (1|repeat_number),  
                           data = data_main, na.action = na.exclude)
summary(lme_shape_dynamic_main)
Anova(lme_shape_dynamic_main) 
pred_shape_dynamic_main <- ggpredict(lme_shape_dynamic_main, terms = c("population[all]","generation[all]"))
pred_shape_dynamic_main$generation <- as.numeric(as.character(pred_shape_dynamic_main$group))
pred_shape_dynamic_main_p1 <- subset(pred_shape_dynamic_main, x == "p1")
pred_shape_dynamic_main_p2 <- subset(pred_shape_dynamic_main, x == "p2")

fig_3e <-ggplot( ) + 
  geom_point(aes(x = generation, y = shape_p1,col= "p1"),data=shape_main)+
  geom_point(aes(x = generation, y = shape_p2,col= "p2"),data=shape_main)+
  geom_line(aes(x = generation, y = predicted,group =x,col =x ),data = pred_shape_dynamic_main) +
  labs(x = "Generation", y = "Ratio of Eigenvalue") + 
  scale_color_manual(name="Population", values = c("p1" = "red", "p2" = "blue")) +
  theme_bw() + 
  theme(legend.position = c(0.75, 0.75), legend.background = element_rect(linetype = 1, color="black", size=0.25), legend.text = element_text(size = 6), legend.title = element_text(size = 8), panel.border = element_rect(colour = "black")) 
ggsave(filename = paste0(path_figures,"/shape_dynamic_main.png"),dpi = 600)

lme_PC1_dynamic_main <- lmer(PC1_length ~ population + factor(generation)  + (1|repeat_number),  
                               data = data_main, na.action = na.exclude)
summary(lme_PC1_dynamic_main)
Anova(lme_PC1_dynamic_main) 
pred_PC1_dynamic_main <- ggpredict(lme_PC1_dynamic_main, terms = c("population[all]","generation[all]"))
pred_PC1_dynamic_main$generation <- as.numeric(as.character(pred_PC1_dynamic_main$group))
pred_PC1_dynamic_main_p1 <- subset(pred_PC1_dynamic_main, x == "p1")
pred_PC1_dynamic_main_p2 <- subset(pred_PC1_dynamic_main, x == "p2")
fig_3f <-ggplot( ) + 
  geom_point(aes(x = generation, y = PC1_length_p1,col= "p1"),data=shape_main)+
  geom_point(aes(x = generation, y = PC1_length_p2,col= "p2"),data=shape_main)+
  geom_line(aes(x = generation, y = predicted,group =x,col =x ),data = pred_PC1_dynamic_main) +
  labs(x = "Generation", y = "Size") + 
  scale_color_manual(name="Population", values = c("p1" = "red", "p2" = "blue")) +
  theme_bw() + 
  theme(legend.position = c(0.8, 0.3), legend.background = element_rect(linetype = 1, color="black", size=0.25), legend.text = element_text(size = 6), legend.title = element_text(size = 8), panel.border = element_rect(colour = "black")) 

ggsave(filename = paste0(path_figures,"/PC1_dynamic_main.png"),dpi = 600)

###############Dynamic of angle in main test
angle_main  <- aggregate(data_main_p1$angle, by = list(data_main_p1$generation), mean)
colnames(angle_main) <- c("generation", "angle_p1")
angle_main_p2_mean  <- aggregate(data_main_p2$angle, by = list(data_main_p2$generation), mean)
angle_main$angle_p2 <- angle_main_p2_mean$x
lme_angle_dynamic_main <- lmer(angle ~ population + factor(generation)  + (1|repeat_number),  
                             data = data_main, na.action = na.exclude)
summary(lme_angle_dynamic_main)
Anova(lme_angle_dynamic_main) 
pred_angle_dynamic_main <- ggpredict(lme_angle_dynamic_main, terms = c("population[all]","generation[all]"))
pred_angle_dynamic_main$generation <- as.numeric(as.character(pred_angle_dynamic_main$group))
pred_angle_dynamic_main_p1 <- subset(pred_angle_dynamic_main, x == "p1")
pred_angle_dynamic_main_p2 <- subset(pred_angle_dynamic_main, x == "p2")

fig_3d <-ggplot( ) + 
  geom_point(aes(x = generation, y = angle_p1,col= "p1"),data=angle_main)+
  geom_point(aes(x = generation, y = angle_p2,col= "p2"),data=angle_main)+
  geom_line(aes(x = generation, y = predicted,group =x,col =x ),data = pred_angle_dynamic_main) +
  labs(x = "Generation", y = "Deviation Angle (rad)") + 
  scale_color_manual(name="Population", values = c("p1" = "red", "p2" = "blue")) +
  theme_bw() + 
  theme(legend.position = c(0.4, 0.75), legend.background = element_rect(linetype = 1, color="black", size=0.25), legend.text = element_text(size = 6), legend.title = element_text(size = 8), panel.border = element_rect(colour = "black")) 

ggsave(filename = paste0(path_figures,"/angle_dynamic_main.png"),dpi = 600)

###############Dynamic of fitness and fitness increase in main test
fitness_increase_main_p1_mean  <- aggregate(data_main_p1$fitness_increase, by = list(data_main_p1$generation), mean)
fitness_main  <- aggregate(data_main_p1$fitness, by = list(data_main_p1$generation), mean)
colnames(fitness_main) <- c("generation", "fitness_p1")
fitness_main$fitness_increase_p1 <- fitness_increase_main_p1_mean$x
fitness_increase_main_p2_mean  <- aggregate(data_main_p2$fitness_increase, by = list(data_main_p1$generation), mean)
fitness_main$fitness_increase_p2 <- fitness_increase_main_p2_mean$x
fitness_main_p2  <- aggregate(data_main_p2$fitness, by = list(data_main_p2$generation), mean)
fitness_main$fitness_p2 <-   fitness_main_p2$x

lme_fitness_dynamic_main <- lmer(fitness ~ population + factor(generation)  + (1|repeat_number),  
                               data = data_main, na.action = na.exclude)
summary(lme_fitness_dynamic_main)
Anova(lme_fitness_dynamic_main) 
pred_fitness_dynamic_main <- ggpredict(lme_fitness_dynamic_main, terms = c("population[all]","generation[all]"))
pred_fitness_dynamic_main$generation <- as.numeric(as.character(pred_fitness_dynamic_main$group))
pred_fitness_dynamic_main_p1 <- subset(pred_fitness_dynamic_main, x == "p1")
pred_fitness_dynamic_main_p2 <- subset(pred_fitness_dynamic_main, x == "p2")
fig_3a <-ggplot( ) + 
  geom_point(aes(x = generation, y = fitness_p1,col= "p1"),data=fitness_main)+
  geom_point(aes(x = generation, y = fitness_p2,col= "p2"),data=fitness_main)+
  geom_line(aes(x = generation, y = predicted,group =x,col =x ),data = pred_fitness_dynamic_main) +
  labs(x = "Generation", y = "Fitness") +  
  scale_color_manual(name="Population", values = c("p1" = "red", "p2" = "blue")) +
  theme_bw() + 
  theme(legend.position = c(0.8, 0.3), legend.background = element_rect(linetype = 1, color="black", size=0.25), legend.text = element_text(size = 6), legend.title = element_text(size = 8), panel.border = element_rect(colour = "black")) 

ggsave(filename = paste0(path_figures,"/fitness_dynamic_main.png"),dpi = 600)

lme_fitness_increase_dynamic_main <- lmer(fitness_increase ~ population + factor(generation)  + (1|repeat_number),  
                                 data = data_main, na.action = na.exclude)
summary(lme_fitness_increase_dynamic_main)
Anova(lme_fitness_increase_dynamic_main) 
pred_fitness_increase_dynamic_main <- ggpredict(lme_fitness_increase_dynamic_main, terms = c("population[all]","generation[all]"))
pred_fitness_increase_dynamic_main$generation <- as.numeric(as.character(pred_fitness_increase_dynamic_main$group))
pred_fitness_increase_dynamic_main_p1 <- subset(pred_fitness_increase_dynamic_main, x == "p1")
pred_fitness_increase_dynamic_main_p2 <- subset(pred_fitness_increase_dynamic_main, x == "p2")

fig_3b <-ggplot( ) + 
  geom_point(aes(x = generation, y = fitness_increase_p1,col= "p1"),data=fitness_main)+
  geom_point(aes(x = generation, y = fitness_increase_p2,col= "p2"),data=fitness_main)+
  geom_line(aes(x = generation, y = predicted,group =x,col =x ),data = pred_fitness_increase_dynamic_main) +
  labs(x = "Generation", y = "Fitness Increase") + 
  scale_color_manual(name="Population", values = c("p1" = "red", "p2" = "blue")) +
  theme_bw() + 
  theme(legend.position = c(0.8, 0.7), legend.background = element_rect(linetype = 1, color="black", size=0.25), legend.text = element_text(size = 6), legend.title = element_text(size = 8), panel.border = element_rect(colour = "black")) 

ggsave(filename = paste0(path_figures,"/fitness_increase_dynamic_main.png"),dpi = 600)

################################dynamic of distance to optimum
distance_main  <- aggregate(data_main_p1$phenotype_distance, by = list(data_main_p1$generation), mean)
colnames(distance_main) <- c("generation", "distance_p1")
distance_main_p2_mean  <- aggregate(data_main_p2$phenotype_distance, by = list(data_main_p1$generation), mean)
distance_main$distance_p2 <- distance_main_p2_mean$x

lme_distance_dynamic_main <- lmer(phenotype_distance ~ population + factor(generation)  + (1|repeat_number),  
                                 data = data_main, na.action = na.exclude)
summary(lme_distance_dynamic_main)
Anova(lme_distance_dynamic_main) 
pred_distance_dynamic_main <- ggpredict(lme_distance_dynamic_main, terms = c("population[all]","generation[all]"))
pred_distance_dynamic_main$generation <- as.numeric(as.character(pred_distance_dynamic_main$group))
pred_distance_dynamic_main_p1 <- subset(pred_distance_dynamic_main, x == "p1")
pred_distance_dynamic_main_p2 <- subset(pred_distance_dynamic_main, x == "p2")

fig_3c <-ggplot( ) + 
  geom_point(aes(x = generation, y = distance_p1,col= "p1"),data=distance_main)+
  geom_point(aes(x = generation, y = distance_p2,col= "p2"),data=distance_main)+
  geom_line(aes(x = generation, y = predicted,group =x,col =x ),data = pred_distance_dynamic_main) +
  labs(x = "Generation", y = "Distance to Optimum") + 
  scale_color_manual(name="Population", values = c("p1" = "red", "p2" = "blue")) +
  theme_bw() + 
  theme(legend.position = c(0.8, 0.7), legend.background = element_rect(linetype = 1, color="black", size=0.25), legend.text = element_text(size = 6), legend.title = element_text(size = 8), panel.border = element_rect(colour = "black")) 
ggsave(filename = paste0(path_figures,"/distance_dynamic_main.png"),dpi = 600)


 (fig_2c + fig_2d) / (fig_2e + fig_2f) + plot_annotation(tag_levels = "a")
ggsave(filename = paste0(path_figures,"/figure_2.png"),dpi = 600, width=8, height=7)
(fig_3a + fig_3b) / (fig_3c + fig_3d)  / (fig_3e + fig_3f) + plot_annotation(tag_levels = "a")
ggsave(filename = paste0(path_figures,"/figure_3.png"),dpi = 600, width=8, height=7)
(fig_4a + fig_4b) / (fig_4c + fig_4d)  + plot_annotation(tag_levels = "a")
ggsave(filename = paste0(path_figures,"/figure_4.png"),dpi = 600, width=8, height=7)
(fig_5a + fig_5b) / (fig_5c + fig_5d)  + plot_annotation(tag_levels = "a")
ggsave(filename = paste0(path_figures,"/figure_5.png"),dpi = 600, width=8, height=7)
fig_s1a + fig_s1b + plot_annotation(tag_levels = "a")
ggsave(filename = paste0(path_figures,"/figure_s_1.png"),dpi = 600, width=8, height=7)
fig_s2a + fig_s2b + plot_annotation(tag_levels = "a")
ggsave(filename = paste0(path_figures,"/figure_s_2.png"),dpi = 600, width=8, height=7)


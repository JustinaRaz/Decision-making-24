setwd('/work/JustinaRazanauskaite#2891/DM/parameter_estimation/PPC/')

install.packages("pacman")
pacman::p_load(ggplot2)

#load control data
young_data <- read.csv("../data/younger_group.csv")

subIDs <- unique(young_data$subjID)
nsubs <- length(subIDs)
ntrials_max <- 100

x_raw <- young_data$deck
X_raw <- young_data$gain + young_data$loss

ntrials_all <- array(0,c(nsubs))
x_all <- array(0,c(nsubs,ntrials_max))
X_all <- array(0,c(nsubs,ntrials_max))

for (s in 1:nsubs) {
  
  #record n trials for subject s
  ntrials_all[s] <- length(x_raw[young_data$subjID==subIDs[s]])
  
  #pad trials with NA if n trials < maximum (i.e. 100)
  x_sub <- x_raw[young_data$subjID==subIDs[s]] 
  length(x_sub) <- ntrials_max
  
  X_sub <- X_raw[young_data$subjID==subIDs[s]] 
  length(X_sub) <- ntrials_max
  
  # assign arrays
  x_all[s,] <- x_sub
  X_all[s,] <- X_sub
  
}

load("output/ppc_pred_young.RData")

pred_df$pred_success <- pred_df$pred_success_adjust * ntrials_all
pred_df$sub <- 1:length(pred_df$pred_success_adjust)

overall_avg <- mean(pred_df$pred_success_adjust, na.rm = TRUE)
overall_std <- sd(pred_df$pred_success_adjust, na.rm = TRUE)

pred_df$chance <- .25

pl <- ggplot(pred_df, aes(sub, pred_success_adjust)) +
  geom_point() +
  geom_line(aes(y = chance), linetype = "dashed", color = "black") +
  geom_ribbon(aes(ymin = overall_avg - overall_std, ymax = overall_avg + overall_std), 
              fill = "chocolate3", alpha = 0.6) + 
  geom_line(aes(y = overall_avg), color = "black") + 
  ylim(0, 1) +
  labs(
    x = "Younger subjects",
    y = "Correct predictions (percentage)"
  ) +
  theme_minimal()

ggsave("plots/ppc_young.jpg", pl, width = 10, height = 6)

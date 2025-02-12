install.packages("pacman")
install.packages("showtext")
pacman::p_load(tidyverse, gridExtra, grid, showtext)
setwd("/work/JustinaRazanauskaite#2891/DM/parameter_recovery/")

old_recovery <- read_csv("old_recovery.csv")
young_recovery <- read_csv("young_recovery.csv")

old_recovery$group <- "old"
young_recovery$group <- "young"

combined_recovery <- rbind(old_recovery, young_recovery)

pl1 <- ggplot(combined_recovery, aes(x = true_mu_a_rew, y = infer_mu_a_rew, color = group)) +
  geom_point(size = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  geom_smooth(method = "lm", se = TRUE, formula = "y ~ x") +
  scale_color_manual(values = c("young" = "chocolate2", "old" = "cornflowerblue")) +
  labs(color = "Group") +
  theme_minimal() +
  xlab("True") + 
  ylab("Inferred") +
  ggtitle(expression(paste(mu, " - ", alpha["rew "]))) +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        legend.position = "none")

pl2 <- ggplot(combined_recovery, aes(x = true_mu_a_pun, y = infer_mu_a_pun, color = group)) +
  geom_point(size = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  geom_smooth(method = "lm", se = TRUE, formula = "y ~ x") +
  scale_color_manual(values = c("young" = "chocolate2", "old" = "cornflowerblue")) +
  labs(color = "Group") +
  theme_minimal() +
  xlab("True") + 
  ylab("Inferred") +
  ggtitle(expression(paste(mu, " - ", alpha["pun "]))) +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        legend.position = "none")

pl3 <- ggplot(combined_recovery, aes(x = true_mu_K, y = infer_mu_K, color = group)) +
  geom_point(size = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  geom_smooth(method = "lm", se = TRUE, formula = "y ~ x") +
  scale_color_manual(values = c("young" = "chocolate2", "old" = "cornflowerblue")) +
  labs(color = "Group") +
  theme_minimal() +
  xlab("True") + 
  ylab("Inferred") +
  ggtitle(expression(paste(mu, " - K"))) +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        legend.position = "none")


pl4 <- ggplot(combined_recovery, aes(x = true_mu_omega_f, y = infer_mu_omega_f, color = group)) +
  geom_point(size = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  geom_smooth(method = "lm", se = TRUE, formula = "y ~ x") +
  scale_color_manual(values = c("young" = "chocolate2", "old" = "cornflowerblue")) +
  labs(color = "Group") +
  theme_minimal() +
  xlab("True") + 
  ylab("Inferred") +
  ggtitle(expression(paste(mu, " - ", omega["f "]))) +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        legend.position = "none")

pl5 <- ggplot(combined_recovery, aes(x = true_mu_omega_p, y = infer_mu_omega_p, color = group)) +
  geom_point(size = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  geom_smooth(method = "lm", se = TRUE, formula = "y ~ x") +
  scale_color_manual(values = c("young" = "chocolate2", "old" = "cornflowerblue")) +
  labs(color = "Group") +
  theme_minimal() +
  xlab("True") + 
  ylab("Inferred") +
  ggtitle(expression(paste(mu, " - ", omega["p "]))) +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        legend.position = "none")

pl_old_1 <- grid.arrange(pl1, pl2, pl3, pl4, pl5, nrow = 2, ncol = 3)
ggsave("plots_recovery/pl_combined_recovery_mu.png", pl_old_1)


pl1 <- ggplot(combined_recovery, aes(x = true_lambda_a_rew, y = infer_lambda_a_rew, color = group)) +
  geom_point(size = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  geom_smooth(method = "lm", se = TRUE, formula = "y ~ x") +
  scale_color_manual(values = c("young" = "chocolate2", "old" = "cornflowerblue")) +
  labs(color = "Group") +
  theme_minimal() +
  xlab("True") + 
  ylab("Inferred") +
  ggtitle(expression(paste(lambda, " - ", alpha["rew "]))) + 
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        legend.position = "none")

pl2 <- ggplot(combined_recovery, aes(x = true_lambda_a_pun, y = infer_lambda_a_pun, color = group)) +
  geom_point(size = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  geom_smooth(method = "lm", se = TRUE, formula = "y ~ x") +
  scale_color_manual(values = c("young" = "chocolate2", "old" = "cornflowerblue")) +
  labs(color = "Group") +
  theme_minimal() +
  xlab("True") + 
  ylab("Inferred") +
  ggtitle(expression(paste(lambda, " - ", alpha["pun "]))) +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        legend.position = "none")

pl3 <- ggplot(combined_recovery, aes(x = true_lambda_K, y = infer_lambda_K, color = group)) +
  geom_point(size = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  geom_smooth(method = "lm", se = TRUE, formula = "y ~ x") +
  scale_color_manual(values = c("young" = "chocolate2", "old" = "cornflowerblue")) +
  labs(color = "Group") +
  theme_minimal() +
  xlab("True") + 
  ylab("Inferred") +
  ggtitle(expression(paste(lambda, " - K"))) +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        legend.position = "none")


pl4 <- ggplot(combined_recovery, aes(x = true_lambda_omega_f, y = infer_lambda_omega_f, color = group)) +
  geom_point(size = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  geom_smooth(method = "lm", se = TRUE, formula = "y ~ x") +
  scale_color_manual(values = c("young" = "chocolate2", "old" = "cornflowerblue")) +
  labs(color = "Group") +
  theme_minimal() +
  xlab("True") + 
  ylab("Inferred") +
  ggtitle(expression(paste(lambda, " - ", omega["f "]))) +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        legend.position = "none")

pl5 <- ggplot(combined_recovery, aes(x = true_lambda_omega_p, y = infer_lambda_omega_p, color = group)) +
  geom_point(size = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  geom_smooth(method = "lm", se = TRUE, formula = "y ~ x") +
  scale_color_manual(values = c("young" = "chocolate2", "old" = "cornflowerblue")) +
  labs(color = "Group") +
  theme_minimal() +
  xlab("True") + 
  ylab("Inferred") +
  ggtitle(expression(paste(lambda, " - ", omega["p "]))) +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        legend.position = "none")

pl_old_2 <- grid.arrange(pl1, pl2, pl3, pl4, pl5, nrow = 2, ncol = 3)
ggsave("plots_recovery/pl_combined_recovery_lambda.png", pl_old_2)

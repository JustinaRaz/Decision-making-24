install.packages("pacman")
install.packages("showtext")
pacman::p_load(tidyverse, gridExtra, grid, showtext)
setwd("/work/JustinaRazanauskaite#2891/DM/parameter_recovery")

# Load all data files (ran 10 trials x 10 times in tmux)

load("parameter_recovery/JAGS_data/JAGS_hier_ORL_recovery_no_theta_old.RData")
older_data <- data.frame(infer_lambda_a_pun, infer_lambda_a_rew, infer_lambda_K, infer_lambda_omega_f,
                 infer_lambda_omega_p, infer_mu_a_pun, infer_mu_a_rew, infer_mu_K,
                 infer_mu_omega_f, infer_mu_omega_p, true_lambda_a_pun, true_lambda_a_rew,
                 true_lambda_K, true_lambda_omega_f, true_lambda_omega_p, true_mu_a_pun,
                 true_mu_a_rew, true_mu_K, true_mu_omega_f, true_mu_omega_p)

# PLOTS

font_add_google("EB Garamond", "ebgaramond")
showtext_auto()

pl1 <- ggplot(older_data, 
              aes(x = true_mu_a_rew,
                  y = infer_mu_a_rew)) +
  geom_point(color = "cornflowerblue", size = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  geom_smooth(method = "lm", se = TRUE, formula = "y ~ x", color = "cornflowerblue") +
  theme_minimal() +
  xlab("True") + 
  ylab("Inferred") +
  ggtitle(expression(paste(mu, " - ", alpha["rew "]))) +
  theme(plot.title = element_text(hjust = 0.5, size = 16))

pl2 <- ggplot(older_data, 
              aes(x = true_mu_a_pun,
                  y = infer_mu_a_pun)) +
  geom_point(color = "cornflowerblue", size = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  geom_smooth(method = "lm", se = TRUE, formula = "y ~ x", color = "cornflowerblue") +
  theme_minimal() +
  xlab("True") + 
  ylab("Inferred") +
  ggtitle(expression(paste(mu, " - ", alpha["pun "]))) +
  theme(plot.title = element_text(hjust = 0.5, size = 16)) 

pl3 <- ggplot(older_data, 
              aes(x = true_mu_K,
                  y = infer_mu_K)) +
  geom_point(color = "cornflowerblue", size = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  geom_smooth(method = "lm", se = TRUE, formula = "y ~ x", color = "cornflowerblue") +
  theme_minimal() +
  xlab("True") + 
  ylab("Inferred") +
  ggtitle(expression(paste(mu, " - K"))) +
  theme(plot.title = element_text(hjust = 0.5, size = 16)) 

pl4 <- ggplot(older_data, 
              aes(x = true_mu_omega_f,
                  y = infer_mu_omega_f)) +
  geom_point(color = "cornflowerblue", size = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  geom_smooth(method = "lm", se = TRUE, formula = "y ~ x", color = "cornflowerblue") +
  theme_minimal() +
  xlab("True") + 
  ylab("Inferred") +
  ggtitle(expression(paste(mu, " - ", omega["f "]))) +
  theme(plot.title = element_text(hjust = 0.5, size = 16))

pl5 <- ggplot(older_data, 
              aes(x = true_mu_omega_p,
                  y = infer_mu_omega_p)) +
  geom_point(color = "cornflowerblue", size = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  geom_smooth(method = "lm", se = TRUE, formula = "y ~ x", color = "cornflowerblue") +
  theme_minimal() +
  xlab("True") + 
  ylab("Inferred") +
  ggtitle(expression(paste(mu, " - ", omega["p "]))) +
  theme(plot.title = element_text(hjust = 0.5, size = 16))

pl_old_1 <- grid.arrange(pl1, pl2, pl3, pl4, pl5, nrow = 2, ncol = 3)
ggsave("plots_recovery/pl_old_recovery_mu.png", pl_old_1)

pl6 <- ggplot(older_data, 
              aes(x = true_lambda_a_rew,
                  y = infer_lambda_a_rew)) +
  geom_point(color = "cornflowerblue", size = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  geom_smooth(method = "lm", se = TRUE, formula = "y ~ x", color = "cornflowerblue") +
  theme_minimal() +
  xlab("True") + 
  ylab("Inferred") +
  ggtitle(expression(paste(lambda, " - ", alpha["rew "]))) +
  theme(plot.title = element_text(hjust = 0.5, size = 16)) 

pl7 <- ggplot(older_data, 
              aes(x = true_lambda_a_pun,
                  y = infer_lambda_a_pun)) +
  geom_point(color = "cornflowerblue", size = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  geom_smooth(method = "lm", se = TRUE, formula = "y ~ x", color = "cornflowerblue") +
  theme_minimal() +
  xlab("True") + 
  ylab("Inferred") +
  ggtitle(expression(paste(lambda, " - ", alpha["pun "]))) +
  theme(plot.title = element_text(hjust = 0.5, size = 16)) 

pl8 <- ggplot(older_data, 
              aes(x = true_lambda_K,
                  y = infer_lambda_K)) +
  geom_point(color = "cornflowerblue", size = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  geom_smooth(method = "lm", se = TRUE, formula = "y ~ x", color = "cornflowerblue") +
  theme_minimal() +
  xlab("True") + 
  ylab("Inferred") +
  ggtitle(expression(paste(lambda, " - K"))) +
  theme(plot.title = element_text(hjust = 0.5, size = 16)) 

pl9 <- ggplot(older_data, 
              aes(x = true_lambda_omega_f,
                  y = infer_lambda_omega_f)) +
  geom_point(color = "cornflowerblue", size = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  geom_smooth(method = "lm", se = TRUE, formula = "y ~ x", color = "cornflowerblue") +
  theme_minimal() +
  xlab("True") + 
  ylab("Inferred") +
  ggtitle(expression(paste(lambda, " - ", omega["f "]))) +
  theme(plot.title = element_text(hjust = 0.5, size = 16))

pl10 <- ggplot(older_data, 
               aes(x = true_lambda_omega_p,
                   y = infer_lambda_omega_p)) +
  geom_point(color = "cornflowerblue", size = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  geom_smooth(method = "lm", se = TRUE, formula = "y ~ x", color = "cornflowerblue") +
  theme_minimal() +
  xlab("True") + 
  ylab("Inferred") +
  ggtitle(expression(paste(lambda, " - ", omega["p "]))) +
  theme(plot.title = element_text(hjust = 0.5, size = 16))

pl_old_2 <- grid.arrange(pl6, pl7, pl8, pl9, pl10, nrow = 2, ncol = 3)
ggsave("plots_recovery/pl_old_recovery_lambda.png", pl_old_2)

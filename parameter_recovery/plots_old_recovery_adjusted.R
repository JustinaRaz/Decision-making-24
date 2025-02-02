install.packages("pacman")
install.packages("showtext")
pacman::p_load(tidyverse, gridExtra, grid, showtext)
setwd("/work/JustinaRazanauskaite#2891/DM/parameter_recovery/")

# Load all data files (ran 10 trials x 10 times in tmux; the 10th time got halted)

load("JAGS_data/JAGS_hier_ORL_recovery_old_adjusted1289.RData")
samples$BUGSoutput$summary

d1 <- data.frame(infer_lambda_a_pun, infer_lambda_a_rew, infer_lambda_K, infer_lambda_omega_f,
                 infer_lambda_omega_p, infer_mu_a_pun, infer_mu_a_rew, infer_mu_K,
                 infer_mu_omega_f, infer_mu_omega_p, true_lambda_a_pun, true_lambda_a_rew,
                 true_lambda_K, true_lambda_omega_f, true_lambda_omega_p, true_mu_a_pun,
                 true_mu_a_rew, true_mu_K, true_mu_omega_f, true_mu_omega_p)
samples1 <- samples

load("JAGS_data/JAGS_hier_ORL_recovery_old_adjusted1910.RData")
samples$BUGSoutput$summary

d2 <- data.frame(infer_lambda_a_pun, infer_lambda_a_rew, infer_lambda_K, infer_lambda_omega_f,
                 infer_lambda_omega_p, infer_mu_a_pun, infer_mu_a_rew, infer_mu_K,
                 infer_mu_omega_f, infer_mu_omega_p, true_lambda_a_pun, true_lambda_a_rew,
                 true_lambda_K, true_lambda_omega_f, true_lambda_omega_p, true_mu_a_pun,
                 true_mu_a_rew, true_mu_K, true_mu_omega_f, true_mu_omega_p)
samples2 <- samples

load("JAGS_data/JAGS_hier_ORL_recovery_old_adjusted2245.RData")
samples$BUGSoutput$summary

d3 <- data.frame(infer_lambda_a_pun, infer_lambda_a_rew, infer_lambda_K, infer_lambda_omega_f,
                 infer_lambda_omega_p, infer_mu_a_pun, infer_mu_a_rew, infer_mu_K,
                 infer_mu_omega_f, infer_mu_omega_p, true_lambda_a_pun, true_lambda_a_rew,
                 true_lambda_K, true_lambda_omega_f, true_lambda_omega_p, true_mu_a_pun,
                 true_mu_a_rew, true_mu_K, true_mu_omega_f, true_mu_omega_p)
samples3 <- samples

load("JAGS_data/JAGS_hier_ORL_recovery_old_adjusted2732.RData")
samples$BUGSoutput$summary

d4 <- data.frame(infer_lambda_a_pun, infer_lambda_a_rew, infer_lambda_K, infer_lambda_omega_f,
                 infer_lambda_omega_p, infer_mu_a_pun, infer_mu_a_rew, infer_mu_K,
                 infer_mu_omega_f, infer_mu_omega_p, true_lambda_a_pun, true_lambda_a_rew,
                 true_lambda_K, true_lambda_omega_f, true_lambda_omega_p, true_mu_a_pun,
                 true_mu_a_rew, true_mu_K, true_mu_omega_f, true_mu_omega_p)
samples4 <- samples

load("JAGS_data/JAGS_hier_ORL_recovery_old_adjusted3697.RData")
samples$BUGSoutput$summary

d5 <- data.frame(infer_lambda_a_pun, infer_lambda_a_rew, infer_lambda_K, infer_lambda_omega_f,
                 infer_lambda_omega_p, infer_mu_a_pun, infer_mu_a_rew, infer_mu_K,
                 infer_mu_omega_f, infer_mu_omega_p, true_lambda_a_pun, true_lambda_a_rew,
                 true_lambda_K, true_lambda_omega_f, true_lambda_omega_p, true_mu_a_pun,
                 true_mu_a_rew, true_mu_K, true_mu_omega_f, true_mu_omega_p)
samples5 <- samples

load("JAGS_data/JAGS_hier_ORL_recovery_old_adjusted4188.RData")
samples$BUGSoutput$summary

d6 <- data.frame(infer_lambda_a_pun, infer_lambda_a_rew, infer_lambda_K, infer_lambda_omega_f,
                 infer_lambda_omega_p, infer_mu_a_pun, infer_mu_a_rew, infer_mu_K,
                 infer_mu_omega_f, infer_mu_omega_p, true_lambda_a_pun, true_lambda_a_rew,
                 true_lambda_K, true_lambda_omega_f, true_lambda_omega_p, true_mu_a_pun,
                 true_mu_a_rew, true_mu_K, true_mu_omega_f, true_mu_omega_p)
samples6 <- samples

load("JAGS_data/JAGS_hier_ORL_recovery_old_adjusted4212.RData")
samples$BUGSoutput$summary

d7 <- data.frame(infer_lambda_a_pun, infer_lambda_a_rew, infer_lambda_K, infer_lambda_omega_f,
                 infer_lambda_omega_p, infer_mu_a_pun, infer_mu_a_rew, infer_mu_K,
                 infer_mu_omega_f, infer_mu_omega_p, true_lambda_a_pun, true_lambda_a_rew,
                 true_lambda_K, true_lambda_omega_f, true_lambda_omega_p, true_mu_a_pun,
                 true_mu_a_rew, true_mu_K, true_mu_omega_f, true_mu_omega_p)
samples7 <- samples

load("JAGS_data/JAGS_hier_ORL_recovery_old_adjusted5415.RData")
samples$BUGSoutput$summary

d8 <- data.frame(infer_lambda_a_pun, infer_lambda_a_rew, infer_lambda_K, infer_lambda_omega_f,
                 infer_lambda_omega_p, infer_mu_a_pun, infer_mu_a_rew, infer_mu_K,
                 infer_mu_omega_f, infer_mu_omega_p, true_lambda_a_pun, true_lambda_a_rew,
                 true_lambda_K, true_lambda_omega_f, true_lambda_omega_p, true_mu_a_pun,
                 true_mu_a_rew, true_mu_K, true_mu_omega_f, true_mu_omega_p)
samples8 <- samples

load("JAGS_data/JAGS_hier_ORL_recovery_old_adjusted8466.RData")
samples$BUGSoutput$summary

d9 <- data.frame(infer_lambda_a_pun, infer_lambda_a_rew, infer_lambda_K, infer_lambda_omega_f,
                 infer_lambda_omega_p, infer_mu_a_pun, infer_mu_a_rew, infer_mu_K,
                 infer_mu_omega_f, infer_mu_omega_p, true_lambda_a_pun, true_lambda_a_rew,
                 true_lambda_K, true_lambda_omega_f, true_lambda_omega_p, true_mu_a_pun,
                 true_mu_a_rew, true_mu_K, true_mu_omega_f, true_mu_omega_p)
samples9 <- samples


old_recovery <- rbind(d1, d2, d3, d4, d5, d6, d7, d8, d9)

# PLOTS

font_add_google("EB Garamond", "ebgaramond")
showtext_auto()

pl1 <- ggplot(old_recovery, 
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

pl2 <- ggplot(old_recovery, 
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

pl3 <- ggplot(old_recovery, 
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

pl4 <- ggplot(old_recovery, 
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

pl5 <- ggplot(old_recovery, 
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
ggsave("plots_recovery/pl_old_recovery_mu_adjusted.png", pl_old_1)


pl6 <- ggplot(old_recovery, 
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

pl7 <- ggplot(old_recovery, 
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

pl8 <- ggplot(old_recovery, 
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

pl9 <- ggplot(old_recovery, 
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

pl10 <- ggplot(old_recovery, 
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
ggsave("plots_recovery/pl_old_recovery_lambda_adjusted.png", pl_old_2)

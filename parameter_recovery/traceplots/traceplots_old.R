install.packages("pacman")
install.packages("showtext")
pacman::p_load(parallel, ggpubr, extraDistr, truncnorm, ggplot2, showtext, gridExtra)

font_add_google("EB Garamond", "ebgaramond")
showtext_auto()

setwd("/work/JustinaRazanauskaite#2891/DM/parameter_recovery/traceplots")
load("../JAGS_data/JAGS_hier_ORL_recovery_no_theta_old.RData")

# Investigating the dimensions:
dim(samples$BUGSoutput$sims.array) 

# Parameter names:
names(samples$BUGSoutput$sims.list)
#[1] "deviance"       "lambda_K"       "lambda_a_pun"   "lambda_a_rew"   "lambda_omega_f" "lambda_omega_p" "mu_K"           "mu_a_pun"       "mu_a_rew"       "mu_omega_f"    
#[11] "mu_omega_p" 

# Extracting samples for each parameter separately:

chains_mu_a_rew <- samples$BUGSoutput$sims.array[, , "mu_a_rew"]
chains_mu_a_pun <- samples$BUGSoutput$sims.array[, , "mu_a_pun"]
chains_mu_omega_f <- samples$BUGSoutput$sims.array[, , "mu_omega_f"]
chains_mu_omega_p <- samples$BUGSoutput$sims.array[, , "mu_omega_p"]
chains_mu_K <- samples$BUGSoutput$sims.array[, , "mu_K"]

chains_lambda_a_rew <- samples$BUGSoutput$sims.array[, , "lambda_a_rew"]
chains_lambda_a_pun <- samples$BUGSoutput$sims.array[, , "lambda_a_pun"]
chains_lambda_omega_f <- samples$BUGSoutput$sims.array[, , "lambda_omega_f"]
chains_lambda_omega_p <- samples$BUGSoutput$sims.array[, , "lambda_omega_p"]
chains_lambda_K <- samples$BUGSoutput$sims.array[, , "lambda_K"]

# Converting matrices to data frames for plotting:

chains_mu_a_rew <- data.frame(
  Iteration = rep(1:nrow(chains_mu_a_rew), times = ncol(chains_mu_a_rew)),
  Chain = factor(rep(1:ncol(chains_mu_a_rew), each = nrow(chains_mu_a_rew))),
  Value = as.vector(chains_mu_a_rew)
)

chains_mu_a_pun <- data.frame(
  Iteration = rep(1:nrow(chains_mu_a_pun), times = ncol(chains_mu_a_pun)),
  Chain = factor(rep(1:ncol(chains_mu_a_pun), each = nrow(chains_mu_a_pun))),
  Value = as.vector(chains_mu_a_pun)
)

chains_mu_omega_f <- data.frame(
  Iteration = rep(1:nrow(chains_mu_omega_f), times = ncol(chains_mu_omega_f)),
  Chain = factor(rep(1:ncol(chains_mu_omega_f), each = nrow(chains_mu_omega_f))),
  Value = as.vector(chains_mu_omega_f)
)

chains_mu_omega_p <- data.frame(
  Iteration = rep(1:nrow(chains_mu_omega_p), times = ncol(chains_mu_omega_p)),
  Chain = factor(rep(1:ncol(chains_mu_omega_p), each = nrow(chains_mu_omega_p))),
  Value = as.vector(chains_mu_omega_p)
)

chains_mu_K <- data.frame(
  Iteration = rep(1:nrow(chains_mu_K), times = ncol(chains_mu_K)),
  Chain = factor(rep(1:ncol(chains_mu_K), each = nrow(chains_mu_K))),
  Value = as.vector(chains_mu_K)
)

chains_lambda_a_rew <- data.frame(
  Iteration = rep(1:nrow(chains_lambda_a_rew), times = ncol(chains_lambda_a_rew)),
  Chain = factor(rep(1:ncol(chains_lambda_a_rew), each = nrow(chains_lambda_a_rew))),
  Value = as.vector(chains_lambda_a_rew)
)

chains_lambda_a_pun <- data.frame(
  Iteration = rep(1:nrow(chains_lambda_a_pun), times = ncol(chains_lambda_a_pun)),
  Chain = factor(rep(1:ncol(chains_lambda_a_pun), each = nrow(chains_lambda_a_pun))),
  Value = as.vector(chains_lambda_a_pun)
)

chains_lambda_omega_f <- data.frame(
  Iteration = rep(1:nrow(chains_lambda_omega_f), times = ncol(chains_lambda_omega_f)),
  Chain = factor(rep(1:ncol(chains_lambda_omega_f), each = nrow(chains_lambda_omega_f))),
  Value = as.vector(chains_lambda_omega_f)
)

chains_lambda_omega_p <- data.frame(
  Iteration = rep(1:nrow(chains_lambda_omega_p), times = ncol(chains_lambda_omega_p)),
  Chain = factor(rep(1:ncol(chains_lambda_omega_p), each = nrow(chains_lambda_omega_p))),
  Value = as.vector(chains_lambda_omega_p)
)

chains_lambda_K <- data.frame(
  Iteration = rep(1:nrow(chains_lambda_K), times = ncol(chains_lambda_K)),
  Chain = factor(rep(1:ncol(chains_lambda_K), each = nrow(chains_lambda_K))),
  Value = as.vector(chains_lambda_K)
)

# Plotting:

pl1 <- ggplot(chains_mu_a_rew, aes(x = Iteration, y = Value, color = Chain)) +
  geom_line() +
  labs(
    title = expression(~ mu ~ alpha["rew"]),
    x = "Iteration",
    y = expression(~ mu ~ alpha["rew"]),
    color = "Chain"
  ) +
  scale_color_manual(
    values = c("1" = "darkred", "2" = "darkgoldenrod1", "3" = "#3399FF")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 30),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15)
  )

pl2 <- ggplot(chains_mu_a_pun, aes(x = Iteration, y = Value, color = Chain)) +
  geom_line() +
  labs(
    title = expression(~ mu ~ alpha["pun"]),
    x = "Iteration",
    y = expression(~ mu ~ alpha["pun"]),
    color = "Chain"
  ) +
  scale_color_manual(
    values = c("1" = "darkred", "2" = "darkgoldenrod1", "3" = "#3399FF")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 30),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15)
  )

pl3 <- ggplot(chains_mu_omega_f, aes(x = Iteration, y = Value, color = Chain)) +
  geom_line() +
  labs(
    title = expression(~ mu ~ omega["f"]),
    x = "Iteration",
    y = expression(~ mu ~ omega["f"]),
    color = "Chain"
  ) +
  scale_color_manual(
    values = c("1" = "darkred", "2" = "darkgoldenrod1", "3" = "#3399FF")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 30),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15)
  )

pl4 <- ggplot(chains_mu_omega_p, aes(x = Iteration, y = Value, color = Chain)) +
  geom_line() +
  labs(
    title = expression(~ mu ~ omega["p"]),
    x = "Iteration",
    y = expression(~ mu ~ omega["p"]),
    color = "Chain"
  ) +
  scale_color_manual(
    values = c("1" = "darkred", "2" = "darkgoldenrod1", "3" = "#3399FF")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 30),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15)
  )

pl5 <- ggplot(chains_mu_K, aes(x = Iteration, y = Value, color = Chain)) +
  geom_line() +
  labs(
    title = expression(~ mu ~ K),
    x = "Iteration",
    y = expression(~ mu ~ K),
    color = "Chain"
  ) +
  scale_color_manual(
    values = c("1" = "darkred", "2" = "darkgoldenrod1", "3" = "#3399FF")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 30),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15)
  )

pl6 <- ggplot(chains_lambda_a_rew, aes(x = Iteration, y = Value, color = Chain)) +
  geom_line() +
  labs(
    title = expression(~ lambda ~ alpha["rew"]),
    x = "Iteration",
    y = expression(~ lambda ~ alpha["rew"]),
    color = "Chain"
  ) +
  scale_color_manual(
    values = c("1" = "darkred", "2" = "darkgoldenrod1", "3" = "#3399FF")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 30),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15)
  )

pl7 <- ggplot(chains_lambda_a_pun, aes(x = Iteration, y = Value, color = Chain)) +
  geom_line() +
  labs(
    title = expression(~ lambda ~ alpha["pun"]),
    x = "Iteration",
    y = expression(~ lambda ~ alpha["pun"]),
    color = "Chain"
  ) +
  scale_color_manual(
    values = c("1" = "darkred", "2" = "darkgoldenrod1", "3" = "#3399FF")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 30),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15)
  )

pl8 <- ggplot(chains_lambda_omega_f, aes(x = Iteration, y = Value, color = Chain)) +
  geom_line() +
  labs(
    title = expression(~ lambda ~ omega["f"]),
    x = "Iteration",
    y = expression(~ lambda ~ omega["f"]),
    color = "Chain"
  ) +
  scale_color_manual(
    values = c("1" = "darkred", "2" = "darkgoldenrod1", "3" = "#3399FF")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 30),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15)
  )

pl9 <- ggplot(chains_lambda_omega_p, aes(x = Iteration, y = Value, color = Chain)) +
  geom_line() +
  labs(
    title = expression(~ lambda ~ omega["p"]),
    x = "Iteration",
    y = expression(~ lambda ~ omega["p"]),
    color = "Chain"
  ) +
  scale_color_manual(
    values = c("1" = "darkred", "2" = "darkgoldenrod1", "3" = "#3399FF")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 30),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15)
  )

pl10 <- ggplot(chains_lambda_K, aes(x = Iteration, y = Value, color = Chain)) +
  geom_line() +
  labs(
    title = expression(~ lambda ~ K),
    x = "Iteration",
    y = expression(~ lambda ~ K),
    color = "Chain"
  ) +
  scale_color_manual(
    values = c("1" = "darkred", "2" = "darkgoldenrod1", "3" = "#3399FF")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 30),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15)
  )

plot_1 <- ggarrange(pl1, pl2, pl3, pl4, pl5, ncol = 1, nrow = 5)

ggsave("traceplots_old_mu.png", plot_1, width = 10, height = 20)

plot_2 <- ggarrange(pl6, pl7, pl8, pl9, pl10, ncol = 1, nrow = 5)

ggsave("traceplots_old_lambda.png", plot_2, width = 10, height = 20)

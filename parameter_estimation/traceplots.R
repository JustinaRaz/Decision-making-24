setwd("/work/JustinaRazanauskaite#2891/DM/parameter_estimation")
install.packages("pacman")
install.packages("showtext")
install.packages("ggpattern")

pacman::p_load(ggpubr, extraDistr, truncnorm, ggplot2, showtext, gridExtra, ggpattern)

font_add_google("EB Garamond", "ebgaramond")
showtext_auto()

#load("output_data/hier_est_old_all_subs.RData")
load("output_data/hier_est_old_all_subs_adjusted.RData")
samples$BUGSoutput$summary

names(samples$BUGSoutput$sims.list)
# "deviance"   "mu_K"       "mu_a_pun"   "mu_a_rew"   "mu_omega_f" "mu_omega_p"

par(mfrow=c(3,2))
plot(density(samples$BUGSoutput$sims.list$mu_a_rew))
plot(density(samples$BUGSoutput$sims.list$mu_a_pun))
plot(density(samples$BUGSoutput$sims.list$mu_K))
plot(density(samples$BUGSoutput$sims.list$mu_omega_f))
plot(density(samples$BUGSoutput$sims.list$mu_omega_p))

# Extracting samples for each parameter separately:

chains_mu_a_rew <- samples$BUGSoutput$sims.array[, , "mu_a_rew"]
chains_mu_a_pun <- samples$BUGSoutput$sims.array[, , "mu_a_pun"]
chains_mu_omega_f <- samples$BUGSoutput$sims.array[, , "mu_omega_f"]
chains_mu_omega_p <- samples$BUGSoutput$sims.array[, , "mu_omega_p"]
chains_mu_K <- samples$BUGSoutput$sims.array[, , "mu_K"]

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
    values = c("1" = "darkred", "2" = "darkgoldenrod1", "3" = "#3399FF", "4" = "darkolivegreen3")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 50),
    axis.title.x = element_text(size = 40),
    axis.title.y = element_text(size = 40),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30)
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
    values = c("1" = "darkred", "2" = "darkgoldenrod1", "3" = "#3399FF", "4" = "darkolivegreen3")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 50),
    axis.title.x = element_text(size = 40),
    axis.title.y = element_text(size = 40),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30)
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
    values = c("1" = "darkred", "2" = "darkgoldenrod1", "3" = "#3399FF", "4" = "darkolivegreen3")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 50),
    axis.title.x = element_text(size = 40),
    axis.title.y = element_text(size = 40),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30)
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
    values = c("1" = "darkred", "2" = "darkgoldenrod1", "3" = "#3399FF", "4" = "darkolivegreen3")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 50),
    axis.title.x = element_text(size = 40),
    axis.title.y = element_text(size = 40),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30)
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
    values = c("1" = "darkred", "2" = "darkgoldenrod1", "3" = "#3399FF", "4" = "darkolivegreen3")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 50),
    axis.title.x = element_text(size = 40),
    axis.title.y = element_text(size = 40),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30)
  )

ggsave("plots/a_rew_old_adjusted.png", pl1)
ggsave("plots/a_pun_old_adjusted.png", pl2)
ggsave("plots/omega_f_old_adjusted.png", pl3)
ggsave("plots/omega_p_old_adjusted.png", pl4)
ggsave("plots/K_old_adjusted.png", pl5)

#load("output_data/hier_est_young_all_subs.RData")
load("output_data/hier_est_young_all_subs_adjusted.RData")
samples$BUGSoutput$summary

par(mfrow=c(3,2))
plot(density(samples$BUGSoutput$sims.list$mu_a_rew))
plot(density(samples$BUGSoutput$sims.list$mu_a_pun))
plot(density(samples$BUGSoutput$sims.list$mu_K))
plot(density(samples$BUGSoutput$sims.list$mu_omega_f))
plot(density(samples$BUGSoutput$sims.list$mu_omega_p))

# Extracting samples for each parameter separately:

chains_mu_a_rew <- samples$BUGSoutput$sims.array[, , "mu_a_rew"]
chains_mu_a_pun <- samples$BUGSoutput$sims.array[, , "mu_a_pun"]
chains_mu_omega_f <- samples$BUGSoutput$sims.array[, , "mu_omega_f"]
chains_mu_omega_p <- samples$BUGSoutput$sims.array[, , "mu_omega_p"]
chains_mu_K <- samples$BUGSoutput$sims.array[, , "mu_K"]

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

# Plotting:

pl6 <- ggplot(chains_mu_a_rew, aes(x = Iteration, y = Value, color = Chain)) +
  geom_line() +
  labs(
    title = expression(~ mu ~ alpha["rew"]),
    x = "Iteration",
    y = expression(~ mu ~ alpha["rew"]),
    color = "Chain"
  ) +
  scale_color_manual(
    values = c("1" = "darkred", "2" = "darkgoldenrod1", "3" = "#3399FF", "4" = "darkolivegreen3")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 50),
    axis.title.x = element_text(size = 40),
    axis.title.y = element_text(size = 40),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30)
  )

pl7 <- ggplot(chains_mu_a_pun, aes(x = Iteration, y = Value, color = Chain)) +
  geom_line() +
  labs(
    title = expression(~ mu ~ alpha["pun"]),
    x = "Iteration",
    y = expression(~ mu ~ alpha["pun"]),
    color = "Chain"
  ) +
  scale_color_manual(
    values = c("1" = "darkred", "2" = "darkgoldenrod1", "3" = "#3399FF", "4" = "darkolivegreen3")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 50),
    axis.title.x = element_text(size = 40),
    axis.title.y = element_text(size = 40),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30)
  )

pl8 <- ggplot(chains_mu_omega_f, aes(x = Iteration, y = Value, color = Chain)) +
  geom_line() +
  labs(
    title = expression(~ mu ~ omega["f"]),
    x = "Iteration",
    y = expression(~ mu ~ omega["f"]),
    color = "Chain"
  ) +
  scale_color_manual(
    values = c("1" = "darkred", "2" = "darkgoldenrod1", "3" = "#3399FF", "4" = "darkolivegreen3")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 50),
    axis.title.x = element_text(size = 40),
    axis.title.y = element_text(size = 40),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30)
  )

pl9 <- ggplot(chains_mu_omega_p, aes(x = Iteration, y = Value, color = Chain)) +
  geom_line() +
  labs(
    title = expression(~ mu ~ omega["p"]),
    x = "Iteration",
    y = expression(~ mu ~ omega["p"]),
    color = "Chain"
  ) +
  scale_color_manual(
    values = c("1" = "darkred", "2" = "darkgoldenrod1", "3" = "#3399FF", "4" = "darkolivegreen3")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 50),
    axis.title.x = element_text(size = 40),
    axis.title.y = element_text(size = 40),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30)
  )

pl10 <- ggplot(chains_mu_K, aes(x = Iteration, y = Value, color = Chain)) +
  geom_line() +
  labs(
    title = expression(~ mu ~ K),
    x = "Iteration",
    y = expression(~ mu ~ K),
    color = "Chain"
  ) +
  scale_color_manual(
    values = c("1" = "darkred", "2" = "darkgoldenrod1", "3" = "#3399FF", "4" = "darkolivegreen3")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 50),
    axis.title.x = element_text(size = 40),
    axis.title.y = element_text(size = 40),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30)
  )

ggsave("plots/a_rew_young_adjusted.png", pl6)
ggsave("plots/a_pun_young_adjusted.png", pl7)
ggsave("plots/omega_f_young_adjusted.png", pl8)
ggsave("plots/omega_p_young_adjusted.png", pl9)
ggsave("plots/K_young_adjusted.png", pl10)

# Plotting posterior densities with Younger and Older:

library(tidyr)
library(dplyr)

#load("output_data/hier_est_old_all_subs.RData")
#samples_old <- as.data.frame(samples$BUGSoutput$sims.list)

#load("output_data/hier_est_young_all_subs.RData")
#samples_young <- as.data.frame(samples$BUGSoutput$sims.list)

load("output_data/hier_est_old_all_subs_adjusted.RData")
samples_old <- as.data.frame(samples$BUGSoutput$sims.list)

load("output_data/hier_est_young_all_subs_adjusted.RData")
samples_young <- as.data.frame(samples$BUGSoutput$sims.list)

samples_long_old <- samples_old %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")
samples_long_old <- samples_long_old %>%
  filter(Variable != "deviance")

samples_long_young <- samples_young %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")
samples_long_young <- samples_long_young %>%
  filter(Variable != "deviance")

samples_long_old$Group <- "Old"
samples_long_young$Group <- "Young"

estimates <- rbind(samples_long_old, samples_long_young)

library(dplyr)
library(ggplot2)

# Calculate 95% CI and density heights at the CI boundaries
ci_data <- estimates %>%
  group_by(Variable, Group) %>%
  summarize(
    CI_low = quantile(Value, 0.025),
    CI_high = quantile(Value, 0.975),
    Density_low = density(Value)$y[which.min(abs(density(Value)$x - quantile(Value, 0.025)))],
    Density_high = density(Value)$y[which.min(abs(density(Value)$x - quantile(Value, 0.975)))]
  )

ggplot(estimates, aes(x = Value, fill = Group, color = Group)) +
  geom_density(alpha = 0.4) +
  facet_wrap(
    ~ Variable, 
    scales = "free", 
    ncol = 2
  ) +
  geom_segment(
    data = ci_data,
    aes(x = CI_low, xend = CI_low, y = 0, yend = Density_low, color = Group),
    linetype = "dashed", size = 0.5, inherit.aes = FALSE
  ) +
  geom_segment(
    data = ci_data,
    aes(x = CI_high, xend = CI_high, y = 0, yend = Density_high, color = Group),
    linetype = "dashed", size = 0.5, inherit.aes = FALSE
  ) +
  theme_minimal() +
  labs(
    y = "Density"
  ) +
  scale_fill_manual(values = c("cornflowerblue", "chocolate3")) +
  scale_color_manual(values = c("cornflowerblue", "chocolate3")) + 
  theme(
    panel.grid = element_blank(),     
    panel.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
    legend.title = element_blank(),
    axis.title.x = element_blank()
  )


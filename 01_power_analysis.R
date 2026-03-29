library(ggplot2)

calc_power_two_sample <- function(n, delta, sd) {
  power.t.test(n = n,
               delta = delta,
               sd = sd,
               sig.level = 0.05,
               type = "two.sample",
               alternative = "one.sided")$power
}

calc_power_paired <- function(n, delta, sd) {
  power.t.test(n = n,
               delta = delta,
               sd = sd,
               sig.level = 0.05,
               type = "paired",
               alternative = "one.sided")$power
}

n_seq <- seq(2, 150, by = 1)

# --- TUG ---
power_huang  <- sapply(n_seq, calc_power_two_sample,
                       delta = 2.36,  sd = 2.9)

power_nocera <- sapply(n_seq, calc_power_two_sample,
                       delta = 2.758, sd = 6.74)

# --- PDQ-39 ---
power_pdq <- sapply(n_seq, calc_power_paired,
                    delta = 4.72, sd = 12.42)

df_all <- rbind(
  data.frame(n = n_seq, power = power_huang,
             line = "Huang et al. (TUG)", panel = "TUG"),
  data.frame(n = n_seq, power = power_nocera,
             line = "Nocera et al. (TUG)", panel = "TUG"),
  data.frame(n = n_seq, power = power_pdq,
             line = "Horváth et al. (PDQ-39)", panel = "PDQ-39")
)

df_all$panel <- factor(df_all$panel,
                       levels = c("TUG", "PDQ-39"))

# --- Plot ---
p <- ggplot(df_all, aes(x = n, y = power, color = line)) +
  
  geom_line(linewidth = 1.2) +
  
  geom_hline(yintercept = 0.8,
             linetype = "dashed",
             linewidth = 0.8) +
  
  geom_vline(data = subset(df_all, panel == "TUG"),
             xintercept = c(20, 75),
             linetype = "dotted",
             linewidth = 0.8) +
  
  geom_vline(data = subset(df_all, panel == "PDQ-39"),
             xintercept = 45,
             linetype = "dotted",
             linewidth = 0.8) +
  
  facet_wrap(~panel, ncol = 1) +
  
  scale_x_continuous(
    limits = c(0, 150),
    breaks = seq(0, 150, by = 25)
  ) +
  
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.1)
  ) +
  
  labs(
    x = "Stichprobengröße (n)",
    y = "Power",
    color = "Datengrundlage"
  ) +
  
  theme_minimal(base_size = 12) +
  
  theme(
    axis.title = element_text(size = 15),
    axis.text  = element_text(size = 13),
    
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 13),
    
    strip.text = element_text(size = 14, face = "bold"),
    
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.4)
  )

print(p)
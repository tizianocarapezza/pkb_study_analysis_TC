par(mai = c(1, 1, 1, 1))

########################################Scatterplot TUG#########################################
op <- par(no.readonly = TRUE)
par(pty = "s", xaxs = "i", yaxs = "i")

plot(tug_scores$tug_pre, tug_scores$tug_post, 
     xlab = "TUG Prä [s]", ylab = "TUG Post [s]",
     asp = 1,
     xlim = c(0, 30), ylim = c(0, 30),
     pch = 18, cex = 1.8, cex.lab = 1.6, cex.axis = 1.3,
     col = ifelse(tug_scores$diff > 0, "green2", "red2"))


cuts <- seq(5, 30, by = 5)

for (c in cuts) {
  segments(x0 = c, y0 = 0, x1 = c, y1 = c,
           lty = 3, col = "grey60")
  
  segments(x0 = 0, y0 = c, x1 = c, y1 = c,
           lty = 3, col = "grey60")
}


abline(a = 0, b = 1, lwd = 2)

legend("bottomright", legend = c("Verschlechterung", "Verbesserung"),
       col = c("red2", "green2"), pch = 18, cex = 1, title = "Prä/Post Differenz")

##########################################Scatterplot UPDRS####################################
plot(updrs_scores$updrs_pre, updrs_scores$updrs_post, 
     xlab = "UPDRS III Prä", ylab = "UPDRS III Post",
     asp=1, 
     xlim = c(0, 70), ylim = c(0, 70),
     pch = 18, cex=1.8, cex.lab=1.6, cex.axis=1.3,
     col = ifelse(updrs_scores$diff > 0, "green2", "red2")) 

cuts <- seq(10, 70, by = 10)

for (c in cuts) {
  segments(x0 = c, y0 = 0, x1 = c, y1 = c,
           lty = 3, col = "grey60")
  
  segments(x0 = 0, y0 = c, x1 = c, y1 = c,
           lty = 3, col = "grey60")
}


abline(a = 0, b = 1, lwd = 2)

legend("bottomright", legend = c("Verbesserung", "Verschlechterung"),
       col = c("green2", "red2"), pch = 18, cex = 1, title = "Prä/Post Differenz")

#########################################Scatterplot PDQ39######################################

plot(pdq39_scores$PDQ39SI_pre, pdq39_scores$PDQ39SI_post, 
     xlab = "PDQ-39-SI Prä", ylab = "PDQ-39-SI Post",
     asp=1, 
     xlim = c(0, 70), ylim = c(0, 70),
     pch = 18, cex=1.8,  cex.lab=1.6, cex.axis=1.3,
     col = ifelse(pdq39_scores$PDQ39SI_diff > 0, "green2", "red2")) 

cuts <- seq(10, 70, by = 10)

for (c in cuts) {
  segments(x0 = c, y0 = 0, x1 = c, y1 = c,
           lty = 3, col = "grey60")
  
  segments(x0 = 0, y0 = c, x1 = c, y1 = c,
           lty = 3, col = "grey60")
}


abline(a = 0, b = 1, lwd = 2)


legend("bottomright", legend = c("Verbesserung", "Verschlechterung"),
       col = c("green2", "red2"), pch = 18, cex = 1, title = "Prä/Post Differenz")

######################################Scatterplot LED#################################################

plot(LED_scores$LED_first, LED_scores$LED_last, 
     xlab = "LED Prä [mg]", ylab = "LED Post [mg]",
     asp=1, 
     xlim = c(0, 3600), ylim = c(0, 3600),
     pch = 18, cex=1.8, cex.lab=1.6, cex.axis=1.3,
     col = ifelse(LED_scores$diff > 0, "green2", "red2")) 

cuts <- seq(500, 3500, by = 500)

for (c in cuts) {
  segments(x0 = c, y0 = 0, x1 = c, y1 = c,
           lty = 3, col = "grey60")
  
  segments(x0 = 0, y0 = c, x1 = c, y1 = c,
           lty = 3, col = "grey60")
}


abline(a = 0, b = 1, lwd = 2)

legend("bottomright", legend = c("LED Erniedrigung", "LED Erhöhung"),
       col = c("green2", "red2"), pch = 18, cex = 1, title = "Prä/Post Differenz")

###################grouped Barchart PKB-E################
library(dplyr)
library(tidyr)
library(ggplot2)

questionnaire_long <- pivot_longer(
  questionnaire_scores,
  cols = everything(),
  names_to = "Item",
  values_to = "Wert"
)

freq <- questionnaire_long %>%
  filter(!is.na(Wert)) %>%
  mutate(
    Wert = factor(
      Wert,
      levels = c(0, 1, 2, 3, 4),
      labels = c("trifft nicht zu", "trifft eher nicht zu", "trifft teilweise zu", "trifft eher zu", "trifft zu")
    )
  ) %>%
  count(Item, Wert)

ggplot(freq, aes(x = Item, y = n, fill = Wert)) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7,
    color = "black"
  ) +
  scale_fill_manual(
    values = c(
      "trifft zu"  = "snow1",
      "trifft eher zu"       = "snow2",
      "trifft teilweise zu"    = "snow3",
      "trifft eher nicht zu"  = "snow4",
      "trifft nicht zu" = "gray1"
    ),
    breaks = c("trifft zu", "trifft eher zu", "trifft teilweise zu", "trifft eher nicht zu", "trifft nicht zu")
  ) +
  labs(
    title = "PKB-E Ergebnisse",
    x = "",
    y = "Anzahl",
    fill = "Verbesserung nach PKB"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 18),
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_text(size = 13),
    legend.text  = element_text(size = 11)
  )
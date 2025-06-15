# Projeto: REGRESSÃO LINEAR em Estudo de Estabilidade
# Autor: Jean Mendes
# Objetivo: Analisar a tendência de degradação de um fármaco em 12 meses

#install.packages(c("ggplot2", "ggpmisc"))
library(ggplot2)
library(ggpmisc)

# Simulação dos dados
set.seed(42)
meses <- 0:12
concentracao_real <- 100 - 1.5 * meses
ruido <- rnorm(length(meses), mean = 0, sd = 1)
concentracao <- concentracao_real + ruido
dados <- data.frame(Mes = meses, Concentracao = concentracao)

# Modelo de regressão linear
modelo <- lm(Concentracao ~ Mes, data = dados)

# Previsão com intervalo de confiança
pred <- predict(modelo, interval = "confidence", level = 0.95)
dados$Fit <- pred[, "fit"]
dados$Lower <- pred[, "lwr"]
dados$Upper <- pred[, "upr"]

# Cálculo da shelf life (concentração = 90%)
shelf_life <- (90 - coef(modelo)[1]) / coef(modelo)[2]

# Texto da equação + R²
eq <- paste0("y = ", round(coef(modelo)[1], 2), 
             " ", ifelse(coef(modelo)[2] < 0, "- ", "+ "), 
             abs(round(coef(modelo)[2], 2)), "x")
r2 <- summary(modelo)$r.squared
label_eq <- paste(eq, "\nR² = ", round(r2, 3))

# Gráfico com shelf life
grafico <- ggplot(dados, aes(x = Mes, y = Concentracao)) +
  geom_point(color = "#0072B2", size = 3) +
  geom_line(aes(y = Fit), color = "#D55E00", size = 1.2) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.2, fill = "#D55E00") +
  geom_hline(yintercept = 90, linetype = "dashed", color = "gray40") +
  geom_vline(xintercept = shelf_life, linetype = "dotted", color = "gray40") +
  annotate("text", x = shelf_life + 0.5, y = 91.5,
           label = paste0("Shelf life ≈ ", round(shelf_life, 1), " months"),
           hjust = 0, size = 4, fontface = "italic", color = "black") +
  labs(
    title = "Stability Study: Degradation Trend",
    subtitle = "Linear Regression Model with Confidence Interval (95%)",
    x = "Months",
    y = "Concentration (%)"
  ) +
  annotate("text", x = 1, y = 82, label = label_eq, hjust = 0, size = 4.2, color = "black") +
  theme_minimal(base_size = 14)

# Exibir o gráfico
print(grafico)

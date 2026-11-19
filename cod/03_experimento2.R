library(dplyr)
library(ggplot2)
library(wooldridge)
library(pROC)

cat("Carpeta de trabajo:", getwd(), "\n")
dir.create("salidas", showWarnings = FALSE)



data(loanapp)
cat("Solicitudes:", nrow(loanapp), "| Columnas:", ncol(loanapp), "\n")


la <- loanapp |>
  mutate(
    appinc   = ifelse(appinc   == 666,            NA, appinc),
    cototinc = ifelse(cototinc == 666,            NA, cototinc),
    hexp     = ifelse(hexp %in% c(666, 999),      NA, hexp),
    gdlin    = ifelse(gdlin    == 666,            NA, gdlin),
    term     = ifelse(term     >= 999999,         NA, term),
    lines    = ifelse(lines    >= 999999,         NA, lines),
    liq      = ifelse(liq      >= 1e6,            NA, liq)
  )




sensibles <- c("black", "hispan", "male", "married")

nombres_sensibles <- c(black   = "Ser afrodescendiente",
                       hispan  = "Ser hispano",
                       male    = "Ser hombre",
                       married = "Estar casado")

neutras <- c("loanamt",   # monto solicitado
             "appinc",    # ingreso del solicitante
             "atotinc",   # ingreso total mensual
             "cototinc",  # ingreso del co-solicitante
             "hexp",      # gasto en vivienda propuesto
             "price",     # precio de la vivienda
             "liq",       # activos líquidos
             "netw",      # patrimonio neto
             "hrat",      # gasto en vivienda / ingreso
             "obrat",     # otras deudas / ingreso
             "term",      # plazo del préstamo
             "self",      # trabaja por cuenta propia
             "unem",      # desempleo en su industria
             "chist",     # historial de crédito aceptable
             "mortg",     # historial de pagos hipotecarios
             "cons",      # historial de pagos de consumo
             "pubrec",    # quiebra registrada
             "lines",     # líneas de crédito abiertas
             "rep",       # reportes de crédito consultados
             "gdlin")     # historial cumple guías del banco

# Solo filas completas: sin faltantes en ninguna variable usada.
d <- la[complete.cases(la[, c(neutras, sensibles)]), ]
cat("Filas completas:", nrow(d), "de", nrow(la), "\n")




adivinar <- function(objetivo, entrenamiento, prueba) {
  formula <- as.formula(paste(objetivo, "~", paste(neutras, collapse = " + ")))
  modelo  <- glm(formula, data = entrenamiento, family = binomial)
  prob    <- predict(modelo, newdata = prueba, type = "response")
  as.numeric(auc(roc(prueba[[objetivo]], prob, quiet = TRUE)))
}


resultados <- data.frame()

for (v in sensibles) {
  for (s in 1:10) {
    set.seed(s)
    i <- sample(nrow(d), floor(0.7 * nrow(d)))
    resultados <- rbind(resultados,
                        data.frame(variable = v, semilla = s,
                                   auc = adivinar(v, d[i, ], d[-i, ])))
  }
  cat("  listo:", v, "\n")
}

resumen <- resultados |>
  group_by(variable) |>
  summarise(minimo = min(auc),
            maximo = max(auc),
            auc    = mean(auc),
            .groups = "drop") |>
  arrange(desc(auc))


for (k in seq_len(nrow(resumen))) {
  cat(sprintf("  %-8s  AUC %.3f   rango %.3f - %.3f\n",
              resumen$variable[k], resumen$auc[k],
              resumen$minimo[k],   resumen$maximo[k]))
}


write.csv(resumen,    "salidas/exp2_resumen.csv",    row.names = FALSE)
write.csv(resultados, "salidas/exp2_por_semilla.csv", row.names = FALSE)



etiquetas <- c(loanamt = "Monto solicitado",       appinc   = "Ingreso del solicitante",
               atotinc = "Ingreso total mensual",  cototinc = "Ingreso del co-solicitante",
               hexp    = "Gasto en vivienda",      price    = "Precio de la vivienda",
               liq     = "Activos líquidos",       netw     = "Patrimonio neto",
               hrat    = "Vivienda / ingreso",     obrat    = "Otras deudas / ingreso",
               term    = "Plazo",                  self     = "Cuenta propia",
               unem    = "Desempleo de su industria", chist = "Historial aceptable",
               mortg   = "Historial hipotecario",  cons     = "Historial de consumo",
               pubrec  = "Quiebra registrada",     lines    = "Líneas de crédito",
               rep     = "Reportes consultados",   gdlin    = "Cumple guías del banco")

d_escalada <- d
d_escalada[neutras] <- scale(d_escalada[neutras])

modelo_raza <- glm(as.formula(paste("black ~", paste(neutras, collapse = " + "))),
                   data = d_escalada, family = binomial)

delatoras <- as.data.frame(summary(modelo_raza)$coefficients[-1, ])
delatoras$variable <- rownames(delatoras)
delatoras <- delatoras |>
  mutate(peso = abs(`z value`), nombre = etiquetas[variable]) |>
  arrange(desc(peso)) |>
  head(6)

cat("\nLas seis variables financieras que más delatan la raza:\n")
for (k in seq_len(nrow(delatoras))) {
  cat(sprintf("  %d. %-28s z = %.2f\n", k, delatoras$nombre[k], delatoras$peso[k]))
}




resumen_g <- resumen |>
  mutate(nombre = factor(nombres_sensibles[variable],
                         levels = rev(nombres_sensibles[variable])))

g1 <- ggplot(resumen_g, aes(y = nombre)) +
  geom_vline(xintercept = 0.5, linetype = "dashed", colour = "grey55") +
  geom_segment(aes(x = minimo, xend = maximo, yend = nombre),
               colour = "#8E3038", alpha = 0.35, linewidth = 2.2) +
  geom_point(aes(x = auc), colour = "#8E3038", size = 3.8) +
  geom_text(aes(x = auc, label = sprintf("%.3f", auc)),
            nudge_y = 0.3, size = 3.8, colour = "grey20") +
  scale_x_continuous(limits = c(0.48, 0.80), breaks = seq(0.5, 0.8, 0.05)) +
  labs(
    title    = "Lo que se puede adivinar\nsin preguntarlo",
    subtitle = "AUC al predecir cada característica usando solo\n20 variables financieras y de historial de crédito",
    x        = "AUC (0.5 = azar)",
    y        = NULL,
    caption  = "Hipotecas, Boston 1990 (n = 1 922). Barra: rango en 10 particiones."
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title          = element_text(face = "bold", size = 15, lineheight = 1.15),
        plot.title.position = "plot",
        plot.caption        = element_text(colour = "grey40", hjust = 0),
        panel.grid.minor    = element_blank(),
        panel.grid.major.y  = element_blank(),
        axis.text.y         = element_text(size = 12, colour = "grey15"))

print(g1)
ggsave("salidas/exp2_adivinar.png", g1, width = 7.5, height = 5, dpi = 200)



g2 <- ggplot(delatoras, aes(x = peso, y = reorder(nombre, peso))) +
  geom_col(fill = "#8E3038", width = 0.6) +
  geom_text(aes(label = sprintf("%.1f", peso)), hjust = -0.25, size = 3.8) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "Las variables que delatan la raza",
    subtitle = "Peso de cada variable financiera al predecir si el\nsolicitante es afrodescendiente (valor z absoluto)",
    x        = "Peso (|z|)",
    y        = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title          = element_text(face = "bold", size = 15),
        plot.title.position = "plot",
        panel.grid.minor    = element_blank(),
        panel.grid.major.y  = element_blank(),
        axis.text.y         = element_text(size = 12, colour = "grey15"))

print(g2)
ggsave("salidas/exp2_delatoras.png", g2, width = 7.5, height = 4.6, dpi = 200)

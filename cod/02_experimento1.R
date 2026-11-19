
library(dplyr)
library(ggplot2)
library(modeldata)
library(pROC)


cat("Carpeta de trabajo:", getwd(), "\n")
dir.create("salidas", showWarnings = FALSE)

data(lending_club)


lc <- lending_club |>
  mutate(malo = ifelse(Class == "bad", 1, 0))

cat("\nFilas:", nrow(lc), "| Créditos malos:", sum(lc$malo),
    sprintf("(%.1f%%)\n", 100 * mean(lc$malo)))




# Lo que la PERSONA escribió en su solicitud
declaradas <- c("funded_amnt",          # monto solicitado
                "term",                 # plazo
                "estado",               # residencia (la creamos en el paso 3)
                "annual_inc",           # ingreso anual
                "emp_length",           # antigüedad laboral
                "verification_status")  # si se verificó el ingreso

# Lo que vino del BURÓ DE CRÉDITO, sin preguntarle a la persona
buro <- c("delinq_2yrs",                 # moras en los últimos 2 años
          "inq_last_6mths",              # consultas de crédito en 6 meses
          "revol_util",                  # uso de la línea revolvente
          "acc_now_delinq",              # cuentas en mora hoy
          "open_il_6m",                  # cuentas a plazos abiertas en 6m
          "open_il_12m",
          "open_il_24m",
          "total_bal_il",                # saldo total a plazos
          "all_util",                    # utilización total del crédito
          "inq_fi",                      # consultas de entidades financieras
          "inq_last_12m",                # consultas en 12 meses
          "delinq_amnt",                 # monto actualmente en mora
          "num_il_tl",                   # número de cuentas a plazos
          "total_il_high_credit_limit")  # límite histórico


prohibidas <- c("int_rate", "sub_grade")

stopifnot(!any(prohibidas %in% c(declaradas, buro)))
cat("Declaradas:", length(declaradas), "| Buró:", length(buro), "\n")


MINIMO <- 100  # un estado necesita al menos esto para ir solo

frecuentes <- lc |>
  count(addr_state) |>
  filter(n >= MINIMO) |>
  pull(addr_state)

lc <- lc |>
  mutate(estado = factor(ifelse(addr_state %in% frecuentes,
                                as.character(addr_state), "OTRO")))

cat("Estados propios:", length(frecuentes),
    "| Agrupados en OTRO:", 50 - length(frecuentes), "\n")




evaluar <- function(variables, entrenamiento, prueba) {

  formula <- as.formula(paste("malo ~", paste(variables, collapse = " + ")))

 
  modelo <- glm(formula, data = entrenamiento, family = binomial)


  probabilidad <- predict(modelo, newdata = prueba, type = "response")


  as.numeric(auc(roc(prueba$malo, probabilidad, quiet = TRUE)))
}



semillas <- c(1, 7, 42, 99, 2026)
resultados <- data.frame()

for (s in semillas) {

  set.seed(s)                                   # fija el azar
  i <- sample(nrow(lc), floor(0.7 * nrow(lc)))  # 70% entrena
  ent <- lc[i, ]
  pru <- lc[-i, ]

  auc_B <- evaluar(declaradas,          ent, pru)
  auc_A <- evaluar(c(declaradas, buro), ent, pru)

  resultados <- rbind(resultados,
                      data.frame(semilla = s, modelo_B = auc_B, modelo_A = auc_A))

  cat(sprintf("  semilla %4d ->  B = %.3f   A = %.3f\n", s, auc_B, auc_A))
}


resumen <- data.frame(
  modelo    = c("B: solo lo declarado", "A: declarado + buro"),
  variables = c(length(declaradas), length(declaradas) + length(buro)),
  auc       = c(mean(resultados$modelo_B), mean(resultados$modelo_A)),
  desv      = c(sd(resultados$modelo_B),   sd(resultados$modelo_A))
) |>
  mutate(gini = 2 * auc - 1)


print(resumen, row.names = FALSE, digits = 3)

brecha <- resumen$auc[2] - resumen$auc[1]
cat(sprintf("\nBrecha: %+.3f de AUC  (%+.1f%% relativo)\n",
            brecha, 100 * brecha / resumen$auc[1]))

write.csv(resumen, "salidas/exp1_resumen.csv", row.names = FALSE)
write.csv(resultados, "salidas/exp1_por_semilla.csv", row.names = FALSE)




set.seed(2026)
i <- sample(nrow(lc), floor(0.7 * nrow(lc)))
ent <- lc[i, ]; pru <- lc[-i, ]

roc_de <- function(variables) {
  m <- glm(as.formula(paste("malo ~", paste(variables, collapse = " + "))),
           data = ent, family = binomial)
  roc(pru$malo, predict(m, newdata = pru, type = "response"), quiet = TRUE)
}

roc_B <- roc_de(declaradas)
roc_A <- roc_de(c(declaradas, buro))


rejilla <- seq(0, 1, by = 0.005)
a_rejilla <- function(r) {
  d <- data.frame(x = 1 - r$specificities, y = r$sensitivities)
  d <- d[order(d$x), ]
  approx(d$x, d$y, xout = rejilla, ties = max, rule = 2)$y
}

curvas <- data.frame(x = rejilla, B = a_rejilla(roc_B), A = a_rejilla(roc_A))

grafico <- ggplot(curvas, aes(x = x)) +
  geom_ribbon(aes(ymin = B, ymax = A), fill = "#8E3038", alpha = 0.16) +
  geom_abline(intercept = 0, slope = 1,
              linetype = "dashed", colour = "grey60") +
  geom_line(aes(y = B, colour = "Solo lo declarado"), linewidth = 0.9) +
  geom_line(aes(y = A, colour = "Declarado + buró"), linewidth = 0.9) +
  scale_colour_manual(values = c("Declarado + buró"  = "#8E3038",
                                 "Solo lo declarado" = "#2F6B63")) +
  coord_equal() +
  labs(
    title    = "Lo que aporta el dato que\nel solicitante no entregó",
    subtitle = sprintf("AUC %.3f con datos del buró · %.3f sin ellos",
                       resumen$auc[2], resumen$auc[1]),
    x        = "Buenos pagadores rechazados de más",
    y        = "Malos pagadores detectados",
    colour   = NULL,
    caption  = "Lending Club, 9 857 créditos. La franja es la diferencia entre ambos modelos."
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position    = "bottom",
        plot.title         = element_text(face = "bold", size = 15, lineheight = 1.15),
        plot.title.position = "plot",
        plot.caption       = element_text(colour = "grey40", hjust = 0),
        panel.grid.minor   = element_blank())

print(grafico)
ggsave("salidas/exp1_curvas_roc.png", grafico,
       width = 7, height = 7.4, dpi = 200)



# Índice de precios regionales 2016, todos los bienes (BEA).
rpp <- c(AL =  90.6, AK = 104.1, AZ =  98.6, AR =  89.3, CA = 109.9,
         CO = 102.7, CT = 107.5, DE =  98.5, DC = 112.5, FL = 101.1,
         GA =  94.9, HI = 109.9, ID =  93.0, IL =  99.5, IN =  91.4,
         IA =  89.6, KS =  92.1, KY =  90.6, LA =  93.6, ME =  97.3,
         MD = 107.3, MA = 108.2, MI =  93.6, MN =  97.3, MS =  88.5,
         MO =  91.7, MT =  93.9, NE =  90.9, NV =  97.2, NH = 105.1,
         NJ = 109.1, NM =  94.5, NY = 110.1, NC =  93.8, ND =  91.7,
         OH =  92.4, OK =  91.5, OR =  99.6, PA =  97.8, RI = 101.7,
         SC =  93.6, SD =  87.4, TN =  93.8, TX =  98.0, UT =  97.1,
         VT =  98.3, VA = 102.8, WA = 105.6, WV =  89.8, WI =  93.2,
         WY =  97.7)

lc <- lc |>
  mutate(siglas       = as.character(addr_state),
         costo_vida   = rpp[siglas],
         ingreso_real = annual_inc / (costo_vida / 100),
         region       = factor(ifelse(siglas == "DC", "South",
                                      as.character(state.region[match(siglas, state.abb)]))))

sin_lugar <- setdiff(declaradas, "estado")

versiones <- list(
  "Estado (original)"        = declaradas,
  "Estado + ingreso real"    = c(setdiff(declaradas, "annual_inc"), "ingreso_real"),
  "Sin ubicacion"            = sin_lugar,
  "Region (4)"               = c(sin_lugar, "region"),
  "Costo de vida (1 numero)" = c(sin_lugar, "costo_vida")
)

# Las mismas cinco semillas: así cada versión se evalúa con
# exactamente las mismas personas y la comparación es justa.
lugar <- data.frame()
for (s in semillas) {
  set.seed(s)
  i   <- sample(nrow(lc), floor(0.7 * nrow(lc)))
  ent <- lc[i, ]
  pru <- lc[-i, ]
  for (v in names(versiones)) {
    lugar <- rbind(lugar, data.frame(
      version = v, semilla = s,
      B = evaluar(versiones[[v]],         ent, pru),
      A = evaluar(c(versiones[[v]], buro), ent, pru)))
  }
}

tabla_lugar <- lugar |>
  group_by(version) |>
  summarise(B = mean(B), A = mean(A), .groups = "drop") |>
  arrange(match(version, names(versiones)))


cat("                              AUC B    AUC A\n")
for (k in seq_len(nrow(tabla_lugar))) {
  cat(sprintf("  %-26s  %.3f    %.3f\n",
              tabla_lugar$version[k], tabla_lugar$B[k], tabla_lugar$A[k]))
}


write.csv(tabla_lugar, "salidas/exp1_ubicacion.csv", row.names = FALSE)

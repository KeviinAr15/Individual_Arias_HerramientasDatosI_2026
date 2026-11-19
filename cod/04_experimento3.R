
library(dplyr)
library(ggplot2)
library(modeldata)
library(pROC)

cat("Carpeta de trabajo:", getwd(), "\n")
dir.create("salidas", showWarnings = FALSE)


data(lending_club)

lc <- lending_club |>
  mutate(malo = ifelse(Class == "bad", 1, 0))

# Los estados con menos de 100 casos van a "OTRO"
frecuentes <- lc |> count(addr_state) |> filter(n >= 100) |> pull(addr_state)
lc <- lc |>
  mutate(estado = factor(ifelse(addr_state %in% frecuentes,
                                as.character(addr_state), "OTRO")))

# Las variables del modelo A del experimento 1, tal cual.
crudas <- c("funded_amnt", "term", "estado", "annual_inc", "emp_length",
            "verification_status",
            "delinq_2yrs", "inq_last_6mths", "revol_util", "acc_now_delinq",
            "open_il_6m", "open_il_12m", "open_il_24m", "total_bal_il",
            "all_util", "inq_fi", "inq_last_12m", "delinq_amnt",
            "num_il_tl", "total_il_high_credit_limit")

# Calcula dónde cortar para tener k grupos del mismo tamaño
cortes_cuantiles <- function(x, k) {
  b <- unique(quantile(x, probs = seq(0, 1, length.out = k + 1)))
  c(-Inf, b[-c(1, length(b))], Inf)
}

# Mete cada valor en su grupo y devuelve el número de grupo
en_grupos <- function(x, cortes) {
  factor(cut(x, breaks = cortes, labels = FALSE),
         levels = seq_len(length(cortes) - 1))
}

# Pone un tope: todo lo que pase de `tope` queda como `tope`
con_tope <- function(x, tope) {
  factor(pmin(x, tope), levels = 0:tope)
}

# Del estado a la región del país. R ya trae la tabla:
region_de <- function(siglas) {
  r <- as.character(state.region[match(siglas, state.abb)])
  r[siglas == "DC"] <- "South"
  factor(r)
}


calcular_cortes <- function(ent) {
  list(ingreso = cortes_cuantiles(ent$annual_inc, 10),
       monto   = cortes_cuantiles(ent$funded_amnt, 5),
       saldo   = cortes_cuantiles(ent$total_bal_il, 5),
       limite  = cortes_cuantiles(ent$total_il_high_credit_limit, 5))
}

agrupar <- function(d, cortes) {
  porcentaje <- c(-Inf, 25, 50, 75, 100, Inf)
  d |> mutate(
    # Declaradas
    g_ingreso = en_grupos(annual_inc, cortes$ingreso),   # deciles
    g_monto   = en_grupos(funded_amnt, cortes$monto),    # quintiles
    region    = region_de(addr_state),                   # 4 regiones
    g_antig   = factor(case_when(
      emp_length %in% c("emp_lt_1", "emp_1")                    ~ "menos de 2",
      emp_length %in% c("emp_2", "emp_3", "emp_4")              ~ "2 a 4",
      emp_length %in% c("emp_5", "emp_6", "emp_7", "emp_8", "emp_9") ~ "5 a 9",
      emp_length == "emp_ge_10"                                 ~ "10 o más",
      TRUE                                                      ~ "desconocida")),
    # Del buró
    g_saldo   = en_grupos(total_bal_il, cortes$saldo),
    g_limite  = en_grupos(total_il_high_credit_limit, cortes$limite),
    g_revol   = cut(revol_util, porcentaje),
    g_util    = cut(all_util,   porcentaje),
    g_cuentas = cut(num_il_tl,  c(-Inf, 3, 7, 12, 18, Inf)),
    g_mora2   = con_tope(delinq_2yrs, 2),
    g_moraHoy = con_tope(acc_now_delinq, 1),
    g_consu6  = con_tope(inq_last_6mths, 3),
    g_consu12 = con_tope(inq_last_12m, 5),
    g_consuFi = con_tope(inq_fi, 3),
    g_abre6   = con_tope(open_il_6m, 6),
    g_abre12  = con_tope(open_il_12m, 2),
    g_abre24  = con_tope(open_il_24m, 4),
    g_enMora  = factor(ifelse(delinq_amnt > 0, "si", "no"))
  )
}

agrupadas <- c("g_ingreso", "g_monto", "region", "term", "g_antig",
               "verification_status",
               "g_saldo", "g_limite", "g_revol", "g_util", "g_cuentas",
               "g_mora2", "g_moraHoy", "g_consu6", "g_consu12", "g_consuFi",
               "g_abre6", "g_abre12", "g_abre24", "g_enMora")


stopifnot(length(crudas) == length(agrupadas))

evaluar <- function(variables, entrenamiento, prueba) {
  formula <- as.formula(paste("malo ~", paste(variables, collapse = " + ")))
  modelo  <- glm(formula, data = entrenamiento, family = binomial)
  prob    <- predict(modelo, newdata = prueba, type = "response")
  as.numeric(auc(roc(prueba$malo, prob, quiet = TRUE)))
}

semillas <- c(1, 7, 42, 99, 2026)
resultados <- data.frame()

for (s in semillas) {
  set.seed(s)
  i   <- sample(nrow(lc), floor(0.7 * nrow(lc)))
  ent <- lc[i, ]
  pru <- lc[-i, ]

  cortes <- calcular_cortes(ent)          # solo con entrenamiento
  ent_g  <- agrupar(ent, cortes)
  pru_g  <- agrupar(pru, cortes)          # mismos cortes a la prueba

  auc_crudo <- evaluar(crudas,    ent,   pru)
  auc_grupo <- evaluar(agrupadas, ent_g, pru_g)

  resultados <- rbind(resultados,
                      data.frame(semilla = s, crudo = auc_crudo, agrupado = auc_grupo))
  cat(sprintf("  semilla %4d ->  crudo %.3f   agrupado %.3f\n", s, auc_crudo, auc_grupo))
}


medir_unicidad <- function(d, identificadores) {
  grupos <- d |> count(across(all_of(identificadores)), name = "k")
  personas <- sum(grupos$k)
  c(unicos     = 100 * sum(grupos$k[grupos$k == 1]) / personas,
    menos_de_5 = 100 * sum(grupos$k[grupos$k <  5]) / personas)
}

lc_g <- agrupar(lc, calcular_cortes(lc))

u_crudo <- medir_unicidad(lc,   c("annual_inc", "funded_amnt", "addr_state",
                                  "term", "emp_length"))
u_grupo <- medir_unicidad(lc_g, c("g_ingreso", "g_monto", "region",
                                  "term", "g_antig"))




resumen <- data.frame(
  version    = c("Datos crudos", "Datos agrupados"),
  auc        = c(mean(resultados$crudo), mean(resultados$agrupado)),
  unicos     = c(u_crudo["unicos"],     u_grupo["unicos"]),
  menos_de_5 = c(u_crudo["menos_de_5"], u_grupo["menos_de_5"])
)

diferencia <- resumen$auc[2] - resumen$auc[1]

dif_semilla <- resultados$agrupado - resultados$crudo

cat(sprintf("                    AUC     unicos   k < 5\n"))
cat(sprintf("  Datos crudos     %.3f   %5.1f %%  %5.1f %%\n",
            resumen$auc[1], resumen$unicos[1], resumen$menos_de_5[1]))
cat(sprintf("  Datos agrupados  %.3f   %5.1f %%  %5.1f %%\n",
            resumen$auc[2], resumen$unicos[2], resumen$menos_de_5[2]))
cat(sprintf("\n  Diferencia de AUC: %+.3f\n", diferencia))
cat(sprintf("  Entre semillas varió de %+.3f a %+.3f\n",
            min(dif_semilla), max(dif_semilla)))
cat(sprintf("  La gente identificable baja de %.1f %% a %.1f %%\n",
            resumen$unicos[1], resumen$unicos[2]))




write.csv(resumen,    "salidas/exp3_resumen.csv",    row.names = FALSE)
write.csv(resultados, "salidas/exp3_por_semilla.csv", row.names = FALSE)



para_graficar <- data.frame(
  medida  = rep(c("Capacidad de predecir (AUC)",
                  "Personas identificables (%)"), each = 2),
  version = rep(c("Datos crudos", "Datos agrupados"), times = 2),
  valor   = c(resumen$auc, resumen$unicos)
) |>
  mutate(version = factor(version, levels = c("Datos crudos", "Datos agrupados")),
         etiqueta = ifelse(grepl("AUC", medida),
                           sprintf("%.3f", valor), sprintf("%.1f %%", valor)))

grafico <- ggplot(para_graficar, aes(x = version, y = valor, fill = version)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = etiqueta), vjust = -0.5, size = 4.2) +
  facet_wrap(~ medida, scales = "free_y") +
  scale_fill_manual(values = c("Datos crudos" = "grey65",
                               "Datos agrupados" = "#8E3038")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "La privacidad sale casi gratis",
    subtitle = sprintf("Agrupar los datos casi no cambia el AUC (%.3f contra %.3f), pero baja\nla gente identificable de %.1f %% a %.1f %%",
                       resumen$auc[2], resumen$auc[1], resumen$unicos[1], resumen$unicos[2]),
    x = NULL, y = NULL,
    caption  = "Lending Club, 9 857 créditos. AUC: promedio de 5 particiones."
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position     = "none",
        plot.title          = element_text(face = "bold", size = 15),
        plot.title.position = "plot",
        plot.caption        = element_text(colour = "grey40", hjust = 0),
        strip.text          = element_text(face = "bold", size = 11.5),
        panel.grid.major.x  = element_blank(),
        panel.grid.minor    = element_blank(),
        axis.text.y         = element_blank())

print(grafico)
ggsave("salidas/exp3_precio_privacidad.png", grafico, width = 8, height = 5, dpi = 200)

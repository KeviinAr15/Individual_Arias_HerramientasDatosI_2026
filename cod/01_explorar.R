install.packages(c("dplyr", "ggplot2", "modeldata", "wooldridge"))

library(dplyr)       
library(ggplot2)     
library(modeldata)   # donde vive la base de datos
data(lending_club)


count(lending_club, Class)
# good = pagó, bad = no pagó.

# El mismo dato en porcentaje
lending_club |>
  count(Class) |>
  mutate(porcentaje = round(100 * n / sum(n), 1))


declaradas <- c("funded_amnt",          # monto solicitado
                "term",                 # plazo
                "addr_state",           # estado de residencia
                "annual_inc",           # ingreso anual
                "emp_length",           # antigüedad laboral
                "verification_status")  # si se verificó el ingreso

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

length(declaradas)
length(buro)

# ¿El ingreso declarado separa a los buenos de los malos?

lending_club |>
  filter(annual_inc < 250000) |>   # quitamos los casos extremos para ver mejor
  ggplot(aes(x = Class, y = annual_inc, fill = Class)) +
  geom_boxplot(alpha = 0.8) +
  labs(title    = "Ingreso anual declarado según el resultado del crédito",
       subtitle = "Si las dos cajas están casi iguales, esta variable sola predice poco",
       x        = "Resultado del crédito",
       y        = "Ingreso anual declarado (USD)") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")


# ¿Las consultas de crédito recientes separan mejor?

lending_club |>
  ggplot(aes(x = Class, y = inq_last_12m, fill = Class)) +
  geom_boxplot(alpha = 0.8) +
  labs(title    = "Consultas de crédito en los últimos 12 meses",
       subtitle = "Este dato NO lo declara la persona: viene del buró",
       x        = "Resultado del crédito",
       y        = "Cantidad de consultas") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")




paquetes <- c(
  "dplyr",       # manipulación de datos
  "ggplot2",     # gráficos
  "knitr",       # tablas del reporte
  "pROC",        # cálculo del AUC
  "modeldata",   # base lending_club
  "wooldridge"   # base loanapp
)

faltantes <- setdiff(paquetes, rownames(installed.packages()))

if (length(faltantes) > 0) {
  install.packages(faltantes)
} else {
  message("Todos los paquetes ya están instalados.")
}

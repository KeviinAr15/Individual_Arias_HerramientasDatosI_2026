# Fuentes de datos del proyecto

No existe una base de crédito costarricense de acceso público, por lo que se
usan dos bases de Estados Unidos incluidas en paquetes de R. El análisis
normativo se refiere al marco costarricense (Ley 8968).

## Préstamos de Lending Club

- **Base:** `lending_club`, paquete `modeldata` (Kuhn, 2024).
- **Cobertura:** préstamos de Lending Club del primer trimestre de 2016.
  9 857 filas y 23 columnas.
- **Variable respuesta:** `Class`, con niveles `bad` (impago) y `good`. En el
  reporte se recodifica como `malo` (1 = impago).
- **Uso:** experimentos 1 y 3.
- **Grupos de variables:** 6 declaradas por el solicitante, 14 de buró de
  crédito y 2 excluidas por fuga de información (tasa de interés y
  calificación asignada por el prestamista).

## Solicitudes de hipoteca en Boston

- **Base:** `loanapp`, paquete `wooldridge`. Proviene del estudio de la
  Reserva Federal de Boston sobre discriminación hipotecaria (Munnell et al.,
  1996).
- **Cobertura:** solicitudes de 1990. 1 989 filas y 59 columnas.
- **Uso:** experimento 2. Es una de las pocas bases de crédito que incluye
  raza, sexo y estado civil.

## Índice de precios regionales (prueba complementaria)

- **Fuente:** Bureau of Economic Analysis, *Regional Price Parities by State*,
  2016, tomado del Hawaii State Data Book, tabla 14.02.
- **Uso:** los valores están escritos dentro de `cod/02_experimento1.R` para
  ajustar el ingreso por el costo de vida de cada estado.

## Advertencias de uso

- En `lending_club`, los estados con menos de 100 préstamos se agrupan en una
  categoría "OTRO" para no estimar parámetros con muy pocos casos.
- `addr_state` es un factor: para buscar por sigla hay que convertirlo antes a
  texto con `as.character()`.

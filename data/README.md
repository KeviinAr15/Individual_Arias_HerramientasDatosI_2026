# Datos

Esta carpeta no contiene archivos de datos. Las dos bases del proyecto vienen
incluidas en paquetes de R y se cargan directamente desde el código:

| Base           | Paquete      | Cómo se carga        | Filas | Columnas |
|:---------------|:-------------|:---------------------|------:|---------:|
| `lending_club` | `modeldata`  | `data(lending_club)` | 9 857 | 23       |
| `loanapp`      | `wooldridge` | `data(loanapp)`      | 1 989 | 59       |

Así cualquier persona que instale los paquetes (ver `config/paquetes.R`)
trabaja exactamente con la misma versión de los datos.

El detalle de cada base está en `info/fuentes_de_datos.md`.

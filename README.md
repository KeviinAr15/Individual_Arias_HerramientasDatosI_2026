# Individual_Arias_HerramientasDatosI_2026

Proyecto individual del curso CA-0204 Herramientas de Ciencia de Datos I

**Autor:** Kevin Arias Reyes

## ¿Cuántos datos necesita un banco para prestarte?

El proyecto evalúa, con modelos de regresión logística, si la información
personal que los prestamistas obtienen de terceros es
necesaria para predecir el impago, a la luz de los principios de adecuación
al fin y de transferencia de datos de la Ley 8968 de Costa Rica. Se hacen
tres experimentos:

1. Cuánto mejora un modelo de riesgo al sumar datos de buró a los que declara
   el solicitante.
2. Si variables financieras aparentemente neutrales permiten reconstruir
   características sensibles como la raza o el sexo.
3. Cuánto se pierde en capacidad predictiva y cuánto se gana en privacidad al
   agrupar los datos antes de usarlos.

## Estructura del repositorio

| Carpeta   | Contenido |
|:----------|:----------|
| `cod/`    | Reporte en Quarto (`Individual_Arias_HerramientasDatosI_2026.qmd` y su PDF) y los guiones de R de cada experimento. |
| `data/`   | Explica de dónde salen las bases. No guarda archivos porque las dos bases vienen dentro de paquetes de R. |
| `info/`   | Documentación de las fuentes de datos y material de la presentación. |
| `config/` | Guion que instala los paquetes necesarios. |

Los guiones de `cod/` son el trabajo exploratorio previo al reporte:

- `01_explorar.R`: primer contacto con `lending_club`.
- `02_experimento1.R`: declarado frente a declarado + buró.
- `03_experimento2.R`: predicción de características sensibles con `loanapp`.
- `04_experimento3.R`: datos exactos frente a datos agrupados y unicidad.

El reporte no depende de ellos: el `.qmd` hace todos los cálculos por sí solo.

## Cómo reproducir el reporte

1. Instalar R (4.3 o superior) y Quarto.
2. Correr una vez `config/paquetes.R` para instalar los paquetes.
3. Abrir `cod/Individual_Arias_HerramientasDatosI_2026.qmd` y renderizar a PDF.

No hay que descargar datos: `lending_club` viene en el paquete `modeldata` y
`loanapp` en el paquete `wooldridge`. Los resultados usan semillas fijas, así
que se obtienen los mismos números en cada corrida.

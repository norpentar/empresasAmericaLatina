library(dplyr)
#library(neo4r)
library(visNetwork)
library(bolt4jr)
library(reticulate)
library(readxl)

#Archivo con variables y funciones auxiliares para el markdown que genera los datos en formato html

columnas_tabla_empresas <- c("name", "country_region", "industry_reclassified_2", "total_revenue", "employees", "market_cap")
variables_financieras_seleccionadas <- c("total revenue", "market capitalization", "total assets")


colores_industria <- c(
  "Food"                      = "#e6550d",  # naranja
  "Finance"                   = "#3182bd",  # azul
  "Real Estate/Construction"  = "#756bb1",  # púrpura
  "Commerce"                  = "#e6ab02",  # dorado/mostaza
  "Telecommunication"         = "#1b9e77",  # verde azulado (teal)
  "Productive Industry"       = "#a50026",  # rojo oscuro/vino
  "Utilities"                 = "#33a02c",  # verde
  "Extractive Industry"       = "#a6761d",  # marrón
  "Transport and Logistics"   = "#e7298a",  # magenta/rosado fuerte
  "Government Finance"        = "#08519c",  # azul marino (más oscuro que Finance)
  "Mining"                    = "#636363",  # gris oscuro
  "Government"                = "#cab2d6",  # lavanda (distinto del púrpura de Real Estate)
  "Other"                     = "#bdbdbd",  # gris claro
  "Agroindustry"              = "#b2df8a"   # verde lima (distinto del verde de Utilities)
)

# Función generadora del filtro de rango (reutilizable para varias columnas numéricas)
range_filter <- function(table_id) {
  function(values, name) {
    min_var <- paste0("min_", name)
    max_var <- paste0("max_", name)
    
    tagList(
      tags$input(
        type = "number",
        placeholder = "Mín",
        style = "width:48%; margin-right:4%; font-size:12px;",
        oninput = sprintf(
          "window.%s = this.value === '' ? undefined : Number(this.value);
           Reactable.setFilter('%s', '%s', { min: window.%s, max: window.%s });",
          min_var, table_id, name, min_var, max_var
        )
      ),
      tags$input(
        type = "number",
        placeholder = "Máx",
        style = "width:48%; font-size:12px;",
        oninput = sprintf(
          "window.%s = this.value === '' ? undefined : Number(this.value);
           Reactable.setFilter('%s', '%s', { min: window.%s, max: window.%s });",
          max_var, table_id, name, min_var, max_var
        )
      )
    )
  }
}

# filterMethod correspondiente, que ahora recibe un objeto {min, max} en vez de un solo valor
filtro_rango_js <- JS("function(rows, columnId, filterValue) {
  return rows.filter(function(row) {
    var valor = row.values[columnId]
    if (filterValue === undefined) return true
    if (filterValue.min !== undefined && valor < filterValue.min) return false
    if (filterValue.max !== undefined && valor > filterValue.max) return false
    return true
  })
}")

bar_chart <- function(column, tabla, color) {
  formatting_function <- if (column %in% c("total_revenue", "market_cap")) {
    scales::label_number(
      suffix = 'B',
      scale = 1e-9,
      accuracy = 0.1,
      big.mark = ','
    )
  } else {
    scales::label_comma(
      accuracy = if (column == 'pop_per_km2') 1 else 0.01
    )
  }
  bar_max_value <- if (column %in% c("total_revenue", "market_cap")) {
    max(tabla[,column], na.rm = TRUE)
  } else {
    NULL
  }
  bar_min_value <- if (column %in% c("total_revenue", "market_cap")) {
    0
  } else {
    NULL
  } 
  data_bars(
    data = tabla |> select(any_of(column)),
    text_position = 'above',
    bar_height = 20,
    number_fmt = formatting_function,
    fill_color = color,
    max_value = bar_max_value,
    min_value = bar_min_value
  )
}








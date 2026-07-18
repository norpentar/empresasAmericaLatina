##funciones auxiliares
##pruebo a cargar directo de los excel
### función que la fecha del número de excel
xldate<- function(x) {
  msg <- glue::glue("Entrando en la función xldate")
  log_trace_multi(msg, namespaces = c("auxiliares"))
  xn <- as.numeric(x)
  x <- as.Date(xn, origin="1899-12-30")
}

renombraFilasTabla <- function(tabla_in, columna_list, valor_list){
  msg <- glue::glue("Entrando en la función renombraFilasTabla")
  log_trace_multi(msg, namespaces = c("auxiliares"))
  renombraFilasTabla_one <- function(tabla_in_single, columna, valor){
    tabla <- tabla_in_single %>% 
      mutate(!!columna := if_else(
        str_detect(tolower(!!sym(columna)), tolower(paste0(".*",valor,".*"))),
        valor,
        !!sym(columna)
      ))
    
    return(tabla)
  } 
  
  tabla <- tabla_in
  for (i in 1:length(columna_list)){
    columna <- columna_list[i]
    valor <- valor_list[i]
    
    tabla <- renombraFilasTabla_one(tabla, columna, valor)
  }
  return(tabla)
  
}

renombraFilasTablaVector <- function(tabla_in, columna = "investor_name", ruta_matriz = "./tablas_input/auxiliares/matriz_renombre_accionistas.xlsx"){
  msg <- glue::glue("Entrando en la función renombraFilasTablaVector")
  log_trace_multi(msg, namespaces = c("auxiliares"))
  tabla <- tabla_in
  matriz <- read_excel(ruta_matriz)
  matriz$nombres_originales <- str_split(matriz$nombres_originales, "; ")
  #browser()
  tabla <- tabla %>%
    mutate(!!columna := sapply(!!sym(columna), function(nombre){
      for (i in 1:nrow(matriz)){
        if(nombre %in% matriz$nombres_originales[[i]]){
          return(matriz$nombre_modificado[i]) 
        } else if(i == nrow(matriz)){
          return(nombre)
        }
      }
    }
    )
    )
  
  return(tabla)
}

reagrupaTabla <- function(tabla_in, columna_list, valor_list){
  msg <- glue::glue("Entrando en la función reagrupaTabla")
  log_trace_multi(msg, namespaces = c("auxiliares"))
  reagrupaTabla_one <- function(tabla_in_single, columna, valor, iteracion = 1){
    if(iteracion == 1){
      tabla <- tabla_in_single %>% mutate(!!paste0(columna,"_agrupado") := tabla_in_single[[columna]])
    } else{
      tabla <- tabla_in_single
    }
    campos_numericos <- tabla %>% select(where(is.numeric)) %>% colnames()
    campos_string <- tabla %>% select(where(is.character)) %>% colnames()
    campos_string <- setdiff(campos_string, columna)
    
    nueva_fila <- tabla %>% filter(str_detect(tolower(!!sym(columna)), tolower(paste0(".*",valor,".*")))) %>% 
      summarise(!!columna := valor, 
                across(all_of(campos_string), ~ paste(unique(.), collapse = ", "), .names = "{.col}"),
                across(all_of(campos_numericos), \(x) sum(x, na.rm = TRUE), .names = "{.col}"),
                .groups = "drop"
      )
    
    tabla <- tabla %>% filter(!str_detect(tolower(!!sym(columna)), tolower(paste0(".*",valor,".*"))))
    if (nrow(tabla) == 0){
      tabla <- tabla
    } else{
      tabla <- bind_rows(tabla, nueva_fila)
    }
    
    return(tabla)
  }
  
  tabla <- tabla_in
  for(i in 1:length(columna_list)){
    columna <- columna_list[i]
    valor <- valor_list[i]
    
    tabla <- reagrupaTabla_one(tabla, columna, valor, i)
    
  }
  
  return(tabla)
  
}

sacaDiagramaBarras <- function(tabla, columna_string, columna_valor, titulo = "Gráfico de barras"){
  msg <- glue::glue("Entrando en la función sacaDiagramaBarras")
  log_trace_multi(msg, namespaces = c("auxiliares"))
  grafico <- ggplot(tabla, aes_string(x = columna_string, y = columna_valor)) +
    geom_bar(stat = "summary", fun = "sum", fill = "steelblue", color="black") +
    theme_minimal() +
    labs(x=columna_string, y=columna_valor, title = titulo) +
    theme(axis.text.x = element_text(size= 6, angle = 45, hjust = 1)) +
    theme(axis.text.y = element_text(size = 6))
  return(grafico)
}


verificaLoops <- function(treeStructrue_input_excel){
  msg <- glue::glue("Entrando en la función verificaLoops")
  log_trace_multi(msg, namespaces = c("auxiliares"))
  treeStructure_excel <- read_excel(treeStructrue_input_excel)
  cat("Cargando el archivo TreeStructure ",treeStructrue_input_excel,"\n")
  treeStructure_excel <- treeStructure_excel[-c(1:5),]
  columnasFaltantes <- 38 - ncol(treeStructure_excel)
  if (columnasFaltantes > 0){
    if (nrow(treeStructure_excel) ==1){
      nuevas_columnas <- as_tibble(t(replicate(columnasFaltantes, rep(NA, nrow(treeStructure_excel)))))
    } else{
      nuevas_columnas <- as_tibble(replicate(columnasFaltantes, rep(NA, nrow(treeStructure_excel))))
    }
    nombres_columnas <- sapply(1:columnasFaltantes, \(x) paste0("nueva",x))
    treeStructure_excel <- treeStructure_excel %>% mutate(!!!setNames(nuevas_columnas, nombres_columnas))
    treeStructure_excel <- treeStructure_excel %>% relocate(starts_with("nueva"), .after = names(treeStructure_excel)[11-columnasFaltantes])
  }
  colnames(treeStructure_excel) <- c("permID", "level_0", "level_1", "level_2", "level_3", "level_4",
                                     "level_5", "level_6","level_7","level_8","level_9","relationship_type","type","country_region","incorporated_country",
                                     "incorporated_date","pe_backed_status","industry","total_revenue","employees",
                                     "market_cap","implied_rating","moody_rating","fitch_rating","ownership_per","ric","issued_bonds",
                                     "outstanding_loans","cds","commercial_paper","futures","options","warrants","equities",
                                     "certificate_deposits","preference_shares","investment_certificates","funds_count")
  ###limpia las líneas NA
  indicesFilasNA <- which(apply(treeStructure_excel[,c("level_0", "level_1", "level_2", "level_3", "level_4", "level_5", "level_6","level_7","level_8","level_9")], 1, function(x) all(is.na(x))))
  if(length(indicesFilasNA) != 0){
    treeStructure_excel <- treeStructure_excel[-indicesFilasNA,]
  }
  
  tabla <- treeStructure_excel
  columnas_tabla <- colnames(tabla)
  
  empresas <- unlist(c(tabla[columnas_tabla]))
  empresas <- as.vector(empresas[!is.na(empresas)])
  
  for (i in 1:length(empresas)){
    empresa <- empresas[i]
    if (empresa %in% empresas[-i]){
      return(list(TRUE, empresa))
    }
  }  
  return(list(FALSE, "nada"))
  
}

reclasificaIndustriaEmpresa <- function(industry, ruta_tabla_reclasificacion = "./tablas_input/auxiliares/recategorizacion_industrias_empresas.xlsx"){
  msg <- glue::glue("Entrando en la función reclasificaIndustriaEmpresa")
  log_trace_multi(msg, namespaces = c("auxiliares"))
  tabla <- read_excel(ruta_tabla_reclasificacion) %>% fill(everything())
  #browser()
  industry_reclassifed <- as.character(tabla[tabla$category_tbrc == industry, "new_category"][1,1])
  return(industry_reclassifed)
}

procesaReclasificaIndustriaEmpresa2 <- function(lista_entidades_principales_in, ruta_tabla_reclasificacion = "./tablas_input/auxiliares/recategorizacion_industrias_empresas_2.xlsx"){
  msg <- glue::glue("Entrando en la función procesaReclasificaIndustriaEmpresa2")
  log_trace_multi(msg, namespaces = c("auxiliares"))
  lista_empresas_principales <- lista_entidades_principales_in
  tabla_reclasificacion <- read_excel(ruta_tabla_reclasificacion)
  
  for(i in 1:nrow(tabla_reclasificacion)){
    empresa_name <- tabla_reclasificacion$name[i]
    
    if (lista_empresas_principales$verificaEntidadLista(empresa_name)){
      empresa <- lista_empresas_principales$extraeEntidadLista(empresa_name)
      empresa$industry_reclassified_2 <- tabla_reclasificacion$industry_reclassified_2[i]
      
    } else {
      stop(paste("La entidad: ",empresa_name," no ha sido encontrada en la lista de industry_reclassified_2\n", sep = ""))
    }
    
  }
  
  return(lista_empresas_principales)
}

#esta función conviertre una lista de valores de una moneda a dólares ajustado por inflación según año base
reconvierteValorUSD <- function(currency, anho_base, anho_recoleccion, list_valor){
  msg <- glue::glue("Entrando en la función reconvierteValorUSD")
  log_trace_multi(msg, namespaces = c("auxiliares"))
  tabla_monedas <- read_excel("./tablas_input/auxiliares/plantilla_monedas_rellenado.xlsx", sheet="tabla_monedas")
  
  list_valor_ajustado <- list_valor
  for (i in 1:length(list_valor)){
    #tasa respecto al dólar, 1/valor de tabla.
    tasa_cambio <- round(1/as.numeric(tabla_monedas[tabla_monedas$monedas==currency, colnames(tabla_monedas)==anho_recoleccion]), 4)
    #dividir por el año de recolección y multiplicar por el año base
    tasa_inflacion <- round(as.numeric(tabla_monedas[1,colnames(tabla_monedas)==anho_base])/as.numeric(tabla_monedas[1,colnames(tabla_monedas)==anho_recoleccion]),20)
    list_valor_ajustado[i] <- round(as.numeric(list_valor[i])*tasa_cambio*tasa_inflacion,0)
  }
  return(list_valor_ajustado)
}

#la misma función que reconvierteValorUSD pero pensada para tibbles.
reconvierteValorUSDArray <- function(currency, anho_base, df_valor){
  msg <- glue::glue("Entrando en la función reconvierteValorUSDArray")
  log_trace_multi(msg, namespaces = c("auxiliares"))
  tabla_monedas <- read_excel("./tablas_input/auxiliares/plantilla_monedas_rellenado.xlsx", sheet="tabla_monedas")
  df_valor_ajustado <- df_valor
  for(i in 1:nrow(df_valor)){
    row <- df_valor[i,]
    row_ajustado <- row
    row_ajustado[4] <- reconvierteValorUSD(currency, anho_base, as.numeric(row[3]), as.numeric(row[4]))
    df_valor_ajustado[i,] <- row_ajustado
  }
  return(df_valor_ajustado)
}

#esta función convierte una lista de valores de una moneda a otra según la tabla de referencia, no hay ajuste de año base.
reconvierteValorInOut <- function(currency_in, currency_out, anho_recoleccion, list_valor){
  msg <- glue::glue("Entrando en la función reconvierteValorInOut")
  log_trace_multi(msg, namespaces = c("auxiliares"))
  tabla_monedas <- read_excel("./tablas_input/auxiliares/plantilla_monedas_rellenado.xlsx", sheet="tabla_monedas")
  list_valor_out <- list_valor
  for(i in 1:length(list_valor)){
    #tasa entre divisas diferentes
    tasa_cambio <- round(as.numeric(tabla_monedas[tabla_monedas$monedas==currency_out, colnames(tabla_monedas)==anho_recoleccion])/as.numeric(tabla_monedas[tabla_monedas$monedas==currency_in, colnames(tabla_monedas)==anho_recoleccion]),20)
    list_valor_out[i] <- round(as.numeric(list_valor[i])*tasa_cambio,0)
  }
  return(list_valor_out)
}

#la misma función que reconvierteValorInOut pero pensada para tibbles.
reconvierteValorInOutArray <- function(currency_in, currency_out, df_valor){
  msg <- glue::glue("Entrando en la función reconvierteValorInOutArray")
  log_trace_multi(msg, namespaces = c("auxiliares"))
  tabla_monedas <- read_excel("./tablas_input/auxiliares/plantilla_monedas_rellenado.xlsx", sheet="tabla_monedas")
  df_valor_ajustado <- df_valor
  for(i in 1:nrow(df_valor)){
    row <- df_valor[i,]
    row_ajustado <- row
    row_ajustado[4] <- reconvierteValorInOut(currency_in, currency_out, as.numeric(row[3]), as.numeric(row[4]))
    df_valor_ajustado[i,] <- row_ajustado
  }
  
  return(df_valor_ajustado)
}


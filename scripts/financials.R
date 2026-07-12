#Procesamiento de las tablas de financials
library(tidyverse)
library(dplyr)
library(readxl)
library(lubridate)
library(purrr)
library(grid)
library(gridExtra)
library(kableExtra)
library(patchwork)
library(ggplot2)
library(cowplot)
library(ggpubr)
library(stringr)



#Funciones auxiliares específicas
detectaFormatoTabla <- function(tabla_in){
  tabla <- tabla_in
  nombre_empresa <- NA
  datos_fechas <- NA
  formato <- NA
  if (tabla[1,1] == "Company Name"){
    colnames(tabla) <-  c("variables",paste("col_",2:ncol(tabla), sep = ""))
    nombre_empresa <- extraeNombreEmpresa(tabla, 1, 2, "(")
    datos_fechas <- identificaFechas(tabla)
    formato <- 1
  } else if("Period End Date" %in% unlist(tabla[,1])){
    tabla[1,] <- as.list(colnames(tabla))
    colnames(tabla) <- c("variables",paste("col_",2:ncol(tabla), sep = ""))
    nombre_empresa <- extraeNombreEmpresa(tabla, 1, 1, "|")
    datos_fechas <- identificaFechas(tabla)
    if("Bank Total Revenue" %in% unlist(tabla$variables)){
      formato <- 4
    } else{
      formato <- 2
    }
  } else if("Period End Date" %in% unlist(tabla[,2])){
    tabla[1,] <- as.list(colnames(tabla))
    colnames(tabla) <- c("col_1","variables",paste("col_",3:ncol(tabla), sep = ""))
    nombre_empresa <- extraeNombreEmpresa(tabla, 1, 1, "|")
    datos_fechas <- identificaFechas(tabla)
    formato <- 3
  } else {
    stop(paste("Formato no encontrado"))
  }
  
  return(list(tabla, nombre_empresa, datos_fechas, formato))
}

extraeNombreEmpresa <- function(tabla, row_id, col_id, character_delim){
  celda_nombre <- as.character(tabla[row_id, col_id])
  cadena <- paste("\\s*\\", character_delim,".*$", sep = "")
  nombre_empresa <- sub(cadena, "", celda_nombre)
  return(nombre_empresa)  
}
identificaFechas <- function(tabla){
  # VIVA CHATGPT
  # 1. Localizar fila que contiene varios años (cualquier año >=1900 por ejemplo)
  matches <- apply(tabla, 1, function(x) {
    sum(!is.na(as.numeric(x)) & as.numeric(x) >= 1980 & as.numeric(x) <= 2024)
  })
  fila_anhos <- which.max(matches)
  
  # 2. Extraer esa fila como vector
  fila <- unlist(tabla[fila_anhos, ])
  
  # convertir a numérico (los no numéricos quedan NA)
  valores <- suppressWarnings(as.numeric(fila))
  
  # 3. Filtrar solo los años válidos
  anhos <- valores[!is.na(valores) & valores >= 1980 & valores <= 2024]
  
  # 4. Año mínimo y máximo
  anho_min <- min(anhos)
  anho_max <- max(anhos)
  
  # 5. Números de columna de esos años dentro del data frame
  col_min <- which(valores == anho_min)
  col_max <- which(valores == anho_max)
  
  datos_fechas <- list(
    fila_anhos = fila_anhos,
    anho_min   = anho_min,
    anho_max   = anho_max,
    col_min    = col_min,
    col_max    = col_max,
    anhos      = anhos
  )
  return(datos_fechas)
}

extraeVariables <- function(nombre_empresa, nombre_variable, tabla, datos_fechas){
  if(nombre_variable == "Standarized Currency"){
    currency <- as.character(tabla[12,2])
    return(currency)
  } else{
    num_fila_variable <- max(which(tabla$variables == nombre_variable))
    anhos_totales <- 1990:2024
    if(is.infinite(num_fila_variable)){
      #stop("extraeVariables: variable no encontrada")
      fila_variable <- as.list(rep(NA,length(1990:2024)))
      names(fila_variable) <- anhos_totales
    } else{
      fila_variable <- tabla[num_fila_variable, datos_fechas$col_max:datos_fechas$col_min]
      anhos <- as.vector(tabla[datos_fechas$fila_anhos, datos_fechas$col_max:datos_fechas$col_min])
      names(fila_variable) <- anhos
      anhos_faltan <- setdiff(anhos_totales,anhos)
      fila_variable_faltan <- as.list(rep(NA,length(anhos_faltan)))
      names(fila_variable_faltan) <- anhos_faltan
      fila_variable <- unlist(c(fila_variable, fila_variable_faltan))
      fila_variable <- fila_variable[order(as.numeric(names(fila_variable)))]
    }
    fila_variable <- as.list(c(empresa=nombre_empresa, variable = nombre_variable, fila_variable))
    
    return(fila_variable)
  }
}

procesa_valores <- function(tabla_procesada_in){
  tabla_procesada <- tabla_procesada_in
  tabla_procesada$valor[!grepl("\\(", tabla_procesada$valor)] <- gsub("\\,", "", tabla_procesada$valor[!grepl("\\(", tabla_procesada$valor)])
  tabla_procesada$valor[grepl("\\(", tabla_procesada$valor)] <- gsub(",(?=[^,]*$)", ".", tabla_procesada$valor[grepl("\\(", tabla_procesada$valor)], perl = TRUE )
  tabla_procesada$valor[grepl("\\(", tabla_procesada$valor)] <- gsub("\\,", "", tabla_procesada$valor[grepl("\\(", tabla_procesada$valor)])
  tabla_procesada$valor[grepl("\\(", tabla_procesada$valor)] <- round(as.numeric(gsub("^\\(([-0-9\\.]+)\\)$", "-\\1", tabla_procesada$valor[grepl("\\(", tabla_procesada$valor)])),2)
  tabla_procesada$valor <- as.numeric(tabla_procesada$valor)
  return(tabla_procesada)
}


#Funciones de carga
##Income Statement
cargaFinancialsIncomeStatement <- function(ruta_income_statement_excel="./tablas_input/Financials/income_statement/012_IncomeStatement_AluarAluminioArgentinoSAIC.xlsx"){
  #leo la tabla
  income_statement_excel <- read_excel(ruta_income_statement_excel)
  cat("se cargó el archivo de financials input stament: ",ruta_income_statement_excel,"\n", sep="")
  #detecto el formato de la tabla
  lista_formato <- detectaFormatoTabla(income_statement_excel)
  income_statement_excel <- lista_formato[[1]]
  nombre_empresa <- lista_formato[[2]]
  datos_fechas <- lista_formato[[3]]
  formato <- lista_formato[[4]]
  #extraigo las variables
  if(formato == 1){
    total_revenue <- extraeVariables(nombre_empresa, "Revenue from Business Activities - Total", income_statement_excel, datos_fechas)
    net_income_after_taxes <- extraeVariables(nombre_empresa, "Net Income after Tax", income_statement_excel, datos_fechas)
  } else if(formato == 4) {
    total_revenue <- extraeVariables(nombre_empresa, "Bank Total Revenue", income_statement_excel, datos_fechas)
    net_income_after_taxes <- extraeVariables(nombre_empresa, "Net Income After Taxes", income_statement_excel, datos_fechas)
  }
  else{
    total_revenue <- extraeVariables(nombre_empresa, "Total Revenue", income_statement_excel, datos_fechas)
    net_income_after_taxes <- extraeVariables(nombre_empresa, "Net Income After Taxes", income_statement_excel, datos_fechas)
  }  
  #ajusto nombres
  total_revenue$variable <- "total revenue"
  net_income_after_taxes$variable <- "net income after taxes"
  #junto las variables extraídas
  tabla_procesada <- bind_rows(total_revenue, net_income_after_taxes)
  tabla_procesada <- tabla_procesada %>% pivot_longer(cols=!c(empresa, variable), names_to = "anho", values_to = "valor")
  tabla_procesada <- procesa_valores(tabla_procesada)
  
  
  return(tabla_procesada)
  
  
}

cargaFinancialsValuation <- function(ruta_valuation_excel="./tablas_input/Financials/valuation/012_Valuation_AluarAluminioArgentinoSAIC.xlsx"){
  #leo la tabla
  valuation_excel <- read_excel(ruta_valuation_excel)
  cat("se cargó el archivo de valuation: ",ruta_valuation_excel,"\n", sep="")
  #detecto el formato de la tabla
  lista_formato <- detectaFormatoTabla(valuation_excel)
  valuation_excel <- lista_formato[[1]]
  nombre_empresa <- lista_formato[[2]]
  datos_fechas <- lista_formato[[3]]
  #extraigo variables
  currency <- extraeVariables(nombre_empresa, "Standarized Currency", valuation_excel, datos_fechas)
  market_capitalization <- extraeVariables(nombre_empresa, "Market Capitalization", valuation_excel, datos_fechas)
  enterprise_value <- extraeVariables(nombre_empresa, "Enterprise Value", valuation_excel, datos_fechas)
  #ajusto nombres
  market_capitalization$variable <- "market capitalization"
  enterprise_value$variable <- "enterprise value"
  #armo la tabla
  tabla_procesada <- bind_rows(market_capitalization, enterprise_value) %>% pivot_longer(cols=!c(empresa, variable), names_to = "anho", values_to = "valor")
  tabla_procesada <- procesa_valores(tabla_procesada)
  
  return(list(currency,tabla_procesada))
}

cargaFinancialsBalanceSheet <- function(ruta_balance_sheet_excel = "./tablas_input/Financials/balance_sheet/012_BalanceSheet_AluarAluminioArgentinoSAIC.xlsx"){
  #leo la tabla
  balance_sheet_excel <- read_excel(ruta_balance_sheet_excel)
  cat("se cargó el archivo de valuation: ",ruta_balance_sheet_excel,"\n", sep="")
  #detecto el formato de la tabla
  lista_formato <- detectaFormatoTabla(balance_sheet_excel)
  balance_sheet_excel <- lista_formato[[1]]
  nombre_empresa <- lista_formato[[2]]
  datos_fechas <- lista_formato[[3]]
  employees <- extraeVariables(nombre_empresa, "Employees - Full-Time/Full-Time Equivalents - Period End", balance_sheet_excel, datos_fechas)
  total_capital <- extraeVariables(nombre_empresa, "Total Capital", balance_sheet_excel, datos_fechas)
  working_capital <- extraeVariables(nombre_empresa, "Working Capital", balance_sheet_excel, datos_fechas)
  land_buildings <- extraeVariables(nombre_empresa, "Land & Buildings - Net", balance_sheet_excel, datos_fechas)
  plant_machinery_equipment <- extraeVariables(nombre_empresa, "Plant, Machinery & Equipment - Net", balance_sheet_excel, datos_fechas)
  construction_in_progress <- extraeVariables(nombre_empresa, "Construction in Progress - Net", balance_sheet_excel, datos_fechas)
  tangible_total_equity <- extraeVariables(nombre_empresa, "Tangible Total Equity", balance_sheet_excel, datos_fechas)
  total_assets <- extraeVariables(nombre_empresa, "Total Assets", balance_sheet_excel, datos_fechas)
  debt_total <- extraeVariables(nombre_empresa, "Debt - Total", balance_sheet_excel, datos_fechas)
  loans_receivables <- extraeVariables(nombre_empresa, "Loans & Receivables - Total", balance_sheet_excel, datos_fechas)
  derivative_finantial_instruments <- extraeVariables(nombre_empresa, "Derivative Financial Instruments - Hedging - Total", balance_sheet_excel, datos_fechas)
  right_use_tangible_assets <- extraeVariables(nombre_empresa, "Right of Use Tangible Assets - Total - Net", balance_sheet_excel, datos_fechas)
  #ajusto nombres
  employees$variable <- "employees"
  total_capital$variable <- "total capital"
  working_capital$variable <- "working capital"
  land_buildings$variable <- "land and buildings"
  plant_machinery_equipment$variable <- "plant machinery and equipment"
  construction_in_progress$variable <- "construction in progress"
  tangible_total_equity$variable <- "tangible total equity"
  total_assets$variable <- "total assets"
  debt_total$variable <- "total debt"
  loans_receivables$variable <- "loans and receivables"
  derivative_finantial_instruments$variable <- "derivative financial instruments"
  right_use_tangible_assets$variable <- "right of use tangible assets"
  #proceso la tabla
  tabla_procesada <- bind_rows(employees,
                               total_capital,
                               working_capital,
                               land_buildings,
                               plant_machinery_equipment,
                               construction_in_progress,
                               tangible_total_equity,
                               total_assets,
                               debt_total,
                               loans_receivables,
                               derivative_finantial_instruments,
                               right_use_tangible_assets
                              )
  
  tabla_procesada <- tabla_procesada %>% pivot_longer(cols=!c(empresa, variable), names_to = "anho", values_to = "valor")
  tabla_procesada <- procesa_valores(tabla_procesada)
  
  return(tabla_procesada)
}

cargaFinancialsCashFlow <- function(ruta_cash_flow_excel = "./tablas_input/Financials/cash_flow/012_CashFlow_AluarAluminioArgentinoSAIC.xlsx"){
  #leo la tabla
  cash_flow_excel <- read_excel(ruta_cash_flow_excel)
  cat("se cargó el archivo de valuation: ",ruta_cash_flow_excel,"\n", sep="")
  #detecto el formato de la tabla
  lista_formato <- detectaFormatoTabla(cash_flow_excel)
  cash_flow_excel <- lista_formato[[1]]
  nombre_empresa <- lista_formato[[2]]
  #if(nombre_empresa == "Centrais Eletricas Brasileiras 3 13 23"){stop(paste("El archivo con el nombre malo es ",ruta_income_statement_excel, sep = ""))}
  datos_fechas <- lista_formato[[3]]
  formato <- lista_formato[[4]]
  #extraigo las variables
  if(formato == 1){
    dividends_paid <- extraeVariables(nombre_empresa, "Dividends Paid - Cash - Total - Cash Flow", cash_flow_excel, datos_fechas)
    dividends_received <- extraeVariables(nombre_empresa, "Interest & Dividends - Received - Cash Flow - Supplemental", cash_flow_excel, datos_fechas)
  } else if(formato == 4) {
    stop(paste("Formato no compatible para el archivo: ", ruta_cash_flow_excel,sep = ""))
    
  } else if(formato == 2){
    dividends_paid <- extraeVariables(nombre_empresa, "Total Cash Dividends Paid", cash_flow_excel, datos_fechas)
    dividends_received <- dividends_paid
    dividends_received[-1] <- NA
    
  } else if(formato == 3){  
    dividends_paid <- extraeVariables(nombre_empresa, "Dividends", cash_flow_excel, datos_fechas)
    dividends_received <- extraeVariables(nombre_empresa, "Dividends Received - Cash Flow", cash_flow_excel, datos_fechas)
    
  }
  #ajusto nombres
  dividends_paid$variable <- "dividends paid"
  dividends_received$variable <- "dividends received"
  
  tabla_procesada <- bind_rows(dividends_paid, dividends_received)
  tabla_procesada <- tabla_procesada %>% pivot_longer(cols=!c(empresa, variable), names_to = "anho", values_to = "valor")
  tabla_procesada <- procesa_valores(tabla_procesada)
  
  return(tabla_procesada)
}


#Funciones de procesamiento

procesaArchivoFinancials <- function(ruta_archivo, lista_empresas_principales_in, tipo_financials="automatic"){
  lista_empresas_principales <- lista_empresas_principales_in
  
  if(tipo_financials == "automatic"){
    tipo_financials <- str_extract(ruta_archivo, regex("(IncomeStatement|BalanceSheet|Valuation|CashFlow)", ignore_case = TRUE))
  
    }
  currency <- "undefined"
  empresa_principal <- NA
  tabla_financials <- NA
  
  if(grepl("IncomeStatement", tipo_financials, ignore.case = TRUE)){
    tabla_financials <- cargaFinancialsIncomeStatement(ruta_income_statement_excel = ruta_archivo)
    nombre_empresa <- as.character(tabla_financials[1,1])
    empresa_principal <- lista_empresas_principales$extraeCreaEntidadLista(nombre_empresa)
    
  }else if(grepl("Valuation", tipo_financials, ignore.case = TRUE)){
    lista_valuation <- cargaFinancialsValuation(ruta_valuation_excel =  ruta_archivo)
    currency <- lista_valuation[[1]]
    tabla_financials <- lista_valuation[[2]]
    nombre_empresa <- as.character(tabla_financials[1,1])
    empresa_principal <- lista_empresas_principales$extraeCreaEntidadLista(nombre_empresa)
    
  }else if(grepl("BalanceSheet", tipo_financials, ignore.case = TRUE)){
    tabla_financials <- cargaFinancialsBalanceSheet(ruta_balance_sheet_excel =  ruta_archivo)
    nombre_empresa <- as.character(tabla_financials[1,1])
    #control
    empresa_principal <- lista_empresas_principales$extraeCreaEntidadLista(nombre_empresa)
    empresa_principal$aumentaFinancials(tabla_financials)
    
  }else if(grepl("CashFlow", tipo_financials, ignore.case = TRUE)){
    tabla_financials <- cargaFinancialsCashFlow(ruta_cash_flow_excel = ruta_archivo)
    nombre_empresa <- as.character(tabla_financials[1,1])
    empresa_principal <- lista_empresas_principales$extraeCreaEntidadLista(nombre_empresa)
    
  }else{
    
  }
  return(list(empresa_principal, tabla_financials, currency))
}

procesaFinancials <- function(ruta_financials = "./tablas_input/Financials", lista_empresas_principales_in){
  
  ruta_income_statement <- paste0(ruta_financials,"/income_statement")
  ruta_valuation <- paste0(ruta_financials,"/valuation")
  ruta_balance_sheet <- paste0(ruta_financials,"/balance_sheet")
  ruta_cash_flow <- paste0(ruta_financials,"/cash_flow")
  
  archivos_income_statement <- list.files(ruta_income_statement, full.names = TRUE, recursive = TRUE)
  archivos_income_statement <- archivos_income_statement[grepl(".*IncomeStatement.*", archivos_income_statement, ignore.case = TRUE)]
  archivos_valuation <- list.files(ruta_valuation, full.names = TRUE, recursive = TRUE)
  archivos_valuation <- archivos_valuation[grepl(".*Valuation.*", archivos_valuation, ignore.case = TRUE)]
  archivos_balance_sheet <- list.files(ruta_balance_sheet, full.names = TRUE, recursive = TRUE)
  archivos_balance_sheet <- archivos_balance_sheet[grepl(".*BalanceSheet*", archivos_balance_sheet, ignore.case = TRUE)]
  archivos_cash_flow <- list.files(ruta_cash_flow, full.names = TRUE, recursive = TRUE)
  archivos_cash_flow <- archivos_cash_flow[grepl(".*CashFlow*", archivos_cash_flow, ignore.case = TRUE)]
  
  archivos_total <- c(archivos_income_statement, archivos_balance_sheet, archivos_valuation, archivos_cash_flow)
  lista_empresas_principales <- lista_empresas_principales_in
  
  for(archivo in archivos_total){
    
    lista <- procesaArchivoFinancials(ruta_archivo = archivo, lista_empresas_principales_in = lista_empresas_principales)
    empresa_principal <- lista[[1]]
    tabla_financials <- lista[[2]]
    currency <- lista[[3]]
    empresa_principal$aumentaFinancials(tabla_financials)
    if (currency != "undefined"){empresa_principal$currency <- currency}
  }
  
  return(lista_empresas_principales)
}

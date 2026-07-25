#Procesamiento de las tablas de shareholders
library(tidyverse)
library(R6)
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

#Funciones auxiliares específicas


#Funciones de carga

cargaShareholders <- function(shareholders_input_excel){
  shareholders_excel <- read_excel(shareholders_input_excel)
  cat("Cargando el archivo shareholders: ",shareholders_input_excel,"\n", sep = "")
  empresa <- colnames(shareholders_excel)[2]
  shareholders_excel <- shareholders_excel[-c(1:5),]
  shareholders_excel$empresa <- empresa
  shareholders_excel <- shareholders_excel %>% select(empresa, everything())
  colnames(shareholders_excel) <- c("empresa_principal","ranking","investor_name","outstanding","position",
                                    "position_change","value","filing_date","filing_source","investor_type","investor_subtype",
                                    "equity_assets","investment_style","turnover","city","country_region")
  stopifnot(shareholders_excel$empresa_principal != "...2")
  ###formato de las columnas del excel
  shareholders_excel$filing_date <- xldate(shareholders_excel$filing_date)
  columnas_numeros  <- c("outstanding","position","position_change","value","equity_assets")
  shareholders_excel <- shareholders_excel %>% mutate(across(all_of(columnas_numeros), as.numeric))
  
  return(shareholders_excel)
}

cargaShareholdersArray <- function(directorio = "./tablas_input/Shareholders"){
  archivos_shareholders <- list.files(directorio, full.names = TRUE, recursive = TRUE)
  archivos_shareholders <- archivos_shareholders[grepl(".*shareholders.*", archivos_shareholders, ignore.case = TRUE)]
  tablas <- lapply(archivos_shareholders, cargaShareholders)
  return(bind_rows(tablas))
}

#Función de procesamiento

procesaShareholders <- function (shareholders_input){
  #parámetros iniciales de la función
  shareholders <- shareholders_input
  entidad_arranque <- Entidad$new(name = "arranque")
  lista_entidades <- ListaEntidades$new("lista_procesa_shareholders", list(entidad_arranque))
  
  total_iteraciones <- nrow(shareholders)
  for (i in 1:total_iteraciones){
    #cargo empresa principal
    empresa_principal <- lista_entidades$extraeCreaEntidadLista(shareholders$empresa_principal[i], tipo = "empresa")
    empresa_principal$entidad_principal <- "si"
    #cargo accionista
    accionista_entidad <- lista_entidades$extraeCreaEntidadLista(shareholders$investor_name[i], tipo = shareholders$investor_subtype[i])
    #creo al accionista con sus propiedades
    accionista <- Accionista$new(entidad = accionista_entidad, ranking = shareholders$ranking[i], outstanding = shareholders$outstanding[i], position = shareholders$position[i],
                                 position_change = shareholders$position_change[i], value = shareholders$value[i], filing_date = shareholders$filing_date[i],
                                 filing_source = shareholders$filing_source[i], investor_type = shareholders$investor_type[i],
                                 investor_subtype = shareholders$investor_subtype[i], equity_assets = shareholders$equity_assets[i],
                                 investment_style = shareholders$investment_style[i], turnover = shareholders$turnover[i], city = shareholders$city[i],
                                 country_region = shareholders$country_region[i])
    
    empresa_principal$aumentaAccionistas(accionista)
    msg <- glue::glue("Shareholders: procesada la iteración: ",i," del total de: ", total_iteraciones, "\n", sep ="")
    log_trace_multi(msg, namespaces = c("global", "accionistas"))
    
  }
  lista_entidades$entidades <- lista_entidades$entidades[-1]
  lista_empresas_principales <- lista_entidades$extraeEntidadesPrincipales(valor = "si", nombre = "principales")
  lista_accionistas_entidades <- lista_entidades$extraeEntidadesPrincipales(valor = "no", nombre = "accionistas")
  
  return(list(lista_empresas_principales, lista_accionistas_entidades))  
}





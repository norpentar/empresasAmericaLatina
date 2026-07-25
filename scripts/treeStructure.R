#Procesamiento de las tablas de treestructure
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

buscaNivelTreeStructure <- function(fila_in){
  fila <- fila_in
  nivel <- which(sapply(fila, \(x) !is.na(x)))
  return(list(fila[[nivel]], nivel))
}

#Funciones de carga

cargaTreeStructure <- function(treeStructrue_input_excel){
  treeStructure_excel <- read_excel(treeStructrue_input_excel)
  cat("Cargando el archivo TreeStructure ",treeStructrue_input_excel,"\n")
  immediate_parent <- as.character(treeStructure_excel[2,6])
  ultimate_parent <- as.character(treeStructure_excel[2,9])
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
  ###evito loops infinitos en la recursión (sacaNodos())
  #browser()
  #verifica <- verificaLoops(treeStructure_excel[c("level_0", "level_1", "level_2", "level_3", "level_4", "level_5", "level_6","level_7","level_8","level_9")])
  #if(verifica[[1]]){
  #stop(paste0("Se ha detectado un loop con el nombre: ", verifica[[2]]))
  #}
  
  ###formateo las columnas
  treeStructure_excel$incorporated_date[!is.na(treeStructure_excel$incorporated_date) & (nchar(treeStructure_excel$incorporated_date) == 4)] <- paste0("Jan 1, ", treeStructure_excel$incorporated_date[!is.na(treeStructure_excel$incorporated_date) & (nchar(treeStructure_excel$incorporated_date) == 4)])
  treeStructure_excel$incorporated_date <- mdy(treeStructure_excel$incorporated_date) #uso está función de lubridate para no tener que cambiar el idioma en R.
  treeStructure_excel$total_revenue <- as.numeric(gsub("[\\$,]","",treeStructure_excel$total_revenue))
  treeStructure_excel$employees <- as.numeric(gsub("[\\$,]","",treeStructure_excel$employees))
  treeStructure_excel$market_cap <- as.numeric(gsub("[\\$,]","",treeStructure_excel$market_cap))
  treeStructure_excel$ownership_per <- as.numeric(gsub("[\\$,%]","",treeStructure_excel$ownership_per))
  columnas_numeros <- c("issued_bonds","outstanding_loans","cds","commercial_paper","futures","options","warrants","equities", "certificate_deposits","preference_shares","investment_certificates","funds_count")
  treeStructure_excel <- treeStructure_excel %>% mutate(across(all_of(columnas_numeros), as.numeric))
  
  ###añado parents
  treeStructure_excel$immediate_parent <- NA
  treeStructure_excel$immediate_parent[1] <- immediate_parent
  treeStructure_excel$ultimate_parent <- NA
  treeStructure_excel$ultimate_parent[1] <- ultimate_parent
  
  return(treeStructure_excel)
}

cargaTreeStructureArray <- function(directorio = "./tablas_input/TreeStructure/"){
  archivos_treeStructure <- list.files(directorio, full.names = TRUE, recursive = TRUE)
  archivos_treeStructure <- archivos_treeStructure[grepl(".*treeStructure*", archivos_treeStructure, ignore.case = TRUE)]
  tablas <- lapply(archivos_treeStructure, cargaTreeStructure)
  return(bind_rows(tablas))
  
  
}

#Función de procesamiento

procesaTreeStructure <- function (treeStructure_input, lista_empresas_principales_in){
  #parámetros iniciales
  treeStructure <- treeStructure_input
  nivel_iteracion <- 0 #para ver el nivel que leo en cada iteración (0:9)
  lista_empresas <- lista_empresas_principales_in
  empresas_niveles_lista <- list()
  
  total_iteraciones <- nrow(treeStructure)
  for (i in 1:total_iteraciones){
    lista <- buscaNivelTreeStructure(treeStructure[i,c("level_0", "level_1", "level_2", "level_3", "level_4", "level_5", "level_6","level_7","level_8","level_9")])
    string <- lista[[1]]
    nivel_iteracion <- lista[[2]]
    if(nivel_iteracion - 1 == 0){
      #empresas principales
      empresa_principal <- lista_empresas$extraeCreaEntidadLista(string)
      ##pongo el resto de propiedades de la empresa
      empresa_principal$type <- treeStructure$type[i]
      empresa_principal$country_region <- treeStructure$country_region[i]
      empresa_principal$industry <- treeStructure$industry[i]
      empresa_principal$permID <- treeStructure$permID[i]
      empresa_principal$incorporated_country <- treeStructure$incorporated_country[i]
      empresa_principal$incorporated_date <- treeStructure$incorporated_date[i]
      empresa_principal$total_revenue <- treeStructure$total_revenue[i]
      empresa_principal$employees <- treeStructure$employees[i]
      empresa_principal$market_cap <- treeStructure$market_cap[i]
      empresa_principal$implied_rating <- treeStructure$implied_rating[i]
      empresa_principal$moody_rating <- treeStructure$moody_rating[i]
      empresa_principal$fitch_rating <- treeStructure$fitch_rating[i]
      empresa_principal$ric <- treeStructure$ric[i]
      empresa_principal$outstanding_loans <- treeStructure$outstanding_loans[i]
      empresa_principal$cds <-treeStructure$cds[i]
      empresa_principal$commercial_paper <- treeStructure$commercial_paper[i]
      empresa_principal$futures <- treeStructure$futures[i]
      empresa_principal$options <- treeStructure$options[i]
      empresa_principal$warrants <- treeStructure$warrants[i]
      empresa_principal$equities <- treeStructure$equities[i]
      empresa_principal$certificate_deposits <- treeStructure$certificate_deposits[i]
      empresa_principal$preference_shares <- treeStructure$preference_shares[i]
      empresa_principal$investment_certificates <- treeStructure$investment_certificates[i]
      empresa_principal$funds_count <- treeStructure$funds_count[i]
      empresa_principal$pe_backed_status <- treeStructure$pe_backed_status[i]
      empresa_principal$industry_reclassified <- reclasificaIndustriaEmpresa(treeStructure$industry[i])
      empresa_principal$entidad_principal <- "si"
      empresa_principal$immediate_parent <- treeStructure$immediate_parent[i]
      empresa_principal$ultimate_parent <- treeStructure$ultimate_parent[i]
      
      empresas_niveles_lista[[nivel_iteracion]] <- empresa_principal
      next
    }
    #filiales
    empresa_filial <- lista_empresas$extraeCreaEntidadLista(string) ##al crear la filial actualiza la lista.
    ##pongo el resto de propiedades de la empresa
    empresa_filial$type <- treeStructure$type[i]
    empresa_filial$country_region <- treeStructure$country_region[i]
    empresa_filial$industry <- treeStructure$industry[i]
    empresa_filial$permID <- treeStructure$permID[i]
    empresa_filial$incorporated_country <- treeStructure$incorporated_country[i]
    empresa_filial$incorporated_date <- treeStructure$incorporated_date[i]
    empresa_filial$total_revenue <- treeStructure$total_revenue[i]
    empresa_filial$employees <- treeStructure$employees[i]
    empresa_filial$market_cap <- treeStructure$market_cap[i]
    empresa_filial$implied_rating <- treeStructure$implied_rating[i]
    empresa_filial$moody_rating <- treeStructure$moody_rating[i]
    empresa_filial$fitch_rating <- treeStructure$fitch_rating[i]
    empresa_filial$ric <- treeStructure$ric[i]
    empresa_filial$outstanding_loans <- treeStructure$outstanding_loans[i]
    empresa_filial$cds <-treeStructure$cds[i]
    empresa_filial$commercial_paper <- treeStructure$commercial_paper[i]
    empresa_filial$futures <- treeStructure$futures[i]
    empresa_filial$options <- treeStructure$options[i]
    empresa_filial$warrants <- treeStructure$warrants[i]
    empresa_filial$equities <- treeStructure$equities[i]
    empresa_filial$certificate_deposits <- treeStructure$certificate_deposits[i]
    empresa_filial$preference_shares <- treeStructure$preference_shares[i]
    empresa_filial$investment_certificates <- treeStructure$investment_certificates[i]
    empresa_filial$funds_count <- treeStructure$funds_count[i]
    empresa_filial$pe_backed_status <- treeStructure$pe_backed_status[i]
    empresa_filial$industry_reclassified <- reclasificaIndustriaEmpresa(treeStructure$industry[i])
    
    ###creo la filial
    filial <- Filial$new(empresa = empresa_filial, ownership_per = treeStructure$ownership_per[i],
                         relationship_type = treeStructure$relationship_type[i], nivel_filial = nivel_iteracion
                        )
    
    empresa_principal <- empresas_niveles_lista[[nivel_iteracion - 1]]
    empresa_principal$aumentaFiliales(filial)
    empresas_niveles_lista[[nivel_iteracion]] <- empresa_filial
    msg <- glue::glue("TreeStructure: procesada la iteración: ",i, " del total de: ", total_iteraciones,"\n", sep="")
    log_trace_multi(msg, namespaces = c("global","treestructure"))
  }
  
  lista_empresas_principales <- lista_empresas$extraeEntidadesPrincipales(valor = "si")
  lista_empresas_filiales <- lista_empresas$extraeEntidadesPrincipales(valor = "no")
  
  return(list(lista_empresas_principales, lista_empresas_filiales))
}





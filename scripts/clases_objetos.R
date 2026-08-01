#Establecimiento de las clases de objetos con las que cargaremos los datos
#Para la tabla de accionistas: accionista, (empresa y persona)

library(R6)
library(tidyverse)
library(dplyr)
library(stringr)
library(purrr)
#library(unix)
library(grid)
library(gridExtra)
library(kableExtra)
library(jsonlite)
library(visNetwork)
library(bolt4jr)
library(reticulate)
library(here)

#Entidad
Entidad <- R6Class("Entidad",
  public = list(
    initialize = function (name, tag_1="entidad", tag_2="nulo", tag_3="nulo", entidad_principal="no"){
      name <- gsub("‘|’|'|-|–", " ", name)
      private$.name <- name
      private$.tag_1 <- tag_1
      private$.tag_2 <- tag_2
      private$.tag_3 <- tag_3
      private$.entidad_principal <- entidad_principal
    },
    print = function(...){
      cat("El nombre da entidad es: ", private$.name, "\n", sep="")
    },
    sacaPropiedades = function(){
      tibble(name = private$.name)
    },
    sacaNodosRecursivo = function(){
      return(list(self))
    }
  )   ,                 
  active = list(
    name = function(value){
      if (missing(value)){
        private$.name
      }
      else{
        stopifnot(is.character(value))
        value <- gsub("‘|’|'|-|–", " ", value)
        private$.name <- value
        invisible(self)
      }
    },
    tag_1 = function(value){
      if (missing(value)){
        private$.tag_1
      }
      else{
        stopifnot(is.character(value))
        private$.tag_1 <- value
        invisible(self)
      }
    },
    tag_2 = function(value){
      if (missing(value)){
        private$.tag_2
      }
      else{
        stopifnot(is.character(value))
        private$.tag_2 <- value
        invisible(self)
      }
    },
    tag_3 = function(value){
      if (missing(value)){
        private$.tag_3
      }
      else{
        stopifnot(is.character(value))
        private$.tag_3 <- value
        invisible(self)
      }
    },
    entidad_principal = function(value){
      if (missing(value)){
        private$.entidad_principal
      }
      else{
        stopifnot(is.character(value))
        private$.entidad_principal <- value
        invisible(self)
      }
    }
  ),
  private = list(
    .name = NA,
    .tag_1 = "entidad",
    .tag_2 = "nulo",
    .tag_3 = "nulo",
    .entidad_principal = "no"
  )
)


#Persona
Persona <- R6Class("Persona",
 inherit = Entidad,                 
 public = list(
    initialize = function (name, age = NA, city = NA, country_region = NA, entidad_principal = "no"){
      super$initialize(name = name, entidad_principal = "no")
      private$.age <- age
      private$.city <- city
      private$.country_region <- country_region
      private$.tag_1 <- "persona"
      msg <- glue::glue("Objeto persona creado con nombre: {private$.name}")
      log_trace(msg, namespace = c("objetos"))
    },
    print = function(...){
      cat("Esta es una persona de nombre: ", private$.name,"\n", sep = "")
      
    },
    sacaPropiedades = function(){
      tibble(name = private$.name, tag_1 = private$.tag_1, tag_2 = private$.tag_2, tag_3 = private$.tag_3,
             age = private$.age, city = private$.city, country_region = private$.country_region,
             entidad_principal = private$.entidad_principal)
    },
    sacaNodosRecursivo = function(){
      return(list(self))
    }
 ),
 active = list(
   age = function(value){
     if (missing(value)){
       private$.age
     }
     else{
       stopifnot(is.numeric(value))
       private$.age <- value
       invisible(self)
     }
   },
   city = function(value){
     if (missing(value)){
       private$.city
     }
     else{
       stopifnot(is.character(value))
       private$.city <- value
       invisible(self)
     }
   },
   country_region = function(value){
     if (missing(value)){
       private$.country_region
     }
     else{
       stopifnot(is.character(value))
       private$.country_region <- value
       invisible(self)
     }
   }
 ),
 private = list(
   .age = NA,
   .city = NA,
   .country_region = NA,
   .tag_1 = "persona"
 )
)

#Empresa

Empresa <- R6Class("Empresa",
  inherit = Entidad,
  public = list(
    initialize = function(name = name, accionistas = NULL, filiales = NULL, type = NA, country_region = NA, industry = NA, permID = NA,
                          incorporated_country = NA, incorporated_date = NA, total_revenue = NA, total_revenue_ajustado = NA, employees = NA,
                          market_cap = NA, market_cap_ajustado = NA, implied_rating = NA, moody_rating = NA, fitch_rating = NA, ownership_per = NA, ric = NA, 
                          issued_bonds = NA, outstanding_loans = NA, cds = NA, commercial_paper = NA, futures = NA, options = NA, warrants = NA, equities = NA,
                          certificate_deposits = NA, preference_shares = NA, investment_certificates = NA, funds_count = NA, relationship_type = NA,
                          pe_backed_status = NA, city = NA, industry_reclassified = NA, industry_reclassified_2 = NA, entidad_principal = "no",
                          ultimate_parent = NA, immediate_parent = NA, currency = NA, financials = NULL, financials_dolares = NULL, financials_dolares_ajustados = NULL,
                          grupo_familiar="no", nombre_familias=NA, pais_origen_familias=NA){
        super$initialize(name, entidad_principal)
        private$.accionistas <- accionistas
        private$.filiales <- filiales
        private$.type <- type
        private$.country_region <- country_region
        private$.industry <- industry
        private$.permID <- permID
        private$.incorporated_country <- incorporated_country
        private$.total_revenue <- total_revenue
        private$.total_revenue_ajustado <- total_revenue_ajustado
        private$.employees <- employees
        private$.market_cap <- market_cap
        private$.market_cap_ajustado <- market_cap_ajustado
        private$.implied_rating <- implied_rating
        private$.moody_rating <- moody_rating
        private$.fitch_rating <- fitch_rating
        private$.ric <- ric
        private$.issued_bonds <- issued_bonds
        private$.outstanding_loans <- outstanding_loans
        private$.cds <- cds
        private$.commercial_paper <- commercial_paper
        private$.futures <- futures
        private$.options <- options
        private$.warrants <- warrants
        private$.equities < equities
        private$.certificate_deposits <- certificate_deposits
        private$.preference_shares <- preference_shares
        private$.investment_certificates <- investment_certificates
        private$.funds_count <- funds_count
        private$.pe_backed_status <- pe_backed_status
        private$.city <- city
        private$.tag_1 <- "empresa"
        private$.industry_reclassified <- industry_reclassified
        private$.industry_reclassified_2 <- industry_reclassified_2
        private$.ultimate_parent <- ultimate_parent
        private$.immediate_parent <- immediate_parent
        private$.currency <- currency
        private$.financials <- financials
        private$.financials_dolares <- financials_dolares
        private$.financials_dolares_ajustados <- financials_dolares_ajustados
        private$.grupo_familiar <- grupo_familiar
        private$.nombre_familias <- nombre_familias
        private$.pais_origen_familias <- pais_origen_familias
        private$.indice_accionistas <- new.env(hash = TRUE, parent = emptyenv())
        if(!is.null(accionistas)){
          for(accionista in private$.accionistas){
            assign(accionista$entidad$name, accionista, envir = private$.indice_accionistas)  # lo registramos en el índice
          }  
        }
        private$.indice_filiales <- new.env(hash = TRUE, parent = emptyenv())
        if(!is.null(filiales)){
          for(filial in private$.filiales){
            assign(filial$empresa$name, filial, envir = private$.indice_filiales)  # lo registramos en el índice
          }  
        }
        msg <- glue::glue("Objeto empresa creado con nombre: {private$.name}")
        log_trace(msg, namespace = c("objetos"))
    },
    print = function(...){
      cat("nombre de la empresa: ",private$.name, "\n", sep="")
      
    },
    sacaPropiedades = function(){
      nodo <- tibble(name = private$.name, tag_1 = private$.tag_1, tag_2 = private$.tag_2, tag_3 = private$.tag_3, type = private$.type, country_region = private$.country_region,
                    industry = private$.industry, permID = private$.permID, incorporated_country = private$.incorporated_country,
                    total_revenue = private$.total_revenue, total_revenue_ajustado = private$.total_revenue_ajustado, employees = private$.employees, market_cap = private$.market_cap,
                    market_cap_ajustado = private$.market_cap_ajustado, implied_rating = private$.implied_rating, moody_rating = private$.moody_rating, fitch_rating = private$.fitch_rating,
                    ownership_per = private$.ownership_per, ric = private$.ric, issued_bonds =private$.issued_bonds,
                    outstanding_loans = private$.outstanding_loans, cds = private$.cds, commercial_paper = private$.commercial_paper,
                    futures = private$.futures, options = private$.options, warrants = private$.warrants, equities = private$.equities,
                    certificate_deposits = private$.certificate_deposits, preference_shares = private$.preference_shares,
                    investment_certificates = private$.investment_certificates, funds_count = private$.funds_count,
                    incorporated_date = private$.incorporated_date, relationship_type = private$.relationship_type, 
                    pe_backed_status = private$.pe_backed_status, city = private$.city, industry_reclassified = private$.industry_reclassified, industry_reclassified_2 = private$.industry_reclassified_2,
                    entidad_principal = private$.entidad_principal, immediate_parent = private$.immediate_parent, ultimate_parent = private$.ultimate_parent,
                    grupo_familiar = private$.grupo_familiar, nombre_familias = self$nombre_familias, pais_origen_familias = self$pais_origen_familias
                    )
      return(nodo)
    },
    sacaNodosColaAccionistas = function(){
      ##funciones auxiliares
      verificaVisitados <- function(nodo, visitados){
        if (length(visitados) == 0){return(FALSE)}
        visitados_names <- sapply(visitados, \(n) n$name)
        verificacion <- nodo$name %in% visitados_names
        return(verificacion)
      }
      actualiza_cola <- function(cola, visitados, vecinos_accionistas){
        nuevos <- list()
        visitados_names <- sapply(visitados, \(nodo_visitados) nodo_visitados$name)
        for (vecino in vecinos_accionistas){
          if (!(vecino$name %in% visitados_names)){
            nuevos <- append(nuevos, vecino)
          }
        }
        cola <- append(cola, nuevos)
        return(cola)
      }
      ##variables
      visitados <- list()
      cola <- list(self)
      lista_nodos <- list()
      ##bucle
      while (length(cola) > 0){
        nodo <- cola[[1]] ##cojo el primer elemento de la cola
        cola <- cola[-1] ##borro el primer elemento de la cola que pillé.
        ##compruebo que el nodo de la cola es nuevo.
        if (!verificaVisitados(nodo, visitados)){
          visitados <- append(visitados, nodo)
          lista_nodos <- append(lista_nodos, nodo)
          ##saco las entidades accionistas conectadas con el nodo.
          vecinos_accionistas <- lapply(nodo$accionistas, \(n) n$entidad)
          ##si hay entidades, actualizo la cola con las entidades nuevas.
          if (length(vecinos_accionistas) != 0){
            cola <- actualiza_cola(cola, visitados, vecinos_accionistas)
          }
        }
      }
      return(lista_nodos)
    },
    sacaNodosColaFiliales = function(){
      ##funciones auxiliares
      verificaVisitados <- function(nodo, visitados){
        if (length(visitados) == 0){return(FALSE)}
        visitados_names <- sapply(visitados, \(n) n$name)
        verificacion <- nodo$name %in% visitados_names
        return(verificacion)
      }
      actualiza_cola <- function(cola, visitados, vecinos_filiales){
        nuevos <- list()
        visitados_names <- sapply(visitados, \(nodo_visitados) nodo_visitados$name)
        for (vecino in vecinos_filiales){
          if (!(vecino$name %in% visitados_names)){
            nuevos <- append(nuevos, vecino)
          }
        }
        cola <- append(cola, nuevos)
        return(cola)
      }
      ##variables
      visitados <- list()
      cola <- list(self)
      lista_nodos <- list()
      ##bucle
      while (length(cola) > 0){
        nodo <- cola[[1]] ##cojo el primer elemento de la cola
        cola <- cola[-1] ##borro el primer elemento de la cola que pillé.
        ##compruebo que el nodo de la cola es nuevo.
        if (!verificaVisitados(nodo, visitados)){
          visitados <- append(visitados, nodo)
          lista_nodos <- append(lista_nodos, nodo)
          ##saco las empresas filiales conectadas con el nodo.
          vecinos_filiales <- lapply(nodo$filiales, \(n) n$empresa)
          ##si hay empresas, actualizo la cola con las entidades nuevas.
          if (length(vecinos_filiales) != 0){
            cola <- actualiza_cola(cola, visitados, vecinos_filiales)
          }
        }
      }
      return(lista_nodos)
    },
    sacaNodosCola = function(){

      lista_nodos <- list(self)
      lista_nodos <- append(lista_nodos, self$sacaNodosColaAccionistas()[-1])
      lista_nodos <- append(lista_nodos, self$sacaNodosColaFiliales()[-1])
      
      return(lista_nodos)
    },
    sacaNodosRecursivo = function(){
      lista_nodos <- list(self)
      #Función auxiliar para evitar duplicados
      agregar_nodos <- function(nodos_nuevos, lista_nodos_in) {
        lista_nodos <- lista_nodos_in
        for (nodo_in in nodos_nuevos) {
          if (!any(sapply(lista_nodos, function(nodo) nodo$name) == nodo_in$name)) {
            lista_nodos <- append(lista_nodos, nodo_in)
          }
        }
        return(lista_nodos)
      }
      if (!is.null(private$.accionistas)){
        lista_nodos_accionistas <- lapply(private$.accionistas, function(accionista) accionista$entidad$sacaNodosRecursivo())
        lista_nodos <- agregar_nodos(unlist(lista_nodos_accionistas, recursive = FALSE), lista_nodos)
      }
      if(!is.null(private$.filiales)){
        lista_nodos_filiales <- lapply(private$.filiales, function(filial) filial$empresa$sacaNodosRecursivo())
        lista_nodos <- agregar_nodos(unlist(lista_nodos_filiales, recursive = FALSE), lista_nodos)
      }
      return(lista_nodos)
    },
    sacaValorTotal = function(){
      total <- 0
      for (accionista in self$accionistas){
        aumento <- ifelse(is.numeric(accionista$value),accionista$value,0)
        total <- total + aumento
      }
      return(total)
    },
    sacaTablaNodos = function(){
      tabla <- bind_rows(lapply(self$sacaNodosCola(), \(x) sacaPropiedades(x) ))
      return(tabla)
    },
    sacaRelacionesEmpresa = function(){
      tabla_accionistas <- bind_rows(lapply(self$accionistas, function(a) a$sacaRelacionesAccionista()))
      tabla_filiales <- bind_rows(lapply(self$filiales, function(f) f$sacaRelacionesFilial()))
      tabla_accionistas$target <- private$.name
      tabla_filiales$target <- private$.name
      
      return (list(tabla_accionistas, tabla_filiales))
    },
    aumentaFinancials = function(tabla_financials){
      stopifnot(class(tabla_financials)[1] == "tbl_df")
      tabla_financials <- tabla_financials %>% mutate(across(all_of(colnames(tabla_financials)), unlist))
      #compruebo que el financials no está vacío
      if(is.null(private$.financials)){
        private$.financials <- tabla_financials
      }else{
        private$.financials <- bind_rows(private$.financials, tabla_financials)
      }
      private$.financials <- private$.financials %>% group_by(empresa, variable, anho) %>% 
        summarise(valor = first(valor, na_rm = TRUE), .groups = "drop")
      invisible(self)
    },
    verificaAccionista = function (nombre_accionista){
      nombre_accionista <- gsub("‘|’|'|-|–", " ", nombre_accionista) 
      return(exists(nombre_accionista, envir = private$.indice_accionistas, inherits = FALSE))
    },
    aumentaAccionistas = function(accionista){
      stopifnot(class(accionista)[1] == "Accionista")
      name <- accionista$entidad$name
      
      if(!self$verificaAccionista(name)){
        private$.accionistas <- append(private$.accionistas , accionista)
        assign(name, accionista, envir = private$.indice_accionistas)  # lo registramos en el índice
      } else{
        msg <- glue::glue("Empresa/aumentaAccionistas, accionista ({name}) ya existe en la lista ({private$.name})")
        log_error_multi(msg, namespaces = c("global", "objetos"))
      }
      
      invisible(self)
    },
    verificaFilial = function (nombre_filial){
      nombre_filial <- gsub("‘|’|'|-|–", " ", nombre_filial) 
      return(exists(nombre_filial, envir = private$.indice_filiales, inherits = FALSE))
    },
    aumentaFiliales = function(filial){
      stopifnot(class(filial)[1] == "Filial")
      name <- filial$empresa$name
      
      if(!self$verificaFilial(name)){
        private$.filiales <- append(private$.filiales , filial)
        assign(name, filial, envir = private$.indice_filiales)  # lo registramos en el índice
      } else{
        msg <- glue::glue("Empresa/aumentaFiliales, filial ({name}) ya existe en la lista ({private$.name})")
        log_error_multi(msg, namespaces = c("global", "objetos"))
      }
      
      invisible(self)
    },
    accionistasTabulado = function(){
      tabla = tibble(investor_name = sapply(private$.accionistas, function(a) a$entidad$name),
                     empresa_principal = rep(private$.name, length(private$.accionistas)),
                     empresa_principal_pais = rep(private$.country_region, length(private$.accionistas)),
                     empresa_principal_industria = rep(private$.industry, length(private$.accionistas)),
                     empresa_principal_valor_total = rep(self$sacaValorTotal(), length(private$.accionistas)),
                     ranking = sapply(private$.accionistas, function(a) a$ranking),
                     outstanding = sapply(private$.accionistas, function(a) a$outstanding),
                     position = sapply(private$.accionistas, function(a) a$position),
                     position_change = sapply(private$.accionistas, function(a) a$position_change),
                     value = sapply(private$.accionistas, function(a) a$value),
                     value_ajustado = sapply(private$.accionistas, function(a) a$value_ajustado),
                     filing_date = sapply(private$.accionistas, function(a) a$filing_date),
                     investor_type = sapply(private$.accionistas, function(a) a$investor_type),
                     investor_subtype = sapply(private$.accionistas, function(a) a$investor_subtype),
                     equity_assets = sapply(private$.accionistas, function(a) a$equity_assets),
                     investment_style = sapply(private$.accionistas, function(a) a$investment_style),
                     turnover = sapply(private$.accionistas, function(a) a$turnover),
                     city = sapply(private$.accionistas, function(a) a$entidad$city),
                     country_region = sapply(private$.accionistas, function(a) a$entidad$country_region)
                     )
      write.csv(tabla, file=here::here("export","tablas",paste(private$.name,"_accionistasTabulado.csv", sep="")), row.names = FALSE)
      return(tabla)
    },
    filialesTabulado = function(){
      tabla = tibble(empresa_principal = rep(private$.name, length(private$.filiales)),
                     empresa_filial = sapply(private$.filiales, function(f) f$empresa$name),
                     permID = sapply(private$.filiales, function(f) f$empresa$permID),
                     type = sapply(private$.filiales, function(f) f$empresa$type),
                     country_region = sapply(private$.filiales, function(f) f$empresa$country_region),
                     incorporated_country = sapply(private$.filiales, function(f) f$empresa$incorporated_country),
                     incorporated_date = sapply(private$.filiales, function(f) f$empresa$incorporated_date),
                     industry = sapply(private$.filiales, function(f) f$empresa$industry),
                     total_revenue = sapply(private$.filiales, function(f) f$empresa$total_revenue),
                     employees = sapply(private$.filiales, function(f) f$empresa$employees),
                     market_cap = sapply(private$.filiales, function(f) f$empresa$market_cap),
                     implied_rating = sapply(private$.filiales, function(f) f$empresa$implied_rating),
                     moody_rating = sapply(private$.filiales, function(f) f$empresa$moody_rating),
                     fitch_rating = sapply(private$.filiales, function (f) f$empresa$fitch_rating),
                     ownership_per = sapply(private$.filiales, function (f) f$ownership_per),
                     ric = sapply(private$.filiales, function(f) f$empresa$ric),
                     outstanding_loans = sapply(private$.filiales, function(f) f$empresa$outstanding_loans),
                     cds = sapply(private$.filiales, function(f) f$empresa$cds),
                     commercial_paper = sapply(private$.filiales, function(f) f$empresa$commercial_paper),
                     futures = sapply(private$.filiales, function(f) f$empresa$futures),
                     options = sapply(private$.filiales, function(f) f$empresa$options),
                     warrants = sapply(private$.filiales, function(f) f$empresa$warrants),
                     equities = sapply(private$.filiales, function (f) f$empresa$equities),
                     certificate_deposits = sapply(private$.filiales, function(f) f$empresa$certificate_deposits),
                     preference_shares = sapply(private$.filiales, function(f) f$empresa$preference_shares),
                     investment_certificates = sapply(private$.filiales, function(f) f$empresa$investment_certificates),
                     funds_count = sapply(private$.filiales, function(f) f$empresa$funds_count),
                     relationship_type = sapply(private$.filiales, function(f) f$relationship_type),
                     pe_backed_status = sapply(private$.filiales, function(f) f$empresa$pe_backed_status)
                     )
      
      write.csv(tabla, file=here::here("export","tablas",paste(private$.name, "_filialesTabulado.csv", sep = "")), row.names = FALSE)
      return(tabla)
    },
    accionistasTabuladoAgrupado = function(campo_agrupacion, campo_ordenacion = NULL){
      stopifnot(is.character(campo_agrupacion))
      #saco la tabla en bruto
      tabla <- self$accionistasTabulado()
      
      #identifico los campos numéricos y los campos string
      campos_numericos <- tabla %>% select(where(is.numeric)) %>% colnames()
      campos_string <- tabla %>% select(where(is.character)) %>% colnames()
      
      #remuevo el campo string agrupación del resto de campos string
      campos_string <- setdiff(campos_string, campo_agrupacion)
      
      #ordeno la tabla
      
      #construyo la tabla resumida
      tabla <- tabla %>% group_by(across(all_of(campo_agrupacion))) %>% summarise(count_unique_empresas_principales = n_distinct(empresa_principal), count_unique_investor_name = n_distinct(investor_name), across(all_of(campos_string), ~ paste(unique(.), collapse = ", "), .names = paste0("total_","{.col}")),
                                                                                  across(all_of(campos_numericos), \(x) sum(x, na.rm = TRUE), .names = paste0("total_","{.col}")),
                                                                                  .groups = "drop"
      )
      #pongo los valores percentuales para los campos numéricos y ordeno la tabla
      tabla <- tabla %>% mutate(across(all_of(paste0("total_",campos_numericos)), \(x) round(100 * x/sum(x, na.rm = TRUE),2), .names = paste0("{col}","_per")))
      if (!is.null(campo_ordenacion)){
        tabla <- tabla %>% arrange(desc(!!sym(campo_ordenacion)))
      }
      
      write.csv(tabla, file=here::here("export", "tablas", paste(private$.name, "_accionistasTabuladoAgrupado_", campo_agrupacion, ".csv", sep = "")), row.names = FALSE)
      return(tabla)
    },
    filialesTabuladoAgrupado = function(campo_agrupacion, campo_ordenacion = NULL){
      stopifnot(is.character(campo_agrupacion))
      #saco la tabla en bruto
      tabla <- self$filialesTabulado()
      
      #identifico los campos numéricos y los campos string
      campos_numericos <- tabla %>% select(where(is.numeric)) %>% colnames()
      campos_string <- tabla %>% select(where(is.character)) %>% colnames()
      
      #remuevo el campo string agrupación del resto de campos string
      campos_string <- setdiff(campos_string, campo_agrupacion)
      
      #ordeno la tabla
      
      #construyo la tabla resumida
      tabla <- tabla %>% group_by(across(all_of(campo_agrupacion))) %>% summarise(count_unique_empresas_principales = n_distinct(empresa_principal), count_unique_filiales = n_distinct(empresa_filial), across(all_of(campos_string), ~ paste(unique(.), collapse = ", "), .names = paste0("total_","{.col}")),
                                                                                  across(all_of(campos_numericos), \(x) sum(x, na.rm = TRUE), .names = paste0("total_","{.col}")),
                                                                                  .groups = "drop"
      )
      #pongo los valores percentuales para los campos numéricos y ordeno
      tabla <- tabla %>% mutate(across(all_of(paste0("total_",campos_numericos)), \(x) round(100 * x/sum(x, na.rm = TRUE),2), .names = paste0("{col}","_per")))
      if (!is.null(campo_ordenacion)){
        tabla <- tabla %>% arrange(desc(!!sym(campo_ordenacion)))
      }
      
      write.csv(tabla, file=here::here("export", "tablas", paste(private$.name, "_filialesTabuladoAgrupado_", campo_agrupacion, ".csv", sep = "")), row.names = FALSE)
      return(tabla)
    },
    queryCypherFinancials = function(){
      query <- NULL
      sacaLineasVariables <- function(tipo_financials){
        if(tipo_financials == "currency"){
          variables_tabla <- private$.financials %>% select(-c(empresa)) %>%
            pivot_wider(names_from = anho, values_from = valor)
        } else if(tipo_financials == "dolares"){
            variables_tabla <- private$.financials_dolares %>% select(-c(empresa)) %>%
              pivot_wider(names_from = anho, values_from = valor)
        } else if(tipo_financials == "dolares_ajustados"){
            variables_tabla <- private$.financials_dolares_ajustados %>% select(-c(empresa)) %>%
              pivot_wider(names_from = anho, values_from = valor)
        }
        variables_tabla[is.na(variables_tabla)] <- 0
        variables_tabla$variable <- paste("r.", gsub(" ", "_", tolower(variables_tabla$variable)), sep="")
        
        lineas_variables <- apply(variables_tabla, 1, function(fila) paste("'",fila[1],"' = [", paste(fila[-1], collapse=", "),"]",sep=""))
        
        return(lineas_variables)
      }
      
      if (!is.null(private$.financials)){
        source <- private$.name
        target <- paste(private$.name, "_financials", sep = "")
        
        lineas_variables <- sacaLineasVariables("currency")
        lineas_variables_dolares <- sacaLineasVariables("dolares")
        lineas_variables_dolares_ajustados <- sacaLineasVariables("dolares_ajustados")

        query_1 <- paste("MERGE (f:financials{name: '",target,"'});", sep = "")
        query_2 <- paste("MATCH (e:empresa{name: '",source,"'}) MATCH (f:financials{name: '",
                         target,"'}) MERGE (e)-[r:financials]->(f) SET ", gsub("'|-", "", paste(c(lineas_variables, lineas_variables_dolares, lineas_variables_dolares_ajustados), collapse=", ")), ";", sep = ""
                        )
        query <- list(query_1, query_2)
        
      }
      return(query)
    },
    insertNeo4jFinancials = function(){
      query <- NULL
      if (class(self)[[1]] == "Empresa"){
        query <- self$queryCypherFinancials()
      }
      if (!is.null(query)){
        use_virtualenv("~/.venvs/neo4j_env", required = TRUE)
        user_neo4j <- "neo4j"
        password_neo4j <- "norpentar"
        query_1 <- query[[1]]
        query_2 <- query[[2]]
        result_query_1 <- run_query("bolt://localhost:7687", user_neo4j, password_neo4j, query_1)
        result_query_2 <- run_query("bolt://localhost:7687", user_neo4j, password_neo4j, query_2)
      }
    },
    #Esta función lo que hace es guardar el valor de financials en dólares según un año de referencia.
    aumentaFinancialsDolares = function(tabla_financials_dolares){
      stopifnot(class(tabla_financials_dolares)[1] == "tbl_df")
      tabla_financials_dolares <- tabla_financials_dolares %>% mutate(across(all_of(colnames(tabla_financials_dolares)), unlist))
      #compruebo que el financials no está vacío
      if(is.null(private$.financials_dolares)){
        private$.financials_dolares <- tabla_financials_dolares
      }else{
        private$.financials_dolares <- bind_rows(private$.financials_dolares, tabla_financials_dolares)
      }
      private$.financials_dolares <- private$.financials_dolares %>% group_by(empresa, variable, anho) %>% 
        summarise(valor = first(valor, na_rm = TRUE), .groups = "drop")
      invisible(self)
    },
    aumentaFinancialsDolaresAjustados = function(tabla_financials_dolares_ajustados){
      stopifnot(class(tabla_financials_dolares_ajustados)[1] == "tbl_df")
      tabla_financials_dolares_ajustados <- tabla_financials_dolares_ajustados %>% mutate(across(all_of(colnames(tabla_financials_dolares_ajustados)), unlist))
      #compruebo que el financials no está vacío
      if(is.null(private$.financials_dolares_ajustados)){
        private$.financials_dolares_ajustados <- tabla_financials_dolares_ajustados
      }else{
        private$.financials_dolares_ajustados <- bind_rows(private$.financials_dolares_ajustados, tabla_financials_dolares_ajustados)
      }
      private$.financials_dolares_ajustados <- private$.financials_dolares_ajustados %>% group_by(empresa, variable, anho) %>% 
        summarise(valor = first(valor, na_rm = TRUE), .groups = "drop")
      invisible(self)
    },
    ajustaTreeStructure = function(anho_base = "2023", anho_recoleccion = "2023"){
      list_result <- list()
      if (is.na(private$.currency)){
        list_result <- c(NA, NA)
      } else{
        list_result <- reconvierteValorUSD(private$.currency, anho_base, anho_recoleccion, c(private$.total_revenue, private$.market_cap))
      }
      private$.total_revenue_ajustado <- list_result[[1]]
      private$.market_cap_ajustado <- list_result[[2]]
      invisible(self)
    },
    ajustaShareholders = function(anho_base = "2023", anho_recoleccion = "2023"){
      list_result <- list()
      if (is.na(private$.currency)){
        invisible(self)
        return()
      } else{
        for (accionista in private$.accionistas){
          accionista$value_ajustado <- reconvierteValorUSD(private$.currency, anho_base, anho_recoleccion, accionista$value)
        }
      }
      invisible(self)
    },
    #esta función coje el país de la empresa y carga su currency.
    cargaCurrencyPais = function(tabla_divisas_paises){
      currency <- tabla_divisas_paises$currency[tabla_divisas_paises$pais==private$.country_region]
      if(is.null(currency)){
        msg <- glue::glue("Empresa: {private$.name} - Error en la asignación de currency a partir del país: {private$.country_region}")
        log_error_multi(msg, namespaces = c("global", "objetos"))
      }
      private$.currency <- currency
      invisible(self)
    }
  ),
  active = list(
    accionistas = function(value){
      if (missing(value)){
        private$.accionistas
      }
      else{
        private$.accionistas <- value
        invisible(self)
      }
    },
    filiales = function(value){
      if (missing(value)){
        private$.filiales
      }
      else{
        private$.filiales <- value
        invisible(self)
      }
    },
    type = function(value){
      if (missing(value)){
        private$.type
      }
      else{
        stopifnot(is.character(value))
        private$.type <- value
        invisible(self)
      }
    },
    country_region = function(value){
      if (missing(value)){
        private$.country_region
      }
      else{
        stopifnot(is.character(value))
        private$.country_region <- value
        invisible(self)
      }
    },
    industry = function(value){
      if (missing(value)){
        private$.industry
      }
      else{
        stopifnot(is.character(value))
        private$.industry <- value
        invisible(self)
      }
    },
    permID = function(value){
      if (missing(value)){
        private$.permID
      }
      else{
        private$.permID <- value
        invisible(self)
      }
    },
    incorporated_country = function(value){
      if (missing(value)){
        private$.incorporated_country
      }
      else{
        stopifnot(is.character(value))
        private$.incorporated_country <- value
        invisible(self)
      }
    },
    incorporated_date = function(value){
      if (missing(value)){
        private$.incorporated_date
      }
      else{
        stopifnot(is.Date(value))
        private$.incorporated_date <- value
        invisible(self)
      }
    },
    total_revenue = function(value){
      if (missing(value)){
        private$.total_revenue
      }
      else{
        stopifnot(is.numeric(value))
        private$.total_revenue <- value
        invisible(self)
      }
    },
    total_revenue_ajustado = function(value){
      if (missing(value)){
        private$.total_revenue_ajustado
      }
      else{
        stopifnot(is.numeric(value))
        private$.total_revenue_ajustado <- value
        invisible(self)
      }
    },
    employees = function(value){
      if (missing(value)){
        private$.employees
      }
      else{
        stopifnot(is.numeric(value))
        private$.employees <- value
        invisible(self)
      }
    },
    market_cap = function(value){
      if (missing(value)){
        private$.market_cap
      }
      else{
        stopifnot(is.numeric(value))
        private$.market_cap <- value
        invisible(self)
      }
    },
    market_cap_ajustado = function(value){
      if (missing(value)){
        private$.market_cap_ajustado
      }
      else{
        stopifnot(is.numeric(value))
        private$.market_cap_ajustado <- value
        invisible(self)
      }
    },
    implied_rating = function(value){
      if (missing(value)){
        private$.implied_rating
      }
      else{
        stopifnot(is.character(value))
        private$.implied_rating <- value
        invisible(self)
      }
    },
    moody_rating = function(value){
      if (missing(value)){
        private$.moody_rating
      }
      else{
        stopifnot(is.character(value))
        private$.moody_rating <- value
        invisible(self)
      }
    },
    fitch_rating = function(value){
      if (missing(value)){
        private$.fitch_rating
      }
      else{
        stopifnot(is.character(value))
        private$.fitch_rating <- value
        invisible(self)
      }
    },
    ric = function(value){
      if (missing(value)){
        private$.ric
      }
      else{
        stopifnot(is.character(value))
        private$.ric <- value
        invisible(self)
      }
    },
    issued_bonds = function(value){
      if (missing(value)){
        private$.issued_bonds
      }
      else{
        stopifnot(is.numeric(value))
        private$.issued_bonds <- value
        invisible(self)
      }
    },
    outstanding_loans = function(value){
      if (missing(value)){
        private$.outstanding_loans
      }
      else{
        stopifnot(is.numeric(value))
        private$.outstanding_loans <- value
        invisible(self)
      }
    },
    cds = function(value){
      if (missing(value)){
        private$.cds
      }
      else{
        stopifnot(is.numeric(value))
        private$.cds <- value
        invisible(self)
      }
    },
    commercial_paper = function(value){
      if (missing(value)){
        private$.commercial_paper
      }
      else{
        stopifnot(is.numeric(value))
        private$.commercial_paper <- value
        invisible(self)
      }
    },
    futures = function(value){
      if (missing(value)){
        private$.futures
      }
      else{
        stopifnot(is.numeric(value))
        private$.futures <- value
        invisible(self)
      }
    },
    options = function(value){
      if (missing(value)){
        private$.options
      }
      else{
        stopifnot(is.numeric(value))
        private$.options <- value
        invisible(self)
      }
    },
    warrants = function(value){
      if (missing(value)){
        private$.warrants
      }
      else{
        stopifnot(is.numeric(value))
        private$.warrants <- value
        invisible(self)
      }
    },
    equities = function(value){
      if (missing(value)){
        private$.equities
      }
      else{
        stopifnot(is.numeric(value))
        private$.equities <- value
        invisible(self)
      }
    },
    certificate_deposits = function(value){
      if (missing(value)){
        private$.certificate_deposits
      }
      else{
        stopifnot(is.numeric(value))
        private$.certificate_deposits <- value
        invisible(self)
      }
    },
    preference_shares = function(value){
      if (missing(value)){
        private$.preference_shares
      }
      else{
        stopifnot(is.numeric(value))
        private$.preference_shares <- value
        invisible(self)
      }
    },
    investment_certificates = function(value){
      if (missing(value)){
        private$.investment_certificates
      }
      else{
        stopifnot(is.numeric(value))
        private$.investment_certificates <- value
        invisible(self)
      }
    },
    funds_count = function(value){
      if (missing(value)){
        private$.funds_count
      }
      else{
        stopifnot(is.numeric(value))
        private$.funds_count <- value
        invisible(self)
      }
    },
    pe_backed_status = function(value){
      if (missing(value)){
        private$.pe_backed_status
      }
      else{
        private$.pe_backed_status <- value
        invisible(self)
      }
    },
    city = function(value){
      if (missing(value)){
        private$.city
      }
      else{
        stopifnot(is.character(value))
        private$.city <- value
        invisible(self)
      }
    },
    industry_reclassified = function(value){
      if (missing(value)){
        private$.industry_reclassified
      }
      else{
        stopifnot(is.character(value))
        private$.industry_reclassified <- value
        invisible(self)
      }
    },
    industry_reclassified_2 = function(value){
      if (missing(value)){
        private$.industry_reclassified_2
      }
      else{
        stopifnot(is.character(value))
        private$.industry_reclassified_2 <- value
        invisible(self)
      }
    },
    ultimate_parent = function(value){
      if (missing(value)){
        private$.ultimate_parent
      }
      else{
        stopifnot(is.character(value))
        private$.ultimate_parent <- value
        invisible(self)
      }
    },
    immediate_parent = function(value){
      if (missing(value)){
        private$.immediate_parent
      }
      else{
        stopifnot(is.character(value))
        private$.immediate_parent <- value
        invisible(self)
      }
    },
    currency = function(value){
      if (missing(value)){
        private$.currency
      }
      else{
        stopifnot(is.character(value))
        private$.currency <- value
        invisible(self)
      }
    },
    financials = function(value){
      if(missing(value)){
        private$.financials
      }
      else{
        private$.financials <- value
        if (!is.null(value)){
          private$.financials <- private$.financials %>% mutate(across(all_of(colnames(private$.financials)), unlist))
          private$.financials <- private$.financials %>% group_by(empresa, variable, anho) %>% 
            summarise(valor = first(valor, na_rm = TRUE), .groups = "drop")
        }
        invisible(self)
      }
    },
    financials_dolares = function(value){
      if(missing(value)){
        private$.financials_dolares
      }
      else{
        private$.financials_dolares <- value
        if (!is.null(value)){
          private$.financials_dolares <- private$.financials_dolares %>% mutate(across(all_of(colnames(private$.financials_dolares)), unlist))
          private$.financials_dolares <- private$.financials_dolares %>% group_by(empresa, variable, anho) %>% 
            summarise(valor = first(valor, na_rm = TRUE), .groups = "drop")
        }
        invisible(self)
      }
    },
    financials_dolares_ajustados = function(value){
      if(missing(value)){
        private$.financials_dolares_ajustados
      }
      else{
        private$.financials_dolares_ajustados <- value
        if (!is.null(value)){
          private$.financials_dolares_ajustados <- private$.financials_dolares_ajustados %>% mutate(across(all_of(colnames(private$.financials_dolares_ajustados)), unlist))
          private$.financials_dolares_ajustados <- private$.financials_dolares_ajustados %>% group_by(empresa, variable, anho) %>% 
            summarise(valor = first(valor, na_rm = TRUE), .groups = "drop")
        }
        invisible(self)
      }
    },
    grupo_familiar = function(value){
      if(missing(value)){
        private$.grupo_familiar
      }
      else{
        private$.grupo_familiar <- value
        invisible(self)
      }
    },
    nombre_familias = function(value){
      if(missing(value)){
        paste(private$.nombre_familias, collapse = ",")
      }
      else{
        private$.nombre_familias <- trimws(strsplit(value, ",")[[1]])
        invisible(self)
      }
    },
    pais_origen_familias = function(value){
      if(missing(value)){
        paste(private$.pais_origen_familias, collapse = ",")
      }
      else{
        private$.pais_origen_familias <- trimws(strsplit(value, ",")[[1]])
        invisible(self)
      }
    }
  ),
  private = list(
    .accionistas = NULL,
    .filiales = NULL,
    .type = NA,
    .country_region = NA,
    .industry = NA,
    .permID = NULL,
    .incorporated_country = NA,
    .incorporated_date = NA,
    .total_revenue = NA,
    .total_revenue_ajustado = NA,
    .employees = NA,
    .market_cap = NA,
    .market_cap_ajustado = NA,
    .implied_rating = NA,
    .moody_rating = NA,
    .fitch_rating = NA,
    .ric = NA,
    .issued_bonds = NA,
    .outstanding_loans = NA,
    .cds = NA,
    .commercial_paper = NA,
    .futures = NA,
    .options = NA,
    .warrants = NA,
    .equities = NA,
    .certificate_deposits = NA,
    .preference_shares = NA,
    .investment_certificates = NA,
    .funds_count = NA,
    .pe_backed_status = NA,
    .city = NA,
    .tag_1 = "empresa",
    .industry_reclassified = NA,
    .industry_reclassified_2 = NA,
    .ultimate_parent = NA,
    .immediate_parent = NA,
    .currency = NA,
    .financials = NULL,
    .financials_dolares = NULL,
    .financials_dolares_ajustados = NULL,
    .grupo_familiar = "no",
    .nombre_familias = NA,
    .pais_origen_familias = NA,
    .indice_accionistas = NULL,
    .indice_filiales = NULL
  )
)

#Filial
Filial <- R6Class("Filial",
  public = list(
    initialize = function(empresa, ownership_per, relationship_type, nivel_filial = NA){
      stopifnot(class(empresa)[[1]] == "Empresa")
      private$.empresa <- empresa
      private$.empresa$tag_2 <- "filial"
      private$.ownership_per <- ownership_per
      private$.relationship_type <- relationship_type
      private$.nivel_filial <- nivel_filial
      msg <- glue::glue("Objeto filial creado con nombre: {private$.name}")
      log_trace(msg, namespace = c("objetos"))
    },
    print = function(){
      cat("Esta es una filial de la empresa: ",private$.empresa$name,"\n", sep = "")
    },
    sacaRelacionesFilial = function(){
      tabla <- tibble(source = private$.empresa$name, tipo_relacion= "filial",
                      ownership_per = private$.ownership_per,
                      relationship_type = private$.relationship_type
                      )
      return(tabla)
    }
  ),                
  private = list(
    .empresa = NULL,
    .ownership_per = NA,
    .relationship_type = NA,
    .nivel_filial = NA
  ),
  active = list(
    empresa = function(value){
      if (missing(value)){
        private$.empresa
      }
      else{
        stopifnot(class(value)[[1]] == "Empresa")
        private$.empresa <- value
        invisible(self)
      }
    },
    ownership_per = function(value){
      if (missing(value)){
        private$.ownership_per
      }
      else{
        stopifnot(is.numeric(value))
        private$.ownership_per <- value
        invisible(self)
      }
    },
    relationship_type = function(value){
      if (missing(value)){
        private$.relationship_type
      }
      else{
        stopifnot(is.character(value))
        private$.relationship_type <- value
        invisible(self)
      }
    },
    nivel_filial = function(value){
      if (missing(value)){
        private$.nivel_filial
      }
      else{
        stopifnot(is.numeric(value))
        private$.nivel_filial <- value
        invisible(self)
      }
    }
    
    
  )
)

#Accionista
Accionista <- R6Class("Accionista",
  public = list(
    initialize = function(entidad, ranking = NA, outstanding = NA, position = NA, position_change = NA, value = NA, value_ajustado = NA,
      filing_date = NA, filing_source = NA, investor_type = NA, investor_subtype = NA, 
      equity_assets = NA, investment_style = NA, turnover = NA, city = NA, country_region = NA){
      
        stopifnot(inherits(entidad,"Entidad"))
        private$.entidad <- entidad
        private$.entidad$tag_2 <- "accionista"
        private$.ranking <- ranking
        private$.outstanding <- outstanding
        private$.position <- position
        private$.position_change <- position_change
        private$.value <- value
        private$.value_ajustado <- value_ajustado
        private$.filing_date <- filing_date
        private$.filing_source <- filing_source
        private$.investor_type <- investor_type
        private$.investor_subtype <- investor_subtype
        private$.equity_assets <- equity_assets
        private$.investment_style <- investment_style
        private$.turnover <- turnover
        private$.entidad$city <- city
        private$.entidad$country_region <- country_region
        msg <- glue::glue("Objeto accionista creado con nombre: {private$.name}")
        log_trace(msg, namespace = c("objetos"))
    },
    print = function(...){
      cat("Objeto de tipo Accionista \n")
      cat("investor_name: ",private$.entidad$name, "\n", sep="")
      cat("investor_subtype: ",private$.investor_subtype, "\n", sep="")
    },
    sacaRelacionesAccionista = function(){
      tabla_relaciones <- tibble(source = private$.entidad$name, tipo_relacion = "accionista", ranking = private$.ranking,
                                 outstanding = private$.outstanding, position = private$.position,
                                 position_change = private$.position_change, value = private$.value, value_ajustado = private$.value_ajustado,
                                 filing_date = private$.filing_date, filing_source = private$.filing_source,
                                 investor_type = private$.investor_type, investor_subtype = private$.investor_subtype,
                                 turnover = private$.turnover
                                 )
      return(tabla_relaciones)
      
    }
  ), 
  private = list(
    .entidad = NA,
    .ranking = NA,
    .outstanding = NA,
    .position = NA,
    .position_change = NA,
    .value = NA,
    .value_ajustado = NA,
    .filing_date = NA,
    .filing_source = NA,
    .investor_type = NA,
    .investor_subtype = NA,
    .equity_assets = NA,
    .investment_style = NA,
    .turnover = NA
  ),
  active = list(
    entidad = function(value){
      if (missing(value)){
        private$.entidad
      }
      else {
        stopifnot(inherits(value,"Entidad"))
        private$.entidad <- value
        invisible(self)
      }
    },
    ranking = function(value){
      if (missing(value)){
        private$.ranking
      }
      else {
        stopifnot(is.numeric(value))
        private$.ranking <- value
        invisible(self)
      }
    },
    outstanding = function(value){
      if (missing(value)){
        private$.outstanding
      }
      else {
        stopifnot(is.numeric(value))
        private$.outstanding <- value
        invisible(self)
      }
    },
    position = function(value){
      if (missing(value)){
        private$.position
      }
      else{
        stopifnot(is.numeric(value))
        private$.position <- value
        invisible(self)
      }
    },
    position_change = function(value){
      if (missing(value)){
        private$.position_change
      }
      else{
        stopifnot(is.numeric(value))
        private$.position_change <- value
        invisible(self)
      }
    },
    value = function(value){
      if (missing(value)){
        private$.value
      }
      else{
        stopifnot(is.numeric(value))
        private$.value <- value
        invisible(self)
      }
    },
    value_ajustado = function(value){
      if (missing(value)){
        private$.value_ajustado
      }
      else{
        stopifnot(is.numeric(value))
        private$.value_ajustado <- value
        invisible(self)
      }
    },
    filing_date = function(value){
      if (missing(value)){
        private$.filing_date
      }
      else{
        stopifnot(is.character(value))
        private$.filing_date <- value
        invisible(self)
      }
    },
    filing_source = function(value){
      if (missing(value)){
        private$.filing_source
      }
      else{
        stopifnot(is.character(value))
        private$.filing_source <- value
        invisible(self)
      }
    },
    investor_type = function(value){
      if (missing(value)){
        private$.investor_type
      }
      else{
        stopifnot(is.character(value))
        private$.investor_type <- value
        invisible(self)
      }
    },
    investor_subtype = function(value){
      if (missing(value)){
        private$.investor_subtype
      }
      else{
        stopifnot(is.character(value))
        private$.investor_subtype <- value
        invisible(self)
      }
    },
    equity_assets = function(value){
      if (missing(value)){
        private$.equity_assets
      }
      else{
        stopifnot(is.character(value))
        private$.equity_assets <- value
        invisible(self)
      }
    },
    investment_style = function(value){
      if (missing(value)){
        private$.investment_style
      }
      else{
        stopifnot(is.character(value))
        private$.investment_style <- value
        invisible(self)
      }
    },
    turnover = function(value){
      if (missing(value)){
        private$.turnover
      }
      else{
        stopifnot(is.character(value))
        private$.turnover <- value
        invisible(self)
      }
    }
  )
)

#Lista de entidades

ListaEntidades <- R6Class("ListaEntidades",
  public = list(
    initialize = function(name, entidades = NULL){
      stopifnot(is.list(entidades))
      private$.name <- name
      private$.entidades <- entidades
      private$.indice_entidades <- new.env(hash = TRUE, parent = emptyenv())
      if(!is.null(private$.entidades)){
        for(entidad in private$.entidades){
          assign(entidad$name, entidad, envir = private$.indice_entidades)  # lo registramos en el índice
        }  
      }  
      msg <- glue::glue("Objeto lista_empresas creado con nombre: {private$.name}")
      log_trace(msg, namespace = c("objetos"))
    },
    print = function(...){
      cat("Esta es una lista de entidades de nombre ", private$.name, ".\n", sep = "")
      cat("Las entidades que la componen son:\n")
      lapply(private$.entidades, \(x) cat(x$name, " \n"))
    },
    sacaNodosListaEntidades = function(){
      eliminaDuplicados <- function(lista_nodos){
        for(nodo in lista_nodos){
            indices <- which(sapply(lista_nodos, function (n) nodo$name == n$name))
            if (length(indices) != 1){
              indices <- indices[-1] #marco todos los índices duplicados menos el primero
              lista_nodos <- lista_nodos[-indices] #elimino indices duplicados
            }
        }
        return(lista_nodos)
      }
      
      #saco todos los nodos de las entidades de la lista
      lista <- lapply(private$.entidades, function(entidad) entidad$sacaNodosCola())
      lista <- unlist(lista, recursive = FALSE)
      lista <- eliminaDuplicados(lista)
      
      return(lista)
      
    },
    sacaTablaNodosListaEntidades = function(){
      lista_nodos <- self$sacaNodosListaEntidades()
      
      tabla_nodos <- bind_rows(lapply(lista_nodos, function(nodo) nodo$sacaPropiedades()))  
      
      write.csv(tabla_nodos, file = here::here("export", "tablas", "neo4j", paste(private$.name, "_ListaEntidades_tabla_nodos.csv", sep = "")), row.names = FALSE)
    
      return(tabla_nodos)
    },
    sacaRelacionesListaEntidades = function(){
      lista_nodos <- self$sacaNodosListaEntidades()
      
      sacaRelaciones <- function(nodo, tipo){
        if (class(nodo)[[1]] == "Empresa"){
          return(nodo$sacaRelacionesEmpresa()[[tipo]])
        }
        else{
          return(NULL)
        }
      }
      lista_tablas_accionistas <- lapply(lista_nodos, \(nodo) sacaRelaciones(nodo, 1))
      lista_tablas_filiales <- lapply(lista_nodos, \(nodo) sacaRelaciones(nodo, 2))
      
      tabla_accionistas <- lista_tablas_accionistas %>% compact() %>% bind_rows()
      tabla_filiales <- lista_tablas_filiales %>% compact() %>% bind_rows()
      
      write.csv(tabla_accionistas, file = here::here("export", "tablas", "neo4j", paste(private$.name, "_ListaEntidades_tabla_accionistas.csv", sep ="")), row.names = FALSE)
      write.csv(tabla_filiales, file = here::here("export", "tablas", "neo4j", paste(private$.name, "_ListaEntidades_tabla_filiales.csv", sep ="")), row.names = FALSE)
      
      return(list(tabla_accionistas, tabla_filiales))
      
    },
    sacaTablasRedListaEntidades = function(lista_nodos_in=NULL, insertar_neo4j=FALSE){
      msg <- "Entrando en la función sacaTablasRedListaEntidades"
      log_debug(msg, namespace = "objetos")
      ##función auxiliar
      sacaRelaciones <- function(nodo, tipo){
        if (class(nodo)[[1]] == "Empresa"){
          return(nodo$sacaRelacionesEmpresa()[[tipo]])
        }
        else{
          return(NULL)
        }
      }
      lista_nodos <- list()
      if(is.null(lista_nodos_in)){
        lista_nodos <- self$sacaNodosListaEntidades()
      } else{
        lista_nodos <- lista_nodos_in
      }
      ##nodos
      tabla_nodos_entidades <- bind_rows(lapply(lista_nodos, function(nodo) nodo$sacaPropiedades()))
      tabla_nodos_paises <- tibble(paises = unlist(unique(lapply(lista_nodos, \(nodo) nodo$country_region))))
      
      ##relaciones
      lista_tablas_accionistas <- lapply(lista_nodos, \(nodo) sacaRelaciones(nodo, 1))
      lista_tablas_filiales <- lapply(lista_nodos, \(nodo) sacaRelaciones(nodo, 2))
      ##elimino nulos y conformo las tablas
      tabla_accionistas <- lista_tablas_accionistas %>% compact() %>% bind_rows()
      tabla_filiales <- lista_tablas_filiales %>% compact() %>% bind_rows()
      tabla_relaciones_paises <- tibble(source = unlist(lapply(lista_nodos, \(nodo) nodo$name)), target = unlist(lapply(lista_nodos, \(nodo) nodo$country_region)))

      ##imprimo las tablas
      write.csv(tabla_nodos_entidades, file = here::here("export", "tablas", "neo4j", paste(private$.name, "_nodos_entidades.csv", sep ="")), row.names = FALSE)
      write.csv(tabla_nodos_paises, file = here::here("export", "tablas", "neo4j", paste(private$.name, "_nodos_paises.csv", sep = "")), row.names = FALSE)
      write.csv(tabla_accionistas, file = here::here("export", "tablas", "neo4j", paste(private$.name, "_relaciones_accionistas.csv", sep = "")), row.names = FALSE)
      write.csv(tabla_filiales, file = here::here("export","tablas", "neo4j", paste(private$.name, "_relaciones_filiales.csv", sep = "")), row.names = FALSE)
      write.csv(tabla_relaciones_paises, file = here::here("export", "tablas", "neo4j", paste(private$.name, "_relaciones_paises.csv", sep = "")), row.names = FALSE)
      ###en el directorio de neo4j también
      write.csv(tabla_nodos_entidades, file = paste0("/home/inigo/programas/neo4j/import/",private$.name,"_nodos_entidades.csv"), row.names = FALSE)
      write.csv(tabla_nodos_paises, file = paste0("/home/inigo/programas/neo4j/import/",private$.name,"_nodos_paises.csv"), row.names = FALSE)
      write.csv(tabla_accionistas, file = paste0("/home/inigo/programas/neo4j/import/",private$.name,"_relaciones_accionistas.csv"), row.names = FALSE)
      write.csv(tabla_filiales, file = paste0("/home/inigo/programas/neo4j/import/",private$.name,"_relaciones_filiales.csv"), row.names = FALSE)
      write.csv(tabla_relaciones_paises, file = paste0("/home/inigo/programas/neo4j/import/",private$.name,"_relaciones_paises.csv"), row.names = FALSE)
      
      msg <- "Las tablas de la red se imprimieron correctamente"
      log_debug(msg, namespace = "objetos")
      
      if(insertar_neo4j == TRUE){
        msg <- "Se procede a cargar la red en el Neo4j"
        log_debug(msg, namespace = "objetos")
        query_nodos_entidades <- "LOAD CSV WITH HEADERS
        from 'file:///lista_entidades_principales_nodos_entidades.csv' as row
        merge (e:load {name: row.name})
        set
        e.name = row.name,
        e.tag_1 = row.tag_1,
        e.tag_2 = row.tag_2,
        e.tag_3 = row.tag_3,
        e.type = row.type,
        e.country_region = row.country_region,
        e.industry = row.industry,
        e.permID = row.permID,
        e.incorporated_country = row.incorporated_country,
        e.incorporated_date = row.incorporated_date,
        e.total_revenue = toFloat(row.total_revenue),
        e.total_revenue_ajustado = toFloat(row.total_revenue_ajustado),
        e.employees = toInteger(row.employees),
        e.market_cap = toFloat(row.market_cap),
        e.market_cap_ajustado = toFloat(row.market_cap_ajustado),
        e.implied_rating = row.implied_rating,
        e.moody_rating = row.moody_rating,
        e.fitch_rating = row.fitch_rating,
        e.ownership_per = toFloat(row.ownership_per),
        e.ric = row.ric,
        e.issued_bonds = toInteger(row.issued_bonds),
        e.outstanding_loans = toInteger(row.outstanding_loans),
        e.cds = toInteger(row.cds),
        e.commercial_paper = toInteger(row.commercial_paper),
        e.futures = toInteger(row.commercial_paper),
        e.options = toInteger(row.options),
        e.warrants = toInteger(row.warrants),
        e.equities = toInteger(row.equities),
        e.certificate_deposits = toInteger(row.certificate_deposits),
        e.preference_shares = toInteger(row.preference_shares),
        e.investment_certificates = toInteger(row.investment_certificates),
        e.funds_count = toInteger(row.funds_count),
        e.relationship_type = toInteger(row.relationship_type),
        e.city = row.city,
        e.industry_reclassified = row.industry_reclassified,
        e.industry_reclassified_2 = row.industry_reclassified_2,
        e.entidad_principal = row.entidad_principal,
        e.immediate_parent = row.immediate_parent,
        e.ultimate_parent = row.ultimate_parent,
        e.age = row.age,
        e.grupo_familiar = row.grupo_familiar;"
        
        query_aux_1 <- "match (n:load {tag_1:'empresa'})
                        set n:empresa
                        remove n:load;"
        
        query_aux_2 <- "match (n:load {tag_1: 'persona'})
                        set n:persona
                        remove n:load;"
        
        query_aux_3 <- "match (n {entidad_principal: 'si'})
                        set n:principal;"
        
        query_aux_4 <- "match (n {tag_2:'accionista'})
                        set n:accionista;"
        
        query_aux_5 <- "match (n {tag_2:'filial'})
                        set n:filial;"
        
        query_relaciones_accionistas <- "LOAD CSV WITH HEADERS
                                        from 'file:///lista_entidades_principales_relaciones_accionistas.csv' as row
                                        match (source{name: row.source})
                                        match (target{name: row.target})
                                        merge (source) -[r:accionista]->(target)
                                        set 
                                        r.ranking = toFloat(row.ranking),
                                        r.outstanding = toFloat(row.outstanding),
                                        r.position = toFloat(row.position),
                                        r.position_change = toFloat(row.position_change),
                                        r.value = toFloat(row.value),
                                        r.value_ajustado = toFloat(row.value_ajustado),
                                        r.filing_date = row.filing_date,
                                        r.filing_source = row.filing_source,
                                        r.investor_type = row.investor_type,
                                        r.investor_subtype = row.investor_subtype,
                                        r.turnover = row.turnover,
                                        r.target = row.target;"
        
        query_relaciones_filiales <- "LOAD CSV WITH HEADERS
                                      from 'file:///lista_entidades_principales_relaciones_filiales.csv' as row
                                      match (source{name: row.source})
                                      match (target{name: row.target})
                                      merge (source) -[r:filial]->(target)
                                      set 
                                      r.ownership_per = toFloat(row.ownership_per),
                                      r.relationship_type = row.relationship_type;"
        
        query_aux_6 <- "match (a)<-[r:accionista]-(n)
                        set a:accionista;"
        
        query_aux_7 <- "match (f)<-[r:filial]-(n)
                        set f:filial;"
          
        use_virtualenv("~/.venvs/neo4j_env", required = TRUE)
        user_neo4j <- "neo4j"
        password_neo4j <- "norpentar"
        
        result_query_nodos_entidades <- run_query("bolt://localhost:7687", user_neo4j, password_neo4j, query_nodos_entidades)
        result_query_aux_1 <- run_query("bolt://localhost:7687", user_neo4j, password_neo4j, query_aux_1)
        result_query_aux_2 <- run_query("bolt://localhost:7687", user_neo4j, password_neo4j, query_aux_2)
        result_query_aux_3 <- run_query("bolt://localhost:7687", user_neo4j, password_neo4j, query_aux_3)
        result_query_aux_4 <- run_query("bolt://localhost:7687", user_neo4j, password_neo4j, query_aux_4)
        result_query_aux_5 <- run_query("bolt://localhost:7687", user_neo4j, password_neo4j, query_aux_5)
        result_query_relaciones_accionistas <- run_query("bolt://localhost:7687", user_neo4j, password_neo4j, query_relaciones_accionistas)
        result_query_relaciones_filiales <- run_query("bolt://localhost:7687", user_neo4j, password_neo4j, query_relaciones_filiales)
        result_query_aux_6 <- run_query("bolt://localhost:7687", user_neo4j, password_neo4j, query_aux_6)
        result_query_aux_7 <- run_query("bolt://localhost:7687", user_neo4j, password_neo4j, query_aux_7)
        
        self$insertNeo4jFinancialsEmpresas()
        
        msg <- ("La base se insertó en Neo4j correctamente\n")
        log_debug(msg, namespace = "objetos")
        
        
      }
      
      
      return(list(tabla_nodos_entidades, tabla_accionistas, tabla_filiales, lista_nodos))
      
    },
    aumentaEntidad = function(entidad){
      #stopifnot(class(empresa)[1] == "Empresa")
      name <- entidad$name
      if(!self$verificaEntidadLista(name)){
        private$.entidades <- append(private$.entidades , entidad)
        assign(name, entidad, envir = private$.indice_entidades)  # lo registramos en el índice
      } else{
        msg <- glue::glue("ListaEntidades/aumentaEntidad, entidad ({name}) ya existe en la lista ({private$.name})")
        log_error_multi(msg, namespaces = c("global", "objetos"))
      }
      
      invisible(self)
    },
    extraeEntidadesPrincipales = function(valor = "si", nombre ="principales"){
      entidades_out_lista <- private$.entidades[which(sapply(private$.entidades, \(e) e$entidad_principal == valor))]
      lista_entidades_out <- ListaEntidades$new(name=paste("lista_entidades_",nombre, sep = ""), entidades = entidades_out_lista)
      return(lista_entidades_out)
    },
    verificaEntidadLista = function (nombre_entidad){
      nombre_entidad <- gsub("‘|’|'|-|–", " ", nombre_entidad) 
      return(exists(nombre_entidad, envir = private$.indice_entidades, inherits = FALSE))
    },
    extraeEntidadLista = function (nombre_entidad){
      nombre_entidad <- gsub("‘|’|'|-|–", " ", nombre_entidad)
      #entidad_encontrada <- Filter(function(entidad) entidad$name == nombre_entidad, private$.entidades)
      #return(entidad_encontrada[[1]])
      if(self$verificaEntidadLista(nombre_entidad)){
        return(get(nombre_entidad, envir = private$.indice_entidades))
      }
      else{
        msg <- glue::glue("La entidad ({nombre_entidad}) no se encuentra dentro de la lista ({private$.name})")
        log_error_multi(msg, namespaces = c("global", "objetos"))
        return(NULL)
      }
    },
    extraeCreaEntidadLista = function(nombre_entidad, tipo = "empresa"){
      nombre_entidad <- gsub("‘|’|'|-|–", " ", nombre_entidad)
      if(self$verificaEntidadLista(nombre_entidad)){
        return(self$extraeEntidadLista(nombre_entidad))
      }
      else{
        if(tipo == "Individual Investor"){
          persona <- Persona$new(name = nombre_entidad)
          self$aumentaEntidad(persona)
          return(persona)
        }
        else{
          empresa <- Empresa$new(name = nombre_entidad)
          self$aumentaEntidad(empresa)
          return(empresa)
        }
      }
    },
    entidadesTabulado = function(){
      lista_empresas <- list()
      for (entidad in private$.entidades){
        if(class(entidad)[[1]] == "Empresa"){
          lista_empresas <- append(lista_empresas, entidad)
        }
        else{
          stop(paste("La entidad: ",entidad$name," no es del tipo empresa, no se puede sacar la tabla empresarial\n", sep = ""))
        }
      }
      
      lista_tabla_empresas <- lapply(lista_empresas, function(e){
        tabla <- e$sacaPropiedades()
        if (nrow(tabla) == 0) return(NULL) else return(tabla)
        })
      tabla_empresas <- lista_tabla_empresas %>% compact() %>% bind_rows()
      
      write.csv(tabla_empresas, file=here("export", "tablas", paste(private$.name, "_entidadesTabulado", sep = "")), row.names = FALSE)
      return(tabla_empresas)
    },
    entidadesTabuladoAgrupado = function(campo_agrupacion, campo_ordenacion = NULL){
      stopifnot(is.character(campo_agrupacion))
      #saco la tabla en bruto
      tabla <- self$entidadesTabulado()
      
      #identifico los campos numéricos y los campos string
      campos_numericos <- tabla %>% select(where(is.numeric)) %>% colnames()
      campos_string <- tabla %>% select(where(is.character)) %>% colnames()
      
      #remuevo el campo string agrupación del resto de campos string
      campos_string <- setdiff(campos_string, campo_agrupacion)
      
      #ordeno la tabla
      
      #construyo la tabla resumida
      tabla <- tabla %>% group_by(across(all_of(campo_agrupacion))) %>% summarise(count_unique_entidades= n_distinct(name), across(all_of(campos_string), ~ paste(unique(.), collapse = ", "), .names = paste0("total_","{.col}")),
                                                                                  across(all_of(campos_numericos), \(x) sum(x, na.rm = TRUE), .names = paste0("total_","{.col}")),
                                                                                  .groups = "drop"
      )
      #pongo los valores percentuales para los campos numéricos y ordeno
      tabla <- tabla %>% mutate(across(all_of(paste0("total_",campos_numericos)), \(x) round(100 * x/sum(x, na.rm = TRUE),2), .names = paste0("{col}","_per")))
      if (!is.null(campo_ordenacion)){
        tabla <- tabla %>% arrange(desc(!!sym(campo_ordenacion)))
      }
      
      write.csv(tabla, file=here::here("export", "tablas", paste(private$.name, "_entidadesTabulado_", campo_agrupacion, ".csv", sep = "")), row.names = FALSE)
      return(tabla)
    },
    entidadesAccionistasTabulado = function(){
      lista_tabulados <- lapply(private$.entidades, function(e){
                          tabulado <- e$accionistasTabulado()
                          if (nrow(tabulado) == 0) return(NULL) else return(tabulado)
                        })
      tabla <- lista_tabulados %>% compact() %>% bind_rows() #elimino nulos
      
      write.csv(tabla, file=here::here("export","tablas", paste(private$.name, "_entidadesAccionistasTabulado.csv", sep = "")), row.names = FALSE)
      return(tabla)
    },
    entidadesAccionistasTabuladoAgrupado = function(campo_agrupacion, campo_ordenacion = NULL){
      stopifnot(is.character(campo_agrupacion))
      #saco la tabla en bruto
      tabla <- self$entidadesAccionistasTabulado()
      
      #identifico los campos numéricos y los campos string
      campos_numericos <- tabla %>% select(where(is.numeric)) %>% colnames()
      campos_string <- tabla %>% select(where(is.character)) %>% colnames()
      
      #remuevo el campo string agrupación del resto de campos string
      campos_string <- setdiff(campos_string, campo_agrupacion)
      
      #ordeno la tabla
      
      #construyo la tabla resumida
      tabla <- tabla %>% group_by(across(all_of(campo_agrupacion))) %>% summarise(count_unique_empresas_principales = n_distinct(empresa_principal), count_investor_name = n_distinct(investor_name), across(all_of(campos_string), ~ paste(unique(.), collapse = ", "), .names = paste0("total_","{.col}")),
                                                                       across(all_of(campos_numericos), \(x) sum(x, na.rm = TRUE), .names = paste0("total_","{.col}")),
                                                                       .groups = "drop"
                                                                        )
      #pongo los valores percentuales para los campos numéricos y ordeno
      tabla <- tabla %>% mutate(across(all_of(paste0("total_",campos_numericos)), \(x) round(100 * x/sum(x, na.rm = TRUE),2), .names = paste0("{col}","_per")))
      if (!is.null(campo_ordenacion)){
        tabla <- tabla %>% arrange(desc(!!sym(campo_ordenacion)))
      }
      
      write.csv(tabla, file=here::here("export", "tablas", paste(private$.name, "_entidadesAccionistasTabuladoAgrupado_", campo_agrupacion, ".csv", sep = "")), row.names = FALSE)
      return(tabla)
    },
    entidadesFilialesTabulado = function(){
      lista_tabulados <- lapply(private$.entidades, function(e){
                                tabulado <- e$filialesTabulado()
                                if(nrow(tabulado) == 0) return(NULL) else return(tabulado)
                                })
      tabla <- lista_tabulados %>% compact() %>% bind_rows() #elimino nulos
      write.csv(tabla, file=here::here("export", "tablas", paste(private$.name, "_entidadesFilialesTabulado.csv", sep = "")), row.names = FALSE)
      return(tabla)
    },
    entidadesFilialesTabuladoAgrupado = function(campo_agrupacion, campo_ordenacion = NULL){
      stopifnot(is.character(campo_agrupacion))
      #saco la tabla en bruto
      tabla <- self$entidadesFilialesTabulado()
      
      #identifico los campos numéricos y los campos string
      campos_numericos <- tabla %>% select(where(is.numeric)) %>% colnames()
      campos_string <- tabla %>% select(where(is.character)) %>% colnames()
      
      #remuevo el campo string agrupación del resto de campos string
      campos_string <- setdiff(campos_string, campo_agrupacion)
      
      #ordeno la tabla
      
      #construyo la tabla resumida
      tabla <- tabla %>% group_by(across(all_of(campo_agrupacion))) %>% summarise(count_unique_empresas_principales = n_distinct(empresa_principal), count_filiales = n_distinct(empresa_filial), across(all_of(campos_string), ~ paste(unique(.), collapse = ", "), .names = paste0("total_","{.col}")),
                                                                                  across(all_of(campos_numericos), \(x) sum(x, na.rm = TRUE), .names = paste0("total_","{.col}")),
                                                                                  .groups = "drop"
      )
      #pongo los valores percentuales para los campos numéricos y ordeno
      tabla <- tabla %>% mutate(across(all_of(paste0("total_",campos_numericos)), \(x) round(100 * x/sum(x, na.rm = TRUE),2), .names = paste0("{col}","_per")))
      if (!is.null(campo_ordenacion)){
        tabla <- tabla %>% arrange(desc(!!sym(campo_ordenacion)))
      }
      
      write.csv(tabla, file=here::here("export", "tablas", paste(private$.name, "_entidadesFilialesTabuladoAgrupado", campo_agrupacion, ".csv", sep = "")), row.names = FALSE)
      return(tabla)
    },
    
    buscaEntidades = function(string_entidad){
      entidades_seleccionadas <- private$.entidades[str_detect(sapply(private$.entidades, \(x) x$name), paste0(".*",tolower(string_entidad),".*"))]
      lista_entidades <- ListaEntidades$new("lista_entidades",entidades_seleccionadas)
      return(lista_entidades)
      
    },
    extraeEntidadesNombre = function(lista_nombres){
      lista_entidades <- list()
      
      verificaEntidadNombre <- function(entidad, lista_nombres_entidades){
        verifica <- FALSE
        for(nombre in lista_nombres_entidades){
          if (nombre == entidad$name) {
            verifica <- TRUE
          }
        }
        return(verifica)
      }
      
      for(entidad in self$entidades){
        if(verificaEntidadNombre(entidad, lista_nombres) == TRUE){
          lista_entidades <- append(lista_entidades, entidad)
        }  
      }    
      lista_entidades <- ListaEntidades$new("lista_entidades_extraídas_nombre",lista_entidades)
      return(lista_entidades)
      
    },
    extraeEntidadesPais = function(valor){
      lista_entidades <- list()
      for (entidad in self$entidades){
        if (!is.na(entidad$country_region) & (str_detect(tolower(entidad$country_region), tolower(paste0(".*",valor,".*"))))){
          lista_entidades <- append(lista_entidades, entidad)
        }  
      }
      lista_entidades <- ListaEntidades$new("lista_entidades_extraídas_país",lista_entidades)
      return(lista_entidades)
      
    },
    extraeEntidadesIndustria = function(valor){
      lista_entidades <- list()
      for (entidad in self$entidades){
        if (!is.na(entidad$industry) & (str_detect(tolower(entidad$industry), tolower(paste0(".*",valor,".*"))))){
          lista_entidades <- append(lista_entidades, entidad)
        }  
      }
      lista_entidades <- ListaEntidades$new("lista_entidades_extraídas_industria",lista_entidades)
      return(lista_entidades)
      
    },
    extraeEntidadesNombreAccionistas = function(valor){
      verificaAccionista <- function(entidad, accionista_name){
        for(accionista in entidad$accionistas){
          if(str_detect(tolower(accionista$entidad$name),tolower(paste0(".*",accionista_name,".*")))){
            return(TRUE)
          }
        }
        return(FALSE)
      }
      lista_entidades <- list()
      for (entidad in private$.entidades){
        if(verificaAccionista(entidad, valor)){
          lista_entidades <- append(lista_entidades, entidad)
        }
      }
      lista_entidades <- ListaEntidades$new("lista_entidades_extraídas_accionistas",lista_entidades)
      return(lista_entidades)
    },
    extraeEntidadesPaisAccionistas = function(valor){
      verificaAccionista <- function(entidad, accionista_pais){
        for(accionista in entidad$accionistas){
          if((!is.na(accionista$entidad$country_region)) & (str_detect(tolower(accionista$entidad$country_region),tolower(paste0(".*",accionista_pais,".*"))))){
            return(TRUE)
          }
        }
        return(FALSE)
      }
      lista_entidades <- list()
      for (entidad in private$.entidades){
        if(verificaAccionista(entidad, valor)){
          lista_entidades <- append(lista_entidades, entidad)
        }
      }
      lista_entidades <- ListaEntidades$new("lista_entidades_extraídas_país_accionistas",lista_entidades)
      return(lista_entidades)
    },
    entidadesAccionistasFiltrado = function(columna_filtro, string, campo_agrupacion = NULL, campo_ordenacion = NULL){
      tabla <- self$entidadesAccionistasTabulado()
      tabla <- tabla %>% filter(str_detect(tolower(!!sym(columna_filtro)), tolower(paste0(".*",string,".*"))))
      
      if(!is.null(campo_agrupacion)){
        #identifico los campos numéricos y los campos string
        campos_numericos <- tabla %>% select(where(is.numeric)) %>% colnames()
        campos_string <- tabla %>% select(where(is.character)) %>% colnames()
      
        #remuevo el campo string agrupación del resto de campos string
        campos_string <- setdiff(campos_string, campo_agrupacion)
      
        #ordeno la tabla
      
        #construyo la tabla resumida
        tabla <- tabla %>% group_by(across(all_of(campo_agrupacion))) %>% summarise(count_unique_empresas_principales = n_distinct(empresa_principal), count_unique_investor_name = n_distinct(investor_name), across(all_of(campos_string), ~ paste(unique(.), collapse = ", "), .names = paste0("total_","{.col}")),
                                                                                  across(all_of(campos_numericos), \(x) sum(x, na.rm = TRUE), .names = paste0("total_","{.col}")),
                                                                                  .groups = "drop"
                                                                          )
        #pongo los valores percentuales para los campos numéricos y ordeno la tabla
        tabla <- tabla %>% mutate(across(all_of(paste0("total_",campos_numericos)), \(x) round(100 * x/sum(x, na.rm = TRUE),2), .names = paste0("{col}","_per")))
      }
      if (!is.null(campo_ordenacion)){
        tabla <- tabla %>% arrange(desc(!!sym(campo_ordenacion)))
      }
      
      return(tabla)
    },
    
    queryCypherFinancialsEmpresas = function(){
      queries <- sapply(private$.entidades, \(entidad) entidad$queryCypherFinancials())
      query <- paste(queries, collapse = "\n")
      write(query, file = here::here("export", "tablas", "neo4j", "queryFinancials.txt"))
      return(query)
    },
    insertNeo4jFinancialsEmpresas = function(){
      lapply(private$.entidades, \(entidad) entidad$insertNeo4jFinancials())
    },
    
    entidadesFinancialsTabulado = function(tipo){
      tablaEmpresa <- function(empresa){
        tabla <- if(tipo == "currency"){
          empresa$financials
        } else if(tipo == "dolares"){
          empresa$financials_dolares
        } else if(tipo == "dolares_ajustados"){
          empresa$financials_dolares_ajustados
        } else {
          NULL
        }
        if(!is.null(tabla)){
          country_region <- empresa$country_region
          currency <- if(tipo == "currency"){
            empresa$currency
          } else {
            "USD"
          }
          tabla <- tabla %>% mutate(country_region=country_region,currency=currency) %>% relocate(empresa,country_region,currency)
        }
        return(tabla)
      }
      lista_tablas_financials <- lapply(private$.entidades, \(entidad) tablaEmpresa(entidad))
      tabla_financials <-lista_tablas_financials %>% compact() %>% bind_rows()
      
      return (tabla_financials)
    },
    ajustaTreeStructureEmpresas = function(anho_base = "2023", anho_recoleccion = "2023"){
      for (entidad in private$.entidades){
        entidad$ajustaTreeStructure(anho_base = anho_base, anho_recoleccion = anho_recoleccion)
      }
      invisible(self)
    },
    ajustaShareholdersEmpresas = function(anho_base = "2023", anho_recoleccion = "2023"){
      for (entidad in private$.entidades){
        entidad$ajustaShareholders(anho_base = anho_base, anho_recoleccion = anho_recoleccion)
      }
      invisible(self)
    },
    rectificaValoresEmpresas = function(){
      #ajusto empresas que no tienen país.
      e1 <- "Hapvida Participacoes e Investimentos SA"
      e2 <- "Pluz Energia Peru SAA"
      
      emp_1 <- self$extraeEntidadesNombre(e1)$entidades
      emp_2 <- self$extraeEntidadesNombre(e2)$entidades

      if(length(emp_1) == 1){
        emp_1[[1]]$country_region <- "Brazil"
        emp_1[[1]]$industry <- "Financial & Commodity Market Operators & Service Providers"
        emp_1[[1]]$industry_reclassified <- "Investment Companies"
      }  
      if(length(emp_2) == 1){
        emp_2[[1]]$country_region <- "Peru"
        emp_2[[1]]$industry <- "Natural Gas Utilities"
        emp_2[[1]]$industry_reclassified <- "Utilities"
      }
      invisible(self)
    },
    
    rectificaFinancialsEmpresas = function(){
      emp_1 <- "Energisa Mato Grosso Distribuidora de Energia SA"
      emp_1 <- self$extraeEntidadesNombre(emp_1)$entidades
      if(length(emp_1) == 1){
        emp_1 <- emp_1[[1]]
        if (!is.null(emp_1$financials)){
          emp_1$financials[(emp_1$financials$variable == "market capitalization")&(emp_1$financials$anho == "1996"), "valor"] <- NA
          emp_1$financials[(emp_1$financials$variable == "market capitalization")&(emp_1$financials$anho == "1997"), "valor"] <- NA
          emp_1$financials_dolares[(emp_1$financials_dolares$variable == "market capitalization")&(emp_1$financials_dolares$anho == "1996"), "valor"] <- NA
          emp_1$financials_dolares[(emp_1$financials_dolares$variable == "market capitalization")&(emp_1$financials_dolares$anho == "1997"), "valor"] <- NA
          emp_1$financials_dolares_ajustados[(emp_1$financials_dolares_ajustados$variable == "market capitalization")&(emp_1$financials_dolares_ajustados$anho == "1996"), "valor"] <- NA
          emp_1$financials_dolares_ajustados[(emp_1$financials_dolares_ajustados$variable == "market capitalization")&(emp_1$financials_dolares_ajustados$anho == "1997"), "valor"] <- NA
        }
      }
      invisible(self)
    },
    #esta función carga las currencies según el paíse de las empresas. Es importante que las empresas tengan país definido, si no da error.
    cargaCurrenciesPaisesEmpresas = function(){
      tabla_divisas_paises <- read_excel("./tablas_input/auxiliares/tabla_divisas_paises.xlsx")
      
      for(entidad in private$.entidades){
        entidad$cargaCurrencyPais(tabla_divisas_paises)
      }
      invisible(self)
    }
    
   
   
  ),
  private = list(
    .name = NA,
    .entidades = NULL,
    .indice_entidades = NULL
  ),
  active = list(
    name = function(value){
      if (missing(value)){
        private$.name
      }
      else{
        stopifnot(is.list(value))
        private$.name <- value
        invisible(self)
      }
    },
    entidades = function(value){
      if (missing(value)){
        private$.entidades
      }
      else{
        stopifnot(is.list(value))
        private$.entidades <- value
        invisible(self)
      }
    }
    
  )
)



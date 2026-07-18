#Script para procesar las tablas de familias de las empresas

procesaFamilias <- function(archivo_familias = "./tablas_input/auxiliares/familias_empresas.xlsx", lista_entidades_principales_in){
  msg <- glue::glue("Entrando en la función procesa Familias con parámetros archivo_familias={archivo_familias} y lista_entidades={lista_entidades_principales_in$name}")
  log_info_multi(msg, namespaces = c("global", "familias"))
  tabla_familias <- read_excel(archivo_familias)
  lista_entidades_principales <- lista_entidades_principales_in
  
  for(i in 1:nrow(tabla_familias)){
    empresa_name <- tabla_familias$name[i]
    
    if (lista_entidades_principales$verificaEntidadLista(empresa_name)){
      msg <- glue::glue("En el bucle de porcesa familias con la empresa: {empresa_name}")
      log_trace_multi(msg, namespaces = c("familias"))
      empresa <- lista_entidades_principales$extraeEntidadLista(empresa_name)
      empresa$grupo_familiar <- tabla_familias$grupo_familiar[i]
      empresa$nombre_familias <- tabla_familias$nombre_familias[i]
      empresa$pais_origen_familias <- tabla_familias$pais_origen_familias[i]
    } else {
      msg <- glue::glue("La entidad: {empresa_name} no ha sido encontrada en la lista")
      log_debug_multi(msg, namespaces = c("familias"))
    }
    
  }
  
  
  return(lista_entidades_principales)
}


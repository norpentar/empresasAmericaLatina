#Script para procesar las tablas de familias de las empresas

procesaFamilias <- function(archivo_familias = "./tablas_input/Familias/familias_empresas.xlsx", lista_entidades_principales_in){
  tabla_familias <- read_excel(archivo_familias)
  lista_entidades_principales <- lista_entidades_principales_in
  
  for(i in 1:nrow(tabla_familias)){
    empresa_name <- tabla_familias$name[i]
    
    if (lista_entidades_principales$verificaEntidadLista(empresa_name)){
      empresa <- lista_entidades_principales$extraeEntidadLista(empresa_name)
      empresa$grupo_familiar <- tabla_familias$grupo_familiar[i]
      empresa$nombre_familias <- tabla_familias$nombre_familias[i]
      empresa$pais_origen_familias <- tabla_familias$pais_origen_familias[i]
    } else {
      stop(paste("La entidad: ",empresa_name," no ha sido encontrada en la lista\n", sep = ""))
    }
    
  }
  
  
  return(lista_entidades_principales)
}


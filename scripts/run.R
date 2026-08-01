library(profvis)
library(qs2)

#profvis({
  
source("./scripts/clases_objetos.R")
source("./scripts/auxiliares.R")
source("./scripts/shareholders.R")
source("./scripts/treeStructure.R")
source("./scripts/financials.R")
source("./scripts/familias.R")
source("./scripts/config_logger.R")


shareholders <- cargaShareholdersArray()
lista <- procesaShareholders(shareholders_input = shareholders)

treeStructure <- cargaTreeStructureArray()
lista <- procesaTreeStructure(treeStructure_input = treeStructure, lista_empresas_principales_in = lista[[1]])
lista_empresas_principales <- lista[[1]]
lista_empresas_principales$rectificaValoresEmpresas()
lista_empresas_principales$cargaCurrenciesPaisesEmpresas()

lista_empresas_principales <- procesaFinancials(lista_empresas_principales_in = lista_empresas_principales)
lista_empresas_principales$rectificaFinancialsEmpresas()

#ajusto valores a dólares en año base 2023.
lista_empresas_principales$ajustaShareholdersEmpresas()
lista_empresas_principales$ajustaTreeStructureEmpresas()


lista_empresas_principales <- procesaFamilias(lista_entidades_principales_in=lista_empresas_principales)
lista_empresas_principales <- procesaReclasificaIndustriaEmpresa2(lista_entidades_principales_in=lista_empresas_principales)

#Red
#lista_red <- lista_empresas_principales$sacaTablasRedListaEntidades(,insertar_neo4j=TRUE)
#tabla_nodos <- lista_red[[1]]
#tabla_relaciones_accionistas <- lista_red[[2]]
#tabla_relaciones_filiales <- lista_red[[3]]
#lista_nodos <- lista_red[[4]]

#Acá genero las tablas que luego necesitaré en los análisis
tabla_empresas <- lista_empresas_principales$entidadesTabulado()
qs_save(tabla_empresas, here::here("export","objetos","tabla_empresas.qs2"))
tabla_accionistas <- lista_empresas_principales$entidadesAccionistasTabulado()
qs_save(tabla_accionistas, file = here::here("export", "objetos", "tabla_accionistas.qs2"))
tabla_filiales <- lista_empresas_principales$entidadesFilialesTabulado()
qs_save(tabla_filiales, file = here::here("export", "objetos", "tabla_filiales.qs2"))
tabla_financials_currency <- lista_empresas_principales$entidadesFinancialsTabulado("currency")
qs_save(tabla_financials_currency, file = here::here("export", "objetos", "tabla_financials_currency.qs2"))
tabla_financials_dolares <- lista_empresas_principales$entidadesFinancialsTabulado("dolares")
qs_save(tabla_financials_dolares, file = here::here("export", "objetos", "tabla_financials_dolares.qs2"))
tabla_financials_dolares_ajustados <- lista_empresas_principales$entidadesFinancialsTabulado("dolares_ajustados")
qs_save(tabla_financials_dolares_ajustados, file = here::here("export", "objetos", "tabla_financials_dolares_ajustados.qs2"))


#})





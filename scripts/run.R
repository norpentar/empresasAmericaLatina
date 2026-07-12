source("./scripts/clases_objetos.R")
source("./scripts/auxiliares.R")
source("./scripts/shareholders.R")
source("./scripts/treeStructure.R")
source("./scripts/financials.R")
source("./scripts/familias.R")


shareholders <- cargaShareholdersArray()
lista <- procesaShareholders(shareholders_input = shareholders)

treeStructure <- cargaTreeStructureArray()
lista <- procesaTreeStructure(treeStructure_input = treeStructure, lista_empresas_principales_in = lista[[1]])

lista_empresas_principales <- lista[[1]]

lista_empresas_principales <- procesaFinancials(lista_empresas_principales_in = lista_empresas_principales)

#ajusto valores a dólares en año base 2023.
lista_empresas_principales$ajustaShareholdersEmpresas()
lista_empresas_principales$ajustaTreeStructureEmpresas()
lista_empresas_principales$ajustaFinancialsEmpresas()


lista_empresas_principales <- procesaFamilias(lista_entidades_principales_in=lista_empresas_principales)
lista_empresas_principales <- procesaReclasificaIndustriaEmpresa2(lista_entidades_principales_in=lista_empresas_principales)

#Red
#lista_red <- lista_empresas_principales$sacaTablasRedListaEntidades(,insertar_neo4j=TRUE)
#tabla_nodos <- lista_red[[1]]
#tabla_relaciones_accionistas <- lista_red[[2]]
#tabla_relaciones_filiales <- lista_red[[3]]
#lista_nodos <- lista_red[[4]]






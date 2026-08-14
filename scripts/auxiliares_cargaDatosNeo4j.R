library(dplyr)
#library(neo4r)
library(visNetwork)
library(bolt4jr)
library(reticulate)
library(readxl)
library(qs2)


use_virtualenv("~/.venvs/neo4j_env", required = TRUE)

user_neo4j <- "neo4j"
password_neo4j <- "norpentar"

query_1 <- "
match (empresa{entidad_principal: 'si'})<-[r:accionista]-(accionista) 
return
accionista.name,
accionista.country_region,
r.outstanding,
r.value,
r.value_ajustado,
empresa.name,
empresa.ultimate_parent,
empresa.incorporated_date,
empresa.grupo_familiar,
empresa.country_region,
empresa.industry,
empresa.industry_reclassified,
empresa.industry_reclassified_2,
empresa.market_cap,
empresa.market_cap_ajustado,
empresa.total_revenue,
empresa.total_revenue_ajustado
"
query_2 <- "
match (empresa{entidad_principal: 'si'})<-[r:accionista]-(accionista) 
with distinct empresa
return
empresa.name,
empresa.ultimate_parent,
empresa.incorporated_date,
empresa.grupo_familiar,
empresa.country_region,
empresa.industry,
empresa.industry_reclassified,
empresa.industry_reclassified_2,
empresa.market_cap,
empresa.market_cap_ajustado,
empresa.total_revenue,
empresa.total_revenue_ajustado,
empresa.employees
"

result_query_1 <- run_query("bolt://localhost:7687", user_neo4j, password_neo4j, query_1)
data_1 <- convert_df(result_query_1, field_names = names(result_query_1[[1]]))
columnas_numericas_1 <- c("r.outstanding", "r.value", "r.value_ajustado", "empresa.market_cap", "empresa.market_cap_ajustado", "empresa.total_revenue", "empresa.total_revenue_ajustado")
data_1 <- data_1 %>% mutate(across(all_of(columnas_numericas_1), as.numeric))
result_query_2 <- run_query("bolt://localhost:7687", user_neo4j, password_neo4j, query_2)
data_2 <- convert_df(result_query_2, field_names = names(result_query_2[[1]]))
columnas_numericas_2 <- c("empresa.market_cap", "empresa.market_cap_ajustado", "empresa.total_revenue", "empresa.total_revenue_ajustado", "empresa.employees")
data_2 <- data_2 %>% mutate(across(all_of(columnas_numericas_2), as.numeric))

tabla_info_accionistas <- read_excel(here::here("tablas_input", "auxiliares", "tabla_info_accionistas.xlsx")) %>% 
  select(investor_name, ownership_structure, ownership_specific, ownership_family, ownership_multifamily,
         principal_activity, finance, country, region
  )
qs_save(data_1, file = here::here("export", "tablas", "peticiones_febrero_2026", "data_1.qs2"))
qs_save(data_2, file = here::here("export", "tablas", "peticiones_febrero_2026", "data_2.qs2"))
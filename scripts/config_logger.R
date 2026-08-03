library(logger)

######
#Niveles típicos de uso
#FATAL Error crítico, la aplicación no puede continuar
#ERROR Algo falló, pero el script puede seguir (o no)
#WARN Advertencia, algo raro pero no bloqueante
#SUCCESS Confirmación de que algo salió bien (propio de logger, no todos los loggers lo tienen)
#INFO Información general del flujo del programa
#DEBUG Detalle técnico para depurar
#TRACE El nivel más detallado, casi línea por línea
######

# Namespace para el módulo de caraga de accionistas
log_threshold(INFO, namespace = "accionistas")
log_appender(appender_tee("logs/accionistas.log"), namespace = "accionistas")

# Namespace para el módulo de caraga de filiales
log_threshold(INFO, namespace = "treestructure")
log_appender(appender_tee("logs/treestructure.log"), namespace = "treestructure")

# Namespace para el módulo financiero
log_threshold(TRACE, namespace = "financials")
log_appender(appender_tee("logs/financials.log"), namespace = "financials")

# Namespace para el módulo de configuración de familias
log_threshold(INFO, namespace = "familias")
log_appender(appender_tee("logs/familias.log"), namespace = "familias")

# Namespace para el módulo de configuración de las funciones auxiliares
log_threshold(INFO, namespace = "auxiliares")
log_appender(appender_tee("logs/auxiliares.log"), namespace = "auxiliares")

# Namespace para el módulo de configuración de los objetos
log_threshold(DEBUG, namespace = "objetos")
log_appender(appender_tee("logs/objetos.log"), namespace = "objetos")

# Namespace general (por si quieres logs que no pertenezcan a ningún módulo específico)
log_threshold(INFO, namespace = "global")
log_appender(appender_tee("logs/global.log"), namespace = "global")

### Funciones auxiliares para crear logs en varios namespaces a la vez.
log_trace_multi <- function(msg, namespaces = c("global")) {
  for (ns in namespaces) {
    log_trace(msg, namespace = ns)
  }
}
log_debug_multi <- function(msg, namespaces = c("global")) {
  for (ns in namespaces) {
    log_debug(msg, namespace = ns)
  }
}
log_info_multi <- function(msg, namespaces = c("global")) {
  for (ns in namespaces) {
    log_info(msg, namespace = ns)
  }
}
log_success_multi <- function(msg, namespaces = c("global")) {
  for (ns in namespaces) {
    log_success(msg, namespace = ns)
  }
}
log_warn_multi <- function(msg, namespaces = c("global")) {
  for (ns in namespaces) {
    log_warn(msg, namespace = ns)
  }
}
log_error_multi <- function(msg, namespaces = c("global")) {
  for (ns in namespaces) {
    log_error(msg, namespace = ns)
  }
}
log_fatal_multi <- function(msg, namespaces = c("global")) {
  for (ns in namespaces) {
    log_fatal(msg, namespace = ns)
  }
}
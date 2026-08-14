empresas <- nodes_grafo_accionistas_ampliado %>% filter(group == "Empresa")
relaciones_empresas <- edges_grafo_accionistas_ampliado filter((from %in% empresas$id)&(to %in% empresas$id))

g <- graph_from_data_frame(edges_grafo_accionistas_ampliado, directed = TRUE, vertices = nodes_grafo_accionistas_ampliado)


# V(g)$group debe ser TRUE/FALSE o un factor de 2 niveles
tipos <- V(g)$group == "Empresa"  # TRUE = accionista, FALSE = empresa

# Chequeo: ¿hay aristas dentro del mismo grupo?
el <- as_edgelist(g, names = FALSE)
mismo_grupo <- tipos[el[,1]] == tipos[el[,2]]
sum(mismo_grupo)  # si es > 0, bipartite_projection va a fallar


# Filtrar solo aristas empresa -> accionista
tipos <- V(g)$group == "Accionista"
el <- as_edgelist(g, names = FALSE)
es_empresa_accionista <- tipos[el[,1]] != tipos[el[,2]]  # bipartita: distinto grupo en cada extremo

g_bipartito <- subgraph_from_edges(g, E(g)[es_empresa_accionista], delete.vertices = FALSE)
V(g_bipartito)$type <- V(g_bipartito)$group == "Accionista"

proy <- bipartite_projection(g_bipartito, multiplicity = TRUE)

#Confirmar cuál proyección es la de empresas
vcount(proy$proj1)
vcount(proy$proj2)

g_empresas <- proy$proj1  # (ajustar según el chequeo de arriba)

#rm(proy)  # opcional, para no tener proj2 dando vueltas si no lo vas a usar

sueltas <- V(g_empresas)[degree(g_empresas) == 0]
length(sueltas)
V(g_empresas)$name[degree(g_empresas) == 0]

pares <- as_data_frame(g_empresas, what = "edges") %>%
  arrange(desc(weight))

head(pares, 20)

V(g_empresas)$grado_ponderado <- strength(g_empresas, weights = E(g_empresas)$weight)

ranking <- data.frame(
  empresa = V(g_empresas)$name,
  grado_ponderado = V(g_empresas)$grado_ponderado,
  grado_simple = degree(g_empresas)
) %>% arrange(desc(grado_ponderado))

head(ranking, 20)


grafo_red_accionistas_ampliado <- visNetwork(nodes_grafo_accionistas_ampliado, edges_grafo_accionistas_ampliado, width = "100%", height = "600px") %>%
  visGroups(groupname = "Empresa",    color = "#4a90d9", shape = "dot") %>%
  visGroups(groupname = "Accionista", color = "#d94a4a", shape = "dot") %>%
  visNodes(scaling = list(min = 15, max = 300)) %>%   # rango visual de tamaño según 'value'
  visEdges(smooth = FALSE) %>%
  visPhysics(
    solver = "forceAtlas2Based",
    forceAtlas2Based = list(
      gravitationalConstant = -2000,   # antes -300: mucha más repulsión
      centralGravity = 0.005,          # nueva: casi anula el "tirón" hacia el centro
      springLength = 400,              # antes 250: más distancia ideal por arista
      springConstant = 0.08,           # antes 0.12: aristas menos rígidas, permite más despliegue
      avoidOverlap = 1,
      damping = 0.4                    # nueva: evita que oscile demasiado antes de asentarse
    ),
    stabilization = list(enabled = TRUE, iterations = 1000)  # antes 200: con más nodos, dale más tiempo para asentarse
  ) %>%
  visEvents(
    stabilizationIterationsDone = "function() { this.setOptions({ physics: false }); }"
  ) %>%
  visOptions(
    highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
    nodesIdSelection  = TRUE,
    # queda oculto: lo controlamos desde los dos selectores propios (ver bloque JS)
  ) %>%
  visLegend(
    main = list(text = "", style = "font-size: 8px;"),
    width = 0.16,      # antes por defecto 0.2 → ocupa menos ancho del canvas
    position = "right",
    stepX = 100,         # separación horizontal entre entradas
    stepY = 100,         # separación vertical entre entradas
    zoom = FALSE        # evita que el zoom del grafo agrande los nodos de la leyenda
  ) %>%
  visLayout(randomSeed = 321)










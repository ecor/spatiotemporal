# install.packages("foodwebr")
# remotes::install_github("ecor/spatiotemporal")
library(foodwebr)
library(spatiotemporal)

ns <- asNamespace("spatiotemporal")

# 1. Grafo completo delle funzioni del pacchetto
fw <- foodweb(env = ns)
fw          # stampa il riepilogo
plot(fw)    # grafo interattivo (DiagrammeR / graphviz)

# 2. Solo ciò che ruota attorno a una funzione (chi la chiama e chi chiama)
fw_tab <- foodweb(FUN = get_table, env = ns)
plot(fw_tab)

# 3. Matrice caller/callee (1 = la riga chiama la colonna)
m <- as.matrix(fw)
m

# 4. Testo graphviz, comodo da salvare o versionare
cat(foodweb(env = ns, as.text = TRUE), sep = "\n")
#writeLines(foodweb(env = ns, as.text = TRUE), "spatiotemporal_foodweb.gv")

# 5. Analisi con igraph/tidygraph (es. funzioni più "centrali")
g <- tidygraph::as_tbl_graph(fw)
igraph::degree(g, mode = "in")
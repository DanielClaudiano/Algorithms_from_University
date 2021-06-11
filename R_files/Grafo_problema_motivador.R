#### Problema motivador ####

# Um exemplo rápido e prático: suponha três pessoas P1, P2, e P3 (vértices). A pessoa P1 é amiga da pessoa P2 (aresta) - e aqui supomos que a recíproca é válida -, mas não de P3; e P3 não é amiga de P2 [nem de P1]. É possível representar isso esquematicamente traçando [apenas] uma linha que liga P1 a P2.

#### Elaborando o grafo ####

m <- matrix(c(0,1,0, 1,0,0, 0,0,0), nrow = 3, ncol = 3, byrow = T,
            dimnames = list(c("P1", "P2", "P3"),
                            c("P1", "P2", "P3"))) # Criando a matriz adjacência. 
m # Visualizando a matriz.

library(igraph) # Carregando o pacote que faz o grafo.

g = graph.adjacency(m,"undirected",NULL) # Qualificando a matriz m criada como uma matriz adjacente.
plot(g,vertex.size= 50,edge.arrow.size=30, edge.width=5, edge.color="blue") # Plotando o grafo.

#plot(graph, edge.arrow.size=.1, vertex.color="black",vertex.size=deg*1.5, 
#     vertex.frame.color="black", vertex.label.color="grey10", vertex.label.degree=-pi/6,
#     vertex.shape="circle",vertex.label.cex=1, vertex.label.dist=2,
#     vertex.label.font=1, edge.arrow.size=8, edge.width=0, edge.curved=0, edge.color="black",
#     edge.lty=1, layout=L)
#### Problema 2 ####

# Problema 2 (Kolman, 1998, adaptado) - Um grupo de seis indivíduos se reúne, já há algum tempo, em sessões de terapia em grupo. O terapeuta, que não faz parte do grupo [de seis indivíduos] deseja representar as relações de influência entre eles. Para isso, com apoio do software RStudio, traçou o seguinte grafo:

library(igraph)

m <- matrix(c(0,0,0,0,1,0, 0,0,0,0,1,0, 1,1,0,0,1,0, 0,1,0,0,1,0, 0,0,0,0,0,0, 0,1,0,1,0,0), nrow = 6, ncol = 6, byrow = T,
            dimnames = list(c("P1", "P2", "P3", "P4", "P5", "P6"),
                            c("P1", "P2", "P3", "P4", "P5", "P6"))) # Criando a matriz adjacência. 
m # Visualizando a matriz.

g = graph.adjacency(m,"directed",NULL) # Qualificando a matriz m criada como uma matriz adjacente.
plot(g, vertex.size = 30, edge.arrow.size=1, edge.width=3, edge.color="blue") # Plotando o grafo.

# Problema 3 (Anton, 2010, adaptado) torneio de tenistas

library(igraph)

m <- matrix(c(0,0,1,1,0, 1,0,1,0,1, 0,0,0,1,0, 0,1,0,0,0, 1,0,1,1,0), nrow = 5, ncol = 5, byrow = T,
            dimnames = list(c("P1", "P2", "P3", "P4", "P5"),
                            c("P1", "P2", "P3", "P4", "P5"))) # Criando a matriz  adjacencias
. 

m # Visualizando a matriz de adjacencias

g = graph.adjacency(m,"directed",NULL) # Qualificando a matriz m criada como uma matriz adjacente.

plot(g, edge.arrow.size=1,vertex.size=30,edge.width=3,edge.color="blue") 


########################################
# Matriz com a soma de todas as conexões

m2 = m%*%m # produto de matrizes (matriz ao quadrado)

m2 # Visualizando a matriz de adjacencias ao quadrado

a = m + m2

a # Visualizando a matriz.

poder = c(0,0,0,0,0) # vetor soma das linhas da matriz a (Poder dos Vertices)

for(i in 1:5) {
  for (j in 1:5){
    poder[i] = poder[i] +  a[i,j]
    }
}
poder

g = graph.adjacency(a,"directed",NULL) # Qualificando a matriz m criada como uma matriz adjacente.


plot(g, edge.arrow.size=.1, vertex.size=poder*10,edge.arrow.size=8, edge.width=3, edge.curved=0, edge.color="blue")



#rm(list = ls())
#Carregando dados
df = read.csv2(file="C:/Users/Daniel/Downloads/DATABASE_TRABALHO_ESTAT1.csv",header = TRUE,  sep=",")

#Gráfico Auxilio x Economia 
install.packages("dplyr")
require(dplyr)
#criando uma matriz através da função group by para contar as respostas de cada beneficiado ou não
#pelo auxílio emergencial
aux_eco = df %>% group_by(AUX_EMERG)
#criando as colunas da matriz
aux_eco %>% summarise(
  Sim_irrestritamente = sum(ECONOMIA == 1),
  Sim_com_restricoes = sum(ECONOMIA == 2),
  Nao = sum(ECONOMIA == 3),
  Nao_sei_opinar = sum(ECONOMIA == 4), 
  Total = sum(Sim_irrestritamente,Sim_com_restricoes,Nao,Nao_sei_opinar)
)
#?group_by()
#transformando a matriz em dataframe
df_aux_eco = data.frame(aux_eco %>% summarise(
  Sim_irrestritamente = sum(ECONOMIA == 1),
  Sim_com_restricoes = sum(ECONOMIA == 2),
  Nao = sum(ECONOMIA == 3),
  Nao_sei_opinar = sum(ECONOMIA == 4),
  Total = sum(Sim_irrestritamente,Sim_com_restricoes,Nao,Nao_sei_opinar)
  )
)
#Criando dataframe para eliminar a coluna AUX_EMERG
df_aux_eco_g = c(df_aux_eco$Sim_irrestritamente,df_aux_eco$Sim_com_restricoes,df_aux_eco$Nao,df_aux_eco$Nao_sei_opinar)
#Como o dataframe ficou com apenas uma coluna, os resultados tiveram linhas intercaladas.
#Linhas impares beneficiados, linhas pares nao beneficiados.
#Criei duas séries extraindo esses valores para cada grupo
#beneficiados
df_aux_eco_no = df_aux_eco_g[seq(1,8,2)] 
#não beneficiados
df_aux_eco_yes = df_aux_eco_g[seq(2,8,2)]

#Gráfico comparativo entre as variáveis Economia e Aux_Emerg
par(mfrow = c(1,2))
my_bar_yes = barplot(df_aux_eco_yes,
                 col = c("grey98","grey86","red","grey71"),
                 ylab = "Frequência",
                 xlab = "Família Recebeu Auxílo Emergencial",
                 xlim = c(0,5),
                 ylim = c(0,8),
                 axes = FALSE
                 )
text(x = my_bar_yes, y = df_aux_eco_yes+0.25, df_aux_eco_yes )
legend("topleft",legend = lb,fill = c("grey98","grey86","red","grey71"))

my_bar_no = barplot(df_aux_eco_no,
                  col = c("grey98","grey86","red","grey71"),
                  ylab = "Frequência",
                  xlab = "Família Não Recebeu Auxílo Emergencial",
                  xlim = c(0,5),
                  ylim = c(0,8),
                  axes = FALSE
                  )

lb = c("Sim, irrestritamente","Sim, com Restrições","Não","Não sei")
text(x = my_bar_no, y = df_aux_eco_no+0.25, df_aux_eco_no )
#text(0.5,0.5,"First title",cex=2,font=2)
title("Você é a favor de um Estado atuante na Economia de Mercado", line = -3, outer = TRUE)
#?par()
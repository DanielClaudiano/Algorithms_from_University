#rm(list = ls())
#Carregando dados
df = read.csv2(file="C:/Users/Daniel/Downloads/DATABASE_TRABALHO_ESTAT1.csv",header = TRUE,  sep=",")

#variavel auxilo
aux_sim = 0
aux_nao = 0
for (i in df$AUX_EMERG) {
  if (i == 1){
    aux_sim = aux_sim + 1
  }
  else{
    aux_nao = aux_nao+1
  }
}
aux_sim 
aux_nao 
resposta = c("Não Receberam","Receberam")
f = c(aux_nao,aux_sim)

fr = c(aux_nao/25,aux_sim/25)
fp = round(fr*100,2)
fa =c()
for (i in 1:2){
  if (i == 1){
    fa[i] = f[i]
  }
  else {
    fa[i] = f[i]+fa[i-1]
  }
}
fpa = round((fa/25)*100,2)


#dataframe sobre o auxilo
df_auxilio = data.frame(resposta,f,fa,fp,fpa)

#Gráfico em pizza de distribuição percentual 
pie(fr,
    clockwise = TRUE,
    labels = paste(fr*100,'%'),
    col = c("blue","red")
    #main = "Distribuição Percentual das Famílias de Calouros que Receberam Auxílio Emergencial")
)
legend("topright",legend = resposta,fill = c("blue","red"))

#variavel economia

eco_yes_plus= 0
eco_yes_minus = 0
eco_no = 0
eco_blank = 0

#contando os resultados das colunas para criar uma série única
for (i in df$ECONOMIA) {
  if (i == 1){
    eco_yes_plus = eco_yes_plus + 1
  }
  else if(i == 2){
    eco_yes_minus = eco_yes_minus+1
  }
  else if(i == 3){
    eco_no = eco_no+1
  }
  else{
    eco_blank = eco_blank+1
  }
}

Resposta = c("Sim, irrestritamente","Sim, com restrições","Não","Não sei opinar")
Resposta2 = c("Sim, com restrições","Não sei opinar","Sim,irrestritamente","Não")

F = c(eco_yes_plus,eco_yes_minus,eco_no,eco_blank)
F_sorted = sort(F, decreasing = FALSE)
F_sorted2 = sort(F,decreasing = TRUE )
Fr = F/25
Fp = round(Fr*100,2)
Fa =c()
for (i in 1:4){
  if (i == 1){
    Fa[i] = F[i]
  }
  else {
    Fa[i] = F[i]+Fa[i-1]
  }
}
Fpa = round((Fa/25)*100,2)

#dataframe sobre economia
df_economia = data.frame(Resposta,F,Fa,Fp,Fpa)

#Gráfico em barra horizontal da váriavel economia 
my_bar = barplot(F_sorted,
           horiz = TRUE,
           col = c("grey86","grey71","yellow","red"),
           ylab = "Frequência",
           xlab = "Respostas",
           xlim = c(0,10),
           axes = FALSE,
           main = "Você é a favor de um Estado atuante na Economia de Mercado? ")

legend("bottomright",legend = Resposta2,fill = c("red","yellow","grey86","grey71"))
text(y = my_bar, x = F_sorted+0.2, F_sorted )

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
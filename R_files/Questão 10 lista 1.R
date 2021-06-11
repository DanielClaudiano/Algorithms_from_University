#Questão 10

#Apresente um histograma sobre a sequência e crie uma tabela de frequencias(não precisa da linha dos totais) : 1,1,1,2,2,3,3,4,4,5,5,6,6,7,8

f = c(1,1,1,2,2,3,3,4,4,5,5,6,6,7,8)
#Determinando as classes 
minimo = min(f) 
maximo = max(f)
amp = maximo - minimo
n = length(f)
k = round(sqrt(n),0)
tam = amp/k
lim = seq(minimo,maximo,tam)
lim
classes = c('[1-2,75)','[2,75-4,5)','[4,5-6,25)','[6,25-8]')
#Determinando as frequências
F = c(5,4,4,2)
Fa =c()
for (i in 1:4){
  if (i == 1){
    Fa[i] = F[i]
  }
  else {
    Fa[i] = F[i]+Fa[i-1]
  }
}
Fap = round((Fa/n)*100,2)

#Criando a tabela de frequências
df_10 = data.frame(classes,F,Fa,Fap)

#Criando o Histograma
hist(f,
     main = 'Histograma da Sequência',
     xlab = 'Sequência',
     ylab = "Frequência",
     breaks = lim,
     right = FALSE,
     xaxt = 'n')

axis(side=1, at = seq(minimo,maximo,tam))




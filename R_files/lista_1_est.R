#Questao 7

library("graphics")

df = read.csv2(file="C:/Users/Daniel/Downloads/Lista1EstatPython.csv",header = TRUE,  sep=",")

df_idade = df$IDADE
df_f = df$F

x = c(rep(df_idade,df_f))

#df_dot =data.frame(x,y) 

abline(h=1)
stripchart(x,
           method = "stack",
           xaxt = "n",
           main = "Idade em início de gestação",
           xlab="Idade",
           ylab="Mães",
           ylim = c(0,20),
           col="brown3",
           pch=16,
           offset = 1,
          # vertical=TRUE,
)
axis(side = 1, at = df_idade )
axis(side = 2, at = (1:20) )

#Questao 2

f = c(8.0,8.1, 8.5,8.7,9.0,9.0,9.1,9.3,9.5,9.8,10.0,10.1,10.1,10.3,10.4,10.5, 10.5,10.5,10.6,10.6,11.2,11.3,11.5,12.7)
minimo = min(f) 
maximo = max(f)
amp = maximo - minimo
n = length(f)
k = round(sqrt(n),0)
tam = amp/k
tam1 =tam
lim = seq(minimo,maximo,tam1)
lim
Quantidade_Calcio_mg = c('[8.00-8.94)','[8.94-9.88)','[9.88-10.82)','[10.82-11.76)','[11.76-12.70]')
F = c(4,6,10,3,1)
Fa =c()
for (i in 1:5){
  if (i == 1){
    Fa[i] = F[i]
  }
  else {
    Fa[i] = F[i]+Fa[i-1]
  }
}
Fap = round((Fa/n)*100,2)

df_2 = data.frame(Quantidade_Calcio_mg,F,Fa,Fap)

hist(f,
     main = 'Quantidade de Cálcio (em mg/ml',
     xlab = 'Quantidade',
     ylab = "Frequência",
     breaks = lim,
     right = FALSE,
     xaxt = 'n')

axis(side=1, at = seq(minimo,maximo,tam1))

#classe de 9.88 até 10.82 é a mais concentrada

#Questão 4
Categoria = c('Castanhos','Pretos','Azuis','Verdes')
Fa = c(10,7,2,1)
Fr = c(0.5,0.35,0.10,0.05)
df_4 = data.frame(Categoria,Fa,Fr)
pie(Fr,
    clockwise = TRUE,
    labels = paste(Fr*100,'%'),
    col = c("brown","black","blue","green"),
    main = "Distribuição Percentual por Cor da Iris")

legend("topright",legend = Categoria,fill = c("brown","black","blue","green"))


#Questao 6

f = c(13,20,21, 21,22,24,25,25,26,27,28,30,30,30,31,31,31,33)
minimo = min(f) 
maximo = max(f)
amp = maximo - minimo
n = length(f)
k = round(sqrt(n),0)
tam = amp/k
tam1 =tam
lim = seq(minimo,maximo,tam1)
lim
Quantidade_Coliformes = c('[13-18)','[18-23)','[23-28)','[28-33]')
F = c(1,4,5,8)
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
Fp = round((F/n)*100,2)
df_6 = data.frame(Quantidade_Coliformes,F,Fa,Fp,Fap)

h_col =hist(f,
     main = 'Quantidade de Coliformes',
     xlab = 'Quantidade',
     ylab = "Frequência",
     breaks = lim,
     right = FALSE,
     xaxt = 'n')

axis(side=1, at = seq(minimo,maximo,tam1))

pm = c((h_col$mids[1]-tam1),h_col$mids,(h_col$mids[k]+tam1))

freq = c(0,h_col$counts,0)
freq

plot(freq~pm,type ='l',xlab='',ylab = 'Frequências',xaxt='n',bty='n',ylim=c(0,10),
     main = "Polígono de Frequências dos Coliformes Encontrados")
abline(h=0)
axis (side = 1, at = pm)
mtext(side = 1,'Pontos Médios - Coliformes', line = 2.5)

#Questao 8

Classificação = c("C/ poucas falhas","C/ muitas falhas","Sem falhas")
Quantidade= c(42,20,58)
Diametro = c("1,5","1,4","1,6","0,5","20,0")
Quantidaded = sort(c(10,170,280,130,10), decreasing = TRUE)
perc = round(Quantidaded/sum(Quantidaded),2)
df_8_1 = data.frame(Classificação,Quantidade)
df_8_2 = data.frame(Diametro,Quantidaded,perc)

pie(Quantidaded,
    clockwise = TRUE,
    labels = paste(perc*100,"%"),
    col = c("red","blue","white","black","yellow"),
    main = "Distribuição Percentual dos Diametros dos Tubos em cm")

legend("topright",legend = Diametro,fill = c("red","blue","white","black","yellow"))


barplot(height = Quantidade,xlab = "Número de Peças", main="Quantidade de Peças com Falhas",
        horiz=TRUE,names.arg=Classificação, xlim = c(0,60), col= c("blue","yellow","red") )
barplot()

#questao 9
f = c(43,56,56,57,58,58,58,59,60,60,61,61,62,62,62,63,64,64,65,65,65,66,67,67,67,67,67,68,68,69,69,70,70,70,71,71,71,71,72,72,73,73,75,76,89)
n = length(f)
minimo = min(f) 
maximo = max(f)
amp = maximo - minimo
n = length(f)
#k = 10
#tam = amp/k
#tam1 =tam
lim = seq(minimo-3,maximo+1,5)
lim
Peso = c('[40-46)','[46-50)','[50-56)','[56-60)','[60-66)','[66-70)','[70-76)','[76-80)','[80-86)','[86-90]')
F = c(1,0,0,7,10,13,11,2,0,1)
Fa =c()
for (i in 1:10){
  if (i == 1){
    Fa[i] = F[i]
  }
  else {
    Fa[i] = F[i]+Fa[i-1]
  }
}
Fap = round((Fa/n)*100,2)
Fp = round((F/n)*100,2)
df_9 = data.frame(Peso,F,Fa,Fp,Fap)

h_peso =hist(f,
            main = 'Distribuição de Peso',
            xlab = 'Peso',
            ylab = "Frequência",
            breaks = lim,
            right = FALSE,
            xaxt = 'n')

axis(side=1, at = lim)
axis(side=2, at = c(0,13))

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




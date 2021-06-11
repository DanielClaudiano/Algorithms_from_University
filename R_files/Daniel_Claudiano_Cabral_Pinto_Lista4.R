#rm(list = ls())
#Questão 1

#Criando a tabela
mes = c("1","2","3","4","5","Total")
Aux_1 = c(12,10,14,11,13,60)
Aux_2 = c(15,10,10,10,15,60)
Aux_3 = c(12,18,15,1,15,70)
Total = c(39,38,39,31,43,190)
df = data.frame(Aux_1,Aux_2,Aux_3,Total)

# i) 
#O auxiliar 3 obteve uma contagem de células na lâmina maior que os outros dois auxíliares.

#ii)
#Irei compara-los utilizando como críterio o coeficente de variancia, pois as amostras possuem tamnhos diferentes

#médias amostrais
media_1 = sum(Aux_1[1:5])/length(Aux_1[1:5]) # 12
media_2 = sum(Aux_2[1:5])/length(Aux_2[1:5]) # 12
media_3 = sum(Aux_3[1:5])/length(Aux_3[1:5]) # 12.2

#desvios padroes amostrais

dp_1 = sqrt(sum((Aux_1[1:5]-media_1)^2)/(length(Aux_1[1:5])-1)) # 1.581139
dp_2 = sqrt(sum((Aux_2[1:5]-media_2)^2)/(length(Aux_2[1:5])-1)) # 2.738613
dp_3 = sqrt(sum((Aux_3[1:5]-media_3)^2)/(length(Aux_3[1:5])-1)) # 6.610598

#Coeficentes de Variação

cv_1 = 100*dp_1/media_1 # 13.17616 %
cv_2 = 100*dp_2/media_2 # 22.82177 %
cv_3 = 100*dp_3/media_3 # 54.18523 %
                            
# Segundo a disperção utilizando o críterio o coeficiente de variância,
#o Auxiliar 1 possui o conjunto mais homogêneo de medições. 

#iii)
# A repetibilidade do auxiliar 1 é aceitavel, enquanto aos outros auxiliares a repetibilidade não foi detectada.

#rm(list = ls())

#2)

X = c(32,34,35,40,40,42,44,46,48,50,52)

#i) utilizarei como críterio para valores discrepantes a cerca interna de um boxplot

X_sorted = sort(X)
n = length(X)
quartis = c()
xs = c()
ks= c()
for (i in 1:3) {
  x = i*(n+1)/4
  xs[i] = x
  k = trunc(x)
  ks[i] = k
  dif = X[k+1]-X[k]
  quartis[i] = X[k]+(x-k)*dif
}

DEQ = quartis[3]-quartis[1]
CII = quartis[1]-DEQ*1.5
CIS = quartis[3]+DEQ*1.5

X<=CII # Não há valores abaixo da cerca interna inferior
X>=CIS # Não há valores abaixo da cerca interna superior

#Portanto não há valores discrepantes

#ii)
#Estabelecendo as classes
minimo = min(X) 
maximo = max(X)
amp = maximo - minimo
n = length(X)
k1 = round(sqrt(n),0)
tam = amp/k1
tam1 =tam
lim = seq(minimo,maximo,tam1)
lim
Quantidade_de_precipitacao_mm = c('[32.00-38.66)','[38.66-45.33)','[45.33-52]')

#Criando a tabela de frequencias
F = c(3,4,4)
Fa =c()
for (i in 1:3){
  if (i == 1){
    Fa[i] = F[i]
  }
  else {
    Fa[i] = F[i]+Fa[i-1]
  }
}
Fap = round((Fa/n)*100,2)

df_2 = data.frame(Quantidade_de_precipitacao_mm,F,Fap)

#iii)
#calculando os pontos médios das classes

pm = c()
for (i in 1:3){
  pm[i] = c((lim[i]+lim[i+1])/2)
  
} 

#calculando a média pelos pontos médios

numerador = rep(pm,F)
denominador = sum(F)
t_pm = length(pm)
media_tab = sum(numerador)/denominador# média 42.60606

##calculando a mediana por classes

mediana_tab = 38.66+(45.33-38.66)*(50-df_2$Fap[1])/df_2$Fap[2]# mediana 41.04229

#Média e mediana de X

media_original = sum(X)/n# media = 42
mediana_original = X_sorted[(n+1)/2]#mediana = 42.09091

#diferença entre as médias 
diferenca_media = media_tab - media_original# 0.5151515
diferenca_media

#diferença entre as medianas
diferenca_mediana = mediana_tab - mediana_original # -0.9577074
diferenca_mediana
# A diferença entre as médias foi de aproximadamente 0.52 mm, 
#enquanto das medianas foi de aproximandamente 0.96 mm.
#Observamos que não houve grandes variações entre a media e mediana por faixas
#e a media e mediana original.

#classificando a distribuição em termos de assimetria pelo coeficiente 
#de assimetria
coeficiente_de_assimetria = ((quartis[3]-quartis[2])-(quartis[2]-quartis[1]))/(quartis[3]-quartis[1]) 
coeficiente_de_assimetria # -0.07692308, portanto temos uma assimetria á esquerda

#gráfico de histograma

hist(X,
      main = 'Quantidade de Preceptação (mm)',
      xlab = 'Quantidade',
      ylab = "Frequência",
      breaks = lim,
      right = FALSE,
      xaxt = 'n')

axis(side=1, at = seq(minimo,maximo,tam1))

#3 
# fotos no anexo

#rm(list = ls())

#4
moda = c(0.69,2,4,8)
mediana = c(1.39,4,6,64)
media = c(1.95,8,48.4,124.5)
variancia = c(0.81,35,480,4900)


#Ln(xi)

indice 

simetria_0 = (media[1]-moda[1])/sqrt(variancia[1]) # 1.4

#xi-1

simetria_1 = (media[2]-moda[2])/sqrt(variancia[2]) # 1.01

#[(xi)²-1]/2
simetria_2 = (media[3]-moda[3])/sqrt(variancia[3]) # 2.027

#[(xi)³-1]/3
simetria_3 = (media[4]-moda[4])/sqrt(variancia[4]) # 1.66

# lambda = 1 foi levou a uma distribuição mais simétrica

#rm(list = ls())

#5

ferro = c(12,14,5,18,2,4,16,10,10,53)

#a
amplitude_total = max(ferro)-min(ferro)# amplitude total da anostra é 51 mg

variancia = sum((ferro-mean(ferro))^2)/(length(ferro)-1)# variancia da amostra é 211.16 mg²

desvio_padrao = sqrt(variancia)#desvio amostral da amostra é 14.53 mg

#b

cv = 100*desvio_padrao/(sum(ferro)/length(ferro)) # coeficiente de variancia da amostra é 101%

#rm(list = ls())
#6
altura = c(100,5,75) 
simetria_altura = (altura[1]-altura[3])/altura[2] # 5
cv_altura = 100*altura[2]/altura[1]# 5%

distancia = c(20,2,15)
simetria_distancia = (distancia[1]-distancia[3])/distancia[2] # 2.5
cv_distancia = 100*distancia[2]/distancia[1] # 10%

  
idade = c(15,10,15)
simetria_idade = (idade[1]-idade[3])/idade[2] # 0
cv_idade = 100*idade[2]/idade[1] # 67%
  
tempo = c(0.05,0.1,0.15)
simetria_tempo = (tempo[1]-tempo[3])/tempo[2] # -1
cv_tempo = 100*tempo[2]/tempo[1] # 200%

#ordem de disperção : idade,altura,distancia,tempo  
# constatamos que há presença de repetibilidade  
#apenas nas séries altura e distancia 

#ordem de simetria : tempo,idade,distancia,altura
# a série idade tem simetria simetria perfeita, enquanto tempo tem assimetria à esquerda,
# e altura e distancia tem assimetria à direita

#rm(list = ls())

#7
A= c(0.1,0.6,0.5,1.2,0.4,0.1)
B = c(0.2,1,1.1,1.4,0.3,0.6)
C = c(0.5,1.5,1.3,2.3,0.3,0.9)
D = c(0.6,1.3,0.9,2.9,0.6,1)
E = c(0.3,1,1.4,1.8,0.5,1.2)


#coeficiente de variancia de A
cva= 100*A[5]/A[2]#66.67%
cvb= 100*B[5]/B[2]#30%
cvc= 100*C[5]/C[2]#20%
cvd= 100*D[5]/D[2]#46.15%
cve= 100*E[5]/E[2]#50#
# podemos observar que a empresa C apresenta menor variação de investimentos
# enquanto a empresa A possui a maior variação.

#Gráfico de Barra de Erro
xbarra = c(A[2],B[2],C[2],D[2],E[2])
DP = c(A[5],B[5],C[5],D[5],E[5])
empresa = c(1.8,2.3,2.8,3.2,3.6)
plot(empresa,xbarra,ylim = c(0,2),xlim = c(0,6),xaxt='n',main="Gráfico de Barra de Erro",xlab="Empresas",ylab="Média +/- DP")
arrows(empresa,xbarra-DP,empresa,xbarra+DP, length = 0.05, angle = 90, code=3)
title("A    B    C   D   E", line = -14.5, outer = TRUE)

#rm(list = ls())

#8
A_idade = c(47.3, 3.2, 47.5, 51, 42, 55) 
A_tempo = c(75.5, 7.6, 77, 74, 71, 82)
B_idade = c(50.1, 3.7,56, 48, 48, 60 )
B_tempo = c(56.2, 9.1, 64.5, 65, 53, 72)

B_tempo[3]-B_tempo[1]

#verificando valores discrepantes
sA_idade = (A_idade[1]-A_idade[4])/A_idade[2]
sA_tempo = (A_tempo[1]-A_tempo[4])/A_tempo[2]
sB_idade = (B_idade[1]-B_idade[4])/B_idade[2]
sB_tempo = (B_tempo[1]-B_tempo[4])/B_tempo[2]
#Como a distancia entre a média e a mediana da Região A nas duas séries é pequena,
#não há sinal de valores discrepantes. 
#Já a Região B é outro cenário, visto que a distancia entre a média e mediana é consideravelmente maior na série  
#"Tempo de duração da camada de proteção (meses)", cerca de 8.3 meses de diferença, observamos também que a moda é bem próxima da mediana
# o que reforça mais a suspeita de valores discrepantes, já que se não houvesse, o desvio padrão não seria tão alto, 9,1 meses
#e a média não estaria tão descolada da mediana.
# Ainda na Região B com relação a série "Idade de aparecimento de fissuras (meses)" vemos que a moda é o valor mínimo
#a mediana é próxima do valor máximo, a amplitude total é 12, não parece ser uma série com valores discrepantes

cva_idade= 100*A_idade[2]/A_idade[1]#6.77%
cva_tempo= 100*A_tempo[2]/A_tempo[1]#10.07%
cvb_idade= 100*B_idade[2]/B_idade[1]#7.39%
cvb_tempo= 100*B_tempo[2]/B_tempo[1]#16.19%

# Observamos que a região B varia mais que a região A em ambas as séries, então a ordem crescente em termos de
#disperção em ambas as séries é : A, B

#olhando para os coeficientes de variação a idade de aparecimento de fissuras rejeito a hipótese que as regiões 
#sejam diferentes, pois a diferença em termos de disperção é minima, cerca de 0.62%,. Já o tempo de duração da camada de proteção
# possui uma diferença significativa, cerca de 6.12%, portando rejeito a hipótese de que não há diferença entre as regiões nesse caso. 
#  
cva_idade-cvb_idade#0.62%
cva_tempo-cvb_tempo#6.12%

#rm(list = ls())
#9

eq1 = c(10,10,9,7,9)
eq2 = c(11, 9, 8, 10, 12)

cv1 = 100*sd(eq1)/mean(eq1)#13.60 %
cv2 = 100*sd(eq2)/mean(eq2)#15.81 %

#i e ii
#Ambos possuem uma repetibilidade aceitavel, não é verificável se há diferenças na repetibilidade,
#tanto faz escolher um ou outro.

#grafico de barras de erro

xbarra = c(mean(eq1),mean(eq2))
DP = c(sd(eq1),sd(eq2))
empresa = c(1,2)
plot(empresa,xbarra,ylim = c(5,12),xlim = c(0,3),xaxt='n',main="Gráfico de Barra de Erro",xlab="Equipamentos",ylab="Média +/- DP")
arrows(empresa,xbarra-DP,empresa,xbarra+DP, length = 0.05, angle = 90, code=3)
title("Equipamento 1  Equipamento 2", line = -14.5, outer = TRUE)

#observamos que não podemos afirmar que os dois equipamentos são diferentes, e portanto tanto faz comprar um ou outro.

#rm(list = ls())

#10 
#Duas equipes de futebol, A e B, tem os saldos de gols por partida abaixo:

A = c(1,2,2,3,3,3,4,4,4,4,5,5,5,5,5,6,6,6,6,7,7,7,8,8,9)

B = c(1,1,1,1,1,1,1,1,1,2,2,2,2,2,3,3,3,4,4,5,6,7,7,7,8)

#i) Quem possui o saldo de gols mais homogenio?
#ii)Quem possui a distribuição mais simétrica?

#coeficiente de variancia
cvA = 100*sd(A)/mean(A) #40%
cvB = 100*sd(B)/mean(B) # 76.25%

#observamos que a equipe A tem um saldo de gols mais homogenio que B

simetria_A = mean(A)-5/sd(A)# 2.55
simetria_B = mean(B)-1/sd(B)# 2.60

#Observamos que ambos são assimétricos para a direita, sendo a equipe A mais próxima ao centro.

  



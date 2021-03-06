#rm(list = ls())
#Quest�o 1

##a

v = c(12,14,5,18,2,4,16,10,10,53)
#Para achar a m�dia � necess�rio dividir a soma dos fatores pelo
#n�mero de fatores

media = sum(v)/length(v)# media = 14.4

##b
#Para achar o valor central de uma distribui��o de tamanho par,
#ou seja a mediana, � necess�rio fazer a m�dia dos seus valores centrais
v_sorted = sort(v)
central_v = sum(v_sorted[5:6])/2# mediana = 11

##c
#obtendo o valor mais frequente da distribui��o
#Trasformando a tabela de frequencias em um dataframe para melhor manipula��o
df = data.frame(table(v))

#Transformando os valores do vetor v em n�meros
df$v = as.character(df$v)
df$v = as.numeric(df$v)

#obtendo o valor na s�rie v que corresponda ao maior valor da s�rie de frequencias
moda = df$v[df$Freq==max(df$Freq)] 

#Resposta : a idade mais frequente da distribui��o, portanto a moda, � 10 anos.
#Classificamos a s�rie como unimodal

##d 
#Minha proposta � retirar o valor discrepante que est� empurrando a m�dia para cima, a pessoa de 53 anos,
#pois observamos que o resto da s�rie estamos lidando com pessoas de at� 18 anos, sendo esse o perfil
#de pacientes a ser estudado. Caberia um estudo de caso para entender o que o paciente de 53 est� com condi��es
#similares aos menores de idade.

media1 = sum(v_sorted)/length(v_sorted)
media_prop = sum(v_sorted[1:9])/length(v_sorted[1:9]) #observamos que aproxima da mediana, diferindo apenas uma unidade.

#rm(list = ls())

#2

camundongos =  c(13,20,21,21,22,24,25,25,26,27,28,30,30,30,31,31,31,33)

#i

n = length(camundongos)
camundongos_sorted = sort(camundongos)
camundongos_mediana = camundongos_sorted[((n+1)/2)] # mediana = 26  

#ii

aritmetica = sum(camundongos)/n # aritmetica = 26
geometrica = prod(camundongos)^(1/n) # geometrica = 25.427
harmonica = 1/(sum(camundongos)/n) # harmonica = 0.03846

ordem = sort(c(aritmetica,geometrica,harmonica))#ordenamento crecente dos valores acima

#iii
#Utilizarei como crit�rio para determinar os valores discrepantes
#valores que estejam fora das Cercas Internas do Box Plot


#calculando os quartis

quartis = c()
xs = c()
ks= c()
for (i in 1:3) {
  x = i*(n+1)/4
  xs[i] = x
  k = trunc(x)
  ks[i] = k
  dif = camundongos_sorted[k+1]-camundongos_sorted[k]
  quartis[i] = camundongos_sorted[k]+(x-k)*dif
}

DEQ = quartis[3]-quartis[1]
CII = quartis[1]-DEQ*1.5
CIS = quartis[3]+DEQ*1.5
camundongos_sorted

#Como as cercas internas apresentam valores 9 e 43, todos os valores est�o dentro das cercas,
#portanto n�o h� valores discrepantes, segundo o m�todo que escolhi.

#rm(list = ls())

#3 

chutes = c(0,0,0,1,0,1,3,5,10,8,7,6,7,5,4,3,2,1,0,7,10,9,20,1,1,2,2,4,4,6,6,8,5,5,2,1,8,7,7,9,3,10,9,1,4,4,4,4,8,4,3,3,1)

n = length(chutes)
chutes_sorted = sort(chutes)
chutes_mediana = chutes_sorted[(n/2)+0.5]  

#calculando os quartis

quartis = c()
xs = c()
ks= c()
for (i in 1:3) {
  x = i*(n+1)/4
  xs[i] = x
  k = trunc(x)
  ks[i] = k
  dif = chutes_sorted[k+1]-chutes_sorted[k]
  quartis[i] = chutes_sorted[k]+(x-k)*dif
}

DEQ = quartis[3]-quartis[1]
CII = quartis[1]-DEQ*1.5
CIS = quartis[3]+DEQ*1.5

#a 
## Todos os valores que est�o acima de 15.5 s�o discrepantes,no caso apenas um jogador com
#20 chutes ao gol


#b
aritmetica = sum(chutes)/n
geometrica = prod(chutes)^(1/n)
##Sim, pois adicionando zeros no produt�rio o resultado sempre ser� zero.

#c 
#obtendo o valor mais frequente da distribui��o
#Trasformando a tabela de frequencias em um dataframe para melhor manipula��o
df = data.frame(table(chutes))

#Transformando os valores do vetor v em n�meros
df$chutes = as.character(df$chutes)
df$chutes = as.numeric(df$chutes)

#obtendo o valor na s�rie v que corresponda ao maior valor da s�rie de frequencias
moda = df$chutes[df$Freq==max(df$Freq)]

## Temos uma s�rie bimodal com modas 1 e 4

#calculando os decis

decis = c()
xs = c()
ks= c()
for (i in 1:9) {
  x = i*(n+1)/10
  xs[i] = x
  k = trunc(x)
  ks[i] = k
  dif = chutes_sorted[k+1]-chutes_sorted[k]
  decis[i] = chutes_sorted[k]+(x-k)*dif
}

# Irei comentar sobre os quartis e alguns decis calculados:
# 1� quartil : nosso primeiro quartil nos diz que 25% da amostra possui valores de zero at� 1.5
# 2� quartil : nosso segundo quartil, ou mediana, nos diz que 50% da amostra possui valores de zero at� 4
# 3� quartil : nosso terceiro quartil nos diz que 75% da amostra possui valores de zero at� 7
# 3� decil   : nosso terceiro decil nos diz que 30% da amostra possui valores de zero at� 2
# 6� decil   : nosso sexto decil nos diz que 60% da amostra possui valores de zero at� 5
# 9� decil   : nosso sexto decil nos diz que 90% da amostra possui valores de zero at� 9

#rm(list = ls())

#4

kg = c(43,56,56,57,58,58,58,59,60,60,61,61,62,62,62,63,64,64,65,65,65,66,67,67,67,67,67,68,68,69,69,70,70,70,71,71,71,71,72,72,73,73,75,76,89)
n = length(kg)

#a
kg_sorted = sort(kg)

df = data.frame(table(kg))

df$kg = as.character(df$kg)
df$kg = as.numeric(df$kg)

moda = df$kg[df$Freq==max(df$Freq)]
# o valor mais frequente, moda, � 67

#b
kg_mediana = kg_sorted[(n/2)+0.5] 
# o valor central da sequ�ncia, mediana, � 67

#c
decis = c()
xs = c()
ks= c()
for (i in 1:9) {
  x = i*(n+1)/10
  xs[i] = x
  k = trunc(x)
  ks[i] = k
  dif = kg_sorted[k+1]-kg_sorted[k]
  decis[i] = kg[k]+(x-k)*dif
}

decis[3]
decis[7]

# 63 kg, portanto 30% dos dados est�o entre 43 � 62 quilos

# 70 kg, portanto 70% dos dados est�o entre 43 � 70 quilos

#rm(list = ls())

#5

n =100
q1 = 24.75
q2 = 34
q3 = 46.75
media = 40.4
x_3 = c(1,5,10)
x_98 = c(140,180,240)
moda = 32
somax = 4040
somax_10 = 104
somax_91 = 1157

#a 
# Defino as cercas internas do boxplot como medida para valores discrepantes
#valores que n�o est�o entre as cercas ser�o considerados discrepantes

DEQ = q3-q1
CII = q1-DEQ*1.5
CIS = q3+DEQ*1.5

#Observamos que valores acima de 79.75 minutos s�o discrepantes,
#portanto h� valores discrepantes na s�rie.

#b
#Na imagem anexada junto a este arquivo
#As conclus�es s�o as mesmas, pois utilizei as
#cercas internas do boxplot como crit�rio para determinar descrepantes.


#c
proporcao = somax_91/somax
proporcao

#c

#Objetivando minimizar os efeitos de valores discrepantes com apenas com os dados expostos,
# adotaria diminuir a diferen�a entre a m�dia com a mediana, respectivamente 40.4 e 34 ,adotando 
#a m�dia aritm�tica truncada para retirar o efeito de valores discrepantes. Visto que os �ltimos
#3 �ltimos valores quase comp�e 30% do somat�rio, retiraria 6 % dos dados para a nossa m�dia truncada
#, os 3 primeiros e os 3 ultimos valores. 

#rm(list = ls())

#6
#Base de dados na planilha csv anexada

df = read.csv2(file="C:/Users/Daniel/Downloads/Lista2_7_pagina2.csv",header = TRUE,  sep=",")

filhos = df$F[1:7]
n = length(filhos)

#a1
media = sum(filhos)/n# m�dia = 11,43

#a2
filhos_sorted = sort(filhos)
mediana = filhos_sorted[(n+1)/2]#mediana = 12
filhos_sorted 
#a3
sum(filhos)# somatorio = 80

#a4
# A resposta � zero, pois os desvios se anulam no somat�rio,
#portanto a soma dos desvios ser� sempre 0.

#a5

desvios = c()
for (i in 1:7){
  desvios[i] = 3*filhos[i]-5
}
soma_desvios = sum(desvios)# resposta : 205

#rm(list = ls())

#7 
tx = c(1.01)
for (i in 1:5){
  tx[i+1] = tx[i]*tx[1.01]
}

rendimento_m�dio = ((prod(tx)^(1/6))-1)*100# redimento m�dio de 3.54%
final =  2500*(1.01)^6# Montante acumulado de R$2653.80

#rm(list = ls())

#8
# arquivo no anexo
df = read.csv2(file="C:/Users/Daniel/Downloads/Lista2_7_pagina4.csv",header = TRUE,  sep=",")

n = length(df$valor)
valor_sorted = sort(df$valor)

#criando os quartis

quartis = c()
xs = c()
ks= c()
for (i in 1:3) {
  x = i*(n+1)/4
  xs[i] = x
  k = trunc(x)
  ks[i] = k
  dif = valor_sorted[k+1]-valor_sorted[k]
  quartis[i] = valor_sorted[k]+(x-k)*dif
}
# primeiro quatil � 17

#criando os decis
decis = c()
xs = c()
ks= c()
for (i in 1:9) {
  x = i*(n+1)/10
  xs[i] = x
  k = trunc(x)
  ks[i] = k
  dif = valor_sorted[k+1]-valor_sorted[k]
  decis[i] = valor_sorted[k]+(x-k)*dif
}
decis[3]# terceiro decil � 18.4

#Criando percentis

decis = c()
xs = c()
ks= c()
for (i in 1:9) {
  x = i*(n+1)/10
  xs[i] = x
  k = trunc(x)
  ks[i] = k
  dif = valor_sorted[k+1]-valor_sorted[k]
  decis[i] = valor_sorted[k]+(x-k)*dif
}


#rm(list = ls())

#Como o tamanho da amostra � menor do que 200, n�o � possivel fazer os percentis.

#9
# Falso, basta fazer a media aritimetica de 1, 2 e 3, 3 , e comparar com a m�dia geometrica, 1.814
# Falso, pois tamanho amostral par n�o tem valores centrais observados, vai ser a m�dia dos dois valores mais centrais
# Falso, m�dia e mediana.
# Verdadeiro.
#Falso, a m�dia que � o ponto de equil�brio
#Falso, pois a moda pode representar uma porcentagem �nfima do total.
#Verdadeiro
#Falso, essa � a descri��o do 1 quartil

#rm(list = ls())

#10


# Criando a tabela de dados agregados
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

df = data.frame(Quantidade_Calcio_mg,F,Fa,Fap)


#calculando mediana

denominador = (df$F[3]/sum(df$F))*100
mediana = 9.88+(10.82-9.88)*(50-df$Fap[2])/denominador# mediana 10.07 mg

#calculando m�dia
p_medios = c((8.00+8.94),(8.94+9.88),(9.88+10.82),(10.82+11.76) ,(11.76+12.70))/2
p_medios
n = length(p_medios)
elementos = c()
for (i in 1:n){
  elementos[i]=df$F[i]*p_medios[i]
} 
media = sum(elementos)/sum(df$F)# m�dia 9.9975

#Calculando moda 

part1 = (df$F[2]/(df$F[2]+df$F[4]))*9.88
part2 = (df$F[4]/(df$F[2]+df$F[4]))*10.82
moda = part1+part2 # moda � 10.19
#rm(list = ls())
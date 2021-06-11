#rm(list = ls())
#caregando o arquivo
df = read.csv2(file="C:/Users/Daniel/Downloads/Trabalho_Daniel_Claudiano_Cabral_Pinto_Tabela_codificada.csv",header = TRUE,  sep=",")

#Obtendo series quantitativas do dataframe

idade = df$IDADE
comodo = df$COMODO

#Descobrindo a variavel quantitativa com maior variabilidade
#Coeficiente de variacao
cv_idade = 100*sd(idade)/mean(idade)# 19.31%
cv_comodo = 100*sd(comodo)/mean(comodo)# 35.19%

# a serie com maior grau de dispersao em relacao sua media é comodo. 

#Medidas de posicao
#media
media = sum(comodo)/length(comodo)#  6.08

#mediana
n = length(comodo) # 25
comodo_sorted = sort(comodo)
mediana = comodo_sorted[(n+1)/2] #6

media/mediana
#observamos que a media e mediana possuem uma diferença de aproximadamente 1%,
#entao, aparentemente nao ha valores discrepantes se considerarmos a distancia dessas
#duas medidas como criterio para identificacao de ocorrencia de valores discrepantes.

#moda
df_2 = data.frame(table(comodo))
#Transformando os valores do vetor idade em numeros
df_2$comodo = as.character(df_2$comodo)
df_2$comodo = as.numeric(df_2$comodo)
#obtendo o valor na sÃ©rie idade que corresponda ao maior valor da sÃ©rie de frequencias
moda = df_2$comodo[df_2$Freq==max(df_2$Freq)] # 5

#O numero de comodos mais frequente na residencia dos alunos é 5.
#quartis

#calculando os quartis
quartis = c()
xs = c()
ks= c()
for (i in 1:3) {
  x = i*(n+1)/4
  xs[i] = x
  k = trunc(x)
  ks[i] = k
  dif = comodo_sorted[k+1]-comodo_sorted[k]
  quartis[i] = comodo_sorted[k]+(x-k)*dif
}
# quartis : 1° = 5 , 2° =6 , 3° =7 

#decis
decis = c()
xs = c()
ks= c()
for (i in 1:9) {
  x = i*(n+1)/10
  xs[i] = x
  k = trunc(x)
  ks[i] = k
  dif = comodo_sorted[k+1]-comodo_sorted[k]
  decis[i] = comodo_sorted[k]+(x-k)*dif
}

#decis : 1° = 3.2, 2° = 5.0 , 3° = 5.0 , 4° =  5.4  , 5° = 6.0, 6° = 6.6 ,
# 7° = 7.0 , 8° = 7.0, 9° = 8.0

#Dados os quartis e decis, observamos que 20% de nossa amostra tem até 5 
#comodos em casa, enquanto metade da amostra tem até 6 comodos e 90% tem até
# 8 comodos em sua residencia.


#Medidas de dispercao
#amplitude total
amplitude_total = max(comodo)-min(comodo) #11

#DEQ
DEQ = quartis[3]-quartis[1] # 2

#variancia amostral
variancia = sum((comodo-media)^2)/(length(comodo)-1)#4.58

#desvio padrao amostral
dp = sqrt(variancia)#2.14
#Coeficiente de variacao amostral

cv_comodo = 100*dp/media# 35.19%

# Como podemos observar, o desvio padrao de nossa variavel corresponde a 35.19%
# do valor da media, dessa forma vemos que nao ha a presenca de uma repetibilidade 
# nos valores da amostra. A maior distancia entre valores na distribuicao é 11.
#A distancia interquartil nos mostra que os valores entre o primeiro quarto
# da amostra e o terceiro, podem variar até 2 unidades.


#Medidas de Assimetria
#coeficiente de assimetria
coeficiente_de_assimetria = ((quartis[3]-quartis[2])-(quartis[2]-quartis[1]))/(quartis[3]-quartis[1]) # 0

#indice de assimetria 
simetria = (media-moda)/sqrt(variancia)# 0.5048346

#As duas medidas de assimetria possuem resultados conflitantes: 
#enquanto o coeficiente de assimetria nos diz que a distribuicao é simetria
#o indice de assimetria nos diz que é assimetrica a direita. Por isso, olhar
# o histograma da serie sera necessario para avaliar a simetria.


#Identificacao de discrepantes
#caixa interna box plot

CII = quartis[1]-DEQ*1.5 #2
CIS = quartis[3]+DEQ*1.5 # 10
#Observamos que ha¡ apenas um valor discrepante, contradizendo a nossa intuicao
#dada pela distancia entre media e mediana que calculamos anteriormente.
#O valor discrepante corresponde a um individuo que vive numa residencia de 13 comodos.


#Representacao grafica
#Box plot
boxplot(comodo, main = "Distribuição dos Cômodos", ylab="Cômodos na Residência", col ="blue")

#Observamos a representacao grafica do que ja avaliamos em termos de valores
# discrepantes e distribuicao dos dados, dentro da caixa observamos que a mediana
# divide aparentemente simetricamente a caixa, porem observado a figura como um todo
# vemos que o valor discrepante nos faz ter a impressao que os dados estao concentrados 
# na faixa de 2 até 8 comodos.



#Histograma

lim = seq(1,14)
hist(comodo,
     main = 'Quantidade de Cômodos na Residencia',
     xlab = 'Quantidade',
     ylab = "Frequencia",
     breaks = lim,
     right = FALSE,
     xaxt = 'n'
     )

axis(side=1, at = lim)

#Observando o histograma, vemos que apesar de nao haver uma simetria 
#propriamente dita nos dados, observamos que as faixas mais populosas 
#estao concentradas no centro da distribuicao. Podemos confirmar graficamente a 
# moda sendo 5, a mediana 6 e que ha um valor discrepante 13.
#rm(list = ls())
#Carregando dados
df = read.csv2(file="C:/Users/Daniel/Downloads/DATABASE_TRABALHO_ESTAT1.csv",header = TRUE,  sep=",")

#Criando a tabela de contigencia
require(dplyr)
#criando uma matriz através da função group by para contar as respostas de cada beneficiado ou não
#pelo auxílio emergencial
aux_comodos = df %>% group_by(COMODO)
#criando as colunas da matriz
aux_comodos %>% summarise(
  RECEBEU = sum(AUX_EMERG == 1),
  NAO_RECEBEU = sum(AUX_EMERG == 0)
)
#transformando a matriz em dataframe
df_aux_comodos = data.frame(aux_comodos %>% summarise(
  RECEBEU = sum(AUX_EMERG == 1),
  NAO_RECEBEU = sum(AUX_EMERG == 0)
)
)

# Criando as series de frequencia de comodos por beneficiarios e nao beneficiarios
yes = rep(df_aux_comodos$COMODO,df_aux_comodos$RECEBEU)
no = rep(df_aux_comodos$COMODO,df_aux_comodos$NAO_RECEBEU)


# Verificando valores discrepantes nos grupos
#calculando os quartis

n1= length(yes)
quartis_yes = c()
xs = c()
ks= c()
for (i in 1:3) {
  x = i*(n1+1)/4
  xs[i] = x
  k = trunc(x)
  ks[i] = k
  dif = yes[k+1]-yes[k]
  quartis_yes[i] = yes[k]+(x-k)*dif
}

DEQ_yes = quartis_yes[3]-quartis_yes[1] #2
CII_yes = quartis_yes[1]-DEQ_yes*1.5 #2
CIS_yes = quartis_yes[3]+DEQ_yes*1.5 # 10

#Observamos que todos os valores dos beneficiarios estão dentro das cercas internas
#portanto nao ha valores discrepantes

n2 = length(no)
quartis_no = c()
xs = c()
ks= c()
for (i in 1:3) {
  x = i*(n2+1)/4
  xs[i] = x
  k = trunc(x)
  ks[i] = k
  dif = no[k+1]-no[k]
  quartis_no[i] = no[k]+(x-k)*dif
}

DEQ_no = quartis_no[3]-quartis_no[1]
CII_no = quartis_no[1]-DEQ_no*1.5 
CIS_no = quartis_no[3]+DEQ_no*1.5 


#Observamos que uma observação dos valores dos não beneficiarios 
#ultrapassa a cerca interna superior, portanto ha valores discrepantes

#GráficoS comparativoS entre quantidade de cômodos na residência e se receubeu ou
#nao o auxilio emergencial

#Histogramas
par(mfrow = c(1,2))
lim = seq(1,14)
hist_yes = hist(yes,
     main = 'Beneficiários',
     xlab = 'Cômodos',
     ylab = "Frequencia",
     breaks = lim,
     right = FALSE,
     xaxt = 'n'
)

axis(side=1, at = lim)


hist_no = hist(no,
     main = ' Não Beneficiários',
     xlab = 'Cômodos',
     ylab = "Frequencia",
     breaks = lim,
     right = FALSE,
     xaxt = 'n'
)

axis(side=1, at = lim)

# Observamos que a frequencia dos comodos dos beneficiarios concentra-se
# entre 4 a 8 comodos, sendo 7 comodos a moda. Pode-se observar que 
# a disperção dos beneficiarios é menor do que dos não beneficiarios,
# tanto que estes últimos possuem valores discrepantes e no limite das caixas
# internas. Para estes, a moda são 5 comodos e ha maior concentraçao
# de valores entre 5 a 7 comodos.

#box plot

par(mfrow = c(1,2))

box_yes = boxplot(yes, main = "Beneficiários", ylab="Cômodos na Residência", col ="blue")

box_no =boxplot(no, main = "Não Beneficiários", ylab="Cômodos na Residência", col ="blue")

#Como podemos observar as medianas dos dois grupos aparentemente divide
# a caixa em pedços iguais, o que pode nos indicar que a mediana está proxima da media.
# observamos que as caixas são bem similares, tendo os mesmos quartis e 
# cercas internas iguais. Porem, o grupo dos nao beneficiarios possui um
# valor discrepante.

#barras de erro
xbarra = c(mean(yes),mean(no)) #5.909091 6.214286
DP = c(sd(yes),sd(no)) #1.814086 2.423557
cv = c(DP/xbarra)*100 #30.69992 38.99976
30.69992-38.99976
#Observamos que a diferemça entre os seus coeficientes de variancia
# e cerca de 8.30% e confirmando o que ja suspeitavamos : os nao benediciarios 
#possuem maior dispercao nos valores

empresa = c(2,4)

plot(empresa,xbarra,ylim = c(0,14),xlim = c(0,6),xaxt='n',main="Gráfico de Barra de Erro",xlab="Empresas",ylab="Média +/- DP")
arrows(empresa,xbarra-DP,empresa,xbarra+DP, length = 0.05, angle = 90, code=3)
title("Beneficiários /  Não Beneficiários", line = -14.5, outer = TRUE)
# Como podemos observar, aparentemente nao ha diferença entre os dois grupos
# na questao de quantidade de comodos na residencia.
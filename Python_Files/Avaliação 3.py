print('********************************************************************************************************************'                          
      '\n'
      '\n                                            Campeonato Mundial'
      '\n'
      '\n'
      '********************************************************************************************************************'
     '\n')

print('********************************************************************************************************************'     
      '\n'
      '\n                                Insira apenas os dados da partida :'
      '\n'
      '\n                                Exemplo : Mandante : Brasil' 
      '\n                                          Visitante : Argentina'
      '\n                                          Faltas do Mandante: 8'
      '\n                                          Faltas do Visitante: 10'
      '\n                                          Gols do Mandante: 2'                                           
      '\n                                          Gols do Visitante: 3'
      '\n'                                              
      '\n'
     '*********************************************************************************************************************')
continuar = True # chave para continuar o looping a importar dados do usuário
dados = [] # matriz a ser preenchida com as informações das partidas
while continuar: # looping para solicitar dados ao usuário
    mandante  = input('Mandante:  ')
    visitante = input('Visitante:  ')
    falta_mandante = int(input('Faltas do Mandante:  '))
    falta_visitante = int(input('Faltas do Visitante:  '))
    gols_mandante = int(input('Gols do Mandante:  '))
    gols_visitante = int(input('Gols do Visitante:  '))
    dados_partidas = [mandante,visitante,[falta_mandante,falta_visitante],[gols_mandante,gols_visitante]]# vetor com as informações das partidas
    dados.append(dados_partidas) # matriz com todos os vetores de informações das partidas
    inserir = input('Deseja inserir mais dados? (s/n)  ') # perguntar se o usuário quer continuar a inserir dados
    if inserir == 'n': # se o usuário desejar parar de inserir dados então o looping é encerrado
        continuar = False
#print(dados)     

#Obter a matriz chamada "campeonato" que possui informaçoes sobre o desempenho individual de cada equipe 
# a matriz será composta por vetores da seguinte característica :[seleção, pontos, saldo de gols, gols pró, faltas feitas]
pais = [] # lista com os nomes das seleções
campeonato = [] # matriz de zeros a ser preenchida
#Obtendo as linhas da matriz campeonato
for i in range(len(dados)):
    if dados[i][0] not in pais:
        pais.append(dados[i][0])# se a seleção mandante não estiver em pais então insira na lista
    if dados[i][1] not in pais:
        pais.append(dados[i][1]) # se a seleção visitante não estiver em pais então insira na lista
#obtivemos uma lista de paises com valores únicos
#print(pais)    
linhas = len(pais)
colunas = 5 
#criando uma matriz nula para pordemos preenche-la
for i in range(linhas):
    campeonato.append([0]*colunas)
#print(campeonato)

w = 0 #contador da para o looping seguinte -> for j in pais
for j in pais: # cada vetor da matriz será formado por uma seleção diferente
#    print(j, 'valores de j')
    saldo_gols = 0 # variaveis a serem imputadas na matriz campeonato
    pts = 0
    faltas = 0
    gols_pro = 0
    for i in range(len(dados)):# obtendo os valores para cada vetor
#        print(i, 'valores de i')
        saldo = 0 
        if dados[i][0] == j: # calculando valores apenas para a seleção da vez se ela for o mandante
            saldo = dados[i][3][0] - dados[i][3][1] # definindo o saldo da partida
            if saldo > 0: # se o saldo for positivo para o mandante ele venceu
                pts += 3
                saldo_gols += saldo # acumulando o saldo de gols do mandante a cada partida
            elif saldo == 0: # saldo zero, empate
                pts +=1
            else:
                saldo_gols -= saldo # saldo do mandante acumulado diminuiu na derrota
            faltas+= dados[i][2][0] # faltas acumuladas feitas pelo mandante
            gols_pro+= dados[i][3][0] # gols acumulados feitos pelo mandante 
        elif dados[i][1] == j: # casso a seleção nessa partida seja o visitante
            saldo = dados[i][3][0] - dados[i][3][1] 
            if saldo < 0: # saldo negativo da partida significa que o visitante venceu
                pts += 3
                saldo_gols += saldo
            elif saldo == 0: 
                pts +=1
            else:
                saldo_gols -= saldo
            faltas+= dados[i][2][1]
            gols_pro+= dados[i][3][1]  
    #atribuindo valores nos vetores        
    campeonato[w][0] = j # seleção
    campeonato[w][1] = pts # pontos acumulados no campeonato
    campeonato[w][2] = saldo_gols # saldo de gols acumulado no campeonato
    campeonato[w][3] = gols_pro # gols feitos no campeonato
    campeonato[w][4] = faltas # faltas feitas no campeonato
#    print(campeonato[w][0],campeonato[w][1],campeonato[w][2],campeonato[w][3],campeonato[w][4])    
    w+=1 # contador de seleções, cada w é uma seleção diferente
#    print(campeonato[w][0],campeonato[w][1],campeonato[w][2],campeonato[w][3],campeonato[w][4])
#print(campeonato) # matriz desempenho individual das seleções
parar = True # chave para encerrar a consulta
while parar:
    desejo = int(input(
      '\n                               Digite o número que deseja'
      '\n'
      '\n                               1 - Obter o total de faltas do campeonato'
      '\n                               2 - Saber o time que fez mais faltas'
      '\n                               3 - Saber o time que fez menos faltas'
      '\n                               4 - Saber a pontuação de cada time'
      '\n                               5 - Tabela de Classificação do Campeonato Mundial'
      '\n'    ))
    #1 - Obter o total de faltas do campeonato'
    if desejo == 1:
        total_faltas = 0
        for i in range(len(campeonato)): # percorrer linhas
            total_faltas += campeonato[i][4] # somar as faltas de cada vetor
        print(total_faltas, 'faltas cometidas no campeonato.')
    
    #2 - Saber o time que fez mais faltas'
    if desejo == 2:
        mais_faltas = campeonato[0][4]
        pos = 0
        for i in range(1,len(campeonato)): # percorrer linhas
            if campeonato[i][4]>mais_faltas:
                mais_faltas = campeonato[i][4]
                pos = i
        print(campeonato[pos][0], 'é o time que fez mais faltas')
    
    #3 Saber o time que fez menos faltas
    if desejo == 3:
        menos_faltas = campeonato[0][4]
        pos = 0
        for i in range(1,len(campeonato)): # percorrer linhas
            if campeonato[i][4]<menos_faltas:
                menos_faltas = campeonato[i][4]
                pos = i
        print(campeonato[pos][0], 'é o time que fez menos faltas')
    
    #4 - Saber a pontuação de cada time'
    if desejo == 4:
        for i in range(len(campeonato)): # percorrer linhas
            print(campeonato[i][0], "-",campeonato[i][1], "pontos")
    
    #5 - Tabela de Classificação das Eliminatórias'
    if desejo == 5:
        for i in range(len(campeonato)):#selection sort para pontos, tendo saldo de gols e gols feitos como desempates respectivos
            elemento = campeonato[i] 
            while i>0 and (((elemento[1] > campeonato[i - 1][1]))or((elemento[1] == campeonato[i - 1][1])and(elemento[2] > campeonato[i - 1][2]))or((elemento[1] == campeonato[i - 1][1])and(elemento[2] == campeonato[i - 1][2]) and  (elemento[3] > campeonato[i - 1][3]))) : #Enauanto elemento selecionado é menor que o anterior e temos carta na mesa
                campeonato[i] = campeonato[i - 1]
                i -= 1 # assumindo posições anteriores a atual
            campeonato[i] = elemento # valor do elemento da paosiçao i caso o looping nao é executado
        print("**********************************************************************************************************************"
             "\n                                               Tabela Campeonato Mundial                                                     "
             "\n" 
             "\n                            |Seleção|       |Pontos|     |Saldo de Gols|     |Gols Pró|                               ")
        for i in range(len(campeonato)): # percorrer linhas
            print("                            |"+str(campeonato[i][0])+"             "+str(campeonato[i][1])+"              "+str(campeonato[i][2])+"                 "+str(campeonato[i][3])+"    |")
        print("\n"
              "\n************************************************************************************************************************")
    retorno = input("Retornar ao Menu? (s/n)")
    if retorno == "n":
        print("Consulta Encerrada !")
        parar = False
        


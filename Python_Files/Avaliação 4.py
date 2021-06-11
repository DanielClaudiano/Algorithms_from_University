# -*- coding: utf-8 -*-
"""
Created on Sat May  1 20:43:45 2021

@author: Daniel Claudiano Cabral Pinto
"""
print('#####################################################################################################################'                          
      '\n'
      '\n                                            Torneio Kart'
      '\n'
      '\n'
      '#####################################################################################################################'
     '\n')


dados = []# matriz a ser preenchida com as informações das corridas
numero_pilotos = int(input("Número de Pilotos:  "))
for i in range(numero_pilotos):  # looping para solicitar dados ao usuário
    piloto  = input('Piloto:  ')
    tempo= list(map(int,(input('Tempo:  ').split(","))))  # obter o tempo em elementos inteiros a partir de uma lista de caracteres
    tupla = (piloto,tempo)
    dados.append(tupla)
corrida = dict(dados) # dicionario
print(corrida)

#corrida = {"Gabriel" : [120,110,130,80,210],
#           "Giorgian" : [100,63,90,80,70], 
#          "Bruno" : [170,102,60,111,120], 
#           "Gerson" : [130,114,135,88,218],
#           "Everton" : [180,170,130,65,200]}

parar = True # chave para encerrar a consulta
while parar:
    desejo = int(input('\n##################################################################################################'
      '\n                               Digite o número que deseja'
      '\n##################################################################################################'
      '\n                               1 - Obter a volta mais rapida'
      '\n                               2 - Classificacao do Torneio'
      '\n' 
      '\n'    ))

    if desejo == 1: # descobrindo qual volta foi mais rápida e quem a fez
        recorde = []
        for nome in corrida:
                recorde.append(min(corrida[nome]))
        rapida = min(recorde) # descobrindo o menor tempo de uma volta                                                                                                                              a fizeram
        for nome in corrida : 
            if rapida in corrida[nome]:
                piloto = nome #descobrindo qual piloto fez a volta mais rapida
        q = corrida[piloto].index(rapida) + 1 # descobrindo qual volta foi a mais rápida
        print(piloto,"fez a volta mais rápida da corrida,",rapida,'segundos na ',q,"ª volta")

    if desejo == 2:
        print('\n###################################################################################################'
              '\n                               Classificação Torneio Kart                                  '
              '\n###################################################################################################'
              '\n')
        medias = [] # vetor das medias
        nomes = [] # vetor do nome dos pilotos
        for nome in corrida: # inputando as listas com os pilotos e suas medias respectivas
            media = sum(corrida[nome])/len(corrida[nome])
            nomes.append(nome)
            medias.append(media)
        print(nomes, medias     )    
        for i in range(1,6): # selection sort 
            menor = min(medias)    
            k = medias.index(menor)# descobrindo a posição da menor média para associar ao piloto
            classificado = nomes[k]# piloto da média respectiva    
            print('                               ',i,"º",classificado)
            medias.pop(k) # eliminando a menor media
            nomes.pop(k)  # eleminando o piloto respectivo 
            
    retorno = input("Retornar ao Menu? (s/n)") 
    if retorno == "n":
        print("Consulta Encerrada !")
        parar = False
        



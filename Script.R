library(tidyverse)
library(readxl)

#Função para normalização da matriz de julgamentos

normaliza = function(lista){
  lista_aux = list()
  for( i in 1:length(lista)){
    aux = lista[[i]] %>%  apply(2,sum)
    lista[[i]] = t(apply(lista[[i]], 1, function(x)x/aux))
    lista_aux[[i]] = apply(lista[[i]],1, sum)/length(lista[[i]][1,])
  }
  names(lista_aux) = names(lista)
  return(lista_aux)
}

#Bunitação 

transforma = function( lista ){
  item = rep(LETTERS[1:length(lista[[1]])],each = length(lista)-1 )
  criterio = rep(names(lista)[-length(names(lista))],length(unique(item)))
  prob = NULL
  for( i in 1:length(lista[[1]])){
    prob = c(prob,unlist(lapply(lista[-length(lista)], function(x)x[i])))
  }
  peso = rep(lista[[length(lista)]], length(unique(item)))
  dados = tibble(item,criterio,prob,peso)
  return(dados)
}

xablau = function(dados) {
  dados %>% 
    mutate(valor = prob*peso) %>% 
    group_by(item) %>% 
    summarise(xablau = sum(valor)) %>% 
    return()
}


# Vai concatenar todas as funçoes
junta = function(lista){
  lista_aux = normaliza(lista)
  lista_aux2 = transforma(lista_aux)
  lista_final = xablau(lista_aux2)
  return(lista_final)
}




### VER SE ESTÀ FUNCIONANDO!!!
AAA = function(lista){
  tabela = c(0,0,0.58,0.9,1.12,1.24,1.32,1.41,1.45)
  Autoval = list()
  print(lista)
  for(i in 1:length(lista)){
    Autoval[[i]] = c(Re(eigen(lista[[i]])$values)[1],(abs(Re(eigen(lista[[i]])$values)[1]-length(lista[[i]][1,]))/(length(lista[[i]][1,])-1))/tabela[length(lista[[i]][1,])])
  }
  names(Autoval) = rep("Autovalor/Índice de consistência",length(Autoval))
  return(Autoval)
}


#Tenho que nomear as matrizes
#Precisa definir o número de alternativas (m=3)
#Precisa definir o número de critérios (k=5)
#Vai ler k matrizes (MACi, i=1, ..., k) mxm que comparam as alternativas para cada critério
#Vai ler uma matriz (MCC) kxk que compara os critérios
"CF = matrix(c(1,1/3,1/6,3,1,1/2,6,2,1),nrow = 3,byrow = T) #nome MAC1
A = matrix(c(1,1/2,1/2,2,1,2,2,1/2,1),nrow = 3,byrow = T) #nome MAC2
PS = matrix(c(1,1,2,1,1,1,1/2,1,1),nrow = 3,byrow = T) #nome MAC3
RV = matrix(c(1,2,3,1/2,1,2,1/3,1/2,1),nrow = 3,byrow = T) #nome MAC4
M = matrix(c(1,5,3,1/5,1,1/3,1/3,3,1),nrow = 3,byrow = T) #nome MAC5
FP = matrix(c(1,1/5,3,1/5,1/3,5,1,5,3,3,1/3,1/5,1,1/3,1/3,5,1/3,3,1,1,3,1/3,3,1,1),nrow = 5,byrow = T) #nome MCC
exemplo = list(CF=CF,A=A,PS=PS,RV=RV,M=M,FP=FP)
"

# Ler diretamente da planilha do excel. Retorna uma lista de tibbles. 
# Lembrar que na arrumação das planilhas do excel devemos ter : 
# MAC1, .... , MACn , MCC 
# A primeira coluna da mcc tem que ser a primeira planilha........

Ler = function(caminho){
  exemplo = lapply(excel_sheets(caminho), read_excel, path = caminho, col_names = F)
  names(exemplo) = excel_sheets(caminho)
  return(exemplo)
}

exemplo = Ler("F://AHP//AHP//Documentação//BD_teste.xlsx")



#Criação das matrizes

teste = normaliza(exemplo) %>% 
  transforma()

dados=junta(exemplo)
AAA(exemplo)

teste = normaliza(exemplo)

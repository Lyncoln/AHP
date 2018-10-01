library(tidyverse)

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
dados=junta(exemplo)



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
AAA(exemplo)

x = list()





#Tenho que nomear as matrizes
CF = matrix(c(1,1/3,1/6,3,1,1/2,6,2,1),nrow = 3,byrow = T)
A = matrix(c(1,1/2,1/2,2,1,2,2,1/2,1),nrow = 3,byrow = T)
PS = matrix(c(1,1,2,1,1,1,1/2,1,1),nrow = 3,byrow = T)
RV = matrix(c(1,2,3,1/2,1,2,1/3,1/2,1),nrow = 3,byrow = T)
M = matrix(c(1,5,3,1/5,1,1/3,1/3,3,1),nrow = 3,byrow = T)
FP = matrix(c(1,1/5,3,1/5,1/3,5,1,5,3,3,1/3,1/5,1,1/3,1/3,5,1/3,3,1,1,3,1/3,3,1,1),nrow = 5,byrow = T)

#Criação das matrizes
exemplo = list(CF=CF,A=A,PS=PS,RV=RV,M=M,FP=FP)
teste = normaliza(exemplo) %>% 
  transforma() %>% 
  xablau()



teste = normaliza(exemplo)

op_carros = rep(LETTERS[1:length(teste[[1]])],each = length(teste)-1 )
criterio = rep(names(teste)[-length(names(teste))],length(unique(op_carros)))
prob = NULL
for( i in 1:length(teste[[1]])){
  prob = c(prob,unlist(lapply(teste[-length(teste)], function(x)x[i])))
}
peso = rep(teste[[length(teste)]], length(unique(op_carros)))
dados = tibble(op_carros,criterio,prob,peso)
dados %>% 
  mutate(valor = prob*peso) %>% 
  group_by(op_carros) %>% 
  summarise(xablau = sum(valor)) 


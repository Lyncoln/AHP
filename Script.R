library(tidyverse)

AHP = function(lista){
  lista_aux = list()
  for( i in 1:length(lista)){
    aux = lista[[i]] %>%  apply(2,sum)
    lista[[i]] = t(apply(lista[[i]], 1, function(x)x/aux))
    lista_aux[[i]] = apply(lista[[i]],1, sum)/length(lista[[i]][1,])
  }
  return(lista)
}

CF = matrix(c(1,1/3,1/6,3,1,1/2,6,2,1),nrow = 3,byrow = T)
A = matrix(c(1,1/2,1/2,2,1,2,2,1/2,1),nrow = 3,byrow = T)
PS = matrix(c(1,1,2,1,1,1,1/2,1,1),nrow = 3,byrow = T)
RV = matrix(c(1,2,3,1/2,1,2,1/3,1/2,1),nrow = 3,byrow = T)
M = matrix(c(1,5,3,1/5,1,1/3,1/3,3,1),nrow = 3,byrow = T)
FP = matrix(c(1,1/5,3,1/5,1/3,5,1,5,3,3,1/3,1/5,1,1/3,1/3,5,1/3,3,1,1,3,1/3,3,1,1),nrow = 5,byrow = T)

exemplo = list(CF,A,PS,RV,M,FP)

##as.tibble(lista[i]) %>%  mutate( soma = apply(1,sum))
apply(lista[[1]],1, sum/length(lista[[1]][1,]))
  
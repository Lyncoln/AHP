# Argumentos: lista = a lista que contem todos os quadros de julgamentos;
#mapeamento = vetor que informa cada criterio e seus subcritérios e por fim qtd de alternativas
tabela2_ahp = function(lista, mapeamento){
  normalizado = normaliza(lista)
  
}

####Rascunho
mapeamento = c(1,3,3,3,3,4)
tabela = Ler("F://GitHub//AHP//Documentação//BD_teste3.xlsx")
normalizado = normaliza(tabela)
normalizado
#Contagens importantes
numeroTotSubcriterios = sum(mapeamento[3:(length(mapeamento)-1)])
numeroTotSubcriterios
qtdSubniveis = contaSubNivel(mapeamento)
qtdSubniveis
#
#Pesos de criterios e subcriterios
pesosCriterios = normalizado[[1]]
pesosCriterios
pesosSubcriterios = list()
for(i in 1:qtdSubniveis){
  pesosSubcriterios[[i]] = normalizado[[i+1]]
}
pesosSubcriterios
#

#Conta quantos subniveis existem no mapeamento
contaSubNivel = function(mapeamento){
  mapeamento = mapeamento[3:(length(mapeamento)-1)]
  cont = 0
  for(i in 1:length(mapeamento)){
    if(mapeamento[i] != 0){
      cont = cont +1
    }
  }
  return(cont)
}

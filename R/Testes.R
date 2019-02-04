# 
# Argumentos: lista = a lista que contem todos os quadros de julgamentos;
#mapeamento = vetor que informa cada criterio e seus subcritérios e por fim qtd de alternativas
#Conta quantos subniveis existem no mapeamento
contaSubNivel = function(mapeamento){
  mapeamento = mapeamento[3:(length(mapeamento)-1)]
  cont = sum(mapeamento!=0)
  return(cont)
}
tabela2_ahp = function(caminho, mapeamento){

####Rascunho
#mapeamento = c(1,3,3,3,3,4)
tabela = Ler(caminho)
normalizado = normaliza(tabela)
normalizado
#Contagens importantes
  numeroTotSubcriterios = sum(mapeamento[3:(length(mapeamento)-1)])
  numeroTotSubcriterios
  qtdSubniveis = contaSubNivel(mapeamento)
  qtdSubniveis
#
#Pesos de criterios e subcriterios
  pesosCriterios = list()
  pesosCriterios[[1]] = normalizado[[1]]
  names(pesosCriterios) = names(normalizado[1])
  pesosCriterios
  #Estou criando uma lista com as prop dos subniveis e dando nome para cada vetor de prop
    pesosSubcriterios = list()
    nomesSubcriterios = c()
  for(i in 1:qtdSubniveis){
    pesosSubcriterios[[i]] = normalizado[[i+1]]
    nomesSubcriterios[i] = names(normalizado[i+1])
  }
  names(pesosSubcriterios) = nomesSubcriterios
  pesosSubcriterios
  #
#
#Pesos das matrizes de comparação envolvendo as alternativas
  pesosMAlternativas = list()
  cont = 1
  nomesMalternativas = c()
  for(i in (length(pesosCriterios)+length(pesosSubcriterios)+1):length(normalizado)){
    pesosMAlternativas[[cont]] = normalizado[[i]]
    nomesMalternativas[cont] = names(normalizado[i])
    cont = cont +1
  }
  names(pesosMAlternativas) = nomesMalternativas
  pesosMAlternativas
#

#Calculos de prop de subcriterios globais
  mapeamento
  qtdSubniveis
  pesosCriterios
  pesosSubcriterios
  criteriosComSubniveis = ifelse(mapeamento[3:(length(mapeamento)-1)]==0,0,1)
  criteriosComSubniveis
  for(i in 1:mapeamento[2]){
    if(criteriosComSubniveis[i]==1){
      pesosSubcriterios[[i]] = pesosSubcriterios[[i]]*pesosCriterios[[1]][[i]]
    }
  }
  pesosSubcriterios

#****Calculando vetor de probabilidade das alternativas
  numAlternativas = mapeamento[length(mapeamento)]
  numCriterios = mapeamento[2]
  qtdSubPorCrit = mapeamento[3:(length(mapeamento)-1)]
  qtdSubPorCrit
  numCriterios
  numAlternativas
  criteriosComSubniveis
  qtdSubniveis
  numeroTotSubcriterios
  pesosSubcriterios
  pesosCriterios
  pesosMAlternativas
  pesosAlternativas = list()
  aux = c()
  for(l in 1:numAlternativas ){
    for(k in 1:numeroTotSubcriterios){
      aux[k] = pesosMAlternativas[[k]][l]
    }
    pesosAlternativas[[l]] = aux
    aux = c()
  }
  pesosAlternativas
  vetorSubCriterios = unlist(pesosSubcriterios,use.names = F)  
  vetorSubCriterios  
  for(i in 1:numAlternativas){
    pesosAlternativas[[i]] = pesosAlternativas[[i]]*vetorSubCriterios
  }
  pesosAlternativas
  pesosAlternativaFinal = c()
  for(i in 1:numAlternativas){
    pesosAlternativaFinal[i] = sum(pesosAlternativas[[i]])
  }
  return(pesosAlternativaFinal)  
} 
tabela2_ahp("F://GitHub//AHP//Documentação//BD_teste3.xlsx",mapeamento = c(1,3,3,3,3,4))
#Referencia: http://www.lbd.dcc.ufmg.br/colecoes/sbsi/2012/0041.pdf
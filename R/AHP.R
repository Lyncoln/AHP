#' Tabela AHP para listas
#' 
#' Essa funcao aplica o metodo AHP de saaty em listas
#'
#'@param dados uma lista contendo as comporacoes entre criterios e alternativas, e por fim, comparacao entre criterios
#'
#'@param nomes vetor que contem o nome das alternativas, se igualado a "padrao" sera gerado automaticamente 
#'pelas letras do alfabeto (A:Z) 
#'
#'@param prop se igualado a TRUE, retornara a tabela em formato de porcentagem, se nao, em formato decimal
#'
#'@return retorna tabela de relacoes de preferencia entre criterios
#'
#'@import readxl
#'@import dplyr
#'@import tidyr
#'@import tibble
#'@import purrr
#'
#'@export

tabela_ahp_lista = function(dados, nomes = "padrao", prop = T) {
  tabela = dados %>% 
    normaliza %>% 
    transforma(nomes)
  organiza = tabela %>% 
    spread(key = item, value = valor)
  linha = tabela %>% 
    group_by(item) %>% 
    summarise(prop = sum(valor))
  coluna = tabela %>% 
    group_by(criterio) %>% 
    summarise(Pesos = sum(valor))
  organiza = inner_join(organiza, coluna, by = "criterio")
  final = linha %>% 
    spread(item,prop) %>% 
    mutate(criterio = names(dados[length(dados)]), Pesos = 1) %>% 
    bind_rows(organiza) %>% 
    select(criterio,Pesos,everything()) %>% 
    inner_join(AAA(dados),by = "criterio")
  if(prop == F) return(final)
  else{
    final = suppressWarnings(mutate_if(final,is_numeric, function(x){paste(round(x*100,2),"%",sep="")}))
    return(final)
  }
}
# Retorna tabela de relações de preferência entre critérios  e alternativas através de um caminho para um arquivo xlsx
#' Tabela AHP para arquivos xlsx
#' 
#' Essa funcao aplica o metodo AHP de saaty em arquivos xlsx
#'
#'@param caminho um caminho de planilha de excel contendo as comporacoes 
#'entre criterios e alternativas, e por fim, comparacao entre criterios
#'
#'@param nomes vetor que contem o nome das alternativas, se igualado a "padrao" sera gerado automaticamente 
#'pelas letras do alfabeto (A:Z) 
#'
#'@param prop se igualado a TRUE, retornara a tabela em formato de porcentagem, se nao, em formato decimal
#'
#'@return retorna tabela de relacoes de preferencia entre criterios
#'
#'@import readxl
#'@import dplyr
#'@import tidyr
#'@import tibble
#'@import purrr
#'
#'@export
tabela_ahp_xlsx = function(caminho, nomes = "padrao", prop = T){
  dados = Ler(caminho)
  final = tabela_ahp_lista(dados, nomes, prop)
  return(final)
}

#'Exemplo de dados 1
#'
#'@format Nesse exemplo teremos quadros de julgamentos onde o objetivo e adquirir o melhor carro entre 3 alternativas.
#'Temos que: AQ = Custo de Aquisicao, CF = Conforto, MA = Custo de manuntencao, PS = Prestigio, RV = Preco de revenda.
#'No primeiro elemento da lista temos o quadro de julgamento do desempenho das alternativas a luz do criterio conforto... 
#'e pelo fim temos o quadro de julgamento da importancia dos criterios a luz do objetivo. 
#'
#'@source dados1
"dados1"


#'Exemplo de dados 2
#'
#'@format Exemplo de dados obtidos para utlizar o método AHP
#'
#'@source dados2
"dados2"



## Inutilizado  pela função tabeola_ahp_excel, que é mais completa
#AHP_SAAT_excel = function(caminho,nomes = "padrao"){
#  BD = Ler(caminho) %>%
#    normaliza() %>%
#    transforma(nomes) %>%
#    xablau()
#  return(BD)
#}



## Inutilizado  pela função tabeola_ahp_lista, que é mais completa
#AHP_SAAT_lista = function(lista,nomes = "padrao"){
#    BD = lista %>%
#    normaliza() %>%
#    transforma(nomes) %>%
#    xablau()
#  return(BD)
#}





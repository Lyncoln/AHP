#' Método AHP
#'
#' Funcao que realiza o metodo AHP em um conjunto de dados apropriado. 
#' 
#'  Lembrar que na arrumacao das planilhas do excel devemos ter : 
#' MAC1, .... , MACn , MCC 
#' As planilhas devem ser nomeadas
#' A primeira coluna da mcc tem que ser a primeira planilha........
#' 
#' @param caminho Um caminho para um arquivo xlsx que contenha as planilhas com os dados

#' 
#' @return Retorna um tibble com as proporcoes para cada alternativa estudada.
#' 
#' @export
#' @import dplyr readxl
#' 
AHP = function(caminho){
  BD = Ler(caminho) %>% 
    normaliza() %>% 
    transforma() %>% 
    xablau()
  return(BD)
}

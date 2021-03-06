% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AHP.R
\name{tabela_ahp_lista}
\alias{tabela_ahp_lista}
\title{Tabela AHP para listas}
\usage{
tabela_ahp_lista(dados, nomes = "padrao", prop = T)
}
\arguments{
\item{dados}{uma lista contendo as comporacoes entre criterios e alternativas, e por fim, comparacao entre criterios}

\item{nomes}{vetor que contem o nome das alternativas, se igualado a "padrao" sera gerado automaticamente 
pelas letras do alfabeto (A:Z)}

\item{prop}{se igualado a TRUE, retornara a tabela em formato de porcentagem, se nao, em formato decimal}
}
\value{
retorna tabela de relacoes de preferencia entre criterios
}
\description{
Essa funcao aplica o metodo AHP de saaty em listas
}

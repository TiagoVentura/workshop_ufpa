## ----knitr_init, echo=FALSE, cache=FALSE---------------------------------

library(knitr)
library(rmdformats)

## Global options
options(max.print="75")
opts_chunk$set(cache=TRUE,
               prompt=FALSE,
               tidy=TRUE,
               comment=NA,
               message=FALSE,
               warning=FALSE)
opts_knit$set(width=75)



## ---- eval=FALSE---------------------------------------------------------
## # Instalando um pacote.
## 
## install.packages("ggplot2")
## install.packages("tidyverse")


## ------------------------------------------------------------------------
# Activando the package
library("ggplot2")
library("tidyverse")


## ------------------------------------------------------------------------
# Objeto numérico
x <- 3

# objeto de texto

my_name <- "Tiago"

# Onde estão? 
ls()

# Remova um objeto
rm(x)

# Checando de novo
ls()

# Você pode reescrevê-los. 

my_name <- "Tiago Da Silva Ventura"

my_name


## ------------------------------------------------------------------------

# vetor de números
X <- c(1, 2.3, 4, 5, 6.78, 6:10)
X

# Class
class(X)




## ------------------------------------------------------------------------

# Coercing
as.data.frame(X)

# Create a data frame

data <- data.frame(name="Tiago", last_name="ventura", school="UMD", age=30)

data


## ------------------------------------------------------------------------

# Coerce to a matrix
as.matrix(X)


## ------------------------------------------------------------------------

# coerce to a list

as.list(X)

# or

list<- list(X, data)

# See the list

str(list)



## ------------------------------------------------------------------------
getwd () # Obter o diretório de trabalho atual



## ------------------------------------------------------------------------
# adicione onde você quer o R olhando. 

setwd("C:/Users/Tiago Ventura/Dropbox/webscraping_workshop_ufpa/html/dia_00")



## ----eval=FALSE----------------------------------------------------------
## # Install from CRAN
## install.packages("tidyverse")
## 
## # Or the development version from GitHub
## # install.packages("devtools")
## 
## devtools::install_github("hadley/tidyverse")
## 


## ------------------------------------------------------------------------

library(tidyverse)



## ---- eval=FALSE---------------------------------------------------------
## 
## # Exemplo 1
## 
## # R
## 
## round(exp(diff(log(runif(100, 0,1)))), 1)
## 
## # Com pipe
## 
## runif(100, 0, 1) %>%
##   log() %>%
##   diff() %>%
##   exp() %>%
##   round(.,1) # or round(1)


## ------------------------------------------------------------------------

# OBSERVAÇÃO AQUI: se for uma função do tidyverse, os dados serão sempre o primeiro argumento,
# portanto, não é necessário adicionar o período. Se não for, você
# deve adicionar o período no argumento de dados

# data %>% function_tidyverse() %>% lm( y~x, data=. )

mtcars %>% 
  lm(mpg~cyl, data=.) %>% 
  coef()

# Exampl2 2: Graph

plot(density(rnorm(1000, 0, 1)))

# With pipe

rnorm(1000, 0, 1) %>% 
  density() %>%
  plot()



## ------------------------------------------------------------------------

#install.packages("electionsBR")
library(electionsBR)
library(kableExtra)

# Importando dados 
d  <- party_mun_zone_fed(2014, uf = "PA")

# O que sao os dados?
glimpse(d) 



## ------------------------------------------------------------------------

# Dplyr 

d <- d %>% 
   select(CODIGO_CARGO, NOME_COLIGACAO, SIGLA_PARTIDO, NOME_MUNICIPIO, 
          QTDE_VOTOS_NOMINAIS,  QTDE_VOTOS_LEGENDA) 



# Se voce nao quiser usar o pipe

select(d, CODIGO_CARGO, NOME_COLIGACAO, SIGLA_PARTIDO, NOME_MUNICIPIO, 
          QTDE_VOTOS_NOMINAIS,     QTDE_VOTOS_LEGENDA) 


# Retirar Coluna
d %>% select(-CODIGO_CARGO)





## ------------------------------------------------------------------------

# Somente o PT

d %>% 
  filter(SIGLA_PARTIDO == "PT")


# Duas condicoes

d %>% 
  filter(SIGLA_PARTIDO == "PT" , NOME_MUNICIPIO=="BELÉM")
  




## ------------------------------------------------------------------------


# Razão entre a quantidade de votos de legenda e nominais para deputado federal

# Primeiro, limite a deputados federais.

d <- d %>% filter(CODIGO_CARGO==6)

d %>%
  mutate(razaovotos=QTDE_VOTOS_LEGENDA/(QTDE_VOTOS_NOMINAIS+QTDE_VOTOS_LEGENDA)) %>% 
  select(razaovotos, SIGLA_PARTIDO, NOME_MUNICIPIO)

  




## ------------------------------------------------------------------------

# What was the largest difference in gols?

d %>%
  mutate(razaovotos=QTDE_VOTOS_LEGENDA/(QTDE_VOTOS_NOMINAIS+QTDE_VOTOS_LEGENDA)) %>% 
  select(razaovotos, SIGLA_PARTIDO, NOME_MUNICIPIO) %>%
  arrange(desc(razaovotos))

# Elimine 1. Sao partidos que receberam somente votos de legenda

d %>%
  mutate(razaovotos=QTDE_VOTOS_LEGENDA/(QTDE_VOTOS_NOMINAIS+QTDE_VOTOS_LEGENDA)) %>% 
  select(razaovotos, SIGLA_PARTIDO, NOME_MUNICIPIO) %>%
  arrange(desc(razaovotos)) %>%
  filter(razaovotos!=1)
  
  


## ------------------------------------------------------------------------

d_new <- d %>%

  # Repetimos o que fizemos 
  mutate(razaovotos=QTDE_VOTOS_LEGENDA/(QTDE_VOTOS_NOMINAIS+QTDE_VOTOS_LEGENDA)) %>% 
 
  select(razaovotos, SIGLA_PARTIDO, NOME_MUNICIPIO) %>%
  
  filter(razaovotos!=1)  %>%
  
  # Vamos filtrar pelos 5 maiores partidos
  
  filter(SIGLA_PARTIDO %in% c("PT", "PSDB", "PSB", "DEM", "PP"))


# Grafico
ggplot(d_new, aes(y=razaovotos, x=SIGLA_PARTIDO,fill=SIGLA_PARTIDO)) + 

  geom_boxplot(alpha=.5) + 
  
  xlab("Sigla Partido") +
  
  ylab("Votos de Legenda / Votos Total ")




## ------------------------------------------------------------------------

# Mais votos  do PSL

d %>%
  arrange(desc(QTDE_VOTOS_NOMINAIS)) %>%
  filter(SIGLA_PARTIDO=="PSL") %>% 
  slice(1:5)  

View(d)




## ------------------------------------------------------------------------

# Média e total dos votos por Partido no Estado

 d %>% 
  
  group_by(SIGLA_PARTIDO) %>%
  
  summarise(media_votos=mean(QTDE_VOTOS_NOMINAIS, na.rm=TRUE), 
            total_votos=sum(QTDE_VOTOS_NOMINAIS, na.rm = TRUE)) %>%
  ungroup()




# Principal Munícipio dos 5 maiores partidos

d %>% 
  
 group_by(SIGLA_PARTIDO, NOME_MUNICIPIO) %>%
  
 summarise(total_votos=sum(QTDE_VOTOS_NOMINAIS, na.rm = TRUE)) %>%
  
 filter(SIGLA_PARTIDO%in%c("DEM", "PT", "PSDB", "PP", "PSB")) %>%
  
  top_n(1)




## ------------------------------------------------------------------------

glimpse(d)




## ------------------------------------------------------------------------

# Número de Zonais que os partidos receberam votos

d %>%
  
  # exclue zero
  
  filter(QTDE_VOTOS_NOMINAIS>0) %>%

  # Contando
  
  count(SIGLA_PARTIDO) 



# Nice graph


d_for_the_graph <- d %>%
  
  # exclue zero
  
  filter(QTDE_VOTOS_NOMINAIS>0) %>%

  # Contando
  
  count(SIGLA_PARTIDO, name="VotoNominal") 

d_for_the_graph2 <- d %>%
  
  # exclue zero
  
  filter(QTDE_VOTOS_LEGENDA>0) %>%

  # Contando
  
  count(SIGLA_PARTIDO, name="VotoLegenda") 


# Ligar os dois data frames

d_for_graph <- left_join(d_for_the_graph, d_for_the_graph2)

library(ggrepel)
ggplot(d_for_graph, aes(y=VotoNominal, x=VotoLegenda, label=SIGLA_PARTIDO)) +
  geom_point() + geom_label_repel() + 
  geom_segment(aes(y=80, yend=160, x=80, xend=160))

  


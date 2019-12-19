## ----knitr_init, echo=FALSE, cache=FALSE---------------------------------

library(knitr)
library(rmdformats)

## Global options
opts_chunk$set(cache=TRUE,
               prompt=FALSE,
               tidy=TRUE,
               comment=NA,
               message=FALSE,
               warning=FALSE)




## ----echo=FALSE----------------------------------------------------------

# Pacotes para acessar APIs
library(httr) 
library(jsonlite)
library(tidyverse)

# Solicitar a senha
get_jwt <- httr::POST("https://api.fogocruzado.org.br/api/v1/auth/login",
                 query = list(email = "seu_email", password = "xxxxxxx"))


# Pegar a senha
token <- httr::content(get_jwt)$access_token

# token = sua senha de acesso à API. Nao compartilhe por ai.


## ------------------------------------------------------------------------

# Passo 1. Crie a Url
base_url <- "https://api.fogocruzado.org.br/api/v1"
occurences <- "/occurrences"
api <- paste0(base_url, occurences)
print(api)

# Passo 2: Acesse a API

response <- GET(api,
                  add_headers('Authorization' = paste("Bearer", token, sep = " ")))

# Qual o resultado?
response




## ------------------------------------------------------------------------

# Converter para um json
json_fogo_cruzado <- content(response, as="text", encoding = "UTF-8")


## ------------------------------------------------------------------------


output <- fromJSON(json_fogo_cruzado) %>%
              tibble::as_tibble()

glimpse(output)


## ------------------------------------------------------------------------
query_list <- list(presen_agen_segur_ocorrencia="1",
                     uf_estado="RJ")


response <- GET(api,
              add_headers('Authorization' = paste("Bearer", token, sep = " ")), 
          query=query_list)

output <- jsonlite::fromJSON(httr::content(response, as="text", encoding = "UTF-8")) %>%
    tibble::as_tibble()



## ------------------------------------------------------------------------

output %>%
  count(nome_cidade) %>% 
  top_n(10) %>% 
  arrange(n)  %>%
  ggplot(., aes(y=n, x=nome_cidade)) +
  geom_col() + theme_minimal() +coord_flip() +
  ylab("Tiroteios com Presença Policial") + xlab(" Cidades do Rio de Janeiro")



## ----echo=FALSE----------------------------------------------------------

library(ggmap)
register_google("token_google") # insert your API key here



## ------------------------------------------------------------------------
library(ggmap)
library(RColorBrewer)


ggmap(get_googlemap("rio de janeiro", zoom = 11, maptype = "roadmap", scale=2)) +
    geom_point(data = output, 
               aes(x = longitude_ocorrencia, 
               y = latitude_ocorrencia), 
               color="tomato2", alpha=.8, size = 0.3)



## ----eval=FALSE----------------------------------------------------------
## 
## #Instalação do pacote
## install.packages("devtools") # pacote para acessar o github
## devtools::install_github("voltdatalab/crossfire")


## ----echo=FALSE----------------------------------------------------------
library(crossfire)

# Registra usuario e senha, e envia sua senha da API

fogocruzado_signin(email = "venturat@umd.edu", password = "xxxxxxxx")

# Extrair os dados que pegamos manualmente antes

fogocruzado_rj <- get_fogocruzado(state= "RJ", security_agent = 1)

# Colocar em gráfico mais uma vez. 

ggmap(get_googlemap("rio de janeiro", zoom = 11, maptype = "roadmap", scale=2)) +
    geom_point(data = fogocruzado_rj, 
               aes(x = longitude_ocorrencia, 
               y = latitude_ocorrencia), 
                alpha=.8, size = 0.5, color="darkred")
    


## ----eval=FALSE----------------------------------------------------------
## library(crossfire)
## 
## # Registra usuario e senha, e envia sua senha da API
## 
## fogocruzado_signin(email = "venturat@umd.edu", password = "xxxxxx")
## 
## # Extrair os dados que pegamos manualmente antes
## 
## fogocruzado_rj <- get_fogocruzado(state= "RJ", security_agent = 1)
## 
## # Colocar em gráfico mais uma vez.
## 
## ggmap(get_googlemap("rio de janeiro", zoom = 11, maptype = "roadmap", scale=2)) +
##     geom_point(data = fogocruzado_rj,
##                aes(x = longitude_ocorrencia,
##                y = latitude_ocorrencia),
##                 alpha=.8, size = 0.5, color="darkred")
## 


## ----eval=FALSE----------------------------------------------------------
## # Instale o pacote
## 
## install.packages("congressbr")
## devtools::install_github("RobertMyles/congressbr")
## 


## ------------------------------------------------------------------------
library(congressbr)

# Ajuda em R
?cham_legislator_list


## ------------------------------------------------------------------------
all <- cham_legislator_list()
glimpse(all)



## ------------------------------------------------------------------------

ano<-c(2002:2018)

proposicoes <- map(ano, ~ 
                     cham_plenary_bills(.x) %>%
                     mutate(ano=.x)) 

# Vamos combinar tudo 

proposicoes <- bind_rows(proposicoes) 

# Eliminar repeticoes 

proposicoes <- proposicoes %>% distinct()

# Agregar por ano

proposicoes_ano <- proposicoes %>% count(ano)

# Marcar anos pre eleitorais
proposicoes_ano <- proposicoes_ano %>% 
                      mutate(ano_eleitoral=ifelse(ano==2002|ano==2006|
                                                    ano==2010|ano==2014|ano==2018, "Ano Eleitoral", 
                                                  "Ano Não Eleitoral"))

ggplot(proposicoes_ano, aes(y=n, x=ano, fill=ano_eleitoral)) +
  geom_col() +
  scale_fill_manual(name="", values = c("darkred", "darkblue")) +
  theme_minimal() +
  xlab("ano") + ylab("Proposições Votadas") 



## ----echo=FALSE----------------------------------------------------------

base="http://www.transparencia.gov.br/"
endpoint="api-de-dados/bolsa-familia-por-municipio"

response <- GET(paste0(base, endpoint), 
          query=list(mesAno="201812", pagina="1", codigoIbge="1501402"))

output <- jsonlite::fromJSON(httr::content(response, as="text", encoding = "UTF-8")) %>%
    tibble::as_tibble()




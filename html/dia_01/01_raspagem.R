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




## ---- eval=FALSE---------------------------------------------------------
## 
## <html>
## <head>
##   <title> Michael Cohen's Email </title>
##   <script>
##     var foot = bar;
##   <script>
## </head>
## <body>
##   <div id="payments">
##   <h2>Second heading</h2>
##   <p class='slick'>information about <br/><i>payments</i></p>
##   <p>Just <a href="http://www.google.com">google it!</a></p>
##   <table>
## 


## ---- eval=FALSE---------------------------------------------------------
## 
## # Instalar pacotes
## install.packages("tidyverse")
## install.packages("purrr")
## install.packages("rvest")
## install.packages("stringr")
## install.packages("kableExtra")
## install.packages("Rcurl")
## 
## 


## ------------------------------------------------------------------------
# Ativar os pacotes
library("tidyverse")
library("purrr")
library("rvest")
library("stringr")
library("kableExtra")



# Crie o nome da sua url

minha_url <- "https://pt.wikipedia.org/wiki/Lista_de_munic%C3%ADpios_fronteiri%C3%A7os_do_Brasil"

# Somente o nome
print(minha_url)

# Raspe os dados. Simples assim:

source <- read_html(minha_url)

# O que é esse objeto?

class(source) # XML=HTML

# Como extrair a tabela?

tabelas <- source %>% 
         rvest::html_table() 


# O que eu tenho aqui?

map(tabelas, head)


## ---- results='asis'-----------------------------------------------------
tabela_limpa <- tabelas[[3]] %>% 
            
            # Converte para um banco de dados mais bonito
            as.tibble() %>% 
  
            # Cria Duas novas Colunas
            mutate(city = Município, 
                   uf_name = Estado) %>%
            select(city, uf_name) %>%
            
            # conserta o enconding
            mutate(city = str_sub(city,5),
             city = str_replace(city, pattern="- ", ""), 
             city =  str_trim(city),
             city_key = stringi::stri_trans_general(city, "Latin-ASCII"), 
             city_key= str_replace_all(city_key, " ", ""), 
             city_key=str_to_lower(city_key)) 


tabela_limpa %>% slice(1:5) %>% kable(.) %>%
  kable_styling(full_width = F) 




## ------------------------------------------------------------------------

# Selecione a URL

minha_url <- "https://pt.wikipedia.org/wiki/Elei%C3%A7%C3%B5es_municipais_no_Brasil_em_1985"

# Pega a página
page <- read_html(minha_url)

substr(html_text(page), 1, 1000) # first 1000 characters


# Pegue as tabelas

out <-  page %>% 
        html_nodes(".wikitable") %>%
        html_table()

# Combinando as tabelas

out_cap <- out[1]
out_mun <- out[2:19]
ot_municipios <- out_mun %>%  bind_rows()

ot_municipios %>% 
  slice(1:5) %>%
  kable(.) %>%
  kable_styling(full_width = F) 



## ------------------------------------------------------------------------
write.csv(ot_municipios, "mun_1985.csv")


## ------------------------------------------------------------------------
# Coleta de todos os nomes

minha_url <- "https://www.alepa.pa.gov.br/deputados.asp"

nomes <- read_html(minha_url) %>%
          html_nodes(css="#conteudo .mb-1") %>% 
          html_text()

# Limpa os nomes

nomes_limpos <- nomes %>% 
                  str_remove_all(., "Dep") %>%
                  str_to_lower() %>%
                  str_trim() %>%
                  str_replace_all(" ", "") %>%
                  str_replace("\\.","") %>%
                  stringi::stri_trans_general("Latin-ASCII")
  
                  


## ------------------------------------------------------------------------
# url base
base_url <- "https://www.alepa.pa.gov.br/"

# Combina com o nome do deputado
url_dep <- paste0(base_url, nomes_limpos)
head(url_dep)

# Vamos usar uma função para ver se cada link existe
library(RCurl)

id <- map(url_dep, url.exists) 

id %>% unlist() %>% tibble(id=., nomes_limpos) %>% filter(id==FALSE)



## ------------------------------------------------------------------------
nomes_limpos <- nomes_limpos %>% 
                  str_replace("bordalo", "carlosbordalo")%>%
                  str_replace("martinhocarmona", "deputado.asp?id_rep=129")

# Repetir nosso controle de qualidade

# Combina com o nome do deputado
url_dep <- paste0(base_url, nomes_limpos)
head(url_dep)

# Vamos usar uma função para ver se cada link existe
library(RCurl)

id <- map(url_dep, url.exists) 

id %>% unlist() %>% tibble(id=., nomes_limpos) %>% filter(id==FALSE)



## ------------------------------------------------------------------------

# url
url_dep1 <- url_dep[[1]]

#source
source <- url_dep1 %>% read_html()


# Informações de nosso interesse

nome <- url_dep1 %>% str_remove("https://www.alepa.pa.gov.br/")

posicao <-  source %>% 
            html_nodes(css=".col-lg-8 .col-lg-8 .text-primary") %>%
            html_text()


biografia <- source %>% 
            html_nodes(css=".col-lg-8 .col-lg-8 p") %>%
            html_text() %>% 
            paste0(., collapse = " ")

noticias <- source %>% 
    html_nodes(css=".font-weight-bold a") %>%
            html_attr("href")

twitter <- source %>% 
    html_nodes(css=".p-2 a") %>%
            html_attr("href") %>%
          str_subset("twitter")

email <- source %>% 
    html_nodes(css=".mt-0 a") %>%
                html_attr("href") %>%
            str_subset("@") %>%
            str_remove("mailto:")

# Combina tudo como um banco de dados

deputados <- data_frame(url_dep1, nome, posicao,
                        biografia, noticias, twitter, email)



deputados


## ---- eval=FALSE---------------------------------------------------------
## 
## 
## nome_da_funcao <- function(arg1,arg2){
## 
## # O que ela faz
## 
## out <- what the function does.
## 
## # Output
## return(out) # output
## 
## }
## 
## 
## 


## ------------------------------------------------------------------------

add_me <- function( argument1, argument2 ){
  value <- argument1 + argument2
  return(value) # "return" means "send this back once the function is done"
}

add_me(2,3)



## ------------------------------------------------------------------------

url <- url_dep[[1]]

raspar_alepa <- function(url){
  
  
# Mudamos onde tinhamos url_dep1 para url.
  
source <- url %>% read_html() # unica modificacao


# Informações de nosso interesse

nome <- url %>% str_remove("https://www.alepa.pa.gov.br/")

posicao <-  source %>% 
            html_nodes(css=".col-lg-8 .col-lg-8 .text-primary") %>%
            html_text()


biografia <- source %>% 
            html_nodes(css=".col-lg-8 .col-lg-8 p") %>%
            html_text() %>% 
            paste0(., collapse = " ")

noticias <- source %>% 
    html_nodes(css=".font-weight-bold a") %>%
            html_attr("href") 
      
twitter <- source %>% 
    html_nodes(css=".p-2 a") %>%
            html_attr("href") %>%
          str_subset("twitter") %>%
            .[1]

email <- source %>% 
    html_nodes(css=".mt-0 a") %>%
                html_attr("href") %>%
            str_subset("@") %>%
            str_remove("mailto:") 
# Combina tudo como um banco de dados

deputados <- tibble(url_dep1, nome, posicao,
                        biografia, noticias, twitter, email)


# Output

return(deputados)

# Desligando R um pouco para nao sobrecarregar os dados

Sys.sleep(sample(5:10, 1))

}


# Vamos aplicar a função a somente um caso

raspar_alepa(url_dep[[20]])




## ------------------------------------------------------------------------
# Aplicando nossa lista de links a uma funcção. 

dados <- map(url_dep, raspar_alepa) 

# Combine tudo

dados <- bind_rows(dados)


# Vamos ver
kable(dados) %>%
  kable_styling() %>%
  scroll_box(width = "100%", height = "200px")




## ------------------------------------------------------------------------
write.csv(dados, "deputados_para.csv")


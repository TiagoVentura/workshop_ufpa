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





## ----eval=FALSE----------------------------------------------------------

## app_name<-"seu_app_name"
## consumer_key<-"seu_consumer_key"
## consumer_secret<-"seu_consumer_secret"
## access_token<- "seu_acess_token"
## access_token_secret<-"seu_token_secret"
## 


## ----eval=FALSE----------------------------------------------------------
## # Faça o download do pacote
## library(devtools)
## install_github("mkearney/rtweet")


## ------------------------------------------------------------------------
library(rtweet)
create_token(app=app_name,
             consumer_key=consumer_key, 
             consumer_secret=consumer_secret,
             access_token = access_token,
             access_secret = access_token_secret)


## ------------------------------------------------------------------------

bolsonaro_tweets <-search_tweets("Bolsonaro", n=100, include_rts = FALSE)




## ------------------------------------------------------------------------
bolsonaro_tweets <-stream_tweets("Bolsonaro", n=100, include_rts = FALSE)
bolsonaro_tweets$text[1]


## ------------------------------------------------------------------------
tt <- get_trends("Brazil")
tt$trend[1:10]


## ----message=FALSE-------------------------------------------------------
library(tidyverse)

deputados <- read_csv("C:/Users/Tiago Ventura/Dropbox/webscraping_workshop_ufpa/html/dia_01/deputados_para.csv")

# Seleciado os que possuem twitter

deputados_twitter <- deputados %>% 
                        filter(!is.na(twitter))


# Vamos extrair somente as tags
names <- str_remove_all(deputados_twitter$twitter, "https://www.twitter.com/|https://twitter.com/"  )
names


## ------------------------------------------------------------------------
dep1_tweets <- get_timelines(names[1], n = 5)
head(dep1_tweets$text)


## ------------------------------------------------------------------------
dep1_usuarios<- lookup_users(names[1])
dep1_usuarios$screen_name


## ------------------------------------------------------------------------
dep1_favorites<-get_favorites(names[1], n=5)
dep1_favorites$text


## ------------------------------------------------------------------------
dep1_seguidores<-get_followers(names[1])
dep1_seguidores


## ---- eval=FALSE---------------------------------------------------------
## 
## post_tweet("Eu estou postando esse tweet a
##            partir do pacote rtweet para
##             mostrar aos alunos da UFPA
##            o incrível mundo do R")
## 


## ------------------------------------------------------------------------
text <- map(names, ~ get_timelines(.x, n = 3000))




## ------------------------------------------------------------------------
# Combinar todos

tweets <- bind_rows(text) %>% 
            select(screen_name, text)

tweets


## ------------------------------------------------------------------------
library(quanteda)
library(tidytext)

# Crie um corpus de textos
corpus <- corpus(tweets$text)
docvars(corpus) <- data_frame(deputados=tweets$screen_name)


# Limpe os textos com palavras que importam pouco para a analise
# Crie uma Document-Feature Matrix


palavras <- c("https","t.co", "http")

dfm <- tokens(corpus, remove_punct = TRUE,
                              remove_numbers = TRUE, 
                              remove_symbols=TRUE) %>% 
                    tokens_select(., min_nchar = 3L) %>% 
    tokens_remove(., c(stopwords("pt"), palavras)) %>%
      dfm()

summary(dfm)



## ------------------------------------------------------------------------
textplot_wordcloud(dfm)


## ------------------------------------------------------------------------
features_dfm <- textstat_frequency(dfm, n = 50)

# Sort by reverse frequency order
features_dfm$feature <- with(features_dfm, reorder(feature, -frequency))

ggplot(features_dfm, aes(x = feature, y = frequency)) +
    geom_point(shape=21, size=3, fill="darkred") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    theme_minimal() + coord_flip()



## ------------------------------------------------------------------------
# Get frequency grouped by president

freq_grouped <- textstat_frequency(dfm, 
                                   groups = "deputados")

# Filter the term "american"
freq_bolsonaro <- subset(freq_grouped, freq_grouped$feature %in% "bolsonaro")  


ggplot(freq_bolsonaro, aes(x = group, y = frequency)) +
    geom_point(shape=21, size=3, fill="darkred") + 
    xlab(NULL) + 
    ylab("Frequency") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    theme_minimal(base_size = 16) + coord_flip()



## ----message=FALSE-------------------------------------------------------
# Para estimar os modelos de tópicos, vamos usar o pacote de R `STM`. 
# Este pacote exige uma pequena transformação no objeto
library(stm)
dfm_stm <-  quanteda::convert(dfm, to = "stm")

model_parag <- stm(dfm_stm$documents, dfm_stm$vocab, K = 5,
                            data = dfm_stm$meta, init.type = "Spectral", verbose = FALSE)

# Topicos 
labelTopics(model_parag)

# Visualizar
plot(model_parag)



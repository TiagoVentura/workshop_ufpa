## ----echo=FALSE----------------------------------------------------------
# Chunck base
library(knitr)
opts_chunk$set(cache=TRUE,
               prompt=FALSE,
               tidy=TRUE,
               comment=NA,
               message=FALSE,
               warning=FALSE)





## ------------------------------------------------------------------------

# Ative os pacotes

library(tidyverse)
library(igraph)
library(data.table)
library(arm)
library(broom)
library(RColorBrewer)
library(KernSmooth)



# Abra o banco de dados
getwd()
#setwd("./html/dia_03/")
load("dados_bolsonaro.RData")
summary(dados_bolsonaro)


## ------------------------------------------------------------------------

# Construíndo uma rede

# Passo 1: Selecione os nós -- Autoridades -> Hub

data <- cbind(dados_bolsonaro$namehub, dados_bolsonaro$nameauth)
head(data)


# Passo 2: Crie a estrutura da rede
net <- graph.empty() # Cria um gráfico vazio
net <- add.vertices(net, length(unique(c(data))),
                      name=as.character(unique(c(data)))) # número de nós
net <- add.edges(net, t(data))

summary(net)

# Passo3: Adicione as variáveis

E(net)$text <- dados_bolsonaro$text
E(net)$tweetidT <- dados_bolsonaro$tweetidT
E(net)$tweetidRT <- dados_bolsonaro$tweetidRT
E(net)$friendsT <- dados_bolsonaro$friendsT
E(net)$followersT <- dados_bolsonaro$followersT
E(net)$friendsRT <- dados_bolsonaro$friendsRT
E(net)$followersRT <- dados_bolsonaro$followersRT
E(net)$timeRT <- dados_bolsonaro$timeRT
E(net)$timeT <- dados_bolsonaro$timeT
E(net)$verifiedRT <- dados_bolsonaro$verifiedRT 
E(net)$verifiedT <- dados_bolsonaro$verifiedT
E(net)$nameauth <- dados_bolsonaro$nameauth
E(net)$namehub <- dados_bolsonaro$namehub
E(net)$web <- dados_bolsonaro$web
E(net)$device <- dados_bolsonaro$device 

summary(net)



## ------------------------------------------------------------------------

# Adicionar indegree and outdegree
V(net)$outdegree<-degree(net, mode="out")
V(net)$indegree<-degree(net, mode="in")

# Não rode isso. Vai travar seu PC

# Layout
# system.time(l <- layout_with_fr(net, grid = c("nogrid")))

#Comunidades
#my.com.fast <- walktrap.community(net)

# Abrindo o material salvo
load("vertices_bolsonaro.Rdata")

# Adicionando a rede

V(net)$l1 <- vertices_bolsonaro$l1
V(net)$l2 <- vertices_bolsonaro$l2
V(net)$membership <- vertices_bolsonaro$membership

summary(net)




## ------------------------------------------------------------------------

comunidades<- data_frame(membership=V(net)$membership)

comunidades %>% 
    count(membership) %>%
    arrange(desc(n)) %>%
    top_n(5)




## ----autoridade----------------------------------------------------------



# Cria um banco com indegree

autoridade <- data_frame(name=V(net)$name, ind=V(net)$indegree, 
                         membership=V(net)$membership) %>%
              filter(membership==1| membership==2 | membership==5) %>%
              split(.$membership) %>%
              map(~ arrange(., desc(ind))) %>%
              map(~ slice(., 1:30)) 




# Comunidade 1

ggplot(autoridade[[1]], aes(x=reorder(name,
                               ind),
                     y=ind)) + 
    geom_histogram(stat="identity", width=.5, color="black", 
                   fill="darkred") +
  coord_flip() +
    xlab("") + ylab("") + 
  theme_minimal(base_size = 12) + 
  theme(plot.title = element_text(size = 22, face = "bold"), 
        axis.title=element_text(size=16), 
        axis.text = element_text(size=12, face="bold")) 


# Comunidade 2

ggplot(autoridade[[2]], aes(x=reorder(name,
                               ind),
                     y=ind)) + 
    geom_histogram(stat="identity", width=.5, color="black", 
                   fill="yellow") +
  coord_flip() +
    xlab("") + ylab("") + 
  theme_minimal(base_size = 12) + 
  theme(plot.title = element_text(size = 22, face = "bold"), 
        axis.title=element_text(size=16), 
        axis.text = element_text(size=12, face="bold")) 




# Comunidade 3

ggplot(autoridade[[3]], aes(x=reorder(name,
                               ind),
                     y=ind)) + 
    geom_histogram(stat="identity", width=.5, color="black", 
                   fill="steelblue") +
  coord_flip() +
    xlab("") + ylab("") + 
  theme_minimal(base_size = 12) + 
  theme(plot.title = element_text(size = 22, face = "bold"), 
        axis.title=element_text(size=16), 
        axis.text = element_text(size=12, face="bold")) 





## ------------------------------------------------------------------------

# Cria uma função

plot_autoridades <- function(dados, nome_arquivo, base){
ggplot(dados, aes(x=reorder(name,
                               ind),
                     y=ind)) + 
    geom_histogram(stat="identity", width=.5, color="black", 
                   fill="darkred") +
  coord_flip() +
    xlab("") + ylab("") + 
  theme_minimal(base_size = 16) + 
  theme(plot.title = element_text(size = 22, face = "bold"), 
        axis.title=element_text(size=22), 
        axis.text = element_text(size=12, face="bold")) 
  
   ggsave(filename = paste0(base,nome_arquivo),
      width = 12, height = 12, units = "in",
     pointsize = 22, bg = "white")
}

# Endereço no meu computador
base="C:/Users/Tiago Ventura/Dropbox/Apps/Overleaf/workshop_UFPA_rep/"

# Usa a função para gerar o arquivo
plot_autoridades(dados=autoridade[[1]], base=base, nome_arquivo = "auth_antibolsonaro.png")
plot_autoridades(dados=autoridade[[2]], base=base, nome_arquivo = "auth_bolsonaro.png")



## ------------------------------------------------------------------------

# Cria um padrão de busca
library(rebus)
padrao_regexpres <- "#[A-Za-z]+[A-Za-z0-9_]+" 
# Coletamos qualquer palavra precedida de um #



# Vamos usar um exemplo pra testar


hashtags <- dados_bolsonaro %>% 
                    mutate(hashtag=str_extract_all(text, padrao_regexpres)) %>% 
                    unnest()


hashtags %>% 
      count(hashtag) %>% 
      arrange(desc(n)) %>% 
      slice(1:10)



## ------------------------------------------------------------------------

# Contando as hashtags

data_hash<- hashtags %>% 
            count(hashtag) %>% 
            arrange(desc(n)) %>% 
            slice(1:30)

ggplot(data_hash, aes(x=reorder(hashtag,
                               n),
                     y=n)) + 
    geom_histogram(stat="identity", width=.5, color="black", 
                   fill="steelblue") +
  coord_flip() +
    xlab("") + ylab("") + 
  theme_minimal(base_size = 12) + 
  theme(plot.title = element_text(size = 22, face = "bold"), 
        axis.title=element_text(size=16), 
        axis.text = element_text(size=12, face="bold")) 




## ------------------------------------------------------------------------

# Primeiro, vamos fazer um merge com os dados de comunidade
hashtags <- left_join(hashtags, vertices_bolsonaro)

# Tres Colunas novas
colnames(hashtags)



# Vamos aggrupar por comunidade

hashtags_por_comunidade <- hashtags %>%
                           filter(membership==1| membership==2) %>%
                             mutate(membership=as_factor(membership), 
                                 membership=fct_recode(membership, "Anti-Bolsonaro"="1", 
                                                                    "Bolsonaro"="2"))


data_hash <- hashtags_por_comunidade %>% 
            group_by(membership, hashtag) %>% 
            summarise(n=n()) %>%
            top_n(10, n)


g <- ggplot(data_hash, aes(x=reorder(hashtag,
                               n),
                     y=n, fill=membership)) + 
  geom_histogram(stat="identity", width=.5, color="black") +
  coord_flip() +
    xlab("") + ylab("") + 
  scale_fill_manual(name="Comunidade", values=c("Red", "Yellow", "Gray")) +
  theme_minimal(base_size = 16) + 
  theme(plot.title = element_text(size = 22, face = "bold"), 
        axis.title=element_text(size=12), 
        axis.text = element_text(size=12, face="bold"), 
        strip.text = element_text(size=16)) +
  facet_wrap(~membership, scale="free") 

# Save
ggsave(plot=g, filename = "graph_community.png",
      width = 12, height = 12, units = "in",
     pointsize = 22, bg = "white")



## ------------------------------------------------------------------------

# Funcao para visualizar a red

my.den.plot <- function(l=l,new.color=new.color, ind=ind, legend){
  #Numero efectivo de Comunidades
  ENCG<-round(1/sum(round(table(new.color)/sum(table(new.color)),3)^2),2)
  
  est <- bkde2D(l, bandwidth=c(10, 10))
  plot(l,cex=log(ind+1)/4, col=new.color, pch=16, xlim=c(-160,140),ylim=c(-140,120), xlab="", ylab="", axes=FALSE)
  contour(est$x1, est$x2, est$fhat,  col = gray(.6), add=TRUE)
  legend("topright", c(legend[1],legend[2]), pch = 17:18, col=c("#B2182B", "#2166AC"))
  text(-140,115,paste("ENCG: ",ENCG,sep=""), cex=1, srt=0)
} 

# Crie as cores para cada comunidade

# Building a empty containes
temp <- rep(1,length(V(net)$membership))
new.color <- "white"
new.color[V(net)$membership==1] <- "red" ####
new.color[V(net)$membership==2] <- "blue" ####
new.color[V(net)$membership==5] <- "gray" ####

# Adiciona a nova cor
V(net)$new.color <- new.color


# Plot

#save first
png(filename ="Bolsonarobasicmap.png", width = 8, height = 8, units = "in", pointsize = 12, bg = "white", res = 300)
my.den.plot(l=cbind(V(net)$l2,V(net)$l1),new.color=V(net)$new.color, ind=V(net)$indegre, legend =c("Anti-Bolsonaro", "Pro-Bolsonaro"))
dev.off()



## ------------------------------------------------------------------------

# Vamos primeiro selecionar as mídias mais ativadas
keynews <- head(sort(table(unlist(E(net)$web)),decreasing=TRUE),12)
keynews.names <- names(keynews)

N<-length(keynews.names)
count.keynews<- array(0,dim=c(length(E(net)),N))

# Looping 
for(i in 1:N){
  temp<- grepl(keynews.names[i],E(net)$web, ignore.case = TRUE)
  #temp <- str_match(E(net)$text,'Arangur[A-Za-z]+[A-Za-z0-9_]+')
  count.keynews[temp==TRUE,i]<-1
  Sys.sleep(0.1)

  }


# Setting the names of the media

colnames(count.keynews)<- keynews.names


## ---- eval=FALSE---------------------------------------------------------
## # Conexoes de Vertices e Edges
## # Vamos recuperar todos os nós e edges que estão ligados uns com os outros.
## 
## el <- get.adjedgelist(net, mode="all")
## al <- get.adjlist(net, mode="all")
## 


## ---- echo=FALSE---------------------------------------------------------
# As funções acima demoram muito. Portanto, vou abrir a versão salva do artigo.
load("el_bolsonaro.Rdata")
load("al_bolsonaro.Rdata")
al[[1]]


## ------------------------------------------------------------------------

fomfE<- function(var=var, adjV=adjV,adjE=adjE){
  stemp <- sapply(adjE, function(x) sum(var[x]))
  mstemp <- sapply(adjV, function(x) mean(stemp[x]))
  out<-cbind(stemp,mstemp)
}



# Cria um container
resultado_midia<- array(0,dim=c(length(V(net)),N))

for(i in 1:N){
  bb<-fomfE(count.keynews[,i],al,el)
  bb[bb[,1]=="NaN"]<-0
  resultado_midia[,i]<- bb[,1]
}


colnames(resultado_midia)<- keynews.names

head(resultado_midia)



## ------------------------------------------------------------------------

op <- par(mfcol=c(3,4))  
  for(i in 1:12){
  plot(V(net)$l2,V(net)$l1,pch=16,  
       col=V(net)$new.color, cex=log(resultado_midia[,i]+1)/3,
       ylim=c(-130,130),xlim=c(-130,130), xlab="", ylab="",
       main=colnames(resultado_midia)[i], cex.main=1)
  }
par(op)
dev.off()


## ----eval=FALSE----------------------------------------------------------
## 
## for(i in 1:12){
## # Salva primeiro
## png(filename = paste0(base,
##         "Embedded News",colnames(resultado_midia)[i],
##         "Bolsonaro.png"), width = 16, height = 8, units = "in", pointsize = 12, bg = "white", res = 100)
## 
## # Plota o gráfico
## plot(V(net)$l2,V(net)$l1,pch=16,
##        col=V(net)$new.color, cex=log(resultado_midia[,i]+1)/3,
##        ylim=c(-130,130),xlim=c(-130,130), xlab="", ylab="",
##        main=colnames(resultado_midia)[i], cex.main=1)
## 
## # Fecha a função de salvar
## dev.off()
##   }
## 
## 


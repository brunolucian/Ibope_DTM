
# Pacotes
require(tidyverse)
require(nnet)
require(lubridate)
require(stringr)
require(tm)
require(gridExtra)
library(ggplot2)
library(ggthemes)

# Lendo arquivo de informacoes municipais
info.mun <- read_csv2("~/Desktop/Ibope_DTM/Ibope_DTM/informacoes_municipais.csv")

## 80% do tamanho da amostra
smp_size <- floor(0.80 * nrow(info.mun))

## fixando seed 
set.seed(123)
train_ind <- sample(seq_len(nrow(info.mun)), size = smp_size)

train <- info.mun[train_ind, ]
test <- info.mun[-train_ind, ]


# Vendo estrutura do arquivo
glimpse(train)

# aplicando scale nos dados
train2 <- scale(train[3:25])

# aplicando conponentes principais
pc <- prcomp(train2)
plot(pc)
plot(pc, type='l')
summary(pc)

# Seis primeiras componentes
comp <- data.frame(pc$x[,1:6])

# Plot
plot(comp, pch=16, col=rgb(0,0,0,0.5))


# Procurando k otimo
wss <- (nrow(comp)-1)*sum(apply(comp,2,var))

for (i in 2:20) wss[i] <- sum(kmeans(comp,
                                     centers=i)$withinss)

plot(1:20, wss, type="b", xlab="Number of Clusters",
     ylab="Soma dos quadrados nos grupos",
     main="Número ótimo de Clusters com Elbow Método",
     pch=20, cex=2)


# Utilizando o kmens com o k encontrado pelo metodo elbow
km = kmeans(comp, 10, nstart=100)

# Quantidade de observacoes por cluster
sort(table(km$clust))

# nomes dos clusters
clust <- names(sort(table(km$clust)))

# Municipios de cada cluster
train[km$clust==clust[1],2]
train[km$clust==clust[2],2]
train[km$clust==clust[3],2]
train[km$clust==clust[4],2]
train[km$clust==clust[5],2]
train[km$clust==clust[6],2]
train[km$clust==clust[7],2]
train[km$clust==clust[8],2]
train[km$clust==clust[9],2]
train[km$clust==clust[10],2]

# Trazendo informacao dos cluster para o DF original
train$km.cluster <-  km$cluster

# Resumo dos dados por cluster
train[3:26] %>%
  group_by(km.cluster) %>% 
  summarise_all(.funs = mean) 

# Aplicando Multilogit para calculo de probabilidade 

mod <- multinom(km.cluster~ ., train[4:26])

# Aplicando o modelo no grupo de teste
head(predict(mod, test[4:25], "probs"))

# Codigos: Rio de Janeiro = 330455 e osasco = 353440
pred.rj <- test[which(test$Código==330455),4:25]
pred.osaco <- test[which(test$Código==353440),4:25]

# predizendo as probabilidades de inclusão em cada grupo para os 2 exemplos
predict(mod, pred.rj, "probs")
predict(mod, pred.osaco, "probs")

######### DADOS TWITTER

# Leitura dos dados
info.tweet <- read_csv2("~/Desktop/Ibope_DTM/Ibope_DTM/informacoes_twitter.csv")

# Formatacao das variaveis
info.tweet$`Data Publicado` <- dmy_hm(info.tweet$`Data Publicado`)
info.tweet$Data <- date(info.tweet$`Data Publicado`)
info.tweet$Hour <- hour(info.tweet$`Data Publicado`)
info.tweet$Minute <- minute(info.tweet$`Data Publicado`)

info.tweet %>% 
  group_by(Data) %>% 
  count() %>% 
  arrange(desc(n))

# regex para extrair os nomes dos usuarios dentro dos posts
regex <- "@([A-Za-z]+[A-Za-z0-9_]+)(?![A-Za-z0-9_]*\\.)"

# extraindo os usuarios dos postas
id.tweet <- str_extract_all(info.tweet$`Corpo Descricao`, regex)

# Contagem dos usuarios mais mencionados nos post
sort(table(unlist(id.tweet)))

# Criando vetor com datas de publicacao dos tweets
dates <- unique(info.tweet$Data)

# Funcao que cria e limpa o corpus para as analises de texto
cria.corpus <- function(DF){
  dfCorpus <- VCorpus(DataframeSource(DF[3]))
  dfCorpus <- dfCorpus %>% 
    tm_map(stripWhitespace) %>% 
    tm_map(content_transformer(tolower)) %>% 
    tm_map(removeWords, stopwords("portuguese")) %>% 
    tm_map(removePunctuation) %>% 
    tm_map(removeNumbers)
}

# Criando graficos em barra das frequencias das palavras
graficos <- list()
for( i in seq_along(dates)){
  dfCorpus <- info.tweet %>% 
    filter(Data == dates[i]) %>% 
    cria.corpus()  
  
  dtm <- DocumentTermMatrix(dfCorpus)
  freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)
  wf <- data.frame(word=names(freq), freq=freq)

graficos[[dates[i]]] <-   subset(wf, freq > freq[15]) %>%
    ggplot(aes(reorder(word,-freq), freq)) +
    geom_bar(stat="identity") +
    theme(axis.text.x=element_text(angle=90)) +
  xlab("tópicos") +
  ylab("")+
  ggtitle(dates[i])

}

# Criando grafico para cada uma dos assuntos
g1 <- grid.arrange(graficos[[dates[7]]], graficos[[dates[6]]], ncol = 1)
g2 <- grid.arrange(graficos[[dates[5]]], graficos[[dates[4]]], graficos[[dates[3]]], ncol = 1) 
g3 <- grid.arrange(graficos[[dates[2]]], graficos[[dates[1]]], ncol = 1)

# Salvando graficos
ggsave("assuntos_t1.png", g1)
ggsave("assuntos_t2.png", g2)
ggsave("assuntos_t3.png", g3)


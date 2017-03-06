

# Pacotes
require(tidyverse)
require(nnet)
require(lubridate)
require(stringr)
require(tm)

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

# Oito primeiras componentes
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

# Examine the result of the clustering algorithm
km

# Quantidade de observacoes por cluster
sort(table(km2$clust))

# nomes dos clusters
clust <- names(sort(table(km2$clust)))

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

mod <- multinom(km.cluster~ ., train[3:26])

# Aplicando o modelo no grupo de teste
head(predict(mod, test[3:25], "probs"))


######### DADOS TWITTER

info.tweet <- read_csv2("~/Desktop/Ibope_DTM/Ibope_DTM/informacoes_twitter.csv")

info.tweet$`Data Publicado` <- dmy_hm(info.tweet$`Data Publicado`)
info.tweet$Data <- date(info.tweet$`Data Publicado`)
info.tweet$Hour <- hour(info.tweet$`Data Publicado`)
info.tweet$Minute <- minute(info.tweet$`Data Publicado`)

regex <- "@([A-Za-z]+[A-Za-z0-9_]+)(?![A-Za-z0-9_]*\\.)"

id.tweet <- str_extract_all(info.tweet$`Corpo Descricao`, regex)
sort(table(unlist(id.tweet)))

DocumentTermMatrix(Corpusinfo.tweet$`Corpo Descricao`))
# str_detect(info.tweet$`Corpo Descricao`, "RT")

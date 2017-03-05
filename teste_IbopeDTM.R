

# Pacotes
require(tidyverse)

# Lendo arquivo de informacoes municipais

info.mun <- read_csv2("~/Desktop/Ibope_DTM/Ibope_DTM/informacoes_municipais.csv")
# Vendo estrutura do arquivo

glimpse(info.mun)


# Criando e selecionando algumas variaveis para o estudo
# 
# info.mun.select <- info.mun %>% 
#                     mutate(razao_ruralurbana = `População rural, 2000`/(`População urbana, 2000`+1),
#                            crescimento25 = `População de 25 anos ou mais de idade, 2000`/`População de 25 anos ou mais de idade, 1991`,
#                            crescimento65 = `População de 65 anos ou mais de idade, 2000`/`População de 65 anos ou mais de idade, 1991`,
#                            Indigencia_pobreza = (`Intensidade da indigência, 2000`+`Intensidade da pobreza, 2000`)/2
#                            ) %>% 
#                     select(-c(`Mortalidade até um ano de idade, 2000`,`Intensidade da indigência, 2000`,`Intensidade da pobreza, 2000`,
#                               `Taxa bruta de freqüência à escola, 2000`), -c(`População de 25 anos ou mais de idade, 1991`:`População rural, 2000`)
#                            )

# Descritivas 

# summary(info.mun.select[3:17])

# aplicando scale nos dados

info.mun2 <- scale(info.mun[3:25])

# aplicando conponentes principais

pc <- princomp(info.mun2)
plot(pc)
plot(pc, type='l')
summary(pc)

# d of princomp
pc <- prcomp(info.mun2)

# Oito primeiras componentes
comp <- data.frame(pc$x[,1:8])
# Plot
plot(comp, pch=16, col=rgb(0,0,0,0.5))


# Procurando k otimo
wss <- (nrow(comp)-1)*sum(apply(comp,2,var))

for (i in 2:35) wss[i] <- sum(kmeans(comp,
                                     centers=i)$withinss)

plot(1:35, wss, type="b", xlab="Number of Clusters",
     ylab="Soma dos quadrados nos grupos",
     main="Número ótimo de Clusters com Elbow Método",
     pch=20, cex=2)
abline(20000, b = 0)


# Utilizando o kmens com o k encontrado pelo metodo elbow

km2 = kmeans(comp, 10, nstart=100)

# Examine the result of the clustering algorithm
km2$cluster

sort(table(km2$clust))

clust <- names(sort(table(km2$clust)))

# First cluster
info.mun[km2$clust==clust[1],]
info.mun[km2$clust==clust[2],]
info.mun[km2$clust==clust[3],]
info.mun[km2$clust==clust[4],]
info.mun[km2$clust==clust[5],]
info.mun[km2$clust==clust[6],]
info.mun[km2$clust==clust[7],]
info.mun[km2$clust==clust[8],]
info.mun[km2$clust==clust[9],]
info.mun[km2$clust==clust[10],]

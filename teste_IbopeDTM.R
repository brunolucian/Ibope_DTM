

# Pacotes
require(tidyverse)

# Lendo arquivo de informacoes municipais

info.mun <- read_csv2("~/Desktop/Ibope_DTM/Ibope_DTM/informacoes_municipais.csv")

# Vendo estrutura do arquivo

glimpse(info.mun)


# Criando e selecionando algumas variaveis para o estudo

info.mun.select <- info.mun %>% 
                    mutate(razao_ruralurbana = `População rural, 2000`/(`População urbana, 2000`+1),
                           crescimento25 = `População de 25 anos ou mais de idade, 2000`/`População de 25 anos ou mais de idade, 1991`,
                           crescimento65 = `População de 65 anos ou mais de idade, 2000`/`População de 65 anos ou mais de idade, 1991`,
                           Indigencia_pobreza = (`Intensidade da indigência, 2000`+`Intensidade da pobreza, 2000`)/2
                           ) %>% 
                    select(-c(`Mortalidade até um ano de idade, 2000`,`Intensidade da indigência, 2000`,`Intensidade da pobreza, 2000`,
                              `Taxa bruta de freqüência à escola, 2000`), -c(`População de 25 anos ou mais de idade, 1991`:`População rural, 2000`)
                           )

# Descritivas 

summary(info.mun.select[3:17])


# Procurando k otimo
dados <- info.mun.select[3:17]
wss <- (nrow(dados)-1)*sum(apply(dados,2,var))

for (i in 2:15) wss[i] <- sum(kmeans(dados,
                                     centers=i)$withinss)

plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Soma dos quadrados nos grupos",
     main="Número ótimo de Clusters com Elbow Método",
     pch=20, cex=2)


# Utilizando o kmens com o k encontrado pelo metodo elbow

km2 = kmeans(dados, 8, nstart=100)

# Examine the result of the clustering algorithm
km2


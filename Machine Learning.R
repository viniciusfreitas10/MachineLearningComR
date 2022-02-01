library('rpart')
library(forecast)

#Arvore de decisão
iris

modelo = rpart(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species, data = iris)
modelo
summary(modelo)
plot(modelo)
text(modelo)

pred = predict(modelo, iris)
head(pred)

compa = cbind(pred, iris$Setal.Length, pred - iris$Sepal.Length)
head(compa)
accuracy(pred, iris$Sepal.Length)

#base de dados
credito = read.csv('C:/Users/Intel/Desktop/Formação Cientista De Dados/Dados/Credit.csv')
#Gerando uma amostra com dados de forma aleatórios  
amostra = sample(2,1000, replace = T, prob = c(0.7,0.3))
#Criando o conjunto de dados como treino
creditotreino <- credito[amostra == 1,]
#Criado o conjunto de dados como teste
creditoteste <- credito[amostra==2,]
#criando arvore
arvore = rpart(class ~ ., 
               data = creditotreino, 
               method = 'class')
#
teste <- predict(arvore, creditoteste)
head(teste)

cred <- cbind(creditoteste, teste)

cred['Result'] = ifelse(cred$bad >= 0.5, 'bad', 'good')
cred

confusao = table(cred$class, cred$Result)

taxa.acerto = (confusao[1] + confusao[4]) / sum(confusao)
taxa.acerto

#Naive Bayes
library(e1071)
dim(credito)

credito$class = as.factor(credito$class)
dim(creditoteste)

modelo <- naiveBayes(class ~ ., creditotreino)
modelo

previsao <- predict(modelo, creditoteste)
previsao  

matriz.conf <- table(creditoteste$class, previsao)
matriz.conf

taxa.ace <- (matriz.conf[1] + matriz.conf[4]) / sum(matriz.conf)
taxa.ace

#Seleção de atributos

library(randomForest)

modelo3 <- svm(class ~., creditotreino)
modelo3
previsao <- predict(modelo3, creditoteste)
previsao
matriz.con <- table(creditoteste$class, previsao)
matriz.con
taxa.acer <- (matriz.con[1] + matriz.con[4]) / sum(matriz.con)
taxa.acer

importancia <- randomForest(class ~., data = creditotreino)
col <- importance(importancia)
col
varImpPlot(importancia)

novomodelo <- svm(class ~ credit_amount + age + duration + checking_status, creditotreino)
prev <- predict(novomodelo, creditoteste)
matriz <- table(creditoteste$class, prev)
matriz
taxa <- (matriz[1] + matriz[4]) / sum(matriz)
taxa

#aprendizaso baseado em instâancia

library(class)
amostra <- sample(2,150, replace = T, prob = c(0.7,0.3))
iristreino <- iris[amostra==1,] 
iristeste <- iris[amostra==2,]
dim(iristeste)
dim(iristreino)

previsao = knn(iristreino[,1:4], iristeste[,1:4], iristreino[,5], k=3)
tabela <- table(iristeste[,5], previsao)
tabela
taxa.acerto <- (tabela[1] + tabela[5] + tabela[9]) / sum(tabela)
taxa.acerto

#Ensable Learning 

library(randomForest)

floresta <- randomForest(class ~ ., data=creditotreino, ntree=100, importance =T)
floresta
previsa <- predict(floresta, creditoteste)

#Codificação de categorias

library(mltools)
library(data.table)
Titanic
Tit <- as.data.frame(Titanic)
Tit
#Transformando os dados categóricos em dados numéricos
labenc <- data.matrix(Tit[,1:3])
laben <- as.data.frame(labenc)
laben
#one hot ecoding (Para cada variável categórica ele cria uma coluna adiconando 0(Não) ou 1(Sim))
hotenco <- one_hot(as.data.table(Tit[,1:3]))
hotenco

#Dimensioanamento de caracteristica
#Padronizaçao

iris_padr <- scale(iris[,1:4])
iris_padr
#normalizaçao 
normaliza <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}
iris_norm <- normaliza(iris[,1:4])
iris_norm
boxplot(iris[,1:4], main = 'Dados Originais')
boxplot(iris_padr[,1:4], main = 'Dados Padronizados')
boxplot(iris_norm, main = 'Dados Normalizados')

#Agrupamento com K-means
library(factoextra)
library(cluster)
cluster = kmeans(iris[1:4], centers = 3)
cluster
confu <- table(iris$Species, cluster$cluster)
taxa <- (confu[1] + confu[6] + confu[8]) / sum(confu)
taxa

plot(iris$Sepal.Length, iris$Sepal.Width, col=cluster$cluster, pch = 20, main = 'iris')
plot(iris[,1:4], col = cluster$cluster, main = 'iris', pch=20)

g2 = fviz_cluster(cluster, data=iris[1:4], ellipse.type = 'convex', ggtheme = theme_bw())
plot(g2)

iris2 = iris
iris2['Grupos'] = cluster$cluster
iris2

#k-medoids

cluster <- pam(iris[1:4], k=3)
plot(cluster)
clu <- table(iris$Species, cluster$clustering)
clu
tx <- (clu[1] + clu[5] + clu[9]) / sum(clu)
tx

fviz_cluster(list(data = iris[,1:4], cluster = cluster$cluster), ellipse.type = 'norm', ggtheme = theme_bw() )

#Fuzzy C-means
library(RColorBrewer)
library(e1071)
cluster <- cmeans(iris[1:4], centers = 3)
cluster$membership

heatmap(cluster$membership, Colv = NA, Rowv = NA)

matriz <- table(iris$Species, cluster$cluster)
matriz

txa.acerto <- (matriz[1]+ matriz[5]+matriz[9]) / sum(matriz)
txa.acerto

fviz_cluster(list(data=iris[1:4], cluster = cluster$cluster), ellipse.type = 'norm', main = 'Iris Cluster Plot')

#cluester hierarquico

hc = hcut(iris[,-5], k=3, hc_method = 'complete', stand = T)
plot(hc)

g1 = fviz_dend(hc, show_labels = F, rect = T)

g2 = fviz_cluster(hc, ellipse.type = 'norm')
g1 ; g2

#Associadores com apriori
library(arules)
library(arulesViz)

transa�oes = read.transactions('C:/Users/roked/Desktop/Dev/Forma��o Cientista De Dados/Dados/transacoes.txt', format = 'basket', sep =',')
plot(transa�oes)

regras = apriori(transa�oes, parameter = list(supp=0.5, conf=0.5, minlen=2))
regras
summary(regras)
inspect(regras)

plot(regras, method = 'graph', control = list(type='items'))
plot(regras, method = 'matrix', control = list(type ='items'))
plot(regras, method = 'matrix3D')

write.csv(DATAFRAME(regras), 'regras.csv')

image(transa�oes)

regras = eclat(transa�oes, parameter = list(minlen=2))
inspect(regras)

plot(regras , method = 'graph', control = list(type='items'))


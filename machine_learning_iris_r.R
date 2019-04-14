#Chamar o famoso dataset Iris já incluso no R
data("iris")
attach(iris)  #Botar um attach pra não ter que digitar iris antes dos campos toda hora


#Dar um check nos dados
str(iris)   #Semelhante ao DESC do Oracle ou SP_HELP no SQlSErver
#Sepal.Length - comprimento da sepala
#Sepal.Width - largura da sepala
#Petal.Length - comprimento da petala
#Petal.Width - largura da petala
summary(iris) #ver as medias min max dados null etc
boxplot(iris[-5]) #ver os outliers e amplitudes de variação iris[-5]já tirava a coluna 5 Species

# Apos compreender os dados e hora de pensar como eles podem ensinar o que podemos aprender com ele
# A partir dai, determina-se qual o melhor metodo ou algoritimo a ser utilizado na analise
# em busca daquilo que queremos
# Para este caso utilizaremos o metodo de  classificao que faz parte da modelagem preditiva.

#Agora vamos dividir o dataset em 2 dataset de treino e o dataset de teste

#Criando numeros aleatorios
set.seed(123)

#chama o pacote catools que contem o algoritmo e a funcao split pra separar os dados
#Criando uma coluna chamado it_train que vai levar TRUE dividindo em 70% train e 30% test
library(caTools)
library(randomForest)
library(randomForestExplainer)

#70% da coluna que criamos vai estar como TRUE indicando que ela é de treino
iris$is_train <- sample.split(iris$Species, SplitRatio = 0.7) 

#Checar se a coluna foi criada
View(iris)

#Agora chegou a hora de treinar e testar
dados_treino <- subset(iris[-6], iris$is_train == T) #Train = TRUE
dados_teste <- subset(iris[-6], iris$is_train == F) #Train = FALSE

#Criar modelo colocando a variável Species contra todas as demais dos dados de treino com 50 arvores
set.seed(1234)
iris_random_f <- randomForest(Species ~., data = dados_treino, ntree=100, importance=T)

#Chama os resultados
iris_random_f

#Plota pra ver o ponto de estabilizacao
plot(iris_random_f)


#Explicando Iris
explain_forest(iris_random_f)


#Plot simples de  variaveis mais importantes
varImpPlot(iris_random_f)

#Fazer a predicao...note que usamos o modelo com dados que treinamos
#e adicionamos um novo dado  pra ver se aprendemos
iris_predict <- predict(iris_random_f, newdata = dados_teste)

#Criar uma coluna predicao com os resultados pra comparar com os dados que ja temos
dados_teste$predicao <- iris_predict

#Visualiza a tabela, Species e o dado real e predicao e o dado que o modelo sugeriu
View(dados_teste)

















###Fim










#Montando a table
table(iris_predict, dados_teste$Species)


#plotando a margem
plot(margin(iris_random_f, dados_teste$Species))


#Tunar o RF
tune_random_f <- tuneRF(iris[,-5], iris[,5], stepFactor = 0.5)




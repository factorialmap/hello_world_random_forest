#Objetivos    ----
#1. Como a partir de algumas característica representadas aqui como medidas eu consigo identificar qual a especie da planta?
#2. Temos 3 espécis setosa, virginica e versicolor

#pacotes
library(tidyverse) #este nos ajuda no manuseio dos dados
library(GGally) #mostras características atraves de um gráfico
library(tidymodels) #este nos ajudará a construir o modelo
library(randomForest) #as formulas do modelo estao neste
library(randomForestExplainer) #se eu quiser explicar as decisões do modelo

#dados        ----
data("iris") #O conjunto de dados é nativo do R então basta chama-lo com a função data

#Explorar dados (medianas, correlacao e distribuicao)
iris %>% 
  ggpairs(mapping = ggplot2::aes(colour = Species)) #mostra cada especie em uma cor

#tradução dos dados para portugues
#Sepal.Length  = comprimento da sepala
#Sepal.Width   = largura da sepala
#Petal.Length  = comprimento da petala
#Petal.Width   = largura da petala

#Estatística descritiva
summary(iris) 

# Apos compreender os dados e hora de pensar como eles podem ensinar e o que podemos aprender com ele
# Temoos 4 variáveis preditoras numericas e 1 resposta categorica, logo o modelo deve ser de classificação


#modelo       ----

#1º passo é dividir o conjunto em 2 uma partes pra fazer o modelo e outro pra testa-lo

#70% pra treino e 30 pra teste
iris_split <- initial_split(iris, prop = 0.7)

iris_train <- training(iris_split)
iris_test  <- testing(iris_split)

#Criar modelo colocando a variável Species contra todas as demais dos dados de treino com 50 arvores
set.seed(1234)
modelo_rf <- 
  randomForest(Species ~.,
               data = iris_train,
               ntree=100,
               mtry = 4,
               importance=TRUE)

#Chama os resultados
modelo_rf  #ja gera a matriz de confucao e o erro aproximado

#Plota pra ver o ponto de estabilizacao
plot(modelo_rf)  #a partir de 20 arvores o erro comeca a diminuir

#Ele gera um arquivo de explicacao e salva ali do lado direito em "file"
explain_forest(modelo_rf)

#Plot simples de  variaveis mais importantes
varImpPlot(modelo_rf)

#predicao     ----

#Usamos o modelo treinado + um dado novo, neste caso os dados de teste
minha_previsao <- predict(modelo_rf, newdata = iris_test)

#Criar uma coluna predicao com os resultados pra comparar com os dados que ja temos
iris_test <- 
  iris_test %>% 
  mutate(dados_originais = Species,
         dados_previstos = minha_previsao)

#Matriz de confusao na base de teste
table(minha_previsao, iris_test$Species) #nosso modelo errou 2 previsoes em virginica

#plotando a margem
plot(margin(modelo_rf, dados_teste$Species))

#Se eu quiser checar os parametros mtry pra melhorar o modelo
tuneRF(iris[,-5], iris[,5], stepFactor = 0.5) #4 é o melhor valor




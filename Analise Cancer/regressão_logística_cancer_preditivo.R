# Leitura da base de dados
base = read.csv('cancer.csv')

# Apaga a coluna clientid
base$ID = NULL

# Valores inconsistentes substituir pela media
base$area = ifelse(base$area < 0, 40.92, base$area)

# Valores faltantes
base$area = ifelse(is.na(base$area), mean(base$area, na.rm = TRUE), base$area)

# Escalonamento
base[, 1:3] = scale(base[, 1:3])

# Divisao entre treinamento e teste
library(caTools)
set.seed(1)
divisao = sample.split(base$diagnosis, SplitRatio = 0.80)
base_treinamento = subset(base, divisao == TRUE)
base_teste = subset(base, divisao == FALSE)

#classificador: rgressão logistica preditivo

classificador = glm(formula = diagnosis ~ ., family = binomial, data = base_treinamento)
probabilidades = predict(classificador, type = 'response', newdata = base_teste[-4])
previsoes = ifelse(probabilidades > 0.5, 1, 0)
matriz_confusao = table(base_teste[, 4], previsoes)
library(caret)
confusionMatrix(matriz_confusao)


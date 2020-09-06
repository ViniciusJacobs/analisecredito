#Etapa 3: Elaborar modelo preditivo para o modelo

library(caret)
library(randomForest)


## Normalização
scale.features <- function(df, variables){
  for (variable in variables){
    df[[variable]] <- scale(df[[variable]], center=T, scale=T)
  }
  return(df)
}

#retirada do status e do indef
df_credito_ajustado_previ <- df_credito_ajustado %>%
  relocate(status_bin, .before = status) %>%
  select(-status) %>%
  filter(estado_civil != "indefinido")
glimpse(df_credito_ajustado)

df_credito_ajustado_previ %>%
  group_by(estado_civil) %>%
  count()

# Normalizando as variáveis
numeric.vars <- c("tempo_empresa", "tempo_emprestimo", "idade",
                  "despesas","renda","ativos","dividas", "valor_emprestimo","preco_do_bem")
df_credito_ajustado_previ <- scale.features(df_credito_ajustado_previ, numeric.vars)

glimpse(df_credito_ajustado)

# Variáveis do tipo fator
categorical.vars <- c('status_bin', 'faixa_tempo_empresa', 'faixa_etaria',
                      'estado_civil', 'trabalho', "moradia","registros")

df_credito_ajustado_previ <- to.factors(df = df_credito_ajustado_previ, variables = categorical.vars)


glimpse(df_credito_ajustado_previ)

# Dividindo os dados em treino e teste - 60:40
indexes <- sample(1:nrow(df_credito_ajustado_previ), size = 0.6 * nrow(df_credito_ajustado_previ))
train.data <- df_credito_ajustado_previ[indexes,]
test.data <- df_credito_ajustado_previ[-indexes,]

rfe.results <- run.feature.selection(feature.vars = train.data[,-1],
                                     class.var = train.data[,1])

# Visualizando os resultados
rfe.results
varImp((rfe.results))


library(caret)
library(ROCR)


## separate feature and class variables
test.feature.vars <- test.data[,-1]
test.class.var <- test.data[,1]


# Construindo um modelo de regressão logística
formula.init <- "status_bin ~ ."
formula.init <- as.formula(formula.init)
lr.model <- glm(formula = formula.init, data = train.data, family = "binomial")

# Visualizando o modelo
summary(lr.model)



# Testando o modelo nos dados de teste
lr.predictions <- predict(lr.model, test.data, type="response")
lr.predictions <- round(lr.predictions)

confusionMatrix(table(data = lr.predictions, reference = test.class.var), positive = '1')


## Feature selection
formula <- "status_bin ~ ."
formula <- as.formula(formula)
control <- trainControl(method = "repeatedcv", number = 10, repeats = 2)
model <- train(formula, data = train.data, method = "glm", trControl = control)
importance <- varImp(model, scale = FALSE)
plot(importance)

# Construindo o modelo com as variáveis selecionadas
formula.new <- "status_bin ~ registros + valor_emprestimo + renda + trabalho + preco_do_bem + moradia + estado_civil + ativos + despesas + dividas + tempo_empresa"
formula.new <- as.formula(formula.new)
lr.model.new <- glm(formula = formula.new, data = train.data, family = "binomial")
glimpse(df_credito_ajustado_previ)

summary(lr.model.new)

# Testando o modelo nos dados de teste
lr.predictions.new <- predict(lr.model.new, test.data, type = "response")
lr.predictions.new <- round(lr.predictions.new)

# Avaliando o modelo
confusionMatrix(table(data = lr.predictions.new, reference = test.class.var), positive = '1')

# Criando curvas ROC
lr.model.best <- lr.model
lr.prediction.values <- predict(lr.model.best, test.feature.vars, type = "response")
predictions <- prediction(lr.prediction.values, test.class.var)
par(mfrow = c(1,2))
plot.roc.curve(predictions, title.text = "Curva ROC")
plot.pr.curve(predictions, title.text = "Curva Precision/Recall")

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

df_credito_ajustado_previ <- df_credito_ajustado %>%
  relocate(status_bin, .before = status) %>%
  select(-status)
glimpse(df_credito_ajustado)

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




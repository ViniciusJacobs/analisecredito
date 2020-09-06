
##Pacotes utilizados
library(tidyverse)
devtools::load_all()


##Leitura do df

df_credito <- read_rds("data/credito.rds")
glimpse(df_credito)

# #leitura de tabua de mortalidade
# tabua_mortalidade <- read_delim("data/tabua_mortalidade.csv",
#                                 ";",locale = locale(encoding = "ASCII"))

# #join da tabua
# df_credito_ajustado <- left_join(df_credito_ajustado, tabua_mortalidade)
#
# df_credito_ajustado <- df_credito_ajustado %>%
#   relocate(idade, .before = qx)




# left_join(df_credito_ajustado,tabua_mortalidade)

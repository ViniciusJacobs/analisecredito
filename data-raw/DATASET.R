## code to prepare `DATASET` dataset goes here

usethis::use_data(DATASET, overwrite = TRUE)

##Pacotes utilizados
library(tidyverse)
library(purrr)
devtools::load_all()


##Leitura do df

df_credito <- read_rds("data/credito.rds")
View(df_credito)
glimpse(df_credito)

#leitura de tabua de mortalidade
tabua_mortalidade <- read_delim("data/tabua_mortalidade.csv",
                                ";",locale = locale(encoding = "ASCII"))

tabua_mortalidade <- rename(tabua_mortalidade, idade = Idade_Tabua_AT2000)



#titulos col
titulos_col<- df_credito %>%
  names
#identificando os NA's
for(i in 1:length(titulos_col)){
 x <- contagem_nas(df_credito, titulos_col[i])[[1]]
  if(x>0){
   print(titulos_col[i])
   print(x)
 }
}

#DF AJUSTADO PARA O MODELO
df_credito_ajustado <- df_credito%>%
  tidyr::replace_na(replace = list(moradia = "indefinido",
                                   estado_civil = "indefinido",
                                   trabalho = "indefinido"))


# VISUALMENTE(graficos) E PELA BAIXA CORRELACAO
#FOI VERIFICADO QUE AS VARIAVEIS RENDA,
#ATIVOS E DIVIDAS SAO POUCO EXPLICATIVAS
#PARA O MODELO

df_credito_ajustado <- df_credito_ajustado %>%
  tidyr::replace_na(replace = list(renda = mean(df_credito_ajustado$renda, na.rm = TRUE),
                                   ativos = mean(df_credito_ajustado$ativos, na.rm = TRUE),
                                    dividas = mean(df_credito_ajustado$dividas, na.rm = TRUE)))
df_credito_ajustado <- df_credito_ajustado %>%
  mutate(faixa_etária = case_when(idade<=25 ~ "(Abaixo dos 25 anos)",
                                  idade<=35 ~ "(Entre 26 e 35 anos",
                                  idade<=45 ~ "Entre 36 e 45 anos",
                                  idade<=55 ~ "Entre 46 e 55 anos",
                                  idade<85 ~"Acima dos 55 anos")) %>%
  relocate(faixa_etária, .after = idade)

#join da tabua
df_credito_ajustado <- left_join(df_credito_ajustado, tabua_mortalidade)

df_credito_ajustado <- df_credito_ajustado %>%
  relocate(idade, .before = qx)

View(df_credito_ajustado)

















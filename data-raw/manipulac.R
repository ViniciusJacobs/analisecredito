## code to prepare `DATASET` dataset goes here

usethis::use_data(DATASET, overwrite = TRUE)

##Pacotes utilizados

library(tidyverse)


devtools::load_all()


##Leitura do df

df_credito <- read_rds("data/credito.rds")

glimpse(df_credito)

# #leitura de tabua de mortalidade
# tabua_mortalidade <- read_delim("data/tabua_mortalidade.csv",
#                                 ";",locale = locale(encoding = "ASCII"))




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
  mutate(status_bin = case_when(status == "bom" ~ 1,
                                status == "ruim" ~ 0)) %>%
  relocate(status_bin, .after = status)

#inclusao de colunas

df_credito_ajustado <- df_credito_ajustado %>%
  mutate(faixa_etaria = case_when(idade<=25 ~ "1.Abaixo dos 25 anos",
                                  idade<=35 ~ "2.Entre 26 e 35 anos",
                                  idade<=45 ~ "3.Entre 36 e 45 anos",
                                  idade<=55 ~ "4.Entre 46 e 55 anos",
                                  idade>55 ~"5.Acima dos 55 anos")) %>%
  relocate(faixa_etaria, .after = idade)

View(df_credito_ajustado)

tab_faixa_etaria <- df_credito_ajustado %>%
  group_by(faixa_etaria, status) %>%
  summarise(contagem = n()) %>%
  arrange(desc(faixa_etaria)) %>%
  spread(status, contagem) %>%
  mutate(prop.bons.ruins = bom/ruim)

ggplot(tab_faixa_etaria) +
  aes(x = faixa_etaria, weight = prop.bons.ruins) +
  geom_bar(fill = "#0c4c8a") +
  coord_flip()+
  labs( title = "Bons Pagadores: (+) Tempo de Empresa ",
        x = "Tempo de Empresa",
        y = "Proporção: Bons/Ruins")+
  theme_gray()+
  theme(
    legend.position = "right",
    plot.title = element_text(
      hjust = 0.5
    ))


tab_estado_civ <- df_credito_ajustado %>%
  group_by(estado_civil, status) %>%
  summarise(contagem = n()) %>%
  arrange(desc(estado_civil)) %>%
  spread(status, contagem) %>%
  mutate(prop.bons.ruins = bom/ruim)

#titulos col
titulos_col<- df_credito_ajustado %>%
  names
#identificando os NA's

for(i in 1:length(titulos_col)){
  x <- contagem_nas(df_credito_ajustado, titulos_col[i])[[1]]
  if(x>0){
    print(titulos_col[i])
    print(x)
  }








df_credito_ajustado <- df_credito_ajustado %>%
  mutate(faixa_tempo_empresa = case_when(tempo_empresa<=2 ~ "1. Abaixo dos 2 anos",
                                  tempo_empresa <= 4 ~ "2. Entre 3 e 4 anos",
                                  tempo_empresa <= 6 ~ "3. Entre 5 e 6 anos",
                                  tempo_empresa <= 8 ~ "4. Entre 7 e 8 anos",
                                  tempo_empresa <= 10 ~ "5. Entre 9 e 10 anos",
                                  tempo_empresa<=15 ~ "6. Entre 11 e 15 anos",
                                  tempo_empresa<=20 ~ "7. Entre 16 e 20 anos",
                                  tempo_empresa<=25 ~ "8. Entre 21 e 25 anos",
                                  tempo_empresa<=36 ~ "9. Entre 26 e 35 anos",
                                  tempo_empresa>36 ~"Acima dos 36 anos")) %>%
  relocate(faixa_tempo_empresa, .after = tempo_empresa)


 tab_faixa_tempoempre <-  df_credito_ajustado %>%
    group_by(faixa_tempo_empresa, status) %>%
    summarise(contagem = n()) %>%
    arrange(desc(faixa_tempo_empresa)) %>%
    spread(status, contagem) %>%
    mutate(prop.bons.ruins = bom/ruim) %>%
    View

 ggplot(tab_faixa_tempoempre) +
   aes(x = faixa_tempo_empresa, weight = prop.bons.ruins) +
   geom_bar(fill = "#0c4c8a") +
   coord_flip()+
   labs( title = "Bons Pagadores: (+) Tempo de Empresa ",
         x = "Tempo de Empresa",
         y = "Proporção: Bons/Ruins")+
   theme_gray()+
   theme(
     legend.position = "right",
     plot.title = element_text(
       hjust = 0.5
     ))






# #join da tabua
# df_credito_ajustado <- left_join(df_credito_ajustado, tabua_mortalidade)
#
# df_credito_ajustado <- df_credito_ajustado %>%
#   relocate(idade, .before = qx)




# left_join(df_credito_ajustado,tabua_mortalidade)






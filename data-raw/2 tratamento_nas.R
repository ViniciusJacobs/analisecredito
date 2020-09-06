
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


glimpse(df_credito_ajustado)


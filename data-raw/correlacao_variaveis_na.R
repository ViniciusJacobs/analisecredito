#substituindo os NA's e criando novo df

#medindo a correlação entre as variaveis que
#poderiam apresentar problemas nos NAS

cor_renda <- df_credito %>%
  mutate(status_num = if_else(status == "bom", 1,0)) %>%
  filter(!is.na(renda))

cor(x = cor_renda$status_num, y = cor_renda$renda)

cor_dividas <- df_credito %>%
  mutate(status_num = if_else(status == "bom", 1,0)) %>%
  filter(!is.na(dividas))

cor(x = cor_ativos$status_num, y = cor_dividas$dividas)

cor_ativos <- df_credito %>%
  mutate(status_num = if_else(status == "bom", 1,0)) %>%
  filter(!is.na(ativos))

cor(x = cor_ativos$status_num, y = cor_ativos$dividas)




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



tab_faixa_etaria <- df_credito_ajustado %>%
  group_by(faixa_etaria, status) %>%
  summarise(contagem = n()) %>%
  arrange(desc(faixa_etaria)) %>%
  spread(status, contagem) %>%
  mutate(prop.bons.ruins = bom/ruim)


tab_estado_civ <- df_credito_ajustado %>%
  group_by(estado_civil, status) %>%
  summarise(contagem = n()) %>%
  arrange(desc(estado_civil)) %>%
  spread(status, contagem) %>%
  mutate(prop.bons.ruins = bom/ruim) %>%
  filter(!is.na(prop.bons.ruins)) %>%
  arrange(prop.bons.ruins)



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
    mutate(prop.bons.ruins = bom/ruim)



#Graficos

#graficos do perfil demográfico e financeiro dos clientes

grafico_status <- ggplot()+
  aes(x = df_credito_ajustado$status)+
  geom_bar(width = 0.4, fill = "#4682B4")+
  labs( title = "Distribuição por perfil: Bom x Ruim",
        x = "Perfil",
        y = "Quantidade")+
  theme_gray()+
  theme(
    legend.position = "right",
    plot.title = element_text(
      hjust = 0.5
    )
  )

grafico_faixa_etaria <- ggplot()+
  aes(x = df_credito_ajustado$faixa_etária)+
  geom_bar(width = 0.7, fill = "#4682B4")+
  coord_flip()+
  labs( title = "Perfil Etário",
        x = "Faixa Etária",
        y = "Quantidade")+
  theme_gray()+
  theme(
    legend.position = "right",
    plot.title = element_text(
      hjust = 0.5
    ))







##Graficos utilizados pra auxiliar na retirada dos NAS

grafico_rel_renda <- df_credito_ajustado %>%
  filter(!is.na(renda)) %>%
  ggplot() +
  aes(x = status, y = log(renda)) +
  geom_boxplot(fill = "#4682B4")+
  labs( title = "Relação: Renda x Status",
        x = "Status",
        y = "Renda")+
  theme_gray()+
  theme(
    legend.position = "right",
    plot.title = element_text(
      hjust = 0.5
    )
  )



#----------------------------------------------------------------------

grafico_rel_ativos <- df_credito_ajustado %>%
  filter(!is.na(ativos)) %>%
  ggplot() +
  aes(x = status, y = log(ativos)) +
  geom_boxplot(fill = "#4682B4")+
  labs( title = "Relação: Ativos x Status",
        x = "Status",
        y = "Ativos")+
  theme_gray()+
  theme(
    legend.position = "right",
    plot.title = element_text(
      hjust = 0.5
    )
  )
#------------------------------------------------------------------------
grafico_rel_dividas <- df_credito_ajustado %>%
  filter(!is.na(dividas)) %>%
  ggplot() +
  aes(x = status, y = (dividas)) +
  geom_boxplot(fill = "#4682B4")+
  labs( title = "Relação: Ativos x Status",
        x = "Status",
        y = "Dividas")+
  theme_gray()+
  theme(
    legend.position = "right",
    plot.title = element_text(
      hjust = 0.5
    )
  )


#----------------------------------------------------------------------



#Graficos

#graficos do perfil demográfico e financeiro dos clientes

dist_por_perfil <-grafico_status <- ggplot()+
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

dist_por_faixa_eta <- grafico_faixa_etaria <- ggplot()+
  aes(x = df_credito_ajustado$faixa_etaria)+
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

grafico_estado_civil <- ggplot()+
  aes(x = df_credito_ajustado$estado_civil)+
  geom_bar(width = 0.4, fill = "#4682B4")+
  labs( title = "Estado Civil",
        x = "Situação",
        y = "Quantidade")+
  theme_gray()+
  theme(
    axis.text.x = element_text(angle=45, hjust=1),
    legend.position = "right",
    plot.title = element_text(
      hjust = 0.5
    )
  )
grafico_estado_civil


###Grafico USAR


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


# df_credito_ajustado %>%
#   group_by(estado_civil) %>%
#   count() %>%
#   ggplot(aes(x="", y=n, fill=estado_civil)) +
#   geom_bar(stat="identity", width=1) +
#   coord_polar("y", start=0)+
#   ggtitle("TESTE")


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



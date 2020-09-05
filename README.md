
# Objetivo

Realizar uma analise de credito na base x.

# Plot

````{r}
ggplot()+
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


````


![](README_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->


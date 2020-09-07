
# Análise de Perfil: Concessão de Crédito

## Objetivo

  - Extrair informações da base fornecida;  
  - Apresentar as caracteristicas demograficas e financeiras dos
    clientes presentes na base de dados;
  - Identificar entre as variáveis do modelo, quais explicam um cliente
    ser bom ou ruim.
  - Elaborar um modelo de previsão para o problema de negócio.

## Pacotes utilizados

``` r
library(tidyverse)
library(tidyr)
library(patchwork)
library(caret)
library(randomForest)
library(ROCR)
devtools::load_all()
```

<font size="5">Etapa 1: Análise Exploratória dos Dados</font>

**Leitura do data.frame e visualização das variáveis**

``` r
df_credito <- read_rds("data/credito.rds")
glimpse(df_credito)
```

    ## Rows: 4,454
    ## Columns: 14
    ## $ status           <chr> "bom", "bom", "ruim", "bom", "bom", "bom", "bom", ...
    ## $ tempo_empresa    <int> 9, 17, 10, 0, 0, 1, 29, 9, 0, 0, 6, 7, 8, 19, 0, 0...
    ## $ moradia          <chr> "alugada", "alugada", "própria", "alugada", "aluga...
    ## $ tempo_emprestimo <int> 60, 60, 36, 60, 36, 60, 60, 12, 60, 48, 48, 36, 60...
    ## $ idade            <int> 30, 58, 46, 24, 26, 36, 44, 27, 32, 41, 34, 29, 30...
    ## $ estado_civil     <chr> "casada(o)", "viúva(o)", "casada(o)", "solteira(o)...
    ## $ registros        <chr> "não", "não", "sim", "não", "não", "não", "não", "...
    ## $ trabalho         <chr> "autônomo", "fixo", "autônomo", "fixo", "fixo", "f...
    ## $ despesas         <int> 73, 48, 90, 63, 46, 75, 75, 35, 90, 90, 60, 60, 75...
    ## $ renda            <int> 129, 131, 200, 182, 107, 214, 125, 80, 107, 80, 12...
    ## $ ativos           <int> 0, 0, 3000, 2500, 0, 3500, 10000, 0, 15000, 0, 400...
    ## $ dividas          <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2500, 260, 0, ...
    ## $ valor_emprestimo <int> 800, 1000, 2000, 900, 310, 650, 1600, 200, 1200, 1...
    ## $ preco_do_bem     <int> 846, 1658, 2985, 1325, 910, 1645, 1800, 1093, 1957...

**Identificar valores NA’s no modelo e formas tratalos**

``` r
titulos_col <- df_credito %>%
  names

for(i in 1:length(titulos_col)){
 x <- contagem_nas(df_credito, titulos_col[i])[[1]]
  if(x>0){
   print(titulos_col[i])
   print(x)
  }

}
```

    ## [1] "moradia"
    ## [1] 26
    ## [1] "estado_civil"
    ## [1] 1
    ## [1] "trabalho"
    ## [1] 2
    ## [1] "renda"
    ## [1] 381
    ## [1] "ativos"
    ## [1] 47
    ## [1] "dividas"
    ## [1] 18

<font size="4">O que fazer com os NA’s do modelo? Eles estão em
variáveis que explicam a variável alvo? São muitos os NA’s?</font>

**As variáveis moradia, estado\_civil e trabalho, apresentaram um número
de NA’s pouco expressivo (menos de 1% do número de obs.), a situação NA
foi considerada como ERRO no cadastro. Para esse estudo não foi feita
análise, mas sim a alteração dos campos NA’s para “indefinido”.**

``` r
df_credito_ajustado <- df_credito%>%
  tidyr::replace_na(replace = list(moradia = "indefinido",
                                   estado_civil = "indefinido",
                                   trabalho = "indefinido"))
```

**As variáveis renda, ativos e dívidas, foram objeto de análise,
gráfica, conforme abaixo:**

``` r
grafico_rel_renda + grafico_rel_dividas + grafico_rel_ativos 
```

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

**Retirada dos NA’s**

**A análise visual dos dados, nos mostra que ambas variáveis tem
distribuição próxima a média para o status, “Bom” ou “Ruim”.**

**Optei pela retirada dos NA’s e inclusão da média para variável.**

``` r
  df_credito_ajustado <- df_credito_ajustado %>%
  replace_na(replace = list(renda = mean(df_credito_ajustado$renda, na.rm = TRUE),
                            ativos = mean(df_credito_ajustado$ativos, na.rm = TRUE),
                            dividas = mean(df_credito_ajustado$dividas, na.rm = TRUE)))
```

<font size="5">Etapa 2: Caracteristicas demograficas e financeiras dos
clientes presentes na base de dados.</font>

**Na etapa inicial, foi realizada analise gráfica e o mapeamento de
algumas variáveis, conforme abaixo:**

**Buscando compreender um pouco mais sobre quem foram meus clientes no
passado, montei gráficos para IDADE(por faixa etária), tempo de empresa
(também por faixas) e distribuição de bons e ruins clientes.**

``` r
dist_por_perfil + grafico_estado_civil + dist_por_faixa_eta
```

![](README_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

**Por fim, quiz checar dentre essas variáveis qual a proporção entre os
clientes Bons e Ruins**

``` r
df_credito_ajustado <- df_credito_ajustado %>%
  mutate(faixa_etaria = case_when(idade<=25 ~ "1.Abaixo dos 25 anos",
                                  idade<=35 ~ "2.Entre 26 e 35 anos",
                                  idade<=45 ~ "3.Entre 36 e 45 anos",
                                  idade<=55 ~ "4.Entre 46 e 55 anos",
                                  idade>55 ~"5.Acima dos 55 anos")) %>%
  relocate(faixa_etaria, .after = idade)

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
```

``` r
tab_faixa_tempoempre
```

    ## # A tibble: 10 x 4
    ## # Groups:   faixa_tempo_empresa [10]
    ##    faixa_tempo_empresa     bom  ruim prop.bons.ruins
    ##    <chr>                 <int> <int>           <dbl>
    ##  1 1. Abaixo dos 2 anos    815   684            1.19
    ##  2 2. Entre 3 e 4 anos     411   158            2.60
    ##  3 3. Entre 5 e 6 anos     332   115            2.89
    ##  4 4. Entre 7 e 8 anos     239    61            3.92
    ##  5 5. Entre 9 e 10 anos    250    65            3.85
    ##  6 6. Entre 11 e 15 anos   502   100            5.02
    ##  7 7. Entre 16 e 20 anos   338    35            9.66
    ##  8 8. Entre 21 e 25 anos   154    17            9.06
    ##  9 9. Entre 26 e 35 anos   128    17            7.53
    ## 10 Acima dos 36 anos        31     2           15.5

``` r
tab_faixa_etaria 
```

    ## # A tibble: 5 x 4
    ## # Groups:   faixa_etaria [5]
    ##   faixa_etaria           bom  ruim prop.bons.ruins
    ##   <chr>                <int> <int>           <dbl>
    ## 1 1.Abaixo dos 25 anos   442   260            1.7 
    ## 2 2.Entre 26 e 35 anos  1101   408            2.70
    ## 3 3.Entre 36 e 45 anos   813   362            2.25
    ## 4 4.Entre 46 e 55 anos   608   167            3.64
    ## 5 5.Acima dos 55 anos    236    57            4.14

``` r
prop.faixa.et 
```

![](README_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

``` r
prop.tempo.emp
```

![](README_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

``` r
prop.est.civi 
```

![](README_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

<font size="5">3: Elaborar modelo preditivo para o modelo</font>

**Quando iniciei essa etapa a primeira coisa que percebi é que a
limpeza/tratamento que havia dado ao modelo no início do projeto não
eram suficientes para agora eu trabalhar com um modelo de regressão
logística. Então mais um poquinho de tratamento aos dados…**

``` r
# Normalizando as variáveis

numeric.vars <- c("tempo_empresa", "tempo_emprestimo", "idade",
                  "despesas","renda","ativos","dividas", "valor_emprestimo","preco_do_bem")


df_credito_ajustado_previ <- scale.features(df_credito_ajustado_previ, numeric.vars)

#Variáveis tipo Fator
categorical.vars <- c('status_bin', 'faixa_tempo_empresa', 'faixa_etaria',
                      'estado_civil', 'trabalho', "moradia","registros")

df_credito_ajustado_previ <- to.factors(df = df_credito_ajustado_previ, variables = categorical.vars)


glimpse(df_credito_ajustado_previ)
```

    ## Rows: 4,453
    ## Columns: 16
    ## $ status_bin          <fct> 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1,...
    ## $ tempo_empresa       <dbl[,1]> <matrix[26 x 1]>
    ## $ faixa_tempo_empresa <fct> 5. Entre 9 e 10 anos, 7. Entre 16 e 20 anos, 5....
    ## $ moradia             <fct> alugada, alugada, própria, alugada, alugada, pr...
    ## $ tempo_emprestimo    <dbl[,1]> <matrix[26 x 1]>
    ## $ idade               <dbl[,1]> <matrix[26 x 1]>
    ## $ faixa_etaria        <fct> 2.Entre 26 e 35 anos, 5.Acima dos 55 anos, 4.En...
    ## $ estado_civil        <fct> casada(o), viúva(o), casada(o), solteira(o), so...
    ## $ registros           <fct> não, não, sim, não, não, não, não, não, não, nã...
    ## $ trabalho            <fct> autônomo, fixo, autônomo, fixo, fixo, fixo, fix...
    ## $ despesas            <dbl[,1]> <matrix[26 x 1]>
    ## $ renda               <dbl[,1]> <matrix[26 x 1]>
    ## $ ativos              <dbl[,1]> <matrix[26 x 1]>
    ## $ dividas             <dbl[,1]> <matrix[26 x 1]>
    ## $ valor_emprestimo    <dbl[,1]> <matrix[26 x 1]>
    ## $ preco_do_bem        <dbl[,1]> <matrix[26 x 1]>

**Divisão do data.frame em treino e teste**

``` r
#60:40
indexes <- sample(1:nrow(df_credito_ajustado_previ), size = 0.6 * nrow(df_credito_ajustado_previ))
train.data <- df_credito_ajustado_previ[indexes,]
test.data <- df_credito_ajustado_previ[-indexes,]
```

**Avaliando as variáveis que mais explicam o modelo**

``` r
rfe.results <- run.feature.selection(feature.vars = train.data[,-1],
                                 class.var = train.data[,1])

#Visualizando os resultados
rfe.results
```

    ## 
    ## Recursive feature selection
    ## 
    ## Outer resampling method: Cross-Validated (10 fold) 
    ## 
    ## Resampling performance over subset size:
    ## 
    ##  Variables Accuracy  Kappa AccuracySD KappaSD Selected
    ##          1   0.7260 0.2426    0.02344 0.06873         
    ##          2   0.7428 0.2634    0.02112 0.05977         
    ##          3   0.7589 0.3272    0.01577 0.04601         
    ##          4   0.7473 0.3175    0.02183 0.05704         
    ##          5   0.7473 0.3282    0.02554 0.06295         
    ##          6   0.7712 0.3928    0.02267 0.05521         
    ##          7   0.7694 0.3871    0.02239 0.05993         
    ##          8   0.7754 0.3950    0.01692 0.05131         
    ##          9   0.7675 0.3869    0.01857 0.04428         
    ##         10   0.7780 0.4064    0.01735 0.04673         
    ##         15   0.7859 0.4176    0.02207 0.05996        *
    ## 
    ## The top 5 variables (out of 15):
    ##    registros, trabalho, tempo_empresa, renda, valor_emprestimo

``` r
varImp((rfe.results))
```

    ##                       Overall
    ## registros           26.487563
    ## trabalho            20.211716
    ## tempo_empresa       18.216975
    ## renda               15.723181
    ## valor_emprestimo    15.624544
    ## ativos              13.888626
    ## moradia             12.054990
    ## faixa_tempo_empresa 11.816381
    ## despesas             8.094701
    ## idade                7.199649
    ## preco_do_bem         6.797639
    ## tempo_emprestimo     5.830558
    ## estado_civil         3.519626
    ## faixa_etaria         2.837387
    ## dividas              1.959206

**As variáveis que apresentaram maior relação explicativa a relação bom
ou ruim pagador (conforme esse modelo) são:**

**-registros**

**-trabalho**

**-tempo\_empresa**

**-renda**

**-valor-emprestimo**

**Na análise descritiva realizada no início do estudo foram avaliadas
faixa etária, tempo de trabalho na empresa e estado civil. Nota-se, que
somente o tempo de empresa dentre as variáveis, consta na lista das 5
mais explicativas.**

**Isso reforça a ideia de que devemos trabalhar com dados**

**Nessa próxima etapa, construi o modelo inicial**

``` r
## Separando as variáveis
test.feature.vars <- test.data[,-1]
test.class.var <- test.data[,1]

# Construindo um modelo de regressão logística
formula.init <- "status_bin ~ ."
formula.init <- as.formula(formula.init)
lr.model <- glm(formula = formula.init, data = train.data, family = "binomial")

# Visualizando o modelo
summary(lr.model)
```

    ## 
    ## Call:
    ## glm(formula = formula.init, family = "binomial", data = train.data)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.9225  -0.5885   0.3671   0.6841   2.3668  
    ## 
    ## Coefficients:
    ##                                           Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)                                0.62607    0.51704   1.211  0.22594
    ## tempo_empresa                              1.12739    0.47024   2.397  0.01651
    ## faixa_tempo_empresa2. Entre 3 e 4 anos     0.19451    0.21019   0.925  0.35476
    ## faixa_tempo_empresa3. Entre 5 e 6 anos     0.14028    0.30968   0.453  0.65057
    ## faixa_tempo_empresa4. Entre 7 e 8 anos     0.22640    0.43921   0.515  0.60622
    ## faixa_tempo_empresa5. Entre 9 e 10 anos   -0.22648    0.54924  -0.412  0.68008
    ## faixa_tempo_empresa6. Entre 11 e 15 anos  -0.34058    0.71397  -0.477  0.63335
    ## faixa_tempo_empresa7. Entre 16 e 20 anos  -0.52928    1.01992  -0.519  0.60380
    ## faixa_tempo_empresa8. Entre 21 e 25 anos  -0.62826    1.33548  -0.470  0.63804
    ## faixa_tempo_empresa9. Entre 26 e 35 anos  -2.21654    1.70734  -1.298  0.19420
    ## faixa_tempo_empresaAcima dos 36 anos      -3.03087    2.47541  -1.224  0.22080
    ## moradiaindefinido                         -0.85562    0.65830  -1.300  0.19369
    ## moradiaoutras                             -0.22149    0.20966  -1.056  0.29079
    ## moradiapais                                0.56591    0.17664   3.204  0.00136
    ## moradiapriv                                0.06867    0.24497   0.280  0.77922
    ## moradiaprópria                             0.85244    0.15048   5.665 1.47e-08
    ## tempo_emprestimo                          -0.01054    0.06666  -0.158  0.87437
    ## idade                                     -0.21860    0.21949  -0.996  0.31928
    ## faixa_etaria2.Entre 26 e 35 anos           0.34894    0.21244   1.643  0.10048
    ## faixa_etaria3.Entre 36 e 45 anos           0.31633    0.37595   0.841  0.40012
    ## faixa_etaria4.Entre 46 e 55 anos           0.53726    0.56900   0.944  0.34506
    ## faixa_etaria5.Acima dos 55 anos            0.37020    0.77035   0.481  0.63083
    ## estado_civildivorciada(o)                 -0.24202    0.59941  -0.404  0.68639
    ## estado_civilseparada(o)                   -1.18960    0.29910  -3.977 6.97e-05
    ## estado_civilsolteira(o)                   -0.40791    0.16844  -2.422  0.01545
    ## estado_civilviúva(o)                      -0.43479    0.43047  -1.010  0.31249
    ## registrossim                              -1.66556    0.13393 -12.436  < 2e-16
    ## trabalhofixo                               0.84018    0.13404   6.268 3.65e-10
    ## trabalhoindefinido                       -10.90610  324.74443  -0.034  0.97321
    ## trabalhomeio período                      -0.38584    0.18928  -2.038  0.04150
    ## trabalhooutros                             0.03421    0.27532   0.124  0.90112
    ## despesas                                  -0.30627    0.07088  -4.321 1.55e-05
    ## renda                                      0.56289    0.07247   7.767 8.02e-15
    ## ativos                                     0.24619    0.09174   2.684  0.00728
    ## dividas                                   -0.29807    0.06537  -4.560 5.11e-06
    ## valor_emprestimo                          -0.90757    0.10600  -8.562  < 2e-16
    ## preco_do_bem                               0.52067    0.10053   5.179 2.23e-07
    ##                                             
    ## (Intercept)                                 
    ## tempo_empresa                            *  
    ## faixa_tempo_empresa2. Entre 3 e 4 anos      
    ## faixa_tempo_empresa3. Entre 5 e 6 anos      
    ## faixa_tempo_empresa4. Entre 7 e 8 anos      
    ## faixa_tempo_empresa5. Entre 9 e 10 anos     
    ## faixa_tempo_empresa6. Entre 11 e 15 anos    
    ## faixa_tempo_empresa7. Entre 16 e 20 anos    
    ## faixa_tempo_empresa8. Entre 21 e 25 anos    
    ## faixa_tempo_empresa9. Entre 26 e 35 anos    
    ## faixa_tempo_empresaAcima dos 36 anos        
    ## moradiaindefinido                           
    ## moradiaoutras                               
    ## moradiapais                              ** 
    ## moradiapriv                                 
    ## moradiaprópria                           ***
    ## tempo_emprestimo                            
    ## idade                                       
    ## faixa_etaria2.Entre 26 e 35 anos            
    ## faixa_etaria3.Entre 36 e 45 anos            
    ## faixa_etaria4.Entre 46 e 55 anos            
    ## faixa_etaria5.Acima dos 55 anos             
    ## estado_civildivorciada(o)                   
    ## estado_civilseparada(o)                  ***
    ## estado_civilsolteira(o)                  *  
    ## estado_civilviúva(o)                        
    ## registrossim                             ***
    ## trabalhofixo                             ***
    ## trabalhoindefinido                          
    ## trabalhomeio período                     *  
    ## trabalhooutros                              
    ## despesas                                 ***
    ## renda                                    ***
    ## ativos                                   ** 
    ## dividas                                  ***
    ## valor_emprestimo                         ***
    ## preco_do_bem                             ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 3201.2  on 2670  degrees of freedom
    ## Residual deviance: 2307.2  on 2634  degrees of freedom
    ## AIC: 2381.2
    ## 
    ## Number of Fisher Scoring iterations: 11

**Testando o modelo nos dados inicial e avaliando a ConfusionMatrix**

``` r
lr.predictions <- predict(lr.model, test.data, type="response")
lr.predictions <- round(lr.predictions)

confusionMatrix(table(data = lr.predictions, reference = test.class.var), positive = '1')
```

    ## Confusion Matrix and Statistics
    ## 
    ##     reference
    ## data    0    1
    ##    0  255  136
    ##    1  233 1158
    ##                                           
    ##                Accuracy : 0.7929          
    ##                  95% CI : (0.7734, 0.8115)
    ##     No Information Rate : 0.7262          
    ##     P-Value [Acc > NIR] : 4.769e-11       
    ##                                           
    ##                   Kappa : 0.445           
    ##                                           
    ##  Mcnemar's Test P-Value : 5.806e-07       
    ##                                           
    ##             Sensitivity : 0.8949          
    ##             Specificity : 0.5225          
    ##          Pos Pred Value : 0.8325          
    ##          Neg Pred Value : 0.6522          
    ##              Prevalence : 0.7262          
    ##          Detection Rate : 0.6498          
    ##    Detection Prevalence : 0.7806          
    ##       Balanced Accuracy : 0.7087          
    ##                                           
    ##        'Positive' Class : 1               
    ## 

**A matrix de confusao mostra que o modelo está acertando muito mais que
errando**

**Esse resultado está alinhado com o apurado para Accuracy, que é
relativamente alto**

<font size="3">Em um primeiro momento poderiamos dizer o que o modelo
está com um bom nível de Accuracy, mas dá pra melhorar?</font>

``` r
#selecionar as melhores variáveis
formula <- "status_bin ~ ."
formula <- as.formula(formula)
control <- trainControl(method = "repeatedcv", number = 10, repeats = 2)
model <- train(formula, data = train.data, method = "glm", trControl = control)
```

    ## Warning in predict.lm(object, newdata, se.fit, scale = 1, type = if (type == :
    ## prediction from a rank-deficient fit may be misleading
    
    ## Warning in predict.lm(object, newdata, se.fit, scale = 1, type = if (type == :
    ## prediction from a rank-deficient fit may be misleading

``` r
importance <- varImp(model, scale = FALSE)
plot(importance)
```

![](README_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->
**Contrstuindo um novo modelo com variáveis selecionadas**

``` r
formula.new <- "status_bin ~ registros + valor_emprestimo + renda + trabalho + preco_do_bem + moradia + estado_civil + ativos + despesas + dividas + tempo_empresa"
formula.new <- as.formula(formula.new)
lr.model.new <- glm(formula = formula.new, data = train.data, family = "binomial")
glimpse(df_credito_ajustado_previ)
```

    ## Rows: 4,453
    ## Columns: 16
    ## $ status_bin          <fct> 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1,...
    ## $ tempo_empresa       <dbl[,1]> <matrix[26 x 1]>
    ## $ faixa_tempo_empresa <fct> 5. Entre 9 e 10 anos, 7. Entre 16 e 20 anos, 5....
    ## $ moradia             <fct> alugada, alugada, própria, alugada, alugada, pr...
    ## $ tempo_emprestimo    <dbl[,1]> <matrix[26 x 1]>
    ## $ idade               <dbl[,1]> <matrix[26 x 1]>
    ## $ faixa_etaria        <fct> 2.Entre 26 e 35 anos, 5.Acima dos 55 anos, 4.En...
    ## $ estado_civil        <fct> casada(o), viúva(o), casada(o), solteira(o), so...
    ## $ registros           <fct> não, não, sim, não, não, não, não, não, não, nã...
    ## $ trabalho            <fct> autônomo, fixo, autônomo, fixo, fixo, fixo, fix...
    ## $ despesas            <dbl[,1]> <matrix[26 x 1]>
    ## $ renda               <dbl[,1]> <matrix[26 x 1]>
    ## $ ativos              <dbl[,1]> <matrix[26 x 1]>
    ## $ dividas             <dbl[,1]> <matrix[26 x 1]>
    ## $ valor_emprestimo    <dbl[,1]> <matrix[26 x 1]>
    ## $ preco_do_bem        <dbl[,1]> <matrix[26 x 1]>

``` r
summary(lr.model.new)
```

    ## 
    ## Call:
    ## glm(formula = formula.new, family = "binomial", data = train.data)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -3.1750  -0.6162   0.3752   0.6927   2.4175  
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                 0.78284    0.15564   5.030 4.91e-07 ***
    ## registrossim               -1.65825    0.13174 -12.587  < 2e-16 ***
    ## valor_emprestimo           -0.91227    0.09267  -9.844  < 2e-16 ***
    ## renda                       0.56186    0.07188   7.817 5.41e-15 ***
    ## trabalhofixo                0.85440    0.12927   6.609 3.86e-11 ***
    ## trabalhoindefinido        -11.17452  324.74437  -0.034  0.97255    
    ## trabalhomeio período       -0.50954    0.18151  -2.807  0.00500 ** 
    ## trabalhooutros             -0.20065    0.26224  -0.765  0.44419    
    ## preco_do_bem                0.53202    0.09648   5.514 3.50e-08 ***
    ## moradiaindefinido          -0.81247    0.64764  -1.255  0.20966    
    ## moradiaoutras              -0.20826    0.20613  -1.010  0.31234    
    ## moradiapais                 0.61598    0.17299   3.561  0.00037 ***
    ## moradiapriv                 0.12318    0.24164   0.510  0.61022    
    ## moradiaprópria              0.86106    0.14807   5.815 6.05e-09 ***
    ## estado_civildivorciada(o)  -0.23205    0.59065  -0.393  0.69441    
    ## estado_civilseparada(o)    -1.17737    0.29337  -4.013 5.99e-05 ***
    ## estado_civilsolteira(o)    -0.41228    0.15865  -2.599  0.00936 ** 
    ## estado_civilviúva(o)       -0.47734    0.43249  -1.104  0.26973    
    ## ativos                      0.21512    0.08716   2.468  0.01358 *  
    ## despesas                   -0.30020    0.06698  -4.482 7.41e-06 ***
    ## dividas                    -0.27562    0.06347  -4.342 1.41e-05 ***
    ## tempo_empresa               0.72442    0.07722   9.382  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 3201.2  on 2670  degrees of freedom
    ## Residual deviance: 2333.1  on 2649  degrees of freedom
    ## AIC: 2377.1
    ## 
    ## Number of Fisher Scoring iterations: 11

**Testando o modelo**

``` r
# Testando o modelo nos dados de teste
lr.predictions.new <- predict(lr.model.new, test.data, type = "response") 
lr.predictions.new <- round(lr.predictions.new)
```

**Avaliando a matrix de confusão e mudança na Accuracy**

``` r
confusionMatrix(table(data = lr.predictions.new, reference = test.class.var), positive = '1')
```

    ## Confusion Matrix and Statistics
    ## 
    ##     reference
    ## data    0    1
    ##    0  247  124
    ##    1  241 1170
    ##                                           
    ##                Accuracy : 0.7952          
    ##                  95% CI : (0.7757, 0.8137)
    ##     No Information Rate : 0.7262          
    ##     P-Value [Acc > NIR] : 1.041e-11       
    ##                                           
    ##                   Kappa : 0.4434          
    ##                                           
    ##  Mcnemar's Test P-Value : 1.265e-09       
    ##                                           
    ##             Sensitivity : 0.9042          
    ##             Specificity : 0.5061          
    ##          Pos Pred Value : 0.8292          
    ##          Neg Pred Value : 0.6658          
    ##              Prevalence : 0.7262          
    ##          Detection Rate : 0.6566          
    ##    Detection Prevalence : 0.7918          
    ##       Balanced Accuracy : 0.7052          
    ##                                           
    ##        'Positive' Class : 1               
    ## 

**Com base na Accuracy conclui que devemos utilizar o segundo modelo,
embora seja pequena a mudança de um para o outro**

**Para concluir, foi apresentado a Curva de ROC no gráfico abaixo:**

``` r
# Criando curvas ROC
lr.model.best <- lr.model
lr.prediction.values <- predict(lr.model.best, test.feature.vars, type = "response")
predictions <- prediction(lr.prediction.values, test.class.var)
par(mfrow = c(1,2))
plot.roc.curve(predictions, title.text = "Curva ROC")
plot.pr.curve(predictions, title.text = "Curva Precision/Recall")
```

![](README_files/figure-gfm/unnamed-chunk-29-1.png)<!-- -->

**O gráfico da esquerda nos mostra que a curva toda acima e a esquerda
da reta. Sendo assim nosso modelo preditivo está estimando bem .**

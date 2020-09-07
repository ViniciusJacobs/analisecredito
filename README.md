
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

![](README_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

**Por fim, quiz checar dentre essas variáveis qual a proporção entre os
clientes Bons e Ruins**

``` r
prop.faixa.et 
```

![](README_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
prop.tempo.emp
```

![](README_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
prop.est.civi 
```

![](README_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

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
    ##          1   0.7424 0.2722    0.02731 0.08408         
    ##          2   0.7432 0.3280    0.02766 0.08399         
    ##          3   0.7450 0.2773    0.01925 0.05578         
    ##          4   0.7446 0.2936    0.02976 0.08424         
    ##          5   0.7522 0.3330    0.02698 0.08395         
    ##          6   0.7653 0.3620    0.03278 0.09131         
    ##          7   0.7690 0.3742    0.03479 0.09354         
    ##          8   0.7675 0.3617    0.03522 0.10169         
    ##          9   0.7630 0.3566    0.04054 0.11542         
    ##         10   0.7698 0.3745    0.03651 0.11098         
    ##         15   0.7727 0.3668    0.03624 0.10865        *
    ## 
    ## The top 5 variables (out of 15):
    ##    registros, trabalho, renda, valor_emprestimo, tempo_empresa

``` r
varImp((rfe.results))
```

    ##                        Overall
    ## registros           32.1318518
    ## trabalho            19.5305996
    ## renda               16.9025853
    ## valor_emprestimo    15.5692081
    ## ativos              13.8470150
    ## tempo_empresa       13.8135015
    ## moradia             12.0475932
    ## faixa_tempo_empresa  9.3742341
    ## despesas             6.2520829
    ## preco_do_bem         5.1549762
    ## idade                4.3789855
    ## estado_civil         3.3481970
    ## tempo_emprestimo     3.3229319
    ## faixa_etaria         3.0103803
    ## dividas             -0.6201949

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
    ## -3.2598  -0.5851   0.3712   0.6700   2.4553  
    ## 
    ## Coefficients:
    ##                                           Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)                                0.56830    0.52344   1.086 0.277609
    ## tempo_empresa                              0.59345    0.47228   1.257 0.208918
    ## faixa_tempo_empresa2. Entre 3 e 4 anos     0.27764    0.21697   1.280 0.200682
    ## faixa_tempo_empresa3. Entre 5 e 6 anos     0.19167    0.31028   0.618 0.536749
    ## faixa_tempo_empresa4. Entre 7 e 8 anos     0.37471    0.43793   0.856 0.392196
    ## faixa_tempo_empresa5. Entre 9 e 10 anos    0.02329    0.54667   0.043 0.966019
    ## faixa_tempo_empresa6. Entre 11 e 15 anos   0.19580    0.72392   0.270 0.786792
    ## faixa_tempo_empresa7. Entre 16 e 20 anos   0.79639    1.03975   0.766 0.443710
    ## faixa_tempo_empresa8. Entre 21 e 25 anos   0.47678    1.36218   0.350 0.726327
    ## faixa_tempo_empresa9. Entre 26 e 35 anos  -0.89112    1.71480  -0.520 0.603299
    ## faixa_tempo_empresaAcima dos 36 anos      -1.04308    2.49062  -0.419 0.675360
    ## moradiaindefinido                         -0.64658    0.60721  -1.065 0.286943
    ## moradiaoutras                             -0.36717    0.20649  -1.778 0.075384
    ## moradiapais                                0.64211    0.17826   3.602 0.000316
    ## moradiapriv                               -0.24046    0.23119  -1.040 0.298301
    ## moradiaprópria                             0.64636    0.14635   4.416 1.00e-05
    ## tempo_emprestimo                           0.01543    0.06689   0.231 0.817604
    ## idade                                     -0.15975    0.22205  -0.719 0.471861
    ## faixa_etaria2.Entre 26 e 35 anos           0.43721    0.21581   2.026 0.042770
    ## faixa_etaria3.Entre 36 e 45 anos           0.27659    0.37787   0.732 0.464185
    ## faixa_etaria4.Entre 46 e 55 anos           0.42905    0.56910   0.754 0.450903
    ## faixa_etaria5.Acima dos 55 anos            0.73136    0.76727   0.953 0.340490
    ## estado_civildivorciada(o)                 -0.86614    0.57292  -1.512 0.130586
    ## estado_civilseparada(o)                   -1.33888    0.29074  -4.605 4.12e-06
    ## estado_civilsolteira(o)                   -0.33340    0.16893  -1.974 0.048422
    ## estado_civilviúva(o)                      -0.51350    0.39921  -1.286 0.198346
    ## registrossim                              -1.81930    0.13418 -13.558  < 2e-16
    ## trabalhofixo                               0.72791    0.13413   5.427 5.73e-08
    ## trabalhoindefinido                       -11.36381  324.74379  -0.035 0.972085
    ## trabalhomeio período                      -0.54649    0.19290  -2.833 0.004611
    ## trabalhooutros                            -0.01083    0.26496  -0.041 0.967400
    ## despesas                                  -0.28035    0.06746  -4.156 3.24e-05
    ## renda                                      0.64016    0.07428   8.618  < 2e-16
    ## ativos                                     0.34188    0.10106   3.383 0.000717
    ## dividas                                   -0.15977    0.05023  -3.181 0.001469
    ## valor_emprestimo                          -0.91619    0.10663  -8.592  < 2e-16
    ## preco_do_bem                               0.53480    0.10741   4.979 6.39e-07
    ##                                             
    ## (Intercept)                                 
    ## tempo_empresa                               
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
    ## moradiaoutras                            .  
    ## moradiapais                              ***
    ## moradiapriv                                 
    ## moradiaprópria                           ***
    ## tempo_emprestimo                            
    ## idade                                       
    ## faixa_etaria2.Entre 26 e 35 anos         *  
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
    ## trabalhomeio período                     ** 
    ## trabalhooutros                              
    ## despesas                                 ***
    ## renda                                    ***
    ## ativos                                   ***
    ## dividas                                  ** 
    ## valor_emprestimo                         ***
    ## preco_do_bem                             ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 3165.9  on 2670  degrees of freedom
    ## Residual deviance: 2288.9  on 2634  degrees of freedom
    ## AIC: 2362.9
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
    ##    0  234   94
    ##    1  273 1181
    ##                                           
    ##                Accuracy : 0.7941          
    ##                  95% CI : (0.7745, 0.8126)
    ##     No Information Rate : 0.7155          
    ##     P-Value [Acc > NIR] : 2.012e-14       
    ##                                           
    ##                   Kappa : 0.434           
    ##                                           
    ##  Mcnemar's Test P-Value : < 2.2e-16       
    ##                                           
    ##             Sensitivity : 0.9263          
    ##             Specificity : 0.4615          
    ##          Pos Pred Value : 0.8122          
    ##          Neg Pred Value : 0.7134          
    ##              Prevalence : 0.7155          
    ##          Detection Rate : 0.6627          
    ##    Detection Prevalence : 0.8159          
    ##       Balanced Accuracy : 0.6939          
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

![](README_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->
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
    ## -3.1752  -0.5962   0.3811   0.6845   2.4101  
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                 1.06661    0.15654   6.814 9.51e-12 ***
    ## registrossim               -1.82057    0.13226 -13.765  < 2e-16 ***
    ## valor_emprestimo           -0.91034    0.09468  -9.615  < 2e-16 ***
    ## renda                       0.64240    0.07366   8.721  < 2e-16 ***
    ## trabalhofixo                0.74507    0.13083   5.695 1.23e-08 ***
    ## trabalhoindefinido        -11.58567  324.74376  -0.036 0.971540    
    ## trabalhomeio período       -0.63443    0.18639  -3.404 0.000664 ***
    ## trabalhooutros             -0.07100    0.25120  -0.283 0.777463    
    ## preco_do_bem                0.53933    0.10377   5.197 2.02e-07 ***
    ## moradiaindefinido          -0.66923    0.61169  -1.094 0.273928    
    ## moradiaoutras              -0.38174    0.20347  -1.876 0.060636 .  
    ## moradiapais                 0.64476    0.17470   3.691 0.000224 ***
    ## moradiapriv                -0.22451    0.22856  -0.982 0.325980    
    ## moradiaprópria              0.65238    0.14407   4.528 5.95e-06 ***
    ## estado_civildivorciada(o)  -0.87287    0.56477  -1.546 0.122215    
    ## estado_civilseparada(o)    -1.34442    0.28335  -4.745 2.09e-06 ***
    ## estado_civilsolteira(o)    -0.37969    0.15992  -2.374 0.017587 *  
    ## estado_civilviúva(o)       -0.57211    0.39292  -1.456 0.145379    
    ## ativos                      0.30268    0.09602   3.152 0.001621 ** 
    ## despesas                   -0.29147    0.06447  -4.521 6.16e-06 ***
    ## dividas                    -0.14727    0.04895  -3.009 0.002624 ** 
    ## tempo_empresa               0.62519    0.07529   8.304  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 3165.9  on 2670  degrees of freedom
    ## Residual deviance: 2315.5  on 2649  degrees of freedom
    ## AIC: 2359.5
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
    ##    0  240   95
    ##    1  267 1180
    ##                                           
    ##                Accuracy : 0.7969          
    ##                  95% CI : (0.7774, 0.8153)
    ##     No Information Rate : 0.7155          
    ##     P-Value [Acc > NIR] : 2.218e-15       
    ##                                           
    ##                   Kappa : 0.4443          
    ##                                           
    ##  Mcnemar's Test P-Value : < 2.2e-16       
    ##                                           
    ##             Sensitivity : 0.9255          
    ##             Specificity : 0.4734          
    ##          Pos Pred Value : 0.8155          
    ##          Neg Pred Value : 0.7164          
    ##              Prevalence : 0.7155          
    ##          Detection Rate : 0.6622          
    ##    Detection Prevalence : 0.8120          
    ##       Balanced Accuracy : 0.6994          
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

![](README_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

**O gráfico da esquerda nos mostra que a curva toda acima e a esquerda
da reta. Sendo assim nosso modelo preditivo está estimando bem .**

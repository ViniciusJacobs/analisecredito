
# Análise de Perfil: Concessão de Crédito

## Objetivo

  - Demonstrar algumas técnicas de AED;
  - Apresentar as caracteristicas demograficas e financeiras dos
    clientes presentes na base de dados;
  - Identificar quais variáveis do modelo explicam quais são os clientes
    bons e ruins;
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

**Etapa 1: Análise Exploratória dos Dados**

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

**Contagem dos valores NAs por váriavel do modelo**

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

**Avaliação para substituição dos NA’s**

**As variáveis moradia, estado\_civil e trabalho, apresentaram um número
de NA’s pouco expressivo, foi considerado como ERRO no cadastro. Para
esse estudo não foi feita análise, mas sim a alteração dos campos NA’s
para “indefinido”.**

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

**Etapa 2: Caracteristicas demograficas e financeiras dos clientes
presentes na base de dados.**

**Na etapa inicial, foi realizada analise gráfica e o mapeamento de
algumas variáveis, conforme abaixo:**

``` r
dist_por_perfil + grafico_estado_civil + dist_por_faixa_eta
```

![](README_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

**Por fim uma análise dentro das variáveis analisadas, buscando a
proporcão de bons pagadores de forma ampla**

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

**Etapa 3: Elaborar modelo preditivo para o modelo**

**Ajustes nas Variáveis**

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
# Dividindo os dados em treino e teste - 60:40
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
    ## Outer resampling method: Cross-Validated (20 fold) 
    ## 
    ## Resampling performance over subset size:
    ## 
    ##  Variables Accuracy  Kappa AccuracySD KappaSD Selected
    ##          1   0.7338 0.2453    0.02279 0.07145         
    ##          2   0.7424 0.3254    0.02518 0.08160         
    ##          3   0.7533 0.3125    0.02589 0.07862         
    ##          4   0.7447 0.3037    0.02625 0.08138         
    ##          5   0.7593 0.3548    0.02497 0.06619         
    ##          6   0.7780 0.4032    0.03038 0.08432         
    ##          7   0.7724 0.3861    0.03303 0.08857         
    ##          8   0.7776 0.3954    0.02585 0.07125         
    ##          9   0.7795 0.4086    0.02525 0.06840         
    ##         10   0.7803 0.4068    0.03424 0.09437         
    ##         15   0.7821 0.3925    0.02380 0.07250        *
    ## 
    ## The top 5 variables (out of 15):
    ##    registros, trabalho, renda, valor_emprestimo, tempo_empresa

``` r
varImp((rfe.results))
```

    ##                       Overall
    ## registros           29.187064
    ## trabalho            25.542825
    ## renda               19.521500
    ## valor_emprestimo    17.582717
    ## tempo_empresa       16.763034
    ## ativos              16.603801
    ## faixa_tempo_empresa 11.436808
    ## moradia              9.305382
    ## tempo_emprestimo     6.700527
    ## despesas             6.246103
    ## idade                6.227962
    ## preco_do_bem         6.209147
    ## estado_civil         3.538029
    ## faixa_etaria         2.737623
    ## dividas              1.417824

**As variáveis que apresentaram maior relação explicativa a relação bom
ou mau pagador (conforme esse modelo) são:**

**-registros**

**-trabalho**

**-tempo\_empresa**

**-renda**

**-valor-emprestimo**

**Na análise descritiva realizada no início do estudo foram avaliadas
faixa etária, tempo de trabalho na empresa e estado civil. Nota-se, que
somente o tempo de empresa dentre as variáveis, consta na lista das 5
mais explicativas.**

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
    ## -3.0694  -0.6158   0.4000   0.6933   2.4424  
    ## 
    ## Coefficients:
    ##                                            Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)                                0.479719   0.502205   0.955 0.339463
    ## tempo_empresa                              0.447114   0.455837   0.981 0.326660
    ## faixa_tempo_empresa2. Entre 3 e 4 anos     0.336222   0.209983   1.601 0.109335
    ## faixa_tempo_empresa3. Entre 5 e 6 anos     0.390558   0.307568   1.270 0.204147
    ## faixa_tempo_empresa4. Entre 7 e 8 anos     0.555982   0.428314   1.298 0.194263
    ## faixa_tempo_empresa5. Entre 9 e 10 anos    0.534453   0.538346   0.993 0.320823
    ## faixa_tempo_empresa6. Entre 11 e 15 anos   0.415233   0.695937   0.597 0.550740
    ## faixa_tempo_empresa7. Entre 16 e 20 anos   1.008333   0.997825   1.011 0.312241
    ## faixa_tempo_empresa8. Entre 21 e 25 anos   0.845337   1.296151   0.652 0.514278
    ## faixa_tempo_empresa9. Entre 26 e 35 anos  -0.007154   1.667556  -0.004 0.996577
    ## faixa_tempo_empresaAcima dos 36 anos      -0.155493   2.455106  -0.063 0.949500
    ## moradiaindefinido                         -0.726578   0.615693  -1.180 0.237962
    ## moradiaoutras                             -0.397661   0.210890  -1.886 0.059345
    ## moradiapais                                0.361633   0.173065   2.090 0.036656
    ## moradiapriv                                0.086019   0.244349   0.352 0.724813
    ## moradiaprópria                             0.657256   0.147066   4.469 7.85e-06
    ## tempo_emprestimo                          -0.054175   0.065514  -0.827 0.408283
    ## idade                                     -0.153131   0.215838  -0.709 0.478032
    ## faixa_etaria2.Entre 26 e 35 anos           0.176754   0.210362   0.840 0.400776
    ## faixa_etaria3.Entre 36 e 45 anos           0.142088   0.367555   0.387 0.699071
    ## faixa_etaria4.Entre 46 e 55 anos           0.139069   0.555964   0.250 0.802478
    ## faixa_etaria5.Acima dos 55 anos            0.143697   0.752303   0.191 0.848518
    ## estado_civildivorciada(o)                 -0.276181   0.509262  -0.542 0.587601
    ## estado_civilseparada(o)                   -1.357698   0.300220  -4.522 6.12e-06
    ## estado_civilsolteira(o)                   -0.231553   0.166317  -1.392 0.163850
    ## estado_civilviúva(o)                      -0.318691   0.456078  -0.699 0.484699
    ## registrossim                              -1.746111   0.133105 -13.118  < 2e-16
    ## trabalhofixo                               0.778825   0.131090   5.941 2.83e-09
    ## trabalhoindefinido                       -12.439337 378.006235  -0.033 0.973748
    ## trabalhomeio período                      -0.653020   0.185409  -3.522 0.000428
    ## trabalhooutros                             0.229837   0.273517   0.840 0.400738
    ## despesas                                  -0.241855   0.067490  -3.584 0.000339
    ## renda                                      0.508890   0.069929   7.277 3.41e-13
    ## ativos                                     0.260487   0.092909   2.804 0.005052
    ## dividas                                   -0.155241   0.067931  -2.285 0.022297
    ## valor_emprestimo                          -0.778657   0.103688  -7.510 5.93e-14
    ## preco_do_bem                               0.451289   0.102042   4.423 9.75e-06
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
    ## moradiapais                              *  
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
    ## estado_civilsolteira(o)                     
    ## estado_civilviúva(o)                        
    ## registrossim                             ***
    ## trabalhofixo                             ***
    ## trabalhoindefinido                          
    ## trabalhomeio período                     ***
    ## trabalhooutros                              
    ## despesas                                 ***
    ## renda                                    ***
    ## ativos                                   ** 
    ## dividas                                  *  
    ## valor_emprestimo                         ***
    ## preco_do_bem                             ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 3164.0  on 2670  degrees of freedom
    ## Residual deviance: 2367.6  on 2634  degrees of freedom
    ## AIC: 2441.6
    ## 
    ## Number of Fisher Scoring iterations: 12

**Testando o modelo nos dados de teste e avaliando a ConfusionMatrix**

``` r
lr.predictions <- predict(lr.model, test.data, type="response")
lr.predictions <- round(lr.predictions)

confusionMatrix(table(data = lr.predictions, reference = test.class.var), positive = '1')
```

    ## Confusion Matrix and Statistics
    ## 
    ##     reference
    ## data    0    1
    ##    0  265  109
    ##    1  243 1165
    ##                                           
    ##                Accuracy : 0.8025          
    ##                  95% CI : (0.7832, 0.8207)
    ##     No Information Rate : 0.7149          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.4737          
    ##                                           
    ##  Mcnemar's Test P-Value : 1.352e-12       
    ##                                           
    ##             Sensitivity : 0.9144          
    ##             Specificity : 0.5217          
    ##          Pos Pred Value : 0.8274          
    ##          Neg Pred Value : 0.7086          
    ##              Prevalence : 0.7149          
    ##          Detection Rate : 0.6538          
    ##    Detection Prevalence : 0.7901          
    ##       Balanced Accuracy : 0.7180          
    ##                                           
    ##        'Positive' Class : 1               
    ## 

**Em um primeiro momento poderiamos dizer o que o modelo está com um bom
nível de Accuracy, mas da pra melhorar?**

``` r
#selecionar as melhores variáveis
formula <- "status_bin ~ ."
formula <- as.formula(formula)
control <- trainControl(method = "repeatedcv", number = 10, repeats = 2)
model <- train(formula, data = train.data, method = "glm", trControl = control)
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
    ## -3.0221  -0.6303   0.4133   0.6921   2.4137  
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                 0.93590    0.15512   6.034 1.60e-09 ***
    ## registrossim               -1.73428    0.13122 -13.217  < 2e-16 ***
    ## valor_emprestimo           -0.81501    0.09101  -8.955  < 2e-16 ***
    ## renda                       0.50779    0.06918   7.340 2.14e-13 ***
    ## trabalhofixo                0.78460    0.12748   6.155 7.53e-10 ***
    ## trabalhoindefinido        -12.61359  377.75358  -0.033  0.97336    
    ## trabalhomeio período       -0.74185    0.17841  -4.158 3.21e-05 ***
    ## trabalhooutros              0.01209    0.25314   0.048  0.96192    
    ## preco_do_bem                0.47715    0.09802   4.868 1.13e-06 ***
    ## moradiaindefinido          -0.70007    0.60165  -1.164  0.24460    
    ## moradiaoutras              -0.36371    0.20798  -1.749  0.08033 .  
    ## moradiapais                 0.42080    0.16988   2.477  0.01324 *  
    ## moradiapriv                 0.09849    0.24019   0.410  0.68178    
    ## moradiaprópria              0.65072    0.14478   4.495 6.97e-06 ***
    ## estado_civildivorciada(o)  -0.30215    0.50509  -0.598  0.54970    
    ## estado_civilseparada(o)    -1.33117    0.29466  -4.518 6.25e-06 ***
    ## estado_civilsolteira(o)    -0.21519    0.15629  -1.377  0.16854    
    ## estado_civilviúva(o)       -0.39040    0.45488  -0.858  0.39076    
    ## ativos                      0.24066    0.08932   2.694  0.00705 ** 
    ## despesas                   -0.23934    0.06412  -3.733  0.00019 ***
    ## dividas                    -0.13782    0.06592  -2.091  0.03656 *  
    ## tempo_empresa               0.61726    0.07262   8.500  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 3164.0  on 2670  degrees of freedom
    ## Residual deviance: 2390.5  on 2649  degrees of freedom
    ## AIC: 2434.5
    ## 
    ## Number of Fisher Scoring iterations: 12

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
    ##    0  260  104
    ##    1  248 1170
    ##                                           
    ##                Accuracy : 0.8025          
    ##                  95% CI : (0.7832, 0.8207)
    ##     No Information Rate : 0.7149          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.4703          
    ##                                           
    ##  Mcnemar's Test P-Value : 2.499e-14       
    ##                                           
    ##             Sensitivity : 0.9184          
    ##             Specificity : 0.5118          
    ##          Pos Pred Value : 0.8251          
    ##          Neg Pred Value : 0.7143          
    ##              Prevalence : 0.7149          
    ##          Detection Rate : 0.6566          
    ##    Detection Prevalence : 0.7957          
    ##       Balanced Accuracy : 0.7151          
    ##                                           
    ##        'Positive' Class : 1               
    ## 

**Frente ao resultado, seria necessário repensar a necessidade da
escolha pelo segundo modelo**

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
da reta. Sendo assim nosso modelo preditivo está estimando bem.**

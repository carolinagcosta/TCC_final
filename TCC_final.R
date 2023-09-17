#### Análises do TCC ####

# Ana Carolina Galindo da Costa #
# Orientador: Rogério Olímpio #

# Machine learning como abordagem analítica para identificação de projetos de pesquisa científica de sucesso #

#### Análise de Correspondência Múltipla ####

# Instalação e carregamento dos pacotes utilizados

pacotes <- c("plotly",
             "tidyverse",
             "ggrepel",
             "knitr", 
             "kableExtra",
             "reshape2",
             "PerformanceAnalytics", 
             "psych",
             "ltm", 
             "Hmisc",
             "readxl",
             "sjPlot",
             "ade4")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

# Carregamento da base de dados

ppg <- read_xlsx('ppg.xlsx')

#Conhecendo os dados
view(ppg)
glimpse(ppg)
summary(ppg)

table(ppg$Categoria)
table(ppg$Cidade_origem)
table(ppg$Estado_origem)
table(ppg$Linha_pesquisa)
table(ppg$Trancamento)
table(ppg$Prorrogacao)
table(ppg$Instituicao)
table(ppg$Experiencia)
table(ppg$Genero)
table(ppg$Coorientador)
table(ppg$Curso_graduacao)
table(ppg$Universidade_graduacao)

# Separação das variáveis qualitativas
#Como curso_graduação, universidade_graduação e Cidade_origem são variáveis com muitos níveis retirei da análise
#Também retirei Estado_origem devido quase todas (mais de 90%) as observações serem do PA

ppg2 <- ppg[,c(3:5, 8:14)]
str(ppg2)

var_quali <- ppg2[,c(3:10)]

## O objetivo é verificar as relações de interdependência entre as variáveis categóricas

# A função para a criação da ACM pede que sejam utilizados "fatores"
var_quali <- as.data.frame(unclass(var_quali), stringsAsFactors=TRUE)

# Estatísticas descritivas
summary(var_quali)
str(var_quali)

#Iniciando a Análise de Correspondência Múltipla nas variáveis qualitativas#


#Identificando se há associação entre as variáveis (tem que haver pelo menos com uma)

sjt.xtab(var.row = var_quali$Linha_pesquisa,
         var.col = var_quali$Prorrogacao,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

sjt.xtab(var.row = var_quali$Linha_pesquisa,
         var.col = var_quali$Trancamento,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

sjt.xtab(var.row = var_quali$Linha_pesquisa,
         var.col = var_quali$Coorientador,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

sjt.xtab(var.row = var_quali$Linha_pesquisa,
         var.col = var_quali$Categoria,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

sjt.xtab(var.row = var_quali$Linha_pesquisa,
         var.col = var_quali$Instituicao,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

sjt.xtab(var.row = var_quali$Linha_pesquisa,
         var.col = var_quali$Experiencia,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

sjt.xtab(var.row = var_quali$Linha_pesquisa,
         var.col = var_quali$Genero,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

sjt.xtab(var.row = var_quali$Prorrogacao,
         var.col = var_quali$Trancamento,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

sjt.xtab(var.row = var_quali$Prorrogacao,
         var.col = var_quali$Coorientador,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

sjt.xtab(var.row = var_quali$Prorrogacao,
         var.col = var_quali$Categoria,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

sjt.xtab(var.row = var_quali$Prorrogacao,
         var.col = var_quali$Instituicao,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

sjt.xtab(var.row = var_quali$Prorrogacao,
         var.col = var_quali$Experiencia,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

sjt.xtab(var.row = var_quali$Prorrogacao,
         var.col = var_quali$Genero,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

sjt.xtab(var.row = var_quali$Trancamento,
         var.col = var_quali$Coorientador,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

sjt.xtab(var.row = var_quali$Trancamento,
         var.col = var_quali$Categoria,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

sjt.xtab(var.row = var_quali$Trancamento,
         var.col = var_quali$Instituicao,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

sjt.xtab(var.row = var_quali$Trancamento,
         var.col = var_quali$Experiencia,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

sjt.xtab(var.row = var_quali$Trancamento,
         var.col = var_quali$Genero,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

sjt.xtab(var.row = var_quali$Coorientador,
         var.col = var_quali$Categoria,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

sjt.xtab(var.row = var_quali$Coorientador,
         var.col = var_quali$Instituicao,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

sjt.xtab(var.row = var_quali$Coorientador,
         var.col = var_quali$Experiencia,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

sjt.xtab(var.row = var_quali$Coorientador,
         var.col = var_quali$Genero,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

sjt.xtab(var.row = var_quali$Categoria,
         var.col = var_quali$Instituicao,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

sjt.xtab(var.row = var_quali$Categoria,
         var.col = var_quali$Experiencia,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

sjt.xtab(var.row = var_quali$Categoria,
         var.col = var_quali$Genero,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

sjt.xtab(var.row = var_quali$Experiencia,
         var.col = var_quali$Genero,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

#Todas as categorias que foram consideradas para análise estavam signiticamente associadas a outra categoria

# Análise de Correspondência Múltipla

ACM <- dudi.acm(var_quali, scannf = FALSE)

# Analisando as variâncias de cada dimensão
perc_variancia <- (ACM$eig / sum(ACM$eig)) * 100
perc_variancia

# Quantidade de categorias por variável
quant_categorias <- apply(var_quali,
                          MARGIN =  2,
                          FUN = function(x) nlevels(as.factor(x)))

# Consolidando as coordenadas-padrão obtidas por meio da matriz binária
df_ACM <- data.frame(ACM$c1, Variável = rep(names(quant_categorias),
                                            quant_categorias))

# Plotando o mapa perceptual
df_ACM %>%
  rownames_to_column() %>%
  rename(Categoria = 1) %>%
  ggplot(aes(x = CS1, y = CS2, label = Categoria, color = Variável)) +
  geom_point() +
  geom_label_repel() +
  geom_vline(aes(xintercept = 0), linetype = "longdash", color = "grey48") +
  geom_hline(aes(yintercept = 0), linetype = "longdash", color = "grey48") +
  labs(x = paste("Dimensão 1:", paste0(round(perc_variancia[1], 2), "%")),
       y = paste("Dimensão 2:", paste0(round(perc_variancia[2], 2), "%"))) +
  theme_bw()


#### Regressão não linear múltipla com variáveis dummies####

#Pacotes
pacotes <- c("plotly","tidyverse","ggrepel","fastDummies","knitr","kableExtra",
             "splines","reshape2","PerformanceAnalytics","correlation","see",
             "ggraph","psych","nortest","rgl","car","ggside","tidyquant","olsrr",
             "jtools","ggstance","magick","cowplot","emojifont","beepr","Rcpp",
             "equatiomatic", "mgcv", "readxl")

options(rgl.debug = TRUE)

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

#Transformar em fator
glimpse(ppg2)
col_names <- names(ppg2[,3:10])
ppg2[, col_names] <- lapply(ppg2[,col_names], factor)
glimpse(ppg2)


#A função 'chart.Correlation' do pacote 'PerformanceAnalytics' apresenta as
#distribuições das variáveis, scatters, valores das correlações e suas
#respectivas significâncias
chart.Correlation(ppg2[,c(1,2)], histogram = TRUE)


#variável dependente é contínua - regressão linear

#PROCEDIMENTO N-1 DUMMIES
ppg_dummies <- dummy_columns(.data = ppg2,
                             select_columns = c("Categoria","Linha_pesquisa", 
                                                "Trancamento", "Prorrogacao", "Instituicao",
                                                "Experiencia", "Genero", "Coorientador"),
                             remove_selected_columns = T,
                             remove_most_frequent_dummy = T)

ppg_dummies %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 16)

#ESTIMAÇÃO DA REGRESSÃO LINEAR MÚLTIPLA

modelo_ppg_dummies <- lm(Nota_defesa ~ ., ppg_dummies)

summary(modelo_ppg_dummies)


#PROCEDIMENTO STEPWISE 
step_ppg <- step(modelo_ppg_dummies, k = 3.841459)

summary(step_ppg)

#TESTE DE VERIFICAÇÃO DA ADERÊNCIA DOS RESÍDUOS À NORMALIDADE

#Shapiro-Francia: n > 30
sf.test(modelo_ppg_dummies$residuals) #função 'sf.test' do pacote 'nortest'
#dados não apresentam distribuição normal, p<0.05

#Plotando os resíduos do modelo step_ppg 
ppg_dummies %>%
  mutate(residuos = step_ppg$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(color = "white", 
                 fill = "#55C667FF", 
                 bins = 15,
                 alpha = 0.6) +
  labs(x = "Resíduos",
       y = "Frequências") + 
  theme_bw()

#Acrescentando uma curva normal teórica para comparação entre as distribuições
ppg_dummies %>%
  mutate(residuos = step_ppg$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), 
                 color = "white", 
                 fill = "#55C667FF", 
                 bins = 15,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(step_ppg$residuals),
                            sd = sd(step_ppg$residuals)),
                size = 2, color = "grey30") +
  scale_color_manual(values = "grey50") +
  labs(x = "Resíduos",
       y = "Frequência") +
  theme_bw()

#Kernel density estimation (KDE) - forma não-paramétrica para estimar a
#função densidade de probabilidade de uma variável aleatória
ppg_dummies %>%
  ggplot() +
  geom_density(aes(x = step_ppg$residuals), fill = "#55C667FF") +
  labs(x = "Resíduos do Modelo Stepwise",
       y = "Densidade") +
  theme_bw()



#DIAGNÓSTICO DE HETEROCEDASTICIDADE

#Teste de Breusch-Pagan para diagnóstico de heterocedasticidade
ols_test_breusch_pagan(step_ppg)
#função 'ols_test_breusch_pagan' do pacote 'olsrr'
#Presença de heterocedasticidade -> omissão de variável(is) explicativa(s) relevante(s)

#H0 do teste: ausência de heterocedasticidade.
#H1 do teste: heterocedasticidade, ou seja, correlação entre resíduos e uma ou mais
#variáveis explicativas, o que indica omissão de variável relevante!

#os dados apresentaram homocedasticidade

#Adicionando fitted values e resíduos do modelo 'step_ppg'
#no dataset 'ppg_dummies'
ppg_dummies$fitted_step <- step_ppg$fitted.values
ppg_dummies$residuos_step <- step_ppg$residuals

#Gráfico que relaciona resíduos e fitted values do modelo 'step_planosaude'
ppg_dummies %>%
  ggplot() +
  geom_point(aes(x = fitted_step, y = residuos_step),
             color = "#55C667FF", size = 3) +
  labs(x = "Fitted Values do Modelo Stepwise",
       y = "Resíduos do Modelo Stepwise") +
  theme_bw()

#TRANSFORMAÇÃO DE BOX-COX 

#Para calcular o lambda de Box-Cox
lambda_BC <- powerTransform(ppg2$Nota_defesa)
lambda_BC

#Inserindo o lambda de Box-Cox na nova base de dados para a estimação de um
#novo modelo
ppg_dummies$bc_nota_defesa <- (((ppg2$Nota_defesa ^ lambda_BC$lambda) - 1) / 
                                   lambda_BC$lambda)

#Visualizando a nova variável na base de dados
ppg_dummies %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 16)

#Estimando um novo modelo múltiplo com dummies
modelo_bc_ppg <- lm(formula = bc_nota_defesa ~ . - Nota_defesa -fitted_step
                           -residuos_step, 
                           data = ppg_dummies)

#Parâmetros do modelo
summary(modelo_bc_ppg)

#Aplicando o procedimento Stepwise
step_bc_ppg <- step(modelo_bc_ppg, k = 3.841459)

summary(step_bc_ppg)

#Verificando a normalidade dos resíduos do modelo step_bc_ppg
#Teste de Shapiro-Francia
sf.test(step_bc_ppg$residuals) #função 'sf.test' do pacote 'nortest'
#dados normais, p > 0.05

#Plotando os novos resíduos do modelo step_bc_ppg com curva normal teórica
ppg_dummies %>%
  mutate(residuos = step_bc_ppg$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), 
                 color = "white", 
                 fill = "#440154FF", 
                 bins = 15,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(step_bc_ppg$residuals),
                            sd = sd(step_bc_ppg$residuals)),
                size = 2, color = "grey30") +
  scale_color_manual(values = "grey50") +
  labs(x = "Resíduos",
       y = "Frequência") +
  theme_bw()


#Kernel density estimation (KDE)
ppg_dummies %>%
  ggplot() +
  geom_density(aes(x = step_bc_ppg$residuals), fill = "#440154FF") +
  labs(x = "Resíduos do Modelo Stepwise com Transformação de Box-Cox",
       y = "Densidade") +
  theme_bw()

#Diagnóstico de Heterocedasticidade para o Modelo Stepwise com Box-Cox
ols_test_breusch_pagan(step_bc_ppg)

#dados apresentaram homocedasticidade

#Adicionando fitted values e resíduos do modelo 'step_bc_ppg'
#no dataset 'ppg_dummies'
ppg_dummies$fitted_step_novo <- step_bc_ppg$fitted.values
ppg_dummies$residuos_step_novo <- step_bc_ppg$residuals

#Gráfico que relaciona resíduos e fitted values do modelo 'step_bc_ppg'
ppg_dummies %>%
  ggplot() +
  geom_point(aes(x = fitted_step_novo, y = residuos_step_novo),
             color = "#440154FF", size = 3) +
  labs(x = "Fitted Values do Modelo Stepwise com Transformação de Box-Cox",
       y = "Resíduos do Modelo Stepwise com Transformação de Box-Cox") +
  theme_bw()

#### Regressão Logística Binária ####                  

#Pacotes utilizados
pacotes <- c("plotly","tidyverse","knitr","kableExtra","fastDummies","rgl","car",
             "reshape2","jtools","stargazer","lmtest","caret","pROC","ROCR","nnet",
             "magick","cowplot","globals","equatiomatic")


options(rgl.debug = TRUE)

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}


view(ppg2)

##Criando variável de prazo ("S" realizou em tempo normal ou "N" não realizou no tempo regular do curso, e precisou prorrogar ou trancar)

ppg_nova <- mutate(ppg2, Prazo = case_when(ppg2$Prorrogacao == 'N' & ppg2$Trancamento == 'N' ~ 'S',
                                         ppg2$Prorrogacao == 'S' & ppg2$Trancamento == 'N' ~ 'N',
                                         ppg2$Prorrogacao == 'N' & ppg2$Trancamento == 'S' ~ 'N',
                                         ppg2$Prorrogacao == 'S' & ppg2$Trancamento == 'S' ~ 'N'))
view(ppg_nova)

#removendo "Trancamento" e "Prorrogação

ppg_nova <- ppg_nova[,c(1:4,7:11)]
str(ppg_nova)


#PROCEDIMENTO N-1 DUMMIES
ppg_dummies2 <- dummy_columns(.data = ppg_nova,
                             select_columns = c("Categoria","Linha_pesquisa", 
                                               "Instituicao","Experiencia", 
                                               "Genero", "Coorientador", 'Prazo'),
                             remove_selected_columns = T,
                             remove_most_frequent_dummy = T)
view(ppg_dummies2)
str(ppg_dummies2)

#Modelo binomial
modelo_prazo <- glm(formula = Prazo_N ~ ., 
                      data = ppg_dummies2, 
                      family = "binomial")


#Parâmetros do modelo
summary(modelo_prazo)

#Procedimento Stepwise
step_prazo <- step(object = modelo_prazo, k = 3.841459)

#Parâmetros do modelo step_fidelidade
summary(step_prazo)

#CONSTRUÇÃO DE UMA MATRIZ DE CONFUSÃO             

confusionMatrix(table(predict(step_prazo, type = "response") >= 0.5, 
                ppg_nova$Prazo == 'N')[2:1, 2:1])


#CRITÉRIOS DE ESPECIFICIDADE E DE SENSITIVIDADE  #

#função prediction do pacote ROCR
predicoes <- prediction(predictions = step_prazo$fitted.values, 
                        labels = ppg_nova$Prazo) 
#a função prediction, do pacote ROCR, cria um objeto com os dados necessários
#para a futura plotagem da curva ROC.

#função performance do pacote ROCR
dados_curva_roc <- performance(predicoes, measure = "sens") 
#A função peformance(), do pacote ROCR, extraiu do objeto 'predicoes' os 
#dados de sensitividade, de sensibilidade e de especificidade para a plotagem.

#Porém, desejamos os dados da sensitividade, então devemos fazer o seguinte 
#ajuste:
sensitividade <- dados_curva_roc@y.values[[1]] 
#extraindo dados da sensitividade do modelo

especificidade <- performance(predicoes, measure = "spec") 
#extraindo os dados da especificidade, mas também há que se fazer um ajuste para a 
#plotagem:
especificidade <- especificidade@y.values[[1]]

cutoffs <- dados_curva_roc@x.values[[1]] 
#extraindo os cutoffs do objeto 'sensitividade'.

#Até o momento, foram extraídos 3 vetores: 'sensitividade', 'especificidade' 
#e 'cutoffs'. Poder-se-ia plotar normalmente a partir daqui com a linguagem 
#base do R, mas demos preferência à ferramenta ggplot2. Assim, criamos um data 
#frame que contém os vetores mencionados.

dados_plotagem <- cbind.data.frame(cutoffs, especificidade, sensitividade)

#Visualizando o novo dataframe dados_plotagem
dados_plotagem %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 22)

#Plotando:
ggplotly(dados_plotagem %>%
           ggplot(aes(x = cutoffs, y = especificidade)) +
           geom_line(aes(color = "Especificidade"),
                     size = 1) +
           geom_point(color = "#95D840FF",
                      size = 1.9) +
           geom_line(aes(x = cutoffs, y = sensitividade, color = "Sensitividade"),
                     size = 1) +
           geom_point(aes(x = cutoffs, y = sensitividade),
                      color = "#440154FF",
                      size = 1.9) +
           labs(x = "Cutoff",
                y = "Sensitividade/Especificidade") +
           scale_color_manual("Legenda:",
                              values = c("#95D840FF", "#440154FF")) +
           theme_bw())

# CONSTRUÇÃO DA CURVA ROC 

ROC <- roc(response = ppg_nova$Prazo, 
           predictor = step_prazo$fitted.values)

#Plotagem da curva ROC propriamente dita
ggplot() +
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1),
               color = "grey40", size = 0.2) +
  geom_line(aes(x = 1 - especificidade, y = sensitividade),
            color = "darkorchid", size = 2) +
  labs(x = "1 - Especificidade",
       y = "Sensitividade",
       title = paste("Área abaixo da curva:",
                     round(ROC$auc, 4),
                     "|",
                     "Coeficiente de Gini:",
                     round((ROC$auc[1] - 0.5) / 0.5, 4))) +
  theme(panel.background = element_rect(NA),
        panel.border = element_rect(color = "black", fill = NA),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10)
  )


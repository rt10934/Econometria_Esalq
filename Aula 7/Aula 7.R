# declarar diretorio
setwd("C:/Econometria_esalq/Econometria_Esalq/Aula 7")
#-------------------------------------------------------------------------------
# previsao de precos do etanol para o RS----------------------------------------
#-------------------------------------------------------------------------------
#_______________________________________________________________________________
# USANDO NOVOS PACOTES PARA O RSTUDIO-------------------------------------------
#_______________________________________________________________________________
#install.packages
install.packages("dplyr")
install.packages("tidyverse")
install.packages("readxl")
install.packages("timetk")
install.packages("rsample")
install.packages("modeltime")
install.packages("tidyquant")
install.packages("parsnip")

# Carregando os pacotes necessários
library(dplyr)
library(tidyverse)
library(readxl)
library(timetk)
library(rsample)
library(modeltime)
library(tidyquant)
library(parsnip)
library(lmtest)

# Importando dados
dados_mensais <- read_excel("dados.xlsx", 
                            col_types = c("date", "numeric", "numeric", "numeric"))

# Selecionando e renomeando colunas
rio_grande1 <- dados_mensais %>%
  dplyr::select(data, numero, prec_revenda, prec_produt) %>%
  dplyr::rename(revenda = prec_revenda)

# Agrupando e resumindo os dados
rio_grande1 <- rio_grande1 %>%
  group_by(data = format(data, "%Y-%m")) %>%
  summarize(prec_produt = sum(prec_produt, na.rm = TRUE)) %>%
  mutate(data = as.Date(paste0(data, "-01")))


# Criando o gráfico de séries temporais
ggplot(rio_grande1, aes(x = data, y = prec_produt)) +
  geom_line() +
  labs(title = "Preço do Produto ao Longo do Tempo",
       x = "Data",
       y = "Preço do Produto") +
  theme_minimal()

# Plotando a FAC
acf(rio_grande1$prec_produt, main="Função de Autocorrelação (FAC)")
# Plotando a FACP
pacf(rio_grande1$prec_produt, main="Função de Autocorrelação Parcial (FACP)")
# Dividindo a série temporal para validação cruzada
splits <- time_series_split(
  rio_grande1,
  assess = "12 months",
  cumulative = TRUE
)

# Extraindo os dados de treino e teste
train_data <- training(splits)
test_data <- testing(splits)

# Criando o gráfico de séries temporais com destaque para os períodos de treino e teste
ggplot() +
  geom_line(data = train_data, aes(x = data, y = prec_produt), color = "blue") +
  geom_line(data = test_data, aes(x = data, y = prec_produt), color = "red") +
  labs(title = "Preço do Produto ao Longo do Tempo",
       x = "Data",
       y = "Preço do Produto",
       caption = "Período de treino em azul, período de teste em vermelho") +
  theme_minimal()

# Definindo o modelo ARIMA

model_arima <- arima_reg(
  seasonal_period = 12,
  non_seasonal_ar = 2,
  non_seasonal_differences = 1,
  non_seasonal_ma = 2
) %>%
  set_engine("arima") %>%
  fit(prec_produt ~ data, data = training(splits))

# Definindo a especificação completa do modelo ARIMA
model_spec <- arima_reg(
  seasonal_period = 12,
  non_seasonal_ar = 3,
  non_seasonal_differences = 1,
  non_seasonal_ma = 3
) %>%
  set_engine("arima") %>%
  fit(prec_produt ~ data, data = training(splits))

# Modelo simples ARIMA
model_simp <- arima_reg(
  non_seasonal_ar = 2,
  non_seasonal_differences = 1,
  non_seasonal_ma = 1
) %>%
  set_engine("arima") %>%
  fit(prec_produt ~ data, data = training(splits))

# Criando a tabela de modelos
model_to <- modeltime_table(model_arima, model_spec, model_simp)

# Calibrando os modelos
calib_to <- model_to %>%
  modeltime_calibrate(testing(splits))

# Obtendo a acurácia dos modelos
calib_to %>% modeltime_accuracy()

# salvando os residuos 
residuals <- calib_to %>% modeltime_residuals()

# Teste de autocorrelação (Ljung-Box)
ljung_box_test <- residuals %>%
  group_by(.model_id) %>%
  summarise(p_value = Box.test(.residuals, type = "Ljung-Box")$p.value)

print(ljung_box_test)

# Verificação de normalidade dos resíduos
shapiro_test <- residuals %>%
  group_by(.model_id) %>%
  summarise(p_value = shapiro.test(.residuals)$p.value)

print(shapiro_test)

# Se o valor p > 0,05 (bom), isso implica que a distribuição dos dados não é
# significativamente diferente da distribuição normal. Em outras palavras,
# podemos assumir a normalidade.

# Previsão dentro do conjunto de teste
calib_forecast <- calib_to %>%
  modeltime_forecast(
    new_data = testing(splits),
    actual_data = rio_grande1
  )

# Visualização da previsão dentro do conjunto de teste com linhas mais espessas
calib_forecast %>%
  plot_modeltime_forecast(.interactive = FALSE) +
  theme_minimal()


# Previsão do futuro apenas com os modelos bem-sucedidos
fut_model_to <- calib_to %>%
  modeltime_refit(rio_grande1) %>%
  modeltime_forecast(
    h = "12 months",
    actual_data = rio_grande1
  )

# Visualização da previsão futura com linhas mais espessas
fut_model_to %>%
  plot_modeltime_forecast(.interactive = FALSE) +
  theme_minimal()

# limpando a memoria:
rm(list = ls())
#-------------------------------------------------------------------------------
# Modelo que visa indentificar assimetria na transmissão dos preços do etanol----
#-------------------------------------------------------------------------------
# passos gerais para regressao de series de tempo:
# buscando dados
library(readxl)
# install.packages("dplyr","lubridate")
library(dplyr)
library(lubridate)
dados <- read_excel("dados.xlsx", 
                    col_types = c("date", "numeric", "numeric","numeric"))
dados |> glimpse()
# filtrando dados---------------------------------------------------------------

rio_grande1 <- dados |> dplyr::select(data, numero, prec_revenda , prec_produt  ) |> 
  dplyr::rename(revenda=prec_revenda,
                prec_produt=prec_produt)

# Agrupar por mês e ano, e calcular a media mensal
rio_grande1 <- rio_grande1 %>%
  mutate(mes = floor_date(data, "month")) %>% # para agrupar meses iguais 
  group_by(mes) %>%
  summarise(
    total_postos = mean(numero, na.rm = TRUE),
    revenda = mean(revenda, na.rm = TRUE),
    prec_produt = mean(prec_produt, na.rm = TRUE)
  )

?floor_date
# Visualizar os resultados
print(rio_grande1)

# fazendo as variaveis do modelo
rio_grande1 <- rio_grande1 %>%
  dplyr::mutate(
    diff_revenda = c(NA, diff(revenda)),
    diff_prec_produt = c(NA, diff(prec_produt)),
    diff_prec_produt_positivo = ifelse(diff_prec_produt < 0, 0, diff_prec_produt),
    diff_prec_produt_positivo_c = cumsum(ifelse(is.na(diff_prec_produt_positivo), 0, diff_prec_produt_positivo)),
    diff_prec_produt_negativo = ifelse(diff_prec_produt >= 0, 0, diff_prec_produt),
    diff_prec_produt_negativo_c = cumsum(ifelse(is.na(diff_prec_produt_negativo), 0, diff_prec_produt_negativo))
  )

## ECM
rio_grande1 <- rio_grande1 |> 
  dplyr::mutate(ecmat = lm(revenda~prec_produt)$residuals,
                ecmat_positivo = ifelse(ecmat < 0, 0, ecmat),
                ecmat_negativo = ifelse(ecmat >= 0, 0, ecmat))

# declarando data 
# Criando a coluna de data
rio_grande11 <- rio_grande1 %>%
  mutate(data = seq(from = as.Date("2005-01-01"), by = "month", length.out = n()))
# grafico ----------------------------------------------------------------------
# install.packages("ggplot2")
library(ggplot2)
ggplot(rio_grande11, aes(x = data)) +
  geom_line(aes(y = revenda, color = "Revenda")) +
  geom_line(aes(y = prec_produt, color = "Produtor")) +
  labs(title = "Preços do Etanol em Rio Grande-RS",
       y = "Valores",
       x = "Ano",
       color = "Variável") +
  theme_minimal()

# grafico da diferenca----------------------------------------------------------
ggplot(rio_grande11, aes(x = data)) +
  geom_line(aes(y = diff_revenda, color = "Revenda")) +
  geom_line(aes(y = diff_prec_produt, color = "Produtor")) +
  labs(title = "Variação dos Preços",
       y = "Valores",
       x = "Ano",
       color = "Variável") +
  theme_minimal()

# Criando um objeto
ts_data <- ts(rio_grande1[, c("total_postos", "revenda", "prec_produt",
                              "diff_prec_produt","diff_revenda",
                              "diff_prec_produt_positivo_c","diff_prec_produt_negativo_c",
                              "ecmat_positivo","ecmat_negativo")], start = c(2005, 1), frequency = 12)
# testes de estacionariedade----------------------------------------------------
# install.packages("urca")
library(urca)
# Realizando o teste ADF
help(ur.df)
# revenda 
ts_data[, "revenda"] |> ur.df( type = "none", selectlags = "AIC") |> summary()
ts_data[, "revenda"] |> ur.df( type = "drift", selectlags = "AIC") |> summary() # constante
ts_data[, "revenda"] |> ur.df( type = "trend", selectlags = "AIC") |> summary()  # trend
# primeira diferenca
ts_data[, "revenda"] |> diff() |>ur.df( type = "none", selectlags = "AIC") |> summary()

# distribuicao 
ts_data[, "prec_produt"] |> ur.df( type = "none", selectlags = "AIC") |> summary()
ts_data[, "prec_produt"] |> ur.df( type = "drift", selectlags = "AIC") |> summary() # constante
ts_data[, "prec_produt"] |> ur.df( type = "trend", selectlags = "AIC") |> summary()  # trend
# primeira diferenca
ts_data[, "prec_produt"] |> diff() |>ur.df( type = "none", selectlags = "AIC") |> summary()
#---- modelo de engle-granger---------------------------------------------------
engle= (lm(revenda~prec_produt,data = ts_data))$residuals
engle |> ur.df( type = "none", selectlags = "AIC") |> summary()

# se o residuo é estacionário, se tem cointegração....
#-------------------------------------------------------------------------------
# defasagens 
library(vars)
defa=VARselect(c(ts_data[, "revenda"],ts_data[, "prec_produt"]), lag.max = 12, type='const')
defa

# Converter ts_data em dataframe
df <- lm(diff_revenda~diff_prec_produt_positivo_c+ diff_prec_produt_negativo_c+
           ecmat_positivo+ecmat_negativo,data = ts_data)

summary(df)
# install.packages("car")
library(car)
help(linearHypothesis)
# testanto assimetria no curto prazo "H0= repasses simetricos; H1: assimetricos
test1=linearHypothesis(df, "diff_prec_produt_positivo_c = diff_prec_produt_negativo_c")
test1
# testanto assimetria no longo prazo "H0= repasses simetricos; H1: assimetricos
test2=linearHypothesis(df, "ecmat_positivo = ecmat_negativo")
test2

rm(list = ls())
#-------------------------------------------------------------------------------








# DECLARAR DIRETORIO () caso a versao nao rode "https://cran.r-project.org/bin/windows/base/old/"
setwd("C:/Econometria_esalq/Econometria_Esalq/Aula 6") # OU "Ctrl+Shif+H"
# importando dados:
#install.packages("modeltime","timetk","tidymodels","tidyverse","lubridate","ggplot2")
library(modeltime)
library(timetk)
library(tidymodels)
library(tidyverse)
library(lubridate)
library(ggplot2)
# importando dados 
library(readxl)
dados_mensais <- read_excel("clima.xlsx") |> janitor::clean_names()
dplyr::glimpse(dados_mensais)
# plotando o grafico: pelos pacotes do modeltime e timetk ----------------------
# precipitacao
dados_mensais|> plot_time_series(data,precipitacao)
# temperatura
dados_mensais|> plot_time_series(data,temperatura)
# ggplot: ----------------------------------------------------------------------
# precipitacao :
ggplot(dados_mensais, aes(x = data, y = precipitacao)) +
  geom_line() +
  labs(title = "Precipitação ao Longo do Tempo", x = "Data",
       y = "Precipitação (mm)") +
  theme_minimal()
# temperatura :
ggplot(dados_mensais, aes(x = data, y = temperatura)) +
  geom_line(color = "red") +
  labs(title = "Temperatura ao Longo do Tempo", x = "Data",
       y = "Temperatura") +
  theme_minimal()
# Avalicao dos componentes da serie --------------------------------------------
#install.packages(forecast) 
library(forecast) 

# Convertendo os dados de temperatura para uma série temporal
dados_ts <- ts(dados_mensais$temperatura, start = c(2001, 1), frequency = 12)

# Decompondo a série temporal
decomposicao <- decompose(dados_ts)

# Verificando os componentes decompostos
plot(decomposicao)
# teste de estacionariedade-----------------------------------------------------
library(readxl)
IPC <- read_excel("IGP.xls") |> janitor::clean_names()
# transformando em serie de tempo:
ipc <- ts(IPC$igp_10_ago_1994_100, start = c(1993, 9), frequency = 12)
ipc # visualizando
# plotando em um grafico--------------------------------------------------------
plot.ts(ipc)
autoplot(ipc)
# realizando os testes de estacionariedade--------------------------------------
# install.packages("urca")
library(urca)
# Realizando o teste ADF
help(ur.df)
ipc |>log() |> ur.df( type = "none", selectlags = "AIC") |> summary()
ipc |>log() |> ur.df( type = "drift", selectlags = "AIC") |> summary() # constante
ipc |>log() |> ur.df( type = "trend", selectlags = "AIC") |> summary()  # trend
# KPSS
help(ur.kpss)
ipc |>log() |> ur.kpss(  type="mu",lags = "long") |> summary() # constante
ipc |>log() |> ur.kpss(  type="tau",lags = "long") |> summary() # trend
ipc |>log() |> diff() |> ur.kpss(  type="mu", lags = "long") |> summary() # primeira diferenca



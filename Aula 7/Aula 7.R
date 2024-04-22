# declarar diretorio
setwd("C:/Econometria_esalq/Econometria_Esalq/Aula 7")
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
  mutate(mes = floor_date(data, "month")) %>%
  group_by(mes) %>%
  summarise(
    total_postos = mean(numero, na.rm = TRUE),
    revenda = mean(revenda, na.rm = TRUE),
    prec_produt = mean(prec_produt, na.rm = TRUE)
  )

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
#-------------------------------------------------------------------------------

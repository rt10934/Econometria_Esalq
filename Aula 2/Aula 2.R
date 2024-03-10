w################################################################################
################################################################################
##### Departamento de Economia, Administração e Sociologia - LES/ESALQ/USP #####
###########       Aula Pática de Econometria - RStudio            ##############


#1- Apresentação do RStudio
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(lmtest)
library(carData)

# install.packages("wooldridge")
data("wage1", package = "wooldridge")
data("ceosal1", package = "wooldridge")

### ESTATISTICA DESCRITIVA

summary(wage1)
summary(ceosal1)
summary(vote1)
sd(wage1$wage)
var(wage1$wage)

### REGRESSAO SIMPLES -----
## EXEMPLO 1 ------------------------------------------------------------------
## Verificar o impacto da educação no salário. ----
#Usaramos as variáveis:
#wage - salário médio por hora, em R$
#educ - anos de estudo do funcionário.
reg<-lm(wage ~ educ, data = wage1)
summary(reg)
# interpretacao: O aumento de um ano de estudo aumenta o os rendimentos por hora em 
# 0,54 centavos de dolar:

#GRÁFICO

ggplot(data=wage1,mapping= aes(x=educ, y = wage))+
  geom_point(shape=21, col="red", fill="blue", size= 2.8)+
  geom_smooth(method = "lm", formula = y ~x, se=FALSE, 
              col= "red")+ 
  stat_regline_equation(aes(label=paste(..eq.label..,
                                        ..rr.label.., sep="~~~~")),
                        label.x = 0, label.y = 25)+
  labs(x= "Anos de Estudo", y= "Salário", title = "Exemplo 1")+
  
  theme_light()

#Testar homocedasticidade

#H0: Há homocedasticidade: p-valor > 0.05
#Ha: Não há homocedasticidade: p-valor <= 0.05

bptest(reg)

#Teste de normalidade dos resíduos

#H0:Distribuição normal : p-valor > 0.05
#Ha: Não há distribuição normal: p-valor <= 0.05

shapiro.test(reg$residuals)

# Independência dos Resíduos:
#H0: : p-valor > 0.05
#Ha: : p-valor <= 0.05

### Incorporação da Não-Linearidade na Regressão Simples ####

##EXEMPLO 2 --------------------------------------------------------------------

#Podemos estimar o modelo Log-Log fazendo:

# OBS: Zero nao pode ser logaritimizado, entao aqui filtraremos para poder usar a mesma base:
wage11 <- wage1 |> dplyr::filter(!(educ==0))
#Usaremos: 
#salary - salario do CEO, em milhares de R$
#sales - vendas da empresa, em milhões de R$
reg1 <- lm (log(wage) ~ log(educ), data = wage11)
summary(reg1)

# interpretacao: O aumento de um 1% em anos de estudo aumenta o os rendimentos por hora em 
# 0,825%.
#Testar homocedasticidade

#H0: Há homocedasticidade: p-valor > 0.05
#Ha: Não há homocedasticidade: p-valor <= 0.05

bptest(reg1)

#Teste de normalidade dos resíduos

#H0:Distribuição normal : p-valor > 0.05
#Ha: Não há distribuição normal: p-valor <= 0.05

shapiro.test(reg1$residuals)

# Independência dos Resíduos:
#H0: : p-valor > 0.05
#Ha: : p-valor <= 0.05

#O aumento em um ponto percentual nas vendas eleva o salario do CEO em 
#aproximadamente 0,257%


## EXEMPLO 3 -------------------------------------------------------------------

# Vamos estimar o modelo Log-Lin:
#log(salário)= bo+b1*educ+u

reg2 <- lm (log(wage) ~ (educ), data = wage1)
#Para visualizar as estimativas usamos summary()
summary(reg2)

#O aumento em um ano adicional de escolaridade aumenta o salário em 
#  8,27% (0,083*100) - semi elasticidade
0.082744*100

#Testar homocedasticidade

#H0: Há homocedasticidade: p-valor > 0.05
#Ha: Não há homocedasticidade: p-valor <= 0.05

bptest(reg1)

#Teste de normalidade dos resíduos

#H0:Distribuição normal : p-valor > 0.05
#Ha: Não há distribuição normal: p-valor <= 0.05

shapiro.test(reg1$residuals)

# Independência dos Resíduos:
#H0: : p-valor > 0.05
#Ha: : p-valor <= 0.05

dwtest(reg1)
durbinWatsonTest(reg1)



#EXEMPLO 4 ---------------------------------------------------------------------
#Podemos estimar o modelo Lin-Log fazendo:
reg3 <- lm (wage ~ log(educ), data = wage11)
summary(reg3)
# O aumento em 1% nos anos de estudos leva a um aumento na remuneracao por hora em 0,0533
# centavos de dolar
5.330/100

plot(reg3)

#Testar homocedasticidade

#H0: Há homocedasticidade: p-valor > 0.05
#Ha: Não há homocedasticidade: p-valor <= 0.05

bptest(reg3)

#Teste de normalidade dos resíduos

#H0:Distribuição normal : p-valor > 0.05
#Ha: Não há distribuição normal: p-valor <= 0.05

shapiro.test(reg3$residuals)

# Independência dos Resíduos:
#H0: : p-valor > 0.05
#Ha: : p-valor <= 0.05
#-------------------------------------------------------------------------------

# nivel 
summary(reg)
# log-log
summary(reg1)
# log-lin 
summary(reg2)
#lin-log
summary(reg3)

###############################################################
# Carregar a biblioteca stargazer
library(stargazer)

# Definir os modelos
models <- list(reg, reg1, reg2, reg3)

# Nomear os modelos
names(models) <- c("Modelo 1: Nível", "Modelo 2: Log-Log", "Modelo 3: Log-Lin", "Modelo 4: Lin-Log")

# Gerar a tabela conjunta de resultados
stargazer(models, type = "text")

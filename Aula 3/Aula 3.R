#_______________________________________________________________________________
## TEST t ----------------------------------------------------------------------
#_______________________________________________________________________________
data("ceosal1", package = "wooldridge")
dplyr::glimpse(ceosal1)
#4.II) Se ros aumenta em 50 pontos, qual e a variacao percentual prevista no salario?
# roe (retornos sobre o patrimonio liquido %)
# ros (retornos sobre o patrimonio liquido %)
reg<-lm(lsalary~lsales + roe + ros, data = ceosal1 )
summary(reg)
# modelo log-lin. logo:
# (\Delta x 0,0002417) x 100 => (50x0,0002417)x100 = 1,2085%

#4.III) Teste a hipotese nula de que ros nao tem efeito sobre o salario contra a 
# alternativa de que ros tem um efeito positivo. Faca o teste ao nivel de signi-
# ficancia de 10% 
# HIPOTESE=> H0: B3=0 e H1: B3>0

# Formula : tcal= (B3^ - B3)/sd(B3^)
sreg8<-summary(reg)
# calculo: 
(sreg8$coefficients[4]-0)/sreg8$coefficients[4,2]
# gl =  n -k-1
gl <- 209 -3 -1;gl
summary(reg8)
# RESPOSTA -----
# Portanto, ao nivel de significancia de 10% e 205 graus de liberdade(tcritico=1,6602), chegamos
# a conclusao de que o valor calculado para B3^ esta abaixo do valor critico, logo
# nao rejeitamos a hipotese nula, e o parametro da variavel nao e estatisticamente 
# significativo.

# Ajustando o modelo de regressão linear
model <- lm(lsalary ~ lsales + roe + ros, data = ceosal1)
#_______________________________________________________________________________
### TESTE F --------------------------------------------------------------------
#_______________________________________________________________________________
# Calculando o F-statistic
#SSR
SSR <- sum((fitted(model) - ceosal1$lsalary)^2) # Soma dos quadrados do erro
SSR
#SQE
SQE <- sum((fitted(model) - mean(ceosal1$lsalary))^2) #  # Soma dos quadrados da regressao
SQE 
# informacoes utilizadas:
p <- 3+1  # Número de regressores
n <- 209  # Número de observações
F_statistic <- (SQE / (p-1)) / (SSR / (n-p))
F_statistic
summary(model)
# outra forma:
R<-(summary(model))$r.squared

(R / (p-1)) / ((1-R) / (n-p))
#_______________________________________________________________________________
### Fatores de Inflação da Variância (VIF) ------------------------------------
#_______________________________________________________________________________
install.packages("regclass")
library(regclass)
VIF(model)
# Tirar a raiz quadrada do VIF informa quanto maior é o erro padrão do coeficiente estimado em relação ao caso em que esse preditor é independente dos outros preditores.

# Uma diretriz geral é que um VIF maior que 5 ou 10 seja grande, indicando que o modelo tem problemas para estimar o coeficiente. 
# No entanto, isto em geral não degrada a qualidade das previsões. Se o VIF for maior que 1/(1-R2), onde R2 é o R-quadrado múltiplo da regressão,
# então esse preditor está mais relacionado aos outros preditores do que à resposta.



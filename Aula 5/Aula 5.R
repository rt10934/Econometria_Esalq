#_______________________________________________________________________________
# VARIAVEIS DUMMY E Turnaround point:-------------------------------------------
#_______________________________________________________________________________
# QUESTAO 7.1)
# MODELO: DORMIR = B_0 + B_1 trabtot + B_2 educ + B_3 idade + B_4 idade^2 + B_5 masculino+ u
data("sleep75", package = "wooldridge")
dplyr::glimpse(sleep75)
model_sleep <- lm(sleep~totwrk+educ+age+agesq+male, data=sleep75)
summary(model_sleep)

#7.1 (I) Supondo todos os outros fatores iguais, existe evidencia de que os homens
# durmam mais que as mulheres? O quanto essa evidencia e forte?
# Resposta----------
# o coeficiente referente ao Homem tem valor 87,75, que indica que sim o homem 
# dorme 85,75 minutos a mais que uma mulher.

# Em que idade se os individuos dormem menos?----
# \partial horas/ \partial idade = B_3 + 2*B_4 idade
# ponto de maximo :  B_4 < 0 
# ponto de minimo : B_4 > 0 

# -B_3 + 2*B_4 idade
# 2*B_4 idade = B_3
# idade = B_3/( 2*B_4 )

(-model_sleep$coefficients[4])/(model_sleep$coefficients[5]*2)

# o ponto e* de minimo, o que indica que a idade em que menos se dorme e* 33,85 anos.

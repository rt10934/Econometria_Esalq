data("wage1", package = "wooldridge")

# Carregamento de pacotes necessários
library(dplyr)

# Supondo que o conjunto de dados 'wage1' esteja disponível no ambiente de trabalho
# Se não estiver, você precisaria carregar o conjunto de dados de alguma forma, como usando read.csv() ou readRDS()

# Visualização inicial da estrutura do conjunto de dados
glimpse(wage1)

# Etapa 1: Regressão com todas as variáveis relevantes (educ e exper)
# Esta regressão avalia o efeito da educação e experiência no salário
modelo_completo <- lm(wage ~ educ + exper, data = wage1)
summary(modelo_completo)

# A saída mostrará os coeficientes estimados para 'educ' e 'exper',
# que representam o efeito marginal dessas variáveis no salário.

# Etapa 2: Regressão omitindo a variável 'exper'
# Esta regressão apenas inclui 'educ' para estimar seu efeito sobre 'wage',
# o que pode levar a um viés devido à omissão de 'exper'
modelo_omitido <- lm(wage ~ educ, data = wage1)
summary(modelo_omitido)

# CORR
cor(wage1$educ,wage1$exper)
# A comparação entre os coeficientes de 'educ' nos dois modelos acima
# pode sugerir a presença de viés de variável omitida.

# Etapa 3: Regressão auxiliar para calcular o viés
# Aqui, 'educ' é regredido em 'exper' para entender a relação entre as duas variáveis
reg_auxiliar <- lm(educ ~ exper, data = wage1)
summary(reg_auxiliar)

# Etapa 4: Cálculo do viés
# Primeiro, obtemos os coeficientes das regressões anteriores
# NOTA: Substitua os valores a seguir pelos resultados obtidos nas regressões
beta1hat <- coef(modelo_omitido)["educ"] # Coeficiente de 'educ' no modelo omitido
beta2hat <- coef(modelo_completo)["exper"] # Coeficiente de 'exper' no modelo completo
deltatil <- coef(reg_auxiliar)["exper"] # Coeficiente de 'exper' na regressão de 'educ' em 'exper'

# Cálculo do viés de 'educ' quando 'exper' é omitida
beta1til <- beta1hat + beta2hat * deltatil

# Cálculo do viés
vies_beta1til <- beta1hat - beta1til

# Exibir o viés calculado
print(paste("O viés em beta1til é:", vies_beta1til))

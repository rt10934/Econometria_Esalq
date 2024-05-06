# Carregar a biblioteca dplyr para manipulação de dados
library(dplyr)

# Copiar o data frame original mtcars para um novo data frame mtcars1
mtcars1 <- mtcars

# Selecionar apenas as colunas mpg, cyl, disp e hp de mtcars1
mtcars1 <- mtcars1 |>
  select(mpg, cyl, disp, hp)

# Adicionar uma nova coluna chamada 'variavel', que é a soma de mpg e cyl
mtcars1 <- mtcars1 |>
  mutate(variavel = mpg + cyl)

# Exibir o data frame mtcars1
mtcars1

# Agrupar os dados por 'cyl' e calcular a média de 'hp', removendo valores NA
mtcars1 <- mtcars1 |>
  group_by(cyl) |>
  summarise(hp = mean(hp, na.rm = TRUE))

# Criar um novo data frame 'nomes' associando 'cyl' com categorias de potência
nomes <- data.frame("cyl" = c(4, 6, 8),
                    "classe" = c("baixa", "media", "alta"))

# Fazer uma junção completa entre mtcars1 e nomes baseada na coluna 'cyl'
dado <- full_join(mtcars1, nomes, by = "cyl")

# Exibir o data frame dado
dado

# Carregar a biblioteca tidyr para manipulação de dados
library(tidyr)

# Transformar o data frame para uma forma mais ampla, movendo categorias de 'classe' para cabeçalhos de coluna
dado1 <- dado |>
  select(-cyl) |>
  pivot_wider(names_from = classe, values_from = hp)

# Exibir os data frames dado e dado1
dado
dado1


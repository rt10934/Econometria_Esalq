# Slide 6-----------------------------------------------------------------------
# DECLARAR DIRETORIO 
setwd("D:/Econometria_esalq/Econometria_Esalq/aula_pos") # OU "Ctrl+Shif+H"

# instalando e liberando as funcoes do pacote:
# install.packages("janitor")
library(janitor)
# install.packages("dplyr")
library(dplyr)
# importando dados:
# fazendo importacao de csv com liberacao de pacote:
library(readr)
A801 <- read_delim("A801.csv", delim = ";", 
                   escape_double = FALSE, trim_ws = TRUE, 
                   skip = 10)
# Sem necessidade de liberar o pacote:
A801 <- readr::read_delim("A801.csv", delim = ";", 
                          escape_double = FALSE, trim_ws = TRUE, 
                          skip = 10)
# Importando dados para area de cultivo de soja XLS:
IBGE <- readxl::read_excel("IBGE.xlsx")
# importando do MAPABIOMAS:
MAPABIOMAS <- readxl::read_excel("MAPABIOMAS.xlsx")

# limpando os nomes:

A801 <- A801 |> janitor::clean_names() # limpando nomes com acentos e outros problemas
IBGE <- IBGE %>% janitor::clean_names()
MAPABIOMAS <- MAPABIOMAS |> janitor::clean_names()


# SALVAR OS DADOS ______________________________________________________________
#install.packages("openxlsx")
# library(openxlsx)
# salvando em CSV
write.csv(IBGE, "D:/Econometria_esalq/Econometria_Esalq/aula_pos/LIXO/BASE1.csv", row.names = FALSE)
# Carregue o pacote openxlsx:
# Exporte o data frame para um arquivo Excel
openxlsx::write.xlsx(IBGE, file = "D:/Econometria_esalq/Econometria_Esalq/aula_pos/LIXO/IBGE.xlsx")
# Salvando em formato RDS (formato do R)
saveRDS(IBGE,"D:/Econometria_esalq/Econometria_Esalq/aula_pos/LIXO/IBGE.rds")

# Slide 7 ----------------------------------------------------------------------
# principais funcoes do "dplyr"
dplyr::glimpse(MAPABIOMAS)

## filter: 
MAPABIOMAS_savana <- MAPABIOMAS |> dplyr::filter(level_4=="Savanna Formation")
rm(MAPABIOMAS_savana)

## select:
MAPABIOMAS_city <- MAPABIOMAS |> dplyr::select(city, level_4)
rm(MAPABIOMAS_city)

## mutate:
MAPABIOMAS_muta <- MAPABIOMAS |> dplyr::mutate(total = x2020+x2021)
MAPABIOMAS_muta <- MAPABIOMAS %>% dplyr::mutate(total = rowSums(dplyr::select(., x2020, x2021), na.rm = T)) # alternativo
rm(MAPABIOMAS_muta)

## group_by() e summarise():
MAPABIOMAS_sum <- MAPABIOMAS |>
  dplyr::group_by(city, class_id) |>
  dplyr::summarise(x1985 = sum(x1985, na.rm = T),
                   x1986 = sum(x1986, na.rm = T))

## arrange()
MAPABIOMAS_sum <- MAPABIOMAS_sum |> dplyr::arrange(desc(x1985))

## join()
dicionario <- data.frame(class_id=c(15,4,3),nome=c("bom", "medio", "ruim"))

MAPABIOMAS_sum <- MAPABIOMAS_sum |> dplyr::left_join(dicionario, by="class_id")
rm(MAPABIOMAS_sum, dicionario)

# Slide 8 ----------------------------------------------------------------------
# ALTERACAO DOS DADOS (pacote tidyr)
# install.packages("tidyr")
library(tidyr)
# usando pivot_longer
MAPABIOMAS1 <- MAPABIOMAS |>
  tidyr::pivot_longer(`x1985`:`x2021`,names_to = "ANO",
                      values_to = "Area")

# pivot_wider (desfazer o que foi feito)
MAPABIOMAS2 <- MAPABIOMAS1 |> 
  tidyr::pivot_wider(names_from=ANO,
                     values_from=Area)
rm(MAPABIOMAS2)

# extra (calculando a proporcao de area por tipo)
MAPABIOMAS1 <- MAPABIOMAS1 |>
  dplyr::group_by(city, ANO)|>
  dplyr::mutate(prop_area=(100*Area)/sum(Area,na.rm = T))|>
  dplyr::arrange(city, ANO)

MAPABIOMAS2 <- MAPABIOMAS1 |> 
  tidyr::pivot_wider(names_from=ANO,
                     values_from=c("Area","prop_area"))
rm(MAPABIOMAS2, MAPABIOMAS1)

# fill()
View(IBGE)
IBGE <-IBGE |> tidyr::fill(cod,municipio,ano)

# Slide 9 ----------------------------------------------------------------------
MAPABIOMAS |> dplyr::glimpse()
str(MAPABIOMAS)
options(scipen=999) 

# operacoes:
MAPABIOMAS1 <- MAPABIOMAS |>
  tidyr::pivot_longer(`x1985`:`x2021`,names_to = "ANO",
                      values_to = "Area")
MAPABIOMAS1 |> dplyr::glimpse()
## gsub()
MAPABIOMAS1 <- MAPABIOMAS1 |>
  dplyr::mutate(ANO=gsub("x","",ANO))
MAPABIOMAS1 |> dplyr::glimpse()

## as.numeric()
MAPABIOMAS1 <- MAPABIOMAS1 |>
  dplyr::mutate(ANO=as.numeric(ANO))
MAPABIOMAS1 |> dplyr::glimpse()
## ifelse:
MAPABIOMAS1 <- MAPABIOMAS1 |>
  dplyr::mutate(ANO_dummy=ifelse(ANO>1990,"Novo","Antigo"))
MAPABIOMAS1 |> dplyr::glimpse()

# Extras (caso sobre tempo)-----------------------------------------------------
# verificando as culturas agricolas, e filtrando para soja e para o total
IBGE |> dplyr::glimpse()

IBGE1 <- IBGE |>
  dplyr::rename(code_muni=cod,
                culturas=produto_das_lavouras_temporarias,
                area_total=area_plantada_hectares)|>
  dplyr::mutate(
    code_muni =as.numeric(code_muni),
    area_total =as.numeric(area_total))|>
  dplyr::filter(culturas%in%c("Total","Soja (em grão)") & code_muni>=100)

# selecionando as variaveis e transformando em coluna:
IBGE1 <- IBGE1 |>
  dplyr::select(code_muni, ano, culturas, area_total)|>
  tidyr::pivot_wider(names_from = "culturas", values_from = "area_total")|>
  janitor::clean_names()

# proporcao da area com soja:
IBGE1 <- IBGE1 |>
  dplyr::mutate(proporcao=100*soja_em_grao/total)

# baixando shapefile:
# install.packages("geobr")
# library(geobr)

mun <- geobr::read_municipality(code_muni = "all",year = 2020)
# juntando as bases:
IBGE1 <- dplyr::full_join(mun,IBGE1, by="code_muni") 
dplyr::glimpse(IBGE1)

# fazendo as categorias de cultivo de soja:
IBGE1 <- IBGE1 |>
  dplyr::mutate(
    categoria = cut(proporcao,
                    breaks = c(0, 20, 40, 60, 80, 100),
                    labels = c("Ate 20", "20 a 40", "40 a 60", "60 a 80", "Acima 80"))
  )
#install.packages("ggplot2")
#library(ggplot2)


IBGE1|> ggplot2::ggplot() + ggplot2::geom_sf(ggplot2::aes( fill=categoria),color="NA",show.legend = TRUE)+
  ggplot2::ggtitle("")+
  ggplot2::labs(fill="Percentual")+
  ggplot2::scale_fill_manual(values = c('#FFFF00','#FFC600','#FF8D00',
                                        '#FF5500','#FF1C00'))
# extra figuras ----------------------------------------------------------------
IBGE1 |> dplyr::glimpse()
# install.packages("esquisse")
esquisse::esquisser(IBGE1)

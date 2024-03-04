#_______________________________________________________________________________
# IMPORTACAO DE DADOS-----------------------------------------------------------
#_______________________________________________________________________________
# DECLARA DIRETORIO 
setwd("C:/Econometria_esalq/Econometria_Esalq/Aula 1") # OU "Ctrl+Shif+H"
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
# tirando a notacao cientifica:
options(scipen = 999)
#________________________________________________________________________
# Limpando os nomes:
#_______________________________________________________________________
str(A801)
#limpando com o paconte janitor:
# install.packages("janitor")
# library(janitor)
A801 <- A801 |> janitor::clean_names() # limpando nomes com acentos e outros problemas
IBGE <- IBGE |> janitor::clean_names()
MAPABIOMAS <- MAPABIOMAS |> janitor::clean_names()
# avaliando as variaveis que serao utilizados, verificando o formato:
str(A801)
# caso necessario alterar data:
# A801$data_medicao <- as.Date(A801$data_medicao) # mudando para formato data
A801$precipitacao_total_horario_mm <- as.numeric(A801$precipitacao_total_horario_mm  ) # mudando para o formato numerico
# Visualizando alteracoes:
str(A801)
head(A801)
#_______________________________________________________________________________
# SALVAR OS DADOS --------------------------------------------------------------
#_______________________________________________________________________________
# salvando em CSV
write.csv(IBGE, "C:/Econometria_esalq/LIXO/BASE1.csv", row.names = FALSE)
# Carregue o pacote openxlsx:
#install.packages("openxlsx")
# library(openxlsx)
# Exporte o data frame para um arquivo Excel
openxlsx::write.xlsx(IBGE, file = "C:/Econometria_esalq/LIXO/IBGE.xlsx")
# Salvando em formato RDS (formato do R)
saveRDS(IBGE,"C:/Econometria_esalq/LIXO/IBGE.rds")
#_______________________________________________________________________________
# ALTERACAO DOS DADOS (pacote tidyr)--------------------------------------------
#_______________________________________________________________________________
# resolvendo o problema com colunas com mesclagem:
# install.packages("tidyr")
# library(tidyr)
View(IBGE)
IBGE <-IBGE |> tidyr::fill(cod,municipio,ano)
# Transpondo colunas, de forma a virar painel de dados:
View(MAPABIOMAS)
# usando pivot_longer
MAPABIOMAS <- MAPABIOMAS |>
  tidyr::pivot_longer(`x1985`:`x2021`,names_to = "ANO",
                      values_to = "Area")
# usando pivot_wider (desfazer o que foi feito)
MAPABIOMAS1<-MAPABIOMAS

MAPABIOMAS1 <- MAPABIOMAS1 |> 
  tidyr::pivot_wider(names_from=ANO,
                     values_from=Area)
head(MAPABIOMAS1)
# comando para retirar letra "x":
head(MAPABIOMAS)
MAPABIOMAS$ANO <-gsub("x","",MAPABIOMAS$ANO)
head(MAPABIOMAS)
#_______________________________________________________________________________
# USANDO O PACOTE DPLYR PARA MANIPULACAO DE DADOS-------------------------------
#_______________________________________________________________________________
# usando o select para deixar somente as variaveis que serao utilizadas:
dplyr::glimpse(IBGE)
IBGE <- IBGE |> dplyr::select(cod,municipio,ano,produto_das_lavouras_temporarias,area_colhida_hectares)
dplyr::glimpse(A801)
A801 <- A801 |> dplyr::select(data_medicao,precipitacao_total_horario_mm)

# Usando a funcao "rename" para renomear os dados
IBGE <- IBGE |>
  dplyr::rename(code_muni=cod,
                culturas=produto_das_lavouras_temporarias,
                area_c=area_colhida_hectares)
A801 <- A801 |>
  dplyr::rename(data=data_medicao,
                precipitacao=precipitacao_total_horario_mm)
# usando a funcao "mutate" para mudar os dados
IBGE <- IBGE |>
  dplyr::mutate(code_muni=as.numeric(code_muni),
                ano=as.numeric(ano),
                area_c=as.numeric(area_c))
# usando o "filter" para filtrar as bases:
A801 <- A801 |> 
  dplyr::filter(data>="2019-01-01")
# verificando as culturas agricolas, e filtrando para soja e para o total
unique(IBGE$culturas)
IBGE <- IBGE |>
  dplyr::filter(culturas%in%c("Total","Soja (em grão)") &
                  code_muni>=100)
# montando bases separadas para base IBGE
IBGE_T <- IBGE |>
  dplyr::filter(culturas=="Total")|>
  dplyr::rename(area_total=area_c)|>
  dplyr::select(-culturas)

IBGE_S <- IBGE |>
  dplyr::filter(culturas=="Soja (em grão)")|>
  dplyr::rename(area_soja=area_c)|>
  dplyr::select(code_muni,area_soja)

# usando a funcao "full_joint" para juntar as bases:
IBGE <- dplyr::full_join(IBGE_T,IBGE_S,by="code_muni")

# utilizando o "summarise" para fazer a proporcao de area com soja:
IBGE <- IBGE |>
  dplyr::mutate(prop_soj=(area_soja/area_total)*100)
# juntando a um shapefile:
# install.packages("geobr")
# library(geobr)
mun <- geobr::read_municipality(code_muni = "all",year = 2020)
# juntando as bases:
IBGE <- dplyr::full_join(mun,IBGE, by="code_muni") 

dplyr::glimpse(IBGE)

IBGE <-  IBGE |>
  dplyr::select(municipio,code_muni,name_muni,code_state,abbrev_state,
                area_total,area_soja,prop_soj,geom)
# fazendo as categorias de cultivo de soja:
IBGE$categoria <- cut((IBGE$prop_soj),
                      breaks = c(0,20,40,60,80,100),
                      labels = c("Ate 20","20 a 40","40 a 60",
                                 "60 a 80", "Acima 80"))
IBGE|> ggplot2::ggplot() + ggplot2::geom_sf(ggplot2::aes( fill=categoria),color="NA",show.legend = TRUE)+
  ggplot2::ggtitle("")+
  ggplot2::labs(fill="Percentual")+
  ggplot2::scale_fill_manual(values = c('#FFFF00','#FFC600','#FF8D00',
                                                 '#FF5500','#FF1C00'))
# agrupando informacoes por estado:
IBGE <- IBGE  |> as.data.frame() |> dplyr::select(-geom)
dplyr::glimpse(IBGE)
# se usa as funcoes "group_by" e "summarise"
IBGE_ES <- IBGE |>
  dplyr::group_by(code_state,abbrev_state)|>
  dplyr::summarise(area_total=sum(area_total,na.rm = T),
                   area_soja =sum(area_soja,na.rm=T))|>
  dplyr::mutate(PERC=(area_soja/area_total)*100)
#_______________________________________________________________________________
# USANDO O ESQUISSE PARA FAZER GRAFICOS-----------------------------------------
#_______________________________________________________________________________
# usando o pacote esquisse para fazer graficos com o ggplot:
# install.packages("esquisse")
esquisse::esquisser(IBGE_ES)
# library(ggplot2)
# Criando os dados
df <- data.frame(x = 1:9, y = rep(1, 9),
                 label = c("O", "B", "R", "I", "G", "A", "D", "O","!"))
# Plotando o grafico
ggplot2::ggplot(df, ggplot2::aes(x, y)) +
  ggplot2::geom_text(ggplot2::aes(label = label), color = "blue", size = 24,
                     fontface = "bold") +
  ggplot2::xlim(0, max(df$x) + 1) +
  ggplot2::ylim(0, 2) +
  ggplot2::labs(x = "", y = "") 


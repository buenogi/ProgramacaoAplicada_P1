# PROVA 1 DE PROGRAMAÇÃO APLICADA A ESTATÍSTICA

# Exercício 1 ----------------------------------

matriz_A <- matrix(c(28,32,8,9,49,7,21,35,28,10,47,43,15,34,2,48,42,19,32,26,
                     45,44,39,50,26), nrow = 5, ncol = 5, byrow = T)

matriz_B <- matrix(c(0,26,3,8,30,35,12,19,27,27,27,24,12,17,29,31,36,40,35,8,
                     24,43,31,21,39), nrow = 5, ncol = 5, byrow = T)

matriz_C <- solve(matriz_A%*%t(matriz_B))

matriz_P <- matriz_B%*%(t(matriz_B)%*%matriz_B)%*%t(matriz_B)


# Alternativa A

autovetores <- eigen(matriz_P)[2]
sum(autovetores$vectors)


for(i in autovetores$vectors){
  for(j in autovetores$vectors){
    soma = soma+autovetores$vectors[i,j]
  }
}

# Alternativa B - verdadeira

sum(abs(diag(matriz_C)))

# Alternativa C

sum(lower.tri(matriz_A))

# Alternativa D

log(abs(det(matriz_A)),10)
log(abs(det(matriz_B)),10)
log(abs(det(matriz_A%*%matriz_B)),10)

# Alternativa E - verdadeira

max(diag(solve(matriz_A%*%t(matriz_B))))

# Exercício 2-------------------------------------------------------------------
library(tidyverse)
library(data.table)

chocolate <- fread(file = "Dados/chocolate.csv.gz")

head(chocolate)

# Alternativa A - Falsa

n_paises <- chocolate%>%
  count(local_compania)

# Alternativa B - Verdadeira

# Contando o número de características
for(i in 1:nrow(chocolate)){
  n_caracteristicas <-  length(str_split(as.list(chocolate$caracteristicas)[i], ",", simplify = T))
  chocolate$n_caracteristicas[i] <- n_caracteristicas
}

# Separação pelo número de ingredientes

chocolate <- chocolate %>%
  separate(ingredientes, into = c("n_ingredientes", "ingredientes"), sep = "- ")

sapply(chocolate, class)
# Número de chocolates com quatro ingredientes com duas características

chocolate%>%
  filter(n_ingredientes == "4" & n_caracteristicas == 2)%>%
  count()

# Alternativa C - Falsa

chocolate%>%
  filter(n_ingredientes == "5")%>%
  count()

# Alternativa D -  Falsa
# caracteristicas <- as.character(unlist(sapply(chocolate$caracteristicas, function(x) strsplit(x, ','))))
# caracteristicas<- as.data.frame(caracteristicas)
# características <- caracteristicas%>%
#   mutate(caracteristicas = str_remove_all(caracteristicas, " "))
# 
# contagem_características <- caracteristicas%>%
#   count(caracteristicas)
# 
# strings <- c("sweet", "nutty", "cocoa", "roasty","creamy", "earthy", "sandy", "fatty")
# 
# maiores <- contagem_características%>%
#   filter(caracteristicas %in% strings)
# sum(maiores$n)


maiores <- sort(table(unlist(strsplit(chocolate$caracteristicas, ", "))), decreasing = T)[1:8]
maiores<- as.data.frame(maiores)
sum(maiores$Freq)


# Alternativa E - Falso

# ADOCANTE <- chocolate%>%
#   mutate(qtdd_adocante = str_count(ingredientes, "S*"))
# 
# str_detect(chocolate$ingredientes, "S*")

sort(table(unlist(strsplit(chocolate$ingredientes, ","))))


#sort(table(unlist(strsplit(my_string, " "))), decreasing = TRUE)  # Get frequencies
#     is string   this      a   nice 
#      2      2      2      1      1
# Exercício 3-------------------------------------------------------------------
arte <- fread("Dados/Art.csv.gz")
arteMoma <- fread("Dados/Art_Moma.csv.gz") 

# Desconsiderar artistas sem nome e sem nacionalidade

arte$artist_name <- na.omit(arte$artist_name)
arte$artist_nationality <- na.omit(arte$artist_nationality)

# exemplos rápidos: https://sparkbyexamples.com/r-programming/remove-rows-with-na-in-r/

# Join

total_arte <- left_join(arte, arteMoma, by = "artist_unique_id")

# Alternativa A - Verdadeira

total_arte%>%
  group_by(artist_name)%>%
  summarise(n_exp_TheWhitney = sum(whitney_count_to_year))%>%
  arrange(desc(n_exp_TheWhitney))

# Alternativa B - Verdadeira

nacionalidades <- total_arte%>%
  filter(artist_nationality == "Swiss"|
           artist_nationality == "Mexican"|
           artist_nationality == "Japanese")%>%
  group_by(artist_name)


# Alternativa C - Falsa 

total_arte%>%
  filter(artist_nationality == "Swiss")%>%
  group_by(artist_name)%>%
  summarise(n_exposicoes = sum(whitney_count_to_year))

  
# Alternativa D - Falsa

Diferenca <- total_arte%>%
  filter(year == 2007)%>%
  group_by(artist_race_nwi)%>%
  summarise(n_paginas = sum(space_ratio_per_page_total))

diferenca = as.numeric(Diferenca[1,2])-as.numeric(Diferenca[2,2])

# Alternativa E - Falsa 

total_arte%>%
  filter(whitney_count_to_year >= 1)%>%
  group_by(book, artist_name)%>%
  count()

# Exercício 4 ------------------------------------------------------------------
# Leitura dos dados
refugiadosPais <- fread("Dados/refugiados_pais.csv.gz")
refugiados <- fread("Dados/refugiados.csv.gz")

  # Alternativa A - Verdadeira ---------------------------------------------------

# 1º filtragem por ano
refugiados_2006 <- refugiados%>%
  filter(ano == 2006)
# Join para identificação das regiões de origem
origem_2006  <- left_join(refugiados_2006,refugiadosPais,
                          by = c("id_origem" = "id"))
# Join para identificação das regiões de destino
destino_2006  <- left_join(refugiados_2006,refugiadosPais,
                           by = c("id_destino" = "id"))
# Alteração dos nomes das colunas
colnames(origem_2006) <- c("ano", "id_origem", "id_destino", "refugiados", 
                           "nome_origem", "regiao_origem", "subregiao_origem")
colnames(destino_2006) <- c("ano", "id_origem", "id_destino", "refugiados", 
                            "nome_destino", "regiao_destino", 
                            "subregiao_destino")

# Full join  com todas as informações necessárias
origem_destino <- full_join(origem_2006, destino_2006, by = c("ano", 
                                                              "id_origem",
                                                              "id_destino",
                                                              "refugiados"))
# Remoção de valores nulos
origem_destino <- na.omit(origem_destino)

# Contagem por nº de refugiados por região de origem e de destino
origem_destino <- origem_destino%>%
  group_by(regiao_origem, regiao_destino)%>%
  summarise(n_refugiados  = sum(refugiados))

# Formatação similar a matriz

matriz <- origem_destino%>%
  pivot_wider(names_from = regiao_destino, values_from = n_refugiados)

matriz[is.na(matriz)] <- 0
matriz

  # Alternativa B - Falsa --------------------------------------------------------


# Join para identificação das regiões de origem
origem <- left_join(refugiados,refugiadosPais,
                          by = c("id_origem" = "id"))
# Join para identificação das regiões de destino
destino  <- left_join(refugiados,refugiadosPais,
                           by = c("id_destino" = "id"))
# Alteração dos nomes das colunas
colnames(origem) <- c("ano", "id_origem", "id_destino", "refugiados", 
                           "nome_origem", "regiao_origem", "subregiao_origem")
colnames(destino) <- c("ano", "id_origem", "id_destino", "refugiados", 
                            "nome_destino", "regiao_destino", 
                            "subregiao_destino")

# Full join  com todas as informações necessárias
total_origem_destino <- full_join(origem, destino, by = c("ano", 
                                                              "id_origem",
                                                              "id_destino",
                                                              "refugiados"))

altB <- total_origem_destino%>%
  filter(ano >= 1979)%>%
  group_by(id_origem, id_destino)%>%
  summarise(n_refugiados = sum(refugiados))

altB%>%
  filter(id_origem == "AFG" & id_destino =="CAN")

altB%>%
  filter(id_origem == "PAK" & id_destino =="CAN")

  # Alternativa C - Falsa --------------------------------------------------------

Alt_C<- total_origem_destino%>%
  filter(ano == 1965)%>%
  group_by(id_origem,subregiao_origem)%>%
  summarise(n_refugiados = sum(refugiados))%>%
  arrange(desc(n_refugiados))
Alt_C<- na.omit(Alt_C)
  




  # Alternativa D - Falsa --------------------------------------------------------
total_origem_destino%>%
  filter(ano >= 1982)%>%
  group_by(nome_destino)%>%
  summarise(n_refugiados_recebidos = sum(refugiados))%>%
  arrange(desc(n_refugiados_recebidos))


  # Alternativa E - Verdadeira --------------------------------------------------- 
total_origem_destino%>%
  group_by(id_destino)%>%
  summarise(n_refugiados = sum(refugiados))%>%
  filter(n_refugiados >= 5382652)%>%
  count()

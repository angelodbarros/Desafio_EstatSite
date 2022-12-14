# Carregando pacotes que ser?o utilizados
library(readr)
library(tidyverse)
library(hrbrthemes)
library(scales)
library(lubridate)


# Carregando dataset Fifa
fifa19 <- read_csv("C:/Users/angel/Downloads/ESTUDOS DIVERSOS/Desafio EstatSite/Bases de Dados/fifa19.csv")

######## 1.Se fossemos classificar a for?a dos clubes de acordo com a m?dia do campo Overall de seus jogadores,
######## considerando somente clubes com pelo menos 25 jogadores, qual seria o clube mais forte? E o mais fraco?

glimpse(fifa19)

clubes_25_jogadores <- fifa19 %>% 
  group_by(Club) %>% 
  count() %>% 
  filter(n >= 25)

clubes_25_jogadores %>% 
  ggplot(aes(n)) +
  geom_freqpoly()

min(clubes_25_jogadores$n)

dataset_overall <- fifa19 %>% 
  inner_join(clubes_25_jogadores, by = "Club") %>% 
  select(-n)

dataset_overall <- dataset_overall %>% 
  group_by(Club) %>% 
  summarise(Ovr_medio = mean(Overall)) %>% 
  arrange(desc(Ovr_medio))

# clube mais forte
dataset_overall[1,1]

# clube mais fraco
dataset_overall[nrow(dataset_overall),1]

######## 2.Se fossemos olhar somente para os 20 melhores jogadores de cada sele??o, qual na??o teria o time
######## mais forte utilizando o crit?rio da m?dia do Overall de seus jogadores? Em outras palavras, 
######## filtre somente os 20 melhores jogadores de cada sele??o, sendo o crit?rio de "melhor" o campo
######## Overall, e, utilizando o mesmo campo, verifique qual sele??o tem a melhor m?dia.

melhores_da_selecao <- fifa19 %>% 
  group_by(Nationality) %>% 
  arrange(desc(Overall)) %>% 
  slice_head(n = 20) %>% 
  ungroup()

melhores_da_selecao %>% 
  count(Nationality) %>% 
  arrange(desc(n))

melhores_da_selecao %>% 
  group_by(Nationality) %>% 
  summarise(Ovr_medio_selecao = mean(Overall)) %>% 
  arrange(desc(Ovr_medio_selecao))


####### 3.Neste exerc?cio, considere o campo Release Clause como sendo o valor do jogador. Considerando
####### somente os clubes que possuem mais de 25 jogadores, quais s?o os 5 clubes mais valiosos?


clubes_valiosos <- fifa19 %>%
 mutate(unidade_contagem_valor = str_sub(`Release Clause`,-1),
        release_clause = as.numeric(str_sub(`Release Clause`,2,-2)))

clubes_valiosos %>% 
  count(unidade_contagem_valor)

clubes_valiosos <- clubes_valiosos %>%
  mutate(release_clause = ifelse(unidade_contagem_valor == "M",
                                 release_clause*1000000,ifelse(unidade_contagem_valor == "K", release_clause*1000,release_clause)))

clubes_valiosos <- clubes_valiosos %>% 
  inner_join(clubes_25_jogadores, by = "Club") %>% 
  select(-n)

clubes_valiosos %>% 
  group_by(Club) %>% 
  summarise(valor_mercado = mean(release_clause)) %>% 
  arrange(desc(valor_mercado)) %>% 
  slice_head(n = 5)

####### 4.Imagine que voc? ? diretor de um clube e possui um certo or?amento para comprar 11 jogadores que
####### ir?o compor o time titular. Cada jogador ? contratado de acordo com a release clause. O presidente
####### deseja trazer jogadores jovens, sendo assim, pede que voc? n?o contrate ningu?m acima de 29 anos. O
####### presidente tamb?m demanda que voc? n?o traga nenhuma estrelinha que possa conturbar o elenco, sendo
####### assim, o pre?o m?ximo a ser pago por um jogador n?o pode ultrapassa os 15 milh?es de euros. Quais
####### s?o os 11 jogadores de maior Overall que voc? consegue trazer para seu clube? Isto, ? claro, 
####### seguindo as restri??es or?ament?rias e et?rias impostas pelo seu chefe.

contratacoes <- fifa19 %>%
  mutate(unidade_contagem_valor = str_sub(`Release Clause`,-1),
         release_clause = as.numeric(str_sub(`Release Clause`,2,-2))) %>% 
  mutate(release_clause = ifelse(unidade_contagem_valor == "M",
                                 release_clause*1000000,ifelse(unidade_contagem_valor == "K", release_clause*1000,release_clause)))

onze_contratacoes <- contratacoes %>% 
  filter(Age < 29 & release_clause < 15000000) %>% 
  arrange(desc(Overall)) %>% 
  slice_head(n = 11) %>% 
  select(Name, Age, Overall, Club, release_clause)

onze_contratacoes

####### 5.Utilizando a tabela com os jogadores que voc? selecionou no exerc?cio anterior, crie uma coluna
####### chamada High_Price que recebe 1 se a Release Clause do jogador est? acima da mediana dos 11
####### selecionados, e 0 caso contr?rio.

onze_contratacoes <- onze_contratacoes %>% 
  mutate(high_price = ifelse(release_clause > median(release_clause), 1, 0))

###### 6.Apresente os histogramas com a distribui??o do peso, idade e sal?rio dos jogadores que voc?
###### escolheu no exerc?cio 4.

histogramas <- contratacoes %>% 
  filter(Age < 29 & release_clause < 15000000) %>% 
  arrange(desc(Overall)) %>% 
  slice_head(n = 11)

histogramas <- histogramas %>%
  mutate(
    unidade_medida_peso = str_sub(Weight,-3),
         peso = as.numeric(str_sub(Weight, 1, -4)),
         unidade_medida_salario = str_sub(Wage, -1),
         salario = as.numeric(str_sub(Wage,2, -2)))


histogramas %>% 
  ggplot(aes(peso)) +
  geom_histogram(binwidth = 10, fill ="green", alpha = 0.5) +
  theme_ipsum_es() +
  scale_x_continuous(breaks = extended_breaks(n = 10)) +
  labs(title = "Histograma - Peso dos Jogadores (lbs)",
       x = "Peso",
       y = "Contagem")

histogramas %>% 
  ggplot(aes(Age)) +
  geom_histogram(binwidth = 1, fill = "green", alpha = 0.5) +
  theme_ipsum_es() +
  scale_x_continuous(breaks = extended_breaks(n = 10)) +
  labs(title = "Histograma - Idade dos Jogadores (lbs)",
       x = "Idade",
       y = "Contagem")

histogramas %>% 
  ggplot(aes(salario)) +
  geom_histogram(binwidth = 10, fill = "green", alpha = 0.5) +
  theme_ipsum_es() +
  scale_x_continuous(breaks = extended_breaks(n = 10)) +
  labs(title = "Histograma - Sal?rio dos Jogadores\n(mil Euros)",
       x = "Sal?rio",
       y = "Contagem")


#Dataset Iris

Iris <- read_csv("C:/Users/angel/Downloads/ESTUDOS DIVERSOS/Desafio EstatSite/Bases de Dados/Iris.csv")

######## 7.Atrav?s de um gr?fico de dispers?o (scatterplot), verifique se h? rela??o linear entre 
######## comprimento da p?tala (Petal Length) e o comprimento da s?pala (Sepal Length). Adicione tamb?m 
######## diferentes cores aos pontos de acordo com a esp?cie da flor. A resposta aqui ? somente o gr?fico,
######## n?o se preocupe em fazer an?lises mais aprofundadas.

glimpse(Iris)

Iris %>% 
  ggplot(aes(SepalLengthCm, PetalLengthCm)) +
  geom_point(aes(color = Species)) +
  theme_ipsum_es() +
  labs(title = "Comprimento da S?pala vs\nComprimento da P?tala (cm)",
       color = "Esp?cies",
       x = "Comprimento - S?pala",
       y = "Comprimento - P?tala")

######## 8.Primeiro, apague a substring "Iris-" da coluna Species. Em seguida, adicione 3 novas colunas ?
######## tabela inicial, sendo que cada coluna receber? uma dummy referente a cada uma das species. Ou
######## seja, voc? deve criar uma coluna chamada Dummy_Setosa, que recebe 1 se a flor for da esp?cie 
######## Setosa e 0 caso contr?rio. O mesmo para as demais esp?cies.

iris_dummy <- Iris %>% 
  mutate(Species = str_sub(Species,6,-1))

iris_dummy <- iris_dummy %>% 
  mutate(Dummy_Setosa = if_else(Species == "setosa", 1, 0),
         Dummy_Versicolor = if_else(Species == "versicolor", 1, 0),
         Dummy_Virginica = if_else(Species == "virginica", 1, 0))


# Dataset MGLU3 e LREN3 

######## 9.Mostre atrav?s de um gr?fico de linhas a evolu??o do pre?o de fechamento das duas a??es durante
######## os anos de 2017, 2018 e 2019. No mesmo gr?fico, trace um gr?fico de linhas pontilhadas com a
######## evolu??o do pre?o de abertura das duas a??es no mesmo per?odo. Utilize cores diferentes para cada
######## linha e insira uma legenda para as cores/linhas. A legenda deve ficar no canto inferior direito

renner <- read_csv("C:/Users/angel/Downloads/ESTUDOS DIVERSOS/Desafio EstatSite/Bases de Dados/LREN3.SA.csv")
magalu <- read_csv("C:/Users/angel/Downloads/ESTUDOS DIVERSOS/Desafio EstatSite/Bases de Dados/MGLU3.SA.csv")

glimpse(renner)

renner_agrupado <- renner %>% 
  mutate(Month = month(Date, label = TRUE),
         Year = as.character(year(Date)),
         Open = as.double(Open),
         Close = as.double(Close)) %>% 
  filter(Year %in% c(2017:2019))

renner_agrupado %>% 
  ggplot() +
  geom_line(aes(Date,Close,group = Year, color = Year)) +
  geom_line(aes(Date,Open,group = Year, linetype = Year)) +
  scale_y_continuous(breaks = breaks_extended(n = 6)) +
  labs(title = "LREN3 (2017-2020) - BRL",
       x = "Ano",
       y = "Pre?o da A??o (R$)") +
  theme_ipsum_es()

magalu_agrupado <- magalu %>% 
  mutate(Month = month(Date, label = TRUE),
         Year = as.character(year(Date)),
         Open = as.double(Open),
         Close = as.double(Close)) %>% 
  filter(Year %in% c(2017:2019))

magalu_agrupado %>% 
  ggplot() +
  geom_line(aes(Date,Close,group = Year, color = Year)) +
  geom_line(aes(Date,Open, group = Year, linetype = Year)) +
  labs(title = "MGLU3 (2017-2020) - BRL",
       x = "Ano",
       y = "Pre?o da A??o (R$)") +
  theme_ipsum_es()


#Datasets Compra e Cadastro

######## 10.A tabela COMPRAS possui as informa??es de todas as compras feitas pelos clientes da sua loja. Em
######## CADASTRO, voc? encontrar? as informa??es cadastrais dos seus clientes. Monte uma nova tabela
######## chamada RESUMO. Essa tabela ter? uma linha por cliente e as colunas ser?o os campos: Id, Idade,
######## Estado, Gasto_Total. As primeiras colunas s?o auto-explicativas e podem ser obtidas diretamente
######## na tabela COMPRAS. A ?ltima coluna deve trazer a soma de todas as compras feitas por cada cliente.
######## Essa tabela ? a primeira parte da resposta. A segunda parte ser? obter a soma, a m?dia e o desvio
######## padr?o dos gastos por estado. Isto ?, qual a soma, a m?dia e o desvio padr?o do campo Total_Gasto
######## para cada estado.


cadastro <- read_delim("C:/Users/angel/Downloads/ESTUDOS DIVERSOS/Desafio EstatSite/Bases de Dados/CADASTRO.csv", 
                       delim = ";", escape_double = FALSE, trim_ws = TRUE)

compras <- read_delim("C:/Users/angel/Downloads/ESTUDOS DIVERSOS/Desafio EstatSite/Bases de Dados/COMPRAS.csv", 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE)

glimpse(cadastro)
glimpse(compras)

resumo <- compras %>% 
  inner_join(cadastro, by = "Id") %>% 
  select(-c(Nome, Data_Compra))

resumo <- resumo %>% 
  group_by(Id) %>% 
  mutate(Gasto_Total = sum(Valor_Compra)) %>% 
  select(-Valor_Compra) %>% 
  ungroup()

metricas_por_UF <- resumo %>% 
  group_by(Estado) %>% 
  summarise(Gasto_UF = sum(Gasto_Total),
            Media_Gasto_UF = mean(Gasto_Total),
            Std_Dev_UF = sd(Gasto_Total))

metricas_por_UF

######## 11.Apresente a distribui??o dos campos num?ricos da tabela RESUMO atrav?s de um boxplot e um
######## histograma. Coloque legenda dos eixos x e y do gr?fico.

# Idade
resumo %>% 
  ggplot(aes(Idade)) +
  geom_histogram(binwidth = 10) +
  scale_x_continuous(breaks = extended_breaks(n = 10)) +
  labs(title = "Histograma - Idade dos Clientes",
       y = "Contagem") +
  theme_ipsum_es()

resumo %>% 
  ggplot(aes(Idade)) +
  geom_boxplot() +
  coord_flip() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        ) +
  labs(title = "Boxplot - Idade dos Clientes") +
  theme_ipsum_es()

resumo %>% 
  ggplot(aes(Gasto_Total)) +
  geom_histogram() +
  scale_x_continuous(breaks = breaks_extended(n = 8)) +
  labs(title = "Histograma - Gasto Total dos Clientes",
       y = "Contagem") +
  theme_ipsum_es() +
  theme(axis.text.x = element_text(angle = 45))

resumo %>% 
  ggplot(aes(Gasto_Total)) +
  geom_boxplot() +
  scale_x_continuous(breaks = breaks_extended(n = 8), labels = label_number()) +
  coord_flip() +
  labs(title = "Boxplot - Gasto Total dos Clientes",
       x = "Gasto Total") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  theme_ipsum_es()

###### 12.Sua empresa considera jovem os clientes com menos de 30 anos. A partir disso, elabore um gr?fico
###### de barras comparando o gasto m?dio de clientes jovens e velhos. Ou seja, a altura da barra ser? o
###### gasto m?dio do gasto de cada um dos grupos.

faixa_etaria <- resumo %>% 
  mutate(grupo_etario = if_else(Idade < 30,"Jovem", "Velho"))

faixa_etaria <- faixa_etaria %>% 
  group_by(grupo_etario) %>% 
  summarise(gasto_faixa_etaria = mean(Gasto_Total))

faixa_etaria %>% 
  ggplot(aes(grupo_etario,gasto_faixa_etaria)) +
  geom_bar(aes(fill = grupo_etario), stat = "identity") +
  scale_y_continuous(labels = label_number()) +
  labs(title = "Compara??o de Gasto M?dio por\nFaixa Et?ria (R$)",
       x = "",
       y = "Gasto M?dio (R$)",
       fill = "Grupo Et?rio") +
  theme_ipsum_es()
                     

######## 13.Crie uma fun??o que, dado um n?mero X, fa?a duas coisas: (1) retorna os n?meros pares de 1 a 9
######## que n?o fazem parte de X; (2) retorna uma mensagem indicando se o n?mero ? par ou ?mpar.
######## Exemplo: se passarmos o n?mero 239, a fun??o deve retornar 4, 6, 8 e "?mpar". Pode ser em forma
######## de duas mensagens ou uma mensagem com os n?meros e a defini??o de par ou ?mpar. A escolha ? sua.


testa_numero <- function(numero){
  # retorna pares n?o inclusos no numero
  pares <- c()
  for(i in 1:9){
    if(i %% 2 == 0){
      pares[length(pares)+1] <- i
      pares
    } 
  }
  digitos <- as.integer(str_split(numero,"")[[1]])
  print(setdiff(pares,digitos))
  # indica se ? par ou impar
  if (numero %% 2 != 0)
    return("N?mero ?mpar")
  else
    return("N?mero par")
}

######## 14.Escreva uma fun??o que receba uma string e retorne a mesma string sem nenhuma letra repetida.
######## Exemplo: se a fun??o receber a palavra "casa", ela deve retornar "cas"

remove_duplicidade <- function(palavra){
  palavra_separada <- as.vector(str_split(palavra, pattern = ""))[[1]]
  palavra_sem_repeticao <- c()
  for(i in 1:length(palavra_separada)){
    if(palavra_separada[i] %in% palavra_sem_repeticao){
      next
    } else {
      bridge <- palavra_separada[i]
      palavra_sem_repeticao[i] <- bridge
      palavra_sem_repeticao <- as.vector(na.omit(palavra_sem_repeticao))
    }
  }
  palavra_sem_repeticao
}

remove_duplicidade("chafariz")

######## 15.Escreva uma fun??o chamada return_percentile que receba como entrada um array de dimens?o (N,1) e
######## um percentile qualquer, e retorne o valor referente a este percentile. N?o vale usar as fun??es
######## percentile, quartile, etc.

percentil <- function(vetor, percentil){
  x <- array(data = vetor, dim = c(length(vetor), 1))
  x <- x[order(x[,1]),]
  if(length(x) %% 2 == 0){
  posicao <- x[(percentil/100)*length(x)]  
  } else {
    if(((percentil/100)*length(x)-as.integer((percentil/100)*length(x)))>=0.5){
      posicao <- x[(percentil/100)*length(x)+1] 
    } else{
      posicao <- x[(percentil/100)*length(x)-1] 
    }
  }
  posicao
}
vetor <- c(56,45,69,78,72,94,82,80,63,59,11)
percentil(vetor,percentil = 50)

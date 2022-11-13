# Carregando pacotes que serão utilizados
library(readr)
library(tidyverse)
library(hrbrthemes)
library(scales)
library(lubridate)


# Carregando dataset Fifa
fifa19 <- read_csv("C:/Users/angel/Downloads/ESTUDOS DIVERSOS/Desafio EstatSite/Bases de Dados/fifa19.csv")

######## 1.Se fossemos classificar a força dos clubes de acordo com a média do campo Overall de seus jogadores,
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

######## 2.Se fossemos olhar somente para os 20 melhores jogadores de cada seleção, qual nação teria o time
######## mais forte utilizando o critério da média do Overall de seus jogadores? Em outras palavras, 
######## filtre somente os 20 melhores jogadores de cada seleção, sendo o critério de "melhor" o campo
######## Overall, e, utilizando o mesmo campo, verifique qual seleção tem a melhor média.

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


####### 3.Neste exercício, considere o campo Release Clause como sendo o valor do jogador. Considerando
####### somente os clubes que possuem mais de 25 jogadores, quais são os 5 clubes mais valiosos?


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

####### 4.Imagine que você é diretor de um clube e possui um certo orçamento para comprar 11 jogadores que
####### irão compor o time titular. Cada jogador é contratado de acordo com a release clause. O presidente
####### deseja trazer jogadores jovens, sendo assim, pede que você não contrate ninguém acima de 29 anos. O
####### presidente também demanda que você não traga nenhuma estrelinha que possa conturbar o elenco, sendo
####### assim, o preço máximo a ser pago por um jogador não pode ultrapassa os 15 milhões de euros. Quais
####### são os 11 jogadores de maior Overall que você consegue trazer para seu clube? Isto, é claro, 
####### seguindo as restrições orçamentárias e etárias impostas pelo seu chefe.

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

####### 5.Utilizando a tabela com os jogadores que você selecionou no exercício anterior, crie uma coluna
####### chamada High_Price que recebe 1 se a Release Clause do jogador está acima da mediana dos 11
####### selecionados, e 0 caso contrário.

onze_contratacoes <- onze_contratacoes %>% 
  mutate(high_price = ifelse(release_clause > median(release_clause), 1, 0))

###### 6.Apresente os histogramas com a distribuição do peso, idade e salário dos jogadores que você
###### escolheu no exercício 4.

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
  labs(title = "Histograma - Salário dos Jogadores\n(mil Euros)",
       x = "Salário",
       y = "Contagem")


#Dataset Iris

Iris <- read_csv("C:/Users/angel/Downloads/ESTUDOS DIVERSOS/Desafio EstatSite/Bases de Dados/Iris.csv")

######## 7.Através de um gráfico de dispersão (scatterplot), verifique se há relação linear entre 
######## comprimento da pétala (Petal Length) e o comprimento da sépala (Sepal Length). Adicione também 
######## diferentes cores aos pontos de acordo com a espécie da flor. A resposta aqui é somente o gráfico,
######## não se preocupe em fazer análises mais aprofundadas.

glimpse(Iris)

Iris %>% 
  ggplot(aes(SepalLengthCm, PetalLengthCm)) +
  geom_point(aes(color = Species)) +
  theme_ipsum_es() +
  labs(title = "Comprimento da Sépala vs\nComprimento da Pétala (cm)",
       color = "Espécies",
       x = "Comprimento - Sépala",
       y = "Comprimento - Pétala")

######## 8.Primeiro, apague a substring "Iris-" da coluna Species. Em seguida, adicione 3 novas colunas à
######## tabela inicial, sendo que cada coluna receberá uma dummy referente a cada uma das species. Ou
######## seja, você deve criar uma coluna chamada Dummy_Setosa, que recebe 1 se a flor for da espécie 
######## Setosa e 0 caso contrário. O mesmo para as demais espécies.

iris_dummy <- Iris %>% 
  mutate(Species = str_sub(Species,6,-1))

iris_dummy <- iris_dummy %>% 
  mutate(Dummy_Setosa = if_else(Species == "setosa", 1, 0),
         Dummy_Versicolor = if_else(Species == "versicolor", 1, 0),
         Dummy_Virginica = if_else(Species == "virginica", 1, 0))


# Dataset MGLU3 e LREN3 

######## 9.Mostre através de um gráfico de linhas a evolução do preço de fechamento das duas ações durante
######## os anos de 2017, 2018 e 2019. No mesmo gráfico, trace um gráfico de linhas pontilhadas com a
######## evolução do preço de abertura das duas ações no mesmo período. Utilize cores diferentes para cada
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
       y = "Preço da Ação (R$)") +
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
       y = "Preço da Ação (R$)") +
  theme_ipsum_es()


#Datasets Compra e Cadastro

######## 10.A tabela COMPRAS possui as informações de todas as compras feitas pelos clientes da sua loja. Em
######## CADASTRO, você encontrará as informações cadastrais dos seus clientes. Monte uma nova tabela
######## chamada RESUMO. Essa tabela terá uma linha por cliente e as colunas serão os campos: Id, Idade,
######## Estado, Gasto_Total. As primeiras colunas são auto-explicativas e podem ser obtidas diretamente
######## na tabela COMPRAS. A última coluna deve trazer a soma de todas as compras feitas por cada cliente.
######## Essa tabela é a primeira parte da resposta. A segunda parte será obter a soma, a média e o desvio
######## padrão dos gastos por estado. Isto é, qual a soma, a média e o desvio padrão do campo Total_Gasto
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

######## 11.Apresente a distribuição dos campos numéricos da tabela RESUMO através de um boxplot e um
######## histograma. Coloque legenda dos eixos x e y do gráfico.

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

###### 12.Sua empresa considera jovem os clientes com menos de 30 anos. A partir disso, elabore um gráfico
###### de barras comparando o gasto médio de clientes jovens e velhos. Ou seja, a altura da barra será o
###### gasto médio do gasto de cada um dos grupos.

faixa_etaria <- resumo %>% 
  mutate(grupo_etario = if_else(Idade < 30,"Jovem", "Velho"))

faixa_etaria <- faixa_etaria %>% 
  group_by(grupo_etario) %>% 
  summarise(gasto_faixa_etaria = mean(Gasto_Total))

faixa_etaria %>% 
  ggplot(aes(grupo_etario,gasto_faixa_etaria)) +
  geom_bar(aes(fill = grupo_etario), stat = "identity") +
  scale_y_continuous(labels = label_number()) +
  labs(title = "Comparação de Gasto Médio por\nFaixa Etária (R$)",
       x = "",
       y = "Gasto Médio (R$)",
       fill = "Grupo Etário") +
  theme_ipsum_es()
                     

######## 13.Crie uma função que, dado um número X, faça duas coisas: (1) retorna os números pares de 1 a 9
######## que não fazem parte de X; (2) retorna uma mensagem indicando se o número é par ou ímpar.
######## Exemplo: se passarmos o número 239, a função deve retornar 4, 6, 8 e "ímpar". Pode ser em forma
######## de duas mensagens ou uma mensagem com os números e a definição de par ou ímpar. A escolha é sua.


testa_numero <- function(numero){
  # retorna pares não inclusos no numero
  pares <- c()
  for(i in 1:9){
    if(i %% 2 == 0){
      pares[length(pares)+1] <- i
      pares
    } 
  }
  digitos <- as.integer(str_split(numero,"")[[1]])
  print(setdiff(pares,digitos))
  # indica se é par ou impar
  if (numero %% 2 != 0)
    return("Número ímpar")
  else
    return("Número par")
}

######## 14.Escreva uma função que receba uma string e retorne a mesma string sem nenhuma letra repetida.
######## Exemplo: se a função receber a palavra "casa", ela deve retornar "cas"

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

######## 15.Escreva uma função chamada return_percentile que receba como entrada um array de dimensão (N,1) e
######## um percentile qualquer, e retorne o valor referente a este percentile. Não vale usar as funções
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

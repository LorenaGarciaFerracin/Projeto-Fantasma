source("rdocs/source/packages.R")

# ---------------------------------------------------------------------------- #

#        ______   _____  ________      ________ 
#      |  ____| / ____| |__   __| /\  |__   __|
#     | |__    | (___     | |   /  \    | |   
#    |  __|    \___ \    | |  / /\ \   | |   
#   | |____   ____) |   | |  /____ \  | |   
#  |______   |_____/   |_| /_/    \_\|_|   
#  
#         Consultoria estatística 
#

# ---------------------------------------------------------------------------- #
# ############################## README ###################################### #
# Consultor, favor utilizar este arquivo .R para realizar TODAS as análises
# alocadas a você neste projeto pelo gerente responsável, salvo instrução 
# explícita do gerente para mudança.
#
# Escreva seu código da forma mais clara e legível possível, eliminando códigos
# de teste depreciados, ou ao menos deixando como comentário. Dê preferência
# as funções dos pacotes contidos no Tidyverse para realizar suas análises.
# ---------------------------------------------------------------------------- #


#CArregando os pacotes que serão usados
library(tidyverse)
library(lubridate)
library(readxl)
library(usethis)

# Lendo o arquivo excel que contém os dados do projeto
dados<- "relatorio_old_town_road.xlsx"
relatorio_data<- read_excel(dados, sheet = "relatorio_vendas")
infos_vendas<- read_excel(dados, sheet = "infos_vendas")
infos_produtos<- read_excel(dados, sheet = "infos_produtos")
infos_funcionarios<- read_excel(dados, sheet = "infos_funcionarios")
infos_cidades<- read_excel(dados, sheet = "infos_cidades")
infos_clientes<- read_excel(dados, sheet = "infos_clientes")

#Arrumando o nome das variáveis para juntar as colunas depois
infos_vendas<- infos_vendas|>
  rename(SaleID=Sal3ID)
infos_produtos<-infos_produtos|>
  rename(ItemID=Ite3ID)

#Alterando o valor dos produtos de dólar para real, considerando a cotação do dólar à 5,31 reais.

infos_produtos$UnityPrice<-infos_produtos$UnityPrice*5.31

# Separando o ano, usando o lubridate
relatorio_data <- relatorio_data|>
  mutate( Ano= year(Date))
view(relatorio_data)

#Juntando os bancos de dados 
banco<-full_join(relatorio_data,infos_vendas, by ="SaleID")
banco<-full_join(banco,infos_produtos, by= "ItemID")

# Criando variáveis que representem a receita.
banco<-banco|>
  mutate(receita_por_venda = Quantity*UnityPrice)

banco2<-banco|>
  group_by(Ano,StoreID)|>
  summarise(receita_total_loja=sum(receita_por_venda),
            .groups = 'drop_last')|>
  summarise(receita_media_anual= mean(receita_total_loja))|>
  ungroup()

# Plotando o gráfico
gráfico<-ggplot(banco2, aes(x= Ano, y= receita_media_anual))+ 
  geom_point()+
  geom_line(color="#A11D21",linewidth = 1.5)+
  labs(
    title = "Receita total média das lojas(1880-1889)",
    x="Ano",
    y="Receita média(R$)"
  )+
  theme_estat()
#Chamando o gráfico
gráfico

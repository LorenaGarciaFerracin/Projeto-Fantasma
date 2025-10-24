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


#Idade dos clientes de Âmbar Seco a depender da loja (26/10)
#Com o intuito de entender melhor o perfil das idades dos clientes nas diferentes
#lojas da cidade de Âmbar Seco, foi solicitado a nós encontrar essas características
#dentro do banco de dados e mostrar quais são os perfis das idades dos clientes para
#cada loja dessa pequena cidade.

#Carregando os pacotes que serão usados
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
infos_lojas<- read_excel(dados, sheet = "infos_lojas")
#Arrumando o nome das variáveis para juntar as colunas depois
infos_vendas<- infos_vendas|>
  rename(SaleID=Sal3ID)
infos_produtos<-infos_produtos|>
  rename(ItemID=Ite3ID)
infos_cidades<-infos_cidades|>
  rename(CityID=C1tyID)
infos_clientes<-infos_clientes|>
  rename(ClientID=Cli3ntID)
infos_lojas<-infos_lojas|>
  rename(StoreID=Stor3ID)

#Juntando os bancos de dados 
banco<-full_join(relatorio_data,infos_vendas, by ="SaleID")
banco<-full_join(banco,infos_produtos, by= "ItemID")
banco<-full_join(banco, infos_clientes, by="ClientID")
banco<-full_join(banco, infos_lojas, by="StoreID")
banco<-full_join(banco, infos_cidades, by="CityID")

#Filtrar para conter apenas os dados da cidadde de Âmbar Seco.
bancos4<- banco|>
  filter(NameCity=="Âmbar Seco")
bancos3<-bancos4|>
  distinct(ClientID, .keep_all = TRUE)
#Criar o gráfico
box<-ggplot(bancos3) +
  aes(x = reorder(StoreID, Age, FUN = median), y = Age) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white") +
  labs(x = "Número da Loja de Âmbar Seco", y = "Idade (Anos)") +
  theme_estat()
ggsave("box_bi.pdf", width = 158, height = 93, units = "mm")
box

tabela<-bancos3 %>%
  group_by(StoreID) %>% # caso mais de uma categoria
  print_quadro_resumo(var_name = Age)


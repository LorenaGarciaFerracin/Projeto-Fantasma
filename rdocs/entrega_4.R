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

#Carregando os pacotes que serão usados
library(tidyverse)
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

banco<-full_join(relatorio_data,infos_vendas, by ="SaleID")
banco<-full_join(banco,infos_produtos, by= "ItemID")
banco<-full_join(banco, infos_clientes, by="ClientID")
banco<-full_join(banco, infos_lojas, by="StoreID")
banco<-full_join(banco, infos_cidades, by="CityID")

#O top 3 produtos mais vendidos nas top 3 lojas com maior receita em
#1889 (02/11)
#O cliente quer saber quais são os 3
#produtos mais vendidos nas 3 lojas que tiveram a maior 
#receita no ano de 1889, para dessa forma, entender quais 
#foram esses produtos, a quantidade vendida e as lojas que mais venderam 
#neste ano.


# Primeiro temos que ver quais foram as 3 lojas que maior receita em 1889
banco5<- banco|>
  mutate( Ano= year(Date))|>
  filter(Ano==1889)|>
  mutate(receita_por_venda = Quantity*UnityPrice*5.31)|>
  group_by(NameStore)|>
  summarise(receita_total_loja=sum(receita_por_venda),
            .groups = 'drop_last')|>
  arrange(desc(receita_total_loja))
print(banco5)


# FAZER UMA TABELA 
\begin{table}[]
\begin{tabular}{ll}
Loja Ouro Fino            & 197313 \\
Loja TendTudo             & 196340 \\
Ferraria Apache           & 181689 \\
Varejo Ejo                & 169266 \\
Vendinha Rápida           & 168723 \\
Ferraria Martelo de Ferro & 167515 \\
Ferraria Bruta            & 160958 \\
Venda Brava               & 156760 \\
Loja Esperança            & 156663 \\
Saloon Luar               & 154093 \\
Saloon Rastro de Poeira   & 153315 \\
Ferraria Seca             & 141790 \\
Banco Careca              & 139092 \\
Loja do Forte             & 136916 \\
Loja Riacho Prateado      & 134294 \\
Banco Santa Fé            & 132694 \\
Saloon Guerreiro          & 132523 \\
Saloon                    & 110220
\end{tabular}
\end{table}

# Agora fazemos um data frame para cada loja para descobrir qual foram os produtos mais vendidos em cada uma delas

lojaourofino<- banco|>
  mutate( Ano= year(Date))|>
  filter(Ano==1889)|>
  filter(NameStore=="Loja Ouro Fino")|>
  group_by(NameProduct)|>
  summarise(quantidade_total_item=sum(Quantity),
            .groups = 'drop_last')|>
  arrange(desc(quantidade_total_item))
head(lojaourofino, 3)  
# FAZER UMA TABELA 
lojaourofino
\begin{table}[]
\begin{tabular}{ll}
Botas de Couro  & 52 \\
Whisky          & 49 \\
Chapéu de Couro & 45 \\
Colt. 45        & 44 \\
Municao         & 39 \\
Espingarda      & 38 \\
Sela            & 37 \\
Pá              & 34 \\
Machado         & 27 \\
Cavalo          & 26
\end{tabular}
\end{table}

loja_tendtudo<- banco|>
  mutate( Ano= year(Date))|>
  filter(Ano==1889)|>
  filter(NameStore=="Loja TendTudo")|>
  group_by(NameProduct)|>
  summarise(quantidade_total_item=sum(Quantity),
            .groups = 'drop_last')|>
  arrange(desc(quantidade_total_item))
head(loja_tendtudo, 3) 
\begin{table}[]
\begin{tabular}{ll}
Espingarda      & 53 \\
Whisky          & 49 \\
Colt. 45        & 43 \\
Botas de Couro  & 39 \\
Chapéu de Couro & 33 \\
Sela            & 29 \\
Cavalo          & 25 \\
Pá              & 24 \\
Municao         & 21 \\
Machado         & 8 
\end{tabular}
\end{table}
#FAzER UMA TABELA
ferraria_apache<- banco|>
  mutate( Ano= year(Date))|>
  filter(Ano==1889)|>
  filter(NameStore=="Ferraria Apache")|>
  group_by(NameProduct)|>
  summarise(quantidade_total_item=sum(Quantity),
            .groups = 'drop_last')|>
  arrange(desc(quantidade_total_item))
head(ferraria_apache, 3) 

#FAZER TABELA 
\begin{table}[]
\begin{tabular}{ll}
Chapéu de Couro & 52 \\
Espingarda      & 42 \\
Machado         & 41 \\
Whisky          & 41 \\
Sela            & 35 \\
Municao         & 30 \\
Botas de Couro  & 28 \\
Cavalo          & 27 \\
Colt. 45        & 23 \\
Pá              & 23
\end{tabular}
\end{table}
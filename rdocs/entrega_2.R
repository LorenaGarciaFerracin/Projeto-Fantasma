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

# Lendo o arquivo excel que contém os dados do projeto
dados<- "relatorio_old_town_road.xlsx"
relatorio_data<- read_excel(dados, sheet = "relatorio_vendas")
infos_vendas<- read_excel(dados, sheet = "infos_vendas")
infos_produtos<- read_excel(dados, sheet = "infos_produtos")
infos_funcionarios<- read_excel(dados, sheet = "infos_funcionarios")
infos_cidades<- read_excel(dados, sheet = "infos_cidades")
infos_clientes<- read_excel(dados, sheet = "infos_clientes")
#Criamos uma nova variavel com o peso dos clientes em kilogramas e 
#outra da aultura em centímetros,

# O peso está em libras(lbs),uma libra é aproximadamente 0.45359237 kilogramas.
infos_clientes<-infos_clientes|>
  mutate(Peso_kg= Weight_lbs*0.45359237)

# E a altura em decímetros(dm), um decímetro é igual a 10 centímetros.
infos_clientes<-infos_clientes|>
  mutate(Altura_cm=Height_dm*10)
# Primeiramente, para compreender como os dados estão se comportando precisamos visualizá-los,
#para isso faremos um diagrama de dispersão.

g_dispersao<-ggplot(infos_clientes) +
  aes(x = Altura_cm, y = Peso_kg) +
  geom_point(colour = "#A11D21", size = 3, alpha=0.5) +
  labs(
    x = "Altura (cm)",
    y = "Peso (kg)"
  ) +
  theme_estat()
ggsave("disp_uni.pdf", width = 158, height = 93, units = "mm")
g_dispersao

#Para calcular as correlações vamos utilizar a função "cor()"
cor(infos_clientes$Altura_cm,infos_clientes$Peso_kg, method = "pearson")

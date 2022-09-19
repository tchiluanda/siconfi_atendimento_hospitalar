library(tidyverse)

#Conecta base dos dados
conecta_basedosdados()

gastos_2019<- gastos_hospitalares_perc(2019)

populacao_2019<- populacao_municipios(2019)

estados_1<- c("AC","AL","AM", "AP", "BA","CE","DF","ES","GO","MA","MT","MS")
dados_sih_1_2019<- dados_atendimento_hospitalar(2019, estados = estados_1)

saveRDS(dados_sih_1_2019,file = "dados_sih_1_2019.RDS")

estados_2<- c("MG","PA","PB", "PR","PE","PI")
dados_sih_2_2019<- dados_atendimento_hospitalar(2019, estados = estados_2)

saveRDS(dados_sih_2_2019,file = "dados_sih_2_2019.RDS")

estados_3<- c("RJ","RS","RN","RO","RR","SC","SP","SE","TO")
dados_sih_3_2019<- dados_atendimento_hospitalar(2019, estados = estados_3)

saveRDS(dados_sih_3_2019,file = "dados_sih_3_2019.RDS")


amostra_sih_2019<- amostra_atendimento_hospitalar(c("dados_sih_1_2019.RDS",
                                                    "dados_sih_2_2019.RDS",
                                                    "dados_sih_3_2019.RDS"))


saveRDS(amostra_sih_2019,file = "amostra_sih_2019.RDS")

dados_cnes<- dados_cnes_hospitais(2019)

saveRDS(dados_cnes,file = "dados_cnes.RDS")

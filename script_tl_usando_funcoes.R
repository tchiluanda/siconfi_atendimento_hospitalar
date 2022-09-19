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

dados_cnes_2019<- dados_cnes_hospitais(2019)

saveRDS(dados_cnes_2019,file = "dados_cnes_2019.RDS")


#################Inclusão de dados sobre os níveis hierárquicos de municípios

library(readxl)
REGIC2018_Cidades <- readxl::read_excel("REGIC2018_Cidades_v2.xlsx")

REGIC2018_Cidades <- janitor::clean_names(REGIC2018_Cidades)



###Transformando os dados

REGIC_trabalho<-
  REGIC2018_Cidades %>%
  select(1:3,13,14)

names(REGIC_trabalho)[4:5]<- c("nivel_hierarquia","nome_nivel_hierarquia")


cnes_trabalho<-
  dados_cnes_2019 %>%
  mutate(munResLat = as.numeric(munResLat),
         munResLon = as.numeric(munResLon)) %>%
  select(CNES, CODUFMUN, munResNome, munResLat, munResLon )

cnes_trabalho <- janitor::clean_names(cnes_trabalho)

gastos_hospitais_pc<-
  populacao_2019 %>%
  left_join(gastos_2019) %>%
  mutate(perc= ifelse(is.na(perc),0,perc),
         gasto_pc= valor/populacao,
         gasto_pc = ifelse(is.na(gasto_pc),0,gasto_pc))


gastos_trabalho<-
  gastos_hospitais_pc %>%
  select(id_municipio, perc, populacao, gasto_pc)


sih_trabalho<-
  amostra_sih_2019 %>%
  mutate(munResLat = as.numeric(munResLat),
         munResLon = as.numeric(munResLon)) %>%
  select(CNES, MUNIC_RES, munResNome, munResLat, munResLon)

sih_trabalho <- janitor::clean_names(sih_trabalho)

###Montagem do dataset único

dataset_analise_2019<-
  sih_trabalho %>%
  inner_join(
    gastos_trabalho %>%
      mutate(munic_res = str_sub(id_municipio,1,6))
  ) %>%
  left_join(
    REGIC_trabalho %>%
      mutate(munic_res = str_sub(as.character(cod_cidade),1,6)),
    by = "munic_res"
  ) %>%
  inner_join(
    cnes_trabalho,
    by="cnes"
  ) %>%
  inner_join(
    gastos_trabalho %>%
      mutate(codufmun= str_sub(id_municipio,1,6) ) %>%
      select(codufmun, perc, populacao, gasto_pc),
    by= "codufmun"
  ) %>%
  inner_join(
    REGIC_trabalho %>%
      mutate(codufmun = str_sub(as.character(cod_cidade),1,6)),
    by= "codufmun"
  )


#função para cálculo de distância
library(geosphere)



dataset_analise_2019$distancia<-

  map_dbl(1:NROW(dataset_analise_2019),function(id){

    #print(id)
    c(geosphere::distm(c(dataset_analise_2019$mun_res_lat.x[id], dataset_analise_2019$mun_res_lon.x[id]),
                       c(dataset_analise_2019$mun_res_lat.y[id], dataset_analise_2019$mun_res_lon.y[id]),
                       fun = distHaversine))


  })

distancia<- dataset_analise_2019$distancia/1000

dataset_analise_2019$distancia<- distancia

dataset_analise_2019$deslocamento<- ifelse(dataset_analise_2019$distancia==0,0,1)



saveRDS(dataset_analise_2019,"dataset_analise_2019.RDS")

####Prepara dados auxiliares para etl

REGIC2018_Cidades <- read_excel("REGIC2018_Cidades_v2.xlsx")

REGIC2018_Cidades <- janitor::clean_names(REGIC2018_Cidades)

REGIC_trabalho<-
  REGIC2018_Cidades %>%
  select(1:3,13,14)

names(REGIC_trabalho)[4:5]<- c("nivel_hierarquia","nome_nivel_hierarquia")

municipios_seat<- geobr::read_municipal_seat(showProgress = FALSE)
municipios<- geobr::read_municipality(showProgress = FALSE)

estados<- geobr::read_state(showProgress = FALSE)

brasil<- geobr::read_country(showProgress = FALSE)

#Busca de dados de população de municípios
pop_municipios <-
populacao_2019

save(list=c("REGIC_trabalho","municipios_seat","municipios","estados_mapa","brasil","pop_municipios"), file = "dados_auxiliares.RData")




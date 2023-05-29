library(tidyverse)

#Conecta base dos dados
conecta_basedosdados()


#gastos_2021<- gastos_hospitalares_perc(2021)
#fab<- gastos_hospitalares_perc(2018)

library(readr)
gastos_2021 <-
  read_delim("finbra.csv", delim = ";",
                     escape_double = FALSE, col_types = cols(Cod.IBGE = col_character()),
                     locale = locale(decimal_mark = ",", grouping_mark = ".",
                                     encoding = "LATIN1"), trim_ws = TRUE,
                     skip = 3)

gastos_2021<- janitor::clean_names(gastos_2021)


gastos_2021_hospital_exportacao<-
  gastos_2021 %>%
  filter(coluna == "Despesas Pagas",
         str_sub(conta,1,6)=="10.302")%>%
  rename(id_municipio = cod_ibge,
         valor_gasto_hospital = valor) %>%
  select(id_municipio, valor_gasto_hospital)

gastos_2021_total_exportacao<-
  gastos_2021 %>%
  filter(coluna == "Despesas Pagas",
         conta %in% c("Despesas Exceto Intraorçamentárias",
                      "Despesas Intraorçamentárias")) %>%
  rename(id_municipio = cod_ibge) %>%
  group_by(instituicao, id_municipio) %>%
  summarise(valor_total = sum(valor)) %>%
  ungroup()

gasto_2021_exportacao<-
  gastos_2021_total_exportacao %>%
  left_join(gastos_2021_hospital_exportacao, by = "id_municipio") %>%
  mutate(perc = (valor_gasto_hospital/valor_total)*100)


gasto_2021_exportacao %>%
  readr::write_csv2("gastos_hospitalares_municipios_2021.csv")


gastos_2021_hospital<-
  gastos_2021 %>%
  filter(coluna == "Despesas Pagas",
         str_sub(conta,1,6)=="10.302")%>%
  rename(id_municipio = cod_ibge) %>%
  select(id_municipio, valor)

gastos_2021_total<-
  gastos_2021 %>%
  filter(coluna == "Despesas Pagas",
         conta %in% c("Despesas Exceto Intraorçamentárias",
                      "Despesas Intraorçamentárias")) %>%
  rename(id_municipio = cod_ibge) %>%
  group_by(id_municipio) %>%
  summarise(valor_total = sum(valor)) %>%
  ungroup()


gastos_2021<-
  gastos_2021_hospital %>%
  inner_join(gastos_2021_total, by = "id_municipio") %>%
  mutate(perc = (valor/valor_total)*100)



populacao_2021<- populacao_municipios(2021)

estados_1<- c("AC","AL","AM", "AP", "BA","CE","DF","ES","GO","MA","MT","MS")
dados_sih_1_2021<- dados_atendimento_hospitalar(2021, estados = estados_1)

saveRDS(dados_sih_1_2021,file = "dados_sih_1_2021.RDS")

estados_2<- c("MG","PA","PB", "PR","PE","PI")
dados_sih_2_2021<- dados_atendimento_hospitalar(2021, estados = estados_2)

saveRDS(dados_sih_2_2021,file = "dados_sih_2_2021.RDS")

estados_3<- c("RJ","RS","RN","RO","RR","SC","SP","SE","TO")
dados_sih_3_2021<- dados_atendimento_hospitalar(2021, estados = estados_3)

saveRDS(dados_sih_3_2021,file = "dados_sih_3_2021.RDS")


amostra_sih_2021<- amostra_atendimento_hospitalar(c("dados_sih_1_2021.RDS",
                                                    "dados_sih_2_2021.RDS",
                                                    "dados_sih_3_2021.RDS"))


saveRDS(amostra_sih_2021,file = "amostra_sih_2021.RDS")

dados_cnes_2021<- dados_cnes_hospitais(2021)

saveRDS(dados_cnes_2021,file = "dados_cnes_2021.RDS")


#################Inclusão de dados sobre os níveis hierárquicos de municípios

library(readxl)
REGIC2018_Cidades <- readxl::read_excel("REGIC2018_Cidades_v2.xlsx")

REGIC2018_Cidades <- janitor::clean_names(REGIC2018_Cidades)





###Transformando os dados

REGIC_trabalho<-
  REGIC2018_Cidades %>%
  select(1:3,13,14)

names(REGIC_trabalho)[4:5]<- c("nivel_hierarquia","nome_nivel_hierarquia")

REGIC_trabalho$nome_nivel_hierarquia_ordenado<-
  factor(REGIC_trabalho$nome_nivel_hierarquia, levels = unique(REGIC_trabalho$nome_nivel_hierarquia[order(REGIC_trabalho$nivel_hierarquia)]))

cnes_trabalho<-
  dados_cnes_2021 %>%
  mutate(munResLat = as.numeric(munResLat),
         munResLon = as.numeric(munResLon)) %>%
  select(CNES, CODUFMUN, munResNome, munResLat, munResLon )

cnes_trabalho <- janitor::clean_names(cnes_trabalho)

gastos_hospitais_pc<-
  populacao_2021 %>%
  left_join(gastos_2021) %>%
  mutate(perc= ifelse(is.na(perc),0,perc),
         gasto_pc= valor/populacao,
         gasto_pc = ifelse(is.na(gasto_pc),0,gasto_pc))


gastos_trabalho<-
  gastos_hospitais_pc %>%
  select(id_municipio, perc, populacao, gasto_pc)


sih_trabalho<-
  amostra_sih_2021 %>%
  mutate(munResLat = as.numeric(munResLat),
         munResLon = as.numeric(munResLon)) %>%
  select(CNES, MUNIC_RES, munResNome, munResLat, munResLon)

sih_trabalho <- janitor::clean_names(sih_trabalho)

###Montagem do dataset único

# NROW(sih_trabalho %>%
#        inner_join(
#          gastos_trabalho %>%
#            mutate(munic_res = str_sub(id_municipio,1,6))
#        ))
#
# fab<-
#   sih_trabalho %>%
#   inner_join(
#     gastos_trabalho %>%
#       mutate(munic_res = str_sub(id_municipio,1,6))
#   ) %>%
#   left_join(
#     REGIC_trabalho %>%
#       mutate(munic_res = str_sub(as.character(cod_cidade),1,6)),
#     by = "munic_res"
#   ) %>%
#   inner_join(
#     cnes_trabalho,
#     by="cnes"
#   )%>%
#   inner_join(
#     gastos_trabalho %>%
#       mutate(codufmun= str_sub(id_municipio,1,6) ) %>%
#       select(codufmun, perc, populacao, gasto_pc),
#     by= "codufmun"
#   )%>%
#   left_join(
#     REGIC_trabalho %>%
#       mutate(codufmun = str_sub(as.character(cod_cidade),1,6)),
#     by= "codufmun"
#   )


fab<-
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
  left_join( #originalmente era inner_join
    REGIC_trabalho %>%
      mutate(codufmun = str_sub(as.character(cod_cidade),1,6)),
    by= "codufmun"
  )



dataset_analise_2021<-
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
  left_join( #originalmente era inner_join
    REGIC_trabalho %>%
      mutate(codufmun = str_sub(as.character(cod_cidade),1,6)),
    by= "codufmun"
  )


#função para cálculo de distância
library(geosphere)



dataset_analise_2021$distancia<-

  map_dbl(1:NROW(dataset_analise_2021),function(id){

    #print(id)
    c(geosphere::distm(c(dataset_analise_2021$mun_res_lat.x[id], dataset_analise_2021$mun_res_lon.x[id]),
                       c(dataset_analise_2021$mun_res_lat.y[id], dataset_analise_2021$mun_res_lon.y[id]),
                       fun = distHaversine))


  })

distancia<- dataset_analise_2021$distancia/1000

dataset_analise_2021$distancia<- distancia

dataset_analise_2021$deslocamento<- ifelse(dataset_analise_2021$distancia==0,0,1)



saveRDS(dataset_analise_2021,"dataset_analise_2021.RDS")

####Prepara dados auxiliares para etl

REGIC2018_Cidades <- read_excel("REGIC2018_Cidades_v2.xlsx")

REGIC2018_Cidades <- janitor::clean_names(REGIC2018_Cidades)

REGIC_trabalho<-
  REGIC2018_Cidades %>%
  select(1:3,13,14)

names(REGIC_trabalho)[4:5]<- c("nivel_hierarquia","nome_nivel_hierarquia")

municipios_seat<- geobr::read_municipal_seat(showProgress = FALSE)
municipios<- geobr::read_municipality(showProgress = FALSE)

estados_mapa<- geobr::read_state(showProgress = FALSE)

brasil<- geobr::read_country(showProgress = FALSE)

#Busca de dados de população de municípios
pop_municipios <-
populacao_2021


REGIC_trabalho$nome_nivel_hierarquia_ordenado<-
  factor(REGIC_trabalho$nome_nivel_hierarquia, levels = unique(REGIC_trabalho$nome_nivel_hierarquia[order(REGIC_trabalho$nivel_hierarquia)]))

municipios_seat<- cbind(municipios_seat, st_coordinates(st_centroid(municipios_seat)))


save(list=c("REGIC_trabalho","municipios_seat","municipios","estados_mapa","brasil","pop_municipios"), file = "dados_auxiliares_2021.RData")

dados_sih_3_2021 %>%
  filter(is.na(ETNIA))


library(geobr)
regioes_saude<- geobr::read_health_region()

saveRDS(regioes_saude,"regioes_saude.RDS")


library(tidyverse)
regioes_saude %>%
  ggplot() +
  geom_sf(color= "green")

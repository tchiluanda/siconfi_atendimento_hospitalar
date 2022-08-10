library(siconfiBD)
library(geobr)

library("basedosdados")
library(tidyverse)
library(colorspace)

library(readxl)

library(microdatasus)



siconfiBD::setup_siconfi(project_id = "nice-diorama-306223")

funcoes<- siconfiBD::get_distinct_function()


#Gastos públicos relativos em Assistência hospitalar e ambulatorial
gastos_hospitais_perc<-
  siconfiBD::get_perc_function_exp_municipality( year= 2020,
                                                gov_function = "Assistência Hospitalar e Ambulatorial",
                                                expense_stage = "Despesas Pagas")


#Gastos públicos relativos em saúde

gastos_saude<-
  siconfiBD::get_perc_function_exp_municipality( year= 2020,
                                                 gov_function = "Saúde",
                                                 expense_stage = "Despesas Pagas")


#Gastos públicos relativos em atenção básica
gastos_atencao_basica<-
  siconfiBD::get_perc_function_exp_municipality( year= 2020,
                                                 gov_function = "Atenção Básica",
                                                 expense_stage = "Despesas Pagas")



#Busca de dados de população de municípios
pop_municipios <- bdplyr("br_ibge_populacao.municipio")

pop_municipios <-
pop_municipios%>%
  filter(ano==2020)%>%
  bd_collect()


#Análises exploratórias iniciais
mean(gastos_hospitais_perc$perc)
median(gastos_hospitais_perc$perc)

names(pop_municipios)

mean(gastos_saude$perc)
median(gastos_saude$perc)


mean(pop_municipios$populacao)
median(pop_municipios$populacao)

municipios_seat<- geobr::read_municipal_seat()
municipios<- geobr::read_municipality()

estados_mapa<- geobr::read_state()

gastos_hospitais_pc<-
pop_municipios %>%
  left_join(gastos_hospitais_perc) %>%
  mutate(perc= ifelse(is.na(perc),0,perc),
         gasto_pc= valor/populacao,
         gasto_pc = ifelse(is.na(gasto_pc),0,gasto_pc))

mun_sem_gasto<-
  gastos_hospitais_pc %>%
  filter(is.na(gasto_pc))


gastos_saude_pc<-
  pop_municipios %>%
  left_join(gastos_saude) %>%
  mutate(gasto_pc= valor/populacao)

mun_sem_gasto_saude<-
  gastos_saude_pc %>%
  filter(is.na(gasto_pc))


gastos_hospitais_pc %>%
  ggplot()+
  geom_histogram(aes(x=perc),binwidth= 5, color="white")


NROW(gastos_hospitais_pc %>%
       filter(perc>=25))

NROW(gastos_hospitais_perc %>%
       filter(perc>=25))


gastos_hospitais_pc %>%
  ggplot()+
  geom_histogram(aes(x=gasto_pc), color="white")


gastos_saude_pc %>%
  ggplot()+
  geom_histogram(aes(x=perc),binwidth= 5, color="white")


mapa<-
municipios_seat %>%
  inner_join(
    gastos_hospitais_pc %>%
      mutate(code_muni = as.numeric(id_municipio))
  ) %>%
  ggplot()+
  geom_sf(aes(color=perc), alpha= 1, size=0.2)+
  labs( color="% gasto") +
  colorspace::scale_color_continuous_sequential(palette = "PinkYl")+
  theme_light()  +
  theme(
    axis.text = element_blank(),
    panel.background = element_rect(fill = "#15202B"),
    panel.grid = element_blank(),
  )


mun_sel<-
  municipios %>%
  filter(name_muni %in% c("Barbalha","Fortaleza"))%>%
  inner_join(
    gastos_hospitais_pc %>%
      filter(sigla_uf %in% c("CE")) %>%
      mutate(code_muni = as.numeric(id_municipio))
  )


library(ggrepel)
library(sf)

municipios %>%
  inner_join(
    gastos_hospitais_pc %>%
      filter(sigla_uf %in% c("CE")) %>%
      mutate(code_muni = as.numeric(id_municipio))
  ) %>%
  ggplot()+
  geom_sf(aes(fill=gasto_pc), alpha= 1, color= "lightgray")+ #, text=name_muni
  geom_text_repel(data=mun_sel[1,], aes(x= -39.3445,
                              y=-7.403116,
                              label=name_muni,
                              color = gasto_pc),
            #color = "white",
            show.legend = FALSE,
            fontface = "bold",
            size=2.5,
            nudge_x = c(-0.40),
            nudge_y = c(-0.25))+
  geom_text_repel(data=mun_sel[2,], aes(x= -38.52775,
                                 y=-3.785651,
                                 label=name_muni,
                                 color= gasto_pc ),
            show.legend = FALSE,
            fontface = "bold",
            size=2.5,
            nudge_x = c(0.6),
            nudge_y = c(0))+
  labs( title = "Gastos em Assistência Hospitalar e Ambulatorial",
        subtitle= "Municípios do berço da civilização (Ceará)",
    fill=str_wrap("gasto per capita",10)) +
  colorspace::scale_fill_continuous_sequential(palette = "PinkYl")+
  colorspace::scale_color_continuous_sequential(palette = "PinkYl")+
  theme_light()  +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.background = element_rect(fill = "#15202B"),
    panel.grid = element_blank(),
  )

municipios %>%
  inner_join(
    gastos_hospitais_pc %>%
      filter(sigla_uf %in% c("CE")) %>%
      mutate(code_muni = as.numeric(id_municipio))
  ) %>%
  ggplot()+
  geom_sf(aes(fill=perc), alpha= 1, color= "lightgray")+ #, text=name_muni
  geom_text_repel(data=mun_sel[1,], aes(x= -39.3445,
                                        y=-7.403116,
                                        label=name_muni,
                                        color = perc),
                  show.legend = FALSE,
                  fontface = "bold",
                  size=2.5,
                  nudge_x = c(-0.40),
                  nudge_y = c(-0.25))+
  geom_text_repel(data=mun_sel[2,], aes(x= -38.52775,
                                        y=-3.785651,
                                        label=name_muni,
                                        color= perc ),
                  show.legend = FALSE,
                  fontface = "bold",
                  size=2.5,
                  nudge_x = c(0.6),
                  nudge_y = c(0))+

  labs( title = "Gastos em Assistência Hospitalar e Ambulatorial",
        subtitle= "Municípios do berço da civilização (Ceará)",
        fill=str_wrap("(%) gasto total",10)) +
  colorspace::scale_fill_continuous_sequential(palette = "PinkYl")+
  colorspace::scale_color_continuous_sequential(palette = "PinkYl")+
  theme_light()  +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.background = element_rect(fill = "#15202B"),
    panel.grid = element_blank(),
  )



municipios %>%
  inner_join(
    gastos_hospitais_pc %>%
      filter(sigla_uf %in% c("CE")) %>%
      mutate(code_muni = as.numeric(id_municipio))
  ) %>%
  ggplot()+
  geom_sf(aes(fill=perc), alpha= 1, color= "lightgray")+ #, text=name_muni
  #geom_sf(data = estados, color="black", fill=NA) +
  labs( fill="% gasto") +
  colorspace::scale_fill_continuous_sequential(palette = "PinkYl")+
  theme_light()  +
  theme(
    axis.text = element_blank(),
    panel.background = element_rect(fill = "#15202B"),
    panel.grid = element_blank(),
  )



plotly::ggplotly(mapa)

plotly::ggplotly(mapa_fill)


############Busca de dados de atendimentos hospitalares
remotes::install_github("rfsaldanha/microdatasus")

estados<- c("AC","AL","AM", "AP", "BA","CE","DF","ES","GO","MA","MT","MS","MG","PA","PB", "PR","PE","PI","RJ","RS","RN","RO","RR","SC","SP","SE","TO")



library(microdatasus)



#Script para download completo. Pode não haver espaço em memória suficiente.
#Recomenda-se quebrar em três blocos de estados e salvar em arquivos RDS distintos
dados_sih<-
  map_dfr(estados,function(estado){
    res<- try(microdatasus::fetch_datasus(year_start = 2020,
                                          year_end = 2020,
                                          uf = estado,
                                          month_start = 1,
                                          month_end = 12,
                                          information_system = "SIH-RD"))

    if (inherits(res, "try-error")){
      return()
    }

    microdatasus::process_sih(res)
  })


estados<- unique(pop_municipios$sigla_uf) #busca as siglas dos estados.
                                          #Pode ser substituída por qualquer tabela que já tenha esse dado


library(microdatasus)


#estados<- c("AC","AP")

dados_sih_1<-
  map_dfr(estados[1:13],function(estado){
    res<- try(microdatasus::fetch_datasus(year_start = 2020,
                                year_end = 2020,
                                uf = estado,
                                month_start = 1,
                                month_end = 12,
                                information_system = "SIH-RD"))

    if (inherits(res, "try-error")){
      return()
    }

    microdatasus::process_sih(res)
  })

saveRDS(dados_sih_1,file = "dados_sih_1.RDS")

dados_sih_2<-
  map_dfr(estados[14:18],function(estado){
    res<- try(microdatasus::fetch_datasus(year_start = 2020,
                                          year_end = 2020,
                                          uf = estado,
                                          month_start = 1,
                                          month_end = 12,
                                          information_system = "SIH-RD"))

    if (inherits(res, "try-error")){
      return()
    }

    microdatasus::process_sih(res)
  })


saveRDS(dados_sih_2,file = "dados_sih_2.RDS")

estados<- c("RJ", "SP", "PR", "SC", "RS", "MS", "MT", "GO", "DF")

dados_sih_3<-
  map_dfr(estados,function(estado){
    res<- try(microdatasus::fetch_datasus(year_start = 2020,
                                          year_end = 2020,
                                          uf = estado,
                                          month_start = 1,
                                          month_end = 12,
                                          information_system = "SIH-RD"))

    if (inherits(res, "try-error")){
      return()
    }

    microdatasus::process_sih(res)
  })


saveRDS(dados_sih_3,file = "dados_sih_3.RDS")


dados_sih_1 <- readRDS("~/Github/siconfi_atendimento_hospitalar/dados_sih_1.RDS")
dados_sih_2 <- readRDS("~/Github/siconfi_atendimento_hospitalar/dados_sih_2.RDS")
dados_sih_3 <- readRDS("~/Github/siconfi_atendimento_hospitalar/dados_sih_3.RDS")


#construção de uma amostra com 10% dos atendimentos hospitalares (1,06 milhões de atendimento)
set.seed(1972)

sample_sih<-
  dados_sih_1 %>%
  slice_sample(prop=0.1) %>%
  bind_rows(
    dados_sih_2 %>%
      slice_sample(prop = 0.1),
    dados_sih_3 %>%
      slice_sample(prop = 0.1)
  )

saveRDS(sample_sih,file = "sample_sih.RDS")

#######Dados sobre os hospitais

estados<- c("AC","AL","AM", "AP", "BA","CE","DF","ES","GO","MA","MT","MS","MG","PA","PB", "PR","PE","PI","RJ","RS","RN","RO","RR","SC","SP","SE","TO")


#Script para download completo. Pode não haver espaço em memória suficiente.
#Recomenda-se quebrar em três blocos de estados e salvar em arquivos RDS distintos
dados_cnes<-
  map_dfr(estados,function(estado){
    res<- try(microdatasus::fetch_datasus(year_start = 2020,
                                          year_end = 2020,
                                          uf = estado,
                                          month_start = 12,
                                          month_end = 12,
                                          information_system = "CNES-ST"))

    if (inherits(res, "try-error")){
      return()
    }

    microdatasus::process_cnes(res)
  })


saveRDS(dados_cnes,file = "dados_cnes.RDS")

dados_cnes %>%
  anti_join(
    gastos_hospitais_pc %>%
      mutate(CODUFMUN= str_sub(id_municipio,1,6))
  ) %>%
  distinct(CODUFMUN)


sample_sih %>%
  anti_join(
    gastos_hospitais_pc %>%
      mutate(UF_ZI= str_sub(id_municipio,1,6))
  ) %>%
  distinct(UF_ZI)




#colunas para joins
dados_cnes$CNES[1]
sample_sih$CNES[1]

#colunas para joins
sample_sih$MUNIC_RES[1]
sample_sih$UF_ZI[1]
dados_cnes$CODUFMUN[1]

#################Inclusão de dados sobre os níveis hierárquicos de municípios


REGIC2018_Cidades <- read_excel("REGIC2018_Cidades_v2.xlsx")

REGIC2018_Cidades <- janitor::clean_names(REGIC2018_Cidades)



###Transformando os dados

REGIC_trabalho<-
  REGIC2018_Cidades %>%
  select(1:3,13,14)

names(REGIC_trabalho)[4:5]<- c("nivel_hierarquia","nome_nivel_hierarquia")


cnes_trabalho<-
  dados_cnes %>%
  mutate(munResLat = as.numeric(munResLat),
         munResLon = as.numeric(munResLon)) %>%
  select(CNES, CODUFMUN, munResNome, munResLat, munResLon )

cnes_trabalho <- janitor::clean_names(cnes_trabalho)

gastos_trabalho<-
  gastos_hospitais_pc %>%
  select(id_municipio, perc, populacao, gasto_pc)


sih_trabalho<-
  sample_sih %>%
  mutate(munResLat = as.numeric(munResLat),
         munResLon = as.numeric(munResLon)) %>%
  select(CNES, MUNIC_RES, munResNome, munResLat, munResLon)

sih_trabalho <- janitor::clean_names(sih_trabalho)

###Montagem do dataset único
dataset_analise_old<- dataset_analise


dataset_analise<-
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

dataset_analise_old <-

sih_trabalho %>%
  inner_join(
    gastos_trabalho %>%
      mutate(munic_res = str_sub(id_municipio,1,6))
  ) %>%
  inner_join(
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



dataset_analise$distancia<-

  map_dbl(1:NROW(dataset_analise),function(id){

    #print(id)
    c(geosphere::distm(c(dataset_analise$mun_res_lat.x[id], dataset_analise$mun_res_lon.x[id]),
                       c(dataset_analise$mun_res_lat.y[id], dataset_analise$mun_res_lon.y[id]),
                       fun = distHaversine))


  })

distancia<- dataset_analise$distancia/1000

dataset_analise$distancia<- distancia

dataset_analise$deslocamento<- ifelse(dataset_analise$distancia==0,0,1)



saveRDS(dataset_analise,"dataset_analise.RDS")

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

siconfiBD::setup_siconfi(project_id = "nice-diorama-306223")


#Busca de dados de população de municípios
pop_municipios <- bdplyr("br_ibge_populacao.municipio")

pop_municipios <-
  pop_municipios%>%
  filter(ano==2020)%>%
  bd_collect()


save(list=c("REGIC_trabalho","municipios_seat","municipios","estados_mapa","brasil","pop_municipios"), file = "dados_auxiliares.RData")


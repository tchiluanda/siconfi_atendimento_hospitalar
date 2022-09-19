library(siconfiBD)
library(geobr)

library("basedosdados")
library(tidyverse)
library(colorspace)

library(readxl)

library(microdatasus)


conecta_basedosdados<- function(projeto="nice-diorama-306223"){

  siconfiBD::setup_siconfi(project_id = projeto)

}



gastos_hospitalares_perc<- function(ano){
  #Gastos públicos relativos em Assistência hospitalar e ambulatorial
    siconfiBD::get_perc_function_exp_municipality( year= ano,
                                                   gov_function = "Assistência Hospitalar e Ambulatorial",
                                                   expense_stage = "Despesas Pagas")

}

populacao_municipios<- function(a_ano){

  #Busca de dados de população de municípios
  pop_municipios<-basedosdados::bdplyr("br_ibge_populacao.municipio")

  pop_municipios%>%
    dplyr::filter(ano==a_ano)%>%
    basedosdados::bd_collect()


}


dados_atendimento_hospitalar<- function(ano, estados=NULL){

  library(microdatasus)

  if (is.null(estados)){
    estados<- c("AC","AL","AM", "AP", "BA","CE","DF","ES","GO","MA","MT","MS","MG","PA","PB", "PR","PE","PI","RJ","RS","RN","RO","RR","SC","SP","SE","TO")
  }

  dados_sih<-
    map_dfr(estados,function(estado){

      print(estado)

      res<- try(microdatasus::fetch_datasus(year_start = ano,
                                            year_end = ano,
                                            uf = estado,
                                            month_start = 1,
                                            month_end = 12,
                                            information_system = "SIH-RD"))

      if (inherits(res, "try-error")){
        return()
      }

      microdatasus::process_sih(res)
    })

  dados_sih

}

amostra_atendimento_hospitalar<- function(path_arquivo_rds, seed=1972){

  purrr::map_dfr(path_arquivo_rds, function(arquivo){
    dados_sih<-readRDS(arquivo)
    set.seed(seed)
    dados_sih %>%
      slice_sample(prop=0.1)

  })

}


dados_cnes_hospitais<- function(ano,estados=NULL, mes_ini=12,mes_fim=12){

  if (is.null(estados)){
    estados<- c("AC","AL","AM", "AP", "BA","CE","DF","ES","GO","MA","MT","MS","MG","PA","PB", "PR","PE","PI","RJ","RS","RN","RO","RR","SC","SP","SE","TO")
  }


  #Script para download completo. Pode não haver espaço em memória suficiente.
  #Recomenda-se quebrar em três blocos de estados e salvar em arquivos RDS distintos
    map_dfr(estados,function(estado){
      res<- try(microdatasus::fetch_datasus(year_start = ano,
                                            year_end = ano,
                                            uf = estado,
                                            month_start = mes_ini,
                                            month_end = mes_fim,
                                            information_system = "CNES-ST"))

      if (inherits(res, "try-error")){
        return()
      }

      microdatasus::process_cnes(res)
    })


}






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



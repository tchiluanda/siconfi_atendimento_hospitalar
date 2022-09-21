library(caret)

dataset_analise<- load("dataset_analise_2020.RDS")

dataset_analise<- dataset_analise_2020

#com três variáveis.
base_ml<-
  dataset_analise %>%
  filter(deslocamento==0) %>%
  mutate(tipo_deslocamento = "local",
         nivel_hierarquia=nivel_hierarquia.x,
         percentual = perc.x) %>%
  select(nivel_hierarquia,percentual, tipo_deslocamento) %>%
    bind_rows(
      dataset_analise %>%
        filter(deslocamento==1) %>%
        mutate(tipo_deslocamento = "saida",
               nivel_hierarquia=nivel_hierarquia.x,
               percentual = perc.x)%>%
      select(nivel_hierarquia,percentual, tipo_deslocamento),
      dataset_analise %>%
        filter(deslocamento==1) %>%
        mutate(tipo_deslocamento = "entrada",
               nivel_hierarquia=nivel_hierarquia.y,
               percentual = perc.y)%>%
      select(nivel_hierarquia, percentual, tipo_deslocamento)

    )


is.na(base_ml)


control_dt <- trainControl(method="cv")

set.seed(1972)
Sys.time()
dt_model <- train(tipo_deslocamento~., data=base_ml, method="rpart",  trControl=control_dt)
Sys.time()

library(rattle)
fancyRpartPlot(dt_model$finalModel)


###Testando alguns achados do primeiro modelo

#1 - 58% dos deslocamentos que envolvem Centro locais estão associados à saída e é a categoria mais relevante para esse resultado
#2 - 95% dos deslocamentos que envolvem metrópoles nacionais são associados a atendimentos locais
dataset_analise %>%
  filter(deslocamento==0) %>%
  mutate(tipo_deslocamento = "local",
         nivel_hierarquia=nome_nivel_hierarquia.x,
         percentual = perc.x) %>%
  select(nivel_hierarquia,percentual, tipo_deslocamento) %>%
  bind_rows(
    dataset_analise %>%
      filter(deslocamento==1) %>%
      mutate(tipo_deslocamento = "saida",
             nivel_hierarquia=nome_nivel_hierarquia.x,
             percentual = perc.x)%>%
      select(nivel_hierarquia,percentual, tipo_deslocamento),
    dataset_analise %>%
      filter(deslocamento==1) %>%
      mutate(tipo_deslocamento = "entrada",
             nivel_hierarquia=nome_nivel_hierarquia.y,
             percentual = perc.y)%>%
      select(nivel_hierarquia, percentual, tipo_deslocamento)) %>%
  ggplot()+
  geom_bar(aes(y=nivel_hierarquia, fill=tipo_deslocamento), position="fill")



######### análise de modelo com três variáveis e amostras balanceadas.
base_ml_balanceada<-
  dataset_analise %>%
  filter(deslocamento==0) %>%
  mutate(tipo_deslocamento = "local",
         nivel_hierarquia=nivel_hierarquia.x,
         percentual = perc.x) %>%
  slice_sample(n=300000) %>%
  select(nivel_hierarquia,percentual, tipo_deslocamento) %>%
  bind_rows(
    dataset_analise %>%
      filter(deslocamento==1) %>%
      mutate(tipo_deslocamento = "saida",
             nivel_hierarquia=nivel_hierarquia.x,
             percentual = perc.x)%>%
      select(nivel_hierarquia,percentual, tipo_deslocamento),
    dataset_analise %>%
      filter(deslocamento==1) %>%
      mutate(tipo_deslocamento = "entrada",
             nivel_hierarquia=nivel_hierarquia.y,
             percentual = perc.y)%>%
      select(nivel_hierarquia, percentual, tipo_deslocamento)

  )


control_dt <- trainControl(method="cv")


Sys.time()
set.seed(1972)
dt_model_balanceada <- train(tipo_deslocamento~., data=base_ml_balanceada, method="rpart",  trControl=control_dt)
Sys.time()

library(rattle)
fancyRpartPlot(dt_model_balanceada$finalModel)

dt_model_balanceada$finalModel

###Testando alguns achados do segundo modelo

#1 Nível hierárquico 3A (Centro Sub-Regional A) é entrada para 50% no segundo modelo (um pouco menos no total de dadls)
dataset_analise %>%
  filter(deslocamento==0) %>%
  mutate(tipo_deslocamento = "local",
         nivel_hierarquia=nome_nivel_hierarquia.x,
         percentual = perc.x) %>%
  select(nivel_hierarquia,percentual, tipo_deslocamento) %>%
  bind_rows(
    dataset_analise %>%
      filter(deslocamento==1) %>%
      mutate(tipo_deslocamento = "saida",
             nivel_hierarquia=nome_nivel_hierarquia.x,
             percentual = perc.x)%>%
      select(nivel_hierarquia,percentual, tipo_deslocamento),
    dataset_analise %>%
      filter(deslocamento==1) %>%
      mutate(tipo_deslocamento = "entrada",
             nivel_hierarquia=nome_nivel_hierarquia.y,
             percentual = perc.y)%>%
      select(nivel_hierarquia, percentual, tipo_deslocamento)) %>%
  ggplot()+
  geom_bar(aes(y=nivel_hierarquia, fill=tipo_deslocamento), position="fill")

# 2 para os atendimentos que não envolvem categorias hierárquicas c("5","1B","4B","3B", "4A", "3A"),
# o percentual passa a ser importante para definição de atendimento local ou deslocamento de entrada


base_ml_balanceada %>%
  filter(!nivel_hierarquia %in% c("5","1B","4B","3B", "4A", "3A") ) %>%
  mutate(faixas_percentuais = case_when(
    percentual< 9.047426 ~"faixa 1",
    between(percentual,9.047426,9.421071)  ~"faixa 2",
    between(percentual, 9.4210711,13.97019) ~"faixa 3",
    between(percentual,13.970191, 23.084639) ~"faixa 4",
    percentual > 23.08464  ~"faixa 5"
  )) %>%
  ggplot() +
  geom_bar(aes(y=faixas_percentuais, fill=tipo_deslocamento), position="fill")


base_ml_balanceada %>%
  filter(!nivel_hierarquia %in% c("5","1B","4B","3B", "4A", "3A") ) %>%
  mutate(faixas_percentuais = case_when(
    percentual< 9.047426 ~"faixa 1",
    between(percentual,9.047426,9.421071)  ~"faixa 2",
    between(percentual, 9.4210711,13.97019) ~"faixa 3",
    between(percentual,13.970191, 23.084639) ~"faixa 4",
    percentual > 23.08464  ~"faixa 5"
  )) %>%
  ggplot() +
  geom_bar(aes(y=faixas_percentuais, fill=tipo_deslocamento))




########### clusterização por tipo de deslocamento

library(cluster)

agrupamento_municipio<-
  dataset_analise %>%
  filter(
    deslocamento ==1) %>%
  group_by(munic_res) %>%
  summarise(
    numero_internacoes = n()
  ) %>%
  mutate(code_muni = munic_res,
         tipo_deslocamento = "saida" ) %>%
  bind_rows(
    dataset_analise %>%
      filter(
        deslocamento ==1) %>%
      group_by(codufmun) %>%
      summarise(
        numero_internacoes = n()
      ) %>%
      mutate(code_muni = codufmun,
             tipo_deslocamento = "entrada"),
    dataset_analise %>%
      filter(
        deslocamento ==0) %>%
      group_by(codufmun) %>%
      summarise(
        numero_internacoes = n()
      ) %>%
      mutate(code_muni = codufmun,
             tipo_deslocamento = "local")
  ) %>%
  group_by(code_muni, tipo_deslocamento) %>%
  summarise(
    total_internacoes = sum(numero_internacoes)
  )

agrupamento_municipio<-
  agrupamento_municipio %>%
  tidyr::pivot_wider(names_from = tipo_deslocamento, values_from = total_internacoes) %>%
  mutate(liquido = ifelse(is.na(entrada),0,entrada)+
           ifelse(is.na(local),0,local)-
           ifelse(is.na(saida),0,saida))

agrupamento_municipio<-
  agrupamento_municipio %>%
  mutate(local = ifelse(is.na(local),0,local),
         saida = ifelse(is.na(saida),0,saida),
         entrada = ifelse(is.na(entrada),0,entrada),
         perc_saida = saida/(saida+local)*100,
         perc_entrada = entrada/(entrada+local)*100,
         perc_entrada = ifelse(is.nan(perc_entrada),0,perc_entrada))


purrr::map_dbl(2:5,function(k){
  set.seed(1972)
  print(k)
  model_cluster<- pam(x=agrupamento_municipio[,6:7],k)
  model_cluster$silinfo$avg.width

})

model_cluster_2_k<- pam(x=agrupamento_municipio[,6:7],2)


set.seed(1972)
library(cluster)
model_cluster_4_k<- pam(x=agrupamento_municipio[,6:7],4)

agrupamento_municipio$cluster_4_k<- factor(model_cluster_4_k$clustering,
                                           levels = c(2,1,3,4),
                                           labels = c("entrada moderada",
                                                      "saída fraca",
                                                      "saída moderada",
                                                      "saída forte") )

saveRDS(agrupamento_municipio,file="agrupamento_municipio_2020.RDS")

agrupamento_municipio %>%
  ggplot()+
  geom_point(aes(x=perc_entrada, y=perc_saida, color=cluster_4_k))

dataset_analise %>%
  filter(deslocamento == 1,
         #perc.x>0,
         perc.x<=50,
         !is.na(nome_nivel_hierarquia.x)) %>% #filtro para tirar os outliers
  distinct(nome_nivel_hierarquia.x,munic_res, perc.x) %>%
  inner_join(
    agrupamento_municipio %>%
      rename(munic_res=code_muni)
  ) %>%
  ggplot() +
  geom_jitter(aes(x=nome_nivel_hierarquia.x, y=perc.x, fill=perc_saida), pch=21, color="#444444",size=2)+
  geom_boxplot(aes(x=nome_nivel_hierarquia.x, y=perc.x),fill=NA, color= "white", outlier.shape = NA)+
  scale_fill_continuous_sequential(palette= "Red-Yellow")+
  theme_light() +
  theme(
    #text = element_text(size=20),
    panel.background = element_rect(fill = "black"),
    panel.grid = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    strip.background = element_rect(fill = "#505050"),
    strip.text = element_text(color = "white"),
    #axis.text = element_blank(),
    axis.text.x = element_text(angle = 90),
    legend.key = element_rect(fill = "#15202B")

  )+
  facet_wrap(cluster_4_k~.)



dataset_analise %>%
  filter(deslocamento == 1,
         #perc.y>0,
         perc.y<=50) %>%
  distinct(nome_nivel_hierarquia.y,codufmun, perc.y) %>%
  inner_join(
    agrupamento_municipio %>%
      rename(codufmun=code_muni)
  ) %>%
  ggplot() +
  geom_jitter(aes(x=cluster_4_k, y=perc.y, fill=perc_entrada), pch=21, color="#444444",size=2)+
  geom_boxplot(aes(x=cluster_4_k, y=perc.y),fill=NA, color= "white")+
  scale_fill_continuous_sequential(palette= "Red-Yellow")+
  theme_light() +
  theme(
    #text = element_text(size=20),
    panel.background = element_rect(fill = "black"),
    panel.grid = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    strip.background = element_rect(fill = "#505050"),
    strip.text = element_text(color = "white"),
    #axis.text = element_blank(),
    legend.key = element_rect(fill = "#15202B")

  ) +
  facet_wrap(nome_nivel_hierarquia.y~.)




dataset_analise %>%
  mutate(deslocamento = as.factor(deslocamento)) %>%
  group_by(deslocamento,nome_nivel_hierarquia.x, munic_res) %>%
  summarise(
    quantidade_internacoes =n()
  )%>%
  mutate(tipo_deslocamento = ifelse(deslocamento=="0","local","saída")) %>%
  mutate(nome_nivel_hierarquia = reorder(nome_nivel_hierarquia.x, quantidade_internacoes)) %>%
  ungroup() %>%
  bind_rows(
    dataset_analise %>%
      filter(deslocamento == 1) %>%
      mutate(deslocamento = as.factor(deslocamento)) %>%
      group_by(deslocamento,nome_nivel_hierarquia.y) %>%
      summarise(
        quantidade_internacoes =n()
      )%>%
      mutate(tipo_deslocamento = "chegada") %>%
      mutate(nome_nivel_hierarquia= reorder(nome_nivel_hierarquia.y, quantidade_internacoes)) %>%
      ungroup()
  )



######### Outra experiência com decision tree

#com três variáveis.
base_ml_hierarquia_saida<-
  dataset_analise %>%
  filter(deslocamento==1) %>%




  mutate(tipo_deslocamento = "local",
         nivel_hierarquia=nivel_hierarquia.x,
         percentual = perc.x) %>%
  select(nivel_hierarquia,percentual, tipo_deslocamento) %>%
  bind_rows(
    dataset_analise %>%
      filter(deslocamento==1) %>%
      mutate(tipo_deslocamento = "saida",
             nivel_hierarquia=nivel_hierarquia.x,
             percentual = perc.x)%>%
      select(nivel_hierarquia,percentual, tipo_deslocamento),
    dataset_analise %>%
      filter(deslocamento==1) %>%
      mutate(tipo_deslocamento = "entrada",
             nivel_hierarquia=nivel_hierarquia.y,
             percentual = perc.y)%>%
      select(nivel_hierarquia, percentual, tipo_deslocamento)

  )



agrupamento_municipio %>%
  ungroup() %>%

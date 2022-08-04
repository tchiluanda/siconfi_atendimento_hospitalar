library(caret)

load("dataset_analise.RDS")


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
#2 - 95% dos deslocamentos que envolvem metrópoles nacional são associados a atendimentos locais
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

#Anotações da conversa com Beatriz Jardim
# rede de atenção primária: UBS referência dos pacientes para os hospitais (atendimento de custo muito baixo)
# perfil de atendimento
#
# SIA (vem coisas do UBS)
# SIH (internação hospitalar) autorização de internação hospitalar
# ver por habilitação
#
# CNES verificar
#
# Verificar por especialização (inclusive por doença específica. ex: colo do útero)
#
# região de saúde



dataset_analise <- readRDS("~/Github/siconfi_atendimento_hospitalar/dataset_analise.RDS")

#Quantidade de atendimento total por tipo de deslocamento
dataset_analise %>%
  ggplot() +
  geom_bar(aes(x=as.factor(deslocamento)))


fab<-
  dataset_analise %>%
  group_by(nome_nivel_hierarquia.x) %>%
  summarise(
    quantidade_internacoes_hierarquia
  ) %>%
  inner_join(
    dataset_analise %>%
      mutate(tipo_deslocamento = ifelse(deslocamento==0,"local","saída")) %>%
      group_by(tipo_deslocamento,nome_nivel_hierarquia.x) %>%
      summarise(
        quantidade_internacoes_tipo_deslocamento =n()
      )
  )

fab<-
  dataset_analise %>%
  mutate(deslocamento = as.factor(deslocamento)) %>%
  group_by(deslocamento,nome_nivel_hierarquia.x) %>%
  summarise(
    quantidade_internacoes =n()
  )%>%
  mutate(tipo_deslocamento = ifelse(deslocamento=="0","local","saída")) %>%
  mutate(nome_nivel_hierarquia.x = reorder(nome_nivel_hierarquia.x, quantidade_internacoes)) %>%
  ungroup() %>%
  bind_rows(
    dataset_analise %>%
      filter(deslocamento == 1) %>%
      mutate(deslocamento = as.factor(deslocamento)) %>%
      group_by(deslocamento,nome_nivel_hierarquia.x) %>%
      summarise(
        quantidade_internacoes =n()
      )%>%
      mutate(tipo_deslocamento = "chegada") %>%
      mutate(nome_nivel_hierarquia.x = reorder(nome_nivel_hierarquia.x, quantidade_internacoes)) %>%
      ungroup()
  )


#Distribuição de quantidade de internações com facets por tipo de deslocamento e ranking por nível hirárquico
dataset_analise %>%
  mutate(deslocamento = as.factor(deslocamento)) %>%
  group_by(deslocamento,nome_nivel_hierarquia.x) %>%
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
  ) %>%
  ggplot(aes(x=quantidade_internacoes, y=nome_nivel_hierarquia, fill=tipo_deslocamento)) +
  geom_col()+
  facet_wrap(tipo_deslocamento~.)

#Distribuição de quantidade de internações com facets por tipo de deslocamento e ranking por nível hirárquico
#com escala livre
dataset_analise %>%
  mutate(deslocamento = as.factor(deslocamento)) %>%
  group_by(deslocamento,nome_nivel_hierarquia.x) %>%
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
  ) %>%
  ggplot(aes(x=quantidade_internacoes, y=nome_nivel_hierarquia)) +
  geom_col()+
  facet_wrap(tipo_deslocamento~., scales = "free_x")


#Distribuição de quantidade de internações com facets por nível hierárquico e ranking por tipo de deslocamento
dataset_analise %>%
  mutate(deslocamento = as.factor(deslocamento)) %>%
  group_by(deslocamento,nome_nivel_hierarquia.x) %>%
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
  ) %>%
  ggplot(aes(x=quantidade_internacoes, y=tipo_deslocamento)) +
  geom_col()+
  facet_wrap(nome_nivel_hierarquia~.)


#Distribuição de quantidade de internações com facets por nível hierárquico e ranking por tipo de deslocamento
#com escala livre

dataset_analise %>%
  mutate(deslocamento = as.factor(deslocamento)) %>%
  group_by(deslocamento,nome_nivel_hierarquia.x) %>%
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
  ) %>%
  ggplot(aes(x=quantidade_internacoes, y=tipo_deslocamento)) +
  geom_col()+
  facet_wrap(nome_nivel_hierarquia~., scales = "free_x")


#Siatribuição de percentual de gastos por tipo de deslocamento
dataset_analise %>%
  filter(deslocamento==0) %>%
  filter(perc.x>0) %>%
  ggplot() +
  geom_density(aes(x=perc.x),color="white", fill="black")+
  scale_x_log10()

dataset_analise %>%
  filter(deslocamento==0) %>%
  filter(perc.x>0) %>%
  summarise(mean(log(perc.x)),
            median(log(perc.x)))

dataset_analise %>%
  filter(deslocamento==1) %>%
  filter(perc.x>0) %>%
  ggplot() +
  geom_density(aes(x=perc.x), fill="black") +
  scale_x_log10()

dataset_analise %>%
  filter(deslocamento==1) %>%
  filter(perc.x>0) %>%
  summarise(mean(log(perc.x)),
            median(log(perc.x)))


dataset_analise %>%
  filter(deslocamento==1) %>%
  filter(perc.y>0) %>%
  ggplot() +
  geom_density(aes(x=perc.y), fill="black")+
  scale_x_log10()

dataset_analise %>%
  filter(deslocamento==1) %>%
  filter(perc.y>0) %>%
  summarise(mean(log(perc.y)),
            median(log(perc.y)))


analise_aov<-
dataset_analise %>%
  filter(deslocamento==1) %>%
  filter(perc.x>0) %>%
  mutate(tipo_deslocamento="saída",
         percentual = log(perc.x)) %>%
  select(tipo_deslocamento,percentual ) %>%
  bind_rows(
    dataset_analise %>%
      filter(deslocamento==1) %>%
      filter(perc.y>0) %>%
      mutate(tipo_deslocamento="entrada",
             percentual = log(perc.y))%>%
      select(tipo_deslocamento,percentual ),
    dataset_analise %>%
      filter(deslocamento==0) %>%
      filter(perc.x>0) %>%
      mutate(tipo_deslocamento="local",
             percentual = log(perc.x))%>%
      select(tipo_deslocamento,percentual )

  )



res.aov<- aov(formula= percentual ~ tipo_deslocamento, data= analise_aov)
summary(res.aov)

TukeyHSD(res.aov)

#Alcance dos deslocamentos por tipo de deslocamento e por nível de heirarquia

#saída
dataset_analise %>%
  filter(deslocamento==1) %>%
  ggplot() +
  scale_x_log10() +
  geom_histogram(aes(x= distancia), color= "white") +
  facet_wrap(nome_nivel_hierarquia.x~., scales = "free_y")

#chegada
dataset_analise %>%
  filter(deslocamento==1) %>%
  ggplot() +
  geom_histogram(aes(x= distancia), color= "white") +
  scale_x_log10() +
  facet_wrap(nome_nivel_hierarquia.y~., scales = "free_y")


#Verificar correlação entre percentual de gasto e distância percorrida
#saída
dataset_analise %>%
  filter(perc.x>0,
         deslocamento ==1) %>%
  ggplot() +
  geom_point(aes(x=perc.x,y=distancia))+
  facet_wrap(nome_nivel_hierarquia.x~.)

cor(dataset_analise$perc.x[dataset_analise$deslocamento==1],
    dataset_analise$distancia[dataset_analise$deslocamento==1])


#Verificar correlação entre percentual de gasto e distância percorrida
#chegada
dataset_analise %>%
  filter(perc.y>0,
         deslocamento ==1) %>%
  ggplot() +
  geom_point(aes(x=perc.y,y=distancia)) +
  facet_wrap(nome_nivel_hierarquia.y~.)

cor(dataset_analise$perc.y[dataset_analise$deslocamento==1],
    dataset_analise$distancia[dataset_analise$deslocamento==1])




#Teste chi-quadrado entre os níveis hierárquicos de saíde e entrada
#sugestão: explorar Diagrama de fluxo entre os níveis hierárquicos de saída e entrada


teste<-chisq.test(dataset_analise$nome_nivel_hierarquia.x[dataset_analise$deslocamento==1],
                  dataset_analise$nome_nivel_hierarquia.y[dataset_analise$deslocamento==1],
                  simulate.p.value=TRUE)


teste$stdres

REGIC_trabalho %>%
  distinct(nivel_hierarquia, nome_nivel_hierarquia)

###########Ranking cidades: Valores absolutos

dataset_analise %>%
  filter(deslocamento==1) %>%
  group_by(nome_nivel_hierarquia.x, mun_res_nome.x) %>%
  summarise(
    quantidade_deslocamentos = n()
  ) %>%
  ungroup() %>%
  slice_max(order_by = quantidade_deslocamentos, n=20) %>%
  mutate(munic_res = reorder(mun_res_nome.x,quantidade_deslocamentos)) %>%
  ggplot(aes(x=quantidade_deslocamentos, y=munic_res, fill= nome_nivel_hierarquia.x)) +
  geom_col()

dataset_analise %>%
  filter(deslocamento==1) %>%
  group_by(nome_nivel_hierarquia.y,mun_res_nome.y) %>%
  summarise(
    quantidade_deslocamentos = n()
  ) %>%
  ungroup() %>%
  slice_max(order_by = quantidade_deslocamentos, n=20) %>%
  mutate(munic_res = reorder(mun_res_nome.y,quantidade_deslocamentos)) %>%
  ggplot(aes(x=quantidade_deslocamentos, y=munic_res,fill= nome_nivel_hierarquia.y )) +
  geom_col()


dataset_analise %>%
  filter(deslocamento==0) %>%
  group_by(nome_nivel_hierarquia.x, mun_res_nome.x) %>%
  summarise(
    quantidade_deslocamentos = n()
  ) %>%
  ungroup() %>%
  slice_max(order_by = quantidade_deslocamentos, n=20) %>%
  mutate(munic_res = reorder(mun_res_nome.x,quantidade_deslocamentos)) %>%
  ggplot(aes(x=quantidade_deslocamentos, y=munic_res, fill= nome_nivel_hierarquia.x)) +
  geom_col()




###########Ranking cidades: Valores percentuais


dados_agrupados<-
  dataset_analise %>%
  filter(deslocamento==1) %>%
  mutate(municipio = mun_res_nome.x,
         hierarquia = nome_nivel_hierarquia.x,
         id_municipio = munic_res) %>%
  select(id_municipio,
         municipio,
         hierarquia) %>%
  bind_rows(
    dataset_analise %>%
      filter(deslocamento==1) %>%
      mutate(municipio = mun_res_nome.y,
             hierarquia = nome_nivel_hierarquia.y,
             id_municipio = codufmun) %>%
      select(id_municipio,
             municipio,
             hierarquia)
      ,
    dataset_analise %>%
      filter(deslocamento==0) %>%
      mutate(municipio = mun_res_nome.x,
             hierarquia = nome_nivel_hierarquia.x,
             id_municipio = munic_res)%>%
      select(id_municipio,
             municipio,
             hierarquia)) %>%
  group_by(id_municipio,hierarquia, municipio) %>%
  summarise(
    quantidade_deslocamentos_total = n()
  ) %>%
  ungroup()


dados_agrupados %>%
  summarise(mean(quantidade_deslocamentos_total),
            median(quantidade_deslocamentos_total))


dados_agrupados %>%
  ggplot() +
  geom_density(aes(x=quantidade_deslocamentos_total)) +
  scale_x_log10()

###########Ranking cidades: Valores normalizados

dataset_analise %>%
  filter(deslocamento==1) %>%
  mutate(id_municipio = munic_res) %>%
  #filter(perc.x>0) %>%
  group_by(id_municipio, nome_nivel_hierarquia.x, mun_res_nome.x) %>%
  summarise(
    mediana_distancia_percorrida = median(distancia),
    quantidade_deslocamentos = n()
  ) %>%
  ungroup() %>%
  inner_join(dados_agrupados) %>%
  mutate(mediana_distancia_padronizada =  mediana_distancia_percorrida * quantidade_deslocamentos/ quantidade_deslocamentos_total) %>%
  slice_max(order_by = mediana_distancia_padronizada, n=20) %>%
  mutate(munic_res = reorder(mun_res_nome.x,mediana_distancia_padronizada)) %>%
  ggplot(aes(x=mediana_distancia_padronizada, y=munic_res, fill= nome_nivel_hierarquia.x)) +
  geom_col()


dataset_trabalho_graf_mediana<-
  dataset_analise %>%
  filter(deslocamento==1) %>%
  mutate(id_municipio = munic_res) %>%
  #filter(perc.x>0) %>%
  group_by(id_municipio, nome_nivel_hierarquia.x, mun_res_nome.x) %>%
  summarise(
    mediana_distancia_percorrida = median(distancia),
    quantidade_deslocamentos = n()
  ) %>%
  ungroup() %>%
  inner_join(dados_agrupados) %>%
  mutate(mediana_distancia_padronizada =  mediana_distancia_percorrida * quantidade_deslocamentos/ quantidade_deslocamentos_total)

dataset_analise %>%
  filter(deslocamento==1) %>%
  mutate(id_municipio = codufmun) %>%
  #filter(perc.y >0) %>%
  group_by(id_municipio,nome_nivel_hierarquia.y,mun_res_nome.y) %>%
  summarise(
    municipios_atendidos = n_distinct(mun_res_nome.x),
    quantidade_deslocamentos = n()
  ) %>%
  ungroup() %>%
  inner_join(dados_agrupados) %>%
  mutate(municipios_atendidos_padronizado =  municipios_atendidos * quantidade_deslocamentos/ quantidade_deslocamentos_total) %>%
  slice_max(order_by = municipios_atendidos_padronizado, n=20) %>%
  mutate(munic_res = reorder(mun_res_nome.y,municipios_atendidos_padronizado)) %>%
  ggplot(aes(x=municipios_atendidos, y=munic_res,fill= nome_nivel_hierarquia.y )) +
  geom_col()

dataset_trabalho_graf_atracao<-
  dataset_analise %>%
  filter(deslocamento==1) %>%
  mutate(id_municipio = codufmun) %>%
  #filter(perc.y >0) %>%
  group_by(id_municipio,nome_nivel_hierarquia.y,mun_res_nome.y) %>%
  summarise(
    municipios_atendidos = n_distinct(mun_res_nome.x),
    quantidade_deslocamentos = n()
  ) %>%
  ungroup() %>%
  inner_join(dados_agrupados) %>%
  mutate(municipios_atendidos_padronizado =  municipios_atendidos * quantidade_deslocamentos/ quantidade_deslocamentos_total)



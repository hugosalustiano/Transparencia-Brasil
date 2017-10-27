##RELATORIO REFORMA POLITICA 2

setwd("C:\\Users\\Renato\\Desktop\\relatorio reforma politica 2")

library(tidyr)
library(dplyr)
library(data.table)

##CARREGANDO NUMERO DE CADEIRAS POR PARTIDO Observacoes:
#1) Total deputados = 512. Total senadores = 79 (em 25/10/17 havia 2 sem partido)
#2) Titularidade da cadeira: a) deputados: titulares os partidos dos titulares ou
#suplentes efetivados; b) senadores: quando houve renuncia, falecimento,
#ou cassacao do mandato, considerei titular o partido do 1o suplente em exercicio.

ndepsen <- fread("depts_sen.csv")

##C?LCULO DA DISTRIBUI??O DO FUNDO NOVO ENTRE PARTIDOS. Total = 1,7 bilh?o.
#Regras: coluna I: 2% distribu?do entre todos; 
#II - 35% distribu?do na propor??o dos votos recebidos (nominal+legenda; partidos com eleitos)
#III - 48% distribu?do na propor??o do n? deputados
#IV - 15% distribu?do na propor??o n? de senadores

##C?LCULO DA DISTRIBUI??O DO FUNDO PARTID?RIO. Total 2018 = 888 milh?es
#(http://g1.globo.com/jornal-nacional/noticia/2017/09/governo-reserva-quase-r-900-milhoes-para-fundo-partidario.html).
#I - 5% distribu?do entre todos
#II - 95% distribu?do na propor??o dos votos recebidos
#D?vida: diferen?a arts. 41 e 41-A da lei 9096/95: qual propor??o usar?

total <- 1700000000
total2 <- 888000000

fundo_eleitoral <- ndepsen %>%
  mutate(perc_votos_obtidos = Deputados/sum(Deputados),
         cadeiras_dep_hoje = Deputados/sum(Deputados),
         cadeiras_sen_hoje = Senadores/sum(Senadores),
         novo1 = total*.02/n(),
         novo2 = total*.35*perc_votos_obtidos,
         novo3 = total*.48*cadeiras_dep_hoje,
         novo4 = total*.15*cadeiras_sen_hoje,
         fundo_novo_total = novo1 + novo2 + novo3 + novo4,
         antigo1 = total2*.05/n(),
         antigo2 = total2*0.95*perc_votos_obtidos,
         fundo_antigo_total = antigo1 + antigo2,
         fundos_total = fundo_novo_total + fundo_antigo_total)

head(fundo_eleitoral)

fundo_eleitoral %>%
  summarise(valid1 = sum(perc_votos_obtidos),
            valid2 = sum(cadeiras_dep_hoje),
            valid3 = sum(cadeiras_sen_hoje),
            valid4 = sum(fundo_novo_total),
            valid5 = sum(fundo_antigo_total),
            valid6 = sum(fundos_total))


##substituir ap?s c?lculo exato:
# novo2 = 1700000000 * 0,35 * (votos candidatos+legenda/votos todos os candidatos+legendas eleitas)


##RELAT?RIO ANTERIOR (alterado). Dado a obter: quanto de doa??o PF teve cada partido?

###############
####C?lculo tipos de receita em 2014

setwd("C:\\Users\\Renato\\Desktop\\Mini-relatório regras eleitorais\\dados_financ_eleit")

rec14 <- fread("receitas_candidatos_2014_brasil.txt")

library(janitor)

##renomear colunas
rec14 <- rec14 %>%
  clean_names()

##composi??o doa??es
rec14 %>%
  group_by(tipo_receita) %>%
  summarise(contagem = n(),
            gasto = sum(as.numeric(gsub(",", "\\.", valor_receita)))) %>%
  ungroup() %>%
  mutate(total_gasto = sum(gasto),
         perc_gasto = round(gasto/total_gasto, 2))

##pegar CNPJ dos diret?rios

library(readr)

cnpj2015 <- read_fwf(
  file="CNPJ_diretorios_partidarios_2015.txt",  
  skip=1,
  fwf_widths(c(1, 2, 14, 150),
             c("registro", "tipo", "cnpj", "nome_partido")))

##determinar quais CNPJs na planilha de receitas s?o de partidos

rec14_backup <- rec14

rec14_r <- rec14 %>%
  select(sigla_partido, nome_do_doador, nome_do_doador_originário, valor_receita, tipo_receita, 
         fonte_recurso, cpf_cnpj_do_doador, cpf_cnpj_do_doador_originário) %>%
  mutate(cnpj_final = ifelse(cpf_cnpj_do_doador_originário == "#NULO",
                             cpf_cnpj_do_doador, cpf_cnpj_do_doador_originário)) %>%
  left_join(cnpj2015, 
            by=c('cnpj_final'='cnpj')) %>%
  mutate(bol_partido = as.numeric(!is.na(tipo)))

##descobrir quem ? comit? e separar nas categorias finais de tipo de doa??o 

rec14_r <- rec14_r %>%
  mutate(bol_juridica = ifelse(nchar(cnpj_final) <= 11, "PF", "PJ"),
         comite = ifelse(grepl("comit", tolower(nome_do_doador_originário)), "comitê", "outro"),
         comite2 = ifelse(grepl("comit", tolower(nome_do_doador)) & bol_juridica == "PJ" & cpf_cnpj_do_doador_originário == "#NULO", "comitê", "outro"),
         tipo_recurso_tb = case_when( bol_juridica == "PJ" & bol_partido == 0  & grepl("outro", comite) & grepl("outro", comite2) ~ "empresas", # & grepl("outro", comite) & grepl("outro", comite2) 
                                      tipo_receita != "Recursos próprios" & bol_juridica == "PF" & grepl("outro", comite) & grepl("outro", comite2) ~ "PF outros",
                                      tipo_receita == "Recursos próprios" & bol_juridica == "PF" ~ "Recursos Próprios",
                                      bol_juridica == "PJ" & bol_partido == 1 ~ "partidos",
                                      bol_juridica == "PJ" & bol_partido == 0 & (grepl("outro", comite) | grepl("outro", comite2)) ~"partidos",
                                      T ~ "PF outros")) 

##valida??o feita para criar tipo_recurso_tb

resultado <- rec14_r%>%
  group_by(tipo_receita, bol_partido, bol_juridica, comite, comite2, tipo_recurso_tb) %>%
  summarise(contagem = n(),
            gasto = sum(as.numeric(gsub(",", "\\.", valor_receita)))) %>%
  ungroup() %>%
  mutate(total_gasto = sum(gasto),
         perc_gasto = round(gasto/total_gasto, 2))

#visualiza??o final

resultado14 <- rec14_r%>%
  group_by(tipo_recurso_tb, sigla_partido) %>%
  summarise(contagem = n(),
            gasto = sum(as.numeric(gsub(",", "\\.", valor_receita)))) %>%
  ungroup() %>%
  mutate(total_gasto = sum(gasto),
         perc_gasto = round(gasto/total_gasto, 2))

#visualiza??o s? PF outros

resultado14_PFoutros <- resultado14 %>%
  filter(tipo_recurso_tb=="PF outros")

View(resultado14_PFoutros)

#receita total por partido

receita_partido <- rec14_r%>%
  group_by(sigla_partido) %>%
  summarise(receita = sum(as.numeric(gsub(",", "\\.", valor_receita))))

##Reduzir resultado14_PFoutros pra dar join na tabela fundo_eleitoral

doac_PF_14 <- resultado14_PFoutros %>%
  select(sigla_partido, gasto)

#############

##FUNDOS + DOA??ES PESSOA F?SICA + (objeto doac_PF_14) + compara??o 2014 (corrigir valores 2014)

fundo_PF <- fundo_eleitoral %>%
  left_join(y=doac_PF_14, by=c("Partido"="sigla_partido")) %>%
  rename(doac_PF = gasto) %>%
  mutate(receita_18 = fundos_total + doac_PF) %>%
  left_join(y=receita_partido, by=c("Partido"="sigla_partido")) %>%
  rename(comparacao_14 = receita) %>%
  mutate(perc = round(receita_18/comparacao_14, 2))

write.table(fundo_PF, file="receitas_partidos2018.csv", sep=";", row.names = F)
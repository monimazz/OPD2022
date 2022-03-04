#teste OPD com dados do IDESP

#https://dados.educacao.sp.gov.br/dataset/%C3%ADndice-de-desenvolvimento-da-educa%C3%A7%C3%A3o-do-estado-de-s%C3%A3o-paulo-idesp-por-escola

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#===== SUBINDO OS DADOS  ========
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(readr)
library(dplyr)

IDESP_Escolas_2007_2019_EM <- read_csv2("IDESP_Escolas_2007_2019_EM.csv")

glimpse(IDESP_Escolas_2007_2019_EM)
 
#IDESP_leitura_errada <- read_delim("IDESP_Escolas_2007_2019_EM.csv")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#===== FILTRANDO OS DADOS  ========
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(dplyr)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Somente Mogi ----------------------------------------
 IDESP_MOGI <- IDESP_Escolas_2007_2019_EM %>% 
  filter(MUNICIPIO == "MOGI DAS CRUZES")

#Vendo o formato dos dados 
 glimpse(IDESP_MOGI)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Quais as escolas que tiveram melhor desempenho em 2019? -------

IDESP_2019T10 <- IDESP_MOGI %>% 
  filter(`2019` > 0) %>% 
  top_n(`2019`, n = 10)
 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Quais escolas tiveram uma melhoria em seu desempenho em um ano? -------
#De 2018 a 2019

IDESP_MOGI1819 <- IDESP_MOGI %>% 
  filter(`2019` > 0) %>% 
  mutate(ev = `2019`-`2018`,
         top102019 = case_when( `2019`> 2.84 ~ "TOP10",
                               TRUE ~ "Outro")) 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Como foi o crescimento desde o primeiro registro do IDESP? --------
 
library(tidyr)

IDESP_MOGI_med <- IDESP_MOGI %>% 
  group_by(ESCOLA) %>% 
  pivot_longer(cols = c(`2007`:`2019`),
               names_to = "Ano", values_to = "IDESP") %>% 
  mutate(Diff = IDESP - lag(IDESP))


IDESP_MOGI_med10 <- IDESP_MOGI_med %>% 
  group_by(ESCOLA) %>% 
  summarize(mediana_tempo = median(Diff, na.rm = T))


IDESP_MOGI_med10 <- IDESP_MOGI_med10 %>% 
  top_n(mediana_tempo, n = 10)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#INSE das escolas x qualidade das escolas -----------------

#https://dados.educacao.sp.gov.br/dataset/%C3%ADndice-de-n%C3%ADvel-socioecon%C3%B4mico-inse-por-escola/resource/35ab614b-b6a0-41a8-b044-e4b4bf07bcce#{

INSE <- read_csv2("INSE_Geral 2018_1_0.csv")

INSEMOGI <- INSE %>% 
  filter(MUN == "MOGI DAS CRUZES")

INSEMOGIIDESP <- left_join(IDESP_MOGI1819, INSEMOGI, by=c("ESCOLA"="NOMESC"))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#===== VISUALIZANDO OS DADOS  ========
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


library(ggplot2)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#TOP 10 escolas 2019 ------------------------

IDESP_2019T10 %>% 
  ggplot(aes(x = reorder(ESCOLA,`2019`), y=`2019`)) +
  geom_col(fill = "lightblue") +
  coord_flip() +
  theme_classic() +
  labs(title = "Escolas com maior desempenho no IDESP", x = "ESCOLA")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#TOP 10 escolas 2018/2019 ------------------------
IDESP_MOGI1819 %>% 
  ggplot(aes(x = reorder(ESCOLA,ev), y=ev, fill = top102019)) +
  geom_col() +
  coord_flip() +
  theme_classic() +
  labs(title = "Escolas com maior desempenho no IDESP",
       x = "ESCOLA", y= "Diferenca do desempenho de 2019 a 2018",
       fill = "Desempenho em 2019")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Rendimento geral ano a ano ----------------

 IDESP_MOGI_med %>% 
  ungroup() %>% 
  ggplot(aes(x= Ano, y=IDESP,  group=ESCOLA, color=ESCOLA)) +
  geom_line() +
  theme(legend.position="none") #-> p

  
IDESP_MOGI_med %>% 
  ungroup() %>% 
  ggplot(aes(x= Ano, y=Diff,  group=ESCOLA, color=ESCOLA)) +
  geom_line() +
  theme(legend.position="none") #-> p


plotly::ggplotly(p)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Top 10 escolas com mediana de crescimento desde 2007 ------------

IDESP_MOGI_med10 %>% 
  ungroup() %>% 
  ggplot(aes(x= reorder(ESCOLA,mediana_tempo), y=mediana_tempo)) +
  geom_bar(stat = "identity") +
  coord_flip()


IDESP_MOGI_med10 <- IDESP_MOGI_med10 %>% 
  mutate(tipo = "Top 10 crescimento IDESP")

IDESP_MOGI_med <- left_join(IDESP_MOGI_med, IDESP_MOGI_med10)

IDESP_MOGI_med %>% 
  filter(tipo != is.na(tipo)) %>% 
  ggplot(aes(x= Ano, y=Diff,  group=ESCOLA, color=ESCOLA)) +
  geom_line() #-> p


#plotly::ggplotly(p)

IDESP_MOGI_med %>% 
  filter(tipo != is.na(tipo)) %>% 
  ggplot(aes(x= Ano, y=IDESP,  group=ESCOLA, color=ESCOLA)) +
  geom_line() #-> p


#plotly::ggplotly(p)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# INSE e qualidade das escolas ---------------

#Relacao INSE e IDESP e top 10 escolas

INSEMOGIIDESP %>% 
  mutate(tipo = case_when(`2019` > 2.84 ~ "Top", TRUE ~ "Outro")) %>% 
  ggplot(aes(x= `2019`, y=`NIVEL SOCIOECONOMICO DOS ALUNOS`, color = tipo)) + 
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE)


#EXERCICIO: REPLICAR COM A BASE DO ENSINO FUNDAMENTAL ANOS FINAIS ===========

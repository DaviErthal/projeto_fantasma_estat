#Esse código é referente à confecção do Projeto fantasma da ESTAT
#Estudante: Davi Dantas Erthal
#Matrícula: 200016741
#Link para o Overleaf: https://pt.overleaf.com/project/606b65b4c2db8018922858f2

#Indicando diretório

setwd("C:/Users/Erthal/OneDrive/Documentos/UNB/CE2/.ESTAT")

#Carregando Pacotes

pacman::p_load(tidyverse, ggcorrplot, scales, stringr, cowplot, RColorBrewer)

#O arquivo será separado pelas questões pedidas pelo cliente, são elas:
# 1 - número de lançamentos por ano
# 2 - Top 5 diretores segundo a nota do IMDb
# 3 - Se há relação entre entre a duração dos filmes e o seu ano de lançamento
# 4 - Se a nota do Rotten Tomatoes é influenciada pelo tempo de duração do filme
# 5 - A distribuição de classificação indicativa do filme por plataforma
# 6 - Uma comparação entre o IMDb das plataformas Netflix e Prime



#========================QUESTÃO 1==========================================

#Para primeira questão sobre número de lançamentos por ano:

#Uma tabela simples contendo o ano e número de vezes que aparece:

lancamentos_ano <- MoviesOnStreamingPlatforms...Copia %>% 
  group_by(Year) %>%
  summarise(freq = n())

#Um gráfico de linha do número de lançamentos por ano:

ggplot(lancamentos_ano, aes(x=Year, y=freq, group=1)) +
  geom_line(size=1,colour="#A11D21") + geom_point(colour="#A11D21",size=2) +
  labs(x="Ano", y="Quantidade de Lançamentos") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  ggsave("graf_lancamentos_ano.png", width = 158, height = 93, units = "mm")

#========================QUESTÃO 2==========================================

#Para a segunda questão: Top 5 diretores segundo a nota do IMDb:

#Tabela dos diretores e suas médias de nota no IMDb.

top5diretores <- MoviesOnStreamingPlatforms...Copia %>% group_by(Directors) %>% 
  summarise(freq= mean(IMDb)) %>%
  mutate(n = n())
mutate(freq_relativa = round((freq/sum(freq))*100,2))

porcentagens <- str_c(tabela$freq_relativa, '%') %>%
  str_replace('\\.',',')

#Boxplot da nota dos diretores.

ggplot(top5diretores, aes(x=factor(""), y=freq)) +
  geom_boxplot(fill=c("#A11D21"), width = 0.5) +
  guides(fill=FALSE) +
  stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="Nota dos diretores")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black")) +
  ggsave("graf_boxplot_nota_diretores.png", width = 158, height = 93, units = "mm")

media_diretores <- mean(top5diretores$freq, na.rm = TRUE)
mediana_diretores <- median(top5diretores$freq, na.rm = TRUE)
desvio_diretores <- sd(top5diretores$freq, na.rm = TRUE)
Q1_diretores <- quantile(top5diretores$freq, probs = 0.25, na.rm = TRUE)
Q3_diretores <- quantile(top5diretores$freq, probs = 0.75, na.rm = TRUE)


#========================QUESTÃO 3==========================================

#Para a terceira questão: Se há relaçao entre duraçao dos filmes e o seu ano de lançamento

#Tabela com a duração e ano.

duracao_ano <- MoviesOnStreamingPlatforms...Copia %>% 
  group_by(Year) %>%
  summarise(freq = Runtime)


#Gráfico de dispersão entre a duração e o ano.

ggplot(duracao_ano, aes(x=Year, y=freq)) + 
  geom_jitter(colour="#A11D21", size=3) +
  labs(x="Ano de Lançamento", y="Duração do Filme") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  ggsave("graf_duracao_ano.png", width = 158, height = 93, units = "mm")

#========================QUESTÃO 4==========================================


#Para a quarta questão: Se a nota do Rotten Tomatos é influenciada pelo tempo de duraçao do filme

#Tabela Simples

duracao_rotten <- MoviesOnStreamingPlatforms...Copia %>% 
  group_by(Runtime) %>%
  summarise(freq = Rotten.Tomatoes) %>%
  mutate(freqcerta = str_replace(freq,'%',''),
         freqcerta = as.numeric(freqcerta))


#Grafico de dispersao

ggplot(duracao_rotten, aes(x=freqcerta, y=Runtime)) + 
  geom_jitter(colour="#A11D21", size=3) +
  labs(x="Nota do Rotten Tomatos em Porcentagem (%)", y="Duração do FIlme") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  ggsave("graf_duracao_rotten.png", width = 158, height = 93, units = "mm")

#========================QUESTÃO 5==========================================


#Para a quinta questão: A distribuição de classificação indicativa do filme por plataforma

#Aqui o Código foi separado por plataforma.

#Em cada um dos casos foi criada um subset contendo somente os filmes que constam
#na respectiva plataforma. E dele foi gerado uma tabela da calssificação indicativa.

#E depois foi feito um gráfico de colunas com porcentagens.

#=======================NETFLIX

netflix <- subset(MoviesOnStreamingPlatforms...Copia, Netflix==1)
tabela <- netflix %>% 
  group_by(Age) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = round((freq/sum(freq))*100,2))

tabela$porcentagens <- str_c(tabela$freq, '(',tabela$freq_relativa, '%',')') %>%
  str_replace('\\.',',')

tabela$Age <- c('Sem Informação', '+13', '+16', '+18', '+7', 'Livre' )
tabela$ordem <- c(1, 4, 5, 6, 3,2)
arrange(tabela, ordem)
tabela <- arrange(tabela, ordem)

tabela %>% 
  ggplot(aes(x=Age, y=freq, label=porcentagens)) +
  geom_bar(stat="identity", fill="#A11D21") +
  geom_text(vjust = -0.5, size = 4) +
  labs(x="Classificação Indicativa (Netflix)", y="Frequência") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  ggsave("CORREC_NETFLIX.png", width = 158, height = 93, units = "mm")


#========================HULU

hulu <- subset(MoviesOnStreamingPlatforms...Copia, Hulu==1)
tabela <- hulu %>% 
  group_by(Age) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = round((freq/sum(freq))*100,2))

tabela$porcentagens <- str_c(tabela$freq, '(',tabela$freq_relativa, '%',')') %>%
  str_replace('\\.',',')

tabela$Age <- c('Sem Informação', '+13', '+16', '+18', '+7', 'Livre' )
tabela$ordem <- c(1, 4, 5, 6, 3,2)
arrange(tabela, ordem)
tabela <- arrange(tabela, ordem)

tabela %>% 
  ggplot(aes(x=Age, y=freq, label=porcentagens)) +
  geom_bar(stat="identity", fill="#A11D21") +
  geom_text(vjust = -0.5, size = 4) +
  labs(x="Classificação Indicativa (Hulu)", y="Frequência") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  ggsave("CORREC_HULU.png", width = 158, height = 93, units = "mm")



#==========================PRIME VIDEO

prime_video <- subset(MoviesOnStreamingPlatforms...Copia, Prime.Video==1)
tabela <- prime_video %>% 
  group_by(Age) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = round((freq/sum(freq))*100,2))

tabela$porcentagens <- str_c(tabela$freq, '(',tabela$freq_relativa, '%',')') %>%
  str_replace('\\.',',')

tabela$Age <- c('Sem Informação', '+13', '+16', '+18', '+7', 'Livre' )
tabela$ordem <- c(1, 4, 5, 6, 3,2)
arrange(tabela, ordem)
tabela <- arrange(tabela, ordem)

tabela %>% 
  ggplot(aes(x=Age, y=freq, label=porcentagens)) +
  geom_bar(stat="identity", fill="#A11D21") +
  geom_text(vjust = -0.5, size = 4) +
  labs(x="Classificação Indicativa (Prime Video)", y="Frequência") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  ggsave("CORREC_PRIMEVIDEO.png", width = 158, height = 93, units = "mm")

#=============================DISNEY

disney <- subset(MoviesOnStreamingPlatforms...Copia, Disney.==1)
tabela <- disney %>% 
  group_by(Age) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = round((freq/sum(freq))*100,2))

tabela$porcentagens <- str_c(tabela$freq, '(',tabela$freq_relativa, '%',')') %>%
  str_replace('\\.',',')

tabela$Age <- c('Sem Informação', '+13', '+16', '+18', '+7', 'Livre' )
tabela$ordem <- c(1, 4, 5, 6, 3,2)
arrange(tabela, ordem)
tabela <- arrange(tabela, ordem)

tabela %>% 
  ggplot(aes(x=Age, y=freq, label=porcentagens)) +
  geom_bar(stat="identity", fill="#A11D21") +
  geom_text(vjust = -0.5, size = 4) +
  labs(x="Classificação Indicativa (Disney)", y="Frequência") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  ggsave("CORREC_DISNEY2.png", width = 158, height = 93, units = "mm")


#========================QUESTÃO 6==========================================

#Para a sexta questão: Uma comparação entre o IMDb das principais plataformas, Netflix e Prime

# Aqui foram utilizados os subsets das plataformas indicadas feitos na questão anterior
# E foi confeccionado um boxplot das notas no IMDb.

# Posteriormente foram calculadas as respectivas medidas descritivas:
# (média, mediana, desvio-padrão, quartis)

# Foram observados também os dados de nota mínima e máxima em cada plataforma pelos subsets

#=======================NETFLIX
ggplot(netflix, aes(x=factor(""), y=IMDb)) +
  geom_boxplot(fill=c("#A11D21"), width = 0.5) +
  guides(fill=FALSE) +
  stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="Nota do IMDb (Netflix)")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black")) +
  ggsave("graf_netflix_imdb.png", width = 158, height = 93, units = "mm")

#=====================PRIME VIDEO

ggplot(prime_video, aes(x=factor(""), y=IMDb)) +
  geom_boxplot(fill=c("#A11D21"), width = 0.5) +
  guides(fill=FALSE) +
  stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="Nota do IMDb (Prime Video)")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black")) +
  ggsave("graf_prime_imdb.png", width = 158, height = 93, units = "mm")

#Calculando a média
media_prime <- mean(prime_video$IMDb, na.rm = TRUE)
media_netflix <- mean(netflix$IMDb, na.rm = TRUE)
media_dire <- mean(top5diretores$freq)
#Calculando a mediana

mediana_prime <- median(prime_video$IMDb, na.rm = TRUE)
mediana_netflix <- median(netflix$IMDb, na.rm = TRUE) 

#Calculando o desvio padrão

desvio_prime <- sd(prime_video$IMDb, na.rm = TRUE)
desvio_netflix <- sd(netflix$IMDb, na.rm = TRUE)

#Calculando quartis

Q1_prime <- quantile(prime_video$IMDb, probs = 0.25, na.rm = TRUE)
Q3_prime <- quantile(prime_video$IMDb, probs = 0.75, na.rm = TRUE)

Q1_netflix <- quantile(netflix$IMDb, probs = 0.25, na.rm = TRUE)
Q3_netflix <- quantile(netflix$IMDb, probs = 0.75, na.rm = TRUE)

#__________   PADRONIZAÇÃO GRÁFICOS   ____________#

# Obs: Foi utilizado R versão 3.5.2
# O encoding utilizado foi ISO8859-1, se der errado algum caractere, vai em "file"
# "reopen with Encoding"

#__ Carregando Pacotes ----

pacman::p_load(tidyverse, ggcorrplot, scales, stringr, cowplot, RColorBrewer)

#__ HISTOGRAMA ----

## Univariado ----

ggplot(mpg, aes(x=cty)) + 
  geom_histogram(colour="white", fill="#A11D21",binwidth=7)+
  labs(x="Consumo em Cidade (milhas/galão)", y="Frequência Absoluta") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) 

## Univariado com porcentagem no gráfico e no eixo ERRAAAAADOOOOOO ----

tabela <- mpg %>% group_by(cyl) %>% 
  summarise(freq = n()) %>% 
  mutate(freq_relativa = round((freq/sum(freq))*100,2))

porcentagens <- str_c(tabela$freq_relativa, '%') %>%
  str_replace('\\.',',')

tabela %>% 
  ggplot(aes(x=cyl, y=freq_relativa, label=porcentagens)) +
  geom_bar(stat="identity", fill="#A11D21") +
  geom_text(vjust=-0.5, size=4)+
  labs(x="Cilindros", y="Frequência Relativa") + 
  lims(y=c(0,100)) +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) 

## Univariado com porcentagem no gráfico e freq absoluta no eixo ----

tabela <- mpg %>% group_by(cyl) %>% 
  summarise(freq = n()) %>% 
  mutate(freq_relativa = round((freq/sum(freq))*100,2))

porcentagens <- str_c(tabela$freq_relativa, '%') %>%
  str_replace('\\.',',')

ggplot(tabela, aes(x=cyl, y=freq, label=porcentagens)) +
  geom_bar(stat="identity", fill="#A11D21") +
  geom_text(vjust=-0.5, size=4)+
  labs(x="Cilindros", y="Frequência Absoluta") + lims(y=c(0,90))+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) 

## Com facet grid ----

ggplot(mpg, aes(x=cty)) + 
  geom_histogram(colour="white", fill="#A11D21", binwidth=7)+
  facet_grid(~class) +
  labs(x="Consumo em Cidade (milhas/galão)", y="Frequência") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"),
        strip.text = element_text(size=12),
        strip.background = element_rect(colour="black", fill="white"))

## Com facet wrap ----

ggplot(mpg, aes(x=cty)) + 
  geom_histogram(colour="white", fill="#A11D21", binwidth=7) +
  facet_wrap(~class) +
  labs(x="Consumo em Cidade (milhas/galão)", y="Frequência") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"),
        strip.text = element_text(size=12),
        strip.background = element_rect(colour="black", fill="white"))

#__ BARRAS ----

## Univariado ----

ggplot(mpg, aes(x = class)) + 
  geom_bar(fill="#A11D21") +
  labs(x="Classe do automóvel", y="Frequência") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  coord_flip()

## Univariado com porcentagem no gráfico e freq relativa no eixo ----

tabela <- mpg %>% 
  group_by(class) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = round((freq/sum(freq))*100,2))

porcentagens <- str_c(tabela$freq_relativa, '%') %>%
  str_replace('\\.',',')

tabela %>% 
  ggplot(aes(x=class, y=freq_relativa, label=porcentagens)) +
  geom_bar(stat="identity", fill="#A11D21") +
  geom_text(vjust=0.5, size=4, hjust=-0.1) +
  labs(x="Classe do automóvel", y="Frequência") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  coord_flip()


## Univariado com porcentagem no gráfico e freq absoluta no eixo ----

tabela <- mpg %>% 
  group_by(class) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = round((freq/sum(freq))*100,2))

porcentagens <- str_c(tabela$freq_relativa, '%') %>%
  str_replace('\\.',',')

tabela %>% 
  ggplot(aes(x=class, y=freq, label=porcentagens)) +
  geom_bar(stat="identity", fill="#A11D21") +
  geom_text(vjust=0.5, size=4, hjust=-0.1) +
  labs(x="Classe do automóvel", y="Frequência") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  coord_flip()

#__ COLUNAS ----

## Univariado ----

ggplot(mpg, aes(x = class)) + 
  geom_bar(fill="#A11D21") +
  labs(x="Classe do automóvel", y="Frequência") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) 

## Univariado com porcentagem no gráfico e freq relativa no eixo ----

tabela <- mpg %>% 
  group_by(class) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = round((freq/sum(freq))*100,2))

porcentagens <- str_c(tabela$freq_relativa, '%') %>%
  str_replace('\\.',',')

tabela %>% 
  ggplot(aes(x=class, y=freq_relativa, label=porcentagens)) +
  geom_bar(stat="identity", fill="#A11D21") +
  geom_text(vjust = -0.5, size = 4) +
  labs(x="Classe do automóvel", y="Frequência") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))


## Univariado com porcentagem no gráfico e freq absoluta no eixo ----

tabela <- mpg %>% 
  group_by(class) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = round((freq/sum(freq))*100,2))

porcentagens <- str_c(tabela$freq_relativa, '%') %>%
  str_replace('\\.',',')

tabela %>% 
  ggplot(aes(x=class, y=freq, label=porcentagens)) +
  geom_bar(stat="identity", fill="#A11D21") +
  geom_text(vjust = -0.5, size = 4) +
  labs(x="Classe do automóvel", y="Frequência") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))

## Bivariado com dodge ----

tabela <- mpg %>%
  group_by(class,drv) %>% 
  summarise(freq = n()) %>%
  mutate(freq_relativa = round((freq/sum(freq)),2)) 

ggplot(tabela, aes(x=class, y=freq_relativa, fill=drv)) + 
  geom_bar(stat="identity",position='dodge') +
  scale_fill_manual(name="Transmissão", values=c("#A11D21", "#003366",'#302566')) +
  labs(x="Classe do automóvel", y="Frequência Relativa") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(legend.position="top")

## Bivariado com stack ----

tabela <- mpg %>% 
  filter(drv %in% c('f','4')) %>% # filtrando para ter duas observações, se tiver mais precisa de mais cores em values no scale_fill_manual
  group_by(class,drv) %>% 
  summarise(freq = n()) %>%
  mutate(freq_relativa = round((freq/sum(freq)),2)) 

ggplot(tabela, aes(x=class, y=freq_relativa, fill=drv)) + 
  geom_bar(stat="identity",position='stack') +
  scale_fill_manual(name="Transmissão", values=c("#A11D21", "#003366")) +
  labs(x="Classe do automóvel", y="Frequência Relativa") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(legend.position="top")

## Bivariado com fill ----

tabela <- mpg %>% 
  filter(drv %in% c('f','4')) %>% # filtrando para ter duas observações, se tiver mais precisa de mais cores em values no scale_fill_manual
  group_by(class,drv) %>% 
  summarise(freq = n()) %>%
  mutate(freq_relativa = round((freq/sum(freq)),2)) 

ggplot(tabela, aes(x=class, y=freq_relativa, fill=drv)) + 
  geom_bar(stat="identity",position='fill') +
  scale_fill_manual(name="Transmissão", values=c("#A11D21", "#003366")) +
  labs(x="Classe do automóvel", y="Frequência Relativa") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(legend.position="top")

## Bivariado com porcentagem ----

tabela <- mpg %>% 
  filter(drv %in% c('f','4')) %>% # filtrando para ter duas observações, se tiver mais precisa de mais cores em values no scale_fill_manual
  group_by(class,drv) %>% 
  summarise(freq = n()) %>%
  mutate(freq_relativa = round((freq/sum(freq)),2)) 

ggplot(tabela, aes(x=class, y=freq_relativa, fill=drv)) + 
  geom_bar(stat="identity",position='dodge') +
  scale_fill_manual(name="Sexo", values=c("#A11D21", "#003366")) + 
  scale_y_continuous(limits = c(0,1), # de 0 até 1
                     expand = c(0,0), 
                     breaks = seq(0,1,.1), # intervalos de 0.1
                     labels = str_c(seq(0,100,10),'%')) + # o eixo com intervalo de 0 a 100 e %
  labs(x="Faixa etária", y="Frequência Relativa") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(legend.position="top")

#__ BOXPLOT ----

## Univariado ----

ggplot(mpg, aes(x=factor(""), y=cty)) +
  geom_boxplot(fill=c("#A11D21"), width = 0.5) +
  guides(fill=FALSE) +
  stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="Consumo em Cidade (milhas/galão)")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))

## Bivariado ----

ggplot(mpg, aes(x=trans, y=cty)) +
  geom_boxplot(fill=c("#A11D21"), width = 0.5) +
  stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="Transmissão", y="Consumo em Cidade (milhas/galão)") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black")) 

#__ SETORES ----

## Com porcentagem ----

contagem <- mpg %>% 
  group_by(drv) %>%
  summarise(Freq = n()) %>%
  mutate(Prop = round(100*(Freq/sum(Freq)), 2)) %>%
  arrange(desc(drv)) %>%
  mutate(posicao = cumsum(Prop) - 0.5*Prop)

ggplot(contagem, aes(x = factor(""), y = Prop , fill = factor(drv))) +
  geom_bar(width = 1, stat = "identity")+
  coord_polar(theta = "y")+
  scale_x_discrete()+
  scale_fill_manual(name = "DRV", values = c("#A11D21", "#003366", "#FF6600")) + 
  theme_bw()+
  theme(axis.title = element_blank(), 
        axis.text = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank()) +
  theme(legend.position="top") +
  geom_text(aes(x = 1.8, y = posicao, label = paste0(Prop, "%")), color = "black")

#___ DISPERSÂO ----

## Univariado com poucos pontos sobrepostos ----

ggplot(mpg, aes(x=cty, y=hwy)) + 
  geom_point(colour="#A11D21", size=3) +
  labs(x="Consumo em Cidade (milhas/galão)", y="Consumo em Rodovias (milhas/galão)") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))

## Univariado com muitos pontos sobrepostos ----

### geom_jitter ----

ggplot(mpg, aes(x=cyl, y=cty)) + 
  geom_jitter(colour="#A11D21", size=3) +
  labs(x="Consumo em Cidade (milhas/galão)", y="Consumo em Rodovias (milhas/galão)") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))

### Alpha ----

ggplot(mpg, aes(x=cyl, y=cty)) + 
  geom_point(colour="#A11D21", size=3, alpha = 0.3) +
  labs(x="Consumo em Cidade (milhas/galão)", y="Consumo em Rodovias (milhas/galão)") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))

## Bivariado ----

mpg$trans <- factor(substr(mpg$trans, 1, nchar(mpg$trans)-4))

ggplot(mpg, aes(x=cty, y=hwy)) + 
  geom_point(aes(colour=trans)) +
  scale_colour_manual(name="Transmissão", values = c("#A11D21", "#003366"), 
                      labels = c("Automático", "Manual"))+
  labs(x="Consumo em Cidade (milhas/galão)", y="Consumo em Rodovias (milhas/galão)")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(legend.position="top")

#__ LINHAS ----

## Univariado ----

ano <- c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2006","2007","2008",
         "2009","2010","2011","2012","2013","2014","2015")
preco <- c(2.5,5.7,3.4,3.7,4.5,4.8,4.1,4.6,4.8,5, 4.5,6.5,3.5,4.6,4.7,4.9,5,5.5,3.5,7.5)
produto <- c("a","a","a","a","a","a","a","a","b","b","b","b","b","b","b","b","b","b","b","b")
dados <- data.frame(ano,preco,produto)

ggplot(dados, aes(x=ano, y=preco, group=1)) +
  geom_line(size=1,colour="#A11D21") + geom_point(colour="#A11D21",size=2) +
  labs(x="Ano", y="Preço") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))

## Bivariado ----

ano <- c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2006","2007","2008",
         "2009","2010","2011","2012","2013","2014","2015")
preco <- c(2.5,5.7,3.4,3.7,4.5,4.8,4.1,4.6,4.8,5, 4.5,6.5,3.5,4.6,4.7,4.9,5,5.5,3.5,7.5)
produto <- c("a","a","a","a","a","a","a","a","b","b","b","b","b","b","b","b","b","b","b","b")
dados <- data.frame(ano,preco,produto)

ggplot(dados, aes(x=ano,y=preco,group=produto,colour=produto)) +
  geom_line(size=1) + geom_point(size=2) +
  scale_colour_manual(name="Produto", values = c("#A11D21", "#003366"), labels = c("A", "B"))+
  labs(x="Ano", y="Preço") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(legend.position="top")

#__ QQPLOT ----

ggplot(mpg, aes(sample = cty)) + 
  stat_qq(colour = "#A11D21") + stat_qq_line(size = 0.8) + 
  labs(x = "Quantis da Normal",y = "Consumo em Cidade (milhas/galÃ£o)") +
  theme_bw()+
  theme(axis.title.y = element_text(colour = "black", size = 12),
        axis.title.x = element_text(colour = "black", size = 12),
        axis.text = element_text(colour = "black", size = 9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))

#__ PAINEL (COWPLOT) ----

a <- ggplot(mpg, aes(x=cty, y=hwy)) + geom_point(colour="#A11D21", size=2) +
  labs(x="Consumo em Cidade (milhas/galão)", y="Consumo em Rodovias (milhas/galão)") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=9),
        axis.title.x = element_text(colour="black", size=9),
        axis.text = element_text(colour = "black", size=8),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))

b <- ggplot(mpg, aes(x=cty, y=hwy)) + geom_point(aes(colour=trans)) +
  scale_colour_manual(name="Transmissão", values = c("#A11D21", "#003366"), 
                      labels = c("Automático", "Manual"))+
  labs(x="Consumo em Cidade (milhas/galão)", y="Consumo em Rodovias (milhas/galão)")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=9),
        axis.title.x = element_text(colour="black", size=9),
        axis.text = element_text(colour = "black", size=8),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(legend.position="top")

c <- ggplot(dados, aes(x=ano, y=preco, group=1)) +
  geom_line(size=0.8,colour="#A11D21") + geom_point(colour="#A11D21",size=1.8) +
  labs(x="Ano", y="Preço") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=9),
        axis.title.x = element_text(colour="black", size=9),
        axis.text = element_text(colour = "black", size=8),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))

d <- ggplot(dados, aes(x=ano,y=preco,group=produto,colour=produto)) +
  geom_line(size=0.8) + geom_point(size=1.8) +
  scale_colour_manual(name="Produto", values = c("#A11D21", "#003366"), labels = c("A", "B"))+
  labs(x="Ano", y="Preço") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=9),
        axis.title.x = element_text(colour="black", size=9),
        axis.text = element_text(colour = "black", size=8),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(legend.position="top")

plot_grid(a, b, c, d, nrow = 2)

#__ CORRELAÇÃO ----

corr <- round(cor(mtcars,method="pearson"),1)
ggcorrplot(corr,hc.order=T,
           ggtheme=ggplot2::theme_bw,
           sig.level=0.05,
           legend.title="",
           type="lower",
           lab=T,
           lab_size=3,
           method="circle",
           colors=c("#A11D21","white","#003366"),
           title="Correlações") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black")) +
  xlab("") + ylab("") 

#__ P-VAOR ----

pvalores <- lapply(mtcars, function(x) lapply(mtcars, function(y) wilcox.test(x, y)$p.value))
pvalores2 <-  data.frame(split(unlist(pvalores), rep(1:11, 11)))
dimnames(pvalores2) <- list(names(mtcars), names(mtcars))
pvalores3 <- as.matrix(pvalores2)

ggcorrplot(pvalores3,hc.order=T,
           sig.level=0.05,
           ggtheme=ggplot2::theme_bw,
           legend.title="",
           type="lower",
           lab=T,
           lab_size=3.2,
           method="circle",
           title="P-valores") +
  scale_fill_gradient(low="#E86F73",high="white",limit = c(0,1),name="")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black")) +
  xlab("") + ylab("") 

#__ CORRELAÇÃO COM P-VALOR ----

k = 1
var1 <- numeric(0)
var2 <- numeric(0)
pvalor <- numeric(0)
estimativa <- numeric(0)
for(i in 1:5){
  for(j in 6:11){
    pvalor[k] <- cor.test(mtcars[[i]],mtcars[[j]],method="kendall")$p.value
    var1[k] <- names(mtcars)[i]
    var2[k] <- names(mtcars)[j]
    estimativa[k] <- cor.test(mtcars[[i]],mtcars[[j]],method="kendall")$estimate
    k <- k+1
  }
}

banco <- data.frame(var1,var2,pvalor,estimativa)
banco$estimativa <- as.numeric(as.character(banco$estimativa))
banco$pvalor <- as.numeric(as.character(banco$pvalor))

ggplot(banco,aes(x=var2,y=var1))+
  geom_tile(aes(fill=estimativa))+
  geom_text(aes(label=round(pvalor,3),x=var2,y=var1),color="#000000")+
  scale_fill_gradientn(colours = brewer.pal(n=11,name = "RdBu"),name="Coeficiente de \n Correlação")+
  labs(x="Grupo 1 de Variáveis",y="Grupo 2 de Variáveis")+
  theme_bw()+
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_blank()) 

#__ EXPORTAÇÃO ----

ggsave("graf.png", width = 158, height = 93, units = "mm")
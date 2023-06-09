#############################RAFAEL SANTANA ARARUNA#############################

# QUEST�O 1

## Tabela com os dados da quest�o:
base_ronco <- data.frame("Freq. de Ronco" = c('Nunca', 'Ocasionalmente', 
                                              'Quase toda noite','Toda noite'), 
                         "Com Doen�a" = c(24,35,21,30), 
                         "Sem Doen�a" = c(1355,603,192,224))
colnames(base_ronco) <- c("Freq. de Ronco", "Com Doen�a", "Sem Doen�a")
knitr::kable(base_ronco,align = 'c')

## a)

### Fiz uma nova tabela com os scores da quest�o:
base2_ronco <-  data.frame("Freq. de Ronco" = c(5,10,15,20), 
                           "Com Doen�a" = c(24,35,21,30), 
                           "Sem Doen�a" = c(1355,603,192,224))
colnames(base2_ronco) <- c("Freq. de Ronco", "Com Doen�a", "Sem Doen�a")
knitr::kable(base2_ronco,align = 'c')

### Construi o modelo:
mod_ronco <- glm(base2_ronco$`Com Doen�a`/(base2_ronco$`Com Doen�a`+ base2_ronco$`Sem Doen�a`) ~ 
                   base2_ronco$`Freq. de Ronco`, weights = base2_ronco$`Com Doen�a` +
                   base2_ronco$`Sem Doen�a`, family = binomial)
summary(mod_ronco)

## b) N�o teve c�digo, usei os resultados do item anterior.

## c)

### Calculo das probabilidades estimadas pelo modelo:
pi_est <- function(alfa,beta,x){ exp(alfa + beta*x) / (exp(alfa + beta*x) + 1)}
pi_5 <-  pi_est(alfa=-4.43191,beta=0.13091,x=5)
pi_10 <-  pi_est(alfa=-4.43191,beta=0.13091,x=10)
pi_15 <-  pi_est(alfa=-4.43191,beta=0.13091,x=15)
pi_20 <-  pi_est(alfa=-4.43191,beta=0.13091,x=20)

df <- data.frame("0" = pi_5, "2" = pi_10, "4" = pi_15, "5" = pi_20)
colnames(df) <- c("pi_5", "pi_10", "pi_15", "pi_20")
knitr::kable(df,align = 'c')

## d) N�o teve c�digo, usei os resultados do item (a).

## e)

### intervalo de confian�a para 95%
confint(mod_ronco,level = 0.90)

### c�digo pra gerar a tabela com os resultados dos intervalos:
tabela <- data.frame("x" = c("Intercepto", "Beta - Freq. de Ronco"),
                     "y" = c(-4.8138759,0.1037494),
                     "w" = c(-4.0709355, 0.1581274))
colnames(tabela) <- c(" ", "Limite Inferior", "Limite Superior")
knitr::kable(tabela, align = 'c', caption = "Intervalo de Confian�a")

## f) N�o teve c�digo, usei os resultados do item (a).

## g) 

### construi o modelo com a vari�vel "ronco" ainda como quantitativa e alterando a codifica��o:
base3_ronco <-  data.frame("Freq. de Ronco" = c(2,4,6,8),
                           "Com Doen�a" = c(24,35,21,30),
                           "Sem Doen�a" = c(1355,603,192,224))
colnames(base3_ronco) <- c("Freq. de Ronco", "Com Doen�a", "Sem Doen�a")

mod2_ronco <- glm(base3_ronco$`Com Doen�a`/(base3_ronco$`Com Doen�a`+ base3_ronco$`Sem Doen�a`) ~
                    base3_ronco$`Freq. de Ronco`, weights = base3_ronco$`Com Doen�a` + 
                    base3_ronco$`Sem Doen�a`, family = binomial)
summary(mod2_ronco)

### construi o modelo com a vari�vel "ronco" como categ�rica e sem alterar a codifica��o:
base4_ronco <-  data.frame("Freq. de Ronco" = c(5,10,15,20),
                           "Com Doen�a" = c(24,35,21,30),
                           "Sem Doen�a" = c(1355,603,192,224))
colnames(base4_ronco) <- c("Freq. de Ronco", "Com Doen�a", "Sem Doen�a")

mod3_ronco <- glm(base4_ronco$`Com Doen�a`/(base4_ronco$`Com Doen�a`+ base4_ronco$`Sem Doen�a`) ~
                    factor(base4_ronco$`Freq. de Ronco`), weights = base4_ronco$`Com Doen�a` +
                    base4_ronco$`Sem Doen�a`, family = binomial)
summary(mod3_ronco)

### construi o modelo com a vari�vel "ronco" como categ�rica e alterando a codifica��o: 
base5_ronco = data.frame("Freq. de Ronco" = c(2,4,6,8),
                         "Com Doen�a" = c(24,35,21,30),
                         "Sem Doen�a" = c(1355,603,192,224))
colnames(base5_ronco) <- c("Freq. de Ronco", "Com Doen�a", "Sem Doen�a")

mod4_ronco <- glm(base5_ronco$`Com Doen�a`/(base5_ronco$`Com Doen�a`+ base5_ronco$`Sem Doen�a`) ~
                    factor(base5_ronco$`Freq. de Ronco`), weights = base5_ronco$`Com Doen�a` +
                    base5_ronco$`Sem Doen�a`, family = binomial)
summary(mod4_ronco)

## h)


#############################RAFAEL SANTANA ARARUNA - 180026798#############################

# QUEST�O 2

## Tabela com os dados da quest�o:
df <- data.frame("y" = c(rep(0,18),rep(1,22)),
                 "x" = c(12, 15, 42, 52, 59, 73, 82, 91, 96, 105, 114, 120, 121, 128,
                         130, 139, 139, 140,1, 1, 2, 8, 11, 18, 22, 31, 37, 61, 72, 
                         81, 97, 112, 118, 127, 131, 150, 151, 159, 200, 206))

## a)

### Construi o modelo:
mod <- glm(df$y ~ df$x, family=binomial)
summary(mod)

## b)

### c�digo do gr�fico:
library(ggplot2)
ggplot(df, aes(x=x,y=as.factor(y), colour = factor(df$y))) +  
  geom_point() +
  labs(x = "Idade", y = "Fun��o Logito") +
  scale_colour_manual("Legenda", values = c("red","blue"),labels = c("Sem Cifose", "Com Cifose")) +
  theme_bw()

## c)

### teste de ANOVA:
library(car)
Anova(mod)

### c�digo pra gerar a tabela com os resultados do teste da ANOVA:
tabela <- data.frame("x" = c("Idade"),
                     "y" = c(0.34802),
                     "z" = c(1),
                     "w" = c(0.5552))
colnames(tabela) <- c("Vari�vel", "Estat�stica", "GL", "P-valor")
knitr::kable(tabela, align = 'c', caption = "Teste de ANOVA")

## d)

### intervalo de confian�a para 95%
confint(mod, level = 0.95)

### c�digo pra gerar a tabela com os resultados dos intervalos:
tabela <- data.frame("x" = c("Intercepto", "Beta - Idade"),
                     "y" = c(-0.6553,-0.0150),
                     "w" = c(1.7159, 0.0078))
colnames(tabela) <- c(" ", "Limite Inferior", "Limite Superior")
knitr::kable(tabela, align = 'c', caption = "Intervalo de Confian�a")

## e)

### curva de ROC:
library(pROC)
curva_roc <- roc(df$y~fitted(mod))
plot.roc(curva_roc,legacy.axes = T, xlab = "1-Especificidade", ylab = "Sensibilidade")

### �rea abaixo da curva de ROC:
auc(curva_roc)
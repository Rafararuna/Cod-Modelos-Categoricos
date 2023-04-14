#############################RAFAEL SANTANA ARARUNA#############################

# QUESTÃO 1

## Tabela com os dados da questão:
base_ronco <- data.frame("Freq. de Ronco" = c('Nunca', 'Ocasionalmente', 
                                              'Quase toda noite','Toda noite'), 
                         "Com Doença" = c(24,35,21,30), 
                         "Sem Doença" = c(1355,603,192,224))
colnames(base_ronco) <- c("Freq. de Ronco", "Com Doença", "Sem Doença")
knitr::kable(base_ronco,align = 'c')

## a)

### Fiz uma nova tabela com os scores da questão:
base2_ronco <-  data.frame("Freq. de Ronco" = c(5,10,15,20), 
                           "Com Doença" = c(24,35,21,30), 
                           "Sem Doença" = c(1355,603,192,224))
colnames(base2_ronco) <- c("Freq. de Ronco", "Com Doença", "Sem Doença")
knitr::kable(base2_ronco,align = 'c')

### Construi o modelo:
mod_ronco <- glm(base2_ronco$`Com Doença`/(base2_ronco$`Com Doença`+ base2_ronco$`Sem Doença`) ~ 
                   base2_ronco$`Freq. de Ronco`, weights = base2_ronco$`Com Doença` +
                   base2_ronco$`Sem Doença`, family = binomial)
summary(mod_ronco)

## b) Não teve código, usei os resultados do item anterior.

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

## d) Não teve código, usei os resultados do item (a).

## e)

### intervalo de confiança para 95%
confint(mod_ronco,level = 0.90)

### código pra gerar a tabela com os resultados dos intervalos:
tabela <- data.frame("x" = c("Intercepto", "Beta - Freq. de Ronco"),
                     "y" = c(-4.8138759,0.1037494),
                     "w" = c(-4.0709355, 0.1581274))
colnames(tabela) <- c(" ", "Limite Inferior", "Limite Superior")
knitr::kable(tabela, align = 'c', caption = "Intervalo de Confiança")

## f) Não teve código, usei os resultados do item (a).

## g) 

### construi o modelo com a variável "ronco" ainda como quantitativa e alterando a codificação:
base3_ronco <-  data.frame("Freq. de Ronco" = c(2,4,6,8),
                           "Com Doença" = c(24,35,21,30),
                           "Sem Doença" = c(1355,603,192,224))
colnames(base3_ronco) <- c("Freq. de Ronco", "Com Doença", "Sem Doença")

mod2_ronco <- glm(base3_ronco$`Com Doença`/(base3_ronco$`Com Doença`+ base3_ronco$`Sem Doença`) ~
                    base3_ronco$`Freq. de Ronco`, weights = base3_ronco$`Com Doença` + 
                    base3_ronco$`Sem Doença`, family = binomial)
summary(mod2_ronco)

### construi o modelo com a variável "ronco" como categórica e sem alterar a codificação:
base4_ronco <-  data.frame("Freq. de Ronco" = c(5,10,15,20),
                           "Com Doença" = c(24,35,21,30),
                           "Sem Doença" = c(1355,603,192,224))
colnames(base4_ronco) <- c("Freq. de Ronco", "Com Doença", "Sem Doença")

mod3_ronco <- glm(base4_ronco$`Com Doença`/(base4_ronco$`Com Doença`+ base4_ronco$`Sem Doença`) ~
                    factor(base4_ronco$`Freq. de Ronco`), weights = base4_ronco$`Com Doença` +
                    base4_ronco$`Sem Doença`, family = binomial)
summary(mod3_ronco)

### construi o modelo com a variável "ronco" como categórica e alterando a codificação: 
base5_ronco = data.frame("Freq. de Ronco" = c(2,4,6,8),
                         "Com Doença" = c(24,35,21,30),
                         "Sem Doença" = c(1355,603,192,224))
colnames(base5_ronco) <- c("Freq. de Ronco", "Com Doença", "Sem Doença")

mod4_ronco <- glm(base5_ronco$`Com Doença`/(base5_ronco$`Com Doença`+ base5_ronco$`Sem Doença`) ~
                    factor(base5_ronco$`Freq. de Ronco`), weights = base5_ronco$`Com Doença` +
                    base5_ronco$`Sem Doença`, family = binomial)
summary(mod4_ronco)

## h)


#############################RAFAEL SANTANA ARARUNA - 180026798#############################

# QUESTÃO 2

## Tabela com os dados da questão:
df <- data.frame("y" = c(rep(0,18),rep(1,22)),
                 "x" = c(12, 15, 42, 52, 59, 73, 82, 91, 96, 105, 114, 120, 121, 128,
                         130, 139, 139, 140,1, 1, 2, 8, 11, 18, 22, 31, 37, 61, 72, 
                         81, 97, 112, 118, 127, 131, 150, 151, 159, 200, 206))

## a)

### Construi o modelo:
mod <- glm(df$y ~ df$x, family=binomial)
summary(mod)

## b)

### código do gráfico:
library(ggplot2)
ggplot(df, aes(x=x,y=as.factor(y), colour = factor(df$y))) +  
  geom_point() +
  labs(x = "Idade", y = "Função Logito") +
  scale_colour_manual("Legenda", values = c("red","blue"),labels = c("Sem Cifose", "Com Cifose")) +
  theme_bw()

## c)

### teste de ANOVA:
library(car)
Anova(mod)

### código pra gerar a tabela com os resultados do teste da ANOVA:
tabela <- data.frame("x" = c("Idade"),
                     "y" = c(0.34802),
                     "z" = c(1),
                     "w" = c(0.5552))
colnames(tabela) <- c("Variável", "Estatística", "GL", "P-valor")
knitr::kable(tabela, align = 'c', caption = "Teste de ANOVA")

## d)

### intervalo de confiança para 95%
confint(mod, level = 0.95)

### código pra gerar a tabela com os resultados dos intervalos:
tabela <- data.frame("x" = c("Intercepto", "Beta - Idade"),
                     "y" = c(-0.6553,-0.0150),
                     "w" = c(1.7159, 0.0078))
colnames(tabela) <- c(" ", "Limite Inferior", "Limite Superior")
knitr::kable(tabela, align = 'c', caption = "Intervalo de Confiança")

## e)

### curva de ROC:
library(pROC)
curva_roc <- roc(df$y~fitted(mod))
plot.roc(curva_roc,legacy.axes = T, xlab = "1-Especificidade", ylab = "Sensibilidade")

### área abaixo da curva de ROC:
auc(curva_roc)
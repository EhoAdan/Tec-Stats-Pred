# Questão 1

set.seed(25032004) # COLOQUE O SEU DIA MÊS E ANO DE NASCIMENTO NA SET.SEED
base = read.csv2("base.csv")
base1 = base[sample(nrow(base), 800),]
base1$Local = as.factor(base1$Local)
# Questão 2

# Modelo de Regressão Múltipla
modelo = lm(Valor ~ Area + Idade + Energia + Local, data=base1)
summary(modelo)

# Questão 3

# Analise Grafica da relacao das variaveis independentes com a variavel dependente
library(car)
anova(modelo)

# Questão 4

summary(modelo)

# Questão 5

# Coeficiente de Determinação Ajustado
modelo = lm(Valor ~ Area + Idade + Energia + Local, data=base1)
summary(modelo)

# Questão 6

# Importancia de cada variavel no modelo
library(relaimpo)
imp = calc.relimp(modelo)
var.exp = data.frame(round(imp$lmg*100,1))
colnames(var.exp) = "imp.lmg"
nome = rownames(var.exp)
var.exp = data.frame(nome,var.exp)
library(ggplot2)
ggplot(var.exp,aes(nome,imp.lmg)) +
  geom_bar(stat = "identity")+
  geom_text(aes(label = imp.lmg), vjust = 1.5, colour = "white")

# Questão 7
plot(fitted(modelo), rstandard(modelo))
abline(0,0)
par(mfrow = c(2,2))
plot(modelo)

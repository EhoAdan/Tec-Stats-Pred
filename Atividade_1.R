# Amostra
set.seed(25032004) # COLOQUE O SEU DIA MÊS E ANO DE NASCIMENTO NA SET.SEED
base = read.csv2("Atividade1.csv")
base1 = base[sample(nrow(base), 800),]

# Questão 2 - Gráfico de Dispersão

base1 = base1[,-2]
base1 = base1[,-5]
plot(base1)

# Questão 3 - Coeficiente de Correlação

# Coeficiente de correlacao
cor(base1)
correlacao <- cor(base1)

# Grafico da Matriz do correlacao
library(ggcorrplot)
ggcorrplot(correlacao)
ggcorrplot(correlacao, hc.order = TRUE, type = "lower",
           lab = TRUE)

library(corrplot)
corrplot(cor(base1), method = 'number')

# Questão 4 - Intervalo de Confiança

# Intervalo de Confianca
cor.test(base1$preco, base1$potencia, conf.level=0.95)$conf.int

# Teste de Hipoteses
cor.test(base1$preco, base1$potencia, alternative='two.sided')

# Questão 5 - Modelo de Regressão

# Coeficientes do modelo
modelo = lm(preco ~ potencia, data=base1)
coef(modelo)

# Coeficiente de determinacao
summary(modelo)$r.squared

# Reta de regressao no modelo
abline(modelo)
novo = data.frame("potencia"=700)
predict(modelo,novo)

# Análise de Resíduos

plot(fitted(modelo),rstandard(modelo))
abline(0,0)

# Teste de Significância do Modelo

summary(modelo)

# Questão 6 - Gráfico de Dispersão

base1 = base[sample(nrow(base), 800),]
base1 = base1[,-5]
base1 = base1[,-3]
base1 = base1[,-2]

summary(modelo)

ggplot(base1, aes(x = preco, y = potencia, color = transm)) +
  geom_point(size = 4) +  # Tamanho dos pontos
  labs(title = "Preço X Potência: Destaca-se Transmissão",
       x = "Preço",
       y = "Potência",
       color = "Transmissão", 
       shape = "Transmissão") +
  theme_minimal() +  # Estilo minimalista
  theme(legend.position = "top")  # Posição da legenda
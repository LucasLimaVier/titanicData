#Bibliotecas
library(readr)
library(janitor)
library(tidyr)
library(tidyverse)

#Leitura do dataframe
titanic = read.csv("D:/Lucas/Doutorado UFC/Doutorado/Disciplinas/Modelos experimentais de investigação/Relatorio/datasets/train.csv")
View(titanic)

#Tratando dados que contem valores NaN

titanicTratado = na.omit(titanic)
View(titanicTratado)

#Obtendo dados descritivos isolados
##Quantidade de sobreviventes e percentuais

titanicTratado %>% dplyr::select(Survived) %>%
  tabyl(Survived) %>%
  adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

ggplot(data = titanicTratado, aes(x = factor(Survived))) +
  geom_bar(aes(fill = factor(Survived))) +
  labs(title = "Frequência de Sobreviventes e Mortos",
       x = "Sobrevivente (1) / Morto (0)",
       y = "Frequência") +
  scale_fill_manual(values = c("1" = "blue", "0" = "red"))

##Quantidade de classes e percentuais

titanicTratado %>% dplyr::select(Pclass) %>%
  tabyl(Pclass) %>%
  adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

ggplot(data = titanicTratado, aes(x = factor(Pclass))) +
  geom_bar(aes(fill = factor(Pclass))) +
  labs(title = "Frequência entre classes",
       x = "1a classe (1) / 2a classe (2), 3a classe (3) ",
       y = "Frequência") +
  scale_fill_manual(values = c("1" = "green", "2" = "blue", "3" = "red"))

##Quantidade por sexo e percentuais

titanicTratado %>% dplyr::select(Sex) %>%
  tabyl(Sex) %>%
  adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

ggplot(data = titanicTratado, aes(x = factor(Sex))) +
  geom_bar(aes(fill = factor(Sex))) +
  labs(title = "Frequência do sexo dos passageiros",
       y = "Frequência") +
  scale_fill_manual(values = c("male" = "blue", "female" = "red"))

##Média de idade
###Geral
mean(titanicTratado$Age)
###Adultos
titanicTratado <- titanicTratado %>% mutate(as.integer(titanicTratado$Age))
idadeAdultos = mean(subset(titanicTratado$`as.integer(titanicTratado$Age)`,as.integer(titanicTratado$Age)>=18))
idadeAdultos
##OBS: Tive que mudar o nome da coluna pois ficou impraticavel as.integerblablabla
titanicTratado = titanicTratado %>% rename(AgeInt = `as.integer(titanicTratado$Age)`)
###Crianças
idadeCriancas = mean(subset(titanicTratado$AgeInt, titanicTratado$AgeInt<18))
idadeCriancas

mediaIdades = data.frame(Grupo = c("Maior de 18", "Menor de 18"),
                           Media_Idade = c(idadeAdultos, idadeCriancas))

view(mediaIdades)

ggplot(mediaIdades, aes(x = Grupo, y = Media_Idade, fill = Grupo)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Média de Idade por Grupo",
       x = "Grupo",
       y = "Média de Idade (anos)") +
  scale_fill_manual(values = c("blue", "lightblue"))



##Média de irmãos por pessoas
mean(titanicTratado$SibSp)

##Média de irmãos por pessoas
mean(titanicTratado$Parch)

##Média do preço da passagem
mean(titanicTratado$Fare)
summary(titanicTratado$Fare)

ggplot(titanicTratado, aes(y = Fare)) +
  geom_boxplot() +
  labs(title = "Sumário do preço das passagens",
       y = "Preço (dólares)") +
  theme_minimal()

##Quantidade Lugares de embarque e percentuais

titanicTratado %>% dplyr::select(Embarked) %>%
  tabyl(Embarked) %>%
  adorn_totals() %>%
  adorn_pct_formatting(digits = 2)

ggplot(data = titanicTratado, aes(x = factor(Embarked))) +
  geom_bar(aes(fill = factor(Embarked))) +
  labs(title = "Embarque",
       x = "Cherbourg (C) / Queenstown (Q), Southampton (S) ",
       y = "Frequência") +
  scale_fill_manual(values = c("C" = "#660057", "Q" = "#c600a9", "S" = "#ff70ea"))

#Correlação preço vs idade

regressao = lm(titanicTratado$AgeInt ~ titanicTratado$Fare, data = titanicTratado)
summary(regressao)
plot(titanicTratado$AgeInt, titanicTratado$Fare,
     main = "Correlação Idade X Preço da passagem",
     xlab = "Idade (anos)",
     ylab = "Preço (dolares)")
abline(regressao, col="blue")

#Obtendo dados em correlacionados em tabelas de contigência

##Sobreviventes por sexo
sobreviventes = table(titanicTratado$Sex, titanicTratado$Survived)
sobreviventes
sobreviventesPercentual = prop.table(sobreviventes, margin = 2) * 100
sobreviventesPercentual

df_sobreviventes <- as.data.frame(as.table(sobreviventes))
ggplot(data = df_sobreviventes, aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5) +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5, size = 3) +
  labs(title = "Sobreviventes por sexo", x = "Sexo", y = "Quantidade") +
  scale_fill_manual(values = c("1" = "blue", "0" = "red"))

##Idade média por sexo
idadePorSexo = aggregate(titanicTratado$AgeInt, by = list(titanicTratado$Sex), FUN = mean)
colnames(idadePorSexo) = c("Sexo", "Média de idade")
idadePorSexo

ggplot(data = idadePorSexo, aes(x = Sexo, y = `Média de idade`, fill = Sexo)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5) +
  labs(title = "Média da idade por Sexo",
       x = "Sexo", y = "Idade (anos)") +
  scale_fill_manual(values = c("red", "blue"))

##Preco da passagem por sexo
passagemPorSexo = aggregate(titanicTratado$Fare, by = list(titanicTratado$Sex), FUN = mean)
colnames(passagemPorSexo) = c("Sexo", "Preço médio")
passagemPorSexo

ggplot(data = passagemPorSexo, aes(x = Sexo, y = `Preço médio`, fill = Sexo)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5) +
  labs(title = "Preço Médio da Passagem por Sexo",
       x = "Sexo", y = "Preço (dólares") +
  scale_fill_manual(values = c("red", "blue"))

##Sexo por classe
sexoPorClasse = table(titanicTratado$Pclass, titanicTratado$Sex)
sexoPorClasse
sexoPorClassePercentual = prop.table(sexoPorClasse, margin = 2) * 100
sexoPorClassePercentual

df_sexoPorClasse <- as.data.frame(as.table(sexoPorClasse))
ggplot(data = df_sexoPorClasse, aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5) +
  labs(title = "Sexo por Classe", x = "Classe", y = "Quantidade") +
  scale_fill_manual(values = c("male" = "blue", "female" = "red"))


##Sobrevivência por classe
sobrevivenciaPorClasse = table(titanicTratado$Pclass, titanicTratado$Survived)
sobrevivenciaPorClasse
sobrevivenciaPorClassePercentual = prop.table(sobrevivenciaPorClasse, margin = 2) * 100
sobrevivenciaPorClassePercentual

df_sobrevivenciaClasse <- as.data.frame(as.table(sobrevivenciaPorClasse))
ggplot(data = df_sobrevivenciaClasse, aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5) +
  labs(title = "Sobrevivência por Classe", x = "Classe", y = "Quantidade") +
  scale_fill_manual(values = c("1" = "blue", "0" = "red"))

##Idade média por Classe
idadePorClasse = aggregate(titanicTratado$AgeInt, by = list(titanicTratado$Pclass), FUN = mean)
colnames(idadePorClasse) = c("Classe", "Média de idade")
idadePorClasse

ggplot(data = idadePorClasse, aes(x = Classe, y = `Média de idade`, fill = factor(Classe))) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(title = "Média de Idade por Classe",
       x = "Classe", y = "Média de Idade") +
  scale_fill_manual(values = c("blue", "green", "red")) +
  theme_minimal()

##Preço da passagem por Classe
precoPorClasse = aggregate(titanicTratado$Fare, by = list(titanicTratado$Pclass), FUN = mean)
colnames(precoPorClasse) = c("Classe", "Preço")
precoPorClasse

ggplot(data = precoPorClasse, aes(x = Classe, y = `Preço`, fill = factor(Classe))) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(title = "Preço por Classe",
       x = "Classe", y = "Preço (dólares)") +
  scale_fill_manual(values = c("blue", "green", "red")) +
  theme_minimal()

##Sobrevivência por idade
sobrevivenciaPorIdade = table(titanicTratado$Age >= 18, titanicTratado$Survived)
rownames(sobrevivenciaPorIdade) <- c("Menor de 18", "Maior de 18")
sobrevivenciaPorIdade
sobrevivenciaPorIdadePerc = prop.table(sobrevivenciaPorIdade, margin = 2) * 100
rownames(sobrevivenciaPorIdadePerc) <- c("Menor de 18", "Maior de 18")
sobrevivenciaPorIdadePerc

#1
stevenAndGarnit <- function(){
  stevenVenceu <- FALSE
  garnitVenceu <- FALSE
  
  #evitando que os dois escolham a mesma sequencia
  do{
    Steven <- c(sample(x = c(0, 1), size = 3, replace = TRUE))
    Garnit <- c(sample(x = c(0, 1), size = 3, replace = TRUE))
  } while(Steven != Garnit)
  
  
  lancamento <- c(sample(x = c(0, 1), size = 3, replace = TRUE))
  
  if(all(lancamento == Steven)){
    stevenVenceu <- TRUE
    return("Steven")
  } else if(all(lancamento == Garnit)){
    garnitVenceu <- TRUE
    return("Garnit")
  } else {
    while(stevenVenceu != TRUE & garnitVenceu != TRUE){
      lancamento <- realizaNovoLancamento(lancamento)
      if(all(lancamento == Steven)){
        stevenVenceu <- TRUE
        return("Steven")
      } else if(all(lancamento == Garnit)){
        garnitVenceu <- TRUE
        return("Garnit")
      }
    }
  }
}

realizaNovoLancamento <- function(lancamento){
  novoLancamento <- sample(x = c(0,1), size = 1)
  lancamento <- c(lancamento[length(lancamento) - 1], lancamento[length(lancamento)], novoLancamento)
  
  return(lancamento)
}

experimentoStevenAndGarnit <- stevenAndGarnit()
print(experimentoStevenAndGarnit)

simulaStevenAndGarnit10milx <- function(){
  res <- 0
  for(i in 1:10000){
    res[i] <- stevenAndGarnit()
  }
  return(mean(res == "Garnit"))
}

paste("A proporção de vitórias da Garnit é de ", simulaStevenAndGarnit10milx())
#   Ao executar o esperimento 10.000x, vemos que a proporção de vitórias da Garnit converge para 43%, o que signifca que a probabilidade do Steven ganhar é mais significante.

#2
dados <- read.table(file = "dados.txt", header = TRUE, sep = ';')

#a)
freq <- table(dados$Genero)
#ggplot(data = dados, mapping = aes(x = Genero, y = table(Genero)))
barplot(freq, ylim= c(0, 200), xlab = "Sexo", ylab = "Frequência")
#O gráfico apropriado para a representação da frequência é o gráfico de barras, pois há somente duas variações (men e women). Assim, o tamanho de cada barra será proporcional à quantidade de dados que corresponde a um dado valor.

#   Ao analisar o gráfico, podemos observar que a quantidade de mulheres é aproximadamente 4 vezes maior qua de homens

#b)

ggplot(dados, aes(x = Idade))+
  geom_histogram(bins = 8, color = "lightblue", width = 5)+
  facet_wrap(~Genero)
# Podemos observar que o assassinio tem uma preferência bastante considerável por mulheres que estão da faixa etária de 70 a 90 anos.
# Em relação aos homens, observa-se que esses estão em menor número em relação às mulheres e que, por sua vez, a faixa etára das vítimas masculinas se concentrar entre aproximadamente 65 a 85 anos.

 #c)
boxplot(dados$Idade)
# - 25% das vítimas tem 70 anos ou menos
# - 50% das vítimas tem até 75 anos
# - 75% das vítimas tem até 85 anos
# - há 8 pontos discrepantes, todos mais jovens que 55 anos
# Podemos concluir que o assassino ataca com frequência vítimas com idade entre 55 e 95 anos. Desses, ele dá preferência por vítimas entre 70 e 85 anos. 

#d)
unique(dados$LocalDaMorte)

ggplot(dados, aes(x = LocalDaMorte))+
  geom_bar() +
  labs(title = "Local da Morte", x = "Local", y = "Frequência") +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 250)) 
#Podemos observar que o local da morte das vítimas, na esmagadora maioria, é a própia casa; uma porcentagem muito pequena se encontra no hospital e no lar de idosos. Portanto, podemos concluir que o Modus Operandi do assassino é atacar suas vítimas em suas próprias residências.

#e)

ggplot(dados, aes(x = AnoDaMorte))+
  geom_bar()
# Conforme a nossa obsercação, nota-se um aumento na quantidade de vítimas a cada década.Na déccada de 70, o assassino lesou uma quantidade menor de vítimas. Na década de 80, esses números aumentaram consideravelmente, de maneira exponencial aproximadamente. Na década seguinte, Harold Shipman teve o seu auge no número de assassinatos, tendo uma quantidade de vítimas maior do que a soma dos assassinatos das duas décadas anteriores.

#f) Com base nos gráficos e informações obtidas nos intens anteriores, podemos traçar uma análise sobre o perfil de Harold Shipman, o Dr. Morte

#   Shipman iniciou os seus ataques no ano de 1975, durando até 1998. O médico aumentava a quantidade de mortes a cada década, tendo o seu auge na nos anos 90. Além disso, Harold tinha preferência por senhoras, que estavam na faixa etária de 70 a 90 anos. Nesse sentido, o local preferido do Dr. Morte para executar suas vítimas eram suas próprias residências, salvo por situações excepcionais em que Shipman assassinava no hospital ou no lar de idosos.


#3
treino <- read.table("treino_baleias.txt", header = TRUE, sep = ",")

teste <- read.table("teste_baleias.txt", header = TRUE, sep = ",")

#a)
unique(treino$especie)
treino$especie <- as.factor(treino$especie)
cachalote <- treino[treino$especie == "Cachalote",]

baleiaAzul <- treino[treino$especie == "Baleia Azul",]

baleiaFin <- treino[treino$especie == "Baleia Fin",]

jubarte <- treino[treino$especie == "Jubarte",]


#b)
media_Cachalote <- mean(cachalote$peso)
variancia_Cachalote <- var(cachalote$peso)
desvioPadrao_Cachalote <- sd(cachalote$peso)
CV_Cachalote <- 100 * (desvioPadrao_Cachalote/media_Cachalote)

media_BaleiaAzul <- mean(baleiaAzul$peso)
variancia_BaleiaAzul <- var(baleiaAzul$peso)
desvioPadrao_BaleiaAzul <- sd(baleiaAzul$peso)
CV_BaleiaAzul <- 100 * (desvioPadrao_BaleiaAzul/media_BaleiaAzul)

media_BaleiaFin <- mean(baleiaFin$peso)
variancia_BaleiaFin <- var(baleiaFin$peso)
desvioPadrao_BaleiaFin <- sd(baleiaFin$peso)
CV_BaleiaFin <- 100 *(desvioPadrao_BaleiaFin/media_BaleiaFin)


media_Jubarte <- mean(jubarte$peso)
variancia_Jubarte <- var(jubarte$peso)
desvioPadrao_Jubarte <- sd(jubarte$peso)
CV_Jubarte <- 100 * (desvioPadrao_Jubarte/media_Jubarte)

#Após os cálculos, podemos observar que a espécie com a maior média de peso é a Baleia Azul, com uma média de 20284.62 kg e a menor é a da Jubarte, com 4089.64 kg

#Podemos observar também que em todos os conjuntos a variância é significativamente maior do que a média, o que indica uma dispersão nos dados.

#como o desvio padrão dos conjuntos é de 14%(Cachalote), 7%(Baleia Azul), 9%(Baleia Fin) e 12% Jubarte, podemos classificar que a Baleia Azul tem uma dispersão moderada, entretanto as outras baleias têm uma dispersão consideravelmente alta.

#c)

hist(baleiaAzul$peso, ylim = c(0, 20))

#Ao analisar o histograma, podemos perceber que a forma em que os dados foram distribuídos é parecida com a distribuição normal; a exceção é a ausência de baleias cujo peso varie entre 23000 a 24000kg.

#d)
ggplot(treino, aes(x = comprimento))+
  geom_boxplot()+
  facet_wrap(~especie)+
  coord_flip()
#Observando os boxplots para cada espécie, é possível observar que há uma medida de comprimento que parece ser característica de cada espécie. Assim, a Baleia Azul tem o seu comprimento concentrado entre 28 a 32 metros; a Baleia Fin entre 24 e 26m; a Cachalote entre 18 e 21m; e a Jubarte de 17 a 19m.
# Todavia, percebe-se que há baleias de espécies distintas mas com o comprimento igual; esse é o caso da Cachalote e Jubarte. A interseção entre o comprimento desses animais está entre 17,5 e 19m, aproximadamente

#E)
ggplot(data = treino, aes(x = comprimento, y = profundidade_maxima, color = especie))+
  geom_point()

#F)
abline(h = 175)
respostas <- c()
for (i in 1:nrow(teste)){
  if(teste$profundidade_maxima[i] < 175){
    respostas[i] <- "Cachalote" 
  } else if(teste$profundidade_maxima[i] < 209){
    respostas[i] <- "Jubarte"
  } else {
    if(teste$comprimento[i] < 27){
    respostas[i] <- "Baleia Fin"
    } else {
    respostas[i] <- "Baleia Azul"
    }
  }
}
mean(respostas == teste$especie)

#Após implementar o modelo de Árvore de Decisão, obtemos uma taxa de acerto de 90%. Foi possível e viável aplicar esse modelo ao nosso conjunto de dados nessas variáveis porque, analisando o gráfico de dispersão, podemos notar um certo padrão de agrupamento dos dados; há uma forte tendência de que uma baleia de uma dada espécie tenha como característa um valor de profundidade máxima e comprimento que esteja presente em um intervalo bem definido.

#G)

ggplot(data = treino, aes(x = comprimento, y = profundidade_maxima, color = especie))+
  geom_point()+
  geom_hline(yintercept = 175, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 209, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 27, linetype = "dashed", color = "blue")

#H)
treino$especie <- as.factor(treino$especie)
teste$especie <- as.factor(teste$especie)
library(class)


modelo_k1 <- knn(train = treino[, -1], test = teste[, -1], cl = treino$especie, k = 1)

modelo_k3 <- knn(train = treino[, -1], test = teste[, -1], cl = treino$especie, k = 3)

mean(modelo_k1 == teste$especie)
mean(modelo_k3 == teste$especie)

#Comparando a taxa de acertos do modelo knn utilizando k = 1 com o modelo que usa k = 3, obtivemos uma taxa igual em ambos, 98%
#4)

cogumelos <- read.csv("cogumelos.csv", header = TRUE)
library(ggplot2)

cogumelos <- as.data.frame(lapply(cogumelos, as.factor))
cogumelos <- cogumelos[sample(nrow(cogumelos)), ]

#cogumelos$class <- as.factor(cogumelos$class)
n <- round(nrow(cogumelos) * 0.8)
treinamento <- cogumelos[1:n, ]
teste_cogumelos <- cogumelos[(n+1):nrow(cogumelos), ]


#Podemos observar que cogumelos com tampa de cores r e u estão em menor quantidade, entretanto, todos aqueles que possuem essa cor em sua tampa são comestíveis.
ggplot(data = treinamento, aes(x = class, color = class))+
  geom_bar()+
  facet_wrap(~cap.color)

#Plotanto o tipo de população dos cogumelos em função de ser comestível ou não, podemos observar que cogumelos cuja população é abundante(a) ou numerosa(n) são todos comestíveis

ggplot(data = treinamento, aes(x = population))+
  geom_bar()+
  facet_wrap(~class)

#Podemos observar que todos os cogumelos que estão situados no habitat de deserto são comestíveis
ggplot(data = treinamento, aes(x = habitat, colour = class))+
  geom_bar()

#Observa-se que cogumelos que apresentam odor duvidoso(y), picante(s) ou pungente(p) são venenosos
ggplot(data = treinamento, aes(x = class, y = odor))+
  geom_point()

#Podemos perceber que os cogumelos com talos enraizados(e) são comestíveis
ggplot(data = treinamento, aes(x = class, y = stalk.root))+
  geom_point()


#Com esse gráfico do tipo de anel em função de ser comestível, podemos observar que cogumelos com o tipo de anel f são sempre comestíveis, enquanto que anel do tipo l e n são sempre venenosos
ggplot(data = treinamento, aes(x = ring.type, y = class))+
  geom_point()
#*****interseções entre comestível e não é ring type = e, ring.type = p***


#cogumelos com tipo de anel e são comestiveis sse sua população for v; se o tipo de anel for  p, são comestíveis sse a população for c

respostas_cogumelos <- c()
for (i in 1:nrow(teste_cogumelos)) {
  if (teste_cogumelos$ring.type[i] == "f") {
    respostas_cogumelos[i] <- "e"
  } else if (teste_cogumelos$ring.type[i] %in% c("l", "n")) {
    respostas_cogumelos[i] <- "p"  
  } else if ((teste_cogumelos$veil.color[i] == "y" | teste_cogumelos$veil.color[i] == "w") & teste_cogumelos$ring.type[i] == "e") {
    respostas_cogumelos[i] <- "e"
  } else if ((teste_cogumelos$veil.color[i] == "w" | teste_cogumelos$veil.color[i] == "o" | teste_cogumelos$veil.color[i] == "n") & teste_cogumelos$ring.type[i] == "p") {
    respostas_cogumelos[i] <- "p"
  }
}

mean(respostas_cogumelos == teste_cogumelos$class)
ggplot(data = treinamento, aes(x = ring.type, y = veil.color, color= class))+
  geom_point()


ggplot(data = treinamento, aes(x = veil.color, y = class))+
  geom_point()


ggplot(data = treinamento, aes(x = cap.surface, y = class))+
  geom_point()

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
hist(dados$Idade, breaks = 9, xlab = "Idade", ylab = "Frequência")

#c)
boxplot(dados$Idade)
# - 25% das vítimas tem 70 anos ou menos
# - 50% das vítimas tem até 75 anos
# - 75% das vítimas tem até 85 anos
# - há 8 pontos discrepantes, todos mais jovens que 55 anos
# Podemos concluir que o assassino ataca com frequência vítimas com idade entre 55 e 95 anos. Desses, ele dá preferência por vítimas entre 70 e 85 anos. 

unique(dados$LocalDaMorte)

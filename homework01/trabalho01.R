#1a)
v1 <- c(10:30)

#1b)
v2 <- c(30:10)

#1c)
v3 <- c(v1, v2)

#2a)
u <- seq(from = 2, to= 8, by = 2)
u <- rep(u, 10)

#2b)
u1 <- rep(c(2, 4, 6, 8), times = 10)
u1[length(u1)] <- 2
u1

#2c)
w <- c(3, 7, 1)

#todos os valores repetidos 3 vezes
w1 <- rep(w, 3)

#o primeiro valor se repete 4x, o segundo, 2x e o ultimo, 3x
w2 <- rep(w, times = c(4, 2, 3))
w2

#3a)
resultado1 <- 0
for(n in 20:30){
  resultado1 <- resultado1 + (n**2) + 4*n
}
resultado1

#3b)
resultado2 <- 0
for(n in 10:20){
  resultado2 <- resultado2 + ((3**n)/n) + ((2**n)/(n**2))
}
resultado2


#4
sorteio <- sample(x = 1:100, size = 40, replace = TRUE)

#a)
pares <- sorteio[sorteio %% 2 == 0]
qtdPares <- length(pares)
#b)
maiorQue70 <- sorteio[sorteio > 70]
qtdMaior70 <- length(maiorQue70)

#c)
which(sorteio %% 2 != 0)

#5

lancamentoAte4 <- function(){
  houve4 <- 0
  count <- 0
  while(houve4 < 2){
    dado <- sample(x = 1:6, size = 1, replace = TRUE)
    if(dado == 4){
      houve4 <- houve4 + 1
    }
    count <- count + 1
  }
  return(count)
}
paste(lancamentoAte4()," lançamentos foram necessários para o 4 ser sorteado 2 vezes")

#6
quantidades <- 0
for (i in 1:10000){
  quantidades[i] <- lancamentoAte4()
}
paste("são necessários", mean(quantidades), "lançamentos, em média, para se obter o 4 duas vezes")

#7

#para n >= 3
fibonacci <- function(n){
  fib <- c(1, 1)
  for (i in 3:n){
    fib[i] <- fib[i-2] + fib[i-1]
  }
  return(fib)
}

#8
contagem <- 0
for( i in 1:100000){
  #a j-ésima pessoa do vetor participantes sorteou a j-ésima pessoa do vetor sorteio.
  participantes <- c("Michael", "Dwight", "Jim", "Kevin", "Creed")
  sorteio <- sample(x = participantes, size = 5, replace = FALSE)
  # ao fazer participantes == sorteio, estamos comparando entrada por entrada e retornaremos um vetor lógico.
  # como o amigo oculto dá errado quando uma pessoa sorteia ela própria, consideraremos que foi um sucesso se TODAS as entradas de um dos vetores diferirem da sua correspondente do outro vetor.
  # Portanto, contamos como sucesso se all() as entradas lógicas forem TRUE.
  if(all(participantes != sorteio) == 0){
    contagem <- contagem + 1
  }
}
qtdVezesErro <- 100000 - contagem
paste("A proporção de vezes em que o  amigo oculto deu errado é de", (qtdVezesErro/100000))


#9
propVitoria <- 0
for(i in 1:100000){
  dado1 <- sample(x = 1:6, size = 1, replace = TRUE)
  dado2 <- sample(x = 1:6, size = 1, replace = TRUE)
  somaInicial <- dado1 + dado2
  somaFinal <- 0
  if(somaInicial == 7 | somaInicial == 1){
    propVitoria <- propVitoria + 1
  }else if(somaInicial == 2| somaInicial == 3 | somaInicial == 12){
  } else {
    while(somaFinal != 7 & somaFinal != somaInicial){
      dado1 <- sample(x = 1:6, size = 1, replace = TRUE)
      dado2 <- sample(x = 1:6, size = 1, replace = TRUE)
      
      somaFinal <- dado1 + dado2
      
      if(somaFinal == 7){
      } else if(somaFinal == somaInicial){
        propVitoria <- propVitoria + 1
      }
    }
  }
}
paste("A proporção de vitória é de: ", propVitoria/100000)


#11-a)
moeda <- c("cara", "coroa")
# L > 0 e L < 20
passeio <- function(l){
  while(l != 20 & l != 0){
    lancamentoSkywallker <- sample(x = moeda, size = 1, replace = TRUE)
    if(lancamentoSkywallker == "cara"){
      l <- l + 1
    } else {
      l <- l - 1
    }
  }
  
  if(l == 0){
    return(0)
  } else {
    return(1)
  }
}
resultado <- passeio(10)
print(resultado)


#b)
replicaPasseio <- function(l){
  resultPasseio <- 0
  for(i in 1:10000){
    resultPasseio[i] <- passeio(l)
  }
  return(mean(resultPasseio))
}
resultadoPasseio <- replicaPasseio(10)
print(resultadoPasseio)

#c)
x <- 1:19
y <- 0
for(i in x){
  y[i] <- replicaPasseio(i)
}

plot(x, y)

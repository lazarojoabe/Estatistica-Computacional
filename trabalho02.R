#1
stevenAndGarnit <- function(){
  stevenVenceu <- FALSE
  garnitVenceu <- FALSE
  Steven <- c(sample(x = c(0, 1), size = 3, replace = TRUE))
  Garnit <- c(sample(x = c(0, 1), size = 3, replace = TRUE))
  
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

#r <- simulaStevenAndGarnit10milx()
paste("A proporção de vitórias da Garnit é de ", simulaStevenAndGarnit10milx())


#2
dados <- read.table(file = "dados.txt", header = TRUE, sep = ';')

install.packages("ggplot")

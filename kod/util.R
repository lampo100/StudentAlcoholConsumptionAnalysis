accuracy <- function(true, predicted){
  sum(predicted == true)/length(predicted)
}
# Funkcja wyświetlająca przewidywane wartości oraz ich prawdziwe wartości
display.prediction.plot <- function(model, d, which.attribute, row.number){
  library(hexbin)
  h <- predict(model, d)
  y <- d[,which.attribute]
  par(xpd = T, mar = par()$mar + c(0,0,0,7))
  plot(x=c(1:row.number), y=as.numeric(d[1:row.number, which.attribute]),pch=15, col="red",
       main="Porownanie predykcji z prawdziwymi wartosciami",
       xlab="Przyklad",
       ylab=paste("Wartosc", which.attribute), bty='L')
  points(x=c(1:row.number), y=as.numeric(h[1:row.number]), col="green", pch=23)
  legend(row.number+1, 4.5, legend=c("Prawdziwa", "Przewidziana"), pch=c(15, 23), col=c("red", "green"))
  par(mar=c(5, 4, 4, 2) + 0.1)
  }
d = read.table("daneAI.txt", 
               sep=",", 
               col.names=c("season", "age", "childDiseas", "tAccidents", "sIntervention", "highFever", "fAlcohol", "smoking", "sitting", "output"), 
               fill=FALSE, 
               strip.white=TRUE)

scatter.smooth(x=d$age, y=d$fAlcohol, main="age ~ alcohol")

d$age <- (d$age * 18) + 18

#dla wygody przypiszemy odpowiednie zmienne do x i y
x<-d$age
y<-d$fAlcohol
#model regresji dla x i y
fit.lm<-lm(y~x)

#współczynniki
beta0<-coef(fit.lm)[1]
beta1<-coef(fit.lm)[2]

#prosta regresji
curve(beta0+beta1*x, from = min(x), to = max(x), col='red', ylim=c(min(y), max(y)),
      xlab='wiek badanego', ylab='czestotliwosc spozywania alkoholu')
points(x,y)

cat("y = ", toString(beta1), " * x + ", toString(beta0))

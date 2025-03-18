alfa <- 0.05
s <- 1
m <- 0.5
n <- 10

los <- rnorm(10,sr,s)
t.test(los)

N <- 1000
los <- replicate(N, rnorm(n,m,s)) 
i<-1

odrzucone_H0 <- 0 
nieodrzucone_H0 <- 0  
wynik_moc<-numeric(N)
for (i in 1:N) {
  test <- t.test(los[, i])
  if (test$p.value < alfa) {
    odrzucone_H0 <- odrzucone_H0 + 1 
  } else {
    nieodrzucone_H0 <- nieodrzucone_H0 + 1 
  }
  
}
wynik_moc[i]<-odrzucone_H0/N
odrzucone_H0/N



alfy <- c(0.001,0.01,0.1,0.2,0.5)

for(k in 1:length(alfy))
{
  for (i in 1:N) {
    test <- t.test(los[, i], conf.level = 1- alfy[k])
    if (test$p.value < alfy[k]) {
      odrzucone_H0 <- odrzucone_H0 + 1 
    } else {
      nieodrzucone_H0 <- nieodrzucone_H0 + 1 
    }
  }
  print(odrzucone_H0/N)
}

odrzucone_H0 <- 0 
nieodrzucone_H0 <- 0 
licznosc <- c(10,20,30,40,50,100,200,300,500,1000,5000,10000)
for(k in 1:length(licznosc))
{
  N <- licznosc[k]
  los <- replicate(N, rnorm(n,m,s)) 
  for (i in 1:N) {
    test <- t.test(los[, i])
    if (test$p.value < alfa) {
      odrzucone_H0 <- odrzucone_H0 + 1 
    } else {
      nieodrzucone_H0 <- nieodrzucone_H0 + 1 
    }
  }
  print(odrzucone_H0/N)
}

# czescs IV

min <- 1
max <- 42
N <- 1000
los <- replicate(N, runif(n,min,max)) 
odrzucone_H0 <- 0 
nieodrzucone_H0 <- 0  

for (i in 1:N) {
  test <- t.test(los[, i], mu=m)
  if (test$p.value < alfa) {
    odrzucone_H0 <- odrzucone_H0 + 1 
  } else {
    nieodrzucone_H0 <- nieodrzucone_H0 + 1 
  }
}
odrzucone_H0/N


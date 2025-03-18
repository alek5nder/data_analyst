set.seed(112)
library(grid)
# Parametry symulacji
n <- c(10, 50, 100,500)
df<- c(5, 10, 30)
N <- 1000

wyniki_shapiro <- expand.grid(n=n,df=df, pvalue=NA )
wyniki_ks <- expand.grid(n = n, df = df, pvalue = NA)
wyniki_chi2 <- expand.grid(n = n, df = df, pvalue = NA)
for (i in 1:nrow(wyniki_shapiro)) {
  probka <- rt(wyniki_shapiro$n[i], wyniki_shapiro$df[i])
  wyniki_shapiro$pvalue[i] <- round(shapiro.test(probka)$p.value,4)
  
  ks_test <- ks.test(probka, "pt", df = wyniki_ks$df[i])
  wyniki_ks$pvalue[i] <- round(ks_test$p.value, 4)
  
  breaks <- qt(seq(0, 1, length.out = 6), df = wyniki_chi2$df[i]) 
  observed <- table(cut(probka, breaks, include.lowest = TRUE)) 
  

  expected_probs <- diff(pt(breaks, df = wyniki_chi2$df[i])) 
  expected <- expected_probs * length(probka)
  
  if (any(expected < 5)) {
    wyniki_chi2$pvalue[i] <- NA
  } else {
    chi2_test <- chisq.test(observed, p = expected / sum(expected), rescale.p = TRUE)
    wyniki_chi2$pvalue[i] <- round(chi2_test$p.value, 4)
  }
  
}


wyniki_shapiro
wyniki_ks
wyniki_chi2

odrzucone_H0


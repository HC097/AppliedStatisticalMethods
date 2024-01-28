myfun <- function(x){
  y <- x^2
  list(x = x, y = y)
}


ll <- myfun(x = 1:10)

ll$y

Intro2R::myreadxl()
v$MTBE -> mtbe
table(mtbe$Aquifier, mtbe$'MTBE-Detect')
addmargins(tabb)

#layering in gg plot
library(ggplot2)
library(INtro2R)
library(magrittr)

g <- ddt %>% ggplot(aes(x=WIEGHT,y=LENGTH))

g <- g + geom_point()

g <- g + stat_smooth(method="lm", formula=y~x)

table(ddt$RIVER, ddt$SPECIES)

#p(a <= Y <= b)
curve(dnorm(x, 10, 16), xlim=c(10-(3*4), 10+(3*4)))
xcurve <- seq(9, 14, 1000)
ycurve <- dnorm(xcurve, 10, 4)
polygon(c(9, xcurve, 14), c(0, ycurve, 0), col="Red")

prob <- pnorm(14, 10,4)- pnorm(9, 10, 4)
prob <- round(prob, 4)

text(locator(1),paste0(("Area= ", prob)))
l <- locator(1)

mychisim <- function(iter = 1000, n = 10){
  mat <- matrix(data = NA, nrow = n, ncol = iter, byrow = TRUE)
  
  for(i in 1:iter){
    mat[,i] <- rnorm(n = n, mean=0, sd=1)^2
    
  }
  
  stat <- apply(mat, 2, sum)
  
  h <- hist(stat, plot = FALSE)
  
  dd <- h$density
  cll <- dd/max(dd)
  
  hist(stat, freq = FALSE, col = cll)
  
  curve(dnorm(x, mean = n, sd = sqrt(2*n)), add = TRUE, lwd = 2, col = "Red")
  
  
}

graphics.off()

mychisim(iter=10000,n=100)

mychisim(iter = 5)

library(Intro2R)
ddt
L <- ddt$LENGTH
L
n <- length(L)
alpha <- .05
chiL <- qchisq(1-alpha/2, n-1)
chiU <- qchisq(alpha/2, n-1)
ci <- c((n-1)*var(L)/chiL, (n-1)*var(L)/chiU)

y1 <- rnorm(30,15,8)
y2 <- rnorm(40,10,8)
t.test(y1,y2,mu=0, var.equal=TRUE)

var.test(y1, y2)

t.test(y1)$conf.int
alpha <- .05
n <- length(y1)
t <- qt(1-alpha/2, n-1)
mp <- c(-1, 1)
mean(y1) + mp*t*sd(y1)/sqrt(n)

#10.72
v<-Intro2R::myreadxl()
helium<-v$HELIUM
names(helium)
ylm<-lm(PROPPASS~etc.inchpt10slides)

x<-1:30
set.seed(23)
y<-5+8*x+rnorm(30,0,10)
windows();plot(x,y)

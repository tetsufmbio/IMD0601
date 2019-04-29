x<-rbeta(5000,2,5)
plot(x,dbeta(x,2,5),main="Prior dist. P(H)")

plot(0:10,dbinom(0:10,10,0.5),,type='h',xlab="",main="Data dist. P(E|H)")

x<-rbeta(5000,7,10)
plot(x,dbeta(x,7,10),main="Posterior dist P(H|E)")

p <- (0:1000)/1000
plot(p,dbeta(p,2,5),,ylim=range(0:4),type="l", ylab="f(p)", main="Comparing Prior and Posterior")
lines(p,dbeta(p,7,10))
legend(0,3,legend="Prior")
legend(0.4,3.6,legend="Posterior")

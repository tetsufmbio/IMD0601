theta<-seq(0,2,0.001)
y<-dgamma(theta,2,5)
plot(theta,y,ylim=range(0:3),ylab="",xlab="theta")
y2<-dgamma(theta,10,15)
lines(theta,y2)
legend(0,2.5,"Prior")
legend(0.6,2.8,"Posterior")

# Beta prior não informativo
x<-seq(0,1,by=.001)
plot(x,dbeta(x,1,1),ylab="",main="Beta (1,1) Prior")

# efeito da função verossimilhança na distribuição posteriori
p <- seq(0, 1, by = 0.001)
plot(p, dbeta(p, 2, 5), ylim = range(0:10), ylab = "f(p)", type = "l", main = "Effect of n on Posterior")
lines(p, dbeta(p, 7, 10))
lines(p, dbeta(p, 52, 55))
legend(0, 4, "Prior")
legend(0.2, 6, "Post.1")
legend(0.5, 9, "Post.2")
       
# efeito da distribuição a priori na distribuição posteriori
p <- seq(0.001, 0.999, by = 0.001)
plot(p, dbeta(p, 1, 1), ylim = range(0:10), ylab = "f(p)", type = "l", main = "Effect of noninformative prior")
lines(p, dbeta(p, 6, 6))
legend(0.5, 4, "Posterior")
legend(0, 3, "Prior")

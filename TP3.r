sample(1 :6)

sample(1 :6,size=2)

sample(1 :6,2,replace=T)

production=c(rep(" NC",2),rep("C",98))
sample(production,80,replace=T)
echantillon=sample(production,80,replace=T) 
echantillon
qualite=c("NC","C")
sample(qualite,80,replace=T,prob=c(2/100,98/100))

compter = function(a,b) {
  d = numeric()
  for(i in 1:length(a))
    d[i] = sum(b==a[i])
  names(d) = as.character(a)
  return(d)
}

couleurs=c("tr","ca","pi","co")
jeu=rep(couleurs,rep(8,4))
tirage=function(nb,k)
  {
    resu=logical(nb)
    for(i in 1 :nb){
      resutirage=sample(jeu,5)
      if(i<=k)
        cat(resutirage,"\n")
      compteresutirage=compter(couleurs,resutirage)
      compterrouge=sum(compteresutirage[c("ca","co")])
      resu[i]=compterrouge==2
    }
    freq=mean(resu)
    prob=choose(16,2)*choose(16,3)/choose(32,5)
    res=c(prob,freq)
    names(res)=c("prob","freq")
    res
}

tirage(10,10)
tirage(1000,10)

#Exercice 11

simul1 = rnorm(1000,15,sqrt(3))
hist(simul1, probability=T, col="green")
curve(dnorm(x,15,sqrt(3)), add=T,col="red")
boxplot(simul1)

#Exercice 12

simul2 = rpois(1000,1)
maxi = max(simul2)
nbsimul2 = compter(0:maxi,simul2)
points(barplot(nbsimul2, col="green"), dpois(0:maxi,1)*1000, type='h')

#Exercice 13 

peage=function(m1,m2,sigma,n){
  attente = numeric(n)
  cabines = c("C1","C2")
  for(i in 1:n){
    
  }
}

#Exercice 15
stu1 = function(n, nb, mu, sigma) {
  vec = numeric(nb)
  for (i in 1:nb) {
    simul = rnorm(n, mu, sigma)
    moy = mean(simul)
    ecart = sd(simul)
    vec[i] = sqrt(n)*(moy - mu)/ecart
  }
  par(mfrow=c(2,1))
  vecnew = vec[vec>=-3 & vec<=3]
  hist(vecnew, probability = T, ylim = c(0,0.5))
  curve(dnorm(x),-3,3, add=T, col="red")
  curve(dt(x, n-1),-3,3, add=T, col="blue")
  legend("topleft", legend = c("N(0,1)", paste("T(",n-1,")",sep="")), col = c("black", "red"), lwd=1)
  
  plot(ecdf(vec), xlim = c(-3,3))
  curve(pnorm(x),-3,3, add=T, col="red")
  curve(pt(x, n-1),-3,3, add=T, col="blue")
  legend("topleft", legend = c("F_N(0,1)", paste("F_T(",n-1,")",sep="")), col = c("black", "red"), lwd=1)
  par(mfrow=c(1,1))
}

stu1(3,1000,10,2)

#Exercice 16
par(mfrow=c(2,3))
deg_khiDeux = c(1,2,3,5,10,20)
range = c(10,10,10,10,20,60)
for (i in 1:6) {
  curve(dchisq(x, deg_khiDeux[i]), 0.01, range[i])
}
par(mfrow=c(1,1))

#Exercice 17
khi2 = function(n, nb, mu, sigma) {
  vec = numeric(nb)
  for (i in 1:nb) {
    simul = rnorm(n, mu, sigma)
    s2 = var(simul)
    vec[i] = (n-1)*s2/(sigma^2)
  }
  vec = vec[vec>=0.01 & vec<=10]
  hist(vec)
  curve(dchisq(x, 4), add = T)
}

khi2(4,10000,10,2)

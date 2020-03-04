conso.dat = read.table("donneesconsommation.txt", header = T)

#Exercice 4.1.1
simuesti =  function(n, k) {
  pop = conso.dat$Revenu
  N = length(pop)
  mu = mean(pop)
  varp = (N-1) / N * var(pop)
  varemp = numeric(k)
  varech = numeric(k)
  
  for (i in 1:k) {
    echan = sample(pop, n, replace = T)
    varech[i] = var(echan)
    varemp[i] = (n-1) / n * varech[i]
  }
  etendue=range(varech, varemp)
  
  par(mfrow=c(2,1))
  
  plot(1:k, varemp, type = "p", col = "purple", ylim = etendue)
  abline(h = varp, col = "blue")
  abline(h = mean(varemp), col = "red")
  legend("topleft", c("varp", "mean(varemp)"), col = c("red", "blue"), lwd = 1)
  
  
  plot(1:k, varech, type = "p", col = "purple", ylim = etendue)
  abline(h = varp, col = "blue")
  abline(h = mean(varech), col = "red")
  legend("topleft", c("varp", "mean(varech)"), col = c("red", "blue"), lwd = 1)
  
  par(mfrow=c(1,1))
}

simuesti(3,1000)
simuesti(30,1000)

#Exercice 4.1.2
simunif = function(teta, n, k) {
  esti1 = numeric(k)
  esti2 = numeric(k)
  
  for (i in 1:k) {
    echan = sample(1:teta, n, replace = T)
    esti1[i] = 2 * mean(echan) - 1
    esti2[i] = max(echan)
  }
  etendue=range(esti1, esti2)
  
  plot(1:k, esti1, type = "p", ylim = etendue, col = "blue")
  points(1:k, esti2, type = "p", ylim = etendue, col = "red")
  abline(h = teta, col = "black")
  abline(h = mean(esti1), col = "blue")
  abline(h = mean(esti2), col = "red")
  legend("topleft", c("teta", "esti1", "esti2"), col = c("black", "blue", "red"), lwd = 1)
  r1 = sum(esti1 - teta)^2 / k
  r2 = sum(esti2 - teta)^2 / k
  text(1, min(etendue), paste("r1 = ", r1, " r2 = ", r2), pos = 4)
}

simunif(1000, 20, 200)

#Exercice 4.2.1
simucf1 = function (k, m, sigma, l0) {
  simulation = rnorm(k, m, sigma)
  #mean.x = numeric(k)
  #for (l in 1:k) {
  #  mean.x[l] = 1/l * sum(simulation[1:l])
  #}
  mean.x = cumsum(simulation)/(1:k)
  plot(l0:k, mean.x[l0:k], type = "l", col = "purple")
  abline(h = m, col = "red")
}

simucf1(300, 10, 2 ,10)

#Exercice 4.2.2
simucf2 = function (k, l0) {
  pop = conso.dat$Revenu
  mu = mean(pop)
  simulation = sample(pop, k, replace = T)
  mean.x = cumsum(simulation) / (1:k)
  plot(l0:k, mean.x[l0:k], type = "l", col = "purple")
  abline(h = mu, col = "red")
}

simucf2(300, 10)

#Exercice 4.2.3
simucf3 = function (k, m, sigma, l0) {
  simulation = rnorm(k, m, sigma)
  s.x = numeric(k)
  for (l in 1:k) {
    s.x[l] = var(simulation[1:l])
  }
  etendue=range(c(sigma, s.x[l0:k]))
  
  plot(l0:k, s.x[l0:k], type = "l", col = "purple", ylim = etendue)
  abline(h = sigma^2, col = "red")
}

simucf3(300, 10, 2 ,10)

#Exercice 4.2.4
simucf4 = function (k, l0) {
  pop = conso.dat$Magnetoscope
  prop = mean(pop)
  simulation = sample(pop, k , replace = T)
  distribOfProp = cumsum(simulation) / (1:k)
  etendue=range(c(prop, distribOfProp[l0:k]))
  plot(l0:k, distribOfProp[l0:k], type = "l", col = "purple", ylim = etendue)
  abline(h = prop, col = "red")
}

simucf4(300, 10)

simucf4bis = function (k, l0, prop) {
  simulation = rbinom(k, 1, p)
  plot(l0:k, distribOfProp[l0:k], type = "l", col = "purple", ylim = etendue)
  abline(h = prop, col = "red")
}

simucf4bis(300, 10, mean(conso.dat$Magnetoscope))

#Exercice 4.2.5
simucf5 = function(k, l0) {
  pop = rcauchy(k)
  plot(l0:k, distribOfProp[l0:k], type = "l", col = "purple", ylim = etendue)
  abline(h = prop, col = "red")
}
simucf5(300, 10)

thcl1 = function(n, k) {
  vec = numeric(k)
  pop = conso.dat$Revenu
  for (i in 1:k) {
    echan = sample(pop, n, replace = T)
    vec[i] = 2 * mean(echan) - 1
  }
}
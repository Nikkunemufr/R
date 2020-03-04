library(stantard)

etude <- data.frame(read.table("enquete.csv", sep="", header=T))
attach(etude)

plot(poids)
plot(1 :10,poids)
plot(c(4,1,2,6,5,9,7,10,3,8),poids)

plot(poids,type = "o") 
plot(poids,type ="b",xlab="numero")

plot(poids,pch=4,xlab="numero")
plot(poids,type ="b",pch="a",xlab="numero")

plot(poids,type ="b",xlab="numero",xlim=c(-10,20),ylim=(c(30,90)))
plot(poids,pch=4,xlab="numero",xaxt="n")

plot(poids,pch=4,xlab="numero",col="red")
plot(poids,pch="a",xlab="numero",cex=0.5)

plot(poids,type="l",lty=2)
plot(poids,type="l",lty=2,axes = F)

plot(poids,type="l",main="poids des personnes")

curve(sin(x),-3,10)

myfunction <- function(x) sin(cos(x)*exp( -x/2))
curve(myfunction, -8,7)
curve(myfunction, -8,7, n=2001)

vec1=seq(from=a,to=,length=n) 
plot(vec1,vec2,type="l")

barplot(poids)
fcouleur=table(couleur)
barplot(fcouleur)




conso.dat <- data.frame(read.table("donneesconsommation.txt", sep="", header=T))
attach(conso.dat)

conso.list = list(conso.dat[3], conso.dat[4], conso.dat[5], conso.dat[6], conso.dat[7], compter(levels(conso.dat$Cafe), conso.dat$Cafe),
                  compter(unique(conso.dat$Enfants), conso.dat$Enfants),
                  compter(unique(conso.dat$Magnetoscope), conso.dat$Magnetoscope),
                  compter(unique(conso.dat$Lavevaisselle), conso.dat$Lavevaisselle), conso.dat[5])
conso.listbis = list(conso.dat[3], conso.dat[4], conso.dat[5], conso.dat[6], conso.dat[7])


conso.list2 = list(compter(levels(conso.dat$Age), conso.dat$Cafe))

loisirsTa1GR1 = conso.dat[conso.dat$Age == "TA1" & conso.dat$Region == "GR1", ]$Loisirs
loisirsTa1GR2 = conso.dat[conso.dat$Age == "TA1" & conso.dat$Region == "GR2", ]$Loisirs
loisirsTa2GR1 = conso.dat[conso.dat$Age == "TA2" & conso.dat$Region == "GR1", ]$Loisirs
loisirsTa2GR2 = conso.dat[conso.dat$Age == "TA2" & conso.dat$Region == "GR2", ]$Loisirs


hist(loisirsTa1GR2, xlab = "Consommation de loisirs",ylab = "Frequence",
     main = " Consommation de loisirs pour la TA1, Groupe1")
hist(loisirsTa1GR2, xlab = "Consommation de loisirs",ylab = "Frequence",
     main = " Consommation de loisirs pour la TA1, Groupe2")
hist(loisirsTa2GR1, xlab = "Consommation de loisirs",ylab = "Frequence",
     main = " Consommation de loisirs pour la TA2, Groupe1")
hist(loisirsTa2GR2, xlab = "Consommation de loisirs",ylab = "Frequence",
     main = " Consommation de loisirs pour la TA2, Groupe2")

consopateTa1GR1 = conso.dat[conso.dat$Age == "TA1" & conso.dat$Region == "GR1", ]$Pates
consopateTa1GR2 = conso.dat[conso.dat$Age == "TA1" & conso.dat$Region == "GR2", ]$Pates
consopateTa2GR1 = conso.dat[conso.dat$Age == "TA2" & conso.dat$Region == "GR1", ]$Pates
consopateTa2GR2 = conso.dat[conso.dat$Age == "TA2" & conso.dat$Region == "GR2", ]$Pates

hist(consopateTa1GR1, xlab = "Consommation de pates",ylab = "Frequence",
     main = " Consommation de pates pour la TA1, Groupe1")
hist(consopateTa1GR2, xlab = "Consommation de pates",ylab = "Frequence",
     main = " Consommation de pates pour la TA1, Groupe2")
hist(consopateTa2GR1, xlab = "Consommation de pates",ylab = "Frequence",
     main = " Consommation de pates pour la TA2, Groupe1")
hist(consopateTa2GR2, xlab = "Consommation de pates",ylab = "Frequence",
     main = " Consommation de pates pour la TA2, Groupe2")

moy.revenu = tapply(conso.dat$Revenu, list(conso.dat$Age, conso.dat$Region), mean)
sd.revenu = tapply(conso.dat$Revenu, list(conso.dat$Age, conso.dat$Region), sd)
moy.consopate = tapply(conso.dat$Pates, list(conso.dat$Age, conso.dat$Region), mean)
sd.consopate = tapply(conso.dat$Pates, list(conso.dat$Age, conso.dat$Region), sd)
moy.loisirs = tapply(conso.dat$Loisirs, list(conso.dat$Age, conso.dat$Region), mean)
sd.loisirs = tapply(conso.dat$Loisirs, list(conso.dat$Age, conso.dat$Region), sd)


hist(moy.revenu,xlab = "Consommation de pates",ylab = "Frequence",
     main = " Consommation de pates pour la TA2, Groupe2")
hist(sd.revenu, xlab = "Consommation de pates",ylab = "Frequence",
     main = " Consommation de pates pour la TA2, Groupe2")
hist(moy.consopate, xlab = "Consommation de pates",ylab = "Frequence",
     main = " Consommation de pates pour la TA2, Groupe2")
hist(sd.consopate, xlab = "Consommation de pates",ylab = "Frequence",
     main = " Consommation de pates pour la TA2, Groupe2")
hist(sd.loisirs, xlab = "Consommation de pates",ylab = "Frequence",
     main = " Consommation de pates pour la TA2, Groupe2")
hist(sd.loisirs, xlab = "Consommation de pates",ylab = "Frequence",
     main = " Consommation de pates pour la TA2, Groupe2")



conso.dat <- as.tibble(read.table("donneesconsommation.txt", header=T))


# Figurersur chaque fenêtre le nuage de points(x[i],y[i])oùx[i]est le revenu ety[i]la dépense de loisirs
par(mfrow=c(2,3))
for (age in unique(conso.dat$Age)) {
  for (region in unique(conso.dat$Region)) {
    local.dat <- conso.dat[conso.dat$Age == age & conso.dat$Region == region,]
    plot(local.dat[, c("Revenu", "Loisirs")])
  }
}


# Faire la même chose mais en remplaçant loisirs par consommation de pâtes alimentaires.
par(mfrow=c(2,3))
for (age in unique(conso.dat$Age)) {
  for (region in unique(conso.dat$Region)) {
    local.dat <- conso.dat[conso.dat$Age == age & conso.dat$Region == region,]
    plot(local.dat[, c("Revenu", "Pates")], main=paste(region, age))
  }
}

# Idem mais en mettant dans chaque fenêtre l’histogramme de revenu et en ajoutant un texte qui indiquela moyenne et l’écart-type d’échantillonnage de la série des revenus pour la grande région et la tranche d’âgeconsidérée.
par(mfrow=c(2,3))
for (age in unique(conso.dat$Age)) {
  for (region in unique(conso.dat$Region)) {
    local.dat <- conso.dat[conso.dat$Age == age & conso.dat$Region == region,]
    local.revenues <-local.dat$Revenu
    hist(local.revenues, main=paste(region, age))
    legend(0,10,paste("mean = ",mean(local.revenues)))
    legend(0,7,paste("sd = ",sd(local.revenues)))
  }
}

# Idem mais en mettant dans chaque fenêtre l’histogramme de loisirs et en ajoutant un texte qui indiquela moyenne et l’écart-type d’échantillonnage de la série des revenus pour la grande région et la tranche d’âgeconsidérée.
par(mfrow=c(2,3))
for (age in unique(conso.dat$Age)) {
  for (region in unique(conso.dat$Region)) {
    local.dat <- conso.dat[conso.dat$Age == age & conso.dat$Region == region,]
    local.loisirs <-local.dat$Loisirs
    hist(local.loisirs, main=paste(region, age))
    legend(0,10,paste("mean = ",mean(local.loisirs)))
    legend(0,7,paste("sd = ",sd(local.loisirs)))
  }
}

# Idem mais en mettant dans chaque fenêtre l’histogramme de pates et en ajoutant un texte qui indique lamoyenne et l’écart-type d’échantillonnage de la série des revenus pour la grande région et la tranche d’âgeconsidérée.
par(mfrow=c(2,3))
for (age in unique(conso.dat$Age)) {
  for (region in unique(conso.dat$Region)) {
    local.dat <- conso.dat[conso.dat$Age == age & conso.dat$Region == region,]
    local.pates <-local.dat$Pates
    hist(local.pates, main=paste(region, age))
    legend(0,10,paste("mean =",mean(local.pates)))
    legend(0,7,paste("sd =",sd(local.pates)))
  }
}

max.ypates = 0 
# Idem avec les boxplots (sans les moyennes et écart-types)
par(mfrow=c(2,3))
for (age in unique(conso.dat$Age)) {
  for (region in unique(conso.dat$Region)) {
    local.dat <- conso.dat[conso.dat$Age == age & conso.dat$Region == region,]
    local.pates <-local.dat$Pates
    max.ypates=max(max.ypates, local.pates)
    boxplot(local.pates, main=paste(region, age), ylim=c(0,max.ypates))
  }
}

max.ycafe = 0
# Idem avec la distribution de la préférence pour le café (sans les moyennes et écart-types)
par(mfrow=c(2,3))
for (age in unique(conso.dat$Age)) {
  for (region in unique(conso.dat$Region)) {
    local.dat <- conso.dat[conso.dat$Age == age & conso.dat$Region == region,]
    local.cafe <-local.dat$Cafe
    local.cafe.dist <- table(local.cafe)
    max.ycafe=max(max.ycafe, local.cafe.dist)
    barplot(local.cafe.dist, main=paste(region, age),ylim=c(0,max.ycafe))
  }
}
max.yenfant = 0
# Idem avec la distribution du nombre d’enfants (sans les moyennes et écart-types)
par(mfrow=c(2,3))
for (age in unique(conso.dat$Age)) {
  for (region in unique(conso.dat$Region)) {
    local.dat <- conso.dat[conso.dat$Age == age & conso.dat$Region == region,]
    local.enfants <-local.dat$Enfants
    local.enfants.dist <- table(factor(local.enfants, levels= min(conso.dat$Enfants):max(conso.dat$Enfants)))
    max.yenfant=max(max.yenfant, local.enfants.dist)
    barplot(local.enfants.dist, main=paste(region, age), ylim = c(0,max.xenfant))
    
  }
}


enquete <- read.csv("enquete.csv", sep=" ")

enquete$index = 1:nrow(enquete)
sorted.enquete = enquete[order(-enquete$poids),]
par(mfrow=c(1,1))

library("viridis")           # Load

sorted.enquete$color = heat.colors(nrow(enquete))

par(lwd=3)
bppoids <- barplot(sorted.enquete$poids, main="poids", ylim = c(0, 80), names.arg = sorted.enquete$index, col=sorted.enquete$color, space = 2, lwd=3)
text(x=bppoids, y=sorted.enquete$poids+4, label=sorted.enquete$poids)
axis(1, at=bppoids, labels=order(poids, decreasing=T), lwd=3)
par(lwd=1)

couleur.count = as.data.frame(table(enquete$couleur))
couleur.count$index = 1:nrow(couleur.count)
couleur.count$color = heat.colors(nrow(couleur.count))
couleur.count.sorted = couleur.count[order(-couleur.count$Freq),]
couleur.count.lign = couleur.count[order(couleur.count$Freq),]
couleur.count.lign$center = cumsum(couleur.count.lign$Freq) - (couleur.count.lign$Freq/2)

par(lwd=3)
bpcouleur <- barplot(couleur.count.sorted$Freq, main="couleurs", xlim =c(0,4), ylim = c(0, 10), names.arg = couleur.count.sorted$Var1, col=couleur.count.sorted$color, width = 0.4, lwd=3, space=1.2)
text(x=bpcouleur, y=couleur.count.sorted$Freq + 0.25, label=couleur.count.sorted$Freq)
axis(1, at=bpcouleur, labels=couleur.count.sorted$Var1, lwd=3)
axis(4, at=(0:10), labels = seq(0,1,by=0.1), lwd=3)
par(lwd=1)

bpcouleurmatrix <- barplot(as.matrix(couleur.count.lign$Freq), main="couleur choisie", horiz=T, axes=F)
text(x=couleur.count.lign$center, y=1, label=couleur.count.lign$Var1)
text(x=couleur.count.lign$center, y=0.8, label=couleur.count.lign$Freq*10)


vec1 = c(2.8, 2.4, 2.1, 3.6, 2.8)
vec2 = c("rouge", "vert", "vert", "vert", "jaune")
vec2bis = as.factor(vec2)
vec3 =  c(T,T,F,F,F)
rep(4,3)
vec4 = rep(vec1,2)
vec5 = rep(vec1,c(2,1,3,3,2))
vec6 = 1:10
vec7 = seq(from = 3, to = 5,by = 0.2)
vec7bis = seq(from = 3, length =11, by = 0.2)

vec8 = numeric(3)
vec8[1] = 41
vec8[2] = -0.3
vec8[3] = 92
vec8
vec10 = logical()

mat1 =  matrix(vec4, ncol = 5)
dim(mat1) = c(2,5)

mat2 = matrix(vec4, ncol = 5, byrow = T)

mat3 = cbind(vec1,3:7)
mat1[,c(2,4,5)]
mat1[,c(F,T,F,T,T)]

mat3[c(1,4),]
mat3[c(T,F,F,T,F),]
mat1[,3]
dim(mat1)

list1 = list(vec1,c("rouge","bleu"), mat1)

list1 = list()
list1[[1]] = vec1
list1[[2]] = c("rouge","bleu")
list1[[3]] =  mat1

enquete = read.csv("enquete.csv", sep=" ")

compter = function(a,b) {
  d = numeric()

  for(i in 1:length(a)){
    d[i] = sum(b==a[i])
  }
  names(d) = as.character(a)
  return(d)
}
#---------------------------------------------------------------
setwd("~/0_MASTER_DOP/R/")
conso.dat = read.table("donneesconsommation.txt", header=T)
attach(conso.dat)
table(conso.dat$Cafe)
compter(levels(conso.dat$Cafe), conso.dat$Cafe)
table(conso.dat$Enfants)
table(conso.dat$Cafe, conso.dat$Enfants)

conso.list = list(conso.dat[3], conso.dat[4], conso.dat[5], conso.dat[6], conso.dat[7], compter(levels(conso.dat$Cafe), conso.dat$Cafe),
                                                                                            compter(unique(conso.dat$Enfants), conso.dat$Enfants),
                                                                                            compter(unique(conso.dat$Magnetoscope), conso.dat$Magnetoscope),
                                                                                            compter(unique(conso.dat$Lavevaisselle), conso.dat$Lavevaisselle), conso.dat[5])
conso.listbis = list(conso.dat[3], conso.dat[4], conso.dat[5], conso.dat[6], conso.dat[7])

 
conso.list2 = list(compter(levels(conso.dat$Age), conso.dat$Cafe))

conso.dat[conso.dat$Age == "TA1" & conso.dat$Region == "GR2", ]$Revenu

conso.dat %>% ggplot(aes(x=Enfants)) +
  geom_bar()
 

moy.revenu = tapply(conso.dat$Revenu, list(conso.dat$Age, conso.dat$Region), mean)
sd.revenu = tapply(conso.dat$Revenu, list(conso.dat$Age, conso.dat$Region), sd)

moy.consopate = tapply(conso.dat$Pates, list(conso.dat$Age, conso.dat$Region), mean)
sd.consopate = tapply(conso.dat$Pates, list(conso.dat$Age, conso.dat$Region), sd)

moy.loisirs = tapply(conso.dat$Loisirs, list(conso.dat$Age, conso.dat$Region), mean)
sd.loisirs = tapply(conso.dat$Loisirs, list(conso.dat$Age, conso.dat$Region), sd)

table(conso.dat$Magnetoscope, conso.dat$Lavevaisselle)

for (colname in colnames(conso.dat[1:7])) {
  print(colname)
  print(table(conso.dat$Magnetoscope, conso.dat$Lavevaisselle, conso.dat[[colname]]))
}


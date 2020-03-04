tension.dat = read.table("tension.csv", header = T, sep=",")
tension5 = tension.dat[1:5,]

barplottension=barplot(tension5$tension,
                       names.arg=1:5,
                       ylim=c(0,180), 
                       main="Représentation de la tension des 5 premiers individus",
                       sub="Diagramme en barres verticale")

text(barplottension,
     tension5$tension+10,
     tension5$tension)

tension5Sorted  = sort(tension5$tension)

barplottensionSorted=barplot(tension5Sorted,
                             names.arg=order(tension5$tension),
                             xlim=c(0,180),
                             horiz=TRUE,#Graphe a l'horizontale
                             main="Représentation de la tension des 5 premiers individus",#Titre du graphe
                             sub="Diagramme en barres verticale",#Sous titre
                             las=1,#Ecris la legende des ordonnées en lecture naturel
                             axis.lty=1)#Trait liant legende au graphe

text(tension5Sorted+10,
     barplottensionSorted,
     tension5Sorted)

pieTension=pie(tension5$tension,
               labels=tension5$tension,
               main="Représentation de la tension des 5 premiers individus",#Titre du graphe
               sub="Diagramme en secteur")


plotTension=plot(tension5$tension,c(1,1,1,1,1),
                 axes=FALSE,
                 xlab="tension",
                 ylab="",
                 xlim=c(120,160),
                 main="Représentation de la tension des 5 premiers individus",
                 sub="Représentation axiale")
axis(side=1, at=seq(120,160,10), labels=seq(120,160,10))
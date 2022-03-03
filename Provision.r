library(ChainLadder)
Paid <- matrix(c(200, 1900, 300, 850, 360, 500, 560, 270, 2800, 7700,7200, 3200, 4900, 3500, 5200, NA, 4200, 15800, 17000, 9300, 10300, 12000, NA, NA, 9500, 21000, 22000, 11600, 15200, NA, NA, NA, 10000, 23000, 24000, 12000, NA, NA, NA, NA, 12000, 24000, 24400, NA, NA, NA, NA, NA, 13000, 24500, NA, NA, NA, NA, NA, NA, 13500, NA, NA, NA, NA, NA, NA, NA), ncol = 8)
Paid


########################  Modele ChainLadder#########################
nc <- ncol(Paid)
nl <- nrow(Paid)

## Fonction pour estimer le facteur de développement

LAMBDA <- rep(NA, nc-1)
for(k in 1:(nc-1)){LAMBDA[k]=(sum(Paid[1:(nl-k),k+1])/sum(Paid[1:(nl-k),k]))}
LAMBDA

#####compléter le triange######

TRIANGLE <- Paid
for(i in 1:(nc-1)){TRIANGLE[(nl-i+1):(nl),i+1]=LAMBDA[i]*TRIANGLE[(nl-i+1):(nl),i]}
TRIANGLE

#####calcul des reserve#######

chargeultime <- TRIANGLE[,nc]
paiements <- diag(TRIANGLE[,nc:1])
RESERVESCL <- chargeultime-paiements
sum(RESERVESCL)


###################### Modele Mack  #######################################


TRIA <- Paid ## Triangle des paiements cumulés
TRIA

class(TRIA) <- "triangle"

cum2incr(TRIA) ## Triangles des paiements incrémentals

ChainLadder::plot(TRIA) ## Graphique du triangle des paiements cumulés

Modele <- MackChainLadder(TRIA) ## Ajustement du triangle

Modele$f ## Facteur de développement

Modele$FullTriangle ## Remplissage du traingle

plot(Modele) ## Graphique de la provision

summary(Modele) ## Sortie du modèle de Mack

#### la reserve totale est le montant: TOTALS IBNR = 40 641,65   ###
####### les facteurs de develloppement (juste pour comparer)###


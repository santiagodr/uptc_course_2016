library("ape")
library("geiger")
library("caper")
library("phytools")
library("MASS")
library("nlme")
library("nortest")
library("lsmeans")

###
setwd("~/Desktop/uptc_curso_2016/uptc_curso_2016/Analisis_Filogenetico") ###

read.csv("ManuElevation.csv", row.names=1, header=TRUE) -> data
ME.data <- data[,1:6]
row.names(ME.data)

read.csv("ManuElevation2.csv", row.names=1, header=TRUE) -> data2
ME.data2 <- data2[,1:6]
row.names(ME.data2)

read.nexus("MCC_Allspecies2014.tre") -> tree

name.check(tree, data2) -> extras
extras

## exclude from tree species without data
ME.tree <- drop.tip(tree,extras$tree_not_data)

name.check(ME.tree, ME.data)

plot(ME.tree, cex=0.7)
is.binary.tree(ME.tree)
is.ultrametric(ME.tree)

## to make tree and data in the same order
ME.data <- ME.data[match(ME.tree$tip.label,rownames(ME.data)),]

row.names(ME.data)
ME.tree$tip.label

names(ME.data)

log.mass <- ME.data[,1]
names(log.mass) <- ME.tree$tip.label
log.watts <- ME.data[,2]
names(log.watts) <- ME.tree$tip.label
altitude<-ME.data[,3]
elevation<-ME.data[,4]
orders<-ME.data[,5]
twoorders<-ME.data[,6]

######### log.mass calculations
MEdata.K <- phylosig(ME.tree, log.mass, method="K", nsim=999, test=TRUE)
MEdata.K

library(picante)
Kcalc(log.mass, ME.tree, checkdata=TRUE) 

ME.data.lambda <- phylosig(ME.tree, log.mass, method="lambda", test=TRUE)
ME.data.lambda

######### Log.watt calculations
MEdata_watts.K <- phylosig(ME.tree, log.watts, method="K", nsim=999, test=TRUE)
MEdata_watts.K

library(picante)
Kcalc(log.watts, ME.tree, checkdata=TRUE) 

ME.data_watts.lambda <- phylosig(ME.tree, log.watts, method="lambda", test=TRUE)
ME.data_watts.lambda

#### PIC

x=log.mass
y=log.watts
pic.x<-pic(x,ME.tree)#Correccion filogenetica para mass
write.csv(pic.x,file="picMass.csv")
pic.y<-pic(y,ME.tree)#Correccion filogenetica para best.bmr
write.csv(pic.y,file="picBMR.csv")

cor.test(pic.x,pic.y)#correlacion a ver si estas variables siguen asociadas despues de corregir por filogenia
plot(pic.x, pic.y)
plot(x,y)

colors<-c("red","green4","blue")
palette(colors)
par(mfrow=c(1,2), mar=c(4.5,4.5,1,2))
plot(log.mass,log.watts, cex=1, col=(ME.data[,4]), pch=19, ylab="BMR (ml O2/min)", xlab="Mass (g)")
legend(0.5,0.3, bty="n",c("400 m","1500 m","3000 m"), y.intersp=0.5,  pt.bg=colors, pch=21, horiz=F,  cex=0.9)
text(0.45,0.21, "a",  cex=2.3)


subset(ME.data, Elevation=="1")->low
subset(ME.data, Elevation=="2")->mid
subset(ME.data, Elevation=="3")->high
abline(lm(log.watts~log.mass), col="black")
abline(lm(low[,2]~low[,1]), col="red")
abline(lm(mid[,2]~mid[,1]), col="green4")
abline(lm(high[,2]~high[,1]), col="blue")

plot(pic.x, pic.y, cex=1, col="black", pch=19, ylab="PIC BMR", xlab="PIC Mass")
abline(lm(pic.x~pic.y))
text(-0.17,0.094, "b",  cex=2.3)
text(0.09,-0.095, "r^2 = 0.71", cex=0.9, pos=4)
text(0.09,-0.103, "p < 0.001", cex=0.9, pos=4)

### to double check species are correctly colored
par(mfrow=c(1,1))
text(x, y, tree$tip.label, cex=0.6, offset=0.4, pos=1)



### General Least Square (GLS)

fit<-gls(log.watts~log.mass+altitude)
fit
summary(fit)
anova(fit)
res=gls(log.watts~log.mass)
res
cor.test(log.watts,log.mass)
cor<-lm(log.watts~log.mass)
cor
red<-residuals(cor)
red
boxplot(red~altitude, ylab= "Residuals log.watts and log.mass", xlab="Latitude")

boxplot(log.watts~altitude, ylab="Basal Metabolic Rate (watts)", xlab="Elevation")

###Fitting a Brownian Motion model in Phylogenetic General Least Squares (PGLS)

pg.altitude<-corPagel(1,phy=ME.tree, fixed=FALSE)
pg.gls<-gls(log.watts ~ altitude*log.mass, correlation=pg.altitude, data=ME.data, method="REML")
summary(pg.gls)
anova (pg.gls)

bm.elevation<-corBrownian(1,phy=ME.tree) 
bm.gls<-gls(log.watts~altitude+log.mass,correlation=bm.elevation, data=ME.data)
ou.elevation<-corMartins(1, phy=ME.tree, fixed=TRUE)
ou.gls<-gls(log.watts~altitude*log.mass,correlation=ou.elevation, data=ME.data)

pg.elevation<-corPagel(1, phy=ME.tree, fixed=TRUE)
pg.gls<-gls(log.watts~altitude+log.mass,correlation=pg.elevation, data=ME.data)
summary(ou.gls)
anova(pg.gls)
anova(ou.gls,bm.gls,pg.gls)

anova(ou.gls)

##### Non phylogenetci PGLS

pg.gls<-gls(log.watts ~ altitude*log.mass, data=ME.data)
summary(pg.gls)
anova (pg.gls)
### Elevation subsets

subset(ME.data, altitude=="Low")->bajo
subset(ME.data, altitude=="Mid")->medio
subset(ME.data, altitude=="High")->alto


#####Lowland data only

name.check(ME.tree, bajo) -> lowtree
lowtree
drop.tip(ME.tree, lowtree$tree_not_data)->bajotree
plot(bajotree)
name.check(bajotree, bajo)

plot(bajotree, cex=0.7)
is.binary.tree(bajotree)
is.ultrametric(bajotree)

## to make tree and data in the same order
ME.data <- ME.data[match(bajotree$tip.label,rownames(ME.data)),]

row.names(ME.data)
bajotree$tip.label

names(ME.data)

bajo[,2]->logmass.bajo
bajo[,3]->logwatts.bajo
bajo[,4]->mass.bajo
bajo[,5]->watts.bajo

names.bajo<-rownames(bajo)
names(logmass.bajo)<-names.bajo
names(logwatts.bajo)<-names.bajo

bm.elevation<-corBrownian(1,phy=bajotree) 
bm.gls<-gls(log.watts~log.mass,correlation=bm.elevation, data=bajo)
ou.elevation<-corMartins(1, phy=bajotree, fixed=TRUE)
ou.gls<-gls(log.watts~log.mass,correlation=ou.elevation, data=bajo)
pg.elevation<-corPagel(1, phy=bajotree, fixed=TRUE)
pg.gls<-gls(log.watts~log.mass,correlation=pg.elevation, data=bajo)
summary(ou.gls)
anova(ou.gls)
anova(ou.gls,bm.gls,pg.gls)

#####Cloud Forest data only

name.check(ME.tree, medio) -> midtree
midtree
drop.tip(ME.tree, midtree$tree_not_data)->mediotree
plot(mediotree)
name.check(mediotree, medio)

plot(mediotree, cex=0.7)
is.binary.tree(mediotree)
is.ultrametric(mediotree)

## to make tree and data in the same order
medio <- medio[match(mediotree$tip.label,rownames(medio)),]

row.names(medio)
mediotree$tip.label

names(medio)

medio[,2]->logmass.medio
medio[,3]->logwatts.medio
medio [,4]->mass.medio
medio [,5]->watts.medio

names.medio<-rownames(medio)
names(logmass.medio)<-names.medio
names(logwatts.medio)<-names.medio

bm.elevation<-corBrownian(1,phy=mediotree) 
bm.gls<-gls(log.watts~log.mass,correlation=bm.elevation, data=medio)
ou.elevation<-corMartins(1, phy=mediotree, fixed=TRUE)
ou.gls<-gls(log.watts~log.mass,correlation=ou.elevation, data=medio)
pg.elevation<-corPagel(1, phy=mediotree, fixed=TRUE)
pg.gls<-gls(log.watts~log.mass,correlation=pg.elevation, data=medio)
summary(ou.gls)
anova(ou.gls)
anova(ou.gls,bm.gls,pg.gls)

##### High Elevation data only

name.check(ME.tree, alto) -> hightree
hightree
drop.tip(ME.tree, hightree$tree_not_data)->altotree
plot(altotree)
name.check(altotree, medio)

plot(altotree, cex=0.7)
is.binary.tree(altotree)
is.ultrametric(altotree)

## to make tree and data in the same order
alto <- alto[match(altotree$tip.label,rownames(alto)),]

row.names(medio)
altotree$tip.label

names(alto)

alto [,2]->logmass.alto
alto [,3]->logwatts.alto
alto [,4]->mass.alto
alto [,5]->watts.alto

names.alto<-rownames(alto)
names(logmass.alto)<-names.alto
names(logwatts.alto)<-names.alto

bm.elevation<-corBrownian(1,phy=altotree) 
bm.gls<-gls(log.watts~log.mass,correlation=bm.elevation, data=alto)
ou.elevation<-corMartins(1, phy=altotree, fixed=TRUE)
ou.gls<-gls(log.watts~log.mass,correlation=ou.elevation, data=alto)
pg.elevation<-corPagel(1, phy=altotree, fixed=TRUE)
pg.gls<-gls(log.watts~log.mass,correlation=pg.elevation, data=alto)
summary(ou.gls)
anova(ou.gls)
anova(ou.gls,bm.gls,pg.gls)

### Testing for normality with phylogentic data
# now simulate data under an arbitrary ANCOVA model
# the same principle applies to regression or ANOVA

x1<-log.watts #response variable
x2<-log.mass#continous covariate
e<-fastBM(ME.tree)
y<-2*as.numeric(x1)+0.75*x2+e

# is y normal? (should fail)
lillie.test(y)

# is x2 normal? (should fail)
lillie.test(x2)

# fit the model
fit<-gls(y~x1+x2,data=data.frame(x1,x2,y), correlation=corBrownian(1,ME.tree))
fit

# are the residuals normal? (should fail)
lillie.test(residuals(fit))
# are the residuals controlling for phylogeny normal?
# (should pass)
lillie.test(chol(solve(vcv(ME.tree)))%*%residuals(fit))
residuals(fit)


###Resultados de los 500 arboles#####

read.csv("ME.PICcorr.csv", row.names=1, header=TRUE) -> PICdata
summary(PICdata)
rho<-PICdata[,1]
p.value<-PICdata[,2]
sd(rho)
mean(rho)
sd(p.value)
mean(p.value)

read.csv("ME.phylosigLambda.csv", row.names=1, header=TRUE) -> lambdadata
summary(lambdadata)
lambdamass<-lambdadata[,1]
lambdamassp.value<-lambdadata[,2]
sd(lambdamass)
mean(lambdamass)
sd(lambdamassp.value)
mean(lambdamassp.value)
lambdawatts<-lambdadata[,3]
lambdawatts.value<-lambdadata[,4]
sd(lambdawatts)
mean(lambdawatts)
sd(lambdawatts.value)
mean(lambdawatts.value)
confint(lambdawatts)



##### Empiece aqui para calcular cada uno de los valores con cada uno de los 5000 arboles. Necesita los mismos programas que la funcion que escribio arriba. ##########
library("ape")
library("geiger")
library("caper")
library("phytools")

multitree<-read.nexus(file.choose())
data<-read.csv(file.choose(),row.names=1,h=T)
ME.data<-data[,6:9]
k<-vector()
p.k<-vector()
lambda<-vector()
p.lambda<-vector()
k.watts<-vector()
p.k.watts<-vector()
lambda.watts<-vector()
p.lambda.watts<-vector()
p.value<-vector()
rho<-vector()
for (i in 1:length(multitree)){
	tree<-multitree[[i]]
	treedata(tree, data) -> extras
	## exclude from tree species without data
		ME.tree <- extras$phy

		## to make tree and data in the same order
		ME.data <- ME.data[match(ME.tree$tip.label,rownames(ME.data)),]
		log.mass <- ME.data[,1]
		names(log.mass) <- ME.tree$tip.label
		log.watts <- ME.data[,2]
		names(log.watts) <- ME.tree$tip.label
######### log.mass calculations
MEdata.K<- phylosig(ME.tree, log.mass, method="K", nsim=999, test=TRUE)
k[i]<-MEdata.K$K
p.k[i]<-MEdata.K$P
ME.data.lambda <- phylosig(ME.tree, log.mass, method="lambda", test=TRUE)
lambda[i]<-ME.data.lambda$lambda
p.lambda[i]<-ME.data.lambda$P
######### Log.watt calculations
MEdata_watts.K<- phylosig(ME.tree, log.watts, method="K", nsim=999, test=TRUE)
k.watts[i]<-MEdata_watts.K$K
p.k.watts[i]<-MEdata_watts.K$P
ME.data_watts.lambda <- phylosig(ME.tree, log.watts, method="lambda", test=TRUE)
lambda.watts[i]<-ME.data_watts.lambda$lambda
p.lambda.watts<-ME.data_watts.lambda$P

#### PIC

x=log.mass
y=log.watts
pic.x<-pic(x,ME.tree)#Correccion filogenetica para mass
pic.y<-pic(y,ME.tree)#Correccion filogenetica para best.bmr
correlation<-cor.test(pic.x,pic.y) #correlacion a ver si estas variables siguen asociadas despues de corregir por filogenia
p.value[i]<-correlation$p.value
rho[i]<-correlation$estimate

}

K<-cbind(k,p.k,k.watts,p.k.watts)
summary (K)
K
lambda<-cbind(lambda,p.lambda,lambda.watts,p.lambda.watts)
lambda
summary(lambda)
corr<-cbind(rho,p.value)
corr
summary(corr)
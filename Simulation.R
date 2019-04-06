


#####################################
#                                   #
#         Simulation Study          #
#                                   #
#####################################


rm(list=ls())


##### Libraries

library(nlme)
library(lme4)
library(MASS)
library(sae)
library(readr)


##### Load data and define objects

setwd("/Users/manugaco/Desktop/Thesis")

datosMCS <- read_csv("Data/datosMCSom.csv", col_names = T)
datosCenso <-  read_csv("Data/datosCensoom.csv", col_names = T)


##### Check outliers and NAs


# We remove data from municipality number 8 (outlying municipality) #Because after adjusting the model, it was outlier

datosMCSom<-datosMCS[datosMCS$mun!=8,]
datosCensoom<-datosCenso[datosCenso$mun!=8,]

attach(datosMCSom) #name columns
municipio <- (datosMCSom$mun) #create vector

##### Setting size bojects

n <- dim(datosMCSom)[1];n

# Population size (estimation)

N <- sum(datosCensoom$factor);N #estimate with w_i factor weights Horbit_Thomsom estimator

N/dim(datosCensoom)[1]
# The available Census is a sample of 1 out of 20 on average, promedio del factor de elevacion del censo


# Num. of municipalities, sample and population sizes of municipalities

muns_uni <- unique(municipio) # vector of unique municipality indicators (codes), sorted from smaller to larger by they are NOT correlative
D <- length(muns_uni)

nd<-rep(0,D)  #sample size
Nd<-rep(0,D)  #population size
ndc<-rep(0,D)  #census size

for (i in 1:D){
  
  d <- muns_uni[i]
  nd[i] <- sum(datosMCSom$mun==d)
  Nd[i] <- round(sum(datosCensoom$factor[datosCensoom$mun==d]))
  ndc[i] <- sum(datosCensoom$mun==d)
  
}

##### Preparing the variables for the model (sample and census)

#Hide
#-------------------------

genero2 <- datosMCSom$sexo
genero <- factor(genero2,labels=c("mujer", "hombre")) #Declara factor con etiquetas

pob_indigena2 <- datosMCSom$pob_ind
pob_indigena <- factor(pob_indigena2,labels=c("No","Si"))

sector_actividad2 <- datosMCSom$sector_act
sector_actividad <- factor(sector_actividad2,labels=c("1","2","3","4","5","6"))

colnames(datosMCSom)[15] <- "años_escolaridad"
years_study <- datosMCSom$años_escolaridad

age <- datosMCSom$edad

clase_hogar2 <- datosMCSom$clase_hog
clase_hogar <- clase_hogar2
clase_hogar[clase_hogar2>3] <- 3 # We add code 3 to households with more than 3 members
clase_hogar <- factor(clase_hogar,labels=c("1","2","3"))

calidad_vivienda2 <- datosMCSom$est_calidad_vivienda
calidad_vivienda <- factor(calidad_vivienda2,labels=c("1","2","3","4"))

remesas_f2 <- datosMCSom$remesas
remesas_f <- factor(remesas_f2,labels=c("si","no"))

ben_gob2 <- datosMCSom$bengob
ben_gob <- factor(ben_gob2,labels=c("si","no"))

bienes_casa2 <- datosMCSom$bienes
bienes_casa <- bienes_casa2
bienes_casa[bienes_casa2 < 5] <- 1
bienes_casa[(bienes_casa2>= 5) & (bienes_casa2 < 9)] <- 2
bienes_casa[(bienes_casa2>=9) & (bienes_casa2<13)] <- 3
bienes_casa[(bienes_casa2>=13) & (bienes_casa2<=16)] <- 4
bienes_casa3 <- factor(bienes_casa,labels=c("1","2","3","4"))

#Change column name

colnames(datosMCSom)[15] <- "años_escolaridad"

escuela2 <- datosMCSom$años_escolaridad
escuela <- escuela2
escuela[escuela2 < 6] <- 1
escuela[(escuela2 >= 6) & (escuela2 < 11)] <- 2
escuela[(escuela2 >= 11) & (escuela2 < 18)] <- 3
escuela[(escuela2 >= 18)] <- 4
escuela3 <- factor(escuela,labels=c("1","2","3","4"))

rur_urb2 <- datosMCSom$rururb
rur_urb <- factor(rur_urb2,labels=c("urb","rur"))

###### Now for the Census

genero2c <- datosCensoom$sexo
generoc <- factor(genero2c,labels=c("mujer", "hombre")) #Declara factor con etiquetas

pob_indigena2c<-datosCensoom$pob_ind
pob_indigenac<-factor(pob_indigena2c,labels=c("No","Si"))

sector_actividad2c<-datosCensoom$sector_act
sector_actividadc<-factor(sector_actividad2c,labels=c("1","2","3","4","5","6"))

# years_study<-datosMCSom.u2$a?os_escolaridad

age2c<-datosCensoom$edad
agec<-age2c

clase_hogar2c<-datosCensoom$clase_hog
clase_hogarc<-clase_hogar2c
clase_hogarc[clase_hogar2c>3]<-3
clase_hogarc<-factor(clase_hogarc,labels=c("1","2","3"))

calidad_vivienda2c<-datosCensoom$est_calidad_vivienda
calidad_viviendac<-factor(calidad_vivienda2c,labels=c("1","2","3","4"))

remesas_f2c<-datosCensoom$remesas
remesas_fc<-factor(remesas_f2c,labels=c("si","no"))

ben_gob2c<-datosCensoom$bengob
ben_gobc<-factor(ben_gob2c,labels=c("si","no"))

bienes_casa2c<-datosCensoom$bienes
bienes_casac<-bienes_casa2c
bienes_casac[bienes_casa2c < 5]<-1
bienes_casac[(bienes_casa2c>= 5) & (bienes_casa2c < 9)]<-2
bienes_casac[(bienes_casa2c>=9) & (bienes_casa2c<13)]<-3
bienes_casac[(bienes_casa2c>=13) & (bienes_casa2c<=16)]<-4
bienes_casa3c<-factor(bienes_casac,labels=c("1","2","3","4"))

#Cambiar variable (años)

colnames(datosCensoom)[16] <- "años_escolaridad"

escuela2c<-datosCensoom$años_escolaridad
escuelac<-escuela2c
escuelac[escuela2c < 6]<-1
escuelac[(escuela2c>= 6) & (escuela2c < 11)]<-2
escuelac[(escuela2c>=11) & (escuela2c<18)]<-3
escuelac[(escuela2c>=18)]<-4
escuela3c<-factor(escuelac,labels=c("1","2","3","4"))

rur_urb2c<-datosCensoom$rururb
rur_urbc<-factor(rur_urb2c,labels=c("urb","rur"))

#-------------------------

##### Study of the income variable

ictpc <- datosMCSom$ictpc

summary(ictpc)

sum(ictpc == 0)

# There are 18 individuals in the sample with zero income

# Poverty line

z <- 0.6*median(ictpc) #This is measurement used more in Europe

# Transform the income variable

k <- 170 #Asymetric constant
m <- abs(min(ictpc)) + k
ictpct <- ictpc + m

hist(ictpc,freq=FALSE,main="")

hist(log(ictpct),freq=FALSE,main="")


##### SIMULATION

##### Step 1 FIT MODEL

ys <- log(ictpct) #Response, transformed

age2 <- age
age2_2 <- age2^2
age2_3 <- age2^3

fit <- lmer(ys~(1|as.factor(municipio)) +
           age2 +
           age2_2 +
           age2_3 +
           pob_indigena +
           sector_actividad +
           clase_hogar +
           calidad_vivienda +
           ben_gob +
           bienes_casa3 +
           escuela3 +
           calidad_vivienda*rur_urb +
           clase_hogar*genero, REML=T)

#summary(fit)

# Remove non significant variables

fit2 <- lmer(ys~(1|as.factor(municipio))+
               age2+
               age2_2+
               age2_3+
               pob_indigena+
               sector_actividad+
               clase_hogar+
               calidad_vivienda+
               ben_gob+
               bienes_casa3+
               escuela3+
               clase_hogar*genero,REML=T)

#RE-SCALING??

Xs <- model.matrix(fit2) #Estimates Xp
p <- dim(Xs)[2]
betaest <- fixed.effects(fit2)
upred <- random.effects(fit2)   #EBLUP
sigmae2est <- summary(fit2)$sigma^2
sigmau2est <- sqrt(as.numeric(VarCorr(fit2)))

#Check the model

res <- residuals(fit2)
fitted <- fitted(fit2)

# Check normality by histogram and normality qq-plot

hist(res,prob=T,breaks=30,main="",xlab="Residuals",xlim=c(-3,3))
x<-seq(-3,3,by=0.1)
lines(x,dnorm(x,mean(res),sd(res)))

xx <- qqnorm(res,main="",ylim=c(-4,4),xlim=c(-4,4))
abline(lm(xx$y~xx$x))

# The distribution has slightly heavier tails than the normal
# but the departure doesn't seem too bad. 
# I think that the most important aspect is symmetry

# Pearson's asymmetry coefficient

mean(res^3)/sd(res)^3

# The distribution is approximatelly symmetric.

# Check linearity

plot(fitted,res,xlab="Fitted values",ylab="Residuals")

# It looks all right, the points that appear aligned are those 
# for the same income values (there are repetitions).

##### Step 7 REPEAT steps 2-6 and compute MSE

#S <- 1000
#sims_Y <- list()
#sims_y_bar <- list()
#sims_f_0 <- list()
#sims_dat_out <- list()
#sims_mods <- list()

#for(k in 1:S){

##### Step 2 GENERATE CENSUS Y_p and compute true values of Y_hat and F_o

#Declare variables
Y <- numeric()
y_bar <- numeric()
f_0 <- numeric()

#Loop for generate the census
for(i in 1:D){
  
  print(paste("Municipality", i, sep = " ")) #indicate step
  d <- muns_uni[i]
  
  Xd <- Xs[datosMCSom$mun==d,] #subset predictors for each municipality
  mean_d <- as.matrix(Xd)%*%matrix(betaest,nr=p,nc=1) #compute the mean matrix
  e_d <- rnorm(nd[i], 0, sqrt(sigmae2est)) #Sample error
  v_d <- rnorm(1, 0, sqrt(sigmau2est)) #Sample random effects
  
  y_d <- mean_d + v_d + e_d #compute simulated response on a given area
  Y[municipio == d] <- y_d #store of the simulation results for each area
  
  y_bar[i] <- sum(y_d)/nrow(Xd) #compute mean of the response for each area
  f_0[i] <- sum(exp(y_d) < z)/nrow(Xd) #compute the poverty rate
  
}

#Store on each iteration
#sims_Y[k] <- Y
#sims_y_bar[k] <- y_bar
#sims_f_o[k] <- f_0

summary(Y)
y_bar
f_0

##### Step 3 SRSWOR municipalities

#Declare variables

dat_in <- data.frame(ys, municipio, age2, age2_2, age2_3, ben_gob, bienes_casa3, clase_hogar, calidad_vivienda, 
                     escuela3, genero, pob_indigena, remesas_f, rur_urb, sector_actividad)

#Loop for create the sampled data
for(i in 1:D){
  
  d <- muns_uni[i] #index of municipality
  dat_mun <- dat_in[dat_in$municipio==d,] #subset each municipality
  colnames(dat_mun)[2] <- "municipio" #Rename column
  mun <- length(which(datosMCSom$mun == d)) #Number of individuals per municipality
  dat_s <- dat_mun[sample(nrow(dat_mun), sample(1:mun, 1), replace = F),] #Sample of individuals on each municipality
  
  #Creating a new dataframe with the sample (used for fit new models)
  if(i == 1){
  dat_out <- dat_s
  }else{
  dat_out <- rbind(dat_out, dat_s)
  }
  
}

#Store on each iteration
#sims_dat_out[k] <- dat_out

##### Step 4 FIT MODELS TO SAMPLE Y


#---------------

# Less Biased model without the non-significat interaction

mod_1 <- lmer(ys ~ (1 | municipio) +
          age2 +
          age2_2 +
          age2_3 +
          ben_gob +
          bienes_casa3 +
          clase_hogar +
          calidad_vivienda +
          escuela3 +
          pob_indigena +
          sector_actividad +
          clase_hogar*genero, data = dat_out, REML=T)

#Define some models adding or deleting variables
  
#Remove calidad_vivienda
mod_2 <- lmer(data = dat_out, ys ~ (1 | municipio) +
                age2 +
                age2_2 +
                age2_3 +
                ben_gob +
                bienes_casa3 +
                clase_hogar +
                escuela3 +
                pob_indigena +
                sector_actividad +
                clase_hogar*genero, REML=T)

#Remove sector_actividad
mod_3 <- lmer(data = dat_out, ys ~ (1| municipio) +
                age2 +
                age2_2 +
                age2_3 +
                ben_gob +
                bienes_casa3 +
                clase_hogar +
                escuela3 +
                pob_indigena +
                clase_hogar*genero, REML=T)

#Add rur_urb
mod_4 <- lmer(data = dat_out, ys ~ (1| municipio) +
                age2 +
                age2_2 +
                age2_3 +
                ben_gob +
                bienes_casa3 +
                clase_hogar +
                calidad_vivienda +
                escuela3 +
                pob_indigena +
                sector_actividad +
                rur_urb +
                clase_hogar*genero, REML=T)

#Add calidad_vivienda*rur_urb
mod_5 <- lmer(data = dat_out, ys ~ (1| municipio) +
                age2 +
                age2_2 +
                age2_3 +
                ben_gob +
                bienes_casa3 +
                clase_hogar +
                calidad_vivienda +
                escuela3 +
                pob_indigena +
                sector_actividad +
                calidad_vivienda*rur_urb +
                clase_hogar*genero, REML=T)

#Store each model, each iteration
#sims_mods[k] <- c(mod_1, mod_2, mod_3, mod_4, mod_5)

#---------------

##### Step 5 GOODNESS OF FIT MEASURES

#Define variables
k <- numeric()
nummod <- 5

for(i in 1:nummod){
  assign(mod, get(paste("mod_", i, sep = ""))) #goodness of fit for each model
  
  for(i in 1:D){
    d <- muns_uni[i]

    #Define parameters of each model and area
    Xs <- model.matrix(paste(mod, i, sep = "_")) #Estimates Xp for each model and area
    Xd <- Xs[datosMCSom$mun==d,]
    p <- dim(Xd)[2] #dimensions of each model
    betaest <- fixed.effects(mod) #beta estimates for each model
    upred <- random.effects(mod)   #EBLUP for each model
    sigmae2est <- summary(mod)$sigma^2 #error estimates for each model
    sigmau2est <- sqrt(as.numeric(VarCorr(mod))) #random effects estimates for each model
    gammadi <- sigmau2est/(sigmau2est+sigmae2est/nd[i])
    k <- sigmae2est/sigmau2est
    
    
    
  }
}





#} #End of simulation



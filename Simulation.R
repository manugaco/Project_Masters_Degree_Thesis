


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
library(psych)
library(matlib)
library(tidyverse)


##### Load data and define objects

setwd("/Users/manugaco/Desktop/Thesis")

datosMCS <- read_csv("Data/datosMCSom.csv", col_names = T)
#datosCenso <-  read_csv("Data/datosCensoom.csv", col_names = T)


##### Check outliers and NAs


# We remove data from municipality number 8 (outlying municipality) 
#Because after adjusting the model, it was outlier

datosMCSom<-datosMCS[datosMCS$mun!=8,]
#datosCensoom<-datosCenso[datosCenso$mun!=8,]

attach(datosMCSom) #name columns
municipio <- (datosMCSom$mun) #create vector

##### Setting size bojects

N <- dim(datosMCSom)[1];N

# Population size (estimation)

#N <- sum(datosCensoom$factor);N #estimate with w_i factor weights Horbit_Thomsom estimator

#N/dim(datosCensoom)[1]
# The available Census is a sample of 1 out of 20 on average, 
# promedio del factor de elevacion del censo


# Num. of municipalities, sample and population sizes of municipalities

muns_uni <- unique(municipio) # vector of unique municipality indicators (codes), 
#sorted from smaller to larger by they are NOT correlative
D <- length(muns_uni)

#nd <- rep(0,D)  #sample size
Nd <- rep(0,D)  #population size
#ndc<-rep(0,D)  #census size

for (i in 1:D){
  
  d <- muns_uni[i]
  Nd[i] <- sum(datosMCSom$mun == d)
  #Nd[i] <- round(sum(datosCensoom$factor[datosCensoom$mun == d]))
  #ndc[i] <- sum(datosCensoom$mun==d)
}
N <- sum(Nd)
N
nd <- round(Nd/3)
nd
n <- sum(nd)
n

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

#-------------------------

##### Study of the income variable

ictpc <- datosMCSom$ictpc

summary(ictpc)

sum(ictpc == 0)

# There are 18 individuals in the sample with zero income

# Poverty line

z <- 1225 #This is measurement used more in Europe

# Transform the income variable

k <- 170 #Asymetric constant
m <- abs(min(ictpc)) + k
ictpct <- ictpc + m

hist(ictpc,freq=FALSE,main="")

hist(log(ictpct),freq=FALSE,main="")


##### SIMULATION

##### Step 1 FIT MODEL

y <- log(ictpct) #Response, transformed

age2 <- age
age2_2 <- age2^2
age2_3 <- age2^3

fit <- lmer(y ~ (1|as.factor(municipio)) +
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


# Remove non significant variables

fit2 <- lmer(y ~ (1|as.factor(municipio)) +
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

X <- model.matrix(fit2) #Estimates Xp
p <- dim(X)[2]
betaest <- fixed.effects(fit2)
upred <- random.effects(fit2)   #EBLUP
sigmae2est <- summary(fit2)$sigma^2
sigmau2est <- sqrt(as.numeric(VarCorr(fit2)))

#Check the model

res <- residuals(fit2)
fitted <- fitted(fit2)

# Check normality by histogram and normality qq-plot

hist(res,prob=T,breaks=30,main="",xlab="Residuals",xlim=c(-3,3))
x <- seq(-3,3,by=0.1)
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

ini <- Sys.time()

S <- 1000

#sims_Y <- list()
#sims_y_bar <- list()
#sims_f_0 <- list()
#sims_dat_out <- list()
#sims_mods <- list()
#sims <- list()

#Input function for eb function
pov_inc <- function(y){
  z <- 1225
  result <- mean(y < z) 
}

#for(k in 1:S){

##### Step 2 GENERATE CENSUS Y_p and compute true values of Y_hat and F_o

#Declare variables
Y <- numeric()
y_bar <- numeric()
f_0 <- numeric()

#Loop for generate the census
for(i in 1:D){
  
  print(paste("Municipality", i, sep = " ")) #Indicate step
  d <- muns_uni[i]
  
  Xd <- X[datosMCSom$mun==d,] #Subset predictors for each municipality
  mean_d <- as.matrix(Xd)%*%matrix(betaest,nr=p,nc=1) #Compute the mean matrix
  e_d <- rnorm(nd[i], 0, sqrt(sigmae2est)) #Sample error
  v_d <- rnorm(1, 0, sqrt(sigmau2est)) #Sample random effects
  
  y_d <- mean_d + v_d + e_d #Compute simulated response on a given area
  Y[municipio == d] <- y_d #Store of the simulation results for each area (Census)
  y_bar[i] <- sum(y_d)/Nd[i] #Compute mean of the response for each area
  f_0[i] <- sum(exp(y_d) - m < z)/Nd[i] #Compute the poverty rate
  
}

#median(exp(Y) - m) * 0.6

#Store on each iteration
#sims_Y[k] <- Y
#sims_y_bar[k] <- y_bar
#sims_f_0[k] <- f_0

Y
y_bar
f_0

##### Step 3 SRSWOR municipalities

#Declare variables

dat_pop <- data.frame(Y, municipio, age2, age2_2, age2_3, ben_gob, bienes_casa3, clase_hogar, 
                      calidad_vivienda, escuela3, genero, pob_indigena, remesas_f, rur_urb, 
                      sector_actividad)

#Loop for create the sampled data
sum_Nd <- 0
ind_v <- NULL

#if(k = 1){
  for(i in 1:D){
    d <- muns_uni[i]
    ind_s <- sample((sum_Nd+1):(sum_Nd+Nd[i]), nd[i], replace = F)
    ind_v <- c(ind_v, ind_s)
    sum_Nd <- sum_Nd + Nd[i]
  }
#}

data_s <- dat_pop[ind_v,]
mun <- data_s$municipio

Xs <- X[ind_v, ]
Xr <- X[-ind_v, ]

Xin <- data.frame(cbind(municipio = municipio[ind_v], Xs[,-1]))

data_means <- Xin %>% group_by(municipio) %>% summarise_all("mean")
data_ind <- Xin %>% group_by(municipio) %>% summarize(area = n())

Xnon <- data.frame(cbind(municipio = municipio[-ind_v], Xr[,-1]))

#Store on each iteration
#sims_dat_out[k] <- dat_out

##### Step 4 FIT MODELS TO SAMPLE Y

#---------------

# Less Biased model without the non-significat interaction

form <- list()
mod <- list()

form[[1]] <- as.formula(Y ~ (1 | municipio) +
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
                     clase_hogar:genero)

mod[[1]] <- lmer(form[[1]], data = data_s, REML=T)

#Define some models adding or deleting variables
  
#Remove calidad_vivienda

form[[2]] <- update(form[[1]], .~. -calidad_vivienda)
mod[[2]] <- lmer(form[[2]], data = data_s, REML=T)

#Remove sector_actividad

form[[3]] <- update(form[[1]], .~. -sector_actividad)
mod[[3]] <- lmer(form[[3]], data = data_s, REML=T)

#Add rur_urb

form[[4]] <- update(form[[1]], .~. +rur_urb)
mod[[4]] <-lmer(form[[4]], data = data_s, REML=T)

#Add calidad_vivienda*rur_urb

form[[5]] <- update(form[[1]], .~. +calidad_vivienda*rur_urb)
mod[[5]] <- lmer(form[[5]], data = data_s, REML=T)

#Store each model, each iteration
#sims_mods[k] <- c(mod_1, mod_2, mod_3, mod_4, mod_5)

#---------------

##### Step 5 GOODNESS OF FIT MEASURES

#Define variables

k <- numeric()
Qfm <- list()
eblup <-list()
eb <- list()
nummod <- 5
alpha1 <- c() #First penalty term
alpha2 <- c()#Proposed penalty term

for(i in 1:nummod){
  
    Qf <- numeric()
    #Define parameters of each model and area
    
    Xs_s <- model.matrix(mod[[i]]) #Estimates Xp for each model
    ps_s <- dim(Xs_s)[2] #dimensions of each model
    betaest_s <- fixed.effects(mod[[i]]) #beta estimates for each model
    upred_s <- random.effects(mod[[i]])$municipio[,1]   #EBLUP for each model
    sigmae2est_s <- summary(mod[[i]])$sigma^2 #error estimates for each model
    sigmau2est_s <- sqrt(as.numeric(VarCorr(mod[[i]]))) #random effects estimates for each model
    
    #sigma_s <- matrix(c(sigmae2est_s, 0, 0, sigmau2est_s), ncol = 2)
    #compute gof (propsed loss function)
    
    
    for(j in 1:D){
      Q <- 0
      d <- muns_uni[j]
      Xd <- Xs_s[data_s$municipio == d,] #Subset predictors for each municipality
      mean_d <- as.matrix(Xd)%*%matrix(betaest, nr=ps_s, nc=1)
      Yd <- data_s$Y[data_s$municipio == d]
      Q <- Q + sum(((Yd - mean_d - upred_s[j])^2))/sigmae2est + sum(upred_s[j]^2)/sigmau2est
      Qf[j] <- Q + n*log(sigmae2est) + D*log(sigmau2est)
    }
    
    Qfm[[i]] <- Qf #absolute value?
    
    ##Step 6 Compute eblup and eb
    
    # eblup[[i]] <- eblupBHF(formula = form[[i]], dom = municipio, meanxpop = data_means, 
    #                      popnsize = data_ind, data = data_s)$eblup$eblup
    # 
    # eb[[i]] <- ebBHF(formula = form[[i]], dom = municipio, Xnonsample = Xnon, constant = m,
    #               indicator = pov_inc, data = data_s)$eb$eb
  }

#par(mfrow = c(1,2))

plot(Qfm[[1]], type = "l", col = "red", ylim = c(-5000, 20000))
lines(Qfm[[2]], type = "l", col = "blue")
lines(Qfm[[3]], type = "l", col = "green") #best model
lines(Qfm[[4]], type = "l", col = "black")
lines(Qfm[[5]], type = "l", col = "purple")

par(mfrow = c(2,3))
for(i in 1:5){
  plot(Qfm[[i]], type = "l")
}

# plot(Qfm[[4]], type = "l", col = "blue", ylim = c(0, 40000))
# lines(eblup[[1]], type = "l", col = "red")

#sims_Y <- list()
#sims_y_bar <- list()
#sims_f_0 <- list()
#sims_dat_out <- list()
#sims_mods <- list()
#sims[k] <- c(sims_Y, sims_y_bar, sims_)

#} #End of simulation

Sys.time() - ini #1.13s 1 loop

which.min(results[which.min(results),])

results <- t(rbind(Qf, eblup, eb))
colnames(results) <- c("NP", "EBLUP", "EB")
rownames(results) <- c("mod1", "mod2", "mod3", "mod4", "mod5")

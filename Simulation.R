


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

#N/dim(datosCensoom)[1]
# The available Census is a sample of 1 out of 20 on average, promedio del factor de elevacion del censo


# Num. of municipalities, sample and population sizes of municipalities

muns_uni <- unique(municipio) # vector of unique municipality indicators (codes), sorted from smaller to larger by they are NOT correlative
D <- length(muns_uni)

nd <- rep(0,D)  #sample size
Nd <- rep(0,D)  #population size
#ndc<-rep(0,D)  #census size

for (i in 1:D){
  
  d <- muns_uni[i]
  nd[i] <- sum(datosMCSom$mun == d)
  Nd[i] <- round(sum(datosCensoom$factor[datosCensoom$mun == d]))
  #ndc[i] <- sum(datosCensoom$mun==d)
  
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

#summary(fit)

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
#sims <- list()

#for(k in 1:S){

##### Step 2 GENERATE CENSUS Y_p and compute true values of Y_hat and F_o

#Declare variables
Y <- numeric()
y_bar <- numeric()
f_0 <- numeric()
s_pop <- numeric()

#Loop for generate the census
for(i in 1:D){
  
  print(paste("Municipality", i, sep = " ")) #Indicate step
  d <- muns_uni[i]
  
  Xd <- Xs[datosMCSom$mun==d,] #Subset predictors for each municipality
  mean_d <- as.matrix(Xd)%*%matrix(betaest,nr=p,nc=1) #Compute the mean matrix
  e_d <- rnorm(nd[i], 0, sqrt(sigmae2est)) #Sample error
  v_d <- rnorm(1, 0, sqrt(sigmau2est)) #Sample random effects
  
  y_d <- mean_d + v_d + e_d #Compute simulated response on a given area
  Y[municipio == d] <- y_d #Store of the simulation results for each area (Census)
  s_pop[i] <- nrow(Xd) 
  y_bar[i] <- sum(y_d)/s_pop[i] #Compute mean of the response for each area
  f_0[i] <- sum(exp(y_d) - m < z)/s_pop[i] #Compute the poverty rate
  
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

dat_in <- data.frame(Y, municipio, age2, age2_2, age2_3, ben_gob, bienes_casa3, clase_hogar, calidad_vivienda, 
                     escuela3, genero, pob_indigena, remesas_f, rur_urb, sector_actividad)

#Loop for create the sampled data

n_s <- round(s_pop/3)
ind_s <- list()

for(i in 1:D){
  d <- muns_uni[i]
  dat_mun <- dat_in[dat_in$municipio==d,]
  ind_s[[i]] <- sample(nrow(dat_mun), n_s[i], replace = F)
}

data_s <- NULL
for(i in 1:D){
  data_s <- rbind(data_s, dat_in[ind_s[[i]],])
}


#Store on each iteration
#sims_dat_out[k] <- dat_out

##### Step 4 FIT MODELS TO SAMPLE Y


#---------------

# Less Biased model without the non-significat interaction

mod_1 <- lmer(Y ~ (1 | municipio) +
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
          clase_hogar*genero, data = data_s, REML=T)

#Define some models adding or deleting variables
  
#Remove calidad_vivienda
mod_2 <- update(mod_1, .~.-calidad_vivienda)

#Remove sector_actividad
mod_3 <- update(mod_2, .~.-sector_actividad)

#Add rur_urb
mod_4 <- update(mod_1, .~.+rur_urb)

#Add calidad_vivienda*rur_urb
mod_5 <- update(mod_1, .~.+calidad_vivienda*rur_urb)

#Store each model, each iteration
#sims_mods[k] <- c(mod_1, mod_2, mod_3, mod_4, mod_5)

#---------------

##### Step 5 GOODNESS OF FIT MEASURES

#Define variables

k <- numeric()
Q1 <- numeric()
Q2 <- numeric()
Q3 <- numeric()
nummod <- 5
alpha1 <- c() #First penalty term
alpha2 #Proposed penalty term

for(i in 1:nummod){
    mod <- c("mod")
    assign(mod, get(paste("mod_", 1, sep = "")))
  
    #Define parameters of each model and area
    
    Xs_s <- model.matrix(mod) #Estimates Xp for each model
    p_s <- dim(Xs_s)[2] #dimensions of each model
    betaest_s <- fixed.effects(mod) #beta estimates for each model
    upred_s <- random.effects(mod)$municipio   #EBLUP for each model
    sigmae2est_s <- summary(mod)$sigma^2 #error estimates for each model
    sigmau2est_s <- sqrt(as.numeric(VarCorr(mod))) #random effects estimates for each model
    alpha1 <- p_s
    #compute gof (propsed loss function)
  
    mean_ss <- as.matrix(Xs_s)%*%matrix(betaest_s, nr = p_s, nc = 1)
    upred_rep <- numeric()
    
    for(j in 1:nrow(upred_s)){
      if(j == 1){
      upred_rep <- rep(upred_s[,1][j], n_s[as.numeric(rownames(upred_s))][j])
      }else{
      upred_rep <- c(upred_rep, rep(upred_s[,1][j], n_s[as.numeric(rownames(upred_s))][j]))  
      }
    }
    
    #loop for each penalty
    
    Q1[i] <- n*log(sigmae2est_s^2) + (sum(mean_ss - upred_rep)^2)/sigmae2est_s + D*log(sigmau2est_s^2) + sum(upred_rep^2) #alpha = 0
    Q2[i] <- n*log(sigmae2est_s^2) + (sum(mean_ss - upred_rep)^2)/sigmae2est_s + D*log(sigmau2est_s^2) + sum(upred_rep^2) + 2*alpha1
    Q3[i] <- n*log(sigmae2est_s^2) + (sum(mean_ss - upred_rep)^2)/sigmae2est_s + D*log(sigmau2est_s^2) + sum(upred_rep^2) #+ 2*alpha2
  }

m <- matrix(1:20, 4)
sigma <- 1:ncol(m)
omega <- 1:nrow(m)
mu <- 2

sum(((m - mu) / outer(omega, sigma))^2)

#sims_Y <- list()
#sims_y_bar <- list()
#sims_f_0 <- list()
#sims_dat_out <- list()
#sims_mods <- list()
#sims[k] <- c(sims_Y, sims_y_bar, sims_)

#} #End of simulation






#####################################
#                                   #
#         Simulation Study          #
#                                   #
#####################################


rm(list=ls())
options(warn=-1)

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

setwd("/Users/manugaco/Desktop/TFM")

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

colnames(datosMCSom)[15] <- "anos_escolaridad"
years_study <- datosMCSom$años_escolaridad

age <- datosMCSom$edad
age2 <- age
age2_2 <- age2^2
age2_3 <- age2^3

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

colnames(datosMCSom)[15] <- "anos_escolaridad"

escuela2 <- datosMCSom$anos_escolaridad
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

summary(ictpc)

hist(ictpc,freq=FALSE,main="")

hist(log(ictpct),freq=FALSE,main="")


##### SIMULATION

##### Step 1 FIT MODEL

y <- log(ictpct) #Response, transformed

summary(y)

fit_1 <- lmer(y ~ (1|as.factor(municipio)) +
                age2 +
                age2_2 +
                age2_3 +
                pob_indigena +
                sector_actividad +
                calidad_vivienda+
                rur_urb +
                ben_gob +
                bienes_casa3 +
                clase_hogar +
                genero +
                escuela3 +
                calidad_vivienda*rur_urb +
                clase_hogar*genero, REML=T)

library(nlme)
pv <- lme(y~age2 +
            age2_2 +
            age2_3 +
            pob_indigena +
            sector_actividad +
            calidad_vivienda+
            rur_urb +
            ben_gob +
            bienes_casa3 +
            clase_hogar +
            genero +
            escuela3 +
            calidad_vivienda*rur_urb +
            clase_hogar*genero,random=~1|as.factor(municipio))
anova(pv)

# Remove non significant variables
# Remove age_3 because is not significant, in advance

fit_2 <- lmer(y ~ (1|as.factor(municipio)) +
                age2 +
                age2_2 +
                pob_indigena +
                sector_actividad +
                calidad_vivienda+
                rur_urb +
                ben_gob +
                bienes_casa3 +
                clase_hogar +
                genero +
                escuela3 +
                calidad_vivienda*rur_urb +
                clase_hogar*genero,REML=T)

pv2 <- lme(y~age2 +
            age2_2 +
            pob_indigena +
            sector_actividad +
            calidad_vivienda+
            rur_urb +
            ben_gob +
            bienes_casa3 +
            clase_hogar +
            genero +
            escuela3 +
            calidad_vivienda*rur_urb +
            clase_hogar*genero,random=~1|as.factor(municipio))
anova(pv2)

X <- model.matrix(fit_2) #Estimates Xp
p <- dim(X)[2]
betaest <- fixed.effects(fit_2)
upred <- random.effects(fit_2)   #EBLUP
sigmae2est <- summary(fit_2)$sigma^2
sigmau2est <- sqrt(as.numeric(VarCorr(fit_2)))

#Check the model

res <- residuals(fit_2)
fitted <- fitted(fit_2)

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

#Input function for eb function
pov_inc <- function(y){
  z <- 1200
  result <- mean(y < z) 
}

#Inizializing objects

eblup_l <- list()
eb_l <- list()

#Goodness of fit
GOF <- list() #Nested list with all the measures, three vectors for each simulation

#Mean squared error
MSE_ybarhat <- list()
MSE_fhat <- list()

#Number of simulations
S <- 200

ini <- Sys.time()

for(h in 1:S){ #Starting simulation

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

e_Y <- exp(Y) - m

##### Step 3 SRSWOR municipalities

#Declare variables

dat_pop <- data.frame(Y, municipio, age2, age2_2, ben_gob, bienes_casa3, clase_hogar, 
                      calidad_vivienda, escuela3, genero, pob_indigena, remesas_f, rur_urb, 
                      sector_actividad)


#Loop for create the sampled data
sum_Nd <- 0
ind_v <- NULL

for(i in 1:D){ 
  d <- muns_uni[i]
  ind_s <- sample((sum_Nd+1):(sum_Nd + Nd[i]), nd[i], replace = F)
  ind_v <- c(ind_v, ind_s)
  sum_Nd <- sum_Nd + Nd[i]
}

data_s1 <- dat_pop[ind_v,]
Y_s <- Y[ind_v]
mean(Y_s)
e_Y_s <- e_Y[ind_v]
mean(e_Y_s)

Xs <- X[ind_v, ]
Xr <- X[-ind_v, ]

Xin <- data.frame(cbind(municipio = municipio[ind_v], Xs[,-1]))
data_ind <- Xin %>% group_by(municipio) %>% summarize(area = n())

Xnon <- data.frame(cbind(municipio = municipio[-ind_v], Xr[,-1]))
popsize <- data.frame(cbind(muns_uni, Nd))

#define variables for eblup and eb formulas

municipio_s <- municipio[ind_v]
age2_s <- age2[ind_v]
age2_2_s <- age2_2[ind_v]
ben_gob_s <- ben_gob[ind_v] 
bienes_casa3_s <- bienes_casa3[ind_v]
clase_hogar_s <- clase_hogar[ind_v]
calidad_vivienda_s <- calidad_vivienda[ind_v]
escuela3_s <- escuela3[ind_v]
pob_indigena_s <- pob_indigena[ind_v]
remesas_f_s <- remesas_f[ind_v]
sector_actividad_s <- sector_actividad[ind_v]
clase_hogar_s <- clase_hogar[ind_v]
genero_s <- genero[ind_v]
rur_urb_s <- rur_urb[ind_v]

dat_s1_s <- data.frame(Y_s, municipio_s, age2_s, age2_2_s, ben_gob_s, bienes_casa3_s, clase_hogar_s, 
                       calidad_vivienda_s, escuela3_s, genero_s, pob_indigena_s, remesas_f_s, rur_urb_s, 
                       sector_actividad_s)

dat_s1_s_e <- data.frame(e_Y_s, municipio_s, age2_s, age2_2_s, ben_gob_s, bienes_casa3_s, clase_hogar_s, 
                         calidad_vivienda_s, escuela3_s, genero_s, pob_indigena_s, remesas_f_s, rur_urb_s, 
                         sector_actividad_s)

#Store on each iteration
#sims_dat_out[k] <- dat_out

##### Step 4 FIT MODELS TO SAMPLE Y

#---------------

# Less Biased model without the non-significat interaction

form <- list()
form_s <- list()
form_s_est <- list()
form_s_e <- list()
form_s_e_est <- list()
mod <- list()
data_mods <- list()
data_mods_Non <- list()
means_mods <- list()
nummod <- 5


#Define some models adding or deleting variables
#We removed the variable rur_urb and its interaction


form[[1]] <- as.formula(Y ~ (1 | municipio) +
                        age2 +
                        age2_2 +
                        ben_gob +
                        bienes_casa3 +
                        clase_hogar +
                        calidad_vivienda +
                        escuela3 +
                        pob_indigena +
                        sector_actividad +
                        clase_hogar*genero)

form_s[[1]] <- as.formula(Y_s ~ (1 | municipio_s) +
                            age2_s +
                            age2_2_s +
                            ben_gob_s +
                            bienes_casa3_s +
                            clase_hogar_s +
                            calidad_vivienda_s +
                            escuela3_s +
                            pob_indigena_s +
                            sector_actividad_s +
                            clase_hogar_s:genero_s)

form_s_est[[1]] <- as.formula(Y_s ~
                            age2_s +
                            age2_2_s +
                            ben_gob_s +
                            bienes_casa3_s +
                            clase_hogar_s +
                            calidad_vivienda_s +
                            escuela3_s +
                            pob_indigena_s +
                            sector_actividad_s +
                            clase_hogar_s:genero_s)

form_s_e[[1]] <- as.formula(e_Y_s ~ (1 | municipio_s) +
                            age2_s +
                            age2_2_s +
                            ben_gob_s +
                            bienes_casa3_s +
                            clase_hogar_s +
                            calidad_vivienda_s +
                            escuela3_s +
                            pob_indigena_s +
                            sector_actividad_s +
                            clase_hogar_s:genero_s)

form_s_e_est[[1]] <- as.formula(e_Y_s ~
                              age2_s +
                              age2_2_s +
                              ben_gob_s +
                              bienes_casa3_s +
                              clase_hogar_s +
                              calidad_vivienda_s +
                              escuela3_s +
                              pob_indigena_s +
                              sector_actividad_s +
                              clase_hogar_s:genero_s)

mod[[1]] <- lmer(form_s[[1]], data = dat_s1_s, REML=T)
data_mods[[1]] <- model.matrix(rep(1, length(age2)) ~
                         age2 +
                         age2_2 +
                         ben_gob +
                         bienes_casa3 +
                         clase_hogar +
                         calidad_vivienda +
                         escuela3 +
                         pob_indigena +
                         sector_actividad +
                         clase_hogar:genero)
data_mods_Non[[1]] <- data.frame(cbind(municipio = municipio[-ind_v], data_mods[[1]][-ind_v,]))
data_mods_Non[[1]] <- data_mods_Non[[1]][,-2]
means_mods[[1]] <- data.frame(cbind(muns_uni = municipio, data_mods[[1]][,-1])) %>% group_by(muns_uni) %>% summarise_all("mean")


#mod1 without calidad_vivienda, (also from the best model, We removed the variable rur_urb and its interaction)

form[[2]] <- update(form[[1]], .~. -calidad_vivienda)
form_s[[2]] <- update(form_s[[1]], .~. -calidad_vivienda_s)
form_s_est[[2]] <- update(form_s_est[[1]], .~. -calidad_vivienda_s)
form_s_e[[2]] <- update(form_s_e[[1]], .~. -calidad_vivienda_s)
form_s_e_est[[2]] <- update(form_s_e_est[[1]], .~. -calidad_vivienda_s)

mod[[2]] <- lmer(form_s[[2]], data = dat_s1_s, REML=T)
data_mods[[2]] <- model.matrix(rep(1, length(age2)) ~ 
                         age2 +
                         age2_2 +
                         ben_gob +
                         bienes_casa3 +
                         clase_hogar +
                         escuela3 +
                         pob_indigena +
                         sector_actividad +
                         clase_hogar*genero)
data_mods_Non[[2]] <- data.frame(cbind(municipio = municipio[-ind_v], data_mods[[2]][-ind_v,]))
data_mods_Non[[2]] <- data_mods_Non[[2]][,-2]
means_mods[[2]] <- data.frame(cbind(muns_uni = municipio, data_mods[[2]][,-1])) %>% group_by(muns_uni) %>% summarise_all("mean")

#mod1 without sector_actividad, (also from the best model, We removed the variable rur_urb and its interaction)

form[[3]] <- update(form[[1]], .~. -sector_actividad)
form_s[[3]] <- update(form_s[[1]], .~. -sector_actividad_s)
form_s_est[[3]] <- update(form_s_est[[1]], .~. -sector_actividad_s)
form_s_e[[3]] <- update(form_s_e[[1]], .~. -sector_actividad_s)
form_s_e_est[[3]] <- update(form_s_e_est[[1]], .~. -sector_actividad_s)

mod[[3]] <- lmer(form_s[[3]], data = dat_s1_s, REML=T)
data_mods[[3]] <- model.matrix(rep(1, length(age2)) ~
                         age2 +
                         age2_2 +
                         ben_gob +
                         bienes_casa3 +
                         clase_hogar +
                         calidad_vivienda +
                         escuela3 +
                         pob_indigena +
                         clase_hogar*genero)
data_mods_Non[[3]] <- data.frame(cbind(municipio = municipio[-ind_v], data_mods[[3]][-ind_v,]))
data_mods_Non[[3]] <- data_mods_Non[[3]][,-2]
means_mods[[3]] <- data.frame(cbind(muns_uni = municipio, data_mods[[3]][,-1])) %>% group_by(muns_uni) %>% summarise_all("mean")

#mod1 adding rur_urb but without interaction

form[[4]] <- update(form[[1]], .~. +rur_urb)
form_s[[4]] <- update(form_s[[1]], .~. +rur_urb_s)
form_s_est[[4]] <- update(form_s_est[[1]], .~. +rur_urb_s)
form_s_e[[4]] <- update(form_s_e[[1]], .~. +rur_urb_s)
form_s_e_est[[4]] <- update(form_s_e_est[[1]], .~. +rur_urb_s)

mod[[4]] <-lmer(form_s[[4]], data = dat_s1_s, REML=T)
data_mods[[4]] <- model.matrix(rep(1, length(age2)) ~ 
                        age2 +
                        age2_2 +
                        ben_gob +
                        bienes_casa3 +
                        clase_hogar +
                        calidad_vivienda +
                        escuela3 +
                        rur_urb +
                        pob_indigena +
                        sector_actividad +
                        clase_hogar*genero)
data_mods_Non[[4]] <- data.frame(cbind(municipio = municipio[-ind_v], data_mods[[4]][-ind_v,]))
data_mods_Non[[4]] <- data_mods_Non[[4]][,-2]
means_mods[[4]] <- data.frame(cbind(muns_uni = municipio, data_mods[[4]][,-1])) %>% group_by(muns_uni) %>% summarise_all("mean")

#Add calidad_vivienda*rur_urb (so rur_urb main_effect is added) (this is the correct model)

form[[5]] <- update(form[[1]], .~. +calidad_vivienda*rur_urb)
form_s[[5]] <- update(form_s[[1]], .~. +calidad_vivienda_s*rur_urb_s)
form_s_est[[5]] <- update(form_s_est[[1]], .~. +calidad_vivienda_s*rur_urb_s)
form_s_e[[5]] <- update(form_s_e[[1]], .~. +calidad_vivienda_s*rur_urb_s)
form_s_e_est[[5]] <- update(form_s_e_est[[1]], .~. +calidad_vivienda_s*rur_urb_s)
mod[[5]] <- lmer(form_s[[5]], data = dat_s1_s, REML=T)
data_mods[[5]] <- model.matrix(rep(1, length(age2)) ~ 
                         age2 +
                         age2_2 +
                         ben_gob +
                         bienes_casa3 +
                         clase_hogar +
                         calidad_vivienda +
                         calidad_vivienda*rur_urb +
                         escuela3 +
                         pob_indigena +
                         sector_actividad +
                         clase_hogar*genero)
data_mods_Non[[5]] <- data.frame(cbind(municipio = municipio[-ind_v], data_mods[[5]][-ind_v,]))
data_mods_Non[[5]] <- data_mods_Non[[5]][,-2]
means_mods[[5]] <- data.frame(cbind(muns_uni = municipio, data_mods[[5]][,-1])) %>% group_by(muns_uni) %>% summarise_all("mean")

#Store each model, each iteration
#sims_mods[k] <- c(mod_1, mod_2, mod_3, mod_4, mod_5)

#---------------

##### Step 5 GOODNESS OF FIT MEASURES

#Define variables

k <- numeric()
Qfm_alpha0 <- numeric()
Qfm_alpha1 <- numeric()
Qfm_alpha2 <- numeric()
eblup <-list()
eb <- list()

#GOF by maximum likelihood, without penalty

for(i in 1:nummod){
  
    #Define parameters of each model and area
    
    Xs_s <- model.matrix(mod[[i]]) #Estimates Xp for each model
    ps_s <- dim(Xs_s)[2] #dimensions of each model
    betaest_s <- fixed.effects(mod[[i]]) #beta estimates for each model
    upred_s <- random.effects(mod[[i]])$municipio[,1]   #EBLUP for each model
    sigmae2est_s <- summary(mod[[i]])$sigma^2 #error estimates for each model
    sigmau2est_s <- sqrt(as.numeric(VarCorr(mod[[i]]))) #random effects estimates for each model
  
    #compute gof (propsed loss function)
    
    Q <- 0
    A <- 0
    B <- 0
    C <- 0
    for(j in 1:D){
      d <- muns_uni[j]
      Xd <- Xs_s[data_s1$municipio == d,,drop=FALSE] #Subset predictors for each municipality
      mean_d <- Xd%*%betaest_s
      Yd <- data_s1$Y[data_s1$municipio == d]
      Q <- Q + sum(((Yd - mean_d - upred_s[j])^2))/sigmae2est + sum(upred_s[j]^2)/sigmau2est
      #Components of rho_tau
      gamma_d <- sigmau2est_s/(sigmau2est_s + (sigmae2est_s/nd[j]))
      x_bar <- (t(Xd)%*%rep(1,nrow(Xd)))
      A <- A + t(Xd)%*%Xd - (nd[j]*gamma_d*(x_bar%*%t(x_bar)))
      B <- B + t(Xd)%*%Xd - (2*nd[j]*(gamma_d-2)*(x_bar%*%t(x_bar)))
      C <- C + nd[j]*gamma_d
      
    }
    
    Qfm_alpha0[i] <- Q + n*log(sigmae2est) + D*log(sigmau2est) #alpha0
    Qfm_alpha1[i] <- Qfm_alpha0[i] + 2*(ncol(Xs_s) + 2)
    
    A <- (1/sigmae2est_s)*A
    B <- (1/sigmae2est_s)*B
    rho_tau <- sum(diag((inv(A) %*% B))) + C
    
    Qfm_alpha2[i] <- Qfm_alpha0[i] + 2*(rho_tau + 2)

}

#Log-likelihood, the best model is the expected, model 5.

##Step 6 Compute eblup and eb

#dom, same size as y in the formula (3522)

#EBLUP

for(i in 1:nummod){
  eblup[[i]] <- eblupBHF(formula = form_s_est[[i]], dom = municipio_s, meanxpop = means_mods[[i]],
                         popnsize = popsize, data = dat_s1_s)$eblup
}

eblup_l[[h]] <- eblup

#EB

for(i in 1:nummod){
  eb[[i]] <- ebBHF(formula = form_s_e_est[[i]], dom =  municipio_s, Xnonsample = data_mods_Non[[i]], constant = m,
                 indicator = pov_inc)$eb
}

eb_l[[h]] <- eblup

#Save GOF
gofs <- list()

gofs[[1]] <- Qfm_alpha0
gofs[[2]] <- Qfm_alpha1
gofs[[3]] <- Qfm_alpha2
GOF[[h]] <- gofs 

#Mean squared error for y_bar

y_barhat_ls <- list()
for(i in 1:nummod){
  y_barhat_ls[[i]] <- (eblup[[i]]$eblup - y_bar)^2
}

MSE_ybarhat[[h]] <- y_barhat_ls

#Mean squared error for f_o

fo_hat_ls <- list()
for(i in 1:nummod){
  fo_hat_ls[[i]] <- (eb[[i]]$eb - f_0)^2
}

MSE_fhat[[h]] <- fo_hat_ls
print(paste("iteration:", h, sep = " "))

} #End of simulation

tt_s <- Sys.time() - ini #8.853463 mins 500 times
tt_s

#Empirical GOF average, per model

numgof <- 3
avg_gof <- list()
for(i in 1:numgof){
  aux <- 0
  for(j in 1:S){
   aux <- GOF[[j]][[i]] + aux
  }
  avg_gof[[i]] <- aux/S #Divide by the number of simulations
}

which.min(avg_gof[[1]])
which.min(avg_gof[[2]])
which.min(avg_gof[[3]])

#The maximum averaged gof test, using the different penalties, is the model 5, as expected

#Computing MSE of y_bar

MSE_Y <- list()
result_y <- matrix(0, ncol = nummod, nrow = D)
for(i in 1:nummod){
  aux <- 0
  for(j in 1:S){
    aux <- MSE_ybarhat[[j]][[i]] + aux
  }
  MSE_Y [[i]] <- aux/S
  result_y[,i] <- t(aux/S)
}
colnames(result_y) <- c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5")

#Computing MSE of F_0

MSE_F0 <- list()
result_f0 <- matrix(0, ncol = nummod, nrow = D)
for(i in 1:nummod){
  aux <- 0
  for(j in 1:S){
    aux <- MSE_fhat[[j]][[i]] + aux
  }
  MSE_F0 [[i]] <- aux/S
  result_f0[,i] <- t(aux/S)
}
colnames(result_f0) <- c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5")

#Step 8 PLOTS

#Boxplot of MSE Y_bar

boxplot(result_y)

#Boxplot of MSE f_0

boxplot(result_f0)

#Scatterplot of maximum MSE (Y_bar and f_0) against each GOFs

#Log of income and poverty rate

df_my <- matrix(0, ncol=2, nrow=5)
df_mf0 <- matrix(0, ncol=2, nrow=5)

for(i in 1:nummod){
  df_my[i,1] <- which.max(result_y[,i])
  df_my[i,2] <- result_y[df_my[i,1],i]
  df_mf0[i,1] <- which.max(result_f0[,i])
  df_mf0[i,2] <- result_f0[df_mf0[i,1],i]
}

colnames(df_my) <- c("index", "value")
colnames(df_mf0) <- c("index", "value")
df_my
df_mf0

#Alpha0

plot(df_my[,2], avg_gof[[1]], main = "Maximum MSE of log-income against GOF alpha0")
plot(df_mf0[,2], avg_gof[[1]], main = "Maximum MSE of poverty rate against GOF alpha0")

#Alpha1

plot(df_my[,2], avg_gof[[2]], main = "Maximum MSE of log-income against GOF alpha1")
plot(df_mf0[,2], avg_gof[[2]], main = "Maximum MSE of poverty rate against GOF alpha1")

#Alpha2

plot(df_my[,2], avg_gof[[3]], main = "Maximum MSE of log-income against GOF alpha2")
plot(df_mf0[,2], avg_gof[[3]], main = "Maximum MSE of poverty rate against GOF alpha2")



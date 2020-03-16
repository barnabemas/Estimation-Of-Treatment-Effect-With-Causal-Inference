#This file is used for dataframe generation following the model chosen

r <- 1 #number of controls available for each treated unit (1 or 2)
N <- 250*(1+r)  #NB people in control and treatment groups
diff_mean <- 0.2 #Difference in mean between covariates in treament and control
sigma_bruit <- 4

#The variables in each dataset consisted of three different possible continuous outcomes
#and eight covariates, four of them continuous and four dichotomous
#More specifically, the dichotomous covariates were two rare and two common Bernoulli random variables,
#all of them conditionally independent from each other and from the continuous covariates
#given the treatment assignment indicator Z


#Bellow are the 3 functions to model f(X) used in Y 

f_1 <- function(X) {3.5*X[,1]+4.5*X[,3]+1.5*X[,5]+2.5*X[,7]}  #Linear

f_2 <- function(X) {3.5*X[,1]+4.5*X[,3]+1.5*X[,5]+2.5*X[,7]   #Additive
  +2.5*sign(X[,5])*sqrt(abs(X[,5]))+5.5*X[,7]^2} 

f_3 <- function(X) {3.5*X[,1]+4.5*X[,3]+1.5*X[,5]+2.5*X[,7]   #Additive with interactions
  +2.5*sign(X[,5])*sqrt(abs(X[,5]))+5.5*X[,7]^2
  +2.5*X[,3]*X[,7]-4.5*abs(X[,5]*X[,7]^3)} 


#Scenario 1, same variances in the treated and control groups with independent covariates in both groups
#f is to be chosen among f_1, f_2 and f_3

dataset_1 <- function(f) {
  
  bern_comun_t <- 0.5
  bern_comun_c <- (8-sqrt(2.6))/16
  bern_rare_t <- 0.1
  bern_rare_c <- (-sqrt(598)+44)/408
  
  var_norm = 1
  mu_t <- 0.5
  mu_c <- 0.3
  
  #Building the covariates
  X_c_1 <- data.frame(cbind(matrix(rbinom(2*250*r,1,bern_comun_c),ncol =2,nrow=250*r),
                            matrix(rbinom(2*250*r,1,bern_rare_c),ncol =2,nrow=250*r),
                            matrix(rnorm(4*250*r, mean = mu_c, sd =var_norm),ncol=4,nrow=250*r)))
  X_t_1 <- data.frame(cbind(matrix(rbinom(2*250,1,bern_comun_t),ncol =2,nrow=250),
                            matrix(rbinom(2*250,1,bern_rare_t),ncol =2,nrow=250),
                            matrix(rnorm(4*250, mean = mu_t, sd =var_norm),ncol=4,nrow=250)))
  
  #Adding the treamtment and merging dataframes
  X_c_1$treat <- 0
  X_t_1$treat <- 1
  X_1 <-rbind(X_c_1,X_t_1)
  
  #Adding Y, here with function f, to dataframe of scenario 1
  X_1$Y <- f(X_1)+rnorm(N,0,sigma_bruit)+X_1$treat
  
  return(X_1)
}  



#Scenario 2, different variances between treated and control groups with independent covariates in both groups

dataset_2 <- function(f) {
  
  bern_comun_t <- 0.5
  bern_comun_c <- (8-sqrt(2.6))/16
  bern_rare_t <- 0.1
  bern_rare_c <- (-sqrt(598)+44)/408
  
  var_c = 1
  var_t = 1.2
  mu_t <- 0.5
  mu_c <- 0.2737
  
  #Building the covariates
  X_c_2 <- data.frame(cbind(matrix(rbinom(2*250*r,1,bern_comun_c),ncol =2,nrow=250*r),
                            matrix(rbinom(2*250*r,1,bern_rare_c),ncol =2,nrow=250*r),
                            matrix(rnorm(4*250*r, mean = mu_c, sd =var_c),ncol=4,nrow=250*r)))
  X_t_2 <- data.frame(cbind(matrix(rbinom(2*250,1,bern_comun_t),ncol =2,nrow=250),
                            matrix(rbinom(2*250,1,bern_rare_t),ncol =2,nrow=250),
                            matrix(rnorm(4*250, mean = mu_t, sd =var_t),ncol=4,nrow=250)))
  
  #Adding the treamtment and merging dataframes
  X_c_2$treat <- 0
  X_t_2$treat <- 1
  X_2 <-rbind(X_c_2,X_t_2)
  
  #Adding Y, here with function f, to dataframe of scenario 2
  X_2$Y <- f(X_2)+rnorm(N,0,sigma_bruit)+X_2$treat
  
  return(X_2)
}  


#Scenario 3, different variances between treated and control groups with independent covariates in the control group and correlated covariates in the treatment group.

#This scenario is less realistic with the hypotesis of PCA ?

dataset_3 <- function(f) {
  
  bern_comun_t <- 0.5
  bern_comun_c <- (8-sqrt(2.6))/16
  bern_rare_t <- 0.1
  bern_rare_c <- (-sqrt(598)+44)/408
  
  var_c = 1
  var_t = 1.2
  mu_t <- 0.5
  mu_c <- 0.2737
  
  ## Correlations
  corr1 <- 0.5
  corr2 <- 0.1
  corr3 <- 0.8
  Sigma <- matrix(c(var_t, corr1, corr3, corr3,
                    corr1, var_t, corr2, corr3,
                    corr3, corr2, var_t, corr2,
                    corr3, corr3, corr2, var_t),
                  ncol = 4, byrow = TRUE)
  
  #Building the covariates
  X_c_3 <- data.frame(cbind(matrix(rbinom(2*250*r,1,bern_comun_c),ncol =2,nrow=250*r),
                            matrix(rbinom(2*250*r,1,bern_rare_c),ncol =2,nrow=250*r),
                            matrix(rnorm(4*250*r, mean = mu_c, sd =var_c),ncol=4,nrow=250*r)))
  X_t_3 <- data.frame(cbind(matrix(rbinom(2*250,1,bern_comun_t),ncol =2,nrow=250),
                            matrix(rbinom(2*250,1,bern_rare_t),ncol =2,nrow=250),
                            matrix(mvrnorm(4*250, rep(mu_t,4),Sigma = Sigma),ncol=4,nrow=250)))
  
  #Adding the treamtment and merging dataframes
  X_c_3$treat <- 0
  X_t_3$treat <- 1
  X_3 <-rbind(X_c_3,X_t_3)
  
  #Adding Y, here with function f, to dataframe of scenario 3
  X_3$Y <- f(X_3)+rnorm(N,0,sigma_bruit)+X_3$treat
  
  return(X_3)
}  



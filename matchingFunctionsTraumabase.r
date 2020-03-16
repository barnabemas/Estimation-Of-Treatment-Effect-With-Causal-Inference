#Those function are matching methods to be called on the traumabase

#################################  PROPENSITY MATCHING ################################# 

## Propensity Matching with logistic regression 

matching_prop_logit_bootstrap<-function(dataset,ind){
  
  dataset <- dataset[ind,]
  dataset$treat <- as.numeric(dataset$treat)-1
  dataset$Y <- as.numeric(dataset$Y)
  
  #Propensity score
  ps_logit <-glm(treat~ Dim.1+Dim.2+Dim.3+Dim.4+Dim.5+Dim.7+Dim.8+Dim.6+Dim.9+Dim.10+Dim.11+Dim.12+Dim.13+Dim.14+Dim.15 + Dim.17+Dim.18+Dim.16+Dim.19+Dim.20+Dim.21+Dim.22+Dim.23+Dim.24+Dim.25+Dim.26,
                 family = binomial(link="logit"),data = dataset)
  dataset$pscore <- ps_logit$fitted
  
  #matching
  Y <- dataset$Y  
  Tr <- dataset$treat 
  X <- dataset$pscore  
  Match.out <- Match(Y = Y, Tr = Tr, estimand="ATT", M=1, replace =TRUE, ties = TRUE,  X)
  
  #Matching Balance
  return(Match.out$est)
}



## Propensity Matching with logistic regression Random Forest

matching_prop_rf_bootstrap<-function(dataset,ind){
  
  dataset <- dataset[ind,]
  dataset$treat <- as.numeric(dataset$treat)-1
  dataset$Y <- as.numeric(dataset$Y)
  
  
  ps.RF <-randomForest(treat ~ Dim.1+Dim.2+Dim.3+Dim.4+Dim.5+Dim.7+Dim.8+Dim.6+Dim.9+Dim.10+Dim.11+Dim.12+Dim.13+Dim.14+Dim.15 + Dim.17+Dim.18+Dim.16+Dim.19+Dim.20+Dim.21+Dim.22+Dim.23+Dim.24+Dim.25+Dim.26,
                       data=dataset, ntree=100)
  dataset$pscore <- ps.RF$predicted
  
  #Matching
  
  Y <- dataset$Y  
  Tr <- dataset$treat 
  X <- dataset$pscore 
  
  Match.outRF <- Match(Y = Y, Tr = Tr, estimand="ATT", M=1, replace = TRUE, ties = TRUE, caliper = 0.15, X)
  return(Match.outRF$est)
  
}


##Covariates balance plot

#propensity
covBal_logit<-function(dataset){
  
  #Logistic regression
  
  ps_logit <-glm(treat~ Dim.1+Dim.2+Dim.3+Dim.4+Dim.5+Dim.7+Dim.8+Dim.6+Dim.9+Dim.10+Dim.11+Dim.12+Dim.13+Dim.14+Dim.15 + Dim.17+Dim.18+Dim.16+Dim.19+Dim.20+Dim.21+Dim.22+Dim.23+Dim.24+Dim.25+Dim.26,
                 family = binomial(link="logit"),data = dataset)
  dataset$pscore <- ps_logit$fitted
  
  #matching
  Y <- dataset$Y  
  Tr <- dataset$treat 
  X <- datset$pscore  
  
  Match.out <- Match(Y = Y, Tr = Tr, estimand="ATT", M=1, replace =TRUE, ties = TRUE,  X)
  
  #Plot
  
  balMatch <- bal.tab(M=Match.out, formula = treat ~  Dim.1+Dim.2+Dim.3+Dim.4+Dim.5+Dim.7+Dim.8+Dim.6+Dim.9+Dim.10+Dim.11+Dim.12+Dim.13+Dim.14+Dim.15 + Dim.17+Dim.18+Dim.16+Dim.19+Dim.20+Dim.21+Dim.22+Dim.23+Dim.24+Dim.25+Dim.26 , data = dataset, un = TRUE, disp.v.ratio = TRUE)
  love.plot(x = balMatch,
            stat = c("mean.diffs", "variance.ratios"),
            stars="none" ,
            threshold = c(m = 0.1, v = 1.2),
            abs = TRUE)
  
}
#Random Forests

covBal_RF<-function(dataset,ind){
  
  #Propensity score
  
  ps.RF <-randomForest(treat~ Dim.1+Dim.2+Dim.3+Dim.4+Dim.5+Dim.7+Dim.8+Dim.6+Dim.9+Dim.10+Dim.11+Dim.12+Dim.13+Dim.14+Dim.15 + Dim.17+Dim.18+Dim.16+Dim.19+Dim.20+Dim.21+Dim.22+Dim.23+Dim.24+Dim.25+Dim.26, family = binomial(link="logit"),data = dataset)
  dataset$pscore <- ps_logit$predicted
  
  #matching
  Y <- dataset$Y  
  Tr <- dataset$treat 
  X <- datset$pscore  
  
  Match.outRF <- Match(Y = Y, Tr = Tr, estimand="ATT", M=1, replace =TRUE, ties = TRUE,  X)
  
  #Plot
  
  balMatch <- bal.tab(M=Match.outRF,
                      formula = treat ~  Dim.1+Dim.2+Dim.3+Dim.4+Dim.5+Dim.7+Dim.8+Dim.6+Dim.9+Dim.10+Dim.11+Dim.12+Dim.13+Dim.14+Dim.15 + Dim.17+Dim.18+Dim.16+Dim.19+Dim.20+Dim.21+Dim.22+Dim.23+Dim.24+Dim.25+Dim.26 , data = dataset, un = TRUE, disp.v.ratio = TRUE)
  love.plot(x = balMatch,stat = c("mean.diffs", "variance.ratios"), stars="none" ,threshold = c(m = 0.1, v = 1.2), abs = TRUE)
  
}



#DISTRIBUTIONAL BALANCE Y

BalY <-function(dataset, ind){ 
  
  #Logistic regression
  
  ps_logit <-glm(treat~ Dim.1+Dim.2+Dim.3+Dim.4+Dim.5+Dim.7+Dim.8+Dim.6+Dim.9+Dim.10+Dim.11+Dim.12+Dim.13+Dim.14+Dim.15 + Dim.17+Dim.18+Dim.16+Dim.19+Dim.20+Dim.21+Dim.22+Dim.23+Dim.24+Dim.25+Dim.26, family = binomial(link="logit"),data = dataset)
  dataset$pscore <- ps_logit$fitted
  
  #matching
  Y <- dataset$Y  
  Tr <- dataset$treat 
  X <- datset$pscore  
  
  Match.out <- Match(Y = Y, Tr = Tr, estimand="ATT", M=1, replace =TRUE, ties = TRUE,  X)
  
  bal.plot(obj = Match.out, formula = treat ~ Y ,
           data = simu_reg,
           var.name = "Y",
           which = "both")
  
}



BalPS <-function(dataset, ind){ 
  
  #Logistic regression
  
  ps_logit <-glm(treat~ Dim.1+Dim.2+Dim.3+Dim.4+Dim.5+Dim.7+Dim.8+Dim.6+Dim.9+Dim.10+Dim.11+Dim.12+Dim.13+Dim.14+Dim.15 + Dim.17+Dim.18+Dim.16+Dim.19+Dim.20+Dim.21+Dim.22+Dim.23+Dim.24+Dim.25+Dim.26, family = binomial(link="logit"),data = dataset)
  dataset$pscore <- ps_logit$fitted
  
  #matching
  Y <- dataset$Y  
  Tr <- dataset$treat 
  X <- datset$pscore  
  
  Match.out <- Match(Y = Y, Tr = Tr, estimand="ATT", M=1, replace =TRUE, ties = TRUE,  X)
  
  bal.plot(obj = Match.out, formula = treat ~ pscore ,
           data = simu_reg,
           var.name = "Y",
           which = "both")
  
}

#################################  CARDINALITY MATCHING ################################# 

matching_card_bootstrap <- function(dataset, ind, tolerance=rep(1, 26), t_max=600) {
  dataset<-dataset[ind,]
  dataset$treat <- as.numeric(dataset$treat)-1
  dataset$Y <- as.numeric(dataset$Y)
  dataset <- dataset[order(dataset$treat, decreasing = TRUE), ]
  treatment <-  dataset$treat
  X_list <- names(dataset)
  X_list <- X_list[X_list != "Y" & X_list != "treat"]
  
  
  ### Solver
  solver = list(name = "glpk", t_max, approximate = 1, round_cplex = 0, trace = 0)
  solver2 = list(name = "glpk", t_max, approximate = 1, round_cplex = 0, trace = 0)
  
  ### Step 1: Find the largest sample of matched pairs respecting the covariates balancing rules
  # Match
  card_match_step1 <- cardmatch(treatment, mom = list(covs=dataset[X_list], tols=tolerance), solver = solver)
  
  # Indices of the treat units and matched controls
  t_id_1 <- card_match_step1$t_id
  c_id_1 <- card_match_step1$c_id
  
  ### Step 2: Repairs the matches to minimize the sum of distances between individuals
  
  treatment2 <- treatment[c(t_id_1, c_id_1)]                 # treatment indicator among the matched
  
  #Distance matrix
  dataset_matched <- dataset[c(t_id_1, c_id_1),]
  minimums <- apply(dataset_matched, MARGIN = 2, FUN = min)  # minimums for each covariate
  maximums <- apply(dataset_matched, MARGIN = 2, FUN = max)  # maximums for each covariate
  dataset_matched_normed <- scale(dataset_matched, center = minimums, scale = maximums - minimums) 
  # renormalization of the covariates
  nb_matched_treated <- dim(dataset_matched_normed)[1]/2
  matrix_dist <- dist(dataset_matched_normed, method = "euclidean")
  matrix_dist <- as.matrix(matrix_dist)
  matrix_dist <- matrix_dist[(nb_matched_treated+1):(2*nb_matched_treated),1:nb_matched_treated] 
  # matrix of distances between each person 
  
  # Match
  out_2 <- distmatch(treatment2, matrix_dist, solver2)
  
  # Indices of the treated units and matched controls
  t_id_2 <- t_id_1[out_2$t_id]
  c_id_2 <- c_id_1[out_2$c_id-length(out_2$c_id)]
  output_cardinality_matching <- dataset[c(t_id_2,c_id_2),]
  
  ### Step 3: computation of the intersting parameters
  
  est <- lm( Y ~ treat , data = output_cardinality_matching)                                                     
  param_ATT <- coef(summary(est))["treat",]
  return(param_ATT["Estimate"])
}

################################# COARSENED EXACT MATCHING ################################# 

matching_cem_bootstrap <- function(dataset,ind){
  dataset <- dataset[ind,]
  dataset$treat <- as.numeric(dataset$treat)-1
  dataset$Y <- as.numeric(dataset$Y)
  
  #on règle à la fin sinon il match personne(13/250)
  mat <- cem(treatment = "treat",
             data = dataset,
             drop = "Y")
  
  est <- att(mat, Y ~ treat , data = dataset)#,  model="linear-RE")
  
  est$att.model[,"treat"]
  return(est$att.model[,"treat"][[1]])
}

################################# DATASET COMPLEXITY ################################# 

VarMeanPlot <-function(dataset,ind){
  #Plot diff in means before matching
  dataTest<-dataset[,-28]
  
  #View(dataTest)
  treated <- filter(dataTest,treat==1)
  noTreated<- filter(dataTest,treat==0)
  treated<-treated[,-27]
  noTreated<-noTreated[,-27]
  
  
  mean1<-colMeans(treated)
  mean2<-colMeans(noTreated)
  #View(treated)
  test<-data.frame(mean1,mean2)
  #View(test)
  abline(coef = c(1, 0), col = 2)
  abline(coef = c(0.5, 0), col = 3)
  
  #Plot Variance Ratio
  Var1<-var(treated)
  Var2<-var(noTreated)
  Var1<-diag(Var1)
  Var2<-diag(Var2)
  
  #View(test)
  
  abline(coef = c(1, 0), col = 2)
  abline(coef = c(2, 0), col = 3)
  
}

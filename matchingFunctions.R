################################# BEFORE MATCHING ################################# 
#Firstly, we compute quantities of interest before matching to have a benchmark

matching_before <- function(dataset){
  
  X_list <- names(dataset)
  X_list <- X_list[X_list != "Y" & X_list != "treat"]
  
  #We create a dataframe with param as columns and observations as ligns
  param <- c("sdiff","ratioVar","KS")
  info <- data.frame(matrix(0,
                            nrow = length(param),
                            ncol=length(X_list)),
                     row.names = param)
  
  #We iter for each covariate 
  for(X in X_list) {
    
    #We compute quantities of interest for each covariate
    info_X <- balanceUV(Tr=dataset[dataset$treat==1,][,X],
                        Co=dataset[dataset$treat!=1,][,X],
                        ks=TRUE,
                        nboots=1000)
    
    # Difference in mean, Ratio fo variance and KS
    
    p0<-abs(info_X$sdiff)
    p1<-info_X$var.ratio
    p2<-info_X$ks$ks.boot.pvalue
    info[,X]<-c(p0,p1,p2)

  }
  
  #Finally we compute estimated ATT 
  
  param_gen <-list("ATT"=mean(dataset[dataset$treat==1,]$Y-dataset[dataset$treat==0,]$Y),
                   "drop_ratio_Co"=0,
                   "drop_ratio_Tr"=0)
  
  return(list("info_covar"=info,"param_gen"=param_gen))
}



################################# COARSENED EXACT MATCHING ################################# 


matching_cem <- function(dataset){
  
  #The cuting points are hand-chosen bellow
  mat <- cem(treatment = "treat",
             data = dataset,
             drop = "Y",
             cut=list(X1=1,X1=1,X3=1,X4=1,X5=8,X6=8,X7=8,X8=8))
  
  X_list <- names(dataset)
  X_list <- X_list[X_list != "Y" & X_list != "treat"]
  
  #We create a dataframe with param as columns and observations as ligns
  param <- c("sdiff","ratioVar","KS")
  info <- data.frame(matrix(0,
                            nrow = length(param),
                            ncol=length(X_list)),
                     row.names = param)
  
  #We iter for each covariate 
  for(X in X_list) {
    
    #We compute quantities of interest for each covariate
    #mat$w gives us the weights
    
    info_X <- balanceUV(Tr=dataset[dataset$treat==1,][,X],
                        Co=dataset[dataset$treat!=1,][,X],
                        weights.Co=mat$w[0:250*r],
                        weights.Tr=mat$w[(1+250*r):N],
                        ks=TRUE,
                        nboots=1000) 
    
    # Difference in mean, Ratio fo variance and KS
    
    p0<-abs(info_X$sdiff)
    p1<-info_X$var.ratio
    p2<-info_X$ks$ks.boot.pvalue
    info[,X]<-c(p0,p1,p2)
  }
  
  #We then compute the ATT after the matching
  
  est <- att(mat, Y ~ treat , data = dataset)#,  model="linear-RE")
  
  param_gen <-list("ATT"=est$att.model[,"treat"][[1]],
                   #"Std_Error"=est$att.model[,"treat"][[2]],
                   #"t_value"=est$att.model[,"treat"][[3]],
                   #"p-value"=est$att.model[,"treat"][[4]],
                   "drop_ratio_Co"=(mat$tab[2,]/mat$tab[1,])[[1]],
                   "drop_ratio_Tr"=(mat$tab[2,]/mat$tab[1,])[[2]])
  
  return(list("info_covar"=info,"param_gen"=param_gen))
}


#################################  PROPENSITY MATCHING ################################# 

## Propensity Matching (with logistic regression, stepAIC and Random Forest)



matching_prop_logit<-function(dataset){
  
  #Estimation prop score using logistic regression
  
  ps.logit <- glm(treat ~ X1+X2+X3+X4+X5+X6+X7+X8,
                  family = binomial(link="logit"),
                  data = dataset)
  
  dataset$pscore <- ps.logit$fitted
  
  #Matching
  Y <- dataset$Y  
  Tr <- dataset$treat 
  X <- dataset$pscore  
  Match.out <- Match(Y = Y,
                     Tr = Tr,
                     estimand="ATT",
                     M=1,
                     replace = TRUE,
                     ties = TRUE,
                     caliper = 0.15, X)
  
  #On récupère les paramètres d'intérêt pour chaque covariable
  X_list <- names(dataset)
  X_list <- X_list[X_list != "Y" & X_list != "treat"& X_list != "pscore"]
  
  #We create a dataframe with param as columns and observations as ligns
  param <- c("sdiff","ratioVar","KS")
  info <- data.frame(matrix(0,
                            nrow = length(param),
                            ncol=length(X_list)),
                     row.names = param)
  
  #We iter for each covariate 
  for(X in X_list) {
    
    #We compute quantities of interest for each covariate
    
    info_X <- balanceUV(Tr=dataset[Match.out$index.treat,X],
                        Co=dataset[Match.out$index.control,X],
                        weights=Match.out$weights,
                        ks=TRUE,
                        nboots=1000) 
    
    # Difference in mean, Ratio fo variance and KS
    
    p0<-abs(info_X$sdiff)
    p1<-info_X$var.ratio
    p2<-info_X$ks$ks.boot.pvalue
    info[,X]<-c(p0,p1,p2)
  }
  
  #We then compute the ATT after the matching
  
  param_gen <-list("ATT"=Match.out$est,
                   #"Std_Error"=Match.out$se.standard,
                   #"t_value"=est$att.model[,"treat"][[3]],
                   #"p-value"=est$att.model[,"treat"][[4]],
                   "drop_ratio_Co"=Match.out$wnobs/Match.out$orig.treated.nobs,
                   "drop_ratio_Tr"=Match.out$wnobs/Match.out$orig.treated.nobs)
  
  return(list("info_covar"=info,"param_gen"=param_gen))
}


matching_prop_AIC<-function(dataset){
  
  #Estimation prop score using backward AIC
  
  int <-glm(treat~ X1+X2+X3+X4+X5+X6+X7+X8,
            family = binomial(link="logit"),
            data = dataset)
  
  ps_AIC <- step(int, direction="backward")
  dataset$pscore <- ps_AIC$fitted
  
  #Matching
  Y <- dataset$Y  
  Tr <- dataset$treat 
  X <- dataset$pscore  
  Match.out <- Match(Y = Y,
                     Tr = Tr,
                     estimand="ATT",
                     M=1,
                     replace = TRUE,
                     ties = TRUE,
                     caliper = 0.15, X)
  
  #On récupère les paramètres d'intérêt pour chaque covariable
  X_list <- names(dataset)
  X_list <- X_list[X_list != "Y" & X_list != "treat"& X_list != "pscore"]
  
  #We create a dataframe with param as columns and observations as ligns
  param <- c("sdiff","ratioVar","KS")
  info <- data.frame(matrix(0,
                            nrow = length(param),
                            ncol=length(X_list)),
                     row.names = param)
  
  #We iter for each covariate 
  for(X in X_list) {
    
    #We compute quantities of interest for each covariate
    
    info_X <- balanceUV(Tr=dataset[Match.out$index.treat,X],
                        Co=dataset[Match.out$index.control,X],
                        weights=Match.out$weights,
                        ks=TRUE,
                        nboots=1000) 
    
    # Difference in mean, Ratio fo variance and KS
    
    p0<-abs(info_X$sdiff)
    p1<-info_X$var.ratio
    p2<-info_X$ks$ks.boot.pvalue
    info[,X]<-c(p0,p1,p2)
  }
  
  #We then compute the ATT after the matching
  
  param_gen <-list("ATT"=Match.out$est,
                   #"Std_Error"=est$att.model[,"treat"][[2]],
                   #"t_value"=est$att.model[,"treat"][[3]],
                   #"p-value"=est$att.model[,"treat"][[4]],
                   "drop_ratio_Co"=Match.out$wnobs/Match.out$orig.treated.nobs,
                   "drop_ratio_Tr"=Match.out$wnobs/Match.out$orig.treated.nobs)
  
  return(list("info_covar"=info,"param_gen"=param_gen))
}

matching_prop_RF<-function(dataset){
  
  #Estimation prop score using random Forest
  ps.RF <-randomForest(treat ~ X1+X2+X3+X4+X5+X6+X7+X8,
                       data=dataset,
                       ntree=100)
  dataset$pscore <- ps.RF$predicted
  
  #Matching
  Y <- dataset$Y  
  Tr <- dataset$treat 
  X <- dataset$pscore  
  Match.out <- Match(Y = Y,
                     Tr = Tr,
                     estimand="ATT",
                     M=1,
                     replace = TRUE,
                     ties = TRUE,
                     caliper = 0.15, X)
  
  #On récupère les paramètres d'intérêt pour chaque covariable
  X_list <- names(dataset)
  X_list <- X_list[X_list != "Y" & X_list != "treat"& X_list != "pscore"]
  
  #We create a dataframe with param as columns and observations as ligns
  param <- c("sdiff","ratioVar","KS")
  info <- data.frame(matrix(0,
                            nrow = length(param),
                            ncol=length(X_list)),
                     row.names = param)
  
  #OWe iter for each covariate 
  for(X in X_list) {
    
    #We compute quantities of interest for each covariate
    
    info_X <- balanceUV(Tr=dataset[Match.out$index.treat,X],
                        Co=dataset[Match.out$index.control,X],
                        weights=Match.out$weights,
                        ks=TRUE,
                        nboots=1000) 
    
    # Difference in mean, Ratio fo variance and KS
    
    p0<-abs(info_X$sdiff)
    p1<-info_X$var.ratio
    p2<-info_X$ks$ks.boot.pvalue
    info[,X]<-c(p0,p1,p2)
  }
  
  #We then compute the ATT after the matching
  
  param_gen <-list("ATT"=Match.out$est,
                   #"Std_Error"=est$att.model[,"treat"][[2]],
                   #"t_value"=est$att.model[,"treat"][[3]],
                   #"p-value"=est$att.model[,"treat"][[4]],
                   "drop_ratio_Co"=Match.out$wnobs/Match.out$orig.treated.nobs,
                   "drop_ratio_Tr"=Match.out$wnobs/Match.out$orig.treated.nobs)
  
  return(list("info_covar"=info,"param_gen"=param_gen))
}


#################################  CPARDINALITY MATCHING ################################# 


matching_card <- function(dataset,
                          tolerance=c(0,0,0,0,0.01,0.01,0.01,0.01),
                          t_max=60) {
  
  dataset <- dataset[order(dataset$treat, decreasing = TRUE), ]
  treatment <-  dataset$treat
  
  ### Solver
  solver = list(name = "glpk", t_max, approximate = 0, round_cplex = 0, trace = 0)
  solver2 = list(name = "glpk", t_max, approximate = 0, round_cplex = 0, trace = 0)
  
  ### Step 1: Find the largest sample of matched pairs respecting the covariates balancing rules
  
  # Match
  card_match_step1 <- cardmatch(treatment,
                                mom = list(covs=dataset[c("X1","X2","X3","X4","X5","X6","X7","X8")],
                                           tols=tolerance),
                                solver = solver)
  
  # Indices of the treated units and matched controls
  t_id_1 <- card_match_step1$t_id
  c_id_1 <- card_match_step1$c_id
  
  ### Step 2: Repairs the matches to minimize the sum of distances between individuals
  
  treatment2 <- treatment[c(t_id_1, c_id_1)]      # treatment indicator among the matched
  
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
  treatment3 <- as.data.frame(output_cardinality_matching$treat)  ### IMPORTANT pour faire tourner balanceUV
  
  ### Step 3: computation of the intersting parameters
  
  X_list <- names(dataset)
  X_list <- X_list[X_list != "Y" & X_list != "treat"]
  
  param <- c("sdiff","ratioVar","KS")
  info <- data.frame(matrix(0,
                            nrow = length(param),
                            ncol=length(X_list)),
                     row.names = param)
  
  #We iter for each covariate 
  for(X in X_list) {
    
    #We compute quantities of interest for each covariate
    
    info_X <- balanceUV(output_cardinality_matching[treatment3==1,][,X],
                        output_cardinality_matching[treatment3==0,][,X],
                        weights.Tr=rep(1,length(output_cardinality_matching[treatment3==1,][,X])),
                        weights.Co=rep(1,length(output_cardinality_matching[treatment3==0,][,X])),
                        ks=TRUE,
                        nboots = 1000)
    
    # Difference in mean, Ratio fo variance and KS
    
    p0<-abs(info_X$sdiff)
    p1<-info_X$var.ratio
    p2<-info_X$ks$ks.boot.pvalue
    info[,X]<-c(p0,p1,p2)
  }
  
  #We then compute the ATT after the matching
  
  est <- lm( Y ~ treat , data = output_cardinality_matching)                                                     
  param_ATT <- coef(summary(est))["treat",]
  param_gen <-list("ATT"=param_ATT["Estimate"],
                   #"Std_Error"=param_ATT["Std. Error"],
                   #"t_value"=param_ATT["t value"],
                   #"p-value"=param_ATT[ "Pr(>|t|)"],
                   "drop_ratio_Co"=length(c_id_2)/length(treatment),
                   "drop_ratio_Tr"=length(t_id_2)/length(treatment))
  
  return(list("info_covar"=info,"param_gen"=param_gen))
}




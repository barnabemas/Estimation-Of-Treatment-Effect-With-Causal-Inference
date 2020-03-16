############################### MISSING VALUES GENERATION ############################### 

#The function bellow generate and impute missing values
#missing_mechanism belong to MCAR or MAR missing competely at random vs missing at random
#imput_mechanism belong to delete, mean or Amelia
#dataset is the dataset we want to use

missing_val_modification <- function(dataset,
                                     missing_mechanism,
                                     imput_mechanism,
                                     plot_graphs=FALSE){
  
  #We generate missing values
  data.miss<- produce_NA(dataset,
                         mechanism=missing_mechanism,
                         perc.missing = 0.2)
  data.miss <- data.miss$data.incomp
  
  #We do not want to modify Y and treat
  data.miss$Y<-dataset$Y
  data.miss$treat<-dataset$treat
  
  #If we want to plot the missing values graphs
  if (plot_graphs){
    matrixplot(data.miss, cex.axis = 0.5, interactive = F)
    print(summary(aggr(data.miss, sortVar=TRUE))$combinations)
  }
  
  #We choose the wanted impute mecanism
  
  #WE delete ligns with missing values
  if (imput_mechanism=='delete'){
    data.miss <- data.miss[complete.cases(data.miss),]
  }
  
  #We imput by the mean
  if (imput_mechanism=='mean'){
    # Imputation of quantitative columns 
    for(i in 5:8) {
      data.miss[ , i][is.na(data.miss[ , i])] <- mean(data.miss[ , i], na.rm = TRUE)
    }
    # Imputation of qualitatative columns 
    for (i in 1:4){
      data.miss[ , i][is.na(data.miss[ , i])] <- if((summary(data.miss[ , i])[4]>0.5)) 1 else 0
    }
  }
  
  #We use Amelia package for imputation
  if (imput_mechanism =='Amelia'){
    
    data.miss <- amelia(data.miss,m = 5,
                        noms = c('X1','X2','X3','X4'))$imputations$imp5
    return(data.miss)
  }
  
  #We use missMDA package for imputation
  if (imput_mechanism == 'imputeFAMD'){
    data.miss <- imputeFAMD(data.miss)$completeObs
    
    for(i in 1:4){
      for(j in 1:500){
        if (data.miss[,i][j] < 0.5){
          data.miss[,i][j] <- 0.0
        }
        else {
          data.miss[,i][j] <- 1.0
        }
      }
    }
  }
  
  return(data.miss)
}


############################ SIMULATION WITH MISSING VALUES ############################

#This function is used to run numerous simulation of each matching methods with missing values
#generation and imputation
#Dataset_function is to be chosen among dataset_1,dataset_2 or dataset_3
#f_function is to be chosen among f_1,f_2 or f_3
#This gives us nine different possible models 
#missing_mechanism belong to MCAR or MAR missing competely at random vs missing at random
#imput_mechanism belong to delete, mean or Amelia
#dataset is the dataset we want to use

global_simulation_with_NA <- function(dataset_function,
                                      f_function,
                                      nb_observation,
                                      missing_mechanism,
                                      imput_mechanism){
  
  methods_df <- data.frame("Before_Match" = rep(0,nb_observation),
                           "CEM" = rep(0,nb_observation),
                           "Prop_logit" = rep(0,nb_observation),
                           #"Prop_AIC" = rep(0,nb_observation),
                           "Prop_RF" = rep(0,nb_observation),
                           "Card_0.1" = rep(0,nb_observation),
                           "Card_0.01" = rep(0,nb_observation))
  
  #A dataframe for each covariate of interest
  info_X1 <- list("sdiff"=methods_df,
                  "ratioVar"=methods_df,
                  "KS"=methods_df)
  
  info_X3 <- copy(info_X1)
  info_X5 <- copy(info_X1)
  info_X7 <- copy(info_X1)
  
  #A dataframe for general parameters
  info_gen  <- list("ATT"=methods_df,
                    "drop_ratio_Co"=methods_df,
                    "drop_ratio_Tr"=methods_df)
  
  for (i in 1:nb_observation){
    
    #To know the progress of the simulation
    print(i)
    
    #We generate a random dataset following the chosen model 
    #and with missing values treatment
    dataset<-missing_val_modification(dataset_function(f_function),
                                      missing_mechanism,
                                      imput_mechanism)
    
    #We compute the results of each matching method for this dataset
    before_eval <- matching_before(dataset)
    cem_eval <- matching_cem(dataset)
    prop_logit_eval <- matching_prop_logit(dataset)
    #prop_AIC_eval <- matching_prop_AIC(dataset)
    prop_RF_eval <- matching_prop_RF(dataset)
    card_eval_0.1 <- matching_card(dataset,
                                   tolerance = c(0,0,0,0,0.1,0.1,0.1,0.1))
    card_eval_0.01 <- matching_card(dataset,
                                    tolerance = c(0,0,0,0,0.01,0.01,0.01,0.01))
    
    
    #We update info_X1
    info_X1$sdiff$Before_Match[i] <-before_eval$info_covar$X1[1]
    info_X1$ratioVar$Before_Match[i] <-before_eval$info_covar$X1[2]
    info_X1$KS$Before_Match[i] <-before_eval$info_covar$X1[3]
    
    info_X1$sdiff$CEM[i] <-cem_eval$info_covar$X1[1]
    info_X1$ratioVar$CEM[i] <-cem_eval$info_covar$X1[2]
    info_X1$KS$CEM[i] <-cem_eval$info_covar$X1[3]
    
    info_X1$sdiff$Prop_logit[i] <-prop_logit_eval$info_covar$X1[1]
    info_X1$ratioVar$Prop_logit[i] <-prop_logit_eval$info_covar$X1[2]
    info_X1$KS$Prop_logit[i] <-prop_logit_eval$info_covar$X1[3]
    
    #info_X1$sdiff$Prop_AIC[i] <-prop_AIC_eval$info_covar$X1[1]
    #info_X1$ratioVar$Prop_AIC[i] <-prop_AIC_eval$info_covar$X1[2]
    #info_X1$KS$Prop_AIC[i] <-prop_AIC_eval$info_covar$X1[3]
    
    info_X1$sdiff$Prop_RF[i] <-prop_RF_eval$info_covar$X1[1]
    info_X1$ratioVar$Prop_RF[i] <-prop_RF_eval$info_covar$X1[2]
    info_X1$KS$Prop_RF[i] <-prop_RF_eval$info_covar$X1[3]
    
    info_X1$sdiff$Card_0.1[i] <-card_eval_0.1$info_covar$X1[1]
    info_X1$ratioVar$Card_0.1[i] <-card_eval_0.1$info_covar$X1[2]
    info_X1$KS$Card_0.1[i] <-card_eval_0.1$info_covar$X1[3]
    
    info_X1$sdiff$Card_0.01[i] <-card_eval_0.01$info_covar$X1[1]
    info_X1$ratioVar$Card_0.01[i] <-card_eval_0.01$info_covar$X1[2]
    info_X1$KS$Card_0.01[i] <-card_eval_0.01$info_covar$X1[3]
    
    #We update info_X3
    info_X3$sdiff$Before_Match[i] <-before_eval$info_covar$X3[1]
    info_X3$ratioVar$Before_Match[i] <-before_eval$info_covar$X3[2]
    info_X3$KS$Before_Match[i] <-before_eval$info_covar$X3[3]
    
    info_X3$sdiff$CEM[i] <-cem_eval$info_covar$X3[1]
    info_X3$ratioVar$CEM[i] <-cem_eval$info_covar$X3[2]
    info_X3$KS$CEM[i] <-cem_eval$info_covar$X3[3]
    
    info_X3$sdiff$Prop_logit[i] <-prop_logit_eval$info_covar$X3[1]
    info_X3$ratioVar$Prop_logit[i] <-prop_logit_eval$info_covar$X3[2]
    info_X3$KS$Prop_logit[i] <-prop_logit_eval$info_covar$X3[3]
    
    #info_X3$sdiff$Prop_AIC[i] <-prop_AIC_eval$info_covar$X3[1]
    #info_X3$ratioVar$Prop_AIC[i] <-prop_AIC_eval$info_covar$X3[2]
    #info_X3$KS$Prop_AIC[i] <-prop_AIC_eval$info_covar$X3[3]
    
    info_X3$sdiff$Prop_RF[i] <-prop_RF_eval$info_covar$X3[1]
    info_X3$ratioVar$Prop_RF[i] <-prop_RF_eval$info_covar$X3[2]
    info_X3$KS$Prop_RF[i] <-prop_RF_eval$info_covar$X3[3]
    
    info_X3$sdiff$Card_0.1[i] <-card_eval_0.1$info_covar$X3[1]
    info_X3$ratioVar$Card_0.1[i] <-card_eval_0.1$info_covar$X3[2]
    info_X3$KS$Card_0.1[i] <-card_eval_0.1$info_covar$X3[3]
    
    info_X3$sdiff$Card_0.01[i] <-card_eval_0.01$info_covar$X3[1]
    info_X3$ratioVar$Card_0.01[i] <-card_eval_0.01$info_covar$X3[2]
    info_X3$KS$Card_0.01[i] <-card_eval_0.01$info_covar$X3[3]
    
    #We update info_X5
    info_X5$sdiff$Before_Match[i] <-before_eval$info_covar$X5[1]
    info_X5$ratioVar$Before_Match[i] <-before_eval$info_covar$X5[2]
    info_X5$KS$Before_Match[i] <-before_eval$info_covar$X5[3]
    
    info_X5$sdiff$CEM[i] <-cem_eval$info_covar$X5[1]
    info_X5$ratioVar$CEM[i] <-cem_eval$info_covar$X5[2]
    info_X5$KS$CEM[i] <-cem_eval$info_covar$X5[3]
    
    info_X5$sdiff$Prop_logit[i] <-prop_logit_eval$info_covar$X5[1]
    info_X5$ratioVar$Prop_logit[i] <-prop_logit_eval$info_covar$X5[2]
    info_X5$KS$Prop_logit[i] <-prop_logit_eval$info_covar$X5[3]
    
    #info_X5$sdiff$Prop_AIC[i] <-prop_AIC_eval$info_covar$X5[1]
    #info_X5$ratioVar$Prop_AIC[i] <-prop_AIC_eval$info_covar$X5[2]
    #info_X5$KS$Prop_AIC[i] <-prop_AIC_eval$info_covar$X5[3]
    
    info_X5$sdiff$Prop_RF[i] <-prop_RF_eval$info_covar$X5[1]
    info_X5$ratioVar$Prop_RF[i] <-prop_RF_eval$info_covar$X5[2]
    info_X5$KS$Prop_RF[i] <-prop_RF_eval$info_covar$X5[3]
    
    info_X5$sdiff$Card_0.1[i] <-card_eval_0.1$info_covar$X5[1]
    info_X5$ratioVar$Card_0.1[i] <-card_eval_0.1$info_covar$X5[2]
    info_X5$KS$Card_0.1[i] <-card_eval_0.1$info_covar$X5[3]
    
    info_X5$sdiff$Card_0.01[i] <-card_eval_0.01$info_covar$X5[1]
    info_X5$ratioVar$Card_0.01[i] <-card_eval_0.01$info_covar$X5[2]
    info_X5$KS$Card_0.01[i] <-card_eval_0.01$info_covar$X5[3]
    
    #We update info_X7
    info_X7$sdiff$Before_Match[i] <-before_eval$info_covar$X7[1]
    info_X7$ratioVar$Before_Match[i] <-before_eval$info_covar$X7[2]
    info_X7$KS$Before_Match[i] <-before_eval$info_covar$X7[3]
    
    info_X7$sdiff$CEM[i] <-cem_eval$info_covar$X7[1]
    info_X7$ratioVar$CEM[i] <-cem_eval$info_covar$X7[2]
    info_X7$KS$CEM[i] <-cem_eval$info_covar$X7[3]
    
    info_X7$sdiff$Prop_logit[i] <-prop_logit_eval$info_covar$X7[1]
    info_X7$ratioVar$Prop_logit[i] <-prop_logit_eval$info_covar$X7[2]
    info_X7$KS$Prop_logit[i] <-prop_logit_eval$info_covar$X7[3]
    
    #info_X7$sdiff$Prop_AIC[i] <-prop_AIC_eval$info_covar$X7[1]
    #info_X7$ratioVar$Prop_AIC[i] <-prop_AIC_eval$info_covar$X7[2]
    #info_X7$KS$Prop_AIC[i] <-prop_AIC_eval$info_covar$X7[3]
    
    info_X7$sdiff$Prop_RF[i] <-prop_RF_eval$info_covar$X7[1]
    info_X7$ratioVar$Prop_RF[i] <-prop_RF_eval$info_covar$X7[2]
    info_X7$KS$Prop_RF[i] <-prop_RF_eval$info_covar$X7[3]
    
    info_X7$sdiff$Card_0.1[i] <-card_eval_0.1$info_covar$X7[1]
    info_X7$ratioVar$Card_0.1[i] <-card_eval_0.1$info_covar$X7[2]
    info_X7$KS$Card_0.1[i] <-card_eval_0.1$info_covar$X7[3]
    
    info_X7$sdiff$Card_0.01[i] <-card_eval_0.01$info_covar$X7[1]
    info_X7$ratioVar$Card_0.01[i] <-card_eval_0.01$info_covar$X7[2]
    info_X7$KS$Card_0.01[i] <-card_eval_0.01$info_covar$X7[3]
    
    
    #We update info_gen
    
    info_gen$ATT$Before_Match[i] <-before_eval$param_gen$ATT
    info_gen$drop_ratio_Co$Before_Match[i] <-before_eval$param_gen$drop_ratio_Co
    info_gen$drop_ratio_Tr$Before_Match[i] <-before_eval$param_gen$drop_ratio_Tr
    
    info_gen$ATT$CEM[i] <-cem_eval$param_gen$ATT
    info_gen$drop_ratio_Co$CEM[i] <-cem_eval$param_gen$drop_ratio_Co
    info_gen$drop_ratio_Tr$CEM[i] <-cem_eval$param_gen$drop_ratio_Tr
    
    info_gen$ATT$Prop_logit[i] <-prop_logit_eval$param_gen$ATT
    info_gen$drop_ratio_Co$Prop_logit[i] <-prop_logit_eval$param_gen$drop_ratio_Co
    info_gen$drop_ratio_Tr$Prop_logit[i] <-prop_logit_eval$param_gen$drop_ratio_Tr
    
    #info_gen$ATT$Prop_AIC[i] <-prop_AIC_eval$param_gen$ATT
    #info_gen$drop_ratio_Co$Prop_AIC[i] <-prop_AIC_eval$param_gen$drop_ratio_Co
    #info_gen$drop_ratio_Tr$Prop_AIC[i] <-prop_AIC_eval$param_gen$drop_ratio_Tr
    
    info_gen$ATT$Prop_RF[i] <-prop_RF_eval$param_gen$ATT
    info_gen$drop_ratio_Co$Prop_RF[i] <-prop_RF_eval$param_gen$drop_ratio_Co
    info_gen$drop_ratio_Tr$Prop_RF[i] <-prop_RF_eval$param_gen$drop_ratio_Tr
    
    info_gen$ATT$Card_0.1[i] <-card_eval_0.1$param_gen$ATT
    info_gen$drop_ratio_Co$Card_0.1[i] <-card_eval_0.1$param_gen$drop_ratio_Co
    info_gen$drop_ratio_Tr$Card_0.1[i] <-card_eval_0.1$param_gen$drop_ratio_Tr
    
    info_gen$ATT$Card_0.01[i] <-card_eval_0.01$param_gen$ATT
    info_gen$drop_ratio_Co$Card_0.01[i] <-card_eval_0.01$param_gen$drop_ratio_Co
    info_gen$drop_ratio_Tr$Card_0.01[i] <-card_eval_0.01$param_gen$drop_ratio_Tr   
    
  }
  
  return(list("info_X1"=info_X1,
              "info_X3"=info_X3,
              "info_X5"=info_X5,
              "info_X7"=info_X7,
              "info_gen"=info_gen))
}

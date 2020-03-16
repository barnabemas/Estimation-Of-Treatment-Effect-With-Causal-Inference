################################# GLOBAL SIMULATION ################################# 

#This function is used to run numerous simulation of each matching methods
#For given model in order to estimate efficency
#Dataset_function is to be chosen among dataset_1,dataset_2 or dataset_3
#f_function is to be chosen among f_1,f_2 or f_3
#This gives us nine different possible models 

global_simulation <- function(dataset_function,f_function,nb_observation){
  
  #We enter here each method of matching used
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
    dataset<-dataset_function(f_function)
    
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

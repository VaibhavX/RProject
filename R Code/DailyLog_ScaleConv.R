#Reading the Data 46 col
dl <- read.csv("/Users/vaibhavx/Desktop/R_Data/daily_log_merge_46col_2017_09_21.csv", header = T, stringsAsFactors = FALSE)
#Reading 52 Col Data
dl <- read.csv("/Users/vaibhavx/Desktop/R_Data/daily_log_merge_52col_2017_09_21.csv", header = T, stringsAsFactors = FALSE)

#Col Q0_Welcome
for(i in 1:nrow(dl))
{
  if(dl$Q0_welcome[i]=="question is not displayed"){dl$Q0_welcome[i] <- 999}
}

#Col Q1_waketime
for (i in 1:nrow(dl))
{
  if(dl$Q1_waketime[i]=="question is not displayed"){dl$Q1_waketime[i] <- 999}
}

#Col Q2_sleeptime
for (i in 1:nrow(dl))
{
  if(dl$Q2_sleeptime[i]=="question is not displayed"){dl$Q2_sleeptime[i] <- 999}
}

#Q3_SleepLoc
for(i in 1:nrow(dl))
{
  ifelse(dl$Q3_sleeploc[i]=="My own apartment/residence", dl$Q3_sleeploc[i] <- 1, 
         ifelse(dl$Q3_sleeploc[i]=="Home of someone I know (family, friend, partner, etc.", dl$Q3_sleeploc[i]<-2,
                ifelse(dl$Q3_sleeploc[i]=="Stranger's home/residence", dl$Q3_sleeploc[i] <-3,
                       ifelse(dl$Q3_sleeploc[i]=="Shelter/transitional living program", dl$Q3_sleeploc[i] <-4,
                              ifelse(dl$Q3_sleeploc[i]=="Hotel/motel", dl$Q3_sleeploc[i] <-5,
                                     ifelse(dl$Q3_sleeploc[i]=="Abandoned building, squat, public transit, or other place inside", dl$Q3_sleeploc[i]<-6,
                                            ifelse(dl$Q3_sleeploc[i]=="Street, park, beach, roof, or other place <u>outside</u>", dl$Q3_sleeploc[i]<-7,
                                                   ifelse(dl$Q3_sleeploc[i]=="Car, RV, van, or other vehicle", dl$Q3_sleeploc[i]<-8,
                                                          ifelse(dl$Q3_sleeploc[i]=="Other", dl$Q3_sleeploc[i]<-77, dl$Q3_sleeploc[i]<-999)))))))))
}

#Q3_1_SleepOther
for(i in 1:nrow(dl))
{
  if(dl$Q3_1_sleeploc_other[i]=="question is not displayed") {dl$Q3_1_sleeploc_other[i] <- 999}
}

#Q3_b_sleepQuality
for (i in 1:nrow(dl))
{
  ifelse(dl$Q3_b_sleep_quality[i]=="Very poor", dl$Q3_b_sleep_quality[i] <- 1, 
         ifelse(dl$Q3_b_sleep_quality[i]=="Poor", dl$Q3_b_sleep_quality[i] <-2,
                ifelse(dl$Q3_b_sleep_quality[i]=="Fair", dl$Q3_b_sleep_quality[i] <-3,
                       ifelse(dl$Q3_b_sleep_quality[i]=="Good", dl$Q3_b_sleep_quality[i] <-4,
                              ifelse(dl$Q3_b_sleep_quality[i]=="Very good", dl$Q3_b_sleep_quality[i] <-5,
                                     ifelse(dl$Q3_b_sleep_quality[i]=="question is not displayed", dl$Q3_b_sleep_quality[i] <-999, print(i)))))))
}

#Q4_soccore
for(i in 1:nrow(dl))
{
  ifelse(dl$Q4_0_soccore[i]=="I did not interact with any of these people <b>yesterday</b>",dl$Q4_0_soccore[i] <-0,
         ifelse(dl$Q4_0_soccore[i]=="question is not displayed", dl$Q4_0_soccore[i]<-999, print(i))) 
}

#Q4_1_subchk
for(i in 1:nrow(dl))
{
  ifelse(dl$Q4a_substances[i]=="Alcohol", dl$Q4a_substances[i] <-1,
         ifelse(dl$Q4a_substances[i]=="Marijuana", dl$Q4a_substances[i] <-2,
                ifelse(dl$Q4a_substances[i]=="Synthetic marijuana (K2, Spice, etc.)",dl$Q4a_substances[i] <- 3,
                       ifelse(dl$Q4a_substances[i]=="Meth", dl$Q4a_substances[i] <-4,
                              ifelse(dl$Q4a_substances[i]=="Prescription drugs, not as prescribed (Rx cough syrup, Oxycontin, Xanax, etc.)", dl$Q4a_substances[i] <-5,
                                     ifelse(dl$Q4a_substances[i]=="Other illicit drug", dl$Q4a_substances[i] <-6,
                                            ifelse(dl$Q4a_substances[i]=="I did not use any drugs yesterday", dl$Q4a_substances[i] <-0,
                                                   ifelse(dl$Q4a_substances[i]=="question is not displayed", dl$Q4a_substances[i]<-999, print(i)))))))))
}

#Q4_2_subchk
for( i in 1:nrow(dl))
{
  ifelse(dl$Q4b_substances[i]=='Ecstacy / MDMA / "Molly"', dl$Q4b_substances[i]<-7,
         ifelse(dl$Q4b_substances[i]=="Hallucinogens/psychedelics", dl$Q4b_substances[i]<-8,
                ifelse(dl$Q4b_substances[i]=="Heroin", dl$Q4b_substances[i]<-9,
                       ifelse(dl$Q4b_substances[i]=="Cocaine or crack", dl$Q4b_substances[i]<-10,
                              ifelse(dl$Q4b_substances[i]=="Something else not listed here", dl$Q4b_substances[i]<-11,
                                     ifelse(dl$Q4b_substances[i]=="question is not displayed", dl$Q4b_substances[i]<-999, print(i)))))))
}

#Q4_17_b_other
for( i in 1:nrow(dl))
{
  ifelse(dl$Q4_17_b_other[i]=="Swallowed", dl$Q4_17_b_other[i] <-1, 
         ifelse(dl$Q4_17_b_other[i]=="Smoked", dl$Q4_17_b_other[i]<-2,
                ifelse(dl$Q4_17_b_other[i]=="Snorted", dl$Q4_17_b_other[i] <-3,
                       ifelse(dl$Q4_17_b_other[i]=="Injected", dl$Q4_17_b_other[i] <-4,
                              ifelse(dl$Q4_17_b_other[i]=="Other", dl$Q4_17_b_other[i] <-77, 
                                     ifelse(dl$Q4_17_b_other[i]=="question is not displayed", dl$Q4_17_b_other[i]<-999, print(i)))))))
}


#Q4_2_alcohol
for(i in 1:nrow(dl))
{
  ifelse(dl$Q4_2_alcohol[i]=="question is not displayed", dl$Q4_2_alcohol[i] <-999, dl$Q4_2_alcohol[i] <- strsplit(dl$Q4_2_alcohol[i], ":", fixed=TRUE)[[1]][2])
}

#Q4_2a_alcohol
for( i in 1:nrow(dl))
{
  ifelse(dl$Q4_2_a_alcohol[i]=="No", dl$Q4_2_a_alcohol[i]<-0,
         ifelse(dl$Q4_2_a_alcohol[i]=="Yes", dl$Q4_2_a_alcohol[i]<-1,
                ifelse(dl$Q4_2_a_alcohol[i]=="Not sure", dl$Q4_2_a_alcohol[i]<-99,
                       ifelse(dl$Q4_2_a_alcohol[i]=="question is not displayed", dl$Q4_2_a_alcohol[i] <-999, print(i)))))
}

#Q4_2b_alcohol
for(i in 1:nrow(dl))
{
  ifelse(dl$Q4_2_b_alcohol_who[i]=="I did not drink alcohol with any of these people <b>yesterday</b>", dl$Q4_2_b_alcohol_who[i] <- 0,
         ifelse(dl$Q4_2_b_alcohol_who[i]=="question is not displayed", dl$Q4_2_b_alcohol_who[i] <- 999, print(i)))
}

#Q4_3_marijuana
for(i in 1:nrow(dl))
{
  ifelse(dl$Q4_3_marijuana[i]=="question is not displayed", dl$Q4_3_marijuana[i] <-999, dl$Q4_3_marijuana[i] <- strsplit(dl$Q4_3_marijuana[i],":", fixed = TRUE)[[1]][2])
}

#Q4_3a_mari
for(i in 1:nrow(dl))
{
  ifelse(dl$Q4_3_a_marijuana[i]=="No", dl$Q4_3_a_marijuana[i]<-0,
         ifelse(dl$Q4_3_a_marijuana[i]=="Yes", dl$Q4_3_a_marijuana[i] <-1,
                ifelse(dl$Q4_3_a_marijuana[i]=="Not sure", dl$Q4_3_a_marijuana[i] <-99,
                       ifelse(dl$Q4_3_a_marijuana[i]=="question is not displayed", dl$Q4_3_a_marijuana[i] <-999, print(i)))))
}

#Q4_3b_mari
for (i in 1:nrow(dl))
{
  ifelse(dl$Q4_3_b_marijuana_who[i]=="I did not use marijuana or marijuana products with any of these people <b>yesterday</b>", dl$Q4_3_b_marijuana_who[i]<-0,
     ifelse(dl$Q4_3_b_marijuana_who[i]=="question is not displayed", dl$Q4_3_b_marijuana_who[i] <-999, print(i)))
}
  
#Q4_6_synthmj
for(i in 1:nrow(dl))
{
  ifelse(dl$Q4_6_synthmj[i]=="question is not displayed", dl$Q4_6_synthmj[i]<-999, dl$Q4_6_synthmj[i] <- strsplit(dl$Q4_6_synthmj[i],":", fixed = TRUE)[[1]][2])
}

#Q4_6b_synthmj
for(i in 1:nrow(dl))
{
  ifelse(dl$Q4_6_b_synthmj_who[i]=="I did not use synthetic marijuana with any of these people yesterday", dl$Q4_6_b_synthmj_who[i] <-0,
         ifelse(dl$Q4_6_b_synthmj_who[i]=="question is not displayed", dl$Q4_6_b_synthmj_who[i] <- 999, print(i)))
}

#Q4_4_meth
for(i in 1:nrow(dl))
{
  ifelse(dl$Q4_4_meth[i]=="question is not displayed", dl$Q4_4_meth[i] <-999, dl$Q4_4_meth[i]<- strsplit(dl$Q4_4_meth[i],":",fixed = TRUE)[[1]][2])
}

#Q4_4a_meth
for(i in 1:nrow(dl))
{
  ifelse(dl$Q4_4_a_meth[i]=="Swallowed", dl$Q4_4_a_meth[i]<-1,
         ifelse(dl$Q4_4_a_meth[i]=="Smoked", dl$Q4_4_a_meth[i]<-2,
                ifelse(dl$Q4_4_a_meth[i]=="Snorted", dl$Q4_4_a_meth[i]<-3,
                       ifelse(dl$Q4_4_a_meth[i]=="Injected", dl$Q4_4_a_meth[i]<-4,
                              ifelse(dl$Q4_4_a_meth[i]=="question is not displayed", dl$Q4_4_a_meth[i]<-999, print(i))))))
}

#Q4_4b_meth
for(i in 1:nrow(dl))
{
  ifelse(dl$Q4_4_b_meth_who[i]=="I did not use meth with any of these people yesterday", dl$Q4_4_b_meth_who[i] <-0,
         ifelse(dl$Q4_4_b_meth_who[i]=="question is not displayed", dl$Q4_4_b_meth_who[i] <-999, print(i)))
}

#Q4_8_prescription
for(i in 1:nrow(dl))
{
  ifelse(dl$Q4_8_prescription[i]=="question is not displayed", dl$Q4_8_prescription[i] <-999, dl$Q4_8_prescription[i] <- strsplit(dl$Q4_8_prescription[i], ":", fixed = TRUE)[[1]][2])
}

#Q4_8a_pres
for(i in 1:nrow(dl))
{
  ifelse(dl$Q4_8_a_prescription[i]=="Swallowed", dl$Q4_8_a_prescription[i]<-1,
         ifelse(dl$Q4_8_a_prescription[i]=="Smoked", dl$Q4_8_a_prescription[i]<-2,
                ifelse(dl$Q4_8_a_prescription[i]=="Snorted", dl$Q4_8_a_prescription[i]<-3,
                       ifelse(dl$Q4_8_a_prescription[i]=="Injected", dl$Q4_8_a_prescription[i]<-4,
                              ifelse(dl$Q4_8_a_prescription[i]=="question is not displayed", dl$Q4_8_a_prescription[i]<-999, print(i))))))
}

#Q4_8b_pres
for(i in 1:nrow(dl))
{
  ifelse(dl$Q4_8_b_prescription_who[i]=="I did not use prescription drugs,not as prescribed with any of these people <b>yesterday</b>", dl$Q4_8_b_prescription_who[i]<-0,
         ifelse(dl$Q4_8_b_prescription_who[i]=="question is not displayed", dl$Q4_8_b_prescription_who[i]<-999, print(i)))
}

#Q4_5_mdma
for(i in 1:nrow(dl))
{
  ifelse(dl$Q4_5_mdma[i]=="question is not displayed", dl$Q4_5_mdma[i] <-999, dl$Q4_5_mdma[i] <- strsplit(dl$Q4_5_mdma[i], ":", fixed = TRUE)[[1]][2])
}

#Q4_5a_mdma
for(i in 1:nrow(dl))
{
  ifelse(dl$Q4_5_a_mdma[i]=="Swallowed", dl$Q4_5_a_mdma[i] <-1,
         ifelse(dl$Q4_5_a_mdma[i]=="Smoked", dl$Q4_5_a_mdma[i] <-2,
                ifelse(dl$Q4_5_a_mdma[i]=="Snorted", dl$Q4_5_a_mdma[i]<-3,
                       ifelse(dl$Q4_5_a_mdma[i]=="Infected", dl$Q4_5_a_mdma[i]<-4,
                              ifelse(dl$Q4_5_a_mdma[i]=="question is not displayed", dl$Q4_5_a_mdma[i]<-999, print(i))))))
}

#Q4_5b_mdma
for(i in 1:nrow(dl))
{
  ifelse(dl$Q4_5_b_mdma_who[i]=='I did not use Ecstacy/MDMA/"Molly" with any of these people <b>yesterday</b>', dl$Q4_5_b_mdma_who[i]<-0,
         ifelse(dl$Q4_5_b_mdma_who[i]=="question is not displayed", dl$Q4_5_b_mdma_who[i]<-999, print(i)))
}

#Q4_7_hall
for(i in 1:nrow(dl))
{
  ifelse(dl$Q4_7_halluc[i]=="question is not displayed", dl$Q4_7_halluc[i] <-999, dl$Q4_7_halluc[i] <- strsplit(dl$Q4_7_halluc[i],":",fixed = TRUE)[[1]][2])
}

#Q4_7b_hall
for(i in 1:nrow(dl))
{
  ifelse(dl$Q4_7_b_halluc_who[i]=="I did not use hallucinogens/psychedelics with any of these people yesterday", dl$Q4_7_b_halluc_who[i]<-0,
         ifelse(dl$Q4_7_b_halluc_who[i]=="question is not displayed", dl$Q4_7_b_halluc_who[i]<-999, print(i)))
}

#Q4_9_heroin
for(i in 1:nrow(dl))
{
  ifelse(dl$Q4_9_heroin[i]=="question is not displayed", dl$Q4_9_heroin[i]<-999, dl$Q4_9_heroin[i]<- strsplit(dl$Q4_9_heroin[i],":",fixed = TRUE)[[1]][2])
}

#Q4_9a_heroin
for(i in 1:nrow(dl))
{
  ifelse(dl$Q4_9_a_heroin[i]=="Smoked", dl$Q4_9_a_heroin[i]<-2,
         ifelse(dl$Q4_9_a_heroin[i]=="Snorted", dl$Q4_9_a_heroin[i]<-3,
                ifelse(dl$Q4_9_a_heroin[i]=="Injected", dl$Q4_9_a_heroin[i]<-4,
                       ifelse(dl$Q4_9_a_heroin[i]=="question is not displayed", dl$Q4_9_a_heroin[i]<-999, print(i)))))
}

#Q4_9b_heroin
for(i in 1:nrow(dl))
{
  ifelse(dl$Q4_9_b_heroin_who[i]=="I did not use heroin with any of these people yesterday", dl$Q4_9_b_heroin_who[i]<-0,
         ifelse(dl$Q4_9_b_heroin_who[i]=="question is not displayed", dl$Q4_9_b_heroin_who[i]<-999, print(i)))
}

#Q4_10a_coke
for(i in 1:nrow(dl))
{
  ifelse(dl$Q4_10_a_coke_type[i]=="Powdered cocaine", dl$Q4_10_a_coke_type[i]<-1,
         ifelse(dl$Q4_10_a_coke_type[i]=="Crack or freebase cocaine", dl$Q4_10_a_coke_type[i]<-2,
                ifelse(dl$Q4_10_a_coke_type[i]=="Both", dl$Q4_10_a_coke_type[i]<-3,
                       ifelse(dl$Q4_10_a_coke_type[i]=="question is not displayed", dl$Q4_10_a_coke_type[i]<-999, print(i)))))
}

#Q4_10b_coke
for(i in 1:nrow(dl))
{
  ifelse(dl$Q4_10_a_coke[i]=="question is not displayed", dl$Q4_10_a_coke[i]<-999, dl$Q4_10_a_coke[i]<- strsplit(dl$Q4_10_a_coke[i],":",fixed = TRUE)[[1]][2])
}

#Q4_10c_coke
for(i in 1:nrow(dl))
{
  ifelse(dl$Q4_10_b_coke[i]=="Smoked", dl$Q4_10_b_coke[i]<-2,
         ifelse(dl$Q4_10_b_coke[i]=="Snorted", dl$Q4_10_b_coke[i]<-3,
                ifelse(dl$Q4_10_b_coke[i]=="Injected", dl$Q4_10_b_coke[i]<-4,
                       ifelse(dl$Q4_10_b_coke[i]=="Orally (swallowed, chewed, rubbed on gums, etc.", dl$Q4_10_b_coke[i]<-5,
                              ifelse(dl$Q4_10_b_coke[i]=="question is not displayed", dl$Q4_10_b_coke[i]<-999, print(i))))))
}

#Q4_10d_coke
for(i in 1:nrow(dl))
{
  ifelse(dl$Q4_10_c_coke_who[i]=="I did not use cocaine or crack with any of these people yesterday", dl$Q4_10_c_coke_who[i]<-0,
         ifelse(dl$Q4_10_c_coke_who[i]=="question is not displayed", dl$Q4_10_c_coke_who[i]<-999, print(i)))
}

#Q5_sex_par
for(i in 1:nrow(dl))
{
  ifelse(dl$Q5_sex_partners[i]=="4 or more", dl$Q5_sex_partners[i]<-4,
         ifelse(dl$Q5_sex_partners[i]=="question is not displayed", dl$Q5_sex_partners[i]<-999, print(i)))
}

#Q5_sex_id
for( i in 1:nrow(dl))
{
  ifelse(dl$Q5_sex_a_id[i]=="question is not displayed", dl$Q5_sex_a_id[i]<-999, print(i))
}

#Q5_sex_id_R1
for( i in 1:nrow(dl))
{
  ifelse(dl$R1_Q5_sex_a_id[i]=="question is not displayed", dl$R1_Q5_sex_a_id[i]<-999, print(i))
}

#Q5_sex_ptype
for(i in 1:nrow(dl))
{
  ifelse(dl$R1_Q5_sex_b_parttype[i]=="Serious partner (husband, wife, life partner, girlfriend, boyfriend, etc.)", dl$R1_Q5_sex_b_parttype[i]<-1,
         ifelse(dl$R1_Q5_sex_b_parttype[i]=="Casual partner (hookup, friends with benefits, one night stand, etc.)", dl$R1_Q5_sex_b_parttype[i]<-2,
                ifelse(dl$R1_Q5_sex_b_parttype[i]=="question is not displayed", dl$R1_Q5_sex_b_parttype[i]<-999, print(i))))
}

#Q5_sex_b2_dur
for(i in 1:nrow(dl))
{
  ifelse(dl$R1_Q5_sex_b2_partdur[i]=="1 day or less", dl$R1_Q5_sex_b2_partdur[i]<-1,
         ifelse(dl$R1_Q5_sex_b2_partdur[i]=="1-30 days", dl$R1_Q5_sex_b2_partdur[i]<-2,
                ifelse(dl$R1_Q5_sex_b2_partdur[i]=="1-6 months", dl$R1_Q5_sex_b2_partdur[i]<-3,
                       ifelse(dl$R1_Q5_sex_b2_partdur[i]=="6-12 months", dl$R1_Q5_sex_b2_partdur[i]<-4,
                              ifelse(dl$R1_Q5_sex_b2_partdur[i]=="1-3 years", dl$R1_Q5_sex_b2_partdur[i]<-5,
                                     ifelse(dl$R1_Q5_sex_b2_partdur[i]=="More than 3 years", dl$R1_Q5_sex_b2_partdur[i]<-6,
                                            ifelse(dl$R1_Q5_sex_b2_partdur[i]=="question is not displayed", dl$R1_Q5_sex_b2_partdur[i]<-999, print(i))))))))
}

#Q5_sex_c_identity
for(i in 1:nrow(dl))
{
  ifelse(dl$R1_Q5_sex_c_identity[i]=="Male", dl$R1_Q5_sex_c_identity[i]<-1,
         ifelse(dl$R1_Q5_sex_c_identity[i]=="Female", dl$R1_Q5_sex_c_identity[i]<-2,
                ifelse(dl$R1_Q5_sex_c_identity[i]=="Trans male/Trans man", dl$R1_Q5_sex_c_identity[i]<-3,
                       ifelse(dl$R1_Q5_sex_c_identity[i]=="Trans female/Trans woman", dl$R1_Q5_sex_c_identity[i]<-4,
                              ifelse(dl$R1_Q5_sex_c_identity[i]=="Genderqueer/Gender non-conforming", dl$R1_Q5_sex_c_identity[i]<-5,
                                     ifelse(dl$R1_Q5_sex_c_identity[i]=="Different Identity (please specify)", dl$R1_Q5_sex_c_identity[i]<-6,
                                            ifelse(dl$R1_Q5_sex_c_identity[i]=="question is not displayed", dl$R1_Q5_sex_c_identity[i]<-999,print(i))))))))
}

#Q5_sex_d_condom
for(i in 1:nrow(dl))
{
  ifelse(dl$R1_Q5_sex_d_condom[i]=="Yes – vaginal without a condom <u>only</u>", dl$R1_Q5_sex_d_condom[i]<-1,
         ifelse(dl$R1_Q5_sex_d_condom[i]=="Yes – anal without a condom <u>only</u>", dl$R1_Q5_sex_d_condom[i]<-2,
                ifelse(dl$R1_Q5_sex_d_condom[i]=="Yes – both anal and vaginal sex without a condom <u>only</u>", dl$R1_Q5_sex_d_condom[i]<-3,
                       ifelse(dl$R1_Q5_sex_d_condom[i]=="No", dl$R1_Q5_sex_d_condom[i]<-0,
                              ifelse(dl$R1_Q5_sex_d_condom[i]=="question is not displayed", dl$R1_Q5_sex_d_condom[i]<-999, print(i))))))
}

#Q5_sex_e_sub
for(i in 1:nrow(dl))
{
  ifelse(dl$R1_Q5_sex_e_substance[i]=="Yes", dl$R1_Q5_sex_e_substance[i]<-1,
         ifelse(dl$R1_Q5_sex_e_substance[i]=="No", dl$R1_Q5_sex_e_substance[i]<-0,
                ifelse(dl$R1_Q5_sex_e_substance[i]=="question is not displayed", dl$R1_Q5_sex_e_substance[i]<-999, print(i)) ))
}

#Q7_sex_exchange
for( i in 1:nrow(dl))
{
  ifelse(dl$Q7_sex_exchange[i]=="I did not trade sex", dl$Q7_sex_exchange[i]<-0,
         ifelse(dl$Q7_sex_exchange[i]=="Money", dl$Q7_sex_exchange[i]<-1,
                ifelse(dl$Q7_sex_exchange[i]=="Drugs", dl$Q7_sex_exchange[i]<-2,
                       ifelse(dl$Q7_sex_exchange[i]=="A place to stay", dl$Q7_sex_exchange[i]<-3,
                              ifelse(dl$Q7_sex_exchange[i]=="Food or meals", dl$Q7_sex_exchange[i]<-4,
                                     ifelse(dl$Q7_sex_exchange[i]=="Something else", dl$Q7_sex_exchange[i]<-5,
                                            ifelse(dl$Q7_sex_exchange[i]=="question is not displayed", dl$Q7_sex_exchange[i]<-999, print(i))))))))
}

#Q8_thankyou
for(i in 1:nrow(dl))
{
  ifelse(dl$Q8_thankyou[i]=="question is not displayed", dl$Q8_thankyou[i]<-999, print(i))
}

#Writing 46 cols
write.csv(dl,"/Users/vaibhavx/Desktop/R_Data/likert_dailylog_46.csv", row.names = FALSE)
#Writing 52 cols
write.csv(dl,"/Users/vaibhavx/Desktop/R_Data/likert_dailylog_52.csv", row.names = FALSE)


#Reading the big merge file for conversion
df <- read.csv("/Users/vaibhavx/Desktop/R_Data/EMA_merged_data.csv", header = T, stringsAsFactors = FALSE)

#Col Q0_Welcome
for(i in 1:nrow(df))
{
  if(df$Q0_welcome=="question is not displayed"){df$Q0_welcome <- 999}
}

#Col Q1_social
for (i in 1:nrow(df))
{
  if(df$Q1_social[i]=="I have not interacted with anyone") {df$Q1_social[i] <- 0}
  if(df$Q1_social[i]=="Someone else not listed here"){ df$Q1_social[i] <-6}
  if(df$Q1_social[i]=="question is not displayed"){ df$Q1_social[i] <- 999}
}

#Col Q1_social_other
for(i in 1:nrow(df))
{
  ifelse(df$Q1_a_socialother[i]=="Friends from home / before you were homeless",df$Q1_a_socialother[i] <- 1,
         ifelse(df$Q1_a_socialother[i]=="Friends / peers from the street or an agency",df$Q1_a_socialother[i] <-2,
                ifelse(df$Q1_a_socialother[i]=="Family (biological or foster)",df$Q1_a_socialother[i] <- 3,
                       ifelse(df$Q1_a_socialother[i]=="Romantic / sexual partner",df$Q1_a_socialother[i] <-4,
                              ifelse(df$Q1_a_socialother[i]=="Case worker or agency staff/volunteer", df$Q1_a_socialother <-5,
                                     ifelse(df$Q1_a_socialother[i]=="People from work or school",df$Q1_a_socialother[i] <-6,
                                            ifelse(df$Q1_a_socialother[i]=="Law enforcement (police, security, etc.", df$Q1_a_socialother <-7,
                                                   ifelse(df$Q1_a_socialother[i] == "Someone I don't know well / random person", df$Q1_a_socialother[i] <-8, df$Q1_a_socialother[i]<-999))))))))
}

#Col Q2_where
for(i in 1:nrow(df))
{
  ifelse(df$Q2_where[i]=="My apartment/residence",df$Q2_where[i] <- 1,
         ifelse(df$Q2_where[i]=="Someone else's residence",df$Q2_where[i] <-2,
                ifelse(df$Q2_where[i]=="In transit (bus,car.etc.)",df$Q2_where[i] <- 3,
                       ifelse(df$Q2_where[i]=="Outdoors (park,beach,sidewalk,etc.)",df$Q2_where[i] <-4,
                              ifelse(df$Q2_where[i]=="School or work",df$Q2_where[i] <-5, 
                                     ifelse(df$Q2_where[i] == "Social service agency (drop-in, shelter, DPSS,etc.)", df$Q2_where[i] <-6, 
                                            ifelse(df$Q2_where[i] == "Place of business (restaurant,bar, mall,etc.)", df$Q2_where[i] <-7,
                                                   ifelse(df$Q2_where[i]=="Other", df$Q2_where[i] <-8, df$Q2_where[i]<-999))))))))
}

#Col Q2_what
for(i in 1:nrow(df))
{
  ifelse(df$Q2_what[i]=="None of the above",df$Q2_what[i] <- 0,
         ifelse(df$Q2_what[i]=="Going somewhere",df$Q2_what[i] <-1,
                ifelse(df$Q2_what[i]=="Hanging out",df$Q2_what[i] <- 2,
                       ifelse(df$Q2_what[i]=="Working/job/school",df$Q2_what[i] <-3,
                              ifelse(df$Q2_what[i]=="Meal/eating food",df$Q2_what[i] <-4, 
                                     ifelse(df$Q2_what[i] == "Sleeping/resting", df$Q2_what[i] <-5, 
                                            ifelse(df$Q2_what[i] == "Appointment/meeting", df$Q2_what[i] <-6,
                                                   ifelse(df$Q2_what[i]=="Other", df$Q2_what[i] <-7, df$Q2_what[i]<-999))))))))
}

#Col Q3_happy
for(i in 1:nrow(df))
{
  ifelse(df$Q3_happy[i]=="Slightly/not at all",df$Q3_happy[i] <- 0,
         ifelse(df$Q3_happy[i]=="A little",df$Q3_happy[i] <-1,
              ifelse(df$Q3_happy[i]=="Moderately",df$Q3_happy[i] <- 2,
                     ifelse(df$Q3_happy[i]=="Quite a bit",df$Q3_happy[i] <-3,
                            ifelse(df$Q3_happy[i]=="Extremely",df$Q3_happy[i] <-4, df$Q3_happy[i] <-999)))))
}
#Col Q4_stressed
for(i in 1:nrow(df))
{
  ifelse(df$Q4_stressed[i]=="Slightly/not at all",df$Q4_stressed[i] <- 0,
         ifelse(df$Q4_stressed[i]=="A little",df$Q4_stressed[i] <-1,
                ifelse(df$Q4_stressed[i]=="Moderately",df$Q4_stressed[i] <- 2,
                       ifelse(df$Q4_stressed[i]=="Quite a bit",df$Q4_stressed[i] <-3,
                              ifelse(df$Q4_stressed[i]=="Extremely",df$Q4_stressed[i] <-4, df$Q4_stressed[i] <-999)))))
}
#Col Q5_sad
for(i in 1:nrow(df))
{
  ifelse(df$Q5_sad[i]=="Slightly/not at all",df$Q5_sad[i] <- 0,
         ifelse(df$Q5_sad[i]=="A little",df$Q5_sad[i] <-1,
                ifelse(df$Q5_sad[i]=="Moderately",df$Q5_sad[i] <- 2,
                       ifelse(df$Q5_sad[i]=="Quite a bit",df$Q5_sad[i] <-3,
                              ifelse(df$Q5_sad[i]=="Extremely",df$Q5_sad[i] <-4, df$Q5_sad[i] <-999)))))
}

#Col Q6_irritated
for(i in 1:nrow(df))
{
  ifelse(df$Q6_irritated[i]=="Slightly/not at all",df$Q6_irritated[i] <- 0,
         ifelse(df$Q6_irritated[i]=="A little",df$Q6_irritated[i] <-1,
                ifelse(df$Q6_irritated[i]=="Moderately",df$Q6_irritated[i] <- 2,
                       ifelse(df$Q6_irritated[i]=="Quite a bit",df$Q6_irritated[i] <-3,
                              ifelse(df$Q6_irritated[i]=="Extremely",df$Q6_irritated[i] <-4, df$Q6_irritated[i] <-999)))))
}

#Col Q7_calm
for(i in 1:nrow(df))
{
  ifelse(df$Q7_calm[i]=="Slightly/not at all",df$Q7_calm[i] <- 0,
         ifelse(df$Q7_calm[i]=="A little",df$Q7_calm[i] <-1,
                ifelse(df$Q7_calm[i]=="Moderately",df$Q7_calm[i] <- 2,
                       ifelse(df$Q7_calm[i]=="Quite a bit",df$Q7_calm[i] <-3,
                              ifelse(df$Q7_calm[i]=="Extremely",df$Q7_calm[i] <-4, df$Q7_calm[i] <-999)))))
}

#Col Q8_excited
for(i in 1:nrow(df))
{
  ifelse(df$Q8_excited[i]=="Slightly/not at all",df$Q8_excited[i] <- 0,
         ifelse(df$Q8_excited[i]=="A little",df$Q8_excited[i] <-1,
                ifelse(df$Q8_excited[i]=="Moderately",df$Q8_excited[i] <- 2,
                       ifelse(df$Q8_excited[i]=="Quite a bit",df$Q8_excited[i] <-3,
                              ifelse(df$Q8_excited[i]=="Extremely",df$Q8_excited[i] <-4, df$Q8_excited[i] <-999)))))
}

#Col Q9_bored
for(i in 1:nrow(df))
{
  ifelse(df$Q9_bored[i]=="Slightly/not at all",df$Q9_bored[i] <- 0,
         ifelse(df$Q9_bored[i]=="A little",df$Q9_bored[i] <-1,
                ifelse(df$Q9_bored[i]=="Moderately",df$Q9_bored[i] <- 2,
                       ifelse(df$Q9_bored[i]=="Quite a bit",df$Q9_bored[i] <-3,
                              ifelse(df$Q9_bored[i]=="Extremely",df$Q9_bored[i] <-4, df$Q9_bored[i] <-999)))))
}

#Col Q10_hungry
for(i in 1:nrow(df))
{
  ifelse(df$Q10_hungry[i]=="Slightly/not at all",df$Q10_hungry[i] <- 0,
         ifelse(df$Q10_hungry[i]=="A little",df$Q10_hungry[i] <-1,
                ifelse(df$Q10_hungry[i]=="Moderately",df$Q10_hungry[i] <- 2,
                       ifelse(df$Q10_hungry[i]=="Quite a bit",df$Q10_hungry[i] <-3,
                              ifelse(df$Q10_hungry[i]=="Extremely",df$Q10_hungry[i] <-4, df$Q10_hungry[i] <-999)))))
}

#Col Q10_stressevents
for(i in 1:nrow(df))
{
  ifelse(df$Q10_stressevents[i]=="None of the above",df$Q10_stressevents[i] <- 0,
         ifelse(df$Q10_stressevents[i]=="I felt threatened or harassed",df$Q10_stressevents[i] <-1,
                ifelse(df$Q10_stressevents[i]=="Verbal fight or argument",df$Q10_stressevents[i] <- 2,
                       ifelse(df$Q10_stressevents[i]=="Physical fight",df$Q10_stressevents[i] <-3,
                              ifelse(df$Q10_stressevents[i]=="I got injured or became ill",df$Q10_stressevents[i] <-4, 
                                     ifelse(df$Q10_stressevents[i] == "Received bad news about something", df$Q10_stressevents[i] <-5, 
                                            ifelse(df$Q10_stressevents[i] == "Received good news about something important", df$Q10_stressevents[i] <-6,
                                                   ifelse(df$Q10_stressevents[i]=="Interaction with security/law enforcement", df$Q10_stressevents[i] <-7, df$Q10_stressevents[i]<-999))))))))
}

#Col Q11_tobacco
for(i in 1:nrow(df))
{
  ifelse(df$Q11_tobacco[i]=="I have not used tobacco",df$Q11_tobacco[i] <- 0,
         ifelse(df$Q11_tobacco[i]=="Paper cigarettes",df$Q11_tobacco[i] <-1,
                ifelse(df$Q11_tobacco[i]=="E-cigarettes/vaped tobacco",df$Q11_tobacco[i] <- 2,
                       ifelse(df$Q11_tobacco[i]=="Chewing tobacco/dip",df$Q11_tobacco[i] <-3,
                              ifelse(df$Q11_tobacco[i]=="A tobacco product not listed here",df$Q11_tobacco[i] <-4, df$Q11_tobacco[i] <-999)))))
}

#Col Q12_alcohol
for(i in 1:nrow(df))
{
  if(df$Q12_alcohol[i]=="question is not displayed") {df$Q12_alcohol[i] <- 999}
  if(df$Q12_alcohol[i]=="5 or more") {df$Q12_alcohol[i] <- 5}
}

#Col Q12_b_alc_where
for(i in 1:nrow(df))
{
  ifelse(df$Q12_b_alcohol_where[i]=="My apartment/residence",df$Q12_b_alcohol_where[i] <- 1,
         ifelse(df$Q12_b_alcohol_where[i]=="Someone else's residence",df$Q12_b_alcohol_where[i] <-2,
                ifelse(df$Q12_b_alcohol_where[i]=="In transit (bus,car.etc.)",df$Q12_b_alcohol_where[i] <- 3,
                       ifelse(df$Q12_b_alcohol_where[i]=="Outdoors (park,beach,sidewalk,etc.)",df$Q12_b_alcohol_where[i] <-4,
                              ifelse(df$Q12_b_alcohol_where[i]=="School or work",df$Q12_b_alcohol_where[i] <-5, 
                                     ifelse(df$Q12_b_alcohol_where[i] == "Social service agency (drop-in, shelter, DPSS,etc.)", df$Q12_b_alcohol_where[i] <-6, 
                                            ifelse(df$Q12_b_alcohol_where[i] == "Place of business (restaurant,bar, mall,etc.)", df$Q12_b_alcohol_where[i] <-7,
                                                   ifelse(df$Q12_b_alcohol_where[i]=="Other", df$Q12_b_alcohol_where[i] <-8, df$Q12_b_alcohol_where[i]<-999))))))))
}

#Col Q12_c_alc_who
for (i in 1:nrow(df))
{
  if(df$Q12_c_alcohol_who[i]=="Nobody") {df$Q12_c_alcohol_who[i] <- 0}
  if(df$Q12_c_alcohol_who[i]=="Someone else not listed here"){ df$Q12_c_alcohol_who[i] <-6}
  if(df$Q12_c_alcohol_who[i]=="question is not displayed"){ df$Q12_c_alcohol_who[i] <- 999}
}

#Col Q12_d_alc_who_other
for(i in 1:nrow(df))
{
  ifelse(df$Q12_d_alcohol_who_other[i]=="Friends from home / before you were homeless",df$Q12_d_alcohol_who_other[i] <- 1,
         ifelse(df$Q12_d_alcohol_who_other[i]=="Friends / peers from the street or an agency",df$Q12_d_alcohol_who_other[i] <-2,
                ifelse(df$Q12_d_alcohol_who_other[i]=="Family (biological or foster)",df$Q12_d_alcohol_who_other[i] <- 3,
                       ifelse(df$Q12_d_alcohol_who_other[i]=="Romantic / sexual partner",df$Q12_d_alcohol_who_other[i] <-4,
                              ifelse(df$Q12_d_alcohol_who_other[i]=="People from work or school",df$Q12_d_alcohol_who_other[i] <-5, 
                                     ifelse(df$Q12_d_alcohol_who_other[i] == "Someone I don't know well / random person", df$Q12_d_alcohol_who_other[i] <-6,df$Q12_d_alcohol_who_other[i]<-999))))))
}

#Col Q12_e_who_use
for(i in 1:nrow(df))
{
  ifelse(df$Q12_e_alcohol_who_use[i]=="No", df$Q12_e_alcohol_who_use[i] <- 0,
         ifelse(df$Q12_e_alcohol_who_use[i]=="Yes",df$Q12_e_alcohol_who_use[i] <-1,
                ifelse(df$Q12_e_alcohol_who_use[i]=="Not sure",df$Q12_e_alcohol_who_use[i] <-2, df$Q12_e_alcohol_who_use[i] <- 999)))
}

#Col Q13_drugs
for(i in 1:nrow(df))
{
  ifelse(df$Q13_drugs[i]=="No", df$Q13_drugs[i] <- 0,
         ifelse(df$Q13_drugs[i]=="Yes",df$Q13_drugs[i] <-1, df$Q13_drugs[i] <- 999))
}

#Col Q13_a_drugs_type
for(i in 1:nrow(df))
{
  ifelse(df$Q13_a_drugs_type[i]=="Marijuana",df$Q13_a_drugs_type[i] <- 1,
         ifelse(df$Q13_a_drugs_type[i]=="Meth",df$Q13_a_drugs_type[i] <-2,
                ifelse(df$Q13_a_drugs_type[i]=="Ecstasy/MDMA",df$Q13_a_drugs_type[i] <- 3,
                       ifelse(df$Q13_a_drugs_type[i]=="Synthetic marijuana (K2, Spice, etc.)",df$Q13_a_drugs_type[i] <-4,
                              ifelse(df$Q13_a_drugs_type[i]=="Hallucinogens/psychedelics",df$Q13_a_drugs_type[i] <-5, 
                                     ifelse(df$Q13_a_drugs_type[i] == "Prescription drugs, not as prescribed (Rx cough syrup, Oxycontin, Xanax, etc.", df$Q13_a_drugs_type[i] <-6, 
                                            ifelse(df$Q13_a_drugs_type[i] == "Heroin", df$Q13_a_drugs_type[i] <-7,
                                                   ifelse(df$Q13_a_drugs_type[i]=="Other", df$Q13_a_drugs_type[i] <-8, df$Q13_a_drugs_type[i]<-999))))))))
}

#Col Q13_a8_drugs_other
for(i in 1:nrow(df))
{
  if(df$Q13_a8_drugs_type_other[i]=="question is not displayed") {df$Q13_a8_drugs_type_other[i] <- 999}
}

#Col Q13_b_drugs_where
for(i in 1:nrow(df))
{
  ifelse(df$Q13_b_drugs_where[i]=="My apartment/residence",df$Q13_b_drugs_where[i] <- 1,
         ifelse(df$Q13_b_drugs_where[i]=="Someone else's residence",df$Q13_b_drugs_where[i] <-2,
                ifelse(df$Q13_b_drugs_where[i]=="In transit (bus,car.etc.)",df$Q13_b_drugs_where[i] <- 3,
                       ifelse(df$Q13_b_drugs_where[i]=="Outdoors (park,beach,sidewalk,etc.)",df$Q13_b_drugs_where[i] <-4,
                              ifelse(df$Q13_b_drugs_where[i]=="School or work",df$Q13_b_drugs_where[i] <-5, 
                                     ifelse(df$Q13_b_drugs_where[i] == "Social service agency (drop-in, shelter, DPSS,etc.)", df$Q13_b_drugs_where[i] <-6, 
                                            ifelse(df$Q13_b_drugs_where[i] == "Place of business (restaurant,bar, mall,etc.)", df$Q13_b_drugs_where[i] <-7,
                                                   ifelse(df$Q13_b_drugs_where[i]=="Other", df$Q13_b_drugs_where[i] <-8, df$Q13_b_drugs_where[i]<-999))))))))
}

#Col Q13_c_drugs_who
for (i in 1:nrow(df))
{
  if(df$Q13_c_drugs_who[i]=="Nobody") {df$Q13_c_drugs_who[i] <- 0}
  if(df$Q13_c_drugs_who[i]=="Someone else not listed here"){ df$Q13_c_drugs_who[i] <-6}
  if(df$Q13_c_drugs_who[i]=="question is not displayed"){ df$Q13_c_drugs_who[i] <- 999}
}

#Col Q13_d_drugs_who_other
for(i in 1:nrow(df))
{
  ifelse(df$Q13_d_drugs_who_other[i]=="Friends from home/before you were homeless",df$Q13_d_drugs_who_other[i] <- 1,
         ifelse(df$Q13_d_drugs_who_other[i]=="Friends/peers from the street or an agency",df$Q13_d_drugs_who_other[i] <-2,
                ifelse(df$Q13_d_drugs_who_other[i]=="Family (biological or foster)",df$Q13_d_drugs_who_other[i] <- 3,
                       ifelse(df$Q13_d_drugs_who_other[i]=="Romantic/sexual partner",df$Q13_d_drugs_who_other[i] <-4,
                              ifelse(df$Q13_d_drugs_who_other[i]=="People from work or school",df$Q13_d_drugs_who_other[i] <-5, 
                                     ifelse(df$Q13_d_drugs_who_other[i] == "Someone I don't know well/random person", df$Q13_d_drugs_who_other[i] <-6,df$Q13_d_drugs_who_other[i]<-999))))))
}

#Col Q13_e_drugs_who_use
for(i in 1:nrow(df))
{
  ifelse(df$Q13_e_drugs_who_use[i]=="No", df$Q13_e_drugs_who_use[i] <- 0,
         ifelse(df$Q13_e_drugs_who_use[i]=="Yes",df$Q13_e_drugs_who_use[i] <-1,
                ifelse(df$Q13_e_drugs_who_use[i]=="Not sure",df$Q13_e_drugs_who_use[i] <-2, df$Q13_e_drugs_who_use[i] <- 999)))
}

#Col Q14_tempted
for(i in 1:nrow(df))
{
  ifelse(df$Q14_tempted[i]=="No", df$Q14_tempted[i] <- 0,
         ifelse(df$Q14_tempted[i]=="Yes",df$Q14_tempted[i] <-1, df$Q14_tempted[i] <- 999))
}

#Col Q14_a_tempt_where
for(i in 1:nrow(df))
{
  ifelse(df$Q14_a_tempted_where[i]=="My apartment/residence",df$Q14_a_tempted_where[i] <- 1,
         ifelse(df$Q14_a_tempted_where[i]=="Someone else's residence",df$Q14_a_tempted_where[i] <-2,
                ifelse(df$Q14_a_tempted_where[i]=="In transit (bus,car.etc.)",df$Q14_a_tempted_where[i] <- 3,
                       ifelse(df$Q14_a_tempted_where[i]=="Outdoors (park,beach,sidewalk,etc.)",df$Q14_a_tempted_where[i] <-4,
                              ifelse(df$Q14_a_tempted_where[i]=="School or work",df$Q14_a_tempted_where[i] <-5, 
                                     ifelse(df$Q14_a_tempted_where[i] == "Social service agency (drop-in, shelter, DPSS,etc.)", df$Q14_a_tempted_where[i] <-6, 
                                            ifelse(df$Q14_a_tempted_where[i] == "Place of business (restaurant,bar, mall,etc.)", df$Q14_a_tempted_where[i] <-7,
                                                   ifelse(df$Q14_a_tempted_where[i]=="Other", df$Q14_a_tempted_where[i] <-8, df$Q14_a_tempted_where[i]<-999))))))))
}

#Col Q14_b_tempt_who
for (i in 1:nrow(df))
{
  if(df$Q14_b_tempted_who[i]=="Nobody") {df$Q14_b_tempted_who[i] <- 0}
  if(df$Q14_b_tempted_who[i]=="Someone else not listed here"){ df$Q14_b_tempted_who[i] <-6}
  if(df$Q14_b_tempted_who[i]=="question is not displayed"){ df$Q14_b_tempted_who[i] <- 999}
}

#Col Q14_c_tempt_who_other
for(i in 1:nrow(df))
{
  ifelse(df$Q14_c_tempted_who_other[i]=="Friends from home / before you were homeless",df$Q14_c_tempted_who_other[i] <- 1,
         ifelse(df$Q14_c_tempted_who_other[i]=="Friends/peers from the street or an agency",df$Q14_c_tempted_who_other[i] <-2,
                ifelse(df$Q14_c_tempted_who_other[i]=="Family (biological or foster)",df$Q14_c_tempted_who_other[i] <- 3,
                       ifelse(df$Q14_c_tempted_who_other[i]=="Romantic/sexual partner",df$Q14_c_tempted_who_other[i] <-4,
                              ifelse(df$Q14_c_tempted_who_other[i]=="People from work or school",df$Q14_c_tempted_who_other[i] <-5, 
                                     ifelse(df$Q14_c_tempted_who_other[i] == "Someone I don't know well/random person", df$Q14_c_tempted_who_other[i] <-6,df$Q14_c_tempted_who_other[i]<-999))))))
}

#Col Q14_d_tempt_who_use
for(i in 1:nrow(df))
{
  ifelse(df$Q14_d_tempted_who_use[i]=="No", df$Q14_d_tempted_who_use[i] <- 0,
         ifelse(df$Q14_d_tempted_who_use[i]=="Yes",df$Q14_d_tempted_who_use[i] <-1,
                ifelse(df$Q14_d_tempted_who_use[i]=="Not sure",df$Q14_d_tempted_who_use[i] <-2, df$Q14_d_tempted_who_use[i] <- 999)))
}

#Col Q15_thankyou
for(i in 1:nrow(df))
{
  if(df$Q15_thankyou =="question is not displayed") {df$Q15_thankyou <- 999}
  if(df$Q15_thankyou=="no need to answer") {df$Q15_thankyou <- 0}
}
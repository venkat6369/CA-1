---------------------------------------------------------------------------
#CA1 DATA ANALYSIS
#AUTHOR - VENKATA SRI SAI GOWTHAM
#TOPIC - HEART ATTACK ANALYSIS 
#MENTOR- JAMES CONOLLY
#MODULE - DATA SCIENCE
--------------------------------------------------------------------
# I. DATA PREPARTION.

#READING THE DATASET INTO R-STUDIO AND THEN PUSHING IT INTO A DATAFRAME  
ht<- read.csv('heart.csv')
ht_df<- ht

#RENAMING THE COLUMNS AS PER DATA DESCRIPTION
new_colnames <- c("Age","Sex","Chest Pain type","Resting blood pressure (in mm Hg)","Cholestrol","Fasting blood sugar","Resting electrocardiographic","Maximum heart rate achieved","Exercise induced angina","Oldpeak","Slope","Number of major vessels","Thal","Target")
colnames(ht_df)<- new_colnames
View(ht_df)

#DISPLAYING THE STRUCTURE OF THE DATAFRAME
str(ht_df)

#VERIFYING FOR NULLS IN THE DATA FRAME
sum(is.na(ht_df))
# THERE ARE NO NULLS IN THE DATA FRAME

#CHECKING FOR MISSING DATA INORDER TO MAKE SURE THERE ARE NO NULLS.
#library mice is the appropraite one for misiing values.
library('mice')
missing_data <- ht_df[!complete.cases(ht_df),]
missing_data

# THE DATA IS COMPLETE IN THIS DATA FRAME.



-----------------------------------------------------------------------------------------------

# II. HYPOTHESIS TESTING.

  #RESEARCH QUESTION 1: PEOPLE WHO HAVE MORE CHANCES OF HEART ATTACK FROM HIGH RESTING BLOOD PRESSURE ARE MOSTLY MALE THAN FEMALE ?
  
  #H0-  MALES ARE THE PEOPLE PRONE TO HEART ATTACK HAVE HIGH RESTING BLOOD PRESSURE THAN FEMALES
  #H1-  MALES ARE NOT THE PEOPLE PRONE TO HEART ATTACK HAVE HIGH RESTING BLOOD PRESSURE THAN FEMALES


# THE VARIABLES REQUIRED FOR THIS HYPOSTHESIS ARE SUBSETTED INTO A DATAFRAME
#PATIENTS WHO HAVE HIGH RISK OF HEART ATTACK HAVE BEEN SELECTED AND THEIR GENDER, RESTING BLOOD PRESSURE READINGS ARE TAKEN INTO THE DATA FRAME

hyp1_df <-subset(ht_df,Target == "1" ,select = c(Sex,`Resting blood pressure (in mm Hg)`))
View(hyp1_df)

#HERE THE SEX VARIABLE IS TO BE CONVERTED IN TO CATEGORICAL DICHOTOMOUS
#IT IS CONVERTED INTO CATEGORICAL DICHOTOMOUS USING FACTOR.
hyp1_df$Sex<- factor(hyp1_df$Sex, labels = c("Male", "Female"))
str(hyp1_df)                     

# PLOTIING A SCATTER PLOT FOR THE ABOVE VARIBALES TO CHECK LINEARITY/COREELATION
attach(hyp1_df)

plot( Sex ,`
      Resting blood pressure (in mm Hg)`,
      pch=19, 
      col ="lightblue",
      main= 'comparision of resting 
      blood pressure', 
      xlab= "Gender", 
      ylab= "Restbp" 
)

# PLOTIING A HISTOGRAM FOR THE ABOVE VARIBALES TO CHECK LINEARITY/COREELATION

library("lattice")

histogram(~ `Resting blood pressure (in mm Hg)`| Sex , 
          data = hyp1_df, 
          main = "Gender wise count of people having high Resting blood pressure",
          xlab= "Restbp",
          ylab= "No of people"
          , col= "green")

#REFERENCING THE MEDIAN VALUE FOR THE MIDPOINT OF THE DATA
tapply(`Resting blood pressure (in mm Hg)`, Sex, median)

#CHECKING NORMALITY THROUGH QQNORM
qqnorm(`Resting blood pressure (in mm Hg)`)

#PLOTTING THE REFERENCE NORMALITY LINE WITH QQLINE
qqline(`Resting blood pressure (in mm Hg)`, col="green") 

#FORMAL TEST OF NORMALITY (SHAPIRO.TEST)
#p-value greater than 0.05 

normality_test <- shapiro.test(hyp1_df$`Resting blood pressure (in mm Hg)`)
normality_test$p.value
#p-value = 0.01190189
#HERE THE P-VALUE IS LESS THEAN 0.05, SO DATA IS NOT NORMALLY DISTRIBUTED

#NORMALITY TEST ON THE SLICE OF DICHOTOMOUS VARIABLE.
with(hyp1_df, tapply(`Resting blood pressure (in mm Hg)`, Sex,shapiro.test))
#p-value =0.3013 (male)- DATA IS NORMALLY DISTRIBUTED
#p- value = 0.05845(female)- DATA IS NOT NORMALLY DISTRIBUTED

#STATISTICAL TEST
#CUTOFF P-VALUE =0.05.
#WILCOX TEST IS PERFORMED HERE.

wilcox.test(`Resting blood pressure (in mm Hg)`~ Sex)
 #p-value = 0.829

# In this case accept null hypothesis H0 as p value is greater than cutoff
 #H0-  MALES ARE THE PEOPLE PRONE TO HEART ATTACK HAVE HIGH RESTING BLOOD PRESSURE THAN FEMALES.

 detach(hyp1_df)
-----------------------------------------------------------------------------------------------------------------------------------
#RESEARCH QUESTION 2:  ARE PEOPLE HAVING HIGH CHANCES OF HEART ATTACK HAVING HIGH CHOLESTROL WITH FASTING BLOOD SUGAR ? 

  #H0- HIGH CHANCE OF HEART ATTACK AND HIGH CHOLESTROL IS ACCOMPAINIED BY FASTING BLOOD SUGAR AS TRUE.
  #H1- HIGH CHANCE OF HEART ATTACK AND HIGH CHOLESTROL IS ACCOMPAINIED BY FASTING BLOOD SUGAR AS FALSE.

# THE VARIABLES REQUIRED FOR THIS HYPOSTHESIS ARE SUBSETTED INTO A DATAFRAME
hyp2_df <- subset(ht_df, Target== "1", select= c(Cholestrol, `Fasting blood sugar`))
View(hyp2_df)

#THE FASTING BLOOD SUGAR VARIABLE IS BEING CONVERTED INTO CATEGORIACL DICHOTOMOUS USING FACTOR(). 
hyp2_df$`Fasting blood sugar`<- factor(hyp2_df$`Fasting blood sugar`, labels = c("True", "False"))
str(hyp2_df)


#checking linearity/correlation with scatter plot.

attach(hyp2_df)
plot( `Fasting blood sugar`,
      Cholestrol, pch=19,
      col ="lightblue",
      main = "Comparision of cholestrol on Fasting blood sugar",
      xlab= "Resting blood sugar level", 
      ylab= 'Cholestrol level' )

#visulaization wrt to the split of dichotomous variable.

library("lattice")
histogram(~ `Cholestrol`| `Fasting blood sugar` ,
          data = hyp2_df,
          main = " Gender wise Comparision of cholestrol on Fasting blood",
          xlab= "Cholestrol level", 
          ylab= " Count of people")

#Caluclating the median for deciding normal distribution.
tapply(`Cholestrol`,`Fasting blood sugar`, median)

#CHECKING THE NORMALITY THROUGH THE QQNORM
qqnorm(`Cholestrol`)
#PLOTTING A NORMAL REFERENCE LINE WITH QQLINE
qqline(`Cholestrol`, col="green") 

# A FORMAL TEST OF NORMALITY (SHAPIRO.TEST)
#p-value greater than 0.05
normality_test <- shapiro.test(hyp2_df$`Cholestrol`)
normality_test$p.value
#p-value = 0.01190189 - not normally distributed

#NORMALITY TEST ON THE SLICE OF DICHOTOMOUS VARIABLE.
with(hyp2_df, tapply(`Cholestrol`, `Fasting blood sugar`,shapiro.test))
#p-value = 6.962e-09- TRUE
#p- value = 0.04107 - FALSE
# both are not normally distributed as p value is less than 0.05


#STATISTICAL TEST
#CUTOFF P-VALUE =0.05.
#WILCOX TEST IS PERFORMED HERE.


wilcox.test(`Cholestrol`~ `Fasting blood sugar`)
#p-value = 0.8085
#p- value is greater than cutoff, so we reject alternate hypothesis.
# IN THIS CASE ACCEPT THE NULL HYPOTHESIS H0
#H0- HIGH CHANCE OF HEART ATTACK AND HIGH CHOLESTROL IS ACCOMPAINIED BY FASTING BLOOD SUGAR AS TRUE.

detach(hyp2_df)
--------------------------------------------------------------------------------------------------------------------------------
#RESEARCH QUESTION 3: DOES HEART RATE HAS ANY EFFECT ON CHANCES OF HEART ATTACK AFTER THE AGE OF 45 ?
  #H0- HEART RATE HAS AN EFFECT ON CHANCES OF HEART ATTACK AFTER 45.
  #H1- HEART RATE HAS NO EFFECT ON CHANCES OF HEART ATTACK AFTER 45.

# THE VARIABLES REQUIRED FOR THIS HYPOSTHESIS ARE SUBSETTED INTO A DATAFRAME    
hyp3_df <- subset(ht_df, Age > 45, select = c(Age,`Maximum heart rate achieved`,Target ))
View(hyp3_df)


#THE TARGET VARIABLE IS BEING CONVERTED INTO CATEGORIACL DICHOTOMOUS USING FACTOR(). 
hyp3_df$Target<- factor(hyp3_df$Target, labels = c("Morechance", "Lesschance"))
str(hyp3_df)

#SCATTER PLOT FOR CHECKING LINEARITY/CORRELATION
attach(hyp3_df)
plot( `Target`, `Maximum heart rate achieved`,
      pch=19,
      col ="lightblue", 
      main= " Age wise comparison if heart rate and chances of heart attack", 
      xlab= "chances", 
      ylab= "Heart rate" )


#HISTOGRAM WITH SPLIT OF DATA WITH DICHTOMOUS VARIABLES.
library("lattice")
histogram(~ `Maximum heart rate achieved`| `Target` , 
          data = hyp3_df,
          main = " Distribution of people wrt to age on chances and heart rate",
          xlab= "heart rate",
          ylab= "count of people")


#CALCULATING THE MEDIAN FOR MIDPOINT OF DATA
tapply(`Maximum heart rate achieved`,`Target`, median)

#CHECKING THE NORMALITY THROUGH THE QQNORM
qqnorm(`Maximum heart rate achieved`)
#PLOTTING A NORMAL REFERENCE LINE WITH QQLINE
qqline(`Maximum heart rate achieved`, col="green") 

#TEST FOR NORMALITY.
#p-value greater than 0.05
normality_test <- shapiro.test(hyp3_df$`Maximum heart rate achieved`)
normality_test$p.value
#p-value = 7.102315e-05 - NOT NORMALLY DISTRIBUTED


#NORMALITY TEST FOR SLICE OF DICHOTOMOUS VARIABLES
with(hyp3_df, tapply(`Maximum heart rate achieved`, `Target`,shapiro.test))
#p- value = 0.1226 -morechance -NOT NORMALLY DISTRIBUTED
#p- value = 0.0012 -less chance-NOT NORMALLY DISTRIBUTED


#STATISTICAL TEST
#CUTOFF P-VALUE =0.05.
#WILCOX TEST IS PERFORMED HERE.

wilcox.test(`Maximum heart rate achieved` ~ `Target`)
#p-value = 5.708e-08
#IN THIS CASE WE MUST REJECT NULL HYPOTHESIS AND ACCEPT ALTERNATIVE CASE H1.
#H1- HEART RATE HAS NO EFFECT ON CHANCES OF HEART ATTACK AFTER 45.

detach(hyp3_df)

-----------------------------------------------------------------------------------------------------------------------

#RESEARCH QUESTION 4 - DOES HEART ATTACK MOSTLY OCCUR IN MALES THAN FEMALES AFTER THE AGE OF 50 ?
 #H0- MALES ARE SENSITIVE TO HEART ATTACK THAN FEMALES AFTER THE AGE OF 50.
 #H1 - MALES ARE NOT SENSITIVE TO HEART ATTACK THAN FEMALES AFTER THE AGE OF 50.

  # THE VARIABLES REQUIRED FOR THIS HYPOSTHESIS ARE SUBSETTED INTO A DATAFRAME  
hyp4_df <- subset(ht_df , Age >50, select = c(Sex,Target))
View(hyp4_df)


#THE TARGET VARIABLE IS BEING CONVERTED INTO CATEGORIACL DICHOTOMOUS USING FACTOR(). 
hyp4_df$Sex<- factor(hyp4_df$Sex, labels = c("Male", "Female"))
hyp4_df$Target<- factor(hyp4_df$Target, labels = c("Morechance", "Lesschance"))
str(hyp4_df)


# scatter plot for checking linearity/correlation
attach(hyp4_df)
plot(Sex, 
     Target, 
     pch = "19", 
     col = "grey", 
     main = "Comparison of Sex with Chance of Heart Attack",
     xlab = "Sex", 
     ylab = "Chance of Heart Attack")

# Histogram for checking linearity/correlation
library("lattice")

histogram(~Sex| Target,
          data = hyp4_df,
          main = "Comparison of Sex with 
          Chance of Heart Attack",
          xlab = "Sex",
          ylab = "Chance of Heart Attack")

#AS THESE ARE DICHOTOMOUS VARIABLES NOT MUCH CAN BE PREDICTED FROM THE ABOVE PLOTS 


#STATISTICAL TEST
#CUTOFF P-VALUE =0.05.
#CHISQ TEST IS PERFORMED HERE.

chisq <- chisq.test(Sex, Target)
chisq$p.value
#p-value= 0.0001894598
#IN THIS CASE WE MUST REJECT NULL HYPOTHESIS AND ACCEPT ALTERNATIVE CASE H1.
#H1 - MALES ARE NOT SENSITIVE TO HEART ATTACK THAN FEMALES AFTER THE AGE OF 50.

detach(hyp4_df)
-----------------------------------------------------------------------------------------------------------------------
#RESEARCH QUESTION 5: DO FEMALES HAVE EXERCISED INDUCED ANGINA MORE THAN MALES?
  #H0- FEMALES ARE MORE EFFECTED BY EXCERCISE INDUCED ANGINA THAN MALES
  #H1- FEMALES ARE NOT MORE EFFECTED BY EXCERCISE INDUCED ANGINA THAN MALES

# THE VARIABLES REQUIRED FOR THIS HYPOSTHESIS ARE SUBSETTED INTO A DATAFRAME    
hyp5_df <- subset(ht_df,select = c(`Exercise induced angina` ,Sex))
View(hyp_5df)

#THE SEX VARIABLE IS BEING CONVERTED INTO CATEGORIACL DICHOTOMOUS USING FACTOR(). 
hyp5_df$Sex <- factor(hyp5_df$Sex, labels=c("Male","Female"))
str(hyp5_df)

#THE EXERCISE VARIABLE IS BEING CONVERTED INTO CATEGORIACL DICHOTOMOUS USING FACTOR()
hyp5df$`Exercise induced angina` <- factor(hyp5df$`Exercise induced angina`, labels = c('yes','no'))  

#SCATTER PLOT FOR CHECKING THE LINEARITY/ CORRELATION 
attach(hyp5_df)
plot(Sex, 
     `Exercise induced angina`, 
     pch = "19", 
     col = "grey", 
     main = "Comparison of Sex with Exercise induced angina",
     xlab = "Sex", 
     ylab = `Exercise induced angina`)

#HISTOGARM FOR CHECKING THE LINEARITY/ CORRELATION 
library("lattice")

histogram(~Sex|`Exercise induced angina` ,
          data = hyp5_df,
          main = "Comparison of Sex with Excercise induced angina ",
          xlab = "Sex",
          ylab = "Excercise induced angina")


#AS THESE ARE DICHOTOMOUS VARIABLES NOT MUCH CAN BE PREDICTED FROM THE ABOVE PLOTS 



#STATISTICAL TEST
#CUTOFF P-VALUE =0.05.
#CHISQ TEST IS PERFORMED HERE.

chisq <- chisq.test(Sex, `Exercise induced angina`)
chisq$p.value
#p-value = 0.00132039
#IN THIS CASE WE MUST REJECT NULL HYPOTHESIS AND ACCEPT ALTERNATIVE CASE H1.
#H1- FEMALES ARE NOT MORE EFFECTED BY EXCERCISE INDUCED ANGINA THAN MALES


detach(hyp5_df)

#-----------------------END OF CODE ---------------------------------------------------


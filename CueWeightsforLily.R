rm(list = ls())
###These are all the libraries that you need to run the script, make sure you have these installed!!
library(dplyr)
library(reshape2)
library(plyr)
library(ggplot2)
library(tidyr)

######################################
#####Five general steps in this code#############
####1. read in the data for all the subjects#########
####2. write a custom function to calculate cue weights. You can define the method for calculation, using 'raw' for Shae's method to calculate raw cue weights or 'normalized' to use Charles's method to normalize and take the absolute values of the weights for two dimensions so they sum up to one.
####3. loop through all subjects and calculate cue weights for each and then store the cue weights in a dataframe called CueWeights.
####4. average across all subjects and make bar plots


######################################
###########step 1###################
##Since data from subjects are in separate files, we store the filenames in a vector first. 
##Note:this is assuming that all subject data files are in a csv format
################set your working directory#########################
setwd("/Users/charleswu/Google Drive/HoltLab/Mentoring/Lily_Noise/Data") #mac
filenames <- list.files(pattern = '.csv')

###########step 2###################
##Next we write a customized function to calculate the cue-weights.
##Because Shae and I are using different methods to calculate the cue weights,
##this function takes that into account and is flexible to perform different ways to calculate the cue weights
CalculateCueWeights <- function(responseV, predictorV1, predictorV2, method){
  #In this function, we assume that there are two dimensions for which we need to calculate the cue weights
  #You first have to specify the method as either 'raw' (Shae's method), which is the raw cue weight or the 
  #'normalized' (Charles' method), which is the normalized absolute value of the weight such that the weights for the two dimensions add up to one
  #note that the input variables would have to be indexed by the data using $
  #responseV is the y variable
  #predictorV1 is the x1
  #predictorV2 is x2
  #Note that the output is a long format with the two cue weights stacked in rows for each participant so make sure to reshape the output matrix
  mod <- glm(responseV ~ predictorV1 + predictorV2, family = 'binomial')
  weight1 <- round(as.numeric(mod$coefficients[2]), digits = 3)
  weight2 <- round(as.numeric(mod$coefficients[3]), digits = 3)
  if (method == 'raw') {
      return(c(weight1, weight2))
  }
  else if (method == 'normalized') {
      sum <- abs(weight1) + abs(weight2)
      weight1_norm <- abs(weight1)/sum
      weight2_norm <- abs(weight2)/sum
      return(c(weight1_norm, weight2_norm))
  }
  else {
      print('Method does not exist')
  }
}

###########step 3###################
##Next we are going to use a for-loop to process data and get cue weights for each participant 
##specify the method for calculating cue weights first (you can change it to 'raw' to use Shae's method or 'normalized' to use Charles' method):
method = 'raw'
CueWeights <- NULL

for (i in 1:length(filenames)) {
    raw <- read.csv(filenames[i]) ###read in the raw data
    Raw_weights <- ddply(raw, ~SubjectNumber+ConditionNumber, summarise, VOT = CalculateCueWeights(Response, VOTlevel, F0level, method = method)[1], F0 = CalculateCueWeights(Response, VOTlevel, F0level, method = method)[2])
    ###since we are missing subject number in the file, we need to paste this into the data
    weights <- melt(Raw_weights, id.vars = c('SubjectNumber','ConditionNumber'), variable.name = 'Dimensions', value.name = 'CW')
    weights$SubjectNumber <- extract_numeric(filenames[i])
    if (substr(filenames[i], 8, 8) == "1" ){
      weights$AgeGroup <- rep("younger", nrow(weights))
    }
    else if (substr(filenames[i], 8, 8) == "2" ) {
      weights$AgeGroup <- rep("older", nrow(weights))
    }
    CueWeights <- rbind(CueWeights, weights)
}

###########step 4###################
###Next we can try to draw bar plots to show the cue weights for different conitions
##We first get an average cue weighs across all subjects
aveCW <- ddply(CueWeights, ~ConditionNumber+Dimensions+AgeGroup, summarise, meanCW = mean(CW), n = length(CW), sd = sd(CW), se = sd/sqrt(n))
aveCW$ConditionNumber <- as.factor(aveCW$ConditionNumber)
ggplot(aveCW, aes(ConditionNumber, meanCW, group = Dimensions, fill = Dimensions))+
  geom_bar(stat = 'identity', position = position_dodge())+
  scale_x_discrete(labels=c('65/-3', '65/0', '65/3', '75/-3', '75/0', '75/3', '65/quiet','75/quiet'))+
  geom_errorbar(aes(ymin = meanCW-se, ymax = meanCW+se), position = position_dodge())+
  ggtitle('Cue Weights in different conditions(raw, using glm)')+
  theme(axis.text.x = element_text(size = 10, face = 'bold', angle = 40))
  
########step5#######
###statistical analysis using the CueWeights matrix
###we decided to get rid of the 75dB conditions, which are condition 4,5,6 and 8
###so we need to generate a new matrix with this information and a new label for noise/quiet
newCueWeights <- CueWeights[!(CueWeights$ConditionNumber %in% c(4,5,6,8)),]
newCueWeights$ConditionNumber[newCueWeights$ConditionNumber == 1] <- '65dB-3'
newCueWeights$ConditionNumber[newCueWeights$ConditionNumber == 2] <- '65dB0'
newCueWeights$ConditionNumber[newCueWeights$ConditionNumber == 3] <- '65dB+3'
newCueWeights$ConditionNumber[newCueWeights$ConditionNumber == 7] <- 'quiet'
newCueWeights$ConditionLabel <- rep(rep(c('noise','quiet'), c(3,1)), nrow(newCueWeights)/4)

####plot these conditions, note this plot includes both younger and older groups######
####we first create a summary matrix
ave <- ddply(newCueWeights, ~ConditionNumber+Dimensions+AgeGroup, summarise, mean = mean(CW), n = length(CW), sd = sd(CW), se = sd/sqrt(n))

png("CueWeights_younger.png", units = "in", width=10, height=8, res=500)
ggplot(ave[ave$AgeGroup == 'younger',], aes(ConditionNumber, mean, fill = Dimensions))+
  geom_bar(stat = 'identity', position = position_dodge())+
  geom_errorbar(aes(ymin=mean-se, ymax = mean+se), width = 0.8, position = position_dodge(0.9), size = 0.9)+
  scale_x_discrete(labels = c('-3SNR/65dB', '0SNR/65dB', '+3SNR/65dB', 'quiet/65dB'))+
  scale_y_continuous(limits = c(-0.5,4))+
  labs(size = 15,
       x = 'Signal to noise levels',
       y = 'Cue Weights',
       title = 'Cue Weights in younger group')+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5), axis.title = element_text(face = 'bold', size = 18), axis.text.x = element_text(face = 'bold', color = 'black', size = 15), axis.text.y = element_text(face = 'bold', color = 'black', size = 15), legend.text = element_text(face = 'bold', color = 'black', size = 14), legend.title = element_text(face = 'bold', color = 'black', size = 14), title = element_text(face = 'bold', size = 20))
dev.off()

Ano_Dimensions <- aov(CW ~ (Dimensions*ConditionNumber) + Error(SubjectNumber/(Dimensions*ConditionNumber)), data=newCueWeights)
summary(Ano_Dimensions)

test <- newCueWeights[newCueWeights$ConditionNumber %in% c('65dB+3','quiet'),]
Ano_test <- aov(CW ~ (Dimensions*ConditionNumber) + Error(SubjectNumber/(Dimensions*ConditionNumber)), data=test)
summary(Ano_test)

Ano_SNR <- aov(CW ~ ConditionNumber + Error(SubjectNumber/ConditionNumber), data=newCueWeights[newCueWeights$ConditionLabel == 'noise' & newCueWeights$Dimensions == 'VOT',])
summary(Ano_SNR)

Ano_total <- aov(CW ~ (ConditionNumber*Dimensions*AgeGroup) + Error(SubjectNumber/ConditionNumber*Dimensions), data=newCueWeights)
summary(Ano_total)

##################step6###########################
#################correlation####################
CueWeights_Cor <- newCueWeights[newCueWeights$ConditionNumber == '65dB0'&newCueWeights$Dimensions == 'F0',]
setwd("/Users/charleswu/Google Drive/HoltLab/Mentoring/Lily_Noise")
WC <- read.csv('WiN_Master.csv')
cor.test(WC$Words.Correct, CueWeights_Cor$CW)
plot(WC$Words.Correct, CueWeights_Cor$CW)
CueWeights_Cor$WiNScore <- WC$Words.Correct
write.csv(CueWeights_Cor, '65dB0SNR_F0.csv')

png("IndividualAnalysis.png", units = "in", width=10, height=8, res=500)
ggplot(CueWeights_Cor, aes(WiNScore, CW))+
  geom_point(color = 'red', size = 3)+
  geom_smooth(method = 'lm', color = 'red', se = FALSE)+
  labs(size= 15,
       x = 'Word in Noise task score',
       y = 'F0 cue weights in 0 SNR/65 dB',
       title = 'Scatterplot of F0 weights and word in noise score')+
  geom_text(aes(20,1,label = "r = 0.522, p = 0.026", vjust = 1), size = 7, color = 'red')+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5), axis.title=element_text(face = 'bold', size = 18), title = element_text(face = 'bold', size = 22), axis.text.x = element_text(size = 13, face = 'bold', color = 'black'), axis.text.y = element_text(size = 13, face = 'bold', color = 'black'))
dev.off()

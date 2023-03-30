#load the data 
mock_foliar <- read.csv(file = './data/mock_foliar.csv')
head(mock_foliar)

#make a graph (use this code later, just keep in here in case you need it)
library(ggplot2)
par(mfrow=c(2,1))
ggplot(bold, aes(x=Type,y=PC1,fill=Type))+geom_boxplot()+ylab("Atomic Percent Enrichment (APE)")+xlab("Snail Type")
ggplot(bold, aes(x=Type,y=PC2,fill=Type))+geom_boxplot()+ylab("Atomic Percent Enrichment (APE)")+xlab("Snail Type")


#Categorical predictors in regression: values of the categorical predictors are usually referred to as treatments/treatment levels
#my categories are the treatment levels:
    #no_defol, con_no_defol, 0.5_donor_defol, 0.5_recip_defol, con_0.5_donor_defol, con_0.5_recip_defol, 0.75_donor_defol, 0.75_recip_defol, con_0.75_donor_defol, con_0.75_recip_defol, 1.0_donor_defol, 1.0_recip_defol, con_1.0_donor_defol, con_1.0_recip_defol
#each treatment has different tissue groups:
    #needles, stem, lowroot, highroot
#to include in the regression, create a set of dummy (indicator) variables. Once created, those dummy variables are called regressors
#create n-1 dummy variables (so for tissues, create 3 and for treatment levels make 13)


#Create dummy variables using tissues as categorical predictor
#load the package
library('fastDummies')

#Create dummy variables for donor or recipient seedling:
dummy_don_recip <- dummy_cols(mock_foliar, select_columns = 'donor_or_recip')
dummy_don_recip

#Create dummy variables for tissue type:
dummy_tissue <- dummy_cols(mock_foliar, select_columns = 'tissue')
dummy_tissue

#create dummy variables using treatment as categorical predictor
#load the package
library('fastDummies')
# Create dummy variables for treatment:
dummy_treat <- dummy_cols(mock_foliar, select_columns = 'treatment')
dummy_treat

#or was I supposed to make dummy variables of two columns? 
dummy_mock_foliar <- dummy_cols(mock_foliar, select_columns = c('treatment', 'tissue'))
dummy_mock_foliar

#do I need to append the APE values to this?
mock_foliar_dummy <- cbind(dummy_tissue, dummy_treat, dummy_don_recip, mock_foliar$APE)
mock_foliar_dummy


#create the model
library(car)
lm_mock_foliar <- lm(APE ~ dummy_tissue + dummy_treat + dummy_don_recip, data=mock_foliar_dummy)
Anova(lm_mock_foliar)
summary(lm_mock_foliar)


#look at power test, pull error values from data

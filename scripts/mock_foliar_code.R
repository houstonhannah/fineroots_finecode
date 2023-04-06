#load the data 
mock_foliar <- read.csv(file = './data/mock_foliar.csv')
data.frame(mock_foliar)

#drop the random X to X.6  columns 
mock_foliar <- subset.data.frame(mock_foliar, select = c('plant_num', 'isotope_lab', 'treatment', 'donor_or_recip', 'tissue', 'APE'))

#delete the NA's from the APE column
mock_foliar <- mock_foliar %>% drop_na(APE) #donor seedlings in the foliar treatment will have no APE for tissue group needles b/c they were directly labeled with isotope solution 
View(mock_foliar)

#load packages
library(ggthemes)
library(ggplot2)
library(dplyr)
library(multcompView)
library(car)
library(pwr)
library(tidyr)


#####Power Analysis############################################################################# 
#####Conduct a power analysis to determine if there are enough experimental replications to detect a trend

pwr.anova.test(f= , k=26, n=5, sig.level=0.05)

#f is the effect size, no data exists yet in the literature about possible effect size. 
#f will be calculated using preliminary data that is currently being analyzed.

#k is the number of groups
#n is the number of pots per group
#sig.level is the alpha value
#The higher the power value, the higher the chance is of detecting an effect

#####Make a Big Ol' Graph######################################################################

#make a graph looking at all treatment types and export as graph to figs folder
pdf('./figs/mock_foliar_all_treatments.pdf') #pdf will show up in figs folder
par(mfrow = c(1,1))
ggplot(mock_foliar, aes(x=treatment,y=APE,fill=treatment))+geom_boxplot()+ylab("Atomic Percent Enrichment (APE)")+xlab("Treatment")
dev.off() #where to stop pdf

#note: this does not separate APE for donor or recipient seedlings for each treatment



#####No Defol############################################################################# 
#####No defoliation occurred in this experimental treatment. 
#####In the control group, cores were rotated to sever mycorrhizal connections.

#subset the data
no_defol <- subset(mock_foliar, treatment %in% c('no_defol', 'con_no_defol'))
View(no_defol)
data.frame(no_defol)

#make a graph: *****this graph needs some help, how to separate into donor vs recips?*****
pdf('./figs/no_defol.vs.con_no_defol.pdf') #pdf will show up in figs folder
par(mfrow = c(1,1))
ggplot(no_defol, aes(x= treatment, y=APE,fill=treatment))+geom_boxplot()+ylab("Atomic Percent Enrichment (APE)")+xlab("Treatment")
dev.off()

ggplot() +   
  geom_boxplot(no_defol, aes(x=treatment,y=APE, group=1, colour = 'salmon')) +    
  geom_boxplot(no_defol, aes(x=treatment,y=APE, group = 2, colour = 'cyan2')) +   
  ylab('Atomic Percent Enrichment (APE)')+
  xlab('Treatment')

ggplot(test_data, aes(date)) + 
  geom_line(aes(x=treatment,y=APE = var0, colour = "salmon")) + 
  geom_line(aes(y = var1, colour = "var1"))


#Create a model: no defol vs. control no defol
lm_no_defol <- lm(APE ~ treatment, data=no_defol)
Anova(lm_no_defol)
summary(lm_no_defol)

#check residuals of the model
par(mfrow = c(2, 2))
plot(lm_no_defol) 
#the residuals vs. fitted and the Scale-Location graphs: look for normal spread of points along red line, patterns show issues with homoscedasticity
#The Q-Q plot: look for points hugging red line, deviance from line shows issues with residuals
#The Residuals vs Leverage graph indicates if there are large outliers that have a disproportionate effect on the regression

#Tukey test to see if different plant tissue groups enriched differently
#subset the data more to look at tissue groups and APE for no_defol treatment
tissue_no_defol <- subset(mock_foliar, treatment %in% c('no_defol'))
View(tissue_no_defol)
data.frame(tissue_no_defol)

#build the model and run TukeyHSD
aov_tissue_no_defol <- aov(APE ~ tissue, data = tissue_no_defol)
summary(aov_tissue_no_defol)
tukey <- TukeyHSD(aov_tissue_no_defol, 'tissue', ordered = TRUE, conf.level = 0.95)

#compact letter display
cld <- multcompLetters4(aov_tissue_no_defol, tukey)

#table with factors and 3rd quartile 
tissue_waje <- group_by(tissue_no_defol, tissue) %>%
  summarise(w=mean(APE),sd = sd(APE)) %>%
  arrange(desc(w))

#extracting compact letter display and adding to the Tukey table
cld <- as.data.frame.list(cld$tissue)
tissue_waje$cld <- cld$Letters
print(tissue_waje) #this prints the letters that are assigned based on sigificance, note that for the "mock" data these will not be significant

#build the graph and save as a PDF to figs folder 
pdf('./figs/tukey_nodefol_tissue.pdf') #pdf will show up in figs folder
  ggplot(tissue_waje, aes(tissue, w)) + 
    geom_bar(stat = "identity", aes(fill = tissue), show.legend = TRUE) +
    geom_errorbar(aes(ymin = w-sd, ymax=w+sd), width = 0.05) +
    labs(x = "Plant Tissue Types", y = "Atomic Percent Enrichment") +
    geom_text(aes(label = cld, y = w + sd), vjust = -0.5)
dev.off() #where to stop pdf


#CONTROL GROUP:Tukey test to see if different plant tissue groups enriched differently
#subset the data more to look at tissue groups and APE for no_defol treatment
tissue_con_no_defol <- subset(mock_foliar, treatment %in% c('con_no_defol'))
View(tissue_con_no_defol)
data.frame(tissue_con_no_defol)

#build the model and run TukeyHSD
aov_tissue_con_no_defol <- aov(APE ~ tissue, data = tissue_con_no_defol)
summary(aov_tissue_con_no_defol)
tukey <- TukeyHSD(aov_tissue_con_no_defol, 'tissue', ordered = TRUE, conf.level = 0.95)
#compact letter display
cld <- multcompLetters4(aov_tissue_con_no_defol, tukey)

#table with factors and 3rd quartile 
tissue_waje <- group_by(tissue_con_no_defol, tissue) %>%
  summarise(w=mean(APE),sd = sd(APE)) %>%
  arrange(desc(w))

#extracting compact letter display and adding to the Tukey table
cld <- as.data.frame.list(cld$tissue)
tissue_waje$cld <- cld$Letters
print(tissue_waje) #this prints the letters that are assigned based on sigificance, note that for the "mock" data these will not be significant

#build the graph and save as a PDF to figs folder 
pdf('./figs/tukey_control_nodefol_tissue.pdf') #pdf will show up in figs folder
ggplot(tissue_waje, aes(tissue, w)) + 
  geom_bar(stat = "identity", aes(fill = tissue), show.legend = TRUE) +
  geom_errorbar(aes(ymin = w-sd, ymax=w+sd), width = 0.05) +
  labs(x = "Plant Tissue Types", y = "Atomic Percent Enrichment") +
  geom_text(aes(label = cld, y = w + sd), vjust = -0.5)
dev.off() #where to stop pdf





#####0.5 Defol#############################################################################
#####In this experimental treatment, designated seedlings had 50% of their needles removed to trigger a root senescence event 
#####In the control group, cores were rotated to sever mycorrhizal connections; preventing isotopes from moving via mycorrhizae

#subset the data
defol_0.5 <- subset(mock_foliar, treatment %in% c('0.5_donor_defol', '0.5_recip_defol', 'con_0.5_donor_defol', 'con_0.5_recip_defol'))
View(defol_0.5)
data.frame(defol_0.5)

#make a graph: 
pdf('./figs/defol_0.5.pdf') #pdf will show up in figs folder
par(mfrow = c(1,1))
ggplot(defol_0.5, aes(x= treatment, y=APE,fill=treatment))+geom_boxplot()+ylab("Atomic Percent Enrichment (APE)")+xlab("Treatment")
dev.off()


##########0.5 Defol Experimental Group
###do pairwise Anova's

#Donor defoliation: donors vs recipients
#subset data a little more
defol_0.5_donor <- subset(mock_foliar, treatment %in% c('0.5_donor_defol'))
View(defol_0.5_donor)
data.frame(defol_0.5_donor)
#run the lm
lm_0.5_donor <- lm(APE ~ donor_or_recip, data=defol_0.5_donor)
Anova(lm_0.5_donor)
summary(lm_0.5_donor)

#Recipient defoliation: donor vs recipients
#subset data a little more
defol_0.5_recip <- subset(mock_foliar, treatment %in% c('0.5_recip_defol'))
View(defol_0.5_recip)
data.frame(defol_0.5_recip)
#run the lm
lm_0.5_recip <- lm(APE ~ treatment, data=defol_0.5)
Anova(lm_0.5_recip)
summary(lm_0.5_recip)

#Donor defoliation: donor vs Recipient defoliation: donor
#subset the data to get only donors 
defol_0.5_donly <- subset(defol_0.5, donor_or_recip %in% c('d'))
defol_0.5_donly <- subset(defol_0.5_donly, treatment %in% c('0.5_donor_defol', '0.5_recip_defol'))
View(defol_0.5_donly)
data.frame(defol_0.5_donly)
#run the lm
lm_defol_0.5_donly <- lm(APE ~ treatment, data=defol_0.5_donly)
Anova(lm_defol_0.5_donly)
summary(lm_defol_0.5_donly)

#Donor defoliation: recipients vs Recipient defoliation: recipients
defol_0.5_ronly <- subset(defol_0.5, donor_or_recip %in% c('r'))
defol_0.5_ronly <- subset(defol_0.5_ronly, treatment %in% c('0.5_donor_defol', '0.5_recip_defol'))
View(defol_0.5_ronly)
data.frame(defol_0.5_ronly)
#run the lm
lm_defol_0.5_ronly <- lm(APE ~ treatment, data=defol_0.5_ronly)
Anova(lm_defol_0.5_ronly)
summary(lm_defol_0.5_ronly)


##########Control Group
###do pairwise Anova's

#Donor defoliation: donors vs recipients
#subset data a little more
defol_con_0.5_donor <- subset(mock_foliar, treatment %in% c('con_0.5_donor_defol'))
data.frame(defol_con_0.5_donor)
#run the lm
lm_con_0.5_donor <- lm(APE ~ donor_or_recip, data=defol_con_0.5_donor)
Anova(lm_con_0.5_donor)
summary(lm_con_0.5_donor)

#Recipient defoliation: donor vs recipients
#subset data a little more
defol_con_0.5_recip <- subset(mock_foliar, treatment %in% c('con_0.5_recip_defol'))
data.frame(defol_con_0.5_recip)
#run the lm
lm_con_0.5_recip <- lm(APE ~ donor_or_recip, data=defol_con_0.5_recip)
Anova(lm_con_0.5_recip)
summary(lm_con_0.5_recip)

#Donor defoliation: donor vs Recipient defoliation: donor
#subset the data to get only donors 
defol_0.5_donly <- subset(defol_0.5, donor_or_recip %in% c('d'))
defol_0.5_con_donly <- subset(defol_0.5_donly, treatment %in% c('con_0.5_donor_defol', 'con_0.5_recip_defol'))
View(defol_0.5_con_donly)
data.frame(defol_0.5_con_donly)
#run the lm
lm_defol_0.5_con_donly <- lm(APE ~ treatment, data=defol_0.5_con_donly)
Anova(lm_defol_0.5_con_donly)
summary(lm_defol_0.5_con_donly)

#Donor defoliation: recipients vs Recipient defoliation: recipients
defol_0.5_ronly <- subset(defol_0.5, donor_or_recip %in% c('r'))
defol_0.5_con_ronly <- subset(defol_0.5_ronly, treatment %in% c('con_0.5_donor_defol', 'con_0.5_recip_defol'))
View(defol_0.5_con_ronly)
data.frame(defol_0.5_con_ronly)
#run the lm
lm_defol_0.5_con_ronly <- lm(APE ~ treatment, data=defol_0.5_con_ronly)
Anova(lm_defol_0.5_con_ronly)
summary(lm_defol_0.5_con_ronly)

#####do Tukey Tests for all tissue groups##############

##Experimental Group Tukey Tests####
######Donor- Donor
#subset the data more to look at tissue groups and APE for donors in the donor defoliation treatment
tissue_0.5_d_defol <- subset(mock_foliar, treatment %in% c('0.5_donor_defol'))
tissue_0.5_d_defol <- subset(tissue_0.5_d_defol, donor_or_recip %in% c('d'))
View(tissue_0.5_d_defol)
data.frame(tissue_0.5_d_defol)

#build the model and run TukeyHSD
aov_tissue_0.5_d_defol <- aov(APE ~ tissue, data = tissue_0.5_d_defol)
summary(aov_tissue_0.5_d_defol)
tukey <- TukeyHSD(aov_tissue_0.5_d_defol, 'tissue', ordered = TRUE, conf.level = 0.95)

#compact letter display
cld <- multcompLetters4(aov_tissue_0.5_d_defol, tukey)

#table with factors and 3rd quartile 
tissue_waje <- group_by(tissue_0.5_d_defol, tissue) %>%
  summarise(w=mean(APE),sd = sd(APE)) %>%
  arrange(desc(w))

#extracting compact letter display and adding to the Tukey table
cld <- as.data.frame.list(cld$tissue)
tissue_waje$cld <- cld$Letters
print(tissue_waje) #this prints the letters that are assigned based on significance, note that for the "mock" data these will not be significant

#build the graph and save as a PDF to figs folder 
pdf('./figs/tukey_0.5_donor_defol_tissue.pdf') #pdf will show up in figs folder
ggplot(tissue_waje, aes(tissue, w)) + 
  geom_bar(stat = "identity", aes(fill = tissue), show.legend = TRUE) +
  geom_errorbar(aes(ymin = w-sd, ymax=w+sd), width = 0.05) +
  labs(x = "Plant Tissue Types", y = "Atomic Percent Enrichment") +
  geom_text(aes(label = cld, y = w + sd), vjust = -0.5)
dev.off() #where to stop pdf
#note: donor seedlings needles were not analyzed, so there will be no APE data for those









######Donor- Recipients
#subset the data more to look at tissue groups and APE for recipients in the donor defoliation treatment
tissue_0.5_r_defol <- subset(mock_foliar, treatment %in% c('0.5_donor_defol'))
tissue_0.5_r_defol <- subset(tissue_0.5_r_defol, donor_or_recip %in% c('r'))
View(tissue_0.5_r_defol)
data.frame(tissue_0.5_r_defol)


######Recipients- Donors
#subset the data more to look at tissue groups and APE for donors in the recipient defoliation treatment
tissue_con_0.5_d_defol <- subset(mock_foliar, treatment %in% c('con_0.5_recip_defol'))
tissue_con_0.5_d_defol <- subset(tissue_con_0.5_d_defol, donor_or_recip %in% c('d'))
View(tissue_con_0.5_d_defol)
data.frame(tissue_con_0.5_d_defol)


######Recipients- Recipients 
#subset the data more to look at tissue groups and APE for recipients in the recipient defoliation treatment
tissue_con_0.5_r_defol <- subset(mock_foliar, treatment %in% c('con_0.5_recip_defol'))
tissue_con_0.5_r_defol <- subset(tissue_con_0.5_r_defol, donor_or_recip %in% c('r'))
View(tissue_con_0.5_r_defol)
data.frame(tissue_con_0.5_r_defol)




##Control Group Tukey Tests####
#Donor- Donor
#Donor- Recipients
#Recipients- Donors
#Recipients- Recipients 

#####0.75 Defol and 1.0 Defol######################################################################################
#in order to analyze APE for the 0.75 defol and 1.0 defol, the above code will be replicated in a different r script

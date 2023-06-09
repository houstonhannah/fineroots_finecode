#load packages
library(ggthemes)
library(ggplot2)
library(dplyr)
library(multcompView)
library(car)
library(pwr)
library(tidyr)

#load the data 
mock_foliar <- read.csv(file = './data/mock_foliar.csv')
mock_foliar$donor_or_recip <- as.factor(mock_foliar$donor_or_recip)

#drop the random X to X.6  columns 
mock_foliar <- subset.data.frame(mock_foliar, select = c('plant_num', 'isotope_lab', 'treatment', 'donor_or_recip', 'tissue', 'APE'))

#delete the NA's from the APE column
mock_foliar <- mock_foliar %>% drop_na(APE) #donor seedlings in the foliar treatment will have no APE for tissue group needles b/c they were directly labeled with isotope solution 
View(mock_foliar)




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
ggplot(mock_foliar, aes(x=treatment,y=APE,fill=treatment))+
  geom_boxplot()+ 
  ylab("Atomic Percent Enrichment (APE)")+xlab("Treatment")
dev.off() #where to stop pdf

#note: this does not separate APE for donor or recipient seedlings for each treatment
#can do that by doing fill = donor_or_recip but then the graph looks kind of crazy, see below:
pdf('./figs/mock_foliar_all_treatments_donorrecip.pdf') #pdf will show up in figs folder
par(mfrow = c(1,1))
ggplot(mock_foliar, aes(x=treatment,y=APE,fill=donor_or_recip))+
  geom_boxplot()+ylab("Atomic Percent Enrichment (APE)")+xlab("Treatment") +
  scale_fill_discrete(name = 'Seedling Type', label = c('Donors', 'Recipients'))
dev.off() #where to stop pdf



######2 x 4 Factorial Anova##################################################################
#make the model
class(mock_foliar$donor_or_recip)
contrasts(mock_foliar$donor_or_recip)
mock_foliar$donor_or_recip <- factor(mock_foliar$donor_or_recip, levels = c('r', 'd'))

lm_mock_foliar <- lm(APE ~ donor_or_recip * tissue, data = mock_foliar)
summary(lm_mock_foliar)

#check residuals of the model
par(mfrow = c(1, 2))
plot(lm_mock_foliar) 
#the residuals vs. fitted and the Scale-Location graphs: look for normal spread of points along red line, patterns show issues with homoscedasticity
#The Q-Q plot: look for points hugging red line, deviance from line shows issues with residuals
#The Residuals vs Leverage graph indicates if there are large outliers that have a disproportionate effect on the regression

#run the Anova
anova(lm_mock_foliar)                         # type 1 anova
aov_mock_foliar <- car::Anova(lm_mock_foliar) # useful if we want to do type 2 or 3 anova
aov_mock_foliar

#maybe not as useful as pairwise anova's but at least you have it!




#####0.5 Defol#############################################################################
#####In this experimental treatment, designated seedlings had 50% of their needles removed to trigger a root senescence event 
#####In the control group, cores were rotated to sever mycorrhizal connections; preventing isotopes from moving via mycorrhizae

#subset the data
defol_0.5 <- subset(mock_foliar, treatment %in% c('0.5_donor_defol', '0.5_recip_defol', 'con_0.5_donor_defol', 'con_0.5_recip_defol'))
data.frame(defol_0.5)

#make a graph: 
pdf('./figs/defol_0.5.pdf') #pdf will show up in figs folder
par(mfrow = c(1,1))
ggplot(defol_0.5, aes(x= treatment, y=APE,fill=donor_or_recip))+geom_boxplot()+ylab("Atomic Percent Enrichment (APE)")+xlab("Treatment")+scale_fill_discrete(name = 'Seedling Type', label = c('Donors', 'Recipients'))
dev.off()

#sort by donor vs recipients 
ggplot(defol_0.5, aes(x = treatment, y = APE, fill = donor_or_recip)) + 
  geom_boxplot() + 
  ylab("Atomic Percent Enrichment (APE)") + xlab("Treatment") +
  scale_fill_discrete(name = 'Seedling Type', label = c('Donors', 'Recipients')) +
  facet_wrap(~ donor_or_recip)



##########0.5 Defol Experimental Group
###do pairwise Anova's

#Donor defoliation: donors vs recipients
#subset data a little more
defol_0.5_donor <- subset(mock_foliar, treatment %in% c('0.5_donor_defol'))
data.frame(defol_0.5_donor)

#make a graph: 
pdf('./figs/0.5_ddefol_d_vs_r.pdf') #pdf will show up in figs folder
par(mfrow = c(1,1))
ggplot(defol_0.5_donor, aes(x = treatment, y=APE, fill = donor_or_recip)) +
  geom_boxplot() +
  ylab("Atomic Percent Enrichment (APE)") + xlab("Treatment") +
  scale_fill_discrete(name = 'Seedling Type', label = c('Donors', 'Recipients'))
dev.off()


#run the lm
lm_0.5_donor <- lm(APE ~ donor_or_recip, data=defol_0.5_donor)
Anova(lm_0.5_donor)
summary(lm_0.5_donor)

#Recipient defoliation: donor vs recipients
#subset data a little more
defol_0.5_recip <- subset(mock_foliar, treatment %in% c('0.5_recip_defol'))
data.frame(defol_0.5_recip)

#make a graph: 
pdf('./figs/0.5_rdefol_d_vs_r.pdf') #pdf will show up in figs folder
par(mfrow = c(1,1))
ggplot(defol_0.5_recip, aes(x = treatment, y=APE, fill = donor_or_recip)) +
  geom_boxplot() +
  ylab("Atomic Percent Enrichment (APE)") + xlab("Treatment") +
  scale_fill_discrete(name = 'Seedling Type', label = c('Donors', 'Recipients'))
dev.off()

#run the lm
lm_0.5_recip <- lm(APE ~ donor_or_recip, data=defol_0.5_recip)
Anova(lm_0.5_recip)
summary(lm_0.5_recip)

#Donor defoliation: donor vs Recipient defoliation: donor
#subset the data to get only donors 
defol_0.5_donly <- subset(defol_0.5, donor_or_recip %in% c('d'))
defol_0.5_donly <- subset(defol_0.5_donly, treatment %in% c('0.5_donor_defol', '0.5_recip_defol'))

#make a graph: 
pdf('./figs/0.5_ddefol_d_vs rdefol_d.pdf') #pdf will show up in figs folder
par(mfrow = c(1,1))
ggplot(defol_0.5_donly, aes(x = treatment, y=APE, fill = donor_or_recip)) +
  geom_boxplot() +
  ylab("Atomic Percent Enrichment (APE)") + xlab("Treatment") +
  scale_fill_discrete(name = 'Seedling Type', label = c('Donors'))
dev.off()

#run the lm
lm_defol_0.5_donly <- lm(APE ~ treatment, data=defol_0.5_donly)
Anova(lm_defol_0.5_donly)
summary(lm_defol_0.5_donly)

#Donor defoliation: recipients vs Recipient defoliation: recipients
defol_0.5_ronly <- subset(defol_0.5, donor_or_recip %in% c('r'))
defol_0.5_ronly <- subset(defol_0.5_ronly, treatment %in% c('0.5_donor_defol', '0.5_recip_defol'))


#make a graph: 
pdf('./figs/0.5_ddefol_r_vs rdefol_r.pdf') #pdf will show up in figs folder
par(mfrow = c(1,1))
ggplot(defol_0.5_ronly, aes(x = treatment, y=APE, fill = donor_or_recip)) +
  geom_boxplot(fill = 'cyan2') +
  ylab("Atomic Percent Enrichment (APE)") + xlab("Treatment") +
  scale_fill_discrete(name = 'Seedling Type', label = c('Recipients'))
dev.off() #the legend is gone here, maybe there is a way to fix that?

#run the lm
lm_defol_0.5_ronly <- lm(APE ~ treatment, data=defol_0.5_ronly)
Anova(lm_defol_0.5_ronly)
summary(lm_defol_0.5_ronly)


##########Control Group
###do pairwise Anova's

#CONTROL Donor defoliation: donors vs recipients
#subset data a little more
defol_con_0.5_donor <- subset(mock_foliar, treatment %in% c('con_0.5_donor_defol'))

#make a graph: 
pdf('./figs/control_0.5_ddefol_d_vs_r.pdf') #pdf will show up in figs folder
par(mfrow = c(1,1))
ggplot(defol_con_0.5_donor, aes(x = treatment, y=APE, fill = donor_or_recip)) +
  geom_boxplot() +
  ylab("Atomic Percent Enrichment (APE)") + xlab("Treatment") +
  scale_fill_discrete(name = 'Seedling Type', label = c('Donors', 'Recipients'))
dev.off()

#run the lm
lm_con_0.5_donor <- lm(APE ~ donor_or_recip, data=defol_con_0.5_donor)
Anova(lm_con_0.5_donor)
summary(lm_con_0.5_donor)

#CONTROL Recipient defoliation: donor vs recipients
#subset data a little more
defol_con_0.5_recip <- subset(mock_foliar, treatment %in% c('con_0.5_recip_defol'))

#make a graph: 
pdf('./figs/control_0.5_rdefol_d_vs_r.pdf') #pdf will show up in figs folder
par(mfrow = c(1,1))
ggplot(defol_con_0.5_recip, aes(x = treatment, y=APE, fill = donor_or_recip)) +
  geom_boxplot() +
  ylab("Atomic Percent Enrichment (APE)") + xlab("Treatment") +
  scale_fill_discrete(name = 'Seedling Type', label = c('Donors', 'Recipients'))
dev.off()

#run the lm
lm_con_0.5_recip <- lm(APE ~ donor_or_recip, data=defol_con_0.5_recip)
Anova(lm_con_0.5_recip)
summary(lm_con_0.5_recip)

#CONTROL Donor defoliation: donor vs Recipient defoliation: donor
#subset the data to get only donors 
defol_0.5_donly <- subset(defol_0.5, donor_or_recip %in% c('d'))
defol_0.5_con_donly <- subset(defol_0.5_donly, treatment %in% c('con_0.5_donor_defol', 'con_0.5_recip_defol'))

#make a graph: 
pdf('./figs/control_0.5_ddefol_d_vs_rdefol_d.pdf') #pdf will show up in figs folder
par(mfrow = c(1,1))
ggplot(defol_0.5_con_donly, aes(x = treatment, y=APE, fill = donor_or_recip)) +
  geom_boxplot() +
  ylab("Atomic Percent Enrichment (APE)") + xlab("Treatment") +
  scale_fill_discrete(name = 'Seedling Type', label = c('Donors'))
dev.off()

#run the lm
lm_defol_0.5_con_donly <- lm(APE ~ treatment, data=defol_0.5_con_donly)
Anova(lm_defol_0.5_con_donly)
summary(lm_defol_0.5_con_donly)

#CONTROL Donor defoliation: recipients vs Recipient defoliation: recipients
defol_0.5_ronly <- subset(defol_0.5, donor_or_recip %in% c('r'))
defol_0.5_con_ronly <- subset(defol_0.5_ronly, treatment %in% c('con_0.5_donor_defol', 'con_0.5_recip_defol'))

#make a graph: 
pdf('./figs/control_0.5_ddefol_r_vs rdefol_r.pdf') #pdf will show up in figs folder
par(mfrow = c(1,1))
ggplot(defol_0.5_con_ronly, aes(x = treatment, y=APE, fill = donor_or_recip)) +
  geom_boxplot(fill = 'cyan2') +
  ylab("Atomic Percent Enrichment (APE)") + xlab("Treatment") +
  scale_fill_discrete(name = 'Seedling Type', label = c('Recipients'))
dev.off() #the legend is gone here, maybe there is a way to fix that?

#run the lm
lm_defol_0.5_con_ronly <- lm(APE ~ treatment, data=defol_0.5_con_ronly)
Anova(lm_defol_0.5_con_ronly)
summary(lm_defol_0.5_con_ronly)



#####do Tukey Tests for all tissue groups##############

##Experimental Group Tukey Tests####
######Donor- Donor
#subset the data more to look at tissue groups and APE for donors in the donor defoliation treatment
tissue_0.5_ddefol_d <- subset(mock_foliar, treatment %in% c('0.5_donor_defol'))
tissue_0.5_ddefol_d <- subset(tissue_0.5_ddefol_d, donor_or_recip %in% c('d'))


#build the model and run TukeyHSD
aov_tissue_0.5_ddefol_d <- aov(APE ~ tissue, data = tissue_0.5_ddefol_d)
summary(aov_tissue_0.5_ddefol_d)
tukey <- TukeyHSD(aov_tissue_0.5_ddefol_d, 'tissue', ordered = TRUE, conf.level = 0.95)

#compact letter display
cld <- multcompLetters4(aov_tissue_0.5_ddefol_d, tukey)

#table with factors and 3rd quartile 
tissue_waje <- group_by(tissue_0.5_ddefol_d, tissue) %>%
  summarise(w=mean(APE),sd = sd(APE)) %>%
  arrange(desc(w))

#extracting compact letter display and adding to the Tukey table
cld <- as.data.frame.list(cld$tissue)
tissue_waje$cld <- cld$Letters
print(tissue_waje) #this prints the letters that are assigned based on significance, note that for the "mock" data these will not be significant

#build the graph and save as a PDF to figs folder 
pdf('./figs/tukey_0.5_donor_defol_donor_tissue.pdf') #pdf will show up in figs folder
ggplot(tissue_waje, aes(tissue, w)) + 
  geom_bar(stat = "identity", aes(fill = tissue), show.legend = TRUE) +
  geom_errorbar(aes(ymin = w-sd, ymax=w+sd), width = 0.05) +
  labs(x = "Plant Tissue Types", y = "Atomic Percent Enrichment") +
  geom_text(aes(label = cld, y = w + sd), vjust = -0.5)
dev.off() #where to stop pdf
#note: donor seedlings needles were not analyzed, so there will be no APE data for those


######Donor- Recipients
#subset the data more to look at tissue groups and APE for recipients in the donor defoliation treatment
tissue_0.5_ddefol_r <- subset(mock_foliar, treatment %in% c('0.5_donor_defol'))
tissue_0.5_ddefol_r <- subset(tissue_0.5_ddefol_r, donor_or_recip %in% c('r'))

#build the model and run TukeyHSD
aov_tissue_0.5_ddefol_r <- aov(APE ~ tissue, data = tissue_0.5_ddefol_r)
summary(aov_tissue_0.5_ddefol_r)
tukey <- TukeyHSD(aov_tissue_0.5_ddefol_r, 'tissue', ordered = TRUE, conf.level = 0.95)

#compact letter display
cld <- multcompLetters4(aov_tissue_0.5_ddefol_r, tukey)

#table with factors and 3rd quartile 
tissue_waje <- group_by(tissue_0.5_ddefol_r, tissue) %>%
  summarise(w=mean(APE),sd = sd(APE)) %>%
  arrange(desc(w))

#extracting compact letter display and adding to the Tukey table
cld <- as.data.frame.list(cld$tissue)
tissue_waje$cld <- cld$Letters
print(tissue_waje) #this prints the letters that are assigned based on significance, note that for the "mock" data these will not be significant

#build the graph and save as a PDF to figs folder 
pdf('./figs/tukey_0.5_donor_defol_recip_tissue.pdf') #pdf will show up in figs folder
ggplot(tissue_waje, aes(tissue, w)) + 
  geom_bar(stat = "identity", aes(fill = tissue), show.legend = TRUE) +
  geom_errorbar(aes(ymin = w-sd, ymax=w+sd), width = 0.05) +
  labs(x = "Plant Tissue Types", y = "Atomic Percent Enrichment") +
  geom_text(aes(label = cld, y = w + sd), vjust = -0.5)
dev.off() #where to stop pdf


######Recipients- Donors
#subset the data more to look at tissue groups and APE for donors in the recipient defoliation treatment
tissue_0.5_rdefol_d <- subset(mock_foliar, treatment %in% c('0.5_recip_defol'))
tissue_0.5_rdefol_d <- subset(tissue_0.5_rdefol_d, donor_or_recip %in% c('d'))

#build the model and run TukeyHSD
aov_tissue_0.5_rdefol_d <- aov(APE ~ tissue, data = tissue_0.5_rdefol_d)
summary(aov_tissue_0.5_rdefol_d)
tukey <- TukeyHSD(aov_tissue_0.5_rdefol_d, 'tissue', ordered = TRUE, conf.level = 0.95)

#compact letter display
cld <- multcompLetters4(aov_tissue_0.5_rdefol_d, tukey)

#table with factors and 3rd quartile 
tissue_waje <- group_by(tissue_0.5_rdefol_d, tissue) %>%
  summarise(w=mean(APE),sd = sd(APE)) %>%
  arrange(desc(w))

#extracting compact letter display and adding to the Tukey table
cld <- as.data.frame.list(cld$tissue)
tissue_waje$cld <- cld$Letters
print(tissue_waje) #this prints the letters that are assigned based on significance, note that for the "mock" data these will not be significant

#build the graph and save as a PDF to figs folder 
pdf('./figs/tukey_0.5_recip_defol_donor_tissue.pdf') #pdf will show up in figs folder
ggplot(tissue_waje, aes(tissue, w)) + 
  geom_bar(stat = "identity", aes(fill = tissue), show.legend = TRUE) +
  geom_errorbar(aes(ymin = w-sd, ymax=w+sd), width = 0.05) +
  labs(x = "Plant Tissue Types", y = "Atomic Percent Enrichment") +
  geom_text(aes(label = cld, y = w + sd), vjust = -0.5)
dev.off() #where to stop pdf


######Recipients- Recipients 
#subset the data more to look at tissue groups and APE for recipients in the recipient defoliation treatment
tissue_0.5_rdefol_r <- subset(mock_foliar, treatment %in% c('0.5_recip_defol'))
tissue_0.5_rdefol_r <- subset(tissue_0.5_rdefol_r, donor_or_recip %in% c('r'))

#build the model and run TukeyHSD
aov_tissue_0.5_rdefol_r <- aov(APE ~ tissue, data = tissue_0.5_rdefol_r)
summary(aov_tissue_0.5_rdefol_r)
tukey <- TukeyHSD(aov_tissue_0.5_rdefol_r, 'tissue', ordered = TRUE, conf.level = 0.95)

#compact letter display
cld <- multcompLetters4(aov_tissue_0.5_rdefol_r, tukey)

#table with factors and 3rd quartile 
tissue_waje <- group_by(tissue_0.5_rdefol_r, tissue) %>%
  summarise(w=mean(APE),sd = sd(APE)) %>%
  arrange(desc(w))

#extracting compact letter display and adding to the Tukey table
cld <- as.data.frame.list(cld$tissue)
tissue_waje$cld <- cld$Letters
print(tissue_waje) #this prints the letters that are assigned based on significance, note that for the "mock" data these will not be significant

#build the graph and save as a PDF to figs folder 
pdf('./figs/tukey_0.5_recip_defol_recip_tissue.pdf') #pdf will show up in figs folder
ggplot(tissue_waje, aes(tissue, w)) + 
  geom_bar(stat = "identity", aes(fill = tissue), show.legend = TRUE) +
  geom_errorbar(aes(ymin = w-sd, ymax=w+sd), width = 0.05) +
  labs(x = "Plant Tissue Types", y = "Atomic Percent Enrichment") +
  geom_text(aes(label = cld, y = w + sd), vjust = -0.5)
dev.off() #where to stop pdf


##Control Group Tukey Tests####

######Control Donor- Donor
#subset the data more to look at tissue groups and APE for donors in the donor defoliation treatment
tissue_con_0.5_ddefol_d <- subset(mock_foliar, treatment %in% c('con_0.5_donor_defol'))
tissue_con_0.5_ddefol_d <- subset(tissue_con_0.5_ddefol_d, donor_or_recip %in% c('d'))

#build the model and run TukeyHSD
aov_tissue_con_0.5_ddefol_d <- aov(APE ~ tissue, data = tissue_con_0.5_ddefol_d)
summary(aov_tissue_con_0.5_ddefol_d)
tukey <- TukeyHSD(aov_tissue_con_0.5_ddefol_d, 'tissue', ordered = TRUE, conf.level = 0.95)

#compact letter display
cld <- multcompLetters4(aov_tissue_con_0.5_ddefol_d, tukey)

#table with factors and 3rd quartile 
tissue_waje <- group_by(tissue_con_0.5_ddefol_d, tissue) %>%
  summarise(w=mean(APE),sd = sd(APE)) %>%
  arrange(desc(w))

#extracting compact letter display and adding to the Tukey table
cld <- as.data.frame.list(cld$tissue)
tissue_waje$cld <- cld$Letters
print(tissue_waje) #this prints the letters that are assigned based on significance, note that for the "mock" data these will not be significant

#build the graph and save as a PDF to figs folder 
pdf('./figs/tukey_control_0.5_donor_defol_donor_tissue.pdf') #pdf will show up in figs folder
ggplot(tissue_waje, aes(tissue, w)) + 
  geom_bar(stat = "identity", aes(fill = tissue), show.legend = TRUE) +
  geom_errorbar(aes(ymin = w-sd, ymax=w+sd), width = 0.05) +
  labs(x = "Plant Tissue Types", y = "Atomic Percent Enrichment") +
  geom_text(aes(label = cld, y = w + sd), vjust = -0.5)
dev.off() #where to stop pdf
#note: donor seedlings needles were not analyzed, so there will be no APE data for those


######Control Donor- Recipients
#subset the data more to look at tissue groups and APE for recipients in the donor defoliation treatment
tissue_con_0.5_ddefol_r <- subset(mock_foliar, treatment %in% c('con_0.5_donor_defol'))
tissue_con_0.5_ddefol_r <- subset(tissue_con_0.5_ddefol_r, donor_or_recip %in% c('r'))

#build the model and run TukeyHSD
aov_tissue_con_0.5_ddefol_r <- aov(APE ~ tissue, data = tissue_con_0.5_ddefol_r)
summary(aov_tissue_con_0.5_ddefol_r)
tukey <- TukeyHSD(aov_tissue_con_0.5_ddefol_r, 'tissue', ordered = TRUE, conf.level = 0.95)

#compact letter display
cld <- multcompLetters4(aov_tissue_con_0.5_ddefol_r, tukey)

#table with factors and 3rd quartile 
tissue_waje <- group_by(tissue_con_0.5_ddefol_r, tissue) %>%
  summarise(w=mean(APE),sd = sd(APE)) %>%
  arrange(desc(w))

#extracting compact letter display and adding to the Tukey table
cld <- as.data.frame.list(cld$tissue)
tissue_waje$cld <- cld$Letters
print(tissue_waje) #this prints the letters that are assigned based on significance, note that for the "mock" data these will not be significant

#build the graph and save as a PDF to figs folder 
pdf('./figs/tukey_control_0.5_donor_defol_recip_tissue.pdf') #pdf will show up in figs folder
ggplot(tissue_waje, aes(tissue, w)) + 
  geom_bar(stat = "identity", aes(fill = tissue), show.legend = TRUE) +
  geom_errorbar(aes(ymin = w-sd, ymax=w+sd), width = 0.05) +
  labs(x = "Plant Tissue Types", y = "Atomic Percent Enrichment") +
  geom_text(aes(label = cld, y = w + sd), vjust = -0.5)
dev.off() #where to stop pdf


######Control Recipients- Donors
#subset the data more to look at tissue groups and APE for donors in the recipient defoliation treatment
tissue_con_0.5_rdefol_d <- subset(mock_foliar, treatment %in% c('con_0.5_recip_defol'))
tissue_con_0.5_rdefol_d <- subset(tissue_con_0.5_rdefol_d, donor_or_recip %in% c('d'))

#build the model and run TukeyHSD
aov_tissue_con_0.5_rdefol_d <- aov(APE ~ tissue, data = tissue_con_0.5_rdefol_d)
summary(aov_tissue_con_0.5_rdefol_d)
tukey <- TukeyHSD(aov_tissue_con_0.5_rdefol_d, 'tissue', ordered = TRUE, conf.level = 0.95)

#compact letter display
cld <- multcompLetters4(aov_tissue_con_0.5_rdefol_d, tukey)

#table with factors and 3rd quartile 
tissue_waje <- group_by(tissue_con_0.5_rdefol_d, tissue) %>%
  summarise(w=mean(APE),sd = sd(APE)) %>%
  arrange(desc(w))

#extracting compact letter display and adding to the Tukey table
cld <- as.data.frame.list(cld$tissue)
tissue_waje$cld <- cld$Letters
print(tissue_waje) #this prints the letters that are assigned based on significance, note that for the "mock" data these will not be significant

#build the graph and save as a PDF to figs folder 
pdf('./figs/tukey_control_0.5_recip_defol_donor_tissue.pdf') #pdf will show up in figs folder
ggplot(tissue_waje, aes(tissue, w)) + 
  geom_bar(stat = "identity", aes(fill = tissue), show.legend = TRUE) +
  geom_errorbar(aes(ymin = w-sd, ymax=w+sd), width = 0.05) +
  labs(x = "Plant Tissue Types", y = "Atomic Percent Enrichment") +
  geom_text(aes(label = cld, y = w + sd), vjust = -0.5)
dev.off() #where to stop pdf


######Control Recipients- Recipients 
#subset the data more to look at tissue groups and APE for recipients in the recipient defoliation treatment
tissue_con_0.5_rdefol_r <- subset(mock_foliar, treatment %in% c('con_0.5_recip_defol'))
tissue_con_0.5_rdefol_r <- subset(tissue_con_0.5_rdefol_r, donor_or_recip %in% c('r'))

#build the model and run TukeyHSD
aov_tissue_con_0.5_rdefol_r <- aov(APE ~ tissue, data = tissue_con_0.5_rdefol_r)
summary(aov_tissue_con_0.5_rdefol_r)
tukey <- TukeyHSD(aov_tissue_con_0.5_rdefol_r, 'tissue', ordered = TRUE, conf.level = 0.95)

#compact letter display
cld <- multcompLetters4(aov_tissue_con_0.5_rdefol_r, tukey)

#table with factors and 3rd quartile 
tissue_waje <- group_by(tissue_con_0.5_rdefol_r, tissue) %>%
  summarise(w=mean(APE),sd = sd(APE)) %>%
  arrange(desc(w))

#extracting compact letter display and adding to the Tukey table
cld <- as.data.frame.list(cld$tissue)
tissue_waje$cld <- cld$Letters
print(tissue_waje) #this prints the letters that are assigned based on significance, note that for the "mock" data these will not be significant

#build the graph and save as a PDF to figs folder 
pdf('./figs/tukey_control_0.5_recip_defol_recip_tissue.pdf') #pdf will show up in figs folder
ggplot(tissue_waje, aes(tissue, w)) + 
  geom_bar(stat = "identity", aes(fill = tissue), show.legend = TRUE) +
  geom_errorbar(aes(ymin = w-sd, ymax=w+sd), width = 0.05) +
  labs(x = "Plant Tissue Types", y = "Atomic Percent Enrichment") +
  geom_text(aes(label = cld, y = w + sd), vjust = -0.5)
dev.off() #where to stop pdf



#####0.0 Defol, 0.75 Defol and 1.0 Defol######################################################################################
#in order to analyze APE for the 0.0 defol, 0.75 defol and 1.0 defol, the above code will be replicated in a different r script

#load the data 
mock_foliar <- read.csv(file = './data/mock_foliar.csv')
head(mock_foliar)

#####Power Analysis############################################################################# 
#####Conduct a power analysis to determine if there are enough experimental replications to detect a trend




#make a graph looking at all treatment types and export as graph to figs folder
library(ggplot2)
pdf('./figs/mock_foliar_all_treatments.pdf') #pdf will show up in figs folder
par(mfrow = c(1,1))
ggplot(mock_foliar, aes(x=treatment,y=APE,fill=treatment))+geom_boxplot()+ylab("Atomic Percent Enrichment (APE)")+xlab("Treatment")
dev.off() #where to stop pdf





#####No Defol############################################################################# 
#####No defoliaiton occured in this experimental treatment. 
#####In the control group, cores were rotated to sever mycorrhizal connections.

#subset the data
no_defol <- subset(mock_foliar, treatment %in% c('no_defol', 'con_no_defol'))
View(no_defol)
data.frame(no_defol)

#make a graph: 
pdf('./figs/no_defol.vs.con_no_defol.pdf') #pdf will show up in figs folder
par(mfrow = c(1,1))
ggplot(no_defol, aes(x= treatment, y=APE,fill=treatment))+geom_boxplot()+ylab("Atomic Percent Enrichment (APE)")+xlab("Treatment")
dev.off()

#Create a model: no defol vs. control no defol
library(car)
lm_no_defol <- lm(APE ~ treatment, data=no_defol)
Anova(lm_no_defol)
summary(lm_no_defol)

#check residuals of the model
par(mfrow = c(2, 2))
plot(lm_no_defol) 
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
TukeyHSD(aov_tissue_no_defol, 'tissue', ordered = TRUE, conf.level = 0.95)
plot(TukeyHSD(aov_no_defol, 'tissue'))




#####0.5 Defol#############################################################################
#####In this experimental treatment, designated seedlings had 50% of their needles removed to trigger a root senescence event 
#####In the control group, cores were rotated to sever mycorrhizal connections

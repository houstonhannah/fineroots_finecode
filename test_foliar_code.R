#' ---
#' author: "Hannah Houston"
#' title: "Test of Foliar Labeling: Rotated Cores"
#' date: "22 May 2023"
#' output: html_document
#' ---
#' 
  
---
#' **Test of foliar isotopic labeling on loblolly pine seedlings (*Pinus taeda*): Rotated Cores**
---
---
#' Load packages and download data 
---

  #load packages
library(ggthemes)
library(ggplot2)
library(dplyr)
library(multcompView)
library(car)
library(pwr)
library(tidyr)

#download data
test_foliar <- read.csv(file = './data/results_test_foliar.csv')
    # will use 15N vs. At Air: corrected isotope delta value* for 15N measured against a primary reference scale. The primary reference scale for 15N is Atmospheric Air.
    # will use 13C vs. VPDB – This is the corrected isotope delta value* for 13C measured against a primary reference scale. The primary reference scale for 13C is Vienna Pee Dee Belemnite.
        #Note: Delta values are measured in units of per mil (‰)

#rename some columns so they are easier to work with
    #rename 12th column to N
    colnames(test_foliar)[12] ="N"
    #rename 15th column to C
    colnames(test_foliar)[15] ="C"
#drop the random extra rows
    test_foliar <- test_foliar[-(232:1067),]

    
    
    
    

#make a scatterplot with some trendlines for N
#pdf('./figs/mock_foliar_all_treatments_donorrecip.pdf') #pdf will show up in figs folder
  ggplot(data=test_foliar, aes(x = defoliation, y =N, color = rotated))+
  geom_boxplot()+ 
  ylab('N')+xlab('Percent Defoliation')
#dev.off() #where to stop pdf

#make a scatterplot with some trendlines for C
 
   
 
  
  
  
  
  
#sort by donor vs recipients and rotation
  ggplot(test_foliar, aes(x = treatment, y = N, fill = donor_or_recip)) + 
    geom_boxplot() + 
    ylab("15N") + xlab("Treatment") +
    scale_fill_discrete(name = 'Seedling Type', label = c('Donors', 'Recipients')) +
    facet_wrap(~ donor_or_recip)  
  
  
#sort by donor vs recipients and rotation
  ggplot(test_foliar, aes(x = treatment, y = N, fill = donor_or_recip)) + 
    geom_boxplot() + 
    ylab("15N") + xlab("Treatment") +
    scale_fill_discrete(name = 'Seedling Type', label = c('Donors', 'Recipients')) +
    facet_wrap(~ donor_or_recip + rotated)
 
---    
#'Separate ANOVAs to see effect of defoliation 
#'and tissue type on  C and N 
#'(data analyzed separately for donor and recipient plants and rotated and not rotated)
---  
#available treatments: 
  #rotated control 0.6, 1.0, and no defol 
  #non-rotated defoliate .4, .6, .8, 1.0
    

    
#Did the labeling work relative to unlabeled recipients? can only answer this for 0.6 and 1.0

########0.6 defol: control (rotated) vs. nonrotated######################### 
#Donor defoliation: donors vs recipients N
#subset data a little more
defol_0.6 <- subset(test_foliar, treatment %in% c('defoliate_0.6', 'control_0.6_defol'))

#make a graph: 
#pdf('./figs/0.5_ddefol_d_vs_r.pdf') #pdf will show up in figs folder
  par(mfrow = c(1,1))
  ggplot(defol_0.6, aes(x = treatment, y=N, fill = donor_or_recip)) +
    geom_boxplot() +
    ylab("15N") + xlab("Treatment") +
    scale_fill_discrete(name = 'Seedling Type', label = c('Donors', 'Recipients'))
#dev.off()

  
#run the lm
  lm_0.6_N <- lm(N ~ donor_or_recip, data=defol_0.6)
  Anova(lm_0.6_N)
  summary(lm_0.6_N)
    
#' there is a difference between N label in donor and recipients here


#Donor defoliation: donors vs recipients C

  #make a graph: 
  #pdf('./figs/0.5_ddefol_d_vs_r.pdf') #pdf will show up in figs folder
  par(mfrow = c(1,1))
  ggplot(defol_0.6, aes(x = treatment, y=C, fill = donor_or_recip)) +
    geom_boxplot() +
    ylab("13C") + xlab("Treatment") +
    scale_fill_discrete(name = 'Seedling Type', label = c('Donors', 'Recipients'))
  #dev.off()

#' The control (rotated) and the non-rotated graphs look basically the same, 
#' it looks like the label took well but did not make it into the recipient in either case
#' I'd say defoliation did not initiate nutrient transfer here
  
#run the lm
  lm_0.6_C <- lm(C ~ donor_or_recip, data=defol_0.6)
  Anova(lm_0.6_C)
  summary(lm_0.6_C)
    
#'The labeling worked, there is a significant difference in 13C between donor and recipient seedlings
    

########1.0 defol: control (rotated) vs. nonrotated######################### 
#Donor defoliation: donors vs recipients N
#subset data a little more
  defol_1.0 <- subset(test_foliar, treatment %in% c('defoliate_1.0', 'control_1.0_defol'))
  
  #make a graph: 
  #pdf('./figs/0.5_ddefol_d_vs_r.pdf') #pdf will show up in figs folder
  par(mfrow = c(1,1))
  ggplot(defol_1.0, aes(x = treatment, y=N, fill = donor_or_recip)) +
    geom_boxplot() +
    ylab("15N") + xlab("Treatment") +
    scale_fill_discrete(name = 'Seedling Type', label = c('Donors', 'Recipients'))
  #dev.off()
  
  
  #run the lm
  lm_0.6_N <- lm(N ~ donor_or_recip, data=defol_0.6)
  Anova(lm_0.6_N)
  summary(lm_0.6_N)
  #' there is a difference between N label in donor and recipients here
  
  
#Donor defoliation: donors vs recipients C
  
#make a graph: 
  #pdf('./figs/0.5_ddefol_d_vs_r.pdf') #pdf will show up in figs folder
  par(mfrow = c(1,1))
  ggplot(defol_1.0, aes(x = treatment, y=C, fill = donor_or_recip)) +
    geom_boxplot() +
    ylab("13C") + xlab("Treatment") +
    scale_fill_discrete(name = 'Seedling Type', label = c('Donors', 'Recipients'))
  #dev.off()
  
#' The control (rotated) and the non-rotated graphs look basically the same, 
#' it looks like the label took well but did not make it into the recipient in either case
#' I'd say defoliation did not initiate nutrient transfer here
  
#run the lm
  lm_0.6_C <- lm(C ~ donor_or_recip, data=defol_0.6)
  Anova(lm_0.6_C)
  summary(lm_0.6_C)
  
#'The labeling worked, there is a significant difference in 13C between donor and recipient seedlings
  
  
  
---  
#' *Since the label looks like it worked well, let's see how the label appeared in different tissues*
---
#' all seedlings were labeled, so I will be looking at tissue differentiation in donor seedlings regardless of treatment
#' note: since label was applied directly to needles, they will not be included as a plat tissue when evaluating donor seedlings

#filter the data a little 
  donors <- subset(test_foliar, donor_or_recip %in% c('d'))
  
#Tukey HSD test/graph for N
  #build the model and run TukeyHSD
  aov_donors <- aov(N ~ tissue, data = donors)
  summary(aov_donors)
  tukey <- TukeyHSD(aov_donors, 'tissue', ordered = TRUE, conf.level = 0.95)
  
  #compact letter display
  cld <- multcompLetters4(aov_donors, tukey)
  
  #table with factors and 3rd quartile 
  tissue_waje <- group_by(donors, tissue) %>%
    summarise(w=mean(N),sd = sd(N)) %>%
    arrange(desc(w))
  
  #extracting compact letter display and adding to the Tukey table
  cld <- as.data.frame.list(cld$tissue)
  tissue_waje$cld <- cld$Letters
  print(tissue_waje) #this prints the letters that are assigned based on significance, note that for the "mock" data these will not be significant
  
  #build the graph and save as a PDF to figs folder 
  #pdf('./figs/tukey_control_0.5_recip_defol_recip_tissue.pdf') #pdf will show up in figs folder
  ggplot(tissue_waje, aes(tissue, w)) + 
    geom_bar(stat = "identity", aes(fill = tissue), show.legend = TRUE) +
    geom_errorbar(aes(ymin = w-sd, ymax=w+sd), width = 0.05) +
    labs(x = "Plant Tissue Types", y = "15N") +
    geom_text(aes(label = cld, y = w + sd), vjust = -0.5)
  #dev.off() #where to stop pdf
  
#graph being weird, investigate non-numeric arguments for N 
  

#Tukey HSD test/graph for C
  #build the model and run TukeyHSD
  aov_donors <- aov(C ~ tissue, data = donors)
  summary(aov_donors)
  tukey <- TukeyHSD(aov_donors, 'tissue', ordered = TRUE, conf.level = 0.95)
  
  #compact letter display
  cld <- multcompLetters4(aov_donors, tukey)
  
  #table with factors and 3rd quartile 
  tissue_waje <- group_by(donors, tissue) %>%
    summarise(w=mean(C),sd = sd(C)) %>%
    arrange(desc(w))
  
  #extracting compact letter display and adding to the Tukey table
  cld <- as.data.frame.list(cld$tissue)
  tissue_waje$cld <- cld$Letters
  print(tissue_waje) #this prints the letters that are assigned based on significance, note that for the "mock" data these will not be significant
  
  #build the graph and save as a PDF to figs folder 
  #pdf('./figs/tukey_control_0.5_recip_defol_recip_tissue.pdf') #pdf will show up in figs folder
  ggplot(tissue_waje, aes(tissue, w)) + 
    geom_bar(stat = "identity", aes(fill = tissue), show.legend = TRUE) +
    geom_errorbar(aes(ymin = w-sd, ymax=w+sd), width = 0.05) +
    labs(x = "Plant Tissue Types", y = "13C") +
    geom_text(aes(label = cld, y = w + sd), vjust = -0.5)
  #dev.off() #where to stop pdf
  
#'Most of the isotope label appeared in the stem, could be from label dripping onto stem surface instead of actual tissue incorporation?
#'Roots have negative 13C values, because it is a corrected isotope delta value for 13C measured against a primary reference scale?
#'
#'
#'Let's take a look at 15N and 13C in recipient (unlabeled) seedlings just for fun 
#'
  #filter the data a little 
  recipients <- subset(test_foliar, donor_or_recip %in% c('r'))
#need to remove one lowroot line b/c NA
recipients <- na.omit(recipients)
  
  #Tukey HSD test/graph for N
  #build the model and run TukeyHSD
  aov_recipients <- aov(N ~ tissue, data = recipients)
  summary(aov_recipients)
  tukey <- TukeyHSD(aov_recipients, 'tissue', ordered = TRUE, conf.level = 0.95)
  
  #compact letter display
  cld <- multcompLetters4(aov_recipients, tukey)
  
  #table with factors and 3rd quartile 
  tissue_waje <- group_by(recipients, tissue) %>%
    summarise(w=mean(N),sd = sd(N)) %>%
    arrange(desc(w))
  
  #extracting compact letter display and adding to the Tukey table
  cld <- as.data.frame.list(cld$tissue)
  tissue_waje$cld <- cld$Letters
  print(tissue_waje) #this prints the letters that are assigned based on significance, note that for the "mock" data these will not be significant
  
  #build the graph and save as a PDF to figs folder 
  #pdf('./figs/tukey_control_0.5_recip_defol_recip_tissue.pdf') #pdf will show up in figs folder
  ggplot(tissue_waje, aes(tissue, w)) + 
    geom_bar(stat = "identity", aes(fill = tissue), show.legend = TRUE) +
    geom_errorbar(aes(ymin = w-sd, ymax=w+sd), width = 0.05) +
    labs(x = "Plant Tissue Types", y = "15N") +
    geom_text(aes(label = cld, y = w + sd), vjust = -0.5)
  #dev.off() #where to stop pdf
  
  
#'Interesting that needles have a different 15N signal than roots or stem
#'Why?
  
  
#Tukey HSD test/graph for C
  #build the model and run TukeyHSD
  aov_recipients <- aov(C ~ tissue, data = recipients)
  summary(aov_recipients)
  tukey <- TukeyHSD(aov_recipients, 'tissue', ordered = TRUE, conf.level = 0.95)
  
  #compact letter display
  cld <- multcompLetters4(aov_recipients, tukey)
  
  #table with factors and 3rd quartile 
  tissue_waje <- group_by(recipients, tissue) %>%
    summarise(w=mean(C),sd = sd(C)) %>%
    arrange(desc(w))
  
  #extracting compact letter display and adding to the Tukey table
  cld <- as.data.frame.list(cld$tissue)
  tissue_waje$cld <- cld$Letters
  print(tissue_waje) #this prints the letters that are assigned based on significance, note that for the "mock" data these will not be significant
  
  #build the graph and save as a PDF to figs folder 
  #pdf('./figs/tukey_control_0.5_recip_defol_recip_tissue.pdf') #pdf will show up in figs folder
  ggplot(tissue_waje, aes(tissue, w)) + 
    geom_bar(stat = "identity", aes(fill = tissue), show.legend = TRUE) +
    geom_errorbar(aes(ymin = w-sd, ymax=w+sd), width = 0.05) +
    labs(x = "Plant Tissue Types", y = "13C") +
    geom_text(aes(label = cld, y = w + sd), vjust = -0.5)
  #dev.off() #where to stop pdf
  
#' needles and stem are different this time, why?
#' also why is this super negative?

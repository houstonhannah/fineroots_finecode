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
  
  
#sort by donor vs recipients and rotation for 15N
  # Function to customize facet labels
  custom_labeller <- function(variable, value) {
    if (variable == "donor_or_recip") {
      if (value == "d") return ("")
      if (value == "r") return ("")
    }
    if (variable == "rotated") {
      if (value == "no") return ("not rotated")
      if (value == "yes") return ("rotated")
    }
    return (value)
  }
  
  ggplot(test_foliar, aes(x = treatment, y = N, fill = donor_or_recip)) + 
    geom_boxplot() + 
    ylab("15N") + xlab("Treatment") +
    scale_fill_discrete(name = 'Seedling Type', label = c('Donors', 'Recipients')) +
    facet_wrap(~ donor_or_recip + rotated, scales = "free", labeller = custom_labeller) +
    ggtitle('Tissue 15N Content') + theme(plot.title = element_text(hjust = 0.5))
    
  
  
  

  
  
  
  
  
  
#sort by donor vs recipients and rotation for 13C
  ggplot(test_foliar, aes(x = treatment, y = C, fill = donor_or_recip)) + 
    geom_boxplot() + 
    ylab("13C") + xlab("Treatment") +
    scale_fill_discrete(name = 'Seedling Type', label = c('Donors', 'Recipients')) +
    facet_wrap(~ donor_or_recip + rotated)
 
  
---    
#'available treatments: 
  #'rotated control 0.6, 1.0, and no defol 
  #'non-rotated defoliate 0.4, 0.6, 0.8, 1.0
---  

########0.6 defol: control (rotated) vs. nonrotated######################### 
#Donor defoliation: donors vs recipients N
#subset data a little more
defol_0.6 <- subset(test_foliar, treatment %in% c('defoliate_0.6', 'control_0.6_defol'))

#subset a little more to remove needles since we don't have that info for donors
defol_0.6_nn <- subset(defol_0.6, tissue %in% c('stem', 'lowroot', 'highroot'))

#make a graph: 
#pdf('./figs/0.5_ddefol_d_vs_r.pdf') #pdf will show up in figs folder
  par(mfrow = c(1,1))
  ggplot(defol_0.6_nn, aes(x = treatment, y=N, fill = donor_or_recip)) +
    geom_boxplot() +
    ylab("15N") + xlab("Treatment") +
    scale_fill_discrete(name = 'Seedling Type', label = c('Donors', 'Recipients'))
#dev.off()

#There is a slight difference in N in donors in the control (rotated cores, control _0.6_defol) vs. the non-rotated (defoliate_0.6), investigate by looking at individual tissue groups

#make a graph for individual tissue groups for donors 0.6 defol for N
# Custom labels for the x-axis
custom_labels <- c('rotated', 'not rotated')

# Function to customize facet labels
custom_labeller <- function(variable, value) {
  if (variable == "donor_or_recip") {
    if (value == "d") return ("")
    if (value == "r") return ("")
  }
  return (value)
}

#make the plot
ggplot(defol_0.6_nn, aes(x = treatment, y = N, fill = donor_or_recip)) +
  geom_boxplot() +
  facet_wrap(~ tissue + donor_or_recip, scales = "free", labeller = custom_labeller) +
  scale_fill_discrete(name = 'Seedling Type', label = c('Donors', 'Recipients')) +
  scale_x_discrete(labels = custom_labels) + ylab('15N') +
  ggtitle('60% Defoliation: Tissue 15N Content')+ theme(plot.title = element_text(hjust = 0.5))
    
#run the lm
  lm_0.6_N <- lm(N ~ donor_or_recip * treatment * tissue,subset=tissue!="needles", data=defol_0.6)
  Anova(lm_0.6_N)
  

#Donor defoliation: donors vs recipients C

  #make a graph: 
  #pdf('./figs/0.5_ddefol_d_vs_r.pdf') #pdf will show up in figs folder
  par(mfrow = c(1,1))
  ggplot(defol_0.6, aes(x = treatment, y=C, fill = donor_or_recip)) +
    geom_boxplot() +
    ylab("13C") + xlab("Treatment") +
    scale_fill_discrete(name = 'Seedling Type', label = c('Donors', 'Recipients'))
  #dev.off()

#make a graph for individual tissue groups for donors 0.6 defol for C
# Custom labels for the x-axis
custom_labels <- c('rotated', 'not rotated')

# Function to customize facet labels
custom_labeller <- function(variable, value) {
  if (variable == "donor_or_recip") {
    if (value == "d") return ("")
    if (value == "r") return ("")
  }
  return (value)
}


#make the graph
ggplot(defol_0.6_nn, aes(x = treatment, y = C, fill = donor_or_recip)) +
  geom_boxplot() +
  facet_wrap(~ tissue + donor_or_recip, scales = "free", labeller = custom_labeller) +
  scale_fill_discrete(name = 'Seedling Type', label = c('Donors', 'Recipients')) +
  scale_x_discrete(labels = custom_labels) + ylab('13C') +
  ggtitle('60% Defoliation: Tissue 13C Content')+ theme(plot.title = element_text(hjust = 0.5))

  
#run the lm
  lm_0.6_C <- lm(C ~ donor_or_recip * treatment * tissue, subset=tissue!="needles", data=defol_0.6)
  Anova(lm_0.6_C)
      #look for interaction effect, donors respond differently to   treatment
        #rotated cores = donors hold onto C
        #unrotated cores = donors don't hold onto as much C 
        #have suggestive pattern in graphs
        #reversal between stems and roots, stems have less C in controls,but more C in controls with roots
    
    
    

########1.0 defol: control (rotated) vs. nonrotated######################### 
#Donor defoliation: donors vs recipients N
#subset data a little more
  defol_1.0 <- subset(test_foliar, treatment %in% c('defoliate_1.0', 'control_1.0_defol'))

#subset a little more to remove needles since we don't have that info for donors
defol_1.0_nn <- subset(defol_1.0, tissue %in% c('stem', 'lowroot', 'highroot'))

#make a graph: 
  #pdf('./figs/0.5_ddefol_d_vs_r.pdf') #pdf will show up in figs folder
  par(mfrow = c(1,1))
  ggplot(defol_1.0_nn, aes(x = treatment, y=N, fill = donor_or_recip)) +
    geom_boxplot() +
    ylab("15N") + xlab("Treatment") +
    scale_fill_discrete(name = 'Seedling Type', label = c('Donors', 'Recipients'))
  #dev.off()
  
#make a graph for individual tissue groups for donors 1.0 defol for N
# Custom labels for the x-axis
custom_labels <- c('rotated', 'not rotated')

# Function to customize facet labels
custom_labeller <- function(variable, value) {
  if (variable == "donor_or_recip") {
    if (value == "d") return ("")
    if (value == "r") return ("")
  }
  return (value)
}

ggplot(defol_1.0_nn, aes(x = treatment, y = N, fill = donor_or_recip)) +
  geom_boxplot() +
  facet_wrap(~ tissue + donor_or_recip, scales = "free", labeller = custom_labeller) +
  scale_fill_discrete(name = 'Seedling Type', label = c('Donors', 'Recipients')) +
  scale_x_discrete(labels = custom_labels) + ylab('15N') +
  ggtitle('100% Defoliation: Tissue 15N Content')+ theme(plot.title = element_text(hjust = 0.5))


#run the lm
  lm_1.0_N <- lm(N ~ donor_or_recip * treatment * tissue,subset=tissue!="needles", data=defol_1.0)
  Anova(lm_1.0_N)
  


#Donor defoliation: donors vs recipients C
  
#make a graph: 
  #pdf('./figs/0.5_ddefol_d_vs_r.pdf') #pdf will show up in figs folder
  par(mfrow = c(1,1))
  ggplot(defol_1.0, aes(x = treatment, y=C, fill = donor_or_recip)) +
    geom_boxplot() +
    ylab("13C") + xlab("Treatment") +
    scale_fill_discrete(name = 'Seedling Type', label = c('Donors', 'Recipients'))
  #dev.off()
  
#make a graph for individual tissue groups for donors 1.0 defol for C
# Custom labels for the x-axis
custom_labels <- c('rotated', 'not rotated')

# Function to customize facet labels
custom_labeller <- function(variable, value) {
  if (variable == "donor_or_recip") {
    if (value == "d") return ("")
    if (value == "r") return ("")
  }
  return (value)
}

ggplot(defol_1.0_nn, aes(x = treatment, y = C, fill = donor_or_recip)) +
  geom_boxplot() +
  facet_wrap(~ tissue + donor_or_recip, scales = "free", labeller = custom_labeller) +
  scale_fill_discrete(name = 'Seedling Type', label = c('Donors', 'Recipients')) +
  scale_x_discrete(labels = custom_labels) + ylab('13C') +
  ggtitle('100% Defoliation: Tissue 13C Content')+ theme(plot.title = element_text(hjust = 0.5))

  
#run the lm
  lm_1.0_C <- lm(C ~ donor_or_recip * treatment * tissue, subset=tissue!="needles", data=defol_1.0)
  Anova(lm_1.0_C)




  
  
  
---  
#' *Let's see how the label appeared in different tissues*
---
#' all donor seedlings were labeled, so I will be looking at tissue differentiation in donor seedlings regardless of defoliation treatment
#' note: since label was applied directly to needles, they will not be included as a plat tissue when evaluating donor seedlings

#filter the data a little 
  donors <- subset(test_foliar, donor_or_recip %in% c('d'))
  
#Tukey HSD test/graph for N
  #build the model and run TukeyHSD
  aov_donors <- aov(N ~ tissue, data = donors)
  summary(aov_donors)
  tukey <- TukeyHSD(aov_donors, 'tissue', ordered = TRUE, conf.level = 0.95)
  tukey
  
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
    geom_text(aes(label = cld, y = w + sd), vjust = -0.5) +
    ggtitle('Donor Seedlings: Tissue 15N Content')+ theme(plot.title = element_text(hjust = 0.5))
  #dev.off() #where to stop pdf
  
  

#Tukey HSD test/graph for C
  #build the model and run TukeyHSD
  aov_donors <- aov(C ~ tissue, data = donors)
  summary(aov_donors)
  tukey <- TukeyHSD(aov_donors, 'tissue', ordered = TRUE, conf.level = 0.95)
  tukey
  
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
    geom_text(aes(label = cld, y = w + sd), vjust = -0.5) +
    ggtitle('Donor Seedlings: Tissue 13C Content')+ theme(plot.title = element_text(hjust = 0.5))
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
  tukey
  
  #compact letter display
  cld <- multcompLetters4(aov_recipients, tukey)
  
  #table with factors and 3rd quartile 
  tissue_waje <- group_by(recipients, tissue) %>%
    summarise(w=mean(N),sd = sd(N)) %>%
    arrange(desc(w))
  
  #extracting compact letter display and adding to the Tukey table
  cld <- as.data.frame.list(cld$tissue)
  tissue_waje$cld <- cld$Letters
  print(tissue_waje) #this prints the letters that are assigned based on significance
  
  #build the graph and save as a PDF to figs folder 
  #pdf('./figs/tukey_control_0.5_recip_defol_recip_tissue.pdf') #pdf will show up in figs folder
  ggplot(tissue_waje, aes(tissue, w)) + 
    geom_bar(stat = "identity", aes(fill = tissue), show.legend = TRUE) +
    geom_errorbar(aes(ymin = w-sd, ymax=w+sd), width = 0.05) +
    labs(x = "Plant Tissue Types", y = "15N") +
    geom_text(aes(label = cld, y = w + sd), vjust = -0.5) +  
    ggtitle('Recipient Seedlings: Tissue 15N Content')+ theme(plot.title = element_text(hjust = 0.5))
  #dev.off() #where to stop pdf
  
  
#'Interesting that needles have a different 15N signal than roots or stem
#'Why?
  
  
#Tukey HSD test/graph for C
  #build the model and run TukeyHSD
  aov_recipients <- aov(C ~ tissue, data = recipients)
  summary(aov_recipients)
  tukey <- TukeyHSD(aov_recipients, 'tissue', ordered = TRUE, conf.level = 0.95)
  tukey
  
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
    geom_text(aes(label = cld, y = w + sd), vjust = -0.5) +
    ggtitle('Recipient Seedlings: Tissue 13C Content')+ theme(plot.title = element_text(hjust = 0.5))
  #dev.off() #where to stop pdf

  
  
  
  
  
    
#' 
#' 
#' 
#' 
#' 
#' 
#' *Let's look at %N and %C"*
  
  ########0.6 defol: control (rotated) vs. nonrotated######################### 
  #Donor defoliation: donors vs recipients %N
  #subset data a little more
  defol_0.6 <- subset(test_foliar, treatment %in% c('defoliate_0.6', 'control_0.6_defol'))
  
  #subset a little more to remove needles since we don't have that info for donors
  defol_0.6_nn <- subset(defol_0.6, tissue %in% c('stem', 'lowroot', 'highroot'))
  
  #make a graph: 
  #pdf('./figs/0.5_ddefol_d_vs_r.pdf') #pdf will show up in figs folder
  par(mfrow = c(1,1))
  ggplot(defol_0.6_nn, aes(x = treatment, y=X.N, fill = donor_or_recip)) +
    geom_boxplot() +
    ylab("% 15N") + xlab("Treatment") +
    scale_fill_discrete(name = 'Seedling Type', label = c('Donors', 'Recipients'))
  #dev.off()
  
  #There is a slight difference in N in donors in the control (rotated cores, control _0.6_defol) vs. the non-rotated (defoliate_0.6), investigate by looking at individual tissue groups
  
  #make a graph for individual tissue groups for donors 0.6 defol for N
  # Custom labels for the x-axis
  custom_labels <- c('rotated', 'not rotated')
  
  # Function to customize facet labels
  custom_labeller <- function(variable, value) {
    if (variable == "donor_or_recip") {
      if (value == "d") return ("")
      if (value == "r") return ("")
    }
    return (value)
  }
  
  #make the plot
  ggplot(defol_0.6_nn, aes(x = treatment, y = X.N, fill = donor_or_recip)) +
    geom_boxplot() +
    facet_wrap(~ tissue + donor_or_recip, scales = "free", labeller = custom_labeller) +
    scale_fill_discrete(name = 'Seedling Type', label = c('Donors', 'Recipients')) +
    scale_x_discrete(labels = custom_labels) + ylab('% 15N') +
    ggtitle('60% Defoliation: Tissue % 15N Content')+ theme(plot.title = element_text(hjust = 0.5))
  
  #run the lm
  lm_0.6_X.N <- lm(X.N ~ donor_or_recip * treatment * tissue,subset=tissue!="needles", data=defol_0.6)
  Anova(lm_0.6_X.N)
  
  
  #Donor defoliation: donors vs recipients %C
  
  #make a graph: 
  #pdf('./figs/0.5_ddefol_d_vs_r.pdf') #pdf will show up in figs folder
  par(mfrow = c(1,1))
  ggplot(defol_0.6, aes(x = treatment, y=X.C, fill = donor_or_recip)) +
    geom_boxplot() +
    ylab("% 13C") + xlab("Treatment") +
    scale_fill_discrete(name = 'Seedling Type', label = c('Donors', 'Recipients'))
  #dev.off()
  
  #make a graph for individual tissue groups for donors 0.6 defol for C
  # Custom labels for the x-axis
  custom_labels <- c('rotated', 'not rotated')
  
  # Function to customize facet labels
  custom_labeller <- function(variable, value) {
    if (variable == "donor_or_recip") {
      if (value == "d") return ("")
      if (value == "r") return ("")
    }
    return (value)
  }
  
  
  #make the graph
  ggplot(defol_0.6_nn, aes(x = treatment, y = X.C, fill = donor_or_recip)) +
    geom_boxplot() +
    facet_wrap(~ tissue + donor_or_recip, scales = "free", labeller = custom_labeller) +
    scale_fill_discrete(name = 'Seedling Type', label = c('Donors', 'Recipients')) +
    scale_x_discrete(labels = custom_labels) + ylab('% 13C') +
    ggtitle('60% Defoliation: Tissue % 13C Content')+ theme(plot.title = element_text(hjust = 0.5))
  
  
  #run the lm
  lm_0.6_X.C <- lm(X.C ~ donor_or_recip * treatment * tissue, subset=tissue!="needles", data=defol_0.6)
  Anova(lm_0.6_X.C)

  
  
  
  
  ########1.0 defol: control (rotated) vs. nonrotated######################### 
  #Donor defoliation: donors vs recipients % N
  #subset data a little more
  defol_1.0 <- subset(test_foliar, treatment %in% c('defoliate_1.0', 'control_1.0_defol'))
  
  #subset a little more to remove needles since we don't have that info for donors
  defol_1.0_nn <- subset(defol_1.0, tissue %in% c('stem', 'lowroot', 'highroot'))
  
  #make a graph: 
  #pdf('./figs/0.5_ddefol_d_vs_r.pdf') #pdf will show up in figs folder
  par(mfrow = c(1,1))
  ggplot(defol_1.0_nn, aes(x = treatment, y=X.N, fill = donor_or_recip)) +
    geom_boxplot() +
    ylab("% 15N") + xlab("Treatment") +
    scale_fill_discrete(name = 'Seedling Type', label = c('Donors', 'Recipients'))
  #dev.off()
  
  #make a graph for individual tissue groups for donors 1.0 defol for N
  # Custom labels for the x-axis
  custom_labels <- c('rotated', 'not rotated')
  
  # Function to customize facet labels
  custom_labeller <- function(variable, value) {
    if (variable == "donor_or_recip") {
      if (value == "d") return ("")
      if (value == "r") return ("")
    }
    return (value)
  }
  
  ggplot(defol_1.0_nn, aes(x = treatment, y = X.N, fill = donor_or_recip)) +
    geom_boxplot() +
    facet_wrap(~ tissue + donor_or_recip, scales = "free", labeller = custom_labeller) +
    scale_fill_discrete(name = 'Seedling Type', label = c('Donors', 'Recipients')) +
    scale_x_discrete(labels = custom_labels) + ylab('% 15N') +
    ggtitle('100% Defoliation: Tissue % 15N Content')+ theme(plot.title = element_text(hjust = 0.5))
  
  
  #run the lm
  lm_1.0_X.N <- lm(X.N ~ donor_or_recip * treatment * tissue,subset=tissue!="needles", data=defol_1.0)
  Anova(lm_1.0_X.N)
  
  
  
  #Donor defoliation: donors vs recipients % C
  
  #make a graph: 
  #pdf('./figs/0.5_ddefol_d_vs_r.pdf') #pdf will show up in figs folder
  par(mfrow = c(1,1))
  ggplot(defol_1.0, aes(x = treatment, y=X.C, fill = donor_or_recip)) +
    geom_boxplot() +
    ylab("% 13C") + xlab("Treatment") +
    scale_fill_discrete(name = 'Seedling Type', label = c('Donors', 'Recipients'))
  #dev.off()
  
  #make a graph for individual tissue groups for donors 1.0 defol for C
  # Custom labels for the x-axis
  custom_labels <- c('rotated', 'not rotated')
  
  # Function to customize facet labels
  custom_labeller <- function(variable, value) {
    if (variable == "donor_or_recip") {
      if (value == "d") return ("")
      if (value == "r") return ("")
    }
    return (value)
  }
  
  ggplot(defol_1.0_nn, aes(x = treatment, y = X.C, fill = donor_or_recip)) +
    geom_boxplot() +
    facet_wrap(~ tissue + donor_or_recip, scales = "free", labeller = custom_labeller) +
    scale_fill_discrete(name = 'Seedling Type', label = c('Donors', 'Recipients')) +
    scale_x_discrete(labels = custom_labels) + ylab('% 13C') +
    ggtitle('100% Defoliation: Tissue % 13C Content')+ theme(plot.title = element_text(hjust = 0.5))
  
  
  #run the lm
  lm_1.0_X.C <- lm(X.C ~ donor_or_recip * treatment * tissue, subset=tissue!="needles", data=defol_1.0)
  Anova(lm_1.0_X.C)
  
  
  
  
  
  
  
  ---  
    #' *Let's see how the label appeared in different tissues: % C and N*
    ---
    #' all donor seedlings were labeled, so I will be looking at tissue differentiation in donor seedlings regardless of defoliation treatment
    #' note: since label was applied directly to needles, they will not be included as a plat tissue when evaluating donor seedlings
    
    #filter the data a little 
    donors <- subset(test_foliar, donor_or_recip %in% c('d'))
  
  #Tukey HSD test/graph for N
  #build the model and run TukeyHSD
  aov_donors <- aov(X.N ~ tissue, data = donors)
  summary(aov_donors)
  tukey <- TukeyHSD(aov_donors, 'tissue', ordered = TRUE, conf.level = 0.95)
  tukey
  
  #compact letter display
  cld <- multcompLetters4(aov_donors, tukey)
  
  #table with factors and 3rd quartile 
  tissue_waje <- group_by(donors, tissue) %>%
    summarise(w=mean(X.N),sd = sd(X.N)) %>%
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
    labs(x = "Plant Tissue Types", y = "% 15N") +
    geom_text(aes(label = cld, y = w + sd), vjust = -0.5) +
    ggtitle('Donor Seedlings: Tissue % 15N Content')+ theme(plot.title = element_text(hjust = 0.5))
  #dev.off() #where to stop pdf
  
  
  
  #Tukey HSD test/graph for % C
  #build the model and run TukeyHSD
  aov_donors <- aov(X.C ~ tissue, data = donors)
  summary(aov_donors)
  tukey <- TukeyHSD(aov_donors, 'tissue', ordered = TRUE, conf.level = 0.95)
  tukey
  
  #compact letter display
  cld <- multcompLetters4(aov_donors, tukey)
  
  #table with factors and 3rd quartile 
  tissue_waje <- group_by(donors, tissue) %>%
    summarise(w=mean(X.C),sd = sd(X.C)) %>%
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
    labs(x = "Plant Tissue Types", y = "% 13C") +
    geom_text(aes(label = cld, y = w + sd), vjust = -0.5) +
    ggtitle('Donor Seedlings: Tissue % 13C Content')+ theme(plot.title = element_text(hjust = 0.5))
  #dev.off() #where to stop pdf
  

  #'
  #'
  #'Let's take a look at 15N and 13C in recipient (unlabeled) seedlings just for fun 
  #'
  #filter the data a little 
  recipients <- subset(test_foliar, donor_or_recip %in% c('r'))
  #need to remove one lowroot line b/c NA
  recipients <- na.omit(recipients)
  
  #Tukey HSD test/graph for % N
  #build the model and run TukeyHSD
  aov_recipients <- aov(X.N ~ tissue, data = recipients)
  summary(aov_recipients)
  tukey <- TukeyHSD(aov_recipients, 'tissue', ordered = TRUE, conf.level = 0.95)
  tukey
  
  #compact letter display
  cld <- multcompLetters4(aov_recipients, tukey)
  
  #table with factors and 3rd quartile 
  tissue_waje <- group_by(recipients, tissue) %>%
    summarise(w=mean(X.N),sd = sd(X.N)) %>%
    arrange(desc(w))
  
  #extracting compact letter display and adding to the Tukey table
  cld <- as.data.frame.list(cld$tissue)
  tissue_waje$cld <- cld$Letters
  print(tissue_waje) #this prints the letters that are assigned based on significance
  
  #build the graph and save as a PDF to figs folder 
  #pdf('./figs/tukey_control_0.5_recip_defol_recip_tissue.pdf') #pdf will show up in figs folder
  ggplot(tissue_waje, aes(tissue, w)) + 
    geom_bar(stat = "identity", aes(fill = tissue), show.legend = TRUE) +
    geom_errorbar(aes(ymin = w-sd, ymax=w+sd), width = 0.05) +
    labs(x = "Plant Tissue Types", y = "% 15N") +
    geom_text(aes(label = cld, y = w + sd), vjust = -0.5) +  
    ggtitle('Recipient Seedlings: Tissue % 15N Content')+ theme(plot.title = element_text(hjust = 0.5))
  #dev.off() #where to stop pdf
  
  
  
  
  #Tukey HSD test/graph for % C
  #build the model and run TukeyHSD
  aov_recipients <- aov(X.C ~ tissue, data = recipients)
  summary(aov_recipients)
  tukey <- TukeyHSD(aov_recipients, 'tissue', ordered = TRUE, conf.level = 0.95)
  tukey
  
  #compact letter display
  cld <- multcompLetters4(aov_recipients, tukey)
  
  #table with factors and 3rd quartile 
  tissue_waje <- group_by(recipients, tissue) %>%
    summarise(w=mean(X.C),sd = sd(X.C)) %>%
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
    labs(x = "Plant Tissue Types", y = "% 13C") +
    geom_text(aes(label = cld, y = w + sd), vjust = -0.5) +
    ggtitle('Recipient Seedlings: Tissue % 13C Content')+ theme(plot.title = element_text(hjust = 0.5))
  #dev.off() #where to stop pdf

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
#' 1) Load packages and download data 
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

#make a graph looking at all treatment types and export as graph to figs folder
#pdf('./figs/test_foliar_all_treatments.pdf') #pdf will show up in figs folder
par(mfrow = c(1,1))
ggplot(test_foliar, aes(x=treatment,y=N,fill=defoliation))+
  geom_boxplot()+ 
  ylab("15N2")+xlab("Percent Defoliated")
#dev.off() #where to stop pdf

2)

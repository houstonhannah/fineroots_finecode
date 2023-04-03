# fineroots_finecode

Welcome to the project repository for my master's thesis: Fine-rooted problems: the effect of fine root senescence on mycorrhizal nutrient transfer in loblolly pine (Pinus taeda).

The objective of the project is to explore nutrient transfer between loblolly pine seedlings through mycorrhizal networks in the event of experimentally induced fine root senescence. This involves isotopically labeling "donor" seedlings through foliar application or stem infiltration. "Donor" and "recipient" seedlings, those not isotopically labeled, were defoliated at varying levels (0, 50%, 75% and 100%) to induce fine root death. Following defoliation, seedlings were harvested in four separate tissue groups (needles, stem, lowroots and highroots) and sent off for isotopic analysis.

The structure of the code-base for this project is R. My code can be found in the scripts folder, relevant figures can be found in the figs folder, and the data can be found in the data folder in the fineroots_finecode repositroy. The data are in a .csv format and should be read in as such. The data contain plant numbers, the isotope labeling treatment, if the seedlings was designated as a donor or recipient, tissue type, and APE (atomic percent enrichment) values. 

My results can be relpicated by using ggplot to illustrate the relationship between different treatment variables. My analysis consisted of Anova, linear models, and TukeyHSD tests.  

I would like to thank Dr. Seth Pritchard for being an amazing thesis advisor and a fearless leader into the wild world of fine root and mycorrhizal biology. I appreciate the many hours Hastings Marek and Brasil Cumberbatch dedicated to helping me in the greenhouse and am thankful for the statistical mentorship of Dr. Dan McGlinn.
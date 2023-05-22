
#' **metadata for mock_foliar, mock_free_planting, and mock_stem**
#' mock data was used to write r code before having my actual data back from an external lab.

#plant_num: the individual number assigned to each seedling
  
#isotope_label: the way in which isotopic labels were applied to the seedlings; foliar or stem.
  #foliar application: an aqueous solution of 13C and 15N2 was painted onto the needles of seedlings
  #stem infiltration labeling: a hole was drilled into the stems of seedlings and a cotton wick was run through the hole. The wick was placed in a reservoir containing a 13C and 15N2 solution 
  
#treatment: refers to the level of defoliation applied to the seedling to trigger a root death event
  #no_defol: seedlings were not defoliated 
  #con_no_defol: seedlings were not defoliated, cores were rotated to sever mycorrhizal connections
  #0.5_donor_defol: 50% of the needles were removed from donor seedlings
  #0.5_recip_defol: 50% of the needles were removed from recipient seedlings
  #con_0.5_donor_defol: 50% of the needles were removed from donor seedlings, cores were rotated to sever mycorrhizal connections
  #con_0.5_recip_defol: 50% of the needles were removed from recipient seedlings, cores were rotated to sever mycorrhizal connections
  #0.75_donor_defol: 75% of the needles were removed from donor seedlings
  #0.75_recip_defol: 75% of the needles were removed from recipient seedlings
  #con_0.75_donor_defol:75% of the needles were removed from donor seedlings, cores were rotated to sever mycorrhizal connections
  #con_0.75_recip_defol: 75% of the needles were removed from recipient seedlings, cores were rotated to sever mycorrhizal connections
  #1.0_donor_defol: 100% of the needles were removed from donor seedlings
  #1.0_recip_defol: 100% of the needles were removed from recipient seedlings
  #con_1.0_donor_defol: 100% of the needles were removed from donor seedlings, cores were rotated to sever mycorrhizal connections
  #con_1.0_recip_defol: 100% of the needles were removed from recipient seedlings, cores were rotated to sever mycorrhizal connections

#donor_or_recipient: donor seedlings were isotopically labeled, recipient seedlings were not isotopically labeled
  #d: donor seedlings
  #r: recipient seedings
  
#tissue: when harvested, seedlings tissues were separated into four different tissue groups for analysis
  #needles: the needles from the seedling
  #stem: the stem of the seedling
  #lowroot: 1st - 3rd order roots from the seedlings
  #highroot: 4th - highest order roots from the seedlings
  
#APE: Atomic Percent Enrichment of samples (15N2 and 13C) from IRMS analysis of plant tissues

#' *Metadata for test_foliar*
#platenum_wellnum: organizational tool used to track locations of plant tissue samples in 96 well plate

#plant_id: unique identification assigned to each seedling

#treatment: refers to the level of defoliation applied to the seedling to trigger a root death event
      #no_defol: seedlings were not defoliated 
      #con_no_defol: seedlings were not defoliated, cores were rotated to sever mycorrhizal connections
      #0.5_donor_defol: 50% of the needles were removed from donor seedlings
      #0.5_recip_defol: 50% of the needles were removed from recipient seedlings
      #con_0.5_donor_defol: 50% of the needles were removed from donor seedlings, cores were rotated to sever mycorrhizal connections
      #con_0.5_recip_defol: 50% of the needles were removed from recipient seedlings, cores were rotated to sever mycorrhizal connections
      #0.75_donor_defol: 75% of the needles were removed from donor seedlings
      #0.75_recip_defol: 75% of the needles were removed from recipient seedlings
      #con_0.75_donor_defol:75% of the needles were removed from donor seedlings, cores were rotated to sever mycorrhizal connections
      #con_0.75_recip_defol: 75% of the needles were removed from recipient seedlings, cores were rotated to sever mycorrhizal connections
      #1.0_donor_defol: 100% of the needles were removed from donor seedlings
      #1.0_recip_defol: 100% of the needles were removed from recipient seedlings
      #con_1.0_donor_defol: 100% of the needles were removed from donor seedlings, cores were rotated to sever mycorrhizal connections
      #con_1.0_recip_defol: 100% of the needles were removed from recipient seedlings, cores were rotated to sever mycorrhizal connections

#defoliation: indicates level at which seedlings were defoliated (0%, 40%, 60%. 80% 100%). The number of needle bunches were counted for each seedling, then multiplied by defoliation level to determine how many bunches need to be picked off.

#rotated: indicates if donor and recipient cores were rotated to sever mycorrhizal connections (yes/no)

#donor_or_recipient: donor seedlings were isotopically labeled, recipient seedlings were not isotopically labeled
#d: donor seedlings
#r: recipient seedings

#tissue: when harvested, seedlings tissues were separated into four different tissue groups for analysis
    #needles: the needles from the seedling
    #stem: the stem of the seedling
    #lowroot: 1st - 3rd order roots from the seedlings
    #highroot: 4th - highest order roots from the seedlings

#Weight (mg) – Weight of sample/standard in mg.

#N2 Amp – The amplitude of the sample peak in mV of the respective gas.

#%N and %C – the recalculated percent element results.

#AT% 15N/14N and AT% 13C/12C– The atom percent.

#15N vs. At Air – This is the corrected isotope delta value* for 15N measured against a primary reference scale. The primary reference scale for 15N is Atmospheric Air.

#13C vs. VPDB – This is the corrected isotope delta value* for 13C measured against a primary reference scale. The primary reference scale for 13C is Vienna Pee Dee Belemnite.

#Note: Delta values are measured in units of per mil (‰)



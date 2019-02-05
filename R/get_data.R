###############################################################################
#' @title Pull data for Shiny web viz
#' 
#' @author 
#' Katherine LeVan \email{katherine.levan@gmail.com} \cr
#' 
#' @description This function reads in NEON data 
#' 
#' @import neonUtilities
#' 
#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007
#' @export
#' 
#' changelog and author contributions / copyrights
#'    Katherine LeVan (2019-02-01)
#'    original creation
###############################################################################

## STEP 1. Download Relevant NEON data
## Set up environment
rm(list=ls())
options(stringsAsFactors = F)

## Load Libraries
library(neonUtilities)
library(dplyr)

# Set working directory
setwd('~/GitHub/neon-data-vignettes/data') # Path to repo

# At HARV (https://www.neonscience.org/field-sites/field-sites-map/HARV)
# Download the basic package of the
# Mosquito CO2 trapping data product "DP1.10043.001"
# Weather summary data product "DP4.00001.001"
# additional data products
dataproducts = c("DP1.10043.001", "DP4.00001.001", 
                 "DP1.00006.001", "DP1.00098.001", 
                 "DP1.00003.001") # "DP1.00001.001"
sapply(X = dataproducts,
       FUN = neonUtilities::zipsByProduct, site = 'HARV', package = 'basic', 
       check.size = F)

# Squash all the files together so that there is one file per table
dataproducts = gsub('\\.001','', dataproducts)
dataproducts = gsub('DP[1-4]{1}\\.','', dataproducts)
for(n in dataproducts){
  neonUtilities::stackByTable(filepath = list.files(full.names = T, pattern = n), 
                              folder = T)
  
}
rm(n, dataproducts)


# Read in each stacked file into the R session
# and name it according to the table name
files = list.files(getwd(), pattern = '.csv', recursive = T) 
for (x in files){
  # makes the name of each file
  nm = gsub('filesToStack[0-9]{5}/stackedFiles/', '', x) # remove file path
  nm = gsub('\\.csv', '', nm) # remove the '.csv' file ending 
  # read in the csv file and assign the name accordingly
  assign(x = nm, 
         value = read.csv(file = x))
}
rm(nm, files)

###### STEP 2. Process the data
# Mosquitoes - create a high-level summary table
# Not all mosquitoes that are collected are identified -
# The fraction which are identified is described in the sorting table
mos_sorting$scalar = mos_sorting$totalWeight/mos_sorting$subsampleWeight
# Left Join with the trapping information from 2017 
# (the year when paired sensor data are available)
mos = dplyr::left_join(x = mos_trapping[grepl('2017', mos_trapping$collectDate), ], 
                       y = mos_sorting[,c("sampleID", "subsampleID", 
                                          "scalar")], 
                       by = "sampleID")
# # What species were collected?
# spp = unique(mos_expertTaxonomistIDProcessed$scientificName
#              [grepl('species', mos_expertTaxonomistIDProcessed$taxonRank)])
# For each identified sample, describe:
# 1. the total number of mosquitoes collected
# 2. the richness of the mosquito community
# 3. per each individual counts
for(id in mos$subsampleID[mos$subsampleID%in%
                          mos_expertTaxonomistIDProcessed$subsampleID]){
  scalar = mos$scalar[mos$subsampleID%in%id]
  # 1. Find totalAbundance
  if(!is.na(scalar)){
    mos$totalAbundance[mos$subsampleID%in%id] = sum(mos_expertTaxonomistIDProcessed$individualCount
                                                    [mos_expertTaxonomistIDProcessed$subsampleID%in%id], 
                                                    na.rm = T) * scalar
    
  } else {
    mos$totalAbundance[mos$subsampleID%in%id] = sum(mos_expertTaxonomistIDProcessed$individualCount
                                                    [mos_expertTaxonomistIDProcessed$subsampleID%in%id], 
                                                    na.rm = T)
  }
  # 2. Find the number of unique species present
  mos$speciesRichness[mos$subsampleID%in%id] = length(unique(mos_expertTaxonomistIDProcessed$scientificName
                                                             [mos_expertTaxonomistIDProcessed$subsampleID%in%id&
                                                                 !mos_expertTaxonomistIDProcessed$scientificName%in%'']))
  
}
rm(id, scalar)
# Traps with no mosquitoes have zero abundance and zero richness
mos$totalAbundance[mos$targetTaxaPresent%in%'N'] = 0
mos$speciesRichness[mos$targetTaxaPresent%in%'N'] = 0

# Make a species matrix
# TO DO

# Specify date without time
mos$date = as.Date(mos$collectDate)
for(tab in ls(pattern = 'wss')){
  x = get(tab)
  x$date = as.Date(x$date)
  assign(x = tab, value = x)
  rm(x)
}
rm(tab)

# Left JOIN observation data with abiotic data
mos = dplyr::left_join(mos, 
                       wss_daily_temp[,c("date","wssTempTripleMean",
                                         "wssTempTripleMinimum",
                                         "wssTempTripleMaximum")],
                       by = 'date')
mos = dplyr::left_join(mos, 
                       wss_daily_precip[,c("date","wssPrecipTotal" )],
                       by = 'date')
mos = dplyr::left_join(mos, 
                       wss_daily_wind[,c("date","wssWindSpeedMean",
                                         "wssWindSpeedMinimum",
                                         "wssWindSpeedMaximum")],
                       by = 'date')
mos = dplyr::left_join(mos, 
                       wss_daily_humid[,c("date","wssRHMean","wssRHMinimum",
                                          "wssRHMaximum","wssRHVariance",
                                          "wssRHStdErMean","wssDewTempMean",
                                          "wssDewTempMinimum",
                                          "wssDewTempMaximum" )],
                       by = 'date')

# Write files for pulling
files = ls()[!grepl('mos_',ls())&!grepl('va',ls())] 
for (x in files){
  write.csv(get(x), paste0(x,'.csv'), row.names = F, 
            na = '', fileEncoding = 'UTF-8')
}

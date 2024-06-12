#### Setup ####
list.of.packages <- c("rstudioapi", "dplyr", "data.table", "countrycode", "stringr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

wd <- dirname(getActiveDocumentContext()$path) 
setwd(wd)
setwd("../")
#### End setup ####

crs = fread("large_data/crs.csv")
iati = fread("large_data/iati_24052024.csv")

# Correction for missing reporting_org_ref, and nonbreaking space
iati$reporting_org_ref[which(startsWith(iati$iati_identifier,"GB-CHC-294329"))] = "GB-CHC-294329"
iati$reporting_org_ref[which(startsWith(iati$iati_identifier,"FR-6"))] = "FR-6"
iati$reporting_org_ref = str_trim(iati$reporting_org_ref)

oecd_donor_mapping = fread('data/iati_donor_mapping.csv')
oecd_donor_mapping = subset(oecd_donor_mapping, !is.na(`Donor code`))
# Account for splitting
iati$index = c(1:nrow(iati))
pre_sum = sum(iati$x_transaction_value_usd, na.rm=T)
iati = merge(iati, oecd_donor_mapping, by="reporting_org_ref", all.x = T, allow.cartesian=T)
duplicated_indicies = iati$index[which(duplicated(iati$index))]
duplicated_counts = data.table(table(duplicated_indicies))
names(duplicated_counts) = c("index", "count")
duplicated_counts$index = as.numeric(duplicated_counts$index)
duplicated_counts$count = duplicated_counts$count + 1
duplicated_counts$value_multiplier = 1 / duplicated_counts$count 
duplicated_counts$count = NULL
iati = merge(iati, duplicated_counts, by="index", all.x=T)
iati$value_multiplier[which(is.na(iati$value_multiplier))] = 1
iati$x_transaction_value_usd = iati$x_transaction_value_usd * iati$value_multiplier
iati$index = NULL
iati$value_multiplier = NULL
post_sum = sum(iati$x_transaction_value_usd, na.rm=T)
stopifnot(format(pre_sum, scientific=F) == format(post_sum, scientific=F))

# Correction for World Bank publisher mapping to IDA Donor Code
wb = subset(iati, reporting_org_ref=="44000")
wb = subset(wb, grepl("International Development Association", funding_orgs))
nonwb = subset(iati, reporting_org_ref!="44000")
iati = rbind(wb, nonwb)

# Remove non-core funding for FAO
fao = subset(iati, reporting_org_ref=="XM-DAC-41301")
fao = subset(fao, grepl("Food and Agriculture Organization (FAO)", funding_orgs, fixed=T))
nonfao = subset(iati, reporting_org_ref!="XM-DAC-41301")
iati = rbind(fao, nonfao)

# Fix scale of IADB
iadb = subset(iati, reporting_org_ref=="XI-IATI-IADB")
iadb$x_transaction_value_usd = iadb$x_transaction_value_usd / 1000
noniadb = subset(iati, reporting_org_ref!="XI-IATI-IADB")
iati = rbind(iadb, noniadb)

iati = subset(iati, !is.na(`Donor code`))
iati$sector_code = substr(iati$x_sector_code, 1, 3)
iati$x_transaction_value_usd = iati$x_transaction_value_usd / 1000000
iati_agg = data.table(iati)[,.(usd_disbursement_iati=sum(x_transaction_value_usd, na.rm=T)),by=.(
  `Donor code`, `Donor name`, x_transaction_year, x_recipient_code, sector_code
)]
names(iati_agg) = c("donor_code", "donor_name" ,"year", "recipient_iso2_code", "sector_code", "usd_disbursement_iati")
iati_agg$recipient_iso3_code = countrycode(
  iati_agg$recipient_iso2_code, 
  origin="iso2c", 
  destination="iso3c"
)
iati_agg = subset(iati_agg, !is.na(recipient_iso3_code))
iati_agg$recipient_iso2_code = NULL
iati_agg$sector_code = as.character(iati_agg$sector_code)
iati_agg$donor_name = stringr::str_replace_all(iati_agg$donor_name, "\\h", " ") # Replace horizontal space

crs_agg = data.table(crs)[,.(usd_disbursement_crs=sum(usd_disbursement, na.rm=T)),by=.(
  donor_code, donor_name, year, recipient_iso3_code, sector_code
)]
crs_agg$sector_code = as.character(crs_agg$sector_code)

dat = merge(crs_agg, iati_agg, all=T)
dat$usd_disbursement_crs[which(is.na(dat$usd_disbursement_crs))] = 0
dat$usd_disbursement_iati[which(is.na(dat$usd_disbursement_iati))] = 0

dat$recipient_name = countrycode(dat$recipient_iso3_code, origin="iso3c", destination="country.name")
dat = subset(dat, !is.na(recipient_name))

fwrite(dat, "data/merged_crs_iati.csv")

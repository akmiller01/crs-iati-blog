#### Setup ####
list.of.packages <- c("rstudioapi", "data.table")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

wd <- dirname(getActiveDocumentContext()$path) 
setwd(wd)
setwd("../")

rmse = function(vec1, vec2){
  sqrt(mean((vec1 - vec2)^2, na.rm=T))
}
#### End setup ####

dat = fread("data/merged_crs_iati.csv")
dat = dat[,.(
  usd_disbursement_crs=sum(usd_disbursement_crs),
  usd_disbursement_iati=sum(usd_disbursement_iati)
),
by=.(year, sector_code, recipient_name, recipient_iso3_code)]
dat = dat[order(dat$recipient_name, dat$sector_code, dat$year)]
dat[,"usd_disbursement_crs_t1":=shift(usd_disbursement_crs),by=.(recipient_name, sector_code)]
dat[,"usd_disbursement_iati_t1":=shift(usd_disbursement_iati),by=.(recipient_name, sector_code)]

dat$delta_iati = (dat$usd_disbursement_iati - dat$usd_disbursement_iati_t1)

dat$sector_code = factor(dat$sector_code)

dat_train = subset(dat, year < 2022)
dat_test = subset(dat, year == 2022)

fit = lm(
  usd_disbursement_crs~ # CRS this year is a function of
    # Constant alpha
    usd_disbursement_iati+ # plus beta0 * IATI this year
    usd_disbursement_crs_t1+ # plus beta1 * CRS last year
    delta_iati+ # plus beta2 * the absolute change in IATI from last year
    sector_code # plus sector fixed effects
    , data=dat_train)
summary(fit)
confidence = predict.lm(fit, newdata = dat_test, interval = "confidence")
dat_test$usd_disbursement_iati_fit = confidence[,1]
dat_test$usd_disbursement_iati_lwr = confidence[,2]
dat_test$usd_disbursement_iati_upr = confidence[,3]
plot(usd_disbursement_crs~usd_disbursement_iati, data=dat_test)
abline(0,1)
original_rmse = rmse(dat_test$usd_disbursement_crs, dat_test$usd_disbursement_iati)
original_rmse
plot(usd_disbursement_crs~usd_disbursement_iati_fit, data=dat_test)
abline(0,1)
fit_rmse = rmse(dat_test$usd_disbursement_crs, dat_test$usd_disbursement_iati_fit)
fit_rmse

dat_agg = dat_test[,.(
  usd_disbursement_crs=sum(usd_disbursement_crs),
  usd_disbursement_iati_fit=sum(usd_disbursement_iati_fit),
  usd_disbursement_iati=sum(usd_disbursement_iati)
),
by=.(year, recipient_name, recipient_iso3_code)]
rmse(dat_agg$usd_disbursement_crs, dat_agg$usd_disbursement_iati)
rmse(dat_agg$usd_disbursement_crs, dat_agg$usd_disbursement_iati_fit)

dat_train = subset(dat, year < 2023)
dat_predict = subset(dat, year < 2024)
fit = lm(
  usd_disbursement_crs~ # CRS this year is a function of
    # Constant alpha
    usd_disbursement_iati+ # plus beta0 * IATI this year
    usd_disbursement_crs_t1+ # plus beta1 * CRS last year
    delta_iati+ # plus beta2 * the absolute change in IATI from last year
    sector_code # plus sector fixed effects
  , data=dat_train)
summary(fit)
confidence = predict.lm(fit, newdata = dat_predict, interval = "confidence")
dat_predict$usd_disbursement_crs_fit = confidence[,1]
dat_predict$usd_disbursement_crs_lwr = confidence[,2]
dat_predict$usd_disbursement_crs_upr = confidence[,3]
dat_predict$usd_disbursement_crs[which(dat_predict$year==2023)] = dat_predict$usd_disbursement_crs_fit[which(dat_predict$year==2023)]
fwrite(dat_predict, "data/modeled_crs_iati.csv")

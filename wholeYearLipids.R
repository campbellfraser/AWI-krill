# Lipid variation over the whole year
source("prerequisites.R")
lipidsOverYear = tibble(
  yr=as.character(),
  mo=factor(levels=(allMonthsLong)),
  meanLipids=as.numeric()
)
for (i in years){
  krillYEAR = krill %>%
    select(Lipids,date.YMD) %>%
    dplyr::filter(grepl(i,date.YMD))
    krillYEAR$specMonth=(stri_sub(krillYEAR$date.YMD,6,7))
  for (u in monthNums){
    tempMonth = dplyr::filter(krillYEAR,grepl(u,specMonth))
    monthMean = sum(tempMonth$Lipids,na.rm=TRUE)/length(tempMonth$specMonth[!is.na(tempMonth$specMonth)])
    if (monthMean==0|is.nan(monthMean)==TRUE){
      monthMean=NA
    }
    lipidsOverYear = add_row(lipidsOverYear,yr=i,mo=as.factor(allMonthsLong[as.numeric(u)]),meanLipids=monthMean)
  }
}

lipidsOverYear$yr = as.factor(lipidsOverYear$yr)
lipidsOverYear$mo = as.factor(lipidsOverYear$mo)

ggAnnualLipids = ggplot(lipidsOverYear,aes(x=mo,y=meanLipids,group=yr)) +
  geom_line(size=1,aes(color=yr)) +
  geom_point(aes(color=yr))
ggAnnualLipids

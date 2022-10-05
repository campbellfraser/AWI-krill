# Yearly lipids and recruitment
source("prerequisites.R")
peng30mm = read_csv("pengDiets_30mm_agg.csv",col_select=c(pMean,pSD,yr,mo,spp))
peng44mm = read_csv("pengDiets_44mm_agg.csv",col_select=c(pMean,pSD,yr,mo,spp))

recruitmentINFO.years = tibble(
  yr = factor(),
  recruitment.30mm = numeric(),
  recruitment.44mm = numeric()
)

krillINFO.months = tibble(
  yr = factor(),
  mo = factor(),
  meanLipids = numeric(),
  sdLipids = numeric()
)

for (i in years){
  lipidYear = dplyr::filter(krill,grepl(i,date.YMD))
  for (j in monthNums){
    MONTHkrill = dplyr::filter(lipidYear,grepl(paste0("-",j,"-"),date.YMD))
    MONTHmean = sum(MONTHkrill$Lipids,na.rm=TRUE)/length(MONTHkrill$date.YMD[!is.na(MONTHkrill$date.YMD)])
    MONTHsd = sd(MONTHkrill$Lipids,na.rm=TRUE)
    krillINFO.months = add_row(krillINFO.months,
                               yr=i, mo=allMonthsLong[as.numeric(j)], meanLipids=MONTHmean, sdLipids=MONTHsd)
  }
  recruitmentYEAR.30mm = dplyr::filter(peng30mm,yr==i)
  recruitmentMEAN.30mm = sum(recruitmentYEAR.30mm$pMean, na.rm=TRUE)/length(recruitmentYEAR.30mm$pMean[!is.na(recruitmentYEAR.30mm$pMean)])
  recruitmentYEAR.44mm = dplyr::filter(peng44mm,yr==i)
  recruitmentMEAN.44mm = sum(recruitmentYEAR.44mm$pMean, na.rm=TRUE)/length(recruitmentYEAR.44mm$pMean[!is.na(recruitmentYEAR.44mm$pMean)])
  recruitmentINFO.years = add_row(recruitmentINFO.years, yr=i, recruitment.30mm=recruitmentMEAN.30mm, recruitment.44mm=recruitmentMEAN.44mm)
  if (i=="2021"){
    rm(lipidYear,recruitmentYEAR.30mm,recruitmentYEAR.44mm,i,j,MONTHmean,MONTHsd,recruitmentMEAN.30mm,recruitmentMEAN.44mm,MONTHkrill)
  }
}

inspectaMonth = readline(prompt="Months: ") %>%
  strsplit("[[:space:]]") %>%
  unlist()

krillINFO.reduced = dplyr::filter(krillINFO.months,grepl())

ggRecruitment = ggplot(krillINFO, aes(x=meanLipids,y=recruitment)) +
  geom_point() +
  geom_smooth(method="lm",color="#00BA38") +
#  stat_regline_equation(label.x = 31, label.y = 1) +
  geom_text(label=paste("P-val: ",round(lmp(recruitmentMod),digits=4)),
            x=20,y=0.95) +
  ggtitle("Recruitment predicted by mean omega-3 content of previous year") +
  labs(x="Mean omega-3 content (g)",y="Recruitment")
ggRecruitment + theme_bw()

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
  lipidYear = krill %>%
    select(Lipids,date.YMD) %>%
    dplyr::filter(grepl(i,date.YMD))
  for (j in monthNums){
    MONTHkrill = dplyr::filter(lipidYear,grepl(paste0("-",j,"-"),date.YMD))
    MONTHmean = sum(MONTHkrill$Lipids,na.rm=TRUE)/length(MONTHkrill$date.YMD[!is.na(MONTHkrill$date.YMD)])
    MONTHsd = sd(MONTHkrill$Lipids,na.rm=TRUE)
    if (is.nan(MONTHmean)==TRUE|MONTHmean==0){
      MONTHmean=NA
    }
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

# Graphs plotting lipid content of Jul/Aug/Sep against recruitment of following year ####
# I HATE how I did all these.... I promise it's just because I didn't have the time to make a lovely loop.....

#30mm
recruitment30m.staggered = recruitmentINFO.years[-c(1),] %>%
  select(recruitment.30mm) %>% 
  add_row(recruitment.30mm=NaN)
recruitment44m.staggered = recruitmentINFO.years[-c(1),] %>%
  select(recruitment.44mm) %>% 
  add_row(recruitment.44mm=NaN)

march = dplyr::filter(krillINFO.months,grepl("July",mo)) %>% 
  add_column(recruitment30m.staggered,recruitment44m.staggered)
july = dplyr::filter(krillINFO.months,grepl("July",mo)) %>% 
  add_column(recruitment30m.staggered,recruitment44m.staggered)
august = dplyr::filter(krillINFO.months,grepl("August",mo)) %>% 
  add_column(recruitment30m.staggered,recruitment44m.staggered)
september = dplyr::filter(krillINFO.months,grepl("September",mo)) %>% 
  add_column(recruitment30m.staggered,recruitment44m.staggered)

marchMod30 = lm(recruitment.30mm~meanLipids,march)
julyMod30 = lm(recruitment.30mm~meanLipids,july)
augustMod30 = lm(recruitment.30mm~meanLipids,august)
septemberMod30 = lm(recruitment.30mm~meanLipids,september)

ggMar30 = ggplot(march, aes(x=meanLipids,y=recruitment.30mm)) +
  geom_point() +
  geom_smooth(method="lm",color="#00BA38") +
  ggtitle("Recruitment (30mm) ~ previous March") +
  labs(subtitle=paste("P-val: ",round(lmp(julyMod30),digits=4)),
       x="Mean omega-3 content (g)",y="Recruitment") +
  xlim(0,25)

ggJul30 = ggplot(july30, aes(x=meanLipids,y=recruitment.30mm)) +
  geom_point() +
  geom_smooth(method="lm",color="#00BA38") +
  ggtitle("Recruitment (30mm) ~ previous July") +
  labs(subtitle=paste("P-val: ",round(lmp(julyMod30),digits=4)),
       x="Mean omega-3 content (g)",y="Recruitment") +
  xlim(0,25)

ggAug30 = ggplot(august30, aes(x=meanLipids,y=recruitment.30mm)) +
  geom_point() +
  geom_smooth(method="lm",color="#00BA38") +
  ggtitle("Recruitment (30mm) ~ previous August") +
  labs(subtitle=paste("P-val: ",round(lmp(augustMod30),digits=4)),
       x="Mean omega-3 content (g)",y="Recruitment") +
  xlim(0,18)

ggSep30 = ggplot(september30, aes(x=meanLipids,y=recruitment.30mm)) +
  geom_point() +
  geom_smooth(method="lm",color="#00BA38") +
  ggtitle("Recruitment (30mm) ~ previous September") +
  labs(subtitle=paste("P-val: ",round(lmp(septemberMod30),digits=4)),
       x="Mean omega-3 content (g)",y="Recruitment") +
  xlim(0,33)

# 44mm
july44 = dplyr::filter(krillINFO.months,grepl("July",mo)) %>% add_column(recruitment44m.staggered)
august44 = dplyr::filter(krillINFO.months,grepl("August",mo)) %>% add_column(recruitment44m.staggered)
september44 = dplyr::filter(krillINFO.months,grepl("September",mo)) %>% add_column(recruitment44m.staggered)
julyMod44 = lm(recruitment.44mm~meanLipids,july44)
augustMod44 = lm(recruitment.44mm~meanLipids,august44)
septemberMod44 = lm(recruitment.44mm~meanLipids,september44)

ggJul44 = ggplot(july44, aes(x=meanLipids,y=recruitment.44mm)) +
  geom_point() +
  geom_smooth(method="lm",color="#F8766D") +
  ggtitle("Recruitment (44mm) ~ previous July") +
  labs(subtitle=paste("P-val: ",round(lmp(julyMod44),digits=4)),
       x="Mean omega-3 content (g)",y="Recruitment") +
  xlim(0,25)

ggAug44 = ggplot(august44, aes(x=meanLipids,y=recruitment.44mm)) +
  geom_point() +
  geom_smooth(method="lm",color="#F8766D") +
  ggtitle("Recruitment (44mm) ~ previous August") +
  labs(subtitle=paste("P-val: ",round(lmp(augustMod44),digits=4)),
       x="Mean omega-3 content (g)",y="Recruitment") +
  xlim(0,18)

ggSep44 = ggplot(september44, aes(x=meanLipids,y=recruitment.44mm)) +
  geom_point() +
  geom_smooth(method="lm",color="#F8766D") +
  ggtitle("Recruitment (44mm) ~ previous September") +
  labs(subtitle=paste("P-val: ",round(lmp(septemberMod44),digits=4)),
       x="Mean omega-3 content (g)",y="Recruitment") +
  xlim(0,33)

plot_grid(ncol=3,nrow=2,ggJul30,ggAug30,ggSep30,ggJul44,ggAug44,ggSep44)

# Plot of recruitment vs average lipid content of july, august, and september ####
#write.xlsx(krillINFO.months,"krillINFO_months.xlsx")
JAGlips = read.xlsx("krillINFO_months.xlsx",sheetIndex=2)
JAGlips$sepLips = as.numeric(JAGlips$sepLips)
JAGlips$recruitment.44mm = recruitment44m.staggered$recruitment.44mm
JAGlips$recruitment.30mm = recruitment30m.staggered$recruitment.30mm
JAGmod_30 = lm(recruitment.30mm~avgLips,JAGlips)
JAGmod_44 = lm(recruitment.44mm~avgLips,JAGlips)
# I just realised I have no clue why I put a "G" in JAG. It's meant to stand for "July August September". What's the "G"? "Geptember"? I'm not changing it, I've typed it so many times now. And I refuse to do the simple fix (find and replace), out of principle. What principle? None of your business.

ggJAG_44 = ggplot(JAGlips, aes(x=avgLips,y=recruitment.44mm)) +
  geom_point() +
  geom_smooth(method="lm",color="#F8766D") +
  ggtitle("Recruitment (44mm threshhold) predicted by average lipid content of prior Austral Winter") +
  labs(subtitle=paste("P-val: ",round(lmp(JAGmod_44),digits=4)),
       x="Mean omega-3 content",y="Recruitment") +
  xlim(0,25)

ggJAG_30 = ggplot(JAGlips, aes(x=avgLips,y=recruitment.30mm)) +
  geom_point() +
  geom_smooth(method="lm",color="#00BA38") +
  ggtitle("Recruitment (30mm threshhold) predicted by average lipid content of prior Austral Winter") +
  labs(subtitle=paste("P-val: ",round(lmp(JAGmod_30),digits=4)),
       x="Mean omega-3 content",y="Recruitment") +
  xlim(0,25)

plot_grid(ggJAG_30,ggJAG_44)

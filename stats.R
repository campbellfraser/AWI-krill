# Krill stats

require(pacman)
pacman::p_load(car,caret)
krill = read.xlsx2("Large Lipid data.xlsx",sheetIndex=1)
krill = rename(krill, Lipids=GasChromatography_FattyAcidProfile_TotalPUFA_Omega3)
krill = rename(krill, Weight=RawKrill_Weight_g)
krill = rename(krill, Size=RawKrill_Size_mm)
krill[c("Date","Lipids","Size","Weight")] = sapply(krill[c("Date","Lipids","Size","Weight")],as.numeric)
krill$date.YMD = as.character(janitor::excel_numeric_to_date(krill$Date))
years = c('2011','2012','2013','2014','2015','2016','2017','2018','2019','2020','2021')

seasonalLipids = data.frame()
for (i in years){
  krillYEAR = dplyr::filter(krill,grepl(as.character(i),date.YMD)) #Filter into one year, since I need to record the year
  JULkrill = dplyr::filter(krillYEAR,grepl("-07-",date.YMD))
  JULsum = sum(JULkrill$Lipids,na.rm=TRUE)
  JULmean = JULsum/length(JULkrill$date.YMD)
  JULsd = sd(JULkrill$Lipids,na.rm=TRUE)
  AUGkrill = dplyr::filter(krillYEAR,grepl("-08-",date.YMD))
  AUGsum = sum(AUGkrill$Lipids,na.rm=TRUE)
  AUGmean = AUGsum/length(AUGkrill$date.YMD)
  AUGsd = sd(AUGkrill$Lipids,na.rm=TRUE)
  seasonalLipids = rbind(seasonalLipids,c(i,JULmean,as.character(month(JULkrill$date.YMD[1])),JULsd))
  seasonalLipids = rbind(seasonalLipids,c(i,AUGmean,as.character(month(AUGkrill$date.YMD[1])),AUGsd))
}
colnames(seasonalLipids) = c("Year","Lipids","Month","SD")
seasonalLipids$Lipids = as.numeric(seasonalLipids$Lipids)
seasonalLipids$Month = as.factor(seasonalLipids$Month)
seasonalLipids$SD = as.numeric(seasonalLipids$SD)
seasonalLipids[seasonalLipids==0] = NA

krillCocktail = ggplot(seasonalLipids,aes(x=Year,y=Lipids,fill=Month)) +
  geom_bar(stat="identity",position=position_dodge(0.9)) +
  scale_fill_discrete(labels=c("July","August")) +
  geom_errorbar(aes(ymin=Lipids,ymax=Lipids+SD),position=position_dodge(0.9),width=0.2) +
  ggtitle("Mean omega-3 Content of Krill Stomachs July/August") +
  labs(y="Mean Omega-3 Content (mg)")
krillCocktail
 # From this bargraph, we would expect to see more successful spawning in Austral Spring of: 2011, 2014, 2015, 2016, and 2020, since the availability of lipids appears to increase

# So what statistical tests can I do to this data? I have lipid content (continuous), and months (categorical). I can compare the means of each Jul/Aug, to see if they differ significantly - that would be a one-way ANOVA.

# 1-WAY ANOVA testing if there is a significant difference between Jul/Aug each year ####
# Assumptions: the observations are independent, the data are normally distributed (shapiro test), and the data has a homogeneity of variance (levene's test)
shapiro.test(seasonalLipids$Lipids) # p-val < 0.05, non-normal
leveneTest(Lipids~Year,seasonalLipids) # p-val <<<< 0.05, non-homogeneous
seasonalLipids$logLipids = log10(seasonalLipids$Lipids)
shapiro.test(seasonalLipids$logLipids) # p-val > 0.05, normal
leveneTest(logLipids~Year,seasonalLipids) # p-val <<< 0.05, non-homogeneous
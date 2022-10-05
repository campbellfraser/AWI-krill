# Yearly lipids and recruitment
source("prerequisites.R")
peng30mm = read_csv("pengDiets_30mm_agg.csv",col_select=c(pMean,pSD,yr,mo,spp))
peng44mm = read_csv("pengDiets_44mm_agg.csv",col_select=c(pMean,pSD,yr,mo,spp))
krillINFO = tibble(
  yr = factor(),
  meanLipids = numeric(),
  recruitment = numeric()
)
for (i in years){
  lipidYear = dplyr::filter(krill,grepl(as.character(i),date.YMD))
  lipidTotal = sum(lipidYear$Lipids, na.rm = TRUE)
  lipidMean = lipidTotal/length(lipidYear$Lipids[!is.na(lipidYear$Lipids)])
  penguinYear = dplyr::filter(peng44mm,yr==i)
  penguinTotal = sum(penguinYear$pMean, na.rm = TRUE)
  penguinMean = penguinTotal/length(penguinYear$pMean[!is.na(penguinYear$pMean)])
  krillINFO = add_row(krillINFO,yr=as.factor(i),meanLipids=lipidMean,recruitment=penguinMean)
}

# HOWEVER, we want to see the effect of 
with(krillINFO, plot(recruitment~meanLipids))
# Looks like a positive correlation!

shapiro.test(krillINFO$meanLipids) # Normal
shapiro.test(krillINFO$recruitment) # Normal
recruitmentMod = lm(recruitment~meanLipids, krillINFO)
ncvTest(recruitmentMod) # Homoscedastic
summary(recruitmentMod)
ggRecruitment = ggplot(krillINFO, aes(x=meanLipids,y=recruitment)) +
  geom_point() +
  geom_smooth(method="lm",color="#00BA38") +
#  stat_regline_equation(label.x = 31, label.y = 1) +
  geom_text(label=paste("P-val: ",round(lmp(recruitmentMod),digits=4)),
            x=33,y=1.1) +
  ggtitle("Recruitment coefficient predicted by omega-3 content of krill guts") +
  labs(x="Mean omega-3 content (g)",y="Recruitment")
ggRecruitment + theme_bw()

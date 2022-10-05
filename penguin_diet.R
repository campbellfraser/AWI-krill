# Penguin diet
source("prerequisites.R")
peng30mm = read_csv("pengDiets_30mm_agg.csv",col_select=c(pMean,pSD,yr,mo,spp))
peng30mm2011on = peng30mm %>% # Filter to only include the years we have lipid data for (2011 onwards)
  filter(yr > 2010)
peng30mm2011on = add_row(peng30mm2011on, pMean=NA,pSD=NA,yr=2021,mo=NA,spp=NA)
peng30mm2011on$yr = as.factor(peng30mm2011on$yr)

pengBar30mm = ggplot(peng30mm2011on,aes(x=yr,y=pMean)) +
  geom_bar(stat="identity",fill="#7CAE00") +
  #geom_errorbar(aes(ymin=pMean-pSD,ymax=pMean+pSD)) +
  ggtitle("Krill recruitment (30mm threshhold)") +
  labs(x="Year",y="Recruitment")
pengBar30mm

# Same as above, but for 44mm
peng44mm = read_csv("pengDiets_44mm_agg.csv",col_select=c(pMean,pSD,yr,mo,spp))
peng44mm2011on = peng44mm %>%
  filter(yr > 2010)
peng44mm2011on = add_row(peng44mm2011on, pMean=NA,pSD=NA,yr=2021,mo=NA,spp=NA)
peng44mm2011on$yr = as.factor(peng44mm2011on$yr)
peng44mm2011on$mo = as.factor(peng44mm2011on$mo)

pengBar44mm = ggplot(peng44mm2011on,aes(x=yr,y=pMean)) +
  geom_bar(stat="identity",fill="#F8766D") +
  #geom_errorbar(aes(ymin=pMean-pSD,ymax=pMean+pSD)) +
  ggtitle("Krill recruitment (44mm threshhold)") +
  labs(x="Year",y="Recruitment")
pengBar44mm

# Now a big figure to compare each threshhold with each other and with krill lipid data
plot_grid(ncol=1,pengBar30mm,pengBar44mm,krillCocktail)
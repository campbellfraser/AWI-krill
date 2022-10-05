# Krill lipids bar chart
inspectaMonth = readline(prompt="Months: ") %>%
  strsplit("[[:space:]]") %>%
  unlist()

seasonalLipids = data.frame()
for (i in years){
  krillYEAR = dplyr::filter(krill,grepl(as.character(i),date.YMD)) #Filter into one year, since I need to record the year
  for (j in inspectaMonth){
    MONTHkrill = dplyr::filter(krillYEAR,grepl(paste0("-",j,"-"),date.YMD))
    MONTHsum = sum(MONTHkrill$Lipids,na.rm=TRUE)
    MONTHmean = MONTHsum/length(MONTHkrill$date.YMD)
    MONTHsd = sd(MONTHkrill$Lipids,na.rm=TRUE)
    seasonalLipids = rbind(seasonalLipids,c(i,MONTHmean,j,MONTHsd,MONTHsum))
  }
}

colnames(seasonalLipids) = c("Year","Lipids","Month","SD","Total")
seasonalLipids$Lipids = as.numeric(seasonalLipids$Lipids)
seasonalLipids$Month = as.factor(seasonalLipids$Month)
seasonalLipids$SD = as.numeric(seasonalLipids$SD)
seasonalLipids$Total = as.numeric(seasonalLipids$Total)
seasonalLipids[seasonalLipids==0] = NA

krillCocktail = ggplot(seasonalLipids,aes(x=Year,y=Lipids,fill=Month)) +
  geom_bar(stat="identity",position=position_dodge(0.9)) +
  scale_fill_discrete(labels=allMonthsLong[as.numeric(inspectaMonth)]) +
  geom_errorbar(aes(ymin=Lipids,ymax=Lipids+SD),position=position_dodge(0.9),width=0.2) +
  ggtitle("Mean omega-3 Content of Krill Stomachs") +
  labs(y="Mean Omega-3 Content (mg)")
krillCocktail

# From this bar graph, we would expect to see more successful spawning in Austral Spring of: 2011, 2014, 2015, 2016, and 2020, since the availability of lipids appears to increase

krillCocktailSum = ggplot(seasonalLipids,aes(x=Year,y=Total,fill=Month)) +
  geom_bar(stat="identity",position=position_dodge(0.9)) +
  scale_fill_discrete(labels=allMonthsLong[as.numeric(inspectaMonth)]) +
  ggtitle("Total omega-3 Content of Krill Stomachs") +
  labs(y="Total Omega-3 Content (mg)")
krillCocktailSum
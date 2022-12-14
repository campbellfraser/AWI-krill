# 14/09/2022 | Pie charts for each year to illustrate mean monthly lipid content
#Prerequisites ####
source("prerequisites.R")

#Testing with just one year (prior to loop testing) ####
krill2013 = dplyr::filter(krill,grepl(2013,date.YMD)) #Filters the whole data set to only use one year; this one is 2013
for (j in krill2013$date.YMD){ #Extracts the month from each date, so we can easily differentiate by month
  krill2013$specMonth=(stri_sub(krill2013$date.YMD,6,7))
}
monthlyLipids = data.frame() #New data frame to put the monthly lipid data into
#unique.months = sort(unique(krill2013$specMonth))

#This section sums up each month's total lipids and puts them into a separate dataframe
#This means that we can more clearly visualise the separation between months

krill_pal = hue_pal()(12)
for (u in monthNums){
  tempMonth = dplyr::filter(krill2013,grepl(u,specMonth))
  monthSum = sum(tempMonth$Lipids,na.rm=TRUE)
  monthMean = monthSum/length(tempMonth$specMonth)
  if (((is.nan(monthMean)==TRUE|(monthMean==0)==TRUE))==TRUE){
    monthlyLipids = rbind(monthlyLipids,c(0,u,2013))
    krill_pal[as.numeric(u)] = "#454545"
  } else {
    monthlyLipids = rbind(monthlyLipids,c(monthMean,u,2013))}
}

colnames(monthlyLipids) = c("lipidMean","Month","Year")
monthlyLipids$lipidMean = as.numeric(monthlyLipids$lipidMean)
monthlyLipids$Month = as.factor(monthlyLipids$Month)
monthlyLipids$Year = as.character(monthlyLipids$Year)

pieTitle = expression(paste("Mean Monthly Lipid Content of ", italic("Euphausia superba"), ", 2013"))
krillPie = ggplot(monthlyLipids,aes(x=Year,y=lipidMean,fill=Month)) +
  geom_bar(stat="identity",width=1,color="white") +
  coord_polar("y",start=0,direction=-1,clip="off") +
  theme_void() +
  scale_fill_manual(labels=c(allMonths),values=krill_pal) +
  ggtitle(pieTitle)
krillPie

# Looop! ####
for (i in years){
  print(i)
  krill_pal = hue_pal()(12)
  remove(krillYEAR,krillPie,tempMonth,labelPos)
  krillYEAR = krill %>%
    select(Lipids,date.YMD) %>%
    dplyr::filter(grepl(i,date.YMD))
  for (j in krillYEAR$date.YMD){
    krillYEAR$specMonth=(stri_sub(krillYEAR$date.YMD,6,7))
    monthlyLipids = data.frame()
    badMonth = c()
    n=1
    
    for (u in monthNums){
      tempMonth = dplyr::filter(krillYEAR,grepl(u,specMonth))
      monthSum = sum(tempMonth$Lipids,na.rm=TRUE)
      monthMean = monthSum/length(tempMonth$specMonth[!is.na(tempMonth$specMonth)])
      if (((is.nan(monthMean)==TRUE|(monthMean==0)==TRUE))==TRUE){ # Surely there's an easier way to do this? Oh well...
        monthlyLipids = rbind(monthlyLipids,c(0,u,i))
        krill_pal[as.numeric(u)] = "#454545"
        badMonth[n] = as.numeric(u)
        n=n+1
      } else {
        monthlyLipids = rbind(monthlyLipids,c(monthMean,u,i))}
    }
    
    colnames(monthlyLipids) = c("lipidMean","Month","Year")
    monthlyLipids$lipidMean = as.numeric(monthlyLipids$lipidMean)
    monthlyLipids$Month = as.factor(monthlyLipids$Month)
    monthlyLipids$Year = as.character(monthlyLipids$Year)
    
    labelPos = monthlyLipids %>%
      mutate(csum = rev(cumsum(rev(lipidMean))),
             pos = lipidMean/2 + lead(csum, 1),
             pos = if_else(is.na(pos), lipidMean/2, pos,))
    labelPos$pos[badMonth] = NA
    
    krillPie = ggplot(monthlyLipids,aes(x=Year,y=lipidMean,fill=Month)) +
      geom_bar(stat="identity",
               width=1,
               color="white") +
      coord_polar("y",start=0,direction=-1,clip="off") +
      theme_void() +
      scale_fill_manual(labels=c(allMonths),values=krill_pal) +
      geom_label_repel(labelPos, mapping=aes(y=pos, label=paste0(round(lipidMean,digits=2),"mg")),
                       size = 3, nudge_x = 0.7, show.legend = FALSE, na.rm = TRUE) +
      ggtitle(i,subtitle=paste(as.character(round((sum(krillYEAR$Lipids,na.rm=TRUE)/1000),digits=2)), "g total")) + theme(plot.title = element_text(size = 16.5, face = "bold"), plot.subtitle = element_text(size = 15),legend.title=element_text(size=8.5,face="bold"),legend.text=element_text(size=8))
    assign(paste0("krill",i),krillPie)
  }
}

# Combined plot ####
pieFestival = plot_grid(krill2011,krill2012,krill2013,krill2014,krill2015,krill2016,krill2017,krill2018,krill2019,krill2020,krill2021)
title = ggdraw() +
  draw_label("Monthly Mean Omega-3 Content of Krill Stomachs (2011-2022)",
    fontface = "bold",
    size = 17.5,
    x = 0,
    hjust = 0) +
  theme(plot.margin = margin(0,0,0,7))

plot_grid(
  title,pieFestival,
  ncol = 1,
  rel_heights = c(0.1,1))

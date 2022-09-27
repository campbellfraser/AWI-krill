# Krill prerequisites
library(pacman)
pacman::p_load(ggplot2,dplyr,forcats,xlsx,cowplot,janitor,lubridate,colorspace,stringi,scales,ggrepel,car,caret,knitr)
krill = read.xlsx2("Large Lipid data.xlsx",sheetIndex=1) #Load the data
# Rename a bunch of variables... because they're annoying to type
krill = rename(krill, Lipids=GasChromatography_FattyAcidProfile_TotalPUFA_Omega3)
krill = rename(krill, Weight=RawKrill_Weight_g)
krill = rename(krill, Size=RawKrill_Size_mm)
# Convert a bunch of variables to numeric... because xlsx automatically loads them as characters
krill[c("Date","Lipids","Size","Weight")] = sapply(krill[c("Date","Lipids","Size","Weight")],as.numeric)
krill$date.YMD = as.character(janitor::excel_numeric_to_date(krill$Date)) #Convert the dates to a recognisable format using janitor package

# Set up the character objects necessary for a bunch of stuff
years = c('2011','2012','2013','2014','2015','2016','2017','2018','2019','2020','2021')
allMonths = c("J","F","M","A","M","J","J","A","S","O","N","D")
monthNums = c("01","02","03","04","05","06","07","08","09","10","11","12")
allMonthsLong = c("January","February","March","April","May","June","July","August","September","October","November","December")
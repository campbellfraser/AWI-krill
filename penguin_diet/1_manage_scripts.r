# set 'path.nm' to user's working directory
path.nm <- '~/placement/alfred-wegener/Bettina Meyer/data/penguin_diet/'
setwd(paste(path.nm,sep=''))
juv.l <- 30 # krill length in mm defined as <= juveniles
            # krill juv.l of 30, 35, 38, 40, 44 can be run without code modification
nareas <- 1 # no. sampling strata, must be 1 or 4
nlegs <- 1  # no. sampling legs, must be 1 or 2 
nages <- 7  # number of age classes
site.nm <- c('COPA','CS')
geo <- c('481S','481N')
if(!dir.exists(paste('plots_',juv.l,'mm',sep=''))){
    dir.create('propRec_csvs')
    dir.create(paste('plots_',juv.l,'mm',sep=''))
    }
path.s <- paste(path.nm)
path.dat <- paste(path.nm)
path.out <- paste(path.nm,'propRec_csvs/',sep='')

source(paste(path.s,'6_amlr_peng.r',sep='')) # AERD penguins
source(paste(path.s,'7_plot_propRec.r',sep='')) # plot proportional recruitments
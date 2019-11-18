# read deposition parameters from NTE-3000
#
##########################################

# CONFIGURATION
source('config.R')
file.PREFIX = 'NTE.readParameters'
file.list = file.path(path.RAW, dir(path.RAW, pattern='_NTE_', recursive = TRUE))


##########################################
library(chron)
library(lubridate)
library(ggplot2)
library(reshape2)
library(pspline)
library(cowplot)
source('func.R')

r = data.frame()
no = 0
for(filename in file.list) {
  no = no+1
  user = strsplit(filename,'_')[[1]][3]
  sample.name = strsplit(filename,'_')[[1]][5]
  print(paste("Sample:",sample.name))
  d = NTE.load(filename)
  #g1 = NTE.plotSummary(d, sz=1)
  #print(g1)
  #ggsave('images/plotSummary.png', width=4,height=3, dpi=300)
  
  d2 = NTE.depStartEnd(d)
  if (!(is.na(d2))) {
    #g2 = NTE.plotSummary(d2, sz=1)
    #print(g2)
    #ggsave('images/plotDeposition.png', width=4,height=3, dpi=300)
    
    # find relevant parameters
    #summary(d2)
    r1 = NTE.getParams(d)
    r1$user = user
    r1$sample = sample.name
    r = rbind(r, r1)
    
    g3 = NTE.plotDeposition(d)
    ggsave2(plot = g3, file = file.path(path.FIGS,paste0(file.PREFIX,'-',no,'-',sample.name,'.png')),
           width=6, height=4, dpi=300)
  }
  #ggsave('images/plotDepositionColor.png', width=4,height=3, dpi=300)
  
}
r
write.csv(r, file = file.path(path.RESULTS, paste0(file.PREFIX,'-sampleList-NTE.csv')))

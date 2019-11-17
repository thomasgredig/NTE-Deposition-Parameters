# read deposition parameters from NTE-3000
#
##########################################

# CONFIGURATION
source('config.R')
file.list = file.path(path.RAW, dir(path.RAW, pattern='_NTE_', recursive = TRUE))


##########################################
library(chron)
library(lubridate)
library(ggplot2)
library(reshape2)
library(pspline)
library(cowplot)
source('func.R')

filename = file.list[3]
d = NTE.load(file.list[3])
g1 = NTE.plotSummary(d, sz=1)
print(g1)
ggsave('images/plotSummary.png', width=4,height=3, dpi=300)

d2 = NTE.depStartEnd(d)
g2 = NTE.plotSummary(d2, sz=1)
print(g2)
ggsave('images/plotDeposition.png', width=4,height=3, dpi=300)

# find relevant parameters
summary(d2)
NTE.getParams(d)

g3 = NTE.plotDeposition(d)
print(g3)
ggsave('images/plotDepositionColor.png', width=4,height=3, dpi=300)

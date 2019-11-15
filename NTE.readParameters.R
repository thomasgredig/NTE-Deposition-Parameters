# read deposition parameters from NTE-3000
#
##########################################

# CONFIGURATION
filename = "7.22.2017.txt"


##########################################
library(chron)
library(lubridate)
library(ggplot2)
library(reshape2)
library(pspline)
#library(colortools)


# load the data and create extra columns
d <- read.csv(filename, header=FALSE, sep='\t')
names(d)=c('pressure','T1','T2','T.substrate','thickness','timedate','na')
d$na <- NULL
d$time <- chron(times. = substr(d$timedate,1,8))
d$time.sec = period_to_seconds(hms(d$time))
d$time.step = c(1E-6,diff(d$time.sec))
d$deposition.rate = c(0,diff(d$thickness)) / d$time.step
d$deposition.rate.smooth = predict(sm.spline(d$time.sec, d$deposition.rate, df=60), d$time.sec)
head(d)

# determine active times
start.deposition = max(which(d$thickness== 0))
d2 = d[start.deposition:nrow(d),]
d3 = melt(d2, id.vars='time.sec')
d3$value = as.numeric(d3$value)

# estimate start and end of deposition
ratediff =  c(0,diff(d2$deposition.rate.smooth))
deposition.start.elem = which(ratediff == max(ratediff))+1
deposition.start = d2$time.sec[deposition.start.elem]
ratediff.nonsmooth = c(0,diff(d2$deposition.rate))
deposition.end.elem = which(ratediff.nonsmooth == min(ratediff.nonsmooth))-1
deposition.end = d2$time.sec[deposition.end.elem]
d.deposit = subset(d2, time.sec>=deposition.start & time.sec<=deposition.end)


# plot thickness
thickness.est = d2$thickness[deposition.end.elem] - d2$thickness[deposition.start.elem]
time.est = deposition.end - deposition.start
ggplot(d2, aes(time.sec, thickness)) +
  geom_rect(aes(xmin=deposition.start, xmax=deposition.end, ymin=-Inf, ymax=Inf),fill='grey90',alpha=0.1, color='grey') +
  geom_point(col='red') +
  ylab('thickness (units)') +
  ggtitle(paste('estimated thickness: ',thickness.est,'units; est. time: ',time.est,'s')) +
  theme_bw(base_size=14)

# plot rate of deposition
rate.est = signif(mean(d.deposit$deposition.rate),2)
rate.est.sd = signif(sd(d.deposit$deposition.rate),1)
ggplot(subset(d3,variable=='deposition.rate' | variable=='deposition.rate.smooth'),
       aes(time.sec, value, color=variable)) +
  geom_rect(aes(xmin=deposition.start, xmax=deposition.end, ymin=-Inf, ymax=Inf),fill='grey90',alpha=0.1, color='grey') +
  ggtitle(paste('estimated average rate: ',rate.est,'+/-',rate.est.sd,'units/s')) +
  geom_point() +
  geom_line() +
  ylab('rate (units/s)') +
  theme_bw(base_size=14) +
  theme(legend.position = c(0.1,0.9))


# plot sample temperature
Tdep.est = signif(mean(d.deposit$T.substrate),2)
Tdep.est.sd = signif(sd(d.deposit$T.substrate),1)
ggplot(d2, aes(time.sec, T.substrate)) +
  geom_rect(aes(xmin=deposition.start, xmax=deposition.end, ymin=-Inf, ymax=Inf),fill='grey90',alpha=0.1, color='grey') +
  geom_rect(aes(xmin=deposition.start, xmax=deposition.end, ymin=Tdep.est-Tdep.est.sd, ymax=Tdep.est+Tdep.est.sd),
            fill='orange',alpha=1, color='orange') +
  geom_point(col='red') +
  ggtitle(paste('estimated deposition temperature: ',Tdep.est,'+/-',Tdep.est.sd,'oC')) +
  ylab(expression(paste('substrate temperature T'[dep],' ('^o,'C)'))) +
  theme_bw(base_size=14)



# plot pressure
base.pressure = min(d2$pressure)
p.est = signif(mean(d.deposit$pressure),2)
x.pos = (max(d2$time.sec) - min(d2$time.sec))*0.4 + min(d2$time.sec)
base.pressure.text = paste("base pressure = ",signif(base.pressure),'torr')
ggplot(d2, aes(time.sec, pressure)) +
  geom_rect(aes(xmin=deposition.start, xmax=deposition.end, ymin=1E-7, ymax=5E-5),fill='grey90',alpha=0.1, color='grey') +
  geom_point(col='purple') +
  scale_y_log10(limits=c(1E-7, 5E-5)) +
  geom_hline(yintercept = base.pressure, col='coral', size=5, alpha=0.3) +
  ggtitle(paste('estimated average deposition pressure: ',p.est,'torr')) +
  ylab('pressure (torr)') +
  annotate("text", x = x.pos, y = base.pressure, label = base.pressure.text, col='blue') +
  theme_bw(base_size=14)
# find complementary color: col = complementary('coral', plot=FALSE)[2]



# plot crucible temperature
ggplot(subset(d3,variable=='T1' | variable=='T2'),
       aes(time.sec, value, color=variable)) +
  geom_point() +
  geom_line() +
  ylab('Crucible Temperatures (oC)') +
  theme_bw(base_size=14) +
  theme(legend.position = c(0.1,0.9))
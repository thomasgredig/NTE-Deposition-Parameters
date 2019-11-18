# load the data and create extra columns
NTE.load <- function(filename) {
  if (!(file.exists(filename))) { warning(paste("File",filename,"does not exist!"))}
  d <- read.csv(filename, header=FALSE, sep='\t', stringsAsFactors = FALSE)
  names(d)=c('pressure','T1','T2','T.substrate','thickness','timedate','na')
  d = d[,1:6]
  d = na.omit(d)
  
  d[1:5] <- sapply(d[1:5],as.numeric)
  d = na.omit(d)

  d$time <- chron(times. = substr(d$timedate,1,8))
  d$time.sec = period_to_seconds(hms(d$time))
  d$time.step = c(1E-6,diff(d$time.sec))
  d$pressure.mBar = 1.333*d$pressure
  d$deposition.rate = c(0,diff(d$thickness)) / d$time.step
  d$deposition.rate.smooth = predict(sm.spline(d$time.sec, d$deposition.rate, df=60), d$time.sec)
  d$deposition.rate[which(abs(d$deposition.rate)>4)] <- NA
  d
}



# sz = is the point size
NTE.plotSummary <- function(d,sz=2) {
  p1 = ggplot(d, aes(time.sec/3600,pressure*1e6)) + 
    geom_path(size=sz, col='red') + 
    xlab('time (h)') + 
    ylab(expression(paste('p (',mu,'Torr)'))) +
    theme_bw()
  
  p2 = ggplot(d, aes(time.sec/3600,thickness)) + 
    geom_path(size=sz, col='red') + 
    xlab('time (h)') + 
    ylab(expression(paste('t (m.u.)'))) +
    theme_bw()
  
  p3 = ggplot(subset(d, T.substrate<300), aes(time.sec/3600,T.substrate)) + 
    geom_point(size=sz, col='red') + 
    xlab('time (h)') + 
    ylab(expression(paste('T'[sub],' ('^o,'C)'))) +
    theme_bw()
  
  p4 = ggplot(d, aes(time.sec/3600,deposition.rate)) + 
    geom_point(size=sz, col='red') + 
    xlab('time (h)') + 
    ylab(expression(paste('r (m.u./s)'))) +
    theme_bw()
  
  p.sum = plot_grid(p1, p2, p3, p4, labels = c('A', 'B', 'C', 'D'), label_size = 12)
  p.sum
}


NTE.depStartEnd <- function(d,buffer = 0) {
  #q1 = which(d$thickness>8)
  q1 = which(d$deposition.rate>0.2)
  if (length(q1)==0) { return (NA); }
  # add buffer of about 15%
  q1.buf = round(length(q1)*buffer)
  q.start = max(0,q1[1] - q1.buf)
  q.end = min(nrow(d),q1[length(q1)]+q1.buf)
  
  # return subset
  d[(q.start:q.end),]
}


NTE.getParams <- function(d) {
  d2 = NTE.depStartEnd(d)
  data.frame(
    base.pressure.torr = mean(d$pressure[1:nrow(d)*0.2]),
    deposition.pressure = mean(d2$pressure),
    T.deposition.oC = mean(d2$T.substrate),
    T.deposition.oC.sd = sd(d2$T.substrate),
    deposition.time.s = max(d2$time.sec) - min(d2$time.sec),
    thickness.mu = max(d2$thickness) - min(d2$thickness),
    rate.mu.per.s = mean(d2$deposition.rate),
    rate.mu.per.s.sd = sd(d2$deposition.rate)
  )
}

NTE.plotDeposition <- function(d, buffer=0.5) {
  d3 = NTE.depStartEnd(d, buffer)
  deposition.start = d2$time.sec[1]/3600
  deposition.end = d2$time.sec[nrow(d2)]/3600
  Tdep.max = mean(d2$T.substrate) + sd(d2$T.substrate)
  Tdep.min = mean(d2$T.substrate) - sd(d2$T.substrate)
  p.max = (mean(d2$pressure) + sd(d2$pressure))*1e6
  p.min = (mean(d2$pressure) - sd(d2$pressure))*1e6
  rate.max = (mean(d2$deposition.rate) + sd(d2$deposition.rate))
  rate.min = (mean(d2$deposition.rate) - sd(d2$deposition.rate))
  
  p1 = ggplot(d3, aes(time.sec/3600,pressure*1e6)) + 
    geom_rect(aes(xmin=deposition.start, xmax=deposition.end, ymin=-Inf, ymax=Inf),fill='grey90',alpha=0.1, color='grey') +
    geom_rect(aes(xmin=deposition.start, xmax=deposition.end, ymin=p.max, ymax=p.min),
              fill='orange',alpha=1, color='orange') +
    geom_path(size=2, col='red') + 
    xlab('time (h)') + 
    ylab(expression(paste('p (',mu,'Torr)'))) +
    ggtitle(paste('pressure:',signif(mean(d2$pressure),2),'Torr')) +
    theme_bw(base_size = 8)
  
  p2 = ggplot(d3, aes(time.sec/3600,thickness)) + 
    geom_rect(aes(xmin=deposition.start, xmax=deposition.end, ymin=-Inf, ymax=Inf),fill='grey90',alpha=0.1, color='grey') +
    geom_path(size=2, col='red') + 
    xlab('time (h)') + 
    ylab(expression(paste('t (m.u.)'))) +
    ggtitle(paste('thickness:',signif(max(d2$thickness)-min(d2$thickness),2),'m.u.')) +
    theme_bw(base_size = 8)
  
  p3 = ggplot(d3, aes(time.sec/3600,T.substrate)) + 
    geom_rect(aes(xmin=deposition.start, xmax=deposition.end, ymin=-Inf, ymax=Inf),fill='grey90',alpha=0.1, color='grey') +
    geom_rect(aes(xmin=deposition.start, xmax=deposition.end, ymin=Tdep.max, ymax=Tdep.min),
              fill='orange',alpha=1, color='orange') +
    geom_point(size=2, col='red') + 
    xlab('time (h)') + 
    ylab(expression(paste('T'[sub],' ('^o,'C)'))) +
    ggtitle(paste('T substrate:',signif(mean(d2$T.substrate),2),'oC')) +
    theme_bw(base_size = 8)
  
  p4 = ggplot(d3, aes(time.sec/3600,deposition.rate)) + 
    geom_rect(aes(xmin=deposition.start, xmax=deposition.end, ymin=-Inf, ymax=Inf),fill='grey90',alpha=0.1, color='grey') +
    geom_rect(aes(xmin=deposition.start, xmax=deposition.end, ymin=rate.max, ymax=rate.min),
              fill='orange',alpha=1, color='orange') +
    geom_point(size=2, col='red') + 
    xlab('time (h)') + 
    ylab(expression(paste('r (m.u./s)'))) +
    ggtitle(paste('rate:',signif(mean(d2$deposition.rate),2),'m.u./s'))
    theme_bw(base_size = 8)
  
  p.sum = plot_grid(p1, p2, p3, p4, labels = c('A', 'B', 'C', 'D'), label_size = 12)
  p.sum
}

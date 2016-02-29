modu<-function(x,m)
{
  t1<-floor(x/m)
  return(x-t1*m)
}
carrington.l0.angle = function(date, 
                               L0.date=as.Date("1854-01-01"),
                               solar.cycle.period=25.38){
  require(lubridate)
  DT <- interval(L0.date,as.Date(date))
  #DT / solar.cycle.period
  n.P <- as.numeric(seconds(DT)) / as.numeric(seconds(solar.cycle.period * 24 * 60 * 60))
  360 - modu(n.P * 360, 360)
  }

carrington.to.cvm <- function(lat, date){
  # carrington to central visible meridian
  carr.m <- carrington.l0.angle(date)
  lat - carr.m
}
carrington.l0.angle("2015-2-10")

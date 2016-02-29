library(httr)
start.year = 1875
stop.year = 2016
data <- NULL
url ="http://solarscience.msfc.nasa.gov/greenwch/"
for(year in seq(from=start.year, to=stop.year, by=1))
  {
  r <- GET(paste(url, 'g', year,'.txt', sep=""))
  if(r$status_code == 200){
    write(x=as.character(r), file=paste('../data/Sunspots/by.year/g', year, '.txt', sep=""))
    data<-paste(data, as.character(r), sep="")
  }
}
write(x=as.character(data), file='../data/Sunspots/data.txt')

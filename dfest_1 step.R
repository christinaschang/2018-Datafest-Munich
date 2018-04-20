library(readr)
library(tidyverse)
library(ggplot2)
library(plotly)

setwd("/Users/Berlin/Desktop/datafest2018_data_and_documentation/data")

conn <- read_tsv("conn.csv")
class(conn$ts)

conn$ts_POS <- as.POSIXlt(conn$ts, origin = "1970-01-01", tz = "GMT")

conn$weekday <- weekdays(conn$ts_POS)
class(conn$weekday)
# [1] "character"

hours <- format(as.POSIXct(strptime(conn$ts_POS,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%H")
conn$hours <- hours
class(conn$hours)
#[1] "character"
conn$hours <- as.numeric(conn$hours)

write.csv(conn, file = "conn_posix.csv")







table(conn$proto)
#  icmp    tcp    udp 
#  54822 813890 377756 

str(conn)

conn$duration <- as.numeric(conn$duration)
conn$orig_bytes <- as.numeric(conn$orig_bytes)
conn$resp_bytes <- as.numeric(conn$resp_bytes)
conn$proto <- as.factor(conn$proto)

summary(conn$duration)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.0     0.0     3.0    14.7    14.7  2556.5  837801 

# anything above 15?

ggplot(conn) +
  geom_histogram(aes(duration)) ->plt_dur

ggplotly(plt_dur)

ggplot(conn) +
  geom_histogram(aes(duration)) +
  coord_cartesian(ylim=c(0,100)) -> plt_dur_outliers

ggplotly (plt_dur_outliers)

ggplot(conn1, aes(ts,duration, color = proto)) +
  geom_point() -> plt_ts_dur

ggplotly(plt_ts_dur)  


ggplot(conn1) +
  geom_histogram(aes(duration_minutes))

dns <- read_tsv("dns.csv")
dpd <- read_tsv("dpd.csv")
files <- read_tsv("files.csv")
geolocation <- read_tsv("geolocation.csv")
host <- read_tsv("host.csv")
http <- read_tsv("http.csv")
sip <- read_tsv("sip.csv")
snmp <- read_tsv("ssl.csv")
weird <- read_tsv("weird.csv")
x509 <- read_tsv("x509.csv")





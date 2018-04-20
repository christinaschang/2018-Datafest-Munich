library(readr)
library(tidyverse)
library(ggplot2)
library(plotly)
library(RColorBrewer)
library(lubridate)
library(rworldmap)
library(scales)

setwd("/Users/Berlin/Desktop/datafest2018_data_and_documentation/data")

conn1 <- read_csv("conn_posix.csv")

View(conn1)

# Tiles by weekday and hour

conn1$weekday <- wday(conn1$ts_POS)
conn1$cweekday <- wday(conn1$ts_POS, label = T)
conn1$tod <- as.numeric(conn1$ts_POS - as.POSIXct(strftime(conn1$ts_POS,format="%Y-%m-%d")))/60
conn1$bins <- cut(conn1$tod,breaks=1:24,labels=F)
counts <- aggregate(X1~bins+weekday,conn1,length)
colnames(counts)[ncol(counts)] <- "Events"

View(conn1)

conn1 %>% 
  group_by(day) %>% 
  
table(conn1$cweekday)
# Sun    Mon    Tue    Wed    Thu    Fri    Sat 
# 103268 117269 113852 436644 170431 208713  96291 

ggplot(counts, aes(x=bins,y=8-weekday))+
  geom_tile(aes(fill=Events))+
  scale_fill_gradientn(colours=brewer.pal(9,"YlOrRd"),
                       breaks=seq(0,max(counts$Events),by=100))+
  scale_y_continuous(breaks=7:1,labels=c("Sat","Sun","Mon","Tues","Wed","Thurs","Fri"))+
  labs(x="Time of Day (hours)", y="Day of Week")+
  coord_fixed()



# Tiles by weekday & month
conn1$day1 <- day(conn1$ts_POS)
conn1$week <- week(conn1$ts_POS)
conn1$month <- month(conn1$ts_POS)
conn1$year <- year(conn1$ts_POS)

counts1 <- aggregate(X1~week+month,conn1,length)
colnames(counts1)[ncol(counts1)] <- "Events1"

ggplot(counts1, aes(day1, week)) +
  geom_tile(aes(fill = Events1)) +
  scale_fill_gradientn(colours=brewer.pal(9,"YlOrRd"),
                       breaks=seq(0,max(counts$Events),by=100))

ggplot(conn1, aes(week, month, z = day1)) + geom_tile(aes(fill = day1))

+
  scale_y_continuous(breaks=7:1,labels=c("Sun","Mon","Tues","Wed","Thurs","Fri","Sat"))+
  labs(x="Time of Day (hours)", y="Day of Week")+
  coord_fixed()

# Wednesday & Friday

class(conn1$duration)
conn1$duration <- as.numeric(conn1$duration)
conn1$duration_minutes <- conn1$duration/60
summary(conn1$duration_minutes)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.0     0.0     0.0     0.2     0.2    42.6  837801 

# NOT helpful
p <- ggplot(conn1, aes(x=cweekday, y=duration)) + 
  geom_boxplot()

ggplotly(p)

# duration over 1 minute

  
install.packages("rworldmap")

n <- joinCountryData2Map(conn1, joinCode="N", nameJoinColumn="country")

h <- hist(conn1$ts_POS, breaks=32)

conn1$day <- as.numeric(conn1$ts_POS - as.POSIXct(strftime(conn1$ts_POS,format="%Y-%m-%d")))

hist(conn1$ts_POS, breaks = "days", # or weeks
     col="red", main = "Histogram of Connections",  
     xlab = "Timestamp", ylab = "Frequency",freq=TRUE)


conn1$day <- strftime(conn1$ts_POS, "%Y/%m/%d")
View(conn1)
sort(table(conn1$day))

ggplot(conn1, aes(day)) +
  geom_bar(identity="count") 

freqs <- aggregate(conn1$ts_POS, by=list(conn1$ts_POS), FUN=length)
View(freqs)

freqs$names <- as.Date(freqs$Group.1, format="%Y-%m-%d")


freqs %>% 
  group_by(names) %>% 
  ggplot(., aes(x=names, y=x, fill=x)) + geom_bar(stat="identity") +
  ylab("Frequency") + xlab("Year and Month") +
  scale_x_date(labels=date_format("%Y-%b")) +
  scale_y_continuous(labels = comma) +
  scale_fill_gradient()

class(conn1$conn_state)
conn1$day <- strftime(conn1$ts_POS, "%Y/%m/%d")
conn1$day <- as.Date(conn1$day)
conn1$conn_state <- as.factor(conn1$conn_state)


conn1 %>% 
  group_by(day, conn_state) %>% 
  mutate(total=n()) %>% 
  ggplot(.,aes(x=day, y= total, group = conn_state)) +
  geom_line(aes(color = conn_state)) +
  ylab("Freq") + xlab("Year and Month") +
  ggtitle("Frequency of Connection by Status") +
  scale_x_date(labels=date_format("%Y-%b")) +
  scale_y_continuous(labels = comma)

# aggregated

ggplot(conn1,aes(x=day)) +
  geom_histogram(aes(color="red", fill = "red")) +
  ylab("Freq") + xlab("Year and Month") +
  ggtitle("Frequency of Connection by Date") +
  scale_x_date(labels=date_format("%Y-%b")) +
  scale_y_continuous(labels = comma)

View(conn1)


ggplotly(spike_plot)

write.csv(conn1, file = "conn2.csv")


ggplot(conn1, aes(x=day, y=temp)) + 
  geom_line(aes(group=day, fill=day, color=day))+
  stat_summary(fun.y = mean, na.rm = TRUE, group = 3, color = 'black', geom ='line')


write.csv(freqs, file = "freqs.csv")

conn1 %>% 
  filter(day == "2014/10/24") -> oct24 # friday

View(oct23)

sort(table(oct24$id.orig_h))

conn1 %>% 
  filter(day=="2015/04/29") -> apr29 # wednesday

sort(table(apr29$id.orig_h))

spikes <- cbind(c("2015/04/29","2014/10/24"),
                c("331266", " 90849"),
                c("5.61.38.11", "84.151.62.120"),
                c("327711","85040"),
                c("Germany","Germany"))

colnames(spikes) <- c("date","connections_total","ip_adress","ip_counts","geo_location")

spikes <- as.data.frame(spikes, stringsAsFactors = FALSE)

spikes$connections_total <- as.numeric(spikes$connections_total)

spikes$ip_counts <- as.numeric(spikes$ip_counts)

spikes <- transform(spikes, ip_percent = ip_counts / connections_total)

# Detailed Info Retrieval

conn1$id.orig_h[conn1$id.orig_h =="192.168.0.12"]


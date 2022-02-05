library(data.table)
library(curl)
## library(optparse)

today <- substr(Sys.time(), 3, 10)
filename <- paste0("~/Downloads/us-counties-",today,".csv")
source <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"
curl_download(destfile = filename, source)
data <- read.csv(filename)
dim(data)
head(data)
summary(as.Date(data$date))

args <- commandArgs()

# This dataset shows cumulative cases and deaths

dt <- data
setDT(dt)
dt$date <-  as.Date(dt$date)
dt$state <- state.abb[match(dt$state, state.name)]
dt[, cfr := deaths / cases]
dt[, state.county := paste0(state, ":", county)]
dt[, id := paste0(state, ":", county, ":", fips)]

# counties <- data.frame(state=c("CA", "CA", "CA", "CA", "CA", "CA", "CA", "CA", "CA"),
#                       county=c("San Francisco", "San Mateo", "Santa Clara", "Alameda", "Contra Costa", "Marin", "Sonoma", "Solano", "Napa"))#, "Santa Cruz"))
# counties$population <- c(870044, 765935, 1922200, 1643700, 1133247, 260295, 501317, 438530, 137744)#, 273213)
# counties$city.name <- paste(counties$county)
counties <- data.frame(state=c("FL", "FL", "WA", ##"FL",
                              "NY", "MA", "PA",
                              "CA", "AZ", "GA",
                              "CA", "IL", "TX"),
                      county=c("Miami-Dade", "Broward", "King",## "Palm Beach",
                               "New York City", "Suffolk", "Allegheny",
                               "San Francisco", "Maricopa", "Fulton",
                               "Los Angeles", "Cook", "Harris"))
counties$population <- c(2717000, 1953000, 2253000, ##1497000,
                        8419000, 803907, 1216000,
                        874691, 4485000, 1064000,
                        10040000, 5150000, 4713000)#, 273213)
counties$city.name <- c("Miami", "Miami", "Seattle", ## Miami
                       "", "Boston", "Pittsburgh",
                       "", "Phoenix", "Atlanta",
                       "", "Chicago", "Houston")

dt[, state.county := paste0(state, ":", county)]
counties$state.county <- with(counties, paste0(state, ":", county))
this.dt <- dt
this.dt[, state.county := paste0(state, ":", county)]
names(this.dt)
names(counties)
this.dt <- merge(this.dt, counties, by="state.county")
this.dt[, new.cases := c(filter(cases, c(1,-1))), by=state.county]
this.dt[, new.cases.smo := filter(new.cases, rep(1/7, 7)), by=state.county]
this.dt[, new.cases.smo := ifelse(is.na(new.cases.smo), 0, new.cases.smo)]
this.dt[, new.cases.smo.per.million := new.cases.smo * (1e6/population)]
ymax <- max(this.dt[, this.dt$new.cases.smo.per.million])

## -----
setDT(counties)
counties <- counties[order(state.county)]
par(mfrow = c(4, 3), mai=rep(0.4, 4), las=1, mgp=c(3,0.5,0))
for (i in seq_len(nrow(counties))) {
    state. <- counties[i,]$state
    county. <- counties[i,]$county
    city.name. <- counties[i,]$city.name
    city.name.shown <- ifelse(nchar(city.name.)>0,
                              paste0(" (", city.name., ")"), "")
    pop. <- counties[i,]$population
    cat("county =", toString(county.), "\n")
    item <- dt[state==state.,][county==county., ][date >= "2020-03-15",]
    ##item <- dt[state==state. & county==county., ]
    ##with(item, plot(date, cases, type='o'))
    ##
    ## Is this correct even when dates are missing?
    new.cases <- filter(item$cases, c(1,-1))
    new.deaths <- filter(item$deaths, c(1,-1))
    new.cases.smo <- filter(new.cases, rep(1,7)/7)##c(rep(3,7), rep(1,7))/28)
    new.deaths.smo <- filter(new.deaths, rep(1,7)/7)
    with(item, plot(date, new.cases.smo * (1e6 / pop.), type='l', lty=1,
                    xaxt="n", ylab="", ylim=c(0, ymax)))
    ticks.at <- seq(as.Date("2020-03-01"), max(item$date+30), by = "months")
    labels <-  month.name[sapply(ticks.at, month)]
    labels <- sapply(labels, function(s) substr(s,1,1))
    Axis(item$date, at = ticks.at, labels = labels, side = 1, las = 1, cex.axis = 0.6)
    abline(h=1000*(0:20), col="#00000050")
    abline(h=5000*(0:20), col="#000000A0")
    abline(h=0, col="#00000080")
    reveillons <- sapply(paste0(2019:2022, "-01-01"), as.Date)
    abline(v=reveillons, col="#66666688", lty=1)
    with(item, points(date, 100*new.deaths.smo * (1e6 / pop.), type='l', col="red", lty=1))
    title(paste0(county., ", ", state., city.name.shown,
                "\npop. ", round(pop., digits=3)))
}


## DELAGGED by 21 days
filename <- paste0("~/covid-by-county_", today, "_delag21.png")
res <- 100
png(filename, res=res, width=1200, height=180*ceiling(nrow(counties) / 3))
par(mfrow = c(nrow(counties) / 3, 3), mai=rep(0.5, 4), las=1, mgp=c(3,0.5,0))
for (i in seq_len(nrow(counties))) {
    state. <- counties[i,]$state
    county. <- counties[i,]$county
    city.name. <- counties[i,]$city.name
    city.name.shown <- ifelse(nchar(city.name.)>0,
                              paste0(" (", city.name., ")"), "")
    pop. <- counties[i,]$population
    cat("county =", toString(county.), "\n")
    item <- dt[state==state.,][county==county., ][date >= "2020-03-15",]
    ##item <- dt[state==state. & county==county., ]
    ##with(item, plot(date, cases, type='o'))
    ##
    ## Is this correct even when dates are missing?
    new.cases <- filter(item$cases, c(1,-1))
    new.deaths <- filter(item$deaths, c(1,-1))
    new.cases.smo <- filter(new.cases, rep(1,7)/7)##c(rep(3,7), rep(1,7))/28)
    new.deaths.smo <- filter(new.deaths, rep(1,7)/7)
    with(item, plot(date, new.cases.smo * (1e6 / pop.), type='l', lwd=1.5,
                    lty=1, xaxt="n", xlab="", ylab="", ylim=c(0, ymax)))
    ticks.at <- seq(as.Date("2020-03-01"), max(item$date+30), by = "months")
    labels <-  month.name[sapply(ticks.at, month)]
    labels <- sapply(labels, function(s) substr(s,1,1))
    Axis(item$date, at = ticks.at, labels = labels, side = 1, las = 1, cex.axis = 0.6)
    abline(h=1000*(0:20), col="#00000050")
    abline(h=5000*(0:20), col="#000000A0")
    ##    abline(h=100*(0:20), col="#00000020")
    abline(h=0, col="#00000080")
    reveillons <- sapply(paste0(2019:2022, "-01-01"), as.Date)
    abline(v=reveillons, col="#66666688", lty=1)
    with(item, points(date - 21, 100*new.deaths.smo * (1e6 / pop.), type='l',
                      col="red", lty=1, lwd=1.2))
    title(paste0(county., ", ", state., city.name.shown,
                 "\npop. ", round(pop., digits=3)))
}
dev.off()

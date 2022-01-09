library(data.table)
data <- read.csv("~/Downloads/us-counties-220108.csv")
dim(data)
head(data)
summary(as.Date(data$date))



# This dataset shows cumulative cases and deaths

dt <- data
setDT(dt)
dt$date <-  as.Date(dt$date)
dt$state <- state.abb[match(dt$state, state.name)]
dt[, cfr := deaths / cases]
dt[, state.county := paste0(state, ":", county)]
dt[, id := paste0(state, ":", county, ":", fips)]

## ToDo: new cases, smoothed
max(table(dt$fips))
summary(dt$date)

ids <- unique(dt$id)
ds <- data.frame()
cat(length(ids))
count <- 0
for (id. in ids) {
    count <- count + 1
    cat(count, " ")
    d <- dt[id==id., ]
    if (nrow(d)==0){
        cat(id., "\n")
        next
    }
    ## make sure sorted
    left.idx <- seq_len(nrow(d) - 1)
    right.idx <- left.idx + 1
    counts <- c("cases", "deaths")
    new.counts <- d[right.idx, counts, with=FALSE] - d[left.idx, counts, with=FALSE]
    names(new.counts) <- paste0("new.", counts)
    new.counts <- rbind(data.frame(new.cases=NA, new.deaths=NA), new.counts)
    d <- cbind(d, new.counts)
    ds <- rbind(ds, d)
}

#save(dt, file="~/dt.Rdata")
load("~/dt_0108.Rdata")
##load("~/dt.Rdata")

# counies <- data.frame(state=c("CA", "CA", "CA", "CA", "CA", "CA", "CA", "CA", "CA"),
#                       county=c("San Francisco", "San Mateo", "Santa Clara", "Alameda", "Contra Costa", "Marin", "Sonoma", "Solano", "Napa"))#, "Santa Cruz"))
# counies$population <- c(870044, 765935, 1922200, 1643700, 1133247, 260295, 501317, 438530, 137744)#, 273213)
counies <- data.frame(state=c("FL", "FL", "FL",
                              "NY", "MA", "PA",
                              "CA", "AZ", "GA",
                              "CA", "IL", "TX"),
                      county=c("Miami-Dade", "Broward", "Palm Beach",
                               "New York City", "Suffolk", "Allegheny",
                               "San Francisco", "Maricopa", "Fulton",
                               "Los Angeles", "Cook", "Harris"))
counies$population <- c(2717000, 1953000, 1497000,
                        8419000, 803907, 1216000,
                        874691, 4485000, 1064000,
                        10040000, 5150000, 4713000)#, 273213)
counies$city.name <- c("Miami", "Miami", "Miami",
                       "", "Boston", "Pittsburgh",
                       "", "Phoenix", "Atlanta",
                       "", "Chicago", "Houston")

dt[, state.county := paste0(state, ":", county)]
state.counties <- with(counies, paste0(state, ":", county))
this.dt <- dt[ state.county %in% state.counties,]
##this.dt <- dt[ date >= "2020-09-01", ]
this.dt <- merge(this.dt, counies, by="state.county")
## Fill in zeros
##this.dt <- merge(, , all.x=TRUE)
this.dt[, new.cases := c(filter(cases, c(1,-1))), by=state.county]
this.dt[, new.cases.smo := filter(new.cases, rep(1/7, 7)), by=state.county]
this.dt[, new.cases.smo := ifelse(is.na(new.cases.smo), 0, new.cases.smo)]
this.dt[, new.cases.smo.per.million := new.cases.smo * (1e6/population)]
ymax <- max(this.dt[, this.dt$new.cases.smo.per.million])

## -----

counies$state.county <- paste0(counies$state, ":", counies$county)
setDT(counies)                                        #counies
counies <- counies[order(state.county)]
par(mfrow = c(4, 3), mai=rep(0.4, 4), las=1, mgp=c(3,0.5,0))
for (i in seq_len(nrow(counies))) {
    state. <- counies[i,]$state
    county. <- counies[i,]$county
    city.name. <- counies[i,]$city.name
    city.name.shown <- ifelse(nchar(city.name.)>0,
                              paste0(" (", city.name., ")"), "")
    pop. <- counies[i,]$population
    cat("county =", toString(county.), "\n")
    item <- dt[state==state.,][county==county., ][date >= "2020-04-15",]
    ##item <- dt[state==state. & county==county., ]
    ##with(item, plot(date, cases, type='o'))
    ##
    ## Is this correct even when dates are missing?
    new.cases <- filter(item$cases, c(1,-1))
    new.deaths <- filter(item$deaths, c(1,-1))
    new.cases.smo <- filter(new.cases, rep(1,7)/7)
    new.deaths.smo <- filter(new.deaths, rep(1,7)/7)
    with(item, plot(date, new.cases.smo * (1e6 / pop.), type='l', lty=1,
                    xaxt="n", ylab="", ylim=c(0, ymax)))
    ticks.at <- seq(as.Date("2020-04-01"), max(item$date+30), by = "months")
    labels <-  month.name[sapply(ticks.at, month)]
    labels <- sapply(labels, function(s) substr(s,1,1))
    Axis(item$date, at = ticks.at, labels = labels, side = 1, las = 1, cex.axis = 0.6)
    abline(h=500*(0:20), col="#00000050")
##    abline(h=100*(0:20), col="#00000020")
    abline(h=0, col="#00000080")
    reveillons <- sapply(paste0(2019:2022, "-01-01"), as.Date)
    abline(v=reveillons, col="#66666688", lty=1)
    with(item, points(date, 100*new.deaths.smo * (1e6 / pop.), type='l', col="red", lty=1))
    title(paste0(county., ", ", state., city.name.shown,
                "\npop. ", round(pop., digits=3)))
    #legend(as.Date("2020-03-01"), max(new.cases.smo, na.rm=TRUE), col=1:2, legend=c("new cases", "new deaths (x 20)"), lty=1, cex=0.7)
}



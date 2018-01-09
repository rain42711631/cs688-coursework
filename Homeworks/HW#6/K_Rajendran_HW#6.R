library(RCurl)
library(RJSONIO)
library(googleVis)
cat("\014")
webpage <- paste("http://powerful-meadow-8588.herokuapp.com/data/12months_departures_joiners.json", sep = "")
data <- fromJSON(getURL(webpage))
names(data)
data$nodes[[1]]

nodes.info <- do.call("rbind", lapply(data$nodes, data.frame))
head(nodes.info)

# ===== Problem-1 =====
# 1-a
x_a <- aggregate(nodes.info$joining, by = list(nodes.info$month), sum)
colnames(x_a) <- c("Month", "Joining")
x_a

# 1-b
x_b <- aggregate(nodes.info$departing, by = list(nodes.info$month),sum)
colnames(x_b) <- c("Month", "Departing")
x_b

# 1-c
x_c = data.frame(x_a, x_b[,2])
colnames(x_c) <- c("Month", "Joining", "Departure")
x_c

# 1-d
line.chart <- gvisLineChart(x_c, xvar = "Month", yvar = c("Joining", "Departure"),
                            options = list(title = "Month-Month Comparison (Line Chart)",
                                           width = 500, height = 300))

column.chart <- gvisColumnChart(x_c, xvar = "Month", yvar = c("Joining", "Departure"),
                                options = list(title = "Month-Month Comparison(Column Chart)",
                                               width = 500, height = 300))
plot(column.chart)

merged.chart <- gvisMerge(line.chart, column.chart, horizontal = TRUE)
plot(merged.chart)

# 1-e
gauge.chart <- gvisGauge(data.frame(x_c$Month,x_c$Departure), options = list(title = "Monthly-departing data (Gauge Chart)",
                                             min = 0, max = 4030, width = 700, height = 300))
plot(gauge.chart)

# 1-f
gauge.chart.colored <- gvisGauge(data.frame(x_c$Month,x_c$Departure), options = list(title = "Monthly-departing data (Gauge Chart)",
                                                min = 0, max = 4030, greenFrom = 0, greenTo = 1000, yellowFrom = 1000, yellowTo = 2000,
                                                redFrom = 2000, redTo = 4030, width = 700, height = 300))
plot(gauge.chart.colored)


# ===== Problem-2 =====
library(SportsAnalytics)

# 2-a
nba <- fetch_NBAPlayerStatistics("13-14")
names(nba)

# 2-b
sprintf("%s has the best field point average.",nba[which.max(nba$FieldGoalsMade / nba$FieldGoalsAttempted),]$Name)

# 2-c
sprintf("%s has the best free throw average.",nba[which.max(nba$FreeThrowsMade / nba$FreeThrowsAttempted),]$Name)

# 2-d
sprintf("%s has the best three point average.",nba[which.max(nba$ThreesMade / nba$ThreesAttempted), ]$Name)

# 2-e
y_e <- data.frame(Name = nba$Name, Team =nba$Team, FieldGoalsMade = nba$FieldGoalsMade,
                  FreeThrowsMade =nba$FreeThrowsMade, ThreesMade = nba$ThreesMade, TotalPoints = nba$TotalPoints)
y_e[,7] <- cbind(2*(nba$FieldGoalsMade - nba$ThreesMade) + nba$FreeThrowsMade + 3*nba$ThreesMade)
colnames(y_e)[7] <- "CalculatedTotalPoints"
y_e[,8] <- cbind(y_e$TotalPoints - y_e$CalculatedTotalPoints)
colnames(y_e)[8] <- "Difference"
sprintf('Are there any differences? %d',max(unique(y_e$Difference)))

# 2-f
nba.ordered <- nba[order(nba$TotalPoints, decreasing = TRUE),]
y_f = data.frame(nba.ordered$Name, nba.ordered$TotalPoints)
colnames(y_f) <-  c("Names", "TotalPoints")
head(y_f, 10)

# 2-g
y_g_line <- gvisLineChart(y_f[1:10,], xvar = "Names", yvar = "TotalPoints",
                          options = list(title = "Players vs TotalPoints"))

y_g_bubble <- gvisBubbleChart(nba.ordered[1:10,], idvar = "Name", xvar = "FreeThrowsAttempted",
                              yvar = "FreeThrowsMade", colorvar = "Team",
                              options = list(title = "FreeThrowsAttempted vs FreeThrowsMade"))

y_g_bar <- gvisColumnChart(data.frame(Players = nba.ordered$Name[1:10], GoalsAttempted = nba.ordered$FieldGoalsAttempted[1:10],
                           GoalsMade = nba.ordered$FieldGoalsMade[1:10]), xvar = "Players", yvar = c("GoalsAttempted", "GoalsMade"),
                           options = list(title = "Players vs FieldGoals", isStacked = TRUE))

y_g_scatter <- gvisScatterChart(data.frame(TotalMinutes = nba.ordered$TotalMinutesPlayed, TotalPoints = nba.ordered$TotalPoints),
                                options = list(title = "TotalMinutesPlayed vs TotalPoints"))


y_g_histogram <- gvisHistogram(data.frame(TotalMinutes = nba.ordered$TotalMinutesPlayed),
                               options = list(title = "Histogram of the Total Minutes played"))

y_g_charts <- gvisMerge(gvisMerge(gvisMerge(y_g_line, y_g_bubble, horizontal = TRUE),
                                  gvisMerge(y_g_bar, y_g_scatter, horizontal = TRUE)), y_g_histogram,
                        horizontal = FALSE)
plot(y_g_charts)

# ===== Problem-3 ======
library(XML)
library(stringr)
library(stringi)

# Fetching the data
webpage <- paste0("http://www.landofbasketball.com/","championships/year_by_year.htm")
data <- readHTMLTable(webpage, which = 1, stringsAsFactors = FALSE)
dim(data)

# Creating an empty data frame
nba.data <- data.frame(Year = numeric(0), Win = numeric(0), Score = numeric(0), Lose = numeric(0),
                       Finals.MVP = numeric(0), Seasons.MVP = numeric(0))
nba.data

# Processing the data
for (ii in 1:dim(data)[1]){
  z <- strsplit(data[ii,], "\\s+")
  numeric.indexes <- grep("-", z[[1]])
  finals.index <- match("Finals", z[[1]])
  season.index <- match("Season", z[[1]])
  a <- z[[1]][1]
  b <- paste(z[[1]][2:(numeric.indexes[2]-1)], collapse = " ")
  c <- z[[1]][numeric.indexes[2]]
  d <- paste(z[[1]][(numeric.indexes[2]+1):(finals.index-1)], collapse = " ")
  e <- paste(z[[1]][(finals.index+2):(season.index-1)], collapse = " ")
  f <- paste(z[[1]][(season.index+2):length(z[[1]])], collapse = " ")
  nba.data[ii,] <- cbind(a,b,c,d,e,f)
}
dim(nba.data)
head(nba.data)

# 3-a
z_a <- nba.data[nba.data$Score == "4-0",]
sprintf("The series swept for %d times",nrow(z_a))

# 3-b
z_b <- table(nba.data$Score)
sprintf("Number of times the series's been decided by 7 times was %d.",z_b["4-3"])

# 3-c
z_c <- as.data.frame(table(nba.data$Win))
z_c.ordered <- z_c[order(z_c$Freq, decreasing = TRUE),]
head(z_c.ordered,5)

# 3-d
z_d_tabled<- table(nba.data$Finals.MVP)

d = z_d_tabled > 1 & names(z_d_tabled) != "-"
z = names(d[d == TRUE])
z_d <- data.frame(z_d_tabled[z])
z_d

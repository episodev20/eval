library(data.table)
library(Synth)
library(reshape2)
library(stringr)

rm(list=ls())




control_countries <- unique(GDP$Country)


control <- c("Brunei Darussalam", "Cabo Verde", "Chile", "China, P.R.: Hong Kong", "China, P.R.: Macao",
                 "China, P.R.: Mainland", "Colombia", "Costa Rica", "Dominican Rep.", "Ecuador", 
                 "Egypt, Arab Rep. of", "El Salvador", "Georgia", "Guatemala", "India", "Indonesia", 
                 "Iran, Islamic Rep. of", "Israel", "Jamaica", "Kazakhstan, Rep. of", "Korea, Rep. of",
                 "Kosovo, Rep. of", "Kyrgyz Rep.", "Malaysia", "Mauritius", "Mexico", "Mongolia", "Morocco",
                 "New Zealand", "Nigeria", "Paraguay", "Peru", "Philippines", "Qatar", "Rwanda", "Samoa",
                 "Saudi Arabia", "Serbia, Rep. of", "Seychelles", "South Africa", "Sri Lanka", "Switzerland",
                 "Thailand", "Turkey", "Uruguay", "West Bank and Gaza", "Treatment")

#control <- c(control, treatment)

GDP <- GDP[Country %in% control]
GDP <- GDP[!is.na(Country_ID)]
GDP <- GDP[Year_coding <= 68]

not_control <- c()

for (country in unique(GDP$Country))
  for (year in c(prep_years, pred_years)) {
    if (is.na(GDP[Country == country & Year == year, GDP])) {
      needed_year <- as.numeric(substr(as.character(year), start = 1, stop = 4))
      if (is.na(GDP[Country == country & Year == needed_year, GDP]) ||
          nrow(GDP[Country == country & Year == needed_year,]) == 0) {
        not_control[length(not_control)+1] <- unique(GDP[Country == country, Country_ID])
      } else {
        holder <- GDP[Country == country & Year == needed_year, GDP]/4
        GDP[Country == country & Year == year, GDP := holder]
      }
    }
  }


for (country in unique(GDP$Country))
  for (year in c(prep_years, pred_years)) {
    if (is.na(GDP[Country == country & Year == year, gdp_change])) {
      needed_year <- as.numeric(substr(as.character(year), start = 1, stop = 4))
      if (is.na(GDP[Country == country & Year == needed_year, gdp_change]) ||
          nrow(GDP[Country == country & Year == needed_year,]) == 0) {
        not_control[length(not_control)+1] <- unique(GDP[Country == country, Country_ID])
      } else {
        holder <- GDP[Country == country & Year == needed_year, gdp_change]/4
        GDP[Country == country & Year == year, gdp_change := holder]
      }
    }
  }


for (country in unique(GDP$Country))
  for (year in c(prep_years, pred_years)) {
    if (is.na(GDP[Country == country & Year == year, Exports])) {
      needed_year <- as.numeric(substr(as.character(year), start = 1, stop = 4))
      if (is.na(GDP[Country == country & Year == needed_year, Exports]) ||
          nrow(GDP[Country == country & Year == needed_year,]) == 0) {
        not_control[length(not_control)+1] <- unique(GDP[Country == country, Country_ID])
      } else {
        GDP[Country == country & Year == year, Exports := GDP[Country == country & Year == needed_year, Exports]/4]
      }
    }
  }

for (country in unique(GDP$Country))
  for (year in c(prep_years, pred_years)) {
    if (is.na(GDP[Country == country & Year == year, Imports])) {
      needed_year <- as.numeric(substr(as.character(year), start = 1, stop = 4))
      if (is.na(GDP[Country == country & Year == needed_year, Imports]) ||
          nrow(GDP[Country == country & Year == needed_year,]) == 0) {
        not_control[length(not_control)+1] <- unique(GDP[Country == country, Country_ID])
      } else {
        GDP[Country == country & Year == year, Imports := GDP[Country == country & Year == needed_year, Imports]/4]
      }
    }
  }



for (country in unique(GDP$Country))
  for (year in c(prep_years, pred_years)) {
    if (is.na(GDP[Country == country & Year == year, Debt])) {
      needed_year <- as.numeric(substr(as.character(year), start = 1, stop = 4))
      if (is.na(GDP[Country == country & Year == needed_year, Debt]) ||
          nrow(GDP[Country == country & Year == needed_year,]) == 0) {
        not_control[length(not_control)+1] <- unique(GDP[Country == country, Country_ID])
      } else {
        GDP[Country == country & Year == year, Debt := GDP[Country == country & Year == needed_year, Debt]]
      }
    }
  }

# for (country in unique(GDP$Country))
#   for (year in c(prep_years, pred_years)) {
#     if (is.na(GDP[Country == country & Year == year, Consumer_price_index])) {
#       needed_year <- as.numeric(substr(as.character(year), start = 1, stop = 4))
#       if (is.na(GDP[Country == country & Year == needed_year, Consumer_price_index]) ||
#           nrow(GDP[Country == country & Year == needed_year,]) == 0) {
#         not_control[length(not_control)+1] <- unique(GDP[Country == country, Country_ID])
#       } else {
#         holder <- GDP[Country == country & Year == needed_year, Consumer_price_index]/4
#         GDP[Country == country & Year == year, Consumer_price_index := holder]
#       }
#     }
#   }

GDP$GDPPC <- GDP$GDP / GDP$Population

controls <- unique(GDP$Country_ID)
controls <- controls[!controls %in% not_control]
# controls <- controls[!controls %in% 147] # nominal
#controls <- controls[!controls %in% 107] # real
controls <- controls[!controls %in% 180] # treatment
#controls <- controls[!controls %in% c(70, 26, 43, 51, 65, 85, 91, 97, 108, 110, 113, 119, 121)] # real
#controls <- controls[!controls %in% c(55, 140, 148, 152)]

GDP <- GDP[!nchar(Year) == 4]

years_matching_table <- as.data.table(unique(GDP$Year))
names(years_matching_table) <- "Year"
years_matching_table$Year_coding <- 1:nrow(years_matching_table)

GDP <- merge(GDP, years_matching_table, all.x = TRUE, by = "Year")
GDP$Year_coding <- as.numeric(GDP$Year_coding)

GDP$GDP_log <- log(GDP$GDP)
GDP$Imports.s<- sqrt(GDP$Imports)

THI.synth.dat <- dataprep(
  foo = GDP,
  predictors = c("Exports", "Imports", "GDP"),
  #predictors = "gdppc",
  predictors.op = "median",
  time.predictors.prior = 37:49,
  dependent = "gdp_change",
  unit.variable = "Country_ID",
  time.variable = "Year_coding",
  #treatment.identifier = 147, # nominal
  treatment.identifier = 180, # real
  controls.identifier = controls,
  time.optimize.ssr = 50:60,
  time.plot = 37:60
)

THI.synth.out <- synth(THI.synth.dat, optimxmethod="All")
path.plot(THI.synth.out, THI.synth.dat, Ylab = "GDP % change", Xlab = "Year",
          Ylim = c(-2, 1.5), 
          Legend = c("EU","synthetic EU"), 
          Legend.position = "bottomright")

path.case <- THI.synth.dat$Y1plot
path.synth <- THI.synth.dat$Y0plot %*% THI.synth.out$solution.w







par(cex.axis=3/4, cex.lab=4/5, mai=c(1/2,1,1/4,1/4))
plot(x=37:60, y=path.synth, type="n", axes=FALSE, ylim=c(-2,2),
     xlab="year", ylab="GDP percent change", lwd=2) 
abline(v=50, col="red", lty=2) # Vertical line at the point of treatment
text(x = 50, y = 14000, "Sanctions", pos = 2, cex = 0.8)  # Label that line
axis(1, tick=FALSE)
axis(2, tick=FALSE, at=seq(-1,0,1), labels=paste0("", seq(-2,0,2)), las=3, pos=min(37:64)+1/2)
lines(x=37:60, y=path.case, type="l", lwd=2) # Plot the main series
lines(x=37:60, y=path.synth, type="l", lty=2, lwd=2, col="gray50") # Plot the synth series as dashed line
legend(x="bottomright", legend=c("Russian Federation", "synthetic control"), lty=c(1,2), lwd=c(2,2), col=c("black", "gray50"), cex=3/4, bty="n")
dev.off()






# 12 is the index of the year index 50 which corresponds to 2012Q4
path_synth_new <- path.synth / mean((path.synth / path.case)[1:12])

par(cex.axis=3/4, cex.lab=4/5, mai=c(1/2,1,1/4,1/4))
plot(x=37:64, y=path_synth_new, type="n", axes=FALSE, ylim=c(300000,800000),
     xlab="year", ylab="GDP per capita (PPP, constant 2011 intl $)", lwd=2) 
abline(v=50, col="red", lty=2) # Vertical line at the point of treatment
text(x = 50, y = 14000, "coup year", pos = 2, cex = 0.8)  # Label that line
axis(1, tick=FALSE)
axis(2, tick=FALSE, at=seq(300000,450000,600000), labels=paste0("$", seq(300000,450000,600000)), las=2, pos=min(37:64)+1/2)
lines(x=37:64, y=path.case, type="l", lwd=2) # Plot the main series
lines(x=37:64, y=path_synth_new, type="l", lty=2, lwd=2, col="gray50") # Plot the synth series as dashed line
legend(x="bottomright", legend=c("Thailand", "synthetic control"), lty=c(1,2), lwd=c(2,2), col=c("black", "gray50"), cex=3/4, bty="n")
dev.off()


synth.tables <- synth.tab(dataprep.res = THI.synth.dat, synth.res = THI.synth.out)


test <- data.table(time_index = 37:60 ,eu_gdp = path.case, synth_gdp = path.synth)
test <- test[time_index > 49]
gdp_real <- 1
for (i in nrow(test)) {
  gdp_real <- gdp_real * (1 + test$eu_gdp.180[i]/100)
}

synth_gdp <- 1
for (i in nrow(test)) {
  synth_gdp <- synth_gdp * (1 + test$synth_gdp.w.weight[i]/100)
}

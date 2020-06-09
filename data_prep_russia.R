library(data.table)
library(Synth)
library(reshape2)

rm(list=ls())

######################
### importing data ###
######################

# https://data.imf.org/regular.aspx?key=62771448
GDP <- read_excel("GDP_real.xlsx",
                  col_types = c("text", rep("numeric", 93)), skip = 6)

#GDP <- read_excel("GDP (3).xlsx", sheet = "Nominal Quarterly", 
#                  col_types = c("text", rep("numeric", 94)), skip = 6)
setDT(GDP)


# https://data.imf.org/regular.aspx?key=61545862
Exchange_Rates <- read_excel("Exchange_Rates_us_dollar.xlsx",
                             col_types = c("text", "text", "text", rep("numeric", 342)), skip = 5)

#Exchange_Rates <- read_excel("Exchange_Rates_incl_Effective_Ex_Ra.xlsx",
#                              col_types = c("text", "text", "text", rep("numeric", 224)), skip = 5)
setDT(Exchange_Rates)
Exchange_Rates[, c("Scale", "Base Year") := NULL]


# https://data.imf.org/regular.aspx?key=61013712
Exports <- read_excel("Exports_and_Imports_by_Areas_and_Co.xlsx", skip = 5)

setDT(Exports)

setnames(Exports, old = "...1", new = "Country")


# https://data.imf.org/regular.aspx?key=61013712
Imports <- read_excel("Exports_and_Imports_by_Areas_and_Co (1).xlsx", skip = 5)

setDT(Imports)

setnames(Imports, old = "...1", new = "Country")


# https://data.worldbank.org/indicator/SP.POP.TOTL
Population <- read_excel("API_SP.POP.TOTL_DS2_en_excel_v2_1121005.xls", skip = 3)
setDT(Population)
Population[, "2019" := NULL]

Population[,c("Country Code", "Indicator Name", "Indicator Code") := NULL]
setnames(Population, old = "Country Name", new = "Country")

# https://data.imf.org/regular.aspx?key=61545861
Consumer_price_index <- read_excel("Consumer_price_index.xlsx",
                                   col_types = c("text", "text", "text", rep("numeric", 306)),
                                   skip = 5)
setDT(Consumer_price_index)
Consumer_price_index[, c("Scale", "Base Year") := NULL]


# https://data.imf.org/?sk=2DFB3380-3603-4D2C-90BE-A04D8BBCE237&sId=1390030341854
# Reserves_and_liquidity <- read_excel("International_Reserves_and_Foreign_.xlsx", skip = 1)
# setDT(Reserves_and_liquidity)
# setnames(Reserves_and_liquidity, old = "...1", new = "Country")
# for (i in 2:164) {
#   Reserves_and_liquidity[, paste0("...", i) := NULL]
# }

# https://data.imf.org/?sk=806ED027-520D-497F-9052-63EC199F5E63&sId=1390030341854
Debt <- read_excel("Historical_Public_Debt_HPDD.xlsx", skip = 1)
setDT(Debt)
setnames(Debt, old = "...1", new = "Country")
for (i in 2:33) {
  Debt[, paste0("...", i) := NULL]
}

# https://data.worldbank.org/indicator/FP.CPI.TOTL.ZG
inflation <- read_excel("inflation.xls", skip = 3)
setDT(inflation)

# https://www.imf.org/external/datamapper/NGDP_RPCH@WEO/OEMDC/ADVEC/WEOWORLD
gdp_change <- read_excel("gdp_change.xls",  col_types = c("text", rep("numeric", 42)))
setDT(gdp_change)
gdp_change <- gdp_change[!1,]
gdp_change <- gdp_change[!227:228,]


###########################
### converting currency ###
###########################

#for countries in exchnage list
for (countries in GDP$Country[GDP$Country %in% Exchange_Rates$Country]) {
  for (time_periods in names(GDP)[2:95]) {
    print(countries)
    print(time_periods) 
    
    if(is.na(GDP[Country == countries, eval(time_periods)])) {
      next()
    } else {
      GDP[Country == countries, eval(time_periods) := 
            GDP[Country == countries, get(time_periods)] / Exchange_Rates[Country == countries, get(time_periods)]]
    }
  }
}




GDP$Country_ID <- as.numeric(as.factor(GDP$Country))


GDP <- melt(GDP, id.vars=c("Country", "Country_ID"))
setnames(GDP, old = c("variable", "value"), new = c("Year", "GDP"))


Exports <- melt(Exports, id.vars="Country")
setnames(Exports, old = c("variable", "value"), new = c("Year", "Exports"))
GDP <- merge(GDP, Exports, all.x = TRUE, by = c("Country", "Year"))



Imports <- melt(Imports, id.vars="Country")
setnames(Imports, old = c("variable", "value"), new = c("Year", "Imports"))
GDP <- merge(GDP, Imports, all.x = TRUE, by = c("Country", "Year"))

GDP$variable <- as.numeric(str_replace(GDP$variable, "Q", ""))


GDP$Country <- sub("\\,.*", "", GDP$Country)


Population <- melt(Population, id.vars="Country")
setnames(Population, old = c("variable", "value"), new = c("Year", "Population"))
Population$Year <- as.numeric(levels(Population$Year))[Population$Year]

Population <- Population[Year > 1999]

GDP$Year <- as.numeric(str_replace(GDP$Year, "Q", ""))
GDP <- merge(GDP, Population, all = TRUE, by = c("Country", "Year"))

prep_years <- as.character(seq(2009, 2014))
prep_years_Q1 <- paste0(prep_years[-length(prep_years)], '1')
prep_years_Q2 <- paste0(prep_years[-length(prep_years)], '2')
prep_years_Q3 <- paste0(prep_years[-length(prep_years)], '3')
prep_years_Q4 <- paste0(prep_years[-length(prep_years)], '4')
prep_years <- c(prep_years_Q1, prep_years_Q2, prep_years_Q3, prep_years_Q4)
prep_years <- as.numeric(prep_years)
prep_years <- sort(prep_years)

pred_years <- as.character(seq(2014, 2016))
pred_years_Q1 <- paste0(pred_years[-length(pred_years)], '1')
pred_years_Q2 <- paste0(pred_years[-length(pred_years)], '2')
pred_years_Q3 <- paste0(pred_years[-length(pred_years)], '3')
pred_years_Q4 <- paste0(pred_years[-length(pred_years)], '4')
pred_years <- c(pred_years_Q1, pred_years_Q2, pred_years_Q3, pred_years_Q4)
pred_years <- as.numeric(pred_years)
pred_years <- sort(pred_years)

for (country in unique(GDP$Country))
  for (year in c(prep_years, pred_years)) {
    print(country)
    # wrong merge, maybe will be deleted later
    if (nrow(GDP[Country == country & Year == year]) == 0) {
      GDP <- GDP[!Country == eval(country)]
      next
    }
    if (is.na(GDP[Country == country & Year == year, Population])) {
      needed_year <- as.numeric(substr(as.character(year), start = 1, stop = 4))
      
      if (nrow(GDP[Country == country & Year == needed_year]) == 0) {
        GDP <- GDP[!Country == eval(country)]
        next
      }
      
      holder <- GDP[Country == country & Year == needed_year, Population]
      GDP[Country == country & Year == year, Population := holder]
    }
  }


#unique(GDP[!sub("\\,.*", "", GDP$Country) %in% Population$`Country Name`]$Country)

#agrep("Central African Rep.", Population$`Country Name`, ignore.case = FALSE, value = FALSE,
#      +       max.distance = 0.1)

#reserves renaiming.
# names_reserves <- strsplit(names(Reserves_and_liquidity), " ")
# for (i in 2:length(names_reserves)) {
#   names_reserves[i] <- paste0(names_reserves[[i]][2], names_reserves[[i]][1])
# }
# names_reserves <- unlist(names_reserves)
# names_reserves <- str_replace(names_reserves, "Q", "")
# names(Reserves_and_liquidity) <- names_reserves
# 
# Reserves_and_liquidity <- melt(Reserves_and_liquidity, id.vars="Country")
# setnames(Reserves_and_liquidity, old = c("variable", "value"), new = c("Year", "Reserves"))
# Reserves_and_liquidity$Year <- as.numeric(levels(Reserves_and_liquidity$Year))[Reserves_and_liquidity$Year]
# GDP <- merge(GDP, Reserves_and_liquidity, all.x = TRUE, by = c("Country", "Year"))
# 



Consumer_price_index <- melt(Consumer_price_index, id.vars="Country")
setnames(Consumer_price_index, old = c("variable", "value"), new = c("Year", "Consumer_price_index"))

Consumer_price_index$Year <- str_replace(Consumer_price_index$Year, "Q", "")
Consumer_price_index <- Consumer_price_index[!str_detect(Year, "M")]

Consumer_price_index$Year <- as.numeric(Consumer_price_index$Year)
GDP <- merge(GDP, Consumer_price_index, all.x = TRUE, by = c("Country", "Year"))







Debt <- melt(Debt, id.vars="Country")
setnames(Debt, old = c("variable", "value"), new = c("Year", "Debt"))
Debt$Year <- as.numeric(levels(Debt$Year))[Debt$Year]
Debt$Country <- str_replace(Debt$Country, "Russia", "Russian Federation")

#country_matching <- data.table(Country=character(), Country_GDP=character())

# for (country in unique(Debt$Country)) {
#   if (country %in% GDP$Country) {
#     Debt$Country_GDP <- country
#     next
#   }
#   
#   holder <- agrep(country, unique(GDP$Country), ignore.case = FALSE, value = FALSE, max.distance = 0.1)
#   if (length(holder) > 1) {next}
#   if (!length(holder) == 0){
#     country_matching <- rbind(country_matching, data.table(Country = country, Country_GDP = unique(Debt$Country)[holder]))
#   }
# }


GDP <- merge(GDP, Debt, all.x = TRUE, by = c("Country", "Year"))





for (country in unique(GDP$Country))
  for (year in c(prep_years, pred_years)) {
    print(country)
    # wrong merge, maybe will be deleted later
    # if (nrow(GDP[Country == country & Year == year]) == 0) {
    #   #GDP <- GDP[!Country == eval(country)]
    #   next
    # }
    if (is.na(GDP[Country == country & Year == year, Debt])) {
      needed_year <- as.numeric(substr(as.character(year), start = 1, stop = 4))
      
      if (nrow(GDP[Country == country & Year == needed_year]) == 0) {
        GDP <- GDP[!Country == eval(country)]
        next
      }
      
      holder <- GDP[Country == country & Year == needed_year, Debt]
      if (is.na(holder)) {next}
      GDP[Country == country & Year == year, Debt := holder]
    }
  }





inflation[,c("Country Code", "Indicator Name", "Indicator Code") := NULL]
setnames(inflation, old = "Country Name", new = "Country")

inflation <- melt(inflation, id.vars="Country")
setnames(inflation, old = c("variable", "value"), new = c("Year", "inflation"))
inflation$Year <- as.numeric(levels(inflation$Year))[inflation$Year]

inflation <- inflation[Year > 1999]

GDP <- merge(GDP, inflation, all = TRUE, by = c("Country", "Year"))




for (country in unique(GDP$Country))
  for (year in c(prep_years, pred_years)) {
    print(country)
    # wrong merge, maybe will be deleted later
    if (nrow(GDP[Country == country & Year == year]) == 0) {
      #GDP <- GDP[!Country == eval(country)]
      next
    }
    if (is.na(GDP[Country == country & Year == year, inflation])) {
      needed_year <- as.numeric(substr(as.character(year), start = 1, stop = 4))
      
      if (nrow(GDP[Country == country & Year == needed_year]) == 0) {
        GDP <- GDP[!Country == eval(country)]
        next
      }
      
      holder <- GDP[Country == country & Year == needed_year, inflation]/4
      if (is.na(holder)) {next}
      GDP[Country == country & Year == year, inflation := holder]
    }
  }




setnames(gdp_change, old = "Real GDP growth (Annual percent change)", new = "Country")
gdp_change <- melt(gdp_change, id.vars="Country")
setnames(gdp_change, old = c("variable", "value"), new = c("Year", "gdp_change"))
gdp_change$Year <- as.numeric(levels(gdp_change$Year))[gdp_change$Year]

gdp_change <- gdp_change[Year > 1999]

GDP <- merge(GDP, gdp_change, all = TRUE, by = c("Country", "Year"))



for (country in unique(GDP$Country))
  for (year in c(prep_years, pred_years)) {
    print(country)
    # wrong merge, maybe will be deleted later
    if (nrow(GDP[Country == country & Year == year]) == 0) {
      GDP <- GDP[!Country == eval(country)]
      next
    }
    if (is.na(GDP[Country == country & Year == year, gdp_change])) {
      needed_year <- as.numeric(substr(as.character(year), start = 1, stop = 4))
      
      if (nrow(GDP[Country == country & Year == needed_year]) == 0) {
        GDP <- GDP[!Country == eval(country)]
        next
      }
      
      holder <- GDP[Country == country & Year == needed_year, gdp_change]/4
      if (is.na(holder)) {next}
      GDP[Country == country & Year == year, gdp_change := holder]
    }
  }

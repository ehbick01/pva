# -----------------------------------------------------------
# Scraping https://jeffersonpva.ky.gov/property-search/property-listings/
#
# Manually do this for the first try, but eventually automate
# -----------------------------------------------------------
library(rvest)
library(RCurl)

# https://jeffersonpva.ky.gov/property-search/property-listings/?psfldLow=0&psfldHigh=5000000&psfldFrom=01%2F01%2F2010&psfldTo=06%2F11%2F2015&psfldNeighborhood=&psfldPtype=&searchType=RangeSearch&psfldStories=&psfldBathrooms=&psfldResSqFeetLow=&psfldResSqFeetHigh=&psfldResYearLow=&psfldResYearHigh=&psfldWall=&psfldStreet=&psfldParcel=&psfldComOwner=&psfldComSqFeetLow=&psfldComSqFeetHigh=&psfldComYearLow=&psfldComYearHigh=&psfldLandLow=&psfldLandHigh=&psfldPropClass=&psfldPropUse=&propertySearchFormButton=Search#results

# Random change

# Set cainfo (for Windows)
options(RCurlOptions = list(cainfo = system.file('CurlSSL', 'cacert.pem', package = 'RCurl')))

# Define URL
url <- "https://jeffersonpva.ky.gov/property-search/property-listings/?psfldLow=0&psfldHigh=5000000&psfldFrom=01%2F01%2F2010&psfldTo=06%2F11%2F2015&psfldNeighborhood=&psfldPtype=&searchType=RangeSearch&psfldStories=&psfldBathrooms=&psfldResSqFeetLow=&psfldResSqFeetHigh=&psfldResYearLow=&psfldResYearHigh=&psfldWall=&psfldStreet=&psfldParcel=&psfldComOwner=&psfldComSqFeetLow=&psfldComSqFeetHigh=&psfldComYearLow=&psfldComYearHigh=&psfldLandLow=&psfldLandHigh=&psfldPropClass=&psfldPropUse=&propertySearchFormButton=Search#results"
url <- getURL(url, ssl.cipher.list = 'RC4-SHA')
url <- html(url)

# Pull property hyperlinks only
links <- html_nodes(url, 'a')
links <- html_attr(links, 'href')
links <- as.data.frame(links)
links <- unique(subset(links, grepl('property-details', links)))
View(links)

# Pull sales price & date
sales <- html_nodes(url, 'tbody, tr.alt, #td:nth-child(2), td:nth-child(3)')
sales <- html_text(sales)
sales <- as.data.frame(sales)
sales <- as.data.frame(substr(sales[,1], gregexpr("\\$", sales[,1]), nchar(as.character(sales[,1]))))
sales <- as.data.frame(substr(as.character(sales[,1]), 1, gregexpr(" ", as.character(sales[,1]))[[1]]))
sales$Space <- gregexpr('\n', as.character(sales[,1]))
sales$Revised <- ifelse(sales[,2] > 1, substr(sales[,1], 1, as.numeric(as.character(sales[,2]))-1), as.character(sales[,1]))
sales <- as.data.frame(sales[,3])

# Need to build a key to avoid eliminating sales that happen on the same day at the same price
sales <- unique(sales)

# Separate price and date -- still need to be able to account for same price on same date sales
salesPrice <- as.data.frame(substr(sales[,1], 1, (nchar(as.character(sales[,1]))-10)))
dates <- as.data.frame(substr(sales[,1], nchar(as.character(sales[,1]))-9, nchar(as.character(sales[,1]))))

# Combine sales price and data
salesFinal <- cbind(dates, salesPrice)

# Iterate through the 568 pages
addylinks <- data.frame()
salesdata <- data.frame()

for (i in 1:568) {
  
  # Define URL
  url <- "https://jeffersonpva.ky.gov/property-search/property-listings/?psfldLow=0&psfldHigh=5000000&psfldFrom=01%2F01%2F2010&psfldTo=06%2F11%2F2015&psfldNeighborhood=&psfldPtype=&searchType=RangeSearch&psfldStories=&psfldBathrooms=&psfldResSqFeetLow=&psfldResSqFeetHigh=&psfldResYearLow=&psfldResYearHigh=&psfldWall=&psfldStreet=&psfldParcel=&psfldComOwner=&psfldComSqFeetLow=&psfldComSqFeetHigh=&psfldComYearLow=&psfldComYearHigh=&psfldLandLow=&psfldLandHigh=&psfldPropClass=&psfldPropUse=&propertySearchFormButton=Search#results"
  url <- html(url)
  
  # Pull property hyperlinks only
  links <- html_nodes(url, 'a')
  links <- html_attr(links, 'href')
  links <- as.data.frame(links)
  links <- unique(subset(links, grepl('property-details', links)))
  
  # Add to complete dataset
  addylinks <- rbind(addylinks, links)
  
  # Pull sales price & date
  sales <- html_nodes(url, 'tbody, tr.alt, #td:nth-child(2), td:nth-child(3)')
  sales <- html_text(sales)
  sales <- as.data.frame(sales)
  sales <- as.data.frame(substr(sales[,1], gregexpr("\\$", sales[,1]), nchar(as.character(sales[,1]))))
  sales <- as.data.frame(substr(as.character(sales[,1]), 1, gregexpr(" ", as.character(sales[,1]))[[1]]))
  sales$Space <- gregexpr('\n', as.character(sales[,1]))
  sales$Revised <- ifelse(sales[,2] > 1, substr(sales[,1], 1, as.numeric(as.character(sales[,2]))-1), as.character(sales[,1]))
  sales <- as.data.frame(sales[,3])
  
  # Need to build a key to avoid eliminating sales that happen on the same day at the same price
  sales <- unique(sales)
  
  # Separate price and date -- still need to be able to account for same price on same date sales
  salesPrice <- as.data.frame(substr(sales[,1], 1, (nchar(as.character(sales[,1]))-10)))
  dates <- as.data.frame(substr(sales[,1], nchar(as.character(sales[,1]))-9, nchar(as.character(sales[,1]))))
  
  # Combine sales price and data
  salesFinal <- cbind(dates, salesPrice)
  
  # Add to complete dataset
  salesdata <- rbind(salesdata, salesFinal)
  
}

# Export to csv
salesComplete <- write.csv(salesdata, 'salesComplete.csv')

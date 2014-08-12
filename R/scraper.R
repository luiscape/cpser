# converting UNHCR's real-time data into HDX's format
# Note: I am labelling data about the 'Horn of Africa' as being 'Somalia'.
# That is plain wrong.
# Use this data with caution.

library(rjson)
library(RCurl)

# ScraperWiki's base URL
sw_url <- 'https://ds-ec2.scraperwiki.com/ye7nhjd/uq816xboyfzffx6/sql?q=select * from unhcr_real_time'
doc <- getURL(sw_url)

# TODO: improve using lapply
for (i in 1:length(fromJSON(doc))) {
    it <- data.frame(fromJSON(doc)[i])
    if (i == 1) data <- it
    else data <- rbind(data, it)
}

# indicator
ind_list <- unique(data$module_name)
indicator <- data.frame(indID = c('CHD.O.PRO.20.T2','CHD.O.PRO.21.T2','CHD.O.PRO.22.T2','CHD.O.PRO.23.T2','CHD.O.PRO.24.T2','CHD.O.PRO.25.T2','CHD.O.PRO.26.T2','CHD.O.PRO.27.T2',
'CHD.O.PRO.28.T2','CHD.O.PRO.29.T2','CHD.O.PRO.30.T2','CHD.O.PRO.31.T2','CHD.O.PRO.32.T2'), name = ind_list, units = 'People')

# dataset
dataset <- data.frame(dsID = 'unhcr-real-time', 
                      last_updated = as.character(as.Date(Sys.time())),
                      last_scraped = as.character(as.Date(Sys.time())),
                      name = 'UNHCR Information Portal Data')

# value
data$module_type <- NULL
data$name <- toupper(data$name)
data <- merge(data, indicator, by.x = 'module_name', by.y = 'name')
data$units <- NULL
data$module_name <- NULL
names(data) <- c('region', 'value','period', 'indID')
data$source <- 'http://data.unhcr.org/wiki/index.php/API_Documentation'
data$is_number <- 1
data$region <- ifelse(data$region == 'SYRIA', 'SYR', data$region)
data$region <- ifelse(data$region == 'SOUTHSUDAN', 'SSD', data$region)
data$region <- ifelse(data$region == 'LIBERIA', 'LBR', data$region)
data$region <- ifelse(data$region == 'HORN', 'SOM', data$region)
data$dsID <- 'unhcr-real-time'

value <- data


# writing CSV
write.csv(indicator, 'data/indicator.csv', row.names = F)
write.csv(value, 'data/value.csv', row.names = F)
write.csv(dataset, 'data/dataset.csv', row.names = F)
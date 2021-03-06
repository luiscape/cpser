#' Working with HDX's Curation Process System, a system designed to curate 
#' humanitarian-relevant datasets.
#'
#' CPSer 
#' 
#' CPSer takes a ScraperWiki connection, GitHub repository or data package as 
#' input and checks if the resulting data conforms to the data shape that HDX
#' can ingest. 
#' 
#' @param df 
#'
#' @keywords gis geocode mapping ip address
#'
#' @export
#'
#' @examples
#' # Geocoding a single IP address
#' # x <- data.frame('72.225.192.110')
#' # y <- freegeoip(x)

## testing environ

# dependencies
library(rjson)
library(RCurl)

# variables from scraperwiki
sw <- 'https://ds-ec2.scraperwiki.com/enf6nmy/8ab0038b6f524ae/sql'  # sw data
rw <- 'https://ds-ec2.scraperwiki.com/bcbk6tq/e81a96f6e040418/sql'  # rw data
fts <- 'https://ds-ec2.scraperwiki.com/gbgac6a/8018ef27ff614e4/sql'  # fts data

# variables from CKAN
sw <- 'http://data.hdx.rwlabs.org/storage/f/2014-08-09_02-00-01/csv.zip'  # sw data
rw <- 'http://data.hdx.rwlabs.org/storage/f/2014-08-09_02-03-01/csv.zip'  # rw data
fts <- 'http://data.hdx.rwlabs.org/storage/f/2014-08-09_02-06-01/csv.zip'  # fts data


cpser <- function(url = NULL, local = FALSE, scraperwiki = FALSE, csv = FALSE, test = FALSE, sample = FALSE, verbose = FALSE) {
    
    ## functionality: take a scraper wiki SQL input, load, and perform tests. -- DONE
    ## functionality: take an URL from CKAN, download file, load, and perform tests. -- DONE
    ## functionality: take a local file, load, and perform tests. -- DONE
    ## functionality: allow for the input of a list of urls.
    
    # sanity checks
    if (is.null(url)) message("Defaulting to local `data` folder.\n")
    if (is.null(url) && local == F) stop('Either use a local file or provide an URL.\n')
    
    # global variables and utils
    table_list <- c('value', 'indicator', 'dataset')  # the current table list
    
    # loop for sw connections
    if (scraperwiki == TRUE) {
        for (i in 1: length(table_list)) {
            query_url <- paste0(url, "?q=select * from ", table_list[i])
            doc <- geturl(query_url)
            
            # TODO: improve using lapply
            message(paste('Downloading:', table_list[i]))
            total <- length(fromJSON(doc))
            if (sample == TRUE) total <- 100
            pb <- txtProgressBar(min = 0, max = total, style = 3, char = ".")
            for (j in 1:total) {
                setTxtProgressBar(pb, j)
                it <- data.frame(fromJSON(doc)[j])
                if (j == 1) data <- it
                else data <- rbind(data, it)
            }
            if (csv == TRUE) {
                file_name <- paste0('data/', table_list[i], ".csv")
                write.csv(data, file_name, row.names = F)
            }
        }
    }
    
    # global tests
    # test values inside tables
    testData <- function() {
        # utils
        names_v <- c('dsID', 'region', 'indID', 'period', 'value', 'is_number', 'source')
        names_i <- c('indID', 'name', 'units')
        names_d <- c('dsID', 'last_updated', 'last_scraped', 'name')
        
        # test list
        test_list <- data.frame(ind = c("Right Names - Values", 
                                        "Right Names - Indicators", 
                                        "Right Names - Datasets"), val = FALSE)
        
        # testing for name length - values
        if (all(names_v %in% names(values))) {
            test_list$val[1] <- TRUE
            message(paste0(test_list$ind[1], ': OK.'))
        } else {
            test_list$val[1] <- FALSE
            message(paste0(test_list$ind[1], ': Not OK.'))
        }
        # testing for name length - indicators
        if (all(names_i %in% names(indicators))) {
            test_list$val[2] <- TRUE
            message('Right Names - Indicators: OK.')
        } else {
            test_list$val[2] <- FALSE
            message('Right Names - Indicators: Not OK.')
        }
        # testing for name length - datasets
        if (all(names_d %in% names(datasets))) {
            test_list$val[3] <- TRUE
            message('Right Names - Datasets: OK.')
        } else {
            test_list$val[3] <- FALSE
            message('Right Names - Datasets: Not OK.')
        }
        
        # printing results
        num <- nrow(test_list[test_list$val == FALSE, ])
        if (num != 0) stop(paste(num, " tests didn't pass.\nCheck the originating file"))
        else message("Data tests successful.\n")
    }
    
    # process for ad hoc data packages
    if (scraperwiki == FALSE && local == FALSE) {
        # wget seems to work best.
        query_url <- url
        if (verbose == TRUE) download.file(query_url, destfile = 'data/csv.zip', method = 'wget')
        else download.file(query_url, destfile = 'data/csv.zip', method = 'wget', quiet = T)
        
        
        # checking if the ZIP package contains the 3 expected files
        testPackage <- function() {
            # test list
            test_list <- data.frame(ind = c("3 Files", "Right Files"), val = FALSE)
            
            # checking for 3 files
            package_list <- unzip('data/csv.zip', list = TRUE)
            if (length(package_list) == 3) {
                test_list$val[1] <- TRUE
                message('3 Files: OK.')
            } else {
                test_list$val[1] <- FALSE
                message('3 Files: Not OK.')
            }
            
            # checking for the right 3 files
            if (any(package_list %in% c('indicator.csv', 'value.csv', 'dataset.csv')) == FALSE) {
                test_list$val[2] <- TRUE
                message('Right Files: OK.')
            } else {
                test_list$val[2] <- FALSE
                message('Right Files: Not OK.')
            }
            
            # printing results
            num <- nrow(test_list[test_list$val == FALSE, ])
            if (num != 0) stop(paste(num, " tests didn't pass.\nCheck the originating file"))
            else message("Package tests successful.\n")
        }
        if (test == TRUE) testPackage()
        
        # if tests pass, then proceed to load package into memory
        # and do other tests.
        
        # utils
        names_v <- c('dsID', 'region', 'indID', 'period', 'value', 'is_number', 'source')
        names_i <- c('indID', 'name', 'units')
        names_d <- c('dsID', 'last_updated', 'last_scraped', 'name')
        
        # unzip + load
        unzip('data/csv.zip', overwrite = TRUE, exdir = "data/")
        values <- read.csv('data/value.csv')
        indicators <- read.csv('data/indicator.csv')
        datasets <- read.csv('data/dataset.csv')
        
        # making tests
        if (test == TRUE) testData()
    }
    
    # testing a local file
    testFile <- function() {

        # read folder
        file <- list.files('data/')
        if (length(file) > 1) stop('There should be a single ZIP package.\nRead the docs.')
        
        # unzip + load
        unzip(paste0('data/', file), overwrite = TRUE, exdir = "data/")
        values <- read.csv('data/value.csv')
        indicators <- read.csv('data/indicator.csv')
        datasets <- read.csv('data/dataset.csv')
        
        # making tests
        if (test == TRUE) testData()
    }
    if (test == TRUE && local == TRUE) testFile()
    ## else message('Something is wrong with your path or local file.\n')
    
    # end
    message("You're done here.")
}
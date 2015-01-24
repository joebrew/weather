library(XML)
require(RCurl)
library(maps)

gadm <- function(country = "USA",
                 level = 2){
  
  # Get alpha-3 ISO codes from wikipedia, naming the resulting df x
  theurl <- "http://en.wikipedia.org/wiki/ISO_3166-1_alpha-3"
  tables <- readHTMLTable(theurl, stringsAsFactors = FALSE)
  n.rows <- unlist(lapply(tables, function(t) dim(t)[1]))
  
  x2 <- tables[[2]]; x2[nrow(x2)+1,] <- names(x2); names(x2) <- c("code", "country")
  x3 <- tables[[3]]; x3[nrow(x3)+1,] <- names(x3); names(x3) <- c("code", "country")
  x4 <- tables[[4]]; x4[nrow(x4)+1,] <- names(x4); names(x4) <- c("code", "country")
  
  x <- rbind(x2, x3, x4)
  x$country <- toupper(x$country)
  
  # which is the best ISO match with the given country?
  best_match <- adist(x = toupper(country),
                      y = x$country)
  best_match <- which.min(best_match)[1]
  
  country_code <- x$code[best_match]
  actual_country <- x$country[best_match]
  
  link <- paste0("http://biogeo.ucdavis.edu/data/gadm2/R/",
                 country_code,
                 "_adm",
                 level,
                 ".RData")
  
  
  if(url.exists(link)){
    new_link <- url(link)
  } else {
    stop("Try a different country or level")
  }
  
  actual_country <- gsub(" |,", "", tolower(actual_country))
  
  load(new_link,
       envir=environment()) # only loads into function environment
  x <- gadm
  assign(paste0(actual_country, level), x, envir =  .GlobalEnv)
  close(new_link)
  
  
  print(paste0("Your SpatialPolygonsDataFrame is named ",
               actual_country, level))
}



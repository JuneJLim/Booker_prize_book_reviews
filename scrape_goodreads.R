# this code is originally from https://community.rstudio.com/t/scraping-goodreads-with-rselenium/48731/5
# and modified to suit my purpose

library(data.table)   
library(dplyr)        
library(magrittr)     
library(rvest)
library(RSelenium)
library(lubridate)
library(stringr)
library(purrr)

#Set your browser settings
rD <- rsDriver(chromever = "80.0.3987.16")
remDr <- rD[["client"]]

# repeat from here to scrape each book (shorcut: ctrl+alt+E)
options(stringsAsFactors = F) #needed to prevent errors when merging data frames

#Paste the GoodReads Url
url <- "https://www.goodreads.com/book/show/43706466-10-minutes-38-seconds-in-this-strange-world"

#Enter the number of review pages (manually check for now)
nPages = 10 
remDr$setTimeout(type = "implicit", 2000)

remDr$navigate(url)

bookTitle = unlist(remDr$getTitle())
finalData = data.frame()

# Main loop going through the website pages
for(pageNumber in 1:nPages){
  
  #Expand all reviews
  expandMore <- remDr$findElements("link text", "...more")
  sapply(expandMore, function(x) x$clickElement())
  
  #Extracting the reviews from the page
  reviews <- remDr$findElements("css selector", "#bookReviews .stacked")
  reviews.html <- lapply(reviews, function(x){x$getElementAttribute("outerHTML")[[1]]})
  reviews.list <- lapply(reviews.html, function(x){read_html(x) %>% html_text()} )
  reviews.text <- unlist(reviews.list)
  
  #Some reviews have only rating and no text, so we process them separately
  onlyRating = unlist(map(1:length(reviews.text), function(i) str_detect(reviews.text[i], "^\\\n\\\n")))
  
  #Full reviews
  if(sum(!onlyRating) > 0){
    
    filterData = reviews.text[!onlyRating]
    fullReviews = purrr::map_df(seq(1, length(filterData), by=2), function(i){
      review = unlist(strsplit(filterData[i], "\n"))
      
      data.frame(
        date = mdy(review[2]), #date
        username = str_trim(review[5]), #user
        rating = str_trim(review[9]), #overall
        comment = str_trim(review[12]) #comment
      )
    })
    
    #Add review text to full reviews
    fullReviews$review = unlist(purrr::map(seq(2, length(filterData), by=2), function(i){
      str_trim(str_remove(filterData[i], "\\s*\\n\\s*\\(less\\)"))
    }))
    
  } else {
    fullReviews = data.frame()
  }
  
  
  #partial reviews (only rating)
  if(sum(onlyRating) > 0){
    
    filterData = reviews.text[onlyRating]
    partialReviews = purrr::map_df(1:length(filterData), function(i){
      review = unlist(strsplit(filterData[i], "\n"))
      
      data.frame(
        date = mdy(review[9]), #date
        username = str_trim(review[4]), #user
        rating = str_trim(review[8]), #overall
        comment = "",
        review = ""
      )
    })
    
  } else {
    partialReviews = data.frame()
  }
  
  finalData = rbind(finalData, fullReviews, partialReviews)
  
  NextPageButton <- remDr$findElement("css selector", ".next_page")
  NextPageButton$clickElement()
  
  message(paste("PAGE", pageNumber, "of", nPages, "Processed"))
  Sys.sleep(4)
}   
#end of the main loop

#Replace missing ratings by 'not rated'
finalData$rating = ifelse(finalData$rating == "", "not rated", finalData$rating)

#Stop server
rD[["server"]]$stop()

#Write results
write.csv(finalData, paste0(bookTitle, ".csv"), row.names = F)
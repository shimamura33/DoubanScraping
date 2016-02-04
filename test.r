library(RCurl)
library(XML)

url <- "http://movie.douban.com/subject/26303622/"    
doc <- htmlTreeParse(url, useInternal=TRUE)    

tablehead <- xpathSApply(doc, "//div[@class='info-area']/strong", xmlValue)

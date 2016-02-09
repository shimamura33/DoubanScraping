################
# my profile ###
################

library(RCurl)
library(XML)

url1 = "http://movie.douban.com/people/99975820/collect?start=" 
url2 = "&sort=time&rating=all&filter=all&mode=grid" 
url = c()
for (k in seq(from=0, to=145, by=15)) {
  url = c(url,paste(url1, k, url2, sep = ""))
}

attribute.name = c()
attribute.release_date = c()
attribute.run_time = c()
attribute.genre = c()
attribute.num_raters = c()
attribute.rating_dist = c()
attribute.imdb_link = c()
attribute.ctry = c()

for (j in url) {
  
  doc = htmlTreeParse(j, useInternal=TRUE)    
  
  hrefs = xpathSApply(doc, "//div/div/div/a", xmlGetAttr, 'href')
  subjects = hrefs[grepl("subject", hrefs)]
  
  
  for (i in subjects) {
    url = i
    
    doc = 
      try(
        htmlTreeParse(
          url, 
          useInternal=TRUE))
    if(class(doc) == "try-error") next;
    
    attribute.name = 
      c(
        attribute.name, 
        xpathSApply(doc, "//div[@class='info-area']/strong", xmlValue))
    
    x = xpathSApply(doc, "//div[@id='info']", xmlValue)
    ctry = 
      gsub(
        "地区: |语言", 
        "", 
        regmatches(x, regexpr('地区:.+?语言', x)))
    attribute.ctry = 
      c(
        attribute.ctry,
        ifelse(
          length(ctry) ==0,
          NA,
          ctry))
    
    attribute.release_date = 
      c(
        attribute.release_date, 
        ifelse(
          length(xpathSApply(doc, "//div[@id='info']/span[@property='v:initialReleaseDate']", xmlValue)) == 0,
          NA,
          xpathSApply(doc, "//div[@id='info']/span[@property='v:initialReleaseDate']", xmlValue)))
    
    attribute.run_time = 
      c(
        attribute.run_time,
        ifelse(
          length(xpathSApply(doc, "//div[@id='info']/span[@property='v:runtime']", xmlValue)) == 0, 
          NA, 
          xpathSApply(doc, "//div[@id='info']/span[@property='v:runtime']", xmlValue)))
    
    attribute.genre = 
      c(
        attribute.genre,
        list(xpathSApply(doc, "//div[@id='info']/span[@property='v:genre']", xmlValue)))
    
    attribute.num_raters = 
      c(
        attribute.num_raters,
        ifelse(
          length(xpathSApply(doc, "//div[@class='rating_sum']/a/span[@property='v:votes']", xmlValue)) == 0,
          NA,
          xpathSApply(doc, "//div[@class='rating_sum']/a/span[@property='v:votes']", xmlValue)))
    
    attribute.rating_dist = 
      rbind(
        attribute.rating_dist,
        ifelse(
          length(xpathSApply(doc, "//div[@class='rating_wrap clearbox']/span[@class='rating_per']", xmlValue)) == 0,
          NA,
          xpathSApply(doc, "//div[@class='rating_wrap clearbox']/span[@class='rating_per']", xmlValue)))
    
    k = xpathSApply(doc, "//div[@id='info']/a[@target='_blank']", xmlGetAttr, 'href')
    link = k[grepl("imdb",k)]
    attribute.imdb_link = 
      c(
        attribute.imdb_link,
        ifelse(
          length(link) == 0,
          NA,
          link))
  }
}

 

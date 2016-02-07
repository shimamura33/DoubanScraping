library(RCurl)
library(XML)

url = "http://movie.douban.com/people/99975820/collect?start=15&sort=time&rating=all&filter=all&mode=grid"    
doc = htmlTreeParse(url, useInternal=TRUE)    

hrefs = xpathSApply(doc, "//div/div/div/a", xmlGetAttr, 'href')
subjects = hrefs[grepl("subject", hrefs)]

attribute.name = c()
attribute.release_date = c()
attribute.run_time = c()
attribute.genre = c()
attribute.num_raters = c()
attribute.rating_dist = c()
attribute.imdb_link = c()
attribute.ctry = c()

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
  attribute.ctry = 
    c(
      attribute.ctry,
      gsub(
        "地区: |语言", 
        "", 
        regmatches(x, regexpr('地区:.+?语言', x)))
    )
  attribute.release_date = 
    c(
      attribute.release_date, 
      list(xpathSApply(doc, "//div[@id='info']/span[@property='v:initialReleaseDate']", xmlValue)))
  attribute.run_time = 
    c(
      attribute.run_time,
      unlist(xpathSApply(doc, "//div[@id='info']/span[@property='v:runtime']", xmlValue)))
  attribute.genre = 
    c(
      attribute.genre,
      list(xpathSApply(doc, "//div[@id='info']/span[@property='v:genre']", xmlValue)))
  attribute.num_raters = 
    c(
      attribute.num_raters,
      xpathSApply(doc, "//div[@class='rating_sum']/a/span[@property='v:votes']", xmlValue))
  attribute.rating_dist = 
    rbind(
      attribute.rating_dist,
      xpathSApply(doc, "//div[@class='rating_wrap clearbox']/span[@class='rating_per']", xmlValue))
  k = xpathSApply(doc, "//div[@id='info']/a[@target='_blank']", xmlGetAttr, 'href')
  link = k[grepl("imdb",k)]
  attribute.imdb_link = 
    c(
      attribute.imdb_link,
      link)
}


 

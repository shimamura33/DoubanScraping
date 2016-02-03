library(RCurl)

url_list <- list()

#觀察原始網站如"http://lishi.tianqi.com/taibei/201101.html"
#月份'01'、'02'是字串格式,預先製作一個陣列存放
month <- c('01','02','03','04','05','06','07', '08', '09', '10', '11', '12')

#利用迴圈自動產生月份網址
for (year in 2011:2013){
  url <- paste('http://lishi.tianqi.com/taibei/',year,month,'.html',sep='')
  url_list <- rbind(url_list,url)
}

rl_list <- unlist(url_list)

myTemp <- function(url){
  #抓取url
  get_url = getURL(url,encoding = "UTF-8")
  #將url解析
  get_url_parse = htmlParse(get_url, encoding = "UTF-8")
  #抓取關鍵的變項，我們需要的變項夾在一個div的class=tqtongji2，裡面<ul>中的<li>標籤裡面
  #標籤裡面還有一些沒有用到的東西沒關係，事後再一併移除
  tablehead <- xpathSApply(get_url_parse, "//div[@class='tqtongji2']/ul/li", xmlValue)
  #將擷取到的關鍵字轉成容易閱讀的矩陣格式
  table <- matrix(tablehead, ncol = 6, byrow = T)
  #回傳值
  return(table)
}

Temp_Total <- lapply(url_list, myTemp)

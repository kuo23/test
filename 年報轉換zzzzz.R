
# 舊制 (Before 2012)
# 第一季財報(3 月)必須於 4  月 30 日以前公布
# 第二季財報(6 月)必須於 8  月 31 日以前公布
# 第三季財報(9 月)必須於 10 月 31 日以前公布
# 年     報(12月)必須於 3  月 31 日以前公布

# 新制 (After 2012)
# 第一季財報(3 月)必須於 5  月 15 日以前公布
# 第二季財報(6 月)必須於 8  月 14 日以前公布
# 第三季財報(9 月)必須於 11 月 14 日以前公布
# 年     報(12月)必須於 3  月 31 日以前公布

# Target: 使function輸出stock_date為財報公布截止日後一天


rm(list = ls()); gc()

if(!require(lubridate)) {install.packages("lubridate")}
if(!require(dplyr))     {install.packages("dplyr")}

library("dplyr")
library("lubridate")

ChangeReportDate <- function(date){
  year <- as.numeric(substring(date, 1 ,4))
  month <- as.numeric(substring(date, 5 ,6))
  
  if (year < 2012){
    if (month == 3){
      stock_date <- paste0(year, "0401")
    }else if (month == 6){
      stock_date <- paste0(year, "0901")
    }else if (month == 9){
      stock_date <- paste0(year, "1101")
    }else if (month == 12){
      stock_date <- paste0(year + 1, "0401")
    }
  }else if (year >= 2012){
    if (month == 3){
      stock_date <- paste0(year, "0516")
    }else if (month == 6){
      stock_date <- paste0(year, "0815")
    }else if (month == 9){
      stock_date <- paste0(year, "1115")
    }else if (month == 12){
      stock_date <- paste0(year + 1, "0401")
    }
  }
  return(stock_date)
}


#範例
# https://raw.githubusercontent.com/kuo23/test/master/data_test.Rdata
load("data_test.Rdata")

data_test <- data_test %>% 
  rowwise() %>%
  mutate(stock_date = ChangeReportDate(as.character(date)))




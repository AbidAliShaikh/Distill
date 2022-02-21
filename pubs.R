library(readxl)
p <- read_xlsx("pubs/SEARCH RESULTS 2020-2021 WITH HEADERS.xlsx")

p2 <- p[-grep(".pdf",p$ArticleURL),]
z=NA
for (i in 1:nrow(p2)){
  con <- url(p2[i,]$ArticleURL)
  x <- try(readLines(con),silent = T)
  close(con)
  
  y <- grep("university of sindh",x,ignore.case = T,value=T)
  y=stringi::stri_join_list(list(y))
  z <- c(z, y)
}

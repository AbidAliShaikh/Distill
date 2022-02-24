library(readxl)
p <- read_xlsx("pubs/SEARCH RESULTS 2020-2021 WITH HEADERS.xlsx")

p2 <- p[-grep(".pdf",p$ArticleURL),]
z=NA

a <- read_html(p2[i,]$ArticleURL)
hn<- a %>% html_node("head") %>% html_children()%>%
  xml_find_all("//meta")%>%
  rvest::html_text()
b<-xml_find_all(a,".//meta[@name='citation_author_institution']")
b<-xml_find_all(a,".//meta[. ='University']")

for (i in 1:nrow(p2)){
  con <- url(p2[i,]$ArticleURL)
  x <- try(readLines(con),silent = T)
  close(con)
  
  y <- grep("university of sindh",x,ignore.case = T,value=T)
  y=stringi::stri_join_list(list(y))
  z <- c(z, y)
}


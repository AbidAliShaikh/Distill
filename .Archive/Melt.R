#following function returns molten value col with 5 likert levels 
Melt <- function(s){
  lvls <- c("Strongly Disagree"=1,"Disagree"=2,"uncertain"=3,"Agree"=4,"Strongly Agree"=5)
  q <- read.csv("surveys/SCEQ/Questionnaires.csv",stringsAsFactors = F)

s2=sapply(s[,names(s)%in%q$Qshort],as.character)
for (i in lvls )
  s2[s2==names(lvls[i])] <- (lvls[i])

s2=apply(s2,2,as.integer)
s1 <- s[,!(names(s)%in%q$Qshort)]
s3<- cbind(s1,s2)

molten <- reshape2::melt(s3)

molten$Indicator <- gsub("[0-9]","",molten$variable)

return(molten)
}


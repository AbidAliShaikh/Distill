#following function returns rowMeans of selected columns with 5 likert levels returning only selected columns (converting to num and rounding 2 digits)
Melt <- function(s){
lvls <- c("Strongly Disagree","Disagree","uncertain","Agree","Strongly Agree")
q <- read.csv("surveys/SCEQ/Questionnaires.csv",stringsAsFactors = F)

s2=sapply(s[,names(s)%in%q$Qshort],as.character)

l=1
for (i in lvls ){
  s2[s2==i] <- l
  l=l+1
}

s2=apply(s2,2,as.integer)
s1 <- s[,!(names(s)%in%q$Qshort)]
s3<- cbind(s1,s2)

molten <- reshape2::melt(s3)

molten$Indicator <- gsub("[0-9]","",molten$variable)

return(molten)
}





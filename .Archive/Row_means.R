#following function returns rowMeans of selected columns with 5 likert levels returning only selected columns (converting to num and rounding 2 digits)
Row_means <- function(s){
l=1
lvls <- c("Strongly Disagree","Disagree","uncertain","Agree","Strongly Agree")
q <- read.csv("surveys/SCEQ/Questionnaires.csv",stringsAsFactors = F)

s=sapply(s[,names(s)%in%q$Qshort],as.character)
for (i in lvls ){
  s[s==i] <- l
  l=l+1
}

s=apply(s,2,as.integer)
a=unique(gsub("[0-9]","",colnames(s)))

b=rep(NULL,nrow(s))
for (i in a){
  b<- cbind(b,rowMeans(s[,grep(i,colnames(s))]))
}
b <- apply(b,2,round,2)
colnames(b) <- a
return(b)
}





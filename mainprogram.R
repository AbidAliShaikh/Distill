
#setwd("C:/Users/abid/OneDrive/PROGRAMMING//RSpace/2019-21/code")
setwd('../code/')

#file = "C:/Users/abid/OneDrive/PROGRAMMING/RSpace/2019-21/data/df (Virtual).csv"
file = '~/data/df (Virtual).csv'
source("df (Virtual) functions.R")
library(likert)
library (lattice)
library (dplyr)
library(reshape2)
library(tidyverse)
library(ggcorrplot)
library(splitstackshape)
library(PerformanceAnalytics)
library(plotrix)
library(vcd)
library(gmodels)
library(scales)
fall <- read.csv(file, header=T)
cl = ncol(fall)
names(fall)[(cl-43):cl] <-  question_short_names()

f <- extract_only_sceq_from_merged_proforma(fall,question_short_names())
f <- add_faculty_column(f)

ff <- extract_only_fac_eval_from_merged_proforma(fall)
ff <- add_faculty_column(ff)


#take 10% grouped samples from original dataset f

#f <- stratified(f, group = c("Campuses","Faculties","Levels","Programs"),size = 1)

# the sceq prefix is used for data frames with indicators in column headings with mean values
#sceq <- SCEQ_get_rowMeans(f)
#sceq_correlogram(sceq,By='Faculties')#by Campuses, Faculties, Programs

#pdf(file = '../output/sceq_cor.pdf', onefile = T)
#sceq_correlogram(sceq,By = 'Programs')
#dev.off()


f_with_labels <- relevel_all_with_label_names (f) # e.g. strongly agree , disagree etc.
jpeg(file='../output/sceq.jpg')
make_likert_plot(f_with_labels,"Students Course Evaluation 2019-21")
dev.off()

#LiKERT PLOT FOR ADDITIONAL CAMPUSES
jpeg(file='../output/alladditionalcampuses.jpg')
camp<- f_with_labels[grep('CAMPUS',f_with_labels$Campuses),]
make_likert_plot(camp,"Students Course Evaluation 2019-21\n Additional Campuses Overall")
dev.off()
jpeg(file='../output/subcampusFACEV.jpg')
camp<- ff_with_labels[grep('CAMPUS',ff_with_labels$Campuses),]
make_likert_plot(camp,"Faculty Evaluation 2019-21\n Additional Campuses Overall")
dev.off()



ff_with_labels <- relevel_all_with_label_names(ff)
jpeg(file = '../output/faculty.jpg')
make_likert_plot(ff_with_labels,"Faculty Evaluation 2019-21")
dev.off()

fmelted <- melt_data(f)
jpeg(filename = '../output/correlogram.jpg')
make_correlogram(fmelted, By="Faculties",titl="Students Course EValuation 2019-21") # by Faculties, Campuses, or Programs
dev.off()


#Sub Campuses correlogram
fmelted <- melt_data(f)
jpeg(filename = '../output/campuscor.jpg')
make_correlogram(fmelted, By="Campuses",titl="Students Course EValuation 2019-21") # by Faculties, Campuses, or Programs
dev.off()


#pdf(file = 'AllProgCor.pdf',onefile = T)
#make_correlogram(fmelted, By="Programs",titl=NA)
#dev.off()

ffmelted <- melt_data(ff)
ffmelted$Indicators <- ffmelted$Qshorts # for compatibility with SCEQ

jpeg(filename = '../output/facultycor.jpg')
make_correlogram(ffmelted, By="Faculties",titl="Faculty Evaluation 2019-21")
dev.off()

# FOR ADDITIONAL CAMPUSES
jpeg(filename = '../output/facultyevalinsubcampuscor.jpg')
make_correlogram(ffmelted, By="Campuses",titl="Faculty Evaluation 2019-21")
dev.off()

fagregated <- aggregate(fmelted$rating, by=list(fmelted$Campuses,fmelted$Faculties,fmelted$Programs,fmelted$Indicators,fmelted$Levels),FUN = mean)
names(fagregated) <- c("Campuses","Faculties","Programs","Indicators","Levels","Means")


# By = 'Faculties' or 'Campuses' or 'Programs'
jpeg(filename = '../output/sceqbwplot.jpg')
make_bw_plot(fagregated, By = 'Faculties',titl = "Students Course Evaluation 2019-21")
dev.off()


# By =  'Campuses'
campusaggregate <- aggregate(fmelted$rating, by=list(fmelted$Campuses,fmelted$Indicators),FUN = mean)
names(campusaggregate) <- c("Campuses","Indicators","Means")
jpeg(filename = '../output/SUBCAMPUSsceqbwplot.jpg')
make_bw_plot(fagregated, By = 'Campuses',titl = "Students Course Evaluation 2019-21")
dev.off()




ffagregated <- aggregate(ffmelted$rating, by=list(ffmelted$Campuses,ffmelted$Faculties,ffmelted$Programs,ffmelted$Indicators,ffmelted$Levels),FUN=mean)
names(ffagregated) <- c("Campuses","Faculties","Programs","Indicators","Levels","Means")

pdf(file   = '../output/faculty_eval_bw.pdf',onefile=T,width=28,height=28)
make_bw_plot(ffagregated, By = 'Faculties',titl="Faculty Evaluation 2019-21")
dev.off()


pdf(file = "../output/FACBWBYPROGS.pdf",onefile = T,width=28, height=28)
make_bw_plot(ffagregated , By = 'Programs',titl = "Faculty Evaluation 2019-21") #if by Programs then barcharts are created
dev.off()




tab = CrossTable(fmelted$Indicators,fmelted$rating,chisq = T)
barplot(tab$prop.row*100)
barpp <- as.data.frame(tab$prop.row)
names(barpp) <- c('Indicators','Rating','percent')
#barpp$percent <- round(barpp$percent*100,2)
ggplot(barpp,aes(x=factor(Rating),y=percent,fill=factor(Rating)))+
  geom_bar(stat='identity',position='dodge')+
  scale_fill_brewer(palette = 'Set2')+
  ggtitle(label='Barplot of Students Course Evaluation 2019-21\nUniversity of Sindh')+
  scale_y_continuous(labels=percent)



fftab = CrossTable(ffmelted$Indicators,ffmelted$rating,chisq = T)
#barplot(tab$prop.row*100)
ffbarpp <- as.data.frame(fftab$prop.row)
names(ffbarpp) <- c('Indicators','Rating','percent')
#barpp$percent <- round(barpp$percent*100,2)
ggplot(ffbarpp,aes(x=factor(Rating),y=percent,fill=factor(Rating)))+
  geom_bar(stat='identity',position='dodge')+
  scale_fill_brewer(palette = 'Set3')
  ggtitle(label='Barplot of Faculty Evaluation 2019-21\nUniversity of Sindh')+
  scale_y_continuous(labels=percent)


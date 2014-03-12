###############################################################################
# example: reading an excel file
#
# case study: one excel file with several worksheets, each containing one
# column of relevant age data - plot all these against each other as KDE (MDS)

#load packages:
library(kdemds)
library(gdata)
#column name of relevant ages in each worksheet:
age_column="Best.Age"

#load data:
filename="/home/martin/Documents/LA-Lab-related/for_Andy/kde_plotting/ZANSKAR2014.xls"
sheets<-sheetNames(filename)
data<-list()
for(s in sheets){
	current<-read.xls(filename,sheet=s,skip=1,fileEncoding="latin1")
	data[[s]]<-current[[age_column]]
}

plotKDE(data)

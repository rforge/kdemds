#################################################
# general example script For U-Pb age spectra (KDEs), MDS mapping and some wizardry...
#################################################

# #set this path to the unzipped folder with all the r scrips:
# setwd("/media/data/martin/LOESS/data/ranalyses/UPb_examples")

library(kdemds)

#################################################
#reading data, store it in a data.frame():
data<-read.csv(paste0(path.package("kdemds"),"/example1.csv"),stringsAsFactors=FALSE)
#cut out "NA" (empty) values from data:
data<-as.list(data)
for(i in 1:length(data)){data[[i]]<-data[[i]][!is.na(data[[i]])]}

#look at sample names contained:
names(data)


#################################################
#KDE plots, new jack-of-all-trades plotting function with many options:

### single KDE plot:
plotKDE(data[["Yellow.River"]])

#similar, but note automatic title (in previous example, column name was lost - this way retains it):
plotKDE(data,plot=c("Yellow.River"))

#set manual title:
plotKDE(data[["Yellow.River"]],title="Pooled data from Yellow River, China")

#another one:
plotKDE(data,plot="Tarim")
#what went wrong?
optimal_bw(data[["Tarim"]])
#for some reason, "optimal" bandwidth is calculated way too large
#prettier: choose bandwidth manually
p1<-plotKDE(data,plot="Tarim",title="Tarim basin",bandwidth=c(30))
#plot it:
p1

#to save it as pdf, uncomment and run this line:
#ggsave(file="KDE_single.pdf",width=12,height=8,plot=p1)

#modify plot object after it's defined:
p1+theme(panel.background=element_blank(),panel.grid.major=element_line(colour="grey90",size=0.5))


### built-in plot options:

#split age axis, set manual bandwidth:
plotKDE(data,plot="Yellow.River",splitat=550)

#add histogram:
plotKDE(data,plot="Yellow.River",hist=TRUE)

#add data markers:
plotKDE(data,plot="Yellow.River",markers="dash")
#or:
plotKDE(data,plot="Yellow.River",markers="circle")


#separate limits and manual bandwidths left and right:
plotKDE(data,plot="Yellow.River",splitat=500,limits=c(200,550,1500,2600),bandwidth=c(10,25))


### stack of KDE plots:
#first, look at calculated optimal bandwidths:
bws<-sapply(data,optimal_bw,n=2^12)
print(bws)
median(bws)

#"full auto" stacked KDE plots:
plotKDE(data)

#classification of samples: lump all "CH11....." into class "Loess", the rest gets the original sample name
tps<-names(data)
tps[grep("CH11.*",tps)]<-"Loess"

#plotting function does good job of choosing bandwidth, tick marks,....:
p2<-plotKDE(data,classes=tps)
#look at it:
p2

#save it as pdf:
#ggsave(file="KDE_comparison.pdf",width=12,height=8,plot=p2)


### showcase new plotting options:

#stacked plots with split x-axis:
plotKDE(data,classes=tps,splitat=500)

#plot all stack in same colour (note automatic removal of legend):
plotKDE(data,classes=-1,splitat=500)

#select only certain data columns:
plotKDE(data,plot=c("Yellow.River","Tarim","Pamir"))

#for the colour-blind:
require(scales)
colours<-brewer_pal(type="div",palette=4)(length(unique(tps)))
plotKDE(data,classes=tps,fcolour=colours)

#(close to) publication quality graph:
plotKDE(data,fcolour="#000000FF")+theme(panel.background=element_blank(),panel.grid.major.x=element_line(colour="grey90",size=0.5),panel.grid.minor.x=element_line(colour="grey90",size=0.3))


#play:
#left and right are same range, different bandwidth. Reversed age scale (oldest left) - messes up titles
plotKDE(data,splitat=500,limits=c(100,550,100,550),bandwidth=c(10,25),title="")+scale_x_reverse()

#################################################
#MDS map:
#calculate dissimilarities:
diss = dissimilarity(data)
#calculate MDS coordinates, reshape to a data.frame():
mds<-isoMDS(as.matrix(diss))$points
mds<-as.data.frame(mds)
names(mds)<-c("x","y")

#add column "area"
mds$area<-row.names(mds)
#lump all loess samples with a common label "Loess":
mds$area[grep("CH11.*",mds$area)]<-"Loess"

#create plot:
p3<-plotMDS(mds,diss,col="area")
#delete legend title:
p3<-p3+guides(fill=guide_legend(title=NULL))

#look at it:
p3

#save output:
#ggsave("MDS_comparison.pdf",plot=p3,width=12,height=8)


#################################################
#Shepard plot:
p4<-plotShepard(mds,diss)

#look at it:
p4

#save it:
#ggsave("Shepard_comparison.pdf",plot=p4,width=12,height=8)

#remove grey background:
p4+theme(panel.background=element_blank(),panel.grid.major=element_line(colour="grey90",size=0.3))

#################################################
#very quick-and-dirty Cumulative age spectra:
#reshape data:
require(reshape2)
data3<-melt(data)
ggplot(data=data3,aes(x=value,colour=L1))+stat_ecdf()


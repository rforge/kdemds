#set this path to the unzipped folder with all the r scrips:
setwd("/media/data/martin/LOESS/data/ranalyses/UPb_examples")

require(compositions)
require(reshape2)
source("PCA_MDS.r")

### A.B. master table:
hmdata<-read.csv("HM_all_master.csv",header=TRUE,stringsAsFactors=FALSE)
#row.names(hmdata)<-hmdata[[2]]
#cut out relevant columns:
mins<-hmdata[,6:(dim(hmdata)[2]-3)]

smpl<-hmdata$Sample
area<-hmdata$area
type<-hmdata$type

#select only most relevant minerals:
mins<-mins[,grep("Zircon|Tourmaline|Rutile|Titanite|Apatite|Epidote|Garnet|Amphibole|Cpx",names(mins))]
#select only certain areas:
ars<-c("Yellow River|Tarim|Beiguyuan|Lingtai|Luochan|Jingbian|Cret sst")
mins<-mins[grep(ars,area),]

# convert to a genuine composition:
comp <- acomp(mins)

#MDS-map that:
X <- clr(comp)        # central logratio transformation
# calculate the Euclidean distance between the clr coordinates
clrdist = dist(X, method = "euclidean", diag = TRUE, upper = TRUE, p = 2)

# MULTIDIMENSIONAL SCALING
MDS = cmdscale(clrdist)
#add further columns for colour- and symbol-scales
MDS<-as.data.frame(MDS)
MDS$site<-area[grep(ars,area)]
MDS$type<-type[grep(ars,area)]
MDS$site[MDS$type!="Loess"]<-"other areas"

g<-plotMDS(MDS,clrdist,labels=FALSE,col="type",sym="site")
ggsave("HM_MDS.pdf",plot=g,width=12,height=8)

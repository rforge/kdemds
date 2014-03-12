#########################################
### project info for R-Forge          ###

# A web site at kdemds.r-forge.r-project.org
# A CVS Repository root of /cvsroot/kdemds at kdemds-forge.r-project.org
# Shell access to kdemds.r-forge.r-project.org
# Search engines throughout the site

#########################################

#########################################
### load test data                    ###
#########################################

alldata<-read.csv("/media/data/martin/LOESS/data/Tb_raw/Tb_all.csv",stringsAsFactors=FALSE)
alldata<-as.list(alldata)
for(i in 1:length(alldata)){alldata[[i]]<-alldata[[i]][!is.na(alldata[[i]])]}

########
# geol. periods for orientation ... test:

tsdf<-data.frame(stringsAsFactors=FALSE,
	bdry=c(0.0,2.59,23.0,66.0,145,201.3,252.2,298.9,358.9,419.2,443.8,485.4,541.0,2500,4000),
	name=c("Quaternary","Neogene","Paleogene","Cretaceous","Jurassic","Triassic",
				 "Permian","Carboniferous","Devonian","Silurian","Ordovician","Cambrian","Proterozoic","Archean","Hadean"),
	short=c("Q","N","Pg","K","J","Tr","P","Ca","D","S","O","Cm","Pz","A","pA"))

h<-ggplot(data=alldata,aes(x=age))+geom_density(aes(x=age,y=..count..,fill=smpl),adjust=1/3,position="stack")


##### MDS map, testing...

require(reshape2)
bins<-seq(cutoffx[1],cutoffx[2],by=10)
alldata$bin<-findInterval(alldata$age,bins)

ad<-acast(alldata[c("age","smpl")],smpl ~ age,mean,na.rm=TRUE)


#########
###### bootstrapping data point for error estimation:
library(MASS)
library(ggplot2)
source('/media/data/martin/LOESS/data/ranalyses/PCA_MDS.r')

tcast<-read.csv("/media/data/martin/LOESS/data/Tb_raw/Tb_all.csv",header=TRUE,stringsAsFactors=FALSE)
tcast<-as.list(tcast)
for(i in 1:length(tcast)){tcast[[i]]<-tcast[[i]][!is.na(tcast[[i]])]}

csmpl<-sample(tcast,3)
curname<-names(csmpl)

reps<-lapply(csmpl,FUN=function(x){return(replicate(n=100,expr=sample(x,floor(0.7*length(x)))))})
reps<-lapply(reps,as.data.frame)
reps<-unlist(reps,recursive=FALSE)

#add main data points
reps<-c(reps,tcast)

diss = dissimilarity(reps)
#plotmapMDS(isoMDS(as.matrix(diss))$points,diss)

locs<-as.data.frame(isoMDS(as.matrix(diss))$points)
names(locs)<-c("x","y")
locs$smpl<-sub("[.].*","",row.names(locs))
ggplot(data=locs[1:100,])+geom_point(aes(x=x,y=y))+stat_density2d(aes(x=x,y=y))+geom_point(data=locs[101:length(locs[[1]]),],aes(x=x,y=y,colour=smpl))

########
#error estimation of all data:
allscatter<-data.frame(x=NULL,y=NULL,smpl=NULL)
nreps<-100
#for(i in 1:length(tcast)){
for(i in 1:3){
	csmpl<-tcast[i]
	curname<-names(csmpl)
	message("subsampling ",curname)
	
	reps<-lapply(csmpl,FUN=function(x){return(replicate(n=nreps,expr=sample(x,floor(0.7*length(x)))))})
	reps<-lapply(reps,as.data.frame)
	reps<-unlist(reps,recursive=FALSE)
	reps<-c(reps,tcast)
	
	diss = dissimilarity(reps)
	locs<-as.data.frame(isoMDS(as.matrix(diss))$points)
	names(locs)<-c("x","y")
	locs$smpl<-sub("[.].*","",row.names(locs))
	allscatter<-rbind(allscatter,locs)
}

diss<-dissimilarity(tcast)
locs<-as.data.frame(isoMDS(as.matrix(diss))$points)
names(locs)<-c("x","y")
locs$smpl<-row.names(locs)

ggplot(data=allscatter)+geom_point(aes(x=x,y=y,colour=smpl))+geom_text(data=locs,aes(x=x,y=y,label=smpl),size=6)



#########
# calc dissimilarity with a new metric
# try: cvmts.test()
# try: difference between ecdfs

require(reshape2)
require(MASS)

ids<-seq(0,3500,10)
### area between ecdfs
cs<-list()
for(i in seq_along(tcast)){cs[[i]]<-ecdf(sort(tcast[[i]]))}
#diss<-outer(seq_along(cs),seq_along(cs),function(x,y){return(sum(cs[[y]](ids)-cs[[x]](ids)))})
diss<-matrix(0,nrow=length(cs),ncol=length(cs))
for(i in seq_along(cs)){
	for(j in seq_along(cs)){
		diss[i,j]<-abs(sum(cs[[j]](ids)-cs[[i]](ids)))
	}
}
row.names(diss)<-names(tcast)
# diss<-diss / max(diss)

### other approach:
#calculate kde for each data set:
require(kdemds)
cds<-lapply(data,kde,n=2^12)
cds<-lapply(cds,FUN=function(x){df<-data.frame(t(x));names(df)<-c("x","d");return(df)})
#don't: normalise to [0,1]:
#cds<-lapply(cds,FUN=function(x){return(within(x,d<-d/max(d,na.rm=TRUE)))})
limits<-range(sapply(cds,FUN=function(x){return(x$x)}))

evs<-seq(0,ifelse(limits[2]>4000,4000,limits[2]),by=1)
cds<-lapply(cds,function(x){return(approxfun(x[,1],x[,2],yleft=0,yright=0))})

ddf<-data.frame(age=evs)
ddf<-cbind(ddf,lapply(cds,function(x){return(x(evs))}))
require(reshape2)
mdf<-melt(ddf,id.vars="age",variable.name="sample",value.name="cdf")

#quick plot:
require(ggplot2)
qplot(data=mdf,x=age,y=cdf,colour=sample,geom="line")

#calculate "dissimilarity"(?) as approximate area between ecdfs
diss<-matrix(0,length(data),length(data))
for(i in 1:length(data)){
	for(j in 1:length(data)){
		diss[i,j]<-sum(abs(sapply(evs,function(x,i,j){return(cds[[i]](x)-cds[[j]](x))},i,j)))
	}
}
#not necessary: normalise to max. possible difference=length(evs)
#diss<-abs(diss/length(evs))
row.names(diss)<-names(data)

require(MASS)
mds<-isoMDS(as.matrix(diss))$points
mds<-as.data.frame(mds)
names(mds)<-c("x","y")

mds$area<-row.names(mds)

mds$area[row.names(mds) %in% c("Tb54","Tb57","Tb58","Tb60","Tb62")]<-"Junggar"
mds$area[row.names(mds) %in% c("Tb05","Tb07","Tb11b","Tb20","Tb21","Tb22","Tb27","Tb29","Tb31","Tb04","Tb28","Tb26")]<-"Tarim basin"
mds$area[row.names(mds) %in% c("Tb02","Tb01","Tb48","Tb50","Tb52")]<-"Tienshan"
mds$area[row.names(mds) %in% c("Tb09","Tb15","Tb12","Tb19","Tb35")]<-"Kunlun"
mds$area[row.names(mds) %in% c("Tb43","Tb38","Tb39")]<-"Karakoram"

plotMDS(mds,diss,col="area")


###
ad<-list()
for(n in unique(data$smpl)){
	ad[[n]]<-stepfun(x=data$x[data$smpl==n],y=c(0,data$d[data$smpl==n]))
	#ad[[n]]<-ecdf(x=c(0,data$d[data$smpl==n]))
}

diss<-outer(ad,ids,function(x,y){return(x(y))})
diss<-matrix(0,nrow=length(ad),ncol=length(ad))
for(i in seq_along(ad)){
	for(j in seq_along(ad)){
		diss[i,j]<-sqrt(sum((ad[[j]](ids)^2)-(ad[[i]](ids)^2)))
	}
}
row.names(diss)<-unique(data$smpl)
diss<-diss / max(diss[!is.na(diss)])

###
locs<-as.data.frame(isoMDS(as.matrix(diss))$points)
names(locs)<-c("x","y")
locs$area<-row.names(locs)
getMDSplot(locs,diss)





#########
# mixture modelling?
library(reshape2)
library(ggplot2)
library(mclust)
source('/media/data/martin/LOESS/data/ranalyses/tarim_paper/kde.r')

#synthetic data:

#real data:
tcast<-read.csv("/media/data/martin/LOESS/data/Tb_raw/Tb_all.csv",header=TRUE,stringsAsFactors=FALSE)
tcast<-as.list(tcast)
for(i in 1:length(tcast)){tcast[[i]]<-tcast[[i]][!is.na(tcast[[i]])]}
csmpl<-sample(tcast,1)
curname<-names(csmpl)

#analysis:
ages<-seq(0,3000,10)
tmc<-Mclust(csmpl[[1]])
whatsit<-sapply(c(1:tmc$G),FUN=function(x){return(dnorm(mean=tmc$parameters$mean[x],sd=sqrt(tmc$parameters$variance$sigmasq[x]),x=ages))})
whatsit<-sapply(c(1:tmc$G),FUN=function(x){return(tmc$parameters$pro[x]*whatsit[,x]/max(whatsit[,x],na.rm=TRUE))})
whatsit<-data.frame(whatsit)
whatsit$age<-ages
whatsthat<-melt(whatsit,id.vars="age",variable.name="component",value.name="density")

d<-kde(data=csmpl[[1]][!is.na(csmpl[[1]])],n=2^14)
dd<-data.frame(t(d))
names(dd)<-c("x","d")
dd<-dd[dd$d>0,]
dd$d<-dd$d/max(dd$d,na.rm=TRUE)	#normalise density values

ggplot(data=whatsthat)+geom_line(aes(x=age,y=density,colour=component))+geom_line(data=dd,aes(x=x,y=d))


########


#######################################################
## test Sircombe & Hazelton way of comparing KDEs:

require(ggplot2)
source('/media/data/coding/R/KDE_MDS/noninst/Sircombe_Hazelton.r')

#load alldata from gsa_plots.r....

ev<-c(0:max(alldata$age))

sig2.con(alldata$age[alldata$smpl=="Tb22"],alldata$ageerr[alldata$smpl=="Tb22"])
ttt<-adapt.f(alldata$age[alldata$smpl=="Tb22"],519,ev)

cs<-sapply(unique(alldata$smpl),FUN=function(x){return(sig2.con(alldata$age[alldata$smpl==x],alldata$ageerr[alldata$smpl==x]))})
#calculated bandwidths:
sqrt(cs)

Run.c2<-max(cs)

#calculate (dis?)similarity matrix:
smpls<-unique(alldata$smpl)
nsmpl<-length(smpls)
diss<-matrix(0,nsmpl,nsmpl)
for(i in 1:nsmpl){
	for(j in 1:nsmpl){
		diss[i,j]<-dXY(alldata$age[alldata$smpl==smpls[i]],alldata$ageerr[alldata$smpl==smpls[i]],alldata$age[alldata$smpl==smpls[j]],alldata$ageerr[alldata$smpl==smpls[j]],Run.c2)*1000
	}
}
row.names(diss)<-smpls

require(MASS)
mds<-isoMDS(as.matrix(diss))$points
mds<-as.data.frame(mds)
names(mds)<-c("x","y")

mds$area<-row.names(mds)

mds$area[row.names(mds) %in% c("Tb54","Tb57","Tb58","Tb60","Tb62")]<-"Junggar"
mds$area[row.names(mds) %in% c("Tb05","Tb07","Tb11b","Tb20","Tb21","Tb22","Tb27","Tb29","Tb31","Tb04","Tb28","Tb26")]<-"Tarim basin"
mds$area[row.names(mds) %in% c("Tb02","Tb01","Tb48","Tb49","Tb50","Tb52")]<-"Tienshan"
mds$area[row.names(mds) %in% c("Tb09","Tb15","Tb12","Tb19","Tb35","Tb34")]<-"Kunlun"
mds$area[row.names(mds) %in% c("Tb41","Tb43","Tb38","Tb39")]<-"Karakoram"

plotMDS(mds,diss,col="area")


########
ggplot()+geom_point(data=alldata,aes(x=age,y=ageerr),colour="#00000022")

# #######################################################

###########################################################
# comparing old laser and new laser data #
###########################################################

require(kdemds)

#fls<-list.files(path="/media/data/martin/LOESS/data/Tb_raw/",pattern="Tb.*_preferred.csv",full.names=TRUE)
fls<-list.files(path="/media/ldata/martin/LOESS/data/Tb_raw/",pattern="Tb.*_preferred.csv",full.names=TRUE)

#read all data:
alldata<-list()
for(f in fls){
	spl<-sub("_preferred.csv","",basename(f))
	cdat<-read.table(file=f,header=TRUE,sep=",",stringsAsFactors=FALSE,skip=6)
	for(i in 2:length(cdat))cdat[[i]]<-as.numeric(cdat[[i]])
	alldata[[spl]]<-cdat
}

#collate all in one data.frame()
coll<-data.frame(stringsAsFactors=FALSE)
for(i in 1:length(alldata)){
	cdat<-alldata[[i]]
	cdat$spl<-names(alldata)[i]
	if(i==1){
		coll<-cdat
	}else{
		coll<-rbind(coll,cdat)
	}
}

#play:
require(ggplot2)

ggplot(data=coll)+geom_point(aes(x=preferred.age,y=X1.sigma.age))

ggplot(data=coll[!is.na(coll$preferred.age),])+geom_point(aes(x=Pb206.U238,y=Pb207.Pb206,colour=preferred.age))

ggplot(data=coll[])+geom_point(aes(x=age.206.238,y=age.207.206,colour=preferred.age))

ggplot(data=coll[!is.na(coll$preferred.age),])+geom_point(aes(x=ppm.U,y=ppm.Pb,colour=preferred.age))

ggplot(data=coll[])+geom_point(aes(colour=ppm.Pb/ppm.U,x=age.206.238,y=X1s.age68))+xlim(0,4000)+ylim(0,200)

ggplot(data=coll[])+geom_point(aes(colour=ppm.Pb/ppm.U,x=age.207.206,y=X1s.age76))+xlim(0,4000)+ylim(0,200)

ggplot(data=coll[])+geom_point(aes(colour=ppm.Pb/ppm.U,x=age.207.235,y=X1s.age75))+xlim(0,4000)+ylim(0,200)

ggplot(data=coll[])+geom_point(aes(x=X.discord.68.76,y=X.discord.68.75,colour=atomic.Th.U))+xlim(0,100)+ylim(0,100)


tdt<-coll[,c(2,3,5,7,9,19)]
tdt$smpl<-coll$spl

ggplot(data=tdt[!is.na(tdt$preferred.age),])+geom_point(aes(x=preferred.age,y=smpl,colour=ppm.U),position="jitter")


comps<-names(alldata)[grep("Tb22.*|Tb39.*|Tb02.*",names(alldata))]
comp<-list()
for(s in comps){
	comp[[s]]<-alldata[[s]]$preferred.age
	comp[[s]]<-comp[[s]][!is.na(comp[[s]])]
}

plotKDE(comp)

#########################################
#test plotting

plotKDE(alldata,plot="Tb22")
plotKDE(alldata,plot="Tb22",method="R")
plotKDE(alldata,plot="Tb22",method="R",kernel="tri")
plotKDE(alldata,method="R")
plotKDE(alldata,method="R",logx=TRUE)
plotKDE(alldata,method="R",logx=TRUE,kernel="epan",limits=c(80,2800),bandwidth=0.01,fcolour="black")
plotKDE(alldata,plot="Tb22",method="abr")
plotKDE(alldata,plot="Tb22",method="abr",bandwidth=20)
plotKDE(alldata,plot="Tb22",method="abr",bandwidth=0.04,logx=TRUE)

#test kernel functions:
tdf<-data.frame(t(pkde(data=alldata[["Tb22"]],n=2^12)))
tdf<-data.frame(t(skde(data=alldata[["Tb22"]],n=2^12)))
tdf<-data.frame(t(jkde(data=alldata[["Tb22"]],n=2^12)))

names(tdf)<-c("x","d")
qplot(data=tdf,x=x,y=d,geom="line")


#"optimal bandwidth" following Jann 2007:
dat<-alldata[["Tb22"]]
sigd<-min(sd(dat),IQR(dat))	
delk<-(1/(4*pi))^(1/10)
hs<-1.159*delk*sigd*length(dat)^(-1/5)


################################################################################
### GUI testing
### using gWidgets with tcl/tk (to keep best Mac compatibility)
### tcl/tk should exist on Mac, possible on Linux and Windows
### Gtk should exist on Linux, possible on Windows, for Mac: http://r.research.att.com/

library(gWidgets2)
options(guiToolkit="RGtk2")                     # avoid question if more than one is installed
w <- gwindow("Hello world example")             # top level window
g <- ggroup(cont=w, horizontal=FALSE)           # a box container, added to w
b <- gbutton("Click me for a message", cont=g)  # add button to container g
addHandlerClicked(b, handler=function(h,...) {  # add interactivity through a handler
	galert("Hello world", parent=h$obj)
})
gconfirm("Are we having fun?")

###
require(gWidgets2)
options(guiToolkit="RGtk2")
#options(guiToolkit="tcltk")

filename=gfile(type="open",multi=FALSE,filter=list("Text files"=list(patterns=c("*.csv","*.tab","*.txt")),
		"Excel files"=list(patterns=c("*.xls")),"All files"=list(patterns=c("*"))))

win <- gwindow("KDE Plotter")
g <- ggroup(horizontal=FALSE, cont=win)


###
gwtkdensity <- function() {
	## set up
	availDists <- c(Normal = "rnorm", Exponential="rexp")
	availKernels <- c("gaussian", "epanechnikov", "rectangular",
										"triangular", "biweight", "cosine", "optcosine")
	
	
	updatePlot <- function(h,...) {
		x <- do.call(availDists[svalue(distribution)],list(svalue(sampleSize)))
		plot(density(x, adjust = svalue(bandwidthAdjust), kernel = svalue(kernel)))
		rug(x)
	}
	
	##The widgets
	win <- gwindow("gwtkdensity")
	gp <- ggroup(horizontal=FALSE, cont=win)
	
	tmp <- gframe("Distribution", container=gp, expand=TRUE)
	distribution <- gradio(names(availDists), horizontal=FALSE,
												 cont=tmp,
												 handler=updatePlot)
	
	
	tmp <- gframe("Sample size", container=gp, expand=TRUE)
	sampleSize <- gradio(c(50,100,200, 300), cont=tmp,
											 handler =updatePlot)
	
	
	tmp <- gframe("Kernel", container=gp, expand=TRUE)
	kernel <- gcombobox(availKernels, cont=tmp,
											handler=updatePlot)
	
	tmp <- gframe("Bandwidth adjust", container=gp, expand=TRUE)
	bandwidthAdjust <- gslider(from=0,to=2,by=.01, value=1,
														 cont=tmp, expand=TRUE,
														 handler=updatePlot)
	
}

###
w <- gwindow("Two widgets")
g <- ggroup(container = w)
widget1 <- gbutton("Click me to update the counter", container=g,
									 handler = function(h,...) {
									 	oldVal <- svalue(widget2)
									 	svalue(widget2) <- as.numeric(oldVal) + 1
									 })
widget2 <- glabel(0, container=g)

### test returning values
w <- gwindow("return value")
g <- ggroup(container = w,horizontal=FALSE)
widget1 <- gbutton("do a calculation", container=g,
									 handler = function(h,...) {
									 	res<-eval(call("+",2,3))
									 	.GlobalEnv$p<-res
									 	gmessage(sprintf("result: %s",res))
									 	return(res)
									 })

tunnel<-function(h,...){
	return(h$action)
}
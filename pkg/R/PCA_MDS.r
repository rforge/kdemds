# ### PCA ####################
# # define a function to plot the configuration and connect the nearest neighbours
# plotmapPCA <- function(conf,diss) {
# 	# rank the samples according to their pairwise proximity
# 	i = t(apply(as.matrix(diss),1,function(x) order(x))[2:3,])
# 	# plot coordinates for the lines
# 	x1 = as.vector(conf[i[,1],1])
# 	y1 = as.vector(conf[i[,1],2])
# 	x2 = as.vector(conf[i[,2],1])
# 	y2 = as.vector(conf[i[,2],2])
# 	# create a new (empty) plot
# 	plot(conf[,1],conf[,2],type='n')
# 	# draw lines between closest neighbours
# 	for (j in 1:nrow(conf)) {
# 		lines(c(conf[j,1],x1[j]),c(conf[j,2],y1[j]),lty=1)
# 		lines(c(conf[j,1],x2[j]),c(conf[j,2],y2[j]),lty=2)
# 	}
# 	# plot the configuration as labeled circles
# 	points(conf[,1],conf[,2],pch=21,cex=2.5,col='red',bg='white')
# 	text(conf[,1],conf[,2],row.names(conf))
# }

### MDS ################
# define a function to calculate the dissimilarity matrix
dissimilarity <- function(data) {
	# TODO: make method for calculation chooseable, allow for Sircombe & Hazelton, 2004 - and others?
	# TODO: calculate based on log-transformed ages?
	n = length(data)
	# instantiate the dissimilarity matrix as an empty data frame
	diss = as.data.frame(mat.or.vec(n,n))
	rownames(diss) = names(data)
	for (i in 1:n){   # loop through all possible pairs of samples
		for (j in 1:n){  # calculate the kolmogorov-smirnov statistic
			diss[i,j] = ks.test(data[[i]],data[[j]])$statistic
		}
	}
	return(diss)
}

# # define a function to plot the MDS coordinates and connect the nearest neighbours
# plotmapMDS <- function(mds,diss) {
# 	# indices of nearest and second nearest neighbours:
# 	i = t(apply(as.matrix(diss),1,function(x) order(x))[2:3,])
# 	# plot coordinates for the lines
# 	x1 = as.vector(mds[i[,1],1])
# 	y1 = as.vector(mds[i[,1],2])
# 	x2 = as.vector(mds[i[,2],1])
# 	y2 = as.vector(mds[i[,2],2])
# 	# create a new (empty) plot
# 	plot(mds[,1],mds[,2],type='n')
# 	# draw lines between closest neighbours
# 	for (j in 1:nrow(mds)) {
# 		lines(c(mds[j,1],x1[j]),c(mds[j,2],y1[j]),lty=1)
# 		lines(c(mds[j,1],x2[j]),c(mds[j,2],y2[j]),lty=2)
# 	}
# 	# plot the configuration as labeled circles
# 	points(mds[,1],mds[,2],pch=21,cex=2.5,col='red',bg='white')
# 	text(mds[,1],mds[,2],row.names(mds))
# }

# define a function to plot the MDS coordinates and connect the nearest neighbours, ggplot2-style
plotMDS <- function(mds,diss,col="",sym="",nearest=TRUE,labels=TRUE,symbols=TRUE,fcolour=NA) {
	#quick and dirty, makes big assumptions about what mds looks like if it's not a two-column matrix.
	#mds can be: data.frame(x,y,any,further,columns,...)
	#
	# mds     ... data.frame() or matrix with at least 2 columns representing x and y
	#              coordinates calculated by cmdscale() or isoMDS()
	# diss    ... (dis-)similarities of samples
	# col     ... string giving the name of column in mds to be used for colour scale
	# sym     ... string giving the name of column in mds to be used for symbol scale
	# nearest ... boolean - plot lines connecting nearest neighbours?
	# labels  ... boolean - plot data labels (taken from row.names of mds)?
	# symbols ... boolean - plot data points (useful if plotting only labels)?
  # fcolour ... symbol fill colours - not very functional yet...
	
	require(ggplot2)
	require(scales)
	defcol<-"blue"
	ddf<-as.data.frame(mds)
	if(!all(c("x","y") %in% names(ddf)))names(ddf)[1:2]<-c("x","y")
	# indices of nearest and second nearest neighbours:
	i = t(apply(as.matrix(diss),1,function(x) order(x))[2:3,])
	# plot coordinates for the lines
	x1 = as.vector(ddf[i[,1],"x"])
	y1 = as.vector(ddf[i[,1],"y"])
	x2 = as.vector(ddf[i[,2],"x"])
	y2 = as.vector(ddf[i[,2],"y"])
	# create a new (empty) plot
#	plot(mds[,1],mds[,2],type='n')
	p<-ggplot()
	# draw lines between closest neighbours
#	for (j in 1:nrow(mds)) {
#		lines(c(mds[j,1],x1[j]),c(mds[j,2],y1[j]),lty=1)
#		lines(c(mds[j,1],x2[j]),c(mds[j,2],y2[j]),lty=2)
#	}
	if(nearest){
		p<-p+geom_segment(data=ddf,x=ddf[["x"]],y=ddf[["y"]],xend=x1,yend=y1,linetype="solid")
		p<-p+geom_segment(data=ddf,x=ddf[["x"]],y=ddf[["y"]],xend=x2,yend=y2,linetype="dashed")
	}
	# plot the configuration as labeled circles
#	points(mds[,1],mds[,2],pch=21,cex=2.5,col='red',bg='white')
	# TODO: a lot of checks on col and sym, sensible automatic assumptions
	if(symbols){
		if(length(ddf)>2){
			if(col==""){
				if(sym==""){
					p<-p+geom_point(data=ddf,aes_string(x="x",y="y"),fill=defcol,colour="black",size=7,shape=21)
				}else{
					p<-p+geom_point(data=ddf,aes_string(x="x",y="y",shape=sym),fill=defcol,colour="black",size=7)
				}					
			}else{
				if(sym==""){
					p<-p+geom_point(data=ddf,aes_string(x="x",y="y",fill=col),colour="black",size=7,shape=21)
				}else{
					p<-p+geom_point(data=ddf,aes_string(x="x",y="y",fill=col,shape=sym),colour="black",size=7)
				}	
			}
		}else{
			c2=defcol
			if(col!=""){
				if(length(col)>1){
					warning("more than one colour value specified - using default")
					c2<-defcol
				}else{
					# TODO: check if col IS a colour value
					c2<-col
				}
			}
			p<-p+geom_point(data=ddf,aes(x=x,y=y),fill=c2,colour="black",size=7,shape=21)				
		}
	}
#	text(mds[,1],mds[,2],row.names(mds))
	#add labels:
	if(labels)p<-p+geom_text(data=ddf,aes(x=x,y=y),label=row.names(ddf))
	
	#apply colour scale, symbol scale, format plot:

	cols<-c(rgb(t(col2rgb(defcol))/255))  #default colour
	#print(cols)
  #browser()
	if(col!=""){
    if(length(fcolour)==0||is.na(fcolour)){
      cols<-brewer_pal(type="div",palette=2)(length(unique(ddf[[col]])))
    }else if(length(fcolour)==1){
      #nothing to do
    }else if(length(fcolour)==length(unique(ddf[[col]]))){
      ucols<-fcolour
      names(ucols)<-unique(ddf[[col]])
      cols<-ucols[match(sort(unique(ddf[[col]])),names(ucols))]
      # FIXME: allow reordering here...
    }else{
      warning("invalid fill colour parameter - using default")
      #...to be precise: "leaving" default....
    }
	}
	#print(cols)
	p<-p+scale_fill_manual(values=cols)
	if(sym!=""){
		bks<-unique(ddf[[sym]])	# TODO: warn if too many, define new ones?
		p<-p+scale_shape_manual(values=rep(c(21,22,24,23),length.out=10)[1:length(bks)])	
	}
	p<-p+theme(axis.title.x=element_blank(),axis.title.y=element_blank())+
		theme(panel.grid.minor=element_blank(),panel.grid.major=element_blank())+
		theme(panel.background=element_rect(fill=NA,colour="black"),plot.background=element_rect(fill=NA,colour=NA))
	if(col!="")p<-p+guides(fill=guide_legend(title=NULL,order=2,override.aes=list(fill=cols,shape=21)),shape=guide_legend(title=NULL,order=1))
	return(p)
}

plotShepard<-function(mds,diss,xlab="dissimilarity",ylab="distance",title=""){
	#create a Shepard plot to evaluate MDS quality
	# mds ... data.frame() or 2-column matrix containing x and y coordinates
	# dis ... distance- or dissimarity matrix
	# xlab, ylab ... x- and y-axis labels
	# title ... plot title
	
	require(MASS)
	shp<-Shepard(as.dist(diss),as.matrix(mds[,c("x","y")]))
	shp<-as.data.frame(shp)
	#calculate stress factor:
	stress<-100*sqrt(sum((shp$yf-shp$y)^2)/sum(shp$y^2))
	#create plot:
	p<-ggplot(data=shp)+
		geom_point(aes(x=x,y=y),size=2)+
		geom_step(aes(x=x,y=yf),size=1,colour="red")+
		geom_text(x=max(shp$x),y=min(shp$y),label=sprintf("stress: %.1f %%",stress),size=8,hjust=1,vjust=0)+
		xlab(xlab)+ylab(ylab)
	if(title!="")p<-p+geom_text(x=min(shp$x),y=max(shp$y),label=title,size=8,hjust=0,vjust=1)
	
	return(p)
}

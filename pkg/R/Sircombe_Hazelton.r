# modified from:
# Sircombe, K.N. & Hazelton, M.L., 2004.
# Comparison of detrital zircon age distributions by kernel functional estimation.
# Sedimentary Geology, 171(1), pp.91–111.
#
# Martin Rittner, 2013

############################################################
## Density estimator with sample-point adaptive bandwidth ##
############################################################

adapt.f<-function(x,h,eval,kernel="gauss"){
	#returns the densities of a KDE for input data x, over positions eval, with a kernel bandwidth h 
	f<-eval*0
	for(i in 1:length(eval)){
		f[i]<-mean(dnorm(x-eval[i],0,sd=h))
	}
	return(f)
}

sig2.con<-function(x,sigx,UCV=TRUE){
	sig2max<-max(sigx^2)
	h<-1.06*min(sd(x),IQR(x)/1.34)/length(x)^0.2
	if(UCV) h<-bw.ucv(x,lower=h/20,upper=h)
	#if(UCV) h<-bw.ucv(x)
	return(sig2max+h^2)
}

######################
## Permutation Test ##
######################

## Calculate integrated sqared KDEs ##

Rf<-function(x,sig2,c.con){
	h1<-sqrt(c(outer(c.con-sig2,c.con-sig2,"+")))
	xdiff<-c(outer(x,x,"-"))
	rf<-mean(dnorm(xdiff,sd=h1))
	return(rf)
}

## L2 distance beween KDEs ##

dXY<-function(x,sig2x,y,sig2y,c.con=0){
	if(c.con<=0)c.con<-max(sig2.con(x,sig2x),sig2.con(y,sig2y))
	rfx<-Rf(x,sig2x,c.con)
	rfy<-Rf(y,sig2y,c.con)
	h1<-sqrt(c(outer(c.con-sig2x,c.con-sig2y,"+")))
	XYdiff<-c(outer(x,y,"-"))
	Itmp<-mean(dnorm(XYdiff,sd=h1))
	dxy<-rfx+rfy-2*Itmp
	dxy<-sqrt(dxy)
	return(dxy)
}

## Permutation Test for Equality of KDEs ##

Perm.Test<-function(x,y,sigx,sigy,c.con=0,REP=1000){
	nx<-length(x)
	ny<-length(y)
	n<-nx+ny
	xandy<-c(x,y)
	allsig<-c(sigx,sigy)
	if(c.con<=0)c.con<-max(sig2.con(x,sigx),sig2.con(y,sigy))
	dxy<-dXY(x,sigx^2,y,sigy^2,c.con)
	P<-0
	
	for(i in 1:REP){
		perm.index<-sample(1:n)
		x.index<-perm.index[1:nx]
		y.index<-perm.index[(nx+1):n]
		X<-xandy[x.index]
		Y<-xandy[y.index]
		sigX<-allsig[x.index]
		sigY<-allsig[y.index]
		dxy.2<-dXY(X,sigX^2,Y,sigY^2,max(sig2.con(X,sigX),sig2.con(Y,sigY)))
		P<-P+(dxy.2>dxy)
	}
	P<-P/REP
	return(P)
}

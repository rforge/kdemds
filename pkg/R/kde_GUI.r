kdeGUI<-function(toolkit="tcltk"){
	require(gWidgets2)
	#require(gWidgets2RGtk2)
	#options(guiToolkit="RGtk2")
	require(gWidgets2tcltk)
	options(guiToolkit=toolkit)
	require(kdemds)
	
	# TODO: add "data" input field - if no file is chosen, can enter name of a given variable
	# TODO: make data selection and classes editable lists
	# TODO: add colour chooser dialog for colour, and make editable list for classes
	# TODO: short data summary
	# TODO: let user choose a folder, read all enclosed files (with filter?)
	# TODO: autorange should round values (take logx into account), take "toplot" data selection into account
	# TODO: better handling of different toolkits, automatic loading of appropriate packages...
	
  #tcl/tk seems to work on most platforms
  # windows: install R, RStudio, Strawberry Perl and Tcl/Tk before installing required R packages
  # Mac: R, RStudio, packages - check http://r.research.att.com/
  # Linux: R, RStudio, packages
  
  
	markeroptions<-c("none","dash","circle")
	kdeoptions<-c("botev","R","jann","abramson")
	
	#environment to hold data and info
	.dataEnv<-new.env()
	
	###### handlers:
	#handler function called when "load file" is pressed:
	load_file<-function(h,...){
		#choose data file:
		filename=gfile(type="open",multi=FALSE,text="Select data file",
									 filter=list("Text files"=list(patterns=c("*.csv","*.tab","*.txt")),
									 						"Excel files"=list(patterns=c("*.xls")),"All files"=list(patterns=c("*"))))
		if(length(filename)==0)return()
		
		### read data:
		data<-list()
		#file extension:
		ext<-unlist(strsplit(basename(filename),"\\."))[2]
		if(ext %in% c("csv","tab","txt")){
			#data file is text-based
			message(sprintf("reading %s data file - assumed flat data table",ext))
			data<-read.csv(filename,stringsAsFactors=FALSE)	
			data<-as.list(data)
			for(i in 1:length(data)){data[[i]]<-data[[i]][!is.na(data[[i]])]}
		}else if(ext=="xls"){
			#data file is Excel format
			svalue(statbar)<-"reading Excel file - this can take a while, please stand by..."
			require(gdata)
			age_column="Best.Age"
			sheets<-sheetNames(filename)
			for(s in sheets){
				current<-read.xls(filename,sheet=s,skip=1,fileEncoding="latin1")
				data[[s]]<-current[[age_column]]
			}
		}else{
			svalue(statbar)<-sprintf("unknown data file format: %s\ncould not load data...",ext)
			return(FALSE)
		}
		assign("thedata",data,envir=.dataEnv)
		assign("dataname",filename,envir=.dataEnv)
		svalue(toplot)<-paste0(names(data),collapse=",")
		stat<-sprintf("loaded %s\n%d data series\n%d data (total)",filename,length(data),sum(sapply(data,length)))
		svalue(statdisplay)<-stat
		svalue(statbar)<-sprintf("Ready.")
		enabled(btn_plot)<-TRUE
		enabled(pf)<-TRUE
		return(TRUE)
	}
	
	#handler function called when "load folder" is pressed:
	load_folder<-function(h,...){
		win<-gwindow("KDE Plotter",visible=FALSE)
		g<-ggroup(horizontal=FALSE, cont=win)
		
	}
	
	#handler function called when "plot" is pressed:
	returnplot<-function(h,...){
		svalue(statbar)<-sprintf("calculating KDEs - please stand by...")
		pcall<-sprintf(paste0('plotKDE(data=%s,title="%s",limits=%s,breaks=%s,bandwidth=%s,\n',
													'fcolour=%s,splitat=%s,\nplot=%s,\nclasses=%s,\n',
													'periods=%s,hist=%s,markers="%s",order=%s,logx=%s,',
													'method="%s"%s)'),
									 "data",svalue(title),sprintf("c(%s)",paste0(svalue(from),",",svalue(to))),
									 "NA",svalue(bw),svalue(fcol),svalue(split),
									 #sprintf("c(%s)",paste0('"',names(thedata),'"',collapse=",")),
									 unlist(strsplit(svalue(toplot),",")),
									 unlist(strsplit(svalue(eclasses),",")),"FALSE",svalue(histo),svalue(markers),
									 FALSE,svalue(logx),svalue(algo),
									 ifelse(svalue(ellips)=="","",paste0(",",svalue(ellips))))
		msg<-sprintf("Plotting:\n%s\n\nfunction call:\n%s",get("dataname",envir=.dataEnv),pcall)
		#for some reason, can't evaluate that string as a function call - make it up manually...:
		tc<-list(plotKDE,data=get("thedata",envir=.dataEnv),title=svalue(title),
						 limits=c(as.numeric(svalue(from)),as.numeric(svalue(to))),breaks=NA,
						 bandwidth=if(svalue(bw)=="NA") NA else as.numeric(svalue(bw)),
						 fcolour=if(svalue(fcol)=="NA") NA else svalue(fcol),splitat=as.numeric(svalue(split)),
						 plot=unlist(strsplit(svalue(toplot),",")),
						 classes=if(svalue(eclasses)=="NA") NA else unlist(strsplit(svalue(eclasses),",")),
						 periods=FALSE,hist=as.logical(svalue(histo)),markers=svalue(markers),
						 order=FALSE,logx=as.logical(svalue(logx)),method=svalue(algo))
		if(svalue(ellips)!=""){
			args<-unlist(strsplit(svalue(ellips),","))
			#strs<-grepl("\"",args)
			args<-gsub("\"","",args)
			for(i in 1:length(args)){
				param<-unlist(strsplit(args[i],"="))
				#tc[param[1]]<-ifelse(strs[i],paste0('"',param[2],'"'),param[2])
				tc[param[1]]<-param[2]
			}
		}
		tc<-as.call(tc)
		p<-eval(tc)
		#.GlobalEnv$p<-p
		svalue(statbar)<-sprintf("plotting...")
		eval(call("print",p),env=.GlobalEnv)
		#dispose(win)
		message(msg)
		svalue(statbar)<-sprintf("Ready.")
	}
	
	###### layout:
	win<-gwindow("KDE Plotter",visible=FALSE)
	g<-ggroup(horizontal=FALSE, cont=win)
	statbar<-gstatusbar(text="",container=win)
	
	df<-gframe(text="Data",container=g)
	font(df) <- list(weight="bold")
	statdisplay<-glabel("no data loaded\n\n",container=df)

	of<-gframe(text="Load data",container=g)
	font(of) <- list(weight="bold")
	addSpring(of)
	op_fil<-gbutton("load file",container=of,handler=load_file)
	addSpring(of)
	op_fol<-gbutton("load folder",container=of)
	addSpring(of)
	op_var<-gbutton("load variable",container=of)
	addSpring(of)
	enabled(op_fol)<-FALSE
	enabled(op_var)<-FALSE
	
	pf<-gframe(text="Parameters",container=g)
	enabled(pf)<-FALSE
	font(pf) <- list(weight="bold")
	tbl<-glayout(container=pf,spacing=4)
	
	tbl[1,1] <- glabel("Plot title:",container = tbl)
	tbl[1,2] <- (title <- gedit("",container=tbl))
	tbl[2,1] <- glabel("Age range:",container = tbl)
	tbl[2,2] <- (ag<-ggroup(horizontal=TRUE,container=tbl))
		from<-gedit("0",container=ag,width=6)
		#add(ag,glabel(" to "))
		glabel(" to ",container=ag)
		to<-gedit("3000",container=ag,width=6)
		autorange<-gcheckbox(text="auto range",checked=FALSE,container=ag,handler=function(h,...){
			if(svalue(h$obj)){
				svalue(from)<-0
				svalue(to)<-max(unlist(get("thedata",envir=.dataEnv)),na.rm=TRUE)
				enabled(from)<-FALSE
				enabled(to)<-FALSE
			}else{
				enabled(from)<-TRUE
				enabled(to)<-TRUE
			}
		})
	tbl[3,1] <- glabel("Bandwidth:",container = tbl)
	tbl[3,2] <- (bw <- gedit("NA",container=tbl,width=12))
	tbl[4,1] <- glabel("Fill colour:",container = tbl)
	tbl[4,2] <- (fcol <- gedit("NA",container=tbl,width=12))
	tbl[5,1] <- glabel("Split ages at:",container = tbl)
	tbl[5,2] <- (split <- gedit("NA",container=tbl,width=12))
	tbl[6,1] <- glabel("Data to plot:",container = tbl)
	tbl[6,2] <- (pg<-ggroup(horizontal=TRUE,container=tbl))
		toplot <- gedit("NA",container=pg,width=30)
		btn_rst_plot<-gbutton("reset",container=pg,handler=function(h,...){
			svalue(toplot)<-paste0(names(get("thedata",envir=.dataEnv)),collapse=",")
		})
	tbl[7,1] <- glabel("Data classes:",container = tbl)
	tbl[7,2] <- (cg<-ggroup(horizontal=TRUE,container=tbl))
		eclasses <- gedit("NA",container=cg,width=30)
		btn_rst_class<-gbutton("reset",container=cg,handler=function(h,...){
			svalue(eclasses)<-"NA"
		})	
	tbl[8,1] <- glabel("Data markers:",container = tbl)
	tbl[8,2] <- (markers <- gcombobox(markeroptions,editable=FALSE,container=tbl))
	tbl[9,2] <- (histo<-gcheckbox(text="overlay histogram",checked=FALSE,container=tbl))
	#tbl[10,2] <- (order<-gcheckbox(text="sort plots by name/classes",checked=TRUE,container=tbl))
	tbl[11,2] <- (logx<-gcheckbox(text="logarithmic age scale",checked=FALSE,container=tbl))
	tbl[12,1] <- glabel("KDE calculation:",container = tbl)
	tbl[12,2] <- (algo <- gcombobox(kdeoptions,editable=FALSE,container=tbl))
	tbl[13,1] <- glabel("Additional params:",container = tbl)
	tbl[13,2] <- (ellips <- gedit("",container=tbl))
	
	addSpring(g)
	addSpace(g,5)
	pg<-ggroup(horizontal=TRUE,container=g)
	addSpring(pg)
	btn_plot <- gbutton(" Plot data ",container=pg,handler=returnplot)
	enabled(btn_plot)<-FALSE
	font(btn_plot)<-c(weight="bold",scale="x-large")
	addSpace(pg,5)
	addSpace(g,5)
	
	#tooltips:
	tooltip(title)<-"set plot title (currently only for single KDE plots)"
	tooltip(from)<-"minimum age to plot (calculation of KDE will use all data)"
	tooltip(to)<-"maximum age to plot (calculation of KDE will use all data)"
	tooltip(bw)<-"bandwidth for KDE calculation\nset to 'NA' for automatic calculation of one common value\nset to -1 to individual calculated bandwidths"
	tooltip(fcol)<-"plot fill colour\nnamed colour ('blue') or hex value ('#1111FF99')"
	tooltip(split)<-"split age axis at this value"
	tooltip(toplot)<-"names of data series to plot"
	tooltip(eclasses)<-"classification values for data series - must be of same length as above"
	tooltip(markers)<-"add markers at actual data positions below plot"
	tooltip(histo)<-"add histogram overlay"
	tooltip(order)<-"reorder plots by classification values, otherwise leave order as in data file"
	tooltip(logx)<-"plot ages on logarithmic scale"
	tooltip(algo)<-"KDE calculation method\nBOTEV with optimal bandwidth calculation\nR's 'density()' function\nstandard KDE after JANN\nabove, with adaptive bandwidth after ABRAMSON\n(the latter 3 allow to specify the kernel below)"
	tooltip(ellips)<-"additional parameters passed on to the underlying functions"
	tooltip(op_fil)<-"open data file"
	tooltip(op_fol)<-"load all files from a folder"
	tooltip(op_var)<-"use data existing in the current R session"
	tooltip(autorange)<-"set to automatically include all data values"
	
	visible(win)<-TRUE
	focus(win)<-TRUE
	
	return()
}

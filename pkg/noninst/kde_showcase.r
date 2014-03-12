#################################################
#KDE plots, new jack-of-all-trades plotting function with many options:

### single KDE plot:
kdeplot(ages[["Inn"]])

#similar, but note automatic title (in previous example, column name was lost - this way retains it):
kdeplot(ages,plot=c("Inn"))

#set manual title:
kdeplot(ages[["Wien"]],title="Danube river sediment near Vienna, Austria")

#choose bandwidth manually
kdeplot(ages[["Passau"]],title="Danube near Passau",bandwidth=30)

#save it as pdf:
ggsave(file="KDE_single.pdf",width=12,height=8)

#modify plot object after it's defined:
last_plot()+theme(panel.background=element_blank(),panel.grid.major=element_line(colour="grey90",size=0.5))

#split age axis, set manual bandwidth:
kdeplot(ages,plot=c("Inn"),bandwidth=30,splitat=500)


### stack of KDE plots:
#first, look at calculated optimal bandwidths:
bws<-sapply(ages,optimal_bw,n=2^12)
bws
#Median value:
median(bws)

#"full auto" stacked KDE plots:
kdeplot(ages)

#save it as pdf:
ggsave(file="KDE_comparison.pdf",width=12,height=8)

#classify data:
#set all to "Tributary":
tps<-rep("Tributary",length(ages))
names(tps)<-names(ages)
#set some to "Danube"
tps[c("Wien","Passau","Ulm","Tulcea")]<-"Danube"

#plot - note reordering of plots:
kdeplot(ages,classes=tps)


### showcase new plotting function:

#stacked plots with split x-axis:
kdeplot(ages,splitat=500)

#plot all stack in same colour (note automatic removal of legend):
kdeplot(ages,classes=-1,splitat=500)

#select only certain data columns:
kdeplot(ages,plot=c("Inn","Wien","Passau"))

#for the colour-blind:
require(scales)
colours<-brewer_pal(type="div",palette=4)(length(unique(tps)))
kdeplot(ages,classes=tps,fcolour=colours)

#sepatate limits and bandwidths left and right:
kdeplot(ages[["Inn"]],limits=c(200,800,1500,2500),bandwidth=c(10,25))

#(close to) publication quality graph:
kdeplot(ages,fcolour="#000000FF")+theme(panel.background=element_blank(),panel.grid.major.x=element_line(colour="grey90",size=0.5),panel.grid.minor.x=element_line(colour="grey90",size=0.3))


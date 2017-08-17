require(Hmisc) #for rcorr
require(reshape2) #for melt
require(ggplot2)
require(psych) #for descriptive stats
require(Rmisc) #for summarySE
require(car) #for scatter plot matrix
require(GPArotation) #for factor analysis
###Configuration part

statsRD<-2 ##decimal place for means, std, etc
pvalRD<-3 ###decimal place for p value

###########

###########

##function to make histogram matrix for continuous variables   ##GRAPH
hist_mat<-function (data,stp,filehead) {  #data: dataset # stp: starting column number of continuous variable; filehead: the file name head for the histogram
	stp<-stp   #the starting number of continuous variables
	range<-stp:ncol(data)
	ncl<-round(sqrt(ncol(data)-stp-1))   #number of columns you want in the graph
	data_long<-melt(data,id.var=c(colnames(data)[1:(stp-1)])) #other non-continuous varaibles are conisdered as id
	#print(summary(data_long))
	hist_p<-ggplot(data_long,aes(x=value)) +
        geom_histogram(colour="black",fill="white",bins=round(nrow(data)/3)) +  #colour and fill: outline & fill for bars; bins define the total of bars
		facet_wrap(~variable,ncol=ncl,scales="free")
	print(hist_p)	
	pdf(paste0(filehead,"_histogram.pdf"), height=10,width=10)
	print(hist_p)
	dev.off()
}


###group different t-table  ##Inferential test
##group t-tests
mk_ttable<-function (data,pgrpv,stp,filehead,statsRD,pvalRD) { #pgrpv: column number for the group variable to compare
	stp<-stp   #the starting number of continuous variables
	range<-stp:ncol(data)
	x<-lapply(data[,range], function(x) t.test(x~data[,pgrpv],var.equal=T,conf.level=.95))
	t.table<-matrix(,length(x),7) #create an empty matrix to save the t-tests results for all variables, there are 7 values, t, df, p, ci.lower, ci.upper, group1mean,group2mean
	for (i in 1:nrow(t.table)) {
		t.table[i,1]<-round(x[[i]]$statistic,digits=statsRD)
		t.table[i,2]<-x[[i]]$parameter
		t.table[i,3]<-round(x[[i]]$p.value,digits=pvalRD)
		t.table[i,4]<-round(x[[i]]$conf.int[1],digits=statsRD)
		t.table[i,5]<-round(x[[i]]$conf.int[2],digits=statsRD)
		t.table[i,6]<-round(x[[i]]$estimate[1],digits=statsRD)
		t.table[i,7]<-round(x[[i]]$estimate[2],digits=statsRD)
	}
	colnames(t.table)<-c("t-value","df","p-value","lower.ci","upper.ci","group1mean","group2mean")
	rownames(t.table)<-colnames(data[,range])
	
	##save t-tests results
	grpname<-unique(data[,pgrpv])
	write.csv(t.table,file=paste0(filehead,"_",grpname[1],"_vs_",grpname[2],"comp_t-test_summary_table.csv"),quote=F,row.names=T)
	print(t.table)
}

####make descriptive data summary table
mk_destable<-function (data,pgrpv,stp,filehead,statsRD) {  #pgrpv: column number for group variable; if equals to 0, no stats separate by group will be done
    stp<-stp   #the starting number of continuous variables
	range<-stp:ncol(data)
	###descriptive data
	if (pgrpv == 0) {
		x<-describe(data[,range],fast=T) #the fast argument will produce the means, sd, range, etc.
		write.csv(x,file=paste0(filehead,"_whole_sample_descriptive.csv"))
	} else {
		x<-describeBy(data[,range],group=data[,pgrpv])
		for (i in 1:length(x)) {
			write.csv(x[i],file=paste0(filehead,"_",unique(data[,pgrpv])[i],"_group_descriptive.csv"))
			print(x[i])
		}	
	}
}

###make bar graph for group comparison on multiple repeated measures  ##GRAPH
mk_grpbargraph<-function (data,pgrpv,measrange,filehead,grpasx,h,w) {  #measrange: col numbers of measures; grpasx: wehther to plot group as x-axis, 1= Yes, 0 = No; h and w, the output size of pdf file
	pgrpv<-pgrpv   #the starting number of continuous variables
	range<-measrange
	data_part<-data[,c(pgrpv,range)]
	if (grpasx == 1) {    #controling whether the bar cluster is organized by group or variable/measures
		xval <-colnames(data)[pgrpv] #in order to pass the group variable name in ggplot
		fval <-"variable"
	} else {
		fval <-colnames(data)[pgrpv]
		xval <-"variable"
	}
	data_long<-melt(data_part,id.var=c(colnames(data)[pgrpv])) #other non-continuous varaibles are conisdered as id
	data_long_sum<-summarySE(data_long,measurevar="value",groupvar=c("variable",colnames(data)[pgrpv]),na.rm=T)

    bar_graph<-ggplot(data_long_sum,aes(x=get(xval),y=value,fill=factor(get(fval)))) +
               geom_bar(position=position_dodge(), stat="identity",color="black", width=0.8) +               
               geom_errorbar(aes(ymin=value-se,ymax=value+se),width=0.4,position=position_dodge(.8)) +
               #geom_hline(aes(yintercept=mean(data$fsiq)),color="black",linetype="solid") +
               #geom_hline(aes(yintercept=mean(data$fsiq)+15),color="black",linetype="dashed") +
               #geom_hline(aes(yintercept=mean(data$fsiq)-15),color="black",linetype="dashed") +               #geom_rect(data=data.frame(xmin=Inf,xmax=Inf,ymin=mean(data$fsiq)-15,ymax=mean(data$fsiq)+15),aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),fill="gray",alpha=0.5) +
               #scale_y_continuous(breaks=seq(40,140,20),limits=c(40,140)) +
               coord_cartesian(ylim=c(min(data_long_sum$value)-max(data_long_sum$sd),max(data_long_sum$value)+max(data_long_sum$sd))) +
               #scale_fill_manual(values=c("orangered1","orangered4","skyblue1","skyblue4")) +   #set the color scheme which needs to match the total number of variables
               theme_bw()
	pdf(paste0(filehead,"_",unique(data[,pgrpv])[1],"_",unique(data[,pgrpv])[2],"_group_comp_bargraph.pdf"),height=h,width=w)
	plot(bar_graph)
	dev.off()
	print(bar_graph)
}

###Correlation matrix
mk_rtable<-function (data,stp,statsRD,filehead) {
    data_temp<-data[,(stp:ncol(data))]
	r.mat<-corr.test(data_temp)
	r.table<-r.mat$r
	r.table<-round(r.table,digits=statsRD)
	p<-r.mat$p
	p[is.na(p)]<-1
	r.table[p<0.05]<-paste0(r.table[p<0.05],"*")
	r.table[p<0.01]<-paste0(r.table[p<0.01],"*")
	r.table[p<0.001]<-paste0(r.table[p<0.001],"*")
	r.table[(p<.10 & p>.05)]<-paste0(r.table[(p<.10 & p>.05)],"#")
	#r.table<-matrix(r.table,ncol(data_temp),ncol(data_temp),byrow=T)
	r.table[upper.tri(r.table)]<-""
	diag(r.table)<-"-"
	colnames(r.table)<-colnames(data_temp)
	rownames(r.table)<-colnames(data_temp)
	write.csv(r.table, file=paste0(filehead,"_correlation_matrix.csv"),quote=F,row.names=T)
	print(r.table)
}

####

##make scatter plots for different groups

mk_scatter<-function (data,xname,yname,gname,filehead,seshade) { #colnames for x, y, and grouping variables in the data;seshade: 1 plot SE shades
	xp<-which(colnames(data)==xname)
	yp<-which(colnames(data)==yname)
	gp<-which(colnames(data)==gname)
	scatter_plot<-ggplot(data,aes(get(colnames(data)[xp]),get(colnames(data)[yp]),colour=get(colnames(data)[gp]))) +
	              geom_point(shape=1,size=3) +
				  scale_colour_hue(l=70) +   #use a slightly darker palette than normal
				  geom_smooth(method=lm,se=seshade) +
				  theme_bw()
	pdf(paste0(filehead,"_",xname,"_with_",yname,"_by_",gname,"_scatterplot.pdf"),width=10,height=10)
	plot(scatter_plot)
	dev.off()
	print(scatter_plot)
}

						  
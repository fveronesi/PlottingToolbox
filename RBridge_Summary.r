### KMeans Clustering Toolbox
##Author: Fabio Veronesi
tool_exec <- function(in_params, out_params)
{
	if (!requireNamespace("gridExtra", quietly = TRUE))
		install.packages("gridExtra")
	require(gridExtra)
	
	if (!requireNamespace("sp", quietly = TRUE))
    install.packages("sp")
	require(sp)
	
	if (!requireNamespace("moments", quietly = TRUE))
    install.packages("moments")
	require(moments)
  
	print("Summary")
	print("Author: Fabio Veronesi")
  
	source_shp = in_params[[1]]
	variable = in_params[[2]]
	
	out_folder = in_params[[3]]
	
	dir.create(out_folder)
   
	### Read Data
	d <- arc.open(source_shp)
	
	### Create a Data.Frame with all the variables
	data <- arc.select(d, variable)
	
	data_summary <- data.frame(data[,variable[1]])
  
	if(length(variable)>1){
		for(i in 2:length(variable)){
			data_summary <- cbind(data_summary, data[,variable[i]])
		}
	}
	
	names(data_summary) <- variable
	
	
	Dmean <- c()
	Dmedian <- c()
	DStDev <- c()
	Dvar <- c()
	count <- c()
	q25 <- c()
	q75 <- c()
	dIQR <- c()
	skew <- c()
	kurto <- c()
	Dmin <- c()
	Dmax <- c()
	
	for(i in 1:length(variable)){
		Dmean[i] <- mean(data[,i], na.rm=T)
		Dmedian[i] <- median(data[,i], na.rm=T)
		DStDev[i] <- sd(data[,i], na.rm=T)
		Dvar[i] <- var(data[,i], na.rm=T)
		count[i] <- nrow(data)
		q25[i] <- as.numeric(quantile(data[,i], 0.25, na.rm=T))
		q75[i] <- as.numeric(quantile(data[,i], 0.75, na.rm=T))
		dIQR[i] <- IQR(data[,i], na.rm=T)
		skew[i] <- skewness(data[,i], na.rm=T)
		kurto[i] <- kurtosis(data[,i], na.rm=T)
		Dmin[i] <- min(data[,i], na.rm=T)
		Dmax[i] <- max(data[,i], na.rm=T)
	}
	
		
	summaryDF1 <- data.frame(count, round(Dmin,2), round(q25,2), round(Dmean,2), round(Dmedian,2), round(q75,2), round(Dmax,2))
	
	summaryDF2 <- data.frame(round(DStDev,2), round(Dvar,2), round(dIQR,2), round(skew,2), round(kurto,2))
	
	names(summaryDF1) <- c("Count", "Minimum", "1st Quartile", "Mean", "Median", "3rd Quartile", "Maximum")
	
	names(summaryDF2) <- c("Standard Deviation", "Variance", "Interquartile Range", "Skewness", "Kurtosis")
	
	row.names(summaryDF1) <- variable
	row.names(summaryDF2) <- variable
	
	pl <- grid.arrange(tableGrob(summaryDF1), tableGrob(summaryDF2), nrow=2)
	
	print(pl)
	
	pdf(paste0(out_folder,"/Summary_",variable,".pdf"), height=5, width=8, paper="a4r")
	grid.arrange(tableGrob(summaryDF1), tableGrob(summaryDF2), nrow=2)
	dev.off()
	
	write.table(cbind(summaryDF1,summaryDF2), paste0(out_folder,"/Summary.csv"), sep=",")
	
}

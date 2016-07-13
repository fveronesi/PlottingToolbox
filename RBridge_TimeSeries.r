### KMeans Clustering Toolbox
##Author: Fabio Veronesi
tool_exec <- function(in_params, out_params)
{
	if (!requireNamespace("ggplot2", quietly = TRUE))
	install.packages("ggplot2")
	require(ggplot2)
	
	if (!requireNamespace("sp", quietly = TRUE))
    install.packages("sp")
	require(sp)
	
	if (!requireNamespace("reshape2", quietly = TRUE))
    install.packages("reshape2")
	require(reshape2)
  
	print("Time-Series")
	print("Author: Fabio Veronesi")
  
	source_shp = in_params[[1]]
	variable = in_params[[2]]
	timeCL = in_params[[3]]
	formatTM = in_params[[4]]
	CAT = in_params[[5]]
	subVA = in_params[[6]]
		
	out_folder = in_params[[7]]
	
	dir.create(out_folder)
   
	### Read Data
	d <- arc.open(source_shp)
	
	### Create a Data.Frame with all the variables
	data <- arc.select(d, variable)
	TIME <- arc.select(d, timeCL)
	
	data_plot <- data.frame(VAR=data[,variable], TIME=TIME[,timeCL])
	
	
	if(!is.null(CAT)){
		CATEGORY <- arc.select(d, CAT)
		data_plot <- cbind(data_plot, CAT=CATEGORY[,CAT])
		data_sub <- data_plot[paste(data_plot$CAT)==subVA,]
		
		data_sub$DATETIME <- as.POSIXct(data_sub$TIME, format=formatTM)
			
		dev.new()
		ts <- ggplot(data=data_sub, aes(x=DATETIME, y=VAR)) +
			geom_line() +
			xlab("Time") +
			ylab(variable) +
			labs(title=paste("Time-Series",subVA))
			theme_classic()
			
		print(ts)
		ggsave(filename=paste0(out_folder,"/TimeSeries",variable,".jpg"),plot=ts, dpi=300)
	
	} else {
		data_plot$DATETIME <- as.POSIXct(data_plot$TIME, format=formatTM)
				
		dev.new()
		ts <- ggplot(data=data_plot, aes(x=DATETIME, y=VAR)) +
			xlab("Time") +
			ylab(variable) +
			geom_line() +
			theme_classic()
			
		print(ts)
		ggsave(filename=paste0(out_folder,"/TimeSeries",variable,".jpg"),plot=ts, dpi=300)
	
	}
	
	

	
	
}

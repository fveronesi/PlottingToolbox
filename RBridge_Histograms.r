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
  
	print("Histograms")
	print("Author: Fabio Veronesi")
  
	source_shp = in_params[[1]]
	variable = in_params[[2]]
	facet = in_params[[3]]
	bin = in_params[[4]]
	
	out_folder = in_params[[5]]
	
	dir.create(out_folder)
   
	### Read Data
	d <- arc.open(source_shp)
	
	### Create a Data.Frame with all the variables
	data <- arc.select(d, variable)
	FCT <- arc.select(d, facet)

	data_plot <- data.frame(data[,variable[1]])
  
	if(length(variable)>1){
		for(i in 2:length(variable)){
			data_plot <- cbind(data_plot, data[,variable[i]])
		}
	}
	
	names(data_plot) <- variable
	
	if(!is.null(facet)){
		data_plot <- cbind(data_plot, facet=FCT)
	}
	
	print(str(data_plot))
	print(names(FCT))
		
	if(is.null(facet)){
	if(is.null(bin)){
	for(i in 1:length(variable)){
		dev.new()
		g <- ggplot(data=data_plot, aes(x=data_plot[,i])) + 
			geom_histogram() +
			labs(title = paste0("Histogram of ",variable[i])) +
			xlab(variable[i]) +
			ylab("Frequency") +
			theme_classic()
		print(g)
		
		ggsave(filename=paste0(out_folder,"/Histogram",variable[i],".jpg"),plot=g, dpi=300)
	}
	} else {
	for(i in 1:length(variable)){
		dev.new()
		g <- ggplot(data=data_plot, aes(x=data_plot[,i])) + 
			geom_histogram(binwidth=as.numeric(bin)) +
			labs(title = paste0("Histogram of ",variable[i])) +
			xlab(variable[i]) +
			ylab("Frequency") +
			theme_classic()
		print(g)
		
		ggsave(filename=paste0(out_folder,"/Histogram",variable[i],".jpg"),plot=g, dpi=300)
	}
	}
	
	} else {
	if(is.null(bin)){
	for(i in 1:length(variable)){
		dev.new()
		g <- ggplot(data=data_plot, aes(x=data_plot[,i])) + 
			geom_histogram() +
			labs(title = paste0("Histogram of ",variable[i])) +
			xlab(variable[i]) +
			ylab("Frequency") +
			theme_classic() +
			facet_wrap(as.formula(paste0("~",names(FCT))))
		print(g)
		
		ggsave(filename=paste0(out_folder,"/Histogram",variable[i],".jpg"),plot=g, dpi=300)
	}
	} else {
	for(i in 1:length(variable)){
		dev.new()
		g <- ggplot(data=data_plot, aes(x=data_plot[,i])) + 
			geom_histogram(binwidth=as.numeric(bin)) +
			labs(title = paste0("Histogram of ",variable[i])) +
			xlab(variable[i]) +
			ylab("Frequency") +
			theme_classic() +
			facet_wrap(as.formula(paste0("~",names(FCT))))
		print(g)
		
		ggsave(filename=paste0(out_folder,"/Histogram",variable[i],".jpg"),plot=g, dpi=300)
	}
	}
	
	}
}

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
  
	print("Bar Charts")
	print("Author: Fabio Veronesi")
  
	source_shp = in_params[[1]]
	variable = in_params[[2]]
	group = in_params[[3]]
	avg = in_params[[4]]
		
	out_folder = in_params[[5]]
	
	dir.create(out_folder)
   
	### Read Data
	d <- arc.open(source_shp)
	
	### Create a Data.Frame with all the variables
	data <- arc.select(d, variable)
	FCT <- arc.select(d, group)

	data_plot <- data.frame(VAR=data[,variable])
	data_plot <- cbind(data_plot, group=FCT[,group])
	
	if(!is.null(avg)){
		dt <- sapply(unique(data_plot$group), function(x){
			sub <- data_plot[paste(data_plot$group)==x,]
			return(mean(sub$VAR, na.rm=T))
		})

		data_plot <- data.frame(VAR=dt, group=unique(data_plot$group))
	}

	oind <- order(as.numeric(by(data_plot$VAR, data_plot$group, max)))    
	data_plot$group <- ordered(data_plot$group, levels=levels(data_plot$group)[oind])

	dev.new()
	bc <- ggplot(data=data_plot, aes(x=data_plot[,2], y=data_plot[,1])) + 
		geom_bar(stat="identity") +
		xlab(variable) +
		ylab(group) +
		theme(axis.text.x  = element_text(angle=90, vjust=0.5), panel.background = element_rect(fill = "white")) 
	print(bc)
	
	ggsave(filename=paste0(out_folder,"/BarChart",variable,".jpg"),plot=bc, dpi=300)

	
	
	
}

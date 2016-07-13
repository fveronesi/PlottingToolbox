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
  
	print("Scatterplots")
	print("Author: Fabio Veronesi")
  
	source_shp = in_params[[1]]
	X = in_params[[2]]
	Y = in_params[[3]]
	col = in_params[[4]]
	size = in_params[[5]]
	line = in_params[[6]]
		
	out_folder = in_params[[7]]
	
	dir.create(out_folder, showWarnings = FALSE)
   
	### Read Data
	d <- arc.open(source_shp)
	
	print(line)
	
	### Create a Data.Frame with all the variables
	Xs <- arc.select(d, X)
	Ys <- arc.select(d, Y)
	

	data_plot <- data.frame(Xs=Xs[,1], Ys=Ys[,1])
	
  
	if(!is.null(col)){
		CL <- arc.select(d, col)
		data_plot <- cbind(data_plot, CL=CL[,1])
	}
	
	if(!is.null(size)){
		SZ <- arc.select(d, size)
		data_plot <- cbind(data_plot, SZ=SZ[,1])
	}

	if(!is.null(col)&!is.null(size)){
	if(is.null(line)){
	dev.new()
	sc <- ggplot(data=data_plot, aes(x=Xs, y=Ys, color=CL, size=SZ)) +
		geom_point() +
		xlab(X) +
		ylab(Y) +
		labs(title=paste0("Scatterplot ",X,"/",Y),color=col, size=size) +
		theme_classic() +
		scale_colour_gradientn(colours=c("blue","light blue","green","orange","red"))
	print(sc)
	ggsave(filename=paste0(out_folder,"/Scatterplot_",X,"_",Y,".jpg"),plot=sc, dpi=300)
	} else {
	dev.new()
	sc <- ggplot(data=data_plot, aes(x=Xs, y=Ys, color=CL, size=SZ)) +
		geom_point() +
		xlab(X) +
		ylab(Y) +
		labs(title=paste0("Scatterplot ",X,"/",Y),color=col, size=size) +
		theme_classic() +
		scale_colour_gradientn(colours=c("blue","light blue","green","orange","red")) +
		geom_smooth(method = "lm", se = TRUE)
	print(sc)
	ggsave(filename=paste0(out_folder,"/Scatterplot_",X,"_",Y,".jpg"),plot=sc, dpi=300)
	}
	} else if(!is.null(col)&is.null(size)){
	if(is.null(line)){
	dev.new()
	sc <- ggplot(data=data_plot, aes(x=Xs, y=Ys, color=CL)) +
		geom_point() +
		xlab(X) +
		ylab(Y) +
		labs(title=paste0("Scatterplot ",X,"/",Y),color=col) +
		theme_classic() +
		scale_colour_gradientn(colours=c("blue","light blue","green","orange","red"))
	print(sc)
	ggsave(filename=paste0(out_folder,"/Scatterplot_",X,"_",Y,".jpg"),plot=sc, dpi=300)
	} else {
	dev.new()
	sc <- ggplot(data=data_plot, aes(x=Xs, y=Ys, color=CL)) +
		geom_point() +
		xlab(X) +
		ylab(Y) +
		labs(title=paste0("Scatterplot ",X,"/",Y),color=col) +
		theme_classic() +
		scale_colour_gradientn(colours=c("blue","light blue","green","orange","red")) +
		geom_smooth(method = "lm", se = TRUE)
	print(sc)
	ggsave(filename=paste0(out_folder,"/Scatterplot_",X,"_",Y,".jpg"),plot=sc, dpi=300)
	}
	} else {
	if(is.null(line)){
	dev.new()
	sc <- ggplot(data=data_plot, aes(x=Xs, y=Ys)) +
		geom_point() +
		xlab(X) +
		ylab(Y) +
		labs(title=paste0("Scatterplot ",X,"/",Y)) +
		theme_classic()
	print(sc)
	ggsave(filename=paste0(out_folder,"/Scatterplot_",X,"_",Y,".jpg"),plot=sc, dpi=300)
	} else {
	dev.new()
	sc <- ggplot(data=data_plot, aes(x=Xs, y=Ys)) +
		geom_point() +
		xlab(X) +
		ylab(Y) +
		labs(title=paste0("Scatterplot ",X,"/",Y)) +
		theme_classic() +
		geom_smooth(method = "lm", se = TRUE)
	print(sc)
	ggsave(filename=paste0(out_folder,"/Scatterplot_",X,"_",Y,".jpg"),plot=sc, dpi=300)
	}
	}

}

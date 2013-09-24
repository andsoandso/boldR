library("ggplot2")
library("reshape2")

plot.bolddf.homogeneity <- function(bolddf, name, seperate_by=NA, height=0, width=0){

    if(! is.na(name)){ .pdf.device.setup(name, height, width) }

    if (is.na(seperate_by)){
        p <- ggplot(data=bolddf, 
            aes(x=factor(index), y=voxel, fill=data)) +
            geom_tile() +
            scale_fill_continuous(low="black", high="pink") +

            facet_grid(dataname~cond) +
            
            ylab("Voxel") +
            xlab("Index") +
            theme_bw() + 
            theme(
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank(),
                strip.text.y = element_text(angle=0)
            )
        print(p)
    } else if (seperate_by == "dataname") {
        for (rname in unique(as.character(bolddf$dataname))){
            print(paste("Plotting", rname, sep=" "))

            p <- ggplot(data=bolddf[rname == bolddf$dataname,], 
                    aes(x=factor(index), y=voxel, fill=data)) +

            geom_tile() +
            scale_fill_continuous(low="black", high="pink") +

            facet_grid(.~cond) + 
            ggtitle(rname) +
            
            ylab("Voxel") +
            xlab("Index") +
            theme_bw() + 
            theme(
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank(),
                strip.text.y = element_text(angle=0)
            )
            print(p)
        }
    } else {
        stop("Argument 'seperate_by' not recognized. Try NA, or 'dataname'.")
    }
    dev.off()
}

plot.bolddf.boxplot <- function(bolddf, name=NA, height=0, width=0){
# Plots timecourses for each voxel in a grid. Conds are separatly colored.

    if(! is.na(name)){ .pdf.device.setup(name, height, width) }

    p <- ggplot(data=bolddf, aes(x=factor(index), y=data, fill=cond, 
            colour=cond))
    p <- p + geom_boxplot(alpha=0.5, outlier.colour="light grey")
    p <- p + ylab("BOLD signal") + xlab("Index") + theme_bw()
    p <- p + facet_wrap(~voxel, ncol=3)

    if(! is.na(name)){
        print(p)    
        dev.off()
    }
    else{ return(p) }
}


plot.bolddf.stat <- function(bolddf, stat, name=NA, height=0, width=0, 
        nulldist=FALSE, geom="boxplot"){
# For every voxel plot the named stat, as a boxplot, coloring based on cond.
# NOTE:
# Has two modes.  If name is NA, returns a ggplot() pointer.
# If it is a char string, save the result to file of that name.

    if(! is.na(name)){ .pdf.device.setup(name, height, width) }

    print("Creating stats.")
    bolddf <- bolddf.stats(bolddf, stat)

    # Create the nulldist data?
    if (nulldist) {
        print("Creating voxel specfic null distribution stats.")

        # Create the nulldist, and add a level 
        # to cond indicating denoating
        # the null distribution data
        n_samples <- 100
        print(paste("Taking", n_samples, "samples."))

        bolddf_nulldist <- bolddf.nulldist(bolddf, stat, n_samples)
        bolddf_nulldist[["cond"]] <- factor(
                rep("null", nrow(bolddf_nulldist)))

        bolddf <- rbind(bolddf, bolddf_nulldist)
    }

    # Init the plot aes
    p <- ggplot(data=bolddf, aes(x=voxel, y=data, colour=cond))

    # What geom of plot to use?
    if(geom == "boxplot"){
        p <- p + geom_boxplot(notch=FALSE, outlier.colour="light grey")
    } else if(geom == "violin"){
        p <- p + geom_violin()
    } else {
        stop("Geom not valid. Try 'boxplot' or 'violin'.")
    }
    
    # Finish the plot config
    p <- p + 
        theme_bw() + 
        ylab(paste("Distribution of dataname ", stat, "'s", sep="")) + 
        xlab("Voxel") + 
        theme(
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank()
        ) + 
        coord_flip()
    
    # Plot or return p
    if(! is.na(name)){
        print(p)    
        dev.off()
    }
    else{ return(p) }
}


plot.bolddf.allstats <- function(bolddf, name=NA, height=0, width=0, 
        nulldist=FALSE, geom="boxplot"){
# Plot all the stats available in the stat.timecourse function,
# saving each as a page inside the same pdf file.

    if(! is.na(name)){ .pdf.device.setup(name, height, width) }

    statsnames <- c('mean', 'var', 'diff', 'median', 'time_to_max')
    for(statname in statsnames){
        print(statname)
        p <- bolddf.stat(bolddf, statname, NA, height, width, nulldist, geom)  
        print(p)    
    }

    dev.off()
}

plot.bolddf.rmsdifference <- function(bolddf, stat, name=NA, height=0, width=0){
# TODO doc....

    if(! is.na(name)){ .pdf.device.setup(name, height, width) }

    # Are there enough cond?
    conds <- as.character(unique(bolddf$cond))
    if (length(conds) <= 2) {
        stop("This plot needs more than 2 cond to be of use.")
    }

    condpairs <- combn(conds, 2)
        ## Each col in the condpairs df is a pair.

    bolddf_rmsdiff <- ddply(
        bolddf, 
        .(voxel, dataname),
        # Anonymous fn to calculate 
        # rmss for all the pairs
        function(bolddf, condpairs){
            rmss <- NULL
            pairnames <- NULL
            for(j in 1:ncol(condpairs)){
                # Make conditions and datas clear
                # The do RMS and join the condition names
                c1 = condpairs[1,j]
                c2 = condpairs[2,j]
                v1 = bolddf[c1 == bolddf$cond, "data"]
                v2 = bolddf[c2 == bolddf$cond, "data"]
                rmss <- c(rmss, sqrt((v1 - v2)^2))
                pairnames <- c(pairnames, paste(c1, c2, sep="-"))
            }

            N <- length(rmss)
            data.frame(
                data=rmss, 
                condpair=pairnames,
                voxel=rep(bolddf$voxel, N),
                dataname=rep(bolddf$dataname, N)
            )
        }, 
        condpairs)

    print(str(bolddf_rmsdiff))

    # Finally we plot...
    p <- ggplot(data=bolddf_rmsdiff, aes(x=voxel, y=data, 
        fill=condpair, colour=condpair)) + 
        geom_boxplot(alpha=0.8, notch=FALSE, outlier.colour="light grey") + 
        theme_bw() +
        theme(
            axis.text.y=element_blank(),
            axis.ticks=element_blank(),
            axis.title.y=element_blank()
        ) + 
        ylab(paste("RMS ", stat)) +
        ylim(0,0.002) +
        coord_flip()

    if(! is.na(name)){
        print(p)    
        dev.off()
    }
    else{ return(p) }
}


.pdf.device.setup <- function(name, height, width){
       if((height > 0) && (width > 0)){
            pdf(file=name, height=height, width=width)
        } else {
            pdf(file=name)  ## Use the defaults   
    }
}
library("ggplot2")
library("reshape2")

plot.bolddf.homogeneity <- function(bolddf, name, seperate_by=NA, 
        height=0, width=0, returnp=FALSE, title=NA){
    if(! is.na(name)){ .pdf.device.setup(name, height, width) }
    # else { dev.new(width=width, height=height) }

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
            if (! is.na(title)) { p <- p + ggtitle(title) }

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
            if (! is.na(title)) { p <- p + ggtitle(title) }

            print(p)
        }
    } else {
        stop("Argument 'seperate_by' not recognized. Try NA, or 'dataname'.")
    }
    if (! is.na(name)) { dev.off() }
    if (returnp) {return(p) }
}


plot.bolddf.mediantc <- function(bolddf, name=NA, height=0, width=0, 
            returnp=FALSE, title=NA, drop_cond=NULL, vlines=NULL, defcolor=FALSE){

    if(! is.na(name)){ .pdf.device.setup(name, height, width) }
    # else { dev.new(width=width, height=height) }
    
    if (! is.null(drop_cond)) {
        for(dc in drop_cond){
            bolddf <- bolddf[bolddf$cond != dc, ]
        }
    }

    p <- ggplot(data=bolddf, aes(x=index, y=data, colour=cond, fill=cond))   
    p <- p + stat_summary(fun.y=median, geom="line", size=1.5) +
        facet_grid(voxel~., scales = "free_y") +
        ylab("BOLD signal (AU)") + xlab("Time (TR)") + theme_bw() +
        scale_x_continuous(breaks=1:max(bolddf$index)) +
        theme(
            plot.background = element_blank(),   ## Main facet bkg
            panel.grid.major = element_blank(),  ## Major grid lines
            panel.grid.minor = element_blank(),  ## Minor grid lines
            panel.border = element_blank(),      ## Facet border
            panel.background = element_blank(),  ## Facet bkg
            # axis.text.y=element_blank(),       ## y lab
            # axis.ticks.y=element_blank(),      ## y ticks  
            strip.text.y = element_text(angle=0),## facet name rotate
            strip.background = element_blank()   ## Fam1e bckgrnd (grey+box)
        )
    
    if (! defcolor) {
        p <- p + scale_colour_brewer(palette="BrBG")
    }

    if (! is.null(vlines)) {
        p <- p +  geom_vline(xintercept = vlines, colour="light grey")
    }
    if (! is.na(title)) { p <- p + ggtitle(title) }
    print(p)
    if(! is.na(name)){ dev.off() }
    if (returnp) { return(p) }
}


plot.bolddf.tc <- function(bolddf, name=NA, height=0, width=0, 
            returnp=FALSE, title=NA, drop_cond=NULL, vlines=NULL,
            defcolor=FALSE){

    if(! is.na(name)){ .pdf.device.setup(name, height, width) }
    
    if (! is.null(drop_cond)) {
        for(dc in drop_cond){
            bolddf <- bolddf[bolddf$cond != dc, ]
        }
    }    
    p <- ggplot(data=bolddf, aes(x=index, y=data, colour=cond, group=dataname)) +
        geom_line(alpha=0.25) + 
        facet_grid(voxel~cond, scales="free_y") +
        ylab("BOLD signal (AU)") + xlab("Time (TR)") + theme_minimal() +
        scale_x_continuous(breaks=1:max(bolddf$index)) +
        
        # Strip off all the boxes
        theme(
            plot.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.text.x = element_blank(),
            strip.text.y = element_text(angle=0)
        )
    if (! defcolor) {
        p <- p + scale_colour_brewer(palette="BrBG")
    }

    if (! is.null(vlines)) {
        p <- p +  geom_vline(xintercept = vlines, colour="light grey")
    }

    if (! is.na(title)) { p <- p + ggtitle(title) }
    print(p)
    if(! is.na(name)){ dev.off() }
    if (returnp) { return(p) }
}

plot.bolddf.tcpoint <- function(bolddf, name=NA, height=0, width=0, 
        returnp=FALSE, title=NA){
    if(! is.na(name)){ .pdf.device.setup(name, height, width) }
    # else { dev.new(width=width, height=height) }
    
    p <- ggplot(data=bolddf, aes(x=index, y=data, colour=cond)) +
        geom_point(alpha=0.25) +
        facet_grid(voxel~cond, scales = "free_y") +
        ylab("BOLD signal (AU)") + xlab("Time (TR)") + theme_bw() + 
        scale_x_continuous(breaks=1:max(bolddf$index)) +

        theme(
            plot.background = element_blank(),   ## Main facet bkg
            panel.grid.major = element_blank(),  ## Major grid lines
            panel.grid.minor = element_blank(),  ## Minor grid lines
            panel.border = element_blank(),      ## Facet border
            panel.background = element_blank(),  ## Facet bkg
            # axis.text.y=element_blank(),       ## y lab
            # axis.ticks.y=element_blank(),      ## y ticks  
            strip.text.y = element_text(angle=0),## facet name rotate
            strip.background = element_blank()   ## Fam1e bckgrnd (grey+box)
        )
    if (! is.na(title)) { p <- p + ggtitle(title) }

    print(p)
    if(! is.na(name)){ dev.off() }
    if (returnp) { return(p) }
}


plot.bolddf.boxplot <- function(bolddf, name=NA, seperate_by=NULL, 
        height=0, width=0, returnp=FALSE, title=NA, drop_cond=NULL, 
        notch=FALSE, defcolor=FALSE){
# Plots timecourses for each voxel in a grid. Conds are separatly colored.

    if(! is.na(name)){ .pdf.device.setup(name, height, width) }
    # else { dev.new(width=width, height=height) }
    
    if (! is.null(drop_cond)) {
        for(dc in drop_cond){
            bolddf <- bolddf[bolddf$cond != dc, ]
        }
    }

    p <- NULL
    if (is.null(seperate_by)) { 
        p <- ggplot(data=bolddf, aes(x=factor(
                    index), y=data, colour=cond, fill=cond)) + 
            facet_grid(voxel~., scales = "free_y")
    } else if (seperate_by == "cond") {
        p <- ggplot(data=bolddf, aes(x=factor(index), y=data)) +
                    facet_grid(voxel~cond, scales = "free_y")
    } else { 
        stop("seperate_by was not valid") 
    }

    if (! defcolor) {
        p <- p + scale_colour_brewer(palette="BrBG") +
        scale_fill_brewer(palette="BrBG") 
    }

    p <- p + geom_boxplot(alpha=0.5, outlier.colour="light grey", notch=notch)
    p <- p + ylab("BOLD signal (AU)") + xlab("Index") + theme_bw()
    if (! is.na(title)) { p <- p + ggtitle(title) }
    p <- p + theme(
        plot.background = element_blank(),   ## Main facet bkg
        panel.grid.major = element_blank(),  ## Major grid lines
        panel.grid.minor = element_blank(),  ## Minor grid lines
        panel.border = element_blank(),      ## Facet border
        panel.background = element_blank(),  ## Facet bkg
        #axis.text.y=element_blank(),       ## y lab
        #axis.ticks.y=element_blank(),      ## y ticks  
        axis.text.x=element_blank(),       ## x lab
        #axis.ticks.x=element_blank(),      ## x ticks  
        strip.text.y = element_text(angle=0),## facet name rotate
        strip.background = element_blank()   ## Fame bckgrnd (grey+box)
    )
    print(p)
    if(! is.na(name)){ dev.off() }
    if (returnp) { return(p) }
}


plot.bolddf.stat <- function(bolddf, stat, name=NA, height=0, width=0, 
        nulldist=FALSE, geom="boxplot", returnp=FALSE, title=NA){
# For every voxel plot the named stat, as a boxplot, coloring based on cond.
# NOTE:
# Has two modes.  If name is NA, returns a ggplot() pointer.
# If it is a char string, save the result to file of that name.

    if(! is.na(name)){ .pdf.device.setup(name, height, width) }
    # else { dev.new(width=width, height=height) }
    
    print("Creating stats.")
    bolddf <- bolddf.stat(bolddf, stat)

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
            plot.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            strip.text.y = element_text(angle=0)
        ) + 
        coord_flip()
    
    if (! is.na(title)) { p <- p + ggtitle(title) }

    print(p)
    # Plot or return p
    if(! is.na(name)){        
        dev.off()
    }
    if (returnp) { return(p) }
}


plot.bolddf.allstats <- function(bolddf, name=NA, height=0, width=0, 
        nulldist=FALSE, geom="boxplot", returnp=FALSE, title=NA){
# Plot all the stats available in the stat.timecourse function,
# saving each as a page inside the same pdf file.

    if(! is.na(name)){ .pdf.device.setup(name, height, width) }
    # else { dev.new(width=width, height=height) }
    
    statsnames <- c(mean, var, diff, median, time.to.max)
    for(statname in statsnames){
        print(statname)
        p <- bolddf.stat(bolddf, statname, NA, height, width, nulldist, geom)  
        print(p)    
    }
    
    if (! is.na(name)) { dev.off() }
    if (returnp) { return(p) }
}

plot.bolddf.rmsdifference <- function(bolddf, stat, name=NA, height=0, width=0,
            returnp=FALSE, title=NA){
    if(! is.na(name)){ .pdf.device.setup(name, height, width) }
    # else { dev.new(width=width, height=height) }
    
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

    # Finally we plot...
    p <- ggplot(data=bolddf_rmsdiff, aes(x=voxel, y=data, 
        fill=condpair, colour=condpair)) + 
        geom_boxplot(alpha=0.8, notch=FALSE, outlier.colour="light grey") + 
        theme_bw() +
        theme(
            plot.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            strip.text.y = element_text(angle=0)
        ) + 
        ylab(paste("RMS ", stat)) +
        ylim(0,0.002) +
        coord_flip()

    if (! is.na(title)) { p <- p + ggtitle(title) }

    print(p)
    if(! is.na(name)){ dev.off() }
    if (returnp) { return(p) }
}


.pdf.device.setup <- function(name, height, width){
    # cat("Using pdf().")

    if((height > 0) && (width > 0)){
        pdf(file=name, height=height, width=width)
    } else {
        pdf(file=name)  ## Use the defaults   
    }
}

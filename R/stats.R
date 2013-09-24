library("plyr")
library("reshape2")

# ----
# Private helper fns.
# Creates the Null dists
.puggle <- function(bolddf, statfn){

    conds <- as.character(bolddf$cond)
    cond_levels <- unique(conds)

    datanames <- as.character(bolddf$dataname)
    roi_levels <- unique(datanames)

    # Create a int ID for each datanames/cond
    # combination.
    group_id <- rep(NULL, nrow(bolddf))
    i <- 1
    for (roi in roi_levels) {
        mask_roi <- roi == datanames
        for (cond in cond_levels) {
            mask <- mask_roi & (cond == conds)
            group_id[mask] <- i
            i <- i + 1
        }
    }

    # Use the group_id to swap groups randomly
    id_code <- unique(group_id)
    shuffled_id_code <- sample(id_code)
    newbolddf <- bolddf
    for (id in id_code) {
        maskout <- group_id == id
        maskin <- group_id == shuffled_id_code[id]

        if (sum(maskin) != sum(maskout)){
            stops("Size of maskin did not match maskout")
        }

        # print("***** OUT *****")
        # .print_bolddf_info(bolddf[maskin,])
        # print("***** IN *****")
        # .print_bolddf_info(bolddf[maskout,])
        newbolddf$data[maskin] <- bolddf$data[maskout]
    }
    bolddf_null <- statfn(newbolddf)
}

# Returns the appropriate statistic function.
.get.statfn <- function(stat){
    if(stat == "mean"){
        statfn <- function(bolddf){
            meandf <- ddply(
                bolddf, 
                .(voxel, dataname, cond), 
                function(bolddf) { mean(bolddf$data) }) 
        }
    } else if(stat == "median") {
        statfn <- function(bolddf){
            meandf <- ddply(
                bolddf, 
                .(voxel, dataname, cond),
                function(bolddf){ median(bolddf$data) })
        }
    } else if(stat == "var"){
        statfn <- function(bolddf){
            vardf <- ddply(
                bolddf, 
                .(voxel, dataname, cond), 
                function(bolddf){ var(bolddf$data) })
        }
    } else if(stat == "time_to_max"){
        statfn <- function(bolddf){
            ttmdf <- ddply(
                bolddf, 
                .(voxel, dataname, cond), 
                function(bolddf){
                    idx <- 0:(length(bolddf$data)-1)
                    idx[max(bolddf$data) == bolddf$data]
                })
        }
    } else if(stat == "diff"){
        statfn <- function(bolddf){
            diffdf <- ddply(
                bolddf, 
                .(voxel, dataname, cond), 
                function(bolddf){  
                    ranges <- range(bolddf$data)
                    ranges[2] - ranges[1] 
                })
        }
    } else {
        print("stat not understood.")
    }
    statfn
}

bolddf.stats <- function(bolddf, stat){
    bolddf_stat <- (.get.statfn(stat))(bolddf)
    bolddf_stat[["index"]] <- rep(1, nrow(bolddf_stat))
    colnames(bolddf_stat) <- c("voxel", "dataname", "cond", "data", "index")
    bolddf_stat
}

bolddf.nulldist <- function(bolddf, stat, ninter=100, seed=42){
    set.seed(seed)

    statfn <- .get.statfn(stat)
    bolddf_null <- ddply(bolddf, .(voxel), .puggle, statfn)
    colnames(bolddf_null) <- c("voxel", "dataname", "cond", "data")
    for (i in 1:(ninter-1)){
        # Generate this iterations nulls dist samples, 
        # and average them with the rest.

        # Create this iters data
        bolddf_null_i <- ddply(bolddf, .(voxel), .puggle, statfn)
        colnames(bolddf_null_i) <- c("voxel", "dataname", "cond", "data")

        # Setup update vars 
        x <- bolddf_null_i$data
        mean <- bolddf_null$data

        # Calc the update
        delta <- x - mean
        mean <- mean + (delta / i)
        
        # And reset
        bolddf_null$data <- mean
    }
    bolddf_null[["index"]] <- rep(1,nrow(bolddf_null))
    bolddf_null
}


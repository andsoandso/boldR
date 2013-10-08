library("plyr")
library("reshape2")

# Some useful functions
# that take only arrays.
time.to.max <- function(x){
    idx <- 0:(length(x)-1)
    idx[max(x) == x]
}

diff <- function(x){  
    ranges <- range(x)
    ranges[2] - ranges[1] 
}

# ----
# Private helper fns.
.puggle <- function(bolddf, stat){
# Returns a bolddf full of stats following a bootstraping.
#
# A ugly imperative (but hopefully correct) implemention of a 
# bootstrap sampler. 
#
# Samples are drawn by sampling each joint of dataname+cond.  
# By sampling this way, rather than on the data directly, I 
# maintain trial-level timecourses while creating a NULL for comparison
# in the plots this fn is used in.
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
    shuffled_id_code <- sample(id_code, replace=TRUE)
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
    bolddf_null <- bolddf.stat(newbolddf, stat)
}


make.stat.boldf <- function(stat) {
# NOTE: stat must a be stat(x) signature function.
# Converts any stat function that takes a vextor or list
# into a function that works magically with boldf objects.
    function(bolddf) { stat(bolddf$data) }
}

bolddf.stat <- function(bolddf, stat){
# NOTE: stat must a be stat(x) signature function.
# Slice along bolddf$index calculating stats and return
# them in a bolddf.
    bolddf_stat <-  ddply(
            bolddf,
            .(voxel, dataname, cond), 
            make.stat.bolddf(stat))  ## Wont have an index, add one...
    bolddf_stat[["index"]] <- rep(1, nrow(bolddf_stat))
    colnames(bolddf_stat) <- c("voxel", "dataname", "cond", "data", "index")
    bolddf_stat
}

bolddf.nulldist <- function(bolddf, stat, ninter=100, seed=42){
# Generate voxel level null distributions for stat.
    set.seed(seed)

    bolddf_null <- ddply(bolddf, .(voxel), .puggle, stat)
    colnames(bolddf_null) <- c("voxel", "dataname", "cond", "data")
    for (i in 1:(ninter-1)){
        # Generate this iterations nulls dist samples, 
        # and average them with the rest.

        # Create this iters data
        bolddf_null_i <- ddply(bolddf, .(voxel), .puggle, stat)
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

bolddf.binarize <- function(bolddf, threshold){
# Convert invididual TRs (in bolddf$data) into binary coding
# based on threshold.
    bolddf$data <- aaply(
            bolddf$data, 1, 
            function(x) { 
                if (is.na(x) || is.null(x)) { return(0) }
                else if (x >= threshold) { return(1) } 
                else { return(0)} })
    bolddf
}

bolddf.stat.binarize <- function(bolddf, stat, threshold){
    bolddf.binarize(bolddf.stats(bolddf, stat), threshold)
}
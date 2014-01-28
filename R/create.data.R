# As there is a standard set of metadata for all timecourse plots
# and data requirements all plot and stat routines use a common data
# structure called 'bolddf'.  It is a data.frame with ONLY the 
# following colnames: ("voxel", "dataname", "cond", "index", "value").
create.bolddf <- function(df, datarange, index, cond, dataname) {
	
    # Copy over the voxel data
    # Then the metadata
	bolddf <- as.data.frame(df[ ,datarange])
	bolddf[["index"]] <- df[[index]]
    bolddf[["dataname"]] <- df[[dataname]]
    
    # Convert NA and NaN to no_label along the way
    nnewcond <- as.character(df[[cond]])
    nnewcond[nnewcond == "NA"] <- "no_label"
    nnewcond[nnewcond == "NaN"] <- "no_label"
	bolddf[["cond"]] <- as.factor(nnewcond)
	
    # Combine voxels, keeping the metadata tied
	bolddf <- melt(bolddf, id.var = c("dataname", "cond", "index"))
	colnames(bolddf) <- c("dataname", "cond", "index", "voxel", "data")
    bolddf[["data"]] <- as.numeric(bolddf[["data"]])  ## Ensure not str
    
	bolddf
}


read.boldfs <- function(files, datarange, index, cond, dataname) {
    bolddf <- NULL
    for (file in files){
        bolddf <- rbind(bolddf,
                create.bolddf(read.table(file, sep=",", header=TRUE), 
                datarange=datarange, index=index, cond=cond, dataname=dataname))
    }
    bolddf
}

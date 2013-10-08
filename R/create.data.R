# As there is a standard set of metadata for all timecourse plots
# and data requirements all plot and stat routines use a common data
# structure called 'bolddf'.  It is a data.frame with ONLY the 
# following colnames: ("voxel", "dataname", "cond", "index", "value").
create.bolddf <- function(df, datarange, index, cond, dataname) {
	
	bolddf <- df[ ,datarange]          ## Copy over the voxel data
	bolddf[["index"]] <- df[[index]]   ## Then the metadata
	bolddf[["cond"]] <- df[[cond]]
	bolddf[["dataname"]] <- df[[dataname]]
	
	bolddf <- melt(bolddf, id.var = c("dataname", "cond", "index"))
	colnames(bolddf) <- c("dataname", "cond", "index", "voxel", "data")
    bolddf[["data"]] <- as.numeric(bolddf[["data"]])  ## Ensure not str
    
	bolddf
}

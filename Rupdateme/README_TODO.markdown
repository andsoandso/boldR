read.timecourse(table)

Format of data table.

Required
--------
1. Voxel data is in congtious columns that must be specified with a range (e.g. 1:42).
	
	range - a range of ints, locating the voxel/ROI data

2. There are at least three metadata columns.
	
	index - a index of TRs inside single trials, i.e. the timecourse index 
		(default: 'timecourse_index')
	roiname - the name of the ROI this data belongs to (default: 'roiname')
	cond - the experimental condition (default: 'cond')

Note: the above names are the default, but can be changed on data read if desired.

Optional
--------
3. There may also be a mean column, the average of the voxels ('vox_mean')
4. There may be additional metadata columns, which could for example join bilateral ROIs, or offer additional condition information.


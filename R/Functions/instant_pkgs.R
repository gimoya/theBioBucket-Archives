# Script name: instant_pkgs.R
# Purpose: Package installation and loading
# Author: Kay Cichini
# Date: 2012-06-19
# Licence: cc by-nc-sa

instant_pkgs <- function(pkgs) { 
    pkgs_miss <- pkgs[which(!pkgs %in% installed.packages()[, 1])]
    if (length(pkgs_miss) > 0) {
        install.packages(pkgs_miss)
    }
    
	if (length(pkgs_miss) == 0) {
	message("\n ...Packages were already installed!\n")
	}
	
    # install packages not already loaded:
    pkgs_miss <- pkgs[which(!pkgs %in% installed.packages()[, 1])]
    if (length(pkgs_miss) > 0) {
        install.packages(pkgs_miss)
    }
    
    # load packages not already loaded:
    attached <- search()
    attached_pkgs <- attached[grepl("package", attached)]
    need_to_attach <- pkgs[which(!pkgs %in% gsub("package:", "", attached_pkgs))]
    
    if (length(need_to_attach) > 0) {
      for (i in 1:length(need_to_attach)) require(need_to_attach[i], character.only = TRUE)
    }
	
	if (length(need_to_attach) == 0) {
	message("\n ...Packages were already loaded!\n")
	}
}

# Examples:
instant_pkgs(c("base", "jpeg"))
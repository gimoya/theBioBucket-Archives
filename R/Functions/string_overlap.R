wordsim <- function(word1, word2) {
    
    spl1 <- tolower(unique(unlist(strsplit(word1, ""))))
    spl2 <- tolower(unique(unlist(strsplit(word2, ""))))
    all <- unique(c(spl1, spl2))
    n_spl1 <- length(spl1)
    n_spl2 <- length(spl2)
    n_all <- n_spl1 + n_spl2
    n_uni_all <- length(all)
    
    # overlap
    n_both <- n_all - n_uni_all  # a
    
    # percentage of shared unique letters
    if (Index == "P") 
        return(n_both/n_uni_all)
}

wordsim("abcdefgh", "abcdefgh")





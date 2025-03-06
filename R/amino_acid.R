#' amino acid composition analyses helpers
#'
#' @importFrom Biostrings AA_STANDARD
#' @importFrom stringr str_count
#' @export

# group by, assay, isotype outcome, timepoint. summarize aa composition.
aa_comp <- function(seqs, aa_dict = Biostrings::AA_STANDARD){
  # take as input a character vector of amino acid sequences and count occurance of each standard amino acid

  # create a data frame. col1 seq with input sequences. col 2:21 each one amino acid and related count
  seqs %>% lapply(function(x){
    aa_dict %>% sapply(function(y){
      stringr::str_count(x,y)
    }) %>% t %>%  as.data.frame()
  }) %>% setNames(seqs) %>% bind_rows(.id = "seq")
}


# and group by similarity
aa_order <- tibble::tibble(
  aa = c("G", "A", "V", "L", "M", "I",
         "F", "Y", "W",
         "S", "T", "C",  "N", "Q", "P",
         "H", "K", "R",
         "D", "E"),
  aa_class = c(rep("Nonpolar \nAliphatic", 6), rep("Nonpolar \nAromatic", 3),
               rep("Polar \nUncharged", 6), rep("Positive \nCharge", 3), rep("Neg. \nCharge", 2)) %>%
    factor(levels = c("Nonpolar \nAliphatic", "Nonpolar \nAromatic", "Polar \nUncharged", "Positive \nCharge", "Neg. \nCharge"))
  )
#' vir_evidence
#' Consolidate virscan hits to evidence peptides. Adapted from https://pubmed.ncbi.nlm.nih.gov/26045439/
#' Group hit peptides by species. Rank species by number of hits.
#' Starting with species with most hits, for each hit peptide, remove other hit peptides with 7aa overlap
#' from lower hit-breadth species.
#'
#'
#' @param hit_ids vector of peptide ids enriched for sample
#' @param hit_species vector of viral species ids corresponding to hit_ids
#' @param alignment_matrix binary matrix indicating whether two peptides are similar (e.g. share 7aa ovlerap or blast alignment). matrix rownames and column names should correspond to peptide ids used in hit_ids. use either alignment_matrix or hit_aa
#' @param hit_aa amino acid sequences of peptides listed by hit_ids. use either hit_aa or alignment_matrix.
#' @param similarity_method method to use for characterizing similarity between peptide seuqneces provided by hit_aa.
#' @param similarity_thresh amino acid overlap threshold or blast e-value threshold to use for similarity cutoff.
#'
#' @export



vir_evidence <- function(hit_ids, hit_species,
                         alignment_matrix = NULL,
                         hit_aa = NULL,
                         similarity_method = c("overlap", "blast"),
                         similarity_thresh = 7){



}



if(FALSE){
  sample_index <- 10
  z_thresh <- 5
  this_sample <- sample_ids[sample_index]
  these_minz <- zscore_sample[,sample_index]
  pep_ids <- zscore_sample$id
  hit_ids <- pep_ids[these_minz > z_thresh]
  hit_species <- vir_annot$Species[match(hit_ids, vir_annot$id)]
  hit_aa <- vir_annot$Sequence[match(hit_ids, vir_annot$id)]


  # ---
  `%>%` <- magrittr::`%>%`
  hit_ids_byspecies <- hit_ids %>% split(hit_species)
  hits_perspecies <- hit_ids_byspecies %>% lapply(length)
  hit_ids_byspecies[hits_perspecies %>% unlist %>% order %>% rev]
  # rank()

  # how to handle ties (species with same number of hits? assign 0.5 evidence peptides?)
      # add parameter hit_score (e.g. enrichment) for how to handle ties
  # also application to phip seq. (human). group peptides by depmap association?


  # if aa sequences provided, generate alignment matrix
  # stringdist::stringdist(method = "lcs") #https://www.rdocumentation.org/packages/stringdist/versions/0.9.6.3/topics/stringdist
  # returns number characters not in lcs
  strings <- c("abcdef", "defghi")
  dist <- stringdist::stringdist(strings[1], strings[2], method = "lcs")
  lcs_length <- ((nchar(strings) %>% sum)-dist)/2


  # use alignment matrix to simplify to evidence peptides
    # take subset of alignment matrix for only hit peptides
    # for each hit peptide, remove similar hit peptides from lower priority viruses
      # handling ties! keep both, keep one randomly, etc. keep highest zscore
}
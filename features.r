# Load required libraries
library(dplyr)
library(tidyr)
library(Biostrings)
library(GenomicRanges)
library(BSgenome.Hsapiens.UCSC.hg38)

# Function to calculate k-mer frequencies
calculate_kmer_freq <- function(sequences, k = 3) {
  kmer_counts <- oligonucleotideFrequency(DNAStringSet(sequences), width = k)
  kmer_freq <- sweep(kmer_counts, 1, rowSums(kmer_counts), "/")
  return(kmer_freq)
}

# Function to calculate sequence complexity
calculate_complexity <- function(sequences) {
  complexity <- sapply(sequences, function(seq) {
    unique_chars <- length(unique(strsplit(seq, "")[[1]]))
    return(unique_chars / nchar(seq))
  })
  return(complexity)
}

calculate_complexity <- function(sequences) {
  complexity <- sapply(sequences, function(seq) {
    unique_chars <- length(unique(strsplit(seq, "")[[1]]))
    return(unique_chars / nchar(seq))
  })
  return(complexity)
}

calculate_dna_shape <- function(sequences) {
  dna_shape <- sapply(sequences, function(seq) {
    shape <- pred_DNA_shape(seq)
    return(c(
      mean_MGW = mean(shape$MGW),
      mean_Prot = mean(shape$Prot),
      mean_Roll = mean(shape$Roll),
      mean_HelT = mean(shape$HelT)
    ))
  })
  return(dna_shape)
}

calculate_gc_skew <- function(sequences) {
  gc_skew <- sapply(sequences, function(seq) {
    bases <- oligonucleotideFrequency(DNAString(seq), width = 1)
    return((bases["G"] - bases["C"]) / (bases["G"] + bases["C"]))
  })
  return(gc_skew)
}

calculate_entropy <- function(sequences) {
  entropy <- sapply(sequences, function(seq) {
    bases <- oligonucleotideFrequency(DNAString(seq), width = 1)
    props <- bases / sum(bases)
    return(-sum(props * log2(props), na.rm = TRUE))
  })
  return(entropy)
}

calculate_genomic_distances <- function(gr, features) {
  distances <- lapply(features, function(feat) {
    dist <- distanceToNearest(gr, feat)
    return(mcols(dist)$distance)
  })
  return(do.call(cbind, distances))
}

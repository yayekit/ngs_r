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
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

# Main feature engineering function
engineer_features <- function(data, genome = BSgenome.Hsapiens.UCSC.hg38) {
  # Convert sequences to GRanges object
  gr <- GRanges(
    seqnames = data$chromosome,
    ranges = IRanges(start = data$position, width = nchar(data$sequence)),
    strand = data$strand
  )
  
  kmer_freq <- calculate_kmer_freq(data$sequence)
  colnames(kmer_freq) <- paste0("kmer_", colnames(kmer_freq))
  
  complexity <- calculate_complexity(data$sequence)
  
  dna_shape <- calculate_dna_shape(data$sequence)
  
  gc_skew <- calculate_gc_skew(data$sequence)
  
  entropy <- calculate_entropy(data$sequence)
  
  tss <- promoters(genes(genome))
  cpg_islands <- cpgIslands(genome)
  repeat_masker <- rmsk(genome)
  
  genomic_distances <- calculate_genomic_distances(gr, 
                                                   list(TSS = tss, 
                                                        CpG_Islands = cpg_islands, 
                                                        Repeats = repeat_masker))
  colnames(genomic_distances) <- c("dist_to_TSS", "dist_to_CpG", "dist_to_repeat")
  
  # Combine all features
  features <- cbind(data, kmer_freq, complexity, dna_shape, gc_skew, entropy, genomic_distances)
  
  return(features)
}
library(Biostrings)
library(ShortRead)
#library(dplyr)
library(tidyr)
library(ggplot2)

load_fastq <- function(file_path) {
  fastq <- readFastq(file_path)
  return(fastq)
}

quality_check <- function(fastq) {
  qual <- quality(fastq)
  qual_stats <- as(qual, "matrix") %>%
    as.data.frame() %>%
    rownames_to_column("Position") %>%
    gather(key = "Statistic", value = "Value", -Position) %>%
    mutate(Position = as.numeric(Position))
  
  ggplot(qual_stats, aes(x = Position, y = Value, color = Statistic)) +
    geom_line() +
    theme_minimal() +
    labs(title = "Quality Scores Across All Bases",
         x = "Position in read (bp)",
         y = "Quality Score")
  
  ggsave("results/figures/quality_scores_plot.png")
  
  return(qual_stats)
}

trim_reads <- function(fastq, quality_threshold = 20, min_length = 50) {
  trimmed <- trimTails(fastq, k = quality_threshold, a = "mean")
  trimmed <- trimmed[width(trimmed) >= min_length]
  return(trimmed)
}

remove_adapters <- function(fastq, adapter_seq) {
  cleaned <- trimLRPatterns(Rpattern = DNAString(adapter_seq), subject = fastq)
  return(cleaned)
}
t
calculate_gc_content <- function(fastq) {
  gc_content <- letterFrequency(sread(fastq), letters = "GC", as.prob = TRUE)
  return(gc_content)
}

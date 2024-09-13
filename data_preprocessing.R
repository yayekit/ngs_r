library(Biostrings)
library(ShortRead)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tibble)

# Load a FASTQ file
load_fastq <- function(file_path) {
  fastq <- readFastq(file_path)
  return(fastq)
}

# Perform quality check and plot quality scores
quality_check <- function(fastq, output_plot = "results/figures/quality_scores_plot.png") {
  qual <- as(quality(fastq), "matrix")
  qual_df <- as.data.frame(qual)
  
  qual_stats <- qual_df %>%
    rowid_to_column("Position") %>%
    pivot_longer(-Position, names_to = "Read", values_to = "Quality") %>%
    group_by(Position) %>%
    summarise(
      Mean_Quality = mean(Quality),
      Median_Quality = median(Quality),
      Q1 = quantile(Quality, 0.25),
      Q3 = quantile(Quality, 0.75)
    )
  
  p <- ggplot(qual_stats, aes(x = Position, y = Mean_Quality)) +
    geom_line(color = "blue") +
    geom_ribbon(aes(ymin = Q1, ymax = Q3), fill = "grey80", alpha = 0.5) +
    theme_minimal() +
    labs(title = "Quality Scores Across All Bases",
         x = "Position in read (bp)",
         y = "Quality Score")
  
  ggsave(output_plot, plot = p)
  
  return(qual_stats)
}

# Trim low-quality bases from reads
trim_reads <- function(fastq, quality_threshold = 20, min_length = 50) {
  trimmed <- trimTailw(fastq, k = 2, a = quality_threshold, halfwidth = 2)
  trimmed <- trimmed[width(trimmed) >= min_length]
  return(trimmed)
}

# Remove adapter sequences from reads
remove_adapters <- function(fastq, adapter_seq) {
  cleaned_seq <- trimLRPatterns(Rpattern = DNAString(adapter_seq), subject = sread(fastq))
  cleaned_fastq <- ShortReadQ(sread = cleaned_seq, quality = quality(fastq), id = id(fastq))
  return(cleaned_fastq)
}

# Calculate GC content for each read
calculate_gc_content <- function(fastq) {
  gc_content <- rowSums(letterFrequency(sread(fastq), letters = c("G", "C"))) / width(fastq)
  return(gc_content)
}

# Convert FastQ object to data frame
fastq_to_dataframe <- function(fastq) {
  df <- data.frame(
    read_id = as.character(id(fastq)),
    sequence = as.character(sread(fastq)),
    gc_content = calculate_gc_content(fastq)
  )
  return(df)
}

# Main preprocessing function
preprocess_ngs_data <- function(input_file, output_file, adapter_seq = NULL, quality_threshold = 20, min_length = 50) {
  # Load FASTQ file
  raw_fastq <- load_fastq(input_file)
  
  # Perform quality check
  quality_stats <- quality_check(raw_fastq)
  
  # Trim low-quality bases
  trimmed_fastq <- trim_reads(raw_fastq, quality_threshold, min_length)
  
  # Remove adapters if provided
  if (!is.null(adapter_seq)) {
    cleaned_fastq <- remove_adapters(trimmed_fastq, adapter_seq)
  } else {
    cleaned_fastq <- trimmed_fastq
  }
  
  # Convert to data frame
  processed_data <- fastq_to_dataframe(cleaned_fastq)
  
  # Save processed data
  write.csv(processed_data, output_file, row.names = FALSE)
  
  return(processed_data)
}

# Example usage
if (interactive()) {
  input_file <- "data/raw/sample_ngs_data.fastq"
  output_file <- "data/processed/processed_ngs_data.csv"
  adapter_seq <- "AGATCGGAAGAGC"  # Example adapter sequence
  
  processed_data <- preprocess_ngs_data(input_file, output_file, adapter_seq)
  print(head(processed_data))
}

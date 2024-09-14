# Load necessary libraries
library(Biostrings)
library(ShortRead)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tibble)

# Load a FASTQ file with error handling
load_fastq <- function(file_path) {
  if (!file.exists(file_path)) {
    stop(paste("The file", file_path, "does not exist."))
  }
  fastq <- readFastq(file_path)
  return(fastq)
}

# Perform quality check and plot quality scores
quality_check <- function(fastq, output_plot = NULL) {
  # Extract quality scores
  qual <- as(quality(fastq), "matrix")
  
  # Compute statistics
  qual_stats <- data.frame(
    Position = 1:ncol(qual),
    Mean_Quality = colMeans(qual),
    Median_Quality = apply(qual, 2, median),
    Q1 = apply(qual, 2, quantile, probs = 0.25),
    Q3 = apply(qual, 2, quantile, probs = 0.75)
  )
  
  # Generate plot
  p <- ggplot(qual_stats, aes(x = Position, y = Mean_Quality)) +
    geom_line(color = "blue") +
    geom_ribbon(aes(ymin = Q1, ymax = Q3), fill = "grey80", alpha = 0.5) +
    theme_minimal() +
    labs(
      title = "Quality Scores Across All Bases",
      x = "Position in Read (bp)",
      y = "Quality Score"
    )
  
  # Save plot if output path is provided
  if (!is.null(output_plot)) {
    ggsave(output_plot, plot = p, width = 8, height = 6)
  }
  
  return(list(stats = qual_stats, plot = p))
}

# Trim low-quality bases from reads
trim_reads <- function(fastq, quality_threshold = 20, min_length = 50) {
  # Trim low-quality tails
  trimmed <- trimTails(fastq, k = 2, a = quality_threshold)
  # Filter reads by minimum length
  trimmed <- trimmed[width(trimmed) >= min_length]
  return(trimmed)
}

# Remove adapter sequences from reads
remove_adapters <- function(fastq, adapter_seq) {
  # Allow for a small number of mismatches
  max_mismatch <- floor(0.1 * nchar(adapter_seq))
  cleaned_seq <- trimLRPatterns(
    Rpattern = DNAString(adapter_seq),
    subject = sread(fastq),
    max.Rmismatch = max_mismatch
  )
  # Update FastQ object
  cleaned_fastq <- ShortReadQ(
    sread = cleaned_seq,
    quality = quality(fastq),
    id = id(fastq)
  )
  return(cleaned_fastq)
}

# Calculate GC content for each read
calculate_gc_content <- function(fastq) {
  gc_content <- rowSums(
    letterFrequency(sread(fastq), letters = c("G", "C"), as.prob = TRUE)
  )
  return(gc_content)
}

# Convert FastQ object to data frame
fastq_to_dataframe <- function(fastq) {
  df <- data.frame(
    read_id = as.character(id(fastq)),
    sequence = as.character(sread(fastq)),
    gc_content = calculate_gc_content(fastq),
    stringsAsFactors = FALSE
  )
  return(df)
}

# Main preprocessing function
preprocess_ngs_data <- function(
  input_file,
  output_file,
  adapter_seq = NULL,
  quality_threshold = 20,
  min_length = 50,
  output_plot = NULL
) {
  # Load FASTQ file
  raw_fastq <- load_fastq(input_file)
  
  # Perform quality check
  qc_results <- quality_check(raw_fastq, output_plot)
  
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
  
  return(list(
    data = processed_data,
    quality_stats = qc_results$stats,
    quality_plot = qc_results$plot
  ))
}

# Example usage
if (interactive()) {
  input_file <- "data/raw/sample_ngs_data.fastq"
  output_file <- "data/processed/processed_ngs_data.csv"
  adapter_seq <- "AGATCGGAAGAGC"  # Example adapter sequence
  output_plot <- "results/figures/quality_scores_plot.png"
  
  results <- preprocess_ngs_data(
    input_file,
    output_file,
    adapter_seq,
    quality_threshold = 20,
    min_length = 50,
    output_plot = output_plot
  )
  
  # Display the first few rows of the processed data
  print(head(results$data))
}

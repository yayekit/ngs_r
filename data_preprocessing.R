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

fastq_to_dataframe <- function(fastq) {
  df <- data.frame(
    read_id = id(fastq),
    sequence = as.character(sread(fastq)),
    quality = as.character(quality(fastq)),
    gc_content = calculate_gc_content(fastq)
  )
  return(df)
}

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
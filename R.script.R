# 1. Define function classify_gene
classify_gene <- function(logFC, padj) {
  if (logFC > 1 & padj < 0.05) {
    return("Upregulated")
  } else if (logFC < -1 & padj < 0.05) {
    return("Downregulated")
  } else {
    return("Not_Significant")
  }
}

# 2. Define input and output folders
input_files <- c("G:/AI_Omics_Internship_2025/Assignment 2/raw/DEGs_Data_1.csv", 
                 "G:/AI_Omics_Internship_2025/Assignment 2/raw/DEGs_Data_2.csv")


output_dir <- "G:/AI_Omics_Internship_2025/Assignment 2/Results"
if(!dir.exists(output_dir)){
  dir.create(output_dir)
}


for (file in input_files) {
  cat("\nProcessing:", file, "\n")
  
  # read dataset
  data <- read.csv(file , header = TRUE)
  
  # replace missing padj values with 1
  data$padj[is.na(data$padj)] <- 1
  
  # apply classify_gene() function
  data$status <- mapply(classify_gene, data$logFC, data$padj)
  
  # save processed file
  output_file_path <- file.path(output_dir, paste0("Processed_", basename(file)))
  write.csv(data, output_file_path, row.names = FALSE)
  
  # summary counts
  cat("Summary counts for", file, ":\n")
  print(table(data$status))
}













































































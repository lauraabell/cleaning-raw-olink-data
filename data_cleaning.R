library(dplyr)
library(ggplot2)

##importing olink dataset
rawdata <- read.csv("Anton_samples.csv")
View(rawdata)

##cleaning##

rawdata_columns_removed <- rawdata %>% select(-SampleType,
                                              -DataAnalysisRefID,
                                              -WellID, -AssayType,
                                              -Panel,
                                              -Block,
                                              -Normalization,
                                              -SoftwareVersion,
                                              -PanelDataArchiveVersion,
                                              -PreProcessingVersion,
                                              -PreProcessingSoftware,
                                              -InstrumentType,
                                              -UniProt,
                                              -OlinkID,
                                              -SoftwareName)
head(rawdata_columns_removed)

# Removing AssayQC and SampleQC warn
rawdata_columns_removed_qc_filtered <- rawdata_columns_removed %>%
  filter(AssayQC != "WARN")
unique(rawdata_columns_removed_qc_filtered$AssayQC)
rawdata_columns_removed_qc_filtered <- rawdata_columns_removed_qc_filtered %>%
  filter(SampleQC != "WARN")

length(unique(rawdata_columns_removed_qc_filtered$AssayQC))
length(unique(rawdata_columns_removed_qc_filtered$SampleQC))

# Removing control

clean_data <- rawdata_columns_removed_qc_filtered %>%
  filter(SampleID != "S61T1")

head(clean_data)

# renaming columns to make it clearer

analysis_data <- clean_data %>%
  mutate(Treatment = case_when(
    SampleID %in% c("S62T1") ~ "Control1",
    SampleID %in% c("S63T1") ~ "Treatment1",
    SampleID %in% c("S64T1") ~ "Control2",
    SampleID %in% c("S65T1") ~ "Treatment2",
    SampleID %in% c("S66T1") ~ "Control3",
    SampleID %in% c("S67T1") ~ "Treatment3",
    TRUE ~ NA_character_
  ))

View(analysis_data)
head(analysis_data)

#reorganising data to wide format

proteins <- wide_data$Assay

df <- as.data.frame(t(wide_data[, c(
  "Control1",
  "Treatment1",
  "Control2",
  "Treatment2",
  "Control3",
  "Treatment3"
)]))


colnames(df) <- proteins
head(df)

df$Experiment <- c(
  "Experiment 1",
  "Experiment 1",
  "Experiment 2",
  "Experiment 2",
  "Experiment 3",
  "Experiment 3"
)

df$Sample <- c(
  "Control",
  "Treatment",
  "Control",
  "Treatment",
  "Control",
  "Treatment"
)

View(df)

#checking stuff

na_counts <- colSums(is.na(df[, -(1:2)]))
na_counts[na_counts > 0]

sum(df == "", na.rm = TRUE)      
sum(df == "NA", na.rm = TRUE)    
sum(is.infinite(as.matrix(df[, -(1:2)]))) 


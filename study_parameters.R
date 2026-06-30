# PRIMA METHANE - study parameters
# Paula Rodriguez-Garcia
# 10-NOV-2025


# Directories -------------------------------------------------------------

## PRIMA Trial Master File
prima_tmf_dir      <- # path-to-dir
pm_onedrive_dir <- # path-to-dir


prima_tmf_data_dir <- glue::glue("{prima_tmf_dir}/Data")
prima_tmf_16S_dir <- glue::glue("{prima_tmf_data_dir}/16S")

analysis_dir    <- glue::glue("{pm_onedrive_dir}/prima-methane")


fig_dir <- glue::glue("{analysis_dir}/figures")
tab_dir <- glue::glue("{analysis_dir}/tables")
rds_dir <- glue::glue("{analysis_dir}/rds")
word_dir <- glue::glue("{analysis_dir}/word")



# rds files generated in this Rproject -------------------------------------------------------------------


## 1. Metadata
meta_all_filename   <- glue("{rds_dir}/019_metadata_all.rds")
meta_means_filename <- glue("{rds_dir}/019_metadata_means.rds")



## 2. Microbiome data
microbiome_mapping_filename <- glue("{rds_dir}/003_mapping_sequencing_info.rds")
taxonomic_annotation_filename <- glue("{rds_dir}/021_taxonomic_annotation.rds")


asv_counts_filename <- glue("{rds_dir}/021_asv_counts.rds") # counts at ASV level
asv_counts_means_filename <- glue("{rds_dir}/021_asv_counts_means.rds") # counts at ASV level, subject means
asv_rar_relabu_filename <- glue("{rds_dir}/021_asv_rar_relabu.rds") # rarefied rel abu at ASV level
asv_rar_relabu_means_filename <- glue("{rds_dir}/021_asv_rar_relabu_means.rds") # rarefied rel abu at ASV level, subject means


genus_counts_filename <- glue("{rds_dir}/021_genus_counts.rds") # counts at genus level 
genus_counts_means_filename <- glue("{rds_dir}/021_genus_counts_means.rds") # counts at genus level, subject means
genus_rar_relabu_filename <- glue("{rds_dir}/021_genus_rar_relabu.rds") # rarefied rel abu at genus level 
genus_rar_relabu_means_filename <- glue("{rds_dir}/021_genus_rar_relabu_means.rds") # rarefied rel abu at genus level, subject means


family_counts_filename <- glue("{rds_dir}/021_family_counts.rds") # counts at family level 
family_counts_means_filename <- glue("{rds_dir}/021_family_counts_means.rds") # counts at family level, subject means
family_rar_relabu_filename <- glue("{rds_dir}/021_family_rar_relabu.rds") # rarefied rel abu at family level 
family_rar_relabu_means_filename <- glue("{rds_dir}/021_family_rar_relabu_means.rds") # rarefied rel abu at family level, subject means




## 3. Metabolomics data

fecal_scfa_filename <- glue("{rds_dir}/030_fecal_scfa.rds")
fecal_scfa_dry_filename <- glue("{rds_dir}/030_fecal_scfa_dry.rds")
plasma_scfa_filename <- glue("{rds_dir}/030_plasma_scfa.rds")
plasma_glp1_filename <- glue("{rds_dir}/030_plasma_glp1.rds")

fecal_scfa_means_filename <- glue("{rds_dir}/030_fecal_scfa_means.rds")
fecal_scfa_dry_means_filename <- glue("{rds_dir}/030_fecal_scfa_dry_means.rds")
plasma_scfa_means_filename <- glue("{rds_dir}/030_plasma_scfa_means.rds")
plasma_glp1_means_filename <- glue("{rds_dir}/030_plasma_glp1_means.rds")



# Colors ------------------------------------------------------------------

study_colors <- list(
  "methane" = list("HMP" = "#1f78b4",
                   "LMP" = "#ff7f00"),
  
  "Methane_group" = list("HMP" = "#1f78b4",
                         "LMP" = "#ff7f00"),
  
  "Methane_TT_2group" = list("HMP" = "#1f78b4",
                         "LMP" = "#ff7f00"),
  
  "Methane_TT_3group" = list("HMP-HTT" = "#01386A",
                             "HMP-NTT" = "#1f78b4",
                             "LMP-NTT" = "#ff7f00"),
  
  "correlation" = list("low" = "#5B2A86", 
                       "mid" = "#ffffff",
                       "high" = "#1b7837")
)
  
  

# Common vectors ----------------------------------------------------------

groups <- c("LMP", "HMP")

ph_vars <- c("ph_stomach",
             "ph_duodenum",
             "ph_small_bowel",
             "ph_colon",
             "ph_proximal_colon",
             "ph_distal_colon" ,
             "ph_sigmoid_colon",
             "ph_rectum")

tt_vars <- c("GET", "SBTT", "CTT", "SLBTT", "WGTT")


# Significance asterisks --------------------------------------------------

# P
asterisks_p <- list(
  breaks = c(0, 1e-04, 0.001, 0.01, 0.05, 1),
  labels = c("****", "***", "**", "*", "ns")
  )

# FDR
asterisks_fdr <- list(
  breaks = c(0, 0.001, 0.01, 0.05, 0.1, 1),
  labels = c("****", "***", "**", "*", "ns"))


# Functions ---------------------------------------------------------------

get_significance_asterisks <- function(x, 
                                       asterisks_list) {
  as.character(
    cut(
      x,
      breaks = asterisks_list$breaks,
      labels = asterisks_list$labels,
      include.lowest = TRUE,
      right = FALSE
    )
  )
}

remove_all_na_rows <- function(df, except_cols) {
  
  df %>%
    filter(
      !if_all(
        -all_of(except_cols),
        is.na
      )
    )
}

count_how_many_zero_per_column <- function (data){
  sapply(data, function(x) sum(x == 0, na.rm = T))
}

count_how_many_below_value_per_column <- function (data, threshold) 
{
  sapply(data, function(x) sum(data < threshold, na.rm = T))
}

scientific_notation_fun <- function(x) {
  parse(text = gsub(
    "e\\+?", "%*%10^",
    formatC(x, format = "e", digits = 1)
  ))
}





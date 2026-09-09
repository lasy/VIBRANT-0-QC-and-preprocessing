### Use Anaconda prompt or Positron

##### Create the snakemake environment based on the yml file that declares the dependencies
#conda env create -f NAME OF ENVI FILE.yml

##### Activate snakemake environment
#conda activate snakemake


##### Install quarto in the Snakemake environment 
#conda install -c conda-forge quarto  ### not needed ?

### If I add dependencies in the yml file, to update the environment
# conda env update -f NAME OF ENVI FILE.yml

##### From the command line, run:
#    snakemake -s NAME OF SNAKEFILE.smk -c all

#### To do a dry run 
# XXX 

### When executing with OneDrive
# snakemake -s snakefile.smk -c all --nolock


### if need to remove & re-create the envi
# conda deactivate
# conda env remove -n snakemake
# conda env create -f NOM_ENVI_FILE.yml

########## CREATE names for path

import getpass
import glob

# get user 
USER = getpass.getuser()

## Dropbox repo
DROPBOX_PATHS = {
    #XXXX here add the data paths
  "elise": "/mnt/c/Users/elibertrand/Dropbox/VIBRANT Study Files - Copie/90_VIBRANT_consolidated_data/" 
}

if USER not in DROPBOX_PATHS:
    raise ValueError(
        f"User '{USER}' unknown in DROPBOX_PATHS. "
        f"Need to add user's path above"
    )

## OneDrive paths
ONE_DRIVE_PATHS = {
    #XXXX here add the data paths,
    "elise": "/mnt/c/Users/elibertrand/OneDrive - UCL/Documents/Projets/VIBRANT/"
}

if USER not in ONE_DRIVE_PATHS:
    raise ValueError(
        f"User '{USER}' unknown in ONE_DRIVE_PATHS. "
        f"Need to add user's path above"
    )

#define main onedrive path
ONE_DRIVE_PATH = ONE_DRIVE_PATHS[USER]

#define onedrive clinical data path
ONE_DRIVE_CLINICAL_DATA = {
    #XXXX here add the data paths,
    "elise": f"{ONE_DRIVE_PATH}Fichiers de Laura Symul - VIBRANT clinical data UCLouvain - Copie/"
}

if USER not in ONE_DRIVE_CLINICAL_DATA:
    raise ValueError(
        f"User '{USER}' unknown in ONE_DRIVE_CLINICAL_DATA. "
        f"Need to add user's path above"
    )

#define onedrive data uclouvain path
ONE_DRIVE_DATA_UCLOUVAIN = {
    #XXXX here add the data paths,
    "elise": f"{ONE_DRIVE_PATH}Fichiers de Laura Symul - VIBRANT data UCLouvain - Copie/"
}

if USER not in ONE_DRIVE_DATA_UCLOUVAIN:
    raise ValueError(
        f"User '{USER}' unknown in ONE_DRIVE_DATA_UCLOUVAIN. "
        f"Need to add user's path above"
    )

### HERE DEFINE THE DATA PATH VARIABLES

#Dropbox
DROPBOX_PATH = DROPBOX_PATHS[USER]
# MG_DIR = f"{DROPBOX_PATH}02 Metagenomics/" # to check when access to Dropbox but is used in qmd 02
# QPCR_DIR = f"{DROPBOX_PATH}03 qPCR/" # to check when access to Dropbox but is used in qmd 03
# LUMINEX_DIR = f"{DROPBOX_PATH}05 Luminex/" # to check when access to Dropbox but is used in qmd 10
# FLOW_DIR = f"{DROPBOX_PATH}06 Flow cytometry/" # to check when access to Dropbox but is used in qmd 11
# QPCR_DAILY_DIR = f"{DROPBOX_PATH}14_VIBRANT qPCR/20250818/" # to check when access to Dropbox but is used in qmd 30
# ARM_DIR = f"{DROPBOX_PATH}Unblinding/" # to check when access to Dropbox but is used in qmd 92


#OneDrive (also see what we did before)
ONE_DRIVE_CLINICAL_DATA_RAW = f"{ONE_DRIVE_CLINICAL_DATA[USER]}/Data/raw 20250807/"

#Get Output dir
ONE_DRIVE_OUTPUT_DIR = f"{ONE_DRIVE_DATA_UCLOUVAIN[USER]}actual data/"
ONE_DRIVE_OUTPUT_DIR_1 = f"{ONE_DRIVE_OUTPUT_DIR}01 Preprocessed and QCed/"
# ONE_DRIVE_OUTPUT_DIR_2 = f"{ONE_DRIVE_OUTPUT_DIR}02 MAEs/"
# ONE_DRIVE_OUTPUT_DIR_3 = f"{ONE_DRIVE_OUTPUT_DIR}03 QCed MAEs/"
# ONE_DRIVE_OUTPUT_DIR_4 = f"{ONE_DRIVE_OUTPUT_DIR}04 unblinded MAEs/"
# ONE_DRIVE_OUTPUT_DIR_5 = f"{ONE_DRIVE_OUTPUT_DIR}05 augmented MAEs/"
# ONE_DRIVE_OUTPUT_DIR_6 = f"{ONE_DRIVE_OUTPUT_DIR}06 subsetted MAEs/"
# ONE_DRIVE_OUTPUT_DIR_7 = f"{ONE_DRIVE_OUTPUT_DIR}07 Zenodo exports/"
# OUTPUT_FLOW_DIR = f"{ONE_DRIVE_OUTPUT_DIR_1}06 Flow cytometry tmp files/" #not sure it exists at the end of the process!
# OUTPUT_FLOW_DIR2 = f"{OUTPUT_FLOW_DIR}MGH/step 02/"
# OUTPUT_FLOW_DIR3 = f"{OUTPUT_FLOW_DIR}MGH/step 03/"

# Get today's date for the file name with today's date
from datetime import date
TODAY = date.today().strftime("%Y%m%d")

######################################################################################
## onstart : 
## Run once before all rules are applied, prepare the environment
## Create 2 folders : a hidden folder required by Quarto to work & a folder for logs
######################################################################################

onstart:
    import os
    os.makedirs(".quarto", exist_ok=True)
    os.makedirs("logs", exist_ok=True)
    open(".quarto/idx", "a").close()

###### Writes on a log file whether the workflow failed or not on a specific date.

onsuccess:
    from datetime import datetime
    with open("logs/workflow.log", "a") as f:
        f.write(f"Workflow ended successfully on {datetime.now().strftime('%Y-%m-%d à %H:%M:%S')}\n")

onerror:
    from datetime import datetime
    with open("logs/workflow.log", "a") as f:
        f.write(f"Workflow failed on {datetime.now().strftime('%Y-%m-%d à %H:%M:%S')}\n")


#################################################################################################
## rule all : 
## final target of the workflow
## Snakemake always reads this rule first and resolves dependencies to determine what to run
#################################################################################################
rule all:
    input:
      # Output rule 01a - CRF data cleaning
      Rdata1 = f"{ONE_DRIVE_OUTPUT_DIR_1}01_crf_raw_{TODAY}.Rdata",
      Rdata2 = f"{ONE_DRIVE_OUTPUT_DIR_1}01_crf_clean_{TODAY}.Rdata",
      Rdata3 = f"{ONE_DRIVE_OUTPUT_DIR_1}01_exposures_{TODAY}.Rdata",
      Rdata4 = f"{ONE_DRIVE_OUTPUT_DIR_1}01_crf_plates_dictionary_{TODAY}.Rdata",
      html = "01a-clinical-CRF-data-cleaning.html"
      

#######################################################################
## Now define rules
#######################################################################

########## Rule 01a - CRF data cleaning

rule render_01a_clinical_data_formating:
  input:
      qmd = "01a-clinical-CRF-data-cleaning.qmd",
      Rdata = f"{ONE_DRIVE_CLINICAL_DATA_RAW}cap068_ecrf_data_final.RData"
  output:
    Rdata1 = f"{ONE_DRIVE_OUTPUT_DIR_1}01_crf_raw_{TODAY}.Rdata",
    Rdata2 = f"{ONE_DRIVE_OUTPUT_DIR_1}01_crf_clean_{TODAY}.Rdata",
    Rdata3 = f"{ONE_DRIVE_OUTPUT_DIR_1}01_exposures_{TODAY}.Rdata",
    Rdata4 = f"{ONE_DRIVE_OUTPUT_DIR_1}01_crf_plates_dictionary_{TODAY}.Rdata",
    html = "01a-clinical-CRF-data-cleaning.html"
  log:
      "logs/01a-clinical-CRF-data-cleaning.log"
  shell:
      "quarto render {input.qmd} 2> {log}"



########## Rule 01b - data augmentation

# rule render_01b_data_augmentation:
#     input:
#         qmd = "01b-clinical-CRF-data-augmentation.qmd",
#         Rdata1 = f"{ONE_DRIVE_OUTPUT_DIR_1}01_crf_clean_{TODAY}.Rdata", 
#         Rdata2 = f"{ONE_DRIVE_OUTPUT_DIR_1}01_exposures_{TODAY}.Rdata"
#     output:
#         Rdata3 = f"{ONE_DRIVE_OUTPUT_DIR_1}01_participants_{TODAY}.Rdata",
#         Rdata4 = f"{ONE_DRIVE_OUTPUT_DIR_1}01_participants_variable_dictionary_{TODAY}.Rdata",
#         Rdata5 = f"{ONE_DRIVE_OUTPUT_DIR_1}01_visits_long_{TODAY}.Rdata",
#         Rdata6 = f"{ONE_DRIVE_OUTPUT_DIR_1}01_visits_{TODAY}.Rdata",
#         Rdata7 = f"{ONE_DRIVE_OUTPUT_DIR_1}01_visits_variable_dictionary_{TODAY}.Rdata",
#         html = "01b-clinical-CRF-data-augmentation.html"
#     log:
#         "logs/01b-clinical-CRF-data-augmentation.log"
#     shell:
#         "quarto render {input.qmd} 2> {log}"
# 
# 
# ########## Rule 01c - merge participants & visits table
# 
# rule render_01c_merge_participants_visits_table
#     input:
#         qmd = "01c-clinical-CRF-merged-participants-and-visits-table.qmd",
#         Rdata1 = f"{ONE_DRIVE_OUTPUT_DIR_1}01_crf_clean_{TODAY}.Rdata"
#     output:
#         Rdata2 = f"{ONE_DRIVE_OUTPUT_DIR_1}01_participant_crfs_merged_{TODAY}.Rdata",
#         Rdata3 = f"{ONE_DRIVE_OUTPUT_DIR_1}01_visits_crfs_merged_{TODAY}.Rdata",
#         html = "01c-clinical-CRF-merged-participants-and-visits-table.html"
#     log:
#         "logs/01c-clinical-CRF-merged-participants-and-visits-table.log"
#     shell:
#         "quarto render {input.qmd} 2> {log}"
# 
# 
# ########## Rule 02 - metagenomics taxonomic compo
# 
# rule render_02_metagenomics_taxonomic_compo
#     input:
#         qmd = "02-metagenomics-taxonomic-composition.qmd",
#         csv1 = f"{MG_DIR}MVIBR_kSanityVIRGO2_ReadCounts_20250611.csv",
#         csv2 = f"{MG_DIR}MVIBR_kSanityVirgo2_GLcorr_20250611.csv",
#         csv3 = f"{MG_DIR}MVIBR_kSanityVirgo2_relAbund_20250611.csv",
#         csv4 = f"{MG_DIR}VIBRANT_MG_technicalMetaData_20250611.csv",
#         csv5 = f"{MG_DIR}VIRGO2_taxonomy_key_250429.csv",
#         xlsx = f"{RAW_DATA}00 Trial Data/IsolateNumbers.xlsx", 
# 
#         Rdata1 = f"{ONE_DRIVE_OUTPUT_DIR_1}01_crf_clean_{TODAY}.Rdata"
# 
#         ### Also : 01_visits ? 
#         ### Also : 01_participants ?
#     output:
#         rds1 = f"{ONE_DRIVE_OUTPUT_DIR_1}02_se_mg_TODAY.rds",
#         rds2 = f"{ONE_DRIVE_OUTPUT_DIR_1}02_technical_metadata_agg_TODAY.rds",
#         Rdata4 = f"{ONE_DRIVE_OUTPUT_DIR_1}02_se_mg_{TODAY}.Rdata",
#         html = "02-metagenomics-taxonomic-composition.html", 
# 
#         #### ALSO : MG_final_rel_ab_for_Michael_to_check.csv ??
#     log:
#         "logs/02-metagenomics-taxonomic-composition.log"
#     shell:
#         "quarto render {input.qmd} 2> {log}"
# 
# 
# ########## Rule 03 - qPCR
# 
# rule render_03qPCR
#     input:
#         qmd = "03-qPCR.qmd",
#         csv1 = f"{QPCR_DIR}VIBRANT_qPCR_data_merged_250611.csv",
#         csv2 = f"{QPCR_DIR}VIBRANT_16SqPCR_250616.csv",
#         rds1 = f"{ONE_DRIVE_OUTPUT_DIR_1}02_se_mg_TODAY.rds",
#         rds2 = f"{ONE_DRIVE_OUTPUT_DIR_1}02_technical_metadata_agg_TODAY.rds"
#     output:
#         rds3 = f"{ONE_DRIVE_OUTPUT_DIR_1}03_se_pcr_raw_TODAY.rds",
#         rds4 = f"{ONE_DRIVE_OUTPUT_DIR_1}03_se_pcr_agg_TODAY.rds",
#         html = "03-qPCR.html"
#     log:
#         "logs/03-qPCR.log"
#     shell:
#         "quarto render {input.qmd} 2> {log}"
# 
# ########## Rule 04 - 16S rRNA amplicon sequencing
# 
# rule render_04_16S_rRNA_amplicon_sequencing
#     input:
#         qmd = "04-16S-rRNA-amplicon-sequencing.qmd",
#         rds1 = f"{DATA_DROPBOX}04 16S rRNA sequencing/raw_merged_all_pools_ps_20250523.rds",
#         RData1 = f"{DATA_PROCESSED_ONEDRIVE}visits_summary.RData"
#     output:
#         rds2 = f"{ONE_DRIVE_OUTPUT_DIR_1}04_se_16S_ASV_all_TODAY.rds",
#         rds3 = f"{ONE_DRIVE_OUTPUT_DIR_1}04_se_16S_ASV_TODAY.rds",
#         rds4 = f"{ONE_DRIVE_OUTPUT_DIR_1}04_se_16S_agg_all_TODAY.rds",
#         rds5 = f"{ONE_DRIVE_OUTPUT_DIR_1}04_se_16S_agg_TODAY.rds",
#         html = "04-16S-rRNA-amplicon-sequencing.html"
#     log:
#         "logs/04-16S-rRNA-amplicon-sequencing.log"
#     shell:
#         "quarto render {input.qmd} 2> {log}"
# 
# ########## Rule 10 - luminex
# 
# rule render_10_luminex
#     input:
#         qmd = "10-luminex.qmd",
#         xlsx1 = f"{LUMINEX_DIR}??? 0-9 files to load ??" #TO DO when access to the dropbox
#     output:
#         rds1 = f"{ONE_DRIVE_OUTPUT_DIR_1}05_se_luminex_raw_TODAY.rds",
#         rds2 = f"{ONE_DRIVE_OUTPUT_DIR_1}05_se_luminex_agg_TODAY.rds",
#         html = "10-luminex.html"
#     log:
#         "logs/10-luminex.log"
#     shell:
#         "quarto render {input.qmd} 2> {log}"
# 
# ########## Rule 11 - flow cytometry - to check!!
# 
# rule render_11_flow_cytometry
#     input:
#         qmd = "11-flow-cytometry.qmd",
#         csv1 = f"{VIBRANT_STUDY_FILES}06 Flow cytometry/MGH_sample_list_with_machine_no_dates.csv", 
#         csv2 = TO FILL IN!!! (see line 88 from the qmd),
#         csv3 = f"{OUTPUT_FLOW_DIR}MGH/step 01/mgh_sample_inventory.csv"
#         #NOT SURE I HAVE EVERYTHING HERE!
#     output:
#         csv4 = f"{OUTPUT_FLOW_DIR2}missing_mgh_samples.csv",
#         csv5 = f"{OUTPUT_FLOW_DIR2}mgh_sample_inventory_cleaned.csv",
#         csv6 = f"{OUTPUT_FLOW_DIR3}mgh_sample_inventory_cleaned.csv",
#         rds1 = f"{ONE_DRIVE_OUTPUT_DIR_1}06_se_flow_TODAY.rds",
#         csv7 = f"{ONE_DRIVE_OUTPUT_DIR_1}flow_data_availability_check.csv",
#         html = "11-flow-cytometry.html"
#         #NOT SURE I HAVE EVERYTHING HERE!
#     log:
#         "logs/11-flow-cytometry.log"
#     shell:
#         "quarto render {input.qmd} 2> {log}"
# 
# ########## Rule 30 - qPCR daily
# 
# rule render_30_qPCR_daily
#     input:
#         qmd = "30-qPCR-daily.qmd",
#         csv1 = f"{QPCR_DAILY_DIR}VIBRANT_qPCR_data_merged_081825.csv", 
#         rds1 = f"{ONE_DRIVE_OUTPUT_DIR_1}02_se_mg_TODAY.rds",
#         rds2 = f"{ONE_DRIVE_OUTPUT_DIR_1}04_se_16S_raw_TODAY.rds",
#         rds3 = f"{VIBRANT_STUDY_FILES}04 16S rRNA sequencing/raw_merged_all_pools_ps_20250523.rds",
#     output:
#         rds4 = f"{ONE_DRIVE_OUTPUT_DIR_1}30_se_pcr_raw_TODAY.rds",
#         rds5 = f"{ONE_DRIVE_OUTPUT_DIR_1}30_se_pcr_agg_TODAY.rds",
#         #html = "30-qPCR-daily.html"
#     log:
#         "logs/30-qPCR-daily.log"
#     shell:
#         "quarto render {input.qmd} 2> {log}"
# 
# 
# ########## Rule 90 - MAE integration
# 
# rule render_90_MAE_integration
#     input:
#         qmd = "90-MAE-integration.qmd",
#         ### how to ?
#         ## load crf_clean_file
#         ## load crf_raw_file
#         ## load crf_dict_file,
#         ## 01_participants_TODAY, 
#         ## 01_participants_variable_dictionary_TODAY, 
#         ## 01_participant_crfs_merged_TODAY,
#         ##01_visits_TODAY, 
#         ## 01_visits_long_TODAY,
#         ## 01_visits_crfs_merged_TODAY, 
#         ## 01_exposures_
#         ## The SE objects in get_01_ONE_DRIVE_OUTPUT_DIR
#         ## Metagenomics data - mg_file
#         ##qPCR data - qPCR files
#         ##qPCR daily data - daily_qPCR_file
#         ##16s rRNA data - amplicon_ASV_file & amplicon_file
#         ## Luminex data - luminex_file
#         ## Flow cytometry data - flow_file
#     output:
#         rds1 = f"{ONE_DRIVE_OUTPUT_DIR_2}mae_full_TODAY.rds",
#         html = "90-MAE-integration.html"
#     log:
#         "logs/90-MAE-integration.log"
#     shell:
#         "quarto render {input.qmd} 2> {log}"
# 
# 
# ########## Rule 91a - qPCR
# 
# rule render_91a_qPCR
#     input:
#         qmd = "91a-qPCR.qmd",
#         rds1 = f"{ONE_DRIVE_OUTPUT_DIR_2}mae_full_TODAY.rds",
#     output:
#         rds2 = f"{ONE_DRIVE_OUTPUT_DIR_3}mae_full_01_TODAY.rds",
#         html = "91a-qPCR.html"
#     log:
#         "logs/91a-qPCR.log"
#     shell:
#         "quarto render {input.qmd} 2> {log}"
# 
# 
# ########## Rule 91b - primary outcomes
# 
# rule render_91b_primary_outcomes
#     input:
#         qmd = "91b-primary-outcomes.qmd",
#         rds1 = f"{ONE_DRIVE_OUTPUT_DIR_3}mae_full_01_TODAY.rds",
#     output:
#         rds2 = f"{ONE_DRIVE_OUTPUT_DIR_3}mae_full_02_TODAY.rds",
#         html = "91b-primary-outcomes.html"
#     log:
#         "logs/91b_primary_outcomes.log"
#     shell:
#         "quarto render {input.qmd} 2> {log}"
# 
# 
# ########## Rule 91c - calculated var participant study_day level
# 
# rule render_91c_calculated_variables_at_participant_study_day_level
#     input:
#         qmd = "91c-calculated-variables-at-participant-study_day-level.qmd",
#         rds1 = f"{ONE_DRIVE_OUTPUT_DIR_3}mae_full_02_TODAY.rds",
#     output:
#         rds2 = f"{ONE_DRIVE_OUTPUT_DIR_3}mae_full_03_TODAY.rds",
#         #html = "91c-calculated-variables-at-participant-study_day-level.html"
#     log:
#         "logs/91c-calculated-variables-at-participant-study_day-level.log"
#     shell:
#         "quarto render {input.qmd} 2> {log}"
# 
# 
# ########## Rule 91d - calculated var participant visit_code level
# 
# rule render_91d_calculated_variables_at_participant_visit_code
#     input:
#         qmd = "91d-calculated-variables-at-participant-visit-code.qmd",
#         rds1 = f"{ONE_DRIVE_OUTPUT_DIR_3}mae_full_03_TODAY.rds",
#     output:
#         rds2 = f"{ONE_DRIVE_OUTPUT_DIR_3}mae_full_04_TODAY.rds",
#         html = "91d-calculated-variables-at-participant-visit-code.html"
#     log:
#         "logs/91d-calculated-variables-at-participant-visit-code.log"
#     shell:
#         "quarto render {input.qmd} 2> {log}"
# 
# ########## Rule 91e - calculated var participant leve
# 
# rule render_91e_calculated_variables_at_participant_level
#     input:
#         qmd = "91e-calculated-variables-at-participant-level.qmd",
#         rds1 = f"{ONE_DRIVE_OUTPUT_DIR_3}mae_full_04_TODAY.rds"
#     output:
#         rds2 = f"{ONE_DRIVE_OUTPUT_DIR_3}mae_full_05_TODAY.rds",
#         html = "91e-calculated-variables-at-participant-level.html"
#     log:
#         "logs/91e-calculated-variables-at-participant-level.log"
#     shell:
#         "quarto render {input.qmd} 2> {log}"
# 
# 
# ########## Rule 91f - additional descriptive analyses
# 
# rule render_91f_additional_descriptive_analyses
#     input:
#         qmd = "91f-additional-descriptive-analyses.qmd",
#         rds1 = f"{ONE_DRIVE_OUTPUT_DIR_3}mae_full_05_TODAY.rds"
#     output:
#         rds2 = f"{ONE_DRIVE_OUTPUT_DIR_3}mae_full_06_TODAY.rds"
#         html = "91f-additional-descriptive-analyses.html"
#     log:
#         "logs/91f-additional-descriptive-analyses.log"
#     shell:
#         "quarto render {input.qmd} 2> {log}"
# 
# 
# ########## Rule 91g - absolute abundances
# 
# rule render_91g_absolute_abundances
#     input:
#         qmd = "91g_absolute_abundances.qmd",
#         rds1 = f"{ONE_DRIVE_OUTPUT_DIR_3}mae_full_06_TODAY.rds"
#     output:
#         rds2 = f"{ONE_DRIVE_OUTPUT_DIR_3}mae_full_07_TODAY.rds",
#         html = "91g_absolute_abundances.html"
#     log:
#         "logs/91g_absolute_abundances.log"
#     shell:
#         "quarto render {input.qmd} 2> {log}"
# 
# 
# ########## Rule 91h - absolute abundances imputation
# 
# rule render_91h_absolute_abundances_imputation
#     input:
#         qmd = "91h_absolute_abundances_imputation.qmd",
#         rds1 = f"{ONE_DRIVE_OUTPUT_DIR_3}mae_full_07_TODAY.rds"
#     output:
#         rds2 = f"{ONE_DRIVE_OUTPUT_DIR_3}mae_full_08_TODAY.rds",
#         html = "91h_absolute_abundances_imputation.html"
#     log:
#         "logs/91h_absolute_abundances_imputation.log"
#     shell:
#         "quarto render {input.qmd} 2> {log}"
# 
# 
# ########## Rule 92 - unblinding
# 
# rule render_92_unblinding
#     input:
#         qmd = "92_unblinding.qmd",
#         rds1 = f"{ONE_DRIVE_OUTPUT_DIR_3}mae_full_08_TODAY.rds",
#         xlsx1 = f"{ARM_DIR}US_STAT_ML.xlsx",
#         xlsx2 = f"{ARM_DIR}sa_stats_randomisation_table.xlsx"
#     output:
#         rds2 = f"{ONE_DRIVE_OUTPUT_DIR_4}mae_full_TODAY.rds",
#         html = "92_unblinding.html"
#     log:
#         "logs/92_unblinding.log"
#     shell:
#         "quarto render {input.qmd} 2> {log}"
# 
# ########## Rule 93 - post-unblinding augmentation 
# 
# rule render_93_post_unblinding_augmentation
#     input:
#         qmd = "93-post-unblinding-augmentation.qmd",
#         rds1 = f"{ONE_DRIVE_OUTPUT_DIR_4}mae_full_TODAY.rds"
#     output:
#         rds2 = f"{ONE_DRIVE_OUTPUT_DIR_5}mae_full_TODAY.rds",
#         html = "93-post-unblinding-augmentation.html"
#     log:
#         "logs/93-post-unblinding-augmentation.log"
#     shell:
#         "quarto render {input.qmd} 2> {log}"
# 
# ########## Rule 99 - subsetting MAE
# 
# rule render_99_subsetting_MAE
#     input:
#         qmd = "99_subsetting_MAE.qmd",
#         rds1 = f"{ONE_DRIVE_OUTPUT_DIR_5}mae_full_TODAY.rds"
#         ## add another one ? on line 85?
#     output:
#         rds2 = f"{ONE_DRIVE_OUTPUT_DIR_6}mae_1_TODAY.rds",
#         rds3 = f"{ONE_DRIVE_OUTPUT_DIR_7}01 Primary Outcome Manuscript/mae_1_csv_export_TODAY.rds",#NOT SURE THIS ONE!
#         html = "99_subsetting_MAE.html"
#     log:
#         "logs/99_subsetting_MAE.log"
#     shell:
#         "quarto render {input.qmd} 2> {log}"


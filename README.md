Hello!

This repository provides

- the code (`.qmd` files), 
- associated rendering (`.html`), and 
- `R` scripts (`R * /*.R` files) 

used to QC, clean, format, transform, and harmonize data 
from the different biological assays (metagenomics, 16S amplicon sequencing, luminex, etc.) and CRF survey data for the VMRC VIBRANT trial.

There is one (or a few) quarto documents (`.qmd`) per modality. 
While most are standalone, it is recommended to execute them in the order they are numbered.

These documents read "raw tabular data" from the VIBRANT secure dropbox (or UCLouvain secure One-Drive) and export the processed data on secure shared drives.

Finally, note that while these documents are regularly "rendered" to `html` files, 
the "rendering date" shown on the `html` files may not always reflect the latest changes in the code. 
Most of these files have not been modified since mid-June 2025. 
Please check the GitHub version history for the latest changes.
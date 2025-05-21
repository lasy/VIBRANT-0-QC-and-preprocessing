This repository provides the code (`.qmd` and .`R` files) and associated rendering (`.html`) used to QC, clean, format, transform, and harmonize data from the different biological assays (metagenomics, 16S amplicon sequencing, luminex, etc.) and CRF survey data.

There is one or a few quarto documents (`.qmd`) per modality, and while most are standalone, it is recommended to execute them in the order they are numbered.

In development, these documents read "raw tabular data" from the VIBRANT dropbox. Once all data is generated, QCed, and locked, data will be stored on a Zenodo repository.
# Analysis and comparison of the diversity of global bacterial and fungal communities

## Description

This directory contains R scripts and programs for **"Analysis and comparison of the divesity of global bacterial and fungal communities"**, Chuxuan Ji's 1st project of EEC MRes.

## Languages

R, Bash

## Dependencies

For some scripts in this directory, packages [ggplot2](https://cran.r-project.org/web/packages/ggplot2/index.html), [vegan](https://cran.r-project.org/web/packages/vegan/index.html), [ggpubr](https://cran.r-project.org/web/packages/ggpubr/index.html), [dplyr](https://cran.r-project.org/web/packages/dplyr/index.html), [data.table](https://cran.r-project.org/web/packages/data.table/index.html),[maptools](https://cran.r-project.org/web/packages/maptools/index.html),[ggrepel](https://cran.r-project.org/web/packages/ggrepel/index.html) are required. 
Please run the following script in **R/RStudio** for package installation: 
```R
install.packages(c("ggplot2", "vegan", "ggpubr", "dplyr", "data.table", "maptools", "ggrepel"))
```


[LaTeX](https://www.latex-project.org/) installation is also required. Please run following **bash** script in Linux Terminal for installation:
```bash
sudo apt install texlive-full texlive-fonts-recommended texlive-pictures texlive-latex-extra imagemagick
```
## Installation

To use scripts in this directory clone the repository and run.

```bash
git clone https://github.com/ChuxuanJi/EECProject1.git/
```

## Project structure and Usage 

### Code:

- **shan_box_alpha.R:** An R script to draw a box plot of Shannon Index at different temperatures.

- **PCA_test_Unifrac.r:** An R script to draw a PCA graph using the calculated weighted Unifrac distances.

- **sample_prepare_lati.R:** An R script that filters the OTU table and combines the filtered results with the latitude grouping table.

- **sample_prepare_shan.R:** An R script that filters the OTU table and combines the filtered results with the Shannon Index grouping table.

- **sample_prepare_T.r:** An R script that filters the OTU table and combines the filtered results with the temperature grouping table.

- **sample_prepare_waterns_lati.R:** An R script that filters the OTU table and combines the filtered results with the water (non-saline) samples grouping table classified by latitude.

- **sample_prepare_ns_shan.R:** An R script that filters the OTU table and combines the filtered results with the non-saline samples grouping table classified by Shannon Index.

- **sample_prepare_ns_T.R:** An R script that filters the OTU table and combines the filtered results with the non-saline samples grouping table classified by temperature.

- **sample_prepare_s_shan.R:** An R script that filters the OTU table and combines the filtered results with the saline samples grouping table classified by Shannon Index.

- **sample_prepare_s_T.R:** An R script that filters the OTU table and combines the filtered results with the saline samples grouping table classified by temperature.


<br/>

### Data: 
- **emp_90_gg_1k_weighted_unifrac.txt.pc.first_ten.txt**: Calculation results of the weighted Unifrac distance for the top ten species in abundance. 

- **group_empo.txt**: Different classification results according to the types of sample collection sites. 

- **group_lati_10.txt**: Classification results of samples with OTUs numbers greater than 10 classified by latitude. 

- **group_T_10.txt**: Classification results of samples with OTUs numbers greater than 10 classified by temperature. 

- **group_lati_waterns.txt**: Classification results of samples of water (non-saline) classified by latitude. 

- **group_shan.txt**: Classification results of samples classified by Shannon Index. 

- **group_ns_shan.txt**: Classification results of non-saline samples classified by Shannon Index. 

- **group_ns_T.txt**: Classification results of non-saline samples classified by temperature. 

- **group_s_shan.txt**: Classification results of saline samples classified by Shannon Index.

- **group_s_T.txt**: Classification results of saline samples classified by temperature. 

-


## Author name and contact

Name: Chuxuan Ji

Email: chuxuan.ji21@imperial.ac.uk
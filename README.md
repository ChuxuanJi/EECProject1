# Analysis and comparison of the diversity of global bacterial and fungal communities

## Description

This directory contains R scripts and programs for **"Analysis and comparison of the divesity of global bacterial and fungal communities"**, Chuxuan Ji's 1st project of EEC MRes.

## Languages

R, Bash

## Dependencies

For some scripts in this directory, packages [ggplot2](https://cran.r-project.org/web/packages/ggplot2/index.html), [vegan](https://cran.r-project.org/web/packages/vegan/index.html), [ggpubr](https://cran.r-project.org/web/packages/ggpubr/index.html), [dplyr](https://cran.r-project.org/web/packages/dplyr/index.html), [data.table](https://cran.r-project.org/web/packages/data.table/index.html),[maptools](https://cran.r-project.org/web/packages/maptools/index.html), [ggrepel](https://cran.r-project.org/web/packages/ggrepel/index.html), [RColorBrewer](https://cran.r-project.org/web/packages/RColorBrewer/index.html) are required. 
Please run the following script in **R/RStudio** for package installation: 
```R
install.packages(c("ggplot2", "vegan", "ggpubr", "dplyr", "data.table", "maptools", "ggrepel", "RColorBrewer"))
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

- **sample_prepare_empo2.R:** An R script that filters the OTU table and combines the filtered results with the samples grouping table classified by different sampling sites.

- **sample_prepare_lati.R:** An R script that filters the OTU table and combines the filtered results with the latitude grouping table.

- **sample_prepare_shan.R:** An R script that filters the OTU table and combines the filtered results with the Shannon Index grouping table.

- **sample_prepare_T.r:** An R script that filters the OTU table and combines the filtered results with the temperature grouping table.

- **sample_prepare_waterns_lati.R:** An R script that filters the OTU table and combines the filtered results with the water (non-saline) samples grouping table classified by latitude.

- **sample_prepare_ns_lati.R:** An R script that filters the OTU table and combines the filtered results with the non-saline samples grouping table classified by latitude.

- **sample_prepare_ns_shan.R:** An R script that filters the OTU table and combines the filtered results with the non-saline samples grouping table classified by Shannon Index.

- **sample_prepare_ns_T.R:** An R script that filters the OTU table and combines the filtered results with the non-saline samples grouping table classified by temperature.

- **sample_prepare_s_lati.R:** An R script that filters the OTU table and combines the filtered results with the saline samples grouping table classified by latitude.

- **sample_prepare_s_shan.R:** An R script that filters the OTU table and combines the filtered results with the saline samples grouping table classified by Shannon Index.

- **sample_prepare_s_T.R:** An R script that filters the OTU table and combines the filtered results with the saline samples grouping table classified by temperature.

- **sample_prepare_p_lati.R:** An R script that filters the OTU table and combines the filtered results with the plant samples grouping table classified by latitude.

- **sample_prepare_p_shan.R:** An R script that filters the OTU table and combines the filtered results with the plant samples grouping table classified by Shannon Index.

- **sample_prepare_p_T.R:** An R script that filters the OTU table and combines the filtered results with the plant samples grouping table classified by temperature.

- **sample_prepare_a_lati.R:** An R script that filters the OTU table and combines the filtered results with the animal samples grouping table classified by latitude.

- **nmds_envfit_empo2.R:** An R script for NMDS analysis and graphing of the filtered OTU table classified by different sampling sites.

- **nmds_envfit_lati.R:** An R script for NMDS analysis and graphing of the filtered OTU table classified by latitude.

- **nmds_envfit_T.R:** An R script for NMDS analysis and graphing of the filtered OTU table classified by temperatue.

- **nmds_envfit_shan.R:** An R script for NMDS analysis and graphing of the filtered OTU table classified by Shannon Index.

- **nmds_envfit_ns_lati.R:** An R script for NMDS analysis and graphing of the filtered OTU table of non-saline samples classified by latitude.

- **nmds_envfit_ns_T.R:** An R script for NMDS analysis and graphing of the filtered OTU table of non-saline samples classified by temperatue.

- **nmds_envfit_ns_shan.R:** An R script for NMDS analysis and graphing of the filtered OTU table of non-saline samples classified by Shannon Index.

- **nmds_envfit_s_lati.R:** An R script for NMDS analysis and graphing of the filtered OTU table of saline samples classified by latitude.

- **nmds_envfit_s_T.R:** An R script for NMDS analysis and graphing of the filtered OTU table of saline samples classified by temperatue.

- **nmds_envfit_s_shan.R:** An R script for NMDS analysis and graphing of the filtered OTU table of saline samples classified by Shannon Index.

- **nmds_envfit_p_lati.R:** An R script for NMDS analysis and graphing of the filtered OTU table of plant samples classified by latitude.

- **nmds_envfit_p_T.R:** An R script for NMDS analysis and graphing of the filtered OTU table of plant samples classified by temperatue.

- **nmds_envfit_a_lati.R:** An R script for NMDS analysis and graphing of the filtered OTU table of animal samples classified by latitude.

- **nmds_envfit_waterns_lati.R:** An R script for NMDS analysis and graphing of the filtered OTU table of Water (non-saline) samples classified by latitude.


<br/>

### Data: 
- **emp_90_gg_1k_weighted_unifrac.txt.pc.first_ten.txt**: Calculation results of the weighted Unifrac distance for the top ten species in abundance. 

- **env.txt**: The filtered environmental data includes latitude, longtitude, depth, altitude, temperature and pH factors. 

- **envfit1.txt**: Data used to generate arrows for environmental influence factors. 

- **group_empo.txt**: Different classification results according to the types of sample collection sites. 

- **group_empo2.txt**: Different classification results according to the types of bigger sample collection sites. 

- **group_lati_10.txt**: Classification results of samples with OTUs numbers greater than 10 classified by latitude. 

- **group_T_10.txt**: Classification results of samples with OTUs numbers greater than 10 classified by temperature. 

- **group_lati_waterns.txt**: Classification results of samples of water (non-saline) classified by latitude. 

- **group_shan.txt**: Classification results of samples classified by Shannon Index. 

- **group_ns_lati.txt**: Classification results of non-saline samples classified by latitude. 

- **group_ns_shan.txt**: Classification results of non-saline samples classified by Shannon Index. 

- **group_ns_T.txt**: Classification results of non-saline samples classified by temperature. 

- **group_s_lati.txt**: Classification results of saline samples classified by latitude. 

- **group_s_shan.txt**: Classification results of saline samples classified by Shannon Index.

- **group_s_T.txt**: Classification results of saline samples classified by temperature. 

- **group_p_lati.txt**: Classification results of plant samples classified by latitude. 

- **group_p_shan.txt**: Classification results of plant samples classified by Shannon Index.

- **group_p_T.txt**: Classification results of plant samples classified by temperature. 

- **group_a_lati.txt**: Classification results of animals samples classified by latitude. 


## Author name and contact

Name: Chuxuan Ji

Email: chuxuan.ji21@imperial.ac.uk
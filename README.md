# Changes in microbial diversity along environmental gradients

## Description

This directory contains R scripts and programs for **"Changes in microbial diversity along environmental gradients"**, Chuxuan Ji's winter project of EEC MRes.

## Languages

R, Bash

## Dependencies

For some scripts in this directory, packages [ggplot2](https://cran.r-project.org/web/packages/ggplot2/index.html), [vegan](https://cran.r-project.org/web/packages/vegan/index.html), [ggpubr](https://cran.r-project.org/web/packages/ggpubr/index.html), [dplyr](https://cran.r-project.org/web/packages/dplyr/index.html), [data.table](https://cran.r-project.org/web/packages/data.table/index.html), [maptools](https://cran.r-project.org/web/packages/maptools/index.html), [ggrepel](https://cran.r-project.org/web/packages/ggrepel/index.html), [RColorBrewer](https://cran.r-project.org/web/packages/RColorBrewer/index.html), [lme4](https://cran.r-project.org/web/packages/lme4/index.html), [MASS](https://cran.r-project.org/web/packages/MASS/index.html), [randomForest](https://cran.r-project.org/web/packages/randomForest/index.html), [rfPermute](https://cran.r-project.org/web/packages/rfPermute/index.html), [A3](https://cran.r-project.org/web/packages/A3/index.html), [car](https://cran.r-project.org/web/packages/car/index.html), [rockchalk](https://cran.r-project.org/web/packages/rockchalk/index.html) are required. 
Please run the following script in **R/RStudio** for package installation: 
```R
install.packages(c("ggplot2", "vegan", "ggpubr", "dplyr", "data.table", "maptools", "ggrepel", "RColorBrewer", "lme4", "MASS", "randomForest", "rfPermute", "A3", "car", "rockchalk"))
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

- **Diversity_EVs_Pairplot.R:** An R script to judge whether this is a significant linear relationship between diversity indices and environmental variables.

- **Diversty_Hist_EMPO2.R:** An R script to plot histograms of diversity indices for the whole dataset samples and the EMPO 2 samples.

- **Diversty_Hist_EMPO3.R:** An R script to plot histograms of diversity indices for the EMPO 3 samples.

- **Diversty_lati_EMPO2.R:** An R script to show the patterns between diversity indices and latitude of the whole dataset samples and the EMPO 2 samples.

- **Diversty_lati_EMPO3.R:** An R script to show the patterns between diversity indices and latitude of the EMPO 3 samples.

- **Diversty_LM_all.R:** An R script for linear models between diversity indices and environmental variables of the whole dataset samples.

- **Diversty_LM_EMPO3.R:** An R script for linear models between diversity indices and environmental variables of three EMPO 3 samples.

- **Diversty_LM_local.R:** An R script for linear models between diversity indices and environmental variables of three local dataset samples.

- **Diversty_LM_PM_1EV.R:** An R script to plot the relationship between diversity indices and a single environmental variable.

- **Diversty_LM_PM_2EVs_3D.R:** An R script to plot the relationship between diversity indices and two environmental variables.

- **Diversty_pH_EMPO2.R:** An R script to show the patterns between diversity indices and pH of the whole dataset samples and the EMPO 2 samples.

- **Diversty_PM_all.R:** An R script for polynomial regression models between diversity indices and environmental variables of the whole dataset samples.

- **Diversty_PM_EMPO3.R:** An R script for polynomial regression models between diversity indices and environmental variables of three EMPO 3 samples.

- **Diversty_PM_local.R:** An R script for polynomial regression models between diversity indices and environmental variables of three local dataset samples.

- **Diversty_PM_all.R:** An R script for random forest models between diversity indices and environmental variables of the whole dataset samples.

- **Diversty_PM_EMPO3.R:** An R script for random forest models between diversity indices and environmental variables of three EMPO 3 samples.

- **Diversty_PM_local.R:** An R script for random forest models between diversity indices and environmental variables of three local dataset samples.

- **Diversty_T_EMPO2.R:** An R script to show the patterns between diversity indices and temperature of the whole dataset samples and the EMPO 2 samples.

- **PM_PD_all.R:** An R script for polynomial regression models between phylogenetic diversity and environmental variables of the whole dataset samples.

- **RF_PD_all.R:** An R script for random forest models between phylogenetic diversity and environmental variables of the whole dataset samples.


<br/>

### Data: 
- **Chao_lati_all.txt**: Classification results of samples classified by Chao1 richness and latitude. 

- **Chao_lati_ani.txt**: Classification results of animal samples classified by Chao1 richness and latitude. 

- **Chao_lati_ns.txt**: Classification results of non-saline samples classified by Chao1 richness and latitude. 

- **Chao_lati_p.txt**: Classification results of samples classified by Chao1 richness and latitude. 

- **Chao_lati_s.txt**: Classification results of saline samples classified by Chao1 richness and latitude. 

- **Chao_T_empo2.txt**: Classification results of EMPO 2 samples classified by Chao1 richness and temperature. 

- **OO_lati_all.txt**: Classification results of samples classified by absolute richness (observed OTUs) and latitude. 

- **OO_lati_ani.txt**: Classification results of animal samples classified by absolute richness (observed OTUs) and latitude. 

- **OO_lati_ns.txt**: Classification results of non-saline samples classified by absolute richness (observed OTUs) and latitude. 

- **OO_lati_p.txt**: Classification results of plant samples classified by absolute richness (observed OTUs) and latitude. 

- **OO_lati_s.txt**: Classification results of saline samples classified by absolute richness (observed OTUs) and latitude. 

- **OO_T_empo2.txt**: Classification results of EMPO 2 samples classified by absolute richness (observed OTUs) and temperature. 

- **Shan_lati_all.txt**: Classification results of samples classified by Shannon diversity and latitude. 

- **Shan_lati_ani.txt**: Classification results of animal samples classified by Shannon diversity and latitude. 

- **Shan_lati_ns.txt**: Classification results of non-saline samples classified by Shannon diversity and latitude. 

- **Shan_lati_p.txt**: Classification results of plant samples classified by Shannon diversity and latitude. 

- **Shan_lati_s.txt**: Classification results of saline samples classified by Shannon diversity and latitude. 

- **Shan_T_empo2.txt**: Classification results of EMPO 2 samples classified by Shannon diversity and temperature. 

- **Func_Chao_T_pH_lati.csv**: A filtered data set containing Chao1 richness, temperature, pH and latitude. 

- **Func_OO_T_pH_lati.csv**: A filtered data set containing Chao1 richness, temperature, pH and latitude. 

- **Func_Shan_T_pH_lati.csv**: A filtered data set containing Chao1 richness, temperature, pH and latitude. 


<br/>


### Results: 
- **LM_PM_RF_r2.xlsx**: Results of linear, polynomial regression and random forest model fits at global scale, EMPO3 environment and local scale. 

## Author name and contact

Name: Chuxuan Ji

Email: chuxuan.ji21@imperial.ac.uk
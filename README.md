# Research_PCA_CORDEX_CA
# PCA_CORDEX_CA

PCA_CORDEX_CA is an R program intended to execute dimensionality reduction of the CORDEX-CA GCM-RCM multimodel-ensemble on precipitation using Principal Component Analysis (PCA) and Hierarchical Clustering (HC).
PCA_CORDEX_CA is based on the {FactoMineR} R package.

## Installation

Use Git to clone and install the program

## Usage

Run PCA_CORDEX_CA.R script accordingly

## Contributing

Maikel Mendez Morales. Escuela de Ingeniería en Construcción, Instituto Tecnológico de Costa Rica. email: mamendez@itcr.ac.cr

Luis Alexander Calvo Valverde. Escuela de Ingeniería en Computación, Instituto Tecnológico de Costa Rica. email: lcalvo@itcr.ac.cr

## Publications

Dimensionality reduction of the CORDEX-CA GCM-RCM multimodel-ensemble on precipitation using Principal Component Analysis (PCA) and Hierarchical Clustering (HC)

EDP Sciences - E3S Web of Conferences

https://www.e3s-conferences.org/

https://doi.org/10.1051/bioconf/20236201002

![alt test](/paper_01.png)

Graphical Abstract

![alt test](/FFG01.png)

Abstract: 

Principal Component Analysis (PCA) and Hierarchical Clustering (HC) were applied to reduce the dimensionality of a 19-member multimodel-ensemble combining different General Circulation Models (GCMs) and Regional Climate Models (RCMs) as part of the Coordinated Regional Climate Downscaling Experiment (CORDEX) for the Central America domain (CA). A subset of 12 Expert Team on Climate Change Detection and Indices (ETCCDI) was selected to evaluate the performance of each ensemble-member on precipitation against daily observational data from the Juan Santamaría International Airport (SJO), located in Alajuela, Costa Rica for the baseline period 1971-2000. The ETCCDI indices are designed to measure and quantify climate variability and associated trends. Results from the PCA analysis indicate that over 95% of the variance can be explained by the first three principal components (PC-1 through PC-3), showing high correlations, strong contributions and fair representation of most ETCCDI indices. HC clustering on the other hand, groups ensemble-members into 4 closely related clusters of common attributes (cluster-1 through cluster-4), with models ranging from dry to wet patterns. Afterwards, ensemble-members were sampled from each cluster to generate a sub-ensemble of representative simulations, reducing the original ensemble from 19 to 5 members, while still retaining its fundamental characteristics. Later, two multi-model ensemble-means (MEMs), one using the entire ensemble and the other using the 5-member subset were generated and their performance evaluated by means of five objective functions (nRMSE, MBE, MDA, PBIAS and MAE) against the observational dataset for the reference period. Nevertheless, no significant difference was found between both MEMs, implying that the applied techniques are effective in reducing dimensionality, preventing double-counting of highly dependent simulations, and consequently reducing the associated computational costs. Ultimately however, both MEMs noticeably overestimate seasonal precipitation during the reference period, suggesting the need for applying bias correction (BC) techniques prior to their use in impact assessment studies at local levels.

Results

![alt test](/FFG02.png)

![alt test](/FFG03.png)

![alt test](/FFG04.png)

![alt test](/FFG05.png)

## License

[MIT](https://choosealicense.com/licenses/mit/)

# Data and R codes for: In search of draught cattle: An identification method


### Phoebe Liu, Lenny Salvagno, Benjamin Wimmer, Umberto Albarella
E-mail addresses: wcliu1@sheffield.ac.uk (P. Liu), l.salvagno@sheffield.ac.uk (L. Salvagno), bwimmer1@sheffield.ac.uk (B. Wimmer), u.albarella@sheffield.ac.uk (U. Albarella)

## Repository Structure
1) Data: Biometry data on modern cattle
The data of the Romanian Grey and Brown draught and non-draught cattle are adopted from Bartosiewicz et al., 1997 and Lin et al., 2016.
See Table S2 for the measurement abbreviations.


## Data Files for Producing Table and Figures
The `table and figure` folder contains the following data files used for generating the table and figure:
- `Metacarpal.csv`
- `Metatarsal.csv`
- `First_phalanx.csv`
- `Second_phalanx.csv`

## Data Files for Principal Component Analysis (PCA)
The `PCA` folder contains the following data files used PCA analyses:
- `Metacarpal_PCA.csv`
- `Metatarsal_PCA.csv`
- `First_phalanx_PCA.csv`
- `Second_phalanx_PCA.csv`

## R Code Files

The `R_codes` folder contains the following R scripts used for different parts of the analysis:

- **`01_Table.R`**: Performs the statistical analyses presented in the tables.
- **`02_PCA.R`**: Conducts principal component analysis (PCA).
- **`03_Figure.R`**: Creates the figures used in the paper.
- **`04_Classification.R`**: Classifies new archaeological data based on the study's models.

## Getting Started

To get started with the analysis and reproduce the results from the paper, follow these steps:

1. **Run `01_Table.R`**: This script performs the statistical analyses and generates the results for the tables.
   
2. **Run `02_PCA.R`**: This script conducts the principal component analysis (PCA) on the data. 

3. **Run `03_Figure.R`**: This script generates the figures that are included in the paper. 



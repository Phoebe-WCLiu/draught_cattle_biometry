# Data and R codes for: In search of draught cattle: An identification method

This repository contains data and R scripts for the following paper:
Liu P, Salvagno L, Wimmer B, Albarella U (2025) In search of draught cattle: An identification method. Journal of Archaeological Science 179:106229. https://doi.org/10.1016/j.jas.2025.106229



## Repository
http://doi.org/10.17632/7722hz4z4x

https://github.com/Phoebe-WCLiu/draught_cattle_biometry


##  Biometry data on modern cattle


`Raw_data.csv` is the raw measurement file with cattle information.
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

4. **Run `04_Classification.R`** to classify new archaeological data.  
   - This script is designed for classifying new archaeological data and requires specific measurements to produce **Figure 9**.  
     - **Metacarpal**: `e`, `1 (D1)`, `f`, `4 (D4)`  
     - **Metatarsal**: `e`, `1 (D1)`, `f`, `4 (D4)` *or* `Bd`, `GL`, `f`, `4 (D4)`  
     - **First phalanx**: `Bp`, `Dp`, `Bd`, `GLpe`  
     - **Second phalanx**: `Bp`, `Bd`, `GLpe`  

   **Note**: Before running this script, update the placeholder filename (`your_file.csv`) in the script with the actual name of your data file.  

# Nigeria Antigens Project

## Basic Structure

The Repo is made up of five folders: Code, Data, Figures, Intermediate, and Output. Users need to download all five folders with the same structure to their R project. 

### Code

This folder holds all R script files needed to generate the figures and outputs. The files are divided into 3 data import files (CDCDataImport.R, WorldPop.R, spatial_import.R), 4 intermediate calculation files (ASFR.r, RiskyBirths.R, Rubella_foi.R, Diphtheria_foi3.R), 1 model file (Sero_and_Burden_model.R), 1 figure preparation file (FigurePrep.R), and 2 Figure generation files (FourAntigens_Figures.R, TetanusManuscriptFigures.R). To generate the final output, users should execute the data import files first (order is not important), followed by the intermediate calculation files, followed by the model generation file, followed by the figure preparation file, and finally the figure generation files. 

### Data

This folder includes all required data for the project. Data is organized based on source and type. Instructions on how to download data that is publically available are included in the appropriate folders. For CDC data, please reach out to the authors for more information. 

### Figures

All figures for the project will be stored in this folder. 

### Intermediate

Intermediate objects necessary for final calculations are stored in this folder. 

### Output

Final output in csv form can be found in this folder, including a data dictionary. 
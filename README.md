# Indirect Effects of the COVID-19 Pandemic: A Cause-of-Death Analysis of Life Expectancy Changes in 24 Countries, 2015 to 2022

## Repository Purpose
The repository **indirect-effects-C19** was created to facilitate the replication of findings reported in:

*Indirect Effects of the COVID-19 Pandemic: A Cause-of-Death Analysis of Life Expectancy Changes in 24 Countries, 2015 to 2022*,

hereafter *our manuscript.*

## Repository Structure
The repository **indirect-effects-C19** contains two folders:

### 1. data
This folder is used to store necessary data files. To start running our analysis files described below, the **data** folder must at least hold the data file `harmonized_all.rds` containing the mortality information underlying our decomposition results. The repository [le_cod_datapreparation](https://doi.org/10.17605/OSF.IO/W8XFT) outlines how this data file can be prepared.

### 2. scripts
This folder contains the analysis files to replicate our empirical findings:

- The analysis file `00-main.R` installs and loads all necessary `R` packages, creates the folder structure required for the analysis, and calls all other analysis files.
- The analysis file `10-prep.R` loads and modifies the data file `harmonized_all.rds` described above. It also downloads abridged life tables from the [United Nations World Population Prospects 2024 revision](https://population.un.org/wpp/) and stores them in the **data** folder as the data file `wpp2024_d[YYYYMMDD].rds`. Please note that we use UNWPP data downloaded on 28 July 2024 and that data distributed by UNWPP may have been updated or revised in the meantime.
- The analysis file `09-functions.R` defines life table and decomposition functions necessary for the analysis.
- The analysis files `01-decomp.R`, `02-plots.R`, and `03-tables.R` generate the output from our analysis. All output is stored in the automatically generated sub-folder **output**. Depending on the type of output generated, one of the automatically generated sub-folders **csv**, **data**, **figures**, or **tables** will be used for storage.
 
Please note that some of our analysis files use the parallel processing capabilities of the `furrr` package (Vaughan and Dancho 2022) to speed up estimation. Please make sure that the selected settings work with your machine before running any files.

## How to Use This Repository
In order to run the `R` code provided in the repository **indirect-effects-C19**, please proceed in the following order:

1. Download the repository from `OSF` or `github`. If applicable, unzip the downloaded folder and place it in a location convenient for you. 
2. Store the necessary `harmonized_all.rds` file in the folder **data**. 
3. Double click on the file `indirect-effects-C19.Rproj`. This should open `RStudio` on your machine.  
4. Within `RStudio`, click on `File`/`Open File...` and select the analysis file `00-main.R` located in the **scripts** folder.
5. You should now be able to run our code without adjusting any directories.

## References

- Vaughan D, Dancho M (2022). _furrr: Apply Mapping Functions in Parallel using Futures_. R package version 0.3.1, <https://CRAN.R-project.org/package=furrr>. 

## License
This work is licensed under a
[Creative Commons Attribution 4.0 International License][cc-by].

[![CC BY 4.0][cc-by-image]][cc-by]

[cc-by]: http://creativecommons.org/licenses/by/4.0/
[cc-by-image]: https://i.creativecommons.org/l/by/4.0/88x31.png

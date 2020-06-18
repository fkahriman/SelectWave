# SelectWave

SelectWave is a spectroscopic data analysis tool implemented in the programming language R. 
It is based on the Shiny framework, giving researchers access to advanced data analysis in a few clicks.  
  
Analysis stages in the app include:
- Smoothed derivatives (Savitzky-Golay filtering)
- Various spectral pre-processing
- Variable selection methods
- Multivariate regression with Partial Least Squares Regression and Suppor Vector Machines Regression

## Usage
Clone or download the repository and run the SelectWave.R as a Shiny app, e.g. through R Studio.

## Dependencies
- baseline
- caret
- chillR
- dashboardthemes
- DT
- e1071
- EMSC
- ggplot2
- magrittr
- mdatools
- plsVarSel
- prospectr
- shinycssloaders
- shinydashboard
- shinyFiles

Installation can be done using the following commands in R:
```r
install.packages(c('baseline', 'caret', 'chillR', 'DT', 'e1071', 'EMSC', 'ggplot2', 'magrittr', 'mdatools', 'plsVarSel', 'prospectr', 'shinycssloaders', 'shinydashboard', 'shinyFiles'))
library(devtools)
install_github("nik01010/dashboardthemes")
```

## Example data
The `data` folder contains a set of NIR spectra and reference measurements of amylopectin content of milled corn samples.

## Contact
Doç. Dr. Fatih KAHRIMAN  
fkahriman (at) hotmail.com

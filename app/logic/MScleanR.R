###### MS Pipeline #######
# Author: Marian Freisleben #
# Date: Dec 2024


### Initialization ----

# Needed Packages
packages <- base::c("reticulate", "ggplot2")

# Install missing packages
missing_packages <- packages[!(packages %in% utils::installed.packages()[,"Package"])]
if(length(missing_packages)) utils::install.packages(missing_packages)

# Get UniDec python script
run_unidec <- "C:/Users/Admin/Desktop/MassSpecDataAnalysis/MScleanR/run_unidec.py"

# Get Waters .raw directory path
raw_dir <- "C:/Users/Admin/Desktop/MassSpecDataAnalysis/MS_Workflow/KARL229+DMSO_1hr_1to3.raw"

# Enable execution of python
reticulate::repl_python()


### Running UniDec ----

# raw to txt conversion & deconvolution
cmd <- base::paste("python", run_unidec, raw_dir)
base::system(cmd, intern = T)


### Import results ----

# Get results directories
output_path <- base::gsub(".raw", "_unidecfiles", raw_dir)
output_path2 <- base::gsub(".raw", "_rawdata_unidecfiles", raw_dir)

unidecfiles <- base::list.files(output_path, full.names = TRUE)
rawdata_unidecfiles <- base::list.files(output_path2, full.names = TRUE)

mass_intensity <- base::grep("_mass\\.txt$", unidecfiles, value = TRUE)

# Get file
mass_data <- utils::read.table(mass_intensity, sep = " ", header = TRUE)
base::colnames(mass_data) <- c("mz", "intensity")


### Visualization ----

plot(mass_data$mz, mass_data$intensity, type = "h",
     xlab = "Mass (Da)", ylab = "Intensity",
     main = "Deconvoluted Mass Spectrum",
     col = "blue", lwd = 3)

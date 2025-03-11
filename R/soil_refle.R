#' Diffuse reflectance of soils from Brazil
#'
#' @description
#' A cleaned and organized diffuse reflectance database. The structure of the database includes the first column as the wavelength used in all samples, with the remaining columns representing the soil samples (Frosi et al., 2025).
#'
#' @format A data frame with 4241 rows and 24 columns:
#' \describe{
#'   \item{`wavelength_nm`}{Wavelength values in nanometers.}
#'   \item{`a1`, `a2`, `a3`, `a4`, `a5`, `a6`, `a7`, `a8`, `a9`, `a10`, `a11`, `a12`, `a13`, `a14`, `a15`, `a16`, `a17`, `a18`, `a19`, `a20`, `a22`, `a24`, `a26`}{Reflectance percentage (%R) corresponding to each for each soil sample.}
#' }
#'
#' @references
#' Frosi, G., Inda, A., & Barrón, V. (2025).
#' Diffuse reflectance of soils from Brazil _Data set_. Zenodo \doi{10.5281/zenodo.14879041}
#'
"soil_refle"

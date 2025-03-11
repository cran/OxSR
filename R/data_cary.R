#' Diffuse soil reflectance via Cary equipment
#'
#' @description
#' A dataset containing diffuse reflectance measurements of soil samples from Brazil. The data were obtained using a Cary 5000 UV-Vis-NIR spectrophotometer, covering the spectral range from 2500 nm to 380 nm with a reading interval of 0.5 nm (Frosi et al., 2025).
#'
#' @format A data frame with 5,417 rows and 52 columns:
#' \describe{
#'   \item{`a1`, `a2`, `a3`, `a4`, `a5`, `a6`, `a7`, `a8`, `a9`, `a10`, `a11`, `a12`, `a13`, `a14`, `a15`, `a16`, `a17`, `a18`, `a19`, `a20`, `a21`, `a22`, `a23`, `a24`, `a26`}{Wavelength values in nanometers for each sample (labeled "a"). Each column represents the wavelength for a specific sample.}
#'   \item{`x2`, `x4`, `x6`, `x8`, `x10`, `x12`, `x14`, `x16`, `x18`, `x20`, `x22`, `x24`, `x26`, `x28`, `x30`, `x32`, `x34`, `x36`, `x38`, `x40`, `x42`, `x44`, `x46`, `x48`, `x50`}{Reflectance percentage (%R) corresponding to each wavelength for the same sample. The "x" columns represent the reflectance values for the corresponding "a" sample columns.}
#' }
#'
#' @references
#' Frosi, G., Inda, A., & Barrón, V. (2025).
#' Diffuse reflectance of soils from Brazil _Data set_. Zenodo. \doi{10.5281/zenodo.14879041}
#'
"data_cary"

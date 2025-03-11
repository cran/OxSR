#' Soil color by diffuse reflectance
#' @description
#' The function converts the visible soil spectrum to three-dimensional color systems, such as Munsell and RGB, using the CIE tristimulus values for the conversion. This allows for the standardized representation of colors and facilitates visual and comparative analysis.
#' @usage
#' soil_color(data = data,
#'            name_wave = "wave",
#'            tri_values = "std",
#'            plot = FALSE)
#'
#' @param data A data.frame containing the wavelength and reflectance values.
#' @param name_wave Character indicating the name of the column with the wavelength. It can be only the initial characters.
#' @param tri_values Tristimulus Values. Values from the `colorSpec` package can be used.
#' @param plot Logical, if TRUE, the plot with the colors is generated, the default is FALSE
#' @returns A `data.frame` with color values in `Munsell`, `HVC`, `RGB`, and `hexadecimal code`.
#' @examples
#' # example code
#' data(soil_refle)
#'
#' soil_color(soil_refle)
#'
#' # tristimulus values {colorSpec}
#' soil_color(soil_refle, tri_values = "xyz1931.1nm", plot = TRUE)
#' @export
#' @importFrom rlang .data
#'
soil_color <- function(data = data,
                       name_wave = "wave",
                       tri_values = "std",
                       plot = FALSE) {

  # Data input
  if (missing(data)) {
    stop("The parameter `data` are required.")
  }

  # Data must be data.frame
  if (!is.data.frame(data)) {
    stop("The `data` parameter must be a `data.frame`.")
  }
  # NA data
  # if (any(is.na(data))) {
  #   data <- stats::na.omit(data)
  # }

  # Wave columns
  wave_column <- grep(
    pattern = paste0("^", name_wave, ""),
    ignore.case = T,
    names(data), value = TRUE
  )

  if (length(wave_column) == 0) {
    stop("The `data` does not contain the `wavelength` column.")
  } else if(length(wave_column) > 1){
    stop("The `data` contains more than one `wavelength` column.")
  }

  # limits data
  data <- data[data$wavelength_nm >= 380 & data$wavelength_nm <= 770,]

  col_mun_na <- colSums(is.na(data))

  for (i in 1:length(col_mun_na)) {

    if (col_mun_na[i] >= 1) {
      warning(paste0(
        "\033[32m", "The sample ",
        "\033[31m", names(col_mun_na[i]), "\033[32m",
        " contains ", "\033[31m", col_mun_na[i], "\033[32m",
        " missing values and was removed!\n\n", "\033[0m",
        sep = ""
      ))
    }
  }

  data <- data[,col_mun_na == 0]

  data <- data |>
    dplyr::rename("wavelength" = wave_column[1]) |>
    dplyr::relocate(.data[["wavelength"]])

  if (tri_values == "std") {

    tristimulusEX <- tristimulusEX

  } else if (tri_values == "xyz1931.1nm") {

    tristimulusEX <- colorSpec::xyz1931.1nm |> as.data.frame()

  } else if (tri_values == "xyz1931.5nm") {

    tristimulusEX <- colorSpec::xyz1931.5nm |> as.data.frame()

  } else if (tri_values == "xyz1964.1nm") {

    tristimulusEX <- colorSpec::xyz1964.1nm |> as.data.frame()

  } else if (tri_values == "xyz1964.5nm") {

    tristimulusEX <- colorSpec::xyz1964.5nm |> as.data.frame()

  } else {
    stop(stop("The parameter `tri_values` are required."))
  }

  df_2filter <- data |>
    dplyr::filter(.data[["wavelength"]] %in% tristimulusEX$Wavelength) |>
    dplyr::arrange(.data[["wavelength"]])

  result_list <- list()

  for (i in 2:length(df_2filter)) {
    x1 <- sum(df_2filter[[i]] * tristimulusEX$x) / sum(tristimulusEX$x)
    y1 <- sum(df_2filter[[i]] * tristimulusEX$y) / sum(tristimulusEX$y)
    z1 <- sum(df_2filter[[i]] * tristimulusEX$z) / sum(tristimulusEX$z)

    result_list[[i - 1]] <- data.frame(samples = colnames(df_2filter)[i], x1, y1, z1)
  }

  base_ <- do.call(rbind, result_list)

  # munsel
  dfmun <- list()

  for (i in 1:dim(base_)[1]) {
    mun <- munsellinterpol::XYZtoMunsell(c(
      base_$x1[i],
      base_$y1[i],
      base_$z1[i]
    ))
    munsell <- row.names(mun)
    dfmun[[i]] <- data.frame(sample = base_$samples[i], mun, munsell)
  }

  munsel_soil <- do.call(rbind, dfmun)
  row.names(munsel_soil) <- NULL
  munsel_soil <- munsel_soil |>
    dplyr::relocate(munsell, .after = sample)

  rgb_munsellinte <- munsellinterpol::MunsellToRGB(MunsellSpec = munsel_soil$munsell)$RGB |>
    as.data.frame()
  row.names(rgb_munsellinte) <- NULL
  data_end <- cbind(munsel_soil, rgb_munsellinte)

  hex <- c()
  for (i in 1:length(row.names(data_end))) {
    hex_code <- grDevices::rgb(data_end$R[i],
                               data_end$G[i],
                               data_end$B[i],
                               maxColorValue = 255
    )
    hex[i] <- hex_code
  }

  data_end$hex <- hex

  data_end <- data_end |>
    dplyr::mutate(dplyr::across(c(.data[["H"]],.data[["V"]],
                                  .data[["C"]],.data[["R"]],
                                  .data[["G"]],.data[["B"]]), ~round(.,digits = 2)))

  if (plot == TRUE) {

    graph_hex <- data.frame(
      x = rep(1, length(hex)),
      y = rep(1, length(hex)),
      sample = data_end$sample,
      color = hex
    )

    p1 <- graph_hex |>
      ggplot2::ggplot() +
      ggplot2::aes(.data[["x"]], .data[["y"]]) +
      ggplot2::facet_wrap(~sample) +
      ggplot2::geom_rect(mapping = ggplot2::aes(
        fill = .data[["color"]],
        xmin = 0, xmax = 1,
        ymin = 0, ymax = 1
      )) +
      ggplot2::scale_fill_identity() +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.title = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank()
      )
    print(p1)

  }

  return(data_end)

}


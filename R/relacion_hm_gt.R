#' Ratios Hm/(Hm+Gt)
#' @description
#' Calculates the relationship between the iron oxides hematite and goethite in soil sample: \deqn{Hm/Gt = \frac{\text{hematite}}{\text{hematite} + \text{goethite}}}
#' based on the soil's diffuse reflectance curve in the visible spectrum, the Kubelka-Munk equation, and the second derivative (Scheinost et al., 1998; Torrent et al., 2008).
#'
#' @param data A data.frame containing the input data, where the first column should be the wavelength and the remaining columns should contain the reflectance data of the sample.
#' @param points_smoothing The number of points used for smoothing the data to reduce noise in the spectral readings. The default value is 0.3.
#' @param hm_gt_limits A list containing the detection range values for hematite (hm) and goethite (gt). The default is `list(hem = c(535, 585), gt = c(430, 460))`.
#' @param name_wave The name of the wavelength column in the data. By default, it is set to 'wave'.
#' @param plot A logical value indicating whether to generate a plot of the results. If set to TRUE, a plot will be displayed; if FALSE, no plot will be shown.
#' @param pv_tolerance A numeric vector with 4 elements, each corresponding to one of the limits for hematite (hm) and goethite (gt). This value specifies the tolerance for the point values in the calculation, helping to account for small variations in the data and controlling the precision of the calculation.
#'
#' @return a `data.frame` with each sample in the rows and columns containing the minimum, maximum, and amplitude for Hm and Gt, along with the Hm/Gt ratio.
#' @references
#' Scheinost, A. C., Chavernas, A., Barrón, V., & Torrent, J. (1998).
#' Use and limitations of second-derivative diffuse reflectance spectroscopy in the visible to near-infrared range to identify and quantify Fe oxide minerals in soil. _Clays and Clay Minerals, 46(5), 528–536._ \doi{10.1346/CCMN.1998.0460506}
#'
#' Torrent, J., & Barron, V. (2008).
#' Diffuse Reflectance Spectroscopy. _Methods of Soil Analysis, 5, 367–385._ \doi{10.2136/sssabookser5.5.c13}
#'
#' @examples
#' # example code
#' library(OxSR)
#' data(data_cary)
#'
#' data_clean <- clean_sheet_cary(data_cary, prefix = "x")
#'
#' relation_hm_gt(data_clean)
#'
#' # With plot
#' relation_hm_gt(data_clean[,1:2], plot = TRUE)
#'
#' @export
#' @importFrom rlang .data
#'

relation_hm_gt <- function(data = data,
                           points_smoothing = 0.3,
                           hm_gt_limits = list(hm = c(535,585),
                                          gt = c(430,460)),
                           pv_tolerance =  c(1,1,1,1),
                           name_wave = "wave",
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
  if (any(is.na(data))) {
    data <- stats::na.omit(data)
  }

  # Wave columns
  colunas <- grep(
    pattern = paste0("^", name_wave, ""),
    ignore.case = T,
    names(data), value = TRUE
  )

  if (length(colunas) == 0) {
    stop("The `data` does not contain the `wavelength` column.")
  }

  if (length(colunas) > 1) {
    data <- data |>
      dplyr::select(-colunas[-1]) |>
      dplyr::rename("wavelength" = colunas[1])
  }

  # Kubelka-Munk transformation
  df_km <- data |> dplyr::mutate(dplyr::across(
    .cols = -dplyr::starts_with("wave"),
    .fns = ~ (1 - . / 100)^2 / (2 * . / 100)
  ))

  # smoothing end second derivative
  resultados <- list()

  for (i in 2:length(df_km)) {
    coluna_nome <- names(df_km)[i]
    #
    smoot <- stats::smooth.spline(x = df_km[[1]],
                           y = df_km[[i]],
                           spar = points_smoothing)

    #
    predict_smoot <- stats::predict(smoot, x = smoot$x, deriv = 2)

    resultados[[coluna_nome]] <- data.frame(x = predict_smoot$x,
                                            y = predict_smoot$y)
  }

  resultados_df <- do.call(cbind, lapply(resultados, function(df) df$y))
  colnames(resultados_df) <- names(resultados)
  resultados_df <- data.frame(wavelength = resultados[[1]]$x,
                              resultados_df)


  result_list <- list()
  result_listdt <- list()

    for (i in 2:length(resultados_df)) {

      min_gt <- min(resultados_df[resultados_df[[1]] >= (415 - pv_tolerance[1]) & resultados_df[[1]] <= (425 + pv_tolerance[1]), i], na.rm = TRUE)
      max_gt <- max(resultados_df[resultados_df[[1]] >= (440 - pv_tolerance[2]) & resultados_df[[1]] <= (450 + pv_tolerance[2]), i], na.rm = TRUE)
      min_hm <- min(resultados_df[resultados_df[[1]] >= (530 - pv_tolerance[3]) & resultados_df[[1]] <= (545 + pv_tolerance[3]), i], na.rm = TRUE)
      max_hm <- max(resultados_df[resultados_df[[1]] >= (575 - pv_tolerance[4]) & resultados_df[[1]] <= (590 + pv_tolerance[4]), i], na.rm = TRUE)

      result_list[[i - 1]] <- data.frame(samples = colnames(resultados_df)[i], min_gt, max_gt, min_hm, max_hm)

      #
      min_gtdt <- resultados_df[resultados_df[[1]] >= 415 & resultados_df[[1]] <= 425, c(1,i)]
      line_gtmin <- min_gtdt[which.min(min_gtdt[[2]]), ]

      max_gtdt <- resultados_df[resultados_df[[1]] >= 440 & resultados_df[[1]] <= 450, c(1,i)]
      line_gtmax <- max_gtdt[which.max(max_gtdt[[2]]), ]

      min_hmdt <- resultados_df[resultados_df[[1]] >= 530 & resultados_df[[1]] <= 545, c(1,i)]
      line_hmmin <- min_hmdt[which.min(min_hmdt[[2]]), ]

      max_hmdt <- resultados_df[resultados_df[[1]] >= 575 & resultados_df[[1]] <= 590, c(1,i)]
      line_hmmax <- max_hmdt[which.max(max_hmdt[[2]]), ]

      base_1 <- rbind(line_gtmin, line_gtmax, line_hmmin, line_hmmax)
      base_1 <- cbind(base_1, sample = colnames(resultados_df)[i])
      colnames(base_1) <- c("x","y", "sample")
      result_listdt[[i - 1]] <- base_1

    }

    base_ <- do.call(rbind, result_list)

    base_$range_gt <- abs(base_$min_gt - base_$max_gt)
    base_$range_hm <- abs(base_$min_hm - base_$max_hm)

    base_$y2_y2y2 <- base_$range_hm / (base_$range_hm + base_$range_gt)

    # Regression model
    base_$relation_hm_gt <- round((-0.068 + (1.325 * base_$y2_y2y2)), digits = 4)
    base_ <- base_[,c(1:7,9)]


  # Plot - peak and valley
  if (plot == TRUE) {
    # Rectangles of the minerals in the graph.
    rect_data <- data.frame(
      mineral = c("Hm", "Gt"),
      xmin = c(hm_gt_limits[[1]][1], hm_gt_limits[[2]][1]),
      xmax = c(hm_gt_limits[[1]][2], hm_gt_limits[[2]][2]),
      ymin = c(-Inf, -Inf),
      ymax = c(+Inf, +Inf)
    )

    for (i in 2:ncol(resultados_df)) {

      col_select_fix <- colnames(resultados_df)[1]
      col_select <- colnames(resultados_df)[i]

      df_split <- resultados_df[,c(col_select_fix,col_select)]
      colnames(df_split) <- c("x","y")

      p1 <- ggplot2::ggplot(
        data = df_split,
        ggplot2::aes(x = .data[["x"]], y = .data[["y"]])
      ) +
        ggplot2::ggtitle(paste("Sample: ", col_select)) +
        ggplot2::geom_hline(yintercept = 0, col = "gray70", alpha = .9) +
        ggplot2::geom_line(linewidth = .6, na.rm = TRUE) +
        ggplot2::annotate(na.rm = TRUE,
          geom = "rect", xmin = rect_data$xmin, xmax = rect_data$xmax,
          ymin = rect_data$ymin, ymax = rect_data$ymax,
          fill = "gold", alpha = 0.2, color = "transparent"
        ) +
        ggplot2::geom_point(na.rm = TRUE,
          data = result_listdt[[i - 1]], mapping = ggplot2::aes(
            x = .data[["x"]],
            y = .data[["y"]],
            fill = "Points for range"
          ),
          col = "white",
          pch = 21,
          size = 3
        ) +
        ggplot2::geom_text(na.rm = TRUE,
          data = rect_data,
          ggplot2::aes(
            x = (.data[["xmin"]] + .data[["xmax"]]) / 2,
            y = Inf, label = .data[["mineral"]]
          ),
          vjust = 1.2,
          inherit.aes = FALSE
        ) +
        ggplot2::scale_x_continuous(
          name = "Wavelength (nm)",
          expand = ggplot2::expansion(
            mult = c(0.01, 0),
            add = c(0, 0)
          ),
          limits = c(380, 1000)
        ) +
        ggplot2::scale_y_continuous(
          name = NULL,
          expand = ggplot2::expansion(
            mult = c(0.1, 0.1),
            add = c(0, 0.0001)
          )
        ) +
        ggplot2::geom_text(
          data = base_[i - 1, ],
          mapping = ggplot2::aes(
            x = 880, y = +Inf,
            label = paste("Hm/(Hm+Gt): ", format(.data[["relation_hm_gt"]], digits = 2))
          ),
          col = "black",
          parse = TRUE,
          size = 5, vjust = 1.2
        ) +
        ggplot2::guides(
          fill  = ggplot2::guide_legend(position = "inside")
        ) +
        ggplot2::labs(fill = NULL) +
        ggplot2::scale_fill_manual(values = "red") +
        ggplot2::coord_cartesian(clip = "off") +
        ggplot2::theme_bw() +
        ggplot2::theme(
          legend.position.inside = c(0.8, 0.85),
          legend.text = ggplot2::element_text(size = 14),
          legend.background = ggplot2::element_blank()
        )

 print(p1)
    }
  }

  return(base_)
}

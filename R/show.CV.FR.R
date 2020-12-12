#' show.CV.FR
#'
#' @param obj
#' Un dataframe d'objets a afficher
#' @param option
#' l'option du graphique a sortir
#'
#' @description
#' Affiche un CV facon Francais
#'
#' @export
#'
#'
show.CV.FR <- function(obj, option = 1) {
  M <- 10
  if (dim(obj[is.na(obj[, 3]) == FALSE, ])[1] < 3) {
    if (FALSE %in% is.na(obj[, 3])) {
      knitr::kable(
        head(obj, n = dim(obj)[1]),
        caption = "",
        format = "html",
        booktabs = T,
        linesep = " "
      ) %>%
        kableExtra::kable_styling(latex_options = c("striped", "HOLD_position"))
    } else{
      knitr::kable(
        head(obj[, 1:2], n = dim(obj)[1]),
        caption = "",
        format = "html",
        booktabs = T,
        linesep = " "
      ) %>%
        kableExtra::kable_styling(latex_options = c("striped", "HOLD_position"))
    }
  } else {
    # information graphique
    switch(option,
           {
             #radarchart
             # Create data
             data <-
               as.data.frame(matrix(obj[is.na(obj[, 3]) == FALSE, ][, 3], nrow = 1))
             colnames(data) <- obj[is.na(obj[, 3]) == FALSE, ][, 2]
             # adding scale for each variable
             n <- length(obj[is.na(obj[, 3]) == FALSE, ][, 2])
             #M<-max(obj[is.na(obj[,3])==FALSE,][,3])
             data <- rbind(rep(M, n) , rep(0, n) , data)
             # The default radar chart
             radarchart(data)
           },
           {
             #barplot
             ggplot2::ggplot(data = obj[is.na(obj[, 3]) == FALSE, ], aes(
               x = obj[is.na(obj[, 3]) == FALSE, ][, 2],
               y = obj[is.na(obj[, 3]) == FALSE, ][, 3],
               fill = obj[is.na(obj[, 3]) == FALSE, ][, 3]
             )) +
               geom_bar(width = 1, stat = "identity") +
               coord_flip() +
               theme_void() +
               theme(legend.position = "none") + theme(axis.text.y = element_text()) +
               facet_grid(rows = vars(obj[is.na(obj[, 3]) == FALSE, ][, 1]),
                          scales = "free_y",
                          space = "free_y")
           },
           {
             #piechart
             # Compute the position of labels
             obj$prop <- obj[, 3] / sum(obj[, 3]) * 100
             obj$ypos <- cumsum(obj$prop) - 0.5 * obj$prop
             #plot
             ggplot2::ggplot(data = obj, aes(
               x = "",
               y = prop,
               fill = rev(factor(obj[, 2], levels = obj[, 2]))
             )) +
               geom_bar(width = 1, stat = "identity") +
               coord_polar("y", start = 0) +
               theme_void() +
               theme(legend.position = "none") +
               geom_text(
                 aes(y = ypos, label = obj[, 2]),
                 color = "black",
                 size = 4,
                 nudge_x = 0.2
               ) +
               scale_fill_brewer(palette = "Set3")
           })

    #information non graphique
    if (dim(obj[is.na(obj[, 3]) == TRUE, ][, 1:2])[1] > 0) {
      knitr::kable(
        head(obj[is.na(obj[, 3]) == TRUE, ][, 1:2], n = dim(obj[is.na(obj[, 3]) ==
                                                                  TRUE, ][, 1:2])[1]),
        caption = "",
        format = "html",
        booktabs = T,
        linesep = " ",
        row.names = FALSE
      ) %>%
        kableExtra::kable_styling(latex_options = c("striped", "HOLD_position"))
    }
  }
}

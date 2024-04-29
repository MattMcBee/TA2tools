# library(data.table)
# library(tidyr)
# library(magrittr)
# library(here)
# library(ggplot2)
# library(jsonlite)
# library(stringr)
# library(dplyr)
# devtools::install_github("davidsjoberg/ggsankey")
# library(ggsankey)


# define negation of %in%
"%!in%" <- Negate("%in%")


#' process list elements in imported JSON
#'
#' @param x a list of elements that should be converted to lists
#'
#' @return list of lists
#' @export
#'
fixlist <- function(x) {
  x <- lapply(x, as.list)
  return(x)
}



#' Create the flow dataset expected for sankey plot
#'
#' @param dat a data.table
#' @param filter_string string: filter statements for row selection. defaults to NULL for no filtering
#' @param weight_nodesize logical: if TRUE node sizes are adjusted for values found in the 'weight' column.
#'   defaults to FALSE
#' @param sankey_vars string: the variables to be plotted. order is preserved
#' @param valid_intent a character vector of valid intents. used to remove hallucinations.
#' @param remove_TA1_multi_cat logical: should multi-category TA1 labels be retained? defaults to TRUE
#' @param n_threshold scalar: sets the minimum sample size threshold for including an element in the Sankey plot. defaults to 1000
#'
#' @return a data.table suitable for Sankey plotting
#' @export

prep_for_sankey <- function(dat,
                            filter_string = NULL,
                            weight_nodesize = FALSE,
                            sankey_vars = c("ta1_categories", "detail.categoryMain", "entity_type"),
                            valid_intent = c("complaint", "return", "praise", "other", "comparison", "suggestion", "recommendation"),
                            remove_TA1_multi_cat = TRUE,
                            n_threshold = 1000) {
  dat <- data.table::copy(dat)
  # fix super annoying DT bug! https://github.com/Rdatatable/data.table/issues/3745
  dat <- dat |>
    data.table::data.table() |>
    tibble::as_tibble() |>
    data.table::as.data.table()


  if (!is.null(filter_string)) {
    dat <- dplyr::filter(dat, eval(parse(text = filter_string)))
  }

  if (remove_TA1_multi_cat == TRUE) {
    dat <- dat[!grepl(";", ta1_categories), ]
  }

  if (!is.null(valid_intent)) {
    dat <- dat[detail.intent %in% valid_intent, ]
  }

  reduced_dat <- data.table::copy(dat)

  if ("ta1_categories" %in% sankey_vars & !is.null(n_threshold)) {
    ta_cat_table <- dat[, .N, ta1_categories][N > n_threshold, ]

    reduced_dat <- data.table::merge(reduced_dat,
      ta_cat_table,
      by = "ta1_categories",
    )
    reduced_dat[, N := NULL]
  }

  if ("detail.categoryMain" %in% sankey_vars & !is.null(n_threshold)) {
    ta_cat_table <- dat[, .N, detail.categoryMain][N > n_threshold, ]

    reduced_dat <- data.table::merge(reduced_dat,
      ta_cat_table,
      by = "detail.categoryMain",
    )
    reduced_dat[, N := NULL]
  }

  if ("detail.categorySub" %in% sankey_vars & !is.null(n_threshold)) {
    ta_cat_table <- dat[, .N, detail.categorySub][N > n_threshold, ]

    reduced_dat <- data.table::merge(reduced_dat,
      ta_cat_table,
      by = "detail.categorySub",
    )
    reduced_dat[, N := NULL]
  }

  if ("raw_entity" %in% sankey_vars & !is.null(n_threshold)) {
    ta_cat_table <- dat[, .N, raw_entity][N > n_threshold, ]

    reduced_dat <- data.table::merge(reduced_dat,
      ta_cat_table,
      by = "raw_entity",
    )
    reduced_dat[, N := NULL]
  }


  dat <- dat[id %in% unique(reduced_dat$id), ]

  cat("Number of unique comments:", dat[, .N, UID][, .N], "\n")

  if (weight_nodesize == TRUE) {
    dat_sankey <- data.table::as.data.table(make_long(reduced_dat, all_of(sankey_vars), value = weight))
  } else {
    dat_sankey <- data.table::as.data.table(make_long(reduced_dat, all_of(sankey_vars)))
  }

  dat_sankey[node == "", node := "(missing)"]

  return(dat_sankey)
}



#' Make a sankey plot of new TA data
#'
#' @param dat a data.table
#' @param filter_string string: filter statements for row selection. defaults to NULL for no filtering
#' @param plot_title string: title assigned to plot
#' @param save_plot logical: TRUE saves the file. Defaults to FALSE
#' @param plot_type character: currently only supports "sankey", which is the default
#' @param filename character: path and filename for the saved file. Only used if save_plot=TRUE. Defaults to NULL.
#' @param fig.scale scalar: figure scale. defaults to 1.2
#' @param fig.width scalar: figure width. defauts to 8
#' @param fig.height scalar: figure height. defaults to 6
#' @param sankey_vars string: the variables to be plotted. order is preserved. defaults to
#'  c("ta1_categories", "detail.categoryMain", "entity_type"),
#' @param weight_by character: if TRUE node sizes are adjusted for values found in the 'weight' column.
#'   defaults to FALSE
#' @param valid_intent a character vector of valid intents. used to remove hallucinations. defaults to
#'  c("complaint", "return", "praise", "other", "comparison", "suggestion", "recommendation"),
#' @param remove_TA1_multi_cat logical: should multi-category TA1 labels be retained? defaults to TRUE
#' @param n_threshold scalar: sets the minimum sample size threshold for including an element in the Sankey plot. defaults to 1000
#' @return a ggplot2 object
#' @export
#'
make_sankey <- function(dat,
                        filter_string = NULL,
                        plot_title = NULL,
                        save_plot = FALSE,
                        plot_type = "sankey",
                        filename = NULL,
                        fig.scale = 1.2,
                        fig.width = 8,
                        fig.height = 7,
                        sankey_vars = c("ta1_categories", "detail.categoryMain", "entity_type"),
                        weight_by = NULL,
                        valid_intent = c("complaint", "return", "praise", "other", "comparison", "suggestion", "recommendation"),
                        remove_TA1_multi_cat = TRUE,
                        n_threshold = 1000) {
  dat <- data.table::copy(dat)

  if (!is.null(weight_by)) {
    # create weights
    dat[, N := .N, by = weight_by]
    dat[, weight := 1 / N]
    dat[, N := NULL]
  }

  sankey_dat <- prep_for_sankey(
    dat = dat,
    sankey_vars = sankey_vars,
    filter_string = filter_string,
    n_threshold = n_threshold,
    weight_nodesize = !is.null(weight_by),
    valid_intent = valid_intent,
    remove_TA1_multi_cat = remove_TA1_multi_cat
  )


  sankey_dat[, x := as.character(x)]
  sankey_dat[, next_x := as.character(next_x)]

  new_sankey_vars <- stringr::str_replace_all(sankey_vars, pattern = c(
    "ta1_categories" = "TA 1 category",
    "detail.categoryMain" = "ActiveTA category",
    "entity_type" = "ActiveTA entity type",
    "detail.intent" = "ActiveTA intent",
    "detail.categorySub" = "ActiveTA subcategory",
    "raw_entity" = "ActiveTA raw entity"
  ))

  sankey_dat[, x := stringr::str_replace_all(x, pattern = c(
    "ta1_categories" = "TA 1 category",
    "detail.categoryMain" = "ActiveTA category",
    "entity_type" = "ActiveTA entity type",
    "detail.intent" = "ActiveTA intent",
    "detail.categorySub" = "ActiveTA subcategory",
    "raw_entity" = "ActiveTA raw entity"
  ))]

  sankey_dat[, next_x := stringr::str_replace_all(next_x, pattern = c(
    "ta1_categories" = "TA 1 category",
    "detail.categoryMain" = "ActiveTA category",
    "entity_type" = "ActiveTA entity type",
    "detail.intent" = "ActiveTA intent",
    "detail.categorySub" = "ActiveTA subcategory",
    "raw_entity" = "ActiveTA raw entity"
  ))]

  sankey_dat[, x := factor(x, levels = new_sankey_vars)]
  sankey_dat[, next_x := factor(next_x, levels = new_sankey_vars)]


  if (is.null(weight_by)) {
    p <- ggplot2::ggplot(
      data = sankey_dat,
      ggplot2::aes(
        x = x,
        next_x = next_x,
        node = node,
        next_node = next_node,
        fill = factor(node),
        label = node
      )
    )
  } else {
    p <- ggplot2::ggplot(
      data = sankey_dat,
      ggplot2::aes(
        x = x,
        next_x = next_x,
        node = node,
        next_node = next_node,
        fill = factor(node),
        label = node,
        value = value
      )
    )
  }
  if (plot_type == "sankey") {
    p <- p + ggsankey::geom_sankey(flow.alpha = .6, node.color = "gray30") +
      ggsankey::geom_sankey_label(size = 3, color = "white", fill = "gray40")
  } else if (plot_type == "alluvial") {
    p <- p + ggsankey::geom_alluvial(flow.alpha = .6, node.color = "gray30") +
      ggsankey::geom_alluvial_label(size = 3, color = "white", fill = "gray40")
  }
  p <- p +
    ggplot2::scale_fill_viridis_d() +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_text(hjust = .5),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_blank()
    ) #+
  # scale_x_discrete(breaks=x_labels)

  if (!is.null(plot_title)) {
    p <- p + ggplot2::ggtitle(plot_title)
  }

  if ("ta1_categories" %in% sankey_vars) {
    p <- p + ggplot2::geom_vline(xintercept = 1.5, linewidth = 1, alpha = .6, linetype = "dashed", color = "red")
  }

  if (save_plot == TRUE) {
    cat("saving plot\n")
    ggplot2::ggsave(
      filename = here::here("plots", filename), plot = p, scale = fig.scale,
      height = fig.height, width = fig.width
    )
  }

  return(p)
}



#' Process list elements in imported JSON
#'
#' @param x a list of elements that should be converted to lists
#'
#' @return list of lists
#' @export
#'
fixlist <- function(x) {
  x <- lapply(x, as.list)
  return(x)
}

#' Converting TA2 JSON to a data.table
#'
#' @param path_to_file string: path to the JSON file
#'
#' @return a data.table
#' @export
#'
read_ta2_json <- function(path_to_file) {
  # import TA 2.1 JSON
  ta2_json_str <- readLines(path_to_file)

  # replace any structure-breaking elements in the JSON that could prevent processing
  #   not needed in latest Dwight output
  # ta2_json_str = str_replace_all(string=ta2_json_str, pattern="NaN", replacement='""')

  # convert JSON string to list object
  ta2_list <- jsonlite::fromJSON(ta2_json_str, simplifyDataFrame = TRUE, flatten = TRUE)

  # create a base data.table from the list
  ta2 <- data.table::rbindlist(ta2_list, fill = T, idcol = "UID")

  # get a vector of the colnames that could be lists
  list_colnames <- grep("entities", names(ta2), fixed = T, value = T)

  # apply a function converting all these to lists.
  #  could just use as.list here but the function previously applied some other
  #  transformations which were later removed
  for (i in 1:length(list_colnames)) {
    ta2[[list_colnames[i]]] <- lapply(ta2[[list_colnames[i]]], as.list)
    # fixlist(ta2[[list_colnames[i]]])
  }

  # pivot long to collect all lists in a single column, this will allow them to be
  #  busted into rows
  ta2_long <- data.table::data.table(
    tidyr::pivot_longer(
      data = ta2,
      cols = contains("entities"),
      names_to = "entity_type",
      names_prefix = "detail.entities.",
      values_to = "raw_entity"
    )
  )

  # unnest the lists containing multiple raw entities to multiple rows
  ta2_long <- tidyr::unnest_longer(ta2_long, col = "raw_entity")

  ta2_long <- data.table::data.table(ta2_long)

  # clean up the entities - ensure they are no longer length-1 lists and do not contain NULL
  ta2_long[, raw_entity := as.character(raw_entity)]
  ta2_long[, raw_entity := stringr::str_replace_all(raw_entity, "NULL", "")]


  return(ta2_long)
}



#' Calculates "meta impact" - the difference in impact for positive vs negative sentiment
#'
#' @param data a data.table containing impact
#'
#' @return a tibble
#' @export
#'
calc_meta_impact <- function(data) {
  data <- data.table::data.table(data)
  data <- data.table::copy(data)

  data[, detail.sentiment := factor(detail.sentiment, levels = c(-1, 0, 1), labels = c("negative", "neutral", "positive"))]

  data_wide <- tidyr::pivot_wider(
    data = data, names_from = "detail.sentiment", names_prefix = "sentiment_",
    values_from = c("excluded_n", "osat_excluded", "impact")
  )

  data_wide <- data.table::data.table(data_wide)

  data_wide[, meta_impact := impact_sentiment_positive - impact_sentiment_negative]
  data.table::setorder(data_wide, -meta_impact)

  data.table::setcolorder(data_wide, neworder = c(1, length(names(data_wide)), 3:(length(names(data_wide)) - 1)))

  data_wide <- data_wide[!is.na(meta_impact), ]

  return(tibble::as_tibble(data_wide))
}




#' Calculate impact
#'
#' @param data placeholder
#' @param metric placeholder
#' @param impact_cols placeholder
#' @param by_cols placeholder
#' @param method placeholder
#' @param sort_by placeholder
#' @param sort_order placeholder
#'
#' @return a tibble
#' @export

calc_impact <- function(data,
                        metric,
                        impact_cols,
                        by_cols = NA,
                        method = "tobias",
                        sort_by = "abs_impact",
                        sort_order = -1) {
  # make copy of data to avoid modification
  data <- data.table::copy(data)

  # assign a fake column for aggregation if aggregation_var is NULL
  if (is.na(by_cols)) {
    by_cols <- "dummy"
    data[, dummy := 1]
  }

  if (method == "tobias") {
    # get one row per UID
    data_UID <- data[, first(.SD), UID]

    # df1 is the full data set (optionally by by_cols)
    df1 <- data_UID[, lapply(.SD, mean, na.rm = TRUE), by = by_cols, .SDcols = metric]
    df1_n <- data_UID[, .(total_n = .N), by = by_cols]

    df1 <- data.table::merge(df1, df1_n, by = by_cols)

    # change name of metric var (typically osat)
    data.table::setnames(df1, old = metric, new = paste0(metric, "_all"))

    # df2 will include mean metric value when target case is omitted
    # this will be a data table with varying number of columns
    var_levels_list <- data.table::split(unique(data[, ..impact_cols]), by = impact_cols)

    # each list element will contain a dataframe
    # the underlying function is using an anti-join
    df2_list <- lapply(
      X = var_levels_list, FUN = drop_obs2,
      data = data, metric = metric, by_cols = by_cols
    )

    df2 <- data.table::data.table(
      data.table::rbindlist(df2_list)
    )

    # remove duplicated column names that occur if a column is in both by_cols and impact_cols
    df2 <- df2[, .SD, .SDcols = unique(names(df2))]

    linked <- data.table::merge(
      df1,
      df2,
      by = by_cols
    )

    linked[["impact"]] <- linked[[paste0(metric, "_all")]] - linked[[paste0(metric, "_excluded")]]
  }

  # linked[, impact := osat_all - osat_excluded]
  suppressWarnings(
    linked[, abs_impact := abs(impact)]
  )

  data.table::setorderv(linked, sort_by, sort_order)

  # if dummy column was added, drop it
  if ("dummy" %in% names(linked)) {
    linked[, dummy := NULL]
  }

  # drop abs impact
  linked[, abs_impact := NULL]

  return(tibble::as_tibble(linked))
}

drop_obs2 <- function(data, drop_levels_dt, metric, by_cols) {
  # do an anti-join to get the data where the drop_levels are not found
  # reduced_data = data.table(dplyr::anti_join(data, drop_levels_dt, by=names(drop_levels_dt)))

  # here's what we need to do
  # find the UIDs that match
  # drop them
  UIDs_to_drop <- data.table::merge(data, drop_levels_dt,
    by =
      names(drop_levels_dt)
  )[, .(UID = unique(UID)), by_cols]

  # get reduced data - this has one row per UID
  reduced_data <- data[UID %!in% UIDs_to_drop$UID][, first(.SD), UID]

  reduced_mean <- reduced_data[, lapply(.SD, mean, na.rm = T), .SDcols = metric, by = by_cols]


  reduced_n <- UIDs_to_drop[, .(excluded_n = .N), by_cols]


  reduced_mean <- data.table::merge(
    reduced_mean,
    reduced_n,
    by = by_cols
  )


  reduced_mean <- cbind(reduced_mean, drop_levels_dt)

  data.table::setnames(reduced_mean, old = metric, new = paste0(metric, "_excluded"))

  return(reduced_mean)
}

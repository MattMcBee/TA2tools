"%!in%" <- Negate("%in%")

#' fixlist: function for processing list elements in imported JSON
#'
#' @param x a list of elements that should be converted to lists
#'
#' @return list of lists
#' 
fixlist = function(x) {
  x = lapply(x, as.list)
  return(x)
}



#' prep_for_sankey: function for creating the flow dataset expected for sankey plot
#'
#' @param dat a data.table
#' @param filter_string string: filter statements for row selection. defaults to NULL for no filtering
#' @param weight_nodesize logical: if TRUE node sizes are adjusted for values found in the 'weight' column. 
#'   defaults to FALSE
#' @param sankey_vars string: the variables to be plotted. order is preserved
#' @param valid_intent a character vector of valid intents. used to remove hallucinations. 
#' @param remove_TA1_multi_cat 
#' @param n_threshold 
#'
#' @return
#' @export
#'
#' @examples
prep_for_sankey = function(dat, 
                           filter_string = NULL, 
                           weight_nodesize = FALSE,
                           sankey_vars = c("ta1_categories", "detail.categoryMain", "entity_type"),
                           valid_intent = c("complaint", "return", "praise", "other", "comparison", "suggestion", "recommendation"),
                           remove_TA1_multi_cat = TRUE, 
                           n_threshold=1000) {
  
  dat = copy(dat)
  # fix super annoying DT bug! https://github.com/Rdatatable/data.table/issues/3745
  dat <- dat %>% data.table() %>% dplyr::as_tibble() %>% as.data.table() 
  

  if (!is.null(filter_string)) {
    dat = dplyr::filter(dat, eval(parse(text=filter_string)))
  }
  
  if (remove_TA1_multi_cat == TRUE) {
    dat = dat[!grepl(";", ta1_categories), ]
  }
  
  if (!is.null(valid_intent)) {
    dat = dat[detail.intent %in% valid_intent,]
  }
  
  reduced_dat = copy(dat)
  
  if ("ta1_categories" %in% sankey_vars & !is.null(n_threshold)) {
    ta_cat_table = dat[, .N, ta1_categories][N > n_threshold,]

    reduced_dat = merge(reduced_dat,
                        ta_cat_table,
                        by="ta1_categories",
    )
    reduced_dat[, N := NULL]
  }
  
  if ("detail.categoryMain" %in% sankey_vars & !is.null(n_threshold)) {
    ta_cat_table = dat[, .N, detail.categoryMain][N > n_threshold,]

    reduced_dat = merge(reduced_dat,
                        ta_cat_table,
                        by="detail.categoryMain",
    )
    reduced_dat[, N := NULL]
  }

  if ("detail.categorySub" %in% sankey_vars & !is.null(n_threshold)) {
    ta_cat_table = dat[, .N, detail.categorySub][N > n_threshold,]

    reduced_dat = merge(reduced_dat,
                        ta_cat_table,
                        by="detail.categorySub",
    )
    reduced_dat[, N := NULL]
  }
  
  if ("raw_entity" %in% sankey_vars & !is.null(n_threshold)) {
    ta_cat_table = dat[, .N, raw_entity][N > n_threshold,]
    
    reduced_dat = merge(reduced_dat,
                        ta_cat_table,
                        by="raw_entity",
    )
    reduced_dat[, N := NULL]
  }
  

  dat = dat[id %in% unique(reduced_dat$id), ]
  
  cat('Number of unique comments:', dat[, .N, UID][, .N], '\n')

  if (weight_nodesize == TRUE) {
    dat_sankey = as.data.table(make_long(reduced_dat, all_of(sankey_vars), value=weight))
  } else {
    dat_sankey = as.data.table(make_long(reduced_dat, all_of(sankey_vars)))
  }
  
  dat_sankey[node =='', node := "(missing)"]
  
  return(dat_sankey)
  
}



#' Title
#'
#' @param dat 
#' @param filter_string 
#' @param plot_title 
#' @param save_plot 
#' @param plot_type 
#' @param filename 
#' @param fig.scale 
#' @param fig.width 
#' @param fig.height 
#' @param sankey_vars 
#' @param weight_by 
#' @param valid_intent 
#' @param remove_TA1_multi_cat 
#' @param n_threshold 
#'
#' @return
#' @export
#'
#' @examples
make_sankey = function(dat, 
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
                       n_threshold=1000) {
  
  dat = copy(dat)
  
  if (!is.null(weight_by)) {
    # create weights
    dat[, N := .N, by = weight_by]
    dat[, weight := 1/N]
    dat[, N := NULL]
  }
  
  sankey_dat = prep_for_sankey(dat = dat, 
                               sankey_vars = sankey_vars,
                               filter_string = filter_string,
                               n_threshold = n_threshold,
                               weight_nodesize = !is.null(weight_by),
                               valid_intent = valid_intent,
                               remove_TA1_multi_cat =  remove_TA1_multi_cat)
  

  sankey_dat[, x := as.character(x)]
  sankey_dat[, next_x := as.character(next_x)]
  
  new_sankey_vars =  str_replace_all(sankey_vars, pattern = c(
    "ta1_categories" = "TA 1 category",
    "detail.categoryMain" = "ActiveTA category", 
    "entity_type" = "ActiveTA entity type", 
    "detail.intent" = "ActiveTA intent",
    "detail.categorySub" = "ActiveTA subcategory",
    "raw_entity" = "ActiveTA raw entity")
  )
  
  sankey_dat[, x := str_replace_all(x, pattern = c(
    "ta1_categories" = "TA 1 category",
    "detail.categoryMain" = "ActiveTA category", 
    "entity_type" = "ActiveTA entity type", 
    "detail.intent" = "ActiveTA intent",
    "detail.categorySub" = "ActiveTA subcategory",
    "raw_entity" = "ActiveTA raw entity")
  )]
  
  sankey_dat[, next_x := str_replace_all(next_x, pattern = c(
    "ta1_categories" = "TA 1 category",
    "detail.categoryMain" = "ActiveTA category", 
    "entity_type" = "ActiveTA entity type", 
    "detail.intent" = "ActiveTA intent",
    "detail.categorySub" = "ActiveTA subcategory",
    "raw_entity" = "ActiveTA raw entity")
  )]
  
  sankey_dat[, x := factor(x, levels=new_sankey_vars)]
  sankey_dat[, next_x := factor(next_x, levels=new_sankey_vars)]
  
  
  if (is.null(weight_by)) {
    
    p = ggplot(data = sankey_dat,
               aes(x = x,
                   next_x = next_x, 
                   node = node, 
                   next_node = next_node,
                   fill = factor(node),
                   label = node
               ))
  } else {
    p = ggplot(data = sankey_dat,
               aes(x = x,
                   next_x = next_x, 
                   node = node, 
                   next_node = next_node,
                   fill = factor(node),
                   label = node, 
                   value = value
               ))
  }
  if (plot_type == "sankey") {
    p = p + geom_sankey(flow.alpha = .6, node.color = "gray30") +
      geom_sankey_label(size = 3, color = "white", fill = "gray40")
  } else if (plot_type == "alluvial") {
    p = p + geom_alluvial(flow.alpha = .6, node.color = "gray30") +
      geom_alluvial_label(size = 3, color = "white", fill = "gray40")
  }
  p =  p +
    scale_fill_viridis_d() + 
    labs(x = NULL, y = NULL) +
    theme_classic()+
    theme(legend.position = "none",
          plot.title = element_text(hjust = .5),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.y = element_blank()) #+
  #scale_x_discrete(breaks=x_labels) 
  
  if (!is.null(plot_title)) {
    p = p + ggtitle(plot_title) 
  }
  
  if ("ta1_categories" %in% sankey_vars) {
    p = p + geom_vline(xintercept = 1.5, linewidth=1, alpha=.6, linetype="dashed", color="red") 
  }
  
  if (save_plot == TRUE) {
    cat("saving plot\n")
    ggsave(filename=here("plots", filename), plot=p, scale=fig.scale, height=fig.height, width=fig.width)
  }
  
  return(p)
  
}



#' fixlist: function for processing list elements in imported JSON
#'
#' @param x a list of elements that should be converted to lists
#'
#' @return list of lists
#' 
fixlist = function(x) {
  x = lapply(x, as.list)
  return(x)
}

#' read_ta2_json: function for converting TA2 JSON to a data.table
#'
#' @param path_to_file 
#'
#' @return
#' @export
#'
#' @examples
read_ta2_json = function(path_to_file) {
  
  # import TA 2.1 JSON
  ta2_json_str = readLines(path_to_file)
  
  # replace any structure-breaking elements in the JSON that could prevent processing
  #   not needed in latest Dwight output
  #ta2_json_str = str_replace_all(string=ta2_json_str, pattern="NaN", replacement='""')
  
  # convert JSON string to list object
  ta2_list = jsonlite::fromJSON(ta2_json_str, simplifyDataFrame=TRUE, flatten=TRUE)
  
  # create a base data.table from the list
  ta2 = rbindlist(ta2_list, fill=T, idcol="UID")
  
  # get a vector of the colnames that could be lists
  list_colnames = grep("entities", names(ta2), fixed=T, value=T)
  
  # apply a function converting all these to lists. 
  #  could just use as.list here but the function previously applied some other 
  #  transformations which were later removed
  for (i in 1:length(list_colnames)) {
    ta2[[list_colnames[i]]] = lapply(ta2[[list_colnames[i]]], as.list)
    #fixlist(ta2[[list_colnames[i]]])
  }
  
  # pivot long to collect all lists in a single column, this will allow them to be
  #  busted into rows
  ta2_long = data.table(
    pivot_longer(data=ta2, 
                 cols=contains("entities"), 
                 names_to = "entity_type", 
                 names_prefix="detail.entities.",
                 values_to="raw_entity")
  )
  
  # unnest the lists containing multiple raw entities to multiple rows
  ta2_long = unnest_longer(ta2_long, col="raw_entity")
  
  ta2_long = data.table(ta2_long)
  
  # clean up the entities - ensure they are no longer length-1 lists and do not contain NULL
  ta2_long[, raw_entity := as.character(raw_entity)]
  ta2_long[, raw_entity := str_replace_all(raw_entity, "NULL", "")]
  
  
  return(ta2_long)
}


# 
# drop_obs = function(data, TA_var, drop_level, metric, aggregation_var) {
#   
#   # get the filtered data - filtered to one level of the TA_var
#   filter_string = paste0(TA_var, " %!in% '", drop_level, "'")
# 
#   filtered_data = dplyr::filter(data, eval(parse(text=filter_string)))
#   
#   # included n
#   unfilter_string = paste0(TA_var, " %in% '", drop_level, "'")
# 
#   unfiltered_data = dplyr::filter(data, eval(parse(text=unfilter_string)))
#   
#   filtered_data_mean = filtered_data[, lapply(.SD, mean, na.rm=TRUE), by=aggregation_var, .SDcols=metric]
#   setnames(filtered_data_mean, old=metric, new=paste0(metric, "_excluded"))
#   
#   filtered_data_n = filtered_data[!is.na(metric), .(excluded_n = .N), by=aggregation_var]
#   ubfiltered_data_n = unfiltered_data[!is.na(metric), .(included_n = .N), by=aggregation_var]
#   
#   filtered_data_mean = merge(filtered_data_mean, filtered_data_n, by=aggregation_var)
#   filtered_data_mean = merge(filtered_data_mean, ubfiltered_data_n, by=aggregation_var)
#   
#   return(filtered_data_mean)
# }
# 
# calc_impact = function(data, method="tobias", metric="osat", TA_var, aggregation_var=NULL, denominator_var=NULL) {
#   
#   data = copy(data)
#   
#   agg_var_levels = ifelse(is.null(aggregation_var),
#                           1, # if TRUE
#                           length(unique(data[[aggregation_var]])) # if FALSE
#   )
#   
#   #assign a fake column for aggregation if aggregation_var is NULL
#   if (is.null(aggregation_var)) {
#     aggregation_var = "dummy"
#     data[, dummy := 1]
#   }
#   
#   if (is.null(denominator_var)) {
#     denominator_var = "dummy_denom"
#     data[, dummy_denom := 1]
#   }
#   
#   if (method=="tobias") {
#     # df1
# 
#     df1 = data[, lapply(.SD, mean, na.rm=TRUE), by=denominator_var, .SDcols=target_var]
#     df1_n = data[, .(total_n = .N), by=denominator_var]
#     
#     df1 = merge(df1, df1_n, by=denominator_var)
#     
#  
#     setnames(df1, old=metric, new=paste0(metric, "_all"))
#     
#     df2_list = list()
# 
#     var_levels = unique(data[[TA_var]])
#     browser()
#     
#     # each list element will contain a dataframe where the info
#     df2_list = lapply(X=as.list(var_levels), FUN=drop_obs, 
#                       data=data, TA_var=TA_var, metric=metric, aggregation_var=aggregation_var)
#     
#     for (i in 1:length(df2_list)) {
#       df2_list[[i]][, TA_var := var_levels[i]]
#       setnames(df2_list[[i]], old="TA_var", new=TA_var)
#     }
#     
#     df2 = data.table(rbindlist(df2_list))
#     
#     # what to do about missing levels?
#     #    browser()
#     #df2[, x := rep(var_levels, each=agg_var_levels)]
#     #setnames(df2, old="x", new=TA_var)
#     
#     browser()
#     linked = merge(
#       df1,
#       df2,
#       by=aggregation_var
#     )
#     
#     linked[, impact := osat_all - osat_excluded]
#     
#     setorderv(linked, c(TA_var, "osat_all", "osat_excluded", "impact"))
#     
#     linked = linked[order(linked[[aggregation_var]]),]
#     
#     if ("dummy" %in% names(linked)) {
#       linked[, dummy := NULL]
#     }
#     
#     
#   }
#   
#   return(linked)
#   
# }
# 


calc_meta_impact = function(data) {
  
  data = data.table(data)
  data = copy(data)
  
  data[, detail.sentiment := factor(detail.sentiment, levels=c(-1, 0, 1), labels=c("negative", "neutral", "positive"))]
  
  data_wide = pivot_wider(data=data, names_from="detail.sentiment", names_prefix="sentiment_", 
                          values_from=c("excluded_n", "osat_excluded", "impact"))
  
  data_wide = data.table(data_wide)
  
  data_wide[, meta_impact := impact_sentiment_positive - impact_sentiment_negative]
  setorder(data_wide, -meta_impact)
  
  setcolorder(data_wide, neworder=c(1, length(names(data_wide)), 3:(length(names(data_wide))-1)))
  
  data_wide = data_wide[!is.na(meta_impact),]
  
  return(as_tibble(data_wide))
  
}


#### back to the drawing board
# get clear about what I want
# I want to be able to provide:
#  metric: usually OSAT
#  a vector of columns for calculating impact over, cannot be null
#  a vector of columns for setting the "denominator"; the by-groups for the calculation
#    this would be used for StoreId, for example
#    this vector should not have to be a subset of the impact columns
#  an optional sentiment column

calc_impact = function(data, 
                       metric, 
                       impact_cols, 
                       by_cols=NA, 
                       method="tobias", 
                       sort_by="abs_impact", 
                       sort_order=-1) {
  
  # make copy of data to avoid modification
  data = copy(data)
  
  #assign a fake column for aggregation if aggregation_var is NULL
  if (is.na(by_cols)) {
    by_cols = "dummy"
    data[, dummy := 1]
  }
  
  if (method == "tobias") {
    
    # get one row per UID
    data_UID = data[, first(.SD), UID]
    
    # df1 is the full data set (optionally by by_cols)
    df1 = data_UID [, lapply(.SD, mean, na.rm=TRUE), by=by_cols, .SDcols=metric]
    df1_n = data_UID[, .(total_n = .N), by=by_cols]
    
    df1 = merge(df1, df1_n, by=by_cols)
    
    # change name of metric var (typically osat)
    setnames(df1, old=metric, new=paste0(metric, "_all"))
    
    # df2 will include mean metric value when target case is omitted
    # this will be a data table with varying number of columns
    var_levels_list = split(unique(data[, ..impact_cols]), by=impact_cols)
    
    # each list element will contain a dataframe
    # the underlying function is using an anti-join
    df2_list = lapply(X=var_levels_list, FUN=drop_obs2, 
                      data=data, metric=metric, by_cols=by_cols)
    
    df2 = data.table(rbindlist(df2_list))
    
    # remove duplicated column names that occur if a column is in both by_cols and impact_cols
    df2 = df2[, .SD, .SDcols=unique(names(df2))]
    linked = merge(
      df1,
      df2,
      by=by_cols
    )
    
    linked[['impact']] = linked[[paste0(metric, "_all")]] - linked[[paste0(metric, "_excluded")]]
    
  }
  
  #linked[, impact := osat_all - osat_excluded]
  suppressWarnings(
    linked[, abs_impact := abs(impact)]
  )
  
  setorderv(linked, sort_by, sort_order)
  
  # if dummy column was added, drop it
  if ("dummy" %in% names(linked)) {
    linked[, dummy := NULL]
  }
  
  # drop abs impact
  linked[, abs_impact := NULL]
  
  return(as_tibble(linked))
  
}

drop_obs2 = function(data, drop_levels_dt, metric, by_cols) {
  
  # do an anti-join to get the data where the drop_levels are not found
  # reduced_data = data.table(dplyr::anti_join(data, drop_levels_dt, by=names(drop_levels_dt)))
  
  # here's what we need to do
  # find the UIDs that match
  # drop them
  UIDs_to_drop = merge(data, drop_levels_dt, by=names(drop_levels_dt))[, .(UID = unique(UID)), by_cols]
  
  # get reduced data - this has one row per UID
  reduced_data = data[UID %!in% UIDs_to_drop$UID][, first(.SD), UID]
  
  reduced_mean = reduced_data[, lapply(.SD, mean, na.rm=T), .SDcols=metric, by=by_cols]
  
  
  reduced_n = UIDs_to_drop[, .(excluded_n = .N), by_cols]
  
  
  reduced_mean = merge(
    reduced_mean,
    reduced_n,
    by=by_cols
  )
  
  
  reduced_mean = cbind(reduced_mean, drop_levels_dt)
  
  setnames(reduced_mean, old=metric, new=paste0(metric, "_excluded"))
  
  return(reduced_mean)
}




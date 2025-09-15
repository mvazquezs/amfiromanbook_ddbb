
#' @include m067cyto_ddbb.10_03_stat_summary.R
NULL
#' Performs basic conventional crosstabs descriptives tables.
#' @inheritParams broom::tidy
#' @description Perform basic descriptives parameters as evaluate frequencies, rows and columns
#' percentatges. Also it performs 'sum()' of the last features.
#' @param phen data frame to evaluate diferent variables.
#' @param vars_x variables to evaluate frequencies and rows and columns percentatges. Also, it add
#' margins to the crosstab features.
#' @param vars_y variables to nest and obtain the crosstab features of the dataset.
#' @param multi_tab logical. FALSE (by default) to avoid nest group in the crosstab.
#' @param sum_out logical. TRUE (by default) to avoid showning the 'sum()' fetaures.
#' @seealso \code{\link[broom]{tidy}} to catch the tables as 'data.frame()'.
#' @return a list of dataset by variables tested.

#' @rdname m067cyto_ddbb.stat_summary.crostab
#' @export
m067cyto_ddbb.stat_summary.crosstab <- function(
  phen,
  vars_x,
  vars_y,
  multi_tab = FALSE,
  sum_out = TRUE,
  stat = c('broom'))
{
### the 'match.arg()'
	stat <- match.arg(stat, c('broom'))

## Warning messages:
## 2: 'tidy.table' is deprecated
## Warning messages:
## 2: 'tidy.table' is deprecated
## Warning messages:
## 2: 'tidy.table' is deprecated
  options(warn = -1)

### list of formulas
  l_formulas <- list()
  l_vars <- c(vars_x, vars_y)


### loop for 'vars_x' & 'vars_y'
  if(multi_tab == FALSE) {
    for(j in vars_x) {
      for(i in vars_y) {
        forms <- paste0(' ~ ', j, ' + ', i)

       l_formulas[[length(l_formulas) + 1]] = forms
      }
    }
  } else {
    for(j in vars_x) {
      for(i in vars_y) {
         forms <- paste0(' ~ ', j, ' + ', paste0(unlist(vars_y, i), collapse = ' + '))
      }
      l_formulas[[length(l_formulas) + 1]] = forms
    }
  }

### create a list
  l_tab <- list()

### loop
  for(i in seq_along(l_formulas)) {
    # create list of elements
      tab_1 <- as_tibble(
        broom::tidy(addmargins(xtabs(
         formula = l_formulas[[i]], data = phen, addNA = TRUE, drop.unused.levels = TRUE)))) %>%
        select(everything(), freq = n)# frequencies info

     tab_2 <- as_tibble(
       broom::tidy(prop.table(addmargins(xtabs(
        formula = l_formulas[[i]], data = phen, addNA = TRUE, drop.unused.levels = TRUE)), margin = 1))) %>%
       select(row_perc = n) # row percentatges

     tab_3 <- as_tibble(
       broom::tidy(prop.table(addmargins(xtabs(
        formula = l_formulas[[i]], data = phen, addNA = TRUE, drop.unused.levels = TRUE)), margin = 2))) %>%
       select(col_perc = n) # col percentatges

    tab <- bind_cols(tab_1, tab_2, tab_3)

    # save a list
      l_tab[[length(l_tab) + 1]] = tab
  }


### names()
  names(l_tab) <- vars_x


### argument 'sum'
  if(sum_out == TRUE) {
    for(i in seq_along(vars_x)){
      # filter 'l_dat'
      l_tab[[i]] <- l_tab[[i]] %>%
        filter_at(vars(vars_x[[i]], vars_y), all_vars(. != 'Sum'))
      }
  }


### return
  return(l_tab)
}


### Adding categorised 'pval_labels' to the 'raw_pval' and the 'adj_pval'

#' Adjust 'p_values' for multiple comparisons test.
#' @description A 'pipe_friendly' code to add an adjusted 'p_values' columns into datasets.
#' @inheritParams stats::p.adjust
#' @param phen dataset containing a 'raw_pval' column.
#' @param method method for adjusting 'raw_pval'.
#' Include 'holm', 'hochberg', 'hommel', 'bonferroni', 'BH', 'BY',  'fdr', 'none'.
#' For non_adjusting method (not recommended), use method = 'none'.
#' @param trend logical FALSE (by default). The TRUE option uses 'trend' approach.
#' @seealso \code{\link[stats]{p.adjust}} for the multiple comparisons test.
#' @return a dataset with several recodification of the raw and adjust 'p_values'.
#'
#' @rdname amphi_func.p_adjust
#' @export
amphi_func.p_adjust <- function(
  phen,
  method = c('holm', 'hochberg', 'hommel', 'bonferroni', 'BH', 'BY',  'fdr', 'none'),
  trend = FALSE,
  stat = c('Hmisc'))
{
## nc == no converge
## np == no perform test
## ns == no signifincant

### argument 'stat'
  stat <- match.arg(stat, c('Hmisc'))

### update dataset with 'raw_pval' and 'adjust_pval'
  if('raw_pval' %in% names(phen)) {
    phen <- phen %>%
      mutate(
        raw_pval_text = factor(
          ifelse(is.nan(raw_pval), 'nc',
          ifelse(is.na(raw_pval), 'np',
          ifelse(raw_pval <= 0.0001, 'P < 0.0001',
          ifelse(raw_pval <= 0.001, 'P < 0.001',
          ifelse(raw_pval <= 0.05, paste0('P = ', round(raw_pval, 3)), 'ns')))))),
        raw_pval_signif = factor(
          ifelse(is.nan(raw_pval), 'nc',
          ifelse(is.na(raw_pval), 'np',
          ifelse(raw_pval <= 0.0001, '***',
          ifelse(raw_pval <= 0.001, '**',
          ifelse(raw_pval <= 0.01, '*',
          ifelse(raw_pval <= 0.05, '.', 'ns'))))))),
        adj_pval = as.numeric(
          ifelse(is.na(raw_pval), 'NA', p.adjust(raw_pval, method = method))),
        adj_pval_text = factor(
          ifelse(is.nan(adj_pval), 'nc',
          ifelse(is.na(adj_pval), 'np',
          ifelse(adj_pval <= 0.0001, 'P < 0.0001',
          ifelse(adj_pval <= 0.001, 'P < 0.001',
          ifelse(adj_pval <= 0.05, paste0('P = ', round(adj_pval, 3)), 'ns')))))),
        adj_pval_signif = factor(
          ifelse(is.nan(adj_pval), 'nc',
          ifelse(is.na(adj_pval), 'np',
          ifelse(adj_pval <= 0.0001, '***',
          ifelse(adj_pval <= 0.001, '**',
          ifelse(adj_pval <= 0.01, '*',
          ifelse(adj_pval <= 0.05, '.', 'ns'))))))))

        ### argument 'trend'
          if (trend == TRUE) {
            phen <- phen %>%
             mutate(
              raw_pval_text = factor(
                ifelse(is.nan(raw_pval), 'nc',
                ifelse(is.na(raw_pval), 'np',
                ifelse(raw_pval <= 0.0001, 'P < 0.0001',
                ifelse(raw_pval <= 0.001, 'P < 0.001',
                ifelse(raw_pval <= 0.05, paste0('P = ', round(raw_pval, 3)), 'ns')))))),
              raw_pval_signif = factor(
                ifelse(is.nan(raw_pval), 'nc',
                ifelse(is.na(raw_pval), 'np',
                ifelse(raw_pval <= 0.0001, '***',
                ifelse(raw_pval <= 0.001, '**',
                ifelse(raw_pval <= 0.01, '*',
                ifelse(raw_pval <= 0.05, '.', 'ns'))))))),
              adj_pval = as.numeric(
                ifelse(is.na(raw_pval), 'NA', p.adjust(raw_pval, method = method))),
              adj_pval_text = factor(
                ifelse(is.nan(adj_pval), 'nc',
                ifelse(is.na(adj_pval), 'np',
                ifelse(adj_pval <= 0.01, 'P < 0.01',
                ifelse(adj_pval <= 0.05, 'P < 0.05',
                ifelse(adj_pval <= 0.1, paste0('P = ', round(adj_pval, 3)), 'ns')))))),
              adj_pval_signif = factor(
                ifelse(is.nan(adj_pval), 'nc',
                ifelse(is.na(adj_pval), 'np',
                ifelse(adj_pval <= 0.01, '***',
                ifelse(adj_pval <= 0.05, '**',
                ifelse(adj_pval <= 0.1, '*', 'ns')))))))
        }

    } else {
     phen <- phen %>%
      mutate(
        adj_pval_text = factor(
          ifelse(is.nan(adj_pval), 'nc',
          ifelse(is.na(adj_pval), 'np',
          ifelse(adj_pval <= 0.0001, 'P < 0.0001',
          ifelse(adj_pval <= 0.001, 'P < 0.001',
          ifelse(adj_pval <= 0.05, paste0('P = ', round(adj_pval, 3)), 'ns')))))),
        adj_pval_signif = factor(
          ifelse(is.nan(adj_pval), 'nc',
          ifelse(is.na(adj_pval), 'np',
          ifelse(adj_pval <= 0.0001, '***',
          ifelse(adj_pval <= 0.001, '**',
          ifelse(adj_pval <= 0.01, '*',
          ifelse(adj_pval <= 0.05, '.', 'ns'))))))))
    }


### return
  return(phen)
}

### the 'm067cyto_ddbb.stat.statistical_bitest()'---------------------------------------------------

#' @include m067cyto_ddbb.10_03_stat_summary.R
NULL
#' Performs 'Wilcoxon' and 'T-test' tests.
#' @inheritParams rstatix::wilcox_test
#' @inheritParams rstatix::t_test
#' @inheritParams janitor::clean_names
#' @description Provides a pipe_friendly dataset with info of the 'Wilcoxon' and 'T-test' tests (see 'stats').
#' @param df dataset to evaluate diferent variables.
#' @param gr_nest group of variables where the tests will be performed.
#' @param vars_x variables 'x' to create the formula.
#' @param vars_y variables 'y' to create the formula.
#' @param stat_tests only two test have been implemented: 'Wilcoxon' and 'T-test' tests.
#' @param trend logical. TRUE (by default) uses 'trend' approach.
#' @param adjust_method method for adjusting 'raw_pval'.
#' Include 'holm', 'hochberg', 'hommel', 'bonferroni', 'BH', 'BY',  'fdr', 'none'.
#' For non_adjusting method (not recommended), use method = 'none'.
#' @param adjust_gr group of variables to nest for applying the 'multiple comparison test'.
#' @param gr01_gr02_select select only the group combinations to show and plot.
#' @param gmean_update logical. TRUE (by default) performs calculation of the geometric mean and
#' confidence interval.
#' @param median_update logical. TRUE (by default) evaluates quantiles (include IQR) and median.
#' @param coord_update logical. TRUE (by default) performs coordenates to plot correctly the 'p_values'.
#' @param k_cutoff restriction of the outlier removing range. Generally 3, based of the IQR method.
#' By default NULL.
#' @param upseg_dbl performs the increase of the segments of the 'p_values'. Recomendation '0.02'.
#' By default NULL.
#' @param upseg_dbl performs the increase of the text of the 'p_values'. Recomendation '0.1'.
#' By default NULL.
#' @param down_dbl performs the decrease of the segments of the 'counters'. Recomendation '0.05'.
#' By default NULL.
#' @param step_dbl. performs the distance between the segments of the 'p_values'.
#' Recomendation '20' in standard boxplots. Recomendation '0.6' in 'var_x' multilevel boxplots.
#' By default NULL.
#' @seealso \code{\link[rstatix]{wilcox.test}} for the kurtosis calculation.
#' @seealso \code{\link[rstatix]{t.test}} for the skewness calculation.
#' @seealso \code{\link[stats]{p.adjust}} for the multiple comparisons test.
#' @seealso \code{\link[janitor]{clean_names}} to recode the variable names of the datasets.
#' @return a dataset with the test performed for all the list of variables.

#' @rdname m067cyto_ddbb.stat.statistical_bitest
#' @export
m067cyto_ddbb.stat.statistical_bitest <- function(
  df,
  gr_nest = c('analyte_lb', 't_stimul'),
  vars_x = c('vaccine_t', 'vaccine_point_lb', 'cc21_point_vaccine_lb'),
  vars_y = c('log10dil_conc_imp', 'log10dil_ratio_trunc_val'),
  stat_tests = c('t_test', 'wilcox_test'),
  trend = TRUE,
  adjust_method = c('holm', 'hochberg', 'hommel', 'bonferroni', 'BH', 'BY',  'fdr', 'none'),
  adjust_gr = c('t_stimul', 'vars_y'), ## 't_stimul' include multiple test adjust
  coord_update = TRUE,
  upseg_dbl = .02,
  up_dbl = .1,
  down_dbl = .05,
  step_dbl = 20,
  gr01_gr02_select = NULL,
  k_cutoff = NULL,
  gmean_update = TRUE,
  median_update = FALSE,
  stat = c('janitor', 'stats', 'rstatix'))
{
### m067cyto_ddbb.stat.p_adjust() by default choose 'trend = TRUE'

### argument for the packages loaded
  stat <- match.arg(stat, c('janitor', 'stats', 'rstatix'))


### list of 'l_formula' to perform
### list of 'l_gr_nest' to perform
### list of 'l_gr_adjust' to perform
  l_formula <- list()
  l_gr_nest <- list(gr_nest)
  l_gr_adjust <- list(adjust_gr)


### loop#01 'l_formula'
  for(i in seq_along(vars_y)) {
    for(j in seq_along(vars_x)) {

    ### build 'formula'
      l_form <- paste(vars_y[i], '~', vars_x[j], collapse = '')


    ### save
      l_formula[[length(l_formula) + 1]] = l_form
    }
  }


### list of data to perform
  l_df <- list()


### loop ##02 calculate 'nest()' dimension
### calculation of statistical test proposed
  for(i in seq_along(l_formula)) {

    ### 'group_nest()' and 'slice()' the 'data'
      df_1 <- df %>%
        group_by_at(l_gr_nest[[i]]) %>%
        nest() %>%
        slice(rep(1:n(), each = length(c(l_formula[[i]])))) %>%
        mutate(formula = unlist(rep_len(l_formula[[i]], length.out = length(l_formula[[i]])))) %>%
        separate(formula, c('vars_y', 'vars_x'), sep = ' ~ ', remove = FALSE)


    ### argument 'stat_tests' for calculate the 'p_values'
      if(stat_tests == 't_test') {

          df_1 <- df_1 %>%
            mutate(
              data_test = map2(data, formula,
                ~ try(rstatix::t_test(
                   formula = as.formula(.y),
                   data = .x,
                   p.adjust.method = 'none',
                   paired = FALSE,
                   conf.level = .95),
                   silent = TRUE)))

      } else if(stat_tests == 'wilcox_test') {

          df_1 <- df_1 %>%
            mutate(
              data_test = map2(data, formula,
                ~ try(rstatix::wilcox_test(
                   formula = as.formula(.y),
                   data = .x,
                   p.adjust.method = 'none',
                   paired = FALSE,
                   conf.level = .95),
                   silent = TRUE)))

       }


       ### select columns to include into 'df_1'
         df_1 <- df_1 %>%
           select(-data) %>%
           unnest(cols = c('data_test')) %>%
           ungroup() %>%
           select(everything(), -contains('.'), raw_pval = p) %>%
           janitor::clean_names(case = 'old_janitor') %>%
           mutate_at(
             vars(c('formula', starts_with('vars_'), starts_with('group'))),
             ~ factor(.)) %>%
           mutate(
             gr01_gr02 = factor(as.character(paste0(group1, ' ~ ', group2))))


        ### update with raw_pval()' and 'pval_adjust()' data
        ### argument 'adjust_gr'
          if(!missing(adjust_gr) & !missing(adjust_method)) {

            df_1 <- df_1 %>%
              group_by_at(l_gr_adjust[[i]]) %>%
              arrange_at(l_gr_adjust[[i]]) %>%
              nest() %>%
              mutate(
                adjust_data = map(data,
                ~ m067cyto_ddbb.stat.p_adjust(
                    phen = .x,
                    method = adjust_method,
                    trend = trend))) %>%
              mutate(
                stat_method = factor(paste0(stat_tests, ' ~ ', adjust_method))) %>%
              select(-data) %>%
              unnest(cols = c('adjust_data', 'stat_method')) %>%
              ungroup() %>%
              arrange(adj_pval) %>%
              group_by_at('vars_y') %>%
              group_map(~ as_tibble(.))

          } else {

            df_1 <- df_1 %>%
               m067cyto_ddbb.stat.p_adjust(
                 method = adjust_method,
                 trend = trend) %>%
              ungroup() %>%
              group_by_at('vars_y') %>%
              group_map(~ as_tibble(.))

          }


    ### save
      l_df[[length(l_df) + 1]] = df_1
  }


### update 'list' of 'l_df'
  l_df <- unlist(l_df, recursive = FALSE)


### argument 'gr01_gr02_index' out
  if(!is.null(gr01_gr02_select)) {

    ### loop#06
      for(i in seq_along(l_df)) {

        l_df[[i]] <- l_df[[i]] %>%
          filter(gr01_gr02 %in% gr01_gr02_select) %>%
          droplevels() %>%
          mutate(
            gr01_gr02_index = as.numeric(as.factor(gr01_gr02)))

        }

    } else {

    ### loop#06
      for(i in seq_along(l_df)) {

        l_df[[i]] <- l_df[[i]] %>%
          mutate(
            gr01_gr02_index = as.numeric(as.factor(gr01_gr02)))

    }
  }

### update argument 'gmean_update'
  if(gmean_update == TRUE & !missing(down_dbl)) {

    ### 'list()'
      l_gmean <- list()


      ### custom functions
        count_n <- function(x) sum(!is.na(x))


      ### loop ##03 calculates geometric mean through 'm067cyto_ddbb.stat.geomean_ci()'
        for(i in seq_along(vars_y)) {

        l_gm <- df %>%
          select(any_of(gr_nest), any_of(vars_y), any_of(vars_x)) %>%
          group_by_at(c(gr_nest, vars_x)) %>%
          nest() %>%
          mutate(
            gm_data = map(data,
            ~ m067cyto_ddbb.stat.geomean_ci(
                x = .x[, vars_y[i]],
                alpha = .05,
                gaussian_distribution = FALSE,
                na.rm = TRUE))) %>%
          mutate(
            n = map_dbl(data, ~ count_n(.x[, vars_y[i]])),
            down = map_dbl(data, ~ c(min(.x[, vars_y[i]], na.rm = TRUE) - down_dbl * min(.x[, vars_y[i]], na.rm = TRUE))),
            gm_lower = map_dbl(gm_data, ~ .x[['ci_lower']]),
            gm_mean = map_dbl(gm_data, ~ .x[['gm_mean']]),
            gm_upper = map_dbl(gm_data, ~.x[['ci_upper']])) %>%
            select(-data, -gm_data) %>%
            unnest(cols = c(n, down, gm_mean, gm_lower, gm_upper)) %>%
            ungroup()


        ### save
          l_gmean[[length(l_gmean) + 1]] = l_gm

        }
      }


### update argument 'gmean_update'
  if(median_update == TRUE) {

    ### 'list()'
      l_median <- list()


    ### custom functions
      count_n <- function(x) sum(!is.na(x))


    ### loop#04 calculates median and 'IQRs'
      for(i in seq_along(vars_y)) {


      l_med <- df %>%
        select(any_of(gr_nest), any_of(vars_y), any_of(vars_x)) %>%
        group_by_at(c(gr_nest, vars_x)) %>%
        nest() %>%
        mutate(
          q1 = map_dbl(data, ~ quantile(.x[[vars_y[[i]]]], probs = .25, na.rm = TRUE)),
          median = map_dbl(data, ~ median(.x[[vars_y[[i]]]], na.rm = TRUE)),
          q3 = map_dbl(data, ~ quantile(.x[[vars_y[[i]]]], probs = .75, na.rm = TRUE)),
          iqr = map_dbl(data, ~ stats::IQR(.x[[vars_y[[i]]]], na.rm = TRUE)),
          n = map_dbl(data, ~ count_n(.x[, vars_y[i]]))) %>%
        select(-data) %>%
        unnest(cols = c(n, q1, median, q3, iqr)) %>%
        ungroup()

        ### save
          l_median[[length(l_median) + 1]] = l_med

        }
    }


### update for 'l_df' and 'l_coord'
  if(coord_update == TRUE) {

  ### 'list()'
    l_coord <- list()


  ### loop ##03 calculates geometric mean through 'm067cyto_ddbb.stat.geomean_ci()'
    for(i in seq_along(vars_y)) {


    ### argument for 'outliers remove' through 'm067cyto_ddbb.stat_summary.is_outlier()'
      if(!is.null(k_cutoff)) {

        df <- m067cyto_ddbb.stat_summary.is_outlier(
          df = df,
          gr_by = gr_nest,
          var_y = vars_y[i],
          method = 'k_iqr',
          qt_cutoff = c(.25, .75),
          k_cutoff = k_cutoff,
          rm_outliers = TRUE)

      } else {

        df <- df

      }

    ### update the dataset of 'p_values'
      l_cartesian <- df %>%
        select(any_of(gr_nest), any_of(vars_y), any_of(vars_x)) %>%
        group_by_at(gr_nest) %>%
        nest() %>%
        mutate(
          y_max = map_dbl(data, ~ max(.x[, vars_y[i]], na.rm = TRUE)),
          up_seg = y_max + upseg_dbl * y_max,
          up = up_seg + up_dbl,
          down = map_dbl(data, ~ c(min(.x[, vars_y[i]], na.rm = TRUE) - down_dbl * min(.x[, vars_y[i]], na.rm = TRUE))),
          step_incr = y_max / step_dbl) %>%
        select(-data) %>%
        unnest(cols = c(y_max, up_seg, up, down, step_incr)) %>%
        ungroup()


      ### loop##04 to 'join' the list of 'l_df' and 'l_coord'
        for(i in seq_along(l_df)) {

          l_full <- left_join(
              l_df[[i]],
              l_cartesian,
            by = intersect(names(l_df[[i]]), names(l_cartesian))) %>%
            mutate(
              y_coord = y_max + (gr01_gr02_index * step_incr))

          }

          ### save
            l_coord[[length(l_coord) + 1]] = l_full
          }
      }

### update 'l_df'
  if(coord_update == TRUE) {

    ### update
      l_df <- l_coord

  }


### argumentary to shown correct information
  if (gmean_update == FALSE & median_update == FALSE) {

    ### update 'list()'
      names(l_df) <- c(l_formula)

  } else if (gmean_update == TRUE & median_update == FALSE) {

    ### update 'list()'
      l_df <- c(l_df, l_gmean)

      names(l_df) <- c(l_formula, vars_y)

  } else if (gmean_update == FALSE & median_update == TRUE) {

    ### update 'list()'
      l_df <- c(l_df_coord, l_median)

      names(l_df) <- c(l_formula, vars_y)

  } else if (gmean_update == TRUE & median_update == TRUE) {

    ### update 'list()'
      l_df <- c(l_df, l_gmean, l_median)

      names(l_df) <- unlist(c(
        l_formula,
        rep(vars_y, each = length(vars_y), times = 2 * length(vars_y))))

  }


### return
  return(l_df)
}




### the 'm067cyto_ddbb.stat.statistical_multitest()'------------------------------------------------

#' @include m067cyto_ddbb.10_03_stat_summary.R
NULL
#' Performs 'Wilcoxon' and 'T-test' tests.
#' @inheritParams rstatix::welch_anova_test
#' @inheritParams rstatix::games_howell_test
#' @inheritParams rstatix::kruskal_test
#' @inheritParams rstatix::dunn_test
#' @inheritParams janitor::clean_names
#' @description Provides a pipe_friendly dataset with info of the 'Welch ANOVA', 'Games Howell', 'Kruskal-Wallis'
#' and 'Dunn' tests.
#' @param df dataset to evaluate diferent variables.
#' @param gr_nest group of variables where the tests will be performed.
#' @param vars_x variables 'x' to create the formula.
#' @param vars_y variables 'y' to create the formula.
#' @param stat_tests only four test have been implemented: 'Welch ANOVA', 'Games Howell', 'Kruskal-Wallis'
#' and 'Dunn' tests
#' @param trend logical. TRUE (by default) uses 'trend' approach.
#' @param adjust_method method for adjusting 'raw_pval'.
#' Include 'holm', 'hochberg', 'hommel', 'bonferroni', 'BH', 'BY',  'fdr', 'none'.
#' For non_adjusting method (not recommended), use method = 'none'.
#' @param adjust_gr group of variables to nest for applying the 'multiple comparison test'.
#' @param coord_update logical. TRUE (by default) performs coordenates to plot correctly the 'p_values'.
#' @param upseg_dbl performs the increase of the segments of the 'p_values'. Recomendation '0.02'.
#' By default NULL.
#' @param upseg_dbl performs the increase of the text of the 'p_values'. Recomendation '0.1'.
#' By default NULL.
#' @param down_dbl performs the decrease of the segments of the 'counters'. Recomendation '0.05'.
#' By default NULL.
#' @param step_dbl. performs the distance between the segments of the 'p_values'.
#' Recomendation '20' in standard boxplots. Recomendation '0.6' in 'var_x' multilevel boxplots.
#' By default NULL.
#' @param heatmap_update logical. FALSE (by default) performs a dataset with group comparisons in the same row.
#' @seealso \code{\link[rstatix]{welch_anova_test}} for the gaussian distribution multilevel variable.
#' @seealso \code{\link[rstatix]{games_howell_test}} for the gaussian distribution multilevel variable.
#' @seealso \code{\link[rstatix]{kruskal_test}} for the non-gaussian distribution multilevel variable.
#' @seealso \code{\link[rstatix]{dunn_test}} for the non-gaussian distribution multilevel variable.
#' @seealso \code{\link[stats]{p.adjust}} for the multiple comparisons test.
#' @seealso \code{\link[janitor]{clean_names}} to recode the variable names of the datasets.
#' @return a dataset with the test performed for all the list of variables.

#' @rdname m067cyto_ddbb.stat.statistical_multitest
# '@export
m067cyto_ddbb.stat.statistical_multitest <- function(
  df,
  gr_nest = c('analyte_lb', 't_stimul'),
  vars_x = c('vaccine_t', 'vaccine_point_lb', 'cc21_point_vaccine_lb'),
  vars_y = c('log10dil_conc_imp', 'log10dil_ratio_trunc_val'),
  stat_tests = c('welch_oneway', 'kruskal_wallis', 'games_howell', 'dunn'),
  trend = TRUE,
  adjust_method = c('holm', 'hochberg', 'hommel', 'bonferroni', 'BH', 'BY',  'fdr', 'none'),
  adjust_gr = c('t_stimul', 'vars_y'), ## 't_stimul' include multiple test adjust
  coord_update = TRUE,
  upseg_dbl = NULL, ## .02
  up_dbl = NULL, ## .1
  down_dbl = NULL, ## .05
  step_dbl = NULL, ## 20
  vars_stats, ## 'l_vars_shapiro_00'
  heatmap_update = FALSE,
  stat = c('janitor', 'stats', 'rstatix'))
{
### by default we want it NULL because it's only needed for Dunn & Games Howell tests
### chmi.stat.p_adjust() by default choose 'trend = TRUE'
### the 'heatmap_update' is only performed for one 'vars_y'

### argument for the packages loaded
  stat <- match.arg(stat, c('janitor', 'stats', 'rstatix'))


### list of 'l_formula' to perform
### list of 'l_gr_nest' to perform
### list of 'l_gr_adjust' to perform
  l_formula <- list()
  l_gr_nest <- list(gr_nest)
  l_gr_adjust <- list(adjust_gr)


### loop#01 formula
    for(i in seq_along(vars_y)) {
      for(j in seq_along(vars_x)) {
      # build 'formula'
        form_1 <- paste(vars_y[i], '~', vars_x[j], collapse = '')

      # list
        l_formula[[length(l_formula) + 1]] = form_1
      }
    }


### list of data to perform
  l_df <- list()


### loop ##02 calculate 'nest()' dimension
### calculation of statistical test proposed
  for(i in seq_along(l_formula)) {

    ### 'group_nest()' and 'slice()' the 'data'
      df_1 <- df %>%
        group_by_at(l_gr_nest[[i]]) %>%
        nest() %>%
        slice(rep(1:n(), each = length(c(l_formula[[i]])))) %>%
        mutate(formula = unlist(rep_len(l_formula[[i]], length.out = length(l_formula[[i]])))) %>%
        separate(formula, c('vars_y', 'vars_x'), sep = ' ~ ', remove = FALSE)


      ### argument 'stat_tests' for calculate the 'p_values'
        if(stat_tests == 'welch_oneway') {

          df_1 <- df_1 %>%
            mutate(data_test = map2(data, formula,
            ~ try(rstatix::welch_anova_test(
                formula = as.formula(.y),
                data = .x),
                silent = TRUE)))

      } else if(stat_tests == 'games_howell') {

          df_1 <- df_1 %>%
            mutate(data_test = map2(data, formula,
            ~ try(rstatix::games_howell_test(
              formula = as.formula(.y),
              data = .x,
              conf.level = .95,
              detailed = TRUE),
              silent = TRUE)))

      } else if(stat_tests == 'kruskal_wallis') {

          df_1 <- df_1 %>%
            mutate(data_test = map2(data, formula,
            ~ try(rstatix::kruskal_test(
              formula =  as.formula(.y),
              data = .x),
              silent = TRUE)))

      } else if(stat_tests == 'dunn') {

          df_1 <- df_1 %>%
            mutate(data_test = map2(data, formula,
            ~ try(rstatix::dunn_test(
              formula =  as.formula(.y),
              data = .x,
              p.adjust.method = 'none',
              detailed = TRUE),
              silent = TRUE)))

      }


  ### select columns to include into 'df_pval'
    df_1 <- df_1 %>%
      select(-data) %>%
      unnest(cols = c(data_test)) %>%
      select(everything(), -contains('.y')) %>%
      janitor::clean_names(case = 'old_janitor') %>%
      mutate_at(vars(c('formula', starts_with('vars_'), starts_with('group'))), ~ factor(.))


  ### list of names to preserve
    base_01 <- c(names(df_1), stat_method = 'method', 'n', 'statistic')
    welch <- c(base_01, 'dfn', 'dfd', raw_pval = 'p')
    kruskal <- c(base_01, 'df', raw_pval = 'p')

    base_02 <- c(names(df_1), stat_method = 'method', 'n1', 'n2')
    games_howell <- c(
      base_02, 'group1', 'group2', 'estimate', 'se', ci_lower = 'conf_low', ci_upper = 'conf_high',
      'statistic', 'df')
    dunn <- c(
      base_02, 'group1', 'group2', 'estimate', 'estimate1', 'estimate2', 'statistic', raw_pval = 'p')


  ### 'select()' data
    # update 'data'
      if(stat_tests == 'welch_oneway') {

        df_1 <- df_1 %>%
    			select(!! welch)

      } else if(stat_tests == 'games_howell') {

        df_1 <- df_1 %>%
    			select(!! games_howell) %>%
          select(-c(p_adj, p_adj_signif)) %>%
          mutate(
            gr01_gr02 = factor(as.character(paste0(group1, ' ~ ', group2))),
            gr01_gr02_index = as.numeric(as.factor(gr01_gr02)))

      } else if(stat_tests == 'kruskal_wallis') {

        df_1 <- df_1 %>%
  	  		select(!! kruskal)

      } else if(stat_tests == 'dunn') {

        df_1 <- df_1 %>%
  	  		select(!! dunn) %>%
  				select(-c(p_adj, p_adj_signif)) %>%
          mutate(
            gr01_gr02 = factor(as.character(paste0(group1, ' ~ ', group2))),
            gr01_gr02_index = as.numeric(as.factor(gr01_gr02)))

      }

  ### update with raw_pval()' and 'pval_adjust()' data
  ### argument 'adjust_gr'
    if(!missing(adjust_gr) & !missing(adjust_method)) {

      df_1 <- df_1 %>%
        mutate(
          stat_method = factor(paste0(stat_tests, ' ~ ', adjust_method))) %>%
        group_by_at(l_gr_adjust[[i]]) %>%
        arrange_at(l_gr_adjust[[i]]) %>%
        nest() %>%
        mutate(
          adjust_data = map(data,
          ~ m067cyto_ddbb.stat.p_adjust(
              phen = .x,
              method = adjust_method,
              trend = trend))) %>%
        select(-data) %>%
        unnest(cols = c('adjust_data')) %>%
        ungroup() %>%
        arrange(adj_pval) %>%
        group_by_at('vars_y') %>%
        group_map(~ as_tibble(.))

      } else {

        df_1 <- df_1 %>%
          mutate(
            stat_method = factor(paste0(stat_tests, ' ~ ', adjust_method))) %>%
          m067cyto_ddbb.stat.p_adjust(
            method = adjust_method,
            trend = trend) %>%
            ungroup() %>%
            group_by_at('vars_y') %>%
            group_map(~ as_tibble(.))

      }

      ### save
        l_df[[length(l_df) + 1]] = df_1
    }


### update 'list' of 'l_df'
  l_df <- unlist(l_df, recursive = FALSE)


### 'list()'
  l_coord <- list()


### update for 'l_df' and 'l_coord'
  if(coord_update == TRUE) {

    ### loop ##02 calculates geometric mean through 'm067cyto_ddbb.stat.geomean_ci()'
      for(i in seq_along(vars_y)) {

        l_cartesian <- df %>%
          select(any_of(gr_nest), any_of(vars_y), any_of(vars_x)) %>%
          group_by_at(gr_nest) %>%
          nest() %>%
          mutate(
            y_max = map_dbl(data, ~ max(.x[, vars_y[i]], na.rm = TRUE)),
            up_seg = map_dbl(data, ~ c(max(.x[, vars_y[i]], na.rm = TRUE) + upseg_dbl * max(.x[, vars_y[i]], na.rm = TRUE))),
            up = up_seg + up_dbl,
            down = map_dbl(data, ~ c(min(.x[, vars_y[i]], na.rm = TRUE) - down_dbl * min(.x[, vars_y[i]], na.rm = TRUE))),
            step_incr = y_max / step_dbl) %>%
          select(-data) %>%
          unnest(cols = c(y_max, up_seg, up, down, step_incr)) %>%
          ungroup()


          if (stat_tests %in% c('kruskal_wallis', 'welch_oneway')) {

            ### loop##03 to 'join' the list of 'l_df' and 'l_coord'
              for(i in seq_along(l_df)) {

                l_full <- left_join(
                  l_df[[i]],
                  l_cartesian,
                    by = intersect(names(l_df[[i]]), names(l_cartesian)))

              }

          } else {

            ### loop ##04 calculates geometric mean through 'm067cyto_ddbb.stat.geomean_ci()'
              for(i in seq_along(l_df)) {

                l_full <- left_join(
                  l_df[[i]],
                  l_cartesian,
                    by = intersect(names(l_df[[i]]), names(l_cartesian))) %>%
                  mutate(
                    y_coord = y_max + (gr01_gr02_index * step_incr))

               }
            }

      ### save
        l_coord[[length(l_coord) + 1]] = l_full
      }
    }


### update 'l_df'
  if(coord_update == TRUE) {

  ### update
    l_df <- l_coord

  }


### argument 'heatmap_update'
  if(heatmap_update == TRUE & stat_tests == 'dunn' & !missing(vars_stats)) {

  ### calculate robust statistics ('median()')
    df_robust <- m067cyto_ddbb.stat_summary.descriptive_tab(
      phen = df,
      vars_x = vars_y,
      vars_y = c(gr_nest, vars_x),
      shapiro_test = FALSE)


    ### loop#05 update 'df_robust'
      for(i in seq_along(vars_y)) {

        df_robust[[i]] <- df_robust[[i]] %>%
          mutate(
            vars_y = factor(paste0(vars_y[[i]])))

        }


    ### 'bind_rows()' and 'select()' variables
      df_robust <- bind_rows(df_robust)
      df_robust <- df_robust[, names(df_robust) %in% c('vars_y', gr_nest, vars_x, vars_stats)]


    ### 'rename_at' the 'vars_x'
      df_robust_gr_01 <- df_robust %>% rename_at(vars(any_of(vars_x)), ~ 'group1')
      df_robust_gr_02 <- df_robust %>% rename_at(vars(any_of(vars_x)), ~ 'group2')


    ### list()
      l_df_robust <- list()

      ### loop#06
        for(i in seq_along(l_df)) {

        ### vector to 'rename()' variables
          base_03 <- names(l_df[[i]])

          med_names_gr_01 <- c(
            base_03, gr01_q1 = 'q_1', gr01_median = 'median', gr01_q3 = 'q_3', gr01_iqr = 'iqr')

          med_names_gr_02 <- c(
            base_03, gr02_q1 = 'q_1', gr02_median = 'median', gr02_q3 = 'q_3', gr02_iqr = 'iqr')


          ### 1st 'left_join()' approach
            df_gr_01 <- left_join(
                l_df[[i]],
                df_robust_gr_01,
                  by = intersect(names(l_df[[i]]), names(df_robust_gr_01))) %>%
              select(!! med_names_gr_01)


          ### 2nd 'left_join()' approach
            df_gr_02 <- left_join(
                l_df[[i]],
                df_robust_gr_02,
                  by = intersect(names(l_df[[i]]), names(df_robust_gr_02))) %>%
              select(!! med_names_gr_02)


          ### 3rd 'left_join()' approach
            df_1 <- left_join(
              df_gr_01,
              df_gr_02,
                by = intersect(names(df_gr_01), names(df_gr_02))) %>%
              mutate(
                median_diff = gr02_median - gr01_median)


          ### save
            l_df_robust[[length(l_df_robust) + 1]] = df_1
          }
        }


### update 'l_df'
  if(heatmap_update == TRUE & stat_tests == 'dunn' & !missing(vars_stats)) {

    ### update
      l_df <- l_df_robust

  }

### 'names()'
  names(l_df) <- l_formula


### return
  return(l_df)

}
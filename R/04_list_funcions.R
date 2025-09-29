#' @title Funcions d'Anàlisi del Projecte
#' @description Aquest fitxer conté totes les funcions d'anàlisi utilitzades en el projecte.

# --- Funcions de Taules de Resum ---

#' @title Crea una taula de resum creuat
#' @description Realitza taules de contingència descriptives bàsiques.
#' @param phen Dataframe a avaluar.
#' @param vars_x Variables per a les files.
#' @param vars_y Variables per a les columnes.
#' @param multi_tab Lògic. Si és FALSE, no anida grups.
#' @param sum_out Lògic. Si és TRUE, amaga els totals.
#' @return Una llista de dataframes amb les taules de contingència.
#' @export
amphi_crosstab <- function(
  phen,
  vars_x,
  vars_y,
  multi_tab = FALSE,
  sum_out = TRUE,
  stat = c('broom'))
{
	stat <- match.arg(stat, c('broom'))
  options(warn = -1)

  l_formulas <- list()
  l_vars <- c(vars_x, vars_y)

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

  l_tab <- list()

  for(i in seq_along(l_formulas)) {
      tab_1 <- as_tibble(
        broom::tidy(addmargins(xtabs(
         formula = l_formulas[[i]], data = phen, addNA = TRUE, drop.unused.levels = TRUE)))) %>%
        select(everything(), freq = n)

     tab_2 <- as_tibble(
       broom::tidy(prop.table(addmargins(xtabs(
        formula = l_formulas[[i]], data = phen, addNA = TRUE, drop.unused.levels = TRUE)), margin = 1))) %>%
       select(row_perc = n)

     tab_3 <- as_tibble(
       broom::tidy(prop.table(addmargins(xtabs(
        formula = l_formulas[[i]], data = phen, addNA = TRUE, drop.unused.levels = TRUE)), margin = 2))) %>%
       select(col_perc = n)

    tab <- bind_cols(tab_1, tab_2, tab_3)

      l_tab[[length(l_tab) + 1]] = tab
  }

  names(l_tab) <- vars_x

  if(sum_out == TRUE) {
    for(i in seq_along(vars_x)){
      l_tab[[i]] <- l_tab[[i]] %>%
        filter_at(vars(vars_x[[i]], vars_y), all_vars(. != 'Sum'))
      }
  }

  return(l_tab)
}

#' @title Calcula estadístiques descriptives per a un `data.frame`.
#' @description Calcula un conjunt complet d'estadístiques descriptives per a variables numèriques.
#' @param df El `data.frame` o `tibble` d'entrada.
#' @param seleccio_variables Les variables numèriques a analitzar.
#' @param grup_by Les variables per agrupar les dades.
#' @param stats_adicionals Vector de caràcters per especificar estadístiques addicionals.
#' @param q_lower Quantil inferior.
#' @param q_upper Quantil superior.
#' @param sd_num Nombre de desviacions estàndard.
#' @param na.rm Lògic. Si és `TRUE`, elimina els NA.
#' @param bind_rows Lògic. Si és `TRUE`, combina els resultats en un sol tibble.
#' @param digits Nombre de dígits decimals.
#' @return Un `tibble` amb les estadístiques descriptives.
#' @export
amphi_summary_table <- function(
    df,
    grup_by = NULL,
    seleccio_variables,
    stats_adicionals = FALSE,
    sd_num = NULL,
    q_lower = NULL,
    q_upper = NULL,
    na.rm = TRUE,
    bind_rows = TRUE,
    digits = 2)
{
  sel_exp_variables <- rlang::enquo(seleccio_variables)
  grup_exp_by <- rlang::enquo(grup_by)
  
  sel_vars_names <- names(
    dplyr::select(df, !!sel_exp_variables))
  
  group_cols_names <- if (!is.null(grup_by)) {
    
    names(
      dplyr::select(df, !!grup_exp_by))
    
  } else {
    
    character(0)
  }
  
  if (!is.null(grup_by)) {
    df <- df %>%
      dplyr::mutate(dplyr::across(dplyr::all_of(group_cols_names), as.factor))
  }
  
  tab_bases <- suppressWarnings(lapply(
      sel_vars_names, function(x) {
        df[, x] <- df[, x]
        
        df %>%
          group_by_at(group_cols_names) %>%
          summarise_at(vars(all_of(x)),
                       list(
                         n = ~ sum(!is.na(.)),
                         na = ~ sum(is.na(.)),
                         min = ~ min(., na.rm = na.rm),
                         q_1 = ~ quantile(., probs = 0.25, na.rm = na.rm),
                         median = ~ median(., na.rm = na.rm),
                         mean = ~ mean(., na.rm = na.rm),
                         sd = ~ sd(., na.rm = na.rm),
                         q_3 = ~ quantile(., probs = 0.75, na.rm = na.rm),
                         max = ~ max(., na.rm = na.rm),
                         iqr = ~ stats::IQR(., na.rm = na.rm))) %>%
          as_tibble() %>%
          distinct()
      }))
  
  names(tab_bases) <- sel_vars_names
  
  if (isTRUE(stats_adicionals)) {
    
    tab_adicionals <- suppressWarnings(lapply(
        sel_vars_names, function(x) {
          df[, x] <- df[, x]
          
          df %>%
            group_by_at(group_cols_names) %>%
            summarise_at(vars(all_of(x)),
                         list(
                           q_lower = ~ quantile(., probs = q_lower, na.rm = na.rm),
                           sd_lower = ~ as.double(mean(., na.rm = na.rm) - sd_num * sd(., na.rm = na.rm)), 
                           sd_upper = ~ as.double(mean(., na.rm = na.rm) + sd_num * sd(., na.rm = na.rm)),
                           q_upper = ~ quantile(., probs = q_upper, na.rm = na.rm),
                           kurt = ~ e1071::kurtosis(., na.rm = na.rm),
                           skew = ~ e1071::skewness(., na.rm = na.rm))) %>%
            as_tibble() %>%
            distinct() 
        }))
    
    if (!is.null(grup_by)) {
      
      l_tab_all <- mapply(
        dplyr::left_join,
        tab_bases,
        tab_adicionals,
        MoreArgs = list(by = group_cols_names),
        SIMPLIFY = FALSE)
    
    } else {
      
      l_tab_all <- mapply(
        dplyr::cross_join,
        tab_bases,
        tab_adicionals,
        SIMPLIFY = FALSE)
    
    }
  
  } else {

    l_tab_all <- tab_bases
  
  }
  
  if (isTRUE(bind_rows)) {
    
    df_tab_all <- dplyr::bind_rows(l_tab_all, .id = 'variable') %>%
      dplyr::mutate(variable = as.factor(variable)) %>%
      dplyr::mutate(dplyr::across(where(is.double), ~ round(., digits))) %>%
      dplyr::mutate(across(where(is.double), ~ ifelse(is.finite(.), ., NA)))
    
    return(df_tab_all)
    
  } else {

  for(i in seq_along(l_tab_all)) {

    l_tab_all[[i]] <- l_tab_all[[i]] %>%
      dplyr::mutate(dplyr::across(where(is.double), ~ round(., digits))) %>%
      dplyr::mutate(across(where(is.double), ~ ifelse(is.finite(.), ., NA)))

  }
    
    return(l_tab_all)
  }
}


# --- Funcions Estadístiques ---

#' @title Ajusta els p-values per a comparacions múltiples.
#' @description Afegeix columnes de p-values ajustats a un dataframe.
#' @param phen Dataframe amb una columna 'raw_pval'.
#' @param method Mètode d'ajust.
#' @param trend Lògic. Si és TRUE, utilitza l'aproximació de tendència.
#' @return Un dataframe amb els p-values ajustats.
#' @export
amphi_adjust_pvalue <- function(
  phen,
  method = c('holm', 'hochberg', 'hommel', 'bonferroni', 'BH', 'BY',  'fdr', 'none'),
  trend = FALSE,
  stat = c('Hmisc'))
{
  stat <- match.arg(stat, c('Hmisc'))

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

  return(phen)
}

#' @title Realitza tests estadístics bivariats.
#' @description Realitza tests de Wilcoxon i T-test.
#' @param ... Paràmetres de la funció original.
#' @return Un dataframe amb els resultats dels tests.
#' @export
amphi_stat_test <- function(
  df,
  gr_nest = c('analyte_lb', 't_stimul'),
  vars_x = c('vaccine_t', 'vaccine_point_lb', 'cc21_point_vaccine_lb'),
  vars_y = c('log10dil_conc_imp', 'log10dil_ratio_trunc_val'),
  stat_tests = c('t_test', 'wilcox_test'),
  trend = TRUE,
  adjust_method = c('holm', 'hochberg', 'hommel', 'bonferroni', 'BH', 'BY',  'fdr', 'none'),
  adjust_gr = c('t_stimul', 'vars_y'),
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
  stat <- match.arg(stat, c('janitor', 'stats', 'rstatix'))

  l_formula <- list()
  l_gr_nest <- list(gr_nest)
  l_gr_adjust <- list(adjust_gr)

  for(i in seq_along(vars_y)) {
    for(j in seq_along(vars_x)) {
      l_form <- paste(vars_y[i], '~', vars_x[j], collapse = '')
      l_formula[[length(l_formula) + 1]] = l_form
    }
  }

  l_df <- list()

  for(i in seq_along(l_formula)) {

      df_1 <- df %>%
        group_by_at(l_gr_nest[[i]]) %>%
        nest() %>%
        slice(rep(1:n(), each = length(c(l_formula[[i]])))) %>%
        mutate(formula = unlist(rep_len(l_formula[[i]], length.out = length(l_formula[[i]])))) %>%
        separate(formula, c('vars_y', 'vars_x'), sep = ' ~ ', remove = FALSE)

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

          if(!missing(adjust_gr) & !missing(adjust_method)) {

            df_1 <- df_1 %>%
              group_by_at(l_gr_adjust[[i]]) %>%
              arrange_at(l_gr_adjust[[i]]) %>%
              nest() %>%
              mutate(
                adjust_data = map(data,
                ~ amphi_adjust_pvalue(
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
               amphi_adjust_pvalue(
                 method = adjust_method,
                 trend = trend) %>%
              ungroup() %>%
              group_by_at('vars_y') %>%
              group_map(~ as_tibble(.))

          }

      l_df[[length(l_df) + 1]] = df_1
  }

  l_df <- unlist(l_df, recursive = FALSE)

  if(!is.null(gr01_gr02_select)) {

      for(i in seq_along(l_df)) {

        l_df[[i]] <- l_df[[i]] %>%
          filter(gr01_gr02 %in% gr01_gr02_select) %>%
          droplevels() %>%
          mutate(
            gr01_gr02_index = as.numeric(as.factor(gr01_gr02)))

        }

    } else {

      for(i in seq_along(l_df)) {

        l_df[[i]] <- l_df[[i]] %>%
          mutate(
            gr01_gr02_index = as.numeric(as.factor(gr01_gr02)))

    }
  }

  if(gmean_update == TRUE & !missing(down_dbl)) {

      l_gmean <- list()

        count_n <- function(x) sum(!is.na(x))

        for(i in seq_along(vars_y)) {

        l_gm <- df %>%
          select(any_of(gr_nest), any_of(vars_y), any_of(vars_x)) %>%
          group_by_at(c(gr_nest, vars_x)) %>%
          nest() %>%
          mutate(
            gm_data = map(data,
            ~ amphi_geomean_ci(
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

          l_gmean[[length(l_gmean) + 1]] = l_gm

        }
      }

  if(median_update == TRUE) {

      l_median <- list()

      count_n <- function(x) sum(!is.na(x))

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

          l_median[[length(l_median) + 1]] = l_med

        }
    }

  if(coord_update == TRUE) {

    l_coord <- list()

    for(i in seq_along(vars_y)) {

      if(!is.null(k_cutoff)) {

        df <- amphi_is_outlier(
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

        for(i in seq_along(l_df)) {

          l_full <- left_join(
              l_df[[i]],
              l_cartesian,
            by = intersect(names(l_df[[i]]), names(l_cartesian))) %>%
            mutate(
              y_coord = y_max + (gr01_gr02_index * step_incr))

          }

            l_coord[[length(l_coord) + 1]] = l_full
          }
      }

  if(coord_update == TRUE) {

      l_df <- l_coord

  }

  if (gmean_update == FALSE & median_update == FALSE) {

      names(l_df) <- c(l_formula)

  } else if (gmean_update == TRUE & median_update == FALSE) {

      l_df <- c(l_df, l_gmean)

      names(l_df) <- c(l_formula, vars_y)

  } else if (gmean_update == FALSE & median_update == TRUE) {

      l_df <- c(l_df_coord, l_median)

      names(l_df) <- c(l_formula, vars_y)

  } else if (gmean_update == TRUE & median_update == TRUE) {

      l_df <- c(l_df, l_gmean, l_median)

      names(l_df) <- unlist(c(
        l_formula,
        rep(vars_y, each = length(vars_y), times = 2 * length(vars_y))))

  }

  return(l_df)
}


#' @title Realitza tests estadístics multivariats.
#' @description Realitza tests de Welch ANOVA, Games Howell, Kruskal-Wallis i Dunn.
#' @param ... Paràmetres de la funció original.
#' @return Un dataframe amb els resultats dels tests.
#' @export
amphi_stat_multitest <- function(
  df,
  gr_nest = c('analyte_lb', 't_stimul'),
  vars_x = c('vaccine_t', 'vaccine_point_lb', 'cc21_point_vaccine_lb'),
  vars_y = c('log10dil_conc_imp', 'log10dil_ratio_trunc_val'),
  stat_tests = c('welch_oneway', 'kruskal_wallis', 'games_howell', 'dunn'),
  trend = TRUE,
  adjust_method = c('holm', 'hochberg', 'hommel', 'bonferroni', 'BH', 'BY',  'fdr', 'none'),
  adjust_gr = c('t_stimul', 'vars_y'),
  coord_update = TRUE,
  upseg_dbl = NULL,
  up_dbl = NULL,
  down_dbl = NULL,
  step_dbl = NULL,
  vars_stats,
  heatmap_update = FALSE,
  stat = c('janitor', 'stats', 'rstatix'))
{
  stat <- match.arg(stat, c('janitor', 'stats', 'rstatix'))

  l_formula <- list()
  l_gr_nest <- list(gr_nest)
  l_gr_adjust <- list(adjust_gr)

    for(i in seq_along(vars_y)) {
      for(j in seq_along(vars_x)) {
        form_1 <- paste(vars_y[i], '~', vars_x[j], collapse = '')
        l_formula[[length(l_formula) + 1]] = form_1
      }
    }

  l_df <- list()

  for(i in seq_along(l_formula)) {

      df_1 <- df %>%
        group_by_at(l_gr_nest[[i]]) %>%
        nest() %>%
        slice(rep(1:n(), each = length(c(l_formula[[i]])))) %>%
        mutate(formula = unlist(rep_len(l_formula[[i]], length.out = length(l_formula[[i]])))) %>%
        separate(formula, c('vars_y', 'vars_x'), sep = ' ~ ', remove = FALSE)

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

    df_1 <- df_1 %>%
      select(-data) %>%
      unnest(cols = c(data_test)) %>%
      select(everything(), -contains('.y')) %>%
      janitor::clean_names(case = 'old_janitor') %>%
      mutate_at(vars(c('formula', starts_with('vars_'), starts_with('group'))), ~ factor(.))

    base_01 <- c(names(df_1), stat_method = 'method', 'n', 'statistic')
    welch <- c(base_01, 'dfn', 'dfd', raw_pval = 'p')
    kruskal <- c(base_01, 'df', raw_pval = 'p')

    base_02 <- c(names(df_1), stat_method = 'method', 'n1', 'n2')
    games_howell <- c(
      base_02, 'group1', 'group2', 'estimate', 'se', ci_lower = 'conf_low', ci_upper = 'conf_high',
      'statistic', 'df')
    dunn <- c(
      base_02, 'group1', 'group2', 'estimate', 'estimate1', 'estimate2', 'statistic', raw_pval = 'p')

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

    if(!missing(adjust_gr) & !missing(adjust_method)) {

      df_1 <- df_1 %>%
        mutate(
          stat_method = factor(paste0(stat_tests, ' ~ ', adjust_method))) %>%
        group_by_at(l_gr_adjust[[i]]) %>%
        arrange_at(l_gr_adjust[[i]]) %>%
        nest() %>%
        mutate(
          adjust_data = map(data,
          ~ amphi_adjust_pvalue(
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
          amphi_adjust_pvalue(
            method = adjust_method,
            trend = trend) %>%
            ungroup() %>%
            group_by_at('vars_y') %>%
            group_map(~ as_tibble(.))

      }

        l_df[[length(l_df) + 1]] = df_1
    }

  l_df <- unlist(l_df, recursive = FALSE)

  l_coord <- list()

  if(coord_update == TRUE) {

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

              for(i in seq_along(l_df)) {

                l_full <- left_join(
                  l_df[[i]],
                  l_cartesian,
                    by = intersect(names(l_df[[i]]), names(l_cartesian)))

              }

          } else {

              for(i in seq_along(l_df)) {

                l_full <- left_join(
                  l_df[[i]],
                  l_cartesian,
                    by = intersect(names(l_df[[i]]), names(l_cartesian))) %>%
                  mutate(
                    y_coord = y_max + (gr01_gr02_index * step_incr))

               }
            }

        l_coord[[length(l_coord) + 1]] = l_full
      }
    }

  if(coord_update == TRUE) {

    l_df <- l_coord

  }

  if(heatmap_update == TRUE & stat_tests == 'dunn' & !missing(vars_stats)) {

    df_robust <- amphi_summary_table(
      phen = df,
      vars_x = vars_y,
      vars_y = c(gr_nest, vars_x),
      shapiro_test = FALSE)

      for(i in seq_along(vars_y)) {

        df_robust[[i]] <- df_robust[[i]] %>%
          mutate(
            vars_y = factor(paste0(vars_y[[i]])))

        }

      df_robust <- bind_rows(df_robust)
      df_robust <- df_robust[, names(df_robust) %in% c('vars_y', gr_nest, vars_x, vars_stats)]

      df_robust_gr_01 <- df_robust %>% rename_at(vars(any_of(vars_x)), ~ 'group1')
      df_robust_gr_02 <- df_robust %>% rename_at(vars(any_of(vars_x)), ~ 'group2')

      l_df_robust <- list()

        for(i in seq_along(l_df)) {

          base_03 <- names(l_df[[i]])

          med_names_gr_01 <- c(
            base_03, gr01_q1 = 'q_1', gr01_median = 'median', gr01_q3 = 'q_3', gr01_iqr = 'iqr')

          med_names_gr_02 <- c(
            base_03, gr02_q1 = 'q_1', gr02_median = 'median', gr02_q3 = 'q_3', gr02_iqr = 'iqr')

            df_gr_01 <- left_join(
                l_df[[i]],
                df_robust_gr_01,
                  by = intersect(names(l_df[[i]]), names(df_robust_gr_01))) %>%
              select(!! med_names_gr_01)

            df_gr_02 <- left_join(
                l_df[[i]],
                df_robust_gr_02,
                  by = intersect(names(l_df[[i]]), names(df_robust_gr_02))) %>%
              select(!! med_names_gr_02)

            df_1 <- left_join(
              df_gr_01,
              df_gr_02,
                by = intersect(names(df_gr_01), names(df_gr_02))) %>%
              mutate(
                median_diff = gr02_median - gr01_median)

            l_df_robust[[length(l_df_robust) + 1]] = df_1
          }
        }

  if(heatmap_update == TRUE & stat_tests == 'dunn' & !missing(vars_stats)) {

      l_df <- l_df_robust

  }

  names(l_df) <- l_formula

  return(l_df)

}


# --- Funcions d'Imputació ---

#' @title Imputa valors perduts en un dataframe.
#' @description Aquesta funció ofereix diferents mètodes per a la imputació de valors `NA`.
#' @param df Un `data.frame` o `tibble` d'entrada.
#' @param ... Paràmetres addicionals per als mètodes d'imputació.
#' @return Un `tibble` amb els valors imputats.
#' @export
amphi_impute <- function(
  df, 
  grup_by = NULL,
  seleccio_variables = NULL,
  metode_imputacio = c(
    'mitjana_aritmetica', 'mitjana_geometrica', 'mitjana_truncada', 'mitjana_winsoritzada', 'mediana', 'missForest', 'MICE', 'kNN'),
  metode_reserva = 'mediana',
  valor_trim = .10,
  retornar_originals = FALSE) 
{
  if (!is.data.frame(df) && !tibble::is_tibble(df)) {
    stop("Argument 'df' ha de ser un data.frame o un tibble.")
  }

  valid_methods <- c('mitjana_aritmetica', 'mitjana_geometrica', 'mitjana_truncada', 'mitjana_winsoritzada', 'mediana', 'missForest', 'MICE', 'kNN')
  if (!metode_imputacio %in% valid_methods) {
    stop(paste0("Mètode imputació no vàlid. Si us plau, escolliu un dels següents: ", paste(valid_methods, collapse = ', ')))
  }
  valid_fallback_methods <- c('mitjana_aritmetica', 'mitjana_geometrica', 'mitjana_truncada', 'mitjana_winsoritzada', 'mediana')
  if (!metode_reserva %in% valid_fallback_methods) {
    stop("El 'metode_reserva' només pot ser un dels mètodes estadístics: 'mitjana_aritmetica', 'mitjana_geometrica', 'mitjana_truncada', 'mitjana_winsoritzada', 'mediana'.")
  }

  sel_exp_variables <- rlang::enquo(seleccio_variables)
  sel_vars_names <- names(dplyr::select(df, !!sel_exp_variables))

  imputar_df_estatistic <- function(data, method) {
    data_out <- data
    
    if (method == 'mitjana_aritmetica') {
      data_out <- data_out %>%
        dplyr::mutate(dplyr::across(all_of(sel_vars_names),
          ~ tidyr::replace_na(., mean(., na.rm = TRUE))))
    } else if (method == 'mitjana_truncada' & !is.null(valor_trim)) {
      data_out <- data_out %>%
        dplyr::mutate(dplyr::across(all_of(sel_vars_names),
          ~ tidyr::replace_na(., mean(., trim = valor_trim, na.rm = TRUE))))
    } else if (method == 'mitjana_geometrica') {
      data_out <- data_out %>%
        dplyr::mutate(dplyr::across(all_of(sel_vars_names),
          ~ tidyr::replace_na(. , psych::geometric.mean(., na.rm=TRUE))))
    } else if (method == 'mitjana_winsoritzada' & !is.null(valor_trim)) {
      data_out <- data_out %>%
        dplyr::mutate(dplyr::across(all_of(sel_vars_names),
          ~ tidyr::replace_na(., psych::winsor.mean(., trim = valor_trim, na.rm = TRUE))))
    } else if (method == 'mediana') {
      data_out <- data_out %>%
        dplyr::mutate(dplyr::across(all_of(sel_vars_names),
          ~ tidyr::replace_na(., median(., na.rm = TRUE))))
    }
    return(data_out)
  }

  df_imp <- df %>%
    dplyr::mutate(
      dplyr::across(where(is.double) | where(is.integer), as.numeric),
      dplyr::across(where(is.character), as.factor))

  if (metode_imputacio == 'missForest') {
    if (!requireNamespace('missForest', quietly = TRUE)) {
      stop("El mètode 'missForest' requereix el paquet 'missForest'. Si us plau, instal·leu-lo amb install.packages('missForest').")
    }
    if (!is.null(grup_by)) {
      warning("El mètode 'missForest' no és compatible amb l'agrupació de dades. S'ignora 'grup_by'.")
    }

    tryCatch({
      set.seed(42)
      df_temp <- df_imp %>% dplyr::select(all_of(sel_vars_names)) %>% as.data.frame()
      imputed_result <- missForest::missForest(df_temp, maxiter = 10, ntree = 100)
      df_imp[sel_vars_names] <- imputed_result$ximp
      warning(paste("Error OOB (NRMSE/PFC) per a missForest:", round(imputed_result$OOBerror['NRMSE'], 4), "/", round(imputed_result$OOBerror['PFC'], 4)))

    }, error = function(e) {
      warning(paste("Imputació amb 'missForest' ha fallat:", e$message, "S'utilitzarà el mètode de reserva:", metode_reserva))
      df_imp <- imputar_df_estatistic(df_imp, metode_reserva)
    })

  } else if (metode_imputacio == 'MICE') {
    if (!requireNamespace('mice', quietly = TRUE)) {
      stop("El mètode 'MICE' requereix el paquet 'mice'. Si us plau, instal·leu-lo amb install.packages('mice').")
    }
    if (!is.null(grup_by)) {
      warning("El mètode 'MICE' no és compatible amb l'agrupació de dades. S'ignora 'grup_by'.")
    }

    tryCatch({
      set.seed(42)
      imputed_data <- mice::mice(df_imp, m = 5, maxit = 50, meth = 'pmm', printFlag = FALSE)
      df_imp <- mice::complete(imputed_data)

    }, error = function(e) {
      warning(paste("Imputació amb 'MICE' ha fallat:", e$message, "S'utilitzarà el mètode de reserva:", metode_reserva))
      df_imp <- imputar_df_estatistic(df_imp, metode_reserva)
    })

  } else if (metode_imputacio == 'kNN') {
    if (!requireNamespace('VIM', quietly = TRUE)) {
      stop("El mètode 'kNN' requereix el paquet 'VIM'. Si us plau, instal·leu-lo amb install.packages('VIM').")
    }
    if (!is.null(grup_by)) {
      warning("El mètode 'kNN' no és compatible amb l'agrupació de dades. S'ignora 'grup_by'.")
    }

    tryCatch({
      set.seed(42)
      df_imp <- VIM::kNN(df_imp, k = 5, imp_suffix = 'Imputed')

    }, error = function(e) {
      warning(paste("Imputació amb 'kNN' ha fallat:", e$message, "S'utilitzarà el mètode de reserva:", metode_reserva))
      df_imp <- imputar_df_estatistic(df_imp, metode_reserva)
    })

  } else {
    if (!is.null(grup_by)) {
      grup_exp_by <- rlang::syms(grup_by)
      df_imp <- df_imp %>%
        dplyr::group_by(!!!grup_exp_by) %>%
        dplyr::group_modify( ~ imputar_df_estatistic(data = .x, method = metode_imputacio)) %>%
        dplyr::ungroup()
    } else {
      df_imp <- imputar_df_estatistic(df_imp, metode_imputacio)
    }
  }

  if (retornar_originals == FALSE) {
    return(df_imp)
  } else {
    return(list(df_imputat = df_imp, df_original = df))
  }
}

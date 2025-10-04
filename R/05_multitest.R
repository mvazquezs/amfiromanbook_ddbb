
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
### stat_test

#' @title Realitza tests estadístics bivariats i multivariats
#' @description Aquesta funció és un embolcall (`wrapper`) potent i flexible al
#'   voltant de diverses funcions de test del paquet `rstatix`. Permet executar
#'   tests estadístics per a múltiples combinacions de variables dependents,
#'   independents i de grup, i posteriorment ajusta els p-values per a
#'   comparacions múltiples.
#'
#' @details
#' La funció està dissenyada per simplificar anàlisis complexes on s'han de
#' repetir els mateixos tests en diferents subconjunts de dades. Utilitza
#' avaluació no estàndard (NSE) per als arguments de columnes (`variables_y`,
#' `variables_x`, `grup_by`, `grup_adjust`), la qual cosa permet una sintaxi més
#' intuïtiva i alineada amb el `tidyverse` (p. ex., `variables_y = c(col1, col2)`).
#'
#' El flux de treball intern és el següent:
#' 1.  Genera totes les combinacions de fórmules a partir de `variables_y` i `variables_x`.
#' 2.  Si s'especifica `grup_by`, agrupa les dades segons aquestes columnes.
#' 3.  Executa el `stat_test` seleccionat per a cada combinació i subgrup de dades.
#' 4.  Recopila els resultats, els estandarditza i neteja els noms de les columnes.
#' 5.  Ajusta els p-values (`raw_pval`) utilitzant la funció `adjust_pvalue()` i el
#'     mètode especificat en `metode`, ja sigui globalment o per grups (`grup_adjust`).
#' 6.  Retorna una llista de `tibbles`, on cada element de la llista correspon
#'     a una de les `variables_y`.
#'
#' @param df Un `dataframe` o `tibble` que conté les dades.
#' @param variables_y Columnes que actuen com a variables dependents (numèriques).
#'   S'han de passar sense cometes. Es poden especificar múltiples columnes
#'   utilitzant `c(col1, col2)`.
#' @param variables_x Columnes que actuen com a variables independents o de grup
#'   (categòriques). S'han de passar sense cometes. Es poden especificar
#'   múltiples columnes utilitzant `c(col1, col2)`.
#' @param grup_by Opcional. Columnes per niar o agrupar les dades abans
#'   d'executar els tests. Això és útil per realitzar anàlisis separades per
#'   a cada nivell d'un factor. Passar sense cometes.
#' @param stat_test Cadena de text que especifica el test a realitzar. Ha de ser
#'   un dels valors suportats: `'t_test'`, `'wilcox_test'`,
#'   `'welch_anova_test'`, `'kruskal_test'`, `'games_howell_test'`, `'dunn_test'`.
#' @param metode Cadena de text que especifica el mètode d'ajust de
#'   p-values a passar a `stats::p.adjust`. Valors comuns són `'holm'`, `'BH'`
#'   (Benjamini-Hochberg), `'bonferroni'`, `'fdr'`.
#' @param grup_adjust Opcional. Columnes per agrupar les dades abans d'ajustar
#'   els p-values. Això permet realitzar l'ajust dins de cada subgrup en lloc
#'   de fer-ho globalment. Passar sense cometes.
#' @param trend Lògic. Argument passat a la funció `adjust_pvalue()` per a la
#'   categorització de la significació.
#' @param ... Paràmetres addicionals que es passaran directament a la funció de
#'   test de `rstatix`. Per exemple, `paired = TRUE` per a un t-test aparellat,
#'   o `detailed = TRUE` per a `games_howell_test`.
#'
#' @return Una llista de `tibbles`. Cada `tibble` de la llista està anomenat
#'   segons la variable dependent (`variables_y`) a la qual correspon i conté els
#'   resultats detallats dels tests, incloent els p-values originals (`raw_pval`)
#'   i els ajustats (`adj_pval`), juntament amb el seu format de text i
#'   significació.
#'
#' @rdname stat_tests
#' @export
#'
#' @importFrom rlang enquo quo_is_null !!
#' @importFrom dplyr select
#' @importFrom dplyr group_by
#' @importFrom dplyr across
#' @importFrom dplyr all_of
#' @importFrom dplyr cross_join
#' @importFrom dplyr mutate 
#' @importFrom dplyr filter 
#' @importFrom dplyr arrange
#' @importFrom dplyr ungroup
#' @importFrom dplyr group_split
#' @importFrom dplyr rename
#' @importFrom tidyr nest expand_grid
#' @importFrom tidyr nest unnest 
#' @importFrom purrr map2
#' @importFrom janitor clean_names
#'
#' @examples
#' # Perquè aquest exemple funcioni, la funció `adjust_pvalue` ha d'estar
#' # definida i disponible a l'entorn de treball.
#' # Aquí definim una versió simplificada per a la demostració:
#' adjust_pvalue <- function(df, method, trend = FALSE, pval_col = "raw_pval") {
#'   df %>%
#'     dplyr::mutate(
#'       adj_pval = stats::p.adjust(!!rlang::sym(pval_col), method = method),
#'       adj_pval_signif = rstatix::p_signif(adj_pval)
#'     )
#' }
#'
#' # Carreguem les dades d'exemple 'iris'
#' data(iris)
#'
#' # --- Exemple: Test de Wilcoxon simple ---
#' results_simple <- stat_tests(
#'   df = iris,
#'   variables_y = Sepal.Length,
#'   variables_x = Species,
#'   stat_test = "wilcox_test",
#'   metode = "BH"
#' )
#' print(results_simple$Sepal.Length)
#'
stat_tests <- function(
  df,
  variables_y,
  variables_x,
  grup_by = NULL,
  stat_test = c('t_test', 'wilcox_test', 'welch_anova_test', 'kruskal_test', 'games_howell_test', 'dunn_test'),
  metode = c('holm', 'hochberg', 'hommel', 'bonferroni', 'BH', 'BY', 'fdr', 'none'),
  grup_adjust = NULL,
  trend = FALSE,
  ...)
{
### Validació i captura d'arguments (NSE)
  stat_test <- match.arg(stat_test)
  metode <- match.arg(metode)

  variables_y_enquo <- rlang::enquo(variables_y)
  variables_x_enquo <- rlang::enquo(variables_x)
  grup_by_enquo <- rlang::enquo(grup_by)
  grup_adjust_enquo <- rlang::enquo(grup_adjust)

### Obtenir noms de columnes com a caràcters
  variables_y_names <- names(dplyr::select(df, !!variables_y_enquo))
  variables_x_names <- names(dplyr::select(df, !!variables_x_enquo))
  grup_by_names <- if (!rlang::quo_is_null(grup_by_enquo)) names(dplyr::select(df, !!grup_by_enquo)) else character(0)
  grup_adjust_names <- if (!rlang::quo_is_null(grup_adjust_enquo)) names(dplyr::select(df, !!grup_adjust_enquo)) else character(0)

  # --- 2. Preparació de les tasques de test ---
  tasks <- tidyr::expand_grid(variables_y = variables_y_names, variables_x = variables_x_names) %>%
    dplyr::mutate(formula_str = paste(variables_y, "~", variables_x))

  # --- 3. Execució dels tests ---
  run_safe_test <- function(formula, data, ...) {
    test_func <- get(stat_test, asNamespace("rstatix"))
    all_args <- list(formula = as.formula(formula), data = data, p.adjust.method = "none", ...)
    try(do.call(test_func, all_args), silent = TRUE)
  }

  results <- df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(grup_by_names))) %>%
    tidyr::nest() %>%
    dplyr::cross_join(tasks) %>%
    dplyr::mutate(stat_result = purrr::map2(data, formula_str, ~run_safe_test(.y, .x, ...))) %>%
    dplyr::select(-data)

  # Gestionar resultats fallits
  failed_tests <- results %>% dplyr::filter(sapply(stat_result, inherits, "try-error"))
  if (nrow(failed_tests) > 0) {
    warning(nrow(failed_tests), " combinacions de test han fallat i han estat excloses.")
  }
  
  results <- results %>%
    dplyr::filter(!sapply(stat_result, inherits, "try-error")) %>%
    tidyr::unnest(stat_result) %>%
    dplyr::ungroup()

  # --- 4. Neteja i estandardització ---
  if (!"p" %in% names(results) && !"p.adj" %in% names(results)) {
    warning(paste("El test", stat_test, "no ha retornat una columna de p-valor. No es pot realitzar l'ajust."))
    results$raw_pval <- NA_real_
  } else {
    p_col <- if ("p" %in% names(results)) "p" else "p.adj"
    results <- dplyr::rename(results, raw_pval = !!p_col)
  }

  results <- results %>%
    janitor::clean_names("snake") %>%
    dplyr::mutate(stat_method = factor(paste0(stat_test, " ~ ", metode)))

  # --- 5. Ajust de p-values ---
  if ("raw_pval" %in% names(results)) {
    # Utilitza la funció `adjust_pvalue` especificada per l'usuari.
    if (length(grup_adjust_names) > 0) {
      results <- results %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(grup_adjust_names))) %>%
        dplyr::arrange(raw_pval) %>%
        adjust_pvalue(pval_col = "raw_pval", method = metode, trend = trend) %>%
        dplyr::ungroup()
    } else {
      results <- results %>%
        dplyr::arrange(raw_pval) %>%
        adjust_pvalue(pval_col = "raw_pval", method = metode, trend = trend)
    }
  }

  # --- 6. Organització de la sortida ---
  output_list <- results %>%
    dplyr::group_by(variables_y) %>%
    dplyr::group_split()

  names(output_list) <- sapply(output_list, function(d) unique(d$variables_y))
  return(output_list)
}










#' @title Realitza tests estadístics bivariats.
#' @description Realitza tests de Wilcoxon i T-test.
#' @param ... Paràmetres de la funció original.
#' @return Un dataframe amb els resultats dels tests.
#' @export
stats_bitest <- function(
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
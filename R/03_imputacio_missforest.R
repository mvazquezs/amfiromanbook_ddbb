#' Imputació de dades amb `missForest` amb optimització i agrupació
#'
#' @description
#' Aquesta funció realitza una imputació de valors perduts utilitzant l'algorisme
#' `missForest`. Genera columnes de "flags" per a cada variable imputada.
#'
#' @inheritParams imputacio_estadistics
#' @param optim_mtry Booleà (`TRUE`/`FALSE`) per optimitzar `mtry`.
#' @param ntree Nombre d'arbres a créixer al bosc.
#' @param maxiter Nombre màxim d'iteracions de l'algorisme.
#' @param ... Altres paràmetres per a `missForest::missForest()`.
#'
#' @return Una llista que conté:
#'   - `imputed_df`: El data frame amb els valors imputats i les columnes 'flag_*'.
#'   - `original_df`: (Opcional) El data frame original.
#'   - `report`: (Opcional) Una llista amb un resum i el detall de la imputació.
#' @export
imputacio_missforest <- function(
  df,
  seleccio_variables,
  grup_by = NULL,
  grup_by_reserva = NULL,
  optim_mtry = FALSE,
  ntree = 100,
  maxiter = 10,
  verbose = FALSE,
  set_seed = 19810424,
  report_imputacio = TRUE,
  retorna_original = FALSE)
{
  ### 'Quositation'
  grup_by_enquo <- rlang::enquo(grup_by)
  grup_by_reserva_enquo <- rlang::enquo(grup_by_reserva)
  seleccio_variables_enquo <- rlang::enquo(seleccio_variables)
  
  ### Preparació del data frame
  df_out <- df %>%
    dplyr::mutate(
      dplyr::across(!!seleccio_variables_enquo, as.numeric),
      dplyr::across(-c(!!seleccio_variables_enquo), as.factor)
    )
  
  ### Creació de les flags (inicialitzades a 0)
  df_out <- df_out %>%
    dplyr::mutate(
      dplyr::across(!!seleccio_variables_enquo, ~0, .names = "flag_{.col}")
    )
  
  #' @title Funció interna per executar missForest
  #' @description Envolta la funció `missForest::missForest` per gestionar l'optimització
  #' opcional de `mtry`.
  #' @param df_miss Data frame amb les dades a imputar.
  #' @param optim Booleà per activar o desactivar l'optimització de `mtry`.
  #' @param ... Altres paràmetres per a `missForest::missForest`.
  #' @return Un objecte de classe `missForest`.
  #' @keywords internal
  f_missforest <- function(df_miss, optim = FALSE, ...) {
    if (isTRUE(optim)) {
      p <- ncol(df_miss)
      mtry_candidates <- 1:p
      oob_errors <- numeric(length(mtry_candidates))
      
      for (i in seq_along(mtry_candidates)) {
        result <- missForest::missForest(df_miss, mtry = mtry_candidates[i], ...)$OOBerror
        oob_errors[i] <- result
      }
      
      best_mtry <- mtry_candidates[which.min(oob_errors)]
      df_imputat <- missForest::missForest(df_miss, mtry = best_mtry, ...)
    } else {
      df_imputat <- missForest::missForest(df_miss, ...)
    }
    return(df_imputat)
  }
  
  safe_f_missforest <- purrr::possibly(f_missforest, otherwise = NULL, quiet = !verbose)
  set.seed(set_seed)
  
  ### Informe - Estat Inicial
  report_list <- list()
  if(isTRUE(report_imputacio)) {
    na_originals_per_var <- df_out %>%
      dplyr::select(!!seleccio_variables_enquo) %>%
      purrr::map_df(~sum(is.na(.))) %>%
      tidyr::pivot_longer(cols = dplyr::everything(), names_to = "variable", values_to = "na_originals")
    report_list$estat_inicial <- na_originals_per_var
  }
  
  ### Lògica d'imputació
  if (rlang::quo_is_null(grup_by_enquo)) {
    # Imputació global
    if(sum(is.na(df_out %>% dplyr::select(!!seleccio_variables_enquo))) > 0) {
      df_out <- df_out %>%
        dplyr::mutate(dplyr::across(!!seleccio_variables_enquo, ~ifelse(is.na(.), 1, get(paste0("flag_", dplyr::cur_column()))), .names = "flag_{.col}"))
      
      cols_to_impute <- df_out %>% dplyr::select(!!seleccio_variables_enquo)
      missforest_result <- safe_f_missforest(as.data.frame(cols_to_impute), optim = optim_mtry, ntree = ntree, maxiter = maxiter, verbose = verbose)
      
      if (!is.null(missforest_result)) {
        df_out[, names(cols_to_impute)] <- imputed_missforest$ximp
      }
    }
  } else {
    # Imputació per grup_by
    df_out <- df_out %>%
      dplyr::group_by(!!grup_by_enquo) %>%
      dplyr::mutate(dplyr::across(!!seleccio_variables_enquo, ~ifelse(is.na(.), 1, get(paste0("flag_", dplyr::cur_column()))), .names = "flag_{.col}")) %>%
      tidyr::nest()
      
    nested_data <- df_out %>%
      dplyr::mutate(
        imputed_data = purrr::map(data, ~{
          df_to_impute <- .x
          if (any(is.na(df_to_impute %>% dplyr::select(!!seleccio_variables_enquo)))) {
            cols_to_impute <- df_to_impute %>% dplyr::select(!!seleccio_variables_enquo)
            imputed_missforest <- safe_f_missforest(as.data.frame(cols_to_impute), optim = optim_mtry, ntree = ntree, maxiter = maxiter, verbose = verbose)
            if (!is.null(imputed_missforest)) {
              df_to_impute[, names(cols_to_impute)] <- imputed_missforest$ximp
            }
          }
          df_to_impute
        })
      )
    
    df_out <- nested_data %>%
      dplyr::select(-data) %>%
      tidyr::unnest(cols = c(imputed_data)) %>%
      dplyr::ungroup()
      
    ### Informe - Després de grup_by
    if(isTRUE(report_imputacio)) {
      na_after_g1 <- df_out %>%
        dplyr::select(!!seleccio_variables_enquo) %>%
        purrr::map_df(~sum(is.na(.))) %>%
        tidyr::pivot_longer(cols = dplyr::everything(), names_to = "variable", values_to = "na_post_grup_by")
      report_list$estat_grup_by <- na_after_g1
    }
    
    # Imputació per grup_by_reserva
    if (any(is.na(df_out %>% dplyr::select(!!seleccio_variables_enquo))) && !rlang::quo_is_null(grup_by_reserva_enquo)) {
      df_out <- df_out %>%
        dplyr::group_by(!!grup_by_reserva_enquo) %>%
        dplyr::mutate(dplyr::across(!!seleccio_variables_enquo, ~ifelse(is.na(.), 2, get(paste0("flag_", dplyr::cur_column()))), .names = "flag_{.col}")) %>%
        tidyr::nest()

      nested_data_reserva <- df_out %>%
        dplyr::mutate(
          imputed_data = purrr::map(data, ~{
            df_to_impute <- .x
            if (any(is.na(df_to_impute %>% dplyr::select(!!seleccio_variables_enquo)))) {
              cols_to_impute <- df_to_impute %>% dplyr::select(!!seleccio_variables_enquo)
              imputed_missforest <- safe_f_missforest(as.data.frame(cols_to_impute), optim = optim_mtry, ntree = ntree, maxiter = maxiter, verbose = verbose)
              if (!is.null(imputed_missforest)) {
                df_to_impute[, names(cols_to_impute)] <- imputed_missforest$ximp
              }
            }
            df_to_impute
          })
        )
        
      df_out <- nested_data_reserva %>%
        dplyr::select(-data) %>%
        tidyr::unnest(cols = c(imputed_data)) %>%
        dplyr::ungroup()
        
      ### Informe - Després de grup_by_reserva
      if(isTRUE(report_imputacio)) {
        na_after_g2 <- df_out %>%
          dplyr::select(!!seleccio_variables_enquo) %>%
          purrr::map_df(~sum(is.na(.))) %>%
          tidyr::pivot_longer(cols = dplyr::everything(), names_to = "variable", values_to = "na_post_reserva")
        report_list$estat_grup_by_reserva <- na_after_g2
      }
    }
  }
  
  ### Muntatge del report final
  if (isTRUE(report_imputacio) && length(report_list) > 0) {
    report_final <- purrr::reduce(report_list, dplyr::full_join, by = "variable") %>%
      dplyr::mutate(
        na_post_grup_by = ifelse(is.null(grup_by_enquo), na_originals, na_post_grup_by),
        imputats_grup_by = na_originals - dplyr::coalesce(na_post_grup_by, na_originals),
        imputats_reserva = dplyr::coalesce(na_post_grup_by, 0) - dplyr::coalesce(na_post_reserva, 0),
        na_finals = dplyr::coalesce(na_post_reserva, na_post_grup_by, na_originals)
      ) %>%
      dplyr::select(
        variable, 
        na_originals, 
        imputats_grup_by, 
        imputats_reserva, 
        na_finals
      )
    
    resum <- tibble::tibble(
      na_originals_total = sum(report_final$na_originals, na.rm = TRUE),
      na_imputats_total = sum(report_final$imputats_grup_by, na.rm = TRUE) + sum(report_final$imputats_reserva, na.rm = TRUE),
      na_finals_total = sum(report_final$na_finals, na.rm = TRUE)
    )
    
    report <- list(resum_general = resum, detall_per_variable = report_final)
  } else {
    report <- NULL
  }

  ### Sortida
  result <- list(imputed_df = df_out)
  
  if (isTRUE(retorna_original)) {
    result$original_df <- df
  }
  
  if (isTRUE(report_imputacio)) {
    result$report <- report
  }

  return(result)
}
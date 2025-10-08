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
  retornar_original = FALSE)
{
  ### 'Quositation'
  grup_by_sym <- rlang::ensym(grup_by)
  grup_by_reserva_sym <- rlang::ensym(grup_by_reserva)
  seleccio_variables_enquo <- rlang::enquo(seleccio_variables) # Mantenim enquo per seleccions complexes
  
  df_original <- df

  ### Preparació del data frame
  df_out <- df_original %>%
    dplyr::mutate(.row_id = dplyr::row_number()) %>%
    dplyr::mutate(
      dplyr::across(!!seleccio_variables_enquo, as.numeric),
      dplyr::across(-dplyr::any_of(names(dplyr::select(., !!seleccio_variables_enquo))), as.factor)
    )
  
  ### Creació de les flags (inicialitzades a 0)
  impute_vars_names <- df_out %>% dplyr::select(!!seleccio_variables_enquo) %>% names()
  
  df_out <- df_out %>%
    dplyr::mutate(dplyr::across(all_of(impute_vars_names), ~0, .names = 'flag_{.col}'))
  
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
      tidyr::pivot_longer(cols = dplyr::everything(), names_to = "variable", values_to = "n_na_ori")
    report_list$estat_inicial <- na_originals_per_var
  }
  
  ### Lògica d'imputació
  if (rlang::is_missing(grup_by_sym)) {
    # 3. Imputació global (if no group_by specified or still NAs)
    if(sum(is.na(df_out %>% dplyr::select(!!seleccio_variables_enquo))) > 0) {
      # Get current NA status of impute_vars
      na_status_before_imputation <- df_out %>% dplyr::select(all_of(impute_vars_names)) %>% is.na()
      # Get current flag values
      current_flags_df <- df_out %>% dplyr::select(starts_with("flag_"))

      # Impute
      cols_to_impute <- df_out %>% dplyr::select(!!seleccio_variables_enquo)
      missforest_result <- suppressWarnings(safe_f_missforest(as.data.frame(cols_to_impute), optim = optim_mtry, ntree = ntree, maxiter = maxiter, verbose = verbose))
      
      if (!is.null(missforest_result)) {
        df_out[, names(cols_to_impute)] <- missforest_result$ximp

        # Update flags for values that were NA and had flag 0
        for (col_name in impute_vars_names) {
          flag_col_name <- paste0("flag_", col_name)
          df_out[[flag_col_name]] <- ifelse(
            na_status_before_imputation[, col_name] & current_flags_df[[flag_col_name]] == 0,
            3, # Flag for global imputation
            df_out[[flag_col_name]] # Keep existing flag
          )
        }
      }
    }
  } else {
    # Imputació per grup_by
    df_out <- df_out %>%
      dplyr::group_by(!!grup_by_sym) %>%
      tidyr::nest()
      
    nested_data <- df_out %>%
      dplyr::mutate(
        imputed_data = purrr::map(data, ~{
          df_to_impute <- .x
          
          na_status_before_imputation <- df_to_impute %>% dplyr::select(dplyr::any_of(impute_vars_names)) %>% is.na()
          current_flags_df <- df_to_impute %>% dplyr::select(starts_with("flag_"))
          
          cols_to_impute <- df_to_impute %>% dplyr::select(dplyr::any_of(impute_vars_names))

          if (ncol(cols_to_impute) > 0 && any(na_status_before_imputation)) {
              imputed_missforest <- suppressWarnings(safe_f_missforest(as.data.frame(cols_to_impute), optim = optim_mtry, ntree = ntree, maxiter = maxiter, verbose = verbose))
              
              if (!is.null(imputed_missforest)) {
                # Assign back by name to avoid dimension mismatch if missForest drops a column
                for(col in names(imputed_missforest$ximp)) {
                  df_to_impute[[col]] <- imputed_missforest$ximp[[col]]
                }
                
                # Update flags
                for (col_name in names(cols_to_impute)) {
                  flag_col_name <- paste0("flag_", col_name)
                  if (flag_col_name %in% names(df_to_impute)) {
                    df_to_impute[[flag_col_name]] <- ifelse(
                      na_status_before_imputation[, col_name] & current_flags_df[[flag_col_name]] == 0,
                      1, # Flag for grup_by
                      df_to_impute[[flag_col_name]]
                    )
                  }
                }
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
    if (any(is.na(df_out %>% dplyr::select(!!seleccio_variables_enquo))) && !rlang::is_missing(grup_by_reserva_sym)) {
      df_out <- df_out %>%
        dplyr::group_by(!!grup_by_reserva_sym) %>%
        tidyr::nest()

      nested_data_reserva <- df_out %>%
        dplyr::mutate(
          imputed_data = purrr::map(data, ~{
            df_to_impute <- .x
            
            na_status_before_imputation <- df_to_impute %>% dplyr::select(dplyr::any_of(impute_vars_names)) %>% is.na()
            current_flags_df <- df_to_impute %>% dplyr::select(starts_with("flag_"))
            
            cols_to_impute <- df_to_impute %>% dplyr::select(dplyr::any_of(impute_vars_names))

            if (ncol(cols_to_impute) > 0 && any(na_status_before_imputation)) {
                imputed_missforest <- suppressWarnings(safe_f_missforest(as.data.frame(cols_to_impute), optim = optim_mtry, ntree = ntree, maxiter = maxiter, verbose = verbose))
                
                if (!is.null(imputed_missforest)) {
                  # Assign back by name to avoid dimension mismatch if missForest drops a column
                  for(col in names(imputed_missforest$ximp)) {
                    df_to_impute[[col]] <- imputed_missforest$ximp[[col]]
                  }
                  
                  # Update flags
                  for (col_name in names(cols_to_impute)) {
                    flag_col_name <- paste0("flag_", col_name)
                    if (flag_col_name %in% names(df_to_impute)) {
                      df_to_impute[[flag_col_name]] <- ifelse(
                        na_status_before_imputation[, col_name] & current_flags_df[[flag_col_name]] == 0,
                        2, # Flag for grup_by_reserva
                        df_to_impute[[flag_col_name]]
                      )
                    }
                  }
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
    report <- purrr::reduce(report_list, dplyr::full_join, by = "variable")

    na_finals <- df_out %>%
      dplyr::select(!!seleccio_variables_enquo) %>%
      purrr::map_df(~sum(is.na(.))) %>%
      tidyr::pivot_longer(cols = dplyr::everything(), names_to = "variable", values_to = "n_na_finals")
    report <- dplyr::left_join(report, na_finals, by = "variable")

    report <- report %>%
      dplyr::mutate(
        n_imp_grup = if ("na_post_grup_by" %in% names(.)) n_na_ori - na_post_grup_by else 0,
        n_imp_reserva = if (all(c("na_post_grup_by", "na_post_reserva") %in% names(.))) na_post_grup_by - na_post_reserva else 0
      )

    report_totals <- report %>%
      dplyr::summarise(
        variable = "Total",
        n_na_ori = sum(n_na_ori, na.rm = TRUE),
        n_imp_grup = sum(n_imp_grup, na.rm = TRUE),
        n_imp_reserva = sum(n_imp_reserva, na.rm = TRUE),
        n_na_finals = sum(n_na_finals, na.rm = TRUE)
      )

    report <- dplyr::bind_rows(report, report_totals) %>%
      dplyr::select(
        variable, 
        n_na_ori, n_imp_grup, n_imp_reserva, n_na_finals) %>%
        dplyr::filter(!str_detect(variable, pattern = "flag_"))
  }

  ### Sortida
  # Reorder rows to match original and select columns
  original_cols <- names(df_original)
  flag_cols <- names(df_out) %>% stringr::str_subset("^flag_")

  df_out <- df_out %>%
    dplyr::arrange(.row_id) %>%
    dplyr::select(all_of(original_cols), all_of(flag_cols))

  result <- list(imputed_df = df_out)
  
  if (isTRUE(retornar_original)) {
    result$original_df <- df_original
  }
  
  if (isTRUE(report_imputacio)) {
    result$report <- report
  }

  return(result)
}
# --- Funcions Estadístiques ---
### adjust_pvalue

#' @title Ajusta els p-values per a comparacions múltiples i afegeix format.
#' @description Afegeix columnes de p-values ajustats, text formatat i símbols
#'   de significació a un dataframe.
#' @param df Dataframe que conté una columna de p-values (per defecte, 'raw_pval').
#' @param pval_col El nom de la columna que conté els p-values a processar.
#' @param metode Mètode d'ajust a passar a `stats::p.adjust`.
#' @param trend Lògic. Si és `TRUE`, s'utilitzen llindars de significació
#'   menys estrictes (0.01, 0.05, 0.1) per als p-values ajustats.
#' @return Un dataframe amb les noves columnes de p-values afegides.
#' @rdname adjust_pvalue
#' @export
adjust_pvalue <- function(
  df,
  pval_col = 'raw_pval',
  metode = c('holm', 'hochberg', 'hommel', 'bonferroni', 'BH', 'BY', 'fdr', 'none'),
  trend = FALSE)
{
  # Validació d'arguments
  methode <- match.arg(metode)

### Funció auxiliar per generar text i símbols de significació
### Evita la repetició de codi (DRY principle)
  .format_pval <- function(p_values, trend_levels = FALSE) {
    
  ### Defineix els llindars de significació
    if (trend_levels) {

    ### Llindars per a l'anàlisi de tendències
      levels_signif <- list(
        text = c('P < 0.01', 'P < 0.05', 'ns'),
        symb = c('***', '**', '*', 'ns'))
      
      levels_p_values <- c(0.01, 0.05, 0.1)
      
    } else {
    
    ### Llindars estàndard
      levels_signif <- list(
        text = c('P < 0.0001', 'P < 0.001', 'ns'),
        symb = c('***', '**', '*', '.', 'ns'))
      levels_p_values <- c(0.0001, 0.001, 0.01, 0.05)
    
    }

  ### Genera el text del p-valor (ex: 'P = 0.023')
    text_col <- dplyr::case_when(
      is.nan(p_values) ~ 'nc', # Not computed
      is.na(p_values) ~ 'np',  # Not present
      p_values <= min(levels_p_values) ~ levels_signif$text[1],
      p_values <= levels_p_values[2] & trend_levels ~ levels_signif$text[2],
      p_values <= levels_p_values[2] & !trend_levels ~ levels_signif$text[2],
      p_values <= 0.05 ~ paste0('P = ', round(p_values, 3)),
      TRUE ~ 'ns') # Not significant
      
    if (trend_levels) {
    
      text_col <- dplyr::case_when(
        is.nan(p_values) ~ 'nc',
        is.na(p_values) ~ 'np',
        p_values <= 0.01 ~ 'P < 0.01',
        p_values <= 0.05 ~ 'P < 0.05',
        p_values <= 0.1  ~ paste0('P = ', round(p_values, 3)),
        TRUE ~ 'ns')
  
    }

  ### Genera els símbols de significació (ex: "***")
    signif_col <- dplyr::case_when(
      is.nan(p_values) ~ 'nc',
      is.na(p_values) ~ 'np',
      p_values <= levels_p_values[1] ~ levels_signif$symb[1],
      p_values <= levels_p_values[2] ~ levels_signif$symb[2],
      p_values <= levels_p_values[3] ~ levels_signif$symb[3],
      p_values <= tail(levels_p_values, 1) ~ tail(levels_signif$symb, 2)[1],
      TRUE ~ 'ns')
      
  ### Retorna una llista amb les dues columnes generades
    list(text = factor(text_col), signif = factor(signif_col))

}

### Comprova si la columna de p-values existeix
  if (!pval_col %in% names(df)) {
    
    warning(paste("La columna '", pval_col, "' no existeix al dataframe. S'ha retornat el dataframe original.", sep = ""))
    
    return(df)
  }

### Extreu els p-values originals
  raw_pvals <- df[[pval_col]]
  
### Formata els p-values originals
  raw_formats <- .format_pval(raw_pvals, trend_levels = FALSE)

### Calcula els p-values ajustats (p.adjust gestiona els NA correctament)
  adj_pvals <- stats::p.adjust(raw_pvals, metode = metode)
  
### Formata els p-values ajustats
  adj_formats <- .format_pval(adj_pvals, trend_levels = trend)

### Afegeix totes les columnes al dataframe amb noms clars
  df <- df %>%
    dplyr::mutate(
      !!paste0(pval_col, '_text')   := raw_formats$text,
      !!paste0(pval_col, '_signif')  := raw_formats$signif,
      adj_pval = adj_pvals,
      adj_pval_text = adj_formats$text,
      adj_pval_signif = adj_formats$signif)

  return(df)
}

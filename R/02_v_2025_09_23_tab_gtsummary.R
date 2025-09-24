### the 'amphi_func.tab_gtsummary'

#' @title Crea una taula de resum estil `gtsummary`.
#' @description Aquesta funció embolcalla `gtsummary::tbl_summary` per crear taules de resum descriptiu altament personalitzables.
#'
#' @param df El `data.frame` o `tibble` d'entrada.
#' @param seleccio_variables Les variables a incloure a la taula. Es pot utilitzar la sintaxi de selecció de `dplyr`.
#' @param grup_by La variable per agrupar les dades (es passarà a l'argument `by` de `tbl_summary`).
#' @param type_list Una llista que especifica el tipus de resum per a cada variable (p. ex., `list(age ~ 'continuous2')`).
#' @param statistic_list Una llista que defineix les estadístiques a mostrar (p. ex., `list(all_continuous() ~ "{mean} ({sd})")`).
#' @param digits_list Una llista o valor numèric per controlar els dígits decimals.
#' @param value_list Una llista per especificar el valor a mostrar per a variables dicotòmiques.
#' @param label_list Una llista per personalitzar les etiquetes de les variables.
#' @param missing_text Text per a les dades que falten (per defecte `"NA"`).
#' @param add_overall Lògic. Si és `TRUE`, afegeix una columna "Overall".
#' @param bold_labels Lògic. Si és `TRUE`, posa en negreta les etiquetes de les variables.
#' @param header_list Una llista per personalitzar les capçaleres de la taula.
#' @param table_title Títol per a la taula.
#'
#' @return Un objecte `gtsummary` que representa la taula de resum.
#'
#' @importFrom gtsummary tbl_summary add_overall bold_labels modify_header as_gt
#' @importFrom dplyr select all_of
#' @importFrom rlang enquo
#' @importFrom gt tab_header opt_align_table_header tab_style cells_row_groups everything opt_row_striping tab_options
#'
#' @examples
#' df_prova <- tibble::tibble(
#'  provincia = sample(c("Tarraconensis", "Baetica"), 100, replace = TRUE),
#'  nom = sample(c("Tarraco", "Emporiae", "Italica"), 100, replace = TRUE),
#'  amplada_general = rnorm(100, 110, 15),
#'  alcada_general = rnorm(100, 90, 10),
#'  es_gran = amplada_general > 115
#' )
#'
#' # Exemple d'ús de la funció millorada
#' amphi_func.tab_gtsummary(
#'   df = df_prova,
#'   seleccio_variables = c(amplada_general, alcada_general),
#'   grup_by = provincia,
#'   type_list = list(es_gran ~ "dichotomous"),
#'   label_list = list(
#'     amplada_general ~ 'Amplada General (m)',
#'     alcada_general ~ 'Alçada General (m)')),
#'   table_title = 'Dimensions dels amfiteatres per província')
#'
#' @rdname amphi_func.tab_gtsummary
#' @export
amphi_func.tab_gtsummary <- function(
    df,
    seleccio_variables,
    grup_by = NULL,
    type_list = NULL,
    statistic_list = NULL,
    digits_list = 2,
    value_list = NULL,
    label_list = NULL,
    missing_text = 'NA',
    add_overall = TRUE,
    bold_labels = TRUE,
    header_list = NULL,
    table_title = NULL)
{
  # Captura d'expressions
  grup_by_quo <- rlang::enquo(grup_by)
  seleccio_quo <- rlang::enquo(seleccio_variables)
  
  # Selecció de dades
  df_selected <- df %>%
    dplyr::select(!!grup_by_quo, !!seleccio_quo)
  
  # Construcció de la taula base
  table <- gtsummary::tbl_summary(
    df_selected,
    by = !!grup_by_quo,
    type = type_list,
    statistic = statistic_list,
    digits = digits_list,
    value = value_list,
    label = label_list,
    missing = "no", # Per defecte, no mostra els 'missing'
    missing_text = missing_text
  )
  
  # Afegir columna "Overall"
  if (add_overall) {
    table <- table %>% gtsummary::add_overall(last = FALSE)
  }
  
  # Posar etiquetes en negreta
  if (bold_labels) {
    table <- table %>% gtsummary::bold_labels()
  }
  
  # Modificar capçaleres si s'especifica
  if (!is.null(header_list)) {
    table <- table %>% gtsummary::modify_header(!!!header_list)
  }
  
  # Convertir a objecte 'gt' i aplicar estil comú
  gt_table <- table %>%
    gtsummary::as_gt() %>%
    gt::tab_header(title = gt::md(table_title)) %>%
    gt::opt_align_table_header(align = "left") %>%
    gt::tab_style(
      style = list(gt::cell_text(weight = "bold")),
      locations = gt::cells_row_groups(groups = gt::everything())
    ) %>%
    gt::opt_row_striping(row_striping = TRUE) %>%
    gt::tab_options(table.font.names = "Times New Roman")
  
  return(gt_table)
}

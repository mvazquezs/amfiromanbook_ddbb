#' @title Configura els directoris i carrega els paquets del projecte
#' @description Aquest script s'encarrega de la configuració inicial del projecte, 
#' incloent la creació de directoris i la càrrega de paquets necessaris.

# --- Configuració de Directoris ---

#' @title Configura els directoris del projecte
#' @description Aquesta funció comprova si existeixen els directoris clau del projecte i els crea si no existeixen.
#' @param base_path La ruta base del projecte. Per defecte, el directori de treball actual.
#' @param dirs_to_create Un vector de caràcters amb els noms dels directoris a crear.
#' @return No retorna cap valor, però mostra un missatge per cada directori creat.
#' @export
#' @examples
#' # Executar amb els directoris per defecte
#' # amphi_setup_dirs()
amphi_setup_dirs <- function(base_path = getwd(), dirs_to_create = c("data", "R", "output", "capitols")) {
  
  for (dir in dirs_to_create) {
    dir_path <- file.path(base_path, dir)
    if (!dir.exists(dir_path)) {
      dir.create(dir_path, recursive = TRUE)
      message(paste("Directori creat:", dir_path))
    } else {
      message(paste("El directori ja existeix:", dir_path))
    }
  }
}

# --- Càrrega de Paquets ---

#' @title Carrega i instal·la els paquets necessaris
#' @description Aquesta funció utilitza el paquet 'pacman' per comprovar, instal·lar (si cal) i carregar totes les dependències de R per a l'anàlisi. També configura algunes opcions globals de la sessió.
#' @param update_packages Lògic. Si és `TRUE`, intenta actualitzar tots els paquets a la seva darrera versió. Per defecte és `FALSE`.
#' @return No retorna cap valor, però carrega els paquets a l'entorn global.
#' @import pacman
#' @seealso \code{\link{pacman::p_load}}
#' @export
#' @examples
#' # Carregar tots els paquets necessaris
#' # amphi_load_packages()
#' 
#' # Carregar i forçar l'actualització
#' # amphi_load_packages(update_packages = TRUE)
amphi_load_packages <- function(update_packages = FALSE) {

  # Assegura que 'pacman' estigui instal·lat i carregat
  if (!requireNamespace('pacman', quietly = TRUE)) {
    install.packages('pacman')
  }
  library(pacman)

  # Configura el repositori de CRAN
  local({
    r <- getOption('repos')
    r['CRAN'] <- 'https://cloud.r-project.org/'
    options(repos = r)
  })

  # Llista de paquets a carregar
  packages_to_load <- c(
    'dplyr', 'tibble', 'readxl', 'data.table', 'stringr',
    'rlang', 'purrr', 'tidyr', 'missForest', 'psych',
    'DT', 'htmltools', 'knitr', 'webshot', 'webshot2', 'shiny',
    'gtsummary', 'gt', 'gtExtras', 'flextable', 'huxtable',
    'crayon', 'janitor',
    'rstatix', 'Hmisc')

  # Carrega o instal·la els paquets amb pacman
  pacman::p_load(
    char = packages_to_load,
    install = TRUE,
    update = update_packages)
  
  # Opcions addicionals de la sessió
  options(width = getOption('width'))

  # missatge
  cat(
    crayon::bold(crayon::blue('i')),
    crayon::black(' Els paquets han estat carregats correctament\n'))

}

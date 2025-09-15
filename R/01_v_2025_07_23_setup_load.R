### 'amphi_list.dir_files()'

#' @title Llista arxius de diferents carpetes de dades.
#' @description Llista arxius de diferents carpetes de dades donat un 'home_folder'.
#' @param home_folder Objecte R amb la ruta a les carpetes de dades.
#' @param pattern Carrega un format d'extensió específic entre 'xlsx', 'xls' i 'csv'.
#' @param recursive Lògic (TRUE per defecte). Llista els arxius de forma recursiva.
#'
#' @return Una llista d'arxius estructurada per 'carpetes' anomenades.
#' 
#' @rdname amphi_list.dir_files
#' @export
amphi_list.dir_files <- function(
    home_folder,
    recursive = TRUE,
    pattern = c('xlsx', 'xls', 'csv'))
{
### 1st double check
	if (!dir.exists(home_folder)) {
    
		stop("Error: 'home_folder' no es un directori vàlid o no existeix. Si us plau, verifica la ruta.")
  
	}

### 2nd double check
	valid_patterns <- c('xlsx', 'xls', 'csv')

	if (!any(pattern %in% valid_patterns)) {
    
		warning("Advertència: el 'pattern' especificat no és l'extensió dels arxius. Si us plau, assegura que siguin correctes.")
  
	}

### Llista directoris
  l_dir <- list.dirs(
  	path = home_folder,
  	recursive = recursive,
		full.names = TRUE)

### 3rd double check
	if (length(l_dir) == 0) {

    message("No s'han trobat subdirectoris: ", home_folder)
  
	  return(list())
  
	}

### Crea un patró de cerca regex
  l_pattern <- paste0('\\.(', paste(pattern, collapse = '|'), ')$', sep = '')

### Llista els arxius
	l_files <- list()

	for(i in seq_along(l_dir)) {

  	file <- list.files(
  		path = l_dir[i],
  		pattern = l_pattern,
  		full.names = TRUE,
  		recursive = TRUE)

  	### save in a list
  		l_files[[length(l_files) + 1]] = file

	}

### Nom de la llista
	names(l_files) <- basename(l_dir)

### Guarda un únic element 'l_files'
  rm(list = setdiff(ls(), 'l_files'))


### Rerornar la llista
	return(l_files)
}


### 'amphi_func.read_data()'

#' Loads data in several extension formats as description mentions.
#' @inheritParams readxl::excel_sheets
#' @inheritParams readxl::read_excel
#' @inheritParams data.table::fread
#' @description This function reads a list of files, supporting both Excel (.xls, .xlsx) and .csv formats.
#' It automatically detects the file type based on the file extension.
#' @param l_files Una llista d'arxius obtinguda de 'amphi_list.dir_files'.
#' @param type_file El tipus d'arxiu a llegir ('excel' o 'csv').
#' @param sep The field separator character for CSV files. Defaults to NULL (auto-detect).
#' @param dec The decimal separator character for CSV files. Defaults to NULL (auto-detect).
#' @param skip_rows (integer, per defecte 0) Nombre de files a saltar en la lectura. Només per a 'excel'.
#' @param na_strings na_strings Vector de cadenes de text per identificar com a NA. Només per a 'csv'.
#' @param clean_names (logical, default TRUE) Whether to clean column names using `janitor::clean_names`.
#' 
#' @return A list of dataset in 'data.frame' format.
#' 
#' @importFrom readxl excel_sheets read_excel
#' @importFrom data.table fread
#' @importFrom janitor clean_names
#' @seealso \code{\link[readxl]{excel_sheets}}
#' @seealso \code{\link[readxl]{read_excel}}
#' @seealso \code{\link[data.table]{fread}}
#' @seealso \code{\link[data.table]{fread}}
#' @seealso \code{\link[janitor]{clean_names}}
#' 
#' @rdname amphi_load.read_files
#' @export
amphi_load.read_data <- function(
  l_files,
  type_file = c('excel', 'csv'),
  sep = NULL,
  dec = NULL,
  skip_rows = 0,
  na_strings = NULL,
  clean_names = TRUE) {

### packages required
  amphi_tool.required_packages(
    locale = 'es_ES.UTF-8',
    update_packages = FALSE)

### 1st double check
  if (!is.character(l_files) || length(l_files) == 0) {
    
    message("No s'han trobat llistat d'arxius: ", l_files)
  
  }

### list
  l_df <- list()


### loop #01
	for(i in seq_along(l_files)) {

	### argument 'type_file'
		if(type_file == 'excel') {
			
			### Obtenint info sobre totes les fulles i arxius 'excel'
  			l_sheet <- readxl::excel_sheets(l_files[[i]])

				df <- lapply(l_sheet,
					function(x) readxl::read_excel(l_files[[i]], sheet = x, skip = skip))

  		### Assigna noms a les matrius
  			names(df) <- l_sheet

		} else if(type_file == 'csv') {
    
    ### Argumnent per als NA
      if (is.null(na_strings)) {
    		csv_na_strings <- c('', 'NA')
      } else {
        csv_na_strings <- na_strings
      }

		### getting info about all 'csv' files

		  df <- lapply(l_files[[i]],
  		  data.table::fread,
  			sep = sep,
  			dec = dec,
        skip = skip_rows,
  			na.strings = csv_na_strings)

  	}

	### save list
		l_df[[length(l_df) + 1]] = df

	}

### unlist()
	 l_df <- unlist(l_df, recursive = FALSE)


### apply 'clean_names'
  if (clean_names == TRUE && length(l_df) > 0) {
	  
    for(i in seq_along(l_df)){

		  l_df[[i]] <- l_df[[i]] %>%
			  janitor::clean_names(case = 'snake')

	  }
  }

### kept only the return elements in the environment
  rm(list = setdiff(ls(), 'l_df'))


### return
	return(l_df)
}
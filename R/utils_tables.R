#'
#' #' prepare_rhandsontable
#' #'
#' #' @param r_data data
#' #' @param dictionary dict
#' #' @param dataset_name name
#' #' @param add_total_row logical
#' #' @param digits number
#' #' @param currency_cols character vector
#' #' @param percent_cols character vector
#' #' @param percent_digits number
#' #' @param center_cols character vector
#' #' @param stretch logical
#' #' @param search search
#' #' @param height height
#' #' @param width width
#' #'
#' #' @return [rhandsontable::hot_table()]
#' #' @export
#' #' @importFrom dplyr select_if
#' #' @importFrom janitor adorn_totals
#' #' @importFrom rhandsontable rhandsontable hot_context_menu hot_table hot_cols hot_validate_numeric hot_col
#' prepare_rhandsontable <- function(r_data,
#'                                   dictionary,
#'                                   dataset_name,
#'                                   add_total_row = FALSE,
#'                                   digits = 0,
#'                                   currency_cols = NULL,
#'                                   percent_cols = NULL,
#'                                   percent_digits = 0,
#'                                   center_cols = NULL,
#'                                   stretch = "all",
#'                                   search = TRUE,
#'                                   height = NULL,
#'                                   width = NULL) {
#'   display_data <- r_data %>%
#'     apply_labels(dict = dictionary,
#'                  dataset_name = dataset_name)
#'
#'   if (add_total_row) display_data <- display_data %>%
#'       janitor::adorn_totals(fill = " ")
#'
#'   # if (stretch) fill <- "all" else fill <- "none"
#'
#'   out <- rhandsontable::rhandsontable(
#'     data = display_data,
#'     rowHeaders = NULL,
#'     digits = digits,
#'     search = search,
#'     stretchH = stretch,
#'     height = height,
#'     contextMenu = TRUE,
#'     manualColumnResize = TRUE
#'   ) %>%
#'     rhandsontable::hot_context_menu(
#'       allowReadOnly = TRUE,
#'       allowComments = TRUE,
#'       allowCustomBorders = TRUE
#'     ) %>%
#'     rhandsontable::hot_table(
#'       stretchH = stretch, #fill,
#'       highlightRow = TRUE,
#'       highlightCol = TRUE,
#'       enableComments = TRUE
#'     ) %>%
#'     rhandsontable::hot_cols(multiColumnSorting = TRUE)
#'
#'   if (!is.null(center_cols)) {
#'     centercols <- c(match(center_cols, names(r_data)))
#'     out <- out %>%
#'       rhandsontable::hot_cols(cols = centercols, halign = "htCenter")
#'   }
#'
#'   if (!is.null(currency_cols)) {
#'     currcols <- c(match(currency_cols, names(r_data)))
#'     out <- out %>%
#'       rhandsontable::hot_cols(cols = currcols, format = "$0,0")
#'   }
#'
#'   if (!is.null(percent_cols)) {
#'     pctcols <- c(match(percent_cols, names(r_data)))
#'     if (percent_digits > 0) {
#'       fmt <- paste0("0.", rep("0", percent_digits), "%")
#'     } else {
#'       fmt <- "0%"
#'     }
#'     out <- out %>%
#'       rhandsontable::hot_cols(cols = pctcols, format = fmt)
#'   }
#'
#'   numcols <- display_data %>% dplyr::select_if(is.numeric) %>% names()
#'
#'   if (length(numcols) > 0) {
#'     out <- out %>%
#'       rhandsontable::hot_validate_numeric(cols = numcols,
#'                                           min = 0,
#'                                           allowInvalid = FALSE)
#'   }
#'
#'   if (dataset_name != "renewal_costs") return(out)
#'
#'   out %>%
#'     rhandsontable::hot_col(1, type = "dropdown",
#'                            source = c("Premium", "Expense", "Excluded"))
#'
#'
#'
#' }
#'
#'
#'
#' #' #' Table Utility Functions
#' #' #'
#' #' #' These functions are helper functions that wrap tabling functions such
#' #' #' as [DT::datatable], and [rhandsontable::rhandsontable] to fit out needs
#' #' #' within the property allocation shiny app.
#' #' #'
#' #' #' @rdname utils_tables
#' #' #'
#' #' #' @export
#' #' NULL
#'
#' #' #' @describeIn utils_tables dt_table
#' #' #'
#' #' #' Custom [DT::datatable] utility function.
#' #' #'
#' #' #' @param data a (non-reactive) data.frame or tibble for the table. By default
#' #' #'   this function assumes the data passed to it is the "r" version of the data
#' #' #'   and not the "dispalay" version. Use the \code{labeled} argument to specify
#' #' #'   if labeling needs to be applied before passing into the datatable.
#' #' #'
#' #' #'
#' #' #'
#' #' #' @return a [DT::datatable]
#' #' #' @export
#' #' #'
#' #' dt_table <- function(data,
#' #'                      labeled = FALSE,
#' #'                      dict = NULL,
#' #'                      dataset_name = NULL,
#' #'                      options = list(),
#' #'                      class = "compact stripe row-border nowrap",
#' #'                      callback = htmlwidgets::JS("return table;"),
#' #'                      rownames = FALSE,
#' #'                      colnames = NULL,
#' #'                      container,
#' #'                      caption,
#' #'                      filter = c("top", "none", "bottom"),
#' #'                      escape = TRUE,
#' #'                      style = "bootstrap",
#' #'                      width,
#' #'                      height,
#' #'                      elementId,
#' #'                      editable = TRUE,
#' #'                      ...) {
#' #'
#' #'   # c("count_buckets",
#' #'   #   "loss_run",
#' #'   #   "priors",
#' #'   #   "rates",
#' #'   #   "rels",
#' #'   #   "rels_old",
#' #'   #   "renewal_costs",
#' #'   #   "sov")
#' #'
#' #'   if (!labeled) {
#' #'
#' #'     if (is.null(dict)) dict <- dictionary
#' #'
#' #'     r_colnames <- names(data)
#' #'
#' #'     data <- apply_labels(data, dict = dict, dataset_name = dataset_name)
#' #'
#' #'   }
#' #'
#' #'
#' #'   DT::datatable(
#' #'     out,
#' #'     rownames = rownames,
#' #'     selection = "none",
#' #'     class = "compact stripe row-border nowrap",
#' #'     style = "bootstrap",
#' #'     # Escape the HTML in all except 1st column (which has the buttons)
#' #'     escape = -1,
#' #'     extensions = c("Buttons"),
#' #'     filter = "top",
#' #'     options = list(
#' #'       scrollX = TRUE,
#' #'       dom = 'BRfrltpi',
#' #'       columnDefs = list(
#' #'         list(targets = 0, orderable = FALSE),
#' #'         list(
#' #'           className = "dt-center",
#' #'           targets = "_all" #,
#' #'           # render = htmlwidgets::JS(
#' #'           #   "function(data, type, row, meta) {",
#' #'           #   "return type === 'display' && data.length > 20 ?",
#' #'           #   "'<span title=\"' + data + '\">' + data.substr(0, 20) + '...</span>' : data;",
#' #'           #   "}"
#' #'           # )
#' #'         )
#' #'       ),
#' #'       buttons = list(
#' #'         'copy', 'print',
#' #'         list(
#' #'           extend = "collection",
#' #'           buttons = c('csv', 'excel', 'pdf'),
#' #'           text = "Download",
#' #'           title = paste0("Schedule-of-Values-", Sys.Date()),
#' #'           exportOptions = list(
#' #'             columns = 1:(length(out) - 1)
#' #'           )
#' #'         ),
#' #'         list(
#' #'           extend = 'colvis',
#' #'           text = 'Edit Displayed Columns'
#' #'         )
#' #'       )
#' #'     )
#' #'   ) %>%
#' #'     DT::formatCurrency(c("Total Insured Value (TIV)"), digits = 0)
#' #'
#' #' }

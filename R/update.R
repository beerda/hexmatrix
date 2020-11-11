#' @export
update <- function(updated, updates) {
  if (is.null(updated)) {
    updated <- updates
  } else if (!is.null(updates)) {
    assert_that(is.vector(updated))
    assert_that(is.vector(updates))

    if (is.null(names(updates))) {
      updated[seq_along(updates)] <- updates
    } else {
      updated[names(updates)] <- updates
    }
  }
  updated
}

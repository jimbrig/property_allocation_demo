#' Contacts
#'
#' @return vector of [contact_item()]'s for usage in header dropdown.
#' @export
contacts <- function() {
  c(contact_item("Jimmy Briggs",
                 "Consultant",
                 "404-239-6402",
                 "jimmy.briggs@oliverwyman.com"),
    contact_item("Jack Reardon",
                 "Consultant",
                 "415-743-7821",
                 "jack.reardon@oliverwyman.com"),
    contact_item("Adam Lewis",
                 "Principal",
                 "404-239-6428",
                 "adam.lewis@oliverwyman.com"),
    contact_item("Brian Settle",
                 "Principal",
                 "404-239-6407",
                 "brian.settle@oliverwyman.com"))
}

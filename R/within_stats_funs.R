#' Returns p-value from a binomial test
#' @param k number of successes
#' @param n total number
#' @param digits number of significant digits of p-value
#' @param alternative to pass to binom.test(); "two.sided", "greater", or "less"
get_binom_p_value <- function(k, n, digits = 4, alternative = "two.sided") {
  binom.test(k, n, alternative = alternative)$p.value %>%
    round(digits)
}

#' Helper function that formats data for binomial tests
#' @param data formatted data from exp 1, 2, or 3
#' @param variable variable to test
#' @param variable_option value of variable to test in terms of
format_for_within_stats <- function(data, variable, variable_option) {
  variable = enquo(variable)

  data %>%
    mutate(condition = fct_recode(as.factor(condition),
                                  bt = "Broken Toy",
                                  bb = "Broken Button")) %>% #this makes it easier to access variables later -- no longer have a space
    group_by(condition, !!variable) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    filter(!!variable == variable_option) %>%
    unite("stat", condition, !!variable) %>%
    spread(stat, n)
}

#
within_condition_stats <- function(data, variable, variable_option, digits = 4) {
  total_bt <- data %>% filter(condition == "Broken Toy") %>% nrow()
  total_bb <- data %>% filter(condition == "Broken Button") %>% nrow()

  variable = enquo(variable)

  data %>%
    format_for_within_stats(variable = !!variable,
                            variable_option = variable_option) %>%
    rename_all(funs(str_c("num_", str_extract(., "[a-z][a-z]")))) %>%
    mutate(num_total = num_bb + num_bt,
           perc_total = num_total / (total_bt + total_bb) %>% round(0),
           p_value_bt = get_binom_p_value(num_bt, total_bt, digits = digits),
           p_value_bb = get_binom_p_value(num_bb, total_bb, digits = digits))
}

within_condition_helpful <- function(data, digits = 4) {
  within_condition_stats(data, helpfulCategory, "Helpful", digits = digits)
}

within_condition_response <- function(data, digits = 4) {
  within_condition_stats(data, firstChoice, "Other toy", digits = digits)
}

within_condition_flip <- function(data, digits = 4) {
  within_condition_stats(data, flip, "TRUE", digits = digits)
}

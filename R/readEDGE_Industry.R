#' Read EDGE Industry Data
#' 
#' Read EDGE Industry data

#' @param subtype One of
#'   - `projections_VA_iso3c` for the projections of Value Added from the 
#'     industry sector
#'   - `steel_production_scenarios` for the projections of primary and 
#'     secondary steel production
#'   - `cement_chemicals_otherInd_production_scenarios` for the projections of
#'     cement, chemicals, and other industry production
#' 
#' @return A \code{\link[magclass]{magclass}} object.
#' 
#' @author Michaja Pehl
#' 
#' @seealso \code{\link{readSource}}
#' 
#' @importFrom dplyr %>% filter
#' @importFrom readr read_rds read_csv col_character col_integer col_double
#' @importFrom rlang is_empty

readEDGE_Industry <- function(subtype) {
  
  # list all available subtypes with functions doing all the work
  switchboard <- list(
    projections_VA_iso3c = function() {
      # this is just a dummy for getting SDP trajectories although the new 
      # industry subsector isn't done yet
      read_rds('projections_VA_iso3c.rds') %>% 
        # conversion to please the all-mighty god of magpie
        mutate(!!sym('year') := factor(!!sym('year'))) %>% 
        as.magpie() %>% 
        return()
    },
    
    steel_production_scenarios = function() {
      read_csv('EDGE_Industry__steel_production_scenarios.csv.gz',
               col_names = TRUE,
               col_types = cols(
                 scenario = col_character(),
                 iso3c    = col_character(),
                 year     = col_integer(),
                 variable = col_character(),
                 value    = col_double()
               ),
               comment = '#') %>% 
        filter(!!sym('variable') %in% c('primary.production', 
                                        'secondary.production')) %>% 
        as.magpie()
    },
    
    cement_chemicals_otherInd_production_scenarios = function() {
      read_csv(paste0('EDGE_Industry__cement_chemicals_otherInd_production_',
                      'scenarios.csv.gz'),
               col_names = TRUE,
               col_types = cols(
                 iso3c    = col_character(),
                 scenario = col_character(),
                 variable = col_character(),
                 year     = col_integer(),
                 value    = col_double()),
               comment = '#') %>% 
        rename(!!sym('item') := !!sym('variable')) %>% 
        as.magpie()
    },
    
    NULL
  )
  
  # check if the subtype called is available
  if (is_empty(intersect(subtype, names(switchboard)))) {
    stop(paste('Invalid subtype -- supported subtypes are:', 
               names(switchboard)))
  } else {
    # load data and do whatever
    return(switchboard[[subtype]]())
  }
}

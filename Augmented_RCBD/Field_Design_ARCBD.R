library(tidyverse)

##This code is designed to setup an augmented randomized complete block design.
## Complete blocks are your checks set up as the width of your field
## Incomplete blocks are your lines that you are evaluating

####Paramters to fill in ######
##These are the parameters we need to build our field design
number_lines <- 309
##Enter your genotype info file. This should have one column with Genotype as a column name
##and it contains the names of the lines you are planting. DO not include checks.
genotype_info <- read_tsv("Genotype_Info.txt")

number_checks <- 3
check_names <- c("B73","PHW30","Mo17")

##this represents the number of passes your field width is set to.
##The code it setup for a 4-row planter, so our field size is always in multiple of 4
field_width <- 32


####Calculated field parameters ######
##Based on information before, this determines how many ranges or the length of the
##field
field_length <- if ((number_lines %% (field_width - number_checks)) == 0) {
  (number_lines %/% (field_width - number_checks))
} else {
  (number_lines %/% (field_width - number_checks)) + 1
}

total_plots <- (number_checks * field_length) + number_lines


#####Field Design Function########
field_design <- function(field_length, 
                         field_width, 
                         number_lines, 
                         number_checks,
                         genotype_info,
                         check_names) {
  field_length <- field_length
  field_width <- field_width
  number_lines <- number_lines
  number_checks <- number_checks
  genotype_infor <- genotype_info
  check_names <- c("Range",check_names)

  field_layout <- NULL
  for(i in 1:field_length) {
    if(i != field_length) {
      field_layout <- rbind(field_layout,
                            c(i,sample(1:field_width,
                                       replace = FALSE,
                                       size = number_checks)))
    } else {
      field_layout <- rbind(field_layout,
                            c(i,sample(1:(total_plots - ((field_length - 1) * field_width)),
                                       size = number_checks,
                                       replace = FALSE)))
    }
  }
  
  colnames(field_layout) <- c("Range","B73","Mo17","PHW30")
  
  all_field <- field_layout %>% 
    as.tibble() %>% 
    gather(Genotype,Pass,-Range) %>% 
    complete(Pass = full_seq(c(1:(field_width)),1),
             Range = full_seq(c(1:field_length),1)) %>% 
    filter(!(Range == 11 &
             Pass > (total_plots - ((field_length - 1) * field_width)))) %>%
    select(Range,Pass, everything()) %>% 
    arrange(Range,Pass)
    
  
  final_field <- all_field %>% 
    filter(is.na(Genotype)) %>%
    select(-Genotype) %>%
    mutate(Gen_Num = row_number()) %>% 
    left_join(genotype_info %>% 
                mutate(Gen_Num = sample(x = 1:number_lines,
                                        size = number_lines,
                                        replace = FALSE)),
              by = "Gen_Num") %>% 
    bind_rows(all_field %>% 
                filter(!is.na(Genotype)) %>% 
                mutate(Gen_Num = 0)) %>% 
    select(-Gen_Num) %>% 
    arrange(Range,Pass)
  
  return(final_field)
}

##Design your field#####

field_design <- field_design(field_length = field_length,
                             field_width = field_width,
                             number_lines = number_lines,
                             number_checks = number_checks,
                             genotype_info = genotype_info)
write_tsv(field_design,
          path = "Field_Layout_ARCBD.txt")

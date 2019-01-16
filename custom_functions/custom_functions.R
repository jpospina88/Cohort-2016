#### Custom Functions ####

#### Review dataset ####

check_vars_by_keywords <- function(data, keyword){
  data %>% 
    select(contains(keyword)) %>% 
    names %>% 
    noquote
}

# Example
# check_vars_by_keywords(d0, c("edu"))

#### Standardize variables ####
standardize_list_vars <- function(data, vector){
  # this function requires a vector of the variables that you want to standardize AND an ID vector with your participants' ID to merge them back with the full dataset
  fact <- data %>% select(!!!vector)
  
  fact_z <- fact %>% mutate_all(funs(scale(.)))
  
  colnames(fact_z) <- paste(colnames(fact_z), "z", sep = "_")
  
  fact_z_id <- bind_cols(id, fact_z)
}

#### Turn Factors into Simple Contrasts ####
simpleContrast <- function(data, var, numLevels) {
  numCols <- numLevels - 1
  numCells <- numLevels * numCols
  c <- contr.treatment(numLevels)
  my.coding <- matrix(rep(1/numLevels, numCells), ncol = numLevels-1)
  my.simple <- c - my.coding
  contrasts(data[,var]) <- my.simple
  data
}

#### Move variables in the dataset ####

moveme <- function(data, tomove, where = "last", ba = NULL) {
  temp <- setdiff(names(data), tomove)
  x <- switch(
    where,
    first = data[c(tomove, temp)],
    last = data[c(temp, tomove)],
    before = {
      if (is.null(ba)) stop("must specify ba column")
      if (length(ba) > 1) stop("ba must be a single character string")
      data[append(temp, values = tomove, after = (match(ba, temp)-1))]
    },
    after = {
      if (is.null(ba)) stop("must specify ba column")
      if (length(ba) > 1) stop("ba must be a single character string")
      data[append(temp, values = tomove, after = (match(ba, temp)))]
    })
  x
}

# df <- moveme(df, c("b", "c"))
# df <- moveme(df, c("b", "c"), "first")
# df <- moveme(df, c("b", "c"), "before", "e") # will move b and c before e.
# df <- moveme(df, c("b", "c"), "after", "e")

#### Compute composite variable ####

cv_compute <- function(data, cv_name, cv_vector){
  cv_name <- enquo(cv_name)
  data %>% 
    rowwise() %>% 
    mutate(!!quo_name(cv_name) := mean(c(!!!cv_vector), na.rm = TRUE)) %>% 
    ungroup()
}

#### Compute scale reliability ####

alphatize <- function(data, vector){
  temp <- 
    data %>% 
    select(!!!vector) %>% # note that variables in ... must be pre-quoted using quos() function
    na.omit() %>% 
    psych::alpha() 
  
  temp$total$raw_alpha %>% broman::myround(2)
  # Example: 
  # var_list <- quos(disp, wt, cyl)
  # new_alpha <- alphatize(mtcars, var_list)
}

alphatize_2 <- function(data, vector){
  data %>% 
    select(!!!vector) %>% # note that variables in ... must be pre-quoted using quos() function
    na.omit() %>% 
    psych::alpha() 
}

#### Count number of words ####
nwords <- function(string, pseudo = F){
  ifelse( pseudo, 
          pattern <- "\\S+", 
          pattern <- "[[:alpha:]]+" 
  )
  str_count(string, pattern)
}

# Example
# nwords("one,   two three 4,,,, 5 6")
# nwords("one,   two three 4,,,, 5 6", pseudo = T)

#### Get Deographics ####

demographics_pct <- function(data, factors){
  data %>% 
    count(!!!factors) %>% 
    mutate(total = sum(n, na.rm = F),
           pct = round(n/total*100, 1)) %>% 
    kable_format
}

# Example
# demogs <- quos(t1ses, ses, ses_working_lowmiddle, t1ses_workinglowermiddleclass)
# d0 %>% 
#   # filter(!is.na(race_nointl_wPI_USETHIS)) %>% 
#   demographics_pct(demogs)

#### Summary Statistics Overall ####
summary_stats_overall <- function(data, variables){
  data %>% 
    select(!!!variables) %>% 
    describe %>% 
    as.data.frame() %>% 
    rownames_to_column(var = "dv_name") %>% 
    mutate_at(vars(-dv_name), funs(round(., digits = 2)))
}
# Example
# summary_stats_overall(df, vars_cont)

#### Summary Statistics by Factors ####
summary_stats_by_factors <- function(data, factors, variables){
  data %>% 
    select(!!!variables) %>% 
    describeBy(factors, mat = TRUE, digits = 2)
}

# Example
# treat_adv <- list(d$treat_sc, d$disadv_sc)
# summary_stats_by_factors(d, treat_adv, vars_count)

#### Ranges by Groups ####

range_demog <- function(data, group1, group2, new_names, old_names){
  data %>% 
    select(!!!new_names) %>% 
    rename_at(vars(old_names), ~ new_names) %>% 
    describeBy(
      list(group1, group2), 
      mat = TRUE, 
      digits = 2
    ) %>% 
    group_by(group1, group2) %>% 
    summarise(
      min = min(n),
      max = max(n)
    ) %>% 
    unite(range, c("min", "max"), sep = " - ") %>% 
    spread(key = group1, value = range) %>% 
    kable_format
}

# Example:
# range_demog(d, d$cond_sc, d$adv_minority_sc, vars_inst, vars_dv)

#### Round Summary Tables to Any Decimal ####
round_table <- function(data, excluded_vars, decimal){
  data %>% 
    mutate_at(vars(-c(!!!excluded_vars)), 
              funs(round(., digits = decimal))) 
}

# Example
# data %>% 
#   round_table(quos(dv_name), 2) %>% 
#   kable_format

#### Format Tables with Kable ####
kable_format <- . %>% 
  kable(format = "html") %>% 
  kable_styling(bootstrap_options = c("hover", "responsive"), font_size = 12, full_width = F)

#### Correlation table ####
corr.p.all <- function(x){ 
  require(Hmisc) 
  x <- as.matrix(x) 
  R <- rcorr(x)$r 
  p <- rcorr(x)$P
  n <- rcorr(x)$n
  
  ## define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .001, " ***",
                    ifelse(p < .01, " **",
                           ifelse(p < .05, " *", # significant
                                  ifelse(p < 0.1, " †", # marginal
                                         ifelse(p < 0.15, " .", " "))))) # trending
  
  ## trunctuate the matrix that holds the correlations to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1] 
  
  ## add number of observations in parenthesis
  n.p <- n %>% as.data.frame() %>% 
    mutate_all(funs(paste0("(", . ,")"))) %>% 
    as.matrix()
  
  ## build a new matrix that includes the correlations with their apropriate stars 
  Rnew <- matrix(paste(R, n.p, round(p, 2), mystars, sep=" "), ncol=ncol(x)) 
  diag(Rnew) <- paste(diag(R), " ", sep="") 
  rownames(Rnew) <- colnames(x) 
  colnames(Rnew) <- paste(colnames(x), "", sep="") 
  
  ## remove upper triangle
  Rnew <- as.matrix(Rnew)
  Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
  Rnew <- as.data.frame(Rnew) 
  
  ## remove last column and return the matrix (which is now a data frame)
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  return(Rnew) 
}

corr_table_p <- function(df, vars){
  df %>% # then
    select(!!!vars) %>%
    corr.p.all() %>% 
    rownames_to_column(., var = "var") %>% 
    # mutate(var = new_row_names(.$var)) %>% 
    slice(-1) %>% 
    kable_format
}


#### Correlation Plot ####

corr_plot <- function(data, vector){
  fact <- data %>% select(!!!vector)
  
  corr_table <- round(cor(fact, use = "pairwise.complete.obs"), digits = 3) 
  
  corrplot(corr_table, method = "circle") # plot matrix
}

# Example
# corr_plot(d0, vars_purp)

#### Factor Analysis ####
factor_analysis <- function(data, vector, nfactors){
  fact <- data %>% select(!!!vector)
  
  corr_table <- round(cor(fact, use = "pairwise.complete.obs"), digits = 3) # you need to determine how NAs should be treated. SPSS uses deleting NAs pairwise as a default, so R should do the same in order to compare the results
  
  scree_plot <- scree(corr_table, factors = TRUE, pc = FALSE, main = "Scree plot", hline = NULL, add = FALSE); scree_plot # pc: principal components, pc is used in Confirmatory Factor Analysis (CFA), instead of Exploratory Factor Analysis (EFA)
  # if the Eigen value is above the cutoff in the scree_plot, then you have a factor!
  
  # Begin Factor Analysis
  # dim(fact); str(fact)
  
  # when reviewing the output of factanal, the Factor of each item should be > .6 to consider these items as part of the composite that we want to compute.
  factanal(na.omit(fact), 
           factors = nfactors, # how many factors to calculate
           rotation = "varimax") # "varimax" uncorrelate the items by computing an orthogonal vector with all the items
}

# Example
# factor_analysis(d0, var_purp, 2)
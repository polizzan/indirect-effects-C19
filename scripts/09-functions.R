#############
# FUNCTIONS #
#############

## piecewise-exponential life table from all-cause mortality rates
life.table <-
  function(df, radix = 1){
    
    df %>%
      mutate(
        
        age = age,
        nx = c(diff(age), Inf),
        mx = mx,
        px = ifelse(age == max(age), 0, exp(-nx * mx)), 
        lx = head(cumprod(c(radix, px)), -1), 
        dx = c(-diff(lx), tail(lx, 1)), 
        Lx = ifelse(age == max(age), lx / mx, ifelse(mx == 0, lx * nx, dx / mx)), 
        Tx = rev(cumsum(rev(Lx))),
        ex = Tx / lx,
        .keep = "none"
        
      )
    
  }

## piecewise exponential life expectancy from all-cause mortality rates
life.expectancy.mx <-
  function(x, mx, radix = 1, cond_age = 0){
    
    nx <- c(diff(x), Inf) 
    px <- ifelse(x == max(x), 0, exp(-nx * mx))
    lx <- head(cumprod(c(radix, px)), -1)
    dx <- c(-diff(lx), tail(lx, 1))
    Lx <- ifelse(x == max(x), lx / mx, ifelse(mx == 0, lx * nx, dx / mx))
    Tx <- rev(cumsum(rev(Lx)))
    ex <- Tx / lx
    
    return(ex[x == cond_age])
    
  }


## life expectancy from cause-specific mortality rates
life.expectancy.cod <-
  function(x, mx.cod, radix = 1, cond_age = 0){ 
    ## mx.cod = vector of age- and cause-specific mortality rates: cause 1 (all ages), cause 2 (all ages), ..., cause m (all ages)
    
    dim(mx.cod) <- c(length(x), length(mx.cod) / length(x)) 
    ## mx.cod is transformed into a matrix with: number of rows = number of age groups, & number of columns = number of causes of death
    
    mx <- rowSums(mx.cod) 
    ## all-cause mortality rates = sum of cause-specific mortality rates
    
    life.expectancy.mx(x = x, mx = mx, radix = radix, cond_age = cond_age) 
    ## life expectancy from all-cause mortality rates
    
  }

## decompose life expectancy changes over time into age-group- and cause-of-death contributions
decomp.time <-
  function(df, radix = 1, cond_age = 0){
    
    x <- sort(unique(df$age))
    years <- sort(unique(df$year))
    name <- unique(df$name)
    sex <- unique(df$sex)
    
    decomp <- 
      map(years[-1], function(y, m = df){ ## decomposition for all years (except first year)

        mx1  <- c(m$mx_cause[m$year == (y - 1)]) ## cause-specific mortality in year y-1        
        mx2  <- c(m$mx_cause[m$year == y]) ## cause-specific mortality in year y

        ## linear integral decomposition method
        hor  <- DemoDecomp::horiuchi(func = life.expectancy.cod, ## decomposition function
                                     pars1 = mx1, ## first set of cause-specific mortality rates
                                     pars2 = mx2, ## second set of cause-specific mortality rates
                                     N = 50, ## number of intervals to integrate over
                                     x = x, ## additional parameters for function life.expectancy.cod
                                     radix = radix, ## additional parameters for function life.expectancy.cod
                                     cond_age = cond_age) ## additional parameters for function life.expectancy.cod
        
        dim(hor) <- c(length(x), length(hor) / length(x)) ## transform outcome of decomposition (which is in vector format) into matrix format
        
        hor <- data.table(cbind(x, hor)) ## add age variable
        names(hor) <- c("x", levels(m$cause)) ## assign variable names
        hor <- melt(hor, id.vars = "x", variable.factor = TRUE) ## reshape 'hor' to long format
        names(hor) <- c('x','cause','contribution') ## change variable names
        
        hor$year.final <- as.numeric(y) ## add variable indicating last year of decomposition
        hor$name <- name ## add country variable
        hor$sex <- sex ## add sex variable
        
        hor
        
      })
    
    decomp.results <- 
      data.table(do.call(rbind, decomp)) ## function map returns a list, the entries of this list are combined into a data table
    
    decomp.results
    
  }

    set.seed(123) # set a seed so that people get the same 75% next time they run the same code
    id.train = sample(1:nrow(templist), nrow(templist)*.75) # nrow() gives number of rows
    id.test = setdiff(1:nrow(templist), id.train) # setdiff gives the set difference
    

df <- tibble(x = 1:3, y = 3:1)
filter(df, x == 1)

var <- "x"    
df <- tibble(x = 1:3, y = 3:1)
filter(df, var == 1)
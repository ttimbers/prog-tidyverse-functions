prog-tidyverse-functions
================

## Introduction

> **Attribution:** Derived from the [Programming with {dplyr}
> vignette](https://dplyr.tidyverse.org/articles/programming.html) by
> Hadley Wickham, Romain François, Lionel Henry, Kirill Müller, and
> RStudio.

Most {tidyverse} verbs use **tidy evaluation** in some way. Tidy
evaluation is a special type of non-standard evaluation used throughout
the {tidyverse}. There are two basic forms:

-   `arrange()`, `count()`, `filter()`, `group_by()`, `mutate()`, and
    `summarise()` use **data masking** so that you can use data
    variables as if they were variables in the environment (i.e. you
    write `my_variable` not `df$myvariable`).

-   `across()`, `relocate()`, `rename()`, `select()`, and `pull()` use
    **tidy selection** so you can easily choose variables based on their
    position, name, or type (e.g. `starts_with("x")` or `is.numeric`).

To determine whether a function argument uses data masking or tidy
selection, look at the documentation: in the arguments list, you’ll see
`<data-masking>` or `<tidy-select>`. Let’s look at a few now!

``` r
?dplyr::arrange
```

``` r
?dplyr::rename
```

Data masking and tidy selection make interactive data exploration fast
and fluid, but they add some new challenges when you attempt to use them
indirectly such as in a for loop or a function. This talk will show you
how to overcome those challenges.

> If you’d like to learn more about the underlying theory, or precisely
> how it’s different from non-standard evaluation, we recommend that you
> read the Metaprogramming chapters in [*Advanced
> R*](https://adv-r.hadley.nz).

### Setup for today

Before we get started, let’s load the tidyverse packages:

``` r
library(tidyverse)
```

### Reminder: Functions in R

And then remind ourselves, what a function looks like in R! In R, we
define and name a function via:
`variable <- function(…arguments…) { …body… }`

For example we can define a function named `add` that adds two numbers
together:

``` r
add <- function(x, y) {
  x + y
}
```

Since the function is named, we can then call it by name and pass it
arguments:

``` r
add(5, 10)
```

    ## [1] 15

This was a simple function, and seems a bit unneeded. Which it is! What
would a function look like that we might need in our data analysis?
Perhaps something like this, which groups a data frame by a
user-specified column and then calculates the mean for each group of
another user-specified column:

``` r
group_means <- function(data, grouping_column, column_to_summarize) {
  data %>% 
    group_by(grouping_column) %>% 
    summarise(column_mean = mean(column_to_summarize))
}
```

Would that function work? Could we use it to get the mean height for the
different species in the `starwars` data frame?

``` r
starwars
```

    ## # A tibble: 87 x 14
    ##    name  height  mass hair_color skin_color eye_color birth_year sex   gender
    ##    <chr>  <int> <dbl> <chr>      <chr>      <chr>          <dbl> <chr> <chr> 
    ##  1 Luke…    172    77 blond      fair       blue            19   male  mascu…
    ##  2 C-3PO    167    75 <NA>       gold       yellow         112   none  mascu…
    ##  3 R2-D2     96    32 <NA>       white, bl… red             33   none  mascu…
    ##  4 Dart…    202   136 none       white      yellow          41.9 male  mascu…
    ##  5 Leia…    150    49 brown      light      brown           19   fema… femin…
    ##  6 Owen…    178   120 brown, gr… light      blue            52   male  mascu…
    ##  7 Beru…    165    75 brown      light      blue            47   fema… femin…
    ##  8 R5-D4     97    32 <NA>       white, red red             NA   none  mascu…
    ##  9 Bigg…    183    84 black      light      brown           24   male  mascu…
    ## 10 Obi-…    182    77 auburn, w… fair       blue-gray       57   male  mascu…
    ## # … with 77 more rows, and 5 more variables: homeworld <chr>, species <chr>,
    ## #   films <list>, vehicles <list>, starships <list>

First, how we would do this without our function:

``` r
starwars %>% 
  group_by(species) %>% 
  summarise(column_mean = mean(height))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## # A tibble: 38 x 2
    ##    species   column_mean
    ##    <chr>           <dbl>
    ##  1 Aleena            79 
    ##  2 Besalisk         198 
    ##  3 Cerean           198 
    ##  4 Chagrian         196 
    ##  5 Clawdite         168 
    ##  6 Droid             NA 
    ##  7 Dug              112 
    ##  8 Ewok              88 
    ##  9 Geonosian        183 
    ## 10 Gungan           209.
    ## # … with 28 more rows

Next we’ll try our function:

``` r
group_means(starwars, species, height)
```

    ## Error: Must group by variables found in `.data`.
    ## * Column `grouping_column` is not found.

It turns out this won’t work! Why not? The unquoted column names trip R
up here! Let’s learn more about why, and then what we can do to handle
this!

## Data masking

Data masking makes data manipulation faster because it requires less
typing. In most (but not all) base R functions you need to refer to
variables with `$`, leading to code that repeats the name of the data
frame many times:

``` r
starwars[starwars$homeworld == "Naboo" & starwars$species == "Human", ]
```

The dplyr equivalent of this code is more concise because data masking
allows you to need to type `starwars` once:

``` r
starwars %>% filter(homeworld == "Naboo", species == "Human")
```

### Data- and env-variables

The key idea behind data masking is that it blurs the line between the
two different meanings of the word “variable”:

-   **env-variables** are “programming” variables that live in an
    environment. They are usually created with `<-`.

-   **data-variables** are “statistical” variables that live in a data
    frame. They usually come from data files (e.g. `.csv`, `.xls`), or
    are created manipulating existing variables.

To make those definitions a little more concrete, take this piece of
code:

``` r
df <- data.frame(x = runif(3), y = runif(3))
df$x
```

    ## [1] 0.4561071 0.2836447 0.2413149

It creates a env-variable, `df`, that contains two data-variables, `x`
and `y`. Then it extracts the data-variable `x` out of the env-variable
`df` using `$`.

I think this blurring of the meaning of “variable” is a really nice
feature for interactive data analysis because it allows you to refer to
data-vars as is, without any prefix. And this seems to be fairly
intuitive since many newer R users will attempt to write
`diamonds[x == 0 | y == 0, ]`.

Unfortunately, this benefit does not come for free. When you start to
program with these tools, you’re going to have to grapple with the
distinction!

### Indirection

The main challenge of programming with functions that use data masking
arises when you introduce some indirection, i.e. when you want to get
the data-variable from an env-variable instead of directly typing the
data-variable’s name. There are two main cases:

-   When you have the data-variable in a function argument (i.e. an
    env-variable that holds a promise[1]), you need to **embrace** the
    argument by surrounding it in doubled braces, like
    `filter(df, {{ var }})`.

    The following function uses embracing to create a wrapper around
    `summarise()` that computes the minimum and maximum values of a
    variable, as well as the number of observations that were
    summarised:

``` r
group_means <- function(data, grouping_column, column_to_summarize) {
  data %>% 
    group_by({{ grouping_column }}) %>% 
    summarise(column_mean = mean({{ column_to_summarize }}))
}
```

``` r
group_means(starwars, species, height)
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## # A tibble: 38 x 2
    ##    species   column_mean
    ##    <chr>           <dbl>
    ##  1 Aleena            79 
    ##  2 Besalisk         198 
    ##  3 Cerean           198 
    ##  4 Chagrian         196 
    ##  5 Clawdite         168 
    ##  6 Droid             NA 
    ##  7 Dug              112 
    ##  8 Ewok              88 
    ##  9 Geonosian        183 
    ## 10 Gungan           209.
    ## # … with 28 more rows

-   When you have an env-variable that is a character vector, you need
    to index into the `.data` pronoun with `[[`, like
    `summarise(df, mean = mean(.data[[var]]))`.

    The following example uses `.data` to count the number of unique
    values in each variable of `mtcars`:

    ``` r
    group_means <- function(data, grouping_column, column_to_summarize) {
      data %>% 
        group_by(.data[[grouping_column]]) %>% 
        summarise(column_mean = mean(.data[[column_to_summarize]]))
    }
    ```

    ``` r
    group_means(starwars, "species", "height")
    ```

        ## `summarise()` ungrouping output (override with `.groups` argument)

        ## # A tibble: 38 x 2
        ##    species   column_mean
        ##    <chr>           <dbl>
        ##  1 Aleena            79 
        ##  2 Besalisk         198 
        ##  3 Cerean           198 
        ##  4 Chagrian         196 
        ##  5 Clawdite         168 
        ##  6 Droid             NA 
        ##  7 Dug              112 
        ##  8 Ewok              88 
        ##  9 Geonosian        183 
        ## 10 Gungan           209.
        ## # … with 28 more rows

    Note that `.data` is not a data frame; it’s a special construct, a
    pronoun, that allows you to access the current variables either
    directly, with `.data$x` or indirectly with `.data[[var]]`. Don’t
    expect other functions to work with it.

## Tidy selection

Data masking makes it easy to compute on values within a dataset. Tidy
selection is a complementary tool that makes it easy to work with the
columns of a dataset.

### The tidyselect DSL

Underneath all functions that use tidy selection is the
[tidyselect](https://tidyselect.r-lib.org/) package. It provides a
miniature domain specific language that makes it easy to select columns
by name, position, or type. For example:

-   `select(df, 1)` selects the first column; `select(df, last_col())`
    selects the last column.

-   `select(df, c(a, b, c))` selects columns `a`, `b`, and `c`.

-   `select(df, starts_with("a"))` selects all columns whose name starts
    with “a”; `select(df, ends_with("z"))` selects all columns whose
    name ends with “z”.

-   `select(df, where(is.numeric))` selects all numeric columns.

You can see more details in `?dplyr_tidy_select`.

### Indirection

As with data masking, tidy selection makes a common task easier at the
cost of making a less common task harder. When you want to use tidy
select indirectly with the column specification stored in an
intermediate variable, you’ll need to learn some new tools. Again, there
are two forms of indirection:

-   When you have the data-variable in an env-variable that is a
    function argument, you use the same technique as data masking: you
    **embrace** the argument by surrounding it in doubled braces.

    The following function subsets a data frame using numerical row
    indexing and numbers or names for column indexing:

    ``` r
    rectangle <- function(data, columns, row_start, row_end) {
      data %>% 
        select({{ columns }}) %>% 
        slice(row_start:row_end)
    }
    ```

    ``` r
    rectangle(starwars, height:eye_color, 1, 5)
    ```

        ## # A tibble: 5 x 5
        ##   height  mass hair_color skin_color  eye_color
        ##    <int> <dbl> <chr>      <chr>       <chr>    
        ## 1    172    77 blond      fair        blue     
        ## 2    167    75 <NA>       gold        yellow   
        ## 3     96    32 <NA>       white, blue red      
        ## 4    202   136 none       white       yellow   
        ## 5    150    49 brown      light       brown

-   When you have an env-variable that is a character vector, you need
    to use `all_of()` or `any_of()` depending on whether you want the
    function to error if a variable is not found.

    The following code uses `all_of()` to select all of the variables
    found in a character vector; then `!` plus `all_of()` to select all
    of the variables *not* found in a character vector:

    ``` r
    rectangle <- function(data, columns, row_start, row_end) {
          data %>% 
            select(all_of(columns)) %>% 
            slice(row_start:row_end)
        }
    ```

    ``` r
    vars <- c("height", "mass")
    rectangle(starwars, vars, 1, 5)
    ```

        ## # A tibble: 5 x 2
        ##   height  mass
        ##    <int> <dbl>
        ## 1    172    77
        ## 2    167    75
        ## 3     96    32
        ## 4    202   136
        ## 5    150    49

## The Walrus operator

`:=` is named the “Walrus operator” and is needed in some cases when
assigning values with tidy evaluation. For example, what if we wanted to
take our `group_means` function from earlier and improved it so that the
column name in the summarized data frame was the same column name as in
the original data frame?

We might then try something like this:

``` r
group_means <- function(data, grouping_column, column_to_summarize) {
  data %>% 
    group_by({{ grouping_column }}) %>% 
    summarise({{ column_to_summarize }} = mean({{ column_to_summarize }}))
}
```

    ## Error: <text>:4:41: unexpected '='
    ## 3:     group_by({{ grouping_column }}) %>% 
    ## 4:     summarise({{ column_to_summarize }} =
    ##                                            ^

But it looks like we cannot even define that function! Anytime we want
to use indirection with a column name during assignment, we need to use
the `:=` to make tidy evaluation work correctly:

``` r
group_means <- function(data, grouping_column, column_to_summarize) {
  data %>% 
    group_by({{ grouping_column }}) %>% 
    summarise({{ column_to_summarize }} := mean({{ column_to_summarize }}))
}
```

Now we can define, and use our function!

``` r
group_means(starwars, species, height)
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## # A tibble: 38 x 2
    ##    species   height
    ##    <chr>      <dbl>
    ##  1 Aleena       79 
    ##  2 Besalisk    198 
    ##  3 Cerean      198 
    ##  4 Chagrian    196 
    ##  5 Clawdite    168 
    ##  6 Droid        NA 
    ##  7 Dug         112 
    ##  8 Ewok         88 
    ##  9 Geonosian   183 
    ## 10 Gungan      209.
    ## # … with 28 more rows

We can even go further and combine our indirected column name with a
string to improve the column name further:

``` r
group_means <- function(data, grouping_column, column_to_summarize) {
  data %>% 
    group_by({{ grouping_column }}) %>% 
    summarise("mean_{{ column_to_summarize }}" := mean({{ column_to_summarize }}))
}
```

``` r
group_means(starwars, species, height)
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## # A tibble: 38 x 2
    ##    species   mean_height
    ##    <chr>           <dbl>
    ##  1 Aleena            79 
    ##  2 Besalisk         198 
    ##  3 Cerean           198 
    ##  4 Chagrian         196 
    ##  5 Clawdite         168 
    ##  6 Droid             NA 
    ##  7 Dug              112 
    ##  8 Ewok              88 
    ##  9 Geonosian        183 
    ## 10 Gungan           209.
    ## # … with 28 more rows

## Using tidy evaluation in other {tidyverse} package functions

Finally, we can apply this same logic to other {tidyverse} package
functions (so far we have just focused on {dplyr}), such as {ggplot2}
functions!

``` r
scatter_plot <- function(data_frame, x_axis, y_axis) {
  ggplot(data_frame, aes(y = {{ y_axis }}, x = {{ x_axis }})) +
    geom_point(alpha = 0.5)
}
```

``` r
scatter_plot(starwars, height, mass)
```

    ## Warning: Removed 28 rows containing missing values (geom_point).

![](prog-tidyverse-functions_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

[1] In R, arguments are lazily evaluated which means that until you
attempt to use, they don’t hold a value, just a **promise** that
describes how to compute the value. You can learn more at
<https://adv-r.hadley.nz/functions.html#lazy-evaluation>

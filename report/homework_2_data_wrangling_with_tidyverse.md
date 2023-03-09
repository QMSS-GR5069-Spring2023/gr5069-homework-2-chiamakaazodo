------------------------------------------------------------------------

### 1. Which NBA player scored the most points in 1991?

Ricky Pierce scored the most points in 1991.

``` r
# Select the relevant columns, filter for the year of interest, then group by players so that when
# the sum of points is collected its is by player and then sort in descending order the total number of points scored 
# by the player and limit to the top 10 rows.

seasons_stats %>%
  select(Year, Player, PTS) %>%
  filter(Year == 1991)  %>%
  group_by(Player) %>%
  summarise(total_pts = sum(PTS)) %>%
  arrange(desc(total_pts)) %>%
  top_n(10)
```

    ## Selecting by total_pts

    ## # A tibble: 10 × 2
    ##    Player             total_pts
    ##    <chr>                  <dbl>
    ##  1 Ricky Pierce            3196
    ##  2 Xavier McDaniel         2746
    ##  3 Eddie Johnson           2708
    ##  4 Michael Jordan*         2580
    ##  5 Armen Gilliam           2484
    ##  6 Karl Malone*            2382
    ##  7 Patrick Ewing*          2154
    ##  8 Chris Mullin*           2107
    ##  9 David Robinson*         2101
    ## 10 Dominique Wilkins*      2101

### 2. Which player had the best free throw percentage from the year 2000 to the most recent year in the data?

Since the year 2000, Aaron Miles has had the best free throw percentage.
(When sorted alphabetically due to tied percentages)

``` r
# Select the relevant columns, filter to the year 2000 and after 
# Order the results by free throw percentage and then by player

seasons_stats %>%
  select(Year, Player, `FT%`) %>% 
  filter(Year >= 2000) %>%
  arrange(desc(`FT%`)) %>% 
  arrange((Player)) %>%
  top_n(10)
```

    ## Selecting by FT%

    ## # A tibble: 313 × 3
    ##     Year Player           `FT%`
    ##    <dbl> <chr>            <dbl>
    ##  1  2006 Aaron Miles          1
    ##  2  2003 Adam Harrington      1
    ##  3  2014 Adonis Thomas        1
    ##  4  2014 Adonis Thomas        1
    ##  5  2008 Adrian Griffin       1
    ##  6  2015 Alex Kirk            1
    ##  7  2006 Amir Johnson         1
    ##  8  2006 Andre Barrett        1
    ##  9  2009 Andre Brown          1
    ## 10  2017 Andrew Nicholson     1
    ## # … with 303 more rows

### 3. Rename the variable “Pos” to “position”.

``` r
# Use the rename function to change Pos variable. 

seasons_stats %>%
  rename(position = Pos) %>%
  top_n(10)
```

    ## Selecting by PTS

    ## # A tibble: 10 × 52
    ##     Year Player  position   Age Tm        G    GS    MP   PER `TS%` `3PAr`   FTr
    ##    <dbl> <chr>   <chr>    <dbl> <chr> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl> <dbl>
    ##  1  1961 Wilt C… C           24 PHW      79    NA  3773  27.8 0.519 NA     0.429
    ##  2  1962 Wilt C… C           25 PHW      80    NA  3882  31.7 0.536 NA     0.431
    ##  3  1963 Wilt C… C           26 SFW      80    NA  3806  31.8 0.55  NA     0.402
    ##  4  1964 Wilt C… C           27 SFW      80    NA  3689  31.6 0.537 NA     0.442
    ##  5  1967 Rick B… SF          22 SFW      78    NA  3175  24.2 0.531 NA     0.38 
    ##  6  1972 Kareem… C           24 MIL      81    NA  3583  29.9 0.603 NA     0.363
    ##  7  1975 Bob Mc… C           23 BUF      82    NA  3539  25.8 0.569 NA     0.372
    ##  8  1987 Michae… SG          23 CHI      82    82  3281  29.8 0.562  0.029 0.427
    ##  9  1988 Michae… SG          24 CHI      82    82  3311  31.7 0.603  0.027 0.43 
    ## 10  2006 Kobe B… SG          27 LAL      80    80  3277  28   0.559  0.238 0.377
    ## # … with 40 more variables: ORB% <dbl>, DRB% <dbl>, TRB% <dbl>, AST% <dbl>,
    ## #   STL% <dbl>, BLK% <dbl>, TOV% <dbl>, USG% <dbl>, blanl <lgl>, OWS <dbl>,
    ## #   DWS <dbl>, WS <dbl>, WS/48 <dbl>, blank2 <lgl>, OBPM <dbl>, DBPM <dbl>,
    ## #   BPM <dbl>, VORP <dbl>, FG <dbl>, FGA <dbl>, FG% <dbl>, 3P <dbl>, 3PA <dbl>,
    ## #   3P% <dbl>, 2P <dbl>, 2PA <dbl>, 2P% <dbl>, eFG% <dbl>, FT <dbl>, FTA <dbl>,
    ## #   FT% <dbl>, ORB <dbl>, DRB <dbl>, TRB <dbl>, AST <dbl>, STL <dbl>,
    ## #   BLK <dbl>, TOV <dbl>, PF <dbl>, PTS <dbl>

### 4. Use this variable to create two variables that are called “first_position” and “second_position”.

``` r
# Use the separate function to split position into two variables. 
# There are NAs recorded for some players in the second_position variable because they didn't have a hyphenated position

seasons_stats <- seasons_stats %>%
  rename(position = Pos) %>%
  separate(position, into = c("first_position", "second_position")) 

# Preview the edited data

seasons_stats %>%
  top_n(10)
```

    ## Selecting by PTS

    ## # A tibble: 10 × 53
    ##     Year Player     first_position second_position   Age Tm        G    GS    MP
    ##    <dbl> <chr>      <chr>          <chr>           <dbl> <chr> <dbl> <dbl> <dbl>
    ##  1  1961 Wilt Cham… C              <NA>               24 PHW      79    NA  3773
    ##  2  1962 Wilt Cham… C              <NA>               25 PHW      80    NA  3882
    ##  3  1963 Wilt Cham… C              <NA>               26 SFW      80    NA  3806
    ##  4  1964 Wilt Cham… C              <NA>               27 SFW      80    NA  3689
    ##  5  1967 Rick Barr… SF             <NA>               22 SFW      78    NA  3175
    ##  6  1972 Kareem Ab… C              <NA>               24 MIL      81    NA  3583
    ##  7  1975 Bob McAdo… C              <NA>               23 BUF      82    NA  3539
    ##  8  1987 Michael J… SG             <NA>               23 CHI      82    82  3281
    ##  9  1988 Michael J… SG             <NA>               24 CHI      82    82  3311
    ## 10  2006 Kobe Brya… SG             <NA>               27 LAL      80    80  3277
    ## # … with 44 more variables: PER <dbl>, TS% <dbl>, 3PAr <dbl>, FTr <dbl>,
    ## #   ORB% <dbl>, DRB% <dbl>, TRB% <dbl>, AST% <dbl>, STL% <dbl>, BLK% <dbl>,
    ## #   TOV% <dbl>, USG% <dbl>, blanl <lgl>, OWS <dbl>, DWS <dbl>, WS <dbl>,
    ## #   WS/48 <dbl>, blank2 <lgl>, OBPM <dbl>, DBPM <dbl>, BPM <dbl>, VORP <dbl>,
    ## #   FG <dbl>, FGA <dbl>, FG% <dbl>, 3P <dbl>, 3PA <dbl>, 3P% <dbl>, 2P <dbl>,
    ## #   2PA <dbl>, 2P% <dbl>, eFG% <dbl>, FT <dbl>, FTA <dbl>, FT% <dbl>,
    ## #   ORB <dbl>, DRB <dbl>, TRB <dbl>, AST <dbl>, STL <dbl>, BLK <dbl>, …

### 5. Unite these two variables back into a single variable called “position_united”.

``` r
seasons_stats <- seasons_stats %>%
  unite(position_united, first_position, second_position, sep = "-", na.rm = TRUE) 


# Preview the edited data

seasons_stats %>%
  top_n(10)
```

    ## Selecting by PTS

    ## # A tibble: 10 × 52
    ##     Year Player position_united   Age Tm        G    GS    MP   PER `TS%` `3PAr`
    ##    <dbl> <chr>  <chr>           <dbl> <chr> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>
    ##  1  1961 Wilt … C                  24 PHW      79    NA  3773  27.8 0.519 NA    
    ##  2  1962 Wilt … C                  25 PHW      80    NA  3882  31.7 0.536 NA    
    ##  3  1963 Wilt … C                  26 SFW      80    NA  3806  31.8 0.55  NA    
    ##  4  1964 Wilt … C                  27 SFW      80    NA  3689  31.6 0.537 NA    
    ##  5  1967 Rick … SF                 22 SFW      78    NA  3175  24.2 0.531 NA    
    ##  6  1972 Karee… C                  24 MIL      81    NA  3583  29.9 0.603 NA    
    ##  7  1975 Bob M… C                  23 BUF      82    NA  3539  25.8 0.569 NA    
    ##  8  1987 Micha… SG                 23 CHI      82    82  3281  29.8 0.562  0.029
    ##  9  1988 Micha… SG                 24 CHI      82    82  3311  31.7 0.603  0.027
    ## 10  2006 Kobe … SG                 27 LAL      80    80  3277  28   0.559  0.238
    ## # … with 41 more variables: FTr <dbl>, ORB% <dbl>, DRB% <dbl>, TRB% <dbl>,
    ## #   AST% <dbl>, STL% <dbl>, BLK% <dbl>, TOV% <dbl>, USG% <dbl>, blanl <lgl>,
    ## #   OWS <dbl>, DWS <dbl>, WS <dbl>, WS/48 <dbl>, blank2 <lgl>, OBPM <dbl>,
    ## #   DBPM <dbl>, BPM <dbl>, VORP <dbl>, FG <dbl>, FGA <dbl>, FG% <dbl>,
    ## #   3P <dbl>, 3PA <dbl>, 3P% <dbl>, 2P <dbl>, 2PA <dbl>, 2P% <dbl>, eFG% <dbl>,
    ## #   FT <dbl>, FTA <dbl>, FT% <dbl>, ORB <dbl>, DRB <dbl>, TRB <dbl>, AST <dbl>,
    ## #   STL <dbl>, BLK <dbl>, TOV <dbl>, PF <dbl>, PTS <dbl>

### 6. Create two new datasets.

``` r
# Create a new data set with all variables excluding age using the select function
# Also use select to create a new data with just player information
# Use the mutate and seq functions to create a new column that counts from 1, to the length of the data table, by 1 intervals
# Move mergeid to the front of the table using relocate for easier visibility

season_data <- seasons_stats %>%
  select(-Age) %>%
  mutate(mergeid = seq(1,nrow(seasons_stats),1)) %>%
  relocate(mergeid)

player_data <- seasons_stats %>%
  select(Year, Player, Age) %>%
  mutate(mergeid = seq(1,nrow(seasons_stats),1)) %>%
  relocate(mergeid)

# Preview the completed data sets

season_data %>%
  top_n(10)
```

    ## Selecting by PTS

    ## # A tibble: 10 × 52
    ##    mergeid  Year Player      position_united Tm        G    GS    MP   PER `TS%`
    ##      <dbl> <dbl> <chr>       <chr>           <chr> <dbl> <dbl> <dbl> <dbl> <dbl>
    ##  1    1594  1961 Wilt Chamb… C               PHW      79    NA  3773  27.8 0.519
    ##  2    1707  1962 Wilt Chamb… C               PHW      80    NA  3882  31.7 0.536
    ##  3    1828  1963 Wilt Chamb… C               SFW      80    NA  3806  31.8 0.55 
    ##  4    1963  1964 Wilt Chamb… C               SFW      80    NA  3689  31.6 0.537
    ##  5    2356  1967 Rick Barry* SF              SFW      78    NA  3175  24.2 0.531
    ##  6    3317  1972 Kareem Abd… C               MIL      81    NA  3583  29.9 0.603
    ##  7    4256  1975 Bob McAdoo* C               BUF      82    NA  3539  25.8 0.569
    ##  8    8470  1987 Michael Jo… SG              CHI      82    82  3281  29.8 0.562
    ##  9    8870  1988 Michael Jo… SG              CHI      82    82  3311  31.7 0.603
    ## 10   17743  2006 Kobe Bryant SG              LAL      80    80  3277  28   0.559
    ## # … with 42 more variables: 3PAr <dbl>, FTr <dbl>, ORB% <dbl>, DRB% <dbl>,
    ## #   TRB% <dbl>, AST% <dbl>, STL% <dbl>, BLK% <dbl>, TOV% <dbl>, USG% <dbl>,
    ## #   blanl <lgl>, OWS <dbl>, DWS <dbl>, WS <dbl>, WS/48 <dbl>, blank2 <lgl>,
    ## #   OBPM <dbl>, DBPM <dbl>, BPM <dbl>, VORP <dbl>, FG <dbl>, FGA <dbl>,
    ## #   FG% <dbl>, 3P <dbl>, 3PA <dbl>, 3P% <dbl>, 2P <dbl>, 2PA <dbl>, 2P% <dbl>,
    ## #   eFG% <dbl>, FT <dbl>, FTA <dbl>, FT% <dbl>, ORB <dbl>, DRB <dbl>,
    ## #   TRB <dbl>, AST <dbl>, STL <dbl>, BLK <dbl>, TOV <dbl>, PF <dbl>, …

``` r
player_data %>%
  top_n(10)
```

    ## Selecting by Age

    ## # A tibble: 10 × 4
    ##    mergeid  Year Player                 Age
    ##      <dbl> <dbl> <chr>                <dbl>
    ##  1    2901  1970 Bob Cousy*              41
    ##  2    9109  1989 Kareem Abdul-Jabbar*    41
    ##  3   12145  1995 Robert Parish*          41
    ##  4   12645  1996 Robert Parish*          42
    ##  5   13233  1997 Robert Parish*          43
    ##  6   17068  2004 Kevin Willis            41
    ##  7   17655  2005 Kevin Willis            42
    ##  8   18737  2007 Kevin Willis            44
    ##  9   19126  2008 Dikembe Mutombo*        41
    ## 10   19721  2009 Dikembe Mutombo*        42

### 7. Join the two datasets from question (6) together to recreate the original dataset plus the new merge id.

``` r
# Use an inner join to combine the two tables since all records should appear in both

merged_data <- player_data %>%
  inner_join(season_data, by = "mergeid")

# Preview the merged data

merged_data %>%
  top_n(10)
```

    ## Selecting by PTS

    ## # A tibble: 10 × 55
    ##    mergeid Year.x Player.x     Age Year.y Player.y   position_united Tm        G
    ##      <dbl>  <dbl> <chr>      <dbl>  <dbl> <chr>      <chr>           <chr> <dbl>
    ##  1    1594   1961 Wilt Cham…    24   1961 Wilt Cham… C               PHW      79
    ##  2    1707   1962 Wilt Cham…    25   1962 Wilt Cham… C               PHW      80
    ##  3    1828   1963 Wilt Cham…    26   1963 Wilt Cham… C               SFW      80
    ##  4    1963   1964 Wilt Cham…    27   1964 Wilt Cham… C               SFW      80
    ##  5    2356   1967 Rick Barr…    22   1967 Rick Barr… SF              SFW      78
    ##  6    3317   1972 Kareem Ab…    24   1972 Kareem Ab… C               MIL      81
    ##  7    4256   1975 Bob McAdo…    23   1975 Bob McAdo… C               BUF      82
    ##  8    8470   1987 Michael J…    23   1987 Michael J… SG              CHI      82
    ##  9    8870   1988 Michael J…    24   1988 Michael J… SG              CHI      82
    ## 10   17743   2006 Kobe Brya…    27   2006 Kobe Brya… SG              LAL      80
    ## # … with 46 more variables: GS <dbl>, MP <dbl>, PER <dbl>, TS% <dbl>,
    ## #   3PAr <dbl>, FTr <dbl>, ORB% <dbl>, DRB% <dbl>, TRB% <dbl>, AST% <dbl>,
    ## #   STL% <dbl>, BLK% <dbl>, TOV% <dbl>, USG% <dbl>, blanl <lgl>, OWS <dbl>,
    ## #   DWS <dbl>, WS <dbl>, WS/48 <dbl>, blank2 <lgl>, OBPM <dbl>, DBPM <dbl>,
    ## #   BPM <dbl>, VORP <dbl>, FG <dbl>, FGA <dbl>, FG% <dbl>, 3P <dbl>, 3PA <dbl>,
    ## #   3P% <dbl>, 2P <dbl>, 2PA <dbl>, 2P% <dbl>, eFG% <dbl>, FT <dbl>, FTA <dbl>,
    ## #   FT% <dbl>, ORB <dbl>, DRB <dbl>, TRB <dbl>, AST <dbl>, STL <dbl>, …

### 8. Subset the original dataset to 1995. Group the data by year and team name and then summarize the average number of points per team. Arrange from most to least points.

``` r
# Summarizing points scored per team for the year 1995 using dplyr functions

team_pts_95 <- seasons_stats %>%
  filter(Year == 1995) %>%
  group_by(Year, Tm) %>%
  summarise(avg_pts = mean(PTS, na.rm = TRUE)) %>%
  arrange(desc(avg_pts)) 


team_pts_95 %>%
  top_n(10)
```

    ## # A tibble: 10 × 3
    ## # Groups:   Year [1]
    ##     Year Tm    avg_pts
    ##    <dbl> <chr>   <dbl>
    ##  1  1995 SEA      647.
    ##  2  1995 ORL      606.
    ##  3  1995 PHO      605.
    ##  4  1995 DAL      604.
    ##  5  1995 MIL      582.
    ##  6  1995 UTA      582.
    ##  7  1995 MIA      553.
    ##  8  1995 SAS      546.
    ##  9  1995 IND      542.
    ## 10  1995 LAL      538.

### 9. Reshape the data in the previous question into a wide format using the `tidyr` package. Create a wide dataset that keeps year in a single column, but spreads team names to multiple individual columns with each column delineating points per team in 1995.

``` r
# Use the pivot_wider function to spread the data specifying the columns to collect the names and values from

team_pts_95_wide <- team_pts_95 %>%
  pivot_wider(names_from = Tm, values_from = avg_pts)

team_pts_95_wide %>%
  top_n(10)
```

    ## Selecting by PHI

    ## # A tibble: 1 × 29
    ## # Groups:   Year [1]
    ##    Year   SEA   ORL   PHO   DAL   MIL   UTA   MIA   SAS   IND   LAL   SAC   NJN
    ##   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
    ## 1  1995  647.  606.  605.  604.  582.  582.  553.  546.  542.  538.  537.  536.
    ## # … with 16 more variables: POR <dbl>, CHI <dbl>, CHH <dbl>, WSB <dbl>,
    ## #   NYK <dbl>, DET <dbl>, HOU <dbl>, BOS <dbl>, LAC <dbl>, CLE <dbl>,
    ## #   DEN <dbl>, TOT <dbl>, GSW <dbl>, ATL <dbl>, MIN <dbl>, PHI <dbl>

### 10. Now return the data to a long (tidy) format by moving teams back into a single column and points in a single column.

``` r
# Use the pivot_longer function to return the data back to normal
# Instead of listing all the column names, just exclude the year column from pivot_longer

team_pts_95_wide %>%
  pivot_longer(!Year, names_to = "team", values_to = "avg_pts") %>%
  top_n(10)
```

    ## Selecting by avg_pts

    ## # A tibble: 10 × 3
    ## # Groups:   Year [1]
    ##     Year team  avg_pts
    ##    <dbl> <chr>   <dbl>
    ##  1  1995 SEA      647.
    ##  2  1995 ORL      606.
    ##  3  1995 PHO      605.
    ##  4  1995 DAL      604.
    ##  5  1995 MIL      582.
    ##  6  1995 UTA      582.
    ##  7  1995 MIA      553.
    ##  8  1995 SAS      546.
    ##  9  1995 IND      542.
    ## 10  1995 LAL      538.

# homework 2 - data wrangling with the `tidyverse`

## Instructions

1. Use data logging NBA player season statistics from the `/data` folder in this repo. Variable descriptions can be found [here](https://www.kaggle.com/drgilermo/nba-players-stats/data).
2. Your final document should be an `.md` file (GitHub-flavored markdown) knitted from an R Markdown file. Create a folder called `/homework` where you will add the `.md` file, and a folder called `/src` where you will  place the `.Rmd` file and any other scripts you used to create the reports.

  In answering each of the questions for the assignment please include
  - the question as a header in your R Markdown report,
  - the raw code that you used to generate any tables, and
  - the top ten rows of the resulting `tibble`. (Do not include more than ten rows for any table in your report).

3. when you are done with your final `push` to this repo, submit the link to this repo on Canvas. (Make sure to `commit` your progress throughout the day, and `push` your progress at the end of each day.)


### Assignment items `[100 pts]`


Use `tidyverse` functions to answer the following questions:

1. `[10 pts]` __Which NBA player scored the most points in 1991?__

  _Hint: Remember that `tibble`s don't show every column when you print them. There are more column names below the ten printed rows._


2. `[10 pts]` __Which player had the best free throw percentage from the year 2000 to the most recent year in the data?__  
  If there are multiple players who have tied for the best percentage, then sort on the column with player names and select the top name in ascending order.

  _Hint: Some variable names in the `tibble` are surrounded by back ticks.  Note that when you refer to these names in your tidyverse functions you should include the back ticks._

3. `[10 pts]` __Rename the variable "pos" to "position".__

4. `[10 pts]` __Use this variable to create two variables that are called "first_position" and "second_position".__

  _Hint: separate by splitting the position variable in two._

5. `[10 pts]` __Unite these two variables back into a single variable called "position_united".__

6. `[10 pts]` __Create two new datasets.__
  - a new dataset from the original dataset that includes all data except the age variable (be sure to give this dataset a new name).  
  - a new dataset from the original dataset that includes the year, the player name, and age.  
  - add a new column to both datasets called `mergeid` that includes a sequence of numbers beginning with a 1 in the first row of the data and ending with the total number of rows in the last row of the data

  _Hint:_ `df1$mergeid<-seq(1,nrow(df1),1)`

7. `[10 pts]` __Join the two datasets from question (6) together to recreate the original dataset plus the new merge id.__

8. `[10 pts]` __Subset the original dataset to 1995.  Group the data by year and team name and then summarize the average number of points per team. Arrange from most to least points.__

9. `[10 pts]` __Reshape the data in the previous question into a wide format using the `tidyr` package.  Create a wide dataset that keeps year in a single column, but spreads team names to multiple individual columns with each column delineating points per team in 1995.__

  _Hint: you should only have one year in the resulting data._

10. `[10 pts]` __Now return the data to a long (tidy) format by moving teams back into a single column and points in a single column.__ 

  _Hint: data should include three columns: one for year, one for teams, and one for points._

#' @title one variable freq table
#' @description creating function for one variable frequency table
#' @param data using dummy_data for LFS/ APS
#' @return freq_table
#' @export

calc_tally<-function(data, levels) {
  selected_var <-factor(data, levels = levels)
  freq_table<-data.frame(table(selected_var))
  return(freq_table)
}

#' using function to create one variable data table.
calc_tally(dummy_data$gender)

#' @title two variable freq table
#' @description creating function for two variable frequency table
#' @param data using dummy_data for LFS/ APS
#' @return freq_table
#' @export

calc_tally_cross <- function(var1, levels1, var2, levels2) {
  selected_var1 <-factor(var1, levels = levels1)
  selected_var2 <-factor(var2, levels = levels2)
  freq_table <-(table(selected_var1, selected_var2))
  return(freq_table)
}

#' using function to create two variable data table.
# unique creates random label names, otherwise need to define like in first line.
calc_tally_cross(dummy_data$gender, levels1 = c("Male", "Female"),
                 dummy_data$marital_status, levels2 = unique(dummy_data$marital_status))

install.packages("plotly")

#' @title function for basic bar chart
#' @description creating function for two variable frequency table
#' @param data using dummy_data for LFS/ APS
#' @return freq_table
#' @export

#create function for bar chart
create_bar_chart <- function(freq_table){
  fig <- plotly::plot_ly(
    x = freq_table[[1]],
    y = freq_table[[2]],
    name = colnames(freq_table[1]),
    type = "bar")
  return(fig)
}

#using function to create a bar chart of marital status and gender.
create_bar_chart(freq_table_ms)
create_bar_chart(freq_table_gender)

data <- calc_tally_cross(dummy_data$Anxious, levels1 = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
                 dummy_data$furloughed, levels2 = c("Yes", "No"))

install.packages("magrittr")
library("magrittr")

#creating grouped bar chart using plotly.
Anxious <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10")
Yes <- c("51", "39", "43", "45", "55", "38", "45", "51", "38", "44", "48")
No <- c("55", "44", "48", "43", "55", "36", "49", "52", "37", "45", "39")
data <- data.frame(Anxious, Yes, No)
fig <- plotly::plot_ly(data, x= ~Anxious, y= ~Yes, base=1, type = 'bar', name = 'Yes')
fig <-fig %>% plotly::add_trace (y = ~No, name = 'No')
fig <- fig %>% plotly::layout(yaxis = list(title = 'Count'), barmode = 'group')

#variable 2 has to only have 2 unique responses. Add validation check WB_var is one of the wellbeing variables and var2 has 2 or less unique responses.
create_grouped_bar <- function(data){
  wb_fig <- plotly::plot_ly(data, x= ~selected_var1, y = ~Yes, type= 'bar', name = 'Yes')
  wb_fig <- wb_fig %>% plotly::add_trace (y =~No, name = 'No')
  wb_fig <- wb_fig %>% plotly::layout(yaxis = list(title = 'Count'), barmode = 'group')
  return(wb_fig)
}

data[1]
data$selected_var1
create_grouped_bar(data)

dummy_data$gender[1]
dummy_data$gender[[1]]

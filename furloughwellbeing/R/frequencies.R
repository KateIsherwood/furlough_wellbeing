#create a frequency table for gender.

calc_freqs_coding<-function(data)
data$gender <-factor(dummy_data$gender, levels = c("Male", "Female"))
freq_table <-data.frame(table(dummy_data$gender))
colnames(freq_table) <-c("gender", "count")



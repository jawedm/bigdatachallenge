library (ggplot2)


paste("Today is", date())
age = 15
sprintf("Age is %d",age)
election <- data.frame("Candidates" = c("John Tory", "Doug Ford", "Olivia Chow"),"Cand.first" = c("John", "Doug", "Olivia"), "Cand.last" = c("Tory", "Ford", "Chow"), "Votes" = c(395124, 331006, 227003))
election <- election[,c(-2,-3)]
str(election)
class(election)
print(election)

plot(election$Candidates, election$Votes)


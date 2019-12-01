if (!is.element("plotly", installed.packages()[,"Package"]))
  install.packages("plotly", repos="http://cran.us.r-project.org", dependencies = TRUE)
library(plotly)


movie<-read.csv("/Users/xinyu/Git/movie_analysis_1986/movies.csv")
View(movie)

#the number of movies in 1986
length(movie$name)

#the issue companies
##the kinds of companies with the issuing numeber of movies of each company
movie_comp<-table(movie$company)
movie_comp
###how many companies
length(names(movie_comp))
###companies which issuing numbers are in top 10
top.movie_comp<-sort(movie_comp,decreasing = TRUE)
top.10.company<-head(top.movie_comp,n=10)
###the countries of these companies
movie$country[top.10.company]
####USA
which(movie$country[top.10.company]=="USA")
top.10.compnay_USA<-top.10.company[which(movie$country[top.10.company]=="USA")]
top.10.compnay_USA
####UK
which(movie$country[top.10.company]=="UK")
top.10.compnay_UK<-top.10.company[which(movie$country[top.10.company]=="UK")]
top.10.compnay_UK
####Canada
which(movie$country[top.10.company]=="UK")
top.10.compnay_Canada<-top.10.company[which(movie$country[top.10.company]=="Canada")]
top.10.compnay_Canada
####The bar of top 10 companies
distr.top.10.company<-plot_ly(y=as.numeric(top.10.compnay_USA), x=names(top.10.compnay_USA),type = "bar", 
           marker = list(color = 'lightblue'),name = "USA") 
distr.top.10.company<-add_trace(distr.top.10.company,y=as.numeric(top.10.compnay_UK), x=names(top.10.compnay_UK),type = "bar", 
          marker = list(color = "lightpink"),name="UK")
distr.top.10.company<-add_trace(distr.top.10.company,y=as.numeric(top.10.compnay_Canada), x=names(top.10.compnay_Canada),type = "bar", 
          marker = list(color = "lightgreen"),name="Canada")
distr.top.10.company


#the buget of movies
## remove rows whose budgets are 0; options(scipen=200)
movie_budget <- movie[movie$budget != 0,]
movie_budget
##the distribution of buget
###the five numbers of movie budget and average of movie budget
fivenum(movie_budget$budget)
mean(movie_budget$budget)
###logarithmetics the data to analyze the distribution
log(movie_budget$budget,base=10)
boxplot_movie_budget <- plot_ly(x =log(movie_budget$budget,base=10), type = "box")
boxplot_movie_budget

# the country of movies
##the numbers of country
movie_count<-table(movie$country)
movie_count
length(movie_count)
##top 5 countries of issuing movies in 1986
top.movie_count<-sort(movie_count,decreasing = TRUE)
top.movie_count
top.5.movie_count<-head(top.movie_count,n=5)
top.5.movie_count
###the distribution of these 5 countries
###data of top5
data.movie_count <- data.frame(top.5.movie_count)
data.movie_count
names(data.movie_count)<-c("Country","Number of movies")
data.movie_count
####the percentage of 5 countries
count.percentage <- round(top.5.movie_count/sum(top.5.movie_count)*100)
count.percentage
data.movie_count$Percentage<-count.percentage
data.movie_count
####pie of the countries
country.top <- data.movie_count[,c("Country", "Percentage")]
country.top
class(data.movie_count)
percen.5 <- plot_ly(country.top, labels = country.top$Country, values = country.top$Percentage, type = 'pie') %>%
  layout(title = 'The Percetage of Top5 Countries')
precen.5


# director
as.data.frame(table(movie$director))

# genre
genre_count <- as.data.frame(table(movie$genre))  # data.Frame(Var1, Freq)
genre_count <- genre_count[order(-genre_count$Freq),]
genre_count
cou_top <- movie[movie$genre == genre_count[1:10,]$Var1,]
barplot(table(droplevels(cou_top$genre)), )

# gross
movie_gross <- movie[movie$gross != 0,]
hist(movie_gross$gross, breaks=20, col="lightblue", xlab="Miles Per Gallon", ylab="Budget")


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


# directors of movies
number.director<-table(movie$director)
number.director<-length(table(movie$director))
number.director


# genre
##the genres of movies
movie.genre<-table(movie$genre)
movie.genre
##five main genres of movies
###five man genres of movies table
top.5.genere<-sort(table(movie$genre),decreasing = TRUE)
top.5.genere
top.5.genere<-head(top.5.genere)
top.5.genere
###bar of five man genres
p <- plot_ly(x = names(top.5.genere), y = as.numeric(top.5.genere), type = 'bar',
             marker = list(color = "blue")) %>%
  layout(title = "Least Used Features",
         xaxis = list(title = ""),
         yaxis = list(title = ""))
p
##percentage of each genere 
###dataframe
movie.genre<-table(movie$genre)
movie.genre
data.movie.genre<-data.frame(movie.genre)
data.movie.genre
names(data.movie.genre)<-c("Genere","Number of movies")
data.movie.genre
###percentage
genere.percentage <- round(data.movie.genre$`Number of movies`/sum(data.movie.genre$`Number of movies`)*100,digits = 2)
genere.percentage
data.movie.genre$Percentage<-genere.percentage
data.movie.genre
###pie
percen.genre <- plot_ly(data.movie.genre, labels = data.movie.genre$Genere, values = data.movie.genre$Percentage, type = 'pie') %>%
  layout(title = 'The Percetage of genere')
percen.genre


# gross of movies
## remove 0 
movie_gross <- movie[movie$gross != 0,]
movie_gross
##the distribution of gross
###the five numbers of movie budget and average of movie budget
fivenum(movie_gross$gross)
mean(movie_gross$gross)
###log the distribution
log(movie_gross$gross,base=10)
boxplot_movie_gross <- plot_ly(x =log(movie_gross$gross,base=10), type = "box")
boxplot_movie_gross

#rate of movie
##data frame of rate
movie_rate<-data.frame(table(movie$rating))
movie_rate
names(movie_rate)<-c("Rating","Numbers of rating")
movie_rate
##percentage of rate
rate.percentage <- round(movie_rate$`Numbers of rating`/sum(movie_rate$`Numbers of rating`)*100,digits = 2)
rate.percentage
movie_rate$Percentage<-rate.percentage
movie_rate
##pie
percen.rate <- plot_ly(movie_rate, labels = movie_rate$Rating, values = movie_rate$Percentage, type = 'pie') %>%
  layout(title = 'The Percetage of rating')
percen.rate

#scores of movies
##distribution of scores
###five numbers of movies
fivenum(movie$score)
mean(movie$score)
###the range of distribution
boxplot_movie_score <- plot_ly(x =movie$score, type = "box")
boxplot_movie_score
##the top scores of movies
top.score.movie<-movie$name[order(movie$score),decreasing=TRUE]
top.score.movie<-








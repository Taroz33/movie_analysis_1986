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
p<-plot_ly(y=as.numeric(top.10.compnay_USA), x=names(top.10.compnay_USA),type = "bar", 
           marker = list(color = 'rgb(8,48,107)')) 
p
p<-add_trace(p,y=as.numeric(top.10.compnay_UK), x=names(top.10.compnay_UK),type = "bar", 
          marker = list(color = "blue"))
p

p<-add_trace(p,y=as.numeric(top.10.compnay_Canada), x=names(top.10.compnay_Canada),type = "bar", 
          marker = list(color = "green"))
p
?add_trace




names(top.10.company)
as.numeric(top.10.company)
p <- plot_ly(y=as.numeric(top.10.company), x=names(top.10.company),type = "bar", 
             marker = list(color = 'rgb(58,200,225)')) 
p

p <- plot_ly(data, x = ~x, y = ~y1, type = 'bar', name = 'Primary Product', marker = list(color = 'rgb(49,130,189)')) %>%
  add_trace(y = ~y2, name = 'Secondary Product', marker = list(color = 'rgb(204,204,204)')) %>%
  layout(xaxis = list(title = "", tickangle = -45),
         yaxis = list(title = ""),
         margin = list(b = 100),
         barmode = 'group')





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

# mydata<-rnorm(50,0,30)
# mydata.cut<-cut(mydata,breaks=seq(-50,50,20))
# table(mydata.cut)
# mydata.cut

# country
country_count <- as.data.frame(table(movie$country))  # data.Frame(Var1, Freq)
country_count <- country_count[order(-country_count$Freq),]
country_count[1:10,]
cou_top <- movie[movie$country == country_count[1:10,]$Var1,]
barplot(table(droplevels(cou_top$country)), )

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


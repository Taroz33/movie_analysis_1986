movie<-read.csv("/Users/xinyu/Git/movie_analysis_1986/movies.csv")
View(movie)

#the number of movies during 3 decades
length(movie$name)

movie$company


#the issue companies
##the kinds of companies
comp<-table(movie$company)
comp
length(as.character(comp))
top.comp<-sort(comp,decreasing = TRUE)
head(top.comp,n=10)

movie$budget
which(movie$budget==0)
length(which(movie$budget==0))

# remove rows whose budgets are 0; options(scipen=200)
movie_budget <- movie[movie$budget != 0,]
hist(movie_budget$budget, breaks=20, col="lightblue", xlab="Miles Per Gallon", ylab="Budget")
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


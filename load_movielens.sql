# SQL code for creating database and loading csv files into SQLite
sqlite3 movielens.db
create table ratings(userId integer, movieId integer, rating real, time_stamp text);
.table
.separator ','

# Import data from csv files to respective tables in the database
.import data/ratings.csv ratings
select * from ratings limit 10;

create table movies(movieId integer, title text, genres text);
.import data/movies.csv movies
select * from movies limit 10;

create table tags(userId integer, movieId integer, tag text, time_stamp text);
.import data/tags.csv tags
select * from tags limit 10;

create table links(movieId integer, imdbId integer, tmdbId integer);
.import data/links.csv links
select * from links limit 10;

.tables
.exit
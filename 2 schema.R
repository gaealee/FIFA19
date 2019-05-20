library(RPostgreSQL)
require('RPostgreSQL');
require('dplyr');
require('tidyr');
drv <- dbDriver('PostgreSQL')
con <- dbConnect(drv, dbname = 'sql_fifa', host = 's19db.apan5310.com', port = 50203, user = 'postgres', password = 'rjxklxet')
# continent
sql <- "
DROP TABLE IF EXISTS continent;
CREATE TABLE continent (
continent_id varchar(6),
continent_name varchar(15),
PRIMARY KEY (continent_id)
);"
dbGetQuery(con, sql)

# country
sql <- "
DROP TABLE IF EXISTS country;
CREATE TABLE country (
country_id varchar(6),
nationality varchar(45),
continent_id varchar(6),
flag text,
PRIMARY KEY (country_id),
FOREIGN KEY (continent_id) REFERENCES continent (continent_id)
);"
dbGetQuery(con, sql)

# player_info
sql <- "
DROP TABLE IF EXISTS player_info;
CREATE TABLE player_info (
player_id varchar(10),
player_name varchar(50),
country_id varchar(6),
height numeric(5,2),
weight smallint,
PRIMARY KEY (player_id),
FOREIGN KEY (country_id) REFERENCES country (country_id)
);"
dbGetQuery(con, sql)

# foot
sql <- "
DROP TABLE IF EXISTS foot;
CREATE TABLE foot (
player_id varchar(10),
preferred_foot varchar(5),
weak_foot smallint,
PRIMARY KEY (player_id),
FOREIGN KEY (player_id) REFERENCES player_info (player_id)
);"
dbGetQuery(con, sql)

# club
sql <- "
DROP TABLE IF EXISTS club;
CREATE TABLE club (
club_id varchar(6),
club_name varchar(50),
club_logo text,
coaches varchar(50),
PRIMARY KEY (club_id)
);"
dbGetQuery(con, sql)

# player_club
sql <- "
DROP TABLE IF EXISTS player_club;
CREATE TABLE player_club (
player_id varchar(10),
club_id varchar(6),
loaned_from varchar(50),
jersey_number smallint,
PRIMARY KEY (player_id),
FOREIGN KEY (player_id) REFERENCES player_info (player_id),
FOREIGN KEY (club_id) REFERENCES club (club_id)
);"
dbGetQuery(con, sql)


# player_rating
sql <- "
DROP TABLE IF EXISTS player_rating;
CREATE TABLE player_rating (
player_id varchar(10),
overall_rating int,
potential int,
PRIMARY KEY (player_id),
FOREIGN KEY (player_id) REFERENCES player_info (player_id)
);"
dbGetQuery(con, sql)

# player_value
sql <- "
DROP TABLE IF EXISTS player_value;
CREATE TABLE player_value (
player_id varchar(10),
value int,
PRIMARY KEY (player_id),
FOREIGN KEY (player_id) REFERENCES player_info (player_id)
);"
dbGetQuery(con, sql)

# contract
sql <- "
DROP TABLE IF EXISTS contract;
CREATE TABLE contract (
player_id varchar(10),
joined_date date,
club_id varchar(6),
contract_valid_until int,
release_clause int,
wage int,
PRIMARY KEY (player_id),
FOREIGN KEY (player_id) REFERENCES player_info (player_id),
FOREIGN KEY (club_id) REFERENCES club (club_id)
);"
dbGetQuery(con, sql)

# physical_skill
sql <- "
DROP TABLE IF EXISTS physical_skill;
CREATE TABLE physical_skill (
player_id varchar(10),
body_type varchar(6),
strength int,
acceleration int,
balance int,
sprint_speed int,
agility int,
reactions int,
jumping int,
shortpower int,
stamina int,
PRIMARY KEY (player_id),
FOREIGN KEY (player_id) REFERENCES player_info (player_id)
);"
dbGetQuery(con, sql)

# technique
sql <- "
DROP TABLE IF EXISTS technique;
CREATE TABLE technique (
player_id varchar(10),
ball_control int,
dribbling int,
volleys int,
curve int,
crossing int,
finishing int,
heading_accuracy int,
short_passing int,
fk_accuracy int,
long_passing int,
longshots int,
interceptions int,
positioning int,
penalities int,
marking int,
standing_tackle int,
sliding_tackle int,
gk_diving int,
gk_handling int,
gk_kicking int,
gk_positioning int,
gk_relfexes int,
PRIMARY KEY (player_id),
FOREIGN KEY (player_id) REFERENCES player_info (player_id)
);"
dbGetQuery(con, sql)

# game_intelligence
sql <- "
DROP TABLE IF EXISTS game_intelligence;
CREATE TABLE game_intelligence (
player_id varchar(10),
composure int,
vision int,
aggression int,
PRIMARY KEY (player_id),
FOREIGN KEY (player_id) REFERENCES player_info (player_id)
);"
dbGetQuery(con, sql)

# position
sql <- "
DROP TABLE IF EXISTS position;
CREATE TABLE position (
player_id varchar(10),
LS int,
ST int,
RS int,
LW int,
LF int,
CF int,
RF int,
RW int,
LAM int,
CAM int,
RAM int,
LM int,
LCM int,
CM int,
RCM int,
RM int,
LWB int,
LDM int,
CDM int,
RDM int,
RWB int,
LB int,
LCB int,
CB int,
RCB int,
RB int,
PRIMARY KEY (player_id),
FOREIGN KEY (player_id) REFERENCES player_info (player_id)
);"
dbGetQuery(con, sql)

# age
sql <- "
DROP TABLE IF EXISTS age;
CREATE TABLE age (
player_id varchar(10),
age smallint,
potential int,
PRIMARY KEY (player_id),
FOREIGN KEY (player_id) REFERENCES player_info (player_id)
);"
dbGetQuery(con, sql)

# player_position
sql <- "
DROP TABLE IF EXISTS player_position;
CREATE TABLE player_position (
player_id varchar(10),
preferred_position varchar(5),
PRIMARY KEY (player_id),
FOREIGN KEY (player_id) REFERENCES player_info (player_id)
);"
dbGetQuery(con, sql)

# international_reputation
sql <- "
DROP TABLE IF EXISTS international_reputation;
CREATE TABLE international_reputation (
player_id varchar(10),
international_reputation smallint,
PRIMARY KEY (player_id),
FOREIGN KEY (player_id) REFERENCES player_info (player_id)
);"
dbGetQuery(con, sql)

# league
sql <- "
DROP TABLE IF EXISTS league;
CREATE TABLE league (
league_name varchar(100),
club_id varchar(50),
PRIMARY KEY (league_name,club_id),
FOREIGN KEY (club_id) REFERENCES club (club_id)
);"
dbGetQuery(con, sql)

# tournament
sql <- "
DROP TABLE IF EXISTS tournament_name;
CREATE TABLE tournament_name (
tournament_name varchar(100),
year int,
first_place varchar(50),
second_place varchar(50),
third_place varchar(50),
PRIMARY KEY (tournament_name, year),
FOREIGN KEY (first_place) REFERENCES club (club_id),
FOREIGN KEY (second_place) REFERENCES club (club_id),
FOREIGN KEY (third_place) REFERENCES club (club_id)
);"
dbGetQuery(con, sql)


# seasons
#sql <- "
#DROP TABLE IF EXISTS seasons;
#CREATE TABLE seasons (
#season_id varchar(10),
#year int,
#PRIMARY KEY (season_id)
#);"
#dbGetQuery(con, sql)

# standings
sql <- "
DROP TABLE IF EXISTS standings;
CREATE TABLE standings (
season varchar(10),
league_name varchar(100),
club_id varchar(50),
ranking int,
PRIMARY KEY (season, club_id),
FOREIGN KEY (club_id) REFERENCES club (club_id)
);"
dbGetQuery(con, sql)


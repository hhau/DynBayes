library(rvest)
library(dplyr)

# somewhere in here make a lookup table for the AFL Teams / number

# the lookup table broke because footywire changed their naming convention
# uncomment below if it changes back

# team.data <- html("http://www.footywire.com/afl/footy/ft_teams")
# all.team.names <- html_text(html_nodes(x = team.data, "form a")) %>% as.matrix()

all.team.names <- matrix(c("Adelaide",
                           "Brisbane",
                           "Carlton",
                           "Collingwood",
                           "Essendon",
                           "Fremantle",
                           "GWS",
                           "Geelong",
                           "Gold Coast",
                           "Hawthorn",
                           "Melbourne",
                           "North Melbourne",
                           "Port Adelaide",
                           "Richmond",
                           "St Kilda",
                           "Sydney",
                           "West Coast",
                           "Western Bulldogs"
), nrow = 18, ncol = 1)

team.nums <- 1:18
lookup.frame <- as.data.frame(all.team.names, stringsAsFactors = FALSE)
names(lookup.frame)[names(lookup.frame)=="V1"] <- "team.names"
lookup.frame$team.number <- team.nums

lookup.table <- lookup.frame$team.number
names(lookup.table) <- lookup.frame$team.names

# now need to create data holding structures


default.url <-  "http://www.footywire.com/afl/footy/ft_match_statistics?mid="


n.bye.rounds <- 3
n.current.round <- 18 # changed, should all be done now
n.games.total <- 9*n.current.round - 3*n.bye.rounds

# These numbers are the important ones, you will need to change them,
# skip some, and some simply won't exist 
# check very carefully the url's of footywire, and ensure you avoid finals


n.games.total <- 6116 - 5550

home.score.vector.2015.rnd.12 <- matrix(0, nrow = 1, ncol = n.games.total)
away.score.vector.2015.rnd.12 <- matrix(0, nrow = 1, ncol = n.games.total)
home.team.vector.2015.rnd.12 <- matrix(0, nrow = 1, ncol = n.games.total)
away.team.vector.2015.rnd.12 <- matrix(0, nrow = 1, ncol = n.games.total)
round.vector.2015.rnd.12 <- matrix(0, nrow = 1, ncol = n.games.total)
#readibilty
home.team.name.2015.rnd.12 <- matrix(0, nrow = 1, ncol = n.games.total)
away.team.name.2015.rnd.12 <- matrix(0, nrow = 1, ncol = n.games.total)

# start including home/away goals & points
# this was a good idea past me
home.goals.2015.rnd.12 <- matrix(0, nrow = 1, ncol = n.games.total)
home.behinds.2015.rnd.12 <- matrix(0, nrow = 1, ncol = n.games.total)
away.goals.2015.rnd.12 <- matrix(0, nrow = 1, ncol = n.games.total)
away.behinds.2015.rnd.12 <- matrix(0, nrow = 1, ncol = n.games.total)

#2015.rnd.12 5964 - 6065
for (i in 5550:6116) {
  q <- i - 5549
  print(i)
  set.url      <-  paste(default.url, i, sep="")
  html.data    <-  html(set.url)
  # teams 1st is home team, 2nd is away team
  teams <- html_text(html_nodes(x = html.data, ".ylwbg a")) %>% as.matrix()
  # team strings
  home.team <- teams[1]
  away.team <- teams[2]

  home.team.name.2015.rnd.12[q] <- home.team
  away.team.name.2015.rnd.12[q] <- away.team
  # team numerical value
  home.team.number <- as.numeric(lookup.table[home.team])
  away.team.number <- as.numeric(lookup.table[away.team])

  home.team.vector.2015.rnd.12[q] <- home.team.number
  away.team.vector.2015.rnd.12[q] <- away.team.number
  # home score
  home.score <- html_text(html_nodes(x = html.data, ".ylwbg tr:nth-child(4) td:nth-child(7)")) %>% as.numeric()
  home.score.vector.2015.rnd.12[q] <- home.score

  # away score
  away.score <- html_text(html_nodes(x = html.data, ".ylwbg tr:nth-child(5) td:nth-child(6)")) %>% as.numeric()
  away.score.vector.2015.rnd.12[q] <- away.score

  # home goals and points
  home.goals.str <- html_text(html_nodes(x = html.data, ".ylwbg tr:nth-child(4) td:nth-child(6)"))
  home.goals.str.1 <- gsub(pattern = " \n", replacement = "",x = home.goals.str)
  home.goals.str.2 <- strsplit(home.goals.str.1, ".", fixed = T)

  home.goals.2015.rnd.12[q] <- as.numeric(home.goals.str.2[[1]][1])
  home.behinds.2015.rnd.12[q] <- as.numeric(home.goals.str.2[[1]][2])

  # away goals and behinds
  away.goals.str <- html_text(html_nodes(x = html.data, ".ylwbg tr:nth-child(5) td:nth-child(5)"))
  away.goals.str.1 <- gsub(pattern = " \n", replacement = "",x = away.goals.str)
  away.goals.str.2 <- strsplit(away.goals.str.1, ".", fixed = T)

  away.goals.2015.rnd.12[q] <- as.numeric(away.goals.str.2[[1]][1])
  away.behinds.2015.rnd.12[q] <- as.numeric(away.goals.str.2[[1]][2])

  # round
  # it's been a long time since i've done anything with regexp
  temp.text <- html_text(html_nodes(x = html.data, "tr:nth-child(2) .lnorm"))
  temp.frame <- as.data.frame(temp.text)
  round.string <- round<- gsub(",.*$", "", temp.frame$temp.text)
  round.number <- as.numeric(unlist(strsplit(gsub("[^0-9]", "", round.string), "")))
  if (length(round.number) > 1) {
    round.number <- 10*round.number[1] + round.number[2]
  }
  round.vector.2015.rnd.12[q] <- round.number

  Sys.sleep(runif(1,1.5,2.3)) 
  # avoid getting connection terminated due to accidentally triggering ddos detection software
}

data.2013.through.r18.2015<- as.data.frame(t(home.team.name.2015.rnd.12))
names(data.2013.through.r18.2015)[names(data.2013.through.r18.2015)=="V1"] <- "home.name"
data.2013.through.r18.2015$away.name <- t(away.team.name.2015.rnd.12)
data.2013.through.r18.2015$home.number <- t(home.team.vector.2015.rnd.12)
data.2013.through.r18.2015$away.number <- t(away.team.vector.2015.rnd.12)
data.2013.through.r18.2015$home.goals <- t(home.goals.2015.rnd.12)
data.2013.through.r18.2015$home.behinds <- t(home.behinds.2015.rnd.12)
data.2013.through.r18.2015$away.goals <- t(away.goals.2015.rnd.12)
data.2013.through.r18.2015$away.behinds <- t(away.behinds.2015.rnd.12)
data.2013.through.r18.2015$home.score <- t(home.score.vector.2015.rnd.12)
data.2013.through.r18.2015$away.score <- t(away.score.vector.2015.rnd.12)
data.2013.through.r18.2015$round <- t(round.vector.2015.rnd.12)

write.csv(data.2013.through.r18.2015, file = "data.2013.through.r18.2015.csv")


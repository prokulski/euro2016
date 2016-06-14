library(XML)
library(dplyr)


# Euro2016 Teams:
euro_teams <- data.frame(
   team = c( "Albania", "Austria", "Belgium", "Croatia", "Czechia", "England", "France", "Germany",
             "Hungary", "Iceland", "Italy", "Northern Ireland", "Poland", "Portugal", "Ireland", "Romania",
             "Russia", "Slovakia", "Spain", "Sweden", "Switzerland", "Turkey", "Ukraine", "Wales"),
   group = c("A", "F", "E", "D", "D", "B", "A", "C", "F", "F", "E", "C", "C", "F", "E", "A", "B", "B",
             "D", "E", "A", "D", "C", "B"))

# przewidywania
euro_matches <- data.frame(group = character(),
                           teama = character(), teamb = character(),
                           awins = numeric(), bwins = numeric(), draw = numeric(),
                           winner = character())

# wyniki
euro_scores <- data.frame(group = character(),
                           teama = character(), teamb = character(),
                           ascore = numeric(), bscore = numeric())



# bierzemy za stan zero dzien rozpoczęcia Euro2016
# temp - local file
u <- "europe_elo.html"

# url z tabelką
#u <- "http://www.eloratings.net/europe.html"

# wcześniej trzeba <br> zamienić na </td><td>
tables <- readHTMLTable(u)
# w trzeciej tabeli są interesujące dane
europa_elos <- as.data.frame(tables[3])
# tylko nazwy i ranking
europa_elos <- europa_elos[,3:4]
names(europa_elos) <- c("team", "elo")
# poprawnie cyferki?
europa_elos$elo <- as.numeric(as.character(europa_elos$elo))
# wywalamy NA
europa_elos <- filter(europa_elos, !is.na(elo))

# dodajemy ELO do listy drużyn z Euro2016
euro_teams <- left_join(euro_teams, europa_elos)

#pozbywamy się śmieci w zmiennych
rm(u, tables, europa_elos)

# w euro_elos bedzie zyl ranking
euro_elos <- euro_teams

################################
# FUNKCJE
################################

f <- function(Delta) {
   return (1 / (1 + 10^(Delta/400)))
}
play_match <- function(team.a, team.b) {
   # teoria:
   # f(Delta) = 1 / (1 + 10^(Delta/400))
   # P(WhiteWins) = f(eloBlack - eloWhite - eloAdvantage + eloDraw)
   # P(BlackWins) = f(eloWhite - eloBlack + eloAdvantage + eloDraw)
   # P(Draw) = 1 - P(WhiteWins) - P(BlackWins)
   # eloAdvantage indicates the advantage of playing first
   # eloAdvantage = 32.8 +/- 4
   # eloDraw indicates how likely draws are.
   # eloDraw = 97.3 +/- 2
   
   elo.a <- euro_elos$elo[which(euro_elos$team == team.a)]
   elo.b <- euro_elos$elo[which(euro_elos$team == team.b)]
   eloAdvantage <- 0 # nie dajemy forów nikomu :)
   eloDraw <- 97.3

   # wygrywa team.a
   p.a <- f(elo.b - elo.a - eloAdvantage + eloDraw)
   
   # wygrywa team.b
   p.b <- f(elo.a - elo.b + eloAdvantage + eloDraw)

   # remis
   p.d <- 1 - p.a - p.b

   # zaokrąglenia
   p.a <- round(100*p.a, 2)
   p.b <- round(100*p.b, 2)
   p.d <- round(100*p.d, 2)
   
   # które prawdopodobieństwo największe?
   if(p.a > p.b) win_team <- team.a
   if(p.b > p.a) win_team <- team.b
   if((p.d > p.a) & (p.d > p.b)) win_team <- "remis"
   
   # cat(paste0(team.a, " - ", team.b))
   # cat(paste0("\nELO:\n",
   #           "\t", team.a, " = ", elo.a, "\n",
   #           "\t", team.b, " = ", elo.b))
   # cat(paste0("\nPrawdopodobieństwo wygranej:\n" ,
   #             "\t", team.a, " - ", p.a, "%", "\n",
   #            "\t", team.b, " - ", p.b, "%"))
   # cat(paste0("\nPrawdopodobieństwo remisu: ", p.d, "%\n\n"))
   
   # jaka to grupa?
   grp <- filter(euro_elos, team == team.a) %>% select(group)
   grp <- as.character(grp[1,1])

   # i pchamy do tabeli wszystkiego
   match_summary <- data.frame(group = grp, teama = team.a, teamb = team.b, awins = p.a, bwins = p.b, draw = p.d, winner = win_team)
   return(match_summary)
}
check_rating <- function(team.a, team.b, score.a, score.b) {
   # teoria:
   # Rn = Ro + K × (W - We)
   # Rn is the new rating, Ro is the old (pre-match) rating.
   # K is the weight constant for the tournament played:
   # 60 for World Cup finals;
   # 50 for continental championship finals and major intercontinental tournaments;
   # 40 for World Cup and continental qualifiers and major tournaments;
   # 30 for all other tournaments;
   # 20 for friendly matches.
   # K is then adjusted for the goal difference in the game.
   # It is increased by half if a game is won by two goals,
   # by 3/4 if a game is won by three goals,
   # and by 3/4 + (N-3)/8 if the game is won by four or more goals, where N is the goal difference.
   # W is the result of the game (1 for a win, 0.5 for a draw, and 0 for a loss).
   # We is the expected result (win expectancy), either from the chart or the following formula:

   K <- 50
   score.diff <- score.a - score.b
   if(score.diff == 2) K <- K * 1.5
   if(score.diff == 3) K <- K * 1.75
   if(score.diff > 3) K <- K * 1.75 + (score.diff-3)/8
   
   id.a <- which(euro_elos$team == team.a)
   id.b <- which(euro_elos$team == team.b)
   oldelo.a <- euro_elos$elo[id.a]
   oldelo.b <- euro_elos$elo[id.b]
   
   # W is the result of the game (1 for a win, 0.5 for a draw, and 0 for a loss).

   # roznica w bramkach
   elo.diff <- oldelo.a - oldelo.b
   # dla Francji + 100 - bo gra w domu
   if(team.a == "France") elo.diff <- elo.diff + 100
   if(team.b == "France") elo.diff <- elo.diff + 100
   
   if(score.a > score.b) W <- 1
   if(score.a < score.b) W <- 0
   if(score.a == score.b) W <- 0.5

   # We = 1 / (10^(-dr/400) + 1)   
   We <- 1/( (10^(-elo.diff/400)) + 1)
   newelo.a <- round(oldelo.a + K*(W-We),0)

   elo_diff <- newelo.a - oldelo.a
   newelo.b <- oldelo.b - elo_diff
   # cat( paste0(team.a, ": old: ", oldelo.a, " new: ", newelo.a, "\n"))
   # cat( paste0(team.b, ": old: ", oldelo.b, " new: ", newelo.b, "\n"))
   
   # zapisz nowy rating do tabeli euro_elos
   euro_elos$elo[id.a] <<- newelo.a
   euro_elos$elo[id.b] <<- newelo.b

   # jaka to grupa?
   grp <- filter(euro_elos, team == team.a) %>% select(group)
   grp <- as.character(grp[1,1])
   
   # i pchamy do tabeli wszystkiego
   match_summary <- data.frame(group = grp, teama = team.a, teamb = team.b, ascore = score.a, bscore = score.b)
   return(match_summary)
}
calculate_groups <- function() {
   # tabele grupowe
   tabela <- select(euro_teams, group, team)
   tabela$match <- as.numeric(0)
   tabela$points <- as.numeric(0)
   tabela$goals_won <- as.numeric(0)
   tabela$goals_lost <- as.numeric(0)
   tabela$match_won <- as.numeric(0)
   tabela$match_lost <- as.numeric(0)
   tabela$match_draw <- as.numeric(0)

   # funkcja przelicza punkty w tabeli grupowej korzystając z tabeli z wynikami meczy euro_scores
   # wez kazdy mecz i daj odpowiednie punkty:
   # wygrany = 3
   # remis = 1 dla obu druzyn
   for(i in 1:nrow(euro_scores))
   {
      grupa <- as.character(euro_scores$group[i])
      team.a <- as.character(euro_scores$teama[i])
      team.b <- as.character(euro_scores$teamb[i])
      ascore <- as.numeric(euro_scores$ascore[i])
      bscore <- as.numeric(euro_scores$bscore[i])

      id.a <- which(tabela$team == team.a)
      id.b <- which(tabela$team == team.b)
      
      # rozegrany mecz
      tabela$match[id.a] <- tabela$match[id.a] + 1
      tabela$match[id.b] <- tabela$match[id.b] + 1

      # bramki zdobyte
      tabela$goals_won[id.a] <- tabela$goals_won[id.a] + ascore
      tabela$goals_won[id.b] <- tabela$goals_won[id.b] + bscore

      # bramki stracone
      tabela$goals_lost[id.a] <- tabela$goals_lost[id.a] + bscore
      tabela$goals_lost[id.b] <- tabela$goals_lost[id.b] + ascore
      
      # wygrywa A, przegrywa B
      if(ascore > bscore)
      {
         tabela$match_won[id.a] <- tabela$match_won[id.a] +1
         tabela$match_lost[id.b] <- tabela$match_lost[id.b] +1
         tabela$points[id.a] <- tabela$points[id.a] + 3
      }
      
      # przegrywa A, wygrywa B
      if(ascore < bscore)
      {
         tabela$match_lost[id.a] <- tabela$match_lost[id.a] +1
         tabela$match_won[id.b] <- tabela$match_won[id.b] +1
         tabela$points[id.b] <- tabela$points[id.b] + 3
      }

      # remis
      if(ascore == bscore)
      {
         tabela$match_draw[id.a] <- tabela$match_draw[id.a] + 1
         tabela$match_draw[id.b] <- tabela$match_draw[id.b] + 1
         tabela$points[id.a] <- tabela$points[id.a] + 1 
         tabela$points[id.b] <- tabela$points[id.b] + 1 
      }  
   }
   
   return(tabela)
}

################################
# FUNKCJE KONIEC
################################


#############################################
# lista meczy - faza grupowa - przewidywania
#############################################

# I tura rozgrywek grupowych
euro_matches <- rbind(euro_matches, play_match("France","Romania"))
euro_matches <- rbind(euro_matches, play_match("Albania","Switzerland"))
euro_matches <- rbind(euro_matches, play_match("Wales","Slovakia"))
euro_matches <- rbind(euro_matches, play_match("England","Russia"))
euro_matches <- rbind(euro_matches, play_match("Turkey","Croatia"))
euro_matches <- rbind(euro_matches, play_match("Poland","Northern Ireland"))
euro_matches <- rbind(euro_matches, play_match("Germany","Ukraine"))
euro_matches <- rbind(euro_matches, play_match("Spain","Czechia"))
euro_matches <- rbind(euro_matches, play_match("Ireland","Sweden"))
euro_matches <- rbind(euro_matches, play_match("Belgium","Italy"))
euro_matches <- rbind(euro_matches, play_match("Austria","Hungary"))
euro_matches <- rbind(euro_matches, play_match("Portugal","Iceland"))
# II tura rozgrywek grupowych
euro_matches <- rbind(euro_matches, play_match("Russia","Slovakia"))
euro_matches <- rbind(euro_matches, play_match("Romania","Switzerland"))
euro_matches <- rbind(euro_matches, play_match("France","Albania"))
euro_matches <- rbind(euro_matches, play_match("England","Wales"))
euro_matches <- rbind(euro_matches, play_match("Ukraine","Northern Ireland"))
euro_matches <- rbind(euro_matches, play_match("Germany","Poland"))
euro_matches <- rbind(euro_matches, play_match("Italy","Sweden"))
euro_matches <- rbind(euro_matches, play_match("Czechia","Croatia"))
euro_matches <- rbind(euro_matches, play_match("Spain","Turkey"))
euro_matches <- rbind(euro_matches, play_match("Belgium","Ireland"))
euro_matches <- rbind(euro_matches, play_match("Iceland","Hungary"))
euro_matches <- rbind(euro_matches, play_match("Portugal","Austria"))
# III tura rozgrywek grupowych
euro_matches <- rbind(euro_matches, play_match("Romania","Albania"))
euro_matches <- rbind(euro_matches, play_match("Switzerland","France"))
euro_matches <- rbind(euro_matches, play_match("Russia","Wales"))
euro_matches <- rbind(euro_matches, play_match("Slovakia","England"))
euro_matches <- rbind(euro_matches, play_match("Ukraine","Poland"))
euro_matches <- rbind(euro_matches, play_match("Northern Ireland","Germany"))
euro_matches <- rbind(euro_matches, play_match("Czechia","Turkey"))
euro_matches <- rbind(euro_matches, play_match("Croatia","Spain"))
euro_matches <- rbind(euro_matches, play_match("Iceland","Austria"))
euro_matches <- rbind(euro_matches, play_match("Hungary","Portugal"))
euro_matches <- rbind(euro_matches, play_match("Italy","Ireland"))
euro_matches <- rbind(euro_matches, play_match("Sweden","Belgium"))

# zapisujemy do CSV
write.table(euro_matches, "prawdopodobienstwo0.csv", dec = ",", sep =";", row.names = FALSE)


###########################################
# lista meczy - faza grupowa - wyniki
###########################################
# I tura rozgrywek grupowych
euro_scores <- rbind(euro_scores, check_rating("France","Romania", 2, 1))
euro_scores <- rbind(euro_scores, check_rating("Albania","Switzerland", 0, 1))
euro_scores <- rbind(euro_scores, check_rating("Wales","Slovakia", 2, 1))
euro_scores <- rbind(euro_scores, check_rating("England","Russia", 1, 1))
euro_scores <- rbind(euro_scores, check_rating("Turkey","Croatia", 0, 1))
euro_scores <- rbind(euro_scores, check_rating("Poland","Northern Ireland", 1, 0))
euro_scores <- rbind(euro_scores, check_rating("Germany","Ukraine", 2, 0))
euro_scores <- rbind(euro_scores, check_rating("Spain","Czechia", 1, 0))
euro_scores <- rbind(euro_scores, check_rating("Ireland","Sweden", 1, 1))
euro_scores <- rbind(euro_scores, check_rating("Belgium","Italy", 0, 2))
euro_scores <- rbind(euro_scores, check_rating("Austria","Hungary", 0, 2))

# rozgrywki do tej pory

#euro_scores <- rbind(euro_scores, check_rating("Portugal","Iceland", 0, 0))

# II tura rozgrywek grupowych
#euro_scores <- rbind(euro_scores, check_rating("Russia","Slovakia", 0, 0))
#euro_scores <- rbind(euro_scores, check_rating("Romania","Switzerland", 0, 0))
#euro_scores <- rbind(euro_scores, check_rating("France","Albania", 0, 0))
#euro_scores <- rbind(euro_scores, check_rating("England","Wales", 0, 0))
#euro_scores <- rbind(euro_scores, check_rating("Ukraine","Northern Ireland", 0, 0))
#euro_scores <- rbind(euro_scores, check_rating("Germany","Poland", 0, 0))
#euro_scores <- rbind(euro_scores, check_rating("Italy","Sweden", 0, 0))
#euro_scores <- rbind(euro_scores, check_rating("Czechia","Croatia", 0, 0))
#euro_scores <- rbind(euro_scores, check_rating("Spain","Turkey", 0, 0))
#euro_scores <- rbind(euro_scores, check_rating("Belgium","Ireland", 0, 0))
#euro_scores <- rbind(euro_scores, check_rating("Iceland","Hungary", 0, 0))
#euro_scores <- rbind(euro_scores, check_rating("Portugal","Austria", 0, 0))
# III tura rozgrywek grupowych
#euro_scores <- rbind(euro_scores, check_rating("Romania","Albania", 0, 0))
#euro_scores <- rbind(euro_scores, check_rating("Switzerland","France", 0, 0))
#euro_scores <- rbind(euro_scores, check_rating("Russia","Wales", 0, 0))
#euro_scores <- rbind(euro_scores, check_rating("Slovakia","England", 0, 0))
#euro_scores <- rbind(euro_scores, check_rating("Ukraine","Poland", 0, 0))
#euro_scores <- rbind(euro_scores, check_rating("Northern Ireland","Germany", 0, 0))
#euro_scores <- rbind(euro_scores, check_rating("Czechia","Turkey", 0, 0))
#euro_scores <- rbind(euro_scores, check_rating("Croatia","Spain", 0, 0))
#euro_scores <- rbind(euro_scores, check_rating("Iceland","Austria", 0, 0))
#euro_scores <- rbind(euro_scores, check_rating("Hungary","Portugal", 0, 0))
#euro_scores <- rbind(euro_scores, check_rating("Italy","Ireland", 0, 0))
#euro_scores <- rbind(euro_scores, check_rating("Sweden","Belgium", 0, 0))


# budujemy tabelke grupową
euro_table <- calculate_groups()
# sortujemy po grupach i punktach
euro_table <- arrange(euro_table, group, desc(points))

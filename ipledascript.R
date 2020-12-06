#Teams with most wins(TOP 10)

DT:: datatable( matches %>% group_by(winner) %>% summarise(wins=n()) %>% 
                  arrange(desc(wins)) %>% top_n(10))

#Teams with most win by runs margin(TOP 10)

max_runs_margin <- matches[matches$win_by_runs!=0,]
DT:: datatable( max_runs_margin %>% group_by(winner) %>% summarise(win_by_runs) %>% 
                  arrange(desc(win_by_runs)) %>% ungroup() %>% top_n(10))

#Teams with most win by wickets margin(TOP 10)

DT::datatable( matches %>% group_by(winner) %>% summarise(win_by_wickets) %>% 
                 arrange(desc(win_by_wickets))  %>% ungroup() %>% top_n(10))

#Most successful batsman runs scored(TOP 10)

deliveries %>% group_by(batsman) %>% summarise(runs_scored=sum(batsman_runs)) %>% 
  arrange(desc(runs_scored)) %>% top_n(10) %>% ggplot(aes(reorder(batsman,runs_scored),runs_scored,fill=batsman,label=runs_scored))+geom_bar(stat="identity")+coord_flip()+geom_text()+xlab("Batsman")+ylab("Runs Scored")+labs(fill="Top 10 Batsman")

#Most successful bowler wickets taken excluding run outs(TOP 10)

wicketstable <- deliveries[deliveries$player_dismissed!="",]
wicketstable1 <- wicketstable[wicketstable$dismissal_kind !="run out",] 
wicketstable1 %>% group_by(bowler) %>% summarise(wickets=length(player_dismissed)) %>% top_n(10)%>% 
  ggplot(aes(reorder(bowler,wickets),wickets,fill=bowler,label=wickets))+ geom_bar(stat="identity")+coord_flip()+geom_text()+xlab("Bowler")+ylab("Wickets")+labs(fill="Bowler Name")

#Most Player of the match Awards(TOP 10)

matches %>% group_by(player_of_match) %>% summarise(totalawards=n()) %>% arrange(desc(totalawards)) %>% top_n(10) %>%
  ggplot(aes(reorder(player_of_match,totalawards),totalawards,fill=player_of_match,label=totalawards))+ geom_bar(stat = "identity")+coord_flip()+geom_text()+xlab("Player")+ylab("Awards") + labs(fill="Player Name")

#Batsman with Top Strike Rate(min.500 balls faced)

deliveries %>% group_by(batsman) %>% filter(length(total_runs)>500)   %>% summarise(strikerate=mean(batsman_runs)*100) %>% arrange(desc(strikerate)) %>% top_n(10,wt=strikerate) %>%
  ggplot(aes(reorder(batsman,strikerate),strikerate,fill=batsman,label=strikerate))+geom_bar(stat="identity")+coord_flip()+geom_text()+xlab("Batsman")+ylab("Strike Rate")+labs(fill="TOP 10 STRIKE RATE")


#Does batting first or fielding first affect the match result?


loss <- matches[matches$toss_decision=="bat",]
bat_first_wins =0
bat_first_loss=0
i=0
for (i in seq(1,nrow(toss))) {
  if(as.character(toss$toss_winner[i])==as.character(toss$winner[i]))
  {
    bat_first_wins=bat_first_wins+1 #bat first wins
  }else
  {
    bat_first_loss=bat_first_loss+1 #bat first loss
  }
  
} 
toss2 <- matches[matches$toss_decision=="field",]
field_first_wins=0
field_first_loss=0
j=0

for (j in seq(1,nrow(toss2))) {
  if(as.character(toss2$toss_winner[j])==as.character(toss2$winner[j]))
  {
    field_first_wins=field_first_wins+1 #chasing wins
  }else
  {
    field_first_loss=field_first_loss+1 #chasing lose
  }
} 
if(bat_first_wins > field_first_wins)
{
  print("Batting first wins the most")
}else
{ print("Fielding first Wins The most")
}
toss3 <- data.frame("bat first or second" = c("batting first","fielding first"),"count" = c(bat_first_wins,field_first_wins))
toss3 %>% ggplot(aes(reorder(toss3$bat.first.or.second,toss3$count),toss3$count,fill=toss3$bat.first.or.second,label=toss3$count))+
  geom_bar(stat = "identity")+coord_flip() + xlab("Bat first or field first") + ylab("Wins")+labs(fill="Legend") +geom_text()


#Does HOME or AWAY matches affect the result of the match?

homeadv <- matches[matches$season != 2009,]
homeadv$date <- as.Date(homeadv$date)
homeadvfiltered <- homeadv[homeadv$date < as.Date("2014-04-16") | homeadv$date > as.Date("2014-04-30"),]
homeadvfiltered$home_team[homeadvfiltered$city=="Bangalore"] <- "Royal Challengers Bangalore"
homeadvfiltered$home_team[homeadvfiltered$city=="Chennai"]<- "Chennai Super Kings"
homeadvfiltered$home_team[homeadvfiltered$city=="Delhi"]<- "Delhi Daredevils"
homeadvfiltered$home_team[homeadvfiltered$city=="Chandigarh"]<- "Kings XI Punjab"
homeadvfiltered$home_team[homeadvfiltered$city=="Jaipur"]<- "Rajasthan Royals"
homeadvfiltered$home_team[homeadvfiltered$city=="Mumbai"]<- "Mumbai Indians"
homeadvfiltered$home_team[homeadvfiltered$city=="Kolkata"]<- "Kolkata Knight Riders"
homeadvfiltered$home_team[homeadvfiltered$city=="Kochi"]<- "Kochi Tuskers Kerala"
homeadvfiltered$home_team[homeadvfiltered$city=="Hyderabad" & homeadvfiltered$season <=2012]<- "Deccan Chargers"
homeadvfiltered$home_team[homeadvfiltered$city=="Hyderabad" & homeadvfiltered$season >2012]<- "Sunrisers Hyderabad"
homeadvfiltered$home_team[homeadvfiltered$city=="Ahmedabad"]<- "Rajasthan Royals"
homeadvfiltered$home_team[homeadvfiltered$city=="Dharamsala"]<- "Kings XI Punjab"
homeadvfiltered$home_team[homeadvfiltered$city=="Visakhapatnam" & homeadvfiltered$season== 2015]<- "Sunrisers Hyderabad"
homeadvfiltered$home_team[homeadvfiltered$city=="Ranchi" & homeadvfiltered$season== 2013]<- "Kolkata Knight Riders"
homeadvfiltered$home_team[homeadvfiltered$city=="Ranchi" & homeadvfiltered$season > 2013]<- "Chennai Super Kings"
homeadvfiltered$home_team[homeadvfiltered$city=="Rajkot" ]<- "Gujarat Lions"
homeadvfiltered$home_team[homeadvfiltered$city=="Kanpur" ]<- "Gujarat Lions"
homeadvfiltered$home_team[homeadvfiltered$city=="Raipur" ]<- "Delhi Daredevils"
homeadvfiltered$home_team[homeadvfiltered$city=="Nagpur" ]<- "Deccan Chargers"
homeadvfiltered$home_team[homeadvfiltered$city=="Indore" ]<- "Kochi Tuskers Kerala"
homeadvfiltered$home_team[homeadvfiltered$city=="Pune" & homeadvfiltered$season!= 2016]<- "Pune Warriors"
homeadvfiltered$home_team[homeadvfiltered$city=="Pune" & homeadvfiltered$season== 2016]<- "Rising Pune Supergiants"
homeadvfilteredna <- homeadvfiltered[which(!is.na(homeadvfiltered$home_team)),]
homeadvfilteredna$home_away_win <- ifelse(as.character(homeadvfilteredna$winner)==as.character(homeadvfilteredna$home_team),"HOME","AWAY")
homeadvfilteredna %>%  ggplot(aes(home_away_win,fill=home_away_win)) +geom_bar()

#Most Home and Away Wins

homeadvfilteredna %>% group_by(winner) %>% filter( home_away_win =="HOME") %>% summarise(homewin=n()) %>% arrange(desc(homewin)) %>% top_n(10) %>%
  ggplot(aes(reorder(winner,homewin),homewin,fill=winner,label=homewin))+geom_bar(stat = "identity") +
  geom_text()+coord_flip()+xlab("Home Team")+ylab("Home Wins")+labs(fill="Home Team")

homeadvfilteredna %>% group_by(winner) %>% filter( home_away_win =="AWAY") %>% summarise(awaywin=n()) %>% arrange(desc(awaywin)) %>% top_n(10) %>%
  ggplot(aes(reorder(winner,awaywin),awaywin,fill=winner,label=awaywin))+geom_bar(stat = "identity") +
  geom_text()+coord_flip()+xlab("Away Team")+ylab("Away Wins")+labs(fill="Away Team")
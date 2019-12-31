


#### Data Preprocessing  ########

#Certain entries are just blank spaces " ". These need to be marked as NaNs
baseDf = read.csv('cleanedData.csv', stringsAsFactors = FALSE, na.strings = c(""," "))

#Type conversions
baseDf$Pitcher = as.character(baseDf$Pitcher)
baseDf$Home_Team = as.character(baseDf$Home_Team)
baseDf$Away_Team = as.character(baseDf$Away_Team)

baseDf$Pitch_Release_Height = as.double(baseDf$Pitch_Release_Height)


#Missing value imputation
baseDf$Pitch_Type = na.fill(baseDf$Pitch_Type, "Undefined")


#### Col Mapping #####

colMapping = c('Date' = 'Date', 
'Time' = 'Time', 
'PA_of_Inning' = 'PA of Inning', 
'Pitch_of_PA' = 'Pitch of PA', 
'Pitcher' = 'Pitcher', 
'Pitcher_ID' = 'Pitcher ID', 
'Pitcher_Handedness' = 'Pitcher Handedness', 
'Pitcher_Team' = 'Pitcher Team', 
'Batter' = 'Batter', 
'Batter_ID' = 'Batter ID', 
'Batter_Handedness' = 'Batter Handedness', 
'Batter_Team' = 'Batter Team', 
'Pitch_Set' = 'Pitch Set', 
'Top/Bottom' = 'Top/Bottom', 
'Pitch_Type' = 'Pitch Type', 
'Pitch_Call' = 'Pitch Call', 
'Pitch_Speed' = 'Pitch Speed', 
'Pitch_Launch_V' = 'Pitch Launch V', 
'Pitch_Launch_H' = 'Pitch Launch H', 
'Pitch_Spin' = 'Pitch Spin', 
'Pitch_Spin_Axis' = 'Pitch Spin Axis', 
'Pitch_Spin_Tilt' = 'Pitch Spin Tilt', 
'Pitch_Release_Height' = 'Pitch Release Height', 
'Pitch_Release_Side' = 'Pitch Release Side', 
'Pitch_Extension' = 'Pitch Extension', 
'Pitch_Break_V' = 'Pitch Break V', 
'Pitch_Break_Ind_V' = 'Pitch Break Ind V', 
'Pitch_Break_H' = 'Pitch Break H', 
'Pitch_Strike_Zone_Height' = 'Pitch Strike Zone Height', 
'Pitch_Strike_Zone_Offset' = 'Pitch Strike Zone Offset', 
'Pitch_Zone_Speed' = 'Pitch Zone Speed', 
'Pitch_Approach_V' = 'Pitch Approach V', 
'Pitch_Approach_H' = 'Pitch Approach H', 
'Pitch_Time' = 'Pitch Time', 
'Hit_Ball_Speed' = 'Hit Ball Speed', 
'Hit_Ball_Launch_V' = 'Hit Ball Launch V', 
'Hit_Ball_Launch_H' = 'Hit Ball Launch H', 
'Hit_Carry_Distance' = 'Hit Carry Distance', 
'Hit_Ball_Flight_Time' = 'Hit Ball Flight Time', 
'pfxx' = 'pfxx', 
'pfxz' = 'pfxz', 
'x0' = 'x0', 
'y0' = 'y0', 
'z0' = 'z0', 
'vx0' = 'vx0', 
'vy0' = 'vy0', 
'vz0' = 'vz0', 
'ax' = 'ax', 
'ay' = 'ay', 
'az' = 'az', 
'Home_Team' = 'Home Team', 
'Away_Team' = 'Away Team', 
'GUID' = 'GUID', 
'px' = 'px', 
'pz' = 'pz', 
'hit' = 'hit', 
'gameID' = 'gameID')


pitchTypeScale = c("Changeup" = '#5D84BC',
                    "Curveball" = '#64B796',
                    "Four Seam Fastball" = '#BC0000',
                    "Knuckleball" = '#0094CE',
                    "Slider"  = '#6152D3',
                    "Splitter" = '#FFA630',
                    "Two Seam Fastball"= '#92C3B2',
                    "Undefined" = '#FBA7B6')



invColMapping = c()
for (i in 1:length(colMapping)){
  invColMapping[colMapping[i][[1]]] = names(colMapping)[i]
}

#### Pitcher Cols ####

pitcherCols_ = c(
  'Pitch_Speed',
  'Pitch_Launch_V',
  'Pitch_Launch_H',
  'Pitch_Spin',
  'Pitch_Spin_Axis',
  'Pitch_Spin_Tilt',
  'Pitch_Release_Height',
  'Pitch_Release_Side',
  'Pitch_Extension',
  'Pitch_Break_V',
  'Pitch_Break_Ind_V',
  'Pitch_Break_H',
  'Pitch_Strike_Zone_Height',
  'Pitch_Strike_Zone_Offset',
  'Pitch_Zone_Speed',
  'Pitch_Approach_V',
  'Pitch_Approach_H',
  'Pitch_Time',
  'Hit_Ball_Speed',
  'Hit_Ball_Launch_V',
  'Hit_Ball_Launch_H',
  'Hit_Carry_Distance',
  'Hit_Ball_Flight_Time',
  'pfxx',
  'pfxz',
  'x0',
  'y0',
  'z0',
  'vx0',
  'vy0',
  'vz0',
  'ax',
  'ay',
  'az',
  'px',
  'pz',
  'hit'
  )

pitcherCols = c()

for(i in 1:length(pitcherCols_)){
  pitcherCols = c(pitcherCols, colMapping[pitcherCols_[i] ][[1]])
}

#### Batter Cols ########
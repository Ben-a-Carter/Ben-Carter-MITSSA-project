
require(c("dplyr", "grid", "ggplot2", "gganimate", "tidyverse", "splancs", "ggpubr", "png", "sf"))

screens <- read.csv("player_play.csv") %>% 
  group_by(gameId, playId) %>%
  mutate(isScreen = ifelse(routeRan == "SCREEN", "yes", NA)) %>%
  fill(isScreen, .direction = "updown") %>%
  select(gameId, playId, isScreen) %>%
  distinct(.keep_all = T) %>%
  filter(is.na(isScreen)) %>%
  select(-isScreen) %>%
  mutate(keep = 1)
plays <- read.csv("plays.csv") %>%
  filter(qbSpike %in% c("FALSE", NA) & dropbackType %in% c("TRADITIONAL", "SCRAMBLE")) %>%
  left_join(screens) %>% filter(keep == 1)
players <- read.csv("players.csv") %>% select(-displayName)
player_play <- read.csv("player_play.csv") %>%
  select(-penaltyYards)
games <- read.csv("games.csv")
img <- readPNG('nfl field.png')

t <- read.csv("tracking_week_1.csv")

tracking <- t %>%
  left_join(plays) %>%
  left_join(player_play) %>%
  left_join(players) %>%
  ungroup() %>%
  mutate(laterals = ifelse(event == "lateral" | grepl("Flea", playDescription), 1, NA)) %>%
  group_by(gameId, playId) %>%
  fill(laterals, .direction = "updown") %>%
  filter(is.na(laterals)) %>% 
  ungroup()

yard_los <- tracking %>%
  select(gameId, playId, frameType, yardlineNumber, x, position, wasInitialPassRusher) %>%
  filter(position %in% c("C", "T", "G") & frameType == "SNAP" | wasInitialPassRusher == 1 & frameType == "SNAP") %>%
  group_by(gameId, playId, wasInitialPassRusher) %>%
  mutate(mean_x = mean(x, na.rm = T)) %>%
  ungroup %>%
  select(-c(frameType, position, wasInitialPassRusher, x)) %>%
  distinct %>%
  group_by(gameId, playId) %>%
  mutate(los = mean(mean_x, na.rm = T)) %>%
  select(-c(mean_x, yardlineNumber)) %>%
  distinct 

convex_hull <- tracking %>%
  select(-playDirection) %>%
  arrange(gameId, playId, frameId, club) %>%
  filter(!frameType == "BEFORE_SNAP") %>%
  group_by(gameId, playId, frameId, club) %>%
  mutate(mean_o = ifelse(position %in% c("C", "T", "G") & frameType == "SNAP" & possessionTeam == club, mean(x), NA), 
         mean_d = ifelse(wasInitialPassRusher == 1 & frameType == "SNAP" & possessionTeam != club, mean(x), NA)) %>%
  group_by(gameId, playId) %>%
  fill(mean_o, .direction = "updown") %>%
  fill(mean_d, .direction = "updown") %>%
  group_by(gameId, playId) %>%
  left_join(yard_los) %>%
  filter(frameType %in% c("AFTER_SNAP", "SNAP") & club != "football" & club == possessionTeam & is.na(wasRunningRoute) & position != "WR") %>%
  mutate(playDirection = ifelse(mean_o > los & frameType == "SNAP", "left", 
                                ifelse(mean_o < los & frameType == "SNAP", "right", NA))) %>%
  fill(playDirection, .direction = "down") %>%
  arrange(gameId, playId, frameId, teamAbbr, y) %>%
  ungroup() %>%
  filter() %>%
  group_by(gameId, playId, frameId) %>%
  mutate(line_order = ifelse(possessionTeam == club & !position %in% c("QB", "TE", "RB", "FB") & frameType == "SNAP", 1, 
                             ifelse(position %in% c("QB", "TE", "RB", "FB"), 0, NA)), 
         line_order = cumsum(line_order), 
         line_order = ifelse(position %in% c("QB", "TE", "RB", "FB"), NA, line_order)) %>%
  group_by(gameId, playId, nflId) %>%
  fill(line_order, .direction = "updown") %>%
  mutate(n_position = ifelse(playDirection == "left" & line_order == 1 | playDirection == "right" & line_order == 5, "LT", 
                             ifelse(playDirection == "left" & line_order == 2 | playDirection == "right" & line_order == 4, "LG",
                                    ifelse(playDirection == "left" & line_order == 4 | playDirection == "right" & line_order == 2, "RG",
                                           ifelse(playDirection == "left" & line_order == 5 | playDirection == "right" & line_order == 1, "RT",
                                                  ifelse(line_order == 3, "C", position)))))) %>%
  mutate(n_position = ifelse(position == "QB", "QB", n_position)) %>% 
  mutate(pocket_state = ifelse(event %in% c("pass_forward", "shovel_pass", "qb_sack", "run"), "concluded", NA), 
         event = ifelse(event %in% c("pass_forward", "shovel_pass", "qb_sack", "run"), event, NA)) %>%
  fill(pocket_state, .direction = "up") %>%
  filter(pocket_state == "concluded") %>%
  mutate(causedPressure = ifelse(causedPressure == "TRUE", "yes", NA)) %>%
  fill(causedPressure, .direction = "updown") %>%
  filter(possessionTeam == club) %>% 
  group_by(gameId, playId, frameId) %>%
  mutate(min_x = ifelse(playDirection == "left", max(x), min(x))) %>%
  fill(min_x, .direction = "updown") %>%
  group_by(gameId, playId, nflId) %>% 
  mutate(blocker = ifelse(n_position %in% c("RT", "LT", "C", "RG", "LG") & playDirection == "left" & x >= los |
                            n_position %in% c("RT", "LT", "C", "RG", "LG") & playDirection == "right" & x <= los, "yes", NA)) %>%
  fill(blocker, .direction = "down") %>%
  filter(!is.na(blocker) | n_position == "QB") %>%
  group_by(gameId, playId, frameId) %>% 
  mutate(mean_x = mean(x), 
         mean_y = mean(y)) %>%
  ungroup() %>%
  mutate(angle = atan2((y-mean_y), (x-mean_x))) %>%
  group_by(gameId, playId, frameId) %>% 
  arrange(gameId, playId, frameId, angle) %>%
  mutate(poly_order = row_number(),
         max_points = max(poly_order)) %>%
  filter(max_points >= 3) %>%
  mutate(tb_x_max = ifelse(n_position %in% c("T", "C", "G", "QB") | !is.na(blocker), max(x), NA),
         tb_x_min = ifelse(n_position %in% c("T", "C", "G", "QB") | !is.na(blocker), min(x), NA),
         tb_y_max = ifelse(n_position %in% c("T", "C", "G") | !is.na(blocker), max(y), NA),
         tb_y_min = ifelse(n_position %in% c("T", "C", "G") | !is.na(blocker), min(y), NA)) %>%
  fill(tb_y_max, .direction = "updown") %>%
  fill(tb_y_min, .direction = "updown") %>%
  select(-c(blockedPlayerNFLId1, blockedPlayerNFLId2, blockedPlayerNFLId3)) %>%
  ungroup()

o_line_pos <- convex_hull %>%
  filter(!is.na(line_order)) %>%
  select(gameId, playId, frameId, nflId, jerseyNumber, n_position, los, playDirection)

keep_frames <- convex_hull %>%
  select(gameId, playId, frameId) %>%
  distinct(.keep_all = T) %>%
  mutate(keep = 1) 

play_direction <- o_line_pos %>%
  ungroup() %>%
  select(gameId, playId, los, playDirection) %>%
  distinct(.keep_all = T)

misc <- tracking %>%
  left_join(games) %>%
  arrange(gameId, playId, frameId) %>%
  group_by(gameId, playId) %>%
  select(gameId, playId, nflId, yardsToGo, down, quarter, timeToThrow, preSnapHomeScore, preSnapVisitorScore, 
         possessionTeam, homeTeamAbbr, passResult, dropbackDistance, wasInitialPassRusher, getOffTimeAsPassRusher,
         pff_manZone) %>%
  distinct(.keep_all = T) %>%
  mutate(wasInitialPassRusher = sum(wasInitialPassRusher, na.rm = T), 
         meanGetOffTimeAsPassRusher = mean(getOffTimeAsPassRusher, na.rm = T),
         minGetOffTimeAsPassRusher = min(getOffTimeAsPassRusher, na.rm = T),
         maxGetOffTimeAsPassRusher = max(getOffTimeAsPassRusher, na.rm = T)) %>%
  select(-c(nflId, getOffTimeAsPassRusher)) %>%
  distinct() %>%
  left_join(play_direction) %>%
  mutate(lineToGain = ifelse(playDirection == "left", los - yardsToGo, los + yardsToGo)) %>%
  
  distinct(.keep_all = T) %>%
  mutate(score_dif = ifelse(possessionTeam == homeTeamAbbr, preSnapHomeScore - preSnapVisitorScore, preSnapVisitorScore - preSnapHomeScore)) %>%
  select(-c(preSnapHomeScore, preSnapVisitorScore, homeTeamAbbr))

distance <- tracking %>%
  select(gameId, playId, frameId, frameType, nflId, possessionTeam, club, causedPressure, x, y, position, blockedPlayerNFLId1, blockedPlayerNFLId2, blockedPlayerNFLId3, routeRan) %>%
  left_join(keep_frames) %>%
  left_join(play_direction) %>%
  arrange(gameId, playId, frameId, club) %>%
  group_by(gameId, playId) %>%
  filter(frameType %in% c("AFTER_SNAP", "SNAP") & club != "football" & keep == 1) %>%
  ungroup() %>%
  mutate(qb_x = ifelse(position == "QB" & is.na(routeRan), x, NA), 
         qb_y = ifelse(position == "QB" & is.na(routeRan), y, NA)) %>%
  group_by(gameId, playId, frameId) %>%
  fill(qb_x, .direction = "updown") %>%
  fill(qb_y, .direction = "updown") %>%
  filter(is.na(routeRan)) %>%
  mutate(dist = ifelse(position != "QB" & club == possessionTeam, round(sqrt((x - qb_x)^2 + (y - qb_y)^2), 2),
                       ifelse(club != possessionTeam, round((sqrt((x - qb_x)^2 + (y - qb_y)^2))+0.1, 2), NA)), 
         def = ifelse(dist == min(dist, na.rm = T) & possessionTeam != club, frameId, NA)) %>%
  group_by(gameId, playId) %>%
  mutate(def = ifelse(def == min(def, na.rm = T), def, NA)) %>% 
  group_by(gameId, playId, nflId) %>%
  fill(def, .direction = "up") %>%
  mutate(qb_x = ifelse(is.na(def), NA, qb_x),
         qb_y = ifelse(is.na(def), NA, qb_y)) %>%
  group_by(gameId, playId) %>%
  fill(def, .direction = "updown")

ch_distance <- distance %>%
  select(-c(position, qb_x, qb_y, def, blockedPlayerNFLId1, causedPressure, blockedPlayerNFLId2, blockedPlayerNFLId3)) %>%
  filter(possessionTeam != club)

extractRow = function(d,index){
  if (ncol(d) > 1)
  {
    return(d[index,])
  } else
  {
    d2 = as.data.frame(d[index,])
    names(d2) = names(d)
    return(d2)
  }
}

in_pocket <- data.frame()
for(i in 1:nrow(keep_frames)){
  frame <- extractRow(keep_frames, i)
  play <- tracking %>% 
    select(gameId, playId, frameId, possessionTeam, club, nflId, x, y) %>% 
    filter(possessionTeam != club & 
             club != "football" & 
             gameId %in% frame$gameId & 
             playId %in% frame$playId & 
             frameId %in% frame$frameId) %>% 
    distinct()
  pocket <- convex_hull %>% 
    filter(gameId %in% frame$gameId & 
             playId %in% frame$playId & 
             frameId %in% frame$frameId) %>%
    select(x, y) %>%
    rbind(head(.,1)) %>%
    as.matrix() %>%
    list %>%
    st_polygon %>% 
    st_sfc
  def <- play %>%
    st_as_sf(., coords = c("x","y"))
  inside <- st_filter(def, pocket) %>%
    as.data.frame() %>%
    mutate(geometry = "YES") %>%
    rename(penetration = geometry)
  in_pocket <- rbind(in_pocket, inside)
}

ch_in_pocket <- in_pocket %>%
  select(-c(possessionTeam, club)) %>%
  distinct(.keep_all = T) %>%
  right_join(ch_distance) %>%
  filter(possessionTeam != club)

position <- players %>% 
  select(nflId, position) %>%
  distinct(.keep_all = T) %>%
  rename(blockerId = nflId)

blocker_dist <- distance %>% 
  select(-c(qb_x, qb_y, x, y, los, playDirection, possessionTeam, club, position, routeRan, frameType, keep)) %>% 
  filter(!is.na(blockedPlayerNFLId1)) %>%
  rename(blocker_dist = dist, 
         blockerId = nflId) %>%
  pivot_longer(cols = -c(gameId, playId, frameId, causedPressure, def, blockerId, blocker_dist), names_to = "blockerNu", values_to = "nflId") %>%
  filter(!is.na(nflId)) %>%
  select(-blockerNu) %>%
  full_join(ch_in_pocket) %>%
  arrange(gameId, playId, frameId) %>%
  fill(penetration, .direction = "down") %>%
  rename(blockeeId = nflId) %>%
  select(-c(club, possessionTeam)) %>%
  group_by(gameId, playId, frameId, blockeeId) %>%
  mutate(multi_block = ifelse(duplicated(blockeeId), "yes", NA)) %>%
  fill(multi_block, .direction = "updown") %>%
  mutate(multi_block_filter = ifelse(multi_block == "yes", min(blocker_dist, na.rm = T), NA)) %>%
  filter(is.na(multi_block_filter) | multi_block_filter == blocker_dist) %>%
  group_by(gameId, playId, frameId) %>%
  mutate(beat_def = ifelse(dist > blocker_dist, NA, "YES")) %>%
  fill(beat_def, .direction = "down") %>%
  ungroup() %>%
  mutate(chip = ifelse(x > los & playDirection == "left", los, 
                       ifelse(x < los & playDirection == "right", los, NA))) %>%
  left_join(position) %>%
  mutate(was_chip = ifelse(!is.na(chip) & !position %in% c("T", "C", "G"), 1, NA)) %>%
  group_by(gameId, playId, blockerId) %>%
  fill(was_chip, .direction = "updown") %>%
  ungroup() %>%
  filter(is.na(was_chip)) %>%
  select(gameId, playId, frameId, penetration, def, beat_def) %>%
  group_by(gameId, playId) %>%
  fill(beat_def, .direction = "down") %>%
  distinct(.keep_all = T) %>%
  mutate(keep = ifelse(!is.na(penetration) & def > frameId & is.na(def) | 
                         !is.na(penetration) & is.na(def) & is.na(beat_def) |
                         is.na(penetration) & def <= frameId & !is.na(beat_def) |
                         !is.na(penetration) & !is.na(def), "keep", NA)) %>%
  filter(!is.na(keep)) %>%
  mutate(penetration = "YES") %>% 
  select(-keep)

d_distance <- distance %>%
  ungroup %>%
  select(gameId, playId, frameId, nflId, qb_x, qb_y) %>%
  filter(!is.na(qb_x))

pocket <- data.frame()
for(i in 1:nrow(keep_frames)){
  frame <- extractRow(keep_frames, i)
  area <- convex_hull %>% 
    mutate(gameId = as.character(gameId), playId = as.character(playId), frameId = as.character(frameId)) %>%
    select(gameId, playId, frameId, x, y, n_position, event) %>%
    filter(gameId %in% frame$gameId & playId %in% frame$playId & frameId %in% frame$frameId)
  ind_ch <- area %>%
    select(x, y) %>%
    as.matrix() %>% 
    areapl
  pocket_i <- area %>% 
    mutate(area = ind_ch, 
           qb = ifelse(n_position == "QB", "yes", NA)) 
  pocket <- bind_rows(pocket, pocket_i)
}

qb_in_pocket <- data.frame()
for(i in 1:nrow(keep_frames)){
  frame <- extractRow(keep_frames, i)
  poly <- convex_hull %>% 
    select(gameId, playId, frameId, n_position, x, y) %>% 
    filter(gameId %in% frame$gameId & 
             playId %in% frame$playId & 
             frameId %in% frame$frameId &
             n_position != "QB") %>%
    select(x, y) %>%
    rbind(head(.,1)) %>%
    as.matrix() %>%
    list %>%
    st_polygon %>% 
    st_sfc
  qb <- convex_hull %>% 
    select(gameId, playId, frameId, n_position, x, y) %>% 
    filter(gameId %in% frame$gameId & 
             playId %in% frame$playId & 
             frameId %in% frame$frameId &
             n_position == "QB") %>%
    select(x, y)
  points <- st_as_sf(qb, coords = c("x","y"), crs = NA)
  inside <- st_filter(points, poly) %>%
    as.data.frame() %>%
    mutate(geometry = "YES", 
           gameId = frame$gameId, 
           playId = frame$playId, 
           frameId = frame$frameId) %>%
    rename(in_pocket = geometry) 
  qb_in_pocket <- rbind(qb_in_pocket, inside)
}

left_pocket <- pocket %>%
  mutate(gameId = as.numeric(gameId), playId = as.numeric(playId), frameId = as.numeric(frameId)) %>%
  left_join(qb_in_pocket) %>%
  group_by(gameId, playId) %>%
  mutate(qb_in_pocket = ifelse(is.na(in_pocket) & lead(!is.na(in_pocket)) | !is.na(in_pocket), "was", NA)) %>%
  fill(qb_in_pocket, .direction = "up") %>%
  mutate(qb_left_pocket = ifelse(lag(qb_in_pocket) == "was" & is.na(qb_in_pocket), "left", NA)) %>%
  fill(qb_left_pocket, .direction = "down") %>%
  mutate(qb_in_pocket = ifelse(is.na(qb_in_pocket) & is.na(qb_left_pocket), "was", 
                               ifelse(is.na(qb_in_pocket) & !is.na(qb_left_pocket), qb_left_pocket, 
                                      ifelse(!is.na(qb_in_pocket), qb_in_pocket, NA)))) %>%
  select(-c(qb, n_position, event, x, y, in_pocket, qb_left_pocket)) %>%
  distinct(.keep_all = T)

jerseyNumber <- t %>%
  ungroup() %>%
  select(gameId, club, nflId, jerseyNumber) %>%
  distinct(.keep_all = T) %>%
  mutate(jerseyNumber = ifelse(club == "football", "+", jerseyNumber))

poly_1 <- convex_hull %>%
  mutate(jerseyNumber = as.character(jerseyNumber)) %>%
  left_join(blocker_dist) %>%
  group_by(gameId, playId) %>%
  fill(beat_def, .direction = "down") %>%
  left_join(jerseyNumber) %>%
  mutate(team = ifelse(club == possessionTeam, "offense",
                       ifelse(club == "football", "football", "defense"))) %>%
  left_join(left_pocket) %>%
  filter(is.na(penetration)) %>%  
  filter(qb_in_pocket == "was") %>%
  group_by(gameId, playId) %>%
  mutate(time = ((frameId) - (min(frameId))) * 0.1)

frames <- poly_1 %>%
  select(gameId, playId, frameId) %>%
  distinct(.keep_all = T) %>%
  mutate(failure = 1)

growth <- poly_1 %>%
  select(gameId, playId, frameId, area, time) %>%
  distinct(.keep_all = T) %>%
  mutate(failure = 1) %>%
  group_by(gameId, playId) %>%
  mutate(rate = ifelse(frameId > min(frameId), diff(area)/diff(time), NA),
         rate = ifelse(frameId == max(frameId), NA, rate),
         accel = ifelse(frameId > min(frameId), diff(rate)/diff(time), NA)) %>%
  select(area, rate, accel) %>%
  mutate(mArea = mean(area, na.rm = T),
         mRate = mean(rate, na.rm = T),
         mAccel = mean(accel, na.rm = T),
         sdArea = sd(area, na.rm = T),
         sdRate = sd(rate, na.rm = T),
         sdAccel = sd(accel, na.rm = T)) %>%
  select(-c(area, rate, accel)) %>%
  distinct(.keep_all = T)

polygon_angles <- function(coords, degrees = TRUE, reflex = TRUE) {
  coords <- as.matrix(coords)
  stopifnot(ncol(coords) == 2, nrow(coords) >= 3)
  
  n <- nrow(coords)
  
  iprev <- ifelse(1:n - 1 < 1, n, 1:n - 1)
  inext <- ifelse(1:n + 1 > n, 1, 1:n + 1)
  
  prev <- coords[iprev, , drop = FALSE]
  curr <- coords
  xnext <- coords[inext, , drop = FALSE]
  
  v1 <- prev - curr
  v2 <- xnext - curr
  
  dot   <- v1[,1]*v2[,1] + v1[,2]*v2[,2]
  cross <- v1[,1]*v2[,2] - v1[,2]*v2[,1]
  
  theta <- atan2(abs(cross), dot) 
  
  x <- coords[,1]; y <- coords[,2]
  area2 <- sum(x * y[c(2:n,1)] - y * x[c(2:n,1)]) 
  
  if (reflex) {
    area_sign <- sign(area2)
    convex <- (cross * area_sign) < 0
    ang <- ifelse(convex, theta, 2*pi - theta)
  } else {
    ang <- theta
  }
  
  if (degrees) ang <- ang * 180 / pi
  ang
}

coords <- poly_1 %>%
  select(gameId, playId, frameId, poly_order, position, nflId, x, y) %>%
  rownames_to_column("id")

pos_angles <- data.frame()

for(i in 1:nrow(coords)){
  coord_gid = coords$gameId[i]
  coord_pid = coords$playId[i]
  coord_fid = coords$frameId[i]
  
  poly_angles <- poly_1 %>%
    filter(gameId == coord_gid & playId == coord_pid & frameId == coord_fid) %>%
    ungroup() %>%
    select(x, y) %>%
    polygon_angles() %>%
    as.data.frame() %>%
    janitor::clean_names() %>%
    rename(angle = x)
  
  pos_angles <- pos_angles %>%
    bind_rows(poly_angles) %>%
    distinct(.keep_all = T)
}

angles <- pos_angles %>%
  rownames_to_column("id") %>%
  right_join(coords) %>%
  select(-c(id, poly_order)) %>%
  group_by(gameId, playId, nflId) %>%
  mutate(concavity = ifelse(angle > 180, "reflex", "non-reflex"), 
         reflex_time = ifelse(concavity == "reflex", 0.1, NA), 
         non_reflex_time = ifelse(concavity == "non-reflex", 0.1, NA)) %>%
  left_join(o_line_pos) %>%
  select(-jerseyNumber) %>%
  mutate(n_position = ifelse(is.na(n_position), position, n_position), 
         m_angle = mean(angle, na.rm = T),
         sd_angle = sd(angle, na.rm = T), 
         total = .1,
         total = sum(total),
         reflex_time = sum(reflex_time, na.rm = T),
         non_reflex_time = sum(non_reflex_time, na.rm = T),
         concavity = (reflex_time/total)*100) %>%
  select(-c(angle, x, y, frameId, position, reflex_time, non_reflex_time, total, nflId)) %>%
  ungroup() %>%
  select(gameId, playId, n_position, concavity, m_angle, sd_angle) %>%
  distinct(.keep_all = T) %>%
  pivot_wider(names_from = n_position, values_from = c(4:6), names_glue = "{n_position}_{.value}")

o_line_dist <- data.frame()

for(i in 1:nrow(frames)){
  angl_gid = frames$gameId[i]
  angl_pid = frames$playId[i]
  angl_fid = frames$frameId[i]
  
  vertices <- coords %>%
    filter(gameId == angl_gid & playId == angl_pid & frameId == angl_fid & position != "QB") %>%
    ungroup() %>%
    select(x, y) %>%
    as.matrix()
  
  t_distances <- sum(sqrt(diff(vertices[,1])^2 + diff(vertices[,2])^2)) %>%
    as.data.frame() %>%
    janitor::clean_names() %>%
    rename(t_distance = x) %>%
    mutate(gameId = angl_gid, 
           playId = angl_pid,
           frameId = angl_fid)
  m_distances <- mean(sqrt(diff(vertices[,1])^2 + diff(vertices[,2])^2)) %>%
    as.data.frame() %>%
    janitor::clean_names() %>%
    rename(m_distance = x) %>%
    mutate(gameId = angl_gid, 
           playId = angl_pid,
           frameId = angl_fid)
  sd_distances <- sd(sqrt(diff(vertices[,1])^2 + diff(vertices[,2])^2)) %>%
    as.data.frame() %>%
    janitor::clean_names() %>%
    rename(sd_distance = x) %>%
    mutate(gameId = angl_gid, 
           playId = angl_pid,
           frameId = angl_fid)
  
  distances <- t_distances %>%
    left_join(m_distances) %>%
    left_join(sd_distances)
  
  o_line_dist <- o_line_dist %>%
    bind_rows(distances) %>%
    distinct(.keep_all = T)
}

o_line_dist <- o_line_dist %>%
  select(-frameId) %>%
  group_by(gameId, playId) %>%
  summarise_all(., mean) %>%
  rename(t_oline_distances = t_distance,
         m_oline_distances = m_distance,
         sd_oline_distances = sd_distance)

data_1 <- tracking %>%
  group_by(gameId, playId) %>%
  filter(frameType %in% c("AFTER_SNAP", "SNAP")) %>%
  mutate(position = ifelse(club == "football", "+", position)) %>%
  select(gameId, playId, frameId, time, possessionTeam, club, nflId, causedPressure, event, position, x, y, blockedPlayerNFLId1, blockedPlayerNFLId2, blockedPlayerNFLId3) %>%
  left_join(blocker_dist) %>%
  left_join(left_pocket) %>%
  arrange(gameId, playId, frameId) %>%
  group_by(gameId, playId) %>%
  mutate(causedPressure = ifelse(causedPressure == "TRUE", "yes", NA)) %>%
  ungroup() %>%
  mutate(event = ifelse(event %in% c("pass_forward", "shovel_pass", "qb_sack", "run"), event, NA)) %>%
  group_by(gameId, playId) %>%
  mutate(time = ifelse(club == "football", ((frameId) - (min(frameId))) * 0.1, "")) %>%
  arrange(gameId, playId, frameId) %>%
  fill(event, .direction = "down") %>%
  fill(penetration, .direction = "down") %>%
  fill(causedPressure, .direction = "updown") %>%
  left_join(d_distance) %>%
  left_join(frames) %>%
  group_by(gameId, playId, frameId) %>%
  mutate(event_frame = ifelse(!is.na(event), frameId, NA), 
         penetration = ifelse(is.na(failure), "YES", penetration), 
         area = ifelse(is.na(penetration), round(area, 2), NA),
         time = ifelse(time == "", NA, time),
         time_label = ifelse(is.na(penetration), time, NA),
         event_fill = ifelse(frameId == min(event_frame, na.rm = T) & event %in% c("pass_forward", "shovel_pass", "qb_sack", "tackle", "run"), event, NA)) %>%
  group_by(gameId, playId) %>%
  fill(event, .direction = "updown") %>%
  fill(def, .direction = "updown") %>%
  fill(event_fill, .direction = "updown") %>%
  fill(time_label, .direction = "down") %>%
  mutate(event = ifelse(frameId >= def & !is.na(penetration) & event %in% c("qb_sack", "play_action", "run") | 
                          frameId >= min(event_frame, na.rm = T), event_fill, NA),
         time_label = ifelse(!is.na(time), time_label, NA)) %>%
  fill(area, .direction = "down") %>%
  mutate(min_event_time = ifelse(!is.na(event), frameId, NA),
         min_in_pocket_time = ifelse(!is.na(penetration), frameId, NA), 
         min_event_time = min(min_event_time, na.rm = T), 
         min_in_pocket_time = min(min_in_pocket_time, na.rm = T), 
         team = ifelse(club == possessionTeam, "offense",
                       ifelse(club == "football", "football", "defense"))) %>%
  left_join(misc) %>%
  left_join(jerseyNumber) %>%
  mutate(penetration = ifelse(is.na(penetration), "NO", penetration)) %>%
  mutate(penetration = ifelse(qb_in_pocket == "left", "YES", penetration)) %>%
  distinct(.keep_all = T) %>%
  fill(penetration, .direction = "down")

d_line <- tracking %>%
  group_by(gameId, playId) %>%
  select(gameId, playId, frameId, position, x, y, wasInitialPassRusher) %>%
  arrange(gameId, playId, frameId, x) %>%
  filter(wasInitialPassRusher == 1) %>%
  left_join(frames) %>%
  filter(!is.na(failure))

d_line_frames <- d_line %>%
  ungroup() %>%
  select(gameId, playId, frameId) %>%
  distinct(.keep_all = T)

d_line_dist <- data.frame()

for(i in 1:nrow(d_line_frames)){
  dl_gid = d_line_frames$gameId[i]
  dl_pid = d_line_frames$playId[i]
  dl_fid = d_line_frames$frameId[i]
  
  vertices <- d_line %>%
    filter(gameId == dl_gid & playId == dl_pid & frameId == dl_fid) %>%
    ungroup() %>%
    select(x, y) %>%
    as.matrix()
  
  t_distances <- sum(sqrt(diff(vertices[,1])^2 + diff(vertices[,2])^2)) %>%
    as.data.frame() %>%
    janitor::clean_names() %>%
    rename(t_distance = x) %>%
    mutate(gameId = dl_gid, 
           playId = dl_pid,
           frameId = dl_fid)
  m_distances <- mean(sqrt(diff(vertices[,1])^2 + diff(vertices[,2])^2)) %>%
    as.data.frame() %>%
    janitor::clean_names() %>%
    rename(m_distance = x) %>%
    mutate(gameId = dl_gid, 
           playId = dl_pid,
           frameId = dl_fid)
  sd_distances <- sd(sqrt(diff(vertices[,1])^2 + diff(vertices[,2])^2)) %>%
    as.data.frame() %>%
    janitor::clean_names() %>%
    rename(sd_distance = x) %>%
    mutate(gameId = dl_gid, 
           playId = dl_pid,
           frameId = dl_fid)
  
  distances <- t_distances %>%
    left_join(m_distances) %>%
    left_join(sd_distances)
  
  d_line_dist <- d_line_dist %>%
    bind_rows(distances) %>%
    distinct(.keep_all = T)
}

d_line_dist <- d_line_dist %>%
  select(-frameId) %>%
  group_by(gameId, playId) %>%
  summarise_all(., mean) %>%
  rename(t_dline_distances = t_distance,
         m_dline_distances = m_distance,
         sd_dline_distances = sd_distance)

misc_data <- data_1 %>%
  select(gameId, playId, frameId, time, area, penetration, event, event_fill) %>%
  distinct(.keep_all = T) %>%
  filter(time != "") %>%
  group_by(gameId, playId) %>%
  mutate(maxArea = max(area, na.rm = T), 
         minArea = min(area, na.rm = T)) %>%
  select(-area) %>%
  group_by(gameId, playId) %>%
  mutate(penTime = ifelse(penetration == "YES" & lag(penetration) == "NO", time, NA),
         eventTime = ifelse(!is.na(event) & lag(is.na(event)), time, NA)) %>%
  fill(eventTime, .direction = "updown") %>%
  fill(penTime, .direction = "updown") %>%
  select(-c(event, penetration, time, frameId)) %>%
  distinct(.keep_all = T) %>%
  mutate(pocketPen = ifelse(is.na(penTime), "no", "yes"))

pocket_metrics <- misc %>% 
  left_join(o_line_dist) %>% 
  left_join(d_line_dist) %>% 
  left_join(angles) %>% 
  left_join(growth) %>%
  left_join(misc_data) %>%
  ungroup() %>%
  mutate(passResult = ifelse(passResult == "" & event_fill == "run", "R", passResult), 
         def_success = ifelse(passResult %in% c("IN", "I", "S", "R"), "successful", "unsuccessful")) %>%
  select(-playDirection)

write.csv(pocket_metrics, 't_pocket_metrics.csv', row.names = F)

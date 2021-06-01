
## !! Change the directory on line 17 (where you want to save files) 
## and on line 25 (where the source excel file is located)!!

###library and work directory####
library(dplyr)
library(ggplot2)
library(ggrepel)
library(gganimate)
library(av)
library(png)
library(gifski)
library(RColorBrewer)
library(BBmisc)
library(imager)
library(plotrix)

setwd(".../...")

###### DEFINE FUNCTIONS ######
# run this entire block at once

# run this function to transform data into [t, ID, x, y] format
datacleanup <- function(name) {
  ptm <- proc.time()
  directory <- paste(".../...", name, ".csv", sep = "")  
  co.table <- read.table(directory, header = T, sep = ",", dec = ".")
  t.index <- which(colnames(co.table)=="t"); t.index # probably 70
  cat("These should be consequential integers:", co.table[1:10, t.index], "\n")

  co.table <- cbind(co.table[,t.index], # remove first column (rownumbers)
                    co.table[,2:(t.index-1)], # and move t column to front
                    co.table[,(t.index+1):ncol(co.table)])
  
  new.co.table <- as.data.frame(matrix(ncol = 4, nrow =0) )
  new.co.table[,1] <- as.numeric(new.co.table[,1])
  new.co.table[,3] <- as.numeric(new.co.table[,3])
  new.co.table[,4] <- as.numeric(new.co.table[,4])
  
  counter <- 0
  part <- ceiling(nrow(co.table)/20)
  cat("Lenght of 5% of table: ", part, "\n")
  
  for (row in 1:nrow(co.table)) {       # rearrange data into t, ID, x 
    inter.table <- array(dim = c(0,4))
    for (kol in seq(2,ncol(co.table),2)) {             #  and y columns
      if (! is.na(co.table[row,kol])) {
        inter.table <- rbind(inter.table, c(as.numeric(co.table[row,1]), 
                                            as.numeric(substr(colnames(co.table)[kol], 2, 
                                                   (nchar(colnames(co.table)[kol])-1))), 
                                            co.table[row, kol], co.table[row, kol+1] ) )
      }
    }
    if (! row%%part) {
      counter <- counter + 5
      cat('\r', counter, '% of rows elapsed') }  
    new.co.table <- rbind(new.co.table, inter.table)
  }
  colnames(new.co.table) <- c("t", "ID", "x", "y")
  new.co.table$ID <- as.factor(new.co.table$ID)
  cat("\nTime elapsed: ", (proc.time()-ptm)[1], "\n")
  return(new.co.table)
}

# delete all ID's which don't move
deldead <- function(df) {
  ptm <- proc.time()
  removed_ID = ""
  for (id in seq(0, length(table(df$ID)), by = 1)) {
    if (dim(table(df[df$ID == id,"x"])) == 1 & 
        dim(table(df[df$ID == id,"y"])) == 1 ) 
      
    {df <- df[!(df$ID == id),]
    removed_ID <- append(removed_ID, id)
    } 
  }
  cat(c("Removed ID's", removed_ID), collapse = " ")
  cat("Time elapsed:", (proc.time()-ptm)[1])
  return(df)
}


# delete artefacts (IDs that move linearly, or are present for only a short amount of time)
delart <- function(df, limit, timelimit) {
  
  IDs <- levels(df$ID)
  duration <- max(df$t)
  max_x <- max(df$x)
  max_y <- max(df$y)
  
  for (ID in IDs) {
    if (table(unlist(df$ID))[ID] < timelimit * duration || 
        abs(min(df$x[df$ID==ID]) - max(df$x[df$ID==ID])) < limit*max_x ||
        abs(min(df$y[df$ID==ID]) - max(df$y[df$ID==ID])) < limit*max_y) {
      df <- df[!(df$ID==ID),]
    }
  }
  return(df)
}

#ard2 <- delart(cd, 0.02) # ard stands for artefact removed data
#ardids <- (subset(as.data.frame(table(ard2$ID)),Freq>0))[,1]
#ardids[1:6]

# delete points on patches
incircle <- function(x,y,xc,yc,r) {
  return ((x-xc)^2 + (y-yc)^2 < r^2)
}

matrixer <- function(df = track.backup, ID, times = "all") {
  coo <- (readline(prompt = "Enter xc, yc, r (centre coordinates and radius of circle, sep= ' ':"))
  xc <- as.numeric(strsplit(coo, " ")[[1]])[1]
  yc <- as.numeric(strsplit(coo, " ")[[1]])[2]
  r <- as.numeric(strsplit(coo, " ")[[1]])[3]
  if (times != "all") {
    time_int <- seq(times[1], times[2], by = 1)
    df_out <- df[((df$t %in% time_int) & (df$ID %in% ID)),]
  }
  else {df_out <- df[df$ID %in% ID,]}
  
  counter <- 0
  part <- ceiling(nrow(df_out)/100)
  print("start")

  for (row in nrow(df_out):1) {
    if (incircle(df_out[row,3],df_out[row,4],xc,yc,r)) {
      df_out <- df_out[-row,]
    }
    if (! row%%part) {
      counter <- counter + 1
      cat('\r', counter, '% of rows elapsed') } 
    } 
  return (df_out)}

# obtain points to plot circle on movement trajectory
CircleFun <- function(centre = c(0,0), r = 1, npoints = 100){
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- centre[1] + r * cos(tt)
  yy <- centre[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

# make graph trajectory of specific ID's (up to 6)
graphID <- function(ID = IDs[1:6], times = "all", df = track) {
  ID_data <- df[df$ID %in% ID,]
  if (times != "all") {
    time_int <- seq(times[1], times[2], by = 1)
    ID_data <- ID_data[ID_data$t %in% time_int,]
  }
  
   print(ggplot(ID_data, aes(x, y, label = t, group = ID)) +
    ylim(min(ID_data$y), max(ID_data$y)) +
    coord_fixed(ratio = 1) +
    theme_classic(base_size = 16) + 
    geom_point(aes(colour = t, shape = ID), size = 2) +
    scale_colour_gradientn(colours = rainbow(5)) +
    ggtitle(paste(c("Trajectory of ID('s)", ID, ". Time interval:", times), collapse = " ")) + 
    xlab("X-coordinates (pixels)") + ylab("Y-coordinates (pixels)") + 
    geom_path(data = dat,aes(x,y), inherit.aes = F))
   }


# make animated trajectory of specific ID's (up to 12)
graphIDani <- function(ID, times = "all", df = track, fps = 20, save = F) {
  ID_data <- df[df$ID %in% ID,]
  if (times != "all") {
    time_int <- seq(times[1], times[2], by = 1)
    ID_data <- ID_data[ID_data$t %in% time_int,]
  }
  
  title <- paste(c("Trajectory of ID('s)", ID, ". Time interval:", times), collapse = " ")
  
  a <- ggplot(ID_data, aes(x, y, label = ID, group = t)) +
    ylim(min(ID_data$y), max(ID_data$y)) +
    coord_fixed(ratio = 1) +
    theme_classic(base_size = 16) + 
    geom_point(aes(colour = ID), size = 2, alpha = 0.8) +
    scale_colour_brewer(palette="Paired") +
    xlab("X-coordinates (pixels)") + ylab("Y-coordinates (pixels)") +
    labs(title = '{title}', subtitle = 'Current time: {frame_time}') + 
    transition_time(t)
  
  if (! save) {
    print(animate(a,fps=fps), nframes = round(length(unique(ID_data$t))/3), end_pause = 20)
  }
  
  else {
    b <- (animate(a,fps=fps, nframes = round(length(unique(ID_data$t))/3), end_pause = 20, renderer = av_renderer()))
    anim_save(paste(c(ID, ".mp4"), collapse = "_"),b) 
    }
}


# delete specified ID's
delID <- function(ID, time = "all", df = track) {
  backup <- df
  if (time != "all") {
    times <- seq(as.numeric(time[1]),as.numeric(time[2]),by=1)
    df <- df[-(which((df$ID %in% ID) & (df$t %in% times))),]
    }
  else {
    df <- df[-(which(df$ID %in% ID)),]
  }
  if (nrow(df)==0) {
    cat(c("Deletion not successful, returning original data frame."))
    return(backup)
    }
  return(df)
}

#cd <- delID(c(34), "all", cd)


# prepare data in one function:
# read and transform table, remove bad data
workflow <- function() {
  filename <- readline(prompt = 'Enter file name (without .csv): ')
  cd <- datacleanup(name = filename) # transform table
  dd <- deldead(df = cd)     # remove still/linear data
  
  answer1 <- "y"
  while (answer1 == "y") {
    limit <- as.numeric(readline(prompt = "Enter limit for data removal based on coordinates (e.g. 0.05): "))
    timelimit <- as.numeric(readline(prompt = "Enter limit for data removal based on time (e.g. 0.05): "))
    ard <- delart(df = dd, limit = limit, timelimit = timelimit) # remove fragments and artefacts
    cat("remaining ID's: ", paste(subset(as.data.frame(table(ard$ID)),Freq>0)[,1], collapse = ', '))
    answer1 <- readline(prompt = 'Do you want to change the limits? Enter y or n. Warning: previous edits with combinator will be lost!')
  }
  IDlist <- paste(subset(as.data.frame(table(ard$ID)),Freq>0)[,1], collapse = ', ')
  output_name <- paste("inter", filename, sep = "_")
  write.csv(ard, output_name, row.names = FALSE)
  return(list(ard, IDlist))
  
}


# the sorter function groups all IDs on their patches. colist format: c(x-co1, x-co2, x-co3, y-co1, y-co2)
# whereby x-co1 and y-co1 are coordinates of patch 1, x-co2 and y-co2 of patch 2, etc. and x-co1 and 
# y-co2 of patch4, etc.
sorter <- function(data_frame = track.backup, IDlist = IDs, colist) {
  list0 <- str(""); list1 <- str(""); list2 <- str(""); list3 <- str(""); list4 <- str(""); list5 <- str(""); list6 <- str("")
  xmargin <- abs(colist[2]-colist[1])/2
  ymargin <- abs(colist[5]-colist[4])/2
  x1 <- c(colist)
  
  for (id in IDlist) {
    xmean <- mean(data_frame$x[data_frame$ID==id])
    ymean <- mean(data_frame$y[data_frame$ID==id])
    if (between(xmean, colist[1]-xmargin, colist[1]+xmargin) && 
        between(ymean, colist[4]-ymargin, colist[4]+ymargin)) {
      list1 <- append(list1, id)
    }
    else if (between(xmean, colist[2]-xmargin, colist[2]+xmargin) && 
             between(ymean, colist[4]-ymargin, colist[4]+ymargin)) {
      list2 <- append(list2, id)
    }
    else if (between(xmean, colist[3]-xmargin, colist[3]+xmargin) && 
             between(ymean, colist[4]-ymargin, colist[4]+ymargin)) {
      list3 <- append(list3, id)
    }
    else if (between(xmean, colist[1]-xmargin, colist[1]+xmargin) && 
             between(ymean, colist[5]-ymargin, colist[5]+ymargin)) {
      list4 <- append(list4, id)
    }
    else if (between(xmean, colist[2]-xmargin, colist[2]+xmargin) && 
             between(ymean, colist[5]-ymargin, colist[5]+ymargin)) {
      list5 <- append(list5, id)
    }
    else if (between(xmean, colist[3]-xmargin, colist[3]+xmargin) && 
             between(ymean, colist[5]-ymargin, colist[5]+ymargin)) {
      list6 <- append(list6, id)
    }
    else {
      list0 <- append(list0, id)
    }
  }
  return(list(list1,list2,list3,list4,list5,list6,list0))
}

#grouped <- sorter(final,IDs,c(380,850,1322,190,661)); grouped


# counts the amount of times where the coordinates of 2 IDs are the same or differ.
overlap <- function(ID1, ID2, df = track, print_unique_points = F, timepoints = "all", check = F) {
  ID1 <- as.character(ID1)
  ID2 <- as.character(ID2)
  ptm <- proc.time()
  ID_data <- df[df$ID %in% c(ID1,ID2),]
  same_counter = 0; different_counter = 0
  t_list <- ""
  overlap_vector <- c()
  if (is.numeric(timepoints)) {times <- seq(as.numeric(timepoints[1]), as.numeric(timepoints[2], by = 1))}
  else times=""
  
  
  for (t in unique(ID_data$t)){
    if (is.numeric(timepoints) & !(t %in% times)) {}
    else if (is.error(try(ID_data[ID_data$t==t & ID_data$ID==ID1,3:4] == ID_data[ID_data$t==t & ID_data$ID==ID2,3:4], silent=T))) 
    {
      different_counter <- different_counter + 1
      t_list <- append(t_list, t)
    }
    else if (all(ID_data[ID_data$t==t & ID_data$ID==ID1,3:4] == ID_data[ID_data$t==t & ID_data$ID==ID2,3:4]))
    {
      same_counter <- same_counter + 1
      if (check) {overlap_vector <- c(overlap_vector,t)}
    }
    else {different_counter <- different_counter + 1; t_list <- append(t_list, t)
    }
  }
  cat("\nTime elapsed: ", (proc.time()-ptm)[1])
  cat(c("\nIDs", ID1, ID2, ". No. of identical points:", same_counter, ". No. of unique points:", different_counter))   
  if (print_unique_points) {cat(c("Timestamps of unique points:", t_list), collapse = " ")}
  if (check) {return(overlap_vector)}
}


# delete overlapping points between ID1 and ID2 (those of ID2 will be deleted)
delov <- function(ID1,ID2, df = track, times = "all") {
  deltimes <- overlap(ID1, ID2, df, timepoints = times, check = T)
  ID2 <- as.character(ID2)
  ptm <- proc.time()
  
  newdf <- df[!(df$ID == ID2 & df$t %in% deltimes),]
  
  cat("\nTime elapsed: ", (proc.time()-ptm)[1])
  return(newdf)
}

# delete tail of ID (since this is not actually recorded, but a artefact of the tracker)
delt <- function(IDlist = IDs, df = track, tail_length = 17) {
  for(x in IDlist) {
    x <- as.character(x)
    max_t <- max(df$t[df$ID == x])
    df <- delID(x,c(max_t - tail_length, max_t),df)
  }
  return(df)
}

# combines IDs
combinator <- function(df = track) {
  answer <- "y"
  original_table <- df
  while (answer == "y") {
    ID1 <- as.character(readline(prompt = "Enter main ID: "))
    ID2 <- as.character(readline(prompt = "Enter ID to add to main ID: "))
    timeQ <- readline(prompt = "Enter time interval in which both IDs should be combined or type 'all':")
    ptm <- proc.time()
    combined_times <- ""
    counter = 0
    if (timeQ == "all") {
      times = ""; df[df$ID == ID2, 2] <- ID1
      }
    else  {
      times <- seq(as.numeric(strsplit(timeQ, " ")[[1]])[1], as.numeric(strsplit(timeQ, " ")[[1]])[2], by = 1)
      df[which(df$ID == ID2 & df$t %in% times), 2] <- ID1
      }
    
    for (t in unique(df[df$ID==ID1,1])) {
      if ((timeQ != "all" & t %in% times) | timeQ == "all") {
        if (nrow(df[df$t==t & df$ID == ID1,])==2) {
          rows <- which( df$t==t & df$ID == ID1 )
          if (!(all(df[rows[1],]==df[rows[2],]))) {
            combined_times <- append(combined_times, t)
            counter <- counter + 1
            }
          df[rows[1], 3] <- round(mean(c(df[rows[1],3],df[rows[2], 3])))
          df[rows[1], 4] <- round(mean(c(df[rows[1],4],df[rows[2], 4])))
          df <- df[-c(rows[2]),]
        }
        }
          
    }
    cat("\nTime elapsed: ", (proc.time()-ptm)[1])
    cat(c("\nDifferent now combined timepoints (", counter,"):", combined_times), collapse = " ")
    answer <- readline(prompt = "Do you want to combine more ID's? Enter y or n.")
  }  
  answer <- readline(prompt = "Do you want to keep the changes? Enter y or n.")
  if (answer == "y") {return(df)}
  else return(original_table)
}


masscombinator <- function(ID1, IDs, df = track) {
  original_table <- df
  ID1 <- as.character(ID1)
  ptm <- proc.time()
  combined_times <- ""
  counter = 0
  times = ""; df[df$ID %in% IDs, 2] <- ID1
  
  for (t in unique(df[df$ID==ID1,1])) {
    if (nrow(df[df$t==t & df$ID == ID1,])>1) {
        rows <- which( df$t==t & df$ID == ID1 )
        if (length(unique(df[rows[1:length(rows)],3:4]))>1) {
          combined_times <- append(combined_times, t)
          counter <- counter + 1
        }
        df[rows[1], 3] <- round(mean(df$x[rows[1:length(rows)]]))
        df[rows[1], 4] <- round(mean(df$y[rows[1:length(rows)]]))
        df <- df[-c(rows[2:length(rows)]),]
      }
    }
    
  cat("\nTime elapsed: ", (proc.time()-ptm)[1])
  cat(c("\nDifferent now combined timepoints (", counter,"):", combined_times), collapse = " ")
  answer <- readline(prompt = "Do you want to keep the changes? Enter y or n.")
  if (answer == "y") {return(df)}
  else return(original_table)
}

# calculates time from frame number
min. <- function(x) {
  x <- x/10
  return(cat(x%/%60,"min.",round(x%%60),"sec."))
}

# calculates frame number from time
fr <- function(m,s) {return((m*60 + s)*10)}


## START ####

workout <- workflow(); track.backup <- workout[[1]]; IDs <- strsplit(workout[[2]], ", ")[[1]]

## MATRIX-ONLY TRAJECTORIES ####

# obtain duration of analysed video fragment
min.(max(track.backup$t))

# obtain patch coordinates
im <- load.image("index.png"); plot(im)
locator() # click on each patch then esc to obtain coordinates

# group by patch
grouped <- sorter(IDlist=unique(track.backup$ID),colist=c(650,1110,1570,540,1000));grouped
track <- track.backup
 
# finetune patch coordinates
plot(im); draw.circle(1211,538,53, nv = 100, border = "white", col = NA, lty = 1, lwd = 1)
dat <- CircleFun(c(1200,730), 93) # enter the final coordinates of the patch centre and its radius, this circle will be plotted by graphID()

# plot by patch (only useful if there are many IDs in this section)
IDs <- grouped[[2]]
totaal <- floor(length(IDs)/6)
for (el in seq(0,totaal)) {  # if an error occurs, you might have to rerun the graphID function definition on line 144
  graphID(ID = IDs[(el*6):(el*6+5)], df = track.backup)
} ; ceiling(length(IDs)/6)
IDs <- c(22,33,44,55) # select IDs that have data points outside of the circle

# keep only matrix coordinates
track <- matrixer(ID = IDs)

IDs <- unique(track$ID); IDs ; length(IDs) ; dataset <- track; totaal <- floor(length(IDs)/6)
# since track is now a subset of track.backup (which you still need for mining
# other areas), the backup for track is now "dataset".

# get overlap between IDs for subset of IDs
IDs <- unique(track$ID) # [1:20] use these square brackets if more than 20 IDs
for (x in seq(1,(length(IDs)-1))) {
  for (ID in IDs[(x+1):length(IDs)]) {overlap(IDs[x],ID,track)}}
# no unique points means you can delete one trajectory, and high overlap could 
# mean that two trajectories belong (partially) to the same individual and can
# therefore be (partially) combined

IDs <- unique(track$ID); IDs; length(IDs); dataset <- track; totaal <- floor(length(IDs)/6)
# plot again by patch
for (el in seq(0,totaal)) {
  graphID(ID = IDs[(el*6):(el*6+5)], df = dataset)
} ; ceiling(length(IDs)/6)

# get summary of ID timestamps
for (x in seq(0,totaal)) { 
  cat(c("\n"))
  for (ID in IDs[(x*6):(x*6+5)]) {
    cat(c("\t ID:", ID,"mean:", round(mean(dataset$t[dataset$ID == ID])), "median:", round(median(dataset$t[dataset$ID == ID])),
        "\n", min.(min(dataset$t[dataset$ID == ID])), "\t", min.(max(dataset$t[dataset$ID == ID])), "\n"))
  }}

# get coordinates vs time for each individual to check if it contains an artefact
par(mfrow=c(2,3)) ; for(X in IDs) {
  plot(track$t[track$ID==X], track$x[track$ID==X], main = X)#, xlim=c(3920,3950)) #, ylim=c(350,360))
}; par(mfrow=c(1,1))

track <- delt(IDs[-c(25,23)]) # delete tails of tracks with a flat end

# Now use the output of three previous sections to combine, split, and delete 
# (parts of) trajectories. Commands to perform these edits are found below.

# combine IDs
track <- combinator(track)
track <- masscombinator(IDs[1], IDs[-c(1,2)])

# miscellaneous
length(unique(track$ID)) # how many IDs are left?

graphID(c(58,55,85,95,87,98))
graphIDani(c(254),save = F)
X <- "16"; plot(track$t[track$ID==X], track$x[track$ID==X], main = X) #, xlim=c(3500,4000))
# if the line above plots in a smaller frame, use this command and then run again: par(mfrow=c(1,1))

track <- delID(c(41),c(0,1000)) # delete ID
track <- track[-which(between(track$x,left = 585,right = 600) & between(track$y,left=1140,right=1150) & track$ID == "153"),] 
# delete points at a certain location (inclusive borders) to eliminate e.g. disturbance caused by debris

dataset <- track # back-up track if you are happy with your edits

track$ID[track$ID == "186" & track$t > 3000] <- "509" # create new ID by splitting an existing one

for (x in seq(5,6)) {graphID(track,IDs,c(x*1000, x*1000+1000))} # plot graphs with time spans of 1000

#write backup
write.csv(track,"track",row.names=F)

#open backup
track <- read.csv("track"); track$ID <- as.character(track$ID)

#write final doc
write.csv(track, "M8_12_1-3",row.names=F)


track <- rbind(track1,track2,track3,track5,track6,track7)
track1 <- track
track2 <- track
track3 <- track
track4 <- track
track5 <- track
track6 <- track
track7 <- track

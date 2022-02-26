# Pre-processing script.

# Convert to data frame
reisby = as.data.frame(Reisby)

# Remove rows with missing data
reisby = na.omit(reisby)

# Get number of groups in data
groups = unique(reisby$id)

# Data pre processing
for (i in 1:dim(reisby)[1]){
  if (reisby$week[i] == 0 | 1){
    reisby$week[i] = 0
  } else{
    reisby$week[i] = 1
  }
    
  if (reisby$hd[i] %in% 0:6){
    reisby$hd[i] = 0
  } else if (reisby$hd[i] %in% 7:17){
    reisby$hd[i] = 1
  } else if (reisby$hd[i] %in% 18:24){
    reisby$hd[i] = 2
  } else {
    reisby$hd[i] = 3
  }
}

# Re-encode id vector
reisby$id = match((reisby$id), groups)

# Split data into test and train // note groups are not split as train[200] is 53
# and test[201] = 54
reisby.train.scaled = reisby.scaled[1:200,]
reisby.test.scaled = reisby.scaled[201:dim(reisby)[1],]
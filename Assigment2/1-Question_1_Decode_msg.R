# Load the functions of dobrow

# ascii
# CharIndex
# decrypt
# Score

source('functions/dobrow_functions.R')

# decode the message in row i of messages.txt

messages <- read.table("data/messages.txt", sep = "\t")

message <- messages$V1[4]
codemess <- message
rm(messages)

mat <- read.table("data/Englishcharacters.txt",header=F)
logmat <- log(mat + 1)
rm(mat)


# Number of characteres
nchar(codemess)

# score codemess
score(codemess)

#codemess <- message
orig_msg <- "tracing the development of monte carlo methods we will also briefly mention what we might call the second generation mcmc revolution starting in the midatoalate nineties this includes the development of particle filters reversible jump and perfect sampling and concludes with more current work on population or sequential monte carlo and regeneration and the computing of honest standard errors"
#https://arxiv.org/pdf/0808.2902.pdf

## List for saving the decryption
descrypted_msg <- list()
descrypted_msg[[1]] <- codemess
pos <- 2



# Step 1: Start with any coding function. Identity for convenience.
curFunc <- 1:27
score_f <- score(decrypt(codemess,curFunc))

# instantiate a map to hold previously computed codes scores
map <- new.env(hash=T, parent=emptyenv())
map[[paste(curFunc, collapse='')]] <- score_f


# run 156000 iterations of the Metropolis-Hastings algorithm
for (iteration in 1:156000) {

  # Step 2: sample two letters to swap uniformly
  set.seed(iteration)
  swaps <- sample(1:26,2)

  # let curFunc be oldFunc but with two letters swapped
  oldFunc <- curFunc
  curFunc[swaps[1]] <- oldFunc[swaps[2]]
  curFunc[swaps[2]] <- oldFunc[swaps[1]]

  # Step 3. Compute the acceptance function: Two scenarios:
  # 3.1 if we have already scored this decoding,
  # retrieve score from our map
  if (exists(paste(curFunc, collapse =''), map)){
    score_f_star <- map[[paste(curFunc, collapse ='')]]
  } else
    # 3.2 if we have not already scored this decoding,
    # calculate it and store it in the map
  {
    score_f_star <- score (decrypt(codemess,curFunc))
    map[[paste(curFunc, collapse = '')]] <- score_f_star
  }
  # acceptance function
  acc <- exp(score_f_star-score_f)
  
  # Step 4. Accept the proposal function f* or keep f.
  if (runif(1) < acc)
  {
    score_f <- score_f_star
    
  } else 
  {
   
    curFunc <- oldFunc
     
  }
  
  # print out our decryption every 100 iterations
  if ((iteration %% 2000) == 0)
  {
    descrypted_text <- decrypt(codemess,curFunc)
    print(c(iteration,descrypted_text, descrypted_text == orig_msg, score_f_star))
    descrypted_msg[[pos]] <- descrypted_text
    pos <- pos + 1
  }
}


save(descrypted_msg, file = 'rda/descrypted_msg.rda')
load('rda/descrypted_msg.rda')


str(descrypted_msg)

decrypted_msg <- descrypted_msg[[79]]

decrypted_msg == orig_msg

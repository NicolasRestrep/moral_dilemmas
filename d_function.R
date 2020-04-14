#load equation information from Interact
equation <- read_table("equation info.txt", col_names = FALSE)
equation<- as.data.frame(equation) #put in better format

#label according to what term the coefficients correspond to
colnames(equation) <- c("Term", "Aep", "App", "Aap", "Bep", "Bpp", "Bap", "Oep", "Opp", "Oap")
rownames(equation) <- c("Constant", "Aeb", "Apb", "Aab", "Beb", "Bpb", "Bab", "Oeb", "Opb", "Oab",
                        "AeBe", "AeOp", "ApBp", "AaBa", "BeOe", "BeOp", "BpOe", "BpOp", "AeBeOe",
                        "AeBeOp")

#get rid of the Z000 column
equation <- equation[,-1]

#writing function
eventcalc <- function(actor, beh, object) {
  
  #make empty matrix to store info
  store <- matrix(NA, 20, 1)
  
  #something that won't affect the constant
  store[1,] <- 1
  
  #setting actor EPA values
  store[2,] <- subdictionary$E[subdictionary$term == "person"] #Ae
  store[3,] <- subdictionary$P[subdictionary$term == "person"] #Ap
  store[4,] <- subdictionary$A[subdictionary$term == "person"] #Aa
  
  #setting subdictionary EPA values
  store[5,] <- subdictionary$E[subdictionary$term == "kill"] #Be
  store[6,] <- subdictionary$P[subdictionary$term == "kill"] #Bp
  store[7,] <- subdictionary$A[subdictionary$term == "kill"] #Ba
  
  #setting object EPA values
  store[8,] <- subdictionary$E[subdictionary$term == "person"] #Oe
  store[9,] <- subdictionary$P[subdictionary$term == "person"] #Op
  store[10,] <- subdictionary$A[subdictionary$term == "person"] #Oa
  
  store[11,] <- store[2,] * store[5,] #Ae*Be
  store[12,] <- store[2,] * store[9,] #Ae*Op
  store[13,] <- store[3,] * store[6,] #Ap*Bp
  store[14,] <- store[4,] * store[7,] #Aa*Ba
  store[15,] <- store[5,] * store[8,] #Be*Oe
  store[16,] <- store[5,] * store[9,] #Be*Op
  store[17,] <- store[6,] * store[8,] #Bp*Oe
  store[18,] <- store[6,] * store[9,] #Bp*Op
  store[19,] <- store[2,] * store[5,] * store[8,] #Ae*Be*Oe
  store[20,] <- store[2,] * store[5,] * store[9,] #Ae*Be*Op
  
  #to store the EPA values for actor, subdictionary, and object after situation
  postepa <- matrix(NA, 20, 9)
  
  #apply function 
  i<- 1
  for(i in 1:9) {
    postepa[,i] <- equation[,i]*store
    i+1
  }
  
  #put in better data format
  postepa <- as.data.frame(postepa)
  
  postepa <- apply(postepa, 2, sum) #get the sum of the equation to get the actual EPA values
  postepa <- cbind(store[2:10], postepa) #put the initial EPA and post EPA in df together
  
  #calculate deflection
  deflection <- apply(postepa, 1, diff) #get difference b/w pre and post EPA
  deflection <- as.data.frame(deflection)
  deflection <- sapply(deflection, function(x) x^2) #sq differences
  deflection <- apply(deflection, 2, sum) #sum squared differences :) 
  return(deflection)
}

# Create just one event 

trans_one <- trans[1,]

trans_one %>% 
  mutate(actor = as.character(actor),
         object = as.character(object), 
         behavior = as.character(behavior)) %>% 
  rowwise %>% 
  mutate(def = eventcalc(actor, behavior, object))


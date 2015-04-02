#################################################################################
#### Functions for BML Simulation Study


#### Initialization function.
## Input : size of grid [r and c] and density [p]
## Output : A matrix [m] with entries 0 (no cars) 1 (red cars) or 2 (blue cars)
## that stores the state of the system (i.e. location of red and blue cars)

bml.init <- function(r, c, p)
{
  
  total <- r*c
  number <- ceiling(p  * total)
  
  if(number %% 2 != 0)
    number = number-13
  
  nocar <- total - number
  enter <- sample(c(rep(0,nocar),rep(1,number/2),rep(2,number/2)))
  res <- matrix(sample(enter),r,c)
  return(res)
}

#### Function to move the system one step (east and north)
## Input : a matrix [m] of the same type as the output from bml.init()
## Output : TWO variables, the updated [m] and a logical variable
## [grid.new] which should be TRUE if the system changed, FALSE otherwise.

## NOTE : the function should move the red cars once and the blue cars once,
## you can write extra functions that do just a step north or just a step east.
moveRight <- function(m)
{
  stoped <- m[,c(2:ncol(m),1)]!=0
  red_cars <- m*(m==1)
  m=m*(m!=1) + red_cars * stoped + (red_cars * !stoped)[,c(ncol(m),1:(ncol(m)-1))] 
  return(m)
}

moveUp <- function(m)
{
  
  stoped <- m[c(nrow(m),1:(nrow(m)-1)),]!=0
  blue_cars <- m*(m==2)
  
  m=m*(m!=2) + blue_cars * stoped + (blue_cars * !stoped)[c(2:nrow(m),1),] 
  
  return(m)
}

bml.step <- function(m)
{
  
  m_new <- moveUp(moveRight(m))
  
  return(list(m_new, !all(m==m_new)))
}



#### Function to do a simulation for a given set of input parameters
## Input : size of grid [r and c] and density [p]
## Output : *up to you* (e.g. number of steps taken, did you hit gridlock, ...)

bml.sim <- function(r, c, p)
{
  
  result <- bml.step(bml.init(r,c,p))
  j = 1
  
  while(result[[2]]==T & j <10)
  {
    result <- bml.step(result[[1]])
    j = j+1
    print(j)
    image(result[[1]],col = c('white','red','blue'))
  }
  
  return(j)
}


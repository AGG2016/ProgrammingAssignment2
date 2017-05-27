#Lexical Scoping

##Intro -Read me-

### Well, the task itself was very simple, since basically replacing
### the "mean" function with the "solve" function in the example provided,
### will do the trick. However, I really wanted to understand a little bit
### how R manage symbols and environments, both during coding and runtime.
### I based my explanation of this workshop on two contributions:

### - The article "Desmystifying makevector()" by Igreski (on the forum:
### https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-breakingDownMakeVector.md).

### - The vignette on "Environments" in "Advanced R", by Hadley Wickham
### (http://adv-r.had.co.nz/Environments.html). 


## How it works

### So, basically, I understand that the key aspect of the function proposed is
### related to the way functions and their respective environments are "nested"
### when written: it all comes at the end to the "environments hierarchy"; more
### specifically, to the hierarchy of "function environments".

### I think, to cut to the chase, that the important difference is the one 
### between the "enclosing" environment and the "execution" environment. The 
### latter is the environment in which a function is defined, the "global 
### environment" -in our exercise-, while the former is an ephemeral environment
### created by the function when called. As Wickham explains, "when you create a
### function inside another function, the enclosing environment of the child 
### function is the execution environment of the parent, and the execution 
### environment is no longer ephemeral".

### We create the function, whose "enclosing" environment is obviously the 
### "global environment".

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

### More interesting than that initial step, let's create a simple matrix as
### input for the "makeCacheMatrix" function and assign the value to list "t":

a<-matrix(1:4,2,2)
t<-makeCacheMatrix(a)

### By printing the object "t":

t

### It is clear that objects "x" (the original matrix), "m" (the calculated in-
### verse), alongside with functions get(), set(), getmean(), and setmean(), are
### now allocated in the "execution" environment of the function and the name for
### allocation in memory is printed on the screen.

### As far as I understand, based on the entry by Igreski in the forum, the magic
### is in the use of the pointer "<<-"; in the way that "makeCacheMatrix" is de-
### fined, the object "t" created cotains pointers to functions within the "exe-
### cution" environment, which is not ephemeral, and "these pointers prevent the
### memory consumed by [the function] from being released by the garbage collector.
### Therefore, the entire [function] environment stays in memory, and [t] can 
### access its functions as well as any data in that environment that is referen-
###ced in its functions" (this is a quote from Igreski, but he refers to his own
### function and objects in the original text).

### Now, in the design of the "makeCacheMatrix" is also clear that "m", the name
### bound to the value if the inverse, is originally set to "NULL". We define the
### complementary function:

cacheinverse <- function(x,...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}

### The first time we call it on "t" -remember: the list created by the
### "makeCacheMatrix" function with pointers to its "execution" environment-, the
### function calculates the inverse and stores it in the not-ephemeral "execution"
### enviroment, bound to the name "m".

cacheinverse(t)

### We finally arrive to our goal when we again call the function on "t":

cacheinverse(t)

### And we get the expected "getting cached data" sign.

### The whole logic can be tested calling the "set" function, nested within the
### "makeCacheMatrix" function and whose "enclosing" environment is therefore the
### "execution" environment of the parent function.

b <- matrix(5:8,2,2) #We define a second matrix

t$set(b)

### By printing "t" on the screen, we prove that the "execution" environment has 
### changed. Again, pointers will keep it safe from the garbage collector, so it 
### will remain in the memory, while the initial "execution" environment -I assu-
### me- was erased (don't be harsh on me if I make some technical mistake in this
### explanation: I'm not a programmer; I'm simply thinking out loud about these
### concepts in order to make them clear to myself. All corrections are more than
### welcome!).

### We call again the "cacheinverse" function on "t" and the first time the inver-
### se must be calculated; it cannot be cached since the input matrix was changed.

cacheinverse(t)

### But, the second time, the solution is cached, as expected.

cacheinverse(t)

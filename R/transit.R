## https://github.com/cognitect/transit-format

library(gtools)


# resp <- rawsugar_api('/api/projects')

## cache

CACHE_CODE_DIGITS <- 44
BASE_CHAR_INDEX <- 48

# note: can only map ints to strings with this method, may not suffice
makeCache <- function() {
    return(hashmap(1000000,"foo"))            #man is this a hacky language!
}

### R makes passing a mutable ht around impossible, so going with a global for now
# cache <- makeCache()

isCacheRef <- function(thing) {
    return(is.character(thing) &&
           substr(thing,1,1) == "^")
}

lookupCacheRef <- function(thing, cache) {     #cache
    code <- cacheCode(thing)
    return(cache[[code]])
}    

## convert string like "^ca" into code
cacheCode <- function(thing) {
    codes <- asc(substr(thing, 2, 100))
    if (length(codes) == 1) {
        return(codes[[1]] - BASE_CHAR_INDEX)
    } else {
        return((codes[[2]] - BASE_CHAR_INDEX) +
               (codes[[2]] - BASE_CHAR_INDEX) * CACHE_CODE_DIGITS)
            
    }
}

cacheable  <- function(thing) {
    return(is.character(thing) &&
           (nchar(thing) > 3 ||
            substr(thing,1,1) == "~"))  # TODO some other patterns
}

## Doesn't work, mutating a list seems weird
## https://stackoverflow.com/questions/15497947/call-by-reference-in-r-using-function-to-modify-an-object
cacheThing <- function(thing, cache) {              #cache
    code <- cache$size() - 1
#    sprint(list("cache", thing, code))
    cache[[code]] <<- thing
    return(code)
}

##  convert from Transit to something R native

untransit  <- function(transit) {
    cache <<- makeCache()
    res <- untransitThing(transit, cache)      #cache
#    print(cache)                        
    return(res)
}

untransitThing  <- function(transit, cache) {  #cache
    if (is.list(transit) && transit[[1]] == "^ ") {
        return(untransitMap(transit, cache))   #cache
    } else {
        if (isCacheRef(transit)) {
            return(lookupCacheRef(transit, cache)) #cache
        } else {
            if (cacheable(transit)) {
                cacheThing(transit, cache)          #cache
                return(transit)
            } else {
                return(transit)                
            }
        }
    }
}

untransitMap <- function(transit, cache) {     #cache
    i = 2                               #0 is list, 1 is "^ "
    res = list()
    while (i < length(transit)) {
        res[[untransitThing(transit[[i]], cache)]] <- untransitThing(transit[[i+1]], cache) #cache*2
        i <- i+2
    }
    return(res)
}



module Examples

-- Walk
toCaps : Inline Prime -> Inline Prime
toCaps (Text str) = Text $ toUpper str
toCaps (Sans str) = Sans $ toUpper str
toCaps (Scap str) = Scap $ toUpper str
toCaps (Mono str) = Mono $ toUpper str
toCaps x             = x

modHeader : Block Prime -> Block Prime
modHeader (Header Prime n l xs) = if n <= 3
                                    then Para Prime (map toCaps xs)
                                    else Header Prime n l xs
modHeader x = x

modHeaders : EddaDoc -> EddaDoc
modHeaders = walk modHeader

-- Query

extractURL : Inline Prime -> List String
extractURL (Hyper uri _) = [uri]
extractURL _            = Nil

getURLs : EddaDoc -> List String
getURLs = query extractURL

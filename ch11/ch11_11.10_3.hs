type AuthorName = String
data Author =
      Fiction AuthorName
    | Nonfiction AuthorName
    deriving (Eq, Show)

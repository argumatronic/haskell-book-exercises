stops = "pbtdkg"
vowels = "aeiou"

-- a)
svs stops vowels = [(s,v,s') | s <- stops, v <- vowels, s' <- stops]

-- b)
svsP stops vowels = [(s,v,s') | s <- stops, v <- vowels, s' <- stops, s == 'p']

-- c)
nouns = ["apple", "banana", "orange"]
verbs = ["turns", "peals", "eats"]
nvn nouns verbs = [(n,v,n') | n <- nouns, v <- verbs, n' <- nouns]

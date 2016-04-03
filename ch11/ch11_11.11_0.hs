data OperatingSystem = GnuPlusLinux
                      | OpenBSDPlusNevermindJustBSDStill
                      | Mac
                      | Windows
                      deriving (Eq, Show)

data ProgrammingLanguage =  Haskell
                            | Agda
                            | Idris
                            | PureScript
                            deriving (Eq, Show)

data Programmer = Programmer { os :: OperatingSystem
                             , lang :: ProgrammingLanguage }
                             deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
    [ GnuPlusLinux
    , OpenBSDPlusNevermindJustBSDStill
    , Mac
    , Windows
    ]

allLanguages :: [ProgrammingLanguage]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = [Programmer {os = o, lang = l} | o <- allOperatingSystems, l <- allLanguages]

-- this should be True to be correct
passed = length allProgrammers == length allOperatingSystems * length allLanguages
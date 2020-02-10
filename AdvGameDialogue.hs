

------------------------- 

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x <  y    = x : merge    xs (y:ys)
    | x == y    = x : merge    xs    ys
    | otherwise = y : merge (x:xs)   ys

mergeFlp :: Ord a => [a] -> [a] -> [a]
mergeFlp xs ys = merge ys xs

minus :: Ord a => [a] -> [a] -> [a]
minus xs [] = xs
minus [] ys = []
minus (x:xs) (y:ys)
    | x <  y    = x : minus    xs (y:ys)
    | x == y    =     minus    xs    ys
    | otherwise =     minus (x:xs)   ys

msort :: Ord a => [a] -> [a]
msort []  = []
msort [x] = [x]
msort xs  = msort (take n xs) `merge` msort (drop n xs)
  where
    n = length xs `div` 2

type Node = Int
type Map  = [(Node,Node)]

bothWays :: Node -> Map -> [Node]
bothWays _ [] = []
bothWays n ((x,y):xs)
    | n == x    = y : bothWays n xs
    | n == y    = x : bothWays n xs
    | otherwise =     bothWays n xs

type Location  = String
type Character = String

type Party = [Character]


------------------------- PART 1: Events

data Game  = Won
    |Game Node Party [Party]
  deriving (Eq,Show)

type Event = Game -> Game


start :: Game
start =  Game 6 [] characters

end :: Game
end = Won

applyAt :: Int -> (a -> a) -> [a] -> [a]
applyAt i f (x:xs)
    | i == 0 = (f x) : xs
    | i > (length xs) = error "index exceeds bounds of list"
    | otherwise = x : applyAt (i-1) f xs

 
updateAt :: Node -> Party -> Party -> Event
updateAt m xs ys (Game n p ps) = Game n p (applyAt m (mergeFlp ys) (applyAt m (minus xs) ps))
updateAt m xs ys Won = Won

update :: Party -> Party -> Party -> Event
update xs ys zs (Game n p ps) = Game n (mergeFlp ys (minus p xs)) (applyAt n (mergeFlp zs) (applyAt n (minus xs) ps))
update xs ys zs Won = Won

------------------------- PART 2: Dialogues

data Dialogue = End     String
              | Choice  String  [( String , Dialogue )]
              | Action String Event

exitWords = ["X","x","Q","q","Exit","exit","Quit","quit"]

enumerate :: Int -> [String] -> String
enumerate n xs = unlines [ "  " ++ show i ++ ". " ++ x | (i,x) <- zip [n..] xs ]

dialogue :: Game -> Dialogue -> IO Game
dialogue Won (End str) = do
   putStrLn str
   return Won
dialogue Won (Action str event) = do
   putStrLn str
   return Won
dialogue Won (Choice str1 xs) = do
   putStrLn str1 
   let displayedList = (enumerate 1 (map fst xs))
   putStrLn displayedList
   input <- getLine
   if (input `elem` exitWords)
    then do
        return Won
    else do 
        let i = read input :: Int
        dialogue Won (snd (xs !! (i-1)))
        return Won
dialogue (Game n p ps) (End str) = 
    do
        putStrLn str
        return (Game n p ps)
dialogue (Game n p ps) (Action str event) = 
    do
        putStrLn str 
        return (event (Game n p ps))
dialogue (Game n p ps) (Choice str1 xs) = do
    putStrLn str1
    let displayedList = (enumerate 1 (map fst xs))
    putStrLn displayedList
    input <- getLine
    if (input `elem` exitWords)
    then do
        return (Game n p ps)
    else do 
        let i = read input :: Int
        dialogue (Game n p ps) (snd (xs !! (i-1)))

findDialogue :: Party -> Dialogue
findDialogue [] = End "There is nothing we can do"
findDialogue p = 
    if length (map snd (filter (\(x,y) -> x == p) dialogues)) == 0
    then do
       End "There is nothing we can do"
    else do
       head (map snd (filter (\(x,y) -> x == p) dialogues))


------------------------- PART 3: The game loop
loop :: Game -> IO ()
loop (Game n p ps) = do
    let ns = bothWays n theMap -- nodes
    let xs = map (locations !!) ns  -- locations at nodes
    let cs = (characters !! n) -- characters at node
    let ys = p -- party
    let i  = length ns + 1
    putStrLn ("You are in "         ++ (locations !! n))
    putStr   ("You can travel to\n" ++ enumerate 1 xs)
    putStr   ("With you are\n"      ++ enumerate i ys)
    putStr   ("You can see\n"       ++ enumerate i cs)
    putStrLn  "What will you do?"
    str <- getLine
    if str `elem` exitWords
    then do
        return ()
    else do 
        let is = map read (words str) :: [Int]
        if (length is == 1 && all (<= length ns) is)
        then do
           let m = (ns !! (read str - 1))
           loop (Game m p ps)
        else do
           let js = msort (merge (characters !! (is !! 1)) (characters !! (is !! 2)))
           let d = findDialogue js
           g <- dialogue (Game n p ps) d
           loop g

game :: IO ()
game = loop start

------------------------- PART 4: Solving the game


-- talk' :: Dialogue -> [(Event,[Int])]
talk' = undefined

-- talk :: Dialogue -> [(Event,String)]
talk = undefined

{-
event :: String -> Event
event s _ = Game 0 ["Event: " ++ s] []

testDialogue :: Dialogue
testDialogue = Choice "Morpheus opens his palms"
 [("Take the blue pill", Action "" (event "You wake up in bed"))
 ,("Take the red pill",  Action "" (event "You are a battery"))]

testTalk' :: [(Game,[Int])]
testTalk' = [ (e Won,xs) | (e,xs) <- talk' testDialogue]

testTalk :: [(Game,String)]
testTalk = [ (e Won,str) | (e,str) <- talk testDialogue]
-}

-------------------------

extend :: Map -> (Node,[Int]) -> [(Node,[Int])]
extend = undefined

travel' :: Map -> [(Node,[Int])] -> [(Node,[Int])] -> [(Node,[Int])]
travel' = undefined

travel :: Map -> Game -> [(Game,String)]
travel = undefined

-------------------------

act :: Game -> [(Game,String)]
act = undefined

-- suitable :: Game -> Event -> Bool
suitable = undefined

solve :: IO ()
solve = undefined
  where
    --solveLoop :: Game -> String
    --solveLoop :: (Game,String) -> String



------------------------- Game data

characters :: [Party]
characters =
  [ ["Duke"]
  , ["Portal Gun"]
  , ["Priest"]
  , ["Lee"]
  , ["Chell","Cortana","Mario","Master Chief"]
  , ["Team Rocket"]
  , ["Peach","Rochelle"]
  ]

locations :: [Location]
locations =
  [ "You are not supposed to be here" -- 0
  , "Aperture Science" -- 1
  , "Church of Halo"   -- 2
  , "Macon"            -- 3
  , "Nintendo Land"    -- 4
  , "Pallet Town"      -- 5
  , "Princess Castle"  -- 6
  ]

theMap :: Map
theMap = [(1,5), (2,4), (2,6), (3,5), (4,5), (4,6)]


dialogues :: [(Party,Dialogue)]
dialogues =
 [ (["Mario"] , Choice "I need to save the Princess."
     [("Sure." ,          Action "Let's go." (update ["Mario"] ["Mario"] []))
     ,("Not right now." , Action "Ok."       (update ["Mario"] [] ["Mario"]))
     ])
 , (["Mario","Peach"] , Choice "Save me, Mario!"
    [("Sure." , Action "Thank you for bringing me my hero. Now I can conveniently leave this hat behind." (update ["Mario","Peach"] [] ["Baseball Cap"]))
    ,("Not right now." , End "Mario, pls.")])
 , (["Peach"] , End "That's *Princess* Peach to you, please. And where's my Mario?")
 , (["Master Chief"] , Choice "I want to marry Cortana. Can you escort us to the Church of Halo?"
     [("Sure." ,          Action "Let's go." (update ["Master Chief"] ["Master Chief"] []))
     ,("Not right now." , Action "Ok."       (update ["Master Chief"] [] ["Master Chief"]))
     ])
 , (["Cortana"] , Choice "I must go with Master Chief."
     [("Sure." ,          Action "Let's go." (update ["Cortana"] ["Cortana"] []))
     ,("Not right now." , Action "Ok."       (update ["Cortana"] [] ["Cortana"]))
     ])
 , (["Master Chief","Priest"] , End "I can't marry you without your bride-to-be.")
 , (["Cortana","Priest"] , End "I can't marry you without your husband-to-be.")
 , (["Priest"] , Choice "Welcome, my child. Have you accepted Master Chief as your savior?"
     [("Hail Master Chief (Blessed Be His Name)" , End "")])
 , (["Cortana","Master Chief","Priest"] , Choice "Do you, Master Chief, accept Cortana to be your beloved bride?" 
      [("I don't", End "The Wedding is cancelled"),
       ("I do", Choice "And do you, Cortana, take Master Chief to be your beloved Husband?" 
        [("I don't", End "The Wedding is cancelled"),
         ("I do", Action "What a beautiful wedding said the bridesmaid to the waiter. But what a shame, that there's some child lurking nearby." (update ["Cortana","Master Chief","Priest"] [] ["Clementine (hiding)"]) )
   ])])
 , (["Baseball Cap"] , Choice "It's a bit grubby, shall I take it?"
     [("Sure." ,          Action "Let's go." (update ["Baseball Cap"] ["Baseball Cap"] []))
     ,("Not right now." , Action "Ok."       (update ["Baseball Cap"] [] ["Baseball Cap"]))
     ])
 , (["Clementine (hiding)"] , End "I'm scared. Where are my parents?")
 , (["Baseball Cap", "Clementine (hiding)"] , Choice "Give the girl the hat?"
    [("Sure." , Action "I feel safe." (update ["Baseball Cap","Clementine (hiding)"] ["Clementine"] []))
    ,("Not right now." , End "")
    ])
 , (["Duke"] , End "Time to k*** a** and chew bubble gum. And I'm all outta gum.")
 , (["Clementine"] , Choice "Will you help me find my parents?"
      [("What do they look like?", Choice "My father's name is Lee" 
        [("I asked what do they look like!", End "Sorry")
        ,("I know him, Let's go!", Action "Yay!" (update ["Clementine"] ["Clementine"] []))
      ])
      ,("Do you know your address?", Choice "I can't remember, I think it rhymes with Bacon" 
        [("How do you not know your own address?", End "Sorry!"),
         ("Are you thinking of Macon?", Choice "Yes! That's it! Do you know where it is?"
          [("Sure, I can take you there", Action "Yay!" (update ["Clementine"] ["Clementine"] []))
          ,("I don't know how to get there", End "Okay then")
          ])
        ,("Are you hungry?", Choice "Yes! Do you have any chocolate?" 
          [("I don't", End "Okay")
          ,("I can go find some", Action "Thanks, I'll stay here in case my parents come back" (update ["Clementine"] [] ["Clementine"]))
   ])])])
 , (["Clementine","Lee"] , Choice "GIVE ME BACK CLEMENTINE!"
     [("Sure...", Action "" (update ["Clementine","Lee"] ["Zombie Lee"] []))
     ])
 , (["Lee"] , End "Clem? Clem, where are you?!")
 , (["Zombie Lee"] , Choice "Uuurrurrhgghghhghgg."
     [("This way." ,  Action "Urg" (update ["Zombie Lee"] ["Zombie Lee"] []))
     ,("Not today." , Action "Hhuuuurgh" (update ["Zombie Lee"] [] ["Zombie Lee"]))
     ])
 , (["Rochelle"] , End "Girl, you should pray there aren't no Zombies around.")
 , (["Rochelle", "Zombie Lee"] , Action "What?! A zombie? You've left me for dead!" (update ["Rochelle","Zombie Lee"] [] ["Pikachu"]))
 , (["Chell"] , Choice "I've just got a volunteering position at Aperture Science. Can you help me find it? I'm not good with directions."
     [("This way." ,  Action "" (update ["Chell"] ["Chell"] []))
     ,("Not today." , Action "" (update ["Chell"] [] ["Chell"]))
    ])
 , (["Chell","Portal Gun"] , Action "This is your fault. It didn't have to be like this. I'm not kidding, now! Turn back, or I will kill you! I'm going to kill you, and all the cake is gone! You don't even care, do you? This is your last chance! ." (update ["Chell","Portal Gun"] [] [] . updateAt 4 ["Team Rocket"] ["Ash"]))
 , (["Team Rocket"] , End "Oh, prepare for trouble, that's what they should do. And make it double, we're grabbing Pikachu.") 
 , (["Pikachu"] , Choice "Pika-Pika"
     [("*throw pokeball*"  , Action "" (update ["Pikachu"] ["Pikachu"] []))
     ,("Nope." ,             Action "" (update ["Pikachu"] [] ["Pikachu"]))
     ])
 , (["Ash", "Pikachu"] , Action "You win." (\_ -> Won))
 , (["Pikachu","Team Rocket"] , End "Hey, look at this! Get a load! Let's grab- ALL GLORY TO THE HYPNOTOAD")
 , (["Portal Gun"] , End "I am an inanimate object. What did you expect?")
 ]


{-# LANGUAGE TemplateHaskell #-}
-- | 

module Main where
    
import Control.Lens
import System.Random
import Data.List
import qualified Data.Map as Map
import Control.Monad.Free
import Control.Monad.State

data Card = 
    CardZero
    | CardOne
    | CardTwo
    | CardThree
    | CardQuit
    deriving (Show, Eq, Ord)
      
type Deck = [Card]
    
initDeck :: Deck
initDeck = [CardZero, CardZero
           , CardOne, CardOne, CardOne
           , CardTwo, CardTwo
           , CardThree
           , CardQuit, CardQuit, CardQuit
           ]

type Gen a = State StdGen a
range :: Random a => (a, a) -> State StdGen a
range r = do
  g <- get
  let (v, g') = randomR r g
  put g'
  return v
         
number :: Random a => State StdGen a
number = do
  g <- get
  let (a, g') = random g
  put g'
  return a

shuffleDeck :: Deck -> StdGen -> (Deck, StdGen) -- StdGen, Deck)
shuffleDeck deck stdGen = runState (do
                           n <- number
                           return $ (permutations deck) !! n
                                  ) stdGen
                          
data Player = Player { _playerDeck :: Deck } deriving (Show)
makeLenses ''Player
_removeCard :: Player -> Card -> Maybe Player
_removeCard p c = 
    case find (==c) (p^.playerDeck) of
       Just _ -> Just $ p & playerDeck %~ delete c 
       Nothing -> Nothing

type PlayerName = String
data Game = Game
          { _gamePlayers :: Map.Map PlayerName Player
          , _mainDeck :: Deck
          , _playerTurn :: [PlayerName]
          , _rndGen :: StdGen
          } deriving (Show)
          
newGame = Game { _gamePlayers = Map.fromList [("Player1", Player initDeck), ("Player2", Player initDeck)]
               , _mainDeck = []
               , _playerTurn = ["Player1", "Player2"]
               , _rndGen = mkStdGen 1
               }

makeLenses ''Game
          
type GameS a = State Game a
            
selectExistingCard' :: Player -> State StdGen Card
selectExistingCard' p = do
  n <- range (0, (length $ p^.playerDeck) - 1)
  return $ (p^.playerDeck) !! n
         
selectExistingCard :: (Monad m) => PlayerName -> StateT Game m Card
selectExistingCard pName = do
  Just p <- use $ gamePlayers .at pName
  gen <- use rndGen
  let (card, g') = runState (selectExistingCard' p) gen
  rndGen .= g'
  return card
   
mkNextPlayer :: (Monad m) => StateT Game m PlayerName
mkNextPlayer = do
  player <- use $ playerTurn._head
  playerTurn %= \p -> tail p ++ [player]
                      
  return player
         
isPlayerTurn :: PlayerName -> StateT Game IO Bool
isPlayerTurn playerName = do
  b <- fmap (playerName ==) $ use $ (playerTurn._head)
  return b

putCard :: PlayerName -> Card -> StateT Game IO (Either String PlayerName)
putCard playerName c = do
  g <- get
  lift $ print (g, playerName)
  isTurn <- isPlayerTurn playerName
  if isTurn 
    then do
        Just player <- use $ gamePlayers.at playerName
        let mPlayer = _removeCard player c
        case mPlayer of
          Just player -> do
                       gamePlayers.at playerName .= Just player
                       nPlayer <- mkNextPlayer
                       return (Right nPlayer)
          Nothing -> do
            return (Left "Select another card")
    else return (Left "Not player turn")
               
testGame = do
  let g = newGame
  g' <- execStateT (do
              foldM (\player _ -> do
                       card <- selectExistingCard player
                       next <- putCard player card
                       case next of 
                         Left err -> lift $ print err >> return player
                         Right next -> return next
                       ) "Player1" [0..4]
              ) g

  print g'

main = do
  testGame

module RCommand
  (RCommand, execRCommand)
where

data RCommand = Say String
                deriving (Read, Show)

execRCommand :: RCommand -> IO ()
execRCommand (Say str) = putStrLn ("Remote: " ++ str)

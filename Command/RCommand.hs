module RCommand
  (RCommand, execRCommand)
where

data RCommand = Say String
                deriving Read

execRCommand :: RCommand -> IO ()
execRCommand (Say str) = putStrLn ("Remote: " ++ str)

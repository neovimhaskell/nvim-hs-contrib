
module Neovim.User.Output where

import Neovim
import Neovim.API.String
import Neovim.User.Choice

import System.Directory

-- | Invokes the substitute command with given parameters
substitute :: (Int, Int) -> String -> String -> [String] -> Neovim env ()
substitute (start,end) ptn replacement flags = vim_command 
  $  show start
  ++ ","
  ++ show end
  ++ "substitute/"
  ++ ptn ++ "/"
  ++ replacement ++ "/"
  ++ mconcat flags


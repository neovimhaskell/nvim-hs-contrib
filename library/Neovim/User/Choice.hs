{-# LANGUAGE LambdaCase #-}
{- |
Module      :  Neovim.User.Choice
Description :  Ask the user for an answer
Copyright   :  (c) Sebastian Witte
License     :  Apache-2.0

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  GHC

-}
module Neovim.User.Choice
    where

import           Neovim

import           Data.Char                    (toLower)
import           Data.List                    (isPrefixOf)
import           Text.PrettyPrint.ANSI.Leijen as P hiding ((<$>))


-- | Call @inputlist()@ on the neovim side and ask the user for a choice. This
-- function returns 'Nothing' if the user input was invalid or 'Just' the chosen
-- element. The elements are rendered via 'Pretty'.
oneOf :: Pretty a => [a] -> Neovim r st (Maybe a)
oneOf cs = fmap (\i -> cs !! (i-1)) <$> askForIndex (zipWith mkChoice cs [1..])
  where
    mkChoice c i = toObject $ int i P.<> text "." <+> pretty c


-- | Ask user for a choice and 'Maybe' return the index of that choice
-- (1-based).
askForIndex :: [Object] -> Neovim r st (Maybe Int)
askForIndex cs = vim_call_function "inputlist" [ObjectArray cs] >>= \case
    Left e ->
        (err . text . show) e

    Right a -> case fromObject a of
        Right i | i >= 1 && i <= length cs ->
            return $ Just i

        Right _ ->
            return Nothing

        Left e ->
            err e


-- | Same as 'oneOf' only that @a@ is constrained by 'Show' insted of 'Pretty'.
oneOfS :: Show a => [a] -> Neovim r st (Maybe a)
oneOfS cs = fmap (\i -> cs !! (i-1)) <$> askForIndex (zipWith mkChoice cs [1..])
  where
    mkChoice c i = toObject $ show (i :: Int) ++ ". " ++ show c


-- | Open @inputdialog@s inside neovim until the user has successfully typed any
-- prefix of @yes@ or @no@ or alternatively aborted the dialog. Defaults to
-- @yes@ for the empty input.
yesOrNo :: String -- ^ Question to the user
        -> Neovim r st Bool
yesOrNo message = do
    spec <- vim_call_function
                "inputdialog" $ (message ++ " (Y/n) ") +: "" +: "no" +: []
    case fmap fromObject spec of
        Right (Right s) | map toLower s `isPrefixOf` "yes" ->
            return True

        Right (Right s) | map toLower s `isPrefixOf` "no" ->
            return False

        Right (Left e) ->
            err e

        _ ->
            yesOrNo message


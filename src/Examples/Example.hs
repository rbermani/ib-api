-- |
-- Module  : IB.Client
-- License : GPL3
-- Author : Robert Bermani <bobbermani@gmail.com>
-- Stability : experimental
-- |

-- Main Loop example

module Examples.Example where

import IB.Client
{--
handleMsg :: MIB -> IBMessage -> IO ()
handleMsg msv ibMsg =

nothreadEx = do let cconf = defaultConf { cc_handler = Just handleMsg } 
                result <- connect cconf False False
                case result of 
                 Left err ->
                 Right msv -> do s <- readMVar msv
                                 unless (not s_connected s) 
                                    do checkMsg False 

threadEx = do let cconf = defaultConf { cc_handler = Just handleMsg } 
                  result <- connect cconf True False
                  case result of 
                   Left err ->
                   Right msv -> do s <- readMVar msv

                  unless (not s_connected s) checkMsg False 

main = 
--}

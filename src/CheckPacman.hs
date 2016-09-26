{-|
Module      : CheckPacman
License     : PublicDomain

Maintainer  : daveparrish@tutanota.com
Stability   : experimental
Portability : untested

Nagios monitoring plugin for available Pacman updates

> check_pacman - Nagios monitoring plugin for available Pacman updates
> 
> Usage: check_pacman [-w|--warning INT] [-c|--critical INT]
>   Return Nagios formatted string based on available Pacman updates.
> 
> Available options:
>   -h,--help                Show this help text
>   -w,--warning INT         Return warning if greater than INT and less than
>                            critical INT. Default is 1.
>   -c,--critical INT        Return critical if greater than INT. Default is 20.
-}

module CheckPacman where

import System.Nagios.Plugin
import Options.Applicative
import System.Process
import System.Exit
import Data.Text

type PackageUpdate = String

data PluginOptions = PluginOptions
  { warningThreshold :: Int
  , criticalThreshold :: Int }

parsePacmanUpdates :: String -> [PackageUpdate]
parsePacmanUpdates = Prelude.words

pluginOptions :: Parser PluginOptions
pluginOptions = PluginOptions
  <$> option auto
      ( long "warning"
     <> short 'w' 
     <> metavar "INT"
     <> value 1
     <> help "Return warning if greater than INT and less than critical INT.\
             \ Default is 1." )
  <*> option auto
      ( long "critical"
     <> short 'c' 
     <> metavar "INT"
     <> value 20
     <> help "Return critical if greater than INT.\
             \ Default is 20." )

addUpdatePerfData :: Int -- ^ Number of updates
                  -> PluginOptions
                  -> NagiosPlugin ()
addUpdatePerfData n (PluginOptions w c) =
  addPerfDatum (pack "Updates")
               (IntegralValue (fromIntegral n))
               NullUnit
               (Just $ IntegralValue 0)
               (Just $ IntegralValue 200)
               (Just $ IntegralValue $ fromIntegral w)
               (Just $ IntegralValue $ fromIntegral c)

execCheck :: PluginOptions -> IO ()
execCheck o = do
  (_,_,_) <- readProcessWithExitCode "pacman" ["-Sy"] []
  (code,out,_) <- readProcessWithExitCode "pacman" ["-Qu"] []
  putStrLn $ show code
  let updates = parsePacmanUpdates out
  let num_of_updates = Prelude.length updates

  runNagiosPlugin $
    if num_of_updates > 0
    then do
      addResult Critical $
        pack $ (show num_of_updates) ++ " packages have available updates.\n" ++
        ((Prelude.unwords) updates)
      addUpdatePerfData num_of_updates o
    else
      if code == (ExitFailure 1)
      then do addResult OK $ pack "Installed packages are up-to-date."
              addUpdatePerfData num_of_updates o
      else addResult Unknown $
        pack $ "Pacman exited with '" ++ (show code) ++ "'." 

mainExecParser :: IO ()
mainExecParser = execParser opts >>= execCheck
  where
    opts = info (helper <*> pluginOptions)
      ( fullDesc
     <> progDesc "Return Nagios formatted string based on available Pacman updates."
     <> header "check_pacman - Nagios monitoring plugin for available Pacman updates" )

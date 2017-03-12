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
import qualified Data.Text as T

type PackageUpdate = String

data PluginOptions = PluginOptions
  { warningThreshold :: Int
  , criticalThreshold :: Int }

parsePacmanUpdates :: String -> [PackageUpdate]
parsePacmanUpdates = lines

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

addPluginPerfData :: Int -- ^ Number of updates
                  -> PluginOptions
                  -> NagiosPlugin ()
addPluginPerfData n (PluginOptions w c) =
  addPerfDatum (T.pack "Updates")
               (IntegralValue (fromIntegral n))
               NullUnit
               (Just $ IntegralValue 0)
               (Just $ IntegralValue 200)
               (Just $ IntegralValue $ fromIntegral w)
               (Just $ IntegralValue $ fromIntegral c)

execCheck :: PluginOptions -> IO ()
execCheck o = do
  (_,out,_) <- readProcess "pacman" ["-Sy"] []
    >> readProcessWithExitCode "pacman" ["-Qu"] []
  let updates = parsePacmanUpdates out
  let num_updates = length updates

  runNagiosPlugin $ do
    addPluginResult num_updates updates o
    addPluginPerfData num_updates o
  where
    addPluginResult :: Int -> [PackageUpdate] -> PluginOptions -> NagiosPlugin ()
    addPluginResult n u (PluginOptions w c)
      | n == 0    = do
          addResult OK $ T.pack "Installed packages are all up-to-date."
      | n < w     = addUpdatesResult n u OK
      | n >= w && n < c
                  = addUpdatesResult n u Warning
      | otherwise = addUpdatesResult n u Critical
    addUpdatesResult :: Int -> [PackageUpdate] -> CheckStatus -> NagiosPlugin ()
    addUpdatesResult n u cs = addResult cs $
      T.pack ((show n)
      ++ " packages have available updates.\n"
      ++ ((unlines) u))

mainExecParser :: IO ()
mainExecParser = execParser opts >>= execCheck
  where
    opts = info (helper <*> pluginOptions)
      ( fullDesc
     <> progDesc "Return Nagios formatted string based on available Pacman updates."
     <> header "check_pacman - Nagios monitoring plugin for available Pacman updates" )

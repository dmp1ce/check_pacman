import Test.Tasty
import Test.Tasty.HUnit
--import Test.Tasty.SmallCheck

import CheckPacman

main :: IO ()
main = defaultMain $ testGroup "all-tests" Main.tests

tests :: [TestTree]
tests =
  [ testGroup "Check Pacman" parseTests ]

parseTests :: [TestTree]
parseTests =
  [ testCase "'pacman -Qu' properly parsed" pacman_updates_parsed ]

pacman_updates_parsed :: Assertion
pacman_updates_parsed =
  (length $ parsePacmanUpdates examplePacmanUpdatesData) @?= 112

examplePacmanUpdatesData :: String
examplePacmanUpdatesData = "mate-settings-daemon 1.14.0-1 -> 1.14.1-1\
\mate-terminal 1.14.0-1 -> 1.14.1-1\
\mesa 12.0.2-1 -> 12.0.3-1\
\mumble 1.2.16-2 -> 1.2.17-1\
\nmap 7.12-1 -> 7.12-2\
\nodejs 6.5.0-1 -> 6.6.0-1\
\npm 3.10.7-1 -> 3.10.8-1\
\openssl 1.0.2.h-1 -> 1.0.2.i-1\
\packagekit 1.1.3-2 -> 1.1.4-1\
\perl 5.24.0-1 -> 5.24.0-2\
\php 7.0.10-1 -> 7.0.11-1\
\php-gd 7.0.10-1 -> 7.0.11-1\
\php-intl 7.0.10-1 -> 7.0.11-1\
\pinentry 0.9.7-2 -> 0.9.7-3\
\python-cffi 1.7.0-1 -> 1.8.3-1\
\python-cryptography 1.5-1 -> 1.5.1-1\
\python-setuptools 1:27.1.2-1 -> 1:27.3.0-1\
\python2-cffi 1.7.0-1 -> 1.8.3-1\
\python2-cryptography 1.5-1 -> 1.5.1-1\
\python2-nbxmpp 0.5.3-2 -> 0.5.4-1\
\python2-psutil 4.3.0-1 -> 4.3.1-1\
\python2-pybluez 0.20-4 -> 0.22-1\
\python2-qrcode 5.1-2 -> 5.3-1\
\python2-setuptools 1:27.1.2-1 -> 1:27.3.0-1\
\python2-sip 4.18-1 -> 4.18.1-1\
\shared-mime-info 1.6-2 -> 1.7-1\
\sip 4.18-1 -> 4.18.1-1\
\sqlite 3.14.1-1 -> 3.14.2-1\
\stack 1.1.2-23 -> 1.2.0-2\
\syncthing 0.14.6-1 -> 0.14.7-1\
\tor 0.2.8.7-1 -> 0.2.8.8-1\
\vim-runtime 7.4.2334-1 -> 8.0.0005-1\
\virtualbox 5.1.4-1 -> 5.1.6-1\
\virtualbox-host-dkms 5.1.4-1 -> 5.1.6-1\
\wayland 1.11.0-1 -> 1.12.0-1\
\xproto 7.0.29-1 -> 7.0.31-1\
\youtube-dl 2016.09.11.1-1 -> 2016.09.24-1"

# check_pacman
Nagios monitoring plugin for checking for available Pacman package updates

```
check_pacman - Nagios monitoring plugin for available Pacman updates

Usage: check_pacman [-w|--warning INT] [-c|--critical INT]
  Return Nagios formatted string based on available Pacman updates.

Available options:
  -h,--help                Show this help text
  -w,--warning INT         Return warning if greater than INT and less than
                           critical INT. Default is 1.
  -c,--critical INT        Return critical if greater than INT. Default is 20.
```

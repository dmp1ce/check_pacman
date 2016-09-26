# check_pacman
Nagios monitoring plugin for checking for available Pacman package updates

## Install
Install using this [AUR](https://aur.archlinux.org/packages/check_pacman-git/) package.

## Usage

`check_pacman` will first run `pacman -Sy` and the `pacman -Qu` to get the latest available packages. You will probably want to run `check_pacman` as `root` user so that pacman can refresh the package database.

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

## Example

`sudo check_pacman -w 5 -c 100`

module github.com/symbiont-io/detsys/debugger/cmd

go 1.15

replace github.com/symbiont-io/detsys/lib => ../../lib

replace github.com/symbiont-io/detsys/debugger => ../

require (
	github.com/gdamore/tcell/v2 v2.0.1-0.20201017141208-acf90d56d591
	github.com/nsf/jsondiff v0.0.0-20200515183724-f29ed568f4ce
	github.com/rivo/tview v0.0.0-20201018122409-d551c850a743
	github.com/symbiont-io/detsys/debugger v0.0.0-00010101000000-000000000000
	github.com/symbiont-io/detsys/lib v0.0.0-00010101000000-000000000000
)

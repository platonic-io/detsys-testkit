package main

import (
	"fmt"
	"github.com/gdamore/tcell/v2"
	"github.com/nsf/jsondiff"
	"github.com/rivo/tview"
	"os"
	"sort"
	"strconv"
	"time"

	"github.com/symbiont-io/detsys-testkit/detsys-debug/internal"
	"github.com/symbiont-io/detsys-testkit/lib"
)

func min(x, y int) int {
	if x > y {
		return y
	}
	return x
}

var textView = tview.NewTextView().
	SetWrap(false).
	SetDynamicColors(true)
var w = tview.ANSIWriter(textView)

var diagram = tview.NewTextView().
	SetWrap(false).
	SetDynamicColors(true)
var w2 = tview.ANSIWriter(diagram)

var componentsWidget = tview.NewList()

var messageView = tview.NewTextView().
	SetWrap(false).
	SetDynamicColors(true)
var wMessageView = tview.ANSIWriter(messageView)

var logView = tview.NewTextView().
	SetWrap(false).
	SetDynamicColors(true)
var wLogView = tview.ANSIWriter(logView)

type DebugApplication struct {
	testId          lib.TestId
	runId           lib.RunId
	heaps           []map[string][]byte
	diagrams        [][]byte
	events          []debugger.NetworkEvent
	components      []string
	activeRow       int // should probably be logic time
	activeComponent int
}

func (da *DebugApplication) setComponent(component string) {
	for i, x := range da.components {
		if x == component {
			da.activeComponent = i
			componentsWidget.SetCurrentItem(i)
			break
		}
	}
}

func (da *DebugApplication) setRow(row int) {
	nrow := debugger.Max(1, row)
	da.activeRow = nrow

	to := da.events[nrow-1].To

	da.setComponent(to)
}

func (da *DebugApplication) redraw() {
	textView.Clear()
	diagram.Clear()
	messageView.Clear()
	logView.Clear()
	row := da.activeRow
	fmt.Fprintf(w2, "%s", string(da.diagrams[row-1]))
	component := da.components[da.activeComponent]
	old := da.heaps[row-1][component]
	new := da.heaps[min(row, len(da.heaps))][component]

	opts := jsondiff.DefaultConsoleOptions()
	opts.Indent = "  "
	_, strdiff := jsondiff.Compare(old, new, &opts)
	fmt.Fprintf(w, "%s", strdiff)

	event := da.events[row-1]
	fmt.Fprintf(wMessageView, "%s", string(event.Args))

	logs := debugger.GetLogMessages(da.testId, da.runId, component, event.At)
	for _, log := range logs {
		fmt.Fprintf(wLogView, "%s\n", string(log))
	}
}

func MakeDebugApplication(testId lib.TestId, runId lib.RunId) *DebugApplication {
	heaps := debugger.Heaps(testId, runId)
	events := debugger.GetNetworkTrace(testId, runId)
	diagrams := debugger.SequenceDiagrams(testId, runId)

	components := make([]string, 0, len(heaps[0])) // lol
	for component := range heaps[0] {
		components = append(components, component)
	}

	to := events[0].To
	ac := 0
	for i, x := range components {
		if x == to {
			ac = i
			break
		}
	}

	sort.Strings(components)
	return &DebugApplication{
		testId:          testId,
		runId:           runId,
		heaps:           heaps,
		diagrams:        diagrams,
		events:          events,
		components:      components,
		activeRow:       1,
		activeComponent: ac,
	}
}

func main() {
	testId, err := strconv.Atoi(os.Args[1])
	if err != nil {
		panic(err)
	}
	runId, err := strconv.Atoi(os.Args[2])
	if err != nil {
		panic(err)
	}

	app := tview.NewApplication()

	da := MakeDebugApplication(lib.TestId{testId}, lib.RunId{runId})

	textView.SetBorderPadding(1, 1, 2, 0).SetBorder(true).SetTitle("Component state")
	messageView.SetBorderPadding(1, 1, 2, 0).SetBorder(true).SetTitle("Current Message")
	diagram.SetBorderPadding(1, 1, 2, 0).SetBorder(true).SetTitle("Sequence diagram")
	logView.SetBorderPadding(1, 1, 2, 0).SetBorder(true).SetTitle("Log")

	da.redraw()

	table := tview.NewTable().
		SetFixed(1, 1)

	for column, header := range []string{"Event", "From", "To", "At", "Time"} {
		tableCell := tview.NewTableCell(header).
			SetSelectable(false).
			SetTextColor(tcell.ColorYellow).
			SetAlign(tview.AlignLeft)
		table.SetCell(0, column, tableCell)
	}
	for row, event := range da.events {
		for column, cell := range []string{"Message", "From", "To", "At", "Time"} {

			var tableCell *tview.TableCell
			switch cell {
			case "Message":
				tableCell = tview.NewTableCell(event.Message)
			case "From":
				tableCell = tview.NewTableCell(event.From)
			case "To":
				tableCell = tview.NewTableCell(event.To)
			case "At":
				tableCell = tview.NewTableCell(strconv.Itoa(event.At))
			case "Time":
				tableCell = tview.NewTableCell(event.Simulated.Format(time.StampNano))
			}
			if event.Dropped {
				tableCell.SetTextColor(tcell.ColorGray)
			}
			table.SetCell(row+1, column, tableCell)
		}
	}
	table.SetBorder(true).SetTitle("Events")
	table.SetSelectable(true, false)
	table.SetSelectionChangedFunc(
		func(row, column int) {
			da.setRow(row)
			da.redraw()
		})

	componentsWidget.SetChangedFunc(func(index int, _mainText string, _secondaryText string, shortcut rune) {
		da.activeComponent = index
		da.redraw()
	})

	for i, c := range da.components {
		theRune := ' '

		if i < 9 {
			theRune = rune(i + 49) // 48 is '0', but we skip that one
		}
		componentsWidget.AddItem(c, "", theRune, nil)
	}

	stateWidget := tview.NewFlex().
		SetDirection(tview.FlexColumn).
		AddItem(componentsWidget, 10, 0, false).
		AddItem(textView, 0, 4, false)

	layout := tview.NewFlex().
		SetDirection(tview.FlexRow).
		AddItem(tview.NewFlex().
			AddItem(tview.NewFlex().
				SetDirection(tview.FlexRow).
				AddItem(stateWidget, 0, 1, false).
				AddItem(table, 20, 1, false), 0, 1, false).
			AddItem(diagram, 0, 1, false), 0, 20, false).
		AddItem(messageView, 5, 1, false).
		AddItem(logView, 10, 1, false)

	app.SetInputCapture(func(event *tcell.EventKey) *tcell.EventKey {
		if event.Rune() == 'q' {
			app.Stop()
			return nil
		}
		return event
	})

	if err := app.SetRoot(layout, true).SetFocus(table).EnableMouse(true).Run(); err != nil {
		panic(err)
	}
}

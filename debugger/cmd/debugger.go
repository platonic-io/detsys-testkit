package main

import (
	"fmt"
	"github.com/gdamore/tcell/v2"
	"github.com/nsf/jsondiff"
	"github.com/rivo/tview"
	"os"
	"strconv"

	"github.com/symbiont-io/detsys/debugger"
	"github.com/symbiont-io/detsys/lib"
)

func max(x, y int) int {
	if x < y {
		return y
	}
	return x
}

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

func selectionHandler(heaps []map[string][]byte, row, column int) {
	textView.Clear()
	opts := jsondiff.DefaultConsoleOptions()
	opts.Indent = "  "
	for component, old := range heaps[max(0, row)] {
		new := heaps[min(max(0, row)+1, len(heaps)-1)][component]
		_, strdiff := jsondiff.Compare(old, new, &opts)
		fmt.Fprintf(w, "%s ", component)
		fmt.Fprintf(w, "%s\n\n", strdiff)
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
	heaps := debugger.Heaps(lib.TestId{testId}, lib.RunId{runId})
	events := debugger.GetNetworkTrace(lib.TestId{testId}, lib.RunId{runId})

	textView.SetBorderPadding(1, 1, 2, 0).SetBorder(true).SetTitle("System state")

	for component, state := range heaps[0] {
		fmt.Fprintf(w, "%s ", component)
		fmt.Fprintf(w, "%s\n\n", string(debugger.PrettyJson(state)))

	}

	table := tview.NewTable().
		SetFixed(1, 1)

	for column, header := range []string{"Event", "Body", "From", "To", "At"} {
		tableCell := tview.NewTableCell(header).
			SetSelectable(false).
			SetTextColor(tcell.ColorYellow).
			SetAlign(tview.AlignLeft)
		table.SetCell(0, column, tableCell)
	}
	for row, event := range events {
		for column, cell := range []string{"Message", "Body", "From", "To", "At"} {

			var tableCell *tview.TableCell
			switch cell {
			case "Message":
				tableCell = tview.NewTableCell(event.Message)
			case "Body":
				tableCell = tview.NewTableCell(string(event.Args))
			case "From":
				tableCell = tview.NewTableCell(event.From)
			case "To":
				tableCell = tview.NewTableCell(event.To)
			case "At":
				tableCell = tview.NewTableCell(strconv.Itoa(event.At))
			}
			table.SetCell(row+1, column, tableCell)
		}
	}
	table.SetBorder(true).SetTitle("Events")
	table.SetSelectable(true, false)
	table.SetSelectionChangedFunc(func(row, column int) { selectionHandler(heaps, row, column) })

	layout := tview.NewFlex().
		SetDirection(tview.FlexRow).
		AddItem(textView, 0, 1, false).
		AddItem(table, 20, 1, false)

	app.SetInputCapture(func(event *tcell.EventKey) *tcell.EventKey {
		if event.Rune() == 'q' {
			app.Stop()
			return nil
		}
		return event
	})

	if err := app.SetRoot(layout, true).SetFocus(table).Run(); err != nil {
		panic(err)
	}
}

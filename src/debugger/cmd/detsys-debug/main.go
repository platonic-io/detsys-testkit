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

	"github.com/symbiont-io/detsys/debugger/internal"
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

var diagram = tview.NewTextView().
	SetWrap(false).
	SetDynamicColors(true)
var w2 = tview.ANSIWriter(diagram)

func selectionHandler(heaps []map[string][]byte, diagrams [][]byte, row int) {
	textView.Clear()
	diagram.Clear()
	fmt.Fprintf(w2, "%s", string(diagrams[row-1]))
	// Debugging
	// fmt.Fprintf(w, "ROW: %d\n", row)
	// fmt.Fprintf(w, "LEN heaps: %d\n\n", (len(heaps)))
	opts := jsondiff.DefaultConsoleOptions()
	opts.Indent = "  "
	components := make([]string, 0, len(heaps))
	for component := range heaps[0] {
		components = append(components, component)
	}
	sort.Strings(components)
	for _, component := range components {
		old := heaps[row-1][component]
		new := heaps[min(row, len(heaps))][component]
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
	diagrams := debugger.SequenceDiagrams(lib.TestId{testId}, lib.RunId{runId})

	textView.SetBorderPadding(1, 1, 2, 0).SetBorder(true).SetTitle("System state")
	diagram.SetBorderPadding(1, 1, 2, 0).SetBorder(true).SetTitle("Sequence diagram")

	selectionHandler(heaps, diagrams, 1)

	table := tview.NewTable().
		SetFixed(1, 1)

	for column, header := range []string{"Event", "Body", "From", "To", "At", "Time"} {
		tableCell := tview.NewTableCell(header).
			SetSelectable(false).
			SetTextColor(tcell.ColorYellow).
			SetAlign(tview.AlignLeft)
		table.SetCell(0, column, tableCell)
	}
	maxBody := 0
	for _, event := range events {
		maxBody = max(maxBody, len(string(event.Args)))
	}
	for row, event := range events {
		for column, cell := range []string{"Message", "Body", "From", "To", "At", "Time"} {

			var tableCell *tview.TableCell
			switch cell {
			case "Message":
				tableCell = tview.NewTableCell(event.Message)
			case "Body":
				tableCell = tview.NewTableCell(fmt.Sprintf(fmt.Sprintf("%%-%ds", maxBody),
					string(event.Args)))
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
		func(row, column int) { selectionHandler(heaps, diagrams, max(1, row)) })

	layout := tview.NewFlex().
		AddItem(tview.NewFlex().
			SetDirection(tview.FlexRow).
			AddItem(textView, 0, 1, false).
			AddItem(table, 20, 1, false), 0, 1, false).
		AddItem(diagram, 0, 1, false)

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

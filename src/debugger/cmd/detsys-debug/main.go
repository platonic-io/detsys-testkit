package main

import (
	"fmt"
	"github.com/gdamore/tcell/v2"
	"github.com/nsf/jsondiff"
	"github.com/rivo/tview"
	"math"
	"os"
	"sort"
	"strconv"
	"time"

	"github.com/symbiont-io/detsys-testkit/src/debugger/internal"
	"github.com/symbiont-io/detsys-testkit/src/lib"
)

func min(x, y int) int {
	if x > y {
		return y
	}
	return x
}

func max(x, y int) int {
	if x < y {
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
	SetDynamicColors(true).
	SetRegions(true).
	Highlight("focused")
var w2 = tview.ANSIWriter(diagram)

var diagramHeader = tview.NewTextView().
	SetWrap(false).
	SetDynamicColors(true)
var wDiagramHeader = tview.ANSIWriter(diagramHeader)

var reactorsWidget = tview.NewList()

var messageView = tview.NewTextView().
	SetWrap(false).
	SetDynamicColors(true)
var wMessageView = tview.ANSIWriter(messageView)

var logView = tview.NewTextView().
	SetWrap(false).
	SetDynamicColors(true)
var wLogView = tview.ANSIWriter(logView)

var sentMsgsView = tview.NewTable().
	SetFixed(1, 1)

type DebugApplication struct {
	testId        lib.TestId
	runId         lib.RunId
	heaps         []map[string][]byte
	diagrams      *debugger.SequenceDiagrams
	events        []debugger.NetworkEvent
	reactors      []string
	activeRow     int // should probably be logic time
	activeReactor int
}

func (da *DebugApplication) setReactor(reactor string) {
	for i, x := range da.reactors {
		if x == reactor {
			da.activeReactor = i
			reactorsWidget.SetCurrentItem(i)
			break
		}
	}
}

func (da *DebugApplication) setRow(row int) {
	nrow := debugger.Max(1, row)
	da.activeRow = nrow

	to := da.events[nrow-1].To

	da.setReactor(to)
}

func (da *DebugApplication) redraw() {
	textView.Clear()
	diagram.Clear()
	diagramHeader.Clear()
	messageView.Clear()
	logView.Clear()
	row := da.activeRow
	{
		fmt.Fprintf(w2, "%s", da.diagrams.At(row-1))
		diagram.ScrollToHighlight()
		fmt.Fprintf(wDiagramHeader, "%s", da.diagrams.Header())
	}
	reactor := da.reactors[da.activeReactor]
	old := da.heaps[row-1][reactor]
	new := da.heaps[min(row, len(da.heaps))][reactor]

	opts := jsondiff.DefaultConsoleOptions()
	opts.Indent = "  "
	_, strdiff := jsondiff.Compare(old, new, &opts)
	fmt.Fprintf(w, "%s", strdiff)

	event := da.events[row-1]
	fmt.Fprintf(wMessageView, "%s", string(event.Args))

	logs := debugger.GetLogMessages(da.testId, da.runId, reactor, event.RecvAt)
	for _, log := range logs {
		fmt.Fprintf(wLogView, "%s\n", string(log))
	}

	da.refreshSentMessages()
}

func (da *DebugApplication) goToFrom(table *tview.Table) {
	event := da.events[da.activeRow-1]
	table.Select(event.SentAt, 0)
}

func (da *DebugApplication) sentEventsAtActive() []debugger.NetworkEvent {
	event := da.events[da.activeRow-1]
	sentEvents := make([]debugger.NetworkEvent, 0, 0)
	for _, ev := range da.events {
		if ev.SentAt == event.RecvAt {
			sentEvents = append(sentEvents, ev)
		}
	}
	return sentEvents
}

func (da *DebugApplication) refreshSentMessages() {
	sentMsgsView.Clear()

	// Should we have absolute time, and/or relative time to current event?
	headers := []string{"Event", "To", "Received", "Message"}

	for column, header := range headers {
		tableCell := tview.NewTableCell(header).
			SetSelectable(false).
			SetTextColor(tcell.ColorYellow).
			SetAttributes(tcell.AttrBold).
			SetAlign(tview.AlignLeft)
		sentMsgsView.SetCell(0, column, tableCell)
	}

	for row, event := range da.sentEventsAtActive() {
		for column, cell := range headers {
			var tableCell *tview.TableCell
			switch cell {
			case "Event":
				tableCell = tview.NewTableCell(event.Message)
			case "To":
				tableCell = tview.NewTableCell(event.To)
			case "Received":
				tableCell = tview.NewTableCell(strconv.Itoa(event.RecvAt))
			case "Message":
				tableCell = tview.NewTableCell(string(event.Args))
			}
			sentMsgsView.SetCell(row+1, column, tableCell)
		}
	}
}

func MakeDebugApplication(testId lib.TestId, runId lib.RunId) *DebugApplication {
	heaps := debugger.Heaps(testId, runId)
	events := debugger.GetNetworkTrace(testId, runId)
	diagrams := debugger.NewSequenceDiagrams(testId, runId)

	reactors := make([]string, 0, len(heaps[0]))
	for reactor := range heaps[0] {
		reactors = append(reactors, reactor)
	}

	to := events[0].To
	ac := 0
	for i, x := range reactors {
		if x == to {
			ac = i
			break
		}
	}

	sort.Strings(reactors)
	return &DebugApplication{
		testId:        testId,
		runId:         runId,
		heaps:         heaps,
		diagrams:      diagrams,
		events:        events,
		reactors:      reactors,
		activeRow:     1,
		activeReactor: ac,
	}
}

var version = "unknown"

func help() {
	fmt.Printf(`
Usage:
  detsys-debug [test-id] [run-id]
Flags:
  -v, --version   version for detsys-debug
`)
	os.Exit(1)
}

func displayDuration(dur time.Duration) string {
	days := int(math.Floor(dur.Seconds() / 86400))
	if days < 1 {
		return dur.String()
	}
	dur = dur.Truncate(24 * time.Hour)
	if days == 1 {
		return fmt.Sprintf("1 day %s", dur)
	}
	return fmt.Sprintf("%d days %s", days, dur)
}

func main() {
	if os.Args[1] == "--version" || os.Args[1] == "-v" {
		fmt.Println(version)
		os.Exit(0)
	}
	testId, err := strconv.Atoi(os.Args[1])
	if err != nil {
		help()
	}
	runId, err := strconv.Atoi(os.Args[2])
	if err != nil {
		help()
	}

	app := tview.NewApplication()

	da := MakeDebugApplication(lib.TestId{testId}, lib.RunId{runId})

	messageView.SetBorderPadding(1, 1, 2, 0).SetBorder(true).SetTitle("Current Message")
	logView.SetBorderPadding(1, 1, 2, 0).SetBorder(true).SetTitle("Reactor Log")

	diagramWidget := tview.NewFlex().
		SetDirection(tview.FlexRow).
		AddItem(diagramHeader, 3, 0, false).
		AddItem(diagram, 0, 1, false)
	diagramWidget.
		SetBorder(true).
		SetTitle("Sequence Diagram")

	da.redraw()

	table := tview.NewTable().
		SetFixed(1, 1)

	{
		headers := []string{"Event", "From", "Sent", "To", "Received", "Time"}

		for column, header := range headers {
			tableCell := tview.NewTableCell(header).
				SetSelectable(false).
				SetTextColor(tcell.ColorYellow).
				SetAttributes(tcell.AttrBold).
				SetAlign(tview.AlignLeft)
			table.SetCell(0, column, tableCell)
		}
		for row, event := range da.events {
			for column, cell := range headers {

				var tableCell *tview.TableCell
				switch cell {
				case "Event":
					tableCell = tview.NewTableCell(event.Message)
				case "From":
					tableCell = tview.NewTableCell(event.From)
				case "Sent":
					tableCell = tview.NewTableCell(strconv.Itoa(event.SentAt))
				case "To":
					tableCell = tview.NewTableCell(event.To)
				case "Received":
					tableCell = tview.NewTableCell(strconv.Itoa(event.RecvAt))
				case "Time":
					if row == 0 {
						tableCell = tview.NewTableCell(event.Simulated.Format(time.StampNano))
					} else {
						tableCell = tview.NewTableCell(
							displayDuration(event.Simulated.Sub(time.Unix(0, 0).UTC())))
					}
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
	}

	sentMsgsView.
		SetBorder(true).
		SetTitle("Sent Messages")
	sentMsgsView.SetSelectable(true, false)
	da.refreshSentMessages()

	sentMsgsView.SetSelectionChangedFunc(
		func(row, column int) {
			events := da.sentEventsAtActive()
			row = max(0, row-1)
			row = min(row, len(events))
			event := events[row]
			table.Select(event.RecvAt, 0)
		})

	reactorsWidget.SetChangedFunc(func(index int, _mainText string, _secondaryText string, shortcut rune) {
		da.activeReactor = index
		da.redraw()
	})

	longestReactor := 0
	for i, c := range da.reactors {
		theRune := ' '

		if len(c) > longestReactor {
			longestReactor = len(c)
		}

		if i < 9 {
			theRune = rune(i + 49) // 48 is '0', but we skip that one
		}
		reactorsWidget.AddItem(c, "", theRune, nil)
	}

	stateWidget := tview.NewFlex().
		SetDirection(tview.FlexColumn).
		AddItem(reactorsWidget, longestReactor+1+2+4, 0, false).
		AddItem(textView, 0, 1, false)

	reactorsWidget.SetBorderPadding(1, 1, 1, 2)
	stateWidget.SetBorder(true).SetTitle("Reactor State")

	msgViews := tview.NewFlex().
		AddItem(messageView, 0, 1, false).
		AddItem(sentMsgsView, 0, 1, false)

	layout := tview.NewFlex().
		SetDirection(tview.FlexRow).
		AddItem(tview.NewFlex().
			AddItem(tview.NewFlex().
				SetDirection(tview.FlexRow).
				AddItem(stateWidget, 0, 1, false).
				AddItem(table, 20, 1, false), 0, 1, false).
			AddItem(diagramWidget, 0, 1, false), 0, 20, false).
		AddItem(msgViews, 8, 1, false).
		AddItem(logView, 10, 1, false)

	app.SetInputCapture(func(event *tcell.EventKey) *tcell.EventKey {
		if event.Rune() == 'q' {
			app.Stop()
			return nil
		}
		if event.Rune() == 'f' {
			da.goToFrom(table)
		}
		return event
	})

	if err := app.SetRoot(layout, true).SetFocus(table).EnableMouse(true).Run(); err != nil {
		panic(err)
	}
}

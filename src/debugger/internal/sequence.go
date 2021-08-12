package debugger

import (
	"strings"
)

type CrashInformation = map[int][]string
type crashInformationInternal = map[int][]int

// inlined version of b.WriteString(strings.Repeat(s,count))
func WriteRepeat(b *strings.Builder, s string, count int) {
	if count == 0 {
		return
	}

	// Since we cannot return an error on overflow,
	// we should panic if the repeat will generate
	// an overflow.
	// See Issue golang.org/issue/16237
	if count < 0 {
		panic("strings: negative Repeat count")
	} else if len(s)*count/count != len(s) {
		panic("strings: Repeat count causes overflow")
	}

	n := len(s) * count
	origLen := b.Len()
	on := n + origLen
	b.WriteString(s)
	for b.Len() < on {
		if b.Len()-origLen <= n/2 {
			b.WriteString(b.String()[origLen:])
		} else {
			b.WriteString(b.String()[origLen:][:on-b.Len()])
			break
		}
	}
}

type Arrow struct {
	From    string
	To      string
	At      int
	Message string
	Dropped bool
}

type arrowInternal struct {
	from               int
	to                 int
	at                 int
	message            string
	dropped            bool
	goingRight         bool
	gapIndexToAnnotate int
	annotationLeft     string
	annotationRight    string
	isLine             bool
}

func boxSize(names []string) int {
	boxSize := 3
	for _, name := range names {
		this := 4 + len(name) + (len(name)+1)%2
		if boxSize < this {
			boxSize = this
		}
	}
	return boxSize
}

const hLine = "─"

func hline(len int) string {
	return strings.Repeat(hLine, len)
}

func wline(len int) string {
	return strings.Repeat(" ", len)
}

const dLine = "╌"

func dline(len int) string {
	return strings.Repeat(dLine, len)
}

func appendBoxes(isTop bool, output *strings.Builder, names []string, gaps []int) {
	boxSize := boxSize(names)
	// top of the boxes
	{
		middle := "┴"
		if isTop {
			middle = "─"
		}

		halfPoint := (boxSize - 2) / 2
		for i, _ := range names {
			WriteRepeat(output, " ", gaps[i])
			output.WriteString("╭")
			WriteRepeat(output, hLine, halfPoint)
			output.WriteString(middle)
			WriteRepeat(output, hLine, halfPoint)
			output.WriteString("╮")
		}
		output.WriteString("\n")
	}
	// content of boxes
	{
		for i, name := range names {
			totalSpace := boxSize - 4 - len(name)
			slack := totalSpace / 2
			remaining := totalSpace - slack
			WriteRepeat(output, " ", gaps[i])
			output.WriteString("│ ")
			WriteRepeat(output, " ", slack)
			output.WriteString(name)
			WriteRepeat(output, " ", remaining)
			output.WriteString(" │")
		}
		output.WriteString("\n")
	}
	// bottom of boxes
	{
		var middle string
		if isTop {
			middle = `┬`
		} else {
			middle = `─`
		}

		halfPoint := (boxSize - 2) / 2

		for i, _ := range names {
			WriteRepeat(output, " ", gaps[i])
			output.WriteString("╰")
			WriteRepeat(output, hLine, halfPoint)
			output.WriteString(middle)
			WriteRepeat(output, hLine, halfPoint)
			output.WriteString("╯")
		}
	}
}

func appendArrows(output *strings.Builder, names []string, arrows []arrowInternal, gaps []int, boxSize int, crashInformation crashInformationInternal) int {
	deadNodes := make(map[int]bool)
	line := 0
	foundLine := false

	halfBox := boxSize / 2
	for _, arr := range arrows {
		if arr.isLine {
			foundLine = true
		}
		newCrashes := crashInformation[arr.at]

		for _, n := range newCrashes {
			deadNodes[n] = true
		}

		// output lines for new crashes
		if newCrashes != nil {
			for i, _ := range names {
				// "Top"
				middle := "  │  "
				newCrash := false
				for _, v := range newCrashes {
					if v == i {
						newCrash = true
					}
				}

				if newCrash {
					middle = "[red]╭─┴─╮[-]"
				} else if deadNodes[i] {
					middle = "     "
				}
				WriteRepeat(output, " ", halfBox-2+gaps[i])
				output.WriteString(middle)
				WriteRepeat(output, " ", halfBox-2)
			}
			output.WriteString("\n")
			if !foundLine {
				line++
			}
			for i, _ := range names {
				middle := "  │  "
				newCrash := false
				for _, v := range newCrashes {
					if v == i {
						newCrash = true
					}
				}

				if newCrash {
					middle = "[red]│ ☠ │[-]"
				} else if deadNodes[i] {
					middle = "     "
				}
				WriteRepeat(output, " ", halfBox-2+gaps[i])
				output.WriteString(middle)
				WriteRepeat(output, " ", halfBox-2)
			}
			output.WriteString("\n")
			if !foundLine {
				line++
			}
			for i, _ := range names {
				middle := "  │  "
				newCrash := false
				for _, v := range newCrashes {
					if v == i {
						newCrash = true
					}
				}

				if newCrash {
					middle = "[red]╰───╯[-]"
				} else if deadNodes[i] {
					middle = "     "
				}
				WriteRepeat(output, " ", halfBox-2+gaps[i])
				output.WriteString(middle)
				WriteRepeat(output, " ", halfBox-2)
			}
			output.WriteString("\n")
			if !foundLine {
				line++
			}
		}

		{
			for i, _ := range names {
				if i == arr.gapIndexToAnnotate {
					totalSpace := 2*halfBox + gaps[i] - len(arr.message)
					slack := totalSpace / 2
					WriteRepeat(output, " ", slack)
					output.WriteString(arr.annotationLeft)
					output.WriteString(arr.message)
					output.WriteString(arr.annotationRight)
					WriteRepeat(output, " ", totalSpace-slack)
				} else {
					space := halfBox + gaps[i]
					if i != 0 {
						space += halfBox
					}
					WriteRepeat(output, " ", space)
				}
				if deadNodes[i] {
					output.WriteString(" ")
				} else {
					output.WriteString("│")
				}
			}
			output.WriteString("\n")
			if !foundLine {
				line++
			}
		}

		halfEmpty := wline(halfBox)
		halfFull := hline(halfBox)
		if arr.dropped {
			halfFull = dline(halfBox)
		}

		if arr.to == arr.from {
			//compute top of loop
			for i, _ := range names {
				leftPart := halfEmpty
				middle := "│"
				rightPart := halfEmpty

				WriteRepeat(output, " ", gaps[i])
				if i == arr.from {
					if arr.goingRight {
						middle = "├"
						rightPart = hline(halfBox-1) + "╮"
					} else {
						middle = "┤"
						leftPart = "╭" + hline(halfBox-1)
					}
				}

				output.WriteString(leftPart)
				if deadNodes[i] {
					output.WriteString(" ")
				} else {
					output.WriteString(middle)
				}
				output.WriteString(rightPart)

			}
			output.WriteString("\n")
			if !foundLine {
				line++
			}
			//compute bottom line of loop
			for i, _ := range names {
				WriteRepeat(output, " ", gaps[i])
				leftPart := halfEmpty
				middle := "│"
				if deadNodes[i] {
					middle = " "
				}
				rightPart := halfEmpty

				if i == arr.from {
					if arr.goingRight {
						middle = "◀"
						rightPart = hline(halfBox-1) + "╯"
					} else {
						middle = "▶"
						leftPart = "╰" + hline(halfBox-1)
					}
				}

				output.WriteString(leftPart)
				output.WriteString(middle)
				output.WriteString(rightPart)
			}
			output.WriteString("\n")
			if !foundLine {
				line++
			}
		} else {
			for i, _ := range names {
				leftPart := halfEmpty
				middle := "│"
				if deadNodes[i] {
					middle = " "
				}
				rightPart := halfEmpty
				if (arr.goingRight && arr.from < i && i <= arr.to) ||
					(!arr.goingRight && arr.to < i && i <= arr.from) {
					if arr.dropped {
						WriteRepeat(output, dLine, gaps[i])
					} else {
						WriteRepeat(output, hLine, gaps[i])
					}
				} else {
					WriteRepeat(output, " ", gaps[i])
				}

				if (arr.goingRight && arr.from < i && i <= arr.to) ||
					(!arr.goingRight && arr.to < i && i <= arr.from) {
					leftPart = halfFull
				}
				output.WriteString(leftPart)

				if arr.goingRight {
					if i == arr.from {
						middle = "├"
					}
					if i == arr.to {
						middle = "▶"
					}
					if arr.from < i && i < arr.to {
						middle = "┼"
					}
				} else {
					if i == arr.from {
						middle = "┤"
					}
					if i == arr.to {
						middle = "◀"
					}
					if arr.to < i && i < arr.from {
						middle = "┼"
					}
				}
				output.WriteString(middle)

				if arr.goingRight && arr.from <= i && i < arr.to {
					rightPart = halfFull
				} else if !arr.goingRight && arr.to <= i && i < arr.from {
					rightPart = halfFull
				}
				output.WriteString(rightPart)
			}
			output.WriteString("\n")
			if !foundLine {
				line++
			}
		}
	}
	return line
}

func drawDiagram(names []string, arrows []arrowInternal, gaps []int, nrLoops int, crashInformation crashInformationInternal) ([]byte, []byte, int) {
	if len(names) < 1 {
		panic("We need at least one box")
	}

	boxSize := boxSize(names)

	var header strings.Builder
	var output strings.Builder
	var expectedSize int
	{
		var lineWidth int
		for _, gap := range gaps {
			lineWidth += gap
		}
		lineWidth += boxSize * len(names)
		lineWidth++ // newline
		// loops have one more line than normal, plus each crash is three lines
		expectedSize = lineWidth * (len(arrows) + nrLoops + len(crashInformation)*3)
	}
	output.Grow(expectedSize)

	appendBoxes(true, &header, names, gaps)

	line := appendArrows(&output, names, arrows, gaps, boxSize, crashInformation)
	appendBoxes(false, &output, names, gaps)

	// remove last newline
	return []byte(header.String()), []byte(output.String()), line
}

func index(haystack []string, needle string) int {
	for i, v := range haystack {
		if v == needle {
			return i
		}
	}
	return -1
}

type DrawSettings struct {
	MarkerSize int
	MarkAt     int
	Crashes    CrashInformation
}

func DrawDiagram(arrows []Arrow, settings DrawSettings) ([]byte, []byte, int) {
	var names []string
	{
		for _, arr := range arrows {
			if index(names, arr.From) == -1 {
				names = append(names, arr.From)
			}
			if index(names, arr.To) == -1 {
				names = append(names, arr.To)
			}
		}

	}
	crashInformation := make(crashInformationInternal)
	for k, crashedNodes := range settings.Crashes {
		targets := make([]int, 0, len(crashedNodes))
		for _, c := range crashedNodes {
			targets = append(targets, index(names, c))
		}
		crashInformation[k] = targets
	}

	gaps := make([]int, len(names), len(names))
	arrowsInternal := make([]arrowInternal, 0, len(arrows))
	allocatedMarkerSize := 2 * settings.MarkerSize
	nrLoops := 0
	emptyMarker := strings.Repeat(" ", settings.MarkerSize)
	leftMarker := strings.Repeat(">", settings.MarkerSize)
	rightMarker := strings.Repeat("<", settings.MarkerSize)
	for i, arr := range arrows {
		from := index(names, arr.From)
		to := index(names, arr.To)

		if from == to {
			nrLoops++
		}

		goingRight := true
		if to < from {
			goingRight = false
		}
		gapIndexToAnnotate := from
		if goingRight {
			gapIndexToAnnotate++
		}

		// if we are a self-loop at the end, we make special case
		if from == to && from == len(names)-1 {
			goingRight = false
			gapIndexToAnnotate = from
		}

		if gaps[gapIndexToAnnotate] < len(arr.Message)+allocatedMarkerSize {
			gaps[gapIndexToAnnotate] = len(arr.Message) + allocatedMarkerSize
		}
		var message string
		annotationLeft := ""
		annotationRight := ""
		isLine := false
		if i == settings.MarkAt {
			annotationLeft = "[\"focused\"][yellow]"
			annotationRight = "[-][\"\"]"
			message = leftMarker + arr.Message + rightMarker
			isLine = true
		} else {
			message = emptyMarker + arr.Message + emptyMarker
		}
		arrowsInternal = append(arrowsInternal, arrowInternal{
			from:               from,
			to:                 to,
			at:                 arr.At,
			message:            message,
			dropped:            arr.Dropped,
			goingRight:         goingRight,
			gapIndexToAnnotate: gapIndexToAnnotate,
			annotationLeft:     annotationLeft,
			annotationRight:    annotationRight,
			isLine:             isLine,
		})
	}

	boxSize := boxSize(names)
	for i, gap := range gaps {
		if gap < boxSize-1 {
			gaps[i] = 0
		} else {
			gaps[i] = gap - (boxSize - 1)

		}
	}

	return drawDiagram(names, arrowsInternal, gaps, nrLoops, crashInformation)
}

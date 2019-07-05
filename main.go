package main

import (
	"math"
	"fmt"
	"strings"

	"github.com/uakotaobi/synthetic-division/colorString"
	"github.com/uakotaobi/synthetic-division/animation"
	"github.com/nsf/termbox-go"
)

// Creates an animation that traces the perimeter of an ASCII box.
//
// The trace will start at the upper-left corner, tracing a path clockwise
// around the perimeter of the rectangle.  If more than length characters of
// the trace are drawn, we will remove characters from the rear of the trace.
//
// These animations are designed to loop forever, so feel free to stop them
// anytime.
//
// - width: The width of the box, in characters.
// - height: The height of the box, in characters.
// - length: The maximum number of non-skipped characters in the trace line.
//           If the number of characters in the trace line exceeds this, we
//           remove characters at the end of the trace.
//
//           A length of 0 here is the same as a length of 2 * width + 2 *
//           height -- the trace will grow throughout the entire perimeter of
//           the rectangle.
// - skip:   As we trace out the perimeter of the rectangle, we skip forward
//           by this many positions before rendering the next character.
//
//           For instance, a trace with a length of 5 on a 6x4 rectangle will
//           look like this after 10 frames when the skip is 0:
//
//                |
//                |
//                |
//               -+
//
//           But with a skip of 1, the same trace will look like this:
//
//           + -
//
//           |
//            - -
//
//           As you can see, adding skips will increase the speed at which the
//           trace moves along the perimeter.

func makeBoxAnimation(width, height, length, skip int) Animation {

	var result animation.Animation
	const framesPerSecond = 10.0

	// The actual line we draw will have this length, counting skipped
	// positions.
	totalCharacterCount := length * skip

	// These variables keep track of our virtual position.
	x, y := 0, 0
	vx, vy := 1, 0

	// Generate the current frame.
	frameString := ""
	for controlCharacter, i := "", 0; i < totalCharacterCount; i++ {

		c := ""
		switch {
		case y == 0:
			switch {
			case x == 0:
				c, controlCharacter = "+", ""   // Upper left corner, heading to the right.
				vx, vy = 1, 0
			case x == width - 1:
				c, controlCharacter = "+", "\n" // Upper right corner, heading down.
				vx, vy = 0, 1
			default:
				c = "-"                         // Horizontal (top row), heading right.
			}
		case y == height - 1:
			switch {
			case x == 0:
				c, controlCharacter = "+", "\v" // Lower left corner, heading up.
				vx, vy = 0, -1
			case x == width - 1:
				c, controlCharacter = "+", "\b" // Lower right corner, heading left.
				vx, vy = -1, 0
			default:
				c = "-"                         // Horizontal (bottom row), heading left.
			}
		default:
			c = "|"         // Vertical (left side or right side.)
		}


		if i % skip == 0 {
			// Add the current character to the current frame.
			frameString += c;
			if controlCharacter != "" {
				// Return to our initial position if we're not
				// trivially moving forward (that is,
				// horizontally and to the right.)  That way,
				// the control character has less work to do.
				frameString += "\b"
			}
		}
		// Use the control character to skip to the next position.
		frameString += controlCharacter

		// Advance our virtual position.
		x += vx
		y += vy
		if x > width - 1 {
			y += (x - (width - 1))
			x = width - 1
		} else if y > height - 1 {
			x -= (y - (height - 1))
			y = height - 1
		} else if x < 0 {
			y += x
			x = 0
		} else if y < 0 {
			x -= y
			y = 0
		}
		// x = int(math.Max(0, math.Min(float64(width - 1), float64(x + vx))))
		// y = int(math.Max(0, math.Min(float64(height - 1), float64(y + vy))))

	} // end (for each character, skipped or not, in the current line)

	var frame animation.Frame
	frame.Content = frameString
	frame.Timestamp = int(1000 / framesPerSecond)

	result.Frames = append(result.Frames, frame)


}

func initializeTerminalLibrary() error {
	err := termbox.Init()
	if err != nil {
		fmt.Printf("Could not initialize termbox-go: %s\n", err.Error())
		return err
	}
	return nil
}

func closeTerminalLibrary() {
	termbox.Close()
}

func main() {

	err := initializeTerminalLibrary()
	if err != nil {
		return
	}
	defer closeTerminalLibrary()

	// TODO: Enable bright background colors.
	message := "`0~7F\n\br\n\bo\n\bg\n\bs`7~0\n\b\b\n\n\nare\v\r     "
	beautiful := "`4b`6e`Ea`Au`3t`9i`1f`5u`Dl`C!\n\b\b\b\b\b\b\b\b\b\b"
	colorString.Print(0, 5, message)
	for i := 0; i < 10; i++ {
		x, y := colorString.CursorPosition()
		colorString.Printf(x, y, beautiful)
	}
	x, y := colorString.CursorPosition()
	var v interface{} = "`9~0agree"
	colorString.Printf(x, y, "`fDon't %s`7~0 %s?", "~1`0you", v)
	termbox.Flush()

	// Press enter to exit
	buf := make([]byte, 10)
	_ = termbox.PollRawEvent(buf)
}

package colorString

import (
	"fmt"

	"math"

	"github.com/uakotaobi/synthetic-division/utils"

	"github.com/nsf/termbox-go"
)

var _ = fmt.Printf

var letterToControlCodeTable = map[byte]byte {
	'b': '\b',
	'f': '\f',
	'n': '\n',
	'r': '\r',
	't': '\t',
	'v': '\v',
}

var hexDigitToColorTable = map[byte]termbox.Attribute {
	'0': termbox.ColorBlack,
	'1': termbox.ColorBlue,
	'2': termbox.ColorGreen,
	'3': termbox.ColorCyan,
	'4': termbox.ColorRed,
	'5': termbox.ColorMagenta,
	'6': termbox.ColorYellow,
	'7': termbox.ColorWhite,
	'8': termbox.ColorBlack | termbox.AttrBold,
	'9': termbox.ColorBlue | termbox.AttrBold,
	'A': termbox.ColorGreen | termbox.AttrBold,
	'a': termbox.ColorGreen | termbox.AttrBold,
	'B': termbox.ColorCyan | termbox.AttrBold,
	'b': termbox.ColorCyan | termbox.AttrBold,
	'C': termbox.ColorRed | termbox.AttrBold,
	'c': termbox.ColorRed | termbox.AttrBold,
	'D': termbox.ColorMagenta | termbox.AttrBold,
	'd': termbox.ColorMagenta | termbox.AttrBold,
	'E': termbox.ColorYellow | termbox.AttrBold,
	'e': termbox.ColorYellow | termbox.AttrBold,
	'F': termbox.ColorWhite | termbox.AttrBold,
	'f': termbox.ColorWhite | termbox.AttrBold,
}

var cursorPosition utils.Position = utils.Position{0, 0}

// The current cursor position starts at 0, 0 and changes as Print() is
// called.  You can use this to continue printing "where you were last."
//
// Note that the CursorPosition never moves off of the screen, even if Print()
// runs out of bounds.
func CursorPosition() (int, int) {
	return int(math.Round(cursorPosition.X)), int(math.Round(cursorPosition.Y))
}

// Prints the given string at the given position on the terminal, then returns
// a pair consisting of the number of characters printed and an error (nil
// means no errors.)
//
// NOTE: You must call termbox.Flush() yourself to show the strings you
// printed.  This way, you can call print multiple times and only flush when
// rendering the final frame.
//
// We interpret certain codes, namely `n and ~n, as foreground and background
// color switching directives, respectively.  "n" here is a color from 0 to
// 15, represented as a single hexadecimal digit.  To print an actual
// backquote or tilde, use '``' or '~~', respectively.
//
// We can print safely off of the screen boundary.  If the string extends
// beyond the boundaries of the screen, it will be clipped.
func Print(x, y int, s string) (charactersPrinted int, err error) {

	var foreground, background = termbox.ColorWhite, termbox.ColorBlack
	width, height := termbox.Size()

	if y < 0 || y >= height {
		return 0, nil
	}
	if x >= width {
		return 0, nil
	}

	for i := 0; i < len(s); i++ {
		c := s[i]
		if i < len(s) - 1 {
			// If we can detect a two-character escape sequence,
			// treat it as if it were the corresponding control
			// code.
			if s[i] == '\\' {

				_, ok := letterToControlCodeTable[s[i + 1]]
				if ok {
					c = letterToControlCodeTable[s[i + 1]]
					// Skip the escape seequence.
					i += 2
				} else {
					// You're escaping a character that
					// didn't need escaping.  Skip the
					// next character.
					c = s[i + 1]
					i += 1
				}

			} else if s[i] == '`' {

				_, ok := hexDigitToColorTable[s[i + 1]]
				if ok {
					// Foreground color switch.  Skip the
					// entire sequence.
					foreground = hexDigitToColorTable[s[i + 1]]
					i += 1
					continue
				} else {
					// Invalid color escape sequence.  We
					// will print it literally.
					c = s[i]
				}

			} else if s[i] == '~' {

				_, ok := hexDigitToColorTable[s[i + 1]]
				if ok {
					// Background color switch.  Skip the
					// entire sequence.
					background = hexDigitToColorTable[s[i + 1]]

					// TODO: termBox doesn't support a
					// blink attribute like ncurses does,
					// but for some terminals, the blink
					// attribute is the only way to get
					// the 8 bright background colors.
					// You'd think termBox would give us
					// more control over this.
					i += 1
					continue
				} else {
					// Invalid color escape sequence.  We
					// will print it literally.
					c = s[i]
				}

			}
		}

		// At this point, all escape sequences are converted or dealt
		// with, and c represents the current character to print or
		// process.
		switch(c) {
		case '\t':
			// Tab: Jump forward to the next tab stop.
			x += (8 - (x & 7))
			if x >= width {
				// You tabbed past the end of the
				// line.
				return charactersPrinted, nil
			}
		case '\b':
			// Backspace: Move backward by one character.  This
			// can move us off of the left side, and that's okay.
			x -= 1
		case '\f':
			// Form feed: Reset to the top of the page.
			x = 0
			y = 0
		case '\r':
			// Carriage return: Move backward to the beginning of the line.
			x = 0
		case '\n':
			// Line feed: Advance by one line, but retain the same
			// column.
			// Note that this is _not_ what C's newline does.
			y += 1
		case '\v':
			// Vertical tab: For us, that jumps up one line,
			// making it the opposite of \n.
			y -= 1
		default:
			// Just print the current character at (x, y).
			// TODO: I thought enumerating through strings yielded
			// runes, not bytes...?
			if x >= 0 && x < width && y >= 0 && y < height {
				termbox.SetCell(x,
					y,
					rune(c),
					foreground,
					background)
				charactersPrinted += 1
			}
			x += 1
		}

		// The ASCII control codes can easily move us out of bounds,
		// but since they can also move us back *in* boudns, we don't
		// quit when that happens.
		//
		// However, the cursorPosition is never allowed to go off-screen.
		if x >= 0 && x < width && y >= 0 && y < height {
			cursorPosition.X = float64(x)
			cursorPosition.Y = float64(y)
		}
	}

	return charactersPrinted, nil
} // end (func Print)


// Prints an arbitrary number of arguments in the same manner as fmt.Printf().
func Printf(x, y int, format string, args ...interface{}) (charactersPrinted int, err error) {
	s := fmt.Sprintf(format, args...)
	return Print(x, y, s)
}

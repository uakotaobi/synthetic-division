package colorString

import (
	"fmt"
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

type position struct {
	x, y int
}

var cursorPosition position = position{0, 0}

// The current cursor position starts at 0, 0 and changes as Print() is
// called.  You can use this to continue printing "where you were last."
//
// Note that the CursorPosition never moves off of the screen, even if Print()
// runs out of bounds.
func CursorPosition() (int, int) {
	return cursorPosition.x, cursorPosition.y
}

// Returns the substring of the given color string (i.e., a string that
// contains color code sequences) having the given starting position and final
// printed length.  Forms use this function to ensure that color strings
// representing polynomials do not exceed the length of a given form field.
//
// The main feature that distinguishes this function from ordinary string
// slicing is its awareness of color code sequences.
//
// Caveats:
// 1. For sanity's sake, all control codes except for the inline ones -- "\b",
//    and "\t" -- are ignored.  They do not factor into the final substring
//    length.
// 2. The substring will start with the default foreground and background
//    color code sequences unless the substring begins with a complete `n or ~n
//    sequence.
// 3. If this function reads the start of a color code sequence, then it will
//    proceed to read the entire sequence. and the first non-sequence
//    character that follows it (if any.)
// 4. Sequences do not count toward the final string's length.
//
// Example:
//   colorString.Substr("`7~1Test", 1, 2)	    // Returns "es"          (initial code skipped)
//   colorString.Substr("`7~1Test", 0, 2)	    // Returns "`7~1Te"      (initial code captured)
//   colorString.Substr("`7~1Test~4String", 0, 5)   // Returns "`7~1Test~4S" (second code captured)
//   colorString.Substr("`7~1Test~4String", 0, 4)   // Returns "`7~1Test"    (second code unread)
//
// Arguments:
//   - s: The color string to read.
//   - start: The index within s where reading should begin.
//   - length: The desired length of the substring.  We might not be able to
//     meet this length if we run out of real characters to read.
//
// Returns:
//   Returns the desired substring, or an empty string if the starting index
//   was invalid.  The substring's length will never exceed len(s).

func Substr(s string, start, length int) (result string) {
	// Sanity checking.
	if start < 0 || start >= len(s) || length <= 0 {
		return ""
	}

	// Notice that we always start at position 0, even if start > 0.
	// That's because we try to never split or break color codes (since
	// such a result would not be useful to the caller.)  This requires us
	// to read _all_ of the color codes in their entirety, skipping ahead
	// until we get to the actual start character.
	characterCount := 0
	for i := 0; i < len(s);  {

		_, nextIndex, _, _ := readNextCharacter(s, i, termbox.ColorWhite, termbox.ColorBlack)
		characterCount += 1

		if characterCount > start {
			result += s[i:nextIndex]
		}

		if characterCount >= start + length {
			break
		}

		i = nextIndex
	}

	return result
}

// This function reads one character from a color string, but it will process
// any escape sequences and color control codes it encounters along the way.
// It will return the next character, the next position to read from, and the
// colors that the next character should have.
//
// Since Substr() and Print() share similar sequence-parsing code, this
// function isolates that.
//
// Arguments:
//
// - s:		 The color string to read.  It may contain any number of
//               escape sequences and control codes; invalid control codes
//               will be treated as ordinary characters.
// - index:      The position in the string from which to start reading.
// - foreground: The current foreground color according to whichever function
//               is reading the string (i.e., Substr() or Print().)
// - background: The current background color according to the caller.
//
// Return values:
//
// - c:              The actual next character that was read.  This will be
//                   an escape sequence character if a two-character escape
//                   sequence was encountered, and it will be a 0 byte if the
//                   end of the string has been reached.
// - nextIndex:      The position where reading should take place the next
//                   time this function is called.  If this is beyond the last
//                   character in the string, parsing is at an end.
// - nextForeground: The new foreground color according to whatever color
//                   sequences we have read.
// - nextBackground: The new background color.

func readNextCharacter(s string, index int,
	foreground, background termbox.Attribute) (c byte, nextIndex int,
		nextForeground, nextBackground termbox.Attribute) {

	nextForeground, nextBackground = foreground, background

	if index >= len(s) {
		return 0, index, nextForeground, nextBackground
	}

	// Loop until we read exactly one "real" character (or until
	// end-of-string.)
	for index < len(s) {

		// s[index] may or may not be the "real" character.
		c = s[index]

		if index < len(s) - 1 {
			// If we can detect a two-character escape sequence,
			// treat it as if it were the corresponding control
			// code.
			if s[index] == '\\' {

				_, ok := letterToControlCodeTable[s[index + 1]]
				if ok {
					// The "real" character is an escape
					// sequence.  Convert 'b' to '\b', 'n'
					// to '\n', and so on.
					c = letterToControlCodeTable[s[index + 1]]
				} else {
					// You're escaping a character that
					// didn't need escaping (like "\8".)
					// Return that character as if it were
					// unescaped.
					c = s[index + 1]
				}

				// The next read should skip the second
				// character of this escape sequence.
				index += 1

			} else if s[index] == '`' {

				_, ok := hexDigitToColorTable[s[index + 1]]
				if ok {
					// Valid color control code, so switch
					// the foreground color, skip the hex
					// digit, and keep looking for the
					// next "real" character.
					nextForeground = hexDigitToColorTable[s[index + 1]]
					index += 2
					continue
				}

			} else if s[index] == '~' {

				_, ok := hexDigitToColorTable[s[index + 1]]
				if ok {
					// Valid color control code, so switch
					// the background color, skip the hex
					// digit, and keep looking for the
					// next "real" character.
					//
					// TODO: termBox doesn't support a
					// blink attribute like ncurses does,
					// but for some terminals, the blink
					// attribute is the only way to get
					// the 8 high-intensity background
					// colors.  You'd think termBox would
					// give us more control over this.

					nextBackground = hexDigitToColorTable[s[index + 1]]
					index += 2
					continue
				}
			}
		} // end (if we are not at the last character in the string)

		// If control makes it here, we are processing either an
		// ordinary character or a converted escape sequence.
		index += 1
		break

	} // end (while we're still reading characters)

	return c, index, nextForeground, nextBackground
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

	for i := 0; i < len(s); {

		var c byte
		c, i, foreground, background = readNextCharacter(s, i, foreground, background)

		if c == 0 && i >= len(s) {
			// This case should only be reached if we were given a
			// string without any escape sequences or real
			// characters in it.  In that In that case, there is
			// nothing to do.
			return charactersPrinted, nil
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
			cursorPosition.x = x
			cursorPosition.y = y
		}
	}

	return charactersPrinted, nil
} // end (func Print)


// Prints an arbitrary number of arguments in the same manner as fmt.Printf().
func Printf(x, y int, format string, args ...interface{}) (charactersPrinted int, err error) {
	s := fmt.Sprintf(format, args...)
	return Print(x, y, s)
}

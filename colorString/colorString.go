package colorString

import (
	"fmt"
	"github.com/nsf/termbox-go"
)

var _ = fmt.Printf

func trueLength(s string) int {
	length := 0
	for i := 0; i < len(s); i++ {
		// fmt.Printf("%d: '%c' (length == %d)\n", i, s[i], length)
		if s[i] == '`' || s[i] == '~' {
			if i == len(s) - 1 {

				// Count the '`' or '~' as a normal character
				// if it lies at the end of the string.
				length += 1

			} else if (s[i] == '`' && s[i + 1] == '`') ||
				  (s[i] == '~' && s[i + 1] == '~') {

				// '``' and '~~' count as normal characters.
				length += 1
				i += 1

			} else {
				// The escape sequence may be valid (like
				// '`5') or invalid (like '~?'),  but it
				// doesn't count toward the length.  Skip the
				// next character.
				i += 1
			}
		} else if s[i] == '\\' {
			if i == len(s) - 1 {

				// Though it is technically invalid for a
				// backslash to be at the end of the string,
				// we also count it as a single ordinary
				// character for the purposes of this module.
				length += 1

			} else if s[i + 1] == 'r' || s[i + 1] == 'n' ||
				s[i + 1] == 't' || s[i + 1] == 'b' ||
				s[i + 1] == 'v' {

				// We treat these escape sequences as motion
				// commands and do not display them.
				i += 1
			} else {

				// '\\' and other escapes count as a single
				// character.
				length += 1
				i += 1
			}
		} else if s[i] == '\b' || s[i] == '\t' || s[i] == '\r' ||
			s[i] == '\n' || s[i] == '\v' {
			// The actual ASCII control codes are *also*
			// interpreted as motion commands and do not
			// contribute to the true length.  But this time,
			// there's nothing to skip, either.
			length += 0
		} else {
			// Normal character.
			length += 1
		}
	}
	return length
}

// Exports trueLength() for testing.  TODO: Remove this.
func TrueLength(s string) int {
	return trueLength(s)
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
// color switching directives, respectively.  "n" represents a color from 0 to
// 15, represented as a single hexadecimal digit.  To print an actual
// backquote or tilde, use '``' or '~~', respectively.
//
// We can print safely off of the screen boundary.  If the string extends
// beyond the boundaries of the screen, it will be clipped.
func Print(x, y int, s string) (charactersPrinted int, err error) {

	var foreground, background = termbox.ColorWhite, termbox.ColorBlack
	width, height := termbox.Size()
	length := trueLength(s)

	if y < 0 || y >= height {
		return 0, nil
	}
	if x + length < 0 || x >= width {
		return 0, nil
	}

	letterToControlCodeTable := map[byte]byte {
		'b': '\b',
		'f': '\f',
		'n': '\n',
		'r': '\r',
		't': '\t',
		'v': '\v',
	}

	hexDigitToColorTable := map[byte]termbox.Attribute{
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
			// Jump forward to the next tab stop.
			x += (8 - (x & 7))
			if x >= width {
				// You tabbed past the end of the
				// line.
				i = length
				continue
			}
		case '\b':
			// Move backward by one character.
			x -= 1
			if x < 0 {
				// You backspaced before the beginning
				// of the line.
				i = length
				continue
			}
		default:
			// Just print the current character at (x, y).
			// TODO: I thought enumerating through strings yielded
			// runes, not bytes...?
			if x >= 0 {
				termbox.SetCell(x,
					y,
					rune(c),
					foreground,
					background)
				charactersPrinted += 1
			}
			x += 1
			if x >= width {
				// Surpassed the right edge.
				return charactersPrinted, nil
			}
		}


	}

	return charactersPrinted, nil
}

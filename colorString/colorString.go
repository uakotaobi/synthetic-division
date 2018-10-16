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
				s[i + 1] == 't' || s[i + 1] == 'b' {

				// We treat these escape sequences as motion
				// commands and do not display them.
				i += 1
			} else {

				// '\\' and other escapes count as a single
				// character.
				length += 1
				i += 1
			}
		} else if s[i] == '\b' || s[i] == '\t' || s[i] == '\r' || s[i] == '\n' {
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

func TrueLength(s string) int {
	return trueLength(s)
}

// Prints the given string at the given position on the terminal, then returns
// a pair consisting of the number of characters printed and an error (nil
// means no errors.)
//
// We interpret certain codes, namely `n and ~n, as foreground and background
// color switching directives, respectively.  "n" represents a color from 0 to
// 15, represented as a single hexadecimal digit.  To print an actual
// backquote or tilde, use '``' or '~~', respectively.
//
// We can print safely off of the screen boundary.  If the string extends
// beyond the boundaries of the screen, it will be clipped.
func Print(x, y int, s string) (charactersPrinted int, err error) {

	width, height := termbox.Size()

	if y < 0 || y >= height {
		return 0, nil
	}
	if x + len(s) < 0 || x >= width {
		return 0, nil
	}

	// Adjust if we're off-screen to the left.
	firstIndex := 0
	if x < 0 {
		firstIndex -= x
		x = 0
	}

	return 0, nil
}

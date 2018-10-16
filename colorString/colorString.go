package colorString

import (
	"github.com/nsf/termbox-go"
)


func trueLength(s string) int {
	length := 0
	for i := 0; i < len(s); i++ {
		if s[i] == '`' || s[i] == '~' {
			if i == len(s) - 1 {

				// The string mistakenly has an escape
				// sequence at the end.  We don't count it
				// toward the true length since it won't be
				// rendered.
				continue

			} else if (s[i] == '`' && s[i + 1] == '`') ||
				(s[i] == '~' && s[i + 1] == '~') {

				// '``' and '~~' count as normal characters.
				length += 1

			} else {
				// The escape sequence may be valid or invalid,
				// but it doesn't count toward the length.
				// Skip the next character.
				i += 1
			}
		} else if s[i] == '\\' {
			// Traditional C-style escapes.
			if i == len(s) - 1 || (s[i] == '\\' && s[i + 1] == '\\') {
				// Backslashes at the end of the string and
				// '\\' count as single characters.
				length += 1
			} else {
				// Don't count the backslash, but do count the
				// character that follows.
			}
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

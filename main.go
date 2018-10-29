package main

import (
	"fmt"

	"github.com/uakotaobi/synthetic-division/colorString"

	"github.com/nsf/termbox-go"
)

func initializeTerminalLibrary() error {
	err := termbox.Init()
	if err != nil {
		fmt.Printf("Could not initialize termbox-go: %s\n", err)
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

	// TODO: Enable bright background colors.
	message := "`0~7F\n\br\n\bo\n\bg\n\bs`7~0\n\b\b\n\n\nare\v\r     `1b`2e`3a`4u`5t`6i`7f`8u`9l`a~c\n!"
	colorString.Print(0, 5, message)
	x, y := colorString.CursorPosition()
	colorString.Printf(x, y, "`fDon't %s`7~0 %s?", "~1`0you", "`9~0agree")
	termbox.Flush()

	// Press enter to exit
	buf := make([]byte, 10)
	_ = termbox.PollRawEvent(buf)

	closeTerminalLibrary()
}

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
	message := "`f~7Frogs`7~0 are\v\r     `1b`2e`3a`4u`5t`6i`7f`8u`9l`a~c!"
	colorString.Print(5, 5, message)
	termbox.Flush()

	// Press enter to exit
	buf := make([]byte, 10)
	_ = termbox.PollRawEvent(buf)

	closeTerminalLibrary()
}

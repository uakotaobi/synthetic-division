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
	// _ = NewPolynomial("10x^2 + 5y + abs 2x")
	// p2, err2 := NewPolynomial("x(3) + 2")
	// p2, err2 := NewPolynomial("(x^2 + x + 1)*(x^3 - x + 2)")
	// if err2 != nil {
	//	fmt.Printf("ERROR: %v\n", err2.Error())
	// }
	// fmt.Printf("%v\n", p2)
	// p2 := NewUnivariatePolynomial([]float64{-1, 3, 1.0, 0, 0, -1}, "x")
	// p2 := NewUnivariatePolynomial([]float64{1, 0}, "x")
	// q2 := NewUnivariatePolynomial([]float64{1, 0}, "x")
	// p2.Multiply(q2)
	// for i, _ := range p2.Terms {
	//	p2.Terms[i].SortKey()
	//	fmt.Printf("%d: sortKey=%v\n", i, p2.Terms[i].sortKey)
	// }
	// fmt.Printf("%v\n", p2)
	// return

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

	p := NewUnivariatePolynomial([]float64{-1, 3, 1.0, 0, 0, -1}, "x")
	colorString.Printf(x, y, "\r\n\n[%v]", p.ColorString())

	termbox.Flush()

	// Press enter to exit
	buf := make([]byte, 10)
	_ = termbox.PollRawEvent(buf)
}

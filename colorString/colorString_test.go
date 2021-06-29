// Test suite for headless (well, terminal-less) colorString functions.
//
// To execute the test suite, simply run "go test".

package colorString

import (
	"testing"
	"github.com/stretchr/testify/assert"
)

// Confirms that the Substr() function works the way it's expected to.
func TestSubstr(t *testing.T) {

	// These examples come straight out of the Substr() documentation, so
	// they had better work!
	testData := []struct{source string; start, length int; expected string} {
		{ "`7~1Test", 1, 2, "es" },
		{ "`7~1Test", 1, 999, "est" },
		{ "`7~1Test", 0, 2, "`7~1Te" },
		{ "`7~1Test~4String", 0, 5, "`7~1Test~4S" },
		{ "`7~1Test~4String", 0, 4, "`7~1Test" },
		{ "`7~1Test~4String", 0, 0, "" },
		{ "`7~1\b", 0, 100, "`7~1\b" },
		{ "`7~1", 0, 100, "" },
	}

	for _, datum := range testData {
		actual := Substr(datum.source, datum.start, datum.length)
		assert.Equal(t,
			datum.expected,
			actual,
			"Expected Substr(\"%v\", %v, %v) to be '%v', not '%v'",
			datum.source,
			datum.start,
			datum.length,
			datum.expected,
			actual)
	}
}

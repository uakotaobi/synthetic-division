package main

////////////////////////////////////////
// Term.go: Defines the Term struct.  //
////////////////////////////////////////

import (
	"fmt"
	"sort"
	"strings"
)

// A _Term_ consists of a numeric coefficient plus any number of variables
// raised to any number of powers.  We represent the variables and powers with
// a dictionary, such that the term:
//
//   5x²y
//
// is represented by Term{5.0, {"x": 2, "y": 1}}.
//
// When the dictionary is empty, the Term represents a constant.
type Term struct {
	Coefficient float64
	Variables map[string]int
	sortedVariables []string  // Cached after being calculated by SortKey().
	sortKey string            // Also cached after being calculated by SortKey().
}

// Go doesn't have constructors, so this is the closest we get to that.
func NewTerm() Term {
	return Term{0, map[string]int{}, []string{}, ""}
}

// The degree of a Term is the sum of the powers of its variables.
func (term Term) Degree() int {
	degree := 0
	if term.Coefficient != 0 {
		for _, power := range term.Variables {
			degree += power
		}
	}
	return degree
}

// A Term can be combined with another Term via addition or subtraction if and
// only if they have the same variables raised to the same powers.
func (term Term) CompatibleWith(other Term) bool {
	if len(term.Variables) == 0 && len(other.Variables) == 0 {
		// Constants (no attached variables.)
		return true
	}

	if len(term.Variables) != len(other.Variables) {
		// Different numbers of variables in each term.
		return false
	}

	for v, power := range term.Variables {
		_, present := other.Variables[v]
		if !present || other.Variables[v] != power {
			return false
		}
	}
	return true
}

// Returns a string that allows diverse lists of Terms to be sorted in a
// preferred order.  The rules are:
//
// 1. Constants come last and have the (lexicographically) lowest sort key.
// 2. Terms of higher maximum exponent have a higher sort key.
// 3. Among Terms with the same maximum exponent, alphabetically earlier
//    variables sort before alphabetically later ones; x³y has a higher sort
//    key than xy³.
//
// When all is said and done, sorting the Terms in reverse alphabetical
// SortKey() order will yield the preferred order for display.
func (term *Term) SortKey() string {

	if term.sortKey == "" {
		// Let's take a couple of fictitious Terms:
		//
		// 1. 7x(ham²)y³
		// 2. 3x³(ham²)y
		//
		// Both Terms have Degree() 6, so this becomes the prefix for
		// our sort key.
		//
		// Sorting the variables alphabetically and concatenating them
		// yields the nonsense string "hamxy" for both.  Prepending
		// the exponents for each, we get:
		//
		// 1. 6:2ham1x3y
		// 2. 6:2ham3x1y
		//
		// Since 2. comes after 1. lexicographically, 3x³(ham²)y will
		// be listed first.  Note that this sort order ignores the
		// coefficients.
		//
		// -----
		//
		// To make constants come after all this, we simply give them
		// a prefix that sorts lower than any digit in ASCII, like
		// "$".  0-padding takes care of the rest.

		term.sortedVariables = []string{}
		if len(term.Variables) == 0 {
			// Degree 0: a constant.
			term.sortKey = fmt.Sprintf("$%020f", term.Coefficient)
		} else {
			sortKeyEntries := []string{}
			maxExponent := -1
			for variable, power := range term.Variables {
				sortKeyEntries = append(sortKeyEntries, fmt.Sprintf("%d%s", power, variable))
				term.sortedVariables = append(term.sortedVariables, variable)
				if power > maxExponent {
					maxExponent = power
				}
			}
			sort.Strings(term.sortedVariables)
			sort.Strings(sortKeyEntries)
			term.sortKey = fmt.Sprintf("%020d:%s", maxExponent * term.Degree(), strings.Join(sortKeyEntries, ""))
		}
	}
	return term.sortKey
}

// Converts a Term to a plain string.
func (t Term) String() string {
	// We could have called NewPolynomialFromTerm(t).toString("", "", "",
	// "", "") here, but I prefer printing a Term to give me information
	// which is useful for debugging.

	return fmt.Sprintf("{C=%v, V=d%v}", t.Coefficient, t.Variables)
}

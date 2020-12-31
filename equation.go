package main

import (
	"strconv"
	"fmt"
	"math"
)

type Equation struct {
	Coefficients []float64
	variable string
}

// Constants that determine how colored equations look.  These same colors are
// used for consistency throughout the rest of the code.
const DefaultColor, CoefficientColor, VariableColor, ExponentColor, OperatorColor string = "`7", "`3", "`9", "`9", "`8"

// Converts the equation to a string in the form "3x^4 - 7x - 3".
//
// The arguments are used to control the colors of the resulting string.  For
// instance, to make variables gray, set variableColor to "`8".  You can leave
// them all empty for a colorless string.
func (e Equation) toString(defaultColor, coefficientColor, variableColor, exponentColor, operatorColor string) string {

	if len(e.Coefficients) == 0 {
		return "0"
	}

	variableString := fmt.Sprintf("%s%s", variableColor, e.variable)
	_, err := strconv.ParseFloat(e.variable, 64)
	if err == nil {
		// The "variable" is a numeric constant.
		variableString = fmt.Sprintf("%s(%s%f%s)", operatorColor, variableColor, e.variable, operatorColor)

		_, err := strconv.ParseInt(e.variable, 10, 64)
		if err == nil {
			// The "variable" is a numeric integer constant.
			fmt.Sprintf("%s(%s%d%s)", operatorColor, variableColor, e.variable, operatorColor)
		}
	}

	// If we have the coefficients -1, -2, and 0, then I prefer printing
	// this as "-x^2 - 2x" rather than "-1x^2 + -2x + 0".  The terms array
	// collects the terms as we wish for them to appear; we handle the
	// signs later.
	//
	// The keys in the map are exponents (so that terms[1], if it exists,
	// corresponds to what we should print for the x^1 term, and so on.)
	terms := map[int]string{}
	firstTerm := -1

	// Gather the terms.
	for i := len(e.Coefficients) - 1; i >= 0; i-- {

		if e.Coefficients[i] == 0 {
			continue
		}

		if i > firstTerm {
			// This is (currently) the highest term in the result
			// -- that is, the term with the largest exponent.
			firstTerm = i
		}

		// Keep the coefficients positive in the terms array in hopes
		// that we will be able to use subtraction when we get to
		// handling the signs.
		coefficient := math.Abs(e.Coefficients[i])

		coefficientString := fmt.Sprintf(coefficientColor + "%f", coefficient)
		if float64(int64(e.Coefficients[i])) == e.Coefficients[i] {
			// The coefficient round-tripped from float to integer
			// and back.  It is therefore an integer, and can be
			// printed more concisely.
			coefficientString = fmt.Sprintf(coefficientColor + "%d", int(coefficient))
		}

		term := ""

		if i == 0 {
			// X^0 is the constant term.
			term = coefficientString
		} else {
			// X^(anything else.)

			exponentString := fmt.Sprintf("%s^%s%d", operatorColor, exponentColor, i)
			if i == 1 {
				// "13x" looks better than "13x^1".
				exponentString = ""
			}

			if coefficient == 1 {
				// "-x^2" looks better than "-1x^2".
				coefficientString = ""
			}

			term = fmt.Sprintf("%s%s%s",
				coefficientString,
				variableString,
				exponentString)


		}
		terms[i] = term
	}

	// Determine the signs.
	for i, _ := range terms {
		if i == firstTerm {
			if e.Coefficients[i] < 0 {
				// Only one term (or we're dealing with the first
				// term), so the minus sign goes right before the
				// coefficient.
				terms[i] = operatorColor + "-" + terms[i]
			} else {
				// Traditionally, we don't add a leading "+"
				// for positive numbers.
			}
		} else {
			// Prepend a + or - operator as appropriate.
			if e.Coefficients[i] < 0 {
				terms[i] = fmt.Sprintf("%s %s-%s %s", defaultColor, operatorColor, defaultColor, terms[i])
			} else {
				terms[i] = fmt.Sprintf("%s %s+%s %s", defaultColor, operatorColor, defaultColor, terms[i])
			}
		}
	}

	// Emit the final result.
	result := ""
	for i := 0; i < len(e.Coefficients); i++ {
		term, exists := terms[i]
		if exists {
			result = term + result
		}
	}
	return result
}

func (e Equation) String() string {
	return e.toString("", "", "", "", "")
}

func (e Equation) ColorString() string {
	return e.toString(DefaultColor, CoefficientColor, VariableColor, ExponentColor, OperatorColor)
}

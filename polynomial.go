package main

///////////////////////////////////////////////////////////////////////////
// Polynomial.go: Defines the Polynomial struct and all of its functions //
// _except_ NewPolynomial(string) -- see parse.go for that.		 //
///////////////////////////////////////////////////////////////////////////

import (
	"strconv"
	"fmt"
	"math"
	"sort"
)

// Constants that determine how colored equations look.  These same colors are
// used for consistency throughout the rest of the code.
const DefaultColor, CoefficientColor, VariableColor, ExponentColor, OperatorColor string = "`7", "`3", "`9", "`9", "`7"

// A Polynomial is a list of terms, along with some simple rules for
// simplifying and combining them.
type Polynomial struct {
	Terms []Term
}

// The degree of a Polynomial is the maximum degree of its Terms.
func (poly Polynomial) Degree() int {
	degree := 0
	for _, term := range poly.Terms {
		if term.Degree() > degree {
			degree = term.Degree()
		}
	}
	return degree
}

// Multiplies the polynomial by the given floating-point number.
func (poly *Polynomial) MultiplyByConstant(f float64) *Polynomial {
	for i, _ := range poly.Terms {
		poly.Terms[i].Coefficient *= f
	}
	return poly
}

// Adds the given polynomial to this one.
func (poly *Polynomial) Add(p Polynomial) *Polynomial {

	// Graft their terms and ours together.
	terms := []Term{}
	index := 0
	for _, term := range poly.Terms {
		terms = append(terms, term)
		index++
	}

	// Merge compatible terms.
	// fmt.Printf("- Initial set is %v\n", terms)
	for i := 0; i < len(p.Terms); i++ {
		newTerm := p.Terms[i]
		if newTerm.Coefficient == 0 {
			// Optimize away terms that have no value.
			continue
		}

		merged := false
		for j := 0; j < len(terms); j++ {
			if newTerm.CompatibleWith(terms[j]) {
				// Add newTerm to the current term.
				// fmt.Printf("- Added %v to %v, yielding", newTerm, terms[j])
				terms[j].Coefficient += newTerm.Coefficient
				if terms[j].Coefficient == 0 {
					// Optimize away terms that summed to
					// 0.
					copy(terms[j:], terms[j+1:])
					terms = terms[:len(terms) - 1]
				}
				merged = true
				// fmt.Printf("%v\n", terms[j])
				break
			}
		}

		if !merged {
			// newTerm is not compatible with any existing term.
			// Add it as a new term in its own right.
			// fmt.Printf("- Added %v directly\n", newTerm)
			terms = append(terms, newTerm)
		}
	}

	// As a convenience, adding automatically sorts the Terms so the one
	// with the highest power comes first.
	sort.Slice(terms, func(i, j int) bool {
		return terms[i].SortKey() > terms[j].SortKey()
	})

	poly.Terms = terms
	// fmt.Printf("- Final sum is %v\n", poly.Terms)
	return poly
}


// Multiplies this polynomial by the given one.
func (poly *Polynomial) Multiply(p Polynomial) *Polynomial {
	result := NewZeroPolynomial()

	for _, term1 := range poly.Terms {
		for _, term2 := range p.Terms {
			product := NewTerm()
			product.Coefficient = term1.Coefficient * term2.Coefficient
			for v, power1 := range term1.Variables {
				product.Variables[v] = power1
			}
			for v, power2 := range term2.Variables {
				power1, present := term1.Variables[v]
				if present {
					// X^n + X^m = X^(n + m).
					product.Variables[v] = power2 + power1
				} else {
					product.Variables[v] = power2
				}
			}

			// Calculate product.sortedVariables and
			// product.sortKey.
			product.SortKey()

			result.Add(NewPolynomialFromTerm(product))
		}
	}
	poly.Terms = result.Terms

	return poly
}

// Converts the polynomial to a string in the form "3x^4 - 7x - 3".
//
// The arguments are used to control the colors of the resulting string.  For
// instance, to make variables gray, set variableColor to "`8".  You can leave
// them all empty for a colorless string.
func (poly Polynomial) toString(defaultColor, coefficientColor, variableColor, exponentColor, operatorColor string) string {

	if len(poly.Terms) == 0 {
		return coefficientColor + "0"
	}

	// Sort the Terms in SortKey() order (reverse lexicographical order).
	//
	// This also precalculates poly.Terms[n].sortedVariables.
	sortedTerms := make([]Term, len(poly.Terms))
	copy(sortedTerms, poly.Terms)
	sort.Slice(sortedTerms, func(i, j int) bool {
		return sortedTerms[i].SortKey() > sortedTerms[j].SortKey()
	})

	indexOfFirstNonzeroTerm := -1
	termStrings := map[int]string{} // The keys are indices into sortedTerms; the values are the converted strings themselves.

	for sortedTermIndex, term := range sortedTerms {

		termString := fmt.Sprintf("%s0", coefficientColor)

		// Keep the coefficients positive in the terms array in hopes
		// that we will be able to use subtraction when we get to
		// handling the signs.
		coefficient := math.Abs(term.Coefficient)

		if term.Coefficient != 0 {

			if indexOfFirstNonzeroTerm < 0 {
				indexOfFirstNonzeroTerm = sortedTermIndex
			}
			termString = ""

			// Print the coefficient as an integer if we can
			// treat it as such.
			coefficientString := fmt.Sprintf("%s%f", coefficientColor, coefficient)
			if coefficient == 1 && len(term.Variables) > 0 {

				// "-x^2" looks better than "-1x^2".
				coefficientString = ""

			} else if float64(int64(term.Coefficient)) == term.Coefficient {

				// The coefficient round-tripped from float to integer
				// and back.  It is therefore an integer, and can be
				// printed more concisely.
				coefficientString = fmt.Sprintf("%s%d", coefficientColor, int(coefficient))
			}
			termString += coefficientString

			// fmt.Printf("Term %d: termString is currently (\"%s\")(sortKey=%v, sortedVariables=%v)\n", sortedTermIndex, termString, term.sortKey, term.sortedVariables)

			// A variable (and its exponent) needs to be surrounded with
			// parentheses if:
			//
			// 1. The term's name is more than one character long, or:
			// 2. The "variable" is actually a numeric constant; or:
			// 3. There is more than one Term, and the current Term is
			//    not the rightmost one.
			for variableIndex, v := range term.sortedVariables {
				exponent := term.Variables[v]
				// fmt.Printf("  Term %d, variable #%d (\"%s\"): exponent = %d\n", sortedTermIndex, variableIndex, v, exponent)
				exponentString := fmt.Sprintf("%s^%s%d", operatorColor, exponentColor, exponent)
				if exponent == 1 {
					// "z" looks better than "z^1".
					exponentString = ""
				}

				_, err := strconv.ParseFloat(v, 64)

				if len(v) > 1 || err == nil || (len(term.sortedVariables) > 1 && variableIndex < len(term.sortedVariables) - 1) {
					// Needs parentheses.
					termString += fmt.Sprintf("%s(%s%s%s%s)",
						operatorColor,
						variableColor,
						v,
						exponentString,
						operatorColor)
				} else {
					// Doesn't need parentheses.
					termString += fmt.Sprintf("%s%s%s",
						variableColor,
						v,
						exponentString)
				}
			}
			termStrings[sortedTermIndex] = termString
		}
	}

	// If all the terms have coefficients of 0, the Polynomial is the zero polynomial.
	if len(termStrings) == 0 {
		return fmt.Sprintf("%s0", coefficientColor)
	}

	// Determine the signs.
	for i, _ := range termStrings {
		if i == indexOfFirstNonzeroTerm {
			if sortedTerms[i].Coefficient < 0 {
				// Only one term (or we're dealing with the first
				// term), so the minus sign goes right before the
				// coefficient.
				termStrings[i] = operatorColor + "-" + termStrings[i]
			} else {
				// Traditionally, we don't add a leading "+"
				// for positive numbers.
			}
		} else {
			// Prepend a + or - operator as appropriate.  This is
			// also where we add the spaces that separate terms.
			if sortedTerms[i].Coefficient < 0 {
				termStrings[i] = fmt.Sprintf("%s %s-%s %s", defaultColor, operatorColor, defaultColor, termStrings[i])
			} else {
				termStrings[i] = fmt.Sprintf("%s %s+%s %s", defaultColor, operatorColor, defaultColor, termStrings[i])
			}
		}
	}

	// Emit the final result in SortedKey order.
	result := ""
	for i, _ := range sortedTerms {
		termString, exists := termStrings[i]
		if exists {
			result += termString
		}
	}
	return result
}

// Converts a polynomial to a plain string.
func (p Polynomial) String() string {
	return p.toString("", "", "", "", "")
}

// Converts a polynomial to a colorful string that can be interpreted by colorPrint().
func (p Polynomial) ColorString() string {
	return p.toString(DefaultColor, CoefficientColor, VariableColor, ExponentColor, OperatorColor)
}

// Makes a new Polynomial of a single variable.
func NewUnivariatePolynomial(coefficients []float64, v string) Polynomial {
	var result Polynomial

	for i := 0; i < len(coefficients); i++ {
		if coefficients[i] == 0 {
			continue
		}
		variables := map[string]int{}
		if i < len(coefficients) - 1 {
			// If variable == "x" and i == 2, then the dictionary
			// {"x": 2} represents the term "x^2".  Meanwhile, the
			// dictionary {}, reserved for the final term,
			// represents x^0.
			variables[v] = len(coefficients) - 1 - i
		}

		// Start each new Term with a precalcualted sortKey.
		term := NewTerm()
		term.Coefficient = coefficients[i]
		term.Variables = variables
		term.SortKey()

		result.Terms = append(result.Terms, term)
	}

	return result
}

// ===========================================================================
// These notes below have nothing to do with Polynomial objects themselves!
// They are all _factorings_ of the Polynomial -- recombinations of Terms in
// order to present a Polynomial in a different way.  The coefficients in
// these cases are _themselves_ (basic) Polynomials.  What's more, these
// expressions can be multiplied out and simplified into a basic Polynomial.
//
// What's being described here are _Polynomial Expressions_, or at least one
// form of them.  Factoring a Polynomial leads to an expression, and has
// nothing to do with synthetic division.
// ===========================================================================
// // A true polynomial combines any number of univariate polynomials, each using
// // a different variable (these are the keys in the map.)
// //
// // The polynomial can be treated as a univariate polynomial where the other
// // terms are all considered constant with respect to that variable.  For
// // instance, 5xy - zx + y can be treated as
// //
// //     (5y - z)x¹ + (y)x⁰
// //
// // with respect to x, or as
// //
// //     (5x + 1)y¹ + (-zx)y⁰
// //
// // with respect to y, or as
// //
// //     (-x)z¹ + (5xy + y)z⁰
// //
// // with respect to z.  So each coefficient of a polynomial with respect to
// // some variable is really itself a polynomial of lower degree which lacks
// // that variable.
// ===========================================================================

// Returns an empty Polynomial.
func NewZeroPolynomial() Polynomial {
	var p Polynomial = Polynomial{[]Term{}}
	return p
}

// Since a Polynomial is, by definition, a list of Terms in any order, making
// a Polynomial from a single Term is trivial.
func NewPolynomialFromTerm(t Term) Polynomial {
	var p Polynomial
	p.Terms = []Term{t}
	return p
}

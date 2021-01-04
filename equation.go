package main

import (
	"strconv"
	"fmt"
	"math"
	"unicode"
	"errors"
	"regexp"
	"sort"
	"strings"
)

// An expression tree is a sequence of tokens in postfix order.

// A _term_ consists of a numeric coefficient plus any number of variables
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

// The degree of a Term is the sum of the powers of its variables.
func (term Term) Degree() (degree int) {
	degree = 0
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
			term.sortKey = fmt.Sprintf("%d:%s", maxExponent, strings.Join(sortKeyEntries, ""))
		}
	}
	return term.sortKey
}

// A Polynomial is a list of terms, along with some simple rules for
// simplifying and combining them.
type Polynomial struct {
	Terms []Term
}

// Multiplies the polynomial by the given floating-point number.
func (poly *Polynomial) MultiplyByConstant(f float64) {
	for i, _ := range poly.Terms {
		poly.Terms[i].Coefficient *= f
	}
}

// Adds the given polynomial to this one.
func (poly *Polynomial) Add(p Polynomial) {

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
		merged := false
		for j := 0; j < len(terms); j++ {
			if newTerm.CompatibleWith(terms[j]) {
				// Add newTerm to the current term.
				// fmt.Printf("- Added %v to %v, yielding", newTerm, terms[j])
				terms[j].Coefficient += newTerm.Coefficient
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

	poly.Terms = terms
	// fmt.Printf("- Final sum is %v\n", poly.Terms)
}


// Converts the equation to a string in the form "3x^4 - 7x - 3".
//
// The arguments are used to control the colors of the resulting string.  For
// instance, to make variables gray, set variableColor to "`8".  You can leave
// them all empty for a colorless string.
func (poly Polynomial) toString(defaultColor, coefficientColor, variableColor, exponentColor, operatorColor string) string {

	if len(poly.Terms) == 0 {
		return coefficientColor + "0"
	}

	// Sort the Terms in SortKey() order.
	//
	// This also precalculates poly.Terms[n].sortedVariables.
	sortedTerms := make([]Term, len(poly.Terms))
	copy(sortedTerms, poly.Terms)
	sort.Slice(sortedTerms, func(i, j int) bool {
		return sortedTerms[i].SortKey() < sortedTerms[j].SortKey()
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

			indexOfFirstNonzeroTerm = sortedTermIndex
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

			// fmt.Printf("Term %d: termString is currently (\"%s\")\n", sortedTermIndex, termString)

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
			result = termString + result
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
		variables := map[string]int{}
		if i < len(coefficients) - 1 {
			// If variable == "x" and i == 2, then the dictionary
			// {"x": 2} represents the term "x^2".  Meanwhile, the
			// dictionary {}, reserved for the final term,
			// represents x^0.
			variables[v] = len(coefficients) - 1 - i
		}
		result.Terms = append(result.Terms, Term{coefficients[i], variables, []string{}, ""})
	}

	return result
}

// These notes below have nothing to do with Polynomial objects themselves!
// They are all _factorings_ of the Polynomial -- recombinations of Terms in
// order to present a Polynomial in a different way.  The coefficients in
// these cases are _themselves_ (basic) Polynomials.  What's more, these
// expressions can be multiplied out and simplified into a basic Polynomial.
//
// What's being described here are _Polynomial Expressions_, or at least one
// form of them.  Factoring a Polynomial leads to an expression, and has
// nothing to do with synthetic division.
//
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

// Constants that determine how colored equations look.  These same colors are
// used for consistency throughout the rest of the code.
const DefaultColor, CoefficientColor, VariableColor, ExponentColor, OperatorColor string = "`7", "`3", "`9", "`9", "`7"

////////////////////////////////
// Shunting-Yard Parser code  //
////////////////////////////////
//
// The following subroutines implement an infix parser that can convert
// strings such as "x^2 - 1" into polynomial Equation objects.  Limitations:
//
// - While any number of variables can be present in the input string, a valid
//   polynomial Equation contains only one variable.
// - Unary functions are supported (and thus expressions containing functions
//   can be fully evaluated),

const (
	operatorToken int = iota
	symbolToken
	functionToken
	numberToken
)

type token struct {
	tokenType int
	inputPosition int
	text string
	operatorValue operator
	numericValue float64
	functionValue function
}

// Prints a token.
func (t token) String() string {
	switch (t.tokenType) {
	case operatorToken:
		return fmt.Sprintf("%v", t.operatorValue.name)
	case symbolToken:
		return fmt.Sprintf("SYMBOL(\"%v\")", t.text)
	case numberToken:
		return fmt.Sprintf("NUMBER(%v)", t.numericValue)
	case functionToken:
		return fmt.Sprintf("FUNCTION(\"%v\")", t.text)
	default:
		return fmt.Sprintf("UNRECOGNIZED-TOKEN-TYPE(\"%v\")", t.text)
	}
}

type operatorType int
const (
	unary operatorType = iota
	binary
	other
)

const (
	leftAssociativity = -1
	rightAssociativity = +1
	noAssociativity = 0
)

type operator struct {
	name string
	type_ operatorType
	precedence int
	associativity int // Negative for left, positive for right, 0 for undetermined
	function int      // TODO: This should be the operator's evaluation routine.
}

// Note that all unary operators must be right-associative and have higher
// precedence than the binary operators in order to fool Shunting Yard into
// handling them.
var operators = map[string]operator {
	"(":   operator{"LEFT-PAREN",  other, 200, noAssociativity,    0},
	")":   operator{"RIGHT-PAREN", other, 200, noAssociativity,    0},
	"!!+": operator{"UNARY +",     unary, 100, rightAssociativity, 0}, // Deduced automatically during scanning
	"!!-": operator{"UNARY -",     unary, 100, rightAssociativity, 0}, // Deduced automatically during scanning
	"^":   operator{"EXPONENT",    binary, 90, rightAssociativity, 0},
	"*":   operator{"MULTIPLY",    binary, 80, leftAssociativity,  0},
	"/":   operator{"DIVIDE",      binary, 80, leftAssociativity,  0},
	"+":   operator{"ADD",         binary, 60, leftAssociativity,  0},
	"-":   operator{"SUBTRACT",    binary, 60, leftAssociativity,  0},
}

type function struct {
	argumentCount int
	function int // TODO: This should be the function's evaluation routine.
}

var functions = map[string]function {
	"abs": function{1, 0},
}

/////////////////////////////////////////////////////////////////////////////////////
// The scanner converts an incoming string into an (infix-)ordered list of tokens. //
/////////////////////////////////////////////////////////////////////////////////////

type parser struct {
	tokens []token
}

// Converts the given input string into a list of tokens.
func (p *parser) scan(inputString string) error {

	p.tokens = []token{}
	i := 0
	for i < len(inputString) {

		c := inputString[i]

		if unicode.IsSpace(rune(c)) {
			i = scanWhiteSpace(inputString, i)
		} else {
			var op string
			var err error
			i, op, err = scanOperator(inputString, i)
			if err == nil {
				// Operator found in stream.
				var t token
				t.tokenType = operatorToken
				t.inputPosition = i - len(op)
				t.text = op
				t.operatorValue = operators[op]
				p.tokens = append(p.tokens, t)
			} else {
				var t token
				i, t, err = scanNumberOrSymbol(inputString, i)
				if err == nil {
					// Number or symbol found in stream.
					p.tokens = append(p.tokens, t)
				} else {
					// Unrecognized garbage.
					return errors.New(fmt.Sprintf("Unrecognized token '%c' at position %v of input string", inputString[i], i + 1))
				}
			}
		}
	} // end (while we have not exhausted the input string)
	return nil
}

// Eats all whitespace starting from the index, and returns a new index that
// is either past the end of the string or whitespace-free.
func scanWhiteSpace(inputString string, index int) int {
	result := index
	for result < len(inputString) && unicode.IsSpace(rune(inputString[result])) {
		result++
	}
	return result
}

// Attempts to read an operator at the given index.  Upon success, returns the
// operator string and the position just after it; upon failure, returns an
// error and the original index.
func scanOperator(inputString string, index int) (newIndex int, text string, err error) {
	for key, _ := range operators {
		if len(inputString) - index >= len(key) && key == inputString[index:index + len(key)] {
			return index + len(key), key, nil
		}
	}
	return index, "", errors.New("No operator found")
}

// Attempts to read a number or symbol token at the given index.  Upon
// success, returns the corresponding token object and the position just after
// it; upon failure, returns an error and the original index.
func scanNumberOrSymbol(inputString string, index int) (newIndex int, t token, err error) {

	const floatingPointRegexpBasic string = "^[+-]?([0-9]*[.])?[0-9]+"
	const floatingPointRegexpComplete = "^[+-]?([0-9]+([.][0-9]*)?([eE][+-]?[0-9]+)?|[.][0-9]+([eE][+-]?[0-9]+)?)"

	var floatingPointRegexp = regexp.MustCompile(floatingPointRegexpComplete)

	// Try to read the next float or int.
	match := floatingPointRegexp.FindString(inputString[index:])

	if match != "" {
		var t token
		t.tokenType = numberToken
		t.inputPosition = index
		t.text = match
		t.numericValue, err = strconv.ParseFloat(match, 64)
		if err == nil {
			return index + len(match), t, nil
		} else {
			// Even though the string matched the regex, we still
			// couldn't convert it to a float64.  This shouldn't
			// happen.
			return index, t, err
		}
	}

	var identifierRegexp = regexp.MustCompile("^[a-zA-Z_][a-zA-Z0-9_]*")

	// Try to read the next symbol.
	match = identifierRegexp.FindString(inputString[index:])
	if match != "" {
		var t token
		t.text = match
		t.inputPosition = index

		// Some symbols are reserved as functions.
		isFunction := false
		for key, _ := range functions {
			if key == match {
				isFunction = true
				t.tokenType = functionToken
				t.functionValue = functions[key]
				break
			}
		}

		if !isFunction {
			// Just a normal symbol.
			t.tokenType = symbolToken
		}

		return index + len(match), t, nil
	}

	var unused token
	return index, unused, errors.New("No numeric or symbol token found")
}

//////////////////////////////////////////////////////////////////
// The parser converts an (infix-)ordered list of tokens into a //
// (postfix-)ordered list of tokens.				//
//////////////////////////////////////////////////////////////////

// Converts p.tokens from infix order to postfix order, suitable for direct
// evaluation.
//
// This function worked the first time it was run, in case you were wondering.
func (p *parser) parse() error {
	outputQueue := []token{}
	operatorStack := []token{}

	type unaryState int
	const (
		defaultState unaryState = iota // During this state, - and + are unary operators.
		infixState                     // During this state, - and + are binary operators.
	)
	var state unaryState

	for index, t := range p.tokens {

		switch (t.tokenType) {
		case numberToken:
			// Always push numbers to the output queue.
			outputQueue = append([]token{t}, outputQueue...)

			// As a special bit of finesse, if the next token is a
			// symbol ("10x") or a function ("10 sin(x)"), we assume implicit
			// multiplication and enqueue the multiplication
			// operator automatically.
			if index < len(p.tokens) - 1 && (p.tokens[index + 1].tokenType == symbolToken || p.tokens[index + 1].tokenType == functionToken) {
				var op token
				op.tokenType = operatorToken
				op.text = "*"
				op.inputPosition = t.inputPosition + len(t.text)
				op.operatorValue = operators["*"]
				operatorStack = append(operatorStack, op)
			}

			// We're at the end of a primary expression.  Addition
			// and subtraction are now expected to be binary.
			state = infixState
		case symbolToken:
			// Symbols are numbers at heart.  Push them directly
			// to the output queue.
			outputQueue = append([]token{t}, outputQueue...)

			// Symbols are numbers for the purpose of unary
			// operators, too.
			state = infixState
		case functionToken:
			// _Functions_ are pushed to the operator stack.
			operatorStack = append(operatorStack, t)
		case operatorToken:
			switch (t.operatorValue.name) {
			case "LEFT-PAREN":
				// Push left parentheses onto the operator stack.
				operatorStack = append(operatorStack, t)
			case "RIGHT-PAREN":
				// Pop operators from the operator stack onto
				// the output queue until the top operator is
				// a left parenthesis.
				for {
					if len(operatorStack) == 0 {
						// No left parenthesis on the
						// stack at all.
						return errors.New(fmt.Sprintf("Unmatched right parenthesis at position %v of input string", t.inputPosition + 1))
					} else if operatorStack[len(operatorStack) - 1].operatorValue.name == "LEFT-PAREN" {
						break
					} else {
						var operatorToken token
						operatorToken, operatorStack = operatorStack[len(operatorStack) - 1], operatorStack[:len(operatorStack) - 1]
						outputQueue = append([]token{operatorToken}, outputQueue...)
					}
				}

				// Discard the left parenthesis that is now at
				// the top of the stack.
				operatorStack = operatorStack[:len(operatorStack) - 1]

				// If we now see a function token at the top
				// of the operator stack, pop it and push it
				// onto the output queue.
				if len(operatorStack) > 0 && operatorStack[len(operatorStack) - 1].tokenType == functionToken {
					var functionToken token
					functionToken, operatorStack = operatorStack[len(operatorStack) - 1], operatorStack[:len(operatorStack) - 1]
					outputQueue = append([]token{functionToken}, outputQueue...)
				}

				// Right parentheses mark the end of primary
				// expressions, too.
				state = infixState
			default:
				// Ordinary operators.

				// Handle unary + and unary -.
				if state == defaultState {
					switch (t.operatorValue.name) {
					case "SUBTRACT":
						t.operatorValue = operators["!!-"]
					case "ADD":
						t.operatorValue = operators["!!+"]
					}
				}

				if (len(operatorStack) > 0 &&
					operatorStack[len(operatorStack) - 1].tokenType == operatorToken) {
					// There is an operator at the top of the
					// operator stack.
					topOperator := operatorStack[len(operatorStack) - 1]

					// Pop operators from the stack as long as:
					//
					// 1a. They have greater precedence than the
					//     current token, t, or
					// 1b. They have equal precedence to the
					//     current token and t is left-associative,
					//     AND...
					// 2.  The operator at the top of the stack is
					//     not a "(".
					//
					// The popped operators go to the output
					// queue.
					for (topOperator.operatorValue.precedence > t.operatorValue.precedence ||
						topOperator.operatorValue.precedence == t.operatorValue.precedence && t.operatorValue.associativity == leftAssociativity) &&
						topOperator.operatorValue.name != "LEFT-PAREN" {

						var operatorToken token
						operatorToken, operatorStack = operatorStack[len(operatorStack) - 1], operatorStack[:len(operatorStack) - 1]

						outputQueue = append([]token{operatorToken}, outputQueue...)
						if len(operatorStack) == 0 {
							break
						}
						topOperator = operatorStack[len(operatorStack) - 1]
					}
				}

				// Push the new operator onto the operator stack.
				operatorStack = append(operatorStack, t)

				// Revert to a state where we can accept new
				// unary operators.
				state = defaultState
			}
		}
	} // end (while there are still tokens to read)

	// Pop any remaining operators to the output queue.
	for len(operatorStack) > 0 {
		var operatorToken token
		operatorToken, operatorStack = operatorStack[len(operatorStack) - 1], operatorStack[:len(operatorStack) - 1]
		if operatorToken.operatorValue.name == "LEFT-PAREN" {
			return errors.New(fmt.Sprintf("Unmatched left parenthesis at position %v of input string", operatorToken.inputPosition + 1))
		} else if operatorToken.operatorValue.name == "RIGHT-PAREN" {
			return errors.New(fmt.Sprintf("Unmatched right parenthesis at position %v of input string", operatorToken.inputPosition + 1))
		} else {
			outputQueue = append([]token{operatorToken}, outputQueue...)
		}
	}

	// Reverse the output queue and we're done.
	for left, right := 0, len(outputQueue) - 1; left < right; left, right = left + 1, right - 1 {
		outputQueue[left], outputQueue[right] = outputQueue[right], outputQueue[left]
	}
	p.tokens = outputQueue
	return nil
}


// The evaluator converts a (postfix-)ordered list of tokens into a polynomial
// Equation (if it can.)
func NewPOlynomial(s string) Polynomial {
	var myParser parser

	err := myParser.scan(s)
	if err == nil {
		err = myParser.parse()
		if err == nil {
			for _, t := range myParser.tokens {
				fmt.Printf("%v, ", t.String())
			}
			fmt.Printf("\n")
		}
	}

	if err != nil {
		fmt.Printf("Error: %v\n", err.Error())
	}

	var unused Polynomial
	return unused
}

package main

import (
	"strconv"
	"fmt"
	"math"
	"unicode"
	"errors"
	"regexp"
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

// Converts an equation to a plain string.
func (e Equation) String() string {
	return e.toString("", "", "", "", "")
}

// Converts an equation to a colorful string that can be interpreted by colorPrint().
func (e Equation) ColorString() string {
	return e.toString(DefaultColor, CoefficientColor, VariableColor, ExponentColor, OperatorColor)
}

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
func NewEquation(s string) Equation {
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

	var unused Equation
	return unused
}

// High-level test suite for the polynomial class.
package main

import (
	"github.com/stretchr/testify/assert"
	"testing"
)

// Tests a few manipulations of a simple polynomial.
func TestAdvancedManipulation(t *testing.T) {

	// (a + b)(a - b) multiplies out to a² - b².  So....
	p, _ := NewPolynomial("Eggs^2 - 1")
	q, _ := NewPolynomial("Eggs^2 + 1")
	r, _ := NewPolynomial("Eggs^4 + 1")

	// p * q       == Eggs⁴ - 1
	// (p * q * r) == Eggs⁸ - 1
	p.Multiply(q).Multiply(r)
	assert.Equal(t, len(p.Terms), 2, "Expected (%v) * (%v) * (%v) to have two terms", p, q, r)
	assert.Equal(t, p.Terms[0].Coefficient, 1.0, "Expected the first coefficient of %v to be 1", p)
	assert.Equal(t, p.Terms[1].Coefficient, -1.0, "Expected the second coefficient of %v to be -1", p)
	assert.Equal(t, p.Terms[0].Variables, map[string]int{"Eggs": 8}, "Expected the first variable of %v to be Eggs^8", p)
	assert.Equal(t, p.Terms[1].Variables, map[string]int{}, "Expected the second variable of %v to be empty", p)
	assert.Equal(t, p.String(), "(Eggs^8) - 1", "Unexpected string conversion of '%v'", p)

	// Throw in an additional term directly, without addition.
	p.Terms = append(p.Terms, Term{60, map[string]int{"y": 4, "x": 3, "z": 1}, nil, ""})
	assert.Equal(t, p.String(), "(Eggs^8) + 60(x^3)(y^4)z - 1", "Unexpected string conversion of '%v'", p)

	// Throw in an easy multiplication.
	p.MultiplyByConstant(2)
	assert.Equal(t, p.String(), "2(Eggs^8) + 120(x^3)(y^4)z - 2", "Unexpected string conversion of '%v'", p)
}

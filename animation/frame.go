package animation

// A low-level construct representing a picture in time, more or less.
type Frame struct {

	// A colorString representing the contents of this frame.  Don't
	// forget that thanks to support for escape sequences, a single string
	// can represent a lot of text in a lot of different places.
	content string

	// The screen position of the upper left corner of this frame.  The
	// coordinates are floating point; we round to the nearest integer
	// when determining where to place cells.
	position Point

	// At what point in the overall animation sequence this frame should
	// appear, measured in milliseconds since the start of the animation
	// sequence.
	timestamp int
}

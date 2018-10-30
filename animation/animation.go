// An animation is an array of colorStrings (at least one) representing the
// different frames of the animation.

package animation


// An animation is just a sequence of frames, usually tweened by higher-level
// functions.
type Animation struct {

	// The frames of the animation.  There needs to be at least one.
	frames []Frame
}

// You can combine two animations by combining their frames.  This function
// also updates their timestamps so one takes place after the other.
func Combine(firstAnimation, secondAnimation Animation) Animation {
	runningTimestamp := 0
	result := Animation{}
	for _, frame := range firstAnimation.frames {
		result.frames = append(result.frames, frame)
		result.frames[len(result.frames) - 1].timestamp = runningTimestamp
		runningTimestamp += frame.timestamp
	}
	for _, frame := range secondAnimation.frames {
		result.frames = append(result.frames, frame)
		result.frames[len(result.frames) - 1].timestamp = runningTimestamp
		runningTimestamp += frame.timestamp
	}
	return result
}

// An animation is an array of colorStrings (at least one) representing the
// different frames of the animation.

package animation

import (
	"time"

	"github.com/uakotaobi/synthetic-division/utils"
	"github.com/uakotaobi/synthetic-division/colorString"
)


// An animation is just a sequence of frames, usually tweened by higher-level
// functions.
type Animation struct {

	// The frames of the animation.  There needs to be at least one.
	frames []Frame

	// How long the animation is intended to last.  This can be adjusted
	// at playback time.
	durationMilliseconds int

	// Should the animation repeat when finished?
	//
	// Note that setting this causes the currentFrame field to reset from
	// len(frames) - 1 to 0 at the end of each animation, so only Stop()
	// can halt the animation entirely.
	loop bool

	// The current frame that we're displaying.  When this is equal to the
	// len(frames), animation is complete.
	currentFrame chan int
}

// You can combine two animations by concatenating their frames.  This
// function also updates their timestamps so one takes place after the other.
func Concat(firstAnimation, secondAnimation Animation) Animation {
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

// Returns true if the animation is complete and false otherwise.
func (animation Animation) Done() bool {
	v := <- animation.currentFrame
	return (v >= len(animation.frames))
}

// Displays an animation after the given delay and at the given playback
// speed.
//
// Parameters:
//
// - position: An offset vector that will be added to each of the animation's
//             frames prior to rendering.  Use this to easily reposition
//             animations at arbitrary locations on the screen.
//
// - delayMilliseconds: The number of milliseconds to wait before playing the
//                      animation.
//
// - playbackSpeed: A floating point number greater than 0.  An N-second
//                  animation will complete in n * playbackSpeed seconds.
//
// This function may be run asynchronously.
func (animation Animation) PlayWithPositionDelayAndPlaybackSpeed(position utils.Position, delayMilliseconds int, playbackSpeed float64) {

	// Frames shouldn't be out of order, but we need to account for the
	// fact that they might be.  So we sort the frames first.

	time.Sleep(time.Duration(delayMilliseconds) * time.Millisecond)

	for index, frame := range(animation.frames) {

		// If the animation is done, don't play it until it is rewound.
		if index := <- animation.currentFrame; index >= len(animation.frames) {
			return
		}

		animation.currentFrame <- index

		framePosition := frame.position
		framePosition.X += position.X
		framePosition.Y += position.Y

		// Show the current frame.
		colorString.Print(int(framePosition.Y), int(framePosition.Y), frame.content)

		if index < len(animation.frames) - 1 {

			// Sleep between frames.
			nextFrame := animation.frames[index + 1]
			nextFrameDelayMilliseconds := float64(nextFrame.timestamp - frame.timestamp) * playbackSpeed
			time.Sleep(time.Duration(nextFrameDelayMilliseconds) * time.Millisecond)

		} else if animation.loop && index == len(animation.frames) - 1 {

			// Looping animation: Don't let currentFrames get past
			// the end.
			animation.currentFrame <- 0
		}

	} // end (for each frame)
}

// Stops an animation if it is in progress.  This is the same as skipping past
// the end.
func (animation Animation) Stop() {
	animation.currentFrame <- len(animation.frames)
}

// Rewinds an animation to the beginning.
func (animation Animation) Rewind() {
	animation.currentFrame <- 0
}

// Displays an animation immediately at the ordinary playback speed.
func (animation Animation) Play() {
	var p utils.Position
	animation.PlayWithPositionDelayAndPlaybackSpeed(p, 0, 1.0)
}

// Displays an animation immediately at the given position, with no delay and
// an ordinary playback speed.
func (animation Animation) PlayWithPosition(position utils.Position) {
	animation.PlayWithPositionDelayAndPlaybackSpeed(position, 0, 1.0)
}

// Displays an animation after the given delay, with the ordinary playback speed.
func (animation Animation) PlayWithDelay(delayMilliseconds int) {
	var p utils.Position
	animation.PlayWithPositionDelayAndPlaybackSpeed(p, delayMilliseconds, 1.0)
}

// Displays an animation immediately at the given position, with an ordinary
// playback speed.
func (animation Animation) PlayWithPositionAndDelay(position utils.Position, delayMilliseconds int) {
	animation.PlayWithPositionDelayAndPlaybackSpeed(position, delayMilliseconds, 1.0)
}

# Frame rate of a Tracks object

Attach a capture frame rate (frames per second) to a \[Tracks\] so the
time aspect of frame-indexed tracks can be represented in real seconds.
Stored in the object's metadata; the time/frame column is not altered.

## Usage

``` r
frame_rate(x)

# S4 method for class 'Tracks'
frame_rate(x)

set_frame_rate(x, fps)

# S4 method for class 'Tracks'
set_frame_rate(x, fps)
```

## Arguments

- x:

  A \[Tracks\] object.

- fps:

  A single positive number, frames per second.

## Value

\`frame_rate()\` the stored fps (or \`NULL\`); \`set_frame_rate()\` the
modified \`Tracks\`.

## See also

\[elapsed_seconds()\], \[track_duration()\]

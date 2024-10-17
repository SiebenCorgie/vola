

# Automatic Differentiation

Vola is able to build the derivative of any function with respect to any parameter to that function.
In practice, this looks like this:

```
impl SomeOperation<sub> for SomeConcept(position){
  let signed_distance = eval sub.Sdf3d(position);
  //This builds the derivative of `signed_distance` with respect to all three `position` axis.
  //the result is therefore a vec3.
  diff(signed_distance, position);
}
```

The `with-respect-to`/`wrt` argument can be specified any way you want. If the `wrt` argument is not used in the
function that is being differentiated, then the derivative will always be 0.


```
impl SomeOperation<sub> for SomeConcept(position){
  let signed_distance = eval sub.Sdf3d(position);

  //Building the derivative with respect to each postion axis.
  let dx = diff(signed_distance, position.x);
  let dy = diff(signed_distance, position.y);
  let dz = diff(signed_distance, position.z);

  dz
}
```

# What about forward vs. backward, optimization etc.?

Vola tries to be _easy to use_. That is why all the decisions whether to use forward or backward mode, which optimization to use etc. are all done by the compiler itself. As far as the user is concerned, you will always just get the derivative with respect to the chosen parameters.


# Testing

The general compilation is tested as part of the compiler integration tests in `tests` by `tests/test-runner`. There is also a differential testing repository against [Enzyme](https://enzyme.mit.edu/) [here](https://gitlab.com/tendsinmende/vola-tests).

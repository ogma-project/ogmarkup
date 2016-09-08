# ogmarkup [![Build Status](https://travis-ci.org/ogma-project/ogmarkup.svg?branch=dev)](https://travis-ci.org/ogma-project/ogmarkup)

ogmarkup is a lightweight markup language for story writers. `ogmarkup` also
refers to a haskell library that provides a generic conversion function to
transform an ogmarkup document into a ready-to-publish document.

## Using `ogmarkup` in your own project

`ogmarkup` is available on Hackage but not on Stackage. If you are using
**stack**, yo can add the following lines in your `stack.yaml` file:

```yaml
# ...
extra-deps:
  - ogmarkup-<version>
# ...
```

If you use Hakyll to build your website, you may want to take a look at
[hakyll-ogmarkup](http://hackage.haskell.org/package/hakyll-ogmarkup) which
brings the two projects together.

## Documentation

The documentation of the `ogmarkup` library can be found online [on
Hackage](http://hackage.haskell.org/package/ogmarkup) and the project has [its
own OpenHub page](https://www.openhub.net/p/ogmarkup).

## Version number policy

From a release `ogmarkup-a.b.c`, if a change breaks the private interface of
`ogmarkup` (that is `Text.Ogmarkup.Private` modules) then `b` is incremented. If
a change breaks the public interface (that is `Text.Ogmarkup`) then `a` is
incremented. Otherwise, just `c` is incremented.

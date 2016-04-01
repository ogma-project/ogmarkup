# ogmarkup [![Build Status](https://travis-ci.org/ogma-project/ogmarkup.svg?branch=dev)](https://travis-ci.org/ogma-project/ogmarkup)

ogmarkup is a lightweight markup language for story writers. `ogmarkup` also
refers to a haskell library that provides a generic conversion function to
transform an ogmarkup document into a ready-to-publish document.

You can try to write your first ogmarkup document by using the [Ogmarkup
Demonstrator](http://ogma.ikyushii.eu/). It is an [open
source](https://github.com/ogma-project/ogmarkup-demo) online editor wich relies
on the `ogmarkup` library to provides a live preview feature.

`ogmarkup` is not available on Hackage yet, but if you are using **stack** to
manage your project, you can add the following lines in your stack.yaml:

```yaml
# ...
packages:
# add the ogmarkup library repository
- location:
    git: https://github.com/ogma-project/ogmarkup.git
    commit: <HASH>
# ...
```

The documentation of the `ogmarkup` library can be [found
online](https://ogma-project.github.io/ogmarkup/) and the project has [its own
OpenHub page](https://www.openhub.net/p/ogmarkup).

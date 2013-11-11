subtitle-fixer
==============

Adjust subtitle for offset and framerate.

Compile with SBT (>= 0.13.0) by running

```
sbt stage
```

and then, run `target/start` to learn usage options.

## Bugs:

* Currently assumes that input and output are Latin-1 encoded. And the text is
  decoded and encoded. Adding options for this should be possible.

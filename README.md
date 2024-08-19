# TST Interview
To run:
- Make sure you have sbt installed (v 1.10.1)
- `sbt compile run`
- Select the problem you want to run
  - This will run the function for the selected problem with the input from the instructions.

## Notes
Created with cats effect `sbt init` template - needed to update some versions and, ultimately, cats effect and its IO monad is certainly overkill and overly complex for basically print statements for a coding interview problem... but... look, I can use weird monad libraries!

Also, the `CatEffectSuite` features are unused, so I just swapped to the munit `FunSuite`... which seemed fine. 
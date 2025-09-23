# fourier-visualizer

Look at a sample heart shape with "stack run".

You can change what image is presented by changing the main function in `app/Main.hs`. For example, instead of 
```
main = animate FullScreen (greyN 0.05) (drawSeed E.heartSeed)
```
you can make it 
```
main = animate FullScreen (greyN 0.05) (drawSeed E.cardioidSeed)
```
to draw a cardioid.

Example seeds are found in `src/ExampleSeeds.hs`.

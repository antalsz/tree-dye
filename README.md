# Tree-Dye

![A gold spanning tree on a deep purple background](README-art/purple-and-gold.png "Generated with `tree-dye --width 400 --height square --foreground '#B08A00' --background '#330066' --sum --bounded README-art/purple-and-gold.png`")
![A purple on green spanning tree image covering the whole canvas](README-art/full-bleed-random.png "Generated with `tree-dye -w400 -hx -m -B README-art/full-bleed-random.png`")

Tree-Dye is an application and library for generating random art like the images
you see above.  These images are generated using random spanning trees, and are
perhaps vaguely reminiscent of [tie-dye][].

## Table of contents

1. [Tree-Dye](#tree-dye)
2. [Table of contents](#table-of-contents)
3. [Documentation](#documentation)
4. [Acknowledgments](#acknowledgments)
5. [References](#references)

## Documentation

```
tree-dye - Generate tie-dye-like images using spanning trees

Usage: tree-dye [-w|--width INT[-INT]] [-h|--height INT[-INT]]
                [-f|--foreground COLOR] [-b|--background COLOR] ([-s|--sum] |
                [-e|--euclidean] | [-m|--max] | [-F|--fixed DIST])
                ([-B|--bounded] | [-W|--wrapping]) FILE
  Generate tie-dye-like images using spanning trees.
  
  The generated image has the specified dimensions; if a dimension is specified
  to be a range, the actual dimension is chosen from that range uniformly at
  random.
  
  A random location in the image is chosen (uniformly) to be the root of the
  spanning tree; it is given the foreground color, and then the color gradually
  changes to the background color heading outwards from there. The color stops
  changing after the specified spreading distance (by default, the Manhattan
  diagonal; that is, width + height).

Available options:
  -w,--width INT[-INT]     Image width or range of possible widths; can also be
                           `square' (default: 100-1000)
  -h,--height INT[-INT]    Image height or range of possible heights; can also
                           be `square' (default: 100-1000)
  -f,--foreground COLOR    The foreground color (the starting color at the root
                           of the tree); can also be `random' (default: random)
  -b,--background COLOR    The background color (the ending color away from the
                           root); can also be `random' (default: random)
  -s,--sum                 Spread color from the root until the distance
                           traveled is the sum of the width and height of the
                           image (its Manhattan diagonal) (default)
  -e,--euclidean           Spread color from the root until the distance
                           traveled is the square root of the sum of the squares
                           of the width and height of the image (its Euclidean
                           diagonal)
  -m,--max                 Spread color from the root all the way until the end
                           of the tree
  -F,--fixed DIST          Spread color from the root until the distance
                           traveled is the specified value
  -B,--bounded             The spanning tree cannot cross the edges of the image
                           (default)
  -W,--wrapping            The spanning tree can wrap across the edges of the
                           image, as though on a torus
  FILE                     Destination PNG file
  -?,--help                Show this help text
```

## Acknowledgments

Tree-Dye was inspired by a conversation with [Michael Klein][] at ICFP 2017.
Thanks, Michael!

## References

* The random spanning tree generation algorithm is the `RandomTreeWithRoot`
  algorithm (Figure 1) from [“Generating Random Spanning Trees More Quickly than
  the Cover Time”][Wilson-RandomSpanningTrees], by David Bruce Wilson, in ACM
  Symposium on the Theory of Computing (STOC) 1996.  It uniformly randomly
  generates a spanning tree using loop-erased random walks.

* Random spanning tree generation is the same problem as maze generation, and I
  found various web pages on the subject very helpful:
  
  - [“Visualizing Algorithms”][Bostock-Visualizing], by Mike Bostock, June 26,
    2014; in particular, the [“Maze Generation”][Bostock-Visualizing-Mazes]
    section.  This section presents various maze/random spanning tree generation
    algorithms, and visualizes them using a rainbow colorization corresponding
    to depth.  It even includes the “tie-dye” analogy!
  
  - The slides from [“Algorithm” is Not a Four-Letter
    Word][Buck-MazeGenerationSlides], Jamis Buck's RubyConf 2011 presentation,
    which explain various different maze generation algorithms; relatedly,
    Buck's post [“Maze Generation: Algorithm Recap”][Buck-MazeGeneration] on his
    blog [The Buckblog][] from February 7, 2011, which expands on some of the
    same algorithms as the presentation.

[tie-dye]:                    https://en.wikipedia.org/wiki/Tie-dye
[Michael Klein]:              https://github.com/michaeljklein
[Wilson-RandomSpanningTrees]: https://www.cs.cmu.edu/~15859n/RelatedWork/RandomTrees-Wilson.pdf
[Bostock-Visualizing]:        https://bost.ocks.org/mike/algorithms/
[Bostock-Visualizing-Mazes]:  https://bost.ocks.org/mike/algorithms/#maze-generation
[Buck-MazeGenerationSlides]:  http://www.jamisbuck.org/presentations/rubyconf2011/index.html
[Buck-MazeGeneration]:        http://weblog.jamisbuck.org/2011/2/7/maze-generation-algorithm-recap
[The Buckblog]:               http://weblog.jamisbuck.org

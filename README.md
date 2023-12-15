# Solutions to Advent of code 2023 with Scryer-Prolog

I chose [Scryer Prolog](https://github.com/mthom/scryer-prolog) because I think
is the more reasonable Prolog out there (not that I investigated much, but swi
always felt too deviant from the ISO Prolog, in the sense that in the past many
times I searched for help the answer was "nah, that is just a SWI-Prolog
specific thing"). Also it feels principled, simple enough, yet powerful enough.
And the authors are super responsive.

I'm not an expert on Prolog, but I like how clean the Prolog solution looks.

This being said, I try to be as idiomatic as possible, using DCGs where possible
and avoiding "imperative style" prolog, and trying to avoid cuts.

Any improvements or corrections you see on the code, please send them my way as
only by correcting our previous knowledge we learn new stuff.

## Tricks

I found quite useful to have the following function around in my emacs as
ediprolog does not work for me on Windows:

``` emacs-lisp
(defun javier/scryer ()
  "Call scryer on save"
  (when (eq major-mode 'prolog-mode)
    (message "%s" (string-trim
      (shell-command-to-string (format "scryer-prolog '%s' -g halt." buffer-file-name))))))

(add-hook 'after-save-hook #'javier/scryer)
```

It shows on the minibuffer whether scryer complains on the current file, which
makes it somewhat easier to find syntax errors and missing punctuation symbols.

## Commentaries

| Number | Part | Commentary                                                                                                  |
|--------|------|-------------------------------------------------------------------------------------------------------------|
| 1      | 1    | N/A                                                                                                         |
| 1      | 2    | Solved just by ;'ed DCGs                                                                                    |
| 2      | 1    | N/A                                                                                                         |
| 2      | 2    | N/A                                                                                                         |
| 3      | 1    | A bit more complex due to usage of big matrices, but can be parsed incrementally easily                     |
| 3      | 2    | More complex state to carry                                                                                 |
| 4      | 1    | N/A                                                                                                         |
| 4      | 2    | N/A                                                                                                         |
| 5      | 1    | Used `asserta` so quite bad practice but it works                                                           |
| 5      | 2    | Needed to manually craft CLP(Z) domains and perform multiple intersections, not trivial                     |
| 6      | 1    | Solved with labeling, a bit of math would have been faster                                                  |
| 6      | 2    | Ditto                                                                                                       |
| 7      | 1    | Boilerplate-y because we can't overload `compare`, had to create `assoc_by.pl`                              |
| 7      | 2    | N/A                                                                                                         |
| 8      | 1    | N/A                                                                                                         |
| 8      | 2    | If I had not visualized the data on `circo` I might had gone the route of using dynamic programming/tabling |
| 9      | 1    | N/A                                                                                                         |
| 10     | 1    | N/A                                                                                                         |
| 10     | 2    | Quite complicated to get the math right, using ray casting solves it                                        |
| 11     | 1    | N/A                                                                                                         |
| 11     | 2    | N/A                                                                                                         |
| 12     | 1    | Easy to brute-force                                                                                         |
| 12     | 2    | **UNSOLVED**                                                                   |

> Disclaimer: I went back to read [The Power of
> Prolog](https://www.metalevel.at/prolog) because I was feeling my answers were
> A) not fast enough B) not declarative. From this point on I try to minimize
> the use of custom "imperative" constructs I had in my `src/utils.pl` and
> instead declare relations. The results are much faster programs.

| Number | Part | Commentary                      |
|--------|------|---------------------------------|
| 13     | 1    | N/A                             |
| 13     | 2    | Ah, a nice declarative solution |
| 14     | 1    | N/A                             |
| 14     | 2    | N/A                             |

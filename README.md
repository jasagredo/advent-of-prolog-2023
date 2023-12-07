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

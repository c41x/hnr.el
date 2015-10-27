# hnr.el
Hacker News Reader for Emacs

## Setup
```lisp
(setq hnr-max-items 10) ;; items to load (10 by default)
(setq hnr-auto-mark-as-read nil) ;; mark all as read when opened (disabled by default)
```
## Usage

Run `M-x hnr`

## Keyboard shortcuts
* n - down - select next
* p - up - select previous
* space - load more
* enter - open selected in browser
* a - mark all as read
* q - close

## Preview
![Preview](/sample.png?raw=true "Preview")
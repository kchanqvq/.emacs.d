(require 'quail)

(quail-define-package
 "ithkuil-romanized" "Ithkuil" "ithk-r" t "Ithkuil input method.

Follow appropriate characters with a , to add a
cedilla (e.g. c,->ç) or a . for a dot (z.->ż). \" will add dots
above a vowel (e\"->ë).

Both the caron and circumflex are added with a following
semicolon, for ease of typing, and the aspiration symbol is
produced thus as well (h;->ʰ).

The grave and accute accents for stress markings are produced
with ` and ' following (a`->à, a'->á).

The macron indicating high tone is produced by an underscore and
a semicolon (_;->¯), and the breve by a caret and a
semicolon (^;->ˇ). A superscripted forward slash cannot be typed,
since this Unicode character does not exist, as far as I know."

 nil t nil t t nil nil nil nil nil t)

(quail-define-rules
 ("_;" ?¯)
 ("^;" ?ˇ)
 ("a`" ?à)
 ("a'" ?á)
 ("a;" ?â)
 ("c;" ?č)
 ("c," ?ç)
 ("e`" ?è)
 ("e'" ?é)
 ("e\"" ?ë)
 ("e;" ?ê)
 ("h;" ?ʰ)
 ("i`" ?ì)
 ("i'" ?í)
 ("i;" ?î)
 ("l," ?ļ)
 ("n;" ?ň)
 ("o`" ?ò)
 ("o'" ?ó)
 ("o\"" ?ö)
 ("o;" ?ô)
 ("r;" ?ř)
 ("s;" ?š)
 ("t," ?ţ)
 ("u'" ?ú)
 ("u`" ?ù)
 ("u\"" ?ü)
 ("u;" ?û)
 ("z;" ?ž)
 ("z." ?ż)
 ("A`" ?À)
 ("A'" ?Á)
 ("A;" ?Â)
 ("C;" ?Č)
 ("C," ?Ç)
 ("E`" ?È)
 ("E'" ?É)
 ("E\"" ?Ë)
 ("E;" ?Ê)
 ("H;" ?ʰ)
 ("I`" ?Ì)
 ("I'" ?Í)
 ("I;" ?Î)
 ("L," ?Ļ)
 ("N;" ?Ň)
 ("O`" ?Ò)
 ("O'" ?Ó)
 ("O\"" ?Ö)
 ("O;" ?Ô)
 ("R;" ?Ř)
 ("S;" ?Š)
 ("T," ?Ţ)
 ("U'" ?Ú)
 ("U`" ?Ù)
 ("U\"" ?Ü)
 ("U;" ?Û)
 ("Z;" ?Ž)
 ("Z." ?Ż))

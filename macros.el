(fset 'clean-nfo
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("^[<^L^S<title^M^F^@^L^S</^M^B\
^B^W^L^S<plot>^M^@^L^S</^B^B^B^W^D^L^Stagline^M^F^@^L^S</^M^B^B^W^L^Spremie^M^F^F^F^F^@^L^S</^M^B^B^L^Sact^B^L^S<name^M^F^@^L^S\
</^M^B^B^W^[<^L^S<title^M^F^X^S" 0 "%d")) arg)))
;; Clone nfo file                                                                                                               
(global-set-key [f6] 'clean-nfo)


(fset 'clone-nfo
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("^@^E^[w^PC^Y^B^B^B^Knfo^Mg" 0\
 "%d")) arg)))
(global-set-key [f5] 'clone-nfo)


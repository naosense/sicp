#lang s-exp framework/keybinding-lang

(keybinding "tab" (lambda (editor event) (send editor auto-complete)))
(keybinding "c:1" (lambda (editor event) (send editor comment-out-selection)))
(keybinding "c:2" (lambda (editor event) (send editor uncomment-selection)))

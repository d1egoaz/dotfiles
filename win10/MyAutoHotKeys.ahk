#NoEnv  ; Recommended for performance and compatibility with future AutoHotkey releases.
; #Warn  ; Enable warnings to assist with detecting common errors.
SendMode Input  ; Recommended for new scripts due to its superior speed and reliability.
SetWorkingDir %A_ScriptDir%  ; Ensures a consistent starting directory.

;#InputLevel 1
;LAlt::
;  Send, ^c
;Return

;#InputLevel 1
;RAlt::
;  Send, ^v
;Return

#InputLevel 0
!h::
   Send, {Left}
Return
!j::
   Send, {Down}
Return
!k::
   Send, {Up}
Return
!l::
   Send, {Right}
Return


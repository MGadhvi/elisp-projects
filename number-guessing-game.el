(defun computer-guess-game ()
  "Start the number guessing game where the computer tries to guess your number between 0 and 9."
  (interactive)
  (let ((low 0)
        (high 9)
        (attempts 0)
        (guess nil))
    (message "Think of a number between 0 and 9 (inclusive). Press Enter when you're ready.")
    (read-string "Press Enter when you're ready...")  ; Wait for user to press Enter
    (while (not (string= guess "c"))
      (setq attempts (1+ attempts))
      (setq guess (computer-make-guess low high))
      (message "My guess is: %d" guess)
      (setq guess (computer-get-feedback))
      (computer-process-feedback guess low high attempts))))

(defun computer-make-guess (low high)
  "Generate a random guess between LOW and HIGH."
  (+ low (random (+ 1 (- high low)))))

(defun computer-get-feedback ()
  "Prompt the user for feedback on the computer's guess."
  (let ((feedback (read-string "Is my guess higher, lower, or correct? (h/l/c): ")))
    (string-to-char feedback)))

(defun computer-process-feedback (feedback low high attempts)
  "Process the user's FEEDBACK and adjust the guessing range accordingly."
  (cond
   ((eq feedback ?h)
    (computer-higher-feedback high))
   ((eq feedback ?l)
    (computer-lower-feedback low))
   ((eq feedback ?c)
    (computer-correct-feedback attempts))
   (t
    (message "Please enter 'h' for higher, 'l' for lower, or 'c' for correct."))))

(defun computer-higher-feedback (high)
  "Handle the case where the guess was too high."
  (if (= high 0)
      (message "I can't guess higher than my current maximum. Please check your input.")
    (setq high (1- high))))

(defun computer-lower-feedback (low)
  "Handle the case where the guess was too low."
  (if (= low 9)
      (message "I can't guess lower than my current minimum. Please check your input.")
    (setq low (1+ low))))

(defun computer-correct-feedback (attempts)
  "Handle the case where the guess was correct."
  (message "Yay! I guessed your number in %d attempts!" attempts))

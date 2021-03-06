(defvar libglfw (load-lib (if (is-linux?) "libglfw.so" "glfw3.dll")))

(defun **glfw:load-sym ((ptr expr) (2lisp-name (ptr expr)) (2c-name (ptr expr)) (2type (ptr expr)))
  (progn
  (print "Defining") (print-expr 2lisp-name) (print newline)
  (expr (load-symbol+ libglfw (unexpr 2lisp-name) (unexpr 2c-name) (unexpr 2type) ))))
(declare-macro glfw:load-sym **glfw:load-sym)
(glfw:load-sym glfw:init glfwInit (fcn void))

;(defmacro glfw:load-sym (lisp-name c-name type)
;  (expr (load-symbol libglfw (quote (unexpr lisp-name)) (quote (unexpr c-name)) (type (unexpr type)))))

(glfw:load-sym glfw:init glfwInit (fcn void))
(glfw:load-sym glfw:terminate glfwTerminate (fcn void))
(glfw:load-sym glfw:get-version glfwGetVersion (fcn void (major (ptr i32)) (minor (ptr i32)) (rev (ptr i32))))
(glfw:load-sym glfw:get-version-string glfwGetVersionString (fcn (ptr char)))
(glfw:load-sym glfw:get-proc-address glfwGetProcAddress (fcn (ptr void) (name (ptr char))))
(glfw:load-sym glfw:create-window glfwCreateWindow
	       (fcn (ptr void) (width i32) (height i32) 
		    (title (ptr char)) (a (ptr void)) (b (ptr void))))

(glfw:load-sym  glfw:swap-buffers glfwSwapBuffers (fcn void (a ( ptr void))))
(glfw:load-sym glfw:make-current glfwMakeContextCurrent (fcn void (win (ptr void))))
(glfw:load-sym  glfw:poll-events glfwPollEvents (fcn void))



;; clipboard
(glfw:load-sym glfw:set-clipboard-string glfwSetClipboardString (fcn void (win (ptr void)) (_str (ptr char))))
(glfw:load-sym glfw:get-clipboard-string glfwGetClipboardString (fcn (ptr char) (win (ptr void))))


;; event callbacks

 (glfw:load-sym glfw:set-mouse-button-callback glfwSetMouseButtonCallback
		(fcn void (win-ptr (ptr void))
		     (callback  (fcn void (win (ptr void)) (button i32) (action i32) (mods i32)))))
(glfw:load-sym glfw:set-key-callback glfwSetKeyCallback
	       (fcn void (win (ptr void))
		    (callback (fcn void (win (ptr void)) (key i32) (scancode i32) (action i32) (mods i32)))))

(glfw:load-sym glfw:set-error-callback glfwSetErrorCallback
	       (fcn void (callback  (fcn void (code i32) (str (ptr char))))))

(glfw:load-sym glfw:set-cursor-pos-callback glfwSetCursorPosCallback
	       (fcn void (win (ptr void)) 
		    (callback  (fcn void (win (ptr void)) (x f64) (y f64)))))

(glfw:load-sym glfw:set-cursor-enter-callback glfwSetCursorEnterCallback
	       (fcn void (win (ptr void)) (callback  (fcn void (win (ptr void)) (entered i32)))))

(glfw:load-sym glfw:set-char-callback glfwSetCharCallback 
	       (fcn void (win (ptr void)) (cb (fcn void (win (ptr void)) (chr char)))))
(glfw:load-sym glfw:set-scroll-callback glfwSetScrollCallback
	       (fcn void (win (ptr void)) (cb  (fcn void (win (ptr void)) (scroll-x f64)
							(scroll-y f64)))))
(glfw:load-sym glfw:set-window-close-callback glfwSetWindowCloseCallback
	       (fcn void (win (ptr void)) (cb (fcn void (win (ptr void))))))
(glfw:load-sym glfw:joystick-present? glfwJoystickPresent
	       (fcn bool (joy-idx i32)))

(glfw:load-sym glfw:get-joystick-axes glfwGetJoystickAxes (fcn (ptr f32) (joy i32) (axis-count (ptr i32))))
(glfw:load-sym glfw:get-joystick-buttons glfwGetJoystickButtons (fcn (ptr u8) (joy i32) (button-count (ptr i32))))
(glfw:load-sym glfw:get-joystick-name glfwGetJoystickName (fcn (ptr char) (joy i32)))
(glfw:load-sym glfw:get-time glfwGetTime (fcn f64))
(glfw:load-sym glfw:set-time glfwSetTime (fcn void (time f64)))
(glfw:load-sym glfw:get-current-context glfwGetCurrentContext (fcn (ptr void)))
(glfw:load-sym glfw:swap-interval glfwSwapInterval (fcn void (interval i32)))
(glfw:load-sym glfw:get-key glfwGetKey (fcn bool (win (ptr void)) (key i64)))

(defun charv (i64 (char-str (ptr char)))
  (cast (deref char-str) i64))

(defvar glfw:key-up 265)
(defvar glfw:key-down 264)
(defvar glfw:key-left 263)
(defvar glfw:key-right 262)
(defvar glfw:key-space 32)
(defvar glfw:key-a (charv "A"))
(defvar glfw:key-b (charv "B"))
(defvar glfw:key-c (charv "C"))
(defvar glfw:key-d (charv "D"))
(defvar glfw:key-e (charv "E"))
(defvar glfw:key-f (charv "F"))
(defvar glfw:key-g (charv "G"))
(defvar glfw:key-h (charv "H"))
(defvar glfw:key-i (charv "I"))
(defvar glfw:key-j (charv "J"))
(defvar glfw:key-k (charv "K"))
(defvar glfw:key-l (charv "L"))
(defvar glfw:key-m (charv "M"))
(defvar glfw:key-n (charv "N"))
(defvar glfw:key-o (charv "O"))
(defvar glfw:key-p (charv "P"))
(defvar glfw:key-q (charv "Q"))
(defvar glfw:key-r (charv "R"))
(defvar glfw:key-s (charv "S"))
(defvar glfw:key-t (charv "T"))
(defvar glfw:key-u (charv "U"))
(defvar glfw:key-v (charv "V"))
(defvar glfw:key-x (charv "X"))
(defvar glfw:key-y (charv "Y"))
(defvar glfw:key-z (charv "Z"))

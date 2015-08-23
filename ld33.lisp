(load "std2.lisp")
(load "truetype.lisp")
(load "stb_image.lisp")
(load "glfw.lisp")


(glfw:init)
(defvar win (glfw:create-window 800 800 "Bloddy!" null null))
(glfw:make-current win)
(defstruct rect
  (upper-left vec2)
  (size vec2))
(load "gl.lisp")
(load "gl-ext.lisp")
(load "textbox-shader.lisp")

(defstruct point
  (x i64)
  (y i64))

(defun make-point (point (x i64) (y i64))
  (let ((pt :type point))
    (setf (member pt x) x)
    (setf (member pt y) y)
    pt))

(defun print-point (void (pt point))
  (print "(P: "(member pt x) " " (member pt y) ")"))

(overload print print-point)

(defun point-to-vec2 (vec2 (pt point))
  (vec (cast (member pt x) f64)
       (cast (member pt y) f64)))

(defun vec2-to-point( point (a vec2))
  (make-point (cast (member a x) i64) 
	      (cast (member a y) i64)))

(defun vec2-to-point2( point (a vec2))
  (make-point (cast (ceil (member a x)) i64) 
	      (cast (ceil (member a y)) i64)))
	  
(defun lookup ((ptr u8) (x i64) (y i64) (im im:image))
  (let ((w (cast (member im width) i64))
	(h (cast (member im height) i64)))
    (let ((index (+ (.% (+ x w) w)
		    (* (.% (+ y h) h) w))))
      (ptr+ (member im data) index))))

(defun lookup-point ((ptr u8) (pt point) (im im:image))
  (lookup (member pt x) 
	  (member pt y) im))

(defun analyze-image (void (im im:image))
  (let ((buf (cast (alloc0 (* 256 4)) (ptr i32)))
	(data (member im data)))
    (let ((size (cast (* (member im width) (member im height)) i64)))
      (range it 0 size
	     (incr (deref (+ buf (cast (deref (+ data it)) i64))) 1))
      (range it 0 256
	     (let ((bv (deref (+ buf it))))
	       (when (not (eq bv 0))
		 (print it ":" bv newline))))
      (dealloc (cast buf (ptr void))))))


(defvar lv (im:load-image "level1.png"))

;(defvar lv2 (im:load-image "level1.png"))
(defvar backbuf (im:make 200 200 1))
(print "Image: " (member lv width) " " (member lv height) " " (member lv bpp) newline)
;(analyze-image lv)
(print newline)

(defun find-levels (void (level u8) (im im:image) (outpts (ptr (ptr point))) (outpts-cnt (ptr i64)))
  (let ((width (cast (member im width) i64))
	(height (cast (member im height) i64))
	(outpts2 (cast null (ptr point)))
	(outpts2-cnt 0))
    (setf (deref outpts-cnt) 0)
    (range row 0 height
	   (range col 0 width
		  (let ((index (+ col (* row width))))
		    (let ((v (deref (+ (member im data) index))))
		      (when (eq v level)
			(add-to-list+ outpts2 outpts2-cnt (make-point col row)))))))
    (setf (deref outpts) outpts2)
    (setf (deref outpts-cnt) outpts2-cnt )))
 
(defmacro gameid (name value)
  (expr (defvar (unexpr name) (cast (unexpr value) u8))))
;(exit 0)
(gameid gameid:player 128)
(gameid gameid:player-next 120)
(gameid gameid:enemy 104)
(gameid gameid:enemy-spawn 50)
(gameid gameid:nothing 0)
(gameid gameid:wall 255)

(defvar player-pos :type vec2)
(defvar player-next-pos :type point)
(defvar player-dir :type vec2)
(defvar enemy-pos (cast null (ptr point)))
(defvar enemy-cnt 0)
(defvar lookup-pos (cast null (ptr point)))
(defvar cnt 0)
(find-levels gameid:player lv (addrof lookup-pos) (addrof cnt))
(print cnt)
(assert (eq cnt 1))
(setf player-pos (point-to-vec2 (deref lookup-pos)))
(defvar player-init-pos player-pos)

(setf cnt 0)
(find-levels gameid:player-next lv (addrof lookup-pos) (addrof cnt))
(assert (eq cnt 1))
(setf player-next-pos (deref lookup-pos))
(print player-pos " " player-next-pos newline)
(setf (deref (lookup-point (vec2-to-point player-pos) lv)) 0)
(setf (deref (lookup-point player-next-pos lv)) 0)
(setf cnt 0)
(find-levels gameid:enemy-spawn lv (addrof enemy-pos) (addrof enemy-cnt))

;(exit 0)
(setf player-dir (vec2-normalize (- (point-to-vec2  player-next-pos) player-pos )))
(print "dir: " player-dir newline)
(defvar player-init-dir player-dir)


(defun get-flow-points (bool (pos vec2) (dir vec2) (lv im:image) 
			(lower (ptr point)) 
			(upper (ptr point)))
  (let ((tpt  pos)
	(t1 (vec2:rot90 dir))
	(found-lower false)
	(found-upper false))
    (range it -100 100
	   (let ((itf (cast it f64)))
	     (let ((pt (vec2-to-point (+ tpt (* t1 itf 0.5)))))
	       (let ((value (deref (lookup-point pt lv))))
		 (when (eq value gameid:wall)
		   (when (< it 0)
		     ;(setf it 0)
		     (setf (deref lower) pt)
		     (setf found-lower true)
		     )
		   (when (> it 0)
		     (setf (deref upper) pt)
		     (setf found-upper true)
		     (setf it 99)
		   
		     ))))))
    (and found-lower found-upper)))
		     
(defvar upper-pt :type point)
(defvar lower-pt :type point)

(defun pt-dst-sqrt (i64 (a point) (b point))
  (let ((x (- (member a x) (member b x)))
	(y (- (member a y) (member b y))))
    (+ (* x x) (* y y))))

(defun find-closer-point (point (center point) (base point) (lv im:image) (radius i64))
  (let ((out-pt base)
	(start-x (- (member base x) radius))
	(end-x (+ (member base x) radius 1))
	(start-y (- (member base y) radius))
	(end-y (+ (member base y) radius 1))
	(current-d (pt-dst-sqrt center base)))
	  
    (range y start-y end-y
	   (range x start-x end-x
		  (let ((value (deref (lookup x y lv))))
		    (when (eq value gameid:wall)
		      (let ((pt (make-point x y)))

			(let ((d (pt-dst-sqrt center pt )))

			  (when (< d current-d)
			    (setf out-pt pt)
			    (setf current-d d))))))))
    out-pt))

(defun game-it
    (let ((upper-pt :type point)
	  (lower-pt :type point)
	  (ppos (vec2-to-point player-pos)))
      (let ((success (get-flow-points player-pos player-dir lv 
				      (addrof lower-pt) (addrof
							 upper-pt))))
	(unless success
	  (setf success (get-flow-points (vec (ceil (member player-pos x)) (ceil
							      (member
							       player-pos
							       y ))) player-dir lv 
			   (addrof lower-pt) (addrof upper-pt))))
	(when success
	  (setf upper-pt (find-closer-point ppos upper-pt lv 1))
	  (setf lower-pt (find-closer-point ppos lower-pt lv 1))
	  ;(setf (deref (lookup-point upper-pt lv2)) 200)
	  ;(setf (deref (lookup-point lower-pt lv2)) 200)
	  (let ((lower-vec (point-to-vec2 lower-pt))
		(upper-vec (point-to-vec2 upper-pt)))
	    (let ((perp-vec (vec2-normalize (vec2:rot90 (- lower-vec upper-vec )))))
	      (when (> (vec2-length perp-vec) 0)
		(setf player-dir (+ (* player-dir 0.8) (* perp-vec 0.2)))))
	  )))
      (incr player-pos (* player-dir 0.2))
      ))


(defvar fs-blit:loaded false)
(defvar fs-blit:shader :type gl:shader-program)
(defvar fs-blit:shader:tex :type gl:uniform-loc)
(defvar fs-blit:shader:offset :type gl:uniform-loc)
(defvar fs-blit:shader:size :type gl:uniform-loc)
(defvar fs-blit:shader:dir :type gl:uniform-loc)
(defvar fs-blit:shader:cam-size (gl:get-uniform-location fs-blit:shader "cam_size"))
(defvar fs-blit:shader:cam-offset (gl:get-uniform-location fs-blit:shader "cam_offset"))
(defun fs-blit:load
    (unless fs-blit:loaded
      (let (
	    (frag (gl:create-shader gl:fragment-shader))
	    (vert (gl:create-shader gl:vertex-shader))
	    (frag-src "
#version 130
uniform sampler2D tex;
in vec2 uv;
void main(){

  vec4 col = texture(tex, vec2(uv.x, uv.y));
  if(col == vec4(1,0,1,1) || col == vec4(0,0,0,1))
   discard;
  gl_FragColor = col;
}
")
	  (vert-src "
#version 330
uniform vec2 offset;
uniform vec2 size;
uniform vec2 dir;
uniform vec2 cam_offset;
uniform vec2 cam_size;


layout(location = 0) in vec2 vertex;
layout(location = 1) in vec2 uv_in;
out vec2 uv;

void main(){
  vec2 v2 = vec2(-dir.y, dir.x);
  vec2 v = vec2(dir.x * vertex.x + v2.x * vertex.y,
                dir.y * vertex.x + v2.y * vertex.y);
  uv = uv_in;
  gl_Position = vec4(((v * size + offset - cam_offset) / cam_size ) * 2,0.0,1.0);
}
"))
	(setf fs-blit:loaded true)
	(setf fs-blit:shader (gl:create-program))
	(let ((frag-src-len (cast (strlen frag-src) u32)))
	  (gl:shader-source frag 1 (addrof frag-src) (addrof frag-src-len)))
	(let ((vert-src-len (cast (strlen vert-src) u32)))
	  (gl:shader-source vert 1 (addrof vert-src) (addrof vert-src-len)))
	(gl:compile-shader frag)
	(gl:compile-shader vert)
	
	(print "**** frag ****" newline)
	(text-box:print-shader-errors frag)
	(print "**** vert ****" newline)
	(text-box:print-shader-errors vert)
	
	(gl:attach-shader fs-blit:shader frag)
	(gl:attach-shader fs-blit:shader vert)
	(gl:link-program fs-blit:shader)
	
	(setf fs-blit:shader:tex
	      (gl:get-uniform-location fs-blit:shader "tex"))
	(setf fs-blit:shader:offset (gl:get-uniform-location fs-blit:shader "offset"))
	(setf fs-blit:shader:size (gl:get-uniform-location fs-blit:shader "size"))
	(setf fs-blit:shader:dir (gl:get-uniform-location fs-blit:shader "dir"))
	(setf fs-blit:shader:cam-size (gl:get-uniform-location fs-blit:shader "cam_size"))
	(setf fs-blit:shader:cam-offset (gl:get-uniform-location fs-blit:shader "cam_offset"))
	(gl:use-program fs-blit:shader)
	(gl:uniform-2f fs-blit:shader:offset 0 0)
	(gl:uniform-2f fs-blit:shader:size 1 1)
	(gl:uniform-2f fs-blit:shader:dir 1 0)
	(gl:uniform-2f fs-blit:shader:cam-size 1 1)
	(gl:uniform-2f fs-blit:shader:cam-offset 0 0)
	)))
(fs-blit:load)
(defun fs-blit:bpp (gl:enum (bpp i32))
  (if (eq bpp 1)
      gl:red
      (if (eq bpp 3)
	  gl:rgb
	  gl:rgba)))
	  
	  
(defun fs-blit:load-image (gl:tex (image im:image) (clamp gl:enum))
  (let ((tex :type gl:tex))
    (gl:gen-textures 1 (addrof tex))
    (gl:bind-texture gl:texture-2d tex)
    (gl:tex-parameter gl:texture-2d gl:texture-min-filter gl:nearest)
    (gl:tex-parameter gl:texture-2d gl:texture-mag-filter gl:nearest)
    (gl:tex-parameter gl:texture-2d gl:texture-wrap-s clamp)
    (gl:tex-parameter gl:texture-2d gl:texture-wrap-t clamp)
    (gl:tex-image-2d gl:texture-2d 0 gl:rgb 
		     (cast (member image width) i64)
		     (cast (member image height) i64)
		     0 
		     (fs-blit:bpp (member image bpp)) gl:ubyte 
		     (cast (member image data) (ptr void)))
    tex))

(defun fs-blit:reload-image (void (tex gl:tex) (image im:image))
  (progn
    (gl:bind-texture gl:texture-2d tex)
    (gl:tex-image-2d gl:texture-2d 0 gl:rgb 
		     (cast (member image width) i64) 
		     (cast (member image height) i64)
		     0 gl:red gl:ubyte 
		     (cast (member image data) (ptr void)))))

(defun fs-blit:render-image (void (tex gl:tex))
  (progn
    (gl:use-program fs-blit:shader)
    (gl:bind-texture gl:texture-2d tex)
    (gl:draw-arrays gl:quads 0 4)))
	  
(defvar rect-vbo (gl:gen-buffer))
(let ((data (cast (alloc0 (* 4 4 2)) (ptr f32)))) ;4 vec2f's
  (setf (deref (ptr+ data 2)) 1)
  (setf (deref (ptr+ data 4)) 1)
  (setf (deref (ptr+ data 5)) 1)
  (setf (deref (ptr+ data 7)) 1)
  (gl:bind-buffer gl:array-buffer rect-vbo)
  (gl:buffer-data gl:array-buffer (* 4 4 2) (cast data (ptr void))
		  gl:static-draw)
  (dealloc (cast data (ptr void))))
(defvar rect-one-vbo (gl:gen-buffer))
(let ((data (cast (alloc (* 4 4 2)) (ptr f32)))) ;4 vec2f's
  (setf (deref (ptr+ data 0)) -0.5)
  (setf (deref (ptr+ data 1)) -0.5)
  (setf (deref (ptr+ data 2)) 0.5)
  (setf (deref (ptr+ data 3)) -0.5)
  (setf (deref (ptr+ data 4)) 0.5)
  (setf (deref (ptr+ data 5)) 0.5)
  (setf (deref (ptr+ data 6)) -0.5)
  (setf (deref (ptr+ data 7)) 0.5)
  (gl:bind-buffer gl:array-buffer rect-one-vbo)
  (gl:buffer-data gl:array-buffer (* 4 4 2) (cast data (ptr void))
		  gl:static-draw)
  (dealloc (cast data (ptr void))))

(defvar rect-huge-uv-vbo (gl:gen-buffer))
(defvar huge-f32 (the 50 f32))
(let ((data (cast (alloc (* 4 4 2)) (ptr f32)))) ;4 vec2f's
  (setf (deref (ptr+ data 0)) 0)
  (setf (deref (ptr+ data 1)) 0)
  (setf (deref (ptr+ data 2)) huge-f32)
  (setf (deref (ptr+ data 3)) 0)
  (setf (deref (ptr+ data 4)) huge-f32)
  (setf (deref (ptr+ data 5)) huge-f32)
  (setf (deref (ptr+ data 6)) 0)
  (setf (deref (ptr+ data 7)) huge-f32)
  (gl:bind-buffer gl:array-buffer rect-huge-uv-vbo)
  (gl:buffer-data gl:array-buffer (* 4 4 2) (cast data (ptr void))
		  gl:static-draw)
  (dealloc (cast data (ptr void))))


(defun tex:load (gl:tex (file (ptr char)))
  (let ((virus (im:load-image file)))
    (fs-blit:load-image virus  gl:clamp-to-border)))

(defun tex:load-repeat (gl:tex (file (ptr char)))
  (let ((virus (im:load-image file)))
    (fs-blit:load-image virus  gl:repeat)))
    
    
(defvar tex:virus (tex:load "virus.png"))
(defvar tex:enemy (tex:load "enemy.png"))
(defvar tex:bubbles (tex:load-repeat "bubbles.png"))
(gl:enable-vertex-attrib-array 0)   
(gl:enable-vertex-attrib-array 1)
(defvar game-tex (fs-blit:load-image backbuf gl:clamp-to-border))
(gl:use-program fs-blit:shader)
;(print (cast tex:bubbles i32) newline)
;(exit 0)
(defun run-game
    (let ((iteration 0)
	  (w (cast (member backbuf width) f64))
	  (h (cast (member backbuf height) f64)))
      (range iteration 0 10000
	

	(game-it)

	(let ((cpos (deref (lookup-point (vec2-to-point player-pos) lv))))
	  (when (or (eq cpos gameid:enemy) 
		    (eq cpos gameid:wall))
	    (print "FAIL" newline)
	    (setf player-pos player-init-pos)
	    (setf player-dir player-init-dir)
	    ))
	(let ((pc 
	       (if (glfw:get-key win glfw:key-a)
		   -1.0
		   (if (glfw:get-key win glfw:key-d)
		       1.0
		       0.0))))
	  (setf player-pos (+ (* (vec2:rot90 player-dir) pc 0.2) player-pos)))
	
	(gl:clear gl:color-buffer-bit)
	;; draw backdrop
	(gl:uniform fs-blit:shader:cam-offset (* player-pos 4.0))
	(gl:uniform fs-blit:shader:cam-size (vec w h))
	
	(gl:bind-buffer gl:array-buffer rect-huge-uv-vbo)
	(gl:vertex-attrib-pointer 1 2 gl:float gl:false 0 null)
	(gl:bind-buffer gl:array-buffer rect-vbo)
	(gl:vertex-attrib-pointer 0 2 gl:float gl:false 0 null) 
	
	(gl:uniform fs-blit:shader:size (vec 2000 2000))
	(gl:uniform fs-blit:shader:offset (vec -1000 -1000))
	(gl:uniform fs-blit:shader:dir 1.0 0.0)

	(gl:bind-texture gl:texture-2d tex:bubbles)
	(gl:draw-arrays gl:quads 0 4)	  
	(gl:bind-buffer gl:array-buffer rect-vbo)
	(gl:vertex-attrib-pointer 1 2 gl:float gl:false 0 null)

	;; draw walls
	(gl:uniform fs-blit:shader:cam-offset player-pos)
	(let ((w (cast (/ (member backbuf width) 2) i64)))
	  ;; draw to back buffer
	  (range row (- 0 w) w
	       (range col (- 0 w) w
		      (let ((pp (vec2-to-point (+ player-pos (vec (cast row f64) (cast col f64))))))
			(let ((color (deref (lookup-point pp lv))))
			  (setf color (if (or (eq color gameid:enemy) 
					      (eq color gameid:wall)) 
					  gameid:wall 0))
			  (setf (deref (lookup-point (make-point (+ row w) (+ col w)) backbuf))
				color))))))
	(fs-blit:reload-image game-tex backbuf)
	(gl:uniform fs-blit:shader:cam-offset player-pos)

	(let ((game-offset (- player-pos (vec2:floor player-pos))))
	  (let ((offset2 game-offset))
	    (setf offset2 (- (vec 0 0) offset2))
	    (gl:uniform fs-blit:shader:offset (+ offset2 (- player-pos (* (vec w h) 0.5))))
	    ))

	(gl:uniform fs-blit:shader:size (vec w h))
	
	(gl:uniform fs-blit:shader:dir 1 0)
	(gl:bind-buffer gl:array-buffer rect-vbo)
	(gl:vertex-attrib-pointer 0 2 gl:float gl:false 0 null) 
	(gl:bind-texture gl:texture-2d game-tex)
	(gl:draw-arrays gl:quads 0 4)	  


	(gl:bind-buffer gl:array-buffer rect-one-vbo)
	(gl:vertex-attrib-pointer 0 2 gl:float gl:false 0 null) 
	(gl:uniform fs-blit:shader:size (vec 4 4))
	(gl:uniform fs-blit:shader:offset player-pos)
	;(print player-pos newline)
	(gl:uniform fs-blit:shader:dir player-dir)
	(gl:bind-texture gl:texture-2d tex:virus)
	(gl:draw-arrays gl:quads 0 4)
	(gl:bind-texture gl:texture-2d tex:enemy)
	(gl:uniform fs-blit:shader:size (vec 2 2))
	(let ((fit (* (cast iteration f32) 0.1)))
	  (print fit newline)
	  (range it 0 enemy-cnt
		 (let ((f2 (+ fit (cast it f32))))
		   (gl:uniform-2f fs-blit:shader:dir (sin32 f2) (cos32 f2))
		   (let ((e-pos (deref (ptr+ enemy-pos it))))
		     (gl:uniform fs-blit:shader:offset (point-to-vec2 e-pos))
		     (gl:draw-arrays gl:quads 0 4)))))
	  
	(glfw:swap-buffers win)
	(glfw:poll-events)
	(usleep 10000))))
(run-game)

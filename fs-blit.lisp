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

  vec4 col = texture(tex, uv);
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

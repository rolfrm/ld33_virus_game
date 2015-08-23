;; Mixed blitting

(defvar fs-blit2:loaded false)
(defvar fs-blit2:shader :type gl:shader-program)
(defvar fs-blit2:shader:tex :type gl:uniform-loc)
(defvar fs-blit2:shader:tex2 :type gl:uniform-loc)
(defvar fs-blit2:shader:offset :type gl:uniform-loc)
(defvar fs-blit2:shader:size :type gl:uniform-loc)
(defvar fs-blit2:shader:dir :type gl:uniform-loc)
(defvar fs-blit2:shader:cam-size  :type gl:uniform-loc)
(defvar fs-blit2:shader:cam-offset :type gl:uniform-loc)
(defvar fs-blit2:shader:uv-offset :type gl:uniform-loc)
(defvar fs-blit2:shader:uv-scale :type gl:uniform-loc)
(defun fs-blit2:load
    (unless fs-blit2:loaded
      (let (
	    (frag (gl:create-shader gl:fragment-shader))
	    (vert (gl:create-shader gl:vertex-shader))
	    (frag-src "
#version 130
uniform sampler2D tex;
uniform sampler2D tex2;
in vec2 uv;
in vec2 uv2;
void main(){

  vec4 col = texture(tex, uv);
  if(col == vec4(1,0,1,1) || col == vec4(0,0,0,1))
   discard;
  col = texture(tex2, uv2) * col.r; 
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
uniform vec2 uv_offset;
uniform vec2 uv_scale;

layout(location = 0) in vec2 vertex;
layout(location = 1) in vec2 uv_in;
out vec2 uv;
out vec2 uv2;

void main(){
  vec2 v2 = vec2(-dir.y, dir.x);
  vec2 v = vec2(dir.x * vertex.x + v2.x * vertex.y,
                dir.y * vertex.x + v2.y * vertex.y);
  uv = uv_in;
  uv2 = uv_in * uv_scale + uv_offset;
  gl_Position = vec4(((v * size + offset - cam_offset) / cam_size ) * 2,0.0,1.0);
}
"))
	(setf fs-blit2:loaded true)
	(setf fs-blit2:shader (gl:create-program))
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
	
	(gl:attach-shader fs-blit2:shader frag)
	(gl:attach-shader fs-blit2:shader vert)
	(gl:link-program fs-blit2:shader)
	
	(setf fs-blit2:shader:tex
	      (gl:get-uniform-location fs-blit2:shader "tex"))
	(setf fs-blit2:shader:tex
	      (gl:get-uniform-location fs-blit2:shader "tex2"))
	(setf fs-blit2:shader:offset (gl:get-uniform-location fs-blit2:shader "offset"))
	(setf fs-blit2:shader:size (gl:get-uniform-location fs-blit2:shader "size"))
	(setf fs-blit2:shader:dir (gl:get-uniform-location fs-blit2:shader "dir"))
	(setf fs-blit2:shader:cam-size (gl:get-uniform-location fs-blit2:shader "cam_size"))
	(setf fs-blit2:shader:cam-offset (gl:get-uniform-location fs-blit2:shader "cam_offset"))
	(setf fs-blit2:shader:uv-offset (gl:get-uniform-location fs-blit2:shader "uv_offset"))
	(setf fs-blit2:shader:uv-scale (gl:get-uniform-location fs-blit2:shader "uv_scale"))
	(gl:use-program fs-blit2:shader)
	(gl:uniform-2f fs-blit2:shader:offset 0 0)
	(gl:uniform-2f fs-blit2:shader:size 1 1)
	(gl:uniform-2f fs-blit2:shader:dir 1 0)
	(gl:uniform-2f fs-blit2:shader:cam-size 1 1)
	(gl:uniform-2f fs-blit2:shader:cam-offset 0 0)
	(gl:uniform-1i fs-blit2:shader:tex2 2)
	(gl:uniform-1i fs-blit2:shader:tex 1)
	(gl:uniform-2f fs-blit2:shader:uv-offset 0 0)
	(gl:uniform-2f fs-blit2:shader:uv-scale 1 1)
	)))

(ql:quickload 'sdl2)
(ql:quickload 'sdl2-ttf)

(defparameter *screenwidth* 640)
(defparameter *screenheight* 480)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3d-wiremesh

(defun arctan (y &optional x)
  (if x
      (/ (* 180 (atan y x)) pi)
      (/ (* 180 (atan y)) pi)))

(defparameter myfont ())
(defparameter arc-x 100)
(defparameter arc-y 75)
(defparameter pixeldist 5)
(defparameter planesize 50)
(defparameter plane (make-array (list planesize planesize)
				:element-type 'single-float
				:initial-element 0.0))
(setf (aref plane (/ planesize 2) (/ planesize 2)) 1.0)

(defun update-plane-simple-sin ()
  (multiple-value-bind (secs usecs) (get-time-of-day)
    (let ((newplane (make-array (list planesize planesize)
				:element-type 'single-float
				:initial-element 0.0)))
      (loop for ix from 0 below planesize do
	   (loop for iy from 0 below planesize do
		(setf (aref newplane ix iy) (sin (+ ix iy (+ secs (/ usecs 1000000)))))))
      (setf plane newplane))))

(defparameter kernelshape '((-1  -1) (0 -1) (1 -1)
			    (-1   0) (0  0) (1  0)
			    (-1   1) (0  1) (1  1)))
;; (defparameter kernelcoeff '(1/3   2/3   1/3
;; 			    2/3   -4    2/3
;; 			    1/3   2/3   1/3))
(defparameter kernelcoeff '(1      2   1
			    2    -12   2
			    1      2   1))

(defparameter kernelnormcoeff 1/1000)

(defun update-plane-with-kernel ()
  (let ((newplane (make-array (list planesize planesize)
			      :element-type 'single-float
			      :initial-element 0.0)))
    (loop for ix from 1 below (- planesize 1) do
	 (loop for iy from 1 below (- planesize 1) do
	      (setf (aref newplane ix iy)
		    (+ (aref plane ix iy)
		       (* kernelnormcoeff
			  (reduce #'+
				  (mapcar (lambda (coords coeff)
					    (* coeff (aref plane
							   (+ (car coords) ix)
							   (+ (cadr coords) iy))))
					  kernelshape
					  kernelcoeff)))))))
    
    (setf plane newplane)))
  
(defparameter vplane (make-array (list planesize planesize)
				 :element-type 'single-float
				 :initial-element 0.0))

;; (defun edge-correct (what shape ix iy)
;;   (mapcar #'cadr
;; 	  (remove-if #'car
;; 		     (mapcar (lambda (w s)
;; 			       (list
;; 				(or (< (+ (car s) ix) 0)
;; 				    (< (+ (cadr s) iy) 0)
;; 				    (>= (+ (car s) ix) planesize)
;; 				    (>= (+ (cadr s) iy) planesize))
;; 				w))
;; 			     what shape))))

(defun edge-correct-torus (shape coeff ix iy)
  (values 
   (mapcar (lambda (s)
             (if (or (< (+ (cadr s) iy) 0)
                     (>= (+ (cadr s) iy) planesize))
                 (list (car s) (* -1 (cadr s) (1- planesize)))
                 s))
           (mapcar (lambda (s)
                     (if (or (< (+ (car s) ix) 0)
                             (>= (+ (car s) ix) planesize))
                         (list (* -1 (car s) (1- planesize)) (cadr s))
                         s))
                   shape))
   coeff))

(defun edge-correct-reflect (shape coeff ix iy)
  (let* ((shape-and-coeff1              ; remove out of plane indices
          (remove-if (lambda (s)
                       (or (< (+ (cadar s) iy) 0)
                           (>= (+ (cadar s) iy) planesize)
                           (< (+ (caar s) ix) 0)
                           (>= (+ (caar s) ix) planesize)))
                     (mapcar #'cons shape coeff)))
         
         (coeffsum                      ; sum all non-negative coefficients
          (apply #'+ (remove-if (lambda (x) (< x 0)) 
                                         (mapcar #'cdr shape-and-coeff1))))
         
         (shape-and-coeff2              ; replace the 'middle' with -coeffsum
          (mapcar (lambda (x)
                    (if (> (cdr x) 0)
                        x
                        (cons (car x) (- coeffsum))))
                  shape-and-coeff1)))
    (values (mapcar #'car shape-and-coeff2)
            (mapcar #'cdr shape-and-coeff2))))

(defun update-plane-with-vplane ()
  (let ((newplane (make-array (list planesize planesize)
			      :element-type 'single-float
			      :initial-element 0.0))
	(newvplane (make-array (list planesize planesize)
				   :element-type 'single-float
				   :initial-element 0.0)))
    (loop for ix from 0 below planesize do
	 (loop for iy from 0 below planesize do
              (multiple-value-bind (ckernelshape ckernelcoeff)
                  (edge-correct-torus kernelshape kernelcoeff ix iy)
                (setf (aref newvplane ix iy)
                      (+ (aref vplane ix iy)
                         (* kernelnormcoeff
                            (reduce #'+
                                    (mapcar (lambda (coords coeff)
                                              (* coeff (aref plane
							   (+ (car coords) ix)
							   (+ (cadr coords) iy))))
					  ckernelshape
					  ckernelcoeff))))))))
    (setf vplane newvplane)
    (loop for ix from 0 below planesize do
	 (loop for iy from 0 below planesize do
	      (setf (aref newplane ix iy)
		    (+ (aref plane ix iy) (aref vplane ix iy)))))
    (setf plane newplane)))

(defun render-text (text renderer x y)
  (let ((s (sdl2-ttf:render-text-solid myfont text 255 255 255 0)))
    (sdl2:render-copy renderer
		      (sdl2:create-texture-from-surface renderer s)
		      :dest-rect (sdl2:make-rect x y
						 (sdl2:surface-width s)
						 (sdl2:surface-height s)))))


(defun line (renderer x1 y1 x2 y2)
  (when (loop for n in (list x1 y1 x2 y2)
	   thereis (and (floatp n) (float-nan-p n)))
    (return-from line))
  (sdl2:render-draw-line renderer
			 (floor (+ (/ *screenwidth* 2) (/ (* *screenwidth* x1) arc-x)))
			 (floor (+ (/ *screenheight* 2) (/ (* *screenheight* y1) arc-y)))
			 (floor (+ (/ *screenwidth* 2) (/ (* *screenwidth* x2) arc-x)))
			 (floor (+ (/ *screenheight* 2) (/ (* *screenheight* y2) arc-y)))))

(defun xline (renderer x1 y1 z1 x2 y2 z2)
  (declare (special kx ky kz))
  (when (loop for n in (list x1 y1 x2 y2)
	   thereis (and (floatp n) (float-nan-p n)))
    (return-from xline))  
  (when (<= (min z1 z2) kz)
    (return-from xline))
  (line renderer
	(arctan (- x1 kx) (- z1 kz))
	(arctan (- y1 ky) (- z1 kz))
	(arctan (- x2 kx) (- z2 kz))
	(arctan (- y2 ky) (- z2 kz))))

(defun rotate (x y angle)  ;; als naechstes dann mit rotationsmatrix
  (when (= angle 0)
    (return-from rotate (values x y)))
  (let* ((s 1)
	 (hyp (sqrt (+ (* x x) (* y y))))
	 (a ()))
    (when (= hyp 0) (return-from rotate (values x y)))
    (when (< x 0)
      (setf s -1)
      (setf x (* -1 x))
      (setf y (* -1 y)))
    (setf a (asin (/ y hyp)))
    (values (* s (cos (+ a (/ (* pi angle) 180))) hyp)
	    (* s (sin (+ a (/ (* pi angle) 180))) hyp))))


(defun drawwire (renderer x1 y1 z1 x2 y2 z2)
  (declare (special alpha beta gamma))
  (setf (values x1 y1) (rotate x1 y1 gamma))
  (setf (values x2 y2) (rotate x2 y2 gamma))

  (setf (values x1 z1) (rotate x1 z1 beta))
  (setf (values x2 z2) (rotate x2 z2 beta))

  (setf (values y1 z1) (rotate y1 z1 alpha))
  (setf (values y2 z2) (rotate y2 z2 alpha))

  (xline renderer x1 y1 z1 x2 y2 z2))

(defun drawarray (renderer arr)
  (sdl2:set-render-draw-color renderer 0 0 0 0)
  (sdl2:render-clear renderer)
  (sdl2:set-render-draw-color renderer 255 255 255 0)  
  (loop
     for ix from 0 below (1- planesize)
     do (loop for iz from 0 below (1- planesize)
	   do (let ((xpos (- (* ix pixeldist) (/ (* pixeldist planesize) 2)))
		    (zpos (- (* iz pixeldist) (/ (* pixeldist planesize) 2))))
		(drawwire renderer
			  xpos (* pixeldist (aref arr ix iz)) zpos
			  (+ xpos pixeldist) (* pixeldist (aref arr (1+ ix) iz)) zpos)
		(drawwire renderer
			  xpos (* pixeldist (aref arr ix iz)) zpos
			  xpos (* pixeldist (aref arr ix (1+ iz))) (+ pixeldist zpos))))))

(defun wiremesh ()
  (sdl2:with-init (:video)
    (sdl2-ttf:init)
    (setf myfont (sdl2-ttf:open-font
		  #p"/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf" 11))
    (let ((kx 0)
	  (ky -40)
	  (kz -250)
	  (alpha 20)
	  (beta 0)
	  (gamma 0)
	  (turnmode ())
	  (frames 0)
	  (starttime (get-universal-time)))
      (declare (special kx ky kz alpha beta gamma))
      (sdl2:with-window (win :title "mo - wiremesh"
			     :w *screenwidth*
			     :h *screenheight*
			     :flags '(:shown))
	(sdl2:with-renderer (renderer win)
	  (sdl2:with-event-loop (:method :poll)
	    (:keydown
	     (:keysym keysym)
	     (cond ((sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
	    (sdl2:push-event :quit))
		   ((sdl2:scancode= (sdl2:scancode-value keysym) :scancode-home)
		    (setf gamma (mod (+ gamma 355) 360)))
		   ((sdl2:scancode= (sdl2:scancode-value keysym) :scancode-end)
		    (setf gamma (mod (+ gamma 5) 360)))
		   
		   ((sdl2:scancode= (sdl2:scancode-value keysym) :scancode-left)
		    (if turnmode
			(setf beta (mod (+ beta 355) 360))
			(decf kx)))
		   ((sdl2:scancode= (sdl2:scancode-value keysym) :scancode-right)
		    (if turnmode
			(setf beta (mod (+ beta 5) 360))
			(incf kx)))
		   ((sdl2:scancode= (sdl2:scancode-value keysym) :scancode-up)
		    (if turnmode
			(setf alpha (mod (+ alpha 5) 360))
			(decf ky)))
		   ((sdl2:scancode= (sdl2:scancode-value keysym) :scancode-down)
		    (if turnmode
			(setf alpha (mod (+ alpha 355) 360))
			(incf ky)))
		   ((sdl2:scancode= (sdl2:scancode-value keysym) :scancode-tab)
		    (setf turnmode (not turnmode)))
		   ((sdl2:scancode= (sdl2:scancode-value keysym) :scancode-kp-plus)
		    (setf kz (1+ kz)))
		   ((sdl2:scancode= (sdl2:scancode-value keysym) :scancode-kp-minus)
		    (setf kz (1- kz)))
		   (t (write (sdl2:scancode-value keysym)) (terpri)))
	     (format t "~S ~S ~S ~S ~S ~S ~%" kx ky kz alpha beta gamma))
	    
	    (:idle
	     ()
	     (update-plane-with-vplane)
	     (drawarray renderer plane)
	     (let ((fps (or (= (get-universal-time) starttime)
	     		    (/ (incf frames) (- (get-universal-time) starttime))))
	     	   (ssq (loop for ix from 0 below planesize summing
	     		     (loop for iy from 0 below planesize summing
	     			  (+ (aref plane ix iy))))))  ; (aref vplane ix iy))))))
	       (render-text (format nil "sum of squares: ~2,4,,$ - FPS ~2,3,,$~%" ssq fps)
			    renderer 10 10))
	     (sdl2:render-present renderer))
	    (:quit () t)))))))

(defun doit ()
  (defparameter plane (make-array (list planesize planesize)
				  :element-type 'single-float
				  :initial-element 0.0))
  (defparameter vplane (make-array (list planesize planesize)
				   :element-type 'single-float
				   :initial-element 0.0))
  ;; (setf (aref plane (/ planesize 5) (/ planesize 5)) 30.0)
  (loop for ix from 0 below planesize do
       (loop for iy from 0 below planesize do
            (setf (aref plane ix iy)
                  (* 20
                     (exp (* (- (+ (expt (- ix (/ planesize 5)) 2)
                                   (expt (- iy (/ planesize 5)) 2)))
                             .5))))))
  
  (wiremesh))


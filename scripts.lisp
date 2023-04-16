(ql:quickload '("opticl" "clusters" "drakma" "iterate" "metabang-bind" "dufy"))
(in-package #:clusters.k-means)
;; fix weird bug
(defmethod initialize-instance :after ((state algorithm-state)
                                       &rest all)
  (declare (ignore all))
  (select-initial-medoids state))
(defpackage k/cl-user
  (:use :cl :iter :bind))
(in-package #:k/cl-user)
(defun get-color (image)
  (declare (type opticl:8-bit-rgb-image image))
  (let (hues)
    (opticl:with-image-bounds (w h) image
            (iter (for x below w)
              (iter (for y below h)
                (bind (((:values hue sat _)
                        (dufy:rgb-to-hsl (/ (aref image x y 0) 255.0)
                                         (/ (aref image x y 1) 255.0)
                                         (/ (aref image x y 2) 255.0)))
                       (hue (* (float pi 1.0) (/ (float hue 1.0) 180.0)))
                       (sat (float sat 1.0))
                       (sat (* sat (- 2.0 sat))))
                  (when (> sat 0.2)
                    (push (make-array 2 :element-type 'single-float
                                               :initial-contents
                                               (list (* sat (cos hue))
                                                     (* sat (sin hue))))
                                 hues))))))
    (let* ((*random-state* (sb-ext:seed-random-state 123))
           (state (clusters:make-algorithm-state
                   (make-instance 'clusters.k-means:parameters
                                  :medoids-count 3
                                  :iterations 30
                                  :distortion-epsilon 1.0)
                   (coerce hues 'vector))))
      (clusters:run-algorithm state)
      (labels ((vector-to-hue (v)
                 (mod (/ (atan (aref v 1) (aref v 0)) 2 (float pi 1.0)) 1.0)))
        (let ((result-alist
                (sort (iter (for v in-vector (clusters.k-means::access-medoids state))
                        (for c in-vector (clusters.k-means::read-clusters state))
                        (collect (cons (length c) v)))
                      #'> :key #'car)))
          (format t "Computed hue vectors: ~a~%" result-alist)
          (bind ((hue-1 (vector-to-hue (cdr (nth 0 result-alist))))
                 (hue-2 (vector-to-hue (cdr (nth 1 result-alist))))
                 (hue-3 (if (> (car (nth 2 result-alist))
                               (* (car (nth 0 result-alist)) 0.4))
                            (vector-to-hue (cdr (nth 2 result-alist)))
                            (vector-to-hue (cdr (nth 0 result-alist)))))
                 (hue-1-2 (mod (- hue-1 hue-2) 1.0))
                 (hue-1-2 (if (> hue-1-2 0.5) (- 1.0 hue-1-2) hue-1-2)))
            (list hue-1
                  (if (> hue-1-2 0.05)
                      hue-2
                      (mod (- hue-1 0.5) 1.0))
                  hue-3)))))))
(defun get-color-url (url)
  (get-color
   (let ((stream (drakma:http-request url :want-stream t)))
     (unwind-protect
          (opticl:read-jpeg-stream stream)
       (close stream)))))

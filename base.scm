(define-syntax point
  (syntax-rules ()
    ( (point (a b))
         (format #f "~A ~A" a b))
    )
  )

(define-syntax bezier
  (syntax-rules ()
     ( (bezier sPT ePT cPT)
         `((d . ,(string-append "M "
                       (point sPT)
                       " Q "
                       (point cPT) " "
                       (point ePT)))
                 (stroke . "black")
                 (fill . "none")) 
     )
  )
)

(define-syntax group
  (syntax-rules ()
    ((group shape ...) ; lista em define-syntax
     (let* ((get_d (lambda (curve) (cdr (assq 'd curve))))
            (newD (map get_d (list shape ...))))
       
            `((d . ,(string-join newD " "))
                 (stroke . "black")
                 (fill . "none"))
       )
     )
    )
  )
  
 (define-syntax fill
  (syntax-rules ()
    ( (fill color shape)
         (let ((newD (cdr (assq 'd shape)) )
               (newStroke (cdr (assq 'stroke shape)) )
               (newColor (format #f "~A" (quote color) ) ))
           
               `((d . ,newD )
                 (stroke . ,newStroke)
                 (fill . ,newColor))
           )
    )
  )
)

(define-syntax new-panel
     (syntax-rules ()
          ( (new-panel w h)
            ( let ((width w)
                   (height h))

                (lambda (shape) 
                      (let ((newD (cdr (assq 'd shape)) )
                           (newStroke (cdr (assq 'stroke shape)) )
                           (newColor (cdr (assq 'fill shape)) ))
                       
                            `(<svg
                                  width="300"
                                  height="300"
                                  viewBox= ,(format #f "0 0 ~A ~A" width height )
                                >
                                  <path
                                    d= ,(format #f "~A" newD )
                                    stroke= ,(format #f "~A" newStroke)
                                    fill= ,(format #f "~A" newColor)
                                    stroke-width="2"
                                  />
                                </svg>)
                       )
                  )
            )
          )
     )
)

(define-syntax defineSVG
  (syntax-rules ()
    ((defineSVG name (args ...)
       template)
     (define-syntax name
       (syntax-rules ()
         ((name args ...) template))))))
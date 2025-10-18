(define-syntax point
  (syntax-rules ()
    ( (point (a b))
         (format #f "~A ~A" a b))
    )
  )

(define-syntax bezier
  (syntax-rules ()
     ( (bezier sPT ePT cPT)
         `( path (d . ,(string-append "M "
                       (point sPT)
                       " Q "
                       (point cPT) " "
                       (point ePT)))
                 (stroke . "black")
                 (fill . "none")) 
     )
  )
)

(define-syntax form
  (syntax-rules ()
    ((form shape ...) ; lista em define-syntax
     (let* ((get_d (lambda (curve) (cdr (assq 'd curve))))
            (newD (map get_d (list shape ...))))
       
            `( path (d . ,(string-join newD " "))
                 (stroke . "black")
                 (fill . "none"))
       )
     )
    )
  )


(define-syntax union
  (syntax-rules ()
    ((union shape ...)
         (let* ((get_shape (lambda (x)
                              (let ((type (car x))
                                    (rest (cdr x)))
                                (cond
                                  ((eq? 'group type) (car rest) )
                                  (else (cons type rest) ))))
                            )
                (temp_s (map get_shape (list shape ...)) )
                (unite (lambda (x)
                             (if (and (list? x) (list? (car x)) (eq? 'path (caar temp_s)))
                                 x
                                 (list x))))
                (shapes (apply append (map unite temp_s) ) ))
                  `( group (,@shapes))
         )
      )
  )
)
  
 (define-syntax fill
  (syntax-rules ()
    ( (fill color shape)
         (let ((newD (cdr (assq 'd (cdr shape))) )
               (newStroke (cdr (assq 'stroke (cdr shape))) )
               (newColor (format #f "~A" (quote color) ) ))
           
               `( path (d . ,newD )
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
                      (let* ((type (car shape) )
                            (content (cdr shape))
                            (toSVG (lambda (cont)
                                            (let ((newD (cdr (assq 'd cont)) )
                                                (newStroke (cdr (assq 'stroke cont)) )
                                                (newColor (cdr (assq 'fill cont)) ))
                                            
                                                  `(<path
                                                          d= ,(format #f "~A" newD )
                                                          stroke= ,(format #f "~A" newStroke)
                                                          fill= ,(format #f "~A" newColor)
                                                          stroke-width="2"
                                                        />)
                                            )

                                    ) ))

                            (cond
                                 ((eq? 'path type)
                                    
                                    (append 
                                        `(<svg
                                            width="300"
                                            height="300"
                                            viewBox= ,(format #f "0 0 ~A ~A" width height )
                                          >)
                                        (toSVG content)
                                        '(</svg>)
                                    )

                                  )
                                 (else  
                                      (append 
                                          `(<svg
                                              width="300"
                                              height="300"
                                              viewBox= ,(format #f "0 0 ~A ~A" width height )
                                            >)
                                          (map (lambda (x) (toSVG (cdr x)) ) (car content))
                                          '(</svg>)
                                      )
                                 )

                                 )
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
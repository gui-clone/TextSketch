(define-syntax point
  (syntax-rules ()
    ( (point (a b))
         (format #f "~A ~A" a b))
    )
  )

(define-syntax bezier
  (syntax-rules ()
     ( (bezier sPT ePT cPT)
         `( path (d . (( ,(point sPT) ,(point ePT) ,(string-append "Q " (point cPT)) )) )
                 (stroke . "black")
                 (fill . "none")) 
     )
  )
)


(define-syntax form
  (syntax-rules ()
    ((form shape ...) ; lista em define-syntax
     (let* ((get_d (lambda (curve) (cadr (assq 'd (cdr curve) ))))
            (dList (map get_d (list shape ...)))
            (sPT (caar dList) )
            (ePT (cadr (car (list-tail dList (- (length dList) 1))) ))
            (bodyList (apply append (map (lambda (path) (reverse (cdr path))) dList)) ) ; get the second and third element of every list
            (body (string-join (reverse (cdr (reverse bodyList))) " ") ) ; remove the last elemet and concatenate strings
            )
       
            `( path (d . ((,sPT ,ePT ,body)) )
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
                                            (let* ((newD (cadr (assq 'd cont)) )
                                                   (sPT (car newD))
                                                   (ePT (cadr newD))
                                                   (otherD (caddr newD))
                                                   (newStroke (cdr (assq 'stroke cont)) )
                                                   (newColor (cdr (assq 'fill cont)) )
                                                   (close-str (if (string=? sPT ePT) " Z" "")))
                                            
                                                  `(<path
                                                          d= ,(string-append "M "
                                                                   sPT
                                                                   " "
                                                                   otherD " "
                                                                   ePT
                                                                   close-str )
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
                                            width="400"
                                            height="400"
                                            viewBox= ,(format #f "0 0 ~A ~A" width height )
                                          >)
                                        (toSVG content)
                                        '(</svg>)
                                    )

                                  )
                                 (else  
                                      (append 
                                          `(<svg
                                              width="400"
                                              height="400"
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
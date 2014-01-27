;Joshua Marple
;EECS 368
;Project 5
#lang web-server/insta
(require mzlib/string)
(static-files-path (current-directory))
(define-namespace-anchor a)
(define environment (namespace-anchor->namespace a))

 
; A blog is a (blog posts)
; where posts is a (listof post)
(struct blog (posts) #:mutable)
 
; and post is a (post title body)
; where title is a string, and body is a string
(struct post (symbolic-derivative evaluated-symderiv numeric-deriv))
 
; BLOG: blog
; The initial BLOG.
(define BLOG
  (blog
   (list )))
 
; blog-insert-post!: blog post -> void
; Consumes a blog and a post, adds the post at the top of the blog.
(define (blog-insert-post! a-blog a-post)
  (set-blog-posts! a-blog
                   (cons a-post (blog-posts a-blog))))
 
; start: request -> doesn't return
; Consumes a request and produces a page that displays
; all of the web content.
(define (start request)
  (render-symderiv-page request))
 
(define (parse-post bindings)
  (define variable (read-from-string (extract-binding/single 'variable bindings)))
  (define function (read-from-string (extract-binding/single 'function bindings)))
  (define order (read-from-string (extract-binding/single 'order bindings)))
  (define point (read-from-string (extract-binding/single 'point bindings)))
  (define simplify (read-from-string (extract-binding/single 'simplify bindings)))
  (define newfunc `(λ ,(list variable) ,function))
  (define nderv (nth-symderiv newfunc order))
  (define ndervs (nderivs newfunc order))
  (define n-1-derv (nth-symderiv newfunc (- order 1)))
  (define n-1-dervs (nderivs newfunc (- order 1)))
  (if (eq? simplify 'no)
      (post (expr->string nderv)
            (expr->string ((eval nderv environment)point))
            (expr->string  ((deriv(eval n-1-derv environment))point)))
      (if (eq? simplify 'yes)
          (post (expr->string (nderivs newfunc order))
                (expr->string ((eval ndervs environment)point))
                (expr->string ((deriv (eval n-1-dervs environment))point))
            )
          (post "did you want to simplify?"))))

(define (nderivs function n)
  (define x (- n 1))
  (if (> n 0)
      (nth-symderiv (symderivs function) x)
      function))

(define (nth-deriv function n)
  (define x (- n 1))
    (if (> n 0 )
    (nth-deriv (deriv function) x)
    function))

(define (nth-symderiv function n)
  (define x (- n 1))
  (if (> n 0)
      (nth-symderiv (symderiv function) x)
      function))

; render-blog-page: request -> doesn't return
; Produces an HTML page of the content of the BLOG.
(define (render-symderiv-page request)
  (local [(define (response-generator make-url)
            (response/xexpr
             `(html
               (head (title "Whooo, lets differentiate"))
               (body ((style "background-color: #F516B9; font-family: helvetica; font-size: 14pt;"))
                     (center (h1 "Some fine differentiation"))(hr)
                     (form ((action,(make-url insert-post-handler)))
                           (nbsp "In terms of ")
                               (input ((type "text")(name "variable")(size "2")(value "x"))) (nbsp ",")
                           (nbsp " function ") 
                               (input ((name "function"))) (nbsp "    ")
                           (nbsp "and differentiate to the power of ")
                               (input ((name "order")(size "5")))(br)(br)
                           (nbsp "Simplify? ") (br)
                               (input ((name "simplify")(type "radio") (value "yes"))) (nbsp "yes ") (br)
                               (input ((name "simplify") (type "radio") (value "no"))) (nbsp "no ") (br)(br)
                           (nbsp "Evaluate at: ")
                               (input ((name "point")(size "15")))(br)(br)
                           (input ((type "submit"))))
                     ,(render-posts)
                     (center (img ((src "http://i.imgur.com/gUPQxIV.png"))))
                     (center (p "Newton being chill"))
                     (br)
                     (center (a ((href "http://en.wikipedia.org/wiki/Differentiation_(mathematics)"))"Wikipedia Differentiation"))(br)))))
          
          (define (insert-post-handler request)
            (blog-insert-post!
             BLOG (parse-post (request-bindings request)))
            (render-symderiv-page request)) ]
    (send/suspend/dispatch response-generator)))
 
; render-post: post -> xexpr
; Consumes a post, produces an xexpr fragment of the post.
(define (render-post a-post)
  `(div ((class "post"))
        ,(post-symbolic-derivative a-post)
        (p ,(post-evaluated-symderiv a-post))
        (p ,(post-numeric-deriv a-post))))
 
; render-posts: -> xexpr
; Consumes a blog, produces an xexpr fragment
; of all its posts.
(define (render-posts)
  `(div ((class "posts"))
        ,@(map render-post (blog-posts BLOG))))

; symderiv stuff

(define (symderivs qf)
  (list 'λ (cadr qf) (simplifier(symderiveat (caadr qf) (caddr qf)))))

(define (symderiv qf)
  (list 'λ (cadr qf) (symderiveat (caadr qf) (caddr qf))))

(define (simplifier func) 
  (if (list? func)
      (case (car func)
        ((+) (if (andmap number? (cdr func))
                 (eval func environment)
                 func))
        ((*) (if (andmap number? (cdr func))
                 (eval func environment)
                 func))
        ((-) (if (andmap number? (cdr func))
                 (eval func environment)
                 func))
        (else func))
      func))
    
(define (symderiveat v e)
        (cond((symbol? e) (if(eq? v e)1 0))
       ((not(pair? e)) 0)
       ((not(pair?(cdr e))) 0)
       (else(let((f (cadr e)))
              (case(car e)
                ((+ -) (cons(car e)(map (λ(x)(symderiveat v x))(cdr e))))
                ((*)   (cons '+(map(λ(x)`(* ,(symderiveat v x) ,@(remv x(cdr e))))(cdr e))))
                ((/)   (if(pair?(cddr e))
                          (symderiveat v `(* ,f(/(* ,@(cddr e)))))
                          `(/ ,(symderiveat v f)-1(expt ,f 2))))
                ((exp) `(* ,e ,(symderiveat v f)))
                ((log) `(/ ,(symderiveat v f)                ,f))
                ((expt)`(* ,e ,(symderiveat v `(* ,(caddr e)(log ,f)))))
                ((sin) `(*(cos ,f) ,(symderiveat v f)))
                ((cos) `(* -1(sin ,f) ,(symderiveat v f)))
                ((tan) `(/ ,(symderiveat v f)(expt(cos ,f)2)))
                ((asin)`(/ ,(symderiveat v(cadr e))(expt(-1(expt ,f 2))1/2)))
                ((acos)`(/(- ,(symderiveat v f))(expt(- 1(expt ,f 2))1/2)))
                ((atan) (if(null?(cddr e))
                           `(/ ,(symderiveat v f)(+ 1(expt ,f 2)))
                           (symderiveat v `(atan(/ ,f ,(caddr e))))))
                ((abs)  `(*(/ ,f ,e) ,(symderiveat v f)))
                (else 'ERROR-IN-symderiv--------------)
                )))))

;derivative approximation function
(define h 1/100000000)
(define (deriv func) (λ(x) (/(-(func(+ x h))(func x))h)))


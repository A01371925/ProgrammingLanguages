;----------------------------------------------------------
; Problem Set #4
; Date: May 18, 2020.
; Authors:
;          A01378562 Karla Daniela López Vega
;          A01371925 Luis Eduardo Díaz Niño de Rivera
;----------------------------------------------------------

(defmacro my-or
  "Make sure that any expression gets evaluated at most once.
  You may not use Clojure's or macro."
  ([] nil)
  ([x] x)
  ([x & args]
   `(let [t# ~x]
      (if t#
        ~x
        (my-or ~@args))))
)


(defmacro do-loop
  "Write a macro called do-loop that implements a post-test loop control statement.
  It must combine the functionality of C's do-while statement and Pascal's repeat-until
  statement."
  )

(defmacro def-pred
  "Write a macro called def-pred,the macro should define two predicate functions: a regular one and its
  negated version"
  )

(defmacro defn-curry
  "The macro should define a function called name that takes only the first argument from
  args and returns a function that takes the second argument from args and returns a function
  that takes the third argument from args, and so on. The last function returned takes the last argument
  from args and evaluates all the expressions in body using a do special form"
  )

(defmacro IF
  "Provide a conditional statement that is syntactically a bit more similar
  to those found in languages like Pascal or Fortran. It has the following
  form:
          (IF condition :THEN exp1 exp2 ... :ELSE exp3 exp4 ...)"
  )





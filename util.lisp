(cl:in-package :sih)

(defmacro random-nth (lst)
  `(nth (random (length ,lst)) ,lst))

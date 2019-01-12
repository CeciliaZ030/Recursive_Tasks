;List Function


(defun append (list1 list2)
	"Append two lists"
	(reverse_help (reverse list1) list2)
)


(defun reverse_help (list1 list2)
	(cond ((null list1)list2)
		((reverse_help (cdr list1) (cons (car list1) list2)))
	)
)

(defun reverse (list1)
	"Reverse sequence of a list"
	(reverse_help list1 '())
)


(defun map (function list1)
	(if(null list1)
		(return-from map 'nil)
		(cons (apply function (list (car list1))) (map function (cdr list1)))
	)
)




(defun nub_help1 (num list2)
	(if (null list2)
		nil
		(if (eq num (car list2))
			T
			(nub_help1 num (cdr list2))
		)
	)
)

(defun nub_help2 (list1 list2)
	(if (null list1)
		(reverse list2)
		(if (nub_help1 (car list1) list2)
			(nub_help2 (cdr list1) list2)
			(nub_help2 (cdr list1) (cons (car list1) list2))
		)
	)
)


(defun nub (list1)
	(nub_help2 list1 '())
)


(defun fold (num fun list1)
	(if (null list1)
		num
		(fold (apply fun num (list (car list1))) fun (cdr list1))
	)
)

(defun filter_help (fun list1 list2)
	(if (null list1)
		(reverse list2)
		(if (apply fun (list (car list1)))
			(filter_help fun (cdr list1) (cons (car list1) list2))
			(filter_help fun (cdr list1) list2)
		)
		
	)
)


(defun filter (fun list1)
	(filter_help fun list1 '())
)


(defun merge_help (list1 list2 result)
	(cond 	((null list1) (append result list2))
			((null list2) (append result list1))
			((< (car list1) (car list2)) (merge_help (cdr list1) list2 (append result (list (car list1)))))
			(T (merge_help (cdr list2) list1 (append result (list (car list2)))))
	)
)


(defun merge (list1 list2)
	(merge_help list1 list2 '())
)



(defun addtoend_help (list2 list1)
	(if (null list1)
		list2
		(addtoend_help (cons (car list1) list2) (cdr list1))
	)
)

(defun addtoend (el list1)
	
	(addtoend_help (list el) (reverse list1))
)

(defun indexof_help (el list1 count)
	(if (null list1)
		(- 0 1)
		(if (eq el (car list1))
			count
			(indexof_help el (cdr list1) (+ count 1))
		)
	)
)

(defun indexof (el list1)
	(indexof_help el list1 0 )
)


(defun remove_help (el list1 list2)
   (if (null list1)
      (reverse list2)
      (if (eq el (car list1))
         (remove_help el (cdr list1) list2)
         (remove_help el (cdr list1) (cons (car list1) list2))
      )
   )
)

(defun remove_all (el list1)
   (remove_help el list1 '())
)

(defun member (el set1)
	(if (null set1)
		nil
		(if (eq el (car set1))
			T
			(member el (cdr set1))
		)
	)
)

(defun insert (el set1)
	(cons el set1)
)

(defun intersection_help (set1 set2 result)
	(if (null set1)
		result
		(if (member (car set1) set2)
			(intersection_help (cdr set1) set2 (insert (car set1) result))
			(intersection_help (cdr set1) set2 result)
		)
	)
)

(defun intersection (set1 set2)
	(intersection_help set1 set2 '())
)

(defun union (set1 set2)
	(if (null set1)
		set2
		(if (member (car set1) set2)
			(union (cdr set1) set2)
			(union (cdr set1) (cons (car set1) set2))
		)
	)
)


(defun abs (num)
	(if (> num 0)
		num
		(- 0 num)
	)
)


(defun factorial (num)
	(if (= num 1)
		1
		(* num (factorial (- num 1)) )
	)
)



(defun right_tri (num1 num2 num3)
	(if (= (+ (* num1 num1) (* num2 num2)) (* num3 num3))
		T
		nil
	)
)

(defun gcd_help (num1 num2)
	(if (eq num1 0)
		0
		(if (< num1 num2)
			num1
			(gcd_help (- num1 num2) num2)
		)
	)
)

(defun gcd_help1 (num1 num2)
	(if (eq num2 0)
		num1
		(gcd_help1 num2 (gcd_help num1 num2))
	)
)

(defun gcd (num1 num2)
	(cond ((> num1 num2) (gcd_help1 num1 num2))
		((< num1 num2) (gcd_help1 num2 num1))
		(T num1)
	)
)

(defun divides (num1 num2)
   (if (= 0 (gcd_help num1 num2))
      T
      nil
   )
)

(defun prime_help (num1 num2)
   (if (= num2 1)
      T
      (if (divides num1 num2)
         nil
         (prime_help num1 (- num2 1))
      )
   )
)

(defun prime (num)
   (if (prime_help num (- num 1))
      T
      nil
   )
)


(defun perfectp_help (num count)
   "retun how many times a number can be divided by two"
   (if (divides num 2)
      (perfectp_help (/ num 2) (+ count 1))
      count
   )
)

(defun divide_2 (num count)
   (if (= count 0)
      num
      (divide_2 (/ num 2) (- count 1))
   )
)

(defun exponential_2 (num count)
   (if (= count 1)
      num
      (exponential_2  (* num 2) (- count 1))
   )
)


(defun perfectp (num)
   (if (= (divide_2 num (perfectp_help num 0)) (- (exponential_2 2 (+ (perfectp_help num 0) 1)) 1))
      T
      nil
   )
)

(defun sum_help (num count sum)
	(if (= count 0)
		sum
		(if (divides num count)
			(sum_help num (- count 1) (+ sum count))
			(sum_help num (- count 1) sum)
		)
	)
)

(defun factor_sum (num)
	(sum_help num (- num 1) 0)
)

(defun abundantp (num)
	(if (< num (factor_sum num))
		T
		nil
	)
)

(defun deficientp (num)
	(if (> num (factor_sum num))
		T
		nil
	)
)


;You must implement four functions from each of the “List,” “Set,” and “Math” sections
;below. You must also implement the three functions from the final “Required Functions”
;section.

;Test list
 (write-line "List Inplementation")

 (write-line "1. Append two lists (enter two lists)")
 (write (append (read) (read)))
 (write-line " ")

 (write-line "2. Reverse a list (enter a list)")
 (write (reverse (read)))
 (write-line " ")

 (write-line "3. Map a function over every element in a list (enter a list) the function is add3")
 (defun add3 (x) (+ 3 x))
 (write (map 'add3 (read)))
 (write-line " ")

 (write-line "4. Remove duplicates from a list (enter a list)")
 (write (nub (read)))
 (write-line " ")

 (write-line "5. Fold-left (arguments are: initial value, function, list) (enter a initial value, a function, and a list)")
 (write (fold (read) (read) (read)))
 (write-line " ")

 (write-line "6. Filter less than 3 (enter a list)")
 (defun lessthan3 (x) (< x 3))
 (write (filter 'lessthan3 (read)))
 (write-line " ")

 (write-line "7. Merge two sorted lists (enter two lists)") 
 (write (merge (read) (read)))
 (write-line " ")

 (write-line "8. Add an element to the end of a list (enter an element and a list)")
 (write (addtoend (read) (read)))
 (write-line " ")

 (write-line "9. Index of (enter an element and a list)")
 (write (indexof (read) (read)))
 (write-line " ")

 (write-line "10. Remove-all (enter an element and a list)")
 (write (remove_all (read) (read)))


 ;Test set 
  (write-line " ")
 (write-line "Set Inplementation")

 (write-line "1. Set membership (enter an element and a sets)")
 (write (member (read) (read)))
 (write-line " ")

 (write-line "2. Insert element into set (enter an element and a set)")
 (write (insert (read) (read)))
 (write-line " ")

 (write-line "3. Set intersection (enter two sets)")
 (write (intersection (read) (read)))
 (write-line " ")

 (write-line "4. Set union (enter two sets)") 
 (write (union (read) (read)))


 ;Test math
  (write-line " ")
 (write-line "Math Functions")
 (write-line "1. Absolute value (enter a number)")
 (write (abs (read)))
 (write-line " ")

 (write-line "2. Factorial (enter a number)")
 (write (factorial (read)))
 (write-line " ")

 (write-line "3. Check if 3 integers can be the lengths of the two sides and the hypoteneuse of a
right triangle (in that order) enter 3 numbers")
 (write (right_tri (read) (read) (read)))
 (write-line " ")

 (write-line "4. Greatest Common Divisor (enter two number)")
 (write (gcd (read) (read)))

 ;Test Required
  (write-line " ")
 (write-line "Required Functions")
  (write-line "1. Check if a number is perfect (enter a number)")
 (write (perfectp (read)))
 (write-line " ")

  (write-line "2. Check if a number is abundant (enter a number)")
 (write (abundantp (read)))
 (write-line " ")

  (write-line "3. Check if a number is deficient (enter a number)")
 (write (deficientp (read)))
 (write-line " ")

  (write-line "Finda the sum of the factorial (enter a number)")
 (write (factor_sum (read)))

















 
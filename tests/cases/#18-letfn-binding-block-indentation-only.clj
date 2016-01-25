(letfn [(double [x]
          (* x 2))
        (triple [x]
          (* x 2))]   ;; special indentation here
  (let [y (double 2)
         z (double 3)]
    (println y
      z)))     ;; but not here

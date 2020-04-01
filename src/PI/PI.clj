double compute_pi(long num_rects) {
                                   double mid, height, width;
                                   double sum = 0.0;

                                   width = 1.0 / (double) num_rects;
                                   for (long i = 0; i < num_rects; i++) {
                                             mid = (i + 0.5) * width;
                                             height = 4.0 / (1.0 + mid * mid);
                                             sum += height;
                                   }
return width * sum;;
}

(defn PI
  [num]
  (let [width (/ 1.0 num)]
    (loop [sum 0.0
           i 0]
      (if (< i num)
        (recur (+ sum (/ 4.0 (+ 1.0 (* (* (+ i 0.5) width) (* (+ i 0.5) width)))))
               (inc i))
        (* sum width)))))

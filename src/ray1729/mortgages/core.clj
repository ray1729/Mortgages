(ns ray1729.mortgages.core)

(defn pct->multiplier
  "Convert a percentage to an equivalent multiplier. For example, adding 10%
  to a value is equivalent to multiplying by 1.1."
  [p]
  (+ 1 (/ p 100)))

(defn multiplier->pct
  "Convert a multiplier to an equivalent percentage. For example, multiplying
  by 1.01 is equivalent to adding 1%."
  [m]
  (* 100 (- m 1)))

;; APR is compounded annual interest rate
;;
;; Suppose the period rate is r, and there are n periods in a year, then:
;;
;; APR = ( (1 + r/100)^n - 1 )*100
;;
;; => 1 + APR/100 = (1 + r/100)^n
;;
;; => log(1 + APR/100) = n log(1+r/100)
;;
;; => log(1 + r/100) = log(1 + APR/100)/n

(defn apr->period
  "Given the number of periods and an annual percentage rate,
  compute the equivalent period rate multiplier. This is essentially
  the inverse of computing compound interest."
  [n apr]
  (Math/exp (/ (Math/log (pct->multiplier apr)) n)))

(def apr->monthly (partial apr->period 12))

(def apr->daily (partial apr->period 365))

;; Compute the monthly repayment for a given loan amount, APR, and mortgage term
;;
;; Let: m = (apr->montly APR)
;;      n = number of repayments
;;      A = loan amount
;;      p = monthly repayment
;;    A_k = amount outstanding after k months
;;
;; We know m,n,A and need to solve for p such that A_n = 0. Assuming that interest
;; is added at the beginning of the month, then a monthly payment made, we have:
;;
;; A_k+1 = A_k * m - p
;;
;; Writing down the first few values:
;;
;; A_0 = A
;; A_1 = A * m - p
;; A_2 = (A * m - p) * m - p = Am^2 - p(m + 1)
;; A_3 = (Am^2 - p(m-1))*m - p = Am^3 - p(m^2 + m + 1)
;;
;; and in general:
;;
;; A_k = Am^k - p(m^(k-1) + m^(k-2) + ... + m + 1)
;;
;; Now, suppose:
;;
;; S_k = m^(k-1) + m^(k-2) + ... + m + 1
;;
;; then:
;;
;; mS_k = m^k + m^(k-1) + m^(k-2) + ... + m^2 + m
;;
;; and:
;;
;; mS_k - S_k = m^k - 1
;;
;; so:
;;
;; S_k = (m^k - 1)/(m - 1)
;;
;; Substituting this back in, we have:
;;
;; A_k = Am^k - p S_k = p(m^k - 1)/(m - 1)
;;
;; We must solve for p such that A_n = 0:
;;
;; Am^n - p(m^n - 1)/(m-1) = 0
;;
;; p = Am^n(m-1)/(m^n - 1)
;;

(defn monthly-repayment
  "Compute the monthly repayment needed to clear the amount owed in
  months-remaining equal payments."
  [amount-owed apr months-remaining]
  (let [m  (apr->monthly apr)]
    (if (<= months-remaining 1)
      (* amount-owed m)
      (let [mn (Math/pow m months-remaining)]
	(/ (* amount-owed mn (- m 1)) (- mn 1))))))

(defn- lazy-seq-simulate-mortgage
  "Helper function that returns a lazy sequence of

  [month number, repayment, repayments to date, loan outstanding]

  for the lifetime of the mortgage."
  [loan-outstanding month-number repayments-to-date apr-fn term-months]
  (when (< month-number term-months)
    (let [apr                (apr-fn month-number)
	  repayment          (monthly-repayment loan-outstanding
						apr
						(- term-months month-number))
	  loan-outstanding   (- (* loan-outstanding (apr->monthly apr)) repayment)
	  repayments-to-date (+ repayments-to-date repayment)]
      (lazy-seq (cons [month-number repayment repayments-to-date loan-outstanding]
		      (lazy-seq-simulate-mortgage loan-outstanding
						  (inc month-number)
						  repayments-to-date
						  apr-fn
						  term-months))))))

(defn simulate-mortgage
  "Simulate a mortgage over the lifetime of its term. Returns a lazy sequence
  of

  [month number, monthly payment amount, total repayments to date, loan amount outstanding]

  for each month of the term.

  The arrangement fee is not included in this calculation.  To compute
  the total cost when the arrangement fee is added to the mortgage,
  simply add it to the loan-amount passed into this function. If it is
  paid up-front, add it to the total payments to date.

  The apr passed in may be a function, in which case it is called with the single
  argument (the current month number). This allows for simulations of variable
  interest rates."
  
  [loan-amount term-years apr]
  (let [apr-fn      (if (fn? apr) apr (constantly apr))
	term-months (* 12 term-years)]
    (lazy-seq-simulate-mortgage loan-amount 0 0 apr-fn term-months)))

(defn simulate-mortgage-with-fee
  "A fee-aware version of simulate-mortgage."
  [loan-amount term-years apr fee add-fee-to-loan?]
  (if add-fee-to-loan?
    (simulate-mortgage (+ loan-amount fee) term-years apr)
    (map (fn [[a b c]] [a (+ b fee) c]) (simulate-mortgage loan-amount term-years apr))))

(defn yearly-report
  "Produce summary of the repayments to date, loan outstanding, and cost to date
  for each year of the mortgage term."
  [loan-amount term-years apr]
  (letfn [(report-line
	   [[month-number _ repayments-to-date loan-outstanding]]
	   (let [cost-to-date (- repayments-to-date (- loan-amount loan-outstanding))
		 year-number  (/ (inc month-number) 12)]
	     (format "%3d %10.2f %10.2f %10.2f" year-number loan-outstanding repayments-to-date cost-to-date)))]
    (doseq [s (filter #(zero? (rem (inc (first %)) 12))
		      (simulate-mortgage loan-amount term-years apr))]
	    (println (report-line s)))))

(defn tracker
  "Simulate a tracker rate that increases by quarterly-incremenet each
  quarter and peaks at cap."
  [initial-rate quarterely-increment cap]
  (fn [month]
    (let [quarter (int (/ month 3))
	  apr     (+ initial-rate (* quarter quarterely-increment))]
      (min apr cap))))

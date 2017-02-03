Data Analytics and Visualization using R

<p>HW1. </p>
<p>1. Implement a function that computes the log of the factorial value of an integer using a for loop. Note that implementing it using log(A)+log(B)+ · · · avoids overflow while implementing it as log(A · B · · · · ) creates an overflow early on. </p>
<p>3. Implement a function that computes the log of the factorial value of an integer using recursion. </p>
<p>4. Using your two implementations of log-factorial in (2) and (3) above, com- pute the sum of the log-factorials of the integers 1, 2, . . . , N for various N values. </p>
<p>5. Compare the execution times of your two implementations for (4) with an implementation based on the o cial R function lfactorial(n). You may use the function system.time() to measure execution time. What are the growth rates of the three implementations as N increases? Use the command options(expressions=500000) to increase the number of nested recursions allowed. Compare the timing of the recursion implemen- tation as much as possible, and continue beyond that for the other two implementations. </p>

package gmrowe.projecteuler.util


object ArrayHelper {

   def binarySearch[T](arr: Array[T], 
                       eql: T => Boolean,
                       lt:  T => Boolean): Option[Int] = {
   
      def bs(low: Int, hi: Int): Option[Int] = {
         if (low > hi) {
            None
         }
         
         val mid = ((hi - low) / 2) + low
         val curr = arr(mid)
         if (eql(curr)) {
            Some(mid)
         } else if (lt(curr)) {
            bs(low, mid - 1)
         } else {
            bs(mid + 1, hi)
         }
      }
      
      bs(0, arr.length - 1)
   }
      
      
}
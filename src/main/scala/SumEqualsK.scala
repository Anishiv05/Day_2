object SumEqualsK {
  def findCombinations(nums: Array[Int], target: Int): List[List[Int]] = {
    def backtrack(index: Int, target: Int, current: List[Int], result: List[List[Int]]): List[List[Int]] = {
      if (target == 0) {
        current.reverse :: result
      } else if (target < 0 || index >= nums.length) {
        result
      } else {
        val take = backtrack(index + 1, target - nums(index), nums(index) :: current, result)
        val notTake = backtrack(index + 1, target, current, take)
        notTake
      }
    }

    backtrack(0, target, List(), List()).distinct // Remove duplicates
  }

  def main(args: Array[String]): Unit = {
    val nums = Array(1, 2, 3, 4, 5)
    val target = 8
    val combinations = findCombinations(nums, target)
    combinations.foreach(println)
  }
}
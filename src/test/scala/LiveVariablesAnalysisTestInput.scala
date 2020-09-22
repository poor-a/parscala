object LiveVariablesAnalysisTestInput {
  def f = {
    var x = 5
    var y = 1
    var w = 2
    var z = 0
    var a = 4
    while (x > 0) {
      y = y * x
      x = x - 1
      w = 1
    }
    if (y > 1)
      a = a + 2
  }
}

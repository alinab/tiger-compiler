structure Main =
struct
  fun semant fname = Semant.transProg(Parse.parse fname)
end

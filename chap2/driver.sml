structure Parse =
struct
  fun parse filename =
      let val file = TextIO.openIn filename
	  fun get _ = TextIO.input file
	  val lexer = Mlex.makeLexer get
	  fun run_lexer() =
	      let val t = lexer()
	       in print t;
           print "\n";
		   if substring(t,0,3)="EOF"
           then ()
           else run_lexer()
	      end
       in run_lexer();
	  TextIO.closeIn file
      end

end


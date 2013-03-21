
(*		
  Sample Mathematica source code file to test `math-mode'
  *)

ClearAll[mxAnd, mxOr, mxInterval, mxMakeInterval, mxIntervalPlot, mxIntervalSymbol];

mxMakeInterval::badarg = "The expression `` is not a relational operator (Inequality|Less|LessEqual|GreaterEqual|Greater) nor a logical Or of relational operators.";

(*
  Less[x,a] and LessEqual[x,a]
  x <  a
  x <= a 
  *) 
mxMakeInterval[(rop:(Less|LessEqual))[var_, value_]] :=
    mxInterval[LessEqual, -\[Infinity], value, rop];

(*** 
     Less[a,x,b] and LessEqual[a,x,b]
     a <  x <  b    
     a <= x <= b    
     ***)
mxMakeInterval[(rop:(Less|LessEqual))[leftValue_, var_, rightValue_]] :=
    mxInterval[rop, leftValue, rightValue, rop];

(***
   Greater[x,a] and GreaterEqual[x,a]
   a >  x
   a >= x    
 ***)
mxMakeInterval[(rop:(Greater|GreaterEqual))[var_, value_]] :=
    mxInterval[rop, value, \[Infinity], GreaterEqual];

(***
   Greater[a,x,b] and GreaterEqual[a,x,b]
   a >  x >  b 
   a >= x >= b   
 ***)
mxMakeInterval[(rop:(Greater|GreaterEqual))[leftValue_, var_, rightValue_]] :=
    mxInterval[rop, leftValue, rightValue, rop];

(***
   Inequality[a, Less|LessEqual, x, Less|LessEqual, b]
 ***)
mxMakeInterval[HoldPattern[Inequality[valueLeft_, ropLeft_, var_, ropRight_, valueRight_]]] :=
    mxInterval[ropLeft, valueLeft, valueRight, ropRight];

(***
    Or[... (relational expressions for one of the above mxMakeInterval functions)]
    ***)
mxMakeInterval[HoldPattern[Or[x: (_Less|_LessEqual|_GreaterEqual|_Greater|_Inequality) ..]]] := 
    mxOr @@ Replace[HoldComplete[x], elem_ :> mxMakeInterval[elem], {1}];

(***
    And[... (relational expressions for one of the above mxMakeInterval functions)]
    ***)
mxMakeInterval[HoldPattern[And[x: (_Less|_LessEqual|_GreaterEqual|_Greater|_Inequality) ..]]] := 
    mxAnd @@ Replace[HoldComplete[x], elem_ :> mxMakeInterval[elem], {1}];

(***
    Catch all for errors.
    ***)
(*mxMakeInterval[expr_] := (Message[mxMakeInterval::badarg, expr]; Return $fail;)*)
    
    (*** 
     
	 Returns interval notation bracket for the given relational operation
	 (Less|LessEqual|GreaterEqual|Greater) and side of the interval (l or
	 r).
	 
	 ***)
    mxIntervalSymbol[op_, side_] := Switch[
					   op, 
					   Less|Greater, Switch[side, l, "[", r, "]"], 
					   LessEqual|GreaterEqual, Switch[side, l, "(", r, ")"]];

(***
    Format mxInterval in interval notation, e.g. [a,b).
    ***)
mxInterval /: MakeBoxes[mxInterval[ropLeft : (Less|LessEqual|Greater|GreaterEqual),
				   valueLeft_, valueRight_,
				   ropRight : (Less|LessEqual|Greater|GreaterEqual)], form_] :=
	RowBox[{mxIntervalSymbol[ropLeft, l], MakeBoxes[valueLeft, form], ",", 
		    MakeBoxes[valueRight, form], mxIntervalSymbol[ropRight, r]}];

(*** 
    Format disjunction of mxIntervals using union infix operator.
    This is another line.
    And another.
    I need to get newline to do an automatic indent.       
    ***)

mxOr /: MakeBoxes[HoldPattern[mxOr[terms : _mxInterval ..]], form_] :=
	    RowBox[Riffle[List @@ Replace[HoldComplete[terms], 
					  elem_ :> Parenthesize[elem, form, Or, True], 
					  {1}], 
			  "\[Union]"]];

(***
    Format conjunction of mxIntervals using intersection infix operator.
    ***)
mxAnd /: MakeBoxes[HoldPattern[mxAnd[terms : _mxInterval ..]], form_] :=
		RowBox[Riffle[List @@ Replace[HoldComplete[terms], 
					      elem_ :> Parenthesize[elem, form, Or, True], 
					      {1}], 
			      "\[Intersection]"]];

(***
    Graph the intervals on a number line.
    ***)

Options[mxIntervalPlot] = {
    Stack -> False,
    Colors -> { Blue, Red, Green }
};

mxIntervalPlot[term_mxInterval] := mxIntervalPlot[mxOr[term]];

mxIntervalPlot[mxOr[term__mxInterval]] := 
    Module[
	   {
	       terms = {term},
		   radius = 0.2, (* The radius of the circle/disk markers *)
		       offset = 0.5,  (* The offset of the markers above the axis *)
		       min = -10.0, (* The minimum number on the extreme left of the number line *)
		       max = 10.0 (* The maximum number of the exterme right of the number line *)
		       },
	   {
	       range = {Min@#, Max@#} & /@ {  
		   Select[Flatten[
				  terms /. mxInterval[lop_, lv_, rv_, rop_] :> {lv, rv}],
			  -\[Infinity] < # < \[Infinity] &]
		       };
	       range = range /. { -\[Infinity] :> 5, \[Infinity] :> -5 };
	       (*Print[range];*)
	       
	       (* Return the end point graphic for the given operation. *)
	       endPoint[op_, px_] :=
		   Switch[op, Less | Greater, Circle, Equal | LessEqual | GreaterEqual, Disk] @@
		       List[{px, offset}, radius];
		   
		   (* The segment is from -inf to +inf. *)
		   segment[opLeft_, -\[Infinity], \[Infinity], opRight_] :=
		   { 
		       Blue, 
			   Arrowheads[{-Medium, Medium}], Arrow[{ {min, offset}, {max, offset} }] 
			   };
		   
		   (* The segment is from -inf to a. *)
		   segment[opLeft_, -\[Infinity], valueRight_, opRight_] :=
		   { 
		       Blue,
			   endPoint[opRight, valueRight], 
			   Arrow[{ {valueRight - radius, offset}, {min, offset} }] 
			   };
		   
		   (* The segment is from a to +inf. *)
		   segment[opLeft_, valueLeft_, \[Infinity], opRight_] :=
		   { 
		       Blue,
			   endPoint[opLeft, valueLeft], 
			   Arrow[{ {valueLeft + radius, offset}, {max, offset} }]
			   };
		   
		   (* The segment is from a to b. *)
		   segment[opLeft_, valueLeft_, valueRight_, opRight_] :=
		   { 
		       Blue,
			   endPoint[opLeft, valueLeft], 
			   endPoint[opRight, valueRight],
			   Line[{ {valueLeft + radius, offset}, {valueRight - radius, offset} }]
			   };
		   
		   (***
		       Print[terms];
		       Print[# /. mxInterval[opLeft_, valueLeft_, valueRight_, opRight_] :> 
		       segment[opLeft, valueLeft, valueRight, opRight]
		       & /@ {terms}];
		       ***)
		   
		   Show[{
			   Plot[0, {x, -10, 10}, Axes -> {True, False}],
			       Graphics[# /. mxInterval[opLeft_, valueLeft_, valueRight_, opRight_] :> 
					segment[opLeft, valueLeft, valueRight, opRight]
					& /@ {terms}
					] (*Close Graphics*)
			       },
		       AspectRatio -> Automatic
		       ] (*Close Show*)
		       }] (*Close Module*)

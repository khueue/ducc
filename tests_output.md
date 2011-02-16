# Testruns


### ducc -t suite/incorrect/lexer/bad.c gives:

	suite/incorrect/lexer/bad.c:16: lexical error, user: unterminated comment



### ducc -t suite/incorrect/lexer/good.c gives:

	suite/incorrect/lexer/good.c:14: syntax error: '{'



### ducc -t suite/incorrect/lexer/long-char.c gives:

	suite/incorrect/lexer/long-char.c:4: lexical error, illegal: 'cc



### ducc -t suite/incorrect/lexer/ugly.c gives:

	suite/incorrect/lexer/ugly.c:1: lexical error, illegal: |+



### ducc -t suite/incorrect/parser/pe01.c gives:

	suite/incorrect/parser/pe01.c:5: syntax error: ')'



### ducc -t suite/incorrect/parser/pe02.c gives:

	suite/incorrect/parser/pe02.c:4: syntax error: '}'



### ducc -t suite/incorrect/parser/pe03.c gives:

	suite/incorrect/parser/pe03.c:6: syntax error: '}'



### ducc -t suite/incorrect/parser/pe04.c gives:

	suite/incorrect/parser/pe04.c:5: syntax error: "a"



### ducc -t suite/incorrect/parser/pe05.c gives:

	suite/incorrect/parser/pe05.c:3: syntax error: else



### ducc -t suite/incorrect/parser/pe06.c gives:

	suite/incorrect/parser/pe06.c:3: syntax error: "b"



### ducc -t suite/incorrect/parser/pe07.c gives:

	suite/incorrect/parser/pe07.c:3: syntax error: ','



### ducc -t suite/incorrect/parser/pe08.c gives:

	suite/incorrect/parser/pe08.c:3: syntax error: 42



### ducc -t suite/incorrect/parser/pe09.c gives:

	suite/incorrect/parser/pe09.c:3: syntax error: ';'



### ducc -t suite/incorrect/parser/pe10.c gives:

	suite/incorrect/parser/pe10.c:8: syntax error: ')'



### ducc -t suite/incorrect/parser/pe11.c gives:

	suite/incorrect/parser/pe11.c:3: syntax error: "foo"



### ducc -t suite/incorrect/parser/pe12.c gives:

	suite/incorrect/parser/pe12.c:3: syntax error: '{'



### ducc -t suite/incorrect/parser/pe13.c gives:

	suite/incorrect/parser/pe13.c:4: syntax error: ')'



### ducc -t suite/incorrect/parser/pe14.c gives:

	suite/incorrect/parser/pe14.c:4: syntax error: '('



### ducc -t suite/incorrect/semantic/se01.c gives:

	suite/incorrect/semantic/se01.c:5: semantic error, 'b' is undeclared



### ducc -t suite/incorrect/semantic/se02.c gives:

	suite/incorrect/semantic/se02.c:5: semantic error, 'foo' is undeclared



### ducc -t suite/incorrect/semantic/se03.c gives:

	suite/incorrect/semantic/se03.c:3: semantic error, 'output' is undeclared



### ducc -t suite/incorrect/semantic/se04.c gives:

	suite/incorrect/semantic/se04.c:5: semantic error, already defined



### ducc -t suite/incorrect/semantic/se05.c gives:

	suite/incorrect/semantic/se05.c:5: semantic error, function 'a' already defined



### ducc -t suite/incorrect/semantic/se06.c gives:

	suite/incorrect/semantic/se06.c:7: semantic error, function 'a' already defined



### ducc -t suite/incorrect/semantic/se07.c gives:

	suite/incorrect/semantic/se07.c:4: semantic error, inconvertible types



### ducc -t suite/incorrect/semantic/se08.c gives:

	suite/incorrect/semantic/se08.c:4: semantic error, inconvertible types



### ducc -t suite/incorrect/semantic/se09.c gives:

	suite/incorrect/semantic/se09.c:6: semantic error, inconvertible types



### ducc -t suite/incorrect/semantic/se10.c gives:

	suite/incorrect/semantic/se10.c:6: semantic error, 'n' is not an array



### ducc -t suite/incorrect/semantic/se11.c gives:

	suite/incorrect/semantic/se11.c:4: semantic error, not an l-value



### ducc -t suite/incorrect/semantic/se12.c gives:

	suite/incorrect/semantic/se12.c:6: semantic error, 'a' is not a function



### ducc -t suite/incorrect/semantic/se13.c gives:

	suite/incorrect/semantic/se13.c:8: semantic error, incompatible types



### ducc -t suite/incorrect/semantic/se14.c gives:

	suite/incorrect/semantic/se14.c:12: semantic error, 'f' is not a function



### ducc -t suite/incorrect/semantic/se15.c gives:

	suite/incorrect/semantic/se15.c:8: semantic error, wrong number of arguments



### ducc -t suite/incorrect/semantic/se16.c gives:

	suite/incorrect/semantic/se16.c:9: semantic error, wrong number of arguments



### ducc -t suite/incorrect/semantic/se17.c gives:

	suite/incorrect/semantic/se17.c:6: semantic error, incompatible types



### ducc -t suite/incorrect/semantic/se18.c gives:

	suite/incorrect/semantic/se18.c:6: semantic error, not an l-value



### ducc -t suite/incorrect/semantic/se19.c gives:

	suite/incorrect/semantic/se19.c:5: semantic error, incompatible types



### ducc -t suite/incorrect/semantic/se20.c gives:

	suite/incorrect/semantic/se20.c:7: semantic error, not an l-value



### ducc -t suite/incorrect/semantic/se21.c gives:

	suite/incorrect/semantic/se21.c:5: semantic error, inconvertible types



### ducc -t suite/incorrect/semantic/se22.c gives:

	suite/incorrect/semantic/se22.c:6: semantic error, incompatible types



### ducc -t suite/incorrect/semantic/se23.c gives:

	suite/incorrect/semantic/se23.c:6: semantic error, 'b' is not an array



### ducc -t suite/incorrect/semantic/se24.c gives:

	suite/incorrect/semantic/se24.c:6: semantic error, not an l-value



### ducc -t suite/incorrect/semantic/se25.c gives:

	suite/incorrect/semantic/se25.c:4: syntax error: '='



### ducc -t suite/incorrect/semantic/se26.c gives:

	suite/incorrect/semantic/se26.c:9: semantic error, inconvertible types



### ducc -t suite/incorrect/semantic/se27.c gives:

	suite/incorrect/semantic/se27.c:4: semantic error, inconvertible types



### ducc -t suite/incorrect/semantic/se28.c gives:

	suite/incorrect/semantic/se28.c:5: semantic error, inconvertible types



### ducc -t suite/incorrect/semantic/se29.c gives:

	suite/incorrect/semantic/se29.c:4: semantic error, already defined



### ducc -t suite/incorrect/semantic/se30.c gives:

	suite/incorrect/semantic/se30.c:6: semantic error, inconvertible types



### ducc -t suite/incorrect/semantic/se31.c gives:

	suite/incorrect/semantic/se31.c:5: semantic error, function 'a' already defined



### ducc -t suite/incorrect/semantic/se32.c gives:

	suite/incorrect/semantic/se32.c:6: semantic error, incompatible types



### ducc -t suite/incorrect/semantic/se33.c gives:

	suite/incorrect/semantic/se33.c:6: semantic error, wrong number of arguments



### ducc -t suite/incorrect/semantic/se34.c gives:

	suite/incorrect/semantic/se34.c:6: semantic error, wrong number of arguments



### ducc -t suite/noisy/advanced/8queens.c gives:

	{'EXIT',{badarg,[{translator_helpers,get_meta,1},
	                 {translator_helpers,get_tag,1},
	                 {translator,translate_stmt,2},
	                 {translator,translate_if,2},
	                 {translator,translate_list,3},
	                 {translator,translate_list,3},
	                 {translator,translate_while,2},
	                 {translator,translate_list,3}]}}



### ducc -t suite/noisy/advanced/bubble.c gives:

	{'EXIT',{{case_clause,farray},
	         [{translator,translate_ident,2},
	          {translator,translate_actuals,2},
	          {translator,translate_funcall,2},
	          {translator,translate_list,3},
	          {translator,translate_fundef,2},
	          {translator,translate_topdecs,2},
	          {translator,translate_topdecs,2},
	          {translator_driver,translate,2}]}}



### ducc -t suite/noisy/advanced/eval.c gives:

	{'EXIT',{badarg,[{translator_helpers,get_meta,1},
	                 {translator_helpers,get_tag,1},
	                 {translator,translate_stmt,2},
	                 {translator,translate_if,2},
	                 {translator,translate_list,3},
	                 {translator,translate_list,3},
	                 {translator,translate_if,2},
	                 {translator,translate_list,3}]}}



### ducc -t suite/noisy/advanced/primes.c gives:

	{'EXIT',{badarg,[{translator_helpers,get_meta,1},
	                 {translator_helpers,get_tag,1},
	                 {translator,translate_stmt,2},
	                 {translator,translate_if,2},
	                 {translator,translate_list,3},
	                 {translator,translate_while,2},
	                 {translator,translate_list,3},
	                 {translator,translate_list,3}]}}



### ducc -t suite/noisy/advanced/quick.c gives:

	{'EXIT',{badarg,[{translator_helpers,get_meta,1},
	                 {translator_helpers,get_tag,1},
	                 {translator,translate_stmt,2},
	                 {translator,translate_if,2},
	                 {translator,translate_list,3},
	                 {translator,translate_list,3},
	                 {translator,translate_while,2},
	                 {translator,translate_list,3}]}}



### ducc -t suite/noisy/medium/circle.c gives:

	[{proc,{label,"drawpos"},
	       [{temp,2}],
	       [{temp,2},
	        {temp,3},
	        {temp,4},
	        {temp,5},
	        {temp,6},
	        {temp,7},
	        {temp,8},
	        {temp,9},
	        {temp,10},
	        {temp,11},
	        {temp,12},
	        {temp,13},
	        {temp,14}],
	       2,
	       [{cjump,eq,{temp,2},0,{label,101}},
	        {eval,{temp,3},{icon,0}},
	        {arrelem},
	        {eval,{temp,4},{icon,'#'}},
	        {eval,{temp,5},{binop,'=',{temp,3},{temp,4}}},
	        {jump,{label,102}},
	        {labdef,{label,101}},
	        {eval,{temp,6},{icon,0}},
	        {arrelem},
	        {eval,{temp,7},{icon,' '}},
	        {eval,{temp,8},{binop,'=',{temp,6},{temp,7}}},
	        {labdef,{label,102}},
	        {eval,{temp,9},{icon,1}},
	        {arrelem},
	        {eval,{temp,10},{icon,0}},
	        {eval,{temp,11},{binop,'=',{temp,9},{temp,10}}},
	        {eval,{temp,12},{icon,0}},
	        {eval,{temp,13},{binop,'+',{temp,1},{temp,12}}},
	        {call,{temp,14},{label,"putstring"},[{temp,13}]}],
	       {labdef,{label,100}}},
	 {proc,{label,"nl"},
	       [],
	       [{temp,15},
	        {temp,16},
	        {temp,17},
	        {temp,18},
	        {temp,19},
	        {temp,20},
	        {temp,21},
	        {temp,22},
	        {temp,23}],
	       4,
	       [{eval,{temp,15},{icon,0}},
	        {arrelem},
	        {eval,{temp,16},{icon,'\\n'}},
	        {eval,{temp,17},{binop,'=',{temp,15},{temp,16}}},
	        {eval,{temp,18},{icon,1}},
	        {arrelem},
	        {eval,{temp,19},{icon,0}},
	        {eval,{temp,20},{binop,'=',{temp,18},{temp,19}}},
	        {eval,{temp,21},{icon,2}},
	        {eval,{temp,22},{binop,'+',{temp,1},{temp,21}}},
	        {call,{temp,23},{label,"putstring"},[{temp,22}]}],
	       {labdef,{label,103}}},
	 {proc,{label,"main"},
	       [],
	       [{temp,24},
	        {temp,25},
	        {temp,26},
	        {temp,27},
	        {temp,28},
	        {temp,29},
	        {temp,30},
	        {temp,31},
	        {temp,32},
	        {temp,33},
	        {temp,34},
	        {temp,35},
	        {temp,36},
	        {temp,37},
	        {temp,38},
	        {temp,39},
	        {temp,40},
	        {temp,41},
	        {temp,42},
	        {temp,43},
	        {temp,44},
	        {temp,45},
	        {temp,46},
	        {temp,47},
	        {temp,48},
	        {temp,49},
	        {temp,50},
	        {temp,51},
	        {temp,52},
	        {temp,53},
	        {temp,54},
	        {temp,55},
	        {temp,56}],
	       4,
	       [{eval,{temp,26},{icon,9}},
	        {unop},
	        {eval,{temp,27},{binop,'=',{temp,24},{temp,26}}},
	        {jump,{label,105}},
	        {labdef,{label,106}},
	        {eval,{temp,30},{icon,20}},
	        {unop},
	        {eval,{temp,31},{binop,'=',{temp,25},{temp,30}}},
	        {call,{temp,32},{label,"nl"},[]},
	        {jump,{label,108}},
	        {labdef,{label,109}},
	        {eval,{temp,35},{binop,'*',{temp,24},{temp,24}}},
	        {eval,{temp,36},{icon,22}},
	        {eval,{temp,37},{binop,'*',{temp,35},{temp,36}}},
	        {eval,{temp,38},{icon,22}},
	        {eval,{temp,39},{binop,'*',{temp,37},{temp,38}}},
	        {eval,{temp,40},{icon,10}},
	        {eval,{temp,41},{icon,10}},
	        {eval,{temp,42},{binop,'*',{temp,40},{temp,41}}},
	        {eval,{temp,43},{binop,'/',{temp,39},{temp,42}}},
	        {eval,{temp,44},{binop,'*',{temp,25},{temp,25}}},
	        {eval,{temp,45},{binop,'+',{temp,43},{temp,44}}},
	        {eval,{temp,46},{icon,380}},
	        {eval,{temp,47},{binop,'>',{temp,45},{temp,46}}},
	        {call,{temp,48},{label,"drawpos"},[{temp,47}]},
	        {eval,{temp,49},{icon,1}},
	        {eval,{temp,50},{binop,'+',{temp,25},{temp,49}}},
	        {eval,{temp,51},{binop,'=',{temp,25},{temp,50}}},
	        {labdef,{label,108}},
	        {eval,{temp,33},{icon,20}},
	        {eval,{temp,34},{binop,'<=',{temp,25},{temp,33}}},
	        {cjump,neq,{temp,34},0,{label,109}},
	        {labdef,{label,110}},
	        {eval,{temp,52},{icon,1}},
	        {eval,{temp,53},{binop,'+',{temp,24},{temp,52}}},
	        {eval,{temp,54},{binop,'=',{temp,24},{temp,53}}},
	        {labdef,{label,105}},
	        {eval,{temp,28},{icon,9}},
	        {eval,{temp,29},{binop,'<=',{temp,24},{temp,28}}},
	        {cjump,neq,{temp,29},0,{label,106}},
	        {labdef,{label,107}},
	        {call,{temp,55},{label,"nl"},[]},
	        {call,{temp,56},{label,"nl"},[]}],
	       {labdef,{label,104}}}].



### ducc -t suite/noisy/medium/fac-b.c gives:

	[{proc,{label,"fac"},
	       [{temp,2}],
	       [{temp,2},
	        {temp,3},
	        {temp,4},
	        {temp,5},
	        {temp,6},
	        {temp,7},
	        {temp,8},
	        {temp,9}],
	       0,
	       [{eval,{temp,3},{icon,0}},
	        {eval,{temp,4},{binop,'==',{temp,2},{temp,3}}},
	        {cjump,eq,{temp,4},0,{label,101}},
	        {eval,{temp,5},{icon,1}},
	        {jump,{label,100}},
	        {jump,{label,102}},
	        {labdef,{label,101}},
	        {eval,{temp,6},{icon,1}},
	        {eval,{temp,7},{binop,'-',{temp,2},{temp,6}}},
	        {call,{temp,8},{label,"fac"},[{temp,7}]},
	        {eval,{temp,9},{binop,'*',{temp,2},{temp,8}}},
	        {jump,{label,100}},
	        {labdef,{label,102}}],
	       {labdef,{label,100}}},
	 [],
	 {proc,{label,"main"},
	       [],
	       [{temp,11},
	        {temp,12},
	        {temp,13},
	        {temp,14},
	        {temp,15},
	        {temp,16},
	        {temp,17},
	        {temp,18},
	        {temp,19},
	        {temp,20},
	        {temp,21},
	        {temp,22},
	        {temp,23},
	        {temp,24},
	        {temp,25},
	        {temp,26},
	        {temp,27},
	        {temp,28},
	        {temp,29},
	        {temp,30},
	        {temp,31},
	        {temp,32},
	        {temp,33},
	        {temp,34},
	        {temp,35},
	        {temp,36},
	        {temp,37},
	        {temp,38},
	        {temp,39}],
	       4,
	       [{eval,{temp,12},{icon,0}},
	        {arrelem},
	        {eval,{temp,13},{icon,' '}},
	        {eval,{temp,14},{binop,'=',{temp,12},{temp,13}}},
	        {eval,{temp,15},{icon,1}},
	        {arrelem},
	        {eval,{temp,16},{icon,0}},
	        {eval,{temp,17},{binop,'=',{temp,15},{temp,16}}},
	        {eval,{temp,18},{icon,0}},
	        {arrelem},
	        {eval,{temp,19},{icon,'\\n'}},
	        {eval,{temp,20},{binop,'=',{temp,18},{temp,19}}},
	        {eval,{temp,21},{icon,1}},
	        {arrelem},
	        {eval,{temp,22},{icon,0}},
	        {eval,{temp,23},{binop,'=',{temp,21},{temp,22}}},
	        {eval,{temp,24},{icon,0}},
	        {eval,{temp,25},{binop,'=',{temp,11},{temp,24}}},
	        {jump,{label,104}},
	        {labdef,{label,105}},
	        {call,{temp,28},{label,"putint"},[{temp,11}]},
	        {eval,{temp,29},{icon,0}},
	        {eval,{temp,30},{binop,'+',{temp,1},{temp,29}}},
	        {call,{temp,31},{label,"putstring"},[{temp,30}]},
	        {call,{temp,32},{label,"fac"},[{temp,11}]},
	        {call,{temp,33},{label,"putint"},[{temp,32}]},
	        {eval,{temp,34},{icon,2}},
	        {eval,{temp,35},{binop,'+',{temp,1},{temp,34}}},
	        {call,{temp,36},{label,"putstring"},[{temp,35}]},
	        {eval,{temp,37},{icon,1}},
	        {eval,{temp,38},{binop,'+',{temp,11},{temp,37}}},
	        {eval,{temp,39},{binop,'=',{temp,11},{temp,38}}},
	        {labdef,{label,104}},
	        {eval,{temp,26},{icon,10}},
	        {eval,{temp,27},{binop,'<=',{temp,11},{temp,26}}},
	        {cjump,neq,{temp,27},0,{label,105}},
	        {labdef,{label,106}}],
	       {labdef,{label,103}}}].



### ducc -t suite/noisy/medium/fac.c gives:

	[{proc,{label,"fac"},
	       [{temp,2}],
	       [{temp,2},
	        {temp,3},
	        {temp,4},
	        {temp,5},
	        {temp,6},
	        {temp,7},
	        {temp,8},
	        {temp,9}],
	       0,
	       [{eval,{temp,3},{icon,0}},
	        {eval,{temp,4},{binop,'==',{temp,2},{temp,3}}},
	        {cjump,eq,{temp,4},0,{label,101}},
	        {eval,{temp,5},{icon,1}},
	        {jump,{label,100}},
	        {jump,{label,102}},
	        {labdef,{label,101}},
	        {eval,{temp,6},{icon,1}},
	        {eval,{temp,7},{binop,'-',{temp,2},{temp,6}}},
	        {call,{temp,8},{label,"fac"},[{temp,7}]},
	        {eval,{temp,9},{binop,'*',{temp,2},{temp,8}}},
	        {jump,{label,100}},
	        {labdef,{label,102}}],
	       {labdef,{label,100}}},
	 [],
	 {proc,{label,"main"},
	       [],
	       [{temp,10},{temp,11},{temp,12},{temp,13},{temp,14}],
	       0,
	       [{call,{temp,11},{label,"getint"},[]},
	        {eval,{temp,12},{binop,'=',{temp,10},{temp,11}}},
	        {call,{temp,13},{label,"fac"},[{temp,10}]},
	        {call,{temp,14},{label,"putint"},[{temp,13}]}],
	       {labdef,{label,103}}}].



### ducc -t suite/noisy/medium/fib.c gives:

	{'EXIT',{badarg,[{translator_helpers,get_meta,1},
	                 {translator_helpers,get_tag,1},
	                 {translator,translate_stmt,2},
	                 {translator,translate_if,2},
	                 {translator,translate_list,3},
	                 {translator,translate_fundef,2},
	                 {translator,translate_topdecs,2},
	                 {translator_driver,translate,2}]}}



### ducc -t suite/noisy/simple/sim01.c gives:

	[{proc,{label,"main"},
	       [],
	       [{temp,2},{temp,3}],
	       0,
	       [{eval,{temp,2},{icon,42}},{call,{temp,3},{label,"putint"},[{temp,2}]}],
	       {labdef,{label,100}}}].



### ducc -t suite/noisy/simple/sim02.c gives:

	[{data,{label,100},long},
	 {proc,{label,"main"},
	       [],
	       [{temp,2},
	        {temp,3},
	        {temp,4},
	        {temp,5},
	        {temp,6},
	        {temp,7},
	        {temp,8},
	        {temp,9},
	        {temp,10},
	        {temp,11},
	        {temp,12}],
	       0,
	       [{eval,{temp,3},{labref,{label,100}}},
	        {eval,{temp,4},{load,long,{temp,3}}},
	        {eval,{temp,5},{icon,76}},
	        {eval,{temp,6},{binop,'=',{temp,4},{temp,5}}},
	        {eval,{temp,7},{icon,54321}},
	        {eval,{temp,8},{binop,'=',{temp,2},{temp,7}}},
	        {eval,{temp,9},{labref,{label,100}}},
	        {eval,{temp,10},{load,long,{temp,9}}},
	        {call,{temp,11},{label,"putint"},[{temp,10}]},
	        {call,{temp,12},{label,"putint"},[{temp,2}]}],
	       {labdef,{label,101}}}].



### ducc -t suite/noisy/simple/sim03.c gives:

	[{data,{label,100},40},
	 {proc,{label,"main"},
	       [],
	       [{temp,2},
	        {temp,3},
	        {temp,4},
	        {temp,5},
	        {temp,6},
	        {temp,7},
	        {temp,8},
	        {temp,9},
	        {temp,10},
	        {temp,11}],
	       40,
	       [{eval,{temp,2},{icon,7}},
	        {arrelem},
	        {eval,{temp,3},{icon,123}},
	        {eval,{temp,4},{binop,'=',{temp,2},{temp,3}}},
	        {eval,{temp,5},{icon,5}},
	        {arrelem},
	        {eval,{temp,6},{icon,456}},
	        {eval,{temp,7},{binop,'=',{temp,5},{temp,6}}},
	        {eval,{temp,8},{icon,7}},
	        {arrelem},
	        {call,{temp,9},{label,"putint"},[{temp,8}]},
	        {eval,{temp,10},{icon,5}},
	        {arrelem},
	        {call,{temp,11},{label,"putint"},[{temp,10}]}],
	       {labdef,{label,101}}}].



### ducc -t suite/noisy/simple/sim04.c gives:

	[{data,{label,100},7},
	 {proc,{label,"main"},
	       [],
	       [{temp,2},
	        {temp,3},
	        {temp,4},
	        {temp,5},
	        {temp,6},
	        {temp,7},
	        {temp,8},
	        {temp,9},
	        {temp,10},
	        {temp,11},
	        {temp,12},
	        {temp,13},
	        {temp,14},
	        {temp,15},
	        {temp,16},
	        {temp,17},
	        {temp,18},
	        {temp,19},
	        {temp,20},
	        {temp,21},
	        {temp,22},
	        {temp,23},
	        {temp,24},
	        {temp,25},
	        {temp,26},
	        {temp,27},
	        {temp,28},
	        {temp,29},
	        {temp,30},
	        {temp,31},
	        {temp,32},
	        {temp,33},
	        {temp,34},
	        {temp,35},
	        {temp,36},
	        {temp,37},
	        {temp,38},
	        {temp,39},
	        {temp,40},
	        {temp,41},
	        {temp,42},
	        {temp,43},
	        {temp,44},
	        {temp,45},
	        {temp,46},
	        {temp,47},
	        {temp,48},
	        {temp,49},
	        {temp,50},
	        {temp,51},
	        {temp,52},
	        {temp,53},
	        {temp,54},
	        {temp,55},
	        {temp,56},
	        {temp,57}],
	       10,
	       [{eval,{temp,2},{icon,0}},
	        {arrelem},
	        {eval,{temp,3},{icon,'H'}},
	        {eval,{temp,4},{binop,'=',{temp,2},{temp,3}}},
	        {eval,{temp,5},{icon,1}},
	        {arrelem},
	        {eval,{temp,6},{icon,e}},
	        {eval,{temp,7},{binop,'=',{temp,5},{temp,6}}},
	        {eval,{temp,8},{icon,2}},
	        {arrelem},
	        {eval,{temp,9},{icon,l}},
	        {eval,{temp,10},{binop,'=',{temp,8},{temp,9}}},
	        {eval,{temp,11},{icon,3}},
	        {arrelem},
	        {eval,{temp,12},{icon,l}},
	        {eval,{temp,13},{binop,'=',{temp,11},{temp,12}}},
	        {eval,{temp,14},{icon,4}},
	        {arrelem},
	        {eval,{temp,15},{icon,o}},
	        {eval,{temp,16},{binop,'=',{temp,14},{temp,15}}},
	        {eval,{temp,17},{icon,5}},
	        {arrelem},
	        {eval,{temp,18},{icon,'\\n'}},
	        {eval,{temp,19},{binop,'=',{temp,17},{temp,18}}},
	        {eval,{temp,20},{icon,6}},
	        {arrelem},
	        {eval,{temp,21},{icon,0}},
	        {eval,{temp,22},{binop,'=',{temp,20},{temp,21}}},
	        {eval,{temp,23},{icon,0}},
	        {arrelem},
	        {eval,{temp,24},{icon,'G'}},
	        {eval,{temp,25},{binop,'=',{temp,23},{temp,24}}},
	        {eval,{temp,26},{icon,1}},
	        {arrelem},
	        {eval,{temp,27},{icon,o}},
	        {eval,{temp,28},{binop,'=',{temp,26},{temp,27}}},
	        {eval,{temp,29},{icon,2}},
	        {arrelem},
	        {eval,{temp,30},{icon,o}},
	        {eval,{temp,31},{binop,'=',{temp,29},{temp,30}}},
	        {eval,{temp,32},{icon,3}},
	        {arrelem},
	        {eval,{temp,33},{icon,d}},
	        {eval,{temp,34},{binop,'=',{temp,32},{temp,33}}},
	        {eval,{temp,35},{icon,4}},
	        {arrelem},
	        {eval,{temp,36},{icon,' '}},
	        {eval,{temp,37},{binop,'=',{temp,35},{temp,36}}},
	        {eval,{temp,38},{icon,5}},
	        {arrelem},
	        {eval,{temp,39},{icon,b}},
	        {eval,{temp,40},{binop,'=',{temp,38},{temp,39}}},
	        {eval,{temp,41},{icon,6}},
	        {arrelem},
	        {eval,{temp,42},{icon,y}},
	        {eval,{temp,43},{binop,'=',{temp,41},{temp,42}}},
	        {eval,{temp,44},{icon,7}},
	        {arrelem},
	        {eval,{temp,45},{icon,e}},
	        {eval,{temp,46},{binop,'=',{temp,44},{temp,45}}},
	        {eval,{temp,47},{icon,8}},
	        {arrelem},
	        {eval,{temp,48},{icon,'\\n'}},
	        {eval,{temp,49},{binop,'=',{temp,47},{temp,48}}},
	        {eval,{temp,50},{icon,9}},
	        {arrelem},
	        {eval,{temp,51},{icon,0}},
	        {eval,{temp,52},{binop,'=',{temp,50},{temp,51}}},
	        {eval,{temp,53},{labref,{label,100}}},
	        {call,{temp,54},{label,"putstring"},[{temp,53}]},
	        {eval,{temp,55},{icon,0}},
	        {eval,{temp,56},{binop,'+',{temp,1},{temp,55}}},
	        {call,{temp,57},{label,"putstring"},[{temp,56}]}],
	       {labdef,{label,101}}}].



### ducc -t suite/noisy/simple/sim05.c gives:

	[{proc,{label,"main"},
	       [],
	       [{temp,2},
	        {temp,3},
	        {temp,4},
	        {temp,5},
	        {temp,6},
	        {temp,7},
	        {temp,8},
	        {temp,9},
	        {temp,10},
	        {temp,11},
	        {temp,12},
	        {temp,13},
	        {temp,14},
	        {temp,15},
	        {temp,16},
	        {temp,17},
	        {temp,18},
	        {temp,19},
	        {temp,20},
	        {temp,21},
	        {temp,22},
	        {temp,23},
	        {temp,24},
	        {temp,25},
	        {temp,26},
	        {temp,27},
	        {temp,28},
	        {temp,29},
	        {temp,30},
	        {temp,31},
	        {temp,32},
	        {temp,33},
	        {temp,34},
	        {temp,35},
	        {temp,36},
	        {temp,37},
	        {temp,38},
	        {temp,39},
	        {temp,40},
	        {temp,41},
	        {temp,42},
	        {temp,43},
	        {temp,44},
	        {temp,45},
	        {temp,46},
	        {temp,47},
	        {temp,48},
	        {temp,49},
	        {temp,50},
	        {temp,51},
	        {temp,52},
	        {temp,53},
	        {temp,54},
	        {temp,55},
	        {temp,56},
	        {temp,57},
	        {temp,58},
	        {temp,59},
	        {temp,60},
	        {temp,61},
	        {temp,62},
	        {temp,63},
	        {temp,64},
	        {temp,65},
	        {temp,66},
	        {temp,67},
	        {temp,68},
	        {temp,69},
	        {temp,70},
	        {temp,71},
	        {temp,72},
	        {temp,73},
	        {temp,74},
	        {temp,75},
	        {temp,76},
	        {temp,77},
	        {temp,78},
	        {temp,79},
	        {temp,80},
	        {temp,81},
	        {temp,82},
	        {temp,83},
	        {temp,84},
	        {temp,85},
	        {temp,86},
	        {temp,87},
	        {temp,88},
	        {temp,89},
	        {temp,90},
	        {temp,91},
	        {temp,92},
	        {temp,93},
	        {temp,94},
	        {temp,95},
	        {temp,96},
	        {temp,97},
	        {temp,98},
	        {temp,99},
	        {temp,100},
	        {temp,101},
	        {temp,102},
	        {temp,103},
	        {temp,104},
	        {temp,105},
	        {temp,106},
	        {temp,107},
	        {temp,108},
	        {temp,109},
	        {temp,110},
	        {temp,111},
	        {temp,112},
	        {temp,113},
	        {temp,114},
	        {temp,115},
	        {temp,116},
	        {temp,117},
	        {temp,118},
	        {temp,119},
	        {temp,120},
	        {temp,121},
	        {temp,122},
	        {temp,123},
	        {temp,124},
	        {temp,125},
	        {temp,126},
	        {temp,127},
	        {temp,128},
	        {temp,129},
	        {temp,130},
	        {temp,131},
	        {temp,132},
	        {temp,133},
	        {temp,134},
	        {temp,135},
	        {temp,136},
	        {temp,137},
	        {temp,138},
	        {temp,139},
	        {temp,140},
	        {temp,141},
	        {temp,142},
	        {temp,143},
	        {temp,144},
	        {temp,145},
	        {temp,146},
	        {temp,147},
	        {temp,148},
	        {temp,149},
	        {temp,150},
	        {temp,151},
	        {temp,152},
	        {temp,153},
	        {temp,154},
	        {temp,155},
	        {temp,156},
	        {temp,157},
	        {temp,158},
	        {temp,159},
	        {temp,160},
	        {temp,161},
	        {temp,162},
	        {temp,163},
	        {temp,164},
	        {temp,165},
	        {temp,166},
	        {temp,167},
	        {temp,168},
	        {temp,169},
	        {temp,170},
	        {temp,171},
	        {temp,172},
	        {temp,173},
	        {temp,174},
	        {temp,175},
	        {temp,176},
	        {temp,177},
	        {temp,178},
	        {temp,179},
	        {temp,180},
	        {temp,181},
	        {temp,182},
	        {temp,183},
	        {temp,184},
	        {temp,185},
	        {temp,186},
	        {temp,187},
	        {temp,188},
	        {temp,189},
	        {temp,190},
	        {temp,191},
	        {temp,192},
	        {temp,193},
	        {temp,194},
	        {temp,195},
	        {temp,196},
	        {temp,197},
	        {temp,198}],
	       1,
	       [{eval,{temp,4},{icon,0}},
	        {arrelem},
	        {eval,{temp,5},{icon,'\\n'}},
	        {eval,{temp,6},{binop,'=',{temp,4},{temp,5}}},
	        {eval,{temp,7},{icon,1}},
	        {arrelem},
	        {eval,{temp,8},{icon,0}},
	        {eval,{temp,9},{binop,'=',{temp,7},{temp,8}}},
	        {eval,{temp,10},{icon,42}},
	        {call,{temp,11},{label,"putint"},[{temp,10}]},
	        {eval,{temp,12},{icon,0}},
	        {eval,{temp,13},{binop,'+',{temp,1},{temp,12}}},
	        {call,{temp,14},{label,"putstring"},[{temp,13}]},
	        {eval,{temp,15},{icon,35}},
	        {eval,{temp,16},{icon,7}},
	        {eval,{temp,17},{binop,'+',{temp,15},{temp,16}}},
	        {call,{temp,18},{label,"putint"},[{temp,17}]},
	        {eval,{temp,19},{icon,0}},
	        {eval,{temp,20},{binop,'+',{temp,1},{temp,19}}},
	        {call,{temp,21},{label,"putstring"},[{temp,20}]},
	        {eval,{temp,22},{icon,6}},
	        {eval,{temp,23},{icon,7}},
	        {eval,{temp,24},{binop,'*',{temp,22},{temp,23}}},
	        {call,{temp,25},{label,"putint"},[{temp,24}]},
	        {eval,{temp,26},{icon,0}},
	        {eval,{temp,27},{binop,'+',{temp,1},{temp,26}}},
	        {call,{temp,28},{label,"putstring"},[{temp,27}]},
	        {eval,{temp,29},{icon,3}},
	        {eval,{temp,30},{icon,4}},
	        {eval,{temp,31},{binop,'*',{temp,29},{temp,30}}},
	        {eval,{temp,32},{icon,5}},
	        {eval,{temp,33},{icon,6}},
	        {eval,{temp,34},{binop,'*',{temp,32},{temp,33}}},
	        {eval,{temp,35},{binop,'+',{temp,31},{temp,34}}},
	        {call,{temp,36},{label,"putint"},[{temp,35}]},
	        {eval,{temp,37},{icon,0}},
	        {eval,{temp,38},{binop,'+',{temp,1},{temp,37}}},
	        {call,{temp,39},{label,"putstring"},[{temp,38}]},
	        {eval,{temp,40},{icon,7}},
	        {eval,{temp,41},{icon,8}},
	        {eval,{temp,42},{binop,'*',{temp,40},{temp,41}}},
	        {eval,{temp,43},{icon,3}},
	        {eval,{temp,44},{icon,4}},
	        {eval,{temp,45},{binop,'*',{temp,43},{temp,44}}},
	        {eval,{temp,46},{binop,'-',{temp,42},{temp,45}}},
	        {eval,{temp,47},{icon,2}},
	        {eval,{temp,48},{binop,'-',{temp,46},{temp,47}}},
	        {call,{temp,49},{label,"putint"},[{temp,48}]},
	        {eval,{temp,50},{icon,0}},
	        {eval,{temp,51},{binop,'+',{temp,1},{temp,50}}},
	        {call,{temp,52},{label,"putstring"},[{temp,51}]},
	        {eval,{temp,53},{icon,6}},
	        {unop},
	        {eval,{temp,54},{icon,7}},
	        {unop},
	        {eval,{temp,55},{binop,'*',{temp,53},{temp,54}}},
	        {call,{temp,56},{label,"putint"},[{temp,55}]},
	        {eval,{temp,57},{icon,0}},
	        {eval,{temp,58},{binop,'+',{temp,1},{temp,57}}},
	        {call,{temp,59},{label,"putstring"},[{temp,58}]},
	        {eval,{temp,60},{icon,9}},
	        {eval,{temp,61},{icon,9}},
	        {eval,{temp,62},{binop,'+',{temp,60},{temp,61}}},
	        {eval,{temp,63},{icon,9}},
	        {eval,{temp,64},{binop,'+',{temp,62},{temp,63}}},
	        {eval,{temp,65},{icon,9}},
	        {eval,{temp,66},{icon,9}},
	        {eval,{temp,67},{binop,'+',{temp,65},{temp,66}}},
	        {eval,{temp,68},{icon,9}},
	        {eval,{temp,69},{binop,'/',{temp,67},{temp,68}}},
	        {eval,{temp,70},{binop,'*',{temp,64},{temp,69}}},
	        {eval,{temp,71},{icon,9}},
	        {eval,{temp,72},{icon,9}},
	        {eval,{temp,73},{binop,'*',{temp,71},{temp,72}}},
	        {eval,{temp,74},{icon,9}},
	        {eval,{temp,75},{icon,9}},
	        {eval,{temp,76},{binop,'+',{temp,74},{temp,75}}},
	        {eval,{temp,77},{binop,'-',{temp,73},{temp,76}}},
	        {eval,{temp,78},{binop,'*',{temp,70},{temp,77}}},
	        {eval,{temp,79},{icon,9}},
	        {eval,{temp,80},{icon,9}},
	        {eval,{temp,81},{binop,'+',{temp,79},{temp,80}}},
	        {eval,{temp,82},{icon,9}},
	        {eval,{temp,83},{icon,9}},
	        {eval,{temp,84},{binop,'+',{temp,82},{temp,83}}},
	        {eval,{temp,85},{icon,9}},
	        {eval,{temp,86},{binop,'+',{temp,84},{temp,85}}},
	        {eval,{temp,87},{binop,'*',{temp,81},{temp,86}}},
	        {eval,{temp,88},{binop,'+',{temp,78},{temp,87}}},
	        {eval,{temp,89},{icon,9}},
	        {eval,{temp,90},{icon,9}},
	        {eval,{temp,91},{binop,'*',{temp,89},{temp,90}}},
	        {eval,{temp,92},{icon,9}},
	        {eval,{temp,93},{binop,'+',{temp,91},{temp,92}}},
	        {eval,{temp,94},{binop,'/',{temp,88},{temp,93}}},
	        {eval,{temp,95},{icon,9}},
	        {eval,{temp,96},{icon,9}},
	        {eval,{temp,97},{binop,'/',{temp,95},{temp,96}}},
	        {eval,{temp,98},{binop,'-',{temp,94},{temp,97}}},
	        {call,{temp,99},{label,"putint"},[{temp,98}]},
	        {eval,{temp,100},{icon,0}},
	        {eval,{temp,101},{binop,'+',{temp,1},{temp,100}}},
	        {call,{temp,102},{label,"putstring"},[{temp,101}]},
	        {eval,{temp,103},{icon,6}},
	        {eval,{temp,104},{binop,'=',{temp,3},{temp,103}}},
	        {eval,{temp,105},{binop,'=',{temp,2},{temp,104}}},
	        {eval,{temp,106},{icon,1}},
	        {eval,{temp,107},{binop,'+',{temp,3},{temp,106}}},
	        {eval,{temp,108},{binop,'*',{temp,2},{temp,107}}},
	        {call,{temp,109},{label,"putint"},[{temp,108}]},
	        {eval,{temp,110},{icon,0}},
	        {eval,{temp,111},{binop,'+',{temp,1},{temp,110}}},
	        {call,{temp,112},{label,"putstring"},[{temp,111}]},
	        {eval,{temp,113},{icon,5}},
	        {eval,{temp,114},{binop,'=',{temp,3},{temp,113}}},
	        {eval,{temp,115},{icon,3}},
	        {eval,{temp,116},{binop,'+',{temp,114},{temp,115}}},
	        {eval,{temp,117},{binop,'=',{temp,2},{temp,116}}},
	        {eval,{temp,118},{binop,'*',{temp,2},{temp,3}}},
	        {eval,{temp,119},{icon,2}},
	        {eval,{temp,120},{binop,'+',{temp,118},{temp,119}}},
	        {call,{temp,121},{label,"putint"},[{temp,120}]},
	        {eval,{temp,122},{icon,0}},
	        {eval,{temp,123},{binop,'+',{temp,1},{temp,122}}},
	        {call,{temp,124},{label,"putstring"},[{temp,123}]},
	        {eval,{temp,125},{icon,3}},
	        {eval,{temp,126},{icon,113}},
	        {eval,{temp,127},{binop,'*',{temp,125},{temp,126}}},
	        {eval,{temp,128},{icon,2}},
	        {eval,{temp,129},{icon,2}},
	        {eval,{temp,130},{binop,'*',{temp,128},{temp,129}}},
	        {eval,{temp,131},{icon,2}},
	        {eval,{temp,132},{icon,2}},
	        {eval,{temp,133},{binop,'*',{temp,131},{temp,132}}},
	        {eval,{temp,134},{binop,'*',{temp,130},{temp,133}}},
	        {eval,{temp,135},{binop,'+',{temp,127},{temp,134}}},
	        {eval,{temp,136},{icon,1000000}},
	        {eval,{temp,137},{binop,'*',{temp,135},{temp,136}}},
	        {eval,{temp,138},{icon,113}},
	        {eval,{temp,139},{binop,'/',{temp,137},{temp,138}}},
	        {call,{temp,140},{label,"putint"},[{temp,139}]},
	        {eval,{temp,141},{icon,0}},
	        {eval,{temp,142},{binop,'+',{temp,1},{temp,141}}},
	        {call,{temp,143},{label,"putstring"},[{temp,142}]},
	        {eval,{temp,144},{icon,1}},
	        {eval,{temp,145},{icon,0}},
	        {eval,{temp,146},{binop,'>',{temp,144},{temp,145}}},
	        {call,{temp,147},{label,"putint"},[{temp,146}]},
	        {eval,{temp,148},{icon,1}},
	        {eval,{temp,149},{icon,0}},
	        {eval,{temp,150},{binop,'>=',{temp,148},{temp,149}}},
	        {call,{temp,151},{label,"putint"},[{temp,150}]},
	        {eval,{temp,152},{icon,1}},
	        {eval,{temp,153},{icon,0}},
	        {eval,{temp,154},{binop,'==',{temp,152},{temp,153}}},
	        {call,{temp,155},{label,"putint"},[{temp,154}]},
	        {eval,{temp,156},{icon,1}},
	        {eval,{temp,157},{icon,0}},
	        {eval,{temp,158},{binop,'<',{temp,156},{temp,157}}},
	        {call,{temp,159},{label,"putint"},[{temp,158}]},
	        {eval,{temp,160},{icon,1}},
	        {eval,{temp,161},{icon,0}},
	        {eval,{temp,162},{binop,'<=',{temp,160},{temp,161}}},
	        {call,{temp,163},{label,"putint"},[{temp,162}]},
	        {eval,{temp,164},{icon,0}},
	        {eval,{temp,165},{icon,1}},
	        {eval,{temp,166},{binop,'<',{temp,164},{temp,165}}},
	        {eval,{temp,167},{icon,0}},
	        {eval,{temp,168},{icon,1}},
	        {eval,{temp,169},{binop,'<',{temp,167},{temp,168}}},
	        {eval,{temp,170},{binop,'&&',{temp,166},{temp,169}}},
	        {call,{temp,171},{label,"putint"},[{temp,170}]},
	        {eval,{temp,172},{icon,0}},
	        {eval,{temp,173},{icon,1}},
	        {eval,{temp,174},{binop,'<',{temp,172},{temp,173}}},
	        {eval,{temp,175},{icon,1}},
	        {eval,{temp,176},{icon,0}},
	        {eval,{temp,177},{binop,'<',{temp,175},{temp,176}}},
	        {eval,{temp,178},{binop,'&&',{temp,174},{temp,177}}},
	        {call,{temp,179},{label,"putint"},[{temp,178}]},
	        {eval,{temp,180},{icon,1}},
	        {eval,{temp,181},{icon,0}},
	        {eval,{temp,182},{binop,'<',{temp,180},{temp,181}}},
	        {eval,{temp,183},{icon,0}},
	        {eval,{temp,184},{icon,1}},
	        {eval,{temp,185},{binop,'<',{temp,183},{temp,184}}},
	        {eval,{temp,186},{binop,'&&',{temp,182},{temp,185}}},
	        {call,{temp,187},{label,"putint"},[{temp,186}]},
	        {eval,{temp,188},{icon,1}},
	        {eval,{temp,189},{icon,0}},
	        {eval,{temp,190},{binop,'<',{temp,188},{temp,189}}},
	        {eval,{temp,191},{icon,1}},
	        {eval,{temp,192},{icon,0}},
	        {eval,{temp,193},{binop,'<',{temp,191},{temp,192}}},
	        {eval,{temp,194},{binop,'&&',{temp,190},{temp,193}}},
	        {call,{temp,195},{label,"putint"},[{temp,194}]},
	        {eval,{temp,196},{icon,0}},
	        {eval,{temp,197},{binop,'+',{temp,1},{temp,196}}},
	        {call,{temp,198},{label,"putstring"},[{temp,197}]}],
	       {labdef,{label,100}}}].



### ducc -t suite/noisy/simple/sim06.c gives:

	[{data,{label,100},2},
	 {proc,{label,"main"},
	       [],
	       [{temp,2},
	        {temp,3},
	        {temp,4},
	        {temp,5},
	        {temp,6},
	        {temp,7},
	        {temp,8},
	        {temp,9},
	        {temp,10},
	        {temp,11},
	        {temp,12},
	        {temp,13},
	        {temp,14},
	        {temp,15},
	        {temp,16},
	        {temp,17},
	        {temp,18},
	        {temp,19},
	        {temp,20},
	        {temp,21},
	        {temp,22},
	        {temp,23}],
	       0,
	       [{eval,{temp,3},{icon,10}},
	        {eval,{temp,4},{binop,'=',{temp,2},{temp,3}}},
	        {eval,{temp,5},{icon,1}},
	        {arrelem},
	        {eval,{temp,6},{icon,0}},
	        {eval,{temp,7},{binop,'=',{temp,5},{temp,6}}},
	        {jump,{label,102}},
	        {labdef,{label,103}},
	        {eval,{temp,8},{icon,0}},
	        {arrelem},
	        {eval,{temp,9},{icon,48}},
	        {eval,{temp,10},{binop,'+',{temp,9},{temp,2}}},
	        {eval,{temp,11},{icon,1}},
	        {eval,{temp,12},{binop,'-',{temp,10},{temp,11}}},
	        {eval,{temp,13},{binop,'=',{temp,8},{temp,12}}},
	        {eval,{temp,14},{labref,{label,100}}},
	        {call,{temp,15},{label,"putstring"},[{temp,14}]},
	        {eval,{temp,16},{icon,1}},
	        {eval,{temp,17},{binop,'-',{temp,2},{temp,16}}},
	        {eval,{temp,18},{binop,'=',{temp,2},{temp,17}}},
	        {labdef,{label,102}},
	        {cjump,neq,{temp,2},0,{label,103}},
	        {labdef,{label,104}},
	        {eval,{temp,19},{icon,0}},
	        {arrelem},
	        {eval,{temp,20},{icon,'\\n'}},
	        {eval,{temp,21},{binop,'=',{temp,19},{temp,20}}},
	        {eval,{temp,22},{labref,{label,100}}},
	        {call,{temp,23},{label,"putstring"},[{temp,22}]}],
	       {labdef,{label,101}}}].



### ducc -t suite/noisy/simple/sim07.c gives:

	{'EXIT',{badarg,[{translator_helpers,get_meta,1},
	                 {translator_helpers,get_tag,1},
	                 {translator,translate_stmt,2},
	                 {translator,translate_if,2},
	                 {translator,translate_list,3},
	                 {translator,translate_list,3},
	                 {translator,translate_while,2},
	                 {translator,translate_list,3}]}}



### ducc -t suite/noisy/simple/sim08.c gives:

	[{proc,{label,"foo"},
	       [{temp,2},{temp,3},{temp,4},{temp,5},{temp,6}],
	       [{temp,2},
	        {temp,3},
	        {temp,4},
	        {temp,5},
	        {temp,6},
	        {temp,7},
	        {temp,8},
	        {temp,9},
	        {temp,10},
	        {temp,11}],
	       0,
	       [{call,{temp,7},{label,"putint"},[{temp,2}]},
	        {call,{temp,8},{label,"putint"},[{temp,3}]},
	        {call,{temp,9},{label,"putint"},[{temp,4}]},
	        {call,{temp,10},{label,"putint"},[{temp,5}]},
	        {call,{temp,11},{label,"putint"},[{temp,6}]}],
	       {labdef,{label,100}}},
	 {proc,{label,"f"},
	       [{temp,12}],
	       [{temp,12},{temp,13},{temp,14}],
	       0,
	       [{eval,{temp,13},{icon,1}},
	        {eval,{temp,14},{binop,'+',{temp,12},{temp,13}}},
	        {jump,{label,101}}],
	       {labdef,{label,101}}},
	 {proc,{label,"g"},
	       [{temp,15}],
	       [{temp,15},
	        {temp,16},
	        {temp,17},
	        {temp,18},
	        {temp,19},
	        {temp,20},
	        {temp,21},
	        {temp,22},
	        {temp,23}],
	       0,
	       [{eval,{temp,16},{icon,0}},
	        {eval,{temp,17},{binop,'==',{temp,15},{temp,16}}},
	        {cjump,eq,{temp,17},0,{label,103}},
	        {eval,{temp,18},{icon,1}},
	        {jump,{label,102}},
	        {jump,{label,104}},
	        {labdef,{label,103}},
	        {eval,{temp,19},{icon,2}},
	        {eval,{temp,20},{icon,1}},
	        {eval,{temp,21},{binop,'-',{temp,15},{temp,20}}},
	        {call,{temp,22},{label,"g"},[{temp,21}]},
	        {eval,{temp,23},{binop,'*',{temp,19},{temp,22}}},
	        {jump,{label,102}},
	        {labdef,{label,104}}],
	       {labdef,{label,102}}},
	 {proc,{label,"main"},
	       [],
	       [{temp,24},
	        {temp,25},
	        {temp,26},
	        {temp,27},
	        {temp,28},
	        {temp,29},
	        {temp,30},
	        {temp,31},
	        {temp,32},
	        {temp,33},
	        {temp,34},
	        {temp,35},
	        {temp,36},
	        {temp,37},
	        {temp,38},
	        {temp,39},
	        {temp,40},
	        {temp,41},
	        {temp,42},
	        {temp,43},
	        {temp,44},
	        {temp,45},
	        {temp,46},
	        {temp,47},
	        {temp,48},
	        {temp,49},
	        {temp,50},
	        {temp,51},
	        {temp,52},
	        {temp,53},
	        {temp,54},
	        {temp,55},
	        {temp,56},
	        {temp,57},
	        {temp,58},
	        {temp,59},
	        {temp,60},
	        {temp,61},
	        {temp,62},
	        {temp,63},
	        {temp,64},
	        {temp,65},
	        {temp,66},
	        {temp,67},
	        {temp,68},
	        {temp,69},
	        {temp,70},
	        {temp,71},
	        {temp,72},
	        {temp,73},
	        {temp,74},
	        {temp,75},
	        {temp,76},
	        {temp,77},
	        {temp,78},
	        {temp,79},
	        {temp,80},
	        {temp,81},
	        {temp,82},
	        {temp,83},
	        {temp,84}],
	       0,
	       [{eval,{temp,25},{icon,0}},
	        {eval,{temp,26},{icon,1}},
	        {eval,{temp,27},{icon,2}},
	        {eval,{temp,28},{icon,3}},
	        {eval,{temp,29},{icon,4}},
	        {call,{temp,30},
	              {label,"foo"},
	              [{temp,25},{temp,26},{temp,27},{temp,28},{temp,29}]},
	        {eval,{temp,31},{icon,5}},
	        {eval,{temp,32},{binop,'=',{temp,24},{temp,31}}},
	        {eval,{temp,33},{icon,0}},
	        {eval,{temp,34},{binop,'+',{temp,24},{temp,33}}},
	        {eval,{temp,35},{icon,1}},
	        {eval,{temp,36},{binop,'+',{temp,24},{temp,35}}},
	        {eval,{temp,37},{icon,2}},
	        {eval,{temp,38},{binop,'+',{temp,24},{temp,37}}},
	        {eval,{temp,39},{binop,'+',{temp,24},{temp,24}}},
	        {eval,{temp,40},{icon,2}},
	        {eval,{temp,41},{binop,'-',{temp,39},{temp,40}}},
	        {eval,{temp,42},{icon,2}},
	        {eval,{temp,43},{binop,'*',{temp,24},{temp,42}}},
	        {eval,{temp,44},{icon,1}},
	        {eval,{temp,45},{binop,'-',{temp,43},{temp,44}}},
	        {call,{temp,46},
	              {label,"foo"},
	              [{temp,34},{temp,36},{temp,38},{temp,41},{temp,45}]},
	        {eval,{temp,47},{icon,0}},
	        {eval,{temp,48},{icon,0}},
	        {call,{temp,49},{label,"f"},[{temp,48}]},
	        {eval,{temp,50},{icon,0}},
	        {call,{temp,51},{label,"f"},[{temp,50}]},
	        {call,{temp,52},{label,"f"},[{temp,51}]},
	        {eval,{temp,53},{icon,0}},
	        {call,{temp,54},{label,"f"},[{temp,53}]},
	        {call,{temp,55},{label,"f"},[{temp,54}]},
	        {call,{temp,56},{label,"f"},[{temp,55}]},
	        {eval,{temp,57},{icon,2}},
	        {call,{temp,58},{label,"g"},[{temp,57}]},
	        {call,{temp,59},
	              {label,"foo"},
	              [{temp,47},{temp,49},{temp,52},{temp,56},{temp,58}]},
	        {eval,{temp,60},{icon,2}},
	        {call,{temp,61},{label,"g"},[{temp,60}]},
	        {eval,{temp,62},{icon,0}},
	        {call,{temp,63},{label,"g"},[{temp,62}]},
	        {eval,{temp,64},{binop,'+',{temp,61},{temp,63}}},
	        {eval,{temp,65},{icon,2}},
	        {call,{temp,66},{label,"g"},[{temp,65}]},
	        {eval,{temp,67},{icon,1}},
	        {call,{temp,68},{label,"g"},[{temp,67}]},
	        {eval,{temp,69},{binop,'+',{temp,66},{temp,68}}},
	        {eval,{temp,70},{icon,0}},
	        {call,{temp,71},{label,"g"},[{temp,70}]},
	        {eval,{temp,72},{icon,1}},
	        {call,{temp,73},{label,"g"},[{temp,72}]},
	        {eval,{temp,74},{binop,'+',{temp,71},{temp,73}}},
	        {eval,{temp,75},{icon,2}},
	        {call,{temp,76},{label,"g"},[{temp,75}]},
	        {eval,{temp,77},{binop,'+',{temp,74},{temp,76}}},
	        {eval,{temp,78},{icon,3}},
	        {call,{temp,79},{label,"g"},[{temp,78}]},
	        {eval,{temp,80},{icon,4}},
	        {call,{temp,81},{label,"g"},[{temp,80}]},
	        {eval,{temp,82},{icon,7}},
	        {eval,{temp,83},{binop,'-',{temp,81},{temp,82}}},
	        {call,{temp,84},
	              {label,"foo"},
	              [{temp,64},{temp,69},{temp,77},{temp,79},{temp,83}]}],
	       {labdef,{label,105}}}].



### ducc -t suite/noisy/simple/sim09.c gives:

	{'EXIT',{{case_clause,farray},
	         [{translator,translate_ident,2},
	          {translator,translate_actuals,2},
	          {translator,translate_funcall,2},
	          {translator,translate_list,3},
	          {translator,translate_fundef,2},
	          {translator,translate_topdecs,2},
	          {translator,translate_topdecs,2},
	          {translator_driver,translate,2}]}}



### ducc -t suite/noisy/simple/sim10.c gives:

	[{proc,{label,"main"},
	       [],
	       [{temp,2},
	        {temp,3},
	        {temp,4},
	        {temp,5},
	        {temp,6},
	        {temp,7},
	        {temp,8},
	        {temp,9},
	        {temp,10},
	        {temp,11},
	        {temp,12},
	        {temp,13},
	        {temp,14},
	        {temp,15},
	        {temp,16},
	        {temp,17},
	        {temp,18},
	        {temp,19},
	        {temp,20},
	        {temp,21},
	        {temp,22},
	        {temp,23},
	        {temp,24},
	        {temp,25},
	        {temp,26},
	        {temp,27},
	        {temp,28},
	        {temp,29},
	        {temp,30},
	        {temp,31},
	        {temp,32},
	        {temp,33},
	        {temp,34},
	        {temp,35},
	        {temp,36},
	        {temp,37},
	        {temp,38},
	        {temp,39},
	        {temp,40},
	        {temp,41},
	        {temp,42},
	        {temp,43},
	        {temp,44},
	        {temp,45},
	        {temp,46},
	        {temp,47},
	        {temp,48},
	        {temp,49},
	        {temp,50},
	        {temp,51},
	        {temp,52},
	        {temp,53},
	        {temp,54},
	        {temp,55},
	        {temp,56},
	        {temp,57},
	        {temp,58},
	        {temp,59},
	        {temp,60},
	        {temp,61},
	        {temp,62},
	        {temp,63},
	        {temp,64},
	        {temp,65},
	        {temp,66},
	        {temp,67},
	        {temp,68},
	        {temp,69},
	        {temp,70},
	        {temp,71},
	        {temp,72},
	        {temp,73},
	        {temp,74},
	        {temp,75},
	        {temp,76},
	        {temp,77},
	        {temp,78},
	        {temp,79},
	        {temp,80},
	        {temp,81},
	        {temp,82},
	        {temp,83},
	        {temp,84},
	        {temp,85},
	        {temp,86},
	        {temp,87},
	        {temp,88},
	        {temp,89},
	        {temp,90},
	        {temp,91},
	        {temp,92},
	        {temp,93},
	        {temp,94},
	        {temp,95},
	        {temp,96},
	        {temp,97},
	        {temp,98},
	        {temp,99},
	        {temp,100},
	        {temp,101},
	        {temp,102},
	        {temp,103},
	        {temp,104},
	        {temp,105},
	        {temp,106},
	        {temp,107},
	        {temp,108},
	        {temp,109},
	        {temp,110},
	        {temp,111},
	        {temp,112},
	        {temp,113},
	        {temp,114},
	        {temp,115},
	        {temp,116},
	        {temp,117},
	        {temp,118},
	        {temp,119},
	        {temp,120},
	        {temp,121},
	        {temp,122},
	        {temp,123},
	        {temp,124},
	        {temp,125},
	        {temp,126},
	        {temp,127},
	        {temp,128},
	        {temp,129},
	        {temp,130},
	        {temp,131}],
	       114,
	       [{eval,{temp,3},{icon,0}},
	        {arrelem},
	        {eval,{temp,4},{icon,'Y'}},
	        {eval,{temp,5},{binop,'=',{temp,3},{temp,4}}},
	        {eval,{temp,6},{icon,1}},
	        {arrelem},
	        {eval,{temp,7},{icon,o}},
	        {eval,{temp,8},{binop,'=',{temp,6},{temp,7}}},
	        {eval,{temp,9},{icon,2}},
	        {arrelem},
	        {eval,{temp,10},{icon,u}},
	        {eval,{temp,11},{binop,'=',{temp,9},{temp,10}}},
	        {eval,{temp,12},{icon,3}},
	        {arrelem},
	        {eval,{temp,13},{icon,r}},
	        {eval,{temp,14},{binop,'=',{temp,12},{temp,13}}},
	        {eval,{temp,15},{icon,4}},
	        {arrelem},
	        {eval,{temp,16},{icon,' '}},
	        {eval,{temp,17},{binop,'=',{temp,15},{temp,16}}},
	        {eval,{temp,18},{icon,5}},
	        {arrelem},
	        {eval,{temp,19},{icon,n}},
	        {eval,{temp,20},{binop,'=',{temp,18},{temp,19}}},
	        {eval,{temp,21},{icon,6}},
	        {arrelem},
	        {eval,{temp,22},{icon,a}},
	        {eval,{temp,23},{binop,'=',{temp,21},{temp,22}}},
	        {eval,{temp,24},{icon,7}},
	        {arrelem},
	        {eval,{temp,25},{icon,m}},
	        {eval,{temp,26},{binop,'=',{temp,24},{temp,25}}},
	        {eval,{temp,27},{icon,8}},
	        {arrelem},
	        {eval,{temp,28},{icon,e}},
	        {eval,{temp,29},{binop,'=',{temp,27},{temp,28}}},
	        {eval,{temp,30},{icon,9}},
	        {arrelem},
	        {eval,{temp,31},{icon,'?'}},
	        {eval,{temp,32},{binop,'=',{temp,30},{temp,31}}},
	        {eval,{temp,33},{icon,10}},
	        {arrelem},
	        {eval,{temp,34},{icon,' '}},
	        {eval,{temp,35},{binop,'=',{temp,33},{temp,34}}},
	        {eval,{temp,36},{icon,11}},
	        {arrelem},
	        {eval,{temp,37},{icon,0}},
	        {eval,{temp,38},{binop,'=',{temp,36},{temp,37}}},
	        {eval,{temp,39},{icon,0}},
	        {arrelem},
	        {eval,{temp,40},{icon,'Y'}},
	        {eval,{temp,41},{binop,'=',{temp,39},{temp,40}}},
	        {eval,{temp,42},{icon,1}},
	        {arrelem},
	        {eval,{temp,43},{icon,o}},
	        {eval,{temp,44},{binop,'=',{temp,42},{temp,43}}},
	        {eval,{temp,45},{icon,2}},
	        {arrelem},
	        {eval,{temp,46},{icon,u}},
	        {eval,{temp,47},{binop,'=',{temp,45},{temp,46}}},
	        {eval,{temp,48},{icon,3}},
	        {arrelem},
	        {eval,{temp,49},{icon,r}},
	        {eval,{temp,50},{binop,'=',{temp,48},{temp,49}}},
	        {eval,{temp,51},{icon,4}},
	        {arrelem},
	        {eval,{temp,52},{icon,' '}},
	        {eval,{temp,53},{binop,'=',{temp,51},{temp,52}}},
	        {eval,{temp,54},{icon,5}},
	        {arrelem},
	        {eval,{temp,55},{icon,a}},
	        {eval,{temp,56},{binop,'=',{temp,54},{temp,55}}},
	        {eval,{temp,57},{icon,6}},
	        {arrelem},
	        {eval,{temp,58},{icon,g}},
	        {eval,{temp,59},{binop,'=',{temp,57},{temp,58}}},
	        {eval,{temp,60},{icon,7}},
	        {arrelem},
	        {eval,{temp,61},{icon,e}},
	        {eval,{temp,62},{binop,'=',{temp,60},{temp,61}}},
	        {eval,{temp,63},{icon,8}},
	        {arrelem},
	        {eval,{temp,64},{icon,' '}},
	        {eval,{temp,65},{binop,'=',{temp,63},{temp,64}}},
	        {eval,{temp,66},{icon,9}},
	        {arrelem},
	        {eval,{temp,67},{icon,0}},
	        {eval,{temp,68},{binop,'=',{temp,66},{temp,67}}},
	        {eval,{temp,69},{icon,0}},
	        {arrelem},
	        {eval,{temp,70},{icon,'Y'}},
	        {eval,{temp,71},{binop,'=',{temp,69},{temp,70}}},
	        {eval,{temp,72},{icon,1}},
	        {arrelem},
	        {eval,{temp,73},{icon,o}},
	        {eval,{temp,74},{binop,'=',{temp,72},{temp,73}}},
	        {eval,{temp,75},{icon,2}},
	        {arrelem},
	        {eval,{temp,76},{icon,u}},
	        {eval,{temp,77},{binop,'=',{temp,75},{temp,76}}},
	        {eval,{temp,78},{icon,3}},
	        {arrelem},
	        {eval,{temp,79},{icon,' '}},
	        {eval,{temp,80},{binop,'=',{temp,78},{temp,79}}},
	        {eval,{temp,81},{icon,4}},
	        {arrelem},
	        {eval,{temp,82},{icon,a}},
	        {eval,{temp,83},{binop,'=',{temp,81},{temp,82}}},
	        {eval,{temp,84},{icon,5}},
	        {arrelem},
	        {eval,{temp,85},{icon,r}},
	        {eval,{temp,86},{binop,'=',{temp,84},{temp,85}}},
	        {eval,{temp,87},{icon,6}},
	        {arrelem},
	        {eval,{temp,88},{icon,e}},
	        {eval,{temp,89},{binop,'=',{temp,87},{temp,88}}},
	        {eval,{temp,90},{icon,7}},
	        {arrelem},
	        {eval,{temp,91},{icon,':'}},
	        {eval,{temp,92},{binop,'=',{temp,90},{temp,91}}},
	        {eval,{temp,93},{icon,8}},
	        {arrelem},
	        {eval,{temp,94},{icon,' '}},
	        {eval,{temp,95},{binop,'=',{temp,93},{temp,94}}},
	        {eval,{temp,96},{icon,9}},
	        {arrelem},
	        {eval,{temp,97},{icon,0}},
	        {eval,{temp,98},{binop,'=',{temp,96},{temp,97}}},
	        {eval,{temp,99},{icon,0}},
	        {arrelem},
	        {eval,{temp,100},{icon,'\\n'}},
	        {eval,{temp,101},{binop,'=',{temp,99},{temp,100}}},
	        {eval,{temp,102},{icon,1}},
	        {arrelem},
	        {eval,{temp,103},{icon,0}},
	        {eval,{temp,104},{binop,'=',{temp,102},{temp,103}}},
	        {eval,{temp,105},{icon,0}},
	        {eval,{temp,106},{binop,'+',{temp,1},{temp,105}}},
	        {call,{temp,107},{label,"putstring"},[{temp,106}]},
	        {eval,{temp,108},{icon,34}},
	        {eval,{temp,109},{binop,'+',{temp,1},{temp,108}}},
	        {call,{temp,110},{label,"getstring"},[{temp,109}]},
	        {eval,{temp,111},{icon,12}},
	        {eval,{temp,112},{binop,'+',{temp,1},{temp,111}}},
	        {call,{temp,113},{label,"putstring"},[{temp,112}]},
	        {call,{temp,114},{label,"getint"},[]},
	        {eval,{temp,115},{binop,'=',{temp,2},{temp,114}}},
	        {eval,{temp,116},{icon,22}},
	        {eval,{temp,117},{binop,'+',{temp,1},{temp,116}}},
	        {call,{temp,118},{label,"putstring"},[{temp,117}]},
	        {eval,{temp,119},{icon,34}},
	        {eval,{temp,120},{binop,'+',{temp,1},{temp,119}}},
	        {call,{temp,121},{label,"putstring"},[{temp,120}]},
	        {eval,{temp,122},{icon,32}},
	        {eval,{temp,123},{binop,'+',{temp,1},{temp,122}}},
	        {call,{temp,124},{label,"putstring"},[{temp,123}]},
	        {eval,{temp,125},{icon,22}},
	        {eval,{temp,126},{binop,'+',{temp,1},{temp,125}}},
	        {call,{temp,127},{label,"putstring"},[{temp,126}]},
	        {call,{temp,128},{label,"putint"},[{temp,2}]},
	        {eval,{temp,129},{icon,32}},
	        {eval,{temp,130},{binop,'+',{temp,1},{temp,129}}},
	        {call,{temp,131},{label,"putstring"},[{temp,130}]}],
	       {labdef,{label,100}}}].



### ducc -t suite/noisy/simple/sim11.c gives:

	[{proc,{label,"f"},
	       [{temp,2},{temp,3}],
	       [{temp,2},{temp,4},{temp,5}],
	       0,
	       [{eval,{temp,4},{icon,0}},
	        {arrelem},
	        {eval,{temp,5},{binop,'=',{temp,4},{temp,2}}}],
	       {labdef,{label,100}}},
	 {proc,{label,"main"},
	       [],
	       [{temp,6},
	        {temp,7},
	        {temp,8},
	        {temp,9},
	        {temp,10},
	        {temp,11},
	        {temp,12},
	        {temp,13},
	        {temp,14},
	        {temp,15},
	        {temp,16},
	        {temp,17},
	        {temp,18},
	        {temp,19},
	        {temp,20},
	        {temp,21},
	        {temp,22},
	        {temp,23},
	        {temp,24},
	        {temp,25},
	        {temp,26},
	        {temp,27},
	        {temp,28},
	        {temp,29},
	        {temp,30},
	        {temp,31}],
	       2,
	       [{eval,{temp,7},{icon,10}},
	        {eval,{temp,8},{binop,'=',{temp,6},{temp,7}}},
	        {eval,{temp,9},{icon,1}},
	        {arrelem},
	        {eval,{temp,10},{icon,0}},
	        {eval,{temp,11},{binop,'=',{temp,9},{temp,10}}},
	        {jump,{label,102}},
	        {labdef,{label,103}},
	        {eval,{temp,12},{icon,48}},
	        {eval,{temp,13},{binop,'+',{temp,12},{temp,6}}},
	        {eval,{temp,14},{icon,1}},
	        {eval,{temp,15},{binop,'-',{temp,13},{temp,14}}},
	        {eval,{temp,16},{icon,0}},
	        {eval,{temp,17},{binop,'+',{temp,1},{temp,16}}},
	        {call,{temp,18},{label,"f"},[{temp,15},{temp,17}]},
	        {eval,{temp,19},{icon,0}},
	        {eval,{temp,20},{binop,'+',{temp,1},{temp,19}}},
	        {call,{temp,21},{label,"putstring"},[{temp,20}]},
	        {eval,{temp,22},{icon,1}},
	        {eval,{temp,23},{binop,'-',{temp,6},{temp,22}}},
	        {eval,{temp,24},{binop,'=',{temp,6},{temp,23}}},
	        {labdef,{label,102}},
	        {cjump,neq,{temp,6},0,{label,103}},
	        {labdef,{label,104}},
	        {eval,{temp,25},{icon,'\\n'}},
	        {eval,{temp,26},{icon,0}},
	        {eval,{temp,27},{binop,'+',{temp,1},{temp,26}}},
	        {call,{temp,28},{label,"f"},[{temp,25},{temp,27}]},
	        {eval,{temp,29},{icon,0}},
	        {eval,{temp,30},{binop,'+',{temp,1},{temp,29}}},
	        {call,{temp,31},{label,"putstring"},[{temp,30}]}],
	       {labdef,{label,101}}}].



### ducc -t suite/quiet/lexer/l01.c gives:

	[{proc,{label,"main"},[],[],0,[],{labdef,{label,100}}}].



### ducc -t suite/quiet/lexer/l02.c gives:

	[{data,{label,100},long},
	 {data,{label,101},byte},
	 {data,{label,102},long},
	 {data,{label,103},long},
	 {data,{label,104},long},
	 {data,{label,105},long},
	 {data,{label,106},long},
	 {proc,{label,"main"},[],[],0,[],{labdef,{label,107}}}].



### ducc -t suite/quiet/lexer/l03.c gives:

	[{proc,{label,"main"},
	       [],
	       [{temp,2},
	        {temp,3},
	        {temp,4},
	        {temp,5},
	        {temp,6},
	        {temp,7},
	        {temp,8},
	        {temp,9},
	        {temp,10},
	        {temp,11},
	        {temp,12}],
	       0,
	       [{eval,{temp,3},{icon,123456789}},
	        {eval,{temp,4},{binop,'=',{temp,2},{temp,3}}},
	        {eval,{temp,5},{icon,'0'}},
	        {eval,{temp,6},{binop,'=',{temp,2},{temp,5}}},
	        {eval,{temp,7},{icon,a}},
	        {eval,{temp,8},{binop,'=',{temp,2},{temp,7}}},
	        {eval,{temp,9},{icon,' '}},
	        {eval,{temp,10},{binop,'=',{temp,2},{temp,9}}},
	        {eval,{temp,11},{icon,'\\n'}},
	        {eval,{temp,12},{binop,'=',{temp,2},{temp,11}}}],
	       {labdef,{label,100}}}].



### ducc -t suite/quiet/lexer/l04.c gives:

	[{proc,{label,"main"},
	       [],
	       [{temp,2},
	        {temp,3},
	        {temp,4},
	        {temp,5},
	        {temp,6},
	        {temp,7},
	        {temp,8},
	        {temp,9},
	        {temp,10},
	        {temp,11},
	        {temp,12},
	        {temp,13},
	        {temp,14},
	        {temp,15},
	        {temp,16}],
	       0,
	       [{eval,{temp,4},{icon,1}},
	        {eval,{temp,5},{icon,0}},
	        {eval,{temp,6},{binop,'==',{temp,4},{temp,5}}},
	        {cjump,eq,{temp,6},0,{label,101}},
	        {eval,{temp,7},{icon,0}},
	        {eval,{temp,8},{binop,'=',{temp,2},{temp,7}}},
	        {jump,{label,102}},
	        {labdef,{label,101}},
	        {eval,{temp,9},{icon,1}},
	        {eval,{temp,10},{binop,'=',{temp,2},{temp,9}}},
	        {labdef,{label,102}},
	        {jump,{label,103}},
	        {labdef,{label,104}},
	        {eval,{temp,14},{icon,0}},
	        {eval,{temp,15},{binop,'=',{temp,2},{temp,14}}},
	        {labdef,{label,103}},
	        {eval,{temp,11},{icon,1}},
	        {eval,{temp,12},{icon,0}},
	        {eval,{temp,13},{binop,'==',{temp,11},{temp,12}}},
	        {cjump,neq,{temp,13},0,{label,104}},
	        {labdef,{label,105}},
	        {eval,{temp,16},{icon,42}},
	        {jump,{label,100}}],
	       {labdef,{label,100}}}].



### ducc -t suite/quiet/lexer/l05.c gives:

	[{proc,{label,"f"},
	       [{temp,2},{temp,3}],
	       [{temp,2},{temp,3},{temp,4}],
	       0,
	       [{eval,{temp,4},{binop,'+',{temp,2},{temp,3}}},{jump,{label,100}}],
	       {labdef,{label,100}}},
	 {proc,{label,"main"},
	       [],
	       [{temp,5},
	        {temp,6},
	        {temp,7},
	        {temp,8},
	        {temp,9},
	        {temp,10},
	        {temp,11},
	        {temp,12},
	        {temp,13},
	        {temp,14},
	        {temp,15},
	        {temp,16},
	        {temp,17},
	        {temp,18},
	        {temp,19},
	        {temp,20},
	        {temp,21},
	        {temp,22},
	        {temp,23},
	        {temp,24},
	        {temp,25},
	        {temp,26},
	        {temp,27},
	        {temp,28},
	        {temp,29},
	        {temp,30},
	        {temp,31},
	        {temp,32},
	        {temp,33},
	        {temp,34},
	        {temp,35},
	        {temp,36},
	        {temp,37}],
	       0,
	       [{eval,{temp,6},{icon,1}},
	        {eval,{temp,7},{icon,3}},
	        {unop},
	        {eval,{temp,8},{binop,'!=',{temp,6},{temp,7}}},
	        {eval,{temp,9},{icon,4}},
	        {eval,{temp,10},{icon,6}},
	        {eval,{temp,11},{binop,'&&',{temp,9},{temp,10}}},
	        {eval,{temp,12},{icon,7}},
	        {eval,{temp,13},{icon,8}},
	        {eval,{temp,14},{binop,'*',{temp,12},{temp,13}}},
	        {eval,{temp,15},{icon,10}},
	        {eval,{temp,16},{binop,'+',{temp,14},{temp,15}}},
	        {eval,{temp,17},{icon,11}},
	        {eval,{temp,18},{icon,12}},
	        {eval,{temp,19},{binop,'-',{temp,17},{temp,18}}},
	        {eval,{temp,20},{icon,12}},
	        {eval,{temp,21},{icon,16}},
	        {eval,{temp,22},{binop,'/',{temp,20},{temp,21}}},
	        {call,{temp,23},{label,"f"},[{temp,19},{temp,22}]},
	        {eval,{temp,24},{icon,17}},
	        {eval,{temp,25},{icon,18}},
	        {eval,{temp,26},{binop,'<=',{temp,24},{temp,25}}},
	        {eval,{temp,27},{icon,20}},
	        {eval,{temp,28},{binop,'<',{temp,26},{temp,27}}},
	        {eval,{temp,29},{icon,21}},
	        {eval,{temp,30},{icon,22}},
	        {eval,{temp,31},{binop,'==',{temp,29},{temp,30}}},
	        {eval,{temp,32},{binop,'=',{temp,5},{temp,31}}},
	        {eval,{temp,33},{icon,25}},
	        {eval,{temp,34},{icon,27}},
	        {eval,{temp,35},{binop,'>=',{temp,33},{temp,34}}},
	        {eval,{temp,36},{icon,28}},
	        {eval,{temp,37},{binop,'>',{temp,35},{temp,36}}}],
	       {labdef,{label,101}}}].



### ducc -t suite/quiet/lexer/l06.c gives:

	[{proc,{label,"main"},[],[],0,[],{labdef,{label,100}}}].



### ducc -t suite/quiet/mips/m01.c gives:

	[{data,{label,100},long},
	 {data,{label,101},byte},
	 {data,{label,102},long},
	 {proc,{label,"jal"},
	       [],
	       [{temp,2},
	        {temp,3},
	        {temp,4},
	        {temp,5},
	        {temp,6},
	        {temp,7},
	        {temp,8},
	        {temp,9},
	        {temp,10},
	        {temp,11},
	        {temp,12}],
	       0,
	       [{eval,{temp,2},{labref,{label,100}}},
	        {eval,{temp,3},{load,long,{temp,2}}},
	        {eval,{temp,4},{labref,{label,100}}},
	        {eval,{temp,5},{load,long,{temp,4}}},
	        {eval,{temp,6},{labref,{label,101}}},
	        {eval,{temp,7},{load,byte,{temp,6}}},
	        {eval,{temp,8},{binop,'*',{temp,5},{temp,7}}},
	        {eval,{temp,9},{labref,{label,102}}},
	        {eval,{temp,10},{load,long,{temp,9}}},
	        {eval,{temp,11},{binop,'+',{temp,8},{temp,10}}},
	        {eval,{temp,12},{binop,'=',{temp,3},{temp,11}}}],
	       {labdef,{label,103}}},
	 {proc,{label,"mov"},
	       [{temp,13}],
	       [{temp,13},{temp,14},{temp,15},{temp,16}],
	       0,
	       [{eval,{temp,14},{labref,{label,101}}},
	        {eval,{temp,15},{load,byte,{temp,14}}},
	        {eval,{temp,16},{binop,'=',{temp,15},{temp,13}}}],
	       {labdef,{label,104}}},
	 {proc,{label,"main"},
	       [],
	       [{temp,17},
	        {temp,18},
	        {temp,19},
	        {temp,20},
	        {temp,21},
	        {temp,22},
	        {temp,23},
	        {temp,24}],
	       0,
	       [{eval,{temp,17},{labref,{label,102}}},
	        {eval,{temp,18},{load,long,{temp,17}}},
	        {eval,{temp,19},{icon,8}},
	        {eval,{temp,20},{binop,'=',{temp,18},{temp,19}}},
	        {call,{temp,21},{label,"jal"},[]},
	        {eval,{temp,22},{labref,{label,102}}},
	        {eval,{temp,23},{load,long,{temp,22}}},
	        {call,{temp,24},{label,"mov"},[{temp,23}]}],
	       {labdef,{label,105}}}].



### ducc -t suite/quiet/mips/m02.c gives:

	{'EXIT',{badarg,[{translator_helpers,get_meta,1},
	                 {translator_helpers,get_tag,1},
	                 {translator,translate_stmt,2},
	                 {translator,translate_if,2},
	                 {translator,translate_list,3},
	                 {translator,translate_fundef,2},
	                 {translator,translate_topdecs,2},
	                 {translator_driver,translate,2}]}}



### ducc -t suite/quiet/mips/m03.c gives:

	{'EXIT',{badarg,[{translator_helpers,get_meta,1},
	                 {translator_helpers,get_tag,1},
	                 {translator,translate_stmt,2},
	                 {translator,translate_if,2},
	                 {translator,translate_list,3},
	                 {translator,translate_fundef,2},
	                 {translator,translate_topdecs,2},
	                 {translator_driver,translate,2}]}}



### ducc -t suite/quiet/parser/p01.c gives:

	[{proc,{label,"main"},
	       [],
	       [{temp,2},{temp,3},{temp,4},{temp,5},{temp,6},{temp,7},{temp,8}],
	       0,
	       [{eval,{temp,4},{icon,42}},
	        {eval,{temp,5},{binop,'=',{temp,2},{temp,4}}},
	        {eval,{temp,6},{icon,4711}},
	        {eval,{temp,7},{binop,'=',{temp,3},{temp,6}}},
	        {eval,{temp,8},{binop,'=',{temp,2},{temp,7}}}],
	       {labdef,{label,100}}}].



### ducc -t suite/quiet/parser/p02.c gives:

	{'EXIT',{badarg,[{translator_helpers,get_meta,1},
	                 {translator_helpers,get_tag,1},
	                 {translator,translate_stmt,2},
	                 {translator,translate_if,2},
	                 {translator,translate_while,2},
	                 {translator,translate_list,3},
	                 {translator,translate_list,3},
	                 {translator,translate_fundef,2}]}}



### ducc -t suite/quiet/parser/p03.c gives:

	[{proc,{label,"main"},
	       [],
	       [{temp,2},
	        {temp,3},
	        {temp,4},
	        {temp,5},
	        {temp,6},
	        {temp,7},
	        {temp,8},
	        {temp,9}],
	       0,
	       [{eval,{temp,3},{icon,1}},
	        {eval,{temp,4},{icon,2}},
	        {eval,{temp,5},{binop,'<',{temp,3},{temp,4}}},
	        {cjump,eq,{temp,5},0,{label,101}},
	        {eval,{temp,6},{icon,1}},
	        {eval,{temp,7},{binop,'=',{temp,2},{temp,6}}},
	        {jump,{label,102}},
	        {labdef,{label,101}},
	        {eval,{temp,8},{icon,2}},
	        {eval,{temp,9},{binop,'=',{temp,2},{temp,8}}},
	        {labdef,{label,102}}],
	       {labdef,{label,100}}}].



### ducc -t suite/quiet/parser/p04.c gives:

	[{data,{label,100},long},
	 {proc,{label,"foo"},[{temp,2}],[],0,[],{labdef,{label,101}}},
	 {proc,{label,"main"},
	       [],
	       [{temp,2},
	        {temp,3},
	        {temp,4},
	        {temp,5},
	        {temp,6},
	        {temp,7},
	        {temp,8},
	        {temp,9},
	        {temp,10},
	        {temp,11},
	        {temp,12},
	        {temp,13},
	        {temp,14},
	        {temp,15},
	        {temp,16},
	        {temp,17},
	        {temp,18},
	        {temp,19},
	        {temp,20},
	        {temp,21},
	        {temp,22},
	        {temp,23},
	        {temp,24},
	        {temp,25},
	        {temp,26},
	        {temp,27}],
	       0,
	       [{eval,{temp,4},{icon,10}},
	        {eval,{temp,5},{icon,2}},
	        {eval,{temp,6},{icon,2}},
	        {eval,{temp,7},{binop,'*',{temp,5},{temp,6}}},
	        {eval,{temp,8},{binop,'-',{temp,4},{temp,7}}},
	        {eval,{temp,9},{icon,1}},
	        {eval,{temp,10},{binop,'-',{temp,8},{temp,9}}},
	        {eval,{temp,11},{binop,'=',{temp,2},{temp,10}}},
	        {eval,{temp,12},{icon,100}},
	        {eval,{temp,13},{binop,'*',{temp,12},{temp,2}}},
	        {eval,{temp,14},{icon,1}},
	        {eval,{temp,15},{binop,'*',{temp,13},{temp,14}}},
	        {eval,{temp,16},{icon,71}},
	        {eval,{temp,17},{binop,'/',{temp,15},{temp,16}}},
	        {eval,{temp,18},{binop,'-',{temp,2},{temp,17}}},
	        {eval,{temp,19},{binop,'=',{temp,3},{temp,18}}},
	        {eval,{temp,20},{binop,'=',{temp,2},{temp,19}}},
	        {eval,{temp,21},{binop,'*',{temp,2},{temp,3}}},
	        {eval,{temp,22},{icon,99}},
	        {eval,{temp,23},{binop,'<',{temp,21},{temp,22}}},
	        {eval,{temp,24},{icon,0}},
	        {eval,{temp,25},{binop,'==',{temp,2},{temp,24}}},
	        {eval,{temp,26},{icon,0}},
	        {eval,{temp,27},{binop,'&&',{temp,25},{temp,26}}}],
	       {labdef,{label,102}}}].



### ducc -t suite/quiet/parser/p05.c gives:

	[{data,{label,100},long},
	 {data,{label,101},byte},
	 {data,{label,102},40},
	 {data,{label,103},10},
	 {proc,{label,"f"},
	       [{temp,2},{temp,3},{temp,4},{temp,5}],
	       [],0,[],
	       {labdef,{label,104}}},
	 {proc,{label,"main"},
	       [],
	       [{temp,6},
	        {temp,7},
	        {temp,8},
	        {temp,9},
	        {temp,10},
	        {temp,11},
	        {temp,12},
	        {temp,13},
	        {temp,14},
	        {temp,15},
	        {temp,16},
	        {temp,17},
	        {temp,18}],
	       50,
	       [{eval,{temp,8},{labref,{label,100}}},
	        {eval,{temp,9},{load,long,{temp,8}}},
	        {eval,{temp,10},{icon,0}},
	        {eval,{temp,11},{binop,'+',{temp,1},{temp,10}}},
	        {eval,{temp,12},{labref,{label,103}}},
	        {call,{temp,13},{label,"f"},[{temp,9},{temp,7},{temp,11},{temp,12}]},
	        {eval,{temp,14},{icon,5}},
	        {arrelem},
	        {eval,{temp,15},{icon,7}},
	        {arrelem},
	        {eval,{temp,16},{icon,1}},
	        {eval,{temp,17},{binop,'+',{temp,15},{temp,16}}},
	        {eval,{temp,18},{binop,'=',{temp,14},{temp,17}}}],
	       {labdef,{label,105}}}].



### ducc -t suite/quiet/rtl/r01.c gives:

	[{data,{label,100},long},
	 {data,{label,101},byte},
	 {proc,{label,"main"},
	       [],
	       [{temp,2},
	        {temp,3},
	        {temp,4},
	        {temp,5},
	        {temp,6},
	        {temp,7},
	        {temp,8},
	        {temp,9},
	        {temp,10},
	        {temp,11},
	        {temp,12},
	        {temp,13},
	        {temp,14},
	        {temp,15},
	        {temp,16},
	        {temp,17},
	        {temp,18},
	        {temp,19},
	        {temp,20},
	        {temp,21},
	        {temp,22},
	        {temp,23},
	        {temp,24},
	        {temp,25},
	        {temp,26},
	        {temp,27}],
	       0,
	       [{eval,{temp,4},{labref,{label,100}}},
	        {eval,{temp,5},{load,long,{temp,4}}},
	        {eval,{temp,6},{icon,42}},
	        {eval,{temp,7},{binop,'=',{temp,5},{temp,6}}},
	        {eval,{temp,8},{labref,{label,101}}},
	        {eval,{temp,9},{load,byte,{temp,8}}},
	        {eval,{temp,10},{icon,43}},
	        {eval,{temp,11},{binop,'=',{temp,9},{temp,10}}},
	        {eval,{temp,12},{icon,44}},
	        {eval,{temp,13},{binop,'=',{temp,2},{temp,12}}},
	        {eval,{temp,14},{icon,45}},
	        {eval,{temp,15},{binop,'=',{temp,3},{temp,14}}},
	        {eval,{temp,16},{labref,{label,100}}},
	        {eval,{temp,17},{load,long,{temp,16}}},
	        {eval,{temp,18},{icon,'A'}},
	        {eval,{temp,19},{binop,'=',{temp,17},{temp,18}}},
	        {eval,{temp,20},{labref,{label,101}}},
	        {eval,{temp,21},{load,byte,{temp,20}}},
	        {eval,{temp,22},{icon,'\\n'}},
	        {eval,{temp,23},{binop,'=',{temp,21},{temp,22}}},
	        {eval,{temp,24},{icon,'C'}},
	        {eval,{temp,25},{binop,'=',{temp,2},{temp,24}}},
	        {eval,{temp,26},{icon,'D'}},
	        {eval,{temp,27},{binop,'=',{temp,3},{temp,26}}}],
	       {labdef,{label,102}}}].



### ducc -t suite/quiet/rtl/r02.c gives:

	[{proc,{label,"f"},[{temp,2}],[],0,[],{labdef,{label,100}}},
	 [],
	 {proc,{label,"main"},
	       [],
	       [{temp,3},{temp,4},{temp,5}],
	       40,
	       [{eval,{temp,3},{icon,0}},
	        {eval,{temp,4},{binop,'+',{temp,1},{temp,3}}},
	        {call,{temp,5},{label,"f"},[{temp,4}]}],
	       {labdef,{label,101}}}].



### ducc -t suite/quiet/rtl/r03.c gives:

	[{proc,{label,"f"},[{temp,2}],[],0,[],{labdef,{label,100}}},
	 {proc,{label,"main"},
	       [],
	       [{temp,3},{temp,4},{temp,5},{temp,6},{temp,7},{temp,8}],
	       28,
	       [{eval,{temp,3},{icon,0}},
	        {arrelem},
	        {eval,{temp,4},{icon,5}},
	        {eval,{temp,5},{binop,'=',{temp,3},{temp,4}}},
	        {eval,{temp,6},{icon,0}},
	        {eval,{temp,7},{binop,'+',{temp,1},{temp,6}}},
	        {call,{temp,8},{label,"f"},[{temp,7}]}],
	       {labdef,{label,101}}}].



### ducc -t suite/quiet/rtl/r04.c gives:

	[{proc,{label,"f"},
	       [{temp,2}],
	       [{temp,3},{temp,4},{temp,5},{temp,6},{temp,7}],
	       0,
	       [{eval,{temp,3},{icon,1}},
	        {arrelem},
	        {eval,{temp,4},{icon,0}},
	        {arrelem},
	        {eval,{temp,5},{icon,7}},
	        {eval,{temp,6},{binop,'+',{temp,4},{temp,5}}},
	        {eval,{temp,7},{binop,'=',{temp,3},{temp,6}}}],
	       {labdef,{label,100}}},
	 [],
	 {proc,{label,"main"},
	       [],
	       [{temp,8},{temp,9},{temp,10},{temp,11},{temp,12},{temp,13}],
	       7,
	       [{eval,{temp,8},{icon,0}},
	        {arrelem},
	        {eval,{temp,9},{icon,5}},
	        {eval,{temp,10},{binop,'=',{temp,8},{temp,9}}},
	        {eval,{temp,11},{icon,0}},
	        {eval,{temp,12},{binop,'+',{temp,1},{temp,11}}},
	        {call,{temp,13},{label,"f"},[{temp,12}]}],
	       {labdef,{label,101}}}].



### ducc -t suite/quiet/rtl/r05.c gives:

	[{proc,{label,"f"},
	       [{temp,2}],
	       [{temp,3},{temp,4},{temp,5},{temp,6},{temp,7}],
	       0,
	       [{eval,{temp,3},{icon,1}},
	        {arrelem},
	        {eval,{temp,4},{icon,0}},
	        {arrelem},
	        {eval,{temp,5},{icon,7}},
	        {eval,{temp,6},{binop,'+',{temp,4},{temp,5}}},
	        {eval,{temp,7},{binop,'=',{temp,3},{temp,6}}}],
	       {labdef,{label,100}}},
	 {proc,{label,"main"},
	       [],
	       [{temp,8},{temp,9},{temp,10},{temp,11},{temp,12},{temp,13}],
	       9,
	       [{eval,{temp,8},{icon,0}},
	        {arrelem},
	        {eval,{temp,9},{icon,5}},
	        {eval,{temp,10},{binop,'=',{temp,8},{temp,9}}},
	        {eval,{temp,11},{icon,0}},
	        {eval,{temp,12},{binop,'+',{temp,1},{temp,11}}},
	        {call,{temp,13},{label,"f"},[{temp,12}]}],
	       {labdef,{label,101}}}].



### ducc -t suite/quiet/rtl/r06.c gives:

	{'EXIT',{badarg,[{translator_helpers,get_meta,1},
	                 {translator_helpers,get_tag,1},
	                 {translator,translate_stmt,2},
	                 {translator,translate_if,2},
	                 {translator,translate_list,3},
	                 {translator,translate_fundef,2},
	                 {translator,translate_topdecs,2},
	                 {translator,translate_topdecs,2}]}}



### ducc -t suite/quiet/rtl/r10.c gives:

	[{data,{label,100},long},
	 {data,{label,101},byte},
	 {data,{label,102},40},
	 {data,{label,103},10},
	 {proc,{label,"f"},
	       [{temp,2},{temp,3},{temp,4},{temp,5}],
	       [{temp,6},{temp,7},{temp,8},{temp,9},{temp,10},{temp,11}],
	       0,
	       [{eval,{temp,6},{icon,2}},
	        {arrelem},
	        {eval,{temp,7},{icon,3}},
	        {eval,{temp,8},{binop,'=',{temp,6},{temp,7}}},
	        {eval,{temp,9},{icon,3}},
	        {arrelem},
	        {eval,{temp,10},{icon,7}},
	        {eval,{temp,11},{binop,'=',{temp,9},{temp,10}}}],
	       {labdef,{label,104}}},
	 {proc,{label,"main"},
	       [],
	       [{temp,12},
	        {temp,13},
	        {temp,14},
	        {temp,15},
	        {temp,16},
	        {temp,17},
	        {temp,18},
	        {temp,19},
	        {temp,20},
	        {temp,21},
	        {temp,22},
	        {temp,23},
	        {temp,24},
	        {temp,25},
	        {temp,26},
	        {temp,27},
	        {temp,28},
	        {temp,29},
	        {temp,30},
	        {temp,31},
	        {temp,32},
	        {temp,33}],
	       50,
	       [{eval,{temp,14},{labref,{label,100}}},
	        {eval,{temp,15},{load,long,{temp,14}}},
	        {eval,{temp,16},{icon,0}},
	        {eval,{temp,17},{binop,'+',{temp,1},{temp,16}}},
	        {eval,{temp,18},{labref,{label,103}}},
	        {call,{temp,19},{label,"f"},[{temp,15},{temp,13},{temp,17},{temp,18}]},
	        {eval,{temp,20},{icon,4}},
	        {arrelem},
	        {eval,{temp,21},{icon,4}},
	        {arrelem},
	        {eval,{temp,22},{icon,11}},
	        {eval,{temp,23},{binop,'+',{temp,21},{temp,22}}},
	        {eval,{temp,24},{binop,'=',{temp,20},{temp,23}}},
	        {eval,{temp,25},{icon,5}},
	        {arrelem},
	        {eval,{temp,26},{icon,13}},
	        {eval,{temp,27},{binop,'=',{temp,25},{temp,26}}},
	        {eval,{temp,28},{icon,6}},
	        {arrelem},
	        {eval,{temp,29},{icon,17}},
	        {eval,{temp,30},{binop,'=',{temp,28},{temp,29}}},
	        {eval,{temp,31},{icon,7}},
	        {arrelem},
	        {eval,{temp,32},{icon,19}},
	        {eval,{temp,33},{binop,'=',{temp,31},{temp,32}}}],
	       {labdef,{label,105}}}].



### ducc -t suite/quiet/semantic/s01.c gives:

	[{data,{label,100},long},
	 {data,{label,101},byte},
	 {proc,{label,"main"},
	       [],
	       [{temp,2},
	        {temp,3},
	        {temp,4},
	        {temp,5},
	        {temp,6},
	        {temp,7},
	        {temp,8},
	        {temp,9},
	        {temp,10},
	        {temp,11},
	        {temp,12},
	        {temp,13},
	        {temp,14},
	        {temp,15},
	        {temp,16},
	        {temp,17},
	        {temp,18},
	        {temp,19},
	        {temp,20},
	        {temp,21},
	        {temp,22},
	        {temp,23},
	        {temp,24},
	        {temp,25},
	        {temp,26},
	        {temp,27},
	        {temp,28},
	        {temp,29},
	        {temp,30},
	        {temp,31},
	        {temp,32},
	        {temp,33},
	        {temp,34},
	        {temp,35},
	        {temp,36},
	        {temp,37},
	        {temp,38},
	        {temp,39},
	        {temp,40},
	        {temp,41},
	        {temp,42},
	        {temp,43},
	        {temp,44},
	        {temp,45},
	        {temp,46},
	        {temp,47},
	        {temp,48},
	        {temp,49},
	        {temp,50},
	        {temp,51},
	        {temp,52},
	        {temp,53},
	        {temp,54},
	        {temp,55},
	        {temp,56},
	        {temp,57},
	        {temp,58},
	        {temp,59}],
	       0,
	       [{eval,{temp,4},{labref,{label,100}}},
	        {eval,{temp,5},{load,long,{temp,4}}},
	        {eval,{temp,6},{labref,{label,100}}},
	        {eval,{temp,7},{load,long,{temp,6}}},
	        {eval,{temp,8},{labref,{label,101}}},
	        {eval,{temp,9},{load,byte,{temp,8}}},
	        {eval,{temp,10},{binop,'+',{temp,7},{temp,9}}},
	        {eval,{temp,11},{binop,'+',{temp,10},{temp,2}}},
	        {eval,{temp,12},{binop,'+',{temp,11},{temp,3}}},
	        {eval,{temp,13},{binop,'=',{temp,5},{temp,12}}},
	        {eval,{temp,14},{labref,{label,100}}},
	        {eval,{temp,15},{load,long,{temp,14}}},
	        {eval,{temp,16},{icon,42}},
	        {eval,{temp,17},{binop,'=',{temp,2},{temp,16}}},
	        {eval,{temp,18},{binop,'=',{temp,15},{temp,17}}},
	        {eval,{temp,19},{labref,{label,100}}},
	        {eval,{temp,20},{load,long,{temp,19}}},
	        {eval,{temp,21},{binop,'==',{temp,20},{temp,2}}},
	        {eval,{temp,22},{icon,42}},
	        {eval,{temp,23},{binop,'==',{temp,21},{temp,22}}},
	        {eval,{temp,24},{labref,{label,100}}},
	        {eval,{temp,25},{load,long,{temp,24}}},
	        {eval,{temp,26},{icon,99}},
	        {eval,{temp,27},{binop,'=',{temp,2},{temp,26}}},
	        {eval,{temp,28},{binop,'==',{temp,25},{temp,27}}},
	        {jump,{label,103}},
	        {labdef,{label,104}},
	        {eval,{temp,31},{labref,{label,100}}},
	        {eval,{temp,32},{load,long,{temp,31}}},
	        {eval,{temp,33},{icon,0}},
	        {eval,{temp,34},{binop,'=',{temp,32},{temp,33}}},
	        {labdef,{label,103}},
	        {eval,{temp,29},{labref,{label,100}}},
	        {eval,{temp,30},{load,long,{temp,29}}},
	        {cjump,neq,{temp,30},0,{label,104}},
	        {labdef,{label,105}},
	        {eval,{temp,35},{icon,123}},
	        {cjump,eq,{temp,35},0,{label,106}},
	        {eval,{temp,36},{labref,{label,101}}},
	        {eval,{temp,37},{load,byte,{temp,36}}},
	        {eval,{temp,38},{icon,4}},
	        {eval,{temp,39},{binop,'=',{temp,37},{temp,38}}},
	        {jump,{label,107}},
	        {labdef,{label,106}},
	        {eval,{temp,40},{labref,{label,101}}},
	        {eval,{temp,41},{load,byte,{temp,40}}},
	        {eval,{temp,42},{icon,7}},
	        {eval,{temp,43},{binop,'=',{temp,41},{temp,42}}},
	        {labdef,{label,107}},
	        {eval,{temp,44},{labref,{label,100}}},
	        {eval,{temp,45},{load,long,{temp,44}}},
	        {eval,{temp,46},{labref,{label,101}}},
	        {eval,{temp,47},{load,byte,{temp,46}}},
	        {eval,{temp,48},{binop,'>',{temp,45},{temp,47}}},
	        {eval,{temp,49},{binop,'=',{temp,3},{temp,48}}},
	        {eval,{temp,50},{labref,{label,101}}},
	        {eval,{temp,51},{load,byte,{temp,50}}},
	        {eval,{temp,52},{icon,0}},
	        {eval,{temp,53},{labref,{label,100}}},
	        {eval,{temp,54},{load,long,{temp,53}}},
	        {eval,{temp,55},{binop,'<',{temp,52},{temp,54}}},
	        {eval,{temp,56},{icon,10}},
	        {eval,{temp,57},{binop,'<',{temp,55},{temp,56}}},
	        {eval,{temp,58},{binop,'=',{temp,51},{temp,57}}},
	        {eval,{temp,59},{icon,42}}],
	       {labdef,{label,102}}}].



### ducc -t suite/quiet/semantic/s02.c gives:

	[{data,{label,100},long},
	 {proc,{label,"foo"},[{temp,2}],[],0,[],{labdef,{label,101}}},
	 {proc,{label,"main"},
	       [],
	       [{temp,3},{temp,4},{temp,5},{temp,6},{temp,7},{temp,8}],
	       0,
	       [{eval,{temp,3},{labref,{label,100}}},
	        {eval,{temp,4},{load,long,{temp,3}}},
	        {eval,{temp,5},{labref,{label,100}}},
	        {eval,{temp,6},{load,long,{temp,5}}},
	        {call,{temp,7},{label,"foo"},[{temp,6}]},
	        {eval,{temp,8},{binop,'=',{temp,4},{temp,7}}}],
	       {labdef,{label,102}}}].



### ducc -t suite/quiet/semantic/s03.c gives:

	[{data,{label,100},40},
	 {proc,{label,"foo"},
	       [{temp,2}],
	       [{temp,3}],
	       0,
	       [{eval,{temp,3},{icon,0}},{arrelem},{jump,{label,101}}],
	       {labdef,{label,101}}},
	 {proc,{label,"main"},
	       [],
	       [{temp,4},{temp,5}],
	       0,
	       [{eval,{temp,4},{labref,{label,100}}},
	        {call,{temp,5},{label,"foo"},[{temp,4}]}],
	       {labdef,{label,102}}}].



### ducc -t suite/quiet/semantic/s04.c gives:

	[{proc,{label,"foo"},
	       [{temp,2}],
	       [{temp,3}],
	       0,
	       [{eval,{temp,3},{icon,0}},{arrelem},{jump,{label,100}}],
	       {labdef,{label,100}}},
	 {proc,{label,"main"},
	       [],
	       [{temp,4},{temp,5},{temp,6}],
	       10,
	       [{eval,{temp,4},{icon,0}},
	        {eval,{temp,5},{binop,'+',{temp,1},{temp,4}}},
	        {call,{temp,6},{label,"foo"},[{temp,5}]}],
	       {labdef,{label,101}}}].



### ducc -t suite/quiet/semantic/s05.c gives:

	[{proc,{label,"main"},
	       [],
	       [{temp,2},{temp,3},{temp,4},{temp,5},{temp,6}],
	       27,
	       [{eval,{temp,3},{icon,0}},
	        {arrelem},
	        {eval,{temp,4},{icon,a}},
	        {eval,{temp,5},{binop,'+',{temp,4},{temp,2}}},
	        {eval,{temp,6},{binop,'=',{temp,3},{temp,5}}}],
	       {labdef,{label,100}}}].



### ducc -t suite/quiet/semantic/s06.c gives:

	[{proc,{label,"main"},
	       [],
	       [{temp,2},{temp,3},{temp,4},{temp,5}],
	       0,
	       [{eval,{temp,4},{binop,'=',{temp,2},{temp,3}}},
	        {eval,{temp,5},{binop,'=',{temp,3},{temp,2}}}],
	       {labdef,{label,100}}}].



### ducc -t suite/uc.c gives:

	suite/uc.c:1: lexical error, illegal: #





module "lisp.parse", package.seeall

require "lulpeg"
require "pl"

import run_with_scope, p from require "moon"
import P, V, S, R, C, Cc, Ct from lpeg

White = S" \t\r\n,"^0
Comment =  ";" * (1 - S"\r\n")^0 * S"\r\n"
White = White * (Comment * White)^0
sym = (s) -> White * s

gensym = do
	gen = coroutine.create ->
		symbols = 0
		while true
			coroutine.yield '__sym'..symbols
			symbols = symbols + 1
	->
		_, num = coroutine.resume(gen)
		num

form = {}
export EDNForm = (obj) -> obj[form]

mark = (name) -> (...) ->
	table = { ... }
	table[form] = name
	table

auto_variable = (fn) ->
  run_with_scope fn, setmetatable {}, {
    __index: (name) =>
      V name if name\match "^[A-Z]"
  }

keyword = (token) -> {token[1], [form]: "keyword"}
float = (token) -> {tonumber(token), [form]: "float"}
integer = (token) -> {tonumber(token), [form]: "integer"}

bool = (token) ->
	val = if token == 'true'
		true
	else
		false
	mark"boolean" val

tag = (token) -> mark"tag" token[1]

map = (token) ->
	tbl = {}
	for i = 1, #token, 2
		tbl[token[i]] = token[i+1]
	tbl[form] = "map"
	tbl

list = (name) -> (token) ->
	token[form] = name
	token

parser = auto_variable ->
  P {
    Code

    Code: Ct(Value^0) * White * -1

    Integer: White * C(R"09"^1) / integer
    Float: White * C(Integer * sym"." * Integer) / float
    Nil: White * C("nil") / mark"nil"
    Boolean: White * C(sym"true" + sym"false") / bool

    Symbol: White * C((R("az", "AZ", "09") + S".*+!-_?$%&/=<>")^1) / mark"symbol"
		
    Keyword: sym":" * Symbol / keyword
    Characters: White * C(sym"\\" * R("az", "AZ", "09")^1) / mark"chars"
    String: sym'"' * C((P"\\\\" + '\\"' + (1 - S'"\r\n'))^0) * '"' / mark"string"
    Quote: White * (P"'" + "`") * Value / mark"quote"
    Unquote: sym"," * (P"@" * Value / mark"unquote_splice" + Value / mark"unquote")
    Tag: sym"#" * Symbol / tag

    SExp: sym"(" * Ct(Value^0) * sym")" / list"list"
    Vector: sym"[" * Ct(Value^0) * sym"]" / list"vector"
    Map: sym"{" * Ct(Value^0) * sym"}" / map
    Set: sym"#" * sym"{" * Ct(Value^0) * sym"}" / list"set"

    Value: Float + Integer + Nil + Boolean + String + SExp + Vector + Set + Map + Quote + Unquote + Tag + Symbol + Characters + Keyword
  }

export parse = (edn_code) ->
  parser\match edn_code

-- generate code for
-- defun
-- deftun
-- defmacro
-- unquote
-- quasiquote
-- setv
-- let
-- var
-- struct
-- vec
-- do
-- destr
-- get/set
-- lambda
-- rest args
-- for
-- while
-- loop
-- with
-- list-comp
-- yield (iterators)
-- co (coroutine scheduler)
-- thunk
-- gensym

ast = parse [[
	(import print)
	(import unpack)

	(setv foo "bar")
	(print foo)

	(defn add [a, b]
		(+ a b))
	
	(print (add 1 1))

	(defn alwaysTrue [] false)

	(while (== (alwaysTrue) true) (print "awesomesauce"))
]]

print pretty.write ast
-- todo ast should be instances of table with
-- metatable that returns primitives on index accesses
-- according to form parameter
operators = {
	'+': true,
	'-': true,
	'*': true,
	'/': true,
	'%': true,
	'^': true,
	'#': true,
	'==': true,
	'~=': true,
	'<=': true,
	'>=': true,
	'<': true,
	'>': true
}
compiler = {
	symbol: (node) =>
		ref = gensym()
		code = 'local '..ref..' = '..node[1]..'\n'
		return code, ref
	
	tag: (node) =>
	
	keyword: (node) =>
		ref = gensym()
		code = 'local '..ref..' = "'..node[1]..'"\n'
		return code, ref
	
	string: (node) =>
		ref = gensym()
		code = 'local '..ref..' = "'..node[1]..'"\n'
		return code, ref

	float: (node) =>
		ref = gensym()
		code = 'local '..ref..' = '..tostring(node[1])..'\n'
		return code, ref
		
	integer: (node) =>
		ref = gensym()
		code = 'local '..ref..' = '..tostring(node[1])..'\n'
		return code, ref
	
	map: (node) =>
		code = ''
		obj = '{ '
		ref = gensym()
		for key, value in pairs node
			if key == form
				continue
			keyCode, keyRef = self[EDNForm(key)](self, key)
			valCode, valRef = self[EDNForm(value)](self, value)
			code = code..keyCode..valCode
			obj = obj..'['..keyRef..']'..' = '..valRef..','
		obj = obj\sub(1, -2)..'}'
		code = code..'local '..ref..' = '..obj..'\n'
		return code, ref
	
	vector: (node) =>
		code = ''
		obj = '{ '
		ref = gensym()
		for i = 1, #node
			valCode, valRef = self[EDNForm(node[i])](self, node[i])
			code = code..valCode
			obj = obj..valRef..','
		obj = obj\sub(1, -2)..'}'
		code = code..'local '..ref..' = '..obj..'\n'
		return code, ref
	
	list: (node) =>
		if operators[node[1][1]]
			-- binary operator
			expr = {}
			code = ''
			ref = gensym()
			for i = 2, #node
				valCode, valRef = self[EDNForm(node[i])](self, node[i])
				code = code..valCode
				table.insert(expr, valRef)
			string = table.concat(expr, ' '..node[1][1]..' ')
			code = code..'local '..ref..' = '..string..'\n'
			return code, ref
		switch node[1][1]
			-- function definition
			when "defn"
				func = 'function '
				func = func..node[2][1]..'( '
				for i = 1, #node[3]
					func = func..node[3][i][1]..','
				func = func\sub(1, -2)..')\n'
				body, ref = self[EDNForm(node[4])](self, node[4])
				func = func..body..'return '..ref..'\nend\n'
				return func, node[2][1]
			when "setv"
				code = ''
				ref = node[2][1]
				valCode, valRef = self[EDNForm(node[3])](self, node[3])
				code = code..valCode
				code = code..ref..' = '..valRef..'\n'
				return code, ref
			when "set"
				code = ''
				ref = gensym()
				target = node[2][1]
				indexCode, indexRef = self[EDNForm(node[3])](self, node[3])
				valCode, valRef = self[EDNForm(node[4])](self, node[4])
				code = code..indexCode..valCode
				code = code..target..'['..indexRef..'] = '..valRef..'\n'
				return code, ref
			when "get"
				code = ''
				ref = gensym()
				target = node[2][1]
				indexCode, indexRef = self[EDNForm(node[3])](self, node[3])
				code = code..indexCode
				code = code..'local '..ref..' = '..target..'['..indexRef..']\n'
				return code, ref
			when "lambda"
				ref = gensym()
				func = 'local '..ref..' = '..'function('
				for i = 1, #node[2]
					func = func..node[2][i][1]..','
				func = func\sub(1, -2)..')\n'
				body, bodyRef = self[EDNForm(node[3])](self, node[3])
				func = func..body..'return '..bodyRef..'\nend\n'
				return func, ref
			when "len"
				ref = gensym()
				code = ''
				targetCode, targetRef = self[EDNForm(node[2])](self, node[2])
				code = code..targetCode
				code = code..'local '..ref..' = #'..targetRef..'\n'
				return code, ref
			when "return"
				code = ''
				targetCode, targetRef = self[EDNForm(node[2])](self, node[2])
				return
			when "let"
				return
			when "do"
				return
			when "for"
				code = ''
				vars = {}
				table.insert(vars, node[2][1][1])
				local iterCode, iterRef
				if EDNForm(node[2][2]) == 'symbol'
					table.insert(vars, node[2][2][1])
					iterCode, iterRef = self[EDNForm(node[2][3])](self, node[2][3])
				else
					iterCode, iterRef = self[EDNForm(node[2][2])](self, node[2][2])
				code = code..iterCode..'for '..table.concat(vars, ',')
				code = code..' in '..iterRef..' do\n'
				bodyCode, bodyRef = self[EDNForm(node[3])](self, node[3])
				code = code..bodyCode..'end\n'
				return code, 'nil'
			when "while"
				code = ''
				condCode, condRef = self[EDNForm(node[2])](self, node[2])
				condFuncRef= gensym()
				condFunc = 'local function '..condFuncRef..'()\n'
				condFunc = condFunc..condCode..'return '..condRef..'\nend\n'
				code = code..condFunc
				code = code..'while '..condFuncRef..'() do\n'
				bodyCode, bodyRef = self[EDNForm(node[3])](self, node[3])
				code = code..bodyCode..'end\n'
				return code, 'nil'
			when "repeat"
				return
			when "if"
				-- conditional
				code = ''
				ref = gensym()
				condCode, condRef = self[EDNForm(node[2])](self, node[2])
				trueCode, trueRef = self[EDNForm(node[3])](self, node[3])
				local falseCode, falseRef
				if node[4]
					falseCode, falseRef = self[EDNForm(node[4])](self, node[4])
				code = code..condCode..'local '..ref..'\nif '..condRef..' then\n'
				code = code..trueCode..ref..' = '..trueRef..'\n'
				code = code..'else\n'..falseCode..ref..' = '..falseRef..'\n'
				code = code..'end\n'
				return code, ref
			else
				-- function call
				code, funcRef = self[EDNForm(node[1])](self, node[1])
				ref = gensym()
				args = {}
				for i = 2, #node
					argCode, argRef = self[EDNForm(node[i])](self, node[i])
					code = code..argCode
					table.insert(args, argRef)
				args = table.concat(args, ',')
				code = code..'local '..ref..' = '..funcRef..'('..args..')\n'
				return code, ref
	
	set: (node) =>
		self.vector(self, node)
	
	boolean: (node) =>
		ref = gensym()
		code = 'local '..ref..' = '..tostring(node[1])..'\n'
		return code, ref
	
	characters: (node) =>
	
	nil: (node) =>
		ref = gensym()
		code = 'local '..ref..' = '..tostring(node[1])..'\n'
		return code, ref
	
	quote: (node) =>
	
	unquote: (node) =>
}

findImports = (ast) ->
	imports = {}
	for i = #ast, 1, -1
		node = ast[i]
		if EDNForm(node[1]) == 'symbol' and node[1][1] == 'import'
			if node[3]
				imports[node[2][1]] = node[3][1]
			else
				imports[node[2][1]] = node[2][1]
			table.remove(ast, i)
	imports

code = [[
local P = {}
pack = P
-- Import Section:
-- declare everything this package needs from outside
local require = require
]]

imports = findImports(ast)
for name, val in pairs(imports)
	code = code..'local '..name..' = '..val..'\n'

code = code..[[
-- no more external access after this point
setfenv(1, P)

]]


for i = 1, #ast
	code = code..compiler[EDNForm(ast[i])](compiler, ast[i])..'\n'

code = code..'return P'

print code..'\n----'

loadstring(code)()

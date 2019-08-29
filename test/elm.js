(function(scope){
'use strict';

function F(arity, fun, wrapper) {
  wrapper.a = arity;
  wrapper.f = fun;
  return wrapper;
}

function F2(fun) {
  return F(2, fun, function(a) { return function(b) { return fun(a,b); }; })
}
function F3(fun) {
  return F(3, fun, function(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  });
}
function F4(fun) {
  return F(4, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  });
}
function F5(fun) {
  return F(5, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  });
}
function F6(fun) {
  return F(6, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  });
}
function F7(fun) {
  return F(7, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  });
}
function F8(fun) {
  return F(8, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  });
}
function F9(fun) {
  return F(9, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  });
}

function A2(fun, a, b) {
  return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
}
function A3(fun, a, b, c) {
  return fun.a === 3 ? fun.f(a, b, c) : fun(a)(b)(c);
}
function A4(fun, a, b, c, d) {
  return fun.a === 4 ? fun.f(a, b, c, d) : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e) {
  return fun.a === 5 ? fun.f(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f) {
  return fun.a === 6 ? fun.f(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g) {
  return fun.a === 7 ? fun.f(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h) {
  return fun.a === 8 ? fun.f(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i) {
  return fun.a === 9 ? fun.f(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}

console.warn('Compiled in DEV mode. Follow the advice at https://elm-lang.org/0.19.0/optimize for better performance and smaller assets.');


var _List_Nil_UNUSED = { $: 0 };
var _List_Nil = { $: '[]' };

function _List_Cons_UNUSED(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons(hd, tl) { return { $: '::', a: hd, b: tl }; }


var _List_cons = F2(_List_Cons);

function _List_fromArray(arr)
{
	var out = _List_Nil;
	for (var i = arr.length; i--; )
	{
		out = _List_Cons(arr[i], out);
	}
	return out;
}

function _List_toArray(xs)
{
	for (var out = []; xs.b; xs = xs.b) // WHILE_CONS
	{
		out.push(xs.a);
	}
	return out;
}

var _List_map2 = F3(function(f, xs, ys)
{
	for (var arr = []; xs.b && ys.b; xs = xs.b, ys = ys.b) // WHILE_CONSES
	{
		arr.push(A2(f, xs.a, ys.a));
	}
	return _List_fromArray(arr);
});

var _List_map3 = F4(function(f, xs, ys, zs)
{
	for (var arr = []; xs.b && ys.b && zs.b; xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A3(f, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map4 = F5(function(f, ws, xs, ys, zs)
{
	for (var arr = []; ws.b && xs.b && ys.b && zs.b; ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A4(f, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map5 = F6(function(f, vs, ws, xs, ys, zs)
{
	for (var arr = []; vs.b && ws.b && xs.b && ys.b && zs.b; vs = vs.b, ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A5(f, vs.a, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_sortBy = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		return _Utils_cmp(f(a), f(b));
	}));
});

var _List_sortWith = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		var ord = A2(f, a, b);
		return ord === elm$core$Basics$EQ ? 0 : ord === elm$core$Basics$LT ? -1 : 1;
	}));
});



// EQUALITY

function _Utils_eq(x, y)
{
	for (
		var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack);
		isEqual && (pair = stack.pop());
		isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack)
		)
	{}

	return isEqual;
}

function _Utils_eqHelp(x, y, depth, stack)
{
	if (depth > 100)
	{
		stack.push(_Utils_Tuple2(x,y));
		return true;
	}

	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object' || x === null || y === null)
	{
		typeof x === 'function' && _Debug_crash(5);
		return false;
	}

	/**/
	if (x.$ === 'Set_elm_builtin')
	{
		x = elm$core$Set$toList(x);
		y = elm$core$Set$toList(y);
	}
	if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
	{
		x = elm$core$Dict$toList(x);
		y = elm$core$Dict$toList(y);
	}
	//*/

	/**_UNUSED/
	if (x.$ < 0)
	{
		x = elm$core$Dict$toList(x);
		y = elm$core$Dict$toList(y);
	}
	//*/

	for (var key in x)
	{
		if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

var _Utils_equal = F2(_Utils_eq);
var _Utils_notEqual = F2(function(a, b) { return !_Utils_eq(a,b); });



// COMPARISONS

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

function _Utils_cmp(x, y, ord)
{
	if (typeof x !== 'object')
	{
		return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
	}

	/**/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**_UNUSED/
	if (typeof x.$ === 'undefined')
	//*/
	/**/
	if (x.$[0] === '#')
	//*/
	{
		return (ord = _Utils_cmp(x.a, y.a))
			? ord
			: (ord = _Utils_cmp(x.b, y.b))
				? ord
				: _Utils_cmp(x.c, y.c);
	}

	// traverse conses until end of a list or a mismatch
	for (; x.b && y.b && !(ord = _Utils_cmp(x.a, y.a)); x = x.b, y = y.b) {} // WHILE_CONSES
	return ord || (x.b ? /*GT*/ 1 : y.b ? /*LT*/ -1 : /*EQ*/ 0);
}

var _Utils_lt = F2(function(a, b) { return _Utils_cmp(a, b) < 0; });
var _Utils_le = F2(function(a, b) { return _Utils_cmp(a, b) < 1; });
var _Utils_gt = F2(function(a, b) { return _Utils_cmp(a, b) > 0; });
var _Utils_ge = F2(function(a, b) { return _Utils_cmp(a, b) >= 0; });

var _Utils_compare = F2(function(x, y)
{
	var n = _Utils_cmp(x, y);
	return n < 0 ? elm$core$Basics$LT : n ? elm$core$Basics$GT : elm$core$Basics$EQ;
});


// COMMON VALUES

var _Utils_Tuple0_UNUSED = 0;
var _Utils_Tuple0 = { $: '#0' };

function _Utils_Tuple2_UNUSED(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3_UNUSED(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr_UNUSED(c) { return c; }
function _Utils_chr(c) { return new String(c); }


// RECORDS

function _Utils_update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


// APPEND

var _Utils_append = F2(_Utils_ap);

function _Utils_ap(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (!xs.b)
	{
		return ys;
	}
	var root = _List_Cons(xs.a, ys);
	xs = xs.b
	for (var curr = root; xs.b; xs = xs.b) // WHILE_CONS
	{
		curr = curr.b = _List_Cons(xs.a, ys);
	}
	return root;
}



var _JsArray_empty = [];

function _JsArray_singleton(value)
{
    return [value];
}

function _JsArray_length(array)
{
    return array.length;
}

var _JsArray_initialize = F3(function(size, offset, func)
{
    var result = new Array(size);

    for (var i = 0; i < size; i++)
    {
        result[i] = func(offset + i);
    }

    return result;
});

var _JsArray_initializeFromList = F2(function (max, ls)
{
    var result = new Array(max);

    for (var i = 0; i < max && ls.b; i++)
    {
        result[i] = ls.a;
        ls = ls.b;
    }

    result.length = i;
    return _Utils_Tuple2(result, ls);
});

var _JsArray_unsafeGet = F2(function(index, array)
{
    return array[index];
});

var _JsArray_unsafeSet = F3(function(index, value, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[index] = value;
    return result;
});

var _JsArray_push = F2(function(value, array)
{
    var length = array.length;
    var result = new Array(length + 1);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[length] = value;
    return result;
});

var _JsArray_foldl = F3(function(func, acc, array)
{
    var length = array.length;

    for (var i = 0; i < length; i++)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_foldr = F3(function(func, acc, array)
{
    for (var i = array.length - 1; i >= 0; i--)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_map = F2(function(func, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = func(array[i]);
    }

    return result;
});

var _JsArray_indexedMap = F3(function(func, offset, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = A2(func, offset + i, array[i]);
    }

    return result;
});

var _JsArray_slice = F3(function(from, to, array)
{
    return array.slice(from, to);
});

var _JsArray_appendN = F3(function(n, dest, source)
{
    var destLen = dest.length;
    var itemsToCopy = n - destLen;

    if (itemsToCopy > source.length)
    {
        itemsToCopy = source.length;
    }

    var size = destLen + itemsToCopy;
    var result = new Array(size);

    for (var i = 0; i < destLen; i++)
    {
        result[i] = dest[i];
    }

    for (var i = 0; i < itemsToCopy; i++)
    {
        result[i + destLen] = source[i];
    }

    return result;
});



// LOG

var _Debug_log_UNUSED = F2(function(tag, value)
{
	return value;
});

var _Debug_log = F2(function(tag, value)
{
	console.log(tag + ': ' + _Debug_toString(value));
	return value;
});


// TODOS

function _Debug_todo(moduleName, region)
{
	return function(message) {
		_Debug_crash(8, moduleName, region, message);
	};
}

function _Debug_todoCase(moduleName, region, value)
{
	return function(message) {
		_Debug_crash(9, moduleName, region, value, message);
	};
}


// TO STRING

function _Debug_toString_UNUSED(value)
{
	return '<internals>';
}

function _Debug_toString(value)
{
	return _Debug_toAnsiString(false, value);
}

function _Debug_toAnsiString(ansi, value)
{
	if (typeof value === 'function')
	{
		return _Debug_internalColor(ansi, '<function>');
	}

	if (typeof value === 'boolean')
	{
		return _Debug_ctorColor(ansi, value ? 'True' : 'False');
	}

	if (typeof value === 'number')
	{
		return _Debug_numberColor(ansi, value + '');
	}

	if (value instanceof String)
	{
		return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
	}

	if (typeof value === 'string')
	{
		return _Debug_stringColor(ansi, '"' + _Debug_addSlashes(value, false) + '"');
	}

	if (typeof value === 'object' && '$' in value)
	{
		var tag = value.$;

		if (typeof tag === 'number')
		{
			return _Debug_internalColor(ansi, '<internals>');
		}

		if (tag[0] === '#')
		{
			var output = [];
			for (var k in value)
			{
				if (k === '$') continue;
				output.push(_Debug_toAnsiString(ansi, value[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (tag === 'Set_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Set')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, elm$core$Set$toList(value));
		}

		if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Dict')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, elm$core$Dict$toList(value));
		}

		if (tag === 'Array_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Array')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, elm$core$Array$toList(value));
		}

		if (tag === '::' || tag === '[]')
		{
			var output = '[';

			value.b && (output += _Debug_toAnsiString(ansi, value.a), value = value.b)

			for (; value.b; value = value.b) // WHILE_CONS
			{
				output += ',' + _Debug_toAnsiString(ansi, value.a);
			}
			return output + ']';
		}

		var output = '';
		for (var i in value)
		{
			if (i === '$') continue;
			var str = _Debug_toAnsiString(ansi, value[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '[' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return _Debug_ctorColor(ansi, tag) + output;
	}

	if (typeof DataView === 'function' && value instanceof DataView)
	{
		return _Debug_stringColor(ansi, '<' + value.byteLength + ' bytes>');
	}

	if (typeof File === 'function' && value instanceof File)
	{
		return _Debug_internalColor(ansi, '<' + value.name + '>');
	}

	if (typeof value === 'object')
	{
		var output = [];
		for (var key in value)
		{
			var field = key[0] === '_' ? key.slice(1) : key;
			output.push(_Debug_fadeColor(ansi, field) + ' = ' + _Debug_toAnsiString(ansi, value[key]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return _Debug_internalColor(ansi, '<internals>');
}

function _Debug_addSlashes(str, isChar)
{
	var s = str
		.replace(/\\/g, '\\\\')
		.replace(/\n/g, '\\n')
		.replace(/\t/g, '\\t')
		.replace(/\r/g, '\\r')
		.replace(/\v/g, '\\v')
		.replace(/\0/g, '\\0');

	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}

function _Debug_ctorColor(ansi, string)
{
	return ansi ? '\x1b[96m' + string + '\x1b[0m' : string;
}

function _Debug_numberColor(ansi, string)
{
	return ansi ? '\x1b[95m' + string + '\x1b[0m' : string;
}

function _Debug_stringColor(ansi, string)
{
	return ansi ? '\x1b[93m' + string + '\x1b[0m' : string;
}

function _Debug_charColor(ansi, string)
{
	return ansi ? '\x1b[92m' + string + '\x1b[0m' : string;
}

function _Debug_fadeColor(ansi, string)
{
	return ansi ? '\x1b[37m' + string + '\x1b[0m' : string;
}

function _Debug_internalColor(ansi, string)
{
	return ansi ? '\x1b[94m' + string + '\x1b[0m' : string;
}

function _Debug_toHexDigit(n)
{
	return String.fromCharCode(n < 10 ? 48 + n : 55 + n);
}


// CRASH


function _Debug_crash_UNUSED(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash(identifier, fact1, fact2, fact3, fact4)
{
	switch(identifier)
	{
		case 0:
			throw new Error('What node should I take over? In JavaScript I need something like:\n\n    Elm.Main.init({\n        node: document.getElementById("elm-node")\n    })\n\nYou need to do this with any Browser.sandbox or Browser.element program.');

		case 1:
			throw new Error('Browser.application programs cannot handle URLs like this:\n\n    ' + document.location.href + '\n\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.');

		case 2:
			var jsonErrorString = fact1;
			throw new Error('Problem with the flags given to your Elm program on initialization.\n\n' + jsonErrorString);

		case 3:
			var portName = fact1;
			throw new Error('There can only be one port named `' + portName + '`, but your program has multiple.');

		case 4:
			var portName = fact1;
			var problem = fact2;
			throw new Error('Trying to send an unexpected type of value through port `' + portName + '`:\n' + problem);

		case 5:
			throw new Error('Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Elm sense.\nRead more about this at https://package.elm-lang.org/packages/elm/core/latest/Basics#== which describes why it is this way and what the better version will look like.');

		case 6:
			var moduleName = fact1;
			throw new Error('Your page is loading multiple Elm scripts with a module named ' + moduleName + '. Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!');

		case 8:
			var moduleName = fact1;
			var region = fact2;
			var message = fact3;
			throw new Error('TODO in module `' + moduleName + '` ' + _Debug_regionToString(region) + '\n\n' + message);

		case 9:
			var moduleName = fact1;
			var region = fact2;
			var value = fact3;
			var message = fact4;
			throw new Error(
				'TODO in module `' + moduleName + '` from the `case` expression '
				+ _Debug_regionToString(region) + '\n\nIt received the following value:\n\n    '
				+ _Debug_toString(value).replace('\n', '\n    ')
				+ '\n\nBut the branch that handles it says:\n\n    ' + message.replace('\n', '\n    ')
			);

		case 10:
			throw new Error('Bug in https://github.com/elm/virtual-dom/issues');

		case 11:
			throw new Error('Cannot perform mod 0. Division by zero error.');
	}
}

function _Debug_regionToString(region)
{
	if (region.start.line === region.end.line)
	{
		return 'on line ' + region.start.line;
	}
	return 'on lines ' + region.start.line + ' through ' + region.end.line;
}



// MATH

var _Basics_add = F2(function(a, b) { return a + b; });
var _Basics_sub = F2(function(a, b) { return a - b; });
var _Basics_mul = F2(function(a, b) { return a * b; });
var _Basics_fdiv = F2(function(a, b) { return a / b; });
var _Basics_idiv = F2(function(a, b) { return (a / b) | 0; });
var _Basics_pow = F2(Math.pow);

var _Basics_remainderBy = F2(function(b, a) { return a % b; });

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
var _Basics_modBy = F2(function(modulus, x)
{
	var answer = x % modulus;
	return modulus === 0
		? _Debug_crash(11)
		:
	((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0))
		? answer + modulus
		: answer;
});


// TRIGONOMETRY

var _Basics_pi = Math.PI;
var _Basics_e = Math.E;
var _Basics_cos = Math.cos;
var _Basics_sin = Math.sin;
var _Basics_tan = Math.tan;
var _Basics_acos = Math.acos;
var _Basics_asin = Math.asin;
var _Basics_atan = Math.atan;
var _Basics_atan2 = F2(Math.atan2);


// MORE MATH

function _Basics_toFloat(x) { return x; }
function _Basics_truncate(n) { return n | 0; }
function _Basics_isInfinite(n) { return n === Infinity || n === -Infinity; }

var _Basics_ceiling = Math.ceil;
var _Basics_floor = Math.floor;
var _Basics_round = Math.round;
var _Basics_sqrt = Math.sqrt;
var _Basics_log = Math.log;
var _Basics_isNaN = isNaN;


// BOOLEANS

function _Basics_not(bool) { return !bool; }
var _Basics_and = F2(function(a, b) { return a && b; });
var _Basics_or  = F2(function(a, b) { return a || b; });
var _Basics_xor = F2(function(a, b) { return a !== b; });



function _Char_toCode(char)
{
	var code = char.charCodeAt(0);
	if (0xD800 <= code && code <= 0xDBFF)
	{
		return (code - 0xD800) * 0x400 + char.charCodeAt(1) - 0xDC00 + 0x10000
	}
	return code;
}

function _Char_fromCode(code)
{
	return _Utils_chr(
		(code < 0 || 0x10FFFF < code)
			? '\uFFFD'
			:
		(code <= 0xFFFF)
			? String.fromCharCode(code)
			:
		(code -= 0x10000,
			String.fromCharCode(Math.floor(code / 0x400) + 0xD800, code % 0x400 + 0xDC00)
		)
	);
}

function _Char_toUpper(char)
{
	return _Utils_chr(char.toUpperCase());
}

function _Char_toLower(char)
{
	return _Utils_chr(char.toLowerCase());
}

function _Char_toLocaleUpper(char)
{
	return _Utils_chr(char.toLocaleUpperCase());
}

function _Char_toLocaleLower(char)
{
	return _Utils_chr(char.toLocaleLowerCase());
}



var _String_cons = F2(function(chr, str)
{
	return chr + str;
});

function _String_uncons(string)
{
	var word = string.charCodeAt(0);
	return word
		? elm$core$Maybe$Just(
			0xD800 <= word && word <= 0xDBFF
				? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2))
				: _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))
		)
		: elm$core$Maybe$Nothing;
}

var _String_append = F2(function(a, b)
{
	return a + b;
});

function _String_length(str)
{
	return str.length;
}

var _String_map = F2(function(func, string)
{
	var len = string.length;
	var array = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = string.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			array[i] = func(_Utils_chr(string[i] + string[i+1]));
			i += 2;
			continue;
		}
		array[i] = func(_Utils_chr(string[i]));
		i++;
	}
	return array.join('');
});

var _String_filter = F2(function(isGood, str)
{
	var arr = [];
	var len = str.length;
	var i = 0;
	while (i < len)
	{
		var char = str[i];
		var word = str.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += str[i];
			i++;
		}

		if (isGood(_Utils_chr(char)))
		{
			arr.push(char);
		}
	}
	return arr.join('');
});

function _String_reverse(str)
{
	var len = str.length;
	var arr = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = str.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			arr[len - i] = str[i + 1];
			i++;
			arr[len - i] = str[i - 1];
			i++;
		}
		else
		{
			arr[len - i] = str[i];
			i++;
		}
	}
	return arr.join('');
}

var _String_foldl = F3(function(func, state, string)
{
	var len = string.length;
	var i = 0;
	while (i < len)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += string[i];
			i++;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_foldr = F3(function(func, state, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_split = F2(function(sep, str)
{
	return str.split(sep);
});

var _String_join = F2(function(sep, strs)
{
	return strs.join(sep);
});

var _String_slice = F3(function(start, end, str) {
	return str.slice(start, end);
});

function _String_trim(str)
{
	return str.trim();
}

function _String_trimLeft(str)
{
	return str.replace(/^\s+/, '');
}

function _String_trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function _String_words(str)
{
	return _List_fromArray(str.trim().split(/\s+/g));
}

function _String_lines(str)
{
	return _List_fromArray(str.split(/\r\n|\r|\n/g));
}

function _String_toUpper(str)
{
	return str.toUpperCase();
}

function _String_toLower(str)
{
	return str.toLowerCase();
}

var _String_any = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (isGood(_Utils_chr(char)))
		{
			return true;
		}
	}
	return false;
});

var _String_all = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (!isGood(_Utils_chr(char)))
		{
			return false;
		}
	}
	return true;
});

var _String_contains = F2(function(sub, str)
{
	return str.indexOf(sub) > -1;
});

var _String_startsWith = F2(function(sub, str)
{
	return str.indexOf(sub) === 0;
});

var _String_endsWith = F2(function(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
});

var _String_indexes = F2(function(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _List_Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _List_fromArray(is);
});


// TO STRING

function _String_fromNumber(number)
{
	return number + '';
}


// INT CONVERSIONS

function _String_toInt(str)
{
	var total = 0;
	var code0 = str.charCodeAt(0);
	var start = code0 == 0x2B /* + */ || code0 == 0x2D /* - */ ? 1 : 0;

	for (var i = start; i < str.length; ++i)
	{
		var code = str.charCodeAt(i);
		if (code < 0x30 || 0x39 < code)
		{
			return elm$core$Maybe$Nothing;
		}
		total = 10 * total + code - 0x30;
	}

	return i == start
		? elm$core$Maybe$Nothing
		: elm$core$Maybe$Just(code0 == 0x2D ? -total : total);
}


// FLOAT CONVERSIONS

function _String_toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return elm$core$Maybe$Nothing;
	}
	var n = +s;
	// faster isNaN check
	return n === n ? elm$core$Maybe$Just(n) : elm$core$Maybe$Nothing;
}

function _String_fromList(chars)
{
	return _List_toArray(chars).join('');
}




/**/
function _Json_errorToString(error)
{
	return elm$json$Json$Decode$errorToString(error);
}
//*/


// CORE DECODERS

function _Json_succeed(msg)
{
	return {
		$: 0,
		a: msg
	};
}

function _Json_fail(msg)
{
	return {
		$: 1,
		a: msg
	};
}

var _Json_decodeInt = { $: 2 };
var _Json_decodeBool = { $: 3 };
var _Json_decodeFloat = { $: 4 };
var _Json_decodeValue = { $: 5 };
var _Json_decodeString = { $: 6 };

function _Json_decodeList(decoder) { return { $: 7, b: decoder }; }
function _Json_decodeArray(decoder) { return { $: 8, b: decoder }; }

function _Json_decodeNull(value) { return { $: 9, c: value }; }

var _Json_decodeField = F2(function(field, decoder)
{
	return {
		$: 10,
		d: field,
		b: decoder
	};
});

var _Json_decodeIndex = F2(function(index, decoder)
{
	return {
		$: 11,
		e: index,
		b: decoder
	};
});

function _Json_decodeKeyValuePairs(decoder)
{
	return {
		$: 12,
		b: decoder
	};
}

function _Json_mapMany(f, decoders)
{
	return {
		$: 13,
		f: f,
		g: decoders
	};
}

var _Json_andThen = F2(function(callback, decoder)
{
	return {
		$: 14,
		b: decoder,
		h: callback
	};
});

function _Json_oneOf(decoders)
{
	return {
		$: 15,
		g: decoders
	};
}


// DECODING OBJECTS

var _Json_map1 = F2(function(f, d1)
{
	return _Json_mapMany(f, [d1]);
});

var _Json_map2 = F3(function(f, d1, d2)
{
	return _Json_mapMany(f, [d1, d2]);
});

var _Json_map3 = F4(function(f, d1, d2, d3)
{
	return _Json_mapMany(f, [d1, d2, d3]);
});

var _Json_map4 = F5(function(f, d1, d2, d3, d4)
{
	return _Json_mapMany(f, [d1, d2, d3, d4]);
});

var _Json_map5 = F6(function(f, d1, d2, d3, d4, d5)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5]);
});

var _Json_map6 = F7(function(f, d1, d2, d3, d4, d5, d6)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6]);
});

var _Json_map7 = F8(function(f, d1, d2, d3, d4, d5, d6, d7)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
});

var _Json_map8 = F9(function(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
});


// DECODE

var _Json_runOnString = F2(function(decoder, string)
{
	try
	{
		var value = JSON.parse(string);
		return _Json_runHelp(decoder, value);
	}
	catch (e)
	{
		return elm$core$Result$Err(A2(elm$json$Json$Decode$Failure, 'This is not valid JSON! ' + e.message, _Json_wrap(string)));
	}
});

var _Json_run = F2(function(decoder, value)
{
	return _Json_runHelp(decoder, _Json_unwrap(value));
});

function _Json_runHelp(decoder, value)
{
	switch (decoder.$)
	{
		case 3:
			return (typeof value === 'boolean')
				? elm$core$Result$Ok(value)
				: _Json_expecting('a BOOL', value);

		case 2:
			if (typeof value !== 'number') {
				return _Json_expecting('an INT', value);
			}

			if (-2147483647 < value && value < 2147483647 && (value | 0) === value) {
				return elm$core$Result$Ok(value);
			}

			if (isFinite(value) && !(value % 1)) {
				return elm$core$Result$Ok(value);
			}

			return _Json_expecting('an INT', value);

		case 4:
			return (typeof value === 'number')
				? elm$core$Result$Ok(value)
				: _Json_expecting('a FLOAT', value);

		case 6:
			return (typeof value === 'string')
				? elm$core$Result$Ok(value)
				: (value instanceof String)
					? elm$core$Result$Ok(value + '')
					: _Json_expecting('a STRING', value);

		case 9:
			return (value === null)
				? elm$core$Result$Ok(decoder.c)
				: _Json_expecting('null', value);

		case 5:
			return elm$core$Result$Ok(_Json_wrap(value));

		case 7:
			if (!Array.isArray(value))
			{
				return _Json_expecting('a LIST', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _List_fromArray);

		case 8:
			if (!Array.isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _Json_toElmArray);

		case 10:
			var field = decoder.d;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return _Json_expecting('an OBJECT with a field named `' + field + '`', value);
			}
			var result = _Json_runHelp(decoder.b, value[field]);
			return (elm$core$Result$isOk(result)) ? result : elm$core$Result$Err(A2(elm$json$Json$Decode$Field, field, result.a));

		case 11:
			var index = decoder.e;
			if (!Array.isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			if (index >= value.length)
			{
				return _Json_expecting('a LONGER array. Need index ' + index + ' but only see ' + value.length + ' entries', value);
			}
			var result = _Json_runHelp(decoder.b, value[index]);
			return (elm$core$Result$isOk(result)) ? result : elm$core$Result$Err(A2(elm$json$Json$Decode$Index, index, result.a));

		case 12:
			if (typeof value !== 'object' || value === null || Array.isArray(value))
			{
				return _Json_expecting('an OBJECT', value);
			}

			var keyValuePairs = _List_Nil;
			// TODO test perf of Object.keys and switch when support is good enough
			for (var key in value)
			{
				if (value.hasOwnProperty(key))
				{
					var result = _Json_runHelp(decoder.b, value[key]);
					if (!elm$core$Result$isOk(result))
					{
						return elm$core$Result$Err(A2(elm$json$Json$Decode$Field, key, result.a));
					}
					keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
				}
			}
			return elm$core$Result$Ok(elm$core$List$reverse(keyValuePairs));

		case 13:
			var answer = decoder.f;
			var decoders = decoder.g;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = _Json_runHelp(decoders[i], value);
				if (!elm$core$Result$isOk(result))
				{
					return result;
				}
				answer = answer(result.a);
			}
			return elm$core$Result$Ok(answer);

		case 14:
			var result = _Json_runHelp(decoder.b, value);
			return (!elm$core$Result$isOk(result))
				? result
				: _Json_runHelp(decoder.h(result.a), value);

		case 15:
			var errors = _List_Nil;
			for (var temp = decoder.g; temp.b; temp = temp.b) // WHILE_CONS
			{
				var result = _Json_runHelp(temp.a, value);
				if (elm$core$Result$isOk(result))
				{
					return result;
				}
				errors = _List_Cons(result.a, errors);
			}
			return elm$core$Result$Err(elm$json$Json$Decode$OneOf(elm$core$List$reverse(errors)));

		case 1:
			return elm$core$Result$Err(A2(elm$json$Json$Decode$Failure, decoder.a, _Json_wrap(value)));

		case 0:
			return elm$core$Result$Ok(decoder.a);
	}
}

function _Json_runArrayDecoder(decoder, value, toElmValue)
{
	var len = value.length;
	var array = new Array(len);
	for (var i = 0; i < len; i++)
	{
		var result = _Json_runHelp(decoder, value[i]);
		if (!elm$core$Result$isOk(result))
		{
			return elm$core$Result$Err(A2(elm$json$Json$Decode$Index, i, result.a));
		}
		array[i] = result.a;
	}
	return elm$core$Result$Ok(toElmValue(array));
}

function _Json_toElmArray(array)
{
	return A2(elm$core$Array$initialize, array.length, function(i) { return array[i]; });
}

function _Json_expecting(type, value)
{
	return elm$core$Result$Err(A2(elm$json$Json$Decode$Failure, 'Expecting ' + type, _Json_wrap(value)));
}


// EQUALITY

function _Json_equality(x, y)
{
	if (x === y)
	{
		return true;
	}

	if (x.$ !== y.$)
	{
		return false;
	}

	switch (x.$)
	{
		case 0:
		case 1:
			return x.a === y.a;

		case 3:
		case 2:
		case 4:
		case 6:
		case 5:
			return true;

		case 9:
			return x.c === y.c;

		case 7:
		case 8:
		case 12:
			return _Json_equality(x.b, y.b);

		case 10:
			return x.d === y.d && _Json_equality(x.b, y.b);

		case 11:
			return x.e === y.e && _Json_equality(x.b, y.b);

		case 13:
			return x.f === y.f && _Json_listEquality(x.g, y.g);

		case 14:
			return x.h === y.h && _Json_equality(x.b, y.b);

		case 15:
			return _Json_listEquality(x.g, y.g);
	}
}

function _Json_listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!_Json_equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

var _Json_encode = F2(function(indentLevel, value)
{
	return JSON.stringify(_Json_unwrap(value), null, indentLevel) + '';
});

function _Json_wrap(value) { return { $: 0, a: value }; }
function _Json_unwrap(value) { return value.a; }

function _Json_wrap_UNUSED(value) { return value; }
function _Json_unwrap_UNUSED(value) { return value; }

function _Json_emptyArray() { return []; }
function _Json_emptyObject() { return {}; }

var _Json_addField = F3(function(key, value, object)
{
	object[key] = _Json_unwrap(value);
	return object;
});

function _Json_addEntry(func)
{
	return F2(function(entry, array)
	{
		array.push(_Json_unwrap(func(entry)));
		return array;
	});
}

var _Json_encodeNull = _Json_wrap(null);



// TASKS

function _Scheduler_succeed(value)
{
	return {
		$: 0,
		a: value
	};
}

function _Scheduler_fail(error)
{
	return {
		$: 1,
		a: error
	};
}

function _Scheduler_binding(callback)
{
	return {
		$: 2,
		b: callback,
		c: null
	};
}

var _Scheduler_andThen = F2(function(callback, task)
{
	return {
		$: 3,
		b: callback,
		d: task
	};
});

var _Scheduler_onError = F2(function(callback, task)
{
	return {
		$: 4,
		b: callback,
		d: task
	};
});

function _Scheduler_receive(callback)
{
	return {
		$: 5,
		b: callback
	};
}


// PROCESSES

var _Scheduler_guid = 0;

function _Scheduler_rawSpawn(task)
{
	var proc = {
		$: 0,
		e: _Scheduler_guid++,
		f: task,
		g: null,
		h: []
	};

	_Scheduler_enqueue(proc);

	return proc;
}

function _Scheduler_spawn(task)
{
	return _Scheduler_binding(function(callback) {
		callback(_Scheduler_succeed(_Scheduler_rawSpawn(task)));
	});
}

function _Scheduler_rawSend(proc, msg)
{
	proc.h.push(msg);
	_Scheduler_enqueue(proc);
}

var _Scheduler_send = F2(function(proc, msg)
{
	return _Scheduler_binding(function(callback) {
		_Scheduler_rawSend(proc, msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});

function _Scheduler_kill(proc)
{
	return _Scheduler_binding(function(callback) {
		var task = proc.f;
		if (task.$ === 2 && task.c)
		{
			task.c();
		}

		proc.f = null;

		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
}


/* STEP PROCESSES

type alias Process =
  { $ : tag
  , id : unique_id
  , root : Task
  , stack : null | { $: SUCCEED | FAIL, a: callback, b: stack }
  , mailbox : [msg]
  }

*/


var _Scheduler_working = false;
var _Scheduler_queue = [];


function _Scheduler_enqueue(proc)
{
	_Scheduler_queue.push(proc);
	if (_Scheduler_working)
	{
		return;
	}
	_Scheduler_working = true;
	while (proc = _Scheduler_queue.shift())
	{
		_Scheduler_step(proc);
	}
	_Scheduler_working = false;
}


function _Scheduler_step(proc)
{
	while (proc.f)
	{
		var rootTag = proc.f.$;
		if (rootTag === 0 || rootTag === 1)
		{
			while (proc.g && proc.g.$ !== rootTag)
			{
				proc.g = proc.g.i;
			}
			if (!proc.g)
			{
				return;
			}
			proc.f = proc.g.b(proc.f.a);
			proc.g = proc.g.i;
		}
		else if (rootTag === 2)
		{
			proc.f.c = proc.f.b(function(newRoot) {
				proc.f = newRoot;
				_Scheduler_enqueue(proc);
			});
			return;
		}
		else if (rootTag === 5)
		{
			if (proc.h.length === 0)
			{
				return;
			}
			proc.f = proc.f.b(proc.h.shift());
		}
		else // if (rootTag === 3 || rootTag === 4)
		{
			proc.g = {
				$: rootTag === 3 ? 0 : 1,
				b: proc.f.b,
				i: proc.g
			};
			proc.f = proc.f.d;
		}
	}
}



function _Process_sleep(time)
{
	return _Scheduler_binding(function(callback) {
		var id = setTimeout(function() {
			callback(_Scheduler_succeed(_Utils_Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}




// PROGRAMS


var _Platform_worker = F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	elm$core$Result$isOk(result) || _Debug_crash(2 /**/, _Json_errorToString(result.a) /**/);
	var managers = {};
	result = init(result.a);
	var model = result.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		result = A2(update, msg, model);
		stepper(model = result.a, viewMetadata);
		_Platform_dispatchEffects(managers, result.b, subscriptions(model));
	}

	_Platform_dispatchEffects(managers, result.b, subscriptions(model));

	return ports ? { ports: ports } : {};
}



// TRACK PRELOADS
//
// This is used by code in elm/browser and elm/http
// to register any HTTP requests that are triggered by init.
//


var _Platform_preload;


function _Platform_registerPreload(url)
{
	_Platform_preload.add(url);
}



// EFFECT MANAGERS


var _Platform_effectManagers = {};


function _Platform_setupEffects(managers, sendToApp)
{
	var ports;

	// setup all necessary effect managers
	for (var key in _Platform_effectManagers)
	{
		var manager = _Platform_effectManagers[key];

		if (manager.a)
		{
			ports = ports || {};
			ports[key] = manager.a(key, sendToApp);
		}

		managers[key] = _Platform_instantiateManager(manager, sendToApp);
	}

	return ports;
}


function _Platform_createManager(init, onEffects, onSelfMsg, cmdMap, subMap)
{
	return {
		b: init,
		c: onEffects,
		d: onSelfMsg,
		e: cmdMap,
		f: subMap
	};
}


function _Platform_instantiateManager(info, sendToApp)
{
	var router = {
		g: sendToApp,
		h: undefined
	};

	var onEffects = info.c;
	var onSelfMsg = info.d;
	var cmdMap = info.e;
	var subMap = info.f;

	function loop(state)
	{
		return A2(_Scheduler_andThen, loop, _Scheduler_receive(function(msg)
		{
			var value = msg.a;

			if (msg.$ === 0)
			{
				return A3(onSelfMsg, router, value, state);
			}

			return cmdMap && subMap
				? A4(onEffects, router, value.i, value.j, state)
				: A3(onEffects, router, cmdMap ? value.i : value.j, state);
		}));
	}

	return router.h = _Scheduler_rawSpawn(A2(_Scheduler_andThen, loop, info.b));
}



// ROUTING


var _Platform_sendToApp = F2(function(router, msg)
{
	return _Scheduler_binding(function(callback)
	{
		router.g(msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});


var _Platform_sendToSelf = F2(function(router, msg)
{
	return A2(_Scheduler_send, router.h, {
		$: 0,
		a: msg
	});
});



// BAGS


function _Platform_leaf(home)
{
	return function(value)
	{
		return {
			$: 1,
			k: home,
			l: value
		};
	};
}


function _Platform_batch(list)
{
	return {
		$: 2,
		m: list
	};
}


var _Platform_map = F2(function(tagger, bag)
{
	return {
		$: 3,
		n: tagger,
		o: bag
	}
});



// PIPE BAGS INTO EFFECT MANAGERS


function _Platform_dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	_Platform_gatherEffects(true, cmdBag, effectsDict, null);
	_Platform_gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		_Scheduler_rawSend(managers[home], {
			$: 'fx',
			a: effectsDict[home] || { i: _List_Nil, j: _List_Nil }
		});
	}
}


function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.$)
	{
		case 1:
			var home = bag.k;
			var effect = _Platform_toEffect(isCmd, home, taggers, bag.l);
			effectsDict[home] = _Platform_insert(isCmd, effect, effectsDict[home]);
			return;

		case 2:
			for (var list = bag.m; list.b; list = list.b) // WHILE_CONS
			{
				_Platform_gatherEffects(isCmd, list.a, effectsDict, taggers);
			}
			return;

		case 3:
			_Platform_gatherEffects(isCmd, bag.o, effectsDict, {
				p: bag.n,
				q: taggers
			});
			return;
	}
}


function _Platform_toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		for (var temp = taggers; temp; temp = temp.q)
		{
			x = temp.p(x);
		}
		return x;
	}

	var map = isCmd
		? _Platform_effectManagers[home].e
		: _Platform_effectManagers[home].f;

	return A2(map, applyTaggers, value)
}


function _Platform_insert(isCmd, newEffect, effects)
{
	effects = effects || { i: _List_Nil, j: _List_Nil };

	isCmd
		? (effects.i = _List_Cons(newEffect, effects.i))
		: (effects.j = _List_Cons(newEffect, effects.j));

	return effects;
}



// PORTS


function _Platform_checkPortName(name)
{
	if (_Platform_effectManagers[name])
	{
		_Debug_crash(3, name)
	}
}



// OUTGOING PORTS


function _Platform_outgoingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		e: _Platform_outgoingPortMap,
		r: converter,
		a: _Platform_setupOutgoingPort
	};
	return _Platform_leaf(name);
}


var _Platform_outgoingPortMap = F2(function(tagger, value) { return value; });


function _Platform_setupOutgoingPort(name)
{
	var subs = [];
	var converter = _Platform_effectManagers[name].r;

	// CREATE MANAGER

	var init = _Process_sleep(0);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, cmdList, state)
	{
		for ( ; cmdList.b; cmdList = cmdList.b) // WHILE_CONS
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = _Json_unwrap(converter(cmdList.a));
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
		}
		return init;
	});

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}



// INCOMING PORTS


function _Platform_incomingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		f: _Platform_incomingPortMap,
		r: converter,
		a: _Platform_setupIncomingPort
	};
	return _Platform_leaf(name);
}


var _Platform_incomingPortMap = F2(function(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});


function _Platform_setupIncomingPort(name, sendToApp)
{
	var subs = _List_Nil;
	var converter = _Platform_effectManagers[name].r;

	// CREATE MANAGER

	var init = _Scheduler_succeed(null);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, subList, state)
	{
		subs = subList;
		return init;
	});

	// PUBLIC API

	function send(incomingValue)
	{
		var result = A2(_Json_run, converter, _Json_wrap(incomingValue));

		elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

		var value = result.a;
		for (var temp = subs; temp.b; temp = temp.b) // WHILE_CONS
		{
			sendToApp(temp.a(value));
		}
	}

	return { send: send };
}



// EXPORT ELM MODULES
//
// Have DEBUG and PROD versions so that we can (1) give nicer errors in
// debug mode and (2) not pay for the bits needed for that in prod mode.
//


function _Platform_export_UNUSED(exports)
{
	scope['Elm']
		? _Platform_mergeExportsProd(scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsProd(obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6)
				: _Platform_mergeExportsProd(obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}


function _Platform_export(exports)
{
	scope['Elm']
		? _Platform_mergeExportsDebug('Elm', scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsDebug(moduleName, obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6, moduleName)
				: _Platform_mergeExportsDebug(moduleName + '.' + name, obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}




// STRINGS


var _Parser_isSubString = F5(function(smallString, offset, row, col, bigString)
{
	var smallLength = smallString.length;
	var isGood = offset + smallLength <= bigString.length;

	for (var i = 0; isGood && i < smallLength; )
	{
		var code = bigString.charCodeAt(offset);
		isGood =
			smallString[i++] === bigString[offset++]
			&& (
				code === 0x000A /* \n */
					? ( row++, col=1 )
					: ( col++, (code & 0xF800) === 0xD800 ? smallString[i++] === bigString[offset++] : 1 )
			)
	}

	return _Utils_Tuple3(isGood ? offset : -1, row, col);
});



// CHARS


var _Parser_isSubChar = F3(function(predicate, offset, string)
{
	return (
		string.length <= offset
			? -1
			:
		(string.charCodeAt(offset) & 0xF800) === 0xD800
			? (predicate(_Utils_chr(string.substr(offset, 2))) ? offset + 2 : -1)
			:
		(predicate(_Utils_chr(string[offset]))
			? ((string[offset] === '\n') ? -2 : (offset + 1))
			: -1
		)
	);
});


var _Parser_isAsciiCode = F3(function(code, offset, string)
{
	return string.charCodeAt(offset) === code;
});



// NUMBERS


var _Parser_chompBase10 = F2(function(offset, string)
{
	for (; offset < string.length; offset++)
	{
		var code = string.charCodeAt(offset);
		if (code < 0x30 || 0x39 < code)
		{
			return offset;
		}
	}
	return offset;
});


var _Parser_consumeBase = F3(function(base, offset, string)
{
	for (var total = 0; offset < string.length; offset++)
	{
		var digit = string.charCodeAt(offset) - 0x30;
		if (digit < 0 || base <= digit) break;
		total = base * total + digit;
	}
	return _Utils_Tuple2(offset, total);
});


var _Parser_consumeBase16 = F2(function(offset, string)
{
	for (var total = 0; offset < string.length; offset++)
	{
		var code = string.charCodeAt(offset);
		if (0x30 <= code && code <= 0x39)
		{
			total = 16 * total + code - 0x30;
		}
		else if (0x41 <= code && code <= 0x46)
		{
			total = 16 * total + code - 55;
		}
		else if (0x61 <= code && code <= 0x66)
		{
			total = 16 * total + code - 87;
		}
		else
		{
			break;
		}
	}
	return _Utils_Tuple2(offset, total);
});



// FIND STRING


var _Parser_findSubString = F5(function(smallString, offset, row, col, bigString)
{
	var newOffset = bigString.indexOf(smallString, offset);
	var target = newOffset < 0 ? bigString.length : newOffset + smallString.length;

	while (offset < target)
	{
		var code = bigString.charCodeAt(offset++);
		code === 0x000A /* \n */
			? ( col=1, row++ )
			: ( col++, (code & 0xF800) === 0xD800 && offset++ )
	}

	return _Utils_Tuple3(newOffset, row, col);
});



var _Bitwise_and = F2(function(a, b)
{
	return a & b;
});

var _Bitwise_or = F2(function(a, b)
{
	return a | b;
});

var _Bitwise_xor = F2(function(a, b)
{
	return a ^ b;
});

function _Bitwise_complement(a)
{
	return ~a;
};

var _Bitwise_shiftLeftBy = F2(function(offset, a)
{
	return a << offset;
});

var _Bitwise_shiftRightBy = F2(function(offset, a)
{
	return a >> offset;
});

var _Bitwise_shiftRightZfBy = F2(function(offset, a)
{
	return a >>> offset;
});
var author$project$OutputMarkdownHtml$RequestedHtml = function (a) {
	return {$: 'RequestedHtml', a: a};
};
var elm$core$Basics$False = {$: 'False'};
var elm$core$Basics$True = {$: 'True'};
var elm$core$Result$isOk = function (result) {
	if (result.$ === 'Ok') {
		return true;
	} else {
		return false;
	}
};
var elm$core$Basics$EQ = {$: 'EQ'};
var elm$core$Basics$GT = {$: 'GT'};
var elm$core$Basics$LT = {$: 'LT'};
var elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = t.b;
				var value = t.c;
				var left = t.d;
				var right = t.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3(elm$core$Dict$foldr, func, acc, right)),
					$temp$t = left;
				func = $temp$func;
				acc = $temp$acc;
				t = $temp$t;
				continue foldr;
			}
		}
	});
var elm$core$List$cons = _List_cons;
var elm$core$Dict$toList = function (dict) {
	return A3(
		elm$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var elm$core$Dict$keys = function (dict) {
	return A3(
		elm$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return A2(elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var elm$core$Set$toList = function (_n0) {
	var dict = _n0.a;
	return elm$core$Dict$keys(dict);
};
var elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var elm$core$Array$foldr = F3(
	function (func, baseCase, _n0) {
		var tree = _n0.c;
		var tail = _n0.d;
		var helper = F2(
			function (node, acc) {
				if (node.$ === 'SubTree') {
					var subTree = node.a;
					return A3(elm$core$Elm$JsArray$foldr, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3(elm$core$Elm$JsArray$foldr, func, acc, values);
				}
			});
		return A3(
			elm$core$Elm$JsArray$foldr,
			helper,
			A3(elm$core$Elm$JsArray$foldr, func, baseCase, tail),
			tree);
	});
var elm$core$Array$toList = function (array) {
	return A3(elm$core$Array$foldr, elm$core$List$cons, _List_Nil, array);
};
var elm$core$Array$branchFactor = 32;
var elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 'Array_elm_builtin', a: a, b: b, c: c, d: d};
	});
var elm$core$Basics$ceiling = _Basics_ceiling;
var elm$core$Basics$fdiv = _Basics_fdiv;
var elm$core$Basics$logBase = F2(
	function (base, number) {
		return _Basics_log(number) / _Basics_log(base);
	});
var elm$core$Basics$toFloat = _Basics_toFloat;
var elm$core$Array$shiftStep = elm$core$Basics$ceiling(
	A2(elm$core$Basics$logBase, 2, elm$core$Array$branchFactor));
var elm$core$Elm$JsArray$empty = _JsArray_empty;
var elm$core$Array$empty = A4(elm$core$Array$Array_elm_builtin, 0, elm$core$Array$shiftStep, elm$core$Elm$JsArray$empty, elm$core$Elm$JsArray$empty);
var elm$core$Array$Leaf = function (a) {
	return {$: 'Leaf', a: a};
};
var elm$core$Array$SubTree = function (a) {
	return {$: 'SubTree', a: a};
};
var elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
var elm$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			if (!list.b) {
				return acc;
			} else {
				var x = list.a;
				var xs = list.b;
				var $temp$func = func,
					$temp$acc = A2(func, x, acc),
					$temp$list = xs;
				func = $temp$func;
				acc = $temp$acc;
				list = $temp$list;
				continue foldl;
			}
		}
	});
var elm$core$List$reverse = function (list) {
	return A3(elm$core$List$foldl, elm$core$List$cons, _List_Nil, list);
};
var elm$core$Array$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _n0 = A2(elm$core$Elm$JsArray$initializeFromList, elm$core$Array$branchFactor, nodes);
			var node = _n0.a;
			var remainingNodes = _n0.b;
			var newAcc = A2(
				elm$core$List$cons,
				elm$core$Array$SubTree(node),
				acc);
			if (!remainingNodes.b) {
				return elm$core$List$reverse(newAcc);
			} else {
				var $temp$nodes = remainingNodes,
					$temp$acc = newAcc;
				nodes = $temp$nodes;
				acc = $temp$acc;
				continue compressNodes;
			}
		}
	});
var elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var elm$core$Basics$eq = _Utils_equal;
var elm$core$Tuple$first = function (_n0) {
	var x = _n0.a;
	return x;
};
var elm$core$Array$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = elm$core$Basics$ceiling(nodeListSize / elm$core$Array$branchFactor);
			if (newNodeSize === 1) {
				return A2(elm$core$Elm$JsArray$initializeFromList, elm$core$Array$branchFactor, nodeList).a;
			} else {
				var $temp$nodeList = A2(elm$core$Array$compressNodes, nodeList, _List_Nil),
					$temp$nodeListSize = newNodeSize;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue treeFromBuilder;
			}
		}
	});
var elm$core$Basics$add = _Basics_add;
var elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var elm$core$Basics$floor = _Basics_floor;
var elm$core$Basics$gt = _Utils_gt;
var elm$core$Basics$max = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var elm$core$Basics$mul = _Basics_mul;
var elm$core$Basics$sub = _Basics_sub;
var elm$core$Elm$JsArray$length = _JsArray_length;
var elm$core$Array$builderToArray = F2(
	function (reverseNodeList, builder) {
		if (!builder.nodeListSize) {
			return A4(
				elm$core$Array$Array_elm_builtin,
				elm$core$Elm$JsArray$length(builder.tail),
				elm$core$Array$shiftStep,
				elm$core$Elm$JsArray$empty,
				builder.tail);
		} else {
			var treeLen = builder.nodeListSize * elm$core$Array$branchFactor;
			var depth = elm$core$Basics$floor(
				A2(elm$core$Basics$logBase, elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? elm$core$List$reverse(builder.nodeList) : builder.nodeList;
			var tree = A2(elm$core$Array$treeFromBuilder, correctNodeList, builder.nodeListSize);
			return A4(
				elm$core$Array$Array_elm_builtin,
				elm$core$Elm$JsArray$length(builder.tail) + treeLen,
				A2(elm$core$Basics$max, 5, depth * elm$core$Array$shiftStep),
				tree,
				builder.tail);
		}
	});
var elm$core$Basics$idiv = _Basics_idiv;
var elm$core$Basics$lt = _Utils_lt;
var elm$core$Elm$JsArray$initialize = _JsArray_initialize;
var elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					elm$core$Array$builderToArray,
					false,
					{nodeList: nodeList, nodeListSize: (len / elm$core$Array$branchFactor) | 0, tail: tail});
			} else {
				var leaf = elm$core$Array$Leaf(
					A3(elm$core$Elm$JsArray$initialize, elm$core$Array$branchFactor, fromIndex, fn));
				var $temp$fn = fn,
					$temp$fromIndex = fromIndex - elm$core$Array$branchFactor,
					$temp$len = len,
					$temp$nodeList = A2(elm$core$List$cons, leaf, nodeList),
					$temp$tail = tail;
				fn = $temp$fn;
				fromIndex = $temp$fromIndex;
				len = $temp$len;
				nodeList = $temp$nodeList;
				tail = $temp$tail;
				continue initializeHelp;
			}
		}
	});
var elm$core$Basics$le = _Utils_le;
var elm$core$Basics$remainderBy = _Basics_remainderBy;
var elm$core$Array$initialize = F2(
	function (len, fn) {
		if (len <= 0) {
			return elm$core$Array$empty;
		} else {
			var tailLen = len % elm$core$Array$branchFactor;
			var tail = A3(elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
			var initialFromIndex = (len - tailLen) - elm$core$Array$branchFactor;
			return A5(elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
		}
	});
var elm$core$Maybe$Just = function (a) {
	return {$: 'Just', a: a};
};
var elm$core$Maybe$Nothing = {$: 'Nothing'};
var elm$core$Result$Err = function (a) {
	return {$: 'Err', a: a};
};
var elm$core$Result$Ok = function (a) {
	return {$: 'Ok', a: a};
};
var elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 'Failure', a: a, b: b};
	});
var elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 'Field', a: a, b: b};
	});
var elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 'Index', a: a, b: b};
	});
var elm$json$Json$Decode$OneOf = function (a) {
	return {$: 'OneOf', a: a};
};
var elm$core$Basics$and = _Basics_and;
var elm$core$Basics$append = _Utils_append;
var elm$core$Basics$or = _Basics_or;
var elm$core$Char$toCode = _Char_toCode;
var elm$core$Char$isLower = function (_char) {
	var code = elm$core$Char$toCode(_char);
	return (97 <= code) && (code <= 122);
};
var elm$core$Char$isUpper = function (_char) {
	var code = elm$core$Char$toCode(_char);
	return (code <= 90) && (65 <= code);
};
var elm$core$Char$isAlpha = function (_char) {
	return elm$core$Char$isLower(_char) || elm$core$Char$isUpper(_char);
};
var elm$core$Char$isDigit = function (_char) {
	var code = elm$core$Char$toCode(_char);
	return (code <= 57) && (48 <= code);
};
var elm$core$Char$isAlphaNum = function (_char) {
	return elm$core$Char$isLower(_char) || (elm$core$Char$isUpper(_char) || elm$core$Char$isDigit(_char));
};
var elm$core$List$length = function (xs) {
	return A3(
		elm$core$List$foldl,
		F2(
			function (_n0, i) {
				return i + 1;
			}),
		0,
		xs);
};
var elm$core$List$map2 = _List_map2;
var elm$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_Utils_cmp(lo, hi) < 1) {
				var $temp$lo = lo,
					$temp$hi = hi - 1,
					$temp$list = A2(elm$core$List$cons, hi, list);
				lo = $temp$lo;
				hi = $temp$hi;
				list = $temp$list;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var elm$core$List$range = F2(
	function (lo, hi) {
		return A3(elm$core$List$rangeHelp, lo, hi, _List_Nil);
	});
var elm$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			elm$core$List$map2,
			f,
			A2(
				elm$core$List$range,
				0,
				elm$core$List$length(xs) - 1),
			xs);
	});
var elm$core$String$all = _String_all;
var elm$core$String$fromInt = _String_fromNumber;
var elm$core$String$join = F2(
	function (sep, chunks) {
		return A2(
			_String_join,
			sep,
			_List_toArray(chunks));
	});
var elm$core$String$uncons = _String_uncons;
var elm$core$String$split = F2(
	function (sep, string) {
		return _List_fromArray(
			A2(_String_split, sep, string));
	});
var elm$json$Json$Decode$indent = function (str) {
	return A2(
		elm$core$String$join,
		'\n    ',
		A2(elm$core$String$split, '\n', str));
};
var elm$json$Json$Encode$encode = _Json_encode;
var elm$json$Json$Decode$errorOneOf = F2(
	function (i, error) {
		return '\n\n(' + (elm$core$String$fromInt(i + 1) + (') ' + elm$json$Json$Decode$indent(
			elm$json$Json$Decode$errorToString(error))));
	});
var elm$json$Json$Decode$errorToString = function (error) {
	return A2(elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
};
var elm$json$Json$Decode$errorToStringHelp = F2(
	function (error, context) {
		errorToStringHelp:
		while (true) {
			switch (error.$) {
				case 'Field':
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _n1 = elm$core$String$uncons(f);
						if (_n1.$ === 'Nothing') {
							return false;
						} else {
							var _n2 = _n1.a;
							var _char = _n2.a;
							var rest = _n2.b;
							return elm$core$Char$isAlpha(_char) && A2(elm$core$String$all, elm$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? ('.' + f) : ('[\'' + (f + '\']'));
					var $temp$error = err,
						$temp$context = A2(elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'Index':
					var i = error.a;
					var err = error.b;
					var indexName = '[' + (elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2(elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'OneOf':
					var errors = error.a;
					if (!errors.b) {
						return 'Ran into a Json.Decode.oneOf with no possibilities' + function () {
							if (!context.b) {
								return '!';
							} else {
								return ' at json' + A2(
									elm$core$String$join,
									'',
									elm$core$List$reverse(context));
							}
						}();
					} else {
						if (!errors.b.b) {
							var err = errors.a;
							var $temp$error = err,
								$temp$context = context;
							error = $temp$error;
							context = $temp$context;
							continue errorToStringHelp;
						} else {
							var starter = function () {
								if (!context.b) {
									return 'Json.Decode.oneOf';
								} else {
									return 'The Json.Decode.oneOf at json' + A2(
										elm$core$String$join,
										'',
										elm$core$List$reverse(context));
								}
							}();
							var introduction = starter + (' failed in the following ' + (elm$core$String$fromInt(
								elm$core$List$length(errors)) + ' ways:'));
							return A2(
								elm$core$String$join,
								'\n\n',
								A2(
									elm$core$List$cons,
									introduction,
									A2(elm$core$List$indexedMap, elm$json$Json$Decode$errorOneOf, errors)));
						}
					}
				default:
					var msg = error.a;
					var json = error.b;
					var introduction = function () {
						if (!context.b) {
							return 'Problem with the given value:\n\n';
						} else {
							return 'Problem with the value at json' + (A2(
								elm$core$String$join,
								'',
								elm$core$List$reverse(context)) + ':\n\n    ');
						}
					}();
					return introduction + (elm$json$Json$Decode$indent(
						A2(elm$json$Json$Encode$encode, 4, json)) + ('\n\n' + msg));
			}
		}
	});
var elm$core$Platform$Cmd$batch = _Platform_batch;
var elm$core$Platform$Cmd$none = elm$core$Platform$Cmd$batch(_List_Nil);
var author$project$OutputMarkdownHtml$init = function (flags) {
	return _Utils_Tuple2(_Utils_Tuple0, elm$core$Platform$Cmd$none);
};
var elm$json$Json$Decode$string = _Json_decodeString;
var author$project$OutputMarkdownHtml$requestHtml = _Platform_incomingPort('requestHtml', elm$json$Json$Decode$string);
var elm$json$Json$Encode$string = _Json_wrap;
var author$project$OutputMarkdownHtml$error = _Platform_outgoingPort('error', elm$json$Json$Encode$string);
var author$project$OutputMarkdownHtml$printOutput = _Platform_outgoingPort('printOutput', elm$json$Json$Encode$string);
var author$project$OutputMarkdownHtml$printHtml = function (renderResult) {
	if (renderResult.$ === 'Ok') {
		var htmlString = renderResult.a;
		return author$project$OutputMarkdownHtml$printOutput(htmlString);
	} else {
		var errorString = renderResult.a;
		return author$project$OutputMarkdownHtml$error(errorString);
	}
};
var author$project$Markdown$Parser$Decoder = function (a) {
	return {$: 'Decoder', a: a};
};
var author$project$Markdown$Parser$resultOr = F2(
	function (ra, rb) {
		if (ra.$ === 'Err') {
			return rb;
		} else {
			return ra;
		}
	});
var elm$core$Basics$identity = function (x) {
	return x;
};
var author$project$Markdown$Parser$htmlOneOf = function (decoders) {
	return A3(
		elm$core$List$foldl,
		F2(
			function (_n0, _n1) {
				var decoder = _n0.a;
				var soFar = _n1.a;
				return author$project$Markdown$Parser$Decoder(
					F3(
						function (tag, attributes, children) {
							return A2(
								author$project$Markdown$Parser$resultOr,
								A3(decoder, tag, attributes, children),
								A3(soFar, tag, attributes, children));
						}));
			}),
		author$project$Markdown$Parser$Decoder(
			F3(
				function (tag, attributes, children) {
					return elm$core$Result$Err('No Html Decoders succeeded in oneOf.');
				})),
		decoders);
};
var elm$core$List$foldrHelper = F4(
	function (fn, acc, ctr, ls) {
		if (!ls.b) {
			return acc;
		} else {
			var a = ls.a;
			var r1 = ls.b;
			if (!r1.b) {
				return A2(fn, a, acc);
			} else {
				var b = r1.a;
				var r2 = r1.b;
				if (!r2.b) {
					return A2(
						fn,
						a,
						A2(fn, b, acc));
				} else {
					var c = r2.a;
					var r3 = r2.b;
					if (!r3.b) {
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(fn, c, acc)));
					} else {
						var d = r3.a;
						var r4 = r3.b;
						var res = (ctr > 500) ? A3(
							elm$core$List$foldl,
							fn,
							acc,
							elm$core$List$reverse(r4)) : A4(elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(
									fn,
									c,
									A2(fn, d, res))));
					}
				}
			}
		}
	});
var elm$core$List$foldr = F3(
	function (fn, acc, ls) {
		return A4(elm$core$List$foldrHelper, fn, acc, 0, ls);
	});
var elm$core$Result$map2 = F3(
	function (func, ra, rb) {
		if (ra.$ === 'Err') {
			var x = ra.a;
			return elm$core$Result$Err(x);
		} else {
			var a = ra.a;
			if (rb.$ === 'Err') {
				var x = rb.a;
				return elm$core$Result$Err(x);
			} else {
				var b = rb.a;
				return elm$core$Result$Ok(
					A2(func, a, b));
			}
		}
	});
var author$project$Markdown$Parser$combineResults = A2(
	elm$core$List$foldr,
	elm$core$Result$map2(elm$core$List$cons),
	elm$core$Result$Ok(_List_Nil));
var author$project$Markdown$Parser$problemToString = function (problem) {
	switch (problem.$) {
		case 'Expecting':
			var string = problem.a;
			return 'Expecting ' + string;
		case 'ExpectingInt':
			return 'Expecting int';
		case 'ExpectingHex':
			return 'Expecting hex';
		case 'ExpectingOctal':
			return 'Expecting octal';
		case 'ExpectingBinary':
			return 'Expecting binary';
		case 'ExpectingFloat':
			return 'Expecting float';
		case 'ExpectingNumber':
			return 'Expecting number';
		case 'ExpectingVariable':
			return 'Expecting variable';
		case 'ExpectingSymbol':
			var string = problem.a;
			return 'Expecting symbol ' + string;
		case 'ExpectingKeyword':
			var string = problem.a;
			return 'Expecting keyword ' + string;
		case 'ExpectingEnd':
			return 'Expecting keyword end';
		case 'UnexpectedChar':
			return 'Unexpected char';
		case 'Problem':
			var problemDescription = problem.a;
			return problemDescription;
		default:
			return 'Bad repeat';
	}
};
var author$project$Markdown$Parser$deadEndToString = function (deadEnd) {
	return 'Problem at row ' + (elm$core$String$fromInt(deadEnd.row) + ('\n' + author$project$Markdown$Parser$problemToString(deadEnd.problem)));
};
var elm$core$List$map = F2(
	function (f, xs) {
		return A3(
			elm$core$List$foldr,
			F2(
				function (x, acc) {
					return A2(
						elm$core$List$cons,
						f(x),
						acc);
				}),
			_List_Nil,
			xs);
	});
var author$project$Markdown$Parser$deadEndsToString = function (deadEnds) {
	return A2(
		elm$core$String$join,
		'\n',
		A2(elm$core$List$map, author$project$Markdown$Parser$deadEndToString, deadEnds));
};
var elm$parser$Parser$ExpectingSymbol = function (a) {
	return {$: 'ExpectingSymbol', a: a};
};
var elm$parser$Parser$Problem = function (a) {
	return {$: 'Problem', a: a};
};
var elm$parser$Parser$Advanced$Token = F2(
	function (a, b) {
		return {$: 'Token', a: a, b: b};
	});
var elm$core$Basics$negate = function (n) {
	return -n;
};
var elm$parser$Parser$Advanced$Bad = F2(
	function (a, b) {
		return {$: 'Bad', a: a, b: b};
	});
var elm$parser$Parser$Advanced$Good = F3(
	function (a, b, c) {
		return {$: 'Good', a: a, b: b, c: c};
	});
var elm$parser$Parser$Advanced$Parser = function (a) {
	return {$: 'Parser', a: a};
};
var elm$parser$Parser$Advanced$findSubString = _Parser_findSubString;
var elm$parser$Parser$Advanced$AddRight = F2(
	function (a, b) {
		return {$: 'AddRight', a: a, b: b};
	});
var elm$parser$Parser$Advanced$DeadEnd = F4(
	function (row, col, problem, contextStack) {
		return {col: col, contextStack: contextStack, problem: problem, row: row};
	});
var elm$parser$Parser$Advanced$Empty = {$: 'Empty'};
var elm$parser$Parser$Advanced$fromInfo = F4(
	function (row, col, x, context) {
		return A2(
			elm$parser$Parser$Advanced$AddRight,
			elm$parser$Parser$Advanced$Empty,
			A4(elm$parser$Parser$Advanced$DeadEnd, row, col, x, context));
	});
var elm$parser$Parser$Advanced$chompUntil = function (_n0) {
	var str = _n0.a;
	var expecting = _n0.b;
	return elm$parser$Parser$Advanced$Parser(
		function (s) {
			var _n1 = A5(elm$parser$Parser$Advanced$findSubString, str, s.offset, s.row, s.col, s.src);
			var newOffset = _n1.a;
			var newRow = _n1.b;
			var newCol = _n1.c;
			return _Utils_eq(newOffset, -1) ? A2(
				elm$parser$Parser$Advanced$Bad,
				false,
				A4(elm$parser$Parser$Advanced$fromInfo, newRow, newCol, expecting, s.context)) : A3(
				elm$parser$Parser$Advanced$Good,
				_Utils_cmp(s.offset, newOffset) < 0,
				_Utils_Tuple0,
				{col: newCol, context: s.context, indent: s.indent, offset: newOffset, row: newRow, src: s.src});
		});
};
var elm$core$String$length = _String_length;
var elm$parser$Parser$Advanced$chompUntilEndOr = function (str) {
	return elm$parser$Parser$Advanced$Parser(
		function (s) {
			var _n0 = A5(_Parser_findSubString, str, s.offset, s.row, s.col, s.src);
			var newOffset = _n0.a;
			var newRow = _n0.b;
			var newCol = _n0.c;
			var adjustedOffset = (newOffset < 0) ? elm$core$String$length(s.src) : newOffset;
			return A3(
				elm$parser$Parser$Advanced$Good,
				_Utils_cmp(s.offset, adjustedOffset) < 0,
				_Utils_Tuple0,
				{col: newCol, context: s.context, indent: s.indent, offset: adjustedOffset, row: newRow, src: s.src});
		});
};
var elm$core$Basics$always = F2(
	function (a, _n0) {
		return a;
	});
var elm$core$String$slice = _String_slice;
var elm$parser$Parser$Advanced$mapChompedString = F2(
	function (func, _n0) {
		var parse = _n0.a;
		return elm$parser$Parser$Advanced$Parser(
			function (s0) {
				var _n1 = parse(s0);
				if (_n1.$ === 'Bad') {
					var p = _n1.a;
					var x = _n1.b;
					return A2(elm$parser$Parser$Advanced$Bad, p, x);
				} else {
					var p = _n1.a;
					var a = _n1.b;
					var s1 = _n1.c;
					return A3(
						elm$parser$Parser$Advanced$Good,
						p,
						A2(
							func,
							A3(elm$core$String$slice, s0.offset, s1.offset, s0.src),
							a),
						s1);
				}
			});
	});
var elm$parser$Parser$Advanced$getChompedString = function (parser) {
	return A2(elm$parser$Parser$Advanced$mapChompedString, elm$core$Basics$always, parser);
};
var elm$parser$Parser$Advanced$map2 = F3(
	function (func, _n0, _n1) {
		var parseA = _n0.a;
		var parseB = _n1.a;
		return elm$parser$Parser$Advanced$Parser(
			function (s0) {
				var _n2 = parseA(s0);
				if (_n2.$ === 'Bad') {
					var p = _n2.a;
					var x = _n2.b;
					return A2(elm$parser$Parser$Advanced$Bad, p, x);
				} else {
					var p1 = _n2.a;
					var a = _n2.b;
					var s1 = _n2.c;
					var _n3 = parseB(s1);
					if (_n3.$ === 'Bad') {
						var p2 = _n3.a;
						var x = _n3.b;
						return A2(elm$parser$Parser$Advanced$Bad, p1 || p2, x);
					} else {
						var p2 = _n3.a;
						var b = _n3.b;
						var s2 = _n3.c;
						return A3(
							elm$parser$Parser$Advanced$Good,
							p1 || p2,
							A2(func, a, b),
							s2);
					}
				}
			});
	});
var elm$parser$Parser$Advanced$ignorer = F2(
	function (keepParser, ignoreParser) {
		return A3(elm$parser$Parser$Advanced$map2, elm$core$Basics$always, keepParser, ignoreParser);
	});
var elm$parser$Parser$Advanced$keeper = F2(
	function (parseFunc, parseArg) {
		return A3(elm$parser$Parser$Advanced$map2, elm$core$Basics$apL, parseFunc, parseArg);
	});
var elm$parser$Parser$Advanced$succeed = function (a) {
	return elm$parser$Parser$Advanced$Parser(
		function (s) {
			return A3(elm$parser$Parser$Advanced$Good, false, a, s);
		});
};
var elm$core$Basics$not = _Basics_not;
var elm$core$String$isEmpty = function (string) {
	return string === '';
};
var elm$parser$Parser$Advanced$fromState = F2(
	function (s, x) {
		return A2(
			elm$parser$Parser$Advanced$AddRight,
			elm$parser$Parser$Advanced$Empty,
			A4(elm$parser$Parser$Advanced$DeadEnd, s.row, s.col, x, s.context));
	});
var elm$parser$Parser$Advanced$isSubString = _Parser_isSubString;
var elm$parser$Parser$Advanced$token = function (_n0) {
	var str = _n0.a;
	var expecting = _n0.b;
	var progress = !elm$core$String$isEmpty(str);
	return elm$parser$Parser$Advanced$Parser(
		function (s) {
			var _n1 = A5(elm$parser$Parser$Advanced$isSubString, str, s.offset, s.row, s.col, s.src);
			var newOffset = _n1.a;
			var newRow = _n1.b;
			var newCol = _n1.c;
			return _Utils_eq(newOffset, -1) ? A2(
				elm$parser$Parser$Advanced$Bad,
				false,
				A2(elm$parser$Parser$Advanced$fromState, s, expecting)) : A3(
				elm$parser$Parser$Advanced$Good,
				progress,
				_Utils_Tuple0,
				{col: newCol, context: s.context, indent: s.indent, offset: newOffset, row: newRow, src: s.src});
		});
};
var elm$parser$Parser$Advanced$symbol = elm$parser$Parser$Advanced$token;
var author$project$Markdown$CodeBlock$parserHelp = function (delimeter) {
	return A2(
		elm$parser$Parser$Advanced$keeper,
		A2(
			elm$parser$Parser$Advanced$keeper,
			A2(
				elm$parser$Parser$Advanced$ignorer,
				elm$parser$Parser$Advanced$succeed(
					F2(
						function (language, body) {
							return {
								body: body,
								language: (language === '') ? elm$core$Maybe$Nothing : elm$core$Maybe$Just(language)
							};
						})),
				elm$parser$Parser$Advanced$symbol(
					A2(
						elm$parser$Parser$Advanced$Token,
						delimeter,
						elm$parser$Parser$ExpectingSymbol(delimeter)))),
			A2(
				elm$parser$Parser$Advanced$ignorer,
				elm$parser$Parser$Advanced$getChompedString(
					elm$parser$Parser$Advanced$chompUntil(
						A2(
							elm$parser$Parser$Advanced$Token,
							'\n',
							elm$parser$Parser$Problem('Expecting newline')))),
				elm$parser$Parser$Advanced$symbol(
					A2(
						elm$parser$Parser$Advanced$Token,
						'\n',
						elm$parser$Parser$ExpectingSymbol('\n'))))),
		A2(
			elm$parser$Parser$Advanced$ignorer,
			elm$parser$Parser$Advanced$getChompedString(
				elm$parser$Parser$Advanced$chompUntilEndOr('\n' + delimeter)),
			elm$parser$Parser$Advanced$symbol(
				A2(
					elm$parser$Parser$Advanced$Token,
					'\n' + delimeter,
					elm$parser$Parser$ExpectingSymbol(delimeter)))));
};
var elm$parser$Parser$Advanced$Append = F2(
	function (a, b) {
		return {$: 'Append', a: a, b: b};
	});
var elm$parser$Parser$Advanced$oneOfHelp = F3(
	function (s0, bag, parsers) {
		oneOfHelp:
		while (true) {
			if (!parsers.b) {
				return A2(elm$parser$Parser$Advanced$Bad, false, bag);
			} else {
				var parse = parsers.a.a;
				var remainingParsers = parsers.b;
				var _n1 = parse(s0);
				if (_n1.$ === 'Good') {
					var step = _n1;
					return step;
				} else {
					var step = _n1;
					var p = step.a;
					var x = step.b;
					if (p) {
						return step;
					} else {
						var $temp$s0 = s0,
							$temp$bag = A2(elm$parser$Parser$Advanced$Append, bag, x),
							$temp$parsers = remainingParsers;
						s0 = $temp$s0;
						bag = $temp$bag;
						parsers = $temp$parsers;
						continue oneOfHelp;
					}
				}
			}
		}
	});
var elm$parser$Parser$Advanced$oneOf = function (parsers) {
	return elm$parser$Parser$Advanced$Parser(
		function (s) {
			return A3(elm$parser$Parser$Advanced$oneOfHelp, s, elm$parser$Parser$Advanced$Empty, parsers);
		});
};
var author$project$Markdown$CodeBlock$parser = elm$parser$Parser$Advanced$oneOf(
	_List_fromArray(
		[
			author$project$Markdown$CodeBlock$parserHelp('```'),
			author$project$Markdown$CodeBlock$parserHelp('~~~')
		]));
var author$project$Markdown$Parser$Body = function (a) {
	return {$: 'Body', a: a};
};
var author$project$Markdown$Parser$CodeBlock = function (a) {
	return {$: 'CodeBlock', a: a};
};
var author$project$Markdown$Parser$Html = F3(
	function (a, b, c) {
		return {$: 'Html', a: a, b: b, c: c};
	});
var elm$parser$Parser$Expecting = function (a) {
	return {$: 'Expecting', a: a};
};
var author$project$Markdown$Parser$blankLine = A2(
	elm$parser$Parser$Advanced$ignorer,
	elm$parser$Parser$Advanced$succeed(
		author$project$Markdown$Parser$Body(_List_Nil)),
	elm$parser$Parser$Advanced$symbol(
		A2(
			elm$parser$Parser$Advanced$Token,
			'\n',
			elm$parser$Parser$Expecting('\n'))));
var elm$parser$Parser$Advanced$andThen = F2(
	function (callback, _n0) {
		var parseA = _n0.a;
		return elm$parser$Parser$Advanced$Parser(
			function (s0) {
				var _n1 = parseA(s0);
				if (_n1.$ === 'Bad') {
					var p = _n1.a;
					var x = _n1.b;
					return A2(elm$parser$Parser$Advanced$Bad, p, x);
				} else {
					var p1 = _n1.a;
					var a = _n1.b;
					var s1 = _n1.c;
					var _n2 = callback(a);
					var parseB = _n2.a;
					var _n3 = parseB(s1);
					if (_n3.$ === 'Bad') {
						var p2 = _n3.a;
						var x = _n3.b;
						return A2(elm$parser$Parser$Advanced$Bad, p1 || p2, x);
					} else {
						var p2 = _n3.a;
						var b = _n3.b;
						var s2 = _n3.c;
						return A3(elm$parser$Parser$Advanced$Good, p1 || p2, b, s2);
					}
				}
			});
	});
var elm$parser$Parser$Advanced$map = F2(
	function (func, _n0) {
		var parse = _n0.a;
		return elm$parser$Parser$Advanced$Parser(
			function (s0) {
				var _n1 = parse(s0);
				if (_n1.$ === 'Good') {
					var p = _n1.a;
					var a = _n1.b;
					var s1 = _n1.c;
					return A3(
						elm$parser$Parser$Advanced$Good,
						p,
						func(a),
						s1);
				} else {
					var p = _n1.a;
					var x = _n1.b;
					return A2(elm$parser$Parser$Advanced$Bad, p, x);
				}
			});
	});
var author$project$Markdown$Parser$combine = function (list) {
	return A3(
		elm$core$List$foldl,
		F2(
			function (parser, listParser) {
				return A2(
					elm$parser$Parser$Advanced$andThen,
					function (soFar) {
						return A2(
							elm$parser$Parser$Advanced$map,
							function (a) {
								return A2(elm$core$List$cons, a, soFar);
							},
							parser);
					},
					listParser);
			}),
		elm$parser$Parser$Advanced$succeed(_List_Nil),
		list);
};
var elm$core$Basics$neq = _Utils_notEqual;
var author$project$Markdown$Inlines$isUninteresting = function (_char) {
	return (!_Utils_eq(
		_char,
		_Utils_chr('*'))) && ((!_Utils_eq(
		_char,
		_Utils_chr('`'))) && (!_Utils_eq(
		_char,
		_Utils_chr('['))));
};
var elm$parser$Parser$Advanced$Loop = function (a) {
	return {$: 'Loop', a: a};
};
var author$project$Markdown$Inlines$nextStepWhenFoundBold = F2(
	function (_n0, string) {
		var currStyle = _n0.a;
		var revStyledStrings = _n0.b;
		return elm$parser$Parser$Advanced$Loop(
			_Utils_Tuple2(
				_Utils_update(
					currStyle,
					{isBold: !currStyle.isBold}),
				A2(
					elm$core$List$cons,
					{string: string, style: currStyle},
					revStyledStrings)));
	});
var author$project$Markdown$Inlines$nextStepWhenFoundCode = F2(
	function (_n0, string) {
		var currStyle = _n0.a;
		var revStyledStrings = _n0.b;
		return elm$parser$Parser$Advanced$Loop(
			_Utils_Tuple2(
				_Utils_update(
					currStyle,
					{isCode: !currStyle.isCode}),
				A2(
					elm$core$List$cons,
					{string: string, style: currStyle},
					revStyledStrings)));
	});
var author$project$Markdown$Inlines$nextStepWhenFoundItalic = F2(
	function (_n0, string) {
		var currStyle = _n0.a;
		var revStyledStrings = _n0.b;
		return elm$parser$Parser$Advanced$Loop(
			_Utils_Tuple2(
				_Utils_update(
					currStyle,
					{isItalic: !currStyle.isItalic}),
				A2(
					elm$core$List$cons,
					{string: string, style: currStyle},
					revStyledStrings)));
	});
var author$project$Markdown$Inlines$nextStepWhenFoundLink = F3(
	function (link, _n0, string) {
		var currStyle = _n0.a;
		var revStyledStrings = _n0.b;
		return elm$parser$Parser$Advanced$Loop(
			_Utils_Tuple2(
				currStyle,
				A2(
					elm$core$List$cons,
					{
						string: link.description,
						style: _Utils_update(
							currStyle,
							{
								link: elm$core$Maybe$Just(
									{destination: link.destination, title: link.title})
							})
					},
					revStyledStrings)));
	});
var elm$core$List$filter = F2(
	function (isGood, list) {
		return A3(
			elm$core$List$foldr,
			F2(
				function (x, xs) {
					return isGood(x) ? A2(elm$core$List$cons, x, xs) : xs;
				}),
			_List_Nil,
			list);
	});
var elm$parser$Parser$Advanced$Done = function (a) {
	return {$: 'Done', a: a};
};
var author$project$Markdown$Inlines$nextStepWhenFoundNothing = F2(
	function (_n0, string) {
		var currStyle = _n0.a;
		var revStyledStrings = _n0.b;
		return elm$parser$Parser$Advanced$Done(
			A2(
				elm$core$List$filter,
				function (thing) {
					return thing.string !== '';
				},
				elm$core$List$reverse(
					A2(
						elm$core$List$cons,
						{string: string, style: currStyle},
						revStyledStrings))));
	});
var author$project$Markdown$Link$parser = A2(
	elm$parser$Parser$Advanced$keeper,
	A2(
		elm$parser$Parser$Advanced$keeper,
		A2(
			elm$parser$Parser$Advanced$ignorer,
			elm$parser$Parser$Advanced$succeed(
				F2(
					function (description, destination) {
						return {description: description, destination: destination, title: elm$core$Maybe$Nothing};
					})),
			elm$parser$Parser$Advanced$symbol(
				A2(
					elm$parser$Parser$Advanced$Token,
					'[',
					elm$parser$Parser$ExpectingSymbol('[')))),
		A2(
			elm$parser$Parser$Advanced$ignorer,
			A2(
				elm$parser$Parser$Advanced$ignorer,
				elm$parser$Parser$Advanced$getChompedString(
					elm$parser$Parser$Advanced$chompUntil(
						A2(
							elm$parser$Parser$Advanced$Token,
							']',
							elm$parser$Parser$ExpectingSymbol(']')))),
				elm$parser$Parser$Advanced$symbol(
					A2(
						elm$parser$Parser$Advanced$Token,
						']',
						elm$parser$Parser$ExpectingSymbol(']')))),
			elm$parser$Parser$Advanced$symbol(
				A2(
					elm$parser$Parser$Advanced$Token,
					'(',
					elm$parser$Parser$ExpectingSymbol('('))))),
	A2(
		elm$parser$Parser$Advanced$ignorer,
		elm$parser$Parser$Advanced$getChompedString(
			elm$parser$Parser$Advanced$chompUntil(
				A2(
					elm$parser$Parser$Advanced$Token,
					')',
					elm$parser$Parser$ExpectingSymbol(')')))),
		elm$parser$Parser$Advanced$symbol(
			A2(
				elm$parser$Parser$Advanced$Token,
				')',
				elm$parser$Parser$ExpectingSymbol(')')))));
var elm$parser$Parser$Advanced$isSubChar = _Parser_isSubChar;
var elm$parser$Parser$Advanced$chompWhileHelp = F5(
	function (isGood, offset, row, col, s0) {
		chompWhileHelp:
		while (true) {
			var newOffset = A3(elm$parser$Parser$Advanced$isSubChar, isGood, offset, s0.src);
			if (_Utils_eq(newOffset, -1)) {
				return A3(
					elm$parser$Parser$Advanced$Good,
					_Utils_cmp(s0.offset, offset) < 0,
					_Utils_Tuple0,
					{col: col, context: s0.context, indent: s0.indent, offset: offset, row: row, src: s0.src});
			} else {
				if (_Utils_eq(newOffset, -2)) {
					var $temp$isGood = isGood,
						$temp$offset = offset + 1,
						$temp$row = row + 1,
						$temp$col = 1,
						$temp$s0 = s0;
					isGood = $temp$isGood;
					offset = $temp$offset;
					row = $temp$row;
					col = $temp$col;
					s0 = $temp$s0;
					continue chompWhileHelp;
				} else {
					var $temp$isGood = isGood,
						$temp$offset = newOffset,
						$temp$row = row,
						$temp$col = col + 1,
						$temp$s0 = s0;
					isGood = $temp$isGood;
					offset = $temp$offset;
					row = $temp$row;
					col = $temp$col;
					s0 = $temp$s0;
					continue chompWhileHelp;
				}
			}
		}
	});
var elm$parser$Parser$Advanced$chompWhile = function (isGood) {
	return elm$parser$Parser$Advanced$Parser(
		function (s) {
			return A5(elm$parser$Parser$Advanced$chompWhileHelp, isGood, s.offset, s.row, s.col, s);
		});
};
var author$project$Markdown$Inlines$parseHelp = function (state) {
	return A2(
		elm$parser$Parser$Advanced$andThen,
		function (chompedString) {
			return elm$parser$Parser$Advanced$oneOf(
				_List_fromArray(
					[
						A2(
						elm$parser$Parser$Advanced$map,
						function (link) {
							return A3(author$project$Markdown$Inlines$nextStepWhenFoundLink, link, state, chompedString);
						},
						author$project$Markdown$Link$parser),
						A2(
						elm$parser$Parser$Advanced$map,
						function (_n0) {
							return A2(author$project$Markdown$Inlines$nextStepWhenFoundCode, state, chompedString);
						},
						elm$parser$Parser$Advanced$token(
							A2(
								elm$parser$Parser$Advanced$Token,
								'`',
								elm$parser$Parser$Expecting('`')))),
						A2(
						elm$parser$Parser$Advanced$map,
						function (_n1) {
							return A2(author$project$Markdown$Inlines$nextStepWhenFoundBold, state, chompedString);
						},
						elm$parser$Parser$Advanced$token(
							A2(
								elm$parser$Parser$Advanced$Token,
								'**',
								elm$parser$Parser$Expecting('**')))),
						A2(
						elm$parser$Parser$Advanced$map,
						function (_n2) {
							return A2(author$project$Markdown$Inlines$nextStepWhenFoundItalic, state, chompedString);
						},
						elm$parser$Parser$Advanced$token(
							A2(
								elm$parser$Parser$Advanced$Token,
								'*',
								elm$parser$Parser$Expecting('*')))),
						elm$parser$Parser$Advanced$succeed(
						A2(author$project$Markdown$Inlines$nextStepWhenFoundNothing, state, chompedString))
					]));
		},
		elm$parser$Parser$Advanced$getChompedString(
			elm$parser$Parser$Advanced$chompWhile(author$project$Markdown$Inlines$isUninteresting)));
};
var elm$parser$Parser$Advanced$loopHelp = F4(
	function (p, state, callback, s0) {
		loopHelp:
		while (true) {
			var _n0 = callback(state);
			var parse = _n0.a;
			var _n1 = parse(s0);
			if (_n1.$ === 'Good') {
				var p1 = _n1.a;
				var step = _n1.b;
				var s1 = _n1.c;
				if (step.$ === 'Loop') {
					var newState = step.a;
					var $temp$p = p || p1,
						$temp$state = newState,
						$temp$callback = callback,
						$temp$s0 = s1;
					p = $temp$p;
					state = $temp$state;
					callback = $temp$callback;
					s0 = $temp$s0;
					continue loopHelp;
				} else {
					var result = step.a;
					return A3(elm$parser$Parser$Advanced$Good, p || p1, result, s1);
				}
			} else {
				var p1 = _n1.a;
				var x = _n1.b;
				return A2(elm$parser$Parser$Advanced$Bad, p || p1, x);
			}
		}
	});
var elm$parser$Parser$Advanced$loop = F2(
	function (state, callback) {
		return elm$parser$Parser$Advanced$Parser(
			function (s) {
				return A4(elm$parser$Parser$Advanced$loopHelp, false, state, callback, s);
			});
	});
var author$project$Markdown$Inlines$parse = A2(
	elm$parser$Parser$Advanced$loop,
	_Utils_Tuple2(
		{isBold: false, isCode: false, isItalic: false, link: elm$core$Maybe$Nothing},
		_List_Nil),
	author$project$Markdown$Inlines$parseHelp);
var author$project$Markdown$Parser$Heading = F2(
	function (a, b) {
		return {$: 'Heading', a: a, b: b};
	});
var elm$core$Basics$ge = _Utils_ge;
var elm$parser$Parser$Advanced$problem = function (x) {
	return elm$parser$Parser$Advanced$Parser(
		function (s) {
			return A2(
				elm$parser$Parser$Advanced$Bad,
				false,
				A2(elm$parser$Parser$Advanced$fromState, s, x));
		});
};
var elm$parser$Parser$Advanced$bagToList = F2(
	function (bag, list) {
		bagToList:
		while (true) {
			switch (bag.$) {
				case 'Empty':
					return list;
				case 'AddRight':
					var bag1 = bag.a;
					var x = bag.b;
					var $temp$bag = bag1,
						$temp$list = A2(elm$core$List$cons, x, list);
					bag = $temp$bag;
					list = $temp$list;
					continue bagToList;
				default:
					var bag1 = bag.a;
					var bag2 = bag.b;
					var $temp$bag = bag1,
						$temp$list = A2(elm$parser$Parser$Advanced$bagToList, bag2, list);
					bag = $temp$bag;
					list = $temp$list;
					continue bagToList;
			}
		}
	});
var elm$parser$Parser$Advanced$run = F2(
	function (_n0, src) {
		var parse = _n0.a;
		var _n1 = parse(
			{col: 1, context: _List_Nil, indent: 1, offset: 0, row: 1, src: src});
		if (_n1.$ === 'Good') {
			var value = _n1.b;
			return elm$core$Result$Ok(value);
		} else {
			var bag = _n1.b;
			return elm$core$Result$Err(
				A2(elm$parser$Parser$Advanced$bagToList, bag, _List_Nil));
		}
	});
var author$project$Markdown$Parser$heading = A2(
	elm$parser$Parser$Advanced$keeper,
	A2(
		elm$parser$Parser$Advanced$keeper,
		A2(
			elm$parser$Parser$Advanced$ignorer,
			elm$parser$Parser$Advanced$succeed(author$project$Markdown$Parser$Heading),
			elm$parser$Parser$Advanced$symbol(
				A2(
					elm$parser$Parser$Advanced$Token,
					'#',
					elm$parser$Parser$Expecting('#')))),
		A2(
			elm$parser$Parser$Advanced$ignorer,
			A2(
				elm$parser$Parser$Advanced$andThen,
				function (additionalHashes) {
					var level = elm$core$String$length(additionalHashes) + 1;
					return (level >= 7) ? elm$parser$Parser$Advanced$problem(
						elm$parser$Parser$Expecting('heading with < 7 #\'s')) : elm$parser$Parser$Advanced$succeed(level);
				},
				elm$parser$Parser$Advanced$getChompedString(
					A2(
						elm$parser$Parser$Advanced$ignorer,
						elm$parser$Parser$Advanced$succeed(_Utils_Tuple0),
						elm$parser$Parser$Advanced$chompWhile(
							function (c) {
								return _Utils_eq(
									c,
									_Utils_chr('#'));
							})))),
			elm$parser$Parser$Advanced$chompWhile(
				function (c) {
					return _Utils_eq(
						c,
						_Utils_chr(' '));
				}))),
	A2(
		elm$parser$Parser$Advanced$andThen,
		function (headingText) {
			var result = A2(elm$parser$Parser$Advanced$run, author$project$Markdown$Inlines$parse, headingText);
			if (result.$ === 'Ok') {
				var styled = result.a;
				return elm$parser$Parser$Advanced$succeed(styled);
			} else {
				var error = result.a;
				return elm$parser$Parser$Advanced$problem(
					elm$parser$Parser$Expecting('TODO'));
			}
		},
		elm$parser$Parser$Advanced$getChompedString(
			A2(
				elm$parser$Parser$Advanced$ignorer,
				elm$parser$Parser$Advanced$succeed(_Utils_Tuple0),
				elm$parser$Parser$Advanced$chompUntilEndOr('\n')))));
var author$project$Markdown$List$singleItemParser = A2(
	elm$parser$Parser$Advanced$keeper,
	A2(
		elm$parser$Parser$Advanced$ignorer,
		A2(
			elm$parser$Parser$Advanced$ignorer,
			elm$parser$Parser$Advanced$succeed(elm$core$Basics$identity),
			elm$parser$Parser$Advanced$symbol(
				A2(
					elm$parser$Parser$Advanced$Token,
					'-',
					elm$parser$Parser$ExpectingSymbol('-')))),
		elm$parser$Parser$Advanced$chompWhile(
			function (c) {
				return _Utils_eq(
					c,
					_Utils_chr(' '));
			})),
	A2(
		elm$parser$Parser$Advanced$ignorer,
		A2(
			elm$parser$Parser$Advanced$andThen,
			function (listItemContent) {
				var _n0 = A2(elm$parser$Parser$Advanced$run, author$project$Markdown$Inlines$parse, listItemContent);
				if (_n0.$ === 'Ok') {
					var content = _n0.a;
					return elm$parser$Parser$Advanced$succeed(content);
				} else {
					var errors = _n0.a;
					return elm$parser$Parser$Advanced$problem(
						elm$parser$Parser$Expecting('Error parsing inlines TODO'));
				}
			},
			elm$parser$Parser$Advanced$getChompedString(
				elm$parser$Parser$Advanced$chompUntilEndOr('\n'))),
		elm$parser$Parser$Advanced$symbol(
			A2(
				elm$parser$Parser$Advanced$Token,
				'\n',
				elm$parser$Parser$ExpectingSymbol('\n')))));
var elm$parser$Parser$Advanced$getOffset = elm$parser$Parser$Advanced$Parser(
	function (s) {
		return A3(elm$parser$Parser$Advanced$Good, false, s.offset, s);
	});
var author$project$Markdown$List$statementsHelp = F2(
	function (firstItem, revStmts) {
		return elm$parser$Parser$Advanced$oneOf(
			_List_fromArray(
				[
					A2(
					elm$parser$Parser$Advanced$keeper,
					A2(
						elm$parser$Parser$Advanced$keeper,
						A2(
							elm$parser$Parser$Advanced$keeper,
							elm$parser$Parser$Advanced$succeed(
								F3(
									function (offsetBefore, stmt, offsetAfter) {
										return elm$parser$Parser$Advanced$Loop(
											A2(elm$core$List$cons, stmt, revStmts));
									})),
							elm$parser$Parser$Advanced$getOffset),
						author$project$Markdown$List$singleItemParser),
					elm$parser$Parser$Advanced$getOffset),
					A2(
					elm$parser$Parser$Advanced$map,
					function (_n0) {
						return elm$parser$Parser$Advanced$Done(
							A2(
								elm$core$List$cons,
								firstItem,
								elm$core$List$reverse(revStmts)));
					},
					elm$parser$Parser$Advanced$succeed(_Utils_Tuple0))
				]));
	});
var author$project$Markdown$List$parser = A2(
	elm$parser$Parser$Advanced$andThen,
	function (firstItem) {
		return A2(
			elm$parser$Parser$Advanced$loop,
			_List_Nil,
			author$project$Markdown$List$statementsHelp(firstItem));
	},
	author$project$Markdown$List$singleItemParser);
var author$project$Markdown$Parser$ListBlock = function (a) {
	return {$: 'ListBlock', a: a};
};
var author$project$Markdown$Parser$listBlock = A2(elm$parser$Parser$Advanced$map, author$project$Markdown$Parser$ListBlock, author$project$Markdown$List$parser);
var elm$core$List$singleton = function (value) {
	return _List_fromArray(
		[value]);
};
var author$project$Markdown$Parser$plainLine = A2(
	elm$parser$Parser$Advanced$map,
	elm$core$List$singleton,
	A2(
		elm$parser$Parser$Advanced$map,
		author$project$Markdown$Parser$Body,
		A2(
			elm$parser$Parser$Advanced$andThen,
			function (line) {
				var _n0 = A2(elm$parser$Parser$Advanced$run, author$project$Markdown$Inlines$parse, line);
				if (_n0.$ === 'Ok') {
					var styledLine = _n0.a;
					return elm$parser$Parser$Advanced$succeed(styledLine);
				} else {
					var error = _n0.a;
					return elm$parser$Parser$Advanced$problem(
						elm$parser$Parser$Expecting('....??? TODO'));
				}
			},
			A2(
				elm$parser$Parser$Advanced$keeper,
				elm$parser$Parser$Advanced$succeed(elm$core$Basics$identity),
				elm$parser$Parser$Advanced$getChompedString(
					elm$parser$Parser$Advanced$chompUntilEndOr('\n'))))));
var author$project$XmlParser$Element = F3(
	function (a, b, c) {
		return {$: 'Element', a: a, b: b, c: c};
	});
var author$project$XmlParser$Text = function (a) {
	return {$: 'Text', a: a};
};
var author$project$XmlParser$Attribute = F2(
	function (name, value) {
		return {name: name, value: value};
	});
var author$project$XmlParser$isWhitespace = function (c) {
	return _Utils_eq(
		c,
		_Utils_chr(' ')) || (_Utils_eq(
		c,
		_Utils_chr('\u000d')) || (_Utils_eq(
		c,
		_Utils_chr('\n')) || _Utils_eq(
		c,
		_Utils_chr('\t'))));
};
var elm$parser$Parser$BadRepeat = {$: 'BadRepeat'};
var author$project$XmlParser$keep = F2(
	function (count, predicate) {
		var n = count.a;
		return A2(
			elm$parser$Parser$Advanced$andThen,
			function (str) {
				return (_Utils_cmp(
					n,
					elm$core$String$length(str)) < 1) ? elm$parser$Parser$Advanced$succeed(str) : elm$parser$Parser$Advanced$problem(elm$parser$Parser$BadRepeat);
			},
			elm$parser$Parser$Advanced$getChompedString(
				A2(
					elm$parser$Parser$Advanced$ignorer,
					elm$parser$Parser$Advanced$succeed(_Utils_Tuple0),
					elm$parser$Parser$Advanced$chompWhile(predicate))));
	});
var author$project$XmlParser$AtLeast = function (a) {
	return {$: 'AtLeast', a: a};
};
var author$project$XmlParser$oneOrMore = author$project$XmlParser$AtLeast(1);
var elm$parser$Parser$Advanced$Located = F3(
	function (row, col, context) {
		return {col: col, context: context, row: row};
	});
var elm$parser$Parser$Advanced$changeContext = F2(
	function (newContext, s) {
		return {col: s.col, context: newContext, indent: s.indent, offset: s.offset, row: s.row, src: s.src};
	});
var elm$parser$Parser$Advanced$inContext = F2(
	function (context, _n0) {
		var parse = _n0.a;
		return elm$parser$Parser$Advanced$Parser(
			function (s0) {
				var _n1 = parse(
					A2(
						elm$parser$Parser$Advanced$changeContext,
						A2(
							elm$core$List$cons,
							A3(elm$parser$Parser$Advanced$Located, s0.row, s0.col, context),
							s0.context),
						s0));
				if (_n1.$ === 'Good') {
					var p = _n1.a;
					var a = _n1.b;
					var s1 = _n1.c;
					return A3(
						elm$parser$Parser$Advanced$Good,
						p,
						a,
						A2(elm$parser$Parser$Advanced$changeContext, s0.context, s1));
				} else {
					var step = _n1;
					return step;
				}
			});
	});
var author$project$XmlParser$attributeName = A2(
	elm$parser$Parser$Advanced$inContext,
	'attributeName',
	A2(
		author$project$XmlParser$keep,
		author$project$XmlParser$oneOrMore,
		function (c) {
			return (!author$project$XmlParser$isWhitespace(c)) && ((!_Utils_eq(
				c,
				_Utils_chr('/'))) && ((!_Utils_eq(
				c,
				_Utils_chr('<'))) && ((!_Utils_eq(
				c,
				_Utils_chr('>'))) && ((!_Utils_eq(
				c,
				_Utils_chr('\"'))) && ((!_Utils_eq(
				c,
				_Utils_chr('\''))) && (!_Utils_eq(
				c,
				_Utils_chr('='))))))));
		}));
var author$project$XmlParser$symbol = function (str) {
	return elm$parser$Parser$Advanced$symbol(
		A2(
			elm$parser$Parser$Advanced$Token,
			str,
			elm$parser$Parser$ExpectingSymbol(str)));
};
var elm$core$Dict$RBEmpty_elm_builtin = {$: 'RBEmpty_elm_builtin'};
var elm$core$Dict$empty = elm$core$Dict$RBEmpty_elm_builtin;
var elm$core$Dict$Black = {$: 'Black'};
var elm$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {$: 'RBNode_elm_builtin', a: a, b: b, c: c, d: d, e: e};
	});
var elm$core$Basics$compare = _Utils_compare;
var elm$core$Dict$Red = {$: 'Red'};
var elm$core$Dict$balance = F5(
	function (color, key, value, left, right) {
		if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Red')) {
			var _n1 = right.a;
			var rK = right.b;
			var rV = right.c;
			var rLeft = right.d;
			var rRight = right.e;
			if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
				var _n3 = left.a;
				var lK = left.b;
				var lV = left.c;
				var lLeft = left.d;
				var lRight = left.e;
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					elm$core$Dict$Red,
					key,
					value,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, lK, lV, lLeft, lRight),
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, rK, rV, rLeft, rRight));
			} else {
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					color,
					rK,
					rV,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, key, value, left, rLeft),
					rRight);
			}
		} else {
			if ((((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) && (left.d.$ === 'RBNode_elm_builtin')) && (left.d.a.$ === 'Red')) {
				var _n5 = left.a;
				var lK = left.b;
				var lV = left.c;
				var _n6 = left.d;
				var _n7 = _n6.a;
				var llK = _n6.b;
				var llV = _n6.c;
				var llLeft = _n6.d;
				var llRight = _n6.e;
				var lRight = left.e;
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					elm$core$Dict$Red,
					lK,
					lV,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, llK, llV, llLeft, llRight),
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, key, value, lRight, right));
			} else {
				return A5(elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
			}
		}
	});
var elm$core$Dict$insertHelp = F3(
	function (key, value, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, key, value, elm$core$Dict$RBEmpty_elm_builtin, elm$core$Dict$RBEmpty_elm_builtin);
		} else {
			var nColor = dict.a;
			var nKey = dict.b;
			var nValue = dict.c;
			var nLeft = dict.d;
			var nRight = dict.e;
			var _n1 = A2(elm$core$Basics$compare, key, nKey);
			switch (_n1.$) {
				case 'LT':
					return A5(
						elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						A3(elm$core$Dict$insertHelp, key, value, nLeft),
						nRight);
				case 'EQ':
					return A5(elm$core$Dict$RBNode_elm_builtin, nColor, nKey, value, nLeft, nRight);
				default:
					return A5(
						elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						nLeft,
						A3(elm$core$Dict$insertHelp, key, value, nRight));
			}
		}
	});
var elm$core$Dict$insert = F3(
	function (key, value, dict) {
		var _n0 = A3(elm$core$Dict$insertHelp, key, value, dict);
		if ((_n0.$ === 'RBNode_elm_builtin') && (_n0.a.$ === 'Red')) {
			var _n1 = _n0.a;
			var k = _n0.b;
			var v = _n0.c;
			var l = _n0.d;
			var r = _n0.e;
			return A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _n0;
			return x;
		}
	});
var elm$core$Dict$fromList = function (assocs) {
	return A3(
		elm$core$List$foldl,
		F2(
			function (_n0, dict) {
				var key = _n0.a;
				var value = _n0.b;
				return A3(elm$core$Dict$insert, key, value, dict);
			}),
		elm$core$Dict$empty,
		assocs);
};
var author$project$XmlParser$entities = elm$core$Dict$fromList(
	_List_fromArray(
		[
			_Utils_Tuple2(
			'amp',
			_Utils_chr('&')),
			_Utils_Tuple2(
			'lt',
			_Utils_chr('<')),
			_Utils_Tuple2(
			'gt',
			_Utils_chr('>')),
			_Utils_Tuple2(
			'apos',
			_Utils_chr('\'')),
			_Utils_Tuple2(
			'quot',
			_Utils_chr('\"'))
		]));
var elm$core$Char$fromCode = _Char_fromCode;
var elm$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return elm$core$Maybe$Nothing;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var _n1 = A2(elm$core$Basics$compare, targetKey, key);
				switch (_n1.$) {
					case 'LT':
						var $temp$targetKey = targetKey,
							$temp$dict = left;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
					case 'EQ':
						return elm$core$Maybe$Just(value);
					default:
						var $temp$targetKey = targetKey,
							$temp$dict = right;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
				}
			}
		}
	});
var elm$core$Maybe$map = F2(
	function (f, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return elm$core$Maybe$Just(
				f(value));
		} else {
			return elm$core$Maybe$Nothing;
		}
	});
var elm$core$Result$fromMaybe = F2(
	function (err, maybe) {
		if (maybe.$ === 'Just') {
			var v = maybe.a;
			return elm$core$Result$Ok(v);
		} else {
			return elm$core$Result$Err(err);
		}
	});
var elm$core$Result$map = F2(
	function (func, ra) {
		if (ra.$ === 'Ok') {
			var a = ra.a;
			return elm$core$Result$Ok(
				func(a));
		} else {
			var e = ra.a;
			return elm$core$Result$Err(e);
		}
	});
var elm$core$Result$mapError = F2(
	function (f, result) {
		if (result.$ === 'Ok') {
			var v = result.a;
			return elm$core$Result$Ok(v);
		} else {
			var e = result.a;
			return elm$core$Result$Err(
				f(e));
		}
	});
var elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			elm$core$String$slice,
			n,
			elm$core$String$length(string),
			string);
	});
var elm$core$String$startsWith = _String_startsWith;
var elm$core$String$toInt = _String_toInt;
var elm$core$List$tail = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return elm$core$Maybe$Just(xs);
	} else {
		return elm$core$Maybe$Nothing;
	}
};
var elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var elm$core$String$foldr = _String_foldr;
var elm$core$String$toList = function (string) {
	return A3(elm$core$String$foldr, elm$core$List$cons, _List_Nil, string);
};
var elm$core$Basics$pow = _Basics_pow;
var elm$core$String$cons = _String_cons;
var elm$core$String$fromChar = function (_char) {
	return A2(elm$core$String$cons, _char, '');
};
var rtfeldman$elm_hex$Hex$fromStringHelp = F3(
	function (position, chars, accumulated) {
		fromStringHelp:
		while (true) {
			if (!chars.b) {
				return elm$core$Result$Ok(accumulated);
			} else {
				var _char = chars.a;
				var rest = chars.b;
				switch (_char.valueOf()) {
					case '0':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated;
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '1':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + A2(elm$core$Basics$pow, 16, position);
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '2':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (2 * A2(elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '3':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (3 * A2(elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '4':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (4 * A2(elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '5':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (5 * A2(elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '6':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (6 * A2(elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '7':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (7 * A2(elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '8':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (8 * A2(elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '9':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (9 * A2(elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case 'a':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (10 * A2(elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case 'b':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (11 * A2(elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case 'c':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (12 * A2(elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case 'd':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (13 * A2(elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case 'e':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (14 * A2(elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case 'f':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (15 * A2(elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					default:
						var nonHex = _char;
						return elm$core$Result$Err(
							elm$core$String$fromChar(nonHex) + ' is not a valid hexadecimal character.');
				}
			}
		}
	});
var rtfeldman$elm_hex$Hex$fromString = function (str) {
	if (elm$core$String$isEmpty(str)) {
		return elm$core$Result$Err('Empty strings are not valid hexadecimal strings.');
	} else {
		var result = function () {
			if (A2(elm$core$String$startsWith, '-', str)) {
				var list = A2(
					elm$core$Maybe$withDefault,
					_List_Nil,
					elm$core$List$tail(
						elm$core$String$toList(str)));
				return A2(
					elm$core$Result$map,
					elm$core$Basics$negate,
					A3(
						rtfeldman$elm_hex$Hex$fromStringHelp,
						elm$core$List$length(list) - 1,
						list,
						0));
			} else {
				return A3(
					rtfeldman$elm_hex$Hex$fromStringHelp,
					elm$core$String$length(str) - 1,
					elm$core$String$toList(str),
					0);
			}
		}();
		var formatError = function (err) {
			return A2(
				elm$core$String$join,
				' ',
				_List_fromArray(
					['\"' + (str + '\"'), 'is not a valid hexadecimal string because', err]));
		};
		return A2(elm$core$Result$mapError, formatError, result);
	}
};
var author$project$XmlParser$decodeEscape = function (s) {
	return A2(elm$core$String$startsWith, '#x', s) ? A2(
		elm$core$Result$mapError,
		elm$parser$Parser$Problem,
		A2(
			elm$core$Result$map,
			elm$core$Char$fromCode,
			rtfeldman$elm_hex$Hex$fromString(
				A2(elm$core$String$dropLeft, 2, s)))) : (A2(elm$core$String$startsWith, '#', s) ? A2(
		elm$core$Result$fromMaybe,
		elm$parser$Parser$Problem('Invalid escaped charactor: ' + s),
		A2(
			elm$core$Maybe$map,
			elm$core$Char$fromCode,
			elm$core$String$toInt(
				A2(elm$core$String$dropLeft, 1, s)))) : A2(
		elm$core$Result$fromMaybe,
		elm$parser$Parser$Problem('No entity named \"&' + (s + ';\" found.')),
		A2(elm$core$Dict$get, s, author$project$XmlParser$entities)));
};
var author$project$XmlParser$fail = function (str) {
	return elm$parser$Parser$Advanced$problem(
		elm$parser$Parser$Problem(str));
};
var author$project$XmlParser$escapedChar = function (end_) {
	return A2(
		elm$parser$Parser$Advanced$inContext,
		'escapedChar',
		A2(
			elm$parser$Parser$Advanced$andThen,
			function (s) {
				return elm$parser$Parser$Advanced$oneOf(
					_List_fromArray(
						[
							A2(
							elm$parser$Parser$Advanced$andThen,
							function (_n0) {
								var _n1 = author$project$XmlParser$decodeEscape(s);
								if (_n1.$ === 'Ok') {
									var c = _n1.a;
									return elm$parser$Parser$Advanced$succeed(c);
								} else {
									var e = _n1.a;
									return elm$parser$Parser$Advanced$problem(e);
								}
							},
							author$project$XmlParser$symbol(';')),
							author$project$XmlParser$fail('Entities must end_ with \";\": &' + s)
						]));
			},
			A2(
				elm$parser$Parser$Advanced$keeper,
				A2(
					elm$parser$Parser$Advanced$ignorer,
					elm$parser$Parser$Advanced$succeed(elm$core$Basics$identity),
					author$project$XmlParser$symbol('&')),
				A2(
					author$project$XmlParser$keep,
					author$project$XmlParser$oneOrMore,
					function (c) {
						return (!_Utils_eq(c, end_)) && (!_Utils_eq(
							c,
							_Utils_chr(';')));
					}))));
};
var author$project$XmlParser$zeroOrMore = author$project$XmlParser$AtLeast(0);
var elm$parser$Parser$Advanced$lazy = function (thunk) {
	return elm$parser$Parser$Advanced$Parser(
		function (s) {
			var _n0 = thunk(_Utils_Tuple0);
			var parse = _n0.a;
			return parse(s);
		});
};
var author$project$XmlParser$textString = function (end_) {
	return A2(
		elm$parser$Parser$Advanced$inContext,
		'textString',
		A2(
			elm$parser$Parser$Advanced$andThen,
			function (s) {
				return elm$parser$Parser$Advanced$oneOf(
					_List_fromArray(
						[
							A2(
							elm$parser$Parser$Advanced$keeper,
							A2(
								elm$parser$Parser$Advanced$keeper,
								elm$parser$Parser$Advanced$succeed(elm$core$String$cons),
								author$project$XmlParser$escapedChar(end_)),
							elm$parser$Parser$Advanced$lazy(
								function (_n0) {
									return author$project$XmlParser$textString(end_);
								})),
							elm$parser$Parser$Advanced$succeed(s)
						]));
			},
			A2(
				author$project$XmlParser$keep,
				author$project$XmlParser$zeroOrMore,
				function (c) {
					return (!_Utils_eq(c, end_)) && (!_Utils_eq(
						c,
						_Utils_chr('&')));
				})));
};
var author$project$XmlParser$attributeValue = A2(
	elm$parser$Parser$Advanced$inContext,
	'attributeValue',
	elm$parser$Parser$Advanced$oneOf(
		_List_fromArray(
			[
				A2(
				elm$parser$Parser$Advanced$keeper,
				A2(
					elm$parser$Parser$Advanced$ignorer,
					elm$parser$Parser$Advanced$succeed(elm$core$Basics$identity),
					author$project$XmlParser$symbol('\"')),
				A2(
					elm$parser$Parser$Advanced$ignorer,
					author$project$XmlParser$textString(
						_Utils_chr('\"')),
					author$project$XmlParser$symbol('\"'))),
				A2(
				elm$parser$Parser$Advanced$keeper,
				A2(
					elm$parser$Parser$Advanced$ignorer,
					elm$parser$Parser$Advanced$succeed(elm$core$Basics$identity),
					author$project$XmlParser$symbol('\'')),
				A2(
					elm$parser$Parser$Advanced$ignorer,
					author$project$XmlParser$textString(
						_Utils_chr('\'')),
					author$project$XmlParser$symbol('\'')))
			])));
var author$project$XmlParser$ignore = F2(
	function (count, predicate) {
		return A2(
			elm$parser$Parser$Advanced$map,
			function (_n0) {
				return _Utils_Tuple0;
			},
			A2(author$project$XmlParser$keep, count, predicate));
	});
var author$project$XmlParser$whiteSpace = A2(author$project$XmlParser$ignore, author$project$XmlParser$zeroOrMore, author$project$XmlParser$isWhitespace);
var author$project$XmlParser$attribute = A2(
	elm$parser$Parser$Advanced$inContext,
	'attribute',
	A2(
		elm$parser$Parser$Advanced$keeper,
		A2(
			elm$parser$Parser$Advanced$keeper,
			elm$parser$Parser$Advanced$succeed(author$project$XmlParser$Attribute),
			A2(
				elm$parser$Parser$Advanced$ignorer,
				A2(
					elm$parser$Parser$Advanced$ignorer,
					A2(elm$parser$Parser$Advanced$ignorer, author$project$XmlParser$attributeName, author$project$XmlParser$whiteSpace),
					author$project$XmlParser$symbol('=')),
				author$project$XmlParser$whiteSpace)),
		author$project$XmlParser$attributeValue));
var elm$core$Set$Set_elm_builtin = function (a) {
	return {$: 'Set_elm_builtin', a: a};
};
var elm$core$Set$insert = F2(
	function (key, _n0) {
		var dict = _n0.a;
		return elm$core$Set$Set_elm_builtin(
			A3(elm$core$Dict$insert, key, _Utils_Tuple0, dict));
	});
var elm$core$Dict$member = F2(
	function (key, dict) {
		var _n0 = A2(elm$core$Dict$get, key, dict);
		if (_n0.$ === 'Just') {
			return true;
		} else {
			return false;
		}
	});
var elm$core$Set$member = F2(
	function (key, _n0) {
		var dict = _n0.a;
		return A2(elm$core$Dict$member, key, dict);
	});
var author$project$XmlParser$attributes = function (keys) {
	return A2(
		elm$parser$Parser$Advanced$inContext,
		'attributes',
		elm$parser$Parser$Advanced$oneOf(
			_List_fromArray(
				[
					A2(
					elm$parser$Parser$Advanced$andThen,
					function (attr) {
						return A2(elm$core$Set$member, attr.name, keys) ? author$project$XmlParser$fail('attribute ' + (attr.name + ' is duplicated')) : A2(
							elm$parser$Parser$Advanced$keeper,
							A2(
								elm$parser$Parser$Advanced$ignorer,
								elm$parser$Parser$Advanced$succeed(
									elm$core$List$cons(attr)),
								author$project$XmlParser$whiteSpace),
							author$project$XmlParser$attributes(
								A2(elm$core$Set$insert, attr.name, keys)));
					},
					author$project$XmlParser$attribute),
					elm$parser$Parser$Advanced$succeed(_List_Nil)
				])));
};
var author$project$XmlParser$tagName = A2(
	elm$parser$Parser$Advanced$inContext,
	'tagName',
	A2(
		author$project$XmlParser$keep,
		author$project$XmlParser$oneOrMore,
		function (c) {
			return (!author$project$XmlParser$isWhitespace(c)) && ((!_Utils_eq(
				c,
				_Utils_chr('/'))) && ((!_Utils_eq(
				c,
				_Utils_chr('<'))) && ((!_Utils_eq(
				c,
				_Utils_chr('>'))) && ((!_Utils_eq(
				c,
				_Utils_chr('\"'))) && ((!_Utils_eq(
				c,
				_Utils_chr('\''))) && (!_Utils_eq(
				c,
				_Utils_chr('='))))))));
		}));
var author$project$XmlParser$closingTag = function (startTagName) {
	return A2(
		elm$parser$Parser$Advanced$inContext,
		'closingTag',
		A2(
			elm$parser$Parser$Advanced$ignorer,
			A2(
				elm$parser$Parser$Advanced$ignorer,
				A2(
					elm$parser$Parser$Advanced$ignorer,
					A2(
						elm$parser$Parser$Advanced$ignorer,
						A2(
							elm$parser$Parser$Advanced$ignorer,
							elm$parser$Parser$Advanced$succeed(_Utils_Tuple0),
							author$project$XmlParser$symbol('</')),
						author$project$XmlParser$whiteSpace),
					A2(
						elm$parser$Parser$Advanced$andThen,
						function (endTagName) {
							return _Utils_eq(startTagName, endTagName) ? elm$parser$Parser$Advanced$succeed(_Utils_Tuple0) : author$project$XmlParser$fail('tag name mismatch: ' + (startTagName + (' and ' + endTagName)));
						},
						author$project$XmlParser$tagName)),
				author$project$XmlParser$whiteSpace),
			author$project$XmlParser$symbol('>')));
};
function author$project$XmlParser$cyclic$cdataContent() {
	return A2(
		elm$parser$Parser$Advanced$inContext,
		'cdataContent',
		elm$parser$Parser$Advanced$oneOf(
			_List_fromArray(
				[
					A2(
					elm$parser$Parser$Advanced$ignorer,
					elm$parser$Parser$Advanced$succeed(''),
					author$project$XmlParser$symbol(']]>')),
					A2(
					elm$parser$Parser$Advanced$andThen,
					function (_n0) {
						return A2(
							elm$parser$Parser$Advanced$map,
							function (tail) {
								return ']]' + tail;
							},
							author$project$XmlParser$cyclic$cdataContent());
					},
					author$project$XmlParser$symbol(']]')),
					A2(
					elm$parser$Parser$Advanced$andThen,
					function (_n1) {
						return A2(
							elm$parser$Parser$Advanced$map,
							function (tail) {
								return ']' + tail;
							},
							author$project$XmlParser$cyclic$cdataContent());
					},
					author$project$XmlParser$symbol(']')),
					A2(
					elm$parser$Parser$Advanced$keeper,
					A2(
						elm$parser$Parser$Advanced$keeper,
						elm$parser$Parser$Advanced$succeed(elm$core$Basics$append),
						A2(
							author$project$XmlParser$keep,
							author$project$XmlParser$zeroOrMore,
							function (c) {
								return !_Utils_eq(
									c,
									_Utils_chr(']'));
							})),
					elm$parser$Parser$Advanced$lazy(
						function (_n2) {
							return author$project$XmlParser$cyclic$cdataContent();
						}))
				])));
}
try {
	var author$project$XmlParser$cdataContent = author$project$XmlParser$cyclic$cdataContent();
	author$project$XmlParser$cyclic$cdataContent = function () {
		return author$project$XmlParser$cdataContent;
	};
} catch ($) {
throw 'Some top-level definitions from `XmlParser` are causing infinite recursion:\n\n  ┌─────┐\n  │    cdataContent\n  └─────┘\n\nThese errors are very tricky, so read https://elm-lang.org/0.19.0/halting-problem to learn how to fix it!';}
var author$project$XmlParser$cdata = A2(
	elm$parser$Parser$Advanced$inContext,
	'cdata',
	A2(
		elm$parser$Parser$Advanced$keeper,
		A2(
			elm$parser$Parser$Advanced$ignorer,
			elm$parser$Parser$Advanced$succeed(elm$core$Basics$identity),
			author$project$XmlParser$symbol('<![CDATA[')),
		author$project$XmlParser$cdataContent));
var author$project$XmlParser$toToken = function (str) {
	return A2(
		elm$parser$Parser$Advanced$Token,
		str,
		elm$parser$Parser$Expecting(str));
};
var author$project$XmlParser$comment = A2(
	elm$parser$Parser$Advanced$ignorer,
	A2(
		elm$parser$Parser$Advanced$ignorer,
		A2(
			elm$parser$Parser$Advanced$ignorer,
			elm$parser$Parser$Advanced$succeed(_Utils_Tuple0),
			elm$parser$Parser$Advanced$token(
				author$project$XmlParser$toToken('<!--'))),
		elm$parser$Parser$Advanced$chompUntil(
			author$project$XmlParser$toToken('-->'))),
	elm$parser$Parser$Advanced$token(
		author$project$XmlParser$toToken('-->')));
function author$project$XmlParser$cyclic$textNodeString() {
	return A2(
		elm$parser$Parser$Advanced$inContext,
		'textNodeString',
		elm$parser$Parser$Advanced$oneOf(
			_List_fromArray(
				[
					A2(
					elm$parser$Parser$Advanced$keeper,
					A2(
						elm$parser$Parser$Advanced$keeper,
						elm$parser$Parser$Advanced$succeed(
							F2(
								function (s, maybeString) {
									return elm$core$Maybe$Just(
										_Utils_ap(
											s,
											A2(elm$core$Maybe$withDefault, '', maybeString)));
								})),
						A2(
							author$project$XmlParser$keep,
							author$project$XmlParser$oneOrMore,
							function (c) {
								return (!_Utils_eq(
									c,
									_Utils_chr('<'))) && (!_Utils_eq(
									c,
									_Utils_chr('&')));
							})),
					elm$parser$Parser$Advanced$lazy(
						function (_n0) {
							return author$project$XmlParser$cyclic$textNodeString();
						})),
					A2(
					elm$parser$Parser$Advanced$keeper,
					A2(
						elm$parser$Parser$Advanced$keeper,
						elm$parser$Parser$Advanced$succeed(
							F2(
								function (c, maybeString) {
									return elm$core$Maybe$Just(
										A2(
											elm$core$String$cons,
											c,
											A2(elm$core$Maybe$withDefault, '', maybeString)));
								})),
						author$project$XmlParser$escapedChar(
							_Utils_chr('<'))),
					elm$parser$Parser$Advanced$lazy(
						function (_n1) {
							return author$project$XmlParser$cyclic$textNodeString();
						})),
					A2(
					elm$parser$Parser$Advanced$keeper,
					A2(
						elm$parser$Parser$Advanced$keeper,
						elm$parser$Parser$Advanced$succeed(
							F2(
								function (s, maybeString) {
									var str = _Utils_ap(
										s,
										A2(elm$core$Maybe$withDefault, '', maybeString));
									return (str !== '') ? elm$core$Maybe$Just(str) : elm$core$Maybe$Nothing;
								})),
						author$project$XmlParser$cdata),
					elm$parser$Parser$Advanced$lazy(
						function (_n2) {
							return author$project$XmlParser$cyclic$textNodeString();
						})),
					A2(
					elm$parser$Parser$Advanced$keeper,
					A2(
						elm$parser$Parser$Advanced$ignorer,
						elm$parser$Parser$Advanced$succeed(
							function (maybeString) {
								var str = A2(elm$core$Maybe$withDefault, '', maybeString);
								return (str !== '') ? elm$core$Maybe$Just(str) : elm$core$Maybe$Nothing;
							}),
						author$project$XmlParser$comment),
					elm$parser$Parser$Advanced$lazy(
						function (_n3) {
							return author$project$XmlParser$cyclic$textNodeString();
						})),
					elm$parser$Parser$Advanced$succeed(elm$core$Maybe$Nothing)
				])));
}
try {
	var author$project$XmlParser$textNodeString = author$project$XmlParser$cyclic$textNodeString();
	author$project$XmlParser$cyclic$textNodeString = function () {
		return author$project$XmlParser$textNodeString;
	};
} catch ($) {
throw 'Some top-level definitions from `XmlParser` are causing infinite recursion:\n\n  ┌─────┐\n  │    textNodeString\n  └─────┘\n\nThese errors are very tricky, so read https://elm-lang.org/0.19.0/halting-problem to learn how to fix it!';}
var elm$core$Set$empty = elm$core$Set$Set_elm_builtin(elm$core$Dict$empty);
var author$project$XmlParser$children = function (startTagName) {
	return A2(
		elm$parser$Parser$Advanced$inContext,
		'children',
		elm$parser$Parser$Advanced$oneOf(
			_List_fromArray(
				[
					A2(
					elm$parser$Parser$Advanced$ignorer,
					elm$parser$Parser$Advanced$succeed(_List_Nil),
					author$project$XmlParser$closingTag(startTagName)),
					A2(
					elm$parser$Parser$Advanced$andThen,
					function (maybeString) {
						if (maybeString.$ === 'Just') {
							var s = maybeString.a;
							return A2(
								elm$parser$Parser$Advanced$keeper,
								elm$parser$Parser$Advanced$succeed(
									function (rest) {
										return A2(
											elm$core$List$cons,
											author$project$XmlParser$Text(s),
											rest);
									}),
								author$project$XmlParser$children(startTagName));
						} else {
							return A2(
								elm$parser$Parser$Advanced$ignorer,
								elm$parser$Parser$Advanced$succeed(_List_Nil),
								author$project$XmlParser$closingTag(startTagName));
						}
					},
					author$project$XmlParser$textNodeString),
					elm$parser$Parser$Advanced$lazy(
					function (_n2) {
						return A2(
							elm$parser$Parser$Advanced$keeper,
							A2(
								elm$parser$Parser$Advanced$keeper,
								elm$parser$Parser$Advanced$succeed(elm$core$List$cons),
								author$project$XmlParser$cyclic$element()),
							author$project$XmlParser$children(startTagName));
					})
				])));
};
function author$project$XmlParser$cyclic$element() {
	return A2(
		elm$parser$Parser$Advanced$inContext,
		'element',
		A2(
			elm$parser$Parser$Advanced$keeper,
			A2(
				elm$parser$Parser$Advanced$ignorer,
				elm$parser$Parser$Advanced$succeed(elm$core$Basics$identity),
				author$project$XmlParser$symbol('<')),
			A2(
				elm$parser$Parser$Advanced$andThen,
				function (startTagName) {
					return A2(
						elm$parser$Parser$Advanced$keeper,
						A2(
							elm$parser$Parser$Advanced$keeper,
							A2(
								elm$parser$Parser$Advanced$ignorer,
								elm$parser$Parser$Advanced$succeed(
									author$project$XmlParser$Element(startTagName)),
								author$project$XmlParser$whiteSpace),
							A2(
								elm$parser$Parser$Advanced$ignorer,
								author$project$XmlParser$attributes(elm$core$Set$empty),
								author$project$XmlParser$whiteSpace)),
						elm$parser$Parser$Advanced$oneOf(
							_List_fromArray(
								[
									A2(
									elm$parser$Parser$Advanced$ignorer,
									elm$parser$Parser$Advanced$succeed(_List_Nil),
									author$project$XmlParser$symbol('/>')),
									A2(
									elm$parser$Parser$Advanced$keeper,
									A2(
										elm$parser$Parser$Advanced$ignorer,
										elm$parser$Parser$Advanced$succeed(elm$core$Basics$identity),
										author$project$XmlParser$symbol('>')),
									elm$parser$Parser$Advanced$lazy(
										function (_n0) {
											return author$project$XmlParser$children(startTagName);
										}))
								])));
				},
				author$project$XmlParser$tagName)));
}
try {
	var author$project$XmlParser$element = author$project$XmlParser$cyclic$element();
	author$project$XmlParser$cyclic$element = function () {
		return author$project$XmlParser$element;
	};
} catch ($) {
throw 'Some top-level definitions from `XmlParser` are causing infinite recursion:\n\n  ┌─────┐\n  │    children\n  │     ↓\n  │    element\n  └─────┘\n\nThese errors are very tricky, so read https://elm-lang.org/0.19.0/halting-problem to learn how to fix it!';}
var elm$core$List$append = F2(
	function (xs, ys) {
		if (!ys.b) {
			return xs;
		} else {
			return A3(elm$core$List$foldr, elm$core$List$cons, ys, xs);
		}
	});
var elm$core$List$concat = function (lists) {
	return A3(elm$core$List$foldr, elm$core$List$append, _List_Nil, lists);
};
var elm$parser$Parser$Advanced$end = function (x) {
	return elm$parser$Parser$Advanced$Parser(
		function (s) {
			return _Utils_eq(
				elm$core$String$length(s.src),
				s.offset) ? A3(elm$parser$Parser$Advanced$Good, false, _Utils_Tuple0, s) : A2(
				elm$parser$Parser$Advanced$Bad,
				false,
				A2(elm$parser$Parser$Advanced$fromState, s, x));
		});
};
var author$project$Markdown$Parser$childToParser = function (node) {
	if (node.$ === 'Element') {
		var tag = node.a;
		var attributes = node.b;
		var children = node.c;
		return A2(
			elm$parser$Parser$Advanced$andThen,
			function (childrenAsBlocks) {
				return elm$parser$Parser$Advanced$succeed(
					_List_fromArray(
						[
							A3(author$project$Markdown$Parser$Html, tag, attributes, childrenAsBlocks)
						]));
			},
			author$project$Markdown$Parser$nodesToBlocksParser(children));
	} else {
		var innerText = node.a;
		var _n3 = A2(
			elm$parser$Parser$Advanced$run,
			author$project$Markdown$Parser$cyclic$multiParser(),
			innerText);
		if (_n3.$ === 'Ok') {
			var value = _n3.a;
			return elm$parser$Parser$Advanced$succeed(value);
		} else {
			var error = _n3.a;
			return elm$parser$Parser$Advanced$problem(
				elm$parser$Parser$Expecting(
					A2(
						elm$core$String$join,
						'\n',
						A2(elm$core$List$map, author$project$Markdown$Parser$deadEndToString, error))));
		}
	}
};
var author$project$Markdown$Parser$nodesToBlocksParser = function (children) {
	return A2(
		elm$parser$Parser$Advanced$map,
		elm$core$List$concat,
		author$project$Markdown$Parser$combine(
			A2(elm$core$List$map, author$project$Markdown$Parser$childToParser, children)));
};
var author$project$Markdown$Parser$statementsHelp = function (revStmts) {
	return elm$parser$Parser$Advanced$oneOf(
		_List_fromArray(
			[
				A2(
				elm$parser$Parser$Advanced$keeper,
				A2(
					elm$parser$Parser$Advanced$keeper,
					A2(
						elm$parser$Parser$Advanced$keeper,
						elm$parser$Parser$Advanced$succeed(
							F3(
								function (offsetBefore, stmts, offsetAfter) {
									var madeProgress = _Utils_cmp(offsetAfter, offsetBefore) > 0;
									return madeProgress ? elm$parser$Parser$Advanced$Loop(
										A2(elm$core$List$cons, stmts, revStmts)) : elm$parser$Parser$Advanced$Done(
										elm$core$List$concat(
											elm$core$List$reverse(
												A2(elm$core$List$cons, stmts, revStmts))));
								})),
						elm$parser$Parser$Advanced$getOffset),
					elm$parser$Parser$Advanced$oneOf(
						_List_fromArray(
							[
								A2(elm$parser$Parser$Advanced$map, elm$core$List$singleton, author$project$Markdown$Parser$listBlock),
								A2(elm$parser$Parser$Advanced$map, elm$core$List$singleton, author$project$Markdown$Parser$blankLine),
								A2(elm$parser$Parser$Advanced$map, elm$core$List$singleton, author$project$Markdown$Parser$heading),
								A2(
								elm$parser$Parser$Advanced$map,
								elm$core$List$singleton,
								A2(elm$parser$Parser$Advanced$map, author$project$Markdown$Parser$CodeBlock, author$project$Markdown$CodeBlock$parser)),
								A2(
								elm$parser$Parser$Advanced$map,
								elm$core$List$singleton,
								author$project$Markdown$Parser$cyclic$htmlParser()),
								A2(
								elm$parser$Parser$Advanced$map,
								elm$core$List$singleton,
								author$project$Markdown$Parser$cyclic$htmlParser()),
								author$project$Markdown$Parser$plainLine
							]))),
				elm$parser$Parser$Advanced$getOffset),
				A2(
				elm$parser$Parser$Advanced$map,
				function (_n1) {
					return elm$parser$Parser$Advanced$Done(
						elm$core$List$concat(
							elm$core$List$reverse(revStmts)));
				},
				elm$parser$Parser$Advanced$succeed(_Utils_Tuple0))
			]));
};
var author$project$Markdown$Parser$xmlNodeToHtmlNode = function (parser) {
	return A2(
		elm$parser$Parser$Advanced$andThen,
		function (xmlNode) {
			if (xmlNode.$ === 'Text') {
				var innerText = xmlNode.a;
				return elm$parser$Parser$Advanced$succeed(
					author$project$Markdown$Parser$Body(
						_List_fromArray(
							[
								{
								string: innerText,
								style: {isBold: false, isCode: false, isItalic: false, link: elm$core$Maybe$Nothing}
							}
							])));
			} else {
				var tag = xmlNode.a;
				var attributes = xmlNode.b;
				var children = xmlNode.c;
				return A2(
					elm$parser$Parser$Advanced$andThen,
					function (parsedChildren) {
						return elm$parser$Parser$Advanced$succeed(
							A3(author$project$Markdown$Parser$Html, tag, attributes, parsedChildren));
					},
					author$project$Markdown$Parser$nodesToBlocksParser(children));
			}
		},
		parser);
};
function author$project$Markdown$Parser$cyclic$htmlParser() {
	return author$project$Markdown$Parser$xmlNodeToHtmlNode(author$project$XmlParser$element);
}
function author$project$Markdown$Parser$cyclic$multiParser() {
	return A2(
		elm$parser$Parser$Advanced$map,
		elm$core$List$filter(
			function (item) {
				return !_Utils_eq(
					item,
					author$project$Markdown$Parser$Body(_List_Nil));
			}),
		A2(
			elm$parser$Parser$Advanced$ignorer,
			A2(
				elm$parser$Parser$Advanced$loop,
				_List_fromArray(
					[_List_Nil]),
				author$project$Markdown$Parser$statementsHelp),
			elm$parser$Parser$Advanced$succeed(elm$parser$Parser$Advanced$end)));
}
try {
	var author$project$Markdown$Parser$htmlParser = author$project$Markdown$Parser$cyclic$htmlParser();
	author$project$Markdown$Parser$cyclic$htmlParser = function () {
		return author$project$Markdown$Parser$htmlParser;
	};
	var author$project$Markdown$Parser$multiParser = author$project$Markdown$Parser$cyclic$multiParser();
	author$project$Markdown$Parser$cyclic$multiParser = function () {
		return author$project$Markdown$Parser$multiParser;
	};
} catch ($) {
throw 'Some top-level definitions from `Markdown.Parser` are causing infinite recursion:\n\n  ┌─────┐\n  │    childToParser\n  │     ↓\n  │    htmlParser\n  │     ↓\n  │    multiParser\n  │     ↓\n  │    nodesToBlocksParser\n  │     ↓\n  │    statementsHelp\n  │     ↓\n  │    xmlNodeToHtmlNode\n  └─────┘\n\nThese errors are very tricky, so read https://elm-lang.org/0.19.0/halting-problem to learn how to fix it!';}
var author$project$Markdown$Parser$parse = function (input) {
	return A2(elm$parser$Parser$Advanced$run, author$project$Markdown$Parser$multiParser, input);
};
var author$project$Markdown$Parser$foldThing = F3(
	function (renderer, _n0, soFar) {
		var style = _n0.style;
		var string = _n0.string;
		var _n1 = style.link;
		if (_n1.$ === 'Just') {
			var link = _n1.a;
			return A2(
				elm$core$List$cons,
				A2(renderer.link, link, string),
				soFar);
		} else {
			return style.isBold ? A2(
				elm$core$List$cons,
				renderer.bold(string),
				soFar) : (style.isItalic ? A2(
				elm$core$List$cons,
				renderer.italic(string),
				soFar) : (style.isCode ? A2(
				elm$core$List$cons,
				renderer.code(string),
				soFar) : A2(
				elm$core$List$cons,
				renderer.plain(string),
				soFar)));
		}
	});
var author$project$Markdown$Parser$renderStyled = F2(
	function (renderer, styledStrings) {
		return A3(
			elm$core$List$foldr,
			author$project$Markdown$Parser$foldThing(renderer),
			_List_Nil,
			styledStrings);
	});
var elm$core$Result$andThen = F2(
	function (callback, result) {
		if (result.$ === 'Ok') {
			var value = result.a;
			return callback(value);
		} else {
			var msg = result.a;
			return elm$core$Result$Err(msg);
		}
	});
var author$project$Markdown$Parser$useRed = F5(
	function (tag, attributes, children, _n0, renderedChildren) {
		var redRenderer = _n0.a;
		return A2(
			elm$core$Result$andThen,
			function (okChildren) {
				return A2(
					elm$core$Result$map,
					function (myRenderer) {
						return myRenderer(okChildren);
					},
					A3(redRenderer, tag, attributes, children));
			},
			author$project$Markdown$Parser$combineResults(renderedChildren));
	});
var author$project$Markdown$Parser$renderHelper = F2(
	function (renderer, blocks) {
		return A2(
			elm$core$List$map,
			function (block) {
				switch (block.$) {
					case 'Heading':
						var level = block.a;
						var content = block.b;
						return elm$core$Result$Ok(
							A2(
								renderer.heading,
								level,
								A2(author$project$Markdown$Parser$renderStyled, renderer, content)));
					case 'Body':
						var content = block.a;
						return elm$core$Result$Ok(
							renderer.raw(
								A2(author$project$Markdown$Parser$renderStyled, renderer, content)));
					case 'Html':
						var tag = block.a;
						var attributes = block.b;
						var children = block.c;
						return A4(author$project$Markdown$Parser$renderHtmlNode, renderer, tag, attributes, children);
					case 'ListBlock':
						var items = block.a;
						return elm$core$Result$Ok(
							renderer.list(
								A2(
									elm$core$List$map,
									renderer.raw,
									A2(
										elm$core$List$map,
										author$project$Markdown$Parser$renderStyled(renderer),
										items))));
					default:
						var codeBlock = block.a;
						return elm$core$Result$Ok(
							renderer.codeBlock(codeBlock));
				}
			},
			blocks);
	});
var author$project$Markdown$Parser$renderHtmlNode = F4(
	function (renderer, tag, attributes, children) {
		return A5(
			author$project$Markdown$Parser$useRed,
			tag,
			attributes,
			children,
			renderer.htmlDecoder,
			A2(author$project$Markdown$Parser$renderHelper, renderer, children));
	});
var author$project$Markdown$Parser$render = F2(
	function (renderer, markdownText) {
		return A2(
			elm$core$Result$andThen,
			function (markdownAst) {
				return author$project$Markdown$Parser$combineResults(
					A2(author$project$Markdown$Parser$renderHelper, renderer, markdownAst));
			},
			A2(
				elm$core$Result$mapError,
				author$project$Markdown$Parser$deadEndsToString,
				author$project$Markdown$Parser$parse(markdownText)));
	});
var zwilias$elm_html_string$Html$Types$Node = F3(
	function (a, b, c) {
		return {$: 'Node', a: a, b: b, c: c};
	});
var zwilias$elm_html_string$Html$Types$Regular = function (a) {
	return {$: 'Regular', a: a};
};
var zwilias$elm_html_string$Html$String$node = F3(
	function (tag, attributes, children) {
		return A3(
			zwilias$elm_html_string$Html$Types$Node,
			tag,
			attributes,
			zwilias$elm_html_string$Html$Types$Regular(children));
	});
var zwilias$elm_html_string$Html$String$a = zwilias$elm_html_string$Html$String$node('a');
var zwilias$elm_html_string$Html$String$code = zwilias$elm_html_string$Html$String$node('code');
var zwilias$elm_html_string$Html$String$em = zwilias$elm_html_string$Html$String$node('em');
var zwilias$elm_html_string$Html$String$h1 = zwilias$elm_html_string$Html$String$node('h1');
var zwilias$elm_html_string$Html$String$h2 = zwilias$elm_html_string$Html$String$node('h2');
var zwilias$elm_html_string$Html$String$h3 = zwilias$elm_html_string$Html$String$node('h3');
var zwilias$elm_html_string$Html$String$h4 = zwilias$elm_html_string$Html$String$node('h4');
var zwilias$elm_html_string$Html$String$h5 = zwilias$elm_html_string$Html$String$node('h5');
var zwilias$elm_html_string$Html$String$h6 = zwilias$elm_html_string$Html$String$node('h6');
var zwilias$elm_html_string$Html$String$li = zwilias$elm_html_string$Html$String$node('li');
var zwilias$elm_html_string$Html$String$p = zwilias$elm_html_string$Html$String$node('p');
var zwilias$elm_html_string$Html$String$pre = zwilias$elm_html_string$Html$String$node('pre');
var zwilias$elm_html_string$Html$String$strong = zwilias$elm_html_string$Html$String$node('strong');
var zwilias$elm_html_string$Html$Types$TextNode = function (a) {
	return {$: 'TextNode', a: a};
};
var zwilias$elm_html_string$Html$String$text = zwilias$elm_html_string$Html$Types$TextNode;
var elm$core$Bitwise$and = _Bitwise_and;
var elm$core$Bitwise$shiftRightBy = _Bitwise_shiftRightBy;
var elm$core$String$repeatHelp = F3(
	function (n, chunk, result) {
		return (n <= 0) ? result : A3(
			elm$core$String$repeatHelp,
			n >> 1,
			_Utils_ap(chunk, chunk),
			(!(n & 1)) ? result : _Utils_ap(result, chunk));
	});
var elm$core$String$repeat = F2(
	function (n, chunk) {
		return A3(elm$core$String$repeatHelp, n, chunk, '');
	});
var zwilias$elm_html_string$Html$Types$indent = F3(
	function (perLevel, level, x) {
		return _Utils_ap(
			A2(elm$core$String$repeat, perLevel * level, ' '),
			x);
	});
var zwilias$elm_html_string$Html$Types$join = F2(
	function (between, list) {
		if (!list.b) {
			return '';
		} else {
			if (!list.b.b) {
				var x = list.a;
				return x;
			} else {
				var x = list.a;
				var xs = list.b;
				return A3(
					elm$core$List$foldl,
					F2(
						function (y, acc) {
							return _Utils_ap(
								y,
								_Utils_ap(between, acc));
						}),
					x,
					xs);
			}
		}
	});
var elm$core$Tuple$second = function (_n0) {
	var y = _n0.b;
	return y;
};
var zwilias$elm_html_string$Html$Types$closingTag = function (tagName) {
	return '</' + (tagName + '>');
};
var elm$core$Basics$composeR = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var elm$core$String$replace = F3(
	function (before, after, string) {
		return A2(
			elm$core$String$join,
			after,
			A2(elm$core$String$split, before, string));
	});
var zwilias$elm_html_string$Html$Types$escapeHtmlText = A2(
	elm$core$Basics$composeR,
	A2(elm$core$String$replace, '&', '&amp;'),
	A2(
		elm$core$Basics$composeR,
		A2(elm$core$String$replace, '<', '&lt;'),
		A2(elm$core$String$replace, '>', '&gt;')));
var NoRedInk$elm_string_conversions$String$Conversions$fromValue = function (value) {
	return A2(elm$json$Json$Encode$encode, 0, value);
};
var elm$core$String$foldl = _String_foldl;
var zwilias$elm_html_string$Html$Types$escape = A2(
	elm$core$String$foldl,
	F2(
		function (_char, acc) {
			return _Utils_eq(
				_char,
				_Utils_chr('\"')) ? (acc + '\\\"') : _Utils_ap(
				acc,
				elm$core$String$fromChar(_char));
		}),
	'');
var elm$core$Char$toLower = _Char_toLower;
var zwilias$elm_html_string$Html$Types$hyphenate = A2(
	elm$core$String$foldl,
	F2(
		function (_char, acc) {
			return elm$core$Char$isUpper(_char) ? (acc + ('-' + elm$core$String$fromChar(
				elm$core$Char$toLower(_char)))) : _Utils_ap(
				acc,
				elm$core$String$fromChar(_char));
		}),
	'');
var zwilias$elm_html_string$Html$Types$buildProp = F2(
	function (key, value) {
		return zwilias$elm_html_string$Html$Types$hyphenate(key) + ('=\"' + (zwilias$elm_html_string$Html$Types$escape(value) + '\"'));
	});
var zwilias$elm_html_string$Html$Types$propName = function (prop) {
	switch (prop) {
		case 'className':
			return 'class';
		case 'defaultValue':
			return 'value';
		case 'htmlFor':
			return 'for';
		default:
			return prop;
	}
};
var zwilias$elm_html_string$Html$Types$addAttribute = F2(
	function (attribute, acc) {
		var classes = acc.a;
		var styles = acc.b;
		var attrs = acc.c;
		switch (attribute.$) {
			case 'Attribute':
				var key = attribute.a;
				var value = attribute.b;
				return _Utils_Tuple3(
					classes,
					styles,
					A2(
						elm$core$List$cons,
						A2(zwilias$elm_html_string$Html$Types$buildProp, key, value),
						attrs));
			case 'StringProperty':
				if (attribute.a === 'className') {
					var value = attribute.b;
					return _Utils_Tuple3(
						A2(elm$core$List$cons, value, classes),
						styles,
						attrs);
				} else {
					var string = attribute.a;
					var value = attribute.b;
					return _Utils_Tuple3(
						classes,
						styles,
						A2(
							elm$core$List$cons,
							A2(
								zwilias$elm_html_string$Html$Types$buildProp,
								zwilias$elm_html_string$Html$Types$propName(string),
								value),
							attrs));
				}
			case 'BoolProperty':
				var string = attribute.a;
				var enabled = attribute.b;
				return enabled ? _Utils_Tuple3(
					classes,
					styles,
					A2(
						elm$core$List$cons,
						zwilias$elm_html_string$Html$Types$hyphenate(
							zwilias$elm_html_string$Html$Types$propName(string)),
						attrs)) : acc;
			case 'ValueProperty':
				var string = attribute.a;
				var value = attribute.b;
				return _Utils_Tuple3(
					classes,
					styles,
					A2(
						elm$core$List$cons,
						A2(
							zwilias$elm_html_string$Html$Types$buildProp,
							zwilias$elm_html_string$Html$Types$propName(string),
							NoRedInk$elm_string_conversions$String$Conversions$fromValue(value)),
						attrs));
			case 'Style':
				var key = attribute.a;
				var value = attribute.b;
				return _Utils_Tuple3(
					classes,
					A2(
						elm$core$List$cons,
						zwilias$elm_html_string$Html$Types$escape(key) + (': ' + zwilias$elm_html_string$Html$Types$escape(value)),
						styles),
					attrs);
			default:
				return acc;
		}
	});
var zwilias$elm_html_string$Html$Types$withClasses = F2(
	function (classes, attrs) {
		if (!classes.b) {
			return attrs;
		} else {
			return A2(
				elm$core$List$cons,
				A2(
					zwilias$elm_html_string$Html$Types$buildProp,
					'class',
					A2(zwilias$elm_html_string$Html$Types$join, ' ', classes)),
				attrs);
		}
	});
var zwilias$elm_html_string$Html$Types$withStyles = F2(
	function (styles, attrs) {
		if (!styles.b) {
			return attrs;
		} else {
			return A2(
				elm$core$List$cons,
				A2(
					zwilias$elm_html_string$Html$Types$buildProp,
					'style',
					A2(zwilias$elm_html_string$Html$Types$join, '; ', styles)),
				attrs);
		}
	});
var zwilias$elm_html_string$Html$Types$attributesToString = function (attrs) {
	var _n0 = A3(
		elm$core$List$foldl,
		zwilias$elm_html_string$Html$Types$addAttribute,
		_Utils_Tuple3(_List_Nil, _List_Nil, _List_Nil),
		attrs);
	var classes = _n0.a;
	var styles = _n0.b;
	var regular = _n0.c;
	return A2(
		zwilias$elm_html_string$Html$Types$withStyles,
		styles,
		A2(zwilias$elm_html_string$Html$Types$withClasses, classes, regular));
};
var zwilias$elm_html_string$Html$Types$tag = F2(
	function (tagName, attributes) {
		return '<' + (A2(
			elm$core$String$join,
			' ',
			A2(
				elm$core$List$cons,
				tagName,
				zwilias$elm_html_string$Html$Types$attributesToString(attributes))) + '>');
	});
var zwilias$elm_html_string$Html$Types$toStringHelper = F3(
	function (indenter, tags, acc) {
		toStringHelper:
		while (true) {
			if (!tags.b) {
				var _n1 = acc.stack;
				if (!_n1.b) {
					return acc;
				} else {
					var _n2 = _n1.a;
					var tagName = _n2.a;
					var cont = _n2.b;
					var rest = _n1.b;
					var $temp$indenter = indenter,
						$temp$tags = cont,
						$temp$acc = _Utils_update(
						acc,
						{
							depth: acc.depth - 1,
							result: A2(
								elm$core$List$cons,
								A2(
									indenter,
									acc.depth - 1,
									zwilias$elm_html_string$Html$Types$closingTag(tagName)),
								acc.result),
							stack: rest
						});
					indenter = $temp$indenter;
					tags = $temp$tags;
					acc = $temp$acc;
					continue toStringHelper;
				}
			} else {
				if (tags.a.$ === 'Node') {
					var _n3 = tags.a;
					var tagName = _n3.a;
					var attributes = _n3.b;
					var children = _n3.c;
					var rest = tags.b;
					switch (children.$) {
						case 'NoChildren':
							var $temp$indenter = indenter,
								$temp$tags = rest,
								$temp$acc = _Utils_update(
								acc,
								{
									result: A2(
										elm$core$List$cons,
										A2(
											indenter,
											acc.depth,
											A2(zwilias$elm_html_string$Html$Types$tag, tagName, attributes)),
										acc.result)
								});
							indenter = $temp$indenter;
							tags = $temp$tags;
							acc = $temp$acc;
							continue toStringHelper;
						case 'Regular':
							var childNodes = children.a;
							var $temp$indenter = indenter,
								$temp$tags = childNodes,
								$temp$acc = _Utils_update(
								acc,
								{
									depth: acc.depth + 1,
									result: A2(
										elm$core$List$cons,
										A2(
											indenter,
											acc.depth,
											A2(zwilias$elm_html_string$Html$Types$tag, tagName, attributes)),
										acc.result),
									stack: A2(
										elm$core$List$cons,
										_Utils_Tuple2(tagName, rest),
										acc.stack)
								});
							indenter = $temp$indenter;
							tags = $temp$tags;
							acc = $temp$acc;
							continue toStringHelper;
						default:
							var childNodes = children.a;
							var $temp$indenter = indenter,
								$temp$tags = A2(elm$core$List$map, elm$core$Tuple$second, childNodes),
								$temp$acc = _Utils_update(
								acc,
								{
									depth: acc.depth + 1,
									result: A2(
										elm$core$List$cons,
										A2(
											indenter,
											acc.depth,
											A2(zwilias$elm_html_string$Html$Types$tag, tagName, attributes)),
										acc.result),
									stack: A2(
										elm$core$List$cons,
										_Utils_Tuple2(tagName, rest),
										acc.stack)
								});
							indenter = $temp$indenter;
							tags = $temp$tags;
							acc = $temp$acc;
							continue toStringHelper;
					}
				} else {
					var string = tags.a.a;
					var rest = tags.b;
					var $temp$indenter = indenter,
						$temp$tags = rest,
						$temp$acc = _Utils_update(
						acc,
						{
							result: A2(
								elm$core$List$cons,
								A2(
									indenter,
									acc.depth,
									zwilias$elm_html_string$Html$Types$escapeHtmlText(string)),
								acc.result)
						});
					indenter = $temp$indenter;
					tags = $temp$tags;
					acc = $temp$acc;
					continue toStringHelper;
				}
			}
		}
	});
var zwilias$elm_html_string$Html$Types$toString = F2(
	function (depth, html) {
		var joinString = function () {
			if (!depth) {
				return '';
			} else {
				return '\n';
			}
		}();
		var initialAcc = {depth: 0, result: _List_Nil, stack: _List_Nil};
		var indenter = function () {
			if (!depth) {
				return elm$core$Basics$always(elm$core$Basics$identity);
			} else {
				return zwilias$elm_html_string$Html$Types$indent(depth);
			}
		}();
		return A2(
			zwilias$elm_html_string$Html$Types$join,
			joinString,
			A3(
				zwilias$elm_html_string$Html$Types$toStringHelper,
				indenter,
				_List_fromArray(
					[html]),
				initialAcc).result);
	});
var zwilias$elm_html_string$Html$String$toString = function (indent) {
	return zwilias$elm_html_string$Html$Types$toString(indent);
};
var zwilias$elm_html_string$Html$String$ul = zwilias$elm_html_string$Html$String$node('ul');
var zwilias$elm_html_string$Html$Types$StringProperty = F2(
	function (a, b) {
		return {$: 'StringProperty', a: a, b: b};
	});
var zwilias$elm_html_string$Html$String$Attributes$stringProperty = zwilias$elm_html_string$Html$Types$StringProperty;
var zwilias$elm_html_string$Html$String$Attributes$href = function (val) {
	return A2(zwilias$elm_html_string$Html$String$Attributes$stringProperty, 'href', val);
};
var author$project$OutputMarkdownHtml$renderMarkdown = function (markdown) {
	return A2(
		elm$core$Result$map,
		elm$core$String$join(''),
		A2(
			elm$core$Result$map,
			elm$core$List$map(
				zwilias$elm_html_string$Html$String$toString(0)),
			A2(
				author$project$Markdown$Parser$render,
				{
					bold: function (content) {
						return A2(
							zwilias$elm_html_string$Html$String$strong,
							_List_Nil,
							_List_fromArray(
								[
									zwilias$elm_html_string$Html$String$text(content)
								]));
					},
					code: function (content) {
						return A2(
							zwilias$elm_html_string$Html$String$code,
							_List_Nil,
							_List_fromArray(
								[
									zwilias$elm_html_string$Html$String$text(content)
								]));
					},
					codeBlock: function (_n0) {
						var body = _n0.body;
						var language = _n0.language;
						return A2(
							zwilias$elm_html_string$Html$String$pre,
							_List_Nil,
							_List_fromArray(
								[
									A2(
									zwilias$elm_html_string$Html$String$code,
									_List_Nil,
									_List_fromArray(
										[
											zwilias$elm_html_string$Html$String$text(body)
										]))
								]));
					},
					heading: F2(
						function (level, content) {
							switch (level) {
								case 1:
									return A2(zwilias$elm_html_string$Html$String$h1, _List_Nil, content);
								case 2:
									return A2(zwilias$elm_html_string$Html$String$h2, _List_Nil, content);
								case 3:
									return A2(zwilias$elm_html_string$Html$String$h3, _List_Nil, content);
								case 4:
									return A2(zwilias$elm_html_string$Html$String$h4, _List_Nil, content);
								case 5:
									return A2(zwilias$elm_html_string$Html$String$h5, _List_Nil, content);
								case 6:
									return A2(zwilias$elm_html_string$Html$String$h6, _List_Nil, content);
								default:
									return zwilias$elm_html_string$Html$String$text('TODO maye use a type here to clean it up... this will never happen');
							}
						}),
					htmlDecoder: author$project$Markdown$Parser$htmlOneOf(_List_Nil),
					italic: function (content) {
						return A2(
							zwilias$elm_html_string$Html$String$em,
							_List_Nil,
							_List_fromArray(
								[
									zwilias$elm_html_string$Html$String$text(content)
								]));
					},
					link: F2(
						function (link, content) {
							return A2(
								zwilias$elm_html_string$Html$String$a,
								_List_fromArray(
									[
										zwilias$elm_html_string$Html$String$Attributes$href(link.destination)
									]),
								_List_fromArray(
									[
										zwilias$elm_html_string$Html$String$text(content)
									]));
						}),
					list: function (items) {
						return A2(
							zwilias$elm_html_string$Html$String$ul,
							_List_Nil,
							A2(
								elm$core$List$map,
								function (itemBlocks) {
									return A2(
										zwilias$elm_html_string$Html$String$li,
										_List_Nil,
										_List_fromArray(
											[itemBlocks]));
								},
								items));
					},
					plain: zwilias$elm_html_string$Html$String$text,
					raw: zwilias$elm_html_string$Html$String$p(_List_Nil)
				},
				markdown)));
};
var author$project$OutputMarkdownHtml$update = F2(
	function (msg, model) {
		var markdown = msg.a;
		return _Utils_Tuple2(
			model,
			author$project$OutputMarkdownHtml$printHtml(
				author$project$OutputMarkdownHtml$renderMarkdown(markdown)));
	});
var elm$core$Platform$worker = _Platform_worker;
var elm$json$Json$Decode$succeed = _Json_succeed;
var author$project$OutputMarkdownHtml$main = elm$core$Platform$worker(
	{
		init: author$project$OutputMarkdownHtml$init,
		subscriptions: function (model) {
			return author$project$OutputMarkdownHtml$requestHtml(author$project$OutputMarkdownHtml$RequestedHtml);
		},
		update: author$project$OutputMarkdownHtml$update
	});
_Platform_export({'OutputMarkdownHtml':{'init':author$project$OutputMarkdownHtml$main(
	elm$json$Json$Decode$succeed(_Utils_Tuple0))(0)}});}(this));
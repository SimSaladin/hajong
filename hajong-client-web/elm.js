var Elm = Elm || { Native: {} };
Elm.Array = Elm.Array || {};
Elm.Array.make = function (_elm) {
   "use strict";
   _elm.Array = _elm.Array || {};
   if (_elm.Array.values)
   return _elm.Array.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Array",
   $Basics = Elm.Basics.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Array = Elm.Native.Array.make(_elm);
   var append = $Native$Array.append;
   var length = $Native$Array.length;
   var slice = $Native$Array.slice;
   var set = $Native$Array.set;
   var get = F2(function (i,
   array) {
      return _U.cmp(0,
      i) < 1 && _U.cmp(i,
      $Native$Array.length(array)) < 0 ? $Maybe.Just(A2($Native$Array.get,
      i,
      array)) : $Maybe.Nothing;
   });
   var push = $Native$Array.push;
   var empty = $Native$Array.empty;
   var filter = F2(function (isOkay,
   arr) {
      return function () {
         var update = F2(function (x,
         xs) {
            return isOkay(x) ? A2($Native$Array.push,
            x,
            xs) : xs;
         });
         return A3($Native$Array.foldl,
         update,
         $Native$Array.empty,
         arr);
      }();
   });
   var foldr = $Native$Array.foldr;
   var foldl = $Native$Array.foldl;
   var indexedMap = $Native$Array.indexedMap;
   var map = $Native$Array.map;
   var toIndexedList = function (array) {
      return A3($List.map2,
      F2(function (v0,v1) {
         return {ctor: "_Tuple2"
                ,_0: v0
                ,_1: v1};
      }),
      _L.range(0,
      $Native$Array.length(array) - 1),
      $Native$Array.toList(array));
   };
   var toList = $Native$Array.toList;
   var fromList = $Native$Array.fromList;
   var initialize = $Native$Array.initialize;
   var repeat = F2(function (n,e) {
      return A2(initialize,
      n,
      $Basics.always(e));
   });
   var Array = {ctor: "Array"};
   _elm.Array.values = {_op: _op
                       ,empty: empty
                       ,repeat: repeat
                       ,initialize: initialize
                       ,fromList: fromList
                       ,length: length
                       ,push: push
                       ,append: append
                       ,get: get
                       ,set: set
                       ,slice: slice
                       ,toList: toList
                       ,toIndexedList: toIndexedList
                       ,map: map
                       ,indexedMap: indexedMap
                       ,filter: filter
                       ,foldl: foldl
                       ,foldr: foldr};
   return _elm.Array.values;
};
Elm.Basics = Elm.Basics || {};
Elm.Basics.make = function (_elm) {
   "use strict";
   _elm.Basics = _elm.Basics || {};
   if (_elm.Basics.values)
   return _elm.Basics.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Basics",
   $Native$Basics = Elm.Native.Basics.make(_elm),
   $Native$Show = Elm.Native.Show.make(_elm),
   $Native$Utils = Elm.Native.Utils.make(_elm);
   var uncurry = F2(function (f,
   _v0) {
      return function () {
         switch (_v0.ctor)
         {case "_Tuple2": return A2(f,
              _v0._0,
              _v0._1);}
         _U.badCase($moduleName,
         "on line 595, column 3 to 8");
      }();
   });
   var curry = F3(function (f,
   a,
   b) {
      return f({ctor: "_Tuple2"
               ,_0: a
               ,_1: b});
   });
   var flip = F3(function (f,b,a) {
      return A2(f,a,b);
   });
   var snd = function (_v4) {
      return function () {
         switch (_v4.ctor)
         {case "_Tuple2": return _v4._1;}
         _U.badCase($moduleName,
         "on line 573, column 3 to 4");
      }();
   };
   var fst = function (_v8) {
      return function () {
         switch (_v8.ctor)
         {case "_Tuple2": return _v8._0;}
         _U.badCase($moduleName,
         "on line 567, column 3 to 4");
      }();
   };
   var always = F2(function (a,
   _v12) {
      return function () {
         return a;
      }();
   });
   var identity = function (x) {
      return x;
   };
   _op["<|"] = F2(function (f,x) {
      return f(x);
   });
   _op["|>"] = F2(function (x,f) {
      return f(x);
   });
   _op[">>"] = F3(function (f,
   g,
   x) {
      return g(f(x));
   });
   _op["<<"] = F3(function (g,
   f,
   x) {
      return g(f(x));
   });
   _op["++"] = $Native$Utils.append;
   var toString = $Native$Show.toString;
   var isInfinite = $Native$Basics.isInfinite;
   var isNaN = $Native$Basics.isNaN;
   var toFloat = $Native$Basics.toFloat;
   var ceiling = $Native$Basics.ceiling;
   var floor = $Native$Basics.floor;
   var truncate = $Native$Basics.truncate;
   var round = $Native$Basics.round;
   var otherwise = true;
   var not = $Native$Basics.not;
   var xor = $Native$Basics.xor;
   _op["||"] = $Native$Basics.or;
   _op["&&"] = $Native$Basics.and;
   var max = $Native$Basics.max;
   var min = $Native$Basics.min;
   var GT = {ctor: "GT"};
   var EQ = {ctor: "EQ"};
   var LT = {ctor: "LT"};
   var compare = $Native$Basics.compare;
   _op[">="] = $Native$Basics.ge;
   _op["<="] = $Native$Basics.le;
   _op[">"] = $Native$Basics.gt;
   _op["<"] = $Native$Basics.lt;
   _op["/="] = $Native$Basics.neq;
   _op["=="] = $Native$Basics.eq;
   var e = $Native$Basics.e;
   var pi = $Native$Basics.pi;
   var clamp = $Native$Basics.clamp;
   var logBase = $Native$Basics.logBase;
   var abs = $Native$Basics.abs;
   var negate = $Native$Basics.negate;
   var sqrt = $Native$Basics.sqrt;
   var atan2 = $Native$Basics.atan2;
   var atan = $Native$Basics.atan;
   var asin = $Native$Basics.asin;
   var acos = $Native$Basics.acos;
   var tan = $Native$Basics.tan;
   var sin = $Native$Basics.sin;
   var cos = $Native$Basics.cos;
   _op["^"] = $Native$Basics.exp;
   _op["%"] = $Native$Basics.mod;
   var rem = $Native$Basics.rem;
   _op["//"] = $Native$Basics.div;
   _op["/"] = $Native$Basics.floatDiv;
   _op["*"] = $Native$Basics.mul;
   _op["-"] = $Native$Basics.sub;
   _op["+"] = $Native$Basics.add;
   var toPolar = $Native$Basics.toPolar;
   var fromPolar = $Native$Basics.fromPolar;
   var turns = $Native$Basics.turns;
   var degrees = $Native$Basics.degrees;
   var radians = function (t) {
      return t;
   };
   _elm.Basics.values = {_op: _op
                        ,max: max
                        ,min: min
                        ,compare: compare
                        ,not: not
                        ,xor: xor
                        ,otherwise: otherwise
                        ,rem: rem
                        ,negate: negate
                        ,abs: abs
                        ,sqrt: sqrt
                        ,clamp: clamp
                        ,logBase: logBase
                        ,e: e
                        ,pi: pi
                        ,cos: cos
                        ,sin: sin
                        ,tan: tan
                        ,acos: acos
                        ,asin: asin
                        ,atan: atan
                        ,atan2: atan2
                        ,round: round
                        ,floor: floor
                        ,ceiling: ceiling
                        ,truncate: truncate
                        ,toFloat: toFloat
                        ,degrees: degrees
                        ,radians: radians
                        ,turns: turns
                        ,toPolar: toPolar
                        ,fromPolar: fromPolar
                        ,isNaN: isNaN
                        ,isInfinite: isInfinite
                        ,toString: toString
                        ,fst: fst
                        ,snd: snd
                        ,identity: identity
                        ,always: always
                        ,flip: flip
                        ,curry: curry
                        ,uncurry: uncurry
                        ,LT: LT
                        ,EQ: EQ
                        ,GT: GT};
   return _elm.Basics.values;
};
Elm.Char = Elm.Char || {};
Elm.Char.make = function (_elm) {
   "use strict";
   _elm.Char = _elm.Char || {};
   if (_elm.Char.values)
   return _elm.Char.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Char",
   $Basics = Elm.Basics.make(_elm),
   $Native$Char = Elm.Native.Char.make(_elm);
   var fromCode = $Native$Char.fromCode;
   var toCode = $Native$Char.toCode;
   var toLocaleLower = $Native$Char.toLocaleLower;
   var toLocaleUpper = $Native$Char.toLocaleUpper;
   var toLower = $Native$Char.toLower;
   var toUpper = $Native$Char.toUpper;
   var isBetween = F3(function (low,
   high,
   $char) {
      return function () {
         var code = toCode($char);
         return _U.cmp(code,
         toCode(low)) > -1 && _U.cmp(code,
         toCode(high)) < 1;
      }();
   });
   var isUpper = A2(isBetween,
   _U.chr("A"),
   _U.chr("Z"));
   var isLower = A2(isBetween,
   _U.chr("a"),
   _U.chr("z"));
   var isDigit = A2(isBetween,
   _U.chr("0"),
   _U.chr("9"));
   var isOctDigit = A2(isBetween,
   _U.chr("0"),
   _U.chr("7"));
   var isHexDigit = function ($char) {
      return isDigit($char) || (A3(isBetween,
      _U.chr("a"),
      _U.chr("f"),
      $char) || A3(isBetween,
      _U.chr("A"),
      _U.chr("F"),
      $char));
   };
   _elm.Char.values = {_op: _op
                      ,isUpper: isUpper
                      ,isLower: isLower
                      ,isDigit: isDigit
                      ,isOctDigit: isOctDigit
                      ,isHexDigit: isHexDigit
                      ,toUpper: toUpper
                      ,toLower: toLower
                      ,toLocaleUpper: toLocaleUpper
                      ,toLocaleLower: toLocaleLower
                      ,toCode: toCode
                      ,fromCode: fromCode};
   return _elm.Char.values;
};
Elm.Color = Elm.Color || {};
Elm.Color.make = function (_elm) {
   "use strict";
   _elm.Color = _elm.Color || {};
   if (_elm.Color.values)
   return _elm.Color.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Color",
   $Basics = Elm.Basics.make(_elm);
   var Radial = F5(function (a,
   b,
   c,
   d,
   e) {
      return {ctor: "Radial"
             ,_0: a
             ,_1: b
             ,_2: c
             ,_3: d
             ,_4: e};
   });
   var radial = Radial;
   var Linear = F3(function (a,
   b,
   c) {
      return {ctor: "Linear"
             ,_0: a
             ,_1: b
             ,_2: c};
   });
   var linear = Linear;
   var fmod = F2(function (f,n) {
      return function () {
         var integer = $Basics.floor(f);
         return $Basics.toFloat(A2($Basics._op["%"],
         integer,
         n)) + f - $Basics.toFloat(integer);
      }();
   });
   var rgbToHsl = F3(function (red,
   green,
   blue) {
      return function () {
         var b = $Basics.toFloat(blue) / 255;
         var g = $Basics.toFloat(green) / 255;
         var r = $Basics.toFloat(red) / 255;
         var cMax = A2($Basics.max,
         A2($Basics.max,r,g),
         b);
         var cMin = A2($Basics.min,
         A2($Basics.min,r,g),
         b);
         var c = cMax - cMin;
         var lightness = (cMax + cMin) / 2;
         var saturation = _U.eq(lightness,
         0) ? 0 : c / (1 - $Basics.abs(2 * lightness - 1));
         var hue = $Basics.degrees(60) * (_U.eq(cMax,
         r) ? A2(fmod,
         (g - b) / c,
         6) : _U.eq(cMax,
         g) ? (b - r) / c + 2 : _U.eq(cMax,
         b) ? (r - g) / c + 4 : _U.badIf($moduleName,
         "between lines 150 and 152"));
         return {ctor: "_Tuple3"
                ,_0: hue
                ,_1: saturation
                ,_2: lightness};
      }();
   });
   var hslToRgb = F3(function (hue,
   saturation,
   lightness) {
      return function () {
         var hue$ = hue / $Basics.degrees(60);
         var chroma = (1 - $Basics.abs(2 * lightness - 1)) * saturation;
         var x = chroma * (1 - $Basics.abs(A2(fmod,
         hue$,
         2) - 1));
         var $ = _U.cmp(hue$,
         0) < 0 ? {ctor: "_Tuple3"
                  ,_0: 0
                  ,_1: 0
                  ,_2: 0} : _U.cmp(hue$,
         1) < 0 ? {ctor: "_Tuple3"
                  ,_0: chroma
                  ,_1: x
                  ,_2: 0} : _U.cmp(hue$,
         2) < 0 ? {ctor: "_Tuple3"
                  ,_0: x
                  ,_1: chroma
                  ,_2: 0} : _U.cmp(hue$,
         3) < 0 ? {ctor: "_Tuple3"
                  ,_0: 0
                  ,_1: chroma
                  ,_2: x} : _U.cmp(hue$,
         4) < 0 ? {ctor: "_Tuple3"
                  ,_0: 0
                  ,_1: x
                  ,_2: chroma} : _U.cmp(hue$,
         5) < 0 ? {ctor: "_Tuple3"
                  ,_0: x
                  ,_1: 0
                  ,_2: chroma} : _U.cmp(hue$,
         6) < 0 ? {ctor: "_Tuple3"
                  ,_0: chroma
                  ,_1: 0
                  ,_2: x} : {ctor: "_Tuple3"
                            ,_0: 0
                            ,_1: 0
                            ,_2: 0},
         r = $._0,
         g = $._1,
         b = $._2;
         var m = lightness - chroma / 2;
         return {ctor: "_Tuple3"
                ,_0: r + m
                ,_1: g + m
                ,_2: b + m};
      }();
   });
   var toRgb = function (color) {
      return function () {
         switch (color.ctor)
         {case "HSLA":
            return function () {
                 var $ = A3(hslToRgb,
                 color._0,
                 color._1,
                 color._2),
                 r = $._0,
                 g = $._1,
                 b = $._2;
                 return {_: {}
                        ,alpha: color._3
                        ,blue: $Basics.round(255 * b)
                        ,green: $Basics.round(255 * g)
                        ,red: $Basics.round(255 * r)};
              }();
            case "RGBA": return {_: {}
                                ,alpha: color._3
                                ,blue: color._2
                                ,green: color._1
                                ,red: color._0};}
         _U.badCase($moduleName,
         "between lines 124 and 132");
      }();
   };
   var toHsl = function (color) {
      return function () {
         switch (color.ctor)
         {case "HSLA": return {_: {}
                              ,alpha: color._3
                              ,hue: color._0
                              ,lightness: color._2
                              ,saturation: color._1};
            case "RGBA":
            return function () {
                 var $ = A3(rgbToHsl,
                 color._0,
                 color._1,
                 color._2),
                 h = $._0,
                 s = $._1,
                 l = $._2;
                 return {_: {}
                        ,alpha: color._3
                        ,hue: h
                        ,lightness: l
                        ,saturation: s};
              }();}
         _U.badCase($moduleName,
         "between lines 114 and 121");
      }();
   };
   var HSLA = F4(function (a,
   b,
   c,
   d) {
      return {ctor: "HSLA"
             ,_0: a
             ,_1: b
             ,_2: c
             ,_3: d};
   });
   var hsla = F4(function (hue,
   saturation,
   lightness,
   alpha) {
      return A4(HSLA,
      hue - $Basics.turns($Basics.toFloat($Basics.floor(hue / (2 * $Basics.pi)))),
      saturation,
      lightness,
      alpha);
   });
   var hsl = F3(function (hue,
   saturation,
   lightness) {
      return A4(hsla,
      hue,
      saturation,
      lightness,
      1);
   });
   var complement = function (color) {
      return function () {
         switch (color.ctor)
         {case "HSLA": return A4(hsla,
              color._0 + $Basics.degrees(180),
              color._1,
              color._2,
              color._3);
            case "RGBA":
            return function () {
                 var $ = A3(rgbToHsl,
                 color._0,
                 color._1,
                 color._2),
                 h = $._0,
                 s = $._1,
                 l = $._2;
                 return A4(hsla,
                 h + $Basics.degrees(180),
                 s,
                 l,
                 color._3);
              }();}
         _U.badCase($moduleName,
         "between lines 105 and 111");
      }();
   };
   var grayscale = function (p) {
      return A4(HSLA,0,0,1 - p,1);
   };
   var greyscale = function (p) {
      return A4(HSLA,0,0,1 - p,1);
   };
   var RGBA = F4(function (a,
   b,
   c,
   d) {
      return {ctor: "RGBA"
             ,_0: a
             ,_1: b
             ,_2: c
             ,_3: d};
   });
   var rgba = RGBA;
   var rgb = F3(function (r,g,b) {
      return A4(RGBA,r,g,b,1);
   });
   var lightRed = A4(RGBA,
   239,
   41,
   41,
   1);
   var red = A4(RGBA,204,0,0,1);
   var darkRed = A4(RGBA,
   164,
   0,
   0,
   1);
   var lightOrange = A4(RGBA,
   252,
   175,
   62,
   1);
   var orange = A4(RGBA,
   245,
   121,
   0,
   1);
   var darkOrange = A4(RGBA,
   206,
   92,
   0,
   1);
   var lightYellow = A4(RGBA,
   255,
   233,
   79,
   1);
   var yellow = A4(RGBA,
   237,
   212,
   0,
   1);
   var darkYellow = A4(RGBA,
   196,
   160,
   0,
   1);
   var lightGreen = A4(RGBA,
   138,
   226,
   52,
   1);
   var green = A4(RGBA,
   115,
   210,
   22,
   1);
   var darkGreen = A4(RGBA,
   78,
   154,
   6,
   1);
   var lightBlue = A4(RGBA,
   114,
   159,
   207,
   1);
   var blue = A4(RGBA,
   52,
   101,
   164,
   1);
   var darkBlue = A4(RGBA,
   32,
   74,
   135,
   1);
   var lightPurple = A4(RGBA,
   173,
   127,
   168,
   1);
   var purple = A4(RGBA,
   117,
   80,
   123,
   1);
   var darkPurple = A4(RGBA,
   92,
   53,
   102,
   1);
   var lightBrown = A4(RGBA,
   233,
   185,
   110,
   1);
   var brown = A4(RGBA,
   193,
   125,
   17,
   1);
   var darkBrown = A4(RGBA,
   143,
   89,
   2,
   1);
   var black = A4(RGBA,0,0,0,1);
   var white = A4(RGBA,
   255,
   255,
   255,
   1);
   var lightGrey = A4(RGBA,
   238,
   238,
   236,
   1);
   var grey = A4(RGBA,
   211,
   215,
   207,
   1);
   var darkGrey = A4(RGBA,
   186,
   189,
   182,
   1);
   var lightGray = A4(RGBA,
   238,
   238,
   236,
   1);
   var gray = A4(RGBA,
   211,
   215,
   207,
   1);
   var darkGray = A4(RGBA,
   186,
   189,
   182,
   1);
   var lightCharcoal = A4(RGBA,
   136,
   138,
   133,
   1);
   var charcoal = A4(RGBA,
   85,
   87,
   83,
   1);
   var darkCharcoal = A4(RGBA,
   46,
   52,
   54,
   1);
   _elm.Color.values = {_op: _op
                       ,rgb: rgb
                       ,rgba: rgba
                       ,hsl: hsl
                       ,hsla: hsla
                       ,greyscale: greyscale
                       ,grayscale: grayscale
                       ,complement: complement
                       ,linear: linear
                       ,radial: radial
                       ,toRgb: toRgb
                       ,toHsl: toHsl
                       ,red: red
                       ,orange: orange
                       ,yellow: yellow
                       ,green: green
                       ,blue: blue
                       ,purple: purple
                       ,brown: brown
                       ,lightRed: lightRed
                       ,lightOrange: lightOrange
                       ,lightYellow: lightYellow
                       ,lightGreen: lightGreen
                       ,lightBlue: lightBlue
                       ,lightPurple: lightPurple
                       ,lightBrown: lightBrown
                       ,darkRed: darkRed
                       ,darkOrange: darkOrange
                       ,darkYellow: darkYellow
                       ,darkGreen: darkGreen
                       ,darkBlue: darkBlue
                       ,darkPurple: darkPurple
                       ,darkBrown: darkBrown
                       ,white: white
                       ,lightGrey: lightGrey
                       ,grey: grey
                       ,darkGrey: darkGrey
                       ,lightCharcoal: lightCharcoal
                       ,charcoal: charcoal
                       ,darkCharcoal: darkCharcoal
                       ,black: black
                       ,lightGray: lightGray
                       ,gray: gray
                       ,darkGray: darkGray};
   return _elm.Color.values;
};
Elm.Debug = Elm.Debug || {};
Elm.Debug.make = function (_elm) {
   "use strict";
   _elm.Debug = _elm.Debug || {};
   if (_elm.Debug.values)
   return _elm.Debug.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Debug",
   $Graphics$Collage = Elm.Graphics.Collage.make(_elm),
   $Native$Debug = Elm.Native.Debug.make(_elm);
   var trace = $Native$Debug.tracePath;
   var watchSummary = $Native$Debug.watchSummary;
   var watch = $Native$Debug.watch;
   var crash = $Native$Debug.crash;
   var log = $Native$Debug.log;
   _elm.Debug.values = {_op: _op
                       ,log: log
                       ,crash: crash
                       ,watch: watch
                       ,watchSummary: watchSummary
                       ,trace: trace};
   return _elm.Debug.values;
};
Elm.Dict = Elm.Dict || {};
Elm.Dict.make = function (_elm) {
   "use strict";
   _elm.Dict = _elm.Dict || {};
   if (_elm.Dict.values)
   return _elm.Dict.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Dict",
   $Basics = Elm.Basics.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Debug = Elm.Native.Debug.make(_elm),
   $String = Elm.String.make(_elm);
   var foldr = F3(function (f,
   acc,
   t) {
      return function () {
         switch (t.ctor)
         {case "RBEmpty":
            switch (t._0.ctor)
              {case "LBlack": return acc;}
              break;
            case "RBNode": return A3(foldr,
              f,
              A3(f,
              t._1,
              t._2,
              A3(foldr,f,acc,t._4)),
              t._3);}
         _U.badCase($moduleName,
         "between lines 408 and 416");
      }();
   });
   var keys = function (dict) {
      return A3(foldr,
      F3(function (key,
      value,
      keyList) {
         return A2($List._op["::"],
         key,
         keyList);
      }),
      _L.fromArray([]),
      dict);
   };
   var values = function (dict) {
      return A3(foldr,
      F3(function (key,
      value,
      valueList) {
         return A2($List._op["::"],
         value,
         valueList);
      }),
      _L.fromArray([]),
      dict);
   };
   var toList = function (dict) {
      return A3(foldr,
      F3(function (key,value,list) {
         return A2($List._op["::"],
         {ctor: "_Tuple2"
         ,_0: key
         ,_1: value},
         list);
      }),
      _L.fromArray([]),
      dict);
   };
   var foldl = F3(function (f,
   acc,
   dict) {
      return function () {
         switch (dict.ctor)
         {case "RBEmpty":
            switch (dict._0.ctor)
              {case "LBlack": return acc;}
              break;
            case "RBNode": return A3(foldl,
              f,
              A3(f,
              dict._1,
              dict._2,
              A3(foldl,f,acc,dict._3)),
              dict._4);}
         _U.badCase($moduleName,
         "between lines 397 and 405");
      }();
   });
   var isBBlack = function (dict) {
      return function () {
         switch (dict.ctor)
         {case "RBEmpty":
            switch (dict._0.ctor)
              {case "LBBlack": return true;}
              break;
            case "RBNode":
            switch (dict._0.ctor)
              {case "BBlack": return true;}
              break;}
         return false;
      }();
   };
   var showFlag = function (f) {
      return function () {
         switch (f.ctor)
         {case "Insert": return "Insert";
            case "Remove": return "Remove";
            case "Same": return "Same";}
         _U.badCase($moduleName,
         "between lines 173 and 179");
      }();
   };
   var Same = {ctor: "Same"};
   var Remove = {ctor: "Remove"};
   var Insert = {ctor: "Insert"};
   var get = F2(function (targetKey,
   dict) {
      return function () {
         switch (dict.ctor)
         {case "RBEmpty":
            switch (dict._0.ctor)
              {case "LBlack":
                 return $Maybe.Nothing;}
              break;
            case "RBNode":
            return function () {
                 var _v29 = A2($Basics.compare,
                 targetKey,
                 dict._1);
                 switch (_v29.ctor)
                 {case "EQ":
                    return $Maybe.Just(dict._2);
                    case "GT": return A2(get,
                      targetKey,
                      dict._4);
                    case "LT": return A2(get,
                      targetKey,
                      dict._3);}
                 _U.badCase($moduleName,
                 "between lines 129 and 135");
              }();}
         _U.badCase($moduleName,
         "between lines 124 and 135");
      }();
   });
   var member = F2(function (key,
   dict) {
      return function () {
         var _v30 = A2(get,key,dict);
         switch (_v30.ctor)
         {case "Just": return true;
            case "Nothing": return false;}
         _U.badCase($moduleName,
         "between lines 138 and 140");
      }();
   });
   var max = function (dict) {
      return function () {
         switch (dict.ctor)
         {case "RBEmpty":
            return $Native$Debug.crash("(max Empty) is not defined");
            case "RBNode":
            switch (dict._4.ctor)
              {case "RBEmpty":
                 return {ctor: "_Tuple2"
                        ,_0: dict._1
                        ,_1: dict._2};}
              return max(dict._4);}
         _U.badCase($moduleName,
         "between lines 100 and 121");
      }();
   };
   var min = function (dict) {
      return function () {
         switch (dict.ctor)
         {case "RBEmpty":
            switch (dict._0.ctor)
              {case "LBlack":
                 return $Native$Debug.crash("(min Empty) is not defined");}
              break;
            case "RBNode":
            switch (dict._3.ctor)
              {case "RBEmpty":
                 switch (dict._3._0.ctor)
                   {case "LBlack":
                      return {ctor: "_Tuple2"
                             ,_0: dict._1
                             ,_1: dict._2};}
                   break;}
              return min(dict._3);}
         _U.badCase($moduleName,
         "between lines 87 and 95");
      }();
   };
   var RBEmpty = function (a) {
      return {ctor: "RBEmpty"
             ,_0: a};
   };
   var RBNode = F5(function (a,
   b,
   c,
   d,
   e) {
      return {ctor: "RBNode"
             ,_0: a
             ,_1: b
             ,_2: c
             ,_3: d
             ,_4: e};
   });
   var showLColor = function (color) {
      return function () {
         switch (color.ctor)
         {case "LBBlack":
            return "LBBlack";
            case "LBlack": return "LBlack";}
         _U.badCase($moduleName,
         "between lines 70 and 72");
      }();
   };
   var LBBlack = {ctor: "LBBlack"};
   var LBlack = {ctor: "LBlack"};
   var empty = RBEmpty(LBlack);
   var map = F2(function (f,dict) {
      return function () {
         switch (dict.ctor)
         {case "RBEmpty":
            switch (dict._0.ctor)
              {case "LBlack":
                 return RBEmpty(LBlack);}
              break;
            case "RBNode": return A5(RBNode,
              dict._0,
              dict._1,
              A2(f,dict._1,dict._2),
              A2(map,f,dict._3),
              A2(map,f,dict._4));}
         _U.badCase($moduleName,
         "between lines 385 and 394");
      }();
   });
   var showNColor = function (c) {
      return function () {
         switch (c.ctor)
         {case "BBlack": return "BBlack";
            case "Black": return "Black";
            case "NBlack": return "NBlack";
            case "Red": return "Red";}
         _U.badCase($moduleName,
         "between lines 56 and 60");
      }();
   };
   var reportRemBug = F4(function (msg,
   c,
   lgot,
   rgot) {
      return $Native$Debug.crash($String.concat(_L.fromArray(["Internal red-black tree invariant violated, expected "
                                                             ,msg
                                                             ," and got "
                                                             ,showNColor(c)
                                                             ,"/"
                                                             ,lgot
                                                             ,"/"
                                                             ,rgot
                                                             ,"\nPlease report this bug to <https://github.com/elm-lang/Elm/issues>"])));
   });
   var NBlack = {ctor: "NBlack"};
   var BBlack = {ctor: "BBlack"};
   var Black = {ctor: "Black"};
   var ensureBlackRoot = function (dict) {
      return function () {
         switch (dict.ctor)
         {case "RBEmpty":
            switch (dict._0.ctor)
              {case "LBlack": return dict;}
              break;
            case "RBNode":
            switch (dict._0.ctor)
              {case "Black": return dict;
                 case "Red": return A5(RBNode,
                   Black,
                   dict._1,
                   dict._2,
                   dict._3,
                   dict._4);}
              break;}
         _U.badCase($moduleName,
         "between lines 145 and 157");
      }();
   };
   var blackish = function (t) {
      return function () {
         switch (t.ctor)
         {case "RBEmpty": return true;
            case "RBNode":
            return _U.eq(t._0,
              Black) || _U.eq(t._0,BBlack);}
         _U.badCase($moduleName,
         "between lines 330 and 332");
      }();
   };
   var blacken = function (t) {
      return function () {
         switch (t.ctor)
         {case "RBEmpty":
            return RBEmpty(LBlack);
            case "RBNode": return A5(RBNode,
              Black,
              t._1,
              t._2,
              t._3,
              t._4);}
         _U.badCase($moduleName,
         "between lines 369 and 371");
      }();
   };
   var Red = {ctor: "Red"};
   var moreBlack = function (color) {
      return function () {
         switch (color.ctor)
         {case "BBlack":
            return $Native$Debug.crash("Can\'t make a double black node more black!");
            case "Black": return BBlack;
            case "NBlack": return Red;
            case "Red": return Black;}
         _U.badCase($moduleName,
         "between lines 235 and 239");
      }();
   };
   var lessBlack = function (color) {
      return function () {
         switch (color.ctor)
         {case "BBlack": return Black;
            case "Black": return Red;
            case "NBlack":
            return $Native$Debug.crash("Can\'t make a negative black node less black!");
            case "Red": return NBlack;}
         _U.badCase($moduleName,
         "between lines 244 and 248");
      }();
   };
   var lessBlackTree = function (dict) {
      return function () {
         switch (dict.ctor)
         {case "RBEmpty":
            switch (dict._0.ctor)
              {case "LBBlack":
                 return RBEmpty(LBlack);}
              break;
            case "RBNode": return A5(RBNode,
              lessBlack(dict._0),
              dict._1,
              dict._2,
              dict._3,
              dict._4);}
         _U.badCase($moduleName,
         "between lines 253 and 255");
      }();
   };
   var redden = function (t) {
      return function () {
         switch (t.ctor)
         {case "RBEmpty":
            return $Native$Debug.crash("can\'t make a Leaf red");
            case "RBNode": return A5(RBNode,
              Red,
              t._1,
              t._2,
              t._3,
              t._4);}
         _U.badCase($moduleName,
         "between lines 377 and 382");
      }();
   };
   var balance_node = function (t) {
      return function () {
         var assemble = function (col) {
            return function (xk) {
               return function (xv) {
                  return function (yk) {
                     return function (yv) {
                        return function (zk) {
                           return function (zv) {
                              return function (a) {
                                 return function (b) {
                                    return function (c) {
                                       return function (d) {
                                          return A5(RBNode,
                                          lessBlack(col),
                                          yk,
                                          yv,
                                          A5(RBNode,Black,xk,xv,a,b),
                                          A5(RBNode,Black,zk,zv,c,d));
                                       };
                                    };
                                 };
                              };
                           };
                        };
                     };
                  };
               };
            };
         };
         return blackish(t) ? function () {
            switch (t.ctor)
            {case "RBNode":
               switch (t._3.ctor)
                 {case "RBNode":
                    switch (t._3._0.ctor)
                      {case "Red":
                         switch (t._3._3.ctor)
                           {case "RBNode":
                              switch (t._3._3._0.ctor)
                                {case "Red":
                                   return assemble(t._0)(t._3._3._1)(t._3._3._2)(t._3._1)(t._3._2)(t._1)(t._2)(t._3._3._3)(t._3._3._4)(t._3._4)(t._4);}
                                break;}
                           switch (t._3._4.ctor)
                           {case "RBNode":
                              switch (t._3._4._0.ctor)
                                {case "Red":
                                   return assemble(t._0)(t._3._1)(t._3._2)(t._3._4._1)(t._3._4._2)(t._1)(t._2)(t._3._3)(t._3._4._3)(t._3._4._4)(t._4);}
                                break;}
                           break;}
                      break;}
                 switch (t._4.ctor)
                 {case "RBNode":
                    switch (t._4._0.ctor)
                      {case "Red":
                         switch (t._4._3.ctor)
                           {case "RBNode":
                              switch (t._4._3._0.ctor)
                                {case "Red":
                                   return assemble(t._0)(t._1)(t._2)(t._4._3._1)(t._4._3._2)(t._4._1)(t._4._2)(t._3)(t._4._3._3)(t._4._3._4)(t._4._4);}
                                break;}
                           switch (t._4._4.ctor)
                           {case "RBNode":
                              switch (t._4._4._0.ctor)
                                {case "Red":
                                   return assemble(t._0)(t._1)(t._2)(t._4._1)(t._4._2)(t._4._4._1)(t._4._4._2)(t._3)(t._4._3)(t._4._4._3)(t._4._4._4);}
                                break;}
                           break;}
                      break;}
                 switch (t._0.ctor)
                 {case "BBlack":
                    switch (t._4.ctor)
                      {case "RBNode":
                         switch (t._4._0.ctor)
                           {case "NBlack":
                              switch (t._4._3.ctor)
                                {case "RBNode":
                                   switch (t._4._3._0.ctor)
                                     {case "Black":
                                        return function () {
                                             switch (t._4._4.ctor)
                                             {case "RBNode":
                                                switch (t._4._4._0.ctor)
                                                  {case "Black":
                                                     return A5(RBNode,
                                                       Black,
                                                       t._4._3._1,
                                                       t._4._3._2,
                                                       A5(RBNode,
                                                       Black,
                                                       t._1,
                                                       t._2,
                                                       t._3,
                                                       t._4._3._3),
                                                       A5(balance,
                                                       Black,
                                                       t._4._1,
                                                       t._4._2,
                                                       t._4._3._4,
                                                       redden(t._4._4)));}
                                                  break;}
                                             return t;
                                          }();}
                                     break;}
                                break;}
                           break;}
                      switch (t._3.ctor)
                      {case "RBNode":
                         switch (t._3._0.ctor)
                           {case "NBlack":
                              switch (t._3._4.ctor)
                                {case "RBNode":
                                   switch (t._3._4._0.ctor)
                                     {case "Black":
                                        return function () {
                                             switch (t._3._3.ctor)
                                             {case "RBNode":
                                                switch (t._3._3._0.ctor)
                                                  {case "Black":
                                                     return A5(RBNode,
                                                       Black,
                                                       t._3._4._1,
                                                       t._3._4._2,
                                                       A5(balance,
                                                       Black,
                                                       t._3._1,
                                                       t._3._2,
                                                       redden(t._3._3),
                                                       t._3._4._3),
                                                       A5(RBNode,
                                                       Black,
                                                       t._1,
                                                       t._2,
                                                       t._3._4._4,
                                                       t._4));}
                                                  break;}
                                             return t;
                                          }();}
                                     break;}
                                break;}
                           break;}
                      break;}
                 break;}
            return t;
         }() : t;
      }();
   };
   var balance = F5(function (c,
   k,
   v,
   l,
   r) {
      return balance_node(A5(RBNode,
      c,
      k,
      v,
      l,
      r));
   });
   var bubble = F5(function (c,
   k,
   v,
   l,
   r) {
      return isBBlack(l) || isBBlack(r) ? A5(balance,
      moreBlack(c),
      k,
      v,
      lessBlackTree(l),
      lessBlackTree(r)) : A5(RBNode,
      c,
      k,
      v,
      l,
      r);
   });
   var remove_max = F5(function (c,
   k,
   v,
   l,
   r) {
      return function () {
         switch (r.ctor)
         {case "RBEmpty": return A3(rem,
              c,
              l,
              r);
            case "RBNode": return A5(bubble,
              c,
              k,
              v,
              l,
              A5(remove_max,
              r._0,
              r._1,
              r._2,
              r._3,
              r._4));}
         _U.badCase($moduleName,
         "between lines 314 and 319");
      }();
   });
   var rem = F3(function (c,l,r) {
      return function () {
         var _v169 = {ctor: "_Tuple2"
                     ,_0: l
                     ,_1: r};
         switch (_v169.ctor)
         {case "_Tuple2":
            switch (_v169._0.ctor)
              {case "RBEmpty":
                 switch (_v169._1.ctor)
                   {case "RBEmpty":
                      return function () {
                           switch (c.ctor)
                           {case "Black":
                              return RBEmpty(LBBlack);
                              case "Red":
                              return RBEmpty(LBlack);}
                           _U.badCase($moduleName,
                           "between lines 273 and 277");
                        }();
                      case "RBNode":
                      return function () {
                           var _v191 = {ctor: "_Tuple3"
                                       ,_0: c
                                       ,_1: _v169._0._0
                                       ,_2: _v169._1._0};
                           switch (_v191.ctor)
                           {case "_Tuple3":
                              switch (_v191._0.ctor)
                                {case "Black":
                                   switch (_v191._1.ctor)
                                     {case "LBlack":
                                        switch (_v191._2.ctor)
                                          {case "Red": return A5(RBNode,
                                               Black,
                                               _v169._1._1,
                                               _v169._1._2,
                                               _v169._1._3,
                                               _v169._1._4);}
                                          break;}
                                     break;}
                                break;}
                           return A4(reportRemBug,
                           "Black/LBlack/Red",
                           c,
                           showLColor(_v169._0._0),
                           showNColor(_v169._1._0));
                        }();}
                   break;
                 case "RBNode":
                 switch (_v169._1.ctor)
                   {case "RBEmpty":
                      return function () {
                           var _v195 = {ctor: "_Tuple3"
                                       ,_0: c
                                       ,_1: _v169._0._0
                                       ,_2: _v169._1._0};
                           switch (_v195.ctor)
                           {case "_Tuple3":
                              switch (_v195._0.ctor)
                                {case "Black":
                                   switch (_v195._1.ctor)
                                     {case "Red":
                                        switch (_v195._2.ctor)
                                          {case "LBlack":
                                             return A5(RBNode,
                                               Black,
                                               _v169._0._1,
                                               _v169._0._2,
                                               _v169._0._3,
                                               _v169._0._4);}
                                          break;}
                                     break;}
                                break;}
                           return A4(reportRemBug,
                           "Black/Red/LBlack",
                           c,
                           showNColor(_v169._0._0),
                           showLColor(_v169._1._0));
                        }();
                      case "RBNode":
                      return function () {
                           var l$ = A5(remove_max,
                           _v169._0._0,
                           _v169._0._1,
                           _v169._0._2,
                           _v169._0._3,
                           _v169._0._4);
                           var r = A5(RBNode,
                           _v169._1._0,
                           _v169._1._1,
                           _v169._1._2,
                           _v169._1._3,
                           _v169._1._4);
                           var l = A5(RBNode,
                           _v169._0._0,
                           _v169._0._1,
                           _v169._0._2,
                           _v169._0._3,
                           _v169._0._4);
                           var $ = max(l),
                           k = $._0,
                           v = $._1;
                           return A5(bubble,c,k,v,l$,r);
                        }();}
                   break;}
              break;}
         _U.badCase($moduleName,
         "between lines 271 and 300");
      }();
   });
   var update = F3(function (k,
   alter,
   dict) {
      return function () {
         var up = function (dict) {
            return function () {
               switch (dict.ctor)
               {case "RBEmpty":
                  switch (dict._0.ctor)
                    {case "LBlack":
                       return function () {
                            var _v206 = alter($Maybe.Nothing);
                            switch (_v206.ctor)
                            {case "Just":
                               return {ctor: "_Tuple2"
                                      ,_0: Insert
                                      ,_1: A5(RBNode,
                                      Red,
                                      k,
                                      _v206._0,
                                      empty,
                                      empty)};
                               case "Nothing":
                               return {ctor: "_Tuple2"
                                      ,_0: Same
                                      ,_1: empty};}
                            _U.badCase($moduleName,
                            "between lines 185 and 189");
                         }();}
                    break;
                  case "RBNode":
                  return function () {
                       var _v208 = A2($Basics.compare,
                       k,
                       dict._1);
                       switch (_v208.ctor)
                       {case "EQ": return function () {
                               var _v209 = alter($Maybe.Just(dict._2));
                               switch (_v209.ctor)
                               {case "Just":
                                  return {ctor: "_Tuple2"
                                         ,_0: Same
                                         ,_1: A5(RBNode,
                                         dict._0,
                                         dict._1,
                                         _v209._0,
                                         dict._3,
                                         dict._4)};
                                  case "Nothing":
                                  return {ctor: "_Tuple2"
                                         ,_0: Remove
                                         ,_1: A3(rem,
                                         dict._0,
                                         dict._3,
                                         dict._4)};}
                               _U.badCase($moduleName,
                               "between lines 192 and 197");
                            }();
                          case "GT": return function () {
                               var $ = up(dict._4),
                               flag = $._0,
                               newRight = $._1;
                               return function () {
                                  switch (flag.ctor)
                                  {case "Insert":
                                     return {ctor: "_Tuple2"
                                            ,_0: Insert
                                            ,_1: A5(balance,
                                            dict._0,
                                            dict._1,
                                            dict._2,
                                            dict._3,
                                            newRight)};
                                     case "Remove":
                                     return {ctor: "_Tuple2"
                                            ,_0: Remove
                                            ,_1: A5(bubble,
                                            dict._0,
                                            dict._1,
                                            dict._2,
                                            dict._3,
                                            newRight)};
                                     case "Same":
                                     return {ctor: "_Tuple2"
                                            ,_0: Same
                                            ,_1: A5(RBNode,
                                            dict._0,
                                            dict._1,
                                            dict._2,
                                            dict._3,
                                            newRight)};}
                                  _U.badCase($moduleName,
                                  "between lines 206 and 211");
                               }();
                            }();
                          case "LT": return function () {
                               var $ = up(dict._3),
                               flag = $._0,
                               newLeft = $._1;
                               return function () {
                                  switch (flag.ctor)
                                  {case "Insert":
                                     return {ctor: "_Tuple2"
                                            ,_0: Insert
                                            ,_1: A5(balance,
                                            dict._0,
                                            dict._1,
                                            dict._2,
                                            newLeft,
                                            dict._4)};
                                     case "Remove":
                                     return {ctor: "_Tuple2"
                                            ,_0: Remove
                                            ,_1: A5(bubble,
                                            dict._0,
                                            dict._1,
                                            dict._2,
                                            newLeft,
                                            dict._4)};
                                     case "Same":
                                     return {ctor: "_Tuple2"
                                            ,_0: Same
                                            ,_1: A5(RBNode,
                                            dict._0,
                                            dict._1,
                                            dict._2,
                                            newLeft,
                                            dict._4)};}
                                  _U.badCase($moduleName,
                                  "between lines 199 and 204");
                               }();
                            }();}
                       _U.badCase($moduleName,
                       "between lines 190 and 211");
                    }();}
               _U.badCase($moduleName,
               "between lines 183 and 211");
            }();
         };
         var $ = up(dict),
         flag = $._0,
         updatedDict = $._1;
         return function () {
            switch (flag.ctor)
            {case "Insert":
               return ensureBlackRoot(updatedDict);
               case "Remove":
               return blacken(updatedDict);
               case "Same":
               return updatedDict;}
            _U.badCase($moduleName,
            "between lines 213 and 219");
         }();
      }();
   });
   var insert = F3(function (key,
   value,
   dict) {
      return A3(update,
      key,
      $Basics.always($Maybe.Just(value)),
      dict);
   });
   var singleton = F2(function (key,
   value) {
      return A3(insert,
      key,
      value,
      RBEmpty(LBlack));
   });
   var union = F2(function (t1,
   t2) {
      return A3(foldl,
      insert,
      t2,
      t1);
   });
   var fromList = function (assocs) {
      return A3($List.foldl,
      F2(function (_v214,dict) {
         return function () {
            switch (_v214.ctor)
            {case "_Tuple2":
               return A3(insert,
                 _v214._0,
                 _v214._1,
                 dict);}
            _U.badCase($moduleName,
            "on line 457, column 38 to 59");
         }();
      }),
      empty,
      assocs);
   };
   var filter = F2(function (predicate,
   dictionary) {
      return function () {
         var add = F3(function (key,
         value,
         dict) {
            return A2(predicate,
            key,
            value) ? A3(insert,
            key,
            value,
            dict) : dict;
         });
         return A3(foldl,
         add,
         empty,
         dictionary);
      }();
   });
   var intersect = F2(function (t1,
   t2) {
      return A2(filter,
      F2(function (k,_v218) {
         return function () {
            return A2(member,k,t2);
         }();
      }),
      t1);
   });
   var partition = F2(function (predicate,
   dict) {
      return function () {
         var add = F3(function (key,
         value,
         _v220) {
            return function () {
               switch (_v220.ctor)
               {case "_Tuple2":
                  return A2(predicate,
                    key,
                    value) ? {ctor: "_Tuple2"
                             ,_0: A3(insert,
                             key,
                             value,
                             _v220._0)
                             ,_1: _v220._1} : {ctor: "_Tuple2"
                                              ,_0: _v220._0
                                              ,_1: A3(insert,
                                              key,
                                              value,
                                              _v220._1)};}
               _U.badCase($moduleName,
               "between lines 478 and 480");
            }();
         });
         return A3(foldl,
         add,
         {ctor: "_Tuple2"
         ,_0: empty
         ,_1: empty},
         dict);
      }();
   });
   var remove = F2(function (key,
   dict) {
      return A3(update,
      key,
      $Basics.always($Maybe.Nothing),
      dict);
   });
   var diff = F2(function (t1,t2) {
      return A3(foldl,
      F3(function (k,v,t) {
         return A2(remove,k,t);
      }),
      t1,
      t2);
   });
   _elm.Dict.values = {_op: _op
                      ,empty: empty
                      ,singleton: singleton
                      ,insert: insert
                      ,update: update
                      ,get: get
                      ,remove: remove
                      ,member: member
                      ,filter: filter
                      ,partition: partition
                      ,foldl: foldl
                      ,foldr: foldr
                      ,map: map
                      ,union: union
                      ,intersect: intersect
                      ,diff: diff
                      ,keys: keys
                      ,values: values
                      ,toList: toList
                      ,fromList: fromList};
   return _elm.Dict.values;
};
Elm.Events = Elm.Events || {};
Elm.Events.make = function (_elm) {
   "use strict";
   _elm.Events = _elm.Events || {};
   if (_elm.Events.values)
   return _elm.Events.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Events",
   $GameTypes = Elm.GameTypes.make(_elm);
   var forceStart = function (n) {
      return $GameTypes.ForceStart({_: {}
                                   ,ident: n});
   };
   var joinGame = function (n) {
      return $GameTypes.JoinGame({_: {}
                                 ,ident: n
                                 ,nick: ""});
   };
   _elm.Events.values = {_op: _op
                        ,joinGame: joinGame
                        ,forceStart: forceStart};
   return _elm.Events.values;
};
Elm.Game = Elm.Game || {};
Elm.Game.make = function (_elm) {
   "use strict";
   _elm.Game = _elm.Game || {};
   if (_elm.Game.values)
   return _elm.Game.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Game",
   $Basics = Elm.Basics.make(_elm),
   $Color = Elm.Color.make(_elm),
   $GameTypes = Elm.GameTypes.make(_elm),
   $Graphics$Collage = Elm.Graphics.Collage.make(_elm),
   $Graphics$Element = Elm.Graphics.Element.make(_elm),
   $Graphics$Input = Elm.Graphics.Input.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $Text = Elm.Text.make(_elm),
   $Time = Elm.Time.make(_elm),
   $Util = Elm.Util.make(_elm);
   var kantsu = function (t) {
      return A3($GameTypes.Mentsu,
      $GameTypes.Kantsu,
      t,
      $Maybe.Nothing);
   };
   var processTurnAction = F3(function (player,
   action,
   gs) {
      return function () {
         var _v0 = gs.roundState;
         switch (_v0.ctor)
         {case "Just":
            return function () {
                 switch (action.ctor)
                 {case "TurnAnkan":
                    return _U.replace([["roundState"
                                       ,$Maybe.Just(_U.replace([["hands"
                                                                ,A3($Util.listModify,
                                                                player,
                                                                function (h) {
                                                                   return _U.replace([["called"
                                                                                      ,A2($Basics._op["++"],
                                                                                      h.called,
                                                                                      _L.fromArray([kantsu(action._0)]))]],
                                                                   h);
                                                                },
                                                                _v0._0.hands)]],
                                       _v0._0))]],
                      gs);
                    case "TurnShouminkan":
                    return gs;
                    case "TurnTileDiscard":
                    return _U.replace([["roundState"
                                       ,$Maybe.Just(_U.replace([["hands"
                                                                ,A3($Util.listModify,
                                                                player,
                                                                function (h) {
                                                                   return _U.replace([["discards"
                                                                                      ,A2($Basics._op["++"],
                                                                                      h.discards,
                                                                                      _L.fromArray([action._0]))]
                                                                                     ,["riichi"
                                                                                      ,h.riichi || action._0.riichi]],
                                                                   h);
                                                                },
                                                                _v0._0.hands)]],
                                       _v0._0))]],
                      gs);
                    case "TurnTileDraw":
                    return _U.replace([["roundState"
                                       ,$Maybe.Just(_U.replace([["tilesleft"
                                                                ,_v0._0.tilesleft - 1]],
                                       _v0._0))]],
                      gs);
                    case "TurnTsumo": return gs;}
                 _U.badCase($moduleName,
                 "between lines 399 and 416");
              }();}
         _U.badCase($moduleName,
         "between lines 398 and 416");
      }();
   });
   var updateHand = F2(function (player,
   hand) {
      return A2($Util.listModify,
      player,
      function (_v8) {
         return function () {
            return hand;
         }();
      });
   });
   var setPoints = F3(function (p,
   n,
   gs) {
      return function () {
         var _v10 = gs.roundState;
         switch (_v10.ctor)
         {case "Just": return gs;
            case "Nothing": return gs;}
         _U.badCase($moduleName,
         "between lines 388 and 390");
      }();
   });
   var setRiichi = F2(function (pk,
   gs) {
      return function () {
         var _v12 = gs.roundState;
         switch (_v12.ctor)
         {case "Just":
            return _U.replace([["roundState"
                               ,$Maybe.Just(_U.replace([["hands"
                                                        ,A3($Util.listModify,
                                                        pk,
                                                        function (h) {
                                                           return _U.replace([["riichi"
                                                                              ,true]],
                                                           h);
                                                        },
                                                        _v12._0.hands)]],
                               _v12._0))]],
              gs);
            case "Nothing": return gs;}
         _U.badCase($moduleName,
         "between lines 384 and 386");
      }();
   });
   var flipDora = F2(function (tile,
   gs) {
      return function () {
         var _v14 = gs.roundState;
         switch (_v14.ctor)
         {case "Just":
            return _U.replace([["roundState"
                               ,$Maybe.Just(_U.replace([["dora"
                                                        ,A2($List._op["::"],
                                                        tile,
                                                        _v14._0.dora)]
                                                       ,["tilesleft"
                                                        ,_v14._0.tilesleft - 1]],
                               _v14._0))]],
              gs);
            case "Nothing": return gs;}
         _U.badCase($moduleName,
         "between lines 379 and 382");
      }();
   });
   var setResults = F2(function (res,
   gs) {
      return function () {
         var _v16 = gs.roundState;
         switch (_v16.ctor)
         {case "Just":
            return _U.replace([["roundState"
                               ,$Maybe.Just(_U.replace([["results"
                                                        ,$Maybe.Just(res)]],
                               _v16._0))]],
              gs);
            case "Nothing": return gs;}
         _U.badCase($moduleName,
         "between lines 375 and 377");
      }();
   });
   var setNick = F3(function (pk,
   nick,
   gs) {
      return function () {
         var _v18 = gs.roundState;
         switch (_v18.ctor)
         {case "Just":
            return _U.replace([["roundState"
                               ,$Maybe.Just(_U.replace([["players"
                                                        ,A3($Util.listModify,
                                                        pk,
                                                        function (_v20) {
                                                           return function () {
                                                              switch (_v20.ctor)
                                                              {case "_Tuple3":
                                                                 return {ctor: "_Tuple3"
                                                                        ,_0: _v20._0
                                                                        ,_1: _v20._1
                                                                        ,_2: nick};}
                                                              _U.badCase($moduleName,
                                                              "on line 372, column 94 to 104");
                                                           }();
                                                        },
                                                        _v18._0.players)]],
                               _v18._0))]],
              gs);
            case "Nothing": return gs;}
         _U.badCase($moduleName,
         "between lines 371 and 373");
      }();
   });
   var setPlayerHand = F3(function (player,
   hand,
   gs) {
      return function () {
         var _v25 = gs.roundState;
         switch (_v25.ctor)
         {case "Just":
            return _U.replace([["roundState"
                               ,$Maybe.Just(_U.replace([["hands"
                                                        ,A3(updateHand,
                                                        player,
                                                        hand,
                                                        _v25._0.hands)]],
                               _v25._0))]],
              gs);
            case "Nothing": return gs;}
         _U.badCase($moduleName,
         "between lines 367 and 369");
      }();
   });
   var setTurnPlayer = F2(function (player,
   gs) {
      return function () {
         var _v27 = gs.roundState;
         switch (_v27.ctor)
         {case "Just":
            return _U.replace([["roundState"
                               ,$Maybe.Just(_U.replace([["turn"
                                                        ,player]],
                               _v27._0))]],
              gs);
            case "Nothing": return gs;}
         _U.badCase($moduleName,
         "between lines 363 and 365");
      }();
   });
   var setMyHand = F2(function (hand,
   gs) {
      return function () {
         var _v29 = gs.roundState;
         switch (_v29.ctor)
         {case "Just":
            return _U.replace([["roundState"
                               ,$Maybe.Just(_U.replace([["myhand"
                                                        ,hand]],
                               _v29._0))]],
              gs);
            case "Nothing": return gs;}
         _U.badCase($moduleName,
         "between lines 359 and 361");
      }();
   });
   var processInGameEvent = F2(function (event,
   gs) {
      return function () {
         switch (event.ctor)
         {case "RoundEnded":
            return A2(setResults,
              event._0,
              gs);
            case "RoundFlippedDora":
            return A2(flipDora,
              event._0.tile,
              gs);
            case "RoundGamePoints":
            return A3(setPoints,
              event._0.player,
              event._0.points,
              gs);
            case "RoundHandChanged":
            return A3(setPlayerHand,
              event._0.player_kaze,
              event._0.hand,
              gs);
            case "RoundNick":
            return A3(setNick,
              event._0.player_kaze,
              event._0.nick,
              gs);
            case "RoundPrivateChange":
            return A2(setMyHand,
              event._0.hand,
              gs);
            case "RoundPrivateStarts":
            return _U.replace([["status"
                               ,$GameTypes.InGame]
                              ,["gameWait",$Maybe.Nothing]
                              ,["roundState"
                               ,$Maybe.Just(event._0)]],
              gs);
            case "RoundPrivateWaitForShout":
            return _U.replace([["waitShout"
                               ,$Maybe.Just({ctor: "_Tuple2"
                                            ,_0: A2($GameTypes.WaitRecord,
                                            event._0.seconds,
                                            gs.updated)
                                            ,_1: A3($Util.maybe,
                                            event._0.shouts,
                                            function (x) {
                                               return A2($Basics._op["++"],
                                               $Basics.snd(x),
                                               event._0.shouts);
                                            },
                                            gs.waitShout)})]],
              gs);
            case "RoundPrivateWaitForTurnAction":
            return _U.replace([["waitTurnAction"
                               ,$Maybe.Just(A2($GameTypes.WaitRecord,
                               event._0.seconds,
                               gs.updated))]
                              ,["riichiWith"
                               ,event._0.riichiWith]],
              gs);
            case "RoundRiichi":
            return A2(setRiichi,
              event._0.player_kaze,
              gs);
            case "RoundTurnAction":
            return A3(processTurnAction,
              event._0.player_kaze,
              event._0.action,
              gs);
            case "RoundTurnBegins":
            return function (gs) {
                 return _U.replace([["turnBegan"
                                    ,gs.updated]
                                   ,["waitTurnAction"
                                    ,$Maybe.Nothing]
                                   ,["waitShout",$Maybe.Nothing]
                                   ,["riichiWith"
                                    ,_L.fromArray([])]],
                 gs);
              }(setTurnPlayer(event._0.player_kaze)(A2($Util.log,
              A2($Basics._op["++"],
              "Turn of ",
              $Basics.toString(event._0.player_kaze)),
              gs)));
            case "RoundTurnShouted":
            return A2($Util.log,
              A2($Basics._op["++"],
              $Basics.toString(event._0.player_kaze),
              A2($Basics._op["++"],
              " shouted: ",
              $Basics.toString(event._0.shout))),
              gs);}
         _U.badCase($moduleName,
         "between lines 316 and 356");
      }();
   });
   var buttonElem$ = F2(function (str,
   col) {
      return A3($Graphics$Collage.collage,
      80,
      40,
      _L.fromArray([$Graphics$Collage.filled(col)(A2($Graphics$Collage.oval,
                   80,
                   40))
                   ,$Graphics$Collage.toForm($Graphics$Element.centered($Text.fromString(str)))]));
   });
   var counted = F2(function (m,
   ts) {
      return function () {
         var go = F2(function (n,xs) {
            return function () {
               switch (xs.ctor)
               {case "::": return _U.eq(n,
                    0) ? $Maybe.Just(xs._0) : function () {
                       switch (xs._1.ctor)
                       {case "::": return _U.eq(xs._0,
                            xs._1._0) ? A2(go,
                            n - 1,
                            xs._1) : A2(counted,m,xs._1);}
                       return $Maybe.Nothing;
                    }();}
               return $Maybe.Nothing;
            }();
         });
         return A2(go,m,ts);
      }();
   });
   var findShouminkan = function (h) {
      return $List.map(function (_) {
         return _.tile;
      })(A2($List.filter,
      function (x) {
         return _U.eq(x.mentsuKind,
         $GameTypes.Koutsu) && (A2($List.any,
         function (t) {
            return _U.eq(t,x.tile);
         },
         h.concealed) || _U.eq(h.pick,
         $Maybe.Just(x.tile)));
      },
      h.called));
   };
   var findFourTiles = function (rs) {
      return counted(4)($GameTypes.sortTiles(A2($Basics._op["++"],
      rs.myhand.concealed,
      A3($Util.maybe,
      _L.fromArray([]),
      function (x) {
         return _L.fromArray([x]);
      },
      rs.myhand.pick))));
   };
   var tileImage = function (tile) {
      return function () {
         var $ = function () {
            switch (tile.ctor)
            {case "Honor":
               switch (tile._0.ctor)
                 {case "Kazehai":
                    return {ctor: "_Tuple2"
                           ,_0: 222 + $GameTypes.kazeNth(tile._0._0) * 97
                           ,_1: 356};
                    case "Sangenpai":
                    return {ctor: "_Tuple2"
                           ,_0: 707 + $GameTypes.sangenNth(tile._0._0) * 97
                           ,_1: 356};}
                 break;
               case "Suited":
               switch (tile._0.ctor)
                 {case "ManTile":
                    return {ctor: "_Tuple2"
                           ,_0: 125 + (tile._1 - 1) * 97
                           ,_1: 47};
                    case "PinTile":
                    return {ctor: "_Tuple2"
                           ,_0: 125 + (tile._1 - 1) * 97
                           ,_1: 142};
                    case "SouTile":
                    return {ctor: "_Tuple2"
                           ,_0: 125 + (tile._1 - 1) * 97
                           ,_1: 237};}
                 break;}
            _U.badCase($moduleName,
            "between lines 255 and 261");
         }(),
         row = $._0,
         col = $._1;
         return A4($Graphics$Element.croppedImage,
         {ctor: "_Tuple2"
         ,_0: row
         ,_1: col},
         62,
         82,
         "/static/img/Mahjong-tiles.jpg");
      }();
   };
   var dispYaku = function (_v58) {
      return function () {
         return A2($Graphics$Element.beside,
         A2($Graphics$Element.beside,
         $Graphics$Element.show(_v58.name),
         $Graphics$Element.show(" ")),
         $Graphics$Element.show(_v58.han));
      }();
   };
   var dispHandValue = function (_v60) {
      return function () {
         return A2($Graphics$Element.above,
         A2($Graphics$Element.flow,
         $Graphics$Element.right,
         _L.fromArray([$Graphics$Element.show(_v60.han)
                      ,$Graphics$Element.show(" Han, ")
                      ,$Graphics$Element.show(_v60.fu)
                      ,$Graphics$Element.show(" Fu.")])),
         A2($Graphics$Element.flow,
         $Graphics$Element.down,
         A2($List.map,
         dispYaku,
         _v60.yaku)));
      }();
   };
   var dispPlayerInfo = F2(function (rs,
   k) {
      return function () {
         var $ = A2($Util.listFind,
         k,
         rs.players),
         p = $._0,
         points = $._1,
         name = $._2;
         return $Graphics$Collage.toForm(A2($Graphics$Element.above,
         A2($Graphics$Element.beside,
         $Graphics$Element.show(p),
         $Graphics$Element.centered(_U.eq(name,
         "") ? $Text.fromString("(bot)") : $Text.bold($Text.fromString(name)))),
         A2($Graphics$Element.flow,
         $Graphics$Element.right,
         _L.fromArray([A2($Graphics$Element.spacer,
                      5,
                      5)
                      ,$Graphics$Element.show(k)
                      ,A2($Graphics$Element.spacer,
                      5,
                      5)
                      ,$Graphics$Element.show(points)]))));
      }();
   });
   var turnIndicator = function (gs) {
      return function () {
         var col = $Util.isNothing(gs.waitShout) ? $Color.lightGreen : $Color.lightOrange;
         return $Graphics$Collage.group(_L.fromArray([$Graphics$Collage.rotate($Basics.degrees(90))($Graphics$Collage.filled(col)(A2($Graphics$Collage.ngon,
                                                     3,
                                                     80)))
                                                     ,$Graphics$Collage.moveY(30)($Graphics$Collage.toForm($Graphics$Element.show($Basics.floor(function () {
                                                        var _v62 = gs.waitTurnAction;
                                                        switch (_v62.ctor)
                                                        {case "Just":
                                                           return $Basics.toFloat(_v62._0.seconds) - $Time.inSeconds(gs.updated - _v62._0.added);
                                                           case "Nothing":
                                                           return $Time.inSeconds(gs.updated - gs.turnBegan);}
                                                        _U.badCase($moduleName,
                                                        "between lines 152 and 155");
                                                     }()))))]));
      }();
   };
   var playerRiichi = $Graphics$Collage.toForm(A3($Graphics$Element.image,
   200,
   20,
   "/static/img/point1000.svg"));
   var moveRotateKaze = F3(function (off,
   mypos,
   pos) {
      return function () {
         var _v64 = A2($Basics._op["%"],
         $GameTypes.kazeNth(pos) - $GameTypes.kazeNth(mypos),
         4);
         switch (_v64)
         {case 0:
            return $Graphics$Collage.moveY(0 - off);
            case 1: return function ($) {
                 return $Graphics$Collage.moveX(off)($Graphics$Collage.rotate($Basics.degrees(90))($));
              };
            case 2: return function ($) {
                 return $Graphics$Collage.moveY(off)($Graphics$Collage.rotate($Basics.degrees(180))($));
              };
            case 3: return function ($) {
                 return $Graphics$Collage.moveX(0 - off)($Graphics$Collage.rotate($Basics.degrees(270))($));
              };}
         _U.badCase($moduleName,
         "between lines 119 and 123");
      }();
   });
   var getShout = A2($Util.maybe,
   $Maybe.Nothing,
   function (k) {
      return $Maybe.Just(A4($GameTypes.Shout,
      k,
      $GameTypes.Ton,
      A3($GameTypes.Suited,
      $GameTypes.ManTile,
      1,
      false),
      _L.fromArray([])));
   });
   var shoutChooseTile = $Signal.mailbox($Maybe.Nothing);
   var nocare = $Signal.mailbox(false);
   var nocareButton = F2(function (w,
   str) {
      return function () {
         switch (w.ctor)
         {case "Just": switch (w._0.ctor)
              {case "_Tuple2":
                 switch (w._0._1.ctor)
                   {case "::":
                      return $Graphics$Input.clickable(A2($Signal.message,
                        nocare.address,
                        true))(A2(buttonElem$,
                        str,
                        $Color.green));}
                   break;}
              break;}
         return $Graphics$Element.empty;
      }();
   });
   var shouminkan = $Signal.mailbox($Maybe.Nothing);
   var shouminkanButtons = F2(function (rs,
   str) {
      return $List.map(function (t) {
         return $Graphics$Input.clickable(A2($Signal.message,
         shouminkan.address,
         $Maybe.Just(t)))(A2(buttonElem$,
         str,
         $Color.blue));
      })(findShouminkan(rs.myhand));
   });
   var ankan = $Signal.mailbox($Maybe.Nothing);
   var ankanButton = F2(function (rs,
   str) {
      return function () {
         var _v71 = findFourTiles(rs);
         switch (_v71.ctor)
         {case "Just":
            return A2($Graphics$Input.clickable,
              A2($Signal.message,
              ankan.address,
              $Maybe.Just(_v71._0)),
              A2(buttonElem$,
              str,
              $Color.green));
            case "Nothing":
            return $Graphics$Element.empty;}
         _U.badCase($moduleName,
         "between lines 279 and 281");
      }();
   });
   var shout = $Signal.mailbox($Maybe.Nothing);
   var shoutButton = function (s) {
      return $Graphics$Input.clickable(A2($Signal.message,
      shout.address,
      $Maybe.Just(s)))(A3($Graphics$Collage.collage,
      80,
      40,
      _L.fromArray([$Graphics$Collage.filled($Color.green)(A2($Graphics$Collage.oval,
                   80,
                   40))
                   ,$Graphics$Collage.toForm($Graphics$Element.centered($Text.fromString($Basics.toString(s.shoutKind))))])));
   };
   var discardHover = $Signal.mailbox($Maybe.Nothing);
   var tsumo = $Signal.mailbox($Maybe.Nothing);
   var tsumoButton = function (b) {
      return b ? $Graphics$Input.clickable(A2($Signal.message,
      tsumo.address,
      $Maybe.Just({ctor: "_Tuple0"})))(A2(buttonElem$,
      "Tsumo",
      $Color.orange)) : $Graphics$Element.empty;
   };
   var riichi = $Signal.mailbox($Maybe.Nothing);
   var riichiButtons = $List.map(function (t) {
      return A2($Graphics$Input.clickable,
      A2($Signal.message,
      riichi.address,
      $Maybe.Just(A3($GameTypes.Discard,
      t,
      $Maybe.Nothing,
      true))),
      A2(buttonElem$,
      "Riichi",
      $Color.red));
   });
   var discard = $Signal.mailbox($Maybe.Nothing);
   var events = $Signal.mergeMany(_L.fromArray([A2($Signal._op["<~"],
                                               A2($Util.maybe,
                                               $GameTypes.Noop,
                                               function ($) {
                                                  return $GameTypes.InGameAction($GameTypes.GameTurn($GameTypes.TurnTileDiscard($)));
                                               }),
                                               discard.signal)
                                               ,A2($Signal._op["<~"],
                                               A2($Util.maybe,
                                               $GameTypes.Noop,
                                               function ($) {
                                                  return $GameTypes.InGameAction($GameTypes.GameTurn($GameTypes.TurnTileDiscard($)));
                                               }),
                                               riichi.signal)
                                               ,A2($Signal._op["<~"],
                                               A2($Util.maybe,
                                               $GameTypes.Noop,
                                               function ($) {
                                                  return $GameTypes.InGameAction($GameTypes.GameShout($));
                                               }),
                                               shout.signal)
                                               ,A2($Signal._op["<~"],
                                               A2($Util.maybe,
                                               $GameTypes.Noop,
                                               function ($) {
                                                  return $GameTypes.InGameAction($GameTypes.GameTurn($GameTypes.TurnAnkan($)));
                                               }),
                                               ankan.signal)
                                               ,A2($Signal._op["<~"],
                                               A2($Util.maybe,
                                               $GameTypes.Noop,
                                               function ($) {
                                                  return $GameTypes.InGameAction($GameTypes.GameTurn($GameTypes.TurnShouminkan($)));
                                               }),
                                               shouminkan.signal)
                                               ,A2($Signal._op["<~"],
                                               A2($Util.maybe,
                                               $GameTypes.Noop,
                                               function (_v73) {
                                                  return function () {
                                                     return $GameTypes.InGameAction($GameTypes.GameTurn($GameTypes.TurnTsumo));
                                                  }();
                                               }),
                                               tsumo.signal)
                                               ,A2($Signal._op["<~"],
                                               function (x) {
                                                  return x ? $GameTypes.InGameAction($GameTypes.GameDontCare) : $GameTypes.Noop;
                                               },
                                               nocare.signal)]));
   var Controls = function (a) {
      return {_: {}
             ,hoveredTile: a};
   };
   var controls = A2($Signal._op["<~"],
   Controls,
   $Signal.dropRepeats(discardHover.signal));
   var riichi_off = 130;
   var called_off = 650;
   var discards_off = 330;
   var t_h = 82;
   var t_w = 62;
   var dispTile = function (tile) {
      return A3($Graphics$Element.container,
      t_w + 4,
      t_h + 4,
      $Graphics$Element.middle)($Graphics$Input.hoverable(function (h) {
         return $Signal.message(discardHover.address)(h ? $Maybe.Just(tile) : $Maybe.Nothing);
      })(A2($Graphics$Element.size,
      t_w,
      t_h)(tileImage(tile))));
   };
   var dispDiscard = function (d) {
      return d.riichi ? A3($Graphics$Collage.collage,
      t_h + 4,
      t_w + 4,
      _L.fromArray([$Graphics$Collage.rotate($Basics.degrees(90))($Graphics$Collage.toForm(dispTile(d.tile)))])) : dispTile(d.tile);
   };
   var dispHandDiscards = F2(function (co,
   h) {
      return A3($Graphics$Element.container,
      6 * (t_w + 4) + 2,
      3 * (t_h + 4),
      $Graphics$Element.topLeft)($Graphics$Element.flow($Graphics$Element.down)($List.map(function ($) {
         return $Graphics$Element.flow($Graphics$Element.right)($List.map(dispDiscard)($));
      })($Util.groupInto(6)($List.filter(function ($) {
         return $Util.isNothing(function (_) {
            return _.to;
         }($));
      })(h.discards)))));
   });
   var dispDiscards = F3(function (co,
   gs,
   rs) {
      return $Graphics$Collage.group(A2($List.map,
      function (k) {
         return A3(moveRotateKaze,
         discards_off,
         rs.mypos,
         k)($Graphics$Collage.toForm(dispHandDiscards(co)(A2($Util.listFind,
         k,
         rs.hands))));
      },
      _L.fromArray([$GameTypes.Ton
                   ,$GameTypes.$Nan
                   ,$GameTypes.Shaa
                   ,$GameTypes.Pei])));
   });
   var dispMentsu = F2(function (k,
   m) {
      return function () {
         var _v75 = m.from;
         switch (_v75.ctor)
         {case "Just":
            return function () {
                 var t = $Graphics$Collage.rotate($Basics.degrees(90))($Graphics$Collage.toForm(dispTile(_v75._0.shoutTile)));
                 var _ = A2($List.map,
                 dispTile,
                 _v75._0.shoutTo);
                 var a = function () {
                    switch (_.ctor)
                    {case "::": switch (_._1.ctor)
                         {case "::": return _._0;}
                         break;}
                    _U.badCase($moduleName,
                    "on line 230, column 28 to 50");
                 }();
                 var b = function () {
                    switch (_.ctor)
                    {case "::": switch (_._1.ctor)
                         {case "::": return _._1._0;}
                         break;}
                    _U.badCase($moduleName,
                    "on line 230, column 28 to 50");
                 }();
                 var xs = function () {
                    switch (_.ctor)
                    {case "::": switch (_._1.ctor)
                         {case "::": return _._1._1;}
                         break;}
                    _U.badCase($moduleName,
                    "on line 230, column 28 to 50");
                 }();
                 var shoutTile = function () {
                    switch (xs.ctor)
                    {case "[]":
                       return A3($Graphics$Collage.collage,
                         t_h,
                         t_w,
                         _L.fromArray([t]));}
                    return A3($Graphics$Collage.collage,
                    t_h,
                    2 * t_w,
                    _L.fromArray([A2($Graphics$Collage.moveY,
                                 t_w / 2,
                                 t)
                                 ,A2($Graphics$Collage.moveY,
                                 (0 - t_w) / 2,
                                 t)]));
                 }();
                 return $Graphics$Element.flow($Graphics$Element.right)(function () {
                    var _v93 = $Basics.abs($GameTypes.kazeNth(_v75._0.shoutFrom) - $GameTypes.kazeNth(k));
                    switch (_v93)
                    {case 1:
                       return A2($List._op["::"],
                         a,
                         A2($List._op["::"],
                         b,
                         A2($List._op["::"],
                         shoutTile,
                         _L.fromArray([]))));
                       case 2:
                       return A2($List._op["::"],
                         a,
                         A2($List._op["::"],
                         shoutTile,
                         A2($List._op["::"],
                         b,
                         _L.fromArray([]))));
                       case 3:
                       return A2($List._op["::"],
                         shoutTile,
                         A2($List._op["::"],
                         a,
                         A2($List._op["::"],
                         b,
                         _L.fromArray([]))));}
                    _U.badCase($moduleName,
                    "between lines 235 and 238");
                 }());
              }();
            case "Nothing":
            return $Graphics$Element.empty;}
         _U.badCase($moduleName,
         "between lines 226 and 238");
      }();
   });
   var dispValued = function (_v94) {
      return function () {
         return A2($Graphics$Element.above,
         A2($Graphics$Element.flow,
         $Graphics$Element.right,
         A2($Basics._op["++"],
         A2($List.map,
         dispTile,
         _v94.tiles),
         A2($List.map,
         dispMentsu($GameTypes.Ton),
         _v94.mentsu))),
         dispHandValue(_v94.value));
      }();
   };
   var dispWinner = function (_v96) {
      return function () {
         return $Graphics$Element.flow($Graphics$Element.down)(_L.fromArray([$Graphics$Element.show(_v96.player)
                                                                            ,dispValued(_v96.valuehand)]));
      }();
   };
   var dispResults = function (res) {
      return function () {
         var $ = function () {
            switch (res.ctor)
            {case "DealDraw":
               return {ctor: "_Tuple2"
                      ,_0: $Color.lightYellow
                      ,_1: _L.fromArray([A2($Graphics$Element.beside,
                                        $Graphics$Element.show("Tenpai players: "),
                                        $Graphics$Element.show(res._0.tenpai))
                                        ,A2($Graphics$Element.beside,
                                        $Graphics$Element.show("Nooten: "),
                                        $Graphics$Element.show(res._0.nooten))])};
               case "DealRon":
               return {ctor: "_Tuple2"
                      ,_0: $Color.lightGreen
                      ,_1: _L.fromArray([A2($Graphics$Element.beside,
                                        $Graphics$Element.show("Win: "),
                                        $Graphics$Element.show(res._0.winners))
                                        ,A2($Graphics$Element.beside,
                                        $Graphics$Element.show("Payers: "),
                                        $Graphics$Element.show(res._0.payers))])};
               case "DealTsumo":
               return {ctor: "_Tuple2"
                      ,_0: $Color.lightBlue
                      ,_1: A2($Basics._op["++"],
                      _L.fromArray([$Graphics$Element.show("Tsumo! ")]),
                      A2($Basics._op["++"],
                      _L.fromArray([$Graphics$Element.flow($Graphics$Element.down)(A2($List.map,
                      dispWinner,
                      res._0.winners))]),
                      _L.fromArray([A2($Graphics$Element.beside,
                      $Graphics$Element.show("Payers: "),
                      $Graphics$Element.show(res._0.payers))])))};}
            _U.badCase($moduleName,
            "between lines 166 and 179");
         }(),
         col = $._0,
         view = $._1;
         return $Graphics$Collage.toForm($Graphics$Element.color(col)(A3($Graphics$Element.container,
         700,
         300,
         $Graphics$Element.middle)(A2($Graphics$Element.flow,
         $Graphics$Element.down,
         view))));
      }();
   };
   var dispPublicMentsu = F3(function (co,
   k,
   h) {
      return $Graphics$Element.flow($Graphics$Element.right)(A2($List.map,
      dispMentsu(k),
      h.called));
   });
   var dispCalled = F3(function (co,
   gs,
   rs) {
      return $Graphics$Collage.group($List.map(function (k) {
         return A3(moveRotateKaze,
         called_off,
         rs.mypos,
         k)($Graphics$Collage.toForm(A2(dispPublicMentsu,
         co,
         k)(A2($Util.listFind,
         k,
         rs.hands))));
      })($List.filter(function (x) {
         return !_U.eq(x,rs.mypos);
      })(_L.fromArray([$GameTypes.Ton
                      ,$GameTypes.$Nan
                      ,$GameTypes.Shaa
                      ,$GameTypes.Pei]))));
   });
   var dispWanpai = function (co) {
      return function ($) {
         return $Graphics$Element.flow($Graphics$Element.right)($List.map(dispTile)(function (_) {
            return _.dora;
         }($)));
      };
   };
   var dispInfoBlock = F3(function (co,
   gs,
   rs) {
      return $Graphics$Collage.toForm($Graphics$Element.color($Color.black)(A3($Graphics$Element.container,
      234,
      234,
      $Graphics$Element.middle)($Graphics$Element.color($Color.white)(A2($Graphics$Element.size,
      230,
      230)(A2($Graphics$Collage.collage,
      230,
      230)(A2($Basics._op["++"],
      _L.fromArray([A3(moveRotateKaze,
                   90,
                   rs.mypos,
                   rs.turn)(turnIndicator(gs))
                   ,$Graphics$Collage.toForm(A2($Graphics$Element.beside,
                   $Graphics$Element.centered($Text.bold($Text.fromString($Basics.toString(rs.round)))),
                   $Graphics$Element.show(A2($Basics._op["++"],
                   " ",
                   $Basics.toString(rs.deal)))))
                   ,$Graphics$Collage.move({ctor: "_Tuple2"
                                           ,_0: -60
                                           ,_1: 60})($Graphics$Collage.scale(0.6)($Graphics$Collage.toForm(A2(dispWanpai,
                   co,
                   rs))))
                   ,_U.cmp(rs.honba,
                   0) > 0 ? $Graphics$Collage.move({ctor: "_Tuple2"
                                                   ,_0: -60
                                                   ,_1: -60})($Graphics$Collage.toForm($Graphics$Element.centered($Text.fromString(A2($Basics._op["++"],
                   $Basics.toString(rs.honba * 100),
                   "H"))))) : $Graphics$Collage.toForm($Graphics$Element.empty)
                   ,$Graphics$Collage.moveY(-30)($Graphics$Collage.toForm($Graphics$Element.centered($Text.fromString($Basics.toString(rs.tilesleft)))))]),
      A2($List.map,
      function (k) {
         return A3(moveRotateKaze,
         95,
         rs.mypos,
         k)(A2(dispPlayerInfo,rs,k));
      },
      _L.fromArray([$GameTypes.Ton
                   ,$GameTypes.$Nan
                   ,$GameTypes.Shaa
                   ,$GameTypes.Pei])))))))));
   });
   var dispTileClickable = F2(function (co,
   tile) {
      return $Graphics$Input.clickable(A2($Signal.message,
      discard.address,
      $Maybe.Just(A3($GameTypes.Discard,
      tile,
      $Maybe.Nothing,
      false))))(dispTile(tile));
   });
   var dispHand = F3(function (k,
   co,
   hand) {
      return A2($Graphics$Element.flow,
      $Graphics$Element.right,
      _L.fromArray([$Graphics$Element.flow($Graphics$Element.right)($List.map(dispTileClickable(co))($GameTypes.sortTiles(hand.concealed)))
                   ,A2($Graphics$Element.spacer,
                   10,
                   10)
                   ,A3($Util.maybe,
                   $Graphics$Element.empty,
                   function ($) {
                      return $Graphics$Element.color($Color.lightGreen)(dispTileClickable(co)($));
                   },
                   hand.pick)
                   ,A2($Graphics$Element.spacer,
                   10,
                   10)
                   ,$Graphics$Element.flow($Graphics$Element.right)(A2($List.map,
                   dispMentsu(k),
                   hand.called))]));
   });
   var display = F2(function (co,
   gs) {
      return function () {
         var _v102 = gs.roundState;
         switch (_v102.ctor)
         {case "Just":
            return A2($Graphics$Element.flow,
              $Graphics$Element.down,
              _L.fromArray([A3($Graphics$Element.container,
                           1000,
                           650,
                           $Graphics$Element.midTop)(A3($Graphics$Collage.collage,
                           1000,
                           650,
                           _L.fromArray([A3(dispInfoBlock,
                                        co,
                                        gs,
                                        _v102._0)
                                        ,$Graphics$Collage.scale(0.7)(A3(dispDiscards,
                                        co,
                                        gs,
                                        _v102._0))
                                        ,$Graphics$Collage.scale(0.6)(A3(dispCalled,
                                        co,
                                        gs,
                                        _v102._0))
                                        ,$Graphics$Collage.group($List.map(function ($) {
                                           return function (k) {
                                              return A4(moveRotateKaze,
                                              riichi_off,
                                              _v102._0.mypos,
                                              k,
                                              playerRiichi);
                                           }($Basics.fst($));
                                        })(A2($List.filter,
                                        function ($) {
                                           return function (_) {
                                              return _.riichi;
                                           }($Basics.snd($));
                                        },
                                        _v102._0.hands)))
                                        ,A3($Util.maybe,
                                        $Graphics$Collage.toForm($Graphics$Element.empty),
                                        dispResults,
                                        _v102._0.results)])))
                           ,A3($Graphics$Element.container,
                           1000,
                           40,
                           $Graphics$Element.midTop)($Graphics$Element.flow($Graphics$Element.right)(A2($Basics._op["++"],
                           A3($Util.maybe,
                           _L.fromArray([]),
                           function ($) {
                              return $List.map(shoutButton)($Basics.snd($));
                           },
                           gs.waitShout),
                           A2($Basics._op["++"],
                           A2(shouminkanButtons,
                           _v102._0,
                           "Shouminkan"),
                           A2($Basics._op["++"],
                           _L.fromArray([A2(ankanButton,
                                        _v102._0,
                                        "Ankan")
                                        ,A2(nocareButton,
                                        gs.waitShout,
                                        "Pass")]),
                           A2($Basics._op["++"],
                           riichiButtons(gs.riichiWith),
                           _L.fromArray([tsumoButton(_v102._0.myhand.canTsumo)])))))))
                           ,A3($Graphics$Element.container,
                           1000,
                           100,
                           $Graphics$Element.midTop)(A3(dispHand,
                           _v102._0.mypos,
                           co,
                           _v102._0.myhand))]));
            case "Nothing":
            return $Graphics$Element.show("Hmm, roundState is Nothing but I should be in a game");}
         _U.badCase($moduleName,
         "between lines 74 and 95");
      }();
   });
   _elm.Game.values = {_op: _op
                      ,t_w: t_w
                      ,t_h: t_h
                      ,discards_off: discards_off
                      ,called_off: called_off
                      ,riichi_off: riichi_off
                      ,Controls: Controls
                      ,controls: controls
                      ,discard: discard
                      ,riichi: riichi
                      ,tsumo: tsumo
                      ,discardHover: discardHover
                      ,shout: shout
                      ,ankan: ankan
                      ,shouminkan: shouminkan
                      ,nocare: nocare
                      ,shoutChooseTile: shoutChooseTile
                      ,events: events
                      ,getShout: getShout
                      ,display: display
                      ,dispDiscards: dispDiscards
                      ,dispCalled: dispCalled
                      ,moveRotateKaze: moveRotateKaze
                      ,playerRiichi: playerRiichi
                      ,dispInfoBlock: dispInfoBlock
                      ,turnIndicator: turnIndicator
                      ,dispPlayerInfo: dispPlayerInfo
                      ,dispResults: dispResults
                      ,dispWinner: dispWinner
                      ,dispValued: dispValued
                      ,dispHandValue: dispHandValue
                      ,dispYaku: dispYaku
                      ,dispHand: dispHand
                      ,dispHandDiscards: dispHandDiscards
                      ,dispDiscard: dispDiscard
                      ,dispPublicMentsu: dispPublicMentsu
                      ,dispMentsu: dispMentsu
                      ,dispTile: dispTile
                      ,dispWanpai: dispWanpai
                      ,dispTileClickable: dispTileClickable
                      ,tileImage: tileImage
                      ,shoutButton: shoutButton
                      ,shouminkanButtons: shouminkanButtons
                      ,riichiButtons: riichiButtons
                      ,ankanButton: ankanButton
                      ,tsumoButton: tsumoButton
                      ,nocareButton: nocareButton
                      ,findFourTiles: findFourTiles
                      ,findShouminkan: findShouminkan
                      ,counted: counted
                      ,buttonElem$: buttonElem$
                      ,processInGameEvent: processInGameEvent
                      ,setMyHand: setMyHand
                      ,setTurnPlayer: setTurnPlayer
                      ,setPlayerHand: setPlayerHand
                      ,setNick: setNick
                      ,setResults: setResults
                      ,flipDora: flipDora
                      ,setRiichi: setRiichi
                      ,setPoints: setPoints
                      ,updateHand: updateHand
                      ,processTurnAction: processTurnAction
                      ,kantsu: kantsu};
   return _elm.Game.values;
};
Elm.GameTypes = Elm.GameTypes || {};
Elm.GameTypes.make = function (_elm) {
   "use strict";
   _elm.GameTypes = _elm.GameTypes || {};
   if (_elm.GameTypes.values)
   return _elm.GameTypes.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "GameTypes",
   $Basics = Elm.Basics.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Set = Elm.Set.make(_elm),
   $Time = Elm.Time.make(_elm);
   var Shout = F4(function (a,
   b,
   c,
   d) {
      return {_: {}
             ,shoutFrom: b
             ,shoutKind: a
             ,shoutTile: c
             ,shoutTo: d};
   });
   var Ron = {ctor: "Ron"};
   var Chi = {ctor: "Chi"};
   var Kan = {ctor: "Kan"};
   var Pon = {ctor: "Pon"};
   var Mentsu = F3(function (a,
   b,
   c) {
      return {_: {}
             ,from: c
             ,mentsuKind: a
             ,tile: b};
   });
   var Jantou = {ctor: "Jantou"};
   var Kantsu = {ctor: "Kantsu"};
   var Koutsu = {ctor: "Koutsu"};
   var Shuntsu = {ctor: "Shuntsu"};
   var Discard = F3(function (a,
   b,
   c) {
      return {_: {}
             ,riichi: c
             ,tile: a
             ,to: b};
   });
   var HandPublic$ = F4(function (a,
   b,
   c,
   d) {
      return _U.insert("riichi",
      c,
      _U.insert("discards",
      b,
      _U.insert("called",a,d)));
   });
   var sangenNth = function (k) {
      return function () {
         switch (k.ctor)
         {case "Chun": return 2;
            case "Haku": return 0;
            case "Hatsu": return 1;}
         _U.badCase($moduleName,
         "between lines 181 and 184");
      }();
   };
   var kazeNth = function (k) {
      return function () {
         switch (k.ctor)
         {case "Nan": return 1;
            case "Pei": return 3;
            case "Shaa": return 2;
            case "Ton": return 0;}
         _U.badCase($moduleName,
         "between lines 175 and 179");
      }();
   };
   var suitNth = function (s) {
      return function () {
         switch (s.ctor)
         {case "ManTile": return 1;
            case "PinTile": return 2;
            case "SouTile": return 3;}
         _U.badCase($moduleName,
         "between lines 169 and 172");
      }();
   };
   var suitOrder = F2(function (a,
   b) {
      return A2($Basics.compare,
      suitNth(a),
      suitNth(b));
   });
   var sangenOrder = F2(function (a,
   b) {
      return A2($Basics.compare,
      sangenNth(a),
      sangenNth(b));
   });
   var kazeOrder = F2(function (a,
   b) {
      return A2($Basics.compare,
      kazeNth(a),
      kazeNth(b));
   });
   var tileOrder = F2(function (a,
   b) {
      return function () {
         var _v3 = {ctor: "_Tuple2"
                   ,_0: a
                   ,_1: b};
         switch (_v3.ctor)
         {case "_Tuple2":
            switch (_v3._0.ctor)
              {case "Honor":
                 switch (_v3._1.ctor)
                   {case "Suited":
                      return $Basics.GT;}
                   switch (_v3._0._0.ctor)
                   {case "Kazehai":
                      switch (_v3._1.ctor)
                        {case "Honor":
                           switch (_v3._1._0.ctor)
                             {case "Kazehai":
                                return A2(kazeOrder,
                                  _v3._0._0._0,
                                  _v3._1._0._0);
                                case "Sangenpai":
                                return $Basics.LT;}
                             break;}
                        break;
                      case "Sangenpai":
                      switch (_v3._1.ctor)
                        {case "Honor":
                           switch (_v3._1._0.ctor)
                             {case "Kazehai":
                                return $Basics.GT;
                                case "Sangenpai":
                                return A2(sangenOrder,
                                  _v3._0._0._0,
                                  _v3._1._0._0);}
                             break;}
                        break;}
                   break;
                 case "Suited":
                 switch (_v3._1.ctor)
                   {case "Honor":
                      return $Basics.LT;
                      case "Suited":
                      return function () {
                           var _v25 = A2(suitOrder,
                           _v3._0._0,
                           _v3._1._0);
                           switch (_v25.ctor)
                           {case "EQ":
                              return A2($Basics.compare,
                                _v3._0._1,
                                _v3._1._1);}
                           return _v25;
                        }();}
                   break;}
              break;}
         _U.badCase($moduleName,
         "between lines 154 and 163");
      }();
   });
   var sortTiles = $List.sortWith(tileOrder);
   var Chun = {ctor: "Chun"};
   var Hatsu = {ctor: "Hatsu"};
   var Haku = {ctor: "Haku"};
   var Pei = {ctor: "Pei"};
   var Shaa = {ctor: "Shaa"};
   var $Nan = {ctor: "Nan"};
   var Ton = {ctor: "Ton"};
   var readKaze = function (x) {
      return function () {
         switch (x)
         {case "Nan": return $Nan;
            case "Pei": return Pei;
            case "Shaa": return Shaa;
            case "Ton": return Ton;}
         _U.badCase($moduleName,
         "between lines 147 and 151");
      }();
   };
   var Sangenpai = function (a) {
      return {ctor: "Sangenpai"
             ,_0: a};
   };
   var Kazehai = function (a) {
      return {ctor: "Kazehai"
             ,_0: a};
   };
   var SouTile = {ctor: "SouTile"};
   var PinTile = {ctor: "PinTile"};
   var ManTile = {ctor: "ManTile"};
   var Honor = function (a) {
      return {ctor: "Honor",_0: a};
   };
   var Suited = F3(function (a,
   b,
   c) {
      return {ctor: "Suited"
             ,_0: a
             ,_1: b
             ,_2: c};
   });
   var defaultLounge = {_: {}
                       ,games: _L.fromArray([])
                       ,idle: $Set.empty};
   var GameInfo = F3(function (a,
   b,
   c) {
      return {_: {}
             ,ident: a
             ,players: c
             ,topic: b};
   });
   var LoungeData = F2(function (a,
   b) {
      return {_: {}
             ,games: b
             ,idle: a};
   });
   var GameDontCare = {ctor: "GameDontCare"};
   var GameShout = function (a) {
      return {ctor: "GameShout"
             ,_0: a};
   };
   var GameTurn = function (a) {
      return {ctor: "GameTurn"
             ,_0: a};
   };
   var TurnTsumo = {ctor: "TurnTsumo"};
   var TurnShouminkan = function (a) {
      return {ctor: "TurnShouminkan"
             ,_0: a};
   };
   var TurnAnkan = function (a) {
      return {ctor: "TurnAnkan"
             ,_0: a};
   };
   var TurnTileDraw = F2(function (a,
   b) {
      return {ctor: "TurnTileDraw"
             ,_0: a
             ,_1: b};
   });
   var TurnTileDiscard = function (a) {
      return {ctor: "TurnTileDiscard"
             ,_0: a};
   };
   var RoundGamePoints = function (a) {
      return {ctor: "RoundGamePoints"
             ,_0: a};
   };
   var RoundRiichi = function (a) {
      return {ctor: "RoundRiichi"
             ,_0: a};
   };
   var RoundFlippedDora = function (a) {
      return {ctor: "RoundFlippedDora"
             ,_0: a};
   };
   var RoundNick = function (a) {
      return {ctor: "RoundNick"
             ,_0: a};
   };
   var RoundEnded = function (a) {
      return {ctor: "RoundEnded"
             ,_0: a};
   };
   var RoundHandChanged = function (a) {
      return {ctor: "RoundHandChanged"
             ,_0: a};
   };
   var RoundTurnShouted = function (a) {
      return {ctor: "RoundTurnShouted"
             ,_0: a};
   };
   var RoundTurnAction = function (a) {
      return {ctor: "RoundTurnAction"
             ,_0: a};
   };
   var RoundTurnBegins = function (a) {
      return {ctor: "RoundTurnBegins"
             ,_0: a};
   };
   var RoundPrivateChange = function (a) {
      return {ctor: "RoundPrivateChange"
             ,_0: a};
   };
   var RoundPrivateWaitForTurnAction = function (a) {
      return {ctor: "RoundPrivateWaitForTurnAction"
             ,_0: a};
   };
   var RoundPrivateWaitForShout = function (a) {
      return {ctor: "RoundPrivateWaitForShout"
             ,_0: a};
   };
   var RoundPrivateStarts = function (a) {
      return {ctor: "RoundPrivateStarts"
             ,_0: a};
   };
   var Noop = {ctor: "Noop"};
   var InGameAction = function (a) {
      return {ctor: "InGameAction"
             ,_0: a};
   };
   var ForceStart = function (a) {
      return {ctor: "ForceStart"
             ,_0: a};
   };
   var LoungeInfo = function (a) {
      return {ctor: "LoungeInfo"
             ,_0: a};
   };
   var InGameEvents = function (a) {
      return {ctor: "InGameEvents"
             ,_0: a};
   };
   var GameCreated = function (a) {
      return {ctor: "GameCreated"
             ,_0: a};
   };
   var Invalid = function (a) {
      return {ctor: "Invalid"
             ,_0: a};
   };
   var JoinGame = function (a) {
      return {ctor: "JoinGame"
             ,_0: a};
   };
   var Message = function (a) {
      return {ctor: "Message"
             ,_0: a};
   };
   var Identity = function (a) {
      return {ctor: "Identity"
             ,_0: a};
   };
   var PartServer = function (a) {
      return {ctor: "PartServer"
             ,_0: a};
   };
   var JoinServer = function (a) {
      return {ctor: "JoinServer"
             ,_0: a};
   };
   var HandValue = F5(function (a,
   b,
   c,
   d,
   e) {
      return {_: {}
             ,fu: b
             ,han: c
             ,named: e
             ,points: d
             ,yaku: a};
   });
   var Valued = F3(function (a,
   b,
   c) {
      return {_: {}
             ,mentsu: a
             ,tiles: b
             ,value: c};
   });
   var Yaku = F2(function (a,b) {
      return {_: {}
             ,han: a
             ,name: b};
   });
   var Payer = F2(function (a,b) {
      return {_: {}
             ,player: a
             ,points: b};
   });
   var Winner = F3(function (a,
   b,
   c) {
      return {_: {}
             ,player: a
             ,points: b
             ,valuehand: c};
   });
   var DealDraw = function (a) {
      return {ctor: "DealDraw"
             ,_0: a};
   };
   var DealRon = function (a) {
      return {ctor: "DealRon"
             ,_0: a};
   };
   var DealTsumo = function (a) {
      return {ctor: "DealTsumo"
             ,_0: a};
   };
   var RoundState = function (a) {
      return function (b) {
         return function (c) {
            return function (d) {
               return function (e) {
                  return function (f) {
                     return function (g) {
                        return function (h) {
                           return function (i) {
                              return function (j) {
                                 return function (k) {
                                    return function (l) {
                                       return function (m) {
                                          return function (n) {
                                             return function (o) {
                                                return function (p) {
                                                   return {_: {}
                                                          ,deal: m
                                                          ,dora: h
                                                          ,firstoja: f
                                                          ,hands: i
                                                          ,honba: n
                                                          ,inTable: o
                                                          ,myhand: k
                                                          ,mypos: a
                                                          ,oja: e
                                                          ,player: d
                                                          ,players: j
                                                          ,prevDeals: p
                                                          ,results: l
                                                          ,round: b
                                                          ,tilesleft: g
                                                          ,turn: c};
                                                };
                                             };
                                          };
                                       };
                                    };
                                 };
                              };
                           };
                        };
                     };
                  };
               };
            };
         };
      };
   };
   var WaitRecord = F2(function (a,
   b) {
      return {_: {}
             ,added: b
             ,seconds: a};
   });
   var InGame = {ctor: "InGame"};
   var InLounge = {ctor: "InLounge"};
   var GameState = function (a) {
      return function (b) {
         return function (c) {
            return function (d) {
               return function (e) {
                  return function (f) {
                     return function (g) {
                        return function (h) {
                           return function (i) {
                              return function (j) {
                                 return function (k) {
                                    return function (l) {
                                       return function (m) {
                                          return {_: {}
                                                 ,debuglog: m
                                                 ,eventlog: l
                                                 ,gameWait: e
                                                 ,lounge: d
                                                 ,myid: c
                                                 ,mynick: b
                                                 ,riichiWith: k
                                                 ,roundState: g
                                                 ,status: a
                                                 ,turnBegan: j
                                                 ,updated: f
                                                 ,waitShout: i
                                                 ,waitTurnAction: h};
                                       };
                                    };
                                 };
                              };
                           };
                        };
                     };
                  };
               };
            };
         };
      };
   };
   _elm.GameTypes.values = {_op: _op
                           ,GameState: GameState
                           ,InLounge: InLounge
                           ,InGame: InGame
                           ,WaitRecord: WaitRecord
                           ,RoundState: RoundState
                           ,DealTsumo: DealTsumo
                           ,DealRon: DealRon
                           ,DealDraw: DealDraw
                           ,Winner: Winner
                           ,Payer: Payer
                           ,Yaku: Yaku
                           ,Valued: Valued
                           ,HandValue: HandValue
                           ,JoinServer: JoinServer
                           ,PartServer: PartServer
                           ,Identity: Identity
                           ,Message: Message
                           ,JoinGame: JoinGame
                           ,Invalid: Invalid
                           ,GameCreated: GameCreated
                           ,InGameEvents: InGameEvents
                           ,LoungeInfo: LoungeInfo
                           ,ForceStart: ForceStart
                           ,InGameAction: InGameAction
                           ,Noop: Noop
                           ,RoundPrivateStarts: RoundPrivateStarts
                           ,RoundPrivateWaitForShout: RoundPrivateWaitForShout
                           ,RoundPrivateWaitForTurnAction: RoundPrivateWaitForTurnAction
                           ,RoundPrivateChange: RoundPrivateChange
                           ,RoundTurnBegins: RoundTurnBegins
                           ,RoundTurnAction: RoundTurnAction
                           ,RoundTurnShouted: RoundTurnShouted
                           ,RoundHandChanged: RoundHandChanged
                           ,RoundEnded: RoundEnded
                           ,RoundNick: RoundNick
                           ,RoundFlippedDora: RoundFlippedDora
                           ,RoundRiichi: RoundRiichi
                           ,RoundGamePoints: RoundGamePoints
                           ,TurnTileDiscard: TurnTileDiscard
                           ,TurnTileDraw: TurnTileDraw
                           ,TurnAnkan: TurnAnkan
                           ,TurnShouminkan: TurnShouminkan
                           ,TurnTsumo: TurnTsumo
                           ,GameTurn: GameTurn
                           ,GameShout: GameShout
                           ,GameDontCare: GameDontCare
                           ,LoungeData: LoungeData
                           ,GameInfo: GameInfo
                           ,defaultLounge: defaultLounge
                           ,Suited: Suited
                           ,Honor: Honor
                           ,ManTile: ManTile
                           ,PinTile: PinTile
                           ,SouTile: SouTile
                           ,Kazehai: Kazehai
                           ,Sangenpai: Sangenpai
                           ,Ton: Ton
                           ,$Nan: $Nan
                           ,Shaa: Shaa
                           ,Pei: Pei
                           ,Haku: Haku
                           ,Hatsu: Hatsu
                           ,Chun: Chun
                           ,readKaze: readKaze
                           ,tileOrder: tileOrder
                           ,kazeOrder: kazeOrder
                           ,sangenOrder: sangenOrder
                           ,suitOrder: suitOrder
                           ,suitNth: suitNth
                           ,kazeNth: kazeNth
                           ,sangenNth: sangenNth
                           ,sortTiles: sortTiles
                           ,HandPublic$: HandPublic$
                           ,Discard: Discard
                           ,Shuntsu: Shuntsu
                           ,Koutsu: Koutsu
                           ,Kantsu: Kantsu
                           ,Jantou: Jantou
                           ,Mentsu: Mentsu
                           ,Pon: Pon
                           ,Kan: Kan
                           ,Chi: Chi
                           ,Ron: Ron
                           ,Shout: Shout};
   return _elm.GameTypes.values;
};
Elm.Graphics = Elm.Graphics || {};
Elm.Graphics.Collage = Elm.Graphics.Collage || {};
Elm.Graphics.Collage.make = function (_elm) {
   "use strict";
   _elm.Graphics = _elm.Graphics || {};
   _elm.Graphics.Collage = _elm.Graphics.Collage || {};
   if (_elm.Graphics.Collage.values)
   return _elm.Graphics.Collage.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Graphics.Collage",
   $Basics = Elm.Basics.make(_elm),
   $Color = Elm.Color.make(_elm),
   $Graphics$Element = Elm.Graphics.Element.make(_elm),
   $List = Elm.List.make(_elm),
   $Native$Graphics$Collage = Elm.Native.Graphics.Collage.make(_elm),
   $Text = Elm.Text.make(_elm),
   $Transform2D = Elm.Transform2D.make(_elm);
   var ngon = F2(function (n,r) {
      return function () {
         var m = $Basics.toFloat(n);
         var t = 2 * $Basics.pi / m;
         var f = function (i) {
            return {ctor: "_Tuple2"
                   ,_0: r * $Basics.cos(t * i)
                   ,_1: r * $Basics.sin(t * i)};
         };
         return A2($List.map,
         f,
         _L.range(0,m - 1));
      }();
   });
   var oval = F2(function (w,h) {
      return function () {
         var hh = h / 2;
         var hw = w / 2;
         var n = 50;
         var t = 2 * $Basics.pi / n;
         var f = function (i) {
            return {ctor: "_Tuple2"
                   ,_0: hw * $Basics.cos(t * i)
                   ,_1: hh * $Basics.sin(t * i)};
         };
         return A2($List.map,
         f,
         _L.range(0,n - 1));
      }();
   });
   var circle = function (r) {
      return A2(oval,2 * r,2 * r);
   };
   var rect = F2(function (w,h) {
      return function () {
         var hh = h / 2;
         var hw = w / 2;
         return _L.fromArray([{ctor: "_Tuple2"
                              ,_0: 0 - hw
                              ,_1: 0 - hh}
                             ,{ctor: "_Tuple2"
                              ,_0: 0 - hw
                              ,_1: hh}
                             ,{ctor: "_Tuple2",_0: hw,_1: hh}
                             ,{ctor: "_Tuple2"
                              ,_0: hw
                              ,_1: 0 - hh}]);
      }();
   });
   var square = function (n) {
      return A2(rect,n,n);
   };
   var polygon = function (points) {
      return points;
   };
   var segment = F2(function (p1,
   p2) {
      return _L.fromArray([p1,p2]);
   });
   var path = function (ps) {
      return ps;
   };
   var collage = $Native$Graphics$Collage.collage;
   var alpha = F2(function (a,f) {
      return _U.replace([["alpha"
                         ,a]],
      f);
   });
   var rotate = F2(function (t,f) {
      return _U.replace([["theta"
                         ,f.theta + t]],
      f);
   });
   var scale = F2(function (s,f) {
      return _U.replace([["scale"
                         ,f.scale * s]],
      f);
   });
   var moveY = F2(function (y,f) {
      return _U.replace([["y"
                         ,f.y + y]],
      f);
   });
   var moveX = F2(function (x,f) {
      return _U.replace([["x"
                         ,f.x + x]],
      f);
   });
   var move = F2(function (_v0,f) {
      return function () {
         switch (_v0.ctor)
         {case "_Tuple2":
            return _U.replace([["x"
                               ,f.x + _v0._0]
                              ,["y",f.y + _v0._1]],
              f);}
         _U.badCase($moduleName,
         "on line 226, column 7 to 35");
      }();
   });
   var form = function (f) {
      return {_: {}
             ,alpha: 1
             ,form: f
             ,scale: 1
             ,theta: 0
             ,x: 0
             ,y: 0};
   };
   var Fill = function (a) {
      return {ctor: "Fill",_0: a};
   };
   var Line = function (a) {
      return {ctor: "Line",_0: a};
   };
   var FGroup = F2(function (a,b) {
      return {ctor: "FGroup"
             ,_0: a
             ,_1: b};
   });
   var group = function (fs) {
      return form(A2(FGroup,
      $Transform2D.identity,
      fs));
   };
   var groupTransform = F2(function (matrix,
   fs) {
      return form(A2(FGroup,
      matrix,
      fs));
   });
   var FElement = function (a) {
      return {ctor: "FElement"
             ,_0: a};
   };
   var toForm = function (e) {
      return form(FElement(e));
   };
   var FImage = F4(function (a,
   b,
   c,
   d) {
      return {ctor: "FImage"
             ,_0: a
             ,_1: b
             ,_2: c
             ,_3: d};
   });
   var sprite = F4(function (w,
   h,
   pos,
   src) {
      return form(A4(FImage,
      w,
      h,
      pos,
      src));
   });
   var FText = function (a) {
      return {ctor: "FText",_0: a};
   };
   var text = function (t) {
      return form(FText(t));
   };
   var FOutlinedText = F2(function (a,
   b) {
      return {ctor: "FOutlinedText"
             ,_0: a
             ,_1: b};
   });
   var outlinedText = F2(function (ls,
   t) {
      return form(A2(FOutlinedText,
      ls,
      t));
   });
   var FShape = F2(function (a,b) {
      return {ctor: "FShape"
             ,_0: a
             ,_1: b};
   });
   var fill = F2(function (style,
   shape) {
      return form(A2(FShape,
      Fill(style),
      shape));
   });
   var outlined = F2(function (style,
   shape) {
      return form(A2(FShape,
      Line(style),
      shape));
   });
   var FPath = F2(function (a,b) {
      return {ctor: "FPath"
             ,_0: a
             ,_1: b};
   });
   var traced = F2(function (style,
   path) {
      return form(A2(FPath,
      style,
      path));
   });
   var LineStyle = F6(function (a,
   b,
   c,
   d,
   e,
   f) {
      return {_: {}
             ,cap: c
             ,color: a
             ,dashOffset: f
             ,dashing: e
             ,join: d
             ,width: b};
   });
   var Clipped = {ctor: "Clipped"};
   var Sharp = function (a) {
      return {ctor: "Sharp",_0: a};
   };
   var Smooth = {ctor: "Smooth"};
   var Padded = {ctor: "Padded"};
   var Round = {ctor: "Round"};
   var Flat = {ctor: "Flat"};
   var defaultLine = {_: {}
                     ,cap: Flat
                     ,color: $Color.black
                     ,dashOffset: 0
                     ,dashing: _L.fromArray([])
                     ,join: Sharp(10)
                     ,width: 1};
   var solid = function (clr) {
      return _U.replace([["color"
                         ,clr]],
      defaultLine);
   };
   var dashed = function (clr) {
      return _U.replace([["color"
                         ,clr]
                        ,["dashing"
                         ,_L.fromArray([8,4])]],
      defaultLine);
   };
   var dotted = function (clr) {
      return _U.replace([["color"
                         ,clr]
                        ,["dashing"
                         ,_L.fromArray([3,3])]],
      defaultLine);
   };
   var Grad = function (a) {
      return {ctor: "Grad",_0: a};
   };
   var gradient = F2(function (grad,
   shape) {
      return A2(fill,
      Grad(grad),
      shape);
   });
   var Texture = function (a) {
      return {ctor: "Texture"
             ,_0: a};
   };
   var textured = F2(function (src,
   shape) {
      return A2(fill,
      Texture(src),
      shape);
   });
   var Solid = function (a) {
      return {ctor: "Solid",_0: a};
   };
   var filled = F2(function (color,
   shape) {
      return A2(fill,
      Solid(color),
      shape);
   });
   var Form = F6(function (a,
   b,
   c,
   d,
   e,
   f) {
      return {_: {}
             ,alpha: e
             ,form: f
             ,scale: b
             ,theta: a
             ,x: c
             ,y: d};
   });
   _elm.Graphics.Collage.values = {_op: _op
                                  ,collage: collage
                                  ,toForm: toForm
                                  ,filled: filled
                                  ,textured: textured
                                  ,gradient: gradient
                                  ,outlined: outlined
                                  ,traced: traced
                                  ,text: text
                                  ,outlinedText: outlinedText
                                  ,move: move
                                  ,moveX: moveX
                                  ,moveY: moveY
                                  ,scale: scale
                                  ,rotate: rotate
                                  ,alpha: alpha
                                  ,group: group
                                  ,groupTransform: groupTransform
                                  ,rect: rect
                                  ,oval: oval
                                  ,square: square
                                  ,circle: circle
                                  ,ngon: ngon
                                  ,polygon: polygon
                                  ,segment: segment
                                  ,path: path
                                  ,solid: solid
                                  ,dashed: dashed
                                  ,dotted: dotted
                                  ,defaultLine: defaultLine
                                  ,Form: Form
                                  ,LineStyle: LineStyle
                                  ,Flat: Flat
                                  ,Round: Round
                                  ,Padded: Padded
                                  ,Smooth: Smooth
                                  ,Sharp: Sharp
                                  ,Clipped: Clipped};
   return _elm.Graphics.Collage.values;
};
Elm.Graphics = Elm.Graphics || {};
Elm.Graphics.Element = Elm.Graphics.Element || {};
Elm.Graphics.Element.make = function (_elm) {
   "use strict";
   _elm.Graphics = _elm.Graphics || {};
   _elm.Graphics.Element = _elm.Graphics.Element || {};
   if (_elm.Graphics.Element.values)
   return _elm.Graphics.Element.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Graphics.Element",
   $Basics = Elm.Basics.make(_elm),
   $Color = Elm.Color.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Graphics$Element = Elm.Native.Graphics.Element.make(_elm),
   $Text = Elm.Text.make(_elm);
   var DOut = {ctor: "DOut"};
   var outward = DOut;
   var DIn = {ctor: "DIn"};
   var inward = DIn;
   var DRight = {ctor: "DRight"};
   var right = DRight;
   var DLeft = {ctor: "DLeft"};
   var left = DLeft;
   var DDown = {ctor: "DDown"};
   var down = DDown;
   var DUp = {ctor: "DUp"};
   var up = DUp;
   var Position = F4(function (a,
   b,
   c,
   d) {
      return {_: {}
             ,horizontal: a
             ,vertical: b
             ,x: c
             ,y: d};
   });
   var Relative = function (a) {
      return {ctor: "Relative"
             ,_0: a};
   };
   var relative = Relative;
   var Absolute = function (a) {
      return {ctor: "Absolute"
             ,_0: a};
   };
   var absolute = Absolute;
   var N = {ctor: "N"};
   var bottomLeftAt = F2(function (x,
   y) {
      return {_: {}
             ,horizontal: N
             ,vertical: N
             ,x: x
             ,y: y};
   });
   var Z = {ctor: "Z"};
   var middle = {_: {}
                ,horizontal: Z
                ,vertical: Z
                ,x: Relative(0.5)
                ,y: Relative(0.5)};
   var midLeft = _U.replace([["horizontal"
                             ,N]
                            ,["x",Absolute(0)]],
   middle);
   var middleAt = F2(function (x,
   y) {
      return {_: {}
             ,horizontal: Z
             ,vertical: Z
             ,x: x
             ,y: y};
   });
   var midLeftAt = F2(function (x,
   y) {
      return {_: {}
             ,horizontal: N
             ,vertical: Z
             ,x: x
             ,y: y};
   });
   var midBottomAt = F2(function (x,
   y) {
      return {_: {}
             ,horizontal: Z
             ,vertical: N
             ,x: x
             ,y: y};
   });
   var P = {ctor: "P"};
   var topLeft = {_: {}
                 ,horizontal: N
                 ,vertical: P
                 ,x: Absolute(0)
                 ,y: Absolute(0)};
   var bottomLeft = _U.replace([["vertical"
                                ,N]],
   topLeft);
   var topRight = _U.replace([["horizontal"
                              ,P]],
   topLeft);
   var bottomRight = _U.replace([["horizontal"
                                 ,P]],
   bottomLeft);
   var midRight = _U.replace([["horizontal"
                              ,P]],
   midLeft);
   var midTop = _U.replace([["vertical"
                            ,P]
                           ,["y",Absolute(0)]],
   middle);
   var midBottom = _U.replace([["vertical"
                               ,N]],
   midTop);
   var topLeftAt = F2(function (x,
   y) {
      return {_: {}
             ,horizontal: N
             ,vertical: P
             ,x: x
             ,y: y};
   });
   var topRightAt = F2(function (x,
   y) {
      return {_: {}
             ,horizontal: P
             ,vertical: P
             ,x: x
             ,y: y};
   });
   var bottomRightAt = F2(function (x,
   y) {
      return {_: {}
             ,horizontal: P
             ,vertical: N
             ,x: x
             ,y: y};
   });
   var midRightAt = F2(function (x,
   y) {
      return {_: {}
             ,horizontal: P
             ,vertical: Z
             ,x: x
             ,y: y};
   });
   var midTopAt = F2(function (x,
   y) {
      return {_: {}
             ,horizontal: Z
             ,vertical: P
             ,x: x
             ,y: y};
   });
   var justified = $Native$Graphics$Element.block("justify");
   var centered = $Native$Graphics$Element.block("center");
   var rightAligned = $Native$Graphics$Element.block("right");
   var leftAligned = $Native$Graphics$Element.block("left");
   var show = function (value) {
      return leftAligned($Text.monospace($Text.fromString($Basics.toString(value))));
   };
   var Tiled = {ctor: "Tiled"};
   var Cropped = function (a) {
      return {ctor: "Cropped"
             ,_0: a};
   };
   var Fitted = {ctor: "Fitted"};
   var Plain = {ctor: "Plain"};
   var Custom = {ctor: "Custom"};
   var RawHtml = {ctor: "RawHtml"};
   var Spacer = {ctor: "Spacer"};
   var Flow = F2(function (a,b) {
      return {ctor: "Flow"
             ,_0: a
             ,_1: b};
   });
   var Container = F2(function (a,
   b) {
      return {ctor: "Container"
             ,_0: a
             ,_1: b};
   });
   var Image = F4(function (a,
   b,
   c,
   d) {
      return {ctor: "Image"
             ,_0: a
             ,_1: b
             ,_2: c
             ,_3: d};
   });
   var newElement = $Native$Graphics$Element.newElement;
   var image = F3(function (w,
   h,
   src) {
      return A3(newElement,
      w,
      h,
      A4(Image,Plain,w,h,src));
   });
   var fittedImage = F3(function (w,
   h,
   src) {
      return A3(newElement,
      w,
      h,
      A4(Image,Fitted,w,h,src));
   });
   var croppedImage = F4(function (pos,
   w,
   h,
   src) {
      return A3(newElement,
      w,
      h,
      A4(Image,Cropped(pos),w,h,src));
   });
   var tiledImage = F3(function (w,
   h,
   src) {
      return A3(newElement,
      w,
      h,
      A4(Image,Tiled,w,h,src));
   });
   var container = F4(function (w,
   h,
   pos,
   e) {
      return A3(newElement,
      w,
      h,
      A2(Container,pos,e));
   });
   var spacer = F2(function (w,h) {
      return A3(newElement,
      w,
      h,
      Spacer);
   });
   var link = F2(function (href,
   e) {
      return function () {
         var p = e.props;
         return {_: {}
                ,element: e.element
                ,props: _U.replace([["href"
                                    ,href]],
                p)};
      }();
   });
   var tag = F2(function (name,e) {
      return function () {
         var p = e.props;
         return {_: {}
                ,element: e.element
                ,props: _U.replace([["tag"
                                    ,name]],
                p)};
      }();
   });
   var color = F2(function (c,e) {
      return function () {
         var p = e.props;
         return {_: {}
                ,element: e.element
                ,props: _U.replace([["color"
                                    ,$Maybe.Just(c)]],
                p)};
      }();
   });
   var opacity = F2(function (o,
   e) {
      return function () {
         var p = e.props;
         return {_: {}
                ,element: e.element
                ,props: _U.replace([["opacity"
                                    ,o]],
                p)};
      }();
   });
   var height = F2(function (nh,
   e) {
      return function () {
         var p = e.props;
         var props = function () {
            var _v0 = e.element;
            switch (_v0.ctor)
            {case "Image":
               return _U.replace([["width"
                                  ,$Basics.round($Basics.toFloat(_v0._1) / $Basics.toFloat(_v0._2) * $Basics.toFloat(nh))]],
                 p);}
            return p;
         }();
         return {_: {}
                ,element: e.element
                ,props: _U.replace([["height"
                                    ,nh]],
                p)};
      }();
   });
   var width = F2(function (nw,e) {
      return function () {
         var p = e.props;
         var props = function () {
            var _v5 = e.element;
            switch (_v5.ctor)
            {case "Image":
               return _U.replace([["height"
                                  ,$Basics.round($Basics.toFloat(_v5._2) / $Basics.toFloat(_v5._1) * $Basics.toFloat(nw))]],
                 p);
               case "RawHtml":
               return _U.replace([["height"
                                  ,$Basics.snd(A2($Native$Graphics$Element.htmlHeight,
                                  nw,
                                  e.element))]],
                 p);}
            return p;
         }();
         return {_: {}
                ,element: e.element
                ,props: _U.replace([["width"
                                    ,nw]],
                props)};
      }();
   });
   var size = F3(function (w,h,e) {
      return A2(height,
      h,
      A2(width,w,e));
   });
   var sizeOf = function (e) {
      return {ctor: "_Tuple2"
             ,_0: e.props.width
             ,_1: e.props.height};
   };
   var heightOf = function (e) {
      return e.props.height;
   };
   var widthOf = function (e) {
      return e.props.width;
   };
   var above = F2(function (hi,
   lo) {
      return A3(newElement,
      A2($Basics.max,
      widthOf(hi),
      widthOf(lo)),
      heightOf(hi) + heightOf(lo),
      A2(Flow,
      DDown,
      _L.fromArray([hi,lo])));
   });
   var below = F2(function (lo,
   hi) {
      return A3(newElement,
      A2($Basics.max,
      widthOf(hi),
      widthOf(lo)),
      heightOf(hi) + heightOf(lo),
      A2(Flow,
      DDown,
      _L.fromArray([hi,lo])));
   });
   var beside = F2(function (lft,
   rht) {
      return A3(newElement,
      widthOf(lft) + widthOf(rht),
      A2($Basics.max,
      heightOf(lft),
      heightOf(rht)),
      A2(Flow,
      right,
      _L.fromArray([lft,rht])));
   });
   var layers = function (es) {
      return function () {
         var hs = A2($List.map,
         heightOf,
         es);
         var ws = A2($List.map,
         widthOf,
         es);
         return A3(newElement,
         A2($Maybe.withDefault,
         0,
         $List.maximum(ws)),
         A2($Maybe.withDefault,
         0,
         $List.maximum(hs)),
         A2(Flow,DOut,es));
      }();
   };
   var empty = A2(spacer,0,0);
   var flow = F2(function (dir,
   es) {
      return function () {
         var newFlow = F2(function (w,
         h) {
            return A3(newElement,
            w,
            h,
            A2(Flow,dir,es));
         });
         var maxOrZero = function (list) {
            return A2($Maybe.withDefault,
            0,
            $List.maximum(list));
         };
         var hs = A2($List.map,
         heightOf,
         es);
         var ws = A2($List.map,
         widthOf,
         es);
         return _U.eq(es,
         _L.fromArray([])) ? empty : function () {
            switch (dir.ctor)
            {case "DDown":
               return A2(newFlow,
                 maxOrZero(ws),
                 $List.sum(hs));
               case "DIn": return A2(newFlow,
                 maxOrZero(ws),
                 maxOrZero(hs));
               case "DLeft": return A2(newFlow,
                 $List.sum(ws),
                 maxOrZero(hs));
               case "DOut": return A2(newFlow,
                 maxOrZero(ws),
                 maxOrZero(hs));
               case "DRight":
               return A2(newFlow,
                 $List.sum(ws),
                 maxOrZero(hs));
               case "DUp": return A2(newFlow,
                 maxOrZero(ws),
                 $List.sum(hs));}
            _U.badCase($moduleName,
            "between lines 362 and 373");
         }();
      }();
   });
   var Properties = F9(function (a,
   b,
   c,
   d,
   e,
   f,
   g,
   h,
   i) {
      return {_: {}
             ,click: i
             ,color: e
             ,height: c
             ,hover: h
             ,href: f
             ,id: a
             ,opacity: d
             ,tag: g
             ,width: b};
   });
   var Element = F2(function (a,
   b) {
      return {_: {}
             ,element: b
             ,props: a};
   });
   _elm.Graphics.Element.values = {_op: _op
                                  ,image: image
                                  ,fittedImage: fittedImage
                                  ,croppedImage: croppedImage
                                  ,tiledImage: tiledImage
                                  ,leftAligned: leftAligned
                                  ,rightAligned: rightAligned
                                  ,centered: centered
                                  ,justified: justified
                                  ,show: show
                                  ,width: width
                                  ,height: height
                                  ,size: size
                                  ,color: color
                                  ,opacity: opacity
                                  ,link: link
                                  ,tag: tag
                                  ,widthOf: widthOf
                                  ,heightOf: heightOf
                                  ,sizeOf: sizeOf
                                  ,flow: flow
                                  ,up: up
                                  ,down: down
                                  ,left: left
                                  ,right: right
                                  ,inward: inward
                                  ,outward: outward
                                  ,layers: layers
                                  ,above: above
                                  ,below: below
                                  ,beside: beside
                                  ,empty: empty
                                  ,spacer: spacer
                                  ,container: container
                                  ,middle: middle
                                  ,midTop: midTop
                                  ,midBottom: midBottom
                                  ,midLeft: midLeft
                                  ,midRight: midRight
                                  ,topLeft: topLeft
                                  ,topRight: topRight
                                  ,bottomLeft: bottomLeft
                                  ,bottomRight: bottomRight
                                  ,absolute: absolute
                                  ,relative: relative
                                  ,middleAt: middleAt
                                  ,midTopAt: midTopAt
                                  ,midBottomAt: midBottomAt
                                  ,midLeftAt: midLeftAt
                                  ,midRightAt: midRightAt
                                  ,topLeftAt: topLeftAt
                                  ,topRightAt: topRightAt
                                  ,bottomLeftAt: bottomLeftAt
                                  ,bottomRightAt: bottomRightAt
                                  ,Element: Element
                                  ,Position: Position};
   return _elm.Graphics.Element.values;
};
Elm.Graphics = Elm.Graphics || {};
Elm.Graphics.Input = Elm.Graphics.Input || {};
Elm.Graphics.Input.make = function (_elm) {
   "use strict";
   _elm.Graphics = _elm.Graphics || {};
   _elm.Graphics.Input = _elm.Graphics.Input || {};
   if (_elm.Graphics.Input.values)
   return _elm.Graphics.Input.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Graphics.Input",
   $Graphics$Element = Elm.Graphics.Element.make(_elm),
   $Native$Graphics$Input = Elm.Native.Graphics.Input.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var clickable = $Native$Graphics$Input.clickable;
   var hoverable = $Native$Graphics$Input.hoverable;
   var dropDown = $Native$Graphics$Input.dropDown;
   var checkbox = $Native$Graphics$Input.checkbox;
   var customButton = $Native$Graphics$Input.customButton;
   var button = $Native$Graphics$Input.button;
   _elm.Graphics.Input.values = {_op: _op
                                ,button: button
                                ,customButton: customButton
                                ,checkbox: checkbox
                                ,dropDown: dropDown
                                ,hoverable: hoverable
                                ,clickable: clickable};
   return _elm.Graphics.Input.values;
};
Elm.Graphics = Elm.Graphics || {};
Elm.Graphics.Input = Elm.Graphics.Input || {};
Elm.Graphics.Input.Field = Elm.Graphics.Input.Field || {};
Elm.Graphics.Input.Field.make = function (_elm) {
   "use strict";
   _elm.Graphics = _elm.Graphics || {};
   _elm.Graphics.Input = _elm.Graphics.Input || {};
   _elm.Graphics.Input.Field = _elm.Graphics.Input.Field || {};
   if (_elm.Graphics.Input.Field.values)
   return _elm.Graphics.Input.Field.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Graphics.Input.Field",
   $Color = Elm.Color.make(_elm),
   $Graphics$Element = Elm.Graphics.Element.make(_elm),
   $Native$Graphics$Input = Elm.Native.Graphics.Input.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $Text = Elm.Text.make(_elm);
   var email = $Native$Graphics$Input.email;
   var password = $Native$Graphics$Input.password;
   var field = $Native$Graphics$Input.field;
   var Backward = {ctor: "Backward"};
   var Forward = {ctor: "Forward"};
   var Selection = F3(function (a,
   b,
   c) {
      return {_: {}
             ,direction: c
             ,end: b
             ,start: a};
   });
   var Content = F2(function (a,
   b) {
      return {_: {}
             ,selection: b
             ,string: a};
   });
   var noContent = A2(Content,
   "",
   A3(Selection,0,0,Forward));
   var Style = F4(function (a,
   b,
   c,
   d) {
      return {_: {}
             ,highlight: c
             ,outline: b
             ,padding: a
             ,style: d};
   });
   var Highlight = F2(function (a,
   b) {
      return {_: {}
             ,color: a
             ,width: b};
   });
   var noHighlight = A2(Highlight,
   $Color.blue,
   0);
   var Outline = F3(function (a,
   b,
   c) {
      return {_: {}
             ,color: a
             ,radius: c
             ,width: b};
   });
   var Dimensions = F4(function (a,
   b,
   c,
   d) {
      return {_: {}
             ,bottom: d
             ,left: a
             ,right: b
             ,top: c};
   });
   var uniformly = function (n) {
      return A4(Dimensions,
      n,
      n,
      n,
      n);
   };
   var noOutline = A3(Outline,
   $Color.grey,
   uniformly(0),
   0);
   var defaultStyle = {_: {}
                      ,highlight: A2(Highlight,
                      $Color.blue,
                      1)
                      ,outline: A3(Outline,
                      $Color.grey,
                      uniformly(1),
                      2)
                      ,padding: uniformly(4)
                      ,style: $Text.defaultStyle};
   _elm.Graphics.Input.Field.values = {_op: _op
                                      ,uniformly: uniformly
                                      ,Dimensions: Dimensions
                                      ,Outline: Outline
                                      ,noOutline: noOutline
                                      ,Highlight: Highlight
                                      ,noHighlight: noHighlight
                                      ,Style: Style
                                      ,defaultStyle: defaultStyle
                                      ,Content: Content
                                      ,Selection: Selection
                                      ,Forward: Forward
                                      ,Backward: Backward
                                      ,noContent: noContent
                                      ,field: field
                                      ,password: password
                                      ,email: email};
   return _elm.Graphics.Input.Field.values;
};
Elm.JSON = Elm.JSON || {};
Elm.JSON.make = function (_elm) {
   "use strict";
   _elm.JSON = _elm.JSON || {};
   if (_elm.JSON.values)
   return _elm.JSON.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "JSON",
   $Basics = Elm.Basics.make(_elm),
   $GameTypes = Elm.GameTypes.make(_elm),
   $Json$Decode = Elm.Json.Decode.make(_elm),
   $Json$Encode = Elm.Json.Encode.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Set = Elm.Set.make(_elm);
   var atAction = F2(function (t,
   xs) {
      return A2($List._op["::"],
      {ctor: "_Tuple2"
      ,_0: "action"
      ,_1: $Json$Encode.string(t)},
      xs);
   });
   var atType = F2(function (t,
   xs) {
      return $Json$Encode.object(A2($List._op["::"],
      {ctor: "_Tuple2"
      ,_0: "type"
      ,_1: $Json$Encode.string(t)},
      xs));
   });
   var toJSON_Tile = function (tile) {
      return function () {
         switch (tile.ctor)
         {case "Honor":
            return atType("HonorTile")(function () {
                 switch (tile._0.ctor)
                 {case "Kazehai":
                    return _L.fromArray([{ctor: "_Tuple2"
                                         ,_0: "ident"
                                         ,_1: $Json$Encode.string($Basics.toString(tile._0._0))}]);
                    case "Sangenpai":
                    return _L.fromArray([{ctor: "_Tuple2"
                                         ,_0: "ident"
                                         ,_1: $Json$Encode.string($Basics.toString(tile._0._0))}]);}
                 _U.badCase($moduleName,
                 "between lines 278 and 280");
              }());
            case "Suited": return A2(atType,
              $Basics.toString(tile._0),
              _L.fromArray([{ctor: "_Tuple2"
                            ,_0: "number"
                            ,_1: $Json$Encode.$int(tile._1)}
                           ,{ctor: "_Tuple2"
                            ,_0: "aka"
                            ,_1: $Json$Encode.bool(tile._2)}]));}
         _U.badCase($moduleName,
         "between lines 276 and 280");
      }();
   };
   var toJSON_ShoutKind = function (sk) {
      return $Json$Encode.string(function () {
         switch (sk.ctor)
         {case "Chi": return "chi";
            case "Kan": return "kan";
            case "Pon": return "pon";
            case "Ron": return "ron";}
         _U.badCase($moduleName,
         "between lines 269 and 273");
      }());
   };
   var toJSON_GameAction = function (a) {
      return function () {
         switch (a.ctor)
         {case "GameDontCare":
            return A2(atAction,
              "pass",
              _L.fromArray([]));
            case "GameShout":
            return A2(atAction,
              "shout",
              _L.fromArray([{ctor: "_Tuple2"
                            ,_0: "kind"
                            ,_1: toJSON_ShoutKind(a._0.shoutKind)}
                           ,{ctor: "_Tuple2"
                            ,_0: "from"
                            ,_1: $Json$Encode.string($Basics.toString(a._0.shoutFrom))}
                           ,{ctor: "_Tuple2"
                            ,_0: "tile"
                            ,_1: toJSON_Tile(a._0.shoutTile)}
                           ,{ctor: "_Tuple2"
                            ,_0: "to"
                            ,_1: $Json$Encode.list(A2($List.map,
                            toJSON_Tile,
                            a._0.shoutTo))}]));
            case "GameTurn":
            switch (a._0.ctor)
              {case "TurnAnkan":
                 return A2(atAction,
                   "ankan",
                   _L.fromArray([{ctor: "_Tuple2"
                                 ,_0: "tile"
                                 ,_1: toJSON_Tile(a._0._0)}]));
                 case "TurnShouminkan":
                 return A2(atAction,
                   "shouminkan",
                   _L.fromArray([{ctor: "_Tuple2"
                                 ,_0: "tile"
                                 ,_1: toJSON_Tile(a._0._0)}]));
                 case "TurnTileDiscard":
                 return A2(atAction,
                   "discard",
                   _L.fromArray([{ctor: "_Tuple2"
                                 ,_0: "tile"
                                 ,_1: toJSON_Tile(a._0._0.tile)}
                                ,{ctor: "_Tuple2"
                                 ,_0: "riichi"
                                 ,_1: $Json$Encode.bool(a._0._0.riichi)}]));
                 case "TurnTileDraw":
                 return A2(atAction,
                   "draw",
                   _L.fromArray([{ctor: "_Tuple2"
                                 ,_0: "dead"
                                 ,_1: $Json$Encode.bool(a._0._0)}]));
                 case "TurnTsumo":
                 return A2(atAction,
                   "tsumo",
                   _L.fromArray([]));}
              break;}
         _U.badCase($moduleName,
         "between lines 256 and 267");
      }();
   };
   var toJSON_Event = function (ev) {
      return function () {
         switch (ev.ctor)
         {case "ForceStart":
            return A2(atType,
              "game-fstart",
              _L.fromArray([{ctor: "_Tuple2"
                            ,_0: "ident"
                            ,_1: $Json$Encode.$int(ev._0.ident)}]));
            case "Identity":
            return A2(atType,
              "identity",
              _L.fromArray([{ctor: "_Tuple2"
                            ,_0: "nick"
                            ,_1: $Json$Encode.string(ev._0.nick)}]));
            case "InGameAction":
            return atType("game-action")(toJSON_GameAction(ev._0));
            case "JoinGame":
            return A2(atType,
              "game-join",
              _L.fromArray([{ctor: "_Tuple2"
                            ,_0: "nick"
                            ,_1: $Json$Encode.string(ev._0.nick)}
                           ,{ctor: "_Tuple2"
                            ,_0: "ident"
                            ,_1: $Json$Encode.$int(ev._0.ident)}]));
            case "JoinServer":
            return A2(atType,
              "join",
              _L.fromArray([{ctor: "_Tuple2"
                            ,_0: "nick"
                            ,_1: $Json$Encode.string(ev._0.nick)}
                           ,{ctor: "_Tuple2"
                            ,_0: "ident"
                            ,_1: $Json$Encode.$int(ev._0.ident)}]));
            case "Message":
            return A2(atType,
              "msg",
              _L.fromArray([{ctor: "_Tuple2"
                            ,_0: "from"
                            ,_1: $Json$Encode.string(ev._0.from)}
                           ,{ctor: "_Tuple2"
                            ,_0: "content"
                            ,_1: $Json$Encode.string(ev._0.content)}]));
            case "Noop": return A2(atType,
              "noop",
              _L.fromArray([]));
            case "PartServer":
            return A2(atType,
              "part",
              _L.fromArray([{ctor: "_Tuple2"
                            ,_0: "nick"
                            ,_1: $Json$Encode.string(ev._0.nick)}]));}
         _U.badCase($moduleName,
         "between lines 245 and 253");
      }();
   };
   var encodeEvent = function ($) {
      return $Json$Encode.encode(0)(toJSON_Event($));
   };
   var yaku = A3($Json$Decode.object2,
   $GameTypes.Yaku,
   A2($Json$Decode._op[":="],
   "han",
   $Json$Decode.$int),
   A2($Json$Decode._op[":="],
   "name",
   $Json$Decode.string));
   var handValue = A6($Json$Decode.object5,
   $GameTypes.HandValue,
   A2($Json$Decode._op[":="],
   "yaku",
   $Json$Decode.list(yaku)),
   A2($Json$Decode._op[":="],
   "fu",
   $Json$Decode.$int),
   A2($Json$Decode._op[":="],
   "han",
   $Json$Decode.$int),
   A2($Json$Decode._op[":="],
   "value",
   $Json$Decode.$int),
   A2($Json$Decode._op[":="],
   "named",
   $Json$Decode.maybe($Json$Decode.string)));
   var readShoutKind = function (s) {
      return function () {
         switch (s)
         {case "chi":
            return $GameTypes.Chi;
            case "kan":
            return $GameTypes.Kan;
            case "pon":
            return $GameTypes.Pon;
            case "ron":
            return $GameTypes.Ron;}
         _U.badCase($moduleName,
         "between lines 160 and 164");
      }();
   };
   var shoutKind = A2($Json$Decode.map,
   readShoutKind,
   $Json$Decode.string);
   var readMentsuKind = function (s) {
      return function () {
         switch (s)
         {case "jantou":
            return $GameTypes.Jantou;
            case "kantsu":
            return $GameTypes.Kantsu;
            case "koutsu":
            return $GameTypes.Koutsu;
            case "shuntsu":
            return $GameTypes.Shuntsu;}
         _U.badCase($moduleName,
         "between lines 143 and 148");
      }();
   };
   var mentsuKind = A2($Json$Decode.map,
   readMentsuKind,
   $Json$Decode.string);
   var toHandPublic = F3(function (called,
   discards,
   riichi) {
      return {_: {}
             ,called: called
             ,discards: discards
             ,riichi: riichi};
   });
   var toHand = F7(function (con,
   pick,
   furit,
   canTsumo,
   called,
   discards,
   riichi) {
      return {_: {}
             ,called: called
             ,canTsumo: canTsumo
             ,concealed: con
             ,discards: discards
             ,furiten: furit
             ,pick: pick
             ,riichi: riichi};
   });
   var kaze = $Json$Decode.map(function (x) {
      return function () {
         switch (x)
         {case "Nan":
            return $GameTypes.$Nan;
            case "Pei":
            return $GameTypes.Pei;
            case "Shaa":
            return $GameTypes.Shaa;
            case "Ton":
            return $GameTypes.Ton;}
         _U.badCase($moduleName,
         "between lines 72 and 76");
      }();
   })($Json$Decode.string);
   var points = A3($Json$Decode.tuple2,
   F2(function (v0,v1) {
      return {ctor: "_Tuple2"
             ,_0: v0
             ,_1: v1};
   }),
   kaze,
   $Json$Decode.$int);
   var players = A3($Json$Decode.tuple2,
   F2(function (p,_v28) {
      return function () {
         switch (_v28.ctor)
         {case "_Tuple3":
            return {ctor: "_Tuple2"
                   ,_0: _v28._0
                   ,_1: {ctor: "_Tuple3"
                        ,_0: p
                        ,_1: _v28._1
                        ,_2: _v28._2}};}
         _U.badCase($moduleName,
         "on line 194, column 37 to 50");
      }();
   }),
   $Json$Decode.$int,
   A4($Json$Decode.tuple3,
   F3(function (v0,v1,v2) {
      return {ctor: "_Tuple3"
             ,_0: v0
             ,_1: v1
             ,_2: v2};
   }),
   kaze,
   $Json$Decode.$int,
   $Json$Decode.string));
   var honorFrom = function (str) {
      return function () {
         switch (str)
         {case "Chun":
            return $GameTypes.Sangenpai($GameTypes.Chun);
            case "Haku":
            return $GameTypes.Sangenpai($GameTypes.Haku);
            case "Hatsu":
            return $GameTypes.Sangenpai($GameTypes.Hatsu);
            case "Nan":
            return $GameTypes.Kazehai($GameTypes.$Nan);
            case "Pei":
            return $GameTypes.Kazehai($GameTypes.Pei);
            case "Shaa":
            return $GameTypes.Kazehai($GameTypes.Shaa);
            case "Ton":
            return $GameTypes.Kazehai($GameTypes.Ton);}
         _U.badCase($moduleName,
         "between lines 63 and 70");
      }();
   };
   var honor = A2($Json$Decode.map,
   honorFrom,
   A2($Json$Decode._op[":="],
   "ident",
   $Json$Decode.string));
   var tileOfType = function (tileType) {
      return function () {
         switch (tileType)
         {case "HonorTile":
            return A2($Json$Decode.object1,
              $GameTypes.Honor,
              honor);
            case "ManTile":
            return A3($Json$Decode.object2,
              $GameTypes.Suited($GameTypes.ManTile),
              A2($Json$Decode._op[":="],
              "number",
              $Json$Decode.$int),
              A2($Json$Decode._op[":="],
              "aka",
              $Json$Decode.bool));
            case "PinTile":
            return A3($Json$Decode.object2,
              $GameTypes.Suited($GameTypes.PinTile),
              A2($Json$Decode._op[":="],
              "number",
              $Json$Decode.$int),
              A2($Json$Decode._op[":="],
              "aka",
              $Json$Decode.bool));
            case "SouTile":
            return A3($Json$Decode.object2,
              $GameTypes.Suited($GameTypes.SouTile),
              A2($Json$Decode._op[":="],
              "number",
              $Json$Decode.$int),
              A2($Json$Decode._op[":="],
              "aka",
              $Json$Decode.bool));}
         _U.badCase($moduleName,
         "between lines 54 and 58");
      }();
   };
   var tile = A2($Json$Decode.andThen,
   A2($Json$Decode._op[":="],
   "type",
   $Json$Decode.string),
   tileOfType);
   var tileMaybe = $Json$Decode.oneOf(_L.fromArray([$Json$Decode.$null($Maybe.Nothing)
                                                   ,A2($Json$Decode.map,
                                                   $Maybe.Just,
                                                   tile)]));
   var discard = A4($Json$Decode.object3,
   $GameTypes.Discard,
   A2($Json$Decode._op[":="],
   "tile",
   tile),
   $Json$Decode.oneOf(_L.fromArray([A2($Json$Decode._op[":="],
                                   "to",
                                   A2($Json$Decode.map,
                                   $Maybe.Just,
                                   kaze))
                                   ,$Json$Decode.succeed($Maybe.Nothing)])),
   A2($Json$Decode._op[":="],
   "riichi",
   $Json$Decode.bool));
   var shout = A5($Json$Decode.object4,
   $GameTypes.Shout,
   A2($Json$Decode._op[":="],
   "kind",
   shoutKind),
   A2($Json$Decode._op[":="],
   "from",
   kaze),
   A2($Json$Decode._op[":="],
   "tile",
   tile),
   A2($Json$Decode._op[":="],
   "to",
   $Json$Decode.list(tile)));
   var shoutMaybe = $Json$Decode.oneOf(_L.fromArray([$Json$Decode.$null($Maybe.Nothing)
                                                    ,A2($Json$Decode.map,
                                                    $Maybe.Just,
                                                    shout)]));
   var mentsu = A4($Json$Decode.object3,
   $GameTypes.Mentsu,
   A2($Json$Decode._op[":="],
   "kind",
   mentsuKind),
   A2($Json$Decode._op[":="],
   "tile",
   tile),
   A2($Json$Decode._op[":="],
   "shout",
   shoutMaybe));
   var hand = A8($Json$Decode.object7,
   toHand,
   A2($Json$Decode._op[":="],
   "concealed",
   $Json$Decode.list(tile)),
   A2($Json$Decode._op[":="],
   "pick",
   tileMaybe),
   A2($Json$Decode._op[":="],
   "furiten",
   $Json$Decode.oneOf(_L.fromArray([A2($Json$Decode.map,
                                   $Maybe.Just,
                                   $Json$Decode.bool)
                                   ,$Json$Decode.succeed($Maybe.Nothing)]))),
   A2($Json$Decode._op[":="],
   "can-tsumo",
   $Json$Decode.bool),
   A2($Json$Decode._op[":="],
   "called",
   $Json$Decode.list(mentsu)),
   A2($Json$Decode._op[":="],
   "discards",
   $Json$Decode.list(discard)),
   A2($Json$Decode._op[":="],
   "riichi",
   $Json$Decode.bool));
   var handPublic = A4($Json$Decode.object3,
   toHandPublic,
   A2($Json$Decode._op[":="],
   "called",
   $Json$Decode.list(mentsu)),
   A2($Json$Decode._op[":="],
   "discards",
   $Json$Decode.list(discard)),
   A2($Json$Decode._op[":="],
   "riichi",
   $Json$Decode.bool));
   var playerHand = A3($Json$Decode.tuple2,
   F2(function (v0,v1) {
      return {ctor: "_Tuple2"
             ,_0: v0
             ,_1: v1};
   }),
   kaze,
   handPublic);
   var valuedHand = A4($Json$Decode.object3,
   $GameTypes.Valued,
   A2($Json$Decode._op[":="],
   "mentsu",
   $Json$Decode.list(mentsu)),
   A2($Json$Decode._op[":="],
   "tiles",
   $Json$Decode.list(tile)),
   A2($Json$Decode._op[":="],
   "value",
   handValue));
   var winner = A4($Json$Decode.tuple3,
   F3(function (p,points,h) {
      return A3($GameTypes.Winner,
      p,
      points,
      h);
   }),
   $Json$Decode.$int,
   $Json$Decode.$int,
   valuedHand);
   var turnActionOfType = function (taType) {
      return function () {
         switch (taType)
         {case "ankan":
            return A2($Json$Decode.object1,
              $GameTypes.TurnAnkan,
              A2($Json$Decode._op[":="],
              "tile",
              tile));
            case "discard":
            return A2($Json$Decode.object1,
              $GameTypes.TurnTileDiscard,
              discard);
            case "draw":
            return A3($Json$Decode.object2,
              $GameTypes.TurnTileDraw,
              A2($Json$Decode._op[":="],
              "wanpai",
              $Json$Decode.bool),
              A2($Json$Decode._op[":="],
              "tile",
              $Json$Decode.maybe(tile)));
            case "shouminkan":
            return A2($Json$Decode.object1,
              $GameTypes.TurnShouminkan,
              A2($Json$Decode._op[":="],
              "tile",
              tile));
            case "tsumo":
            return $Json$Decode.succeed($GameTypes.TurnTsumo);}
         _U.badCase($moduleName,
         "between lines 231 and 236");
      }();
   };
   var turnAction = A2($Json$Decode.andThen,
   A2($Json$Decode._op[":="],
   "type",
   $Json$Decode.string),
   turnActionOfType);
   var playerKaze = A2($Json$Decode._op[":="],
   "player-kaze",
   A2($Json$Decode.map,
   $GameTypes.readKaze,
   $Json$Decode.string));
   var player = A2($Json$Decode._op[":="],
   "player",
   $Json$Decode.$int);
   var payer = A3($Json$Decode.tuple2,
   F2(function (p,v) {
      return A2($GameTypes.Payer,
      p,
      v);
   }),
   player,
   $Json$Decode.$int);
   var resultsOfType = function (t) {
      return function () {
         switch (t)
         {case "dealdraw":
            return A3($Json$Decode.object2,
              F2(function (w,p) {
                 return $GameTypes.DealDraw({_: {}
                                            ,nooten: p
                                            ,tenpai: w});
              }),
              A2($Json$Decode._op[":="],
              "tenpais",
              $Json$Decode.list(payer)),
              A2($Json$Decode._op[":="],
              "nooten",
              $Json$Decode.list(payer)));
            case "dealron":
            return A3($Json$Decode.object2,
              F2(function (w,p) {
                 return $GameTypes.DealRon({_: {}
                                           ,payers: p
                                           ,winners: w});
              }),
              A2($Json$Decode._op[":="],
              "winners",
              $Json$Decode.list(winner)),
              A2($Json$Decode._op[":="],
              "payers",
              $Json$Decode.list(payer)));
            case "dealtsumo":
            return A3($Json$Decode.object2,
              F2(function (w,p) {
                 return $GameTypes.DealTsumo({_: {}
                                             ,payers: p
                                             ,winners: w});
              }),
              A2($Json$Decode._op[":="],
              "winners",
              $Json$Decode.list(winner)),
              A2($Json$Decode._op[":="],
              "payers",
              $Json$Decode.list(payer)));}
         _U.badCase($moduleName,
         "between lines 202 and 205");
      }();
   };
   var results = A2($Json$Decode.andThen,
   A2($Json$Decode._op[":="],
   "type",
   $Json$Decode.string),
   resultsOfType);
   var roundState = A2($Json$Decode.andThen,
   A9($Json$Decode.object8,
   $GameTypes.RoundState,
   A2($Json$Decode._op[":="],
   "mypos",
   kaze),
   A2($Json$Decode._op[":="],
   "round",
   kaze),
   A2($Json$Decode._op[":="],
   "turn",
   kaze),
   A2($Json$Decode._op[":="],
   "player",
   $Json$Decode.$int),
   A2($Json$Decode._op[":="],
   "oja",
   $Json$Decode.$int),
   A2($Json$Decode._op[":="],
   "first-oja",
   $Json$Decode.$int),
   A2($Json$Decode._op[":="],
   "tiles-left",
   $Json$Decode.$int),
   A2($Json$Decode._op[":="],
   "dora",
   $Json$Decode.list(tile))),
   function (x) {
      return A9($Json$Decode.object8,
      x,
      A2($Json$Decode._op[":="],
      "hands",
      $Json$Decode.list(playerHand)),
      A2($Json$Decode._op[":="],
      "players",
      $Json$Decode.list(players)),
      A2($Json$Decode._op[":="],
      "myhand",
      hand),
      A2($Json$Decode._op[":="],
      "results",
      $Json$Decode.maybe(results)),
      A2($Json$Decode._op[":="],
      "deal",
      $Json$Decode.$int),
      A2($Json$Decode._op[":="],
      "honba",
      $Json$Decode.$int),
      A2($Json$Decode._op[":="],
      "in-table",
      $Json$Decode.$int),
      $Json$Decode.succeed(_L.fromArray([])));
   });
   var content = A2($Json$Decode._op[":="],
   "content",
   $Json$Decode.string);
   var from = A2($Json$Decode._op[":="],
   "from",
   $Json$Decode.string);
   var ident = A2($Json$Decode._op[":="],
   "ident",
   $Json$Decode.$int);
   var nick = A2($Json$Decode._op[":="],
   "nick",
   $Json$Decode.string);
   var gameEventOfType = function (eventType) {
      return function () {
         switch (eventType)
         {case "end":
            return A2($Json$Decode.object1,
              $GameTypes.RoundEnded,
              A2($Json$Decode._op[":="],
              "results",
              results));
            case "filpped-dora":
            return A2($Json$Decode.object1,
              function (t) {
                 return $GameTypes.RoundFlippedDora({_: {}
                                                    ,tile: t});
              },
              A2($Json$Decode._op[":="],
              "tile",
              tile));
            case "hand":
            return A3($Json$Decode.object2,
              F2(function (pk,h) {
                 return $GameTypes.RoundHandChanged({_: {}
                                                    ,hand: h
                                                    ,player_kaze: pk});
              }),
              playerKaze,
              A2($Json$Decode._op[":="],
              "hand",
              handPublic));
            case "my-hand":
            return A2($Json$Decode.object1,
              function (h) {
                 return $GameTypes.RoundPrivateChange({_: {}
                                                      ,hand: h});
              },
              A2($Json$Decode._op[":="],
              "hand",
              hand));
            case "nick":
            return A3($Json$Decode.object2,
              F2(function (pk,n) {
                 return $GameTypes.RoundNick({_: {}
                                             ,nick: n
                                             ,player_kaze: pk});
              }),
              playerKaze,
              nick);
            case "riichi":
            return A2($Json$Decode.object1,
              function (pk) {
                 return $GameTypes.RoundRiichi({_: {}
                                               ,player_kaze: pk});
              },
              playerKaze);
            case "round-begin":
            return A2($Json$Decode.object1,
              $GameTypes.RoundPrivateStarts,
              roundState);
            case "set-points":
            return A3($Json$Decode.object2,
              F2(function (p,po) {
                 return $GameTypes.RoundGamePoints({_: {}
                                                   ,player: p
                                                   ,points: po});
              }),
              player,
              A2($Json$Decode._op[":="],
              "points",
              $Json$Decode.$int));
            case "shout":
            return A3($Json$Decode.object2,
              F2(function (pk,s) {
                 return $GameTypes.RoundTurnShouted({_: {}
                                                    ,player_kaze: pk
                                                    ,shout: s});
              }),
              playerKaze,
              A2($Json$Decode._op[":="],
              "shout",
              shout));
            case "turn-action":
            return A3($Json$Decode.object2,
              F2(function (pk,a) {
                 return $GameTypes.RoundTurnAction({_: {}
                                                   ,action: a
                                                   ,player_kaze: pk});
              }),
              playerKaze,
              A2($Json$Decode._op[":="],
              "action",
              turnAction));
            case "turn-changed":
            return A2($Json$Decode.object1,
              function (pk) {
                 return $GameTypes.RoundTurnBegins({_: {}
                                                   ,player_kaze: pk});
              },
              playerKaze);
            case "wait-shout":
            return A3($Json$Decode.object2,
              F2(function (s,ss) {
                 return $GameTypes.RoundPrivateWaitForShout({_: {}
                                                            ,seconds: s
                                                            ,shouts: ss});
              }),
              A2($Json$Decode._op[":="],
              "seconds",
              $Json$Decode.$int),
              A2($Json$Decode._op[":="],
              "shouts",
              $Json$Decode.list(shout)));
            case "wait-turn":
            return A4($Json$Decode.object3,
              F3(function (p,s,rw) {
                 return $GameTypes.RoundPrivateWaitForTurnAction({_: {}
                                                                 ,player: p
                                                                 ,riichiWith: rw
                                                                 ,seconds: s});
              }),
              A2($Json$Decode._op[":="],
              "player",
              $Json$Decode.$int),
              A2($Json$Decode._op[":="],
              "seconds",
              $Json$Decode.$int),
              A2($Json$Decode._op[":="],
              "riichi-with",
              $Json$Decode.list(tile)));}
         _U.badCase($moduleName,
         "between lines 91 and 104");
      }();
   };
   var gameEvent = A2($Json$Decode.andThen,
   A2($Json$Decode._op[":="],
   "event",
   $Json$Decode.string),
   gameEventOfType);
   var set = function (x) {
      return A2($Json$Decode.map,
      $Set.fromList,
      $Json$Decode.list(x));
   };
   var gameInfo = A4($Json$Decode.object3,
   F3(function (i,t,p) {
      return {_: {}
             ,ident: i
             ,players: p
             ,topic: t};
   }),
   A2($Json$Decode._op[":="],
   "ident",
   $Json$Decode.$int),
   A2($Json$Decode._op[":="],
   "topic",
   $Json$Decode.string),
   A2($Json$Decode._op[":="],
   "players",
   set(nick)));
   var lounge = A3($Json$Decode.object2,
   F2(function (i,g) {
      return {_: {}
             ,games: g
             ,idle: i};
   }),
   A2($Json$Decode._op[":="],
   "idle",
   A2($Json$Decode.map,
   $Set.fromList,
   $Json$Decode.list($Json$Decode.string))),
   A2($Json$Decode._op[":="],
   "games",
   $Json$Decode.list(gameInfo)));
   var eventOfType = function (eventType) {
      return function () {
         switch (eventType)
         {case "game-created":
            return A2($Json$Decode.object1,
              function (g) {
                 return $GameTypes.GameCreated({_: {}
                                               ,game: g});
              },
              gameInfo);
            case "game-event":
            return A2($Json$Decode.object1,
              $GameTypes.InGameEvents,
              A2($Json$Decode._op[":="],
              "events",
              $Json$Decode.list(gameEvent)));
            case "game-join":
            return A3($Json$Decode.object2,
              F2(function (i,n) {
                 return $GameTypes.JoinGame({_: {}
                                            ,ident: i
                                            ,nick: n});
              }),
              ident,
              nick);
            case "identity":
            return A2($Json$Decode.object1,
              function (n) {
                 return $GameTypes.Identity({_: {}
                                            ,nick: n});
              },
              nick);
            case "invalid":
            return A2($Json$Decode.object1,
              function (c) {
                 return $GameTypes.Invalid({_: {}
                                           ,content: c});
              },
              content);
            case "join":
            return A3($Json$Decode.object2,
              F2(function (n,i) {
                 return $GameTypes.JoinServer({_: {}
                                              ,ident: i
                                              ,nick: n});
              }),
              nick,
              ident);
            case "lounge":
            return A2($Json$Decode.object1,
              function (l) {
                 return $GameTypes.LoungeInfo({_: {}
                                              ,lounge: l});
              },
              lounge);
            case "msg":
            return A3($Json$Decode.object2,
              F2(function (f,c) {
                 return $GameTypes.Message({_: {}
                                           ,content: c
                                           ,from: f});
              }),
              from,
              content);
            case "part":
            return A2($Json$Decode.object1,
              function (n) {
                 return $GameTypes.PartServer({_: {}
                                              ,nick: n});
              },
              nick);}
         return $Json$Decode.succeed($GameTypes.Invalid({_: {}
                                                        ,content: A2($Basics._op["++"],
                                                        "Received unexpected event of type ",
                                                        eventType)}));
      }();
   };
   var event = A2($Json$Decode.andThen,
   A2($Json$Decode._op[":="],
   "type",
   $Json$Decode.string),
   eventOfType);
   var decodeEvent = function (str) {
      return function () {
         var _v39 = A2($Json$Decode.decodeString,
         event,
         str);
         switch (_v39.ctor)
         {case "Err":
            return $GameTypes.Invalid({_: {}
                                      ,content: _v39._0});
            case "Ok": return _v39._0;}
         _U.badCase($moduleName,
         "between lines 18 and 20");
      }();
   };
   _elm.JSON.values = {_op: _op
                      ,set: set
                      ,decodeEvent: decodeEvent
                      ,event: event
                      ,eventOfType: eventOfType
                      ,nick: nick
                      ,ident: ident
                      ,from: from
                      ,content: content
                      ,player: player
                      ,playerKaze: playerKaze
                      ,tile: tile
                      ,tileMaybe: tileMaybe
                      ,tileOfType: tileOfType
                      ,honor: honor
                      ,honorFrom: honorFrom
                      ,kaze: kaze
                      ,lounge: lounge
                      ,gameInfo: gameInfo
                      ,gameEvent: gameEvent
                      ,gameEventOfType: gameEventOfType
                      ,hand: hand
                      ,toHand: toHand
                      ,handPublic: handPublic
                      ,toHandPublic: toHandPublic
                      ,playerHand: playerHand
                      ,discard: discard
                      ,mentsu: mentsu
                      ,mentsuKind: mentsuKind
                      ,readMentsuKind: readMentsuKind
                      ,shout: shout
                      ,shoutMaybe: shoutMaybe
                      ,shoutKind: shoutKind
                      ,readShoutKind: readShoutKind
                      ,roundState: roundState
                      ,points: points
                      ,players: players
                      ,results: results
                      ,resultsOfType: resultsOfType
                      ,winner: winner
                      ,payer: payer
                      ,handValue: handValue
                      ,valuedHand: valuedHand
                      ,yaku: yaku
                      ,turnAction: turnAction
                      ,turnActionOfType: turnActionOfType
                      ,encodeEvent: encodeEvent
                      ,toJSON_Event: toJSON_Event
                      ,toJSON_GameAction: toJSON_GameAction
                      ,toJSON_ShoutKind: toJSON_ShoutKind
                      ,toJSON_Tile: toJSON_Tile
                      ,atType: atType
                      ,atAction: atAction};
   return _elm.JSON.values;
};
Elm.Json = Elm.Json || {};
Elm.Json.Decode = Elm.Json.Decode || {};
Elm.Json.Decode.make = function (_elm) {
   "use strict";
   _elm.Json = _elm.Json || {};
   _elm.Json.Decode = _elm.Json.Decode || {};
   if (_elm.Json.Decode.values)
   return _elm.Json.Decode.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Json.Decode",
   $Array = Elm.Array.make(_elm),
   $Dict = Elm.Dict.make(_elm),
   $Json$Encode = Elm.Json.Encode.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Json = Elm.Native.Json.make(_elm),
   $Result = Elm.Result.make(_elm);
   var tuple8 = $Native$Json.decodeTuple8;
   var tuple7 = $Native$Json.decodeTuple7;
   var tuple6 = $Native$Json.decodeTuple6;
   var tuple5 = $Native$Json.decodeTuple5;
   var tuple4 = $Native$Json.decodeTuple4;
   var tuple3 = $Native$Json.decodeTuple3;
   var tuple2 = $Native$Json.decodeTuple2;
   var tuple1 = $Native$Json.decodeTuple1;
   var succeed = $Native$Json.succeed;
   var fail = $Native$Json.fail;
   var andThen = $Native$Json.andThen;
   var customDecoder = $Native$Json.customDecoder;
   var decodeValue = $Native$Json.runDecoderValue;
   var value = $Native$Json.decodeValue;
   var maybe = $Native$Json.decodeMaybe;
   var $null = $Native$Json.decodeNull;
   var array = $Native$Json.decodeArray;
   var list = $Native$Json.decodeList;
   var bool = $Native$Json.decodeBool;
   var $int = $Native$Json.decodeInt;
   var $float = $Native$Json.decodeFloat;
   var string = $Native$Json.decodeString;
   var oneOf = $Native$Json.oneOf;
   var keyValuePairs = $Native$Json.decodeKeyValuePairs;
   var object8 = $Native$Json.decodeObject8;
   var object7 = $Native$Json.decodeObject7;
   var object6 = $Native$Json.decodeObject6;
   var object5 = $Native$Json.decodeObject5;
   var object4 = $Native$Json.decodeObject4;
   var object3 = $Native$Json.decodeObject3;
   var object2 = $Native$Json.decodeObject2;
   var object1 = $Native$Json.decodeObject1;
   _op[":="] = $Native$Json.decodeField;
   var at = F2(function (fields,
   decoder) {
      return A3($List.foldr,
      F2(function (x,y) {
         return A2(_op[":="],x,y);
      }),
      decoder,
      fields);
   });
   var decodeString = $Native$Json.runDecoderString;
   var map = $Native$Json.decodeObject1;
   var dict = function (decoder) {
      return A2(map,
      $Dict.fromList,
      keyValuePairs(decoder));
   };
   var Decoder = {ctor: "Decoder"};
   _elm.Json.Decode.values = {_op: _op
                             ,Decoder: Decoder
                             ,map: map
                             ,decodeString: decodeString
                             ,at: at
                             ,object1: object1
                             ,object2: object2
                             ,object3: object3
                             ,object4: object4
                             ,object5: object5
                             ,object6: object6
                             ,object7: object7
                             ,object8: object8
                             ,keyValuePairs: keyValuePairs
                             ,dict: dict
                             ,oneOf: oneOf
                             ,string: string
                             ,$float: $float
                             ,$int: $int
                             ,bool: bool
                             ,list: list
                             ,array: array
                             ,$null: $null
                             ,maybe: maybe
                             ,value: value
                             ,decodeValue: decodeValue
                             ,customDecoder: customDecoder
                             ,andThen: andThen
                             ,fail: fail
                             ,succeed: succeed
                             ,tuple1: tuple1
                             ,tuple2: tuple2
                             ,tuple3: tuple3
                             ,tuple4: tuple4
                             ,tuple5: tuple5
                             ,tuple6: tuple6
                             ,tuple7: tuple7
                             ,tuple8: tuple8};
   return _elm.Json.Decode.values;
};
Elm.Json = Elm.Json || {};
Elm.Json.Encode = Elm.Json.Encode || {};
Elm.Json.Encode.make = function (_elm) {
   "use strict";
   _elm.Json = _elm.Json || {};
   _elm.Json.Encode = _elm.Json.Encode || {};
   if (_elm.Json.Encode.values)
   return _elm.Json.Encode.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Json.Encode",
   $Array = Elm.Array.make(_elm),
   $Native$Json = Elm.Native.Json.make(_elm);
   var list = $Native$Json.encodeList;
   var array = $Native$Json.encodeArray;
   var object = $Native$Json.encodeObject;
   var $null = $Native$Json.encodeNull;
   var bool = $Native$Json.identity;
   var $float = $Native$Json.identity;
   var $int = $Native$Json.identity;
   var string = $Native$Json.identity;
   var encode = $Native$Json.encode;
   var Value = {ctor: "Value"};
   _elm.Json.Encode.values = {_op: _op
                             ,Value: Value
                             ,encode: encode
                             ,string: string
                             ,$int: $int
                             ,$float: $float
                             ,bool: bool
                             ,$null: $null
                             ,object: object
                             ,array: array
                             ,list: list};
   return _elm.Json.Encode.values;
};
Elm.Keyboard = Elm.Keyboard || {};
Elm.Keyboard.make = function (_elm) {
   "use strict";
   _elm.Keyboard = _elm.Keyboard || {};
   if (_elm.Keyboard.values)
   return _elm.Keyboard.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Keyboard",
   $Basics = Elm.Basics.make(_elm),
   $Native$Keyboard = Elm.Native.Keyboard.make(_elm),
   $Set = Elm.Set.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var presses = A2($Signal.map,
   function (_) {
      return _.keyCode;
   },
   $Native$Keyboard.presses);
   var toXY = F2(function (_v0,
   keyCodes) {
      return function () {
         return function () {
            var is = function (keyCode) {
               return A2($Set.member,
               keyCode,
               keyCodes) ? 1 : 0;
            };
            return {_: {}
                   ,x: is(_v0.right) - is(_v0.left)
                   ,y: is(_v0.up) - is(_v0.down)};
         }();
      }();
   });
   var Directions = F4(function (a,
   b,
   c,
   d) {
      return {_: {}
             ,down: b
             ,left: c
             ,right: d
             ,up: a};
   });
   var dropMap = F2(function (f,
   signal) {
      return $Signal.dropRepeats(A2($Signal.map,
      f,
      signal));
   });
   var EventInfo = F3(function (a,
   b,
   c) {
      return {_: {}
             ,alt: a
             ,keyCode: c
             ,meta: b};
   });
   var Blur = {ctor: "Blur"};
   var Down = function (a) {
      return {ctor: "Down",_0: a};
   };
   var Up = function (a) {
      return {ctor: "Up",_0: a};
   };
   var rawEvents = $Signal.mergeMany(_L.fromArray([A2($Signal.map,
                                                  Up,
                                                  $Native$Keyboard.ups)
                                                  ,A2($Signal.map,
                                                  Down,
                                                  $Native$Keyboard.downs)
                                                  ,A2($Signal.map,
                                                  $Basics.always(Blur),
                                                  $Native$Keyboard.blurs)]));
   var empty = {_: {}
               ,alt: false
               ,keyCodes: $Set.empty
               ,meta: false};
   var update = F2(function (event,
   model) {
      return function () {
         switch (event.ctor)
         {case "Blur": return empty;
            case "Down": return {_: {}
                                ,alt: event._0.alt
                                ,keyCodes: A2($Set.insert,
                                event._0.keyCode,
                                model.keyCodes)
                                ,meta: event._0.meta};
            case "Up": return {_: {}
                              ,alt: event._0.alt
                              ,keyCodes: A2($Set.remove,
                              event._0.keyCode,
                              model.keyCodes)
                              ,meta: event._0.meta};}
         _U.badCase($moduleName,
         "between lines 68 and 82");
      }();
   });
   var model = A3($Signal.foldp,
   update,
   empty,
   rawEvents);
   var alt = A2(dropMap,
   function (_) {
      return _.alt;
   },
   model);
   var meta = A2(dropMap,
   function (_) {
      return _.meta;
   },
   model);
   var keysDown = A2(dropMap,
   function (_) {
      return _.keyCodes;
   },
   model);
   var arrows = A2(dropMap,
   toXY({_: {}
        ,down: 40
        ,left: 37
        ,right: 39
        ,up: 38}),
   keysDown);
   var wasd = A2(dropMap,
   toXY({_: {}
        ,down: 83
        ,left: 65
        ,right: 68
        ,up: 87}),
   keysDown);
   var isDown = function (keyCode) {
      return A2(dropMap,
      $Set.member(keyCode),
      keysDown);
   };
   var ctrl = isDown(17);
   var shift = isDown(16);
   var space = isDown(32);
   var enter = isDown(13);
   var Model = F3(function (a,
   b,
   c) {
      return {_: {}
             ,alt: a
             ,keyCodes: c
             ,meta: b};
   });
   _elm.Keyboard.values = {_op: _op
                          ,arrows: arrows
                          ,wasd: wasd
                          ,enter: enter
                          ,space: space
                          ,ctrl: ctrl
                          ,shift: shift
                          ,alt: alt
                          ,meta: meta
                          ,isDown: isDown
                          ,keysDown: keysDown
                          ,presses: presses};
   return _elm.Keyboard.values;
};
Elm.List = Elm.List || {};
Elm.List.make = function (_elm) {
   "use strict";
   _elm.List = _elm.List || {};
   if (_elm.List.values)
   return _elm.List.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "List",
   $Basics = Elm.Basics.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$List = Elm.Native.List.make(_elm);
   var sortWith = $Native$List.sortWith;
   var sortBy = $Native$List.sortBy;
   var sort = function (xs) {
      return A2(sortBy,
      $Basics.identity,
      xs);
   };
   var repeat = $Native$List.repeat;
   var drop = $Native$List.drop;
   var take = $Native$List.take;
   var map5 = $Native$List.map5;
   var map4 = $Native$List.map4;
   var map3 = $Native$List.map3;
   var map2 = $Native$List.map2;
   var any = $Native$List.any;
   var all = F2(function (pred,
   xs) {
      return $Basics.not(A2(any,
      function ($) {
         return $Basics.not(pred($));
      },
      xs));
   });
   var foldr = $Native$List.foldr;
   var foldl = $Native$List.foldl;
   var length = function (xs) {
      return A3(foldl,
      F2(function (_v0,i) {
         return function () {
            return i + 1;
         }();
      }),
      0,
      xs);
   };
   var sum = function (numbers) {
      return A3(foldl,
      F2(function (x,y) {
         return x + y;
      }),
      0,
      numbers);
   };
   var product = function (numbers) {
      return A3(foldl,
      F2(function (x,y) {
         return x * y;
      }),
      1,
      numbers);
   };
   var maximum = function (list) {
      return function () {
         switch (list.ctor)
         {case "::":
            return $Maybe.Just(A3(foldl,
              $Basics.max,
              list._0,
              list._1));}
         return $Maybe.Nothing;
      }();
   };
   var minimum = function (list) {
      return function () {
         switch (list.ctor)
         {case "::":
            return $Maybe.Just(A3(foldl,
              $Basics.min,
              list._0,
              list._1));}
         return $Maybe.Nothing;
      }();
   };
   var indexedMap = F2(function (f,
   xs) {
      return A3(map2,
      f,
      _L.range(0,length(xs) - 1),
      xs);
   });
   var member = F2(function (x,
   xs) {
      return A2(any,
      function (a) {
         return _U.eq(a,x);
      },
      xs);
   });
   var isEmpty = function (xs) {
      return function () {
         switch (xs.ctor)
         {case "[]": return true;}
         return false;
      }();
   };
   var tail = function (list) {
      return function () {
         switch (list.ctor)
         {case "::":
            return $Maybe.Just(list._1);
            case "[]":
            return $Maybe.Nothing;}
         _U.badCase($moduleName,
         "between lines 87 and 95");
      }();
   };
   var head = function (list) {
      return function () {
         switch (list.ctor)
         {case "::":
            return $Maybe.Just(list._0);
            case "[]":
            return $Maybe.Nothing;}
         _U.badCase($moduleName,
         "between lines 75 and 84");
      }();
   };
   _op["::"] = $Native$List.cons;
   var map = F2(function (f,xs) {
      return A3(foldr,
      F2(function (x,acc) {
         return A2(_op["::"],
         f(x),
         acc);
      }),
      _L.fromArray([]),
      xs);
   });
   var filter = F2(function (pred,
   xs) {
      return function () {
         var conditionalCons = F2(function (x,
         xs$) {
            return pred(x) ? A2(_op["::"],
            x,
            xs$) : xs$;
         });
         return A3(foldr,
         conditionalCons,
         _L.fromArray([]),
         xs);
      }();
   });
   var maybeCons = F3(function (f,
   mx,
   xs) {
      return function () {
         var _v15 = f(mx);
         switch (_v15.ctor)
         {case "Just":
            return A2(_op["::"],_v15._0,xs);
            case "Nothing": return xs;}
         _U.badCase($moduleName,
         "between lines 179 and 186");
      }();
   });
   var filterMap = F2(function (f,
   xs) {
      return A3(foldr,
      maybeCons(f),
      _L.fromArray([]),
      xs);
   });
   var reverse = function (list) {
      return A3(foldl,
      F2(function (x,y) {
         return A2(_op["::"],x,y);
      }),
      _L.fromArray([]),
      list);
   };
   var scanl = F3(function (f,
   b,
   xs) {
      return function () {
         var scan1 = F2(function (x,
         accAcc) {
            return function () {
               switch (accAcc.ctor)
               {case "::": return A2(_op["::"],
                    A2(f,x,accAcc._0),
                    accAcc);
                  case "[]":
                  return _L.fromArray([]);}
               _U.badCase($moduleName,
               "between lines 148 and 151");
            }();
         });
         return reverse(A3(foldl,
         scan1,
         _L.fromArray([b]),
         xs));
      }();
   });
   var append = F2(function (xs,
   ys) {
      return function () {
         switch (ys.ctor)
         {case "[]": return xs;}
         return A3(foldr,
         F2(function (x,y) {
            return A2(_op["::"],x,y);
         }),
         ys,
         xs);
      }();
   });
   var concat = function (lists) {
      return A3(foldr,
      append,
      _L.fromArray([]),
      lists);
   };
   var concatMap = F2(function (f,
   list) {
      return concat(A2(map,
      f,
      list));
   });
   var partition = F2(function (pred,
   list) {
      return function () {
         var step = F2(function (x,
         _v21) {
            return function () {
               switch (_v21.ctor)
               {case "_Tuple2":
                  return pred(x) ? {ctor: "_Tuple2"
                                   ,_0: A2(_op["::"],x,_v21._0)
                                   ,_1: _v21._1} : {ctor: "_Tuple2"
                                                   ,_0: _v21._0
                                                   ,_1: A2(_op["::"],
                                                   x,
                                                   _v21._1)};}
               _U.badCase($moduleName,
               "between lines 301 and 303");
            }();
         });
         return A3(foldr,
         step,
         {ctor: "_Tuple2"
         ,_0: _L.fromArray([])
         ,_1: _L.fromArray([])},
         list);
      }();
   });
   var unzip = function (pairs) {
      return function () {
         var step = F2(function (_v25,
         _v26) {
            return function () {
               switch (_v26.ctor)
               {case "_Tuple2":
                  return function () {
                       switch (_v25.ctor)
                       {case "_Tuple2":
                          return {ctor: "_Tuple2"
                                 ,_0: A2(_op["::"],
                                 _v25._0,
                                 _v26._0)
                                 ,_1: A2(_op["::"],
                                 _v25._1,
                                 _v26._1)};}
                       _U.badCase($moduleName,
                       "on line 339, column 12 to 28");
                    }();}
               _U.badCase($moduleName,
               "on line 339, column 12 to 28");
            }();
         });
         return A3(foldr,
         step,
         {ctor: "_Tuple2"
         ,_0: _L.fromArray([])
         ,_1: _L.fromArray([])},
         pairs);
      }();
   };
   var intersperse = F2(function (sep,
   xs) {
      return function () {
         switch (xs.ctor)
         {case "::": return function () {
                 var step = F2(function (x,
                 rest) {
                    return A2(_op["::"],
                    sep,
                    A2(_op["::"],x,rest));
                 });
                 var spersed = A3(foldr,
                 step,
                 _L.fromArray([]),
                 xs._1);
                 return A2(_op["::"],
                 xs._0,
                 spersed);
              }();
            case "[]":
            return _L.fromArray([]);}
         _U.badCase($moduleName,
         "between lines 350 and 361");
      }();
   });
   _elm.List.values = {_op: _op
                      ,isEmpty: isEmpty
                      ,length: length
                      ,reverse: reverse
                      ,member: member
                      ,head: head
                      ,tail: tail
                      ,filter: filter
                      ,take: take
                      ,drop: drop
                      ,repeat: repeat
                      ,append: append
                      ,concat: concat
                      ,intersperse: intersperse
                      ,partition: partition
                      ,unzip: unzip
                      ,map: map
                      ,map2: map2
                      ,map3: map3
                      ,map4: map4
                      ,map5: map5
                      ,filterMap: filterMap
                      ,concatMap: concatMap
                      ,indexedMap: indexedMap
                      ,foldr: foldr
                      ,foldl: foldl
                      ,sum: sum
                      ,product: product
                      ,maximum: maximum
                      ,minimum: minimum
                      ,all: all
                      ,any: any
                      ,scanl: scanl
                      ,sort: sort
                      ,sortBy: sortBy
                      ,sortWith: sortWith};
   return _elm.List.values;
};
Elm.Lounge = Elm.Lounge || {};
Elm.Lounge.make = function (_elm) {
   "use strict";
   _elm.Lounge = _elm.Lounge || {};
   if (_elm.Lounge.values)
   return _elm.Lounge.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Lounge",
   $Basics = Elm.Basics.make(_elm),
   $Color = Elm.Color.make(_elm),
   $Events = Elm.Events.make(_elm),
   $GameTypes = Elm.GameTypes.make(_elm),
   $Graphics$Element = Elm.Graphics.Element.make(_elm),
   $Graphics$Input = Elm.Graphics.Input.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Set = Elm.Set.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $String = Elm.String.make(_elm),
   $Text = Elm.Text.make(_elm),
   $Util = Elm.Util.make(_elm);
   var blockElement = F3(function (w,
   h,
   e) {
      return A3($Graphics$Element.size,
      w,
      h,
      A2($Graphics$Element.color,
      $Color.gray,
      e));
   });
   var maybeEvent = F2(function (f,
   s) {
      return A2($Signal._op["<~"],
      function ($) {
         return $Maybe.withDefault($GameTypes.Noop)($Maybe.map(f)($));
      },
      s);
   });
   var chosenGame = $Signal.mailbox($Maybe.Nothing);
   var chooseGame = function (game) {
      return A2($Signal.message,
      chosenGame.address,
      $Maybe.Just(game));
   };
   var forceStartMailbox = $Signal.mailbox($Maybe.Nothing);
   var forceStart = function (n) {
      return A2($Signal.message,
      forceStartMailbox.address,
      $Maybe.Just(n));
   };
   var forceStartButton = function (n) {
      return A2($Graphics$Input.button,
      forceStart(n),
      "Force start");
   };
   var forceStartEvent = A2(maybeEvent,
   $Events.forceStart,
   forceStartMailbox.signal);
   var gameListElement = function (game) {
      return $Graphics$Input.clickable(chooseGame(game));
   };
   var gameInfoView = function (_v0) {
      return function () {
         return function () {
            var pc = $List.length($Set.toList(_v0.players));
            return $Graphics$Element.leftAligned(A2($Basics._op["++"],
            $Text.color($Color.red)($Text.fromString($Basics.toString(_v0.ident))),
            A2($Basics._op["++"],
            $Text.fromString(" "),
            A2($Basics._op["++"],
            $Text.bold($Text.fromString(_v0.topic)),
            A2($Basics._op["++"],
            $Text.fromString(" "),
            A2($Basics._op["++"],
            $Text.fromString(" ("),
            A2($Basics._op["++"],
            $Text.bold($Text.fromString($Basics.toString(pc))),
            A2($Basics._op["++"],
            $Text.fromString("/4 players)"),
            A2($Basics._op["++"],
            $Text.fromString(" {"),
            A2($Basics._op["++"],
            $Text.fromString($String.join(", ")($Set.toList(_v0.players))),
            $Text.fromString("}")))))))))));
         }();
      }();
   };
   var buildGameList = function (act) {
      return function () {
         var active = function (gi) {
            return _U.eq(act,
            $Maybe.Just(gi)) ? $Graphics$Element.color($Color.blue) : $Basics.identity;
         };
         return $List.map(function (gi) {
            return active(gi)(gameListElement(gi)(gameInfoView(gi)));
         });
      }();
   };
   var waitView = function (gameinfo) {
      return $Graphics$Element.color($Color.lightOrange)(A2($Graphics$Element.flow,
      $Graphics$Element.right,
      _L.fromArray([$Graphics$Element.leftAligned($Text.fromString("Waiting for "))
                   ,gameInfoView(gameinfo)
                   ,forceStartButton(gameinfo.ident)])));
   };
   var Controls = function (a) {
      return {_: {},chosen: a};
   };
   var controls = A2($Signal._op["<~"],
   Controls,
   chosenGame.signal);
   var Clear = {ctor: "Clear"};
   var joinGame = $Signal.mailbox(Clear);
   var Submit = {ctor: "Submit"};
   var joinGameButton = A2($Graphics$Input.button,
   A2($Signal.message,
   joinGame.address,
   Submit),
   "Join");
   var gameListView = function (o) {
      return A2($Graphics$Element.flow,
      $Graphics$Element.down,
      _L.fromArray([$Graphics$Element.centered($Text.bold($Text.fromString("Available games")))
                   ,$Graphics$Element.flow($Graphics$Element.down)(A2(buildGameList,
                   o.chosen,
                   o.game.lounge.games))
                   ,joinGameButton
                   ,$Graphics$Element.leftAligned(A2($Text.append,
                   $Text.fromString("Idle players: "),
                   $Text.fromString($String.join(", ")($Set.toList(o.game.lounge.idle)))))]));
   };
   var doDraw = function (o) {
      return A2($Graphics$Element.flow,
      $Graphics$Element.down,
      _L.fromArray([$Maybe.withDefault($Graphics$Element.color($Color.red)(A2($Graphics$Element.spacer,
                   10,
                   10)))(A2($Maybe.map,
                   waitView,
                   A2($Maybe.andThen,
                   o.game.gameWait,
                   $Util.lookupGameInfo(o.game))))
                   ,$Graphics$Element.centered($Text.bold($Text.fromString("Lounge")))
                   ,A3(blockElement,
                   250,
                   400,
                   gameListView(o))]));
   };
   var display = F2(function (v,
   gs) {
      return doDraw(_U.insert("game",
      gs,
      v));
   });
   var isSubmit = function (s) {
      return A2($Signal._op["<~"],
      function (x) {
         return _U.eq(x,Submit);
      },
      s);
   };
   var joined = A2($Signal.sampleOn,
   isSubmit(joinGame.signal),
   chosenGame.signal);
   var events = $Signal.mergeMany(_L.fromArray([A2(maybeEvent,
                                               function ($) {
                                                  return $Events.joinGame(function (_) {
                                                     return _.ident;
                                                  }($));
                                               },
                                               joined)
                                               ,forceStartEvent]));
   _elm.Lounge.values = {_op: _op
                        ,Submit: Submit
                        ,Clear: Clear
                        ,Controls: Controls
                        ,controls: controls
                        ,events: events
                        ,display: display
                        ,doDraw: doDraw
                        ,waitView: waitView
                        ,gameListView: gameListView
                        ,buildGameList: buildGameList
                        ,gameInfoView: gameInfoView
                        ,gameListElement: gameListElement
                        ,forceStartMailbox: forceStartMailbox
                        ,forceStart: forceStart
                        ,forceStartEvent: forceStartEvent
                        ,forceStartButton: forceStartButton
                        ,chosenGame: chosenGame
                        ,chooseGame: chooseGame
                        ,joinGame: joinGame
                        ,joinGameButton: joinGameButton
                        ,joined: joined
                        ,isSubmit: isSubmit
                        ,maybeEvent: maybeEvent
                        ,blockElement: blockElement};
   return _elm.Lounge.values;
};
Elm.Main = Elm.Main || {};
Elm.Main.make = function (_elm) {
   "use strict";
   _elm.Main = _elm.Main || {};
   if (_elm.Main.values)
   return _elm.Main.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Main",
   $Basics = Elm.Basics.make(_elm),
   $Color = Elm.Color.make(_elm),
   $Game = Elm.Game.make(_elm),
   $GameTypes = Elm.GameTypes.make(_elm),
   $Graphics$Element = Elm.Graphics.Element.make(_elm),
   $JSON = Elm.JSON.make(_elm),
   $List = Elm.List.make(_elm),
   $Lounge = Elm.Lounge.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Set = Elm.Set.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $Text = Elm.Text.make(_elm),
   $Time = Elm.Time.make(_elm),
   $Util = Elm.Util.make(_elm);
   var deleteNick = F2(function (n,
   l) {
      return _U.replace([["idle"
                         ,A2($Set.remove,n,l.idle)]
                        ,["games"
                         ,A2($List.map,
                         function (g) {
                            return _U.replace([["players"
                                               ,A2($Set.remove,n,g.players)]],
                            g);
                         },
                         l.games)]],
      l);
   });
   var addJoinedGame = F3(function (i,
   n,
   l) {
      return _U.replace([["games"
                         ,A2($List.map,
                         function (g) {
                            return _U.eq(g.ident,
                            i) ? _U.replace([["players"
                                             ,A2($Set.insert,n,g.players)]],
                            g) : g;
                         },
                         l.games)]
                        ,["idle"
                         ,A2($Set.remove,n,l.idle)]],
      l);
   });
   var addIdle = F2(function (n,
   l) {
      return _U.replace([["idle"
                         ,A2($Set.insert,n,l.idle)]],
      l);
   });
   var addGame = F2(function (g,
   l) {
      return _U.replace([["games"
                         ,A2($Basics._op["++"],
                         l.games,
                         _L.fromArray([g]))]],
      l);
   });
   var stepEvent = F2(function (event,
   gameState) {
      return function () {
         switch (event.ctor)
         {case "GameCreated":
            return _U.replace([["lounge"
                               ,A2(addGame,
                               event._0.game,
                               gameState.lounge)]],
              gameState);
            case "Identity":
            return _U.replace([["mynick"
                               ,event._0.nick]],
              gameState);
            case "InGameEvents":
            return A3($List.foldl,
              $Game.processInGameEvent,
              gameState,
              event._0);
            case "Invalid":
            return gameState;
            case "JoinGame":
            return _U.replace([["lounge"
                               ,A3(addJoinedGame,
                               event._0.ident,
                               event._0.nick,
                               gameState.lounge)]
                              ,["gameWait"
                               ,_U.eq(gameState.mynick,
                               event._0.nick) ? $Maybe.Just(event._0.ident) : gameState.gameWait]],
              gameState);
            case "JoinServer":
            return _U.replace([["lounge"
                               ,A2(addIdle,
                               event._0.nick,
                               gameState.lounge)]],
              gameState);
            case "LoungeInfo":
            return _U.replace([["lounge"
                               ,event._0.lounge]],
              gameState);
            case "Message":
            return gameState;
            case "PartServer":
            return _U.replace([["lounge"
                               ,A2(deleteNick,
                               event._0.nick,
                               gameState.lounge)]],
              gameState);}
         return gameState;
      }();
   });
   var stepGame = F2(function (x,
   gs) {
      return function () {
         switch (x.ctor)
         {case "AnEvent":
            return stepEvent(x._0)(_U.replace([["eventlog"
                                               ,A2($List._op["::"],
                                               x._0,
                                               gs.eventlog)]],
              gs));
            case "TimeDelta":
            return _U.replace([["updated"
                               ,x._0]
                              ,["turnBegan"
                               ,_U.eq(gs.turnBegan,
                               0) ? x._0 : gs.turnBegan]],
              gs);}
         return gs;
      }();
   });
   var soundFromTurnAction = function (ta) {
      return function () {
         switch (ta.ctor)
         {case "TurnTileDiscard":
            return "pop";
            case "TurnTileDraw":
            return "pop";}
         return "none";
      }();
   };
   var soundFromInput = function (inp) {
      return function () {
         switch (inp.ctor)
         {case "AnEvent":
            switch (inp._0.ctor)
              {case "InGameEvents":
                 switch (inp._0._0.ctor)
                   {case "::":
                      switch (inp._0._0._0.ctor)
                        {case "RoundTurnAction":
                           return soundFromTurnAction(inp._0._0._0._0.action);}
                        break;}
                   break;}
              break;}
         return "";
      }();
   };
   var TimeDelta = function (a) {
      return {ctor: "TimeDelta"
             ,_0: a};
   };
   var LoungeInput = function (a) {
      return {ctor: "LoungeInput"
             ,_0: a};
   };
   var GameInput = function (a) {
      return {ctor: "GameInput"
             ,_0: a};
   };
   var AnEvent = function (a) {
      return {ctor: "AnEvent"
             ,_0: a};
   };
   var newState = {_: {}
                  ,debuglog: _L.fromArray([])
                  ,eventlog: _L.fromArray([])
                  ,gameWait: $Maybe.Nothing
                  ,lounge: $GameTypes.defaultLounge
                  ,myid: 0
                  ,mynick: ""
                  ,riichiWith: _L.fromArray([])
                  ,roundState: $Maybe.Nothing
                  ,status: $GameTypes.InLounge
                  ,turnBegan: 0
                  ,updated: 0
                  ,waitShout: $Maybe.Nothing
                  ,waitTurnAction: $Maybe.Nothing};
   var upstream = Elm.Native.Port.make(_elm).outboundSignal("upstream",
   function (v) {
      return v;
   },
   A2($Signal._op["<~"],
   $JSON.encodeEvent,
   $Signal.mergeMany(_L.fromArray([$Lounge.events
                                  ,$Game.events]))));
   var downstream = Elm.Native.Port.make(_elm).inboundSignal("downstream",
   "String",
   function (v) {
      return typeof v === "string" || typeof v === "object" && v instanceof String ? v : _U.badPort("a string",
      v);
   });
   var eventInput = A2($Signal._op["<~"],
   $JSON.decodeEvent,
   downstream);
   var input = $Signal.mergeMany(_L.fromArray([A2($Signal._op["<~"],
                                              AnEvent,
                                              eventInput)
                                              ,A2($Signal._op["<~"],
                                              GameInput,
                                              $Game.controls)
                                              ,A2($Signal._op["<~"],
                                              LoungeInput,
                                              $Lounge.controls)
                                              ,A2($Signal._op["<~"],
                                              TimeDelta,
                                              $Time.every($Time.second))]));
   var sounds = Elm.Native.Port.make(_elm).outboundSignal("sounds",
   function (v) {
      return v;
   },
   A2($Signal._op["<~"],
   soundFromInput,
   input));
   var gameState = A3($Signal.foldp,
   stepGame,
   newState,
   input);
   var mainView = $Signal.mergeMany(_L.fromArray([A2($Signal.sampleOn,
                                                 A3($Signal.filter,
                                                 $Util.atLounge,
                                                 newState,
                                                 gameState),
                                                 A3($Signal.map2,
                                                 $Lounge.display,
                                                 $Lounge.controls,
                                                 gameState))
                                                 ,A2($Signal.sampleOn,
                                                 A3($Signal.filter,
                                                 $Util.inGame,
                                                 newState,
                                                 gameState),
                                                 A3($Signal.map2,
                                                 $Game.display,
                                                 $Game.controls,
                                                 gameState))]));
   var eventView = function (ev) {
      return function () {
         switch (ev.ctor)
         {case "GameCreated":
            return A2($Text.join,
              $Text.fromString(" "),
              _L.fromArray([$Text.fromString("Game")
                           ,$Text.fromString(ev._0.game.topic)
                           ,$Text.fromString($Basics.toString(ev._0.game.ident))
                           ,$Text.join($Text.fromString(" "))($List.map($Text.fromString)($Set.toList(ev._0.game.players)))]));
            case "Identity":
            return $Text.color($Color.blue)($Text.fromString(A2($Basics._op["++"],
              "I am `",
              A2($Basics._op["++"],
              ev._0.nick,
              "\'"))));
            case "InGameEvents":
            return $Text.fromString("in-game");
            case "Invalid":
            return $Text.color($Color.white)($Text.fromString(ev._0.content));
            case "JoinServer":
            return $Text.color($Color.green)($Text.fromString(A2($Basics._op["++"],
              ev._0.nick,
              " joined server.")));
            case "LoungeInfo":
            return $Text.color($Color.blue)($Text.fromString("Lounge updated - connection established!"));
            case "Message":
            return A2($Basics._op["++"],
              $Text.fromString("<"),
              A2($Basics._op["++"],
              $Text.bold($Text.fromString(ev._0.from)),
              A2($Basics._op["++"],
              $Text.fromString("> "),
              $Text.fromString(ev._0.content))));
            case "PartServer":
            return $Text.color($Color.red)($Text.fromString(A2($Basics._op["++"],
              ev._0.nick,
              " has left.")));}
         return $Text.color($Color.orange)($Text.fromString($Basics.toString(ev)));
      }();
   };
   var titled = function (str) {
      return $Graphics$Element.above($Graphics$Element.leftAligned($Text.color($Color.charcoal)($Text.fromString(str))));
   };
   var isNotInGame = function (x) {
      return function () {
         switch (x.ctor)
         {case "InGameEvents":
            return false;}
         return true;
      }();
   };
   var logView = function (game) {
      return A3($Graphics$Element.container,
      500,
      200,
      $Graphics$Element.topLeft)(titled("Log")($Graphics$Element.flow($Graphics$Element.down)($List.map(function ($) {
         return $Graphics$Element.leftAligned(eventView($));
      })($List.take(6)(A2($List.filter,
      isNotInGame,
      game.eventlog))))));
   };
   var display = F2(function (game,
   view) {
      return A2($Graphics$Element.flow,
      $Graphics$Element.down,
      _L.fromArray([view
                   ,logView(game)]));
   });
   var main = A3($Signal.map2,
   display,
   gameState,
   mainView);
   _elm.Main.values = {_op: _op
                      ,logView: logView
                      ,isNotInGame: isNotInGame
                      ,titled: titled
                      ,eventView: eventView
                      ,eventInput: eventInput
                      ,newState: newState
                      ,AnEvent: AnEvent
                      ,GameInput: GameInput
                      ,LoungeInput: LoungeInput
                      ,TimeDelta: TimeDelta
                      ,input: input
                      ,soundFromInput: soundFromInput
                      ,soundFromTurnAction: soundFromTurnAction
                      ,gameState: gameState
                      ,stepGame: stepGame
                      ,stepEvent: stepEvent
                      ,addGame: addGame
                      ,addIdle: addIdle
                      ,addJoinedGame: addJoinedGame
                      ,deleteNick: deleteNick
                      ,display: display
                      ,mainView: mainView
                      ,main: main};
   return _elm.Main.values;
};
Elm.Maybe = Elm.Maybe || {};
Elm.Maybe.make = function (_elm) {
   "use strict";
   _elm.Maybe = _elm.Maybe || {};
   if (_elm.Maybe.values)
   return _elm.Maybe.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Maybe";
   var withDefault = F2(function ($default,
   maybe) {
      return function () {
         switch (maybe.ctor)
         {case "Just": return maybe._0;
            case "Nothing":
            return $default;}
         _U.badCase($moduleName,
         "between lines 45 and 56");
      }();
   });
   var Nothing = {ctor: "Nothing"};
   var oneOf = function (maybes) {
      return function () {
         switch (maybes.ctor)
         {case "::": return function () {
                 switch (maybes._0.ctor)
                 {case "Just": return maybes._0;
                    case "Nothing":
                    return oneOf(maybes._1);}
                 _U.badCase($moduleName,
                 "between lines 64 and 73");
              }();
            case "[]": return Nothing;}
         _U.badCase($moduleName,
         "between lines 59 and 73");
      }();
   };
   var andThen = F2(function (maybeValue,
   callback) {
      return function () {
         switch (maybeValue.ctor)
         {case "Just":
            return callback(maybeValue._0);
            case "Nothing": return Nothing;}
         _U.badCase($moduleName,
         "between lines 110 and 112");
      }();
   });
   var Just = function (a) {
      return {ctor: "Just",_0: a};
   };
   var map = F2(function (f,
   maybe) {
      return function () {
         switch (maybe.ctor)
         {case "Just":
            return Just(f(maybe._0));
            case "Nothing": return Nothing;}
         _U.badCase($moduleName,
         "between lines 76 and 107");
      }();
   });
   _elm.Maybe.values = {_op: _op
                       ,andThen: andThen
                       ,map: map
                       ,withDefault: withDefault
                       ,oneOf: oneOf
                       ,Just: Just
                       ,Nothing: Nothing};
   return _elm.Maybe.values;
};
Elm.Mouse = Elm.Mouse || {};
Elm.Mouse.make = function (_elm) {
   "use strict";
   _elm.Mouse = _elm.Mouse || {};
   if (_elm.Mouse.values)
   return _elm.Mouse.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Mouse",
   $Basics = Elm.Basics.make(_elm),
   $Native$Mouse = Elm.Native.Mouse.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var clicks = $Native$Mouse.clicks;
   var isDown = $Native$Mouse.isDown;
   var position = $Native$Mouse.position;
   var x = A2($Signal.map,
   $Basics.fst,
   position);
   var y = A2($Signal.map,
   $Basics.snd,
   position);
   _elm.Mouse.values = {_op: _op
                       ,position: position
                       ,x: x
                       ,y: y
                       ,isDown: isDown
                       ,clicks: clicks};
   return _elm.Mouse.values;
};
Elm.Native.Array = {};
Elm.Native.Array.make = function(localRuntime) {

	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Array = localRuntime.Native.Array || {};
	if (localRuntime.Native.Array.values)
	{
		return localRuntime.Native.Array.values;
	}
	if ('values' in Elm.Native.Array)
	{
		return localRuntime.Native.Array.values = Elm.Native.Array.values;
	}

	var List = Elm.Native.List.make(localRuntime);

	// A RRB-Tree has two distinct data types.
	// Leaf -> "height"  is always 0
	//         "table"   is an array of elements
	// Node -> "height"  is always greater than 0
	//         "table"   is an array of child nodes
	//         "lengths" is an array of accumulated lengths of the child nodes

	// M is the maximal table size. 32 seems fast. E is the allowed increase
	// of search steps when concatting to find an index. Lower values will
	// decrease balancing, but will increase search steps.
	var M = 32;
	var E = 2;

	// An empty array.
	var empty = {
		ctor: "_Array",
		height: 0,
		table: new Array()
	};


	function get(i, array)
	{
		if (i < 0 || i >= length(array))
		{
			throw new Error(
				"Index " + i + " is out of range. Check the length of " +
				"your array first or use getMaybe or getWithDefault.");
		}
		return unsafeGet(i, array);
	}


	function unsafeGet(i, array)
	{
		for (var x = array.height; x > 0; x--)
		{
			var slot = i >> (x * 5);
			while (array.lengths[slot] <= i)
			{
				slot++;
			}
			if (slot > 0)
			{
				i -= array.lengths[slot - 1];
			}
			array = array.table[slot];
		}
		return array.table[i];
	}


	// Sets the value at the index i. Only the nodes leading to i will get
	// copied and updated.
	function set(i, item, array)
	{
		if (i < 0 || length(array) <= i)
		{
			return array;
		}
		return unsafeSet(i, item, array);
	}


	function unsafeSet(i, item, array)
	{
		array = nodeCopy(array);

		if (array.height == 0)
		{
			array.table[i] = item;
		}
		else
		{
			var slot = getSlot(i, array);
			if (slot > 0)
			{
				i -= array.lengths[slot - 1];
			}
			array.table[slot] = unsafeSet(i, item, array.table[slot]);
		}
		return array;
	}


	function initialize(len, f)
	{
		if (len == 0)
		{
			return empty;
		}
		var h = Math.floor( Math.log(len) / Math.log(M) );
		return initialize_(f, h, 0, len);
	}

	function initialize_(f, h, from, to)
	{
		if (h == 0)
		{
			var table = new Array((to - from) % (M + 1));
			for (var i = 0; i < table.length; i++)
			{
			  table[i] = f(from + i);
			}
			return {
				ctor: "_Array",
				height: 0,
				table: table
			};
		}

		var step = Math.pow(M, h);
		var table = new Array(Math.ceil((to - from) / step));
		var lengths = new Array(table.length);
		for (var i = 0; i < table.length; i++)
		{
			table[i] = initialize_(f, h - 1, from + (i * step), Math.min(from + ((i + 1) * step), to));
			lengths[i] = length(table[i]) + (i > 0 ? lengths[i-1] : 0);
		}
		return {
			ctor: "_Array",
			height: h,
			table: table,
			lengths: lengths
		};
	}

	function fromList(list)
	{
		if (list == List.Nil)
		{
			return empty;
		}

		// Allocate M sized blocks (table) and write list elements to it.
		var table = new Array(M);
		var nodes = new Array();
		var i = 0;

		while (list.ctor !== '[]')
		{
			table[i] = list._0;
			list = list._1;
			i++;

			// table is full, so we can push a leaf containing it into the
			// next node.
			if (i == M)
			{
				var leaf = {
					ctor: "_Array",
					height: 0,
					table: table
				};
				fromListPush(leaf, nodes);
				table = new Array(M);
				i = 0;
			}
		}

		// Maybe there is something left on the table.
		if (i > 0)
		{
			var leaf = {
				ctor: "_Array",
				height: 0,
				table: table.splice(0,i)
			};
			fromListPush(leaf, nodes);
		}

		// Go through all of the nodes and eventually push them into higher nodes.
		for (var h = 0; h < nodes.length - 1; h++)
		{
			if (nodes[h].table.length > 0)
			{
				fromListPush(nodes[h], nodes);
			}
		}

		var head = nodes[nodes.length - 1];
		if (head.height > 0 && head.table.length == 1)
		{
			return head.table[0];
		}
		else
		{
			return head;
		}
	}

	// Push a node into a higher node as a child.
	function fromListPush(toPush, nodes)
	{
		var h = toPush.height;

		// Maybe the node on this height does not exist.
		if (nodes.length == h)
		{
			var node = {
				ctor: "_Array",
				height: h + 1,
				table: new Array(),
				lengths: new Array()
			};
			nodes.push(node);
		}

		nodes[h].table.push(toPush);
		var len = length(toPush);
		if (nodes[h].lengths.length > 0)
		{
			len += nodes[h].lengths[nodes[h].lengths.length - 1];
		}
		nodes[h].lengths.push(len);

		if (nodes[h].table.length == M)
		{
			fromListPush(nodes[h], nodes);
			nodes[h] = {
				ctor: "_Array",
				height: h + 1,
				table: new Array(),
				lengths: new Array()
			};
		}
	}

	// Pushes an item via push_ to the bottom right of a tree.
	function push(item, a)
	{
		var pushed = push_(item, a);
		if (pushed !== null)
		{
			return pushed;
		}

		var newTree = create(item, a.height);
		return siblise(a, newTree);
	}

	// Recursively tries to push an item to the bottom-right most
	// tree possible. If there is no space left for the item,
	// null will be returned.
	function push_(item, a)
	{
		// Handle resursion stop at leaf level.
		if (a.height == 0)
		{
			if (a.table.length < M)
			{
				var newA = {
					ctor: "_Array",
					height: 0,
					table: a.table.slice()
				};
				newA.table.push(item);
				return newA;
			}
			else
			{
			  return null;
			}
		}

		// Recursively push
		var pushed = push_(item, botRight(a));

		// There was space in the bottom right tree, so the slot will
		// be updated.
		if (pushed != null)
		{
			var newA = nodeCopy(a);
			newA.table[newA.table.length - 1] = pushed;
			newA.lengths[newA.lengths.length - 1]++;
			return newA;
		}

		// When there was no space left, check if there is space left
		// for a new slot with a tree which contains only the item
		// at the bottom.
		if (a.table.length < M)
		{
			var newSlot = create(item, a.height - 1);
			var newA = nodeCopy(a);
			newA.table.push(newSlot);
			newA.lengths.push(newA.lengths[newA.lengths.length - 1] + length(newSlot));
			return newA;
		}
		else
		{
			return null;
		}
	}

	// Converts an array into a list of elements.
	function toList(a)
	{
		return toList_(List.Nil, a);
	}

	function toList_(list, a)
	{
		for (var i = a.table.length - 1; i >= 0; i--)
		{
			list =
				a.height == 0
					? List.Cons(a.table[i], list)
					: toList_(list, a.table[i]);
		}
		return list;
	}

	// Maps a function over the elements of an array.
	function map(f, a)
	{
		var newA = {
			ctor: "_Array",
			height: a.height,
			table: new Array(a.table.length)
		};
		if (a.height > 0)
		{
			newA.lengths = a.lengths;
		}
		for (var i = 0; i < a.table.length; i++)
		{
			newA.table[i] =
				a.height == 0
					? f(a.table[i])
					: map(f, a.table[i]);
		}
		return newA;
	}

	// Maps a function over the elements with their index as first argument.
	function indexedMap(f, a)
	{
		return indexedMap_(f, a, 0);
	}

	function indexedMap_(f, a, from)
	{
		var newA = {
			ctor: "_Array",
			height: a.height,
			table: new Array(a.table.length)
		};
		if (a.height > 0)
		{
			newA.lengths = a.lengths;
		}
		for (var i = 0; i < a.table.length; i++)
		{
			newA.table[i] =
				a.height == 0
					? A2(f, from + i, a.table[i])
					: indexedMap_(f, a.table[i], i == 0 ? 0 : a.lengths[i - 1]);
		}
		return newA;
	}

	function foldl(f, b, a)
	{
		if (a.height == 0)
		{
			for (var i = 0; i < a.table.length; i++)
			{
				b = A2(f, a.table[i], b);
			}
		}
		else
		{
			for (var i = 0; i < a.table.length; i++)
			{
				b = foldl(f, b, a.table[i]);
			}
		}
		return b;
	}

	function foldr(f, b, a)
	{
		if (a.height == 0)
		{
			for (var i = a.table.length; i--; )
			{
				b = A2(f, a.table[i], b);
			}
		}
		else
		{
			for (var i = a.table.length; i--; )
			{
				b = foldr(f, b, a.table[i]);
			}
		}
		return b;
	}

	// TODO: currently, it slices the right, then the left. This can be
	// optimized.
	function slice(from, to, a)
	{
		if (from < 0)
		{
			from += length(a);
		}
		if (to < 0)
		{
			to += length(a);
		}
		return sliceLeft(from, sliceRight(to, a));
	}

	function sliceRight(to, a)
	{
		if (to == length(a))
		{
			return a;
		}

		// Handle leaf level.
		if (a.height == 0)
		{
			var newA = { ctor:"_Array", height:0 };
			newA.table = a.table.slice(0, to);
			return newA;
		}

		// Slice the right recursively.
		var right = getSlot(to, a);
		var sliced = sliceRight(to - (right > 0 ? a.lengths[right - 1] : 0), a.table[right]);

		// Maybe the a node is not even needed, as sliced contains the whole slice.
		if (right == 0)
		{
			return sliced;
		}

		// Create new node.
		var newA = {
			ctor: "_Array",
			height: a.height,
			table: a.table.slice(0, right),
			lengths: a.lengths.slice(0, right)
		};
		if (sliced.table.length > 0)
		{
			newA.table[right] = sliced;
			newA.lengths[right] = length(sliced) + (right > 0 ? newA.lengths[right - 1] : 0);
		}
		return newA;
	}

	function sliceLeft(from, a)
	{
		if (from == 0)
		{
			return a;
		}

		// Handle leaf level.
		if (a.height == 0)
		{
			var newA = { ctor:"_Array", height:0 };
			newA.table = a.table.slice(from, a.table.length + 1);
			return newA;
		}

		// Slice the left recursively.
		var left = getSlot(from, a);
		var sliced = sliceLeft(from - (left > 0 ? a.lengths[left - 1] : 0), a.table[left]);

		// Maybe the a node is not even needed, as sliced contains the whole slice.
		if (left == a.table.length - 1)
		{
			return sliced;
		}

		// Create new node.
		var newA = {
			ctor: "_Array",
			height: a.height,
			table: a.table.slice(left, a.table.length + 1),
			lengths: new Array(a.table.length - left)
		};
		newA.table[0] = sliced;
		var len = 0;
		for (var i = 0; i < newA.table.length; i++)
		{
			len += length(newA.table[i]);
			newA.lengths[i] = len;
		}

		return newA;
	}

	// Appends two trees.
	function append(a,b)
	{
		if (a.table.length === 0)
		{
			return b;
		}
		if (b.table.length === 0)
		{
			return a;
		}

		var c = append_(a, b);

		// Check if both nodes can be crunshed together.
		if (c[0].table.length + c[1].table.length <= M)
		{
			if (c[0].table.length === 0)
			{
				return c[1];
			}
			if (c[1].table.length === 0)
			{
				return c[0];
			}

			// Adjust .table and .lengths
			c[0].table = c[0].table.concat(c[1].table);
			if (c[0].height > 0)
			{
				var len = length(c[0]);
				for (var i = 0; i < c[1].lengths.length; i++)
				{
					c[1].lengths[i] += len;
				}
				c[0].lengths = c[0].lengths.concat(c[1].lengths);
			}

			return c[0];
		}

		if (c[0].height > 0)
		{
			var toRemove = calcToRemove(a, b);
			if (toRemove > E)
			{
				c = shuffle(c[0], c[1], toRemove);
			}
		}

		return siblise(c[0], c[1]);
	}

	// Returns an array of two nodes; right and left. One node _may_ be empty.
	function append_(a, b)
	{
		if (a.height === 0 && b.height === 0)
		{
			return [a, b];
		}

		if (a.height !== 1 || b.height !== 1)
		{
			if (a.height === b.height)
			{
				a = nodeCopy(a);
				b = nodeCopy(b);
				var appended = append_(botRight(a), botLeft(b));

				insertRight(a, appended[1]);
				insertLeft(b, appended[0]);
			}
			else if (a.height > b.height)
			{
				a = nodeCopy(a);
				var appended = append_(botRight(a), b);

				insertRight(a, appended[0]);
				b = parentise(appended[1], appended[1].height + 1);
			}
			else
			{
				b = nodeCopy(b);
				var appended = append_(a, botLeft(b));

				var left = appended[0].table.length === 0 ? 0 : 1;
				var right = left === 0 ? 1 : 0;
				insertLeft(b, appended[left]);
				a = parentise(appended[right], appended[right].height + 1);
			}
		}

		// Check if balancing is needed and return based on that.
		if (a.table.length === 0 || b.table.length === 0)
		{
			return [a,b];
		}

		var toRemove = calcToRemove(a, b);
		if (toRemove <= E)
		{
			return [a,b];
		}
		return shuffle(a, b, toRemove);
	}

	// Helperfunctions for append_. Replaces a child node at the side of the parent.
	function insertRight(parent, node)
	{
		var index = parent.table.length - 1;
		parent.table[index] = node;
		parent.lengths[index] = length(node)
		parent.lengths[index] += index > 0 ? parent.lengths[index - 1] : 0;
	}

	function insertLeft(parent, node)
	{
		if (node.table.length > 0)
		{
			parent.table[0] = node;
			parent.lengths[0] = length(node);

			var len = length(parent.table[0]);
			for (var i = 1; i < parent.lengths.length; i++)
			{
				len += length(parent.table[i]);
				parent.lengths[i] = len;
			}
		}
		else
		{
			parent.table.shift();
			for (var i = 1; i < parent.lengths.length; i++)
			{
				parent.lengths[i] = parent.lengths[i] - parent.lengths[0];
			}
			parent.lengths.shift();
		}
	}

	// Returns the extra search steps for E. Refer to the paper.
	function calcToRemove(a, b)
	{
		var subLengths = 0;
		for (var i = 0; i < a.table.length; i++)
		{
			subLengths += a.table[i].table.length;
		}
		for (var i = 0; i < b.table.length; i++)
		{
			subLengths += b.table[i].table.length;
		}

		var toRemove = a.table.length + b.table.length
		return toRemove - (Math.floor((subLengths - 1) / M) + 1);
	}

	// get2, set2 and saveSlot are helpers for accessing elements over two arrays.
	function get2(a, b, index)
	{
		return index < a.length
			? a[index]
			: b[index - a.length];
	}

	function set2(a, b, index, value)
	{
		if (index < a.length)
		{
			a[index] = value;
		}
		else
		{
			b[index - a.length] = value;
		}
	}

	function saveSlot(a, b, index, slot)
	{
		set2(a.table, b.table, index, slot);

		var l = (index == 0 || index == a.lengths.length)
			? 0
			: get2(a.lengths, a.lengths, index - 1);

		set2(a.lengths, b.lengths, index, l + length(slot));
	}

	// Creates a node or leaf with a given length at their arrays for perfomance.
	// Is only used by shuffle.
	function createNode(h, length)
	{
		if (length < 0)
		{
			length = 0;
		}
		var a = {
			ctor: "_Array",
			height: h,
			table: new Array(length)
		};
		if (h > 0)
		{
			a.lengths = new Array(length);
		}
		return a;
	}

	// Returns an array of two balanced nodes.
	function shuffle(a, b, toRemove)
	{
		var newA = createNode(a.height, Math.min(M, a.table.length + b.table.length - toRemove));
		var newB = createNode(a.height, newA.table.length - (a.table.length + b.table.length - toRemove));

		// Skip the slots with size M. More precise: copy the slot references
		// to the new node
		var read = 0;
		while (get2(a.table, b.table, read).table.length % M == 0)
		{
			set2(newA.table, newB.table, read, get2(a.table, b.table, read));
			set2(newA.lengths, newB.lengths, read, get2(a.lengths, b.lengths, read));
			read++;
		}

		// Pulling items from left to right, caching in a slot before writing
		// it into the new nodes.
		var write = read;
		var slot = new createNode(a.height - 1, 0);
		var from = 0;

		// If the current slot is still containing data, then there will be at
		// least one more write, so we do not break this loop yet.
		while (read - write - (slot.table.length > 0 ? 1 : 0) < toRemove)
		{
			// Find out the max possible items for copying.
			var source = get2(a.table, b.table, read);
			var to = Math.min(M - slot.table.length, source.table.length)

			// Copy and adjust size table.
			slot.table = slot.table.concat(source.table.slice(from, to));
			if (slot.height > 0)
			{
				var len = slot.lengths.length;
				for (var i = len; i < len + to - from; i++)
				{
					slot.lengths[i] = length(slot.table[i]);
					slot.lengths[i] += (i > 0 ? slot.lengths[i - 1] : 0);
				}
			}

			from += to;

			// Only proceed to next slots[i] if the current one was
			// fully copied.
			if (source.table.length <= to)
			{
				read++; from = 0;
			}

			// Only create a new slot if the current one is filled up.
			if (slot.table.length == M)
			{
				saveSlot(newA, newB, write, slot);
				slot = createNode(a.height - 1,0);
				write++;
			}
		}

		// Cleanup after the loop. Copy the last slot into the new nodes.
		if (slot.table.length > 0)
		{
			saveSlot(newA, newB, write, slot);
			write++;
		}

		// Shift the untouched slots to the left
		while (read < a.table.length + b.table.length )
		{
			saveSlot(newA, newB, write, get2(a.table, b.table, read));
			read++;
			write++;
		}

		return [newA, newB];
	}

	// Navigation functions
	function botRight(a)
	{
		return a.table[a.table.length - 1];
	}
	function botLeft(a)
	{
		return a.table[0];
	}

	// Copies a node for updating. Note that you should not use this if
	// only updating only one of "table" or "lengths" for performance reasons.
	function nodeCopy(a)
	{
		var newA = {
			ctor: "_Array",
			height: a.height,
			table: a.table.slice()
		};
		if (a.height > 0)
		{
			newA.lengths = a.lengths.slice();
		}
		return newA;
	}

	// Returns how many items are in the tree.
	function length(array)
	{
		if (array.height == 0)
		{
			return array.table.length;
		}
		else
		{
			return array.lengths[array.lengths.length - 1];
		}
	}

	// Calculates in which slot of "table" the item probably is, then
	// find the exact slot via forward searching in  "lengths". Returns the index.
	function getSlot(i, a)
	{
		var slot = i >> (5 * a.height);
		while (a.lengths[slot] <= i)
		{
			slot++;
		}
		return slot;
	}

	// Recursively creates a tree with a given height containing
	// only the given item.
	function create(item, h)
	{
		if (h == 0)
		{
			return {
				ctor: "_Array",
				height: 0,
				table: [item]
			};
		}
		return {
			ctor: "_Array",
			height: h,
			table: [create(item, h - 1)],
			lengths: [1]
		};
	}

	// Recursively creates a tree that contains the given tree.
	function parentise(tree, h)
	{
		if (h == tree.height)
		{
			return tree;
		}

		return {
			ctor: "_Array",
			height: h,
			table: [parentise(tree, h - 1)],
			lengths: [length(tree)]
		};
	}

	// Emphasizes blood brotherhood beneath two trees.
	function siblise(a, b)
	{
		return {
			ctor: "_Array",
			height: a.height + 1,
			table: [a, b],
			lengths: [length(a), length(a) + length(b)]
		};
	}

	function toJSArray(a)
	{
		var jsArray = new Array(length(a));
		toJSArray_(jsArray, 0, a);
		return jsArray;
	}

	function toJSArray_(jsArray, i, a)
	{
		for (var t = 0; t < a.table.length; t++)
		{
			if (a.height == 0)
			{
				jsArray[i + t] = a.table[t];
			}
			else
			{
				var inc = t == 0 ? 0 : a.lengths[t - 1];
				toJSArray_(jsArray, i + inc, a.table[t]);
			}
		}
	}

	function fromJSArray(jsArray)
	{
		if (jsArray.length == 0)
		{
			return empty;
		}
		var h = Math.floor(Math.log(jsArray.length) / Math.log(M));
		return fromJSArray_(jsArray, h, 0, jsArray.length);
	}

	function fromJSArray_(jsArray, h, from, to)
	{
		if (h == 0)
		{
			return {
				ctor: "_Array",
				height: 0,
				table: jsArray.slice(from, to)
			};
		}

		var step = Math.pow(M, h);
		var table = new Array(Math.ceil((to - from) / step));
		var lengths = new Array(table.length);
		for (var i = 0; i < table.length; i++)
		{
			table[i] = fromJSArray_(jsArray, h - 1, from + (i * step), Math.min(from + ((i + 1) * step), to));
			lengths[i] = length(table[i]) + (i > 0 ? lengths[i-1] : 0);
		}
		return {
			ctor: "_Array",
			height: h,
			table: table,
			lengths: lengths
		};
	}

	Elm.Native.Array.values = {
		empty: empty,
		fromList: fromList,
		toList: toList,
		initialize: F2(initialize),
		append: F2(append),
		push: F2(push),
		slice: F3(slice),
		get: F2(get),
		set: F3(set),
		map: F2(map),
		indexedMap: F2(indexedMap),
		foldl: F3(foldl),
		foldr: F3(foldr),
		length: length,

		toJSArray:toJSArray,
		fromJSArray:fromJSArray
	};

	return localRuntime.Native.Array.values = Elm.Native.Array.values;

}

Elm.Native.Basics = {};
Elm.Native.Basics.make = function(localRuntime) {

	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Basics = localRuntime.Native.Basics || {};
	if (localRuntime.Native.Basics.values)
	{
		return localRuntime.Native.Basics.values;
	}

	var Utils = Elm.Native.Utils.make(localRuntime);

	function div(a, b)
	{
		return (a/b)|0;
	}
	function rem(a, b)
	{
		return a % b;
	}
	function mod(a, b)
	{
		if (b === 0)
		{
			throw new Error("Cannot perform mod 0. Division by zero error.");
		}
		var r = a % b;
		var m = a === 0 ? 0 : (b > 0 ? (a >= 0 ? r : r+b) : -mod(-a,-b));

		return m === b ? 0 : m;
	}
	function logBase(base, n)
	{
		return Math.log(n) / Math.log(base);
	}
	function negate(n)
	{
		return -n;
	}
	function abs(n)
	{
		return n < 0 ? -n : n;
	}

	function min(a, b)
	{
		return Utils.cmp(a,b) < 0 ? a : b;
	}
	function max(a, b)
	{
		return Utils.cmp(a,b) > 0 ? a : b;
	}
	function clamp(lo, hi, n)
	{
		return Utils.cmp(n,lo) < 0 ? lo : Utils.cmp(n,hi) > 0 ? hi : n;
	}

	function xor(a, b)
	{
		return a !== b;
	}
	function not(b)
	{
		return !b;
	}
	function isInfinite(n)
	{
		return n === Infinity || n === -Infinity
	}

	function truncate(n)
	{
		return n|0;
	}

	function degrees(d)
	{
		return d * Math.PI / 180;
	}
	function turns(t)
	{
		return 2 * Math.PI * t;
	}
	function fromPolar(point)
	{
		var r = point._0;
		var t = point._1;
		return Utils.Tuple2(r * Math.cos(t), r * Math.sin(t));
	}
	function toPolar(point)
	{
		var x = point._0;
		var y = point._1;
		return Utils.Tuple2(Math.sqrt(x * x + y * y), Math.atan2(y,x));
	}

	return localRuntime.Native.Basics.values = {
		div: F2(div),
		rem: F2(rem),
		mod: F2(mod),

		pi: Math.PI,
		e: Math.E,
		cos: Math.cos,
		sin: Math.sin,
		tan: Math.tan,
		acos: Math.acos,
		asin: Math.asin,
		atan: Math.atan,
		atan2: F2(Math.atan2),

		degrees:  degrees,
		turns:  turns,
		fromPolar:  fromPolar,
		toPolar:  toPolar,

		sqrt: Math.sqrt,
		logBase: F2(logBase),
		negate: negate,
		abs: abs,
		min: F2(min),
		max: F2(max),
		clamp: F3(clamp),
		compare: Utils.compare,

		xor: F2(xor),
		not: not,

		truncate: truncate,
		ceiling: Math.ceil,
		floor: Math.floor,
		round: Math.round,
		toFloat: function(x) { return x; },
		isNaN: isNaN,
		isInfinite: isInfinite
	};
};

Elm.Native.Char = {};
Elm.Native.Char.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Char = localRuntime.Native.Char || {};
	if (localRuntime.Native.Char.values)
	{
		return localRuntime.Native.Char.values;
	}

	var Utils = Elm.Native.Utils.make(localRuntime);

	return localRuntime.Native.Char.values = {
		fromCode : function(c) { return Utils.chr(String.fromCharCode(c)); },
		toCode   : function(c) { return c.charCodeAt(0); },
		toUpper  : function(c) { return Utils.chr(c.toUpperCase()); },
		toLower  : function(c) { return Utils.chr(c.toLowerCase()); },
		toLocaleUpper : function(c) { return Utils.chr(c.toLocaleUpperCase()); },
		toLocaleLower : function(c) { return Utils.chr(c.toLocaleLowerCase()); },
	};
};

Elm.Native.Color = {};
Elm.Native.Color.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Color = localRuntime.Native.Color || {};
	if (localRuntime.Native.Color.values)
	{
		return localRuntime.Native.Color.values;
	}

	function toCss(c)
	{
		var format = '';
		var colors = '';
		if (c.ctor === 'RGBA')
		{
			format = 'rgb';
			colors = c._0 + ', ' + c._1 + ', ' + c._2;
		}
		else
		{
			format = 'hsl';
			colors = (c._0 * 180 / Math.PI) + ', ' +
					 (c._1 * 100) + '%, ' +
					 (c._2 * 100) + '%';
		}
		if (c._3 === 1)
		{
			return format + '(' + colors + ')';
		}
		else
		{
			return format + 'a(' + colors + ', ' + c._3 + ')';
		}
	}

	return localRuntime.Native.Color.values = {
		toCss: toCss
	};

};

Elm.Native.Debug = {};
Elm.Native.Debug.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Debug = localRuntime.Native.Debug || {};
	if (localRuntime.Native.Debug.values)
	{
		return localRuntime.Native.Debug.values;
	}

	var toString = Elm.Native.Show.make(localRuntime).toString;

	function log(tag, value)
	{
		var msg = tag + ': ' + toString(value);
		var process = process || {};
		if (process.stdout)
		{
			process.stdout.write(msg);
		}
		else
		{
			console.log(msg);
		}
		return value;
	}

	function crash(message)
	{
		throw new Error(message);
	}

	function tracePath(tag, form)
	{
		if (localRuntime.debug)
		{
			return localRuntime.debug.trace(tag, form);
		}
		return form;
	}

	function watch(tag, value)
	{
		if (localRuntime.debug)
		{
			localRuntime.debug.watch(tag, value);
		}
		return value;
	}

	function watchSummary(tag, summarize, value)
	{
		if (localRuntime.debug)
		{
			localRuntime.debug.watch(tag, summarize(value));
		}
		return value;
	}

	return localRuntime.Native.Debug.values = {
		crash: crash,
		tracePath: F2(tracePath),
		log: F2(log),
		watch: F2(watch),
		watchSummary:F3(watchSummary),
	};
};


// setup
Elm.Native = Elm.Native || {};
Elm.Native.Graphics = Elm.Native.Graphics || {};
Elm.Native.Graphics.Collage = Elm.Native.Graphics.Collage || {};

// definition
Elm.Native.Graphics.Collage.make = function(localRuntime) {
	'use strict';

	// attempt to short-circuit
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Graphics = localRuntime.Native.Graphics || {};
	localRuntime.Native.Graphics.Collage = localRuntime.Native.Graphics.Collage || {};
	if ('values' in localRuntime.Native.Graphics.Collage)
	{
		return localRuntime.Native.Graphics.Collage.values;
	}

	// okay, we cannot short-ciruit, so now we define everything
	var Color = Elm.Native.Color.make(localRuntime);
	var List = Elm.Native.List.make(localRuntime);
	var NativeElement = Elm.Native.Graphics.Element.make(localRuntime);
	var Transform = Elm.Transform2D.make(localRuntime);
	var Utils = Elm.Native.Utils.make(localRuntime);

	function setStrokeStyle(ctx, style)
	{
		ctx.lineWidth = style.width;

		var cap = style.cap.ctor;
		ctx.lineCap = cap === 'Flat'
			? 'butt'
			: cap === 'Round'
				? 'round'
				: 'square';

		var join = style.join.ctor;
		ctx.lineJoin = join === 'Smooth'
			? 'round'
			: join === 'Sharp'
				? 'miter'
				: 'bevel';

		ctx.miterLimit = style.join._0 || 10;
		ctx.strokeStyle = Color.toCss(style.color);
	}

	function setFillStyle(ctx, style)
	{
		var sty = style.ctor;
		ctx.fillStyle = sty === 'Solid'
			? Color.toCss(style._0)
			: sty === 'Texture'
				? texture(redo, ctx, style._0)
				: gradient(ctx, style._0);
	}

	function trace(ctx, path)
	{
		var points = List.toArray(path);
		var i = points.length - 1;
		if (i <= 0)
		{
			return;
		}
		ctx.moveTo(points[i]._0, points[i]._1);
		while (i--)
		{
			ctx.lineTo(points[i]._0, points[i]._1);
		}
		if (path.closed)
		{
			i = points.length - 1;
			ctx.lineTo(points[i]._0, points[i]._1);
		}
	}

	function line(ctx,style,path)
	{
		(style.dashing.ctor === '[]')
			? trace(ctx, path)
			: customLineHelp(ctx, style, path);
		ctx.scale(1,-1);
		ctx.stroke();
	}

	function customLineHelp(ctx, style, path)
	{
		var points = List.toArray(path);
		if (path.closed)
		{
			points.push(points[0]);
		}
		var pattern = List.toArray(style.dashing);
		var i = points.length - 1;
		if (i <= 0)
		{
			return;
		}
		var x0 = points[i]._0, y0 = points[i]._1;
		var x1=0, y1=0, dx=0, dy=0, remaining=0, nx=0, ny=0;
		var pindex = 0, plen = pattern.length;
		var draw = true, segmentLength = pattern[0];
		ctx.moveTo(x0,y0);
		while (i--)
		{
			x1 = points[i]._0;
			y1 = points[i]._1;
			dx = x1 - x0;
			dy = y1 - y0;
			remaining = Math.sqrt(dx * dx + dy * dy);
			while (segmentLength <= remaining)
			{
				x0 += dx * segmentLength / remaining;
				y0 += dy * segmentLength / remaining;
				ctx[draw ? 'lineTo' : 'moveTo'](x0, y0);
				// update starting position
				dx = x1 - x0;
				dy = y1 - y0;
				remaining = Math.sqrt(dx * dx + dy * dy);
				// update pattern
				draw = !draw;
				pindex = (pindex + 1) % plen;
				segmentLength = pattern[pindex];
			}
			if (remaining > 0)
			{
				ctx[draw ? 'lineTo' : 'moveTo'](x1, y1);
				segmentLength -= remaining;
			}
			x0 = x1;
			y0 = y1;
		}
	}

	function drawLine(ctx, style, path)
	{
		setStrokeStyle(ctx, style);
		return line(ctx, style, path);
	}

	function texture(redo, ctx, src)
	{
		var img = new Image();
		img.src = src;
		img.onload = redo;
		return ctx.createPattern(img, 'repeat');
	}

	function gradient(ctx, grad)
	{
		var g;
		var stops = [];
		if (grad.ctor === 'Linear')
		{
			var p0 = grad._0, p1 = grad._1;
			g = ctx.createLinearGradient(p0._0, -p0._1, p1._0, -p1._1);
			stops = List.toArray(grad._2);
		}
		else
		{
			var p0 = grad._0, p2 = grad._2;
			g = ctx.createRadialGradient(p0._0, -p0._1, grad._1, p2._0, -p2._1, grad._3);
			stops = List.toArray(grad._4);
		}
		var len = stops.length;
		for (var i = 0; i < len; ++i)
		{
			var stop = stops[i];
			g.addColorStop(stop._0, Color.toCss(stop._1));
		}
		return g;
	}

	function drawShape(redo, ctx, style, path)
	{
		trace(ctx, path);
		setFillStyle(ctx, style);
		ctx.scale(1,-1);
		ctx.fill();
	}


	// TEXT RENDERING

	function fillText(redo, ctx, text)
	{
		drawText(ctx, text, ctx.fillText);
	}

	function strokeText(redo, ctx, style, text)
	{
		setStrokeStyle(ctx, style);
		// Use native canvas API for dashes only for text for now
		// Degrades to non-dashed on IE 9 + 10
		if (style.dashing.ctor !== '[]' && ctx.setLineDash)
		{
			var pattern = List.toArray(style.dashing);
			ctx.setLineDash(pattern);
		}
		drawText(ctx, text, ctx.strokeText);
	}

	function drawText(ctx, text, canvasDrawFn)
	{
		var textChunks = chunkText(defaultContext, text);

		var totalWidth = 0;
		var maxHeight = 0;
		var numChunks = textChunks.length;

		ctx.scale(1,-1);

		for (var i = numChunks; i--; )
		{
			var chunk = textChunks[i];
			ctx.font = chunk.font;
			var metrics = ctx.measureText(chunk.text);
			chunk.width = metrics.width;
			totalWidth += chunk.width;
			if (chunk.height > maxHeight)
			{
				maxHeight = chunk.height;
			}
		}

		var x = -totalWidth / 2.0;
		for (var i = 0; i < numChunks; ++i)
		{
			var chunk = textChunks[i];
			ctx.font = chunk.font;
			ctx.fillStyle = chunk.color;
			canvasDrawFn.call(ctx, chunk.text, x, maxHeight / 2);
			x += chunk.width;
		}
	}

	function toFont(props)
	{
		return [
			props['font-style'],
			props['font-variant'],
			props['font-weight'],
			props['font-size'],
			props['font-family']
		].join(' ');
	}


	// Convert the object returned by the text module
	// into something we can use for styling canvas text
	function chunkText(context, text)
	{
		var tag = text.ctor;
		if (tag === 'Text:Append')
		{
			var leftChunks = chunkText(context, text._0);
			var rightChunks = chunkText(context, text._1);
			return leftChunks.concat(rightChunks);
		}
		if (tag === 'Text:Text')
		{
			return [{
				text: text._0,
				color: context.color,
				height: context['font-size'].slice(0,-2) | 0,
				font: toFont(context)
			}];
		}
		if (tag === 'Text:Meta')
		{
			var newContext = freshContext(text._0, context);
			return chunkText(newContext, text._1);
		}
	}

	function freshContext(props, ctx)
	{
		return {
			'font-style': props['font-style'] || ctx['font-style'],
			'font-variant': props['font-variant'] || ctx['font-variant'],
			'font-weight': props['font-weight'] || ctx['font-weight'],
			'font-size': props['font-size'] || ctx['font-size'],
			'font-family': props['font-family'] || ctx['font-family'],
			'color': props['color'] || ctx['color']
		};
	}

	var defaultContext = {
		'font-style': 'normal',
		'font-variant': 'normal',
		'font-weight': 'normal',
		'font-size': '12px',
		'font-family': 'sans-serif',
		'color': 'black'
	};


	// IMAGES

	function drawImage(redo, ctx, form)
	{
		var img = new Image();
		img.onload = redo;
		img.src = form._3;
		var w = form._0,
			h = form._1,
			pos = form._2,
			srcX = pos._0,
			srcY = pos._1,
			srcW = w,
			srcH = h,
			destX = -w/2,
			destY = -h/2,
			destW = w,
			destH = h;

		ctx.scale(1,-1);
		ctx.drawImage(img, srcX, srcY, srcW, srcH, destX, destY, destW, destH);
	}

	function renderForm(redo, ctx, form)
	{
		ctx.save();

		var x = form.x,
			y = form.y,
			theta = form.theta,
			scale = form.scale;

		if (x !== 0 || y !== 0)
		{
			ctx.translate(x, y);
		}
		if (theta !== 0)
		{
			ctx.rotate(theta);
		}
		if (scale !== 1)
		{
			ctx.scale(scale,scale);
		}
		if (form.alpha !== 1)
		{
			ctx.globalAlpha = ctx.globalAlpha * form.alpha;
		}

		ctx.beginPath();
		var f = form.form;
		switch (f.ctor)
		{
			case 'FPath':
				drawLine(ctx, f._0, f._1);
				break;

			case 'FImage':
				drawImage(redo, ctx, f);
				break;

			case 'FShape':
				if (f._0.ctor === 'Line')
				{
					f._1.closed = true;
					drawLine(ctx, f._0._0, f._1);
				}
				else
				{
					drawShape(redo, ctx, f._0._0, f._1);
				}
				break;

			case 'FText':
				fillText(redo, ctx, f._0);
				break;

			case 'FOutlinedText':
				strokeText(redo, ctx, f._0, f._1);
				break;
		}
		ctx.restore();
	}

	function formToMatrix(form)
	{
	   var scale = form.scale;
	   var matrix = A6( Transform.matrix, scale, 0, 0, scale, form.x, form.y );

	   var theta = form.theta
	   if (theta !== 0)
	   {
		   matrix = A2( Transform.multiply, matrix, Transform.rotation(theta) );
	   }

	   return matrix;
	}

	function str(n)
	{
		if (n < 0.00001 && n > -0.00001)
		{
			return 0;
		}
		return n;
	}

	function makeTransform(w, h, form, matrices)
	{
		var props = form.form._0.props;
		var m = A6( Transform.matrix, 1, 0, 0, -1,
					(w - props.width ) / 2,
					(h - props.height) / 2 );
		var len = matrices.length;
		for (var i = 0; i < len; ++i)
		{
			m = A2( Transform.multiply, m, matrices[i] );
		}
		m = A2( Transform.multiply, m, formToMatrix(form) );

		return 'matrix(' +
			str( m[0]) + ', ' + str( m[3]) + ', ' +
			str(-m[1]) + ', ' + str(-m[4]) + ', ' +
			str( m[2]) + ', ' + str( m[5]) + ')';
	}

	function stepperHelp(list)
	{
		var arr = List.toArray(list);
		var i = 0;
		function peekNext()
		{
			return i < arr.length ? arr[i].form.ctor : '';
		}
		// assumes that there is a next element
		function next()
		{
			var out = arr[i];
			++i;
			return out;
		}
		return {
			peekNext: peekNext,
			next: next
		};
	}

	function formStepper(forms)
	{
		var ps = [stepperHelp(forms)];
		var matrices = [];
		var alphas = [];
		function peekNext()
		{
			var len = ps.length;
			var formType = '';
			for (var i = 0; i < len; ++i )
			{
				if (formType = ps[i].peekNext()) return formType;
			}
			return '';
		}
		// assumes that there is a next element
		function next(ctx)
		{
			while (!ps[0].peekNext())
			{
				ps.shift();
				matrices.pop();
				alphas.shift();
				if (ctx)
				{
					ctx.restore();
				}
			}
			var out = ps[0].next();
			var f = out.form;
			if (f.ctor === 'FGroup')
			{
				ps.unshift(stepperHelp(f._1));
				var m = A2(Transform.multiply, f._0, formToMatrix(out));
				ctx.save();
				ctx.transform(m[0], m[3], m[1], m[4], m[2], m[5]);
				matrices.push(m);

				var alpha = (alphas[0] || 1) * out.alpha;
				alphas.unshift(alpha);
				ctx.globalAlpha = alpha;
			}
			return out;
		}
		function transforms()
		{
			return matrices;
		}
		function alpha()
		{
			return alphas[0] || 1;
		}
		return {
			peekNext: peekNext,
			next: next,
			transforms: transforms,
			alpha: alpha
		};
	}

	function makeCanvas(w,h)
	{
		var canvas = NativeElement.createNode('canvas');
		canvas.style.width  = w + 'px';
		canvas.style.height = h + 'px';
		canvas.style.display = "block";
		canvas.style.position = "absolute";
		var ratio = window.devicePixelRatio || 1;
		canvas.width  = w * ratio;
		canvas.height = h * ratio;
		return canvas;
	}

	function render(model)
	{
		var div = NativeElement.createNode('div');
		div.style.overflow = 'hidden';
		div.style.position = 'relative';
		update(div, model, model);
		return div;
	}

	function nodeStepper(w,h,div)
	{
		var kids = div.childNodes;
		var i = 0;
		var ratio = window.devicePixelRatio || 1;

		function transform(transforms, ctx)
		{
			ctx.translate( w / 2 * ratio, h / 2 * ratio );
			ctx.scale( ratio, -ratio );
			var len = transforms.length;
			for (var i = 0; i < len; ++i)
			{
				var m = transforms[i];
				ctx.save();
				ctx.transform(m[0], m[3], m[1], m[4], m[2], m[5]);
			}
			return ctx;
		}
		function nextContext(transforms)
		{
			while (i < kids.length)
			{
				var node = kids[i];
				if (node.getContext)
				{
					node.width = w * ratio;
					node.height = h * ratio;
					node.style.width = w + 'px';
					node.style.height = h + 'px';
					++i;
					return transform(transforms, node.getContext('2d'));
				}
				div.removeChild(node);
			}
			var canvas = makeCanvas(w,h);
			div.appendChild(canvas);
			// we have added a new node, so we must step our position
			++i;
			return transform(transforms, canvas.getContext('2d'));
		}
		function addElement(matrices, alpha, form)
		{
			var kid = kids[i];
			var elem = form.form._0;

			var node = (!kid || kid.getContext)
				? NativeElement.render(elem)
				: NativeElement.update(kid, kid.oldElement, elem);

			node.style.position = 'absolute';
			node.style.opacity = alpha * form.alpha * elem.props.opacity;
			NativeElement.addTransform(node.style, makeTransform(w, h, form, matrices));
			node.oldElement = elem;
			++i;
			if (!kid)
			{
				div.appendChild(node);
			}
			else
			{
				div.insertBefore(node, kid);
			}
		}
		function clearRest()
		{
			while (i < kids.length)
			{
				div.removeChild(kids[i]);
			}
		}
		return {
			nextContext: nextContext,
			addElement: addElement,
			clearRest: clearRest
		};
	}


	function update(div, _, model)
	{
		var w = model.w;
		var h = model.h;

		var forms = formStepper(model.forms);
		var nodes = nodeStepper(w,h,div);
		var ctx = null;
		var formType = '';

		while (formType = forms.peekNext())
		{
			// make sure we have context if we need it
			if (ctx === null && formType !== 'FElement')
			{
				ctx = nodes.nextContext(forms.transforms());
				ctx.globalAlpha = forms.alpha();
			}

			var form = forms.next(ctx);
			// if it is FGroup, all updates are made within formStepper when next is called.
			if (formType === 'FElement')
			{
				// update or insert an element, get a new context
				nodes.addElement(forms.transforms(), forms.alpha(), form);
				ctx = null;
			}
			else if (formType !== 'FGroup')
			{
				renderForm(function() { update(div, model, model); }, ctx, form);
			}
		}
		nodes.clearRest();
		return div;
	}


	function collage(w,h,forms)
	{
		return A3(NativeElement.newElement, w, h, {
			ctor: 'Custom',
			type: 'Collage',
			render: render,
			update: update,
			model: {w:w, h:h, forms:forms}
		});
	}

	return localRuntime.Native.Graphics.Collage.values = {
		collage: F3(collage)
	};

};


// setup
Elm.Native = Elm.Native || {};
Elm.Native.Graphics = Elm.Native.Graphics || {};
Elm.Native.Graphics.Element = Elm.Native.Graphics.Element || {};

// definition
Elm.Native.Graphics.Element.make = function(localRuntime) {
	'use strict';

	// attempt to short-circuit
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Graphics = localRuntime.Native.Graphics || {};
	localRuntime.Native.Graphics.Element = localRuntime.Native.Graphics.Element || {};
	if ('values' in localRuntime.Native.Graphics.Element)
	{
		return localRuntime.Native.Graphics.Element.values;
	}

	var Color = Elm.Native.Color.make(localRuntime);
	var List = Elm.Native.List.make(localRuntime);
	var Maybe = Elm.Maybe.make(localRuntime);
	var Text = Elm.Native.Text.make(localRuntime);
	var Utils = Elm.Native.Utils.make(localRuntime);


	// CREATION

	function createNode(elementType)
	{
		var node = document.createElement(elementType);
		node.style.padding = "0";
		node.style.margin = "0";
		return node;
	}


	function newElement(width, height, elementPrim)
	{
		return {
			_: {},
			element: elementPrim,
			props: {
				_: {},
				id: Utils.guid(),
				width: width,
				height: height,
				opacity: 1,
				color: Maybe.Nothing,
				href: "",
				tag: "",
				hover: Utils.Tuple0,
				click: Utils.Tuple0
			}
		};
	}


	// PROPERTIES

	function setProps(elem, node)
	{
		var props = elem.props;

		var element = elem.element;
		var width = props.width - (element.adjustWidth || 0);
		var height = props.height - (element.adjustHeight || 0);
		node.style.width  = (width |0) + 'px';
		node.style.height = (height|0) + 'px';

		if (props.opacity !== 1)
		{
			node.style.opacity = props.opacity;
		}

		if (props.color.ctor === 'Just')
		{
			node.style.backgroundColor = Color.toCss(props.color._0);
		}

		if (props.tag !== '')
		{
			node.id = props.tag;
		}

		if (props.hover.ctor !== '_Tuple0')
		{
			addHover(node, props.hover);
		}

		if (props.click.ctor !== '_Tuple0')
		{
			addClick(node, props.click);
		}

		if (props.href !== '')
		{
			var anchor = createNode('a');
			anchor.href = props.href;
			anchor.style.display = 'block';
			anchor.style.pointerEvents = 'auto';
			anchor.appendChild(node);
			node = anchor;
		}

		return node;
	}

	function addClick(e, handler)
	{
		e.style.pointerEvents = 'auto';
		e.elm_click_handler = handler;
		function trigger(ev)
		{
			e.elm_click_handler(Utils.Tuple0);
			ev.stopPropagation();
		}
		e.elm_click_trigger = trigger;
		e.addEventListener('click', trigger);
	}

	function removeClick(e, handler)
	{
		if (e.elm_click_trigger)
		{
			e.removeEventListener('click', e.elm_click_trigger);
			e.elm_click_trigger = null;
			e.elm_click_handler = null;
		}
	}

	function addHover(e, handler)
	{
		e.style.pointerEvents = 'auto';
		e.elm_hover_handler = handler;
		e.elm_hover_count = 0;

		function over(evt)
		{
			if (e.elm_hover_count++ > 0) return;
			e.elm_hover_handler(true);
			evt.stopPropagation();
		}
		function out(evt)
		{
			if (e.contains(evt.toElement || evt.relatedTarget)) return;
			e.elm_hover_count = 0;
			e.elm_hover_handler(false);
			evt.stopPropagation();
		}
		e.elm_hover_over = over;
		e.elm_hover_out = out;
		e.addEventListener('mouseover', over);
		e.addEventListener('mouseout', out);
	}

	function removeHover(e)
	{
		e.elm_hover_handler = null;
		if (e.elm_hover_over)
		{
			e.removeEventListener('mouseover', e.elm_hover_over);
			e.elm_hover_over = null;
		}
		if (e.elm_hover_out)
		{
			e.removeEventListener('mouseout', e.elm_hover_out);
			e.elm_hover_out = null;
		}
	}


	// IMAGES

	function image(props, img)
	{
		switch (img._0.ctor)
		{
			case 'Plain':
				return plainImage(img._3);

			case 'Fitted':
				return fittedImage(props.width, props.height, img._3);

			case 'Cropped':
				return croppedImage(img,props.width,props.height,img._3);

			case 'Tiled':
				return tiledImage(img._3);
		}
	}

	function plainImage(src)
	{
		var img = createNode('img');
		img.src = src;
		img.name = src;
		img.style.display = "block";
		return img;
	}

	function tiledImage(src)
	{
		var div = createNode('div');
		div.style.backgroundImage = 'url(' + src + ')';
		return div;
	}

	function fittedImage(w, h, src)
	{
		var div = createNode('div');
		div.style.background = 'url(' + src + ') no-repeat center';
		div.style.webkitBackgroundSize = 'cover';
		div.style.MozBackgroundSize = 'cover';
		div.style.OBackgroundSize = 'cover';
		div.style.backgroundSize = 'cover';
		return div;
	}

	function croppedImage(elem, w, h, src)
	{
		var pos = elem._0._0;
		var e = createNode('div');
		e.style.overflow = "hidden";

		var img = createNode('img');
		img.onload = function() {
			var sw = w / elem._1, sh = h / elem._2;
			img.style.width = ((this.width * sw)|0) + 'px';
			img.style.height = ((this.height * sh)|0) + 'px';
			img.style.marginLeft = ((- pos._0 * sw)|0) + 'px';
			img.style.marginTop = ((- pos._1 * sh)|0) + 'px';
		};
		img.src = src;
		img.name = src;
		e.appendChild(img);
		return e;
	}


	// FLOW

	function goOut(node)
	{
		node.style.position = 'absolute';
		return node;
	}
	function goDown(node)
	{
		return node;
	}
	function goRight(node)
	{
		node.style.styleFloat = 'left';
		node.style.cssFloat = 'left';
		return node;
	}

	var directionTable = {
		DUp    : goDown,
		DDown  : goDown,
		DLeft  : goRight,
		DRight : goRight,
		DIn    : goOut,
		DOut   : goOut
	};
	function needsReversal(dir)
	{
		return dir == 'DUp' || dir == 'DLeft' || dir == 'DIn';
	}

	function flow(dir,elist)
	{
		var array = List.toArray(elist);
		var container = createNode('div');
		var goDir = directionTable[dir];
		if (goDir == goOut)
		{
			container.style.pointerEvents = 'none';
		}
		if (needsReversal(dir))
		{
			array.reverse();
		}
		var len = array.length;
		for (var i = 0; i < len; ++i)
		{
			container.appendChild(goDir(render(array[i])));
		}
		return container;
	}


	// CONTAINER

	function toPos(pos)
	{
		return pos.ctor === "Absolute"
			? pos._0 + "px"
			: (pos._0 * 100) + "%";
	}

	// must clear right, left, top, bottom, and transform
	// before calling this function
	function setPos(pos,elem,e)
	{
		var element = elem.element;
		var props = elem.props;
		var w = props.width + (element.adjustWidth ? element.adjustWidth : 0);
		var h = props.height + (element.adjustHeight ? element.adjustHeight : 0);

		e.style.position = 'absolute';
		e.style.margin = 'auto';
		var transform = '';

		switch (pos.horizontal.ctor)
		{
			case 'P':
				e.style.right = toPos(pos.x);
				e.style.removeProperty('left');
				break;

			case 'Z':
				transform = 'translateX(' + ((-w/2)|0) + 'px) ';

			case 'N':
				e.style.left = toPos(pos.x);
				e.style.removeProperty('right');
				break;
		}
		switch (pos.vertical.ctor)
		{
			case 'N':
				e.style.bottom = toPos(pos.y);
				e.style.removeProperty('top');
				break;

			case 'Z':
				transform += 'translateY(' + ((-h/2)|0) + 'px)';

			case 'P':
				e.style.top = toPos(pos.y);
				e.style.removeProperty('bottom');
				break;
		}
		if (transform !== '')
		{
			addTransform(e.style, transform);
		}
		return e;
	}

	function addTransform(style, transform)
	{
		style.transform       = transform;
		style.msTransform     = transform;
		style.MozTransform    = transform;
		style.webkitTransform = transform;
		style.OTransform      = transform;
	}

	function container(pos,elem)
	{
		var e = render(elem);
		setPos(pos, elem, e);
		var div = createNode('div');
		div.style.position = 'relative';
		div.style.overflow = 'hidden';
		div.appendChild(e);
		return div;
	}


	function rawHtml(elem)
	{
		var html = elem.html;
		var guid = elem.guid;
		var align = elem.align;

		var div = createNode('div');
		div.innerHTML = html;
		div.style.visibility = "hidden";
		if (align)
		{
			div.style.textAlign = align;
		}
		div.style.visibility = 'visible';
		div.style.pointerEvents = 'auto';
		return div;
	}


	// RENDER

	function render(elem)
	{
		return setProps(elem, makeElement(elem));
	}
	function makeElement(e)
	{
		var elem = e.element;
		switch(elem.ctor)
		{
			case 'Image':
				return image(e.props, elem);

			case 'Flow':
				return flow(elem._0.ctor, elem._1);

			case 'Container':
				return container(elem._0, elem._1);

			case 'Spacer':
				return createNode('div');

			case 'RawHtml':
				return rawHtml(elem);

			case 'Custom':
				return elem.render(elem.model);
		}
	}

	function updateAndReplace(node, curr, next)
	{
		var newNode = update(node, curr, next);
		if (newNode !== node)
		{
			node.parentNode.replaceChild(newNode, node);
		}
		return newNode;
	}


	// UPDATE

	function update(node, curr, next)
	{
		var rootNode = node;
		if (node.tagName === 'A')
		{
			node = node.firstChild;
		}
		if (curr.props.id === next.props.id)
		{
			updateProps(node, curr, next);
			return rootNode;
		}
		if (curr.element.ctor !== next.element.ctor)
		{
			return render(next);
		}
		var nextE = next.element;
		var currE = curr.element;
		switch(nextE.ctor)
		{
			case "Spacer":
				updateProps(node, curr, next);
				return rootNode;

			case "RawHtml":
				if(currE.html.valueOf() !== nextE.html.valueOf())
				{
					node.innerHTML = nextE.html;
				}
				updateProps(node, curr, next);
				return rootNode;

			case "Image":
				if (nextE._0.ctor === 'Plain')
				{
					if (nextE._3 !== currE._3)
					{
						node.src = nextE._3;
					}
				}
				else if (!Utils.eq(nextE,currE)
					|| next.props.width !== curr.props.width
					|| next.props.height !== curr.props.height)
				{
					return render(next);
				}
				updateProps(node, curr, next);
				return rootNode;

			case "Flow":
				var arr = List.toArray(nextE._1);
				for (var i = arr.length; i--; )
				{
					arr[i] = arr[i].element.ctor;
				}
				if (nextE._0.ctor !== currE._0.ctor)
				{
					return render(next);
				}
				var nexts = List.toArray(nextE._1);
				var kids = node.childNodes;
				if (nexts.length !== kids.length)
				{
					return render(next);
				}
				var currs = List.toArray(currE._1);
				var dir = nextE._0.ctor;
				var goDir = directionTable[dir];
				var toReverse = needsReversal(dir);
				var len = kids.length;
				for (var i = len; i-- ;)
				{
					var subNode = kids[toReverse ? len - i - 1 : i];
					goDir(updateAndReplace(subNode, currs[i], nexts[i]));
				}
				updateProps(node, curr, next);
				return rootNode;

			case "Container":
				var subNode = node.firstChild;
				var newSubNode = updateAndReplace(subNode, currE._1, nextE._1);
				setPos(nextE._0, nextE._1, newSubNode);
				updateProps(node, curr, next);
				return rootNode;

			case "Custom":
				if (currE.type === nextE.type)
				{
					var updatedNode = nextE.update(node, currE.model, nextE.model);
					updateProps(updatedNode, curr, next);
					return updatedNode;
				}
				return render(next);
		}
	}

	function updateProps(node, curr, next)
	{
		var nextProps = next.props;
		var currProps = curr.props;

		var element = next.element;
		var width = nextProps.width - (element.adjustWidth || 0);
		var height = nextProps.height - (element.adjustHeight || 0);
		if (width !== currProps.width)
		{
			node.style.width = (width|0) + 'px';
		}
		if (height !== currProps.height)
		{
			node.style.height = (height|0) + 'px';
		}

		if (nextProps.opacity !== currProps.opacity)
		{
			node.style.opacity = nextProps.opacity;
		}

		var nextColor = nextProps.color.ctor === 'Just'
			? Color.toCss(nextProps.color._0)
			: '';
		if (node.style.backgroundColor !== nextColor)
		{
			node.style.backgroundColor = nextColor;
		}

		if (nextProps.tag !== currProps.tag)
		{
			node.id = nextProps.tag;
		}

		if (nextProps.href !== currProps.href)
		{
			if (currProps.href === '')
			{
				// add a surrounding href
				var anchor = createNode('a');
				anchor.href = nextProps.href;
				anchor.style.display = 'block';
				anchor.style.pointerEvents = 'auto';

				node.parentNode.replaceChild(anchor, node);
				anchor.appendChild(node);
			}
			else if (nextProps.href === '')
			{
				// remove the surrounding href
				var anchor = node.parentNode;
				anchor.parentNode.replaceChild(node, anchor);
			}
			else
			{
				// just update the link
				node.parentNode.href = nextProps.href;
			}
		}

		// update click and hover handlers
		var removed = false;

		// update hover handlers
		if (currProps.hover.ctor === '_Tuple0')
		{
			if (nextProps.hover.ctor !== '_Tuple0')
			{
				addHover(node, nextProps.hover);
			}
		}
		else
		{
			if (nextProps.hover.ctor === '_Tuple0')
			{
				removed = true;
				removeHover(node);
			}
			else
			{
				node.elm_hover_handler = nextProps.hover;
			}
		}

		// update click handlers
		if (currProps.click.ctor === '_Tuple0')
		{
			if (nextProps.click.ctor !== '_Tuple0')
			{
				addClick(node, nextProps.click);
			}
		}
		else
		{
			if (nextProps.click.ctor === '_Tuple0')
			{
				removed = true;
				removeClick(node);
			}
			else
			{
				node.elm_click_handler = nextProps.click;
			}
		}

		// stop capturing clicks if
		if (removed
			&& nextProps.hover.ctor === '_Tuple0'
			&& nextProps.click.ctor === '_Tuple0')
		{
			node.style.pointerEvents = 'none';
		}
	}


	// TEXT

	function block(align)
	{
		return function(text)
		{
			var raw = {
				ctor :'RawHtml',
				html : Text.renderHtml(text),
				align: align
			};
			var pos = htmlHeight(0, raw);
			return newElement(pos._0, pos._1, raw);
		}
	}

	function markdown(text)
	{
		var raw = {
			ctor:'RawHtml',
			html: text,
			align: null
		};
		var pos = htmlHeight(0, raw);
		return newElement(pos._0, pos._1, raw);
	}

	function htmlHeight(width, rawHtml)
	{
		// create dummy node
		var temp = document.createElement('div');
		temp.innerHTML = rawHtml.html;
		if (width > 0)
		{
			temp.style.width = width + "px";
		}
		temp.style.visibility = "hidden";
		temp.style.styleFloat = "left";
		temp.style.cssFloat   = "left";

		document.body.appendChild(temp);

		// get dimensions
		var style = window.getComputedStyle(temp, null);
		var w = Math.ceil(style.getPropertyValue("width").slice(0,-2) - 0);
		var h = Math.ceil(style.getPropertyValue("height").slice(0,-2) - 0);
		document.body.removeChild(temp);
		return Utils.Tuple2(w,h);
	}


	return localRuntime.Native.Graphics.Element.values = {
		render: render,
		update: update,
		updateAndReplace: updateAndReplace,

		createNode: createNode,
		newElement: F3(newElement),
		addTransform: addTransform,
		htmlHeight: F2(htmlHeight),
		guid: Utils.guid,

		block: block,
		markdown: markdown
	};

};

// setup
Elm.Native = Elm.Native || {};
Elm.Native.Graphics = Elm.Native.Graphics || {};
Elm.Native.Graphics.Input = Elm.Native.Graphics.Input || {};

// definition
Elm.Native.Graphics.Input.make = function(localRuntime) {
	'use strict';

	// attempt to short-circuit
	if ('values' in Elm.Native.Graphics.Input) {
		return Elm.Native.Graphics.Input.values;
	}

	var Color = Elm.Native.Color.make(localRuntime);
	var List = Elm.Native.List.make(localRuntime);
	var Signal = Elm.Native.Signal.make(localRuntime);
	var Text = Elm.Native.Text.make(localRuntime);
	var Utils = Elm.Native.Utils.make(localRuntime);

	var Element = Elm.Native.Graphics.Element.make(localRuntime);


	function renderDropDown(model)
	{
		var drop = Element.createNode('select');
		drop.style.border = '0 solid';
		drop.style.pointerEvents = 'auto';
		drop.style.display = 'block';

		drop.elm_values = List.toArray(model.values);
		drop.elm_handler = model.handler;
		var values = drop.elm_values;

		for (var i = 0; i < values.length; ++i)
		{
			var option = Element.createNode('option');
			var name = values[i]._0;
			option.value = name;
			option.innerHTML = name;
			drop.appendChild(option);
		}
		drop.addEventListener('change', function() {
			Signal.sendMessage(drop.elm_handler(drop.elm_values[drop.selectedIndex]._1));
		});

		return drop;
	}

	function updateDropDown(node, oldModel, newModel)
	{
		node.elm_values = List.toArray(newModel.values);
		node.elm_handler = newModel.handler;

		var values = node.elm_values;
		var kids = node.childNodes;
		var kidsLength = kids.length;

		var i = 0;
		for (; i < kidsLength && i < values.length; ++i)
		{
			var option = kids[i];
			var name = values[i]._0;
			option.value = name;
			option.innerHTML = name;
		}
		for (; i < kidsLength; ++i)
		{
			node.removeChild(node.lastChild);
		}
		for (; i < values.length; ++i)
		{
			var option = Element.createNode('option');
			var name = values[i]._0;
			option.value = name;
			option.innerHTML = name;
			node.appendChild(option);
		}
		return node;
	}

	function dropDown(handler, values)
	{
		return A3(Element.newElement, 100, 24, {
			ctor: 'Custom',
			type: 'DropDown',
			render: renderDropDown,
			update: updateDropDown,
			model: {
				values: values,
				handler: handler
			}
		});
	}

	function renderButton(model)
	{
		var node = Element.createNode('button');
		node.style.display = 'block';
		node.style.pointerEvents = 'auto';
		node.elm_message = model.message;
		function click()
		{
			Signal.sendMessage(node.elm_message);
		}
		node.addEventListener('click', click);
		node.innerHTML = model.text;
		return node;
	}

	function updateButton(node, oldModel, newModel)
	{
		node.elm_message = newModel.message;
		var txt = newModel.text;
		if (oldModel.text !== txt)
		{
			node.innerHTML = txt;
		}
		return node;
	}

	function button(message, text)
	{
		return A3(Element.newElement, 100, 40, {
			ctor: 'Custom',
			type: 'Button',
			render: renderButton,
			update: updateButton,
			model: {
				message: message,
				text:text
			}
		});
	}

	function renderCustomButton(model)
	{
		var btn = Element.createNode('div');
		btn.style.pointerEvents = 'auto';
		btn.elm_message = model.message;

		btn.elm_up    = Element.render(model.up);
		btn.elm_hover = Element.render(model.hover);
		btn.elm_down  = Element.render(model.down);

		btn.elm_up.style.display = 'block';
		btn.elm_hover.style.display = 'none';
		btn.elm_down.style.display = 'none';

		btn.appendChild(btn.elm_up);
		btn.appendChild(btn.elm_hover);
		btn.appendChild(btn.elm_down);

		function swap(visibleNode, hiddenNode1, hiddenNode2)
		{
			visibleNode.style.display = 'block';
			hiddenNode1.style.display = 'none';
			hiddenNode2.style.display = 'none';
		}

		var overCount = 0;
		function over(e)
		{
			if (overCount++ > 0) return;
			swap(btn.elm_hover, btn.elm_down, btn.elm_up);
		}
		function out(e)
		{
			if (btn.contains(e.toElement || e.relatedTarget)) return;
			overCount = 0;
			swap(btn.elm_up, btn.elm_down, btn.elm_hover);
		}
		function up()
		{
			swap(btn.elm_hover, btn.elm_down, btn.elm_up);
			Signal.sendMessage(btn.elm_message);
		}
		function down()
		{
			swap(btn.elm_down, btn.elm_hover, btn.elm_up);
		}

		btn.addEventListener('mouseover', over);
		btn.addEventListener('mouseout' , out);
		btn.addEventListener('mousedown', down);
		btn.addEventListener('mouseup'  , up);

		return btn;
	}

	function updateCustomButton(node, oldModel, newModel)
	{
		node.elm_message = newModel.message;

		var kids = node.childNodes;
		var styleUp    = kids[0].style.display;
		var styleHover = kids[1].style.display;
		var styleDown  = kids[2].style.display;

		Element.updateAndReplace(kids[0], oldModel.up, newModel.up);
		Element.updateAndReplace(kids[1], oldModel.hover, newModel.hover);
		Element.updateAndReplace(kids[2], oldModel.down, newModel.down);

		var kids = node.childNodes;
		kids[0].style.display = styleUp;
		kids[1].style.display = styleHover;
		kids[2].style.display = styleDown;

		return node;
	}

	function max3(a,b,c)
	{
		var ab = a > b ? a : b;
		return ab > c ? ab : c;
	}

	function customButton(message, up, hover, down)
	{
		return A3(Element.newElement,
				  max3(up.props.width, hover.props.width, down.props.width),
				  max3(up.props.height, hover.props.height, down.props.height),
				  { ctor: 'Custom',
					type: 'CustomButton',
					render: renderCustomButton,
					update: updateCustomButton,
					model: {
						message: message,
						up: up,
						hover: hover,
						down: down
					}
				  });
	}

	function renderCheckbox(model)
	{
		var node = Element.createNode('input');
		node.type = 'checkbox';
		node.checked = model.checked;
		node.style.display = 'block';
		node.style.pointerEvents = 'auto';
		node.elm_handler = model.handler;
		function change()
		{
			Signal.sendMessage(node.elm_handler(node.checked));
		}
		node.addEventListener('change', change);
		return node;
	}

	function updateCheckbox(node, oldModel, newModel)
	{
		node.elm_handler = newModel.handler;
		node.checked = newModel.checked;
		return node;
	}

	function checkbox(handler, checked)
	{
		return A3(Element.newElement, 13, 13, {
			ctor: 'Custom',
			type: 'CheckBox',
			render: renderCheckbox,
			update: updateCheckbox,
			model: { handler:handler, checked:checked }
		});
	}

	function setRange(node, start, end, dir)
	{
		if (node.parentNode)
		{
			node.setSelectionRange(start, end, dir);
		}
		else
		{
			setTimeout(function(){node.setSelectionRange(start, end, dir);}, 0);
		}
	}

	function updateIfNeeded(css, attribute, latestAttribute)
	{
		if (css[attribute] !== latestAttribute)
		{
			css[attribute] = latestAttribute;
		}
	}
	function cssDimensions(dimensions)
	{
		return dimensions.top    + 'px ' +
			   dimensions.right  + 'px ' +
			   dimensions.bottom + 'px ' +
			   dimensions.left   + 'px';
	}
	function updateFieldStyle(css, style)
	{
		updateIfNeeded(css, 'padding', cssDimensions(style.padding));

		var outline = style.outline;
		updateIfNeeded(css, 'border-width', cssDimensions(outline.width));
		updateIfNeeded(css, 'border-color', Color.toCss(outline.color));
		updateIfNeeded(css, 'border-radius', outline.radius + 'px');

		var highlight = style.highlight;
		if (highlight.width === 0)
		{
			css.outline = 'none';
		}
		else
		{
			updateIfNeeded(css, 'outline-width', highlight.width + 'px');
			updateIfNeeded(css, 'outline-color', Color.toCss(highlight.color));
		}

		var textStyle = style.style;
		updateIfNeeded(css, 'color', Color.toCss(textStyle.color));
		if (textStyle.typeface.ctor !== '[]')
		{
			updateIfNeeded(css, 'font-family', Text.toTypefaces(textStyle.typeface));
		}
		if (textStyle.height.ctor !== "Nothing")
		{
			updateIfNeeded(css, 'font-size', textStyle.height._0 + 'px');
		}
		updateIfNeeded(css, 'font-weight', textStyle.bold ? 'bold' : 'normal');
		updateIfNeeded(css, 'font-style', textStyle.italic ? 'italic' : 'normal');
		if (textStyle.line.ctor !== 'Nothing')
		{
			updateIfNeeded(css, 'text-decoration', Text.toLine(textStyle.line._0));
		}
	}

	function renderField(model)
	{
		var field = Element.createNode('input');
		updateFieldStyle(field.style, model.style);
		field.style.borderStyle = 'solid';
		field.style.pointerEvents = 'auto';

		field.type = model.type;
		field.placeholder = model.placeHolder;
		field.value = model.content.string;

		field.elm_handler = model.handler;
		field.elm_old_value = field.value;

		function inputUpdate(event)
		{
			var curr = field.elm_old_value;
			var next = field.value;
			if (curr === next)
			{
				return;
			}

			var direction = field.selectionDirection === 'forward' ? 'Forward' : 'Backward';
			var start = field.selectionStart;
			var end = field.selectionEnd;
			field.value = field.elm_old_value;

			Signal.sendMessage(field.elm_handler({
				_:{},
				string: next,
				selection: {
					_:{},
					start: start,
					end: end,
					direction: { ctor: direction }
				}
			}));
		}

		field.addEventListener('input', inputUpdate);
		field.addEventListener('focus', function() {
			field.elm_hasFocus = true;
		});
		field.addEventListener('blur', function() {
			field.elm_hasFocus = false;
		});

		return field;
	}

	function updateField(field, oldModel, newModel)
	{
		if (oldModel.style !== newModel.style)
		{
			updateFieldStyle(field.style, newModel.style);
		}
		field.elm_handler = newModel.handler;

		field.type = newModel.type;
		field.placeholder = newModel.placeHolder;
		var value = newModel.content.string;
		field.value = value;
		field.elm_old_value = value;
		if (field.elm_hasFocus)
		{
			var selection = newModel.content.selection;
			var direction = selection.direction.ctor === 'Forward' ? 'forward' : 'backward';
			setRange(field, selection.start, selection.end, direction);
		}
		return field;
	}

	function mkField(type)
	{
		function field(style, handler, placeHolder, content)
		{
			var padding = style.padding;
			var outline = style.outline.width;
			var adjustWidth = padding.left + padding.right + outline.left + outline.right;
			var adjustHeight = padding.top + padding.bottom + outline.top + outline.bottom;
			return A3(Element.newElement, 200, 30, {
				ctor: 'Custom',
				type: type + 'Field',
				adjustWidth: adjustWidth,
				adjustHeight: adjustHeight,
				render: renderField,
				update: updateField,
				model: {
					handler:handler,
					placeHolder:placeHolder,
					content:content,
					style:style,
					type:type
				}
			});
		}
		return F4(field);
	}

	function hoverable(handler, elem)
	{
		function onHover(bool)
		{
			Signal.sendMessage(handler(bool));
		}
		var props = Utils.replace([['hover',onHover]], elem.props);
		return {
			props: props,
			element: elem.element
		};
	}

	function clickable(message, elem)
	{
		function onClick()
		{
			Signal.sendMessage(message);
		}
		var props = Utils.replace([['click',onClick]], elem.props);
		return {
			props: props,
			element: elem.element
		};
	}

	return Elm.Native.Graphics.Input.values = {
		button: F2(button),
		customButton: F4(customButton),
		checkbox: F2(checkbox),
		dropDown: F2(dropDown),
		field: mkField('text'),
		email: mkField('email'),
		password: mkField('password'),
		hoverable: F2(hoverable),
		clickable: F2(clickable)
	};

};

Elm.Native.Json = {};
Elm.Native.Json.make = function(localRuntime) {

	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Json = localRuntime.Native.Json || {};
	if (localRuntime.Native.Json.values) {
		return localRuntime.Native.Json.values;
	}

	var ElmArray = Elm.Native.Array.make(localRuntime);
	var List = Elm.Native.List.make(localRuntime);
	var Maybe = Elm.Maybe.make(localRuntime);
	var Result = Elm.Result.make(localRuntime);
	var Utils = Elm.Native.Utils.make(localRuntime);


	function crash(expected, actual) {
		throw new Error(
			'expecting ' + expected + ' but got ' + JSON.stringify(actual)
		);
	}


	// PRIMITIVE VALUES

	function decodeNull(successValue) {
		return function(value) {
			if (value === null) {
				return successValue;
			}
			crash('null', value);
		};
	}


	function decodeString(value) {
		if (typeof value === 'string' || value instanceof String) {
			return value;
		}
		crash('a String', value);
	}


	function decodeFloat(value) {
		if (typeof value === 'number') {
			return value;
		}
		crash('a Float', value);
	}


	function decodeInt(value) {
		if (typeof value === 'number' && (value|0) === value) {
			return value;
		}
		crash('an Int', value);
	}


	function decodeBool(value) {
		if (typeof value === 'boolean') {
			return value;
		}
		crash('a Bool', value);
	}


	// ARRAY

	function decodeArray(decoder) {
		return function(value) {
			if (value instanceof Array) {
				var len = value.length;
				var array = new Array(len);
				for (var i = len; i-- ; ) {
					array[i] = decoder(value[i]);
				}
				return ElmArray.fromJSArray(array);
			}
			crash('an Array', value);
		};
	}


	// LIST

	function decodeList(decoder) {
		return function(value) {
			if (value instanceof Array) {
				var len = value.length;
				var list = List.Nil;
				for (var i = len; i-- ; ) {
					list = List.Cons( decoder(value[i]), list );
				}
				return list;
			}
			crash('a List', value);
		};
	}


	// MAYBE

	function decodeMaybe(decoder) {
		return function(value) {
			try {
				return Maybe.Just(decoder(value));
			} catch(e) {
				return Maybe.Nothing;
			}
		};
	}


	// FIELDS

	function decodeField(field, decoder) {
		return function(value) {
			var subValue = value[field];
			if (subValue !== undefined) {
				return decoder(subValue);
			}
			crash("an object with field '" + field + "'", value);
		};
	}


	// OBJECTS

	function decodeKeyValuePairs(decoder) {
		return function(value) {
			var isObject =
				typeof value === 'object'
					&& value !== null
					&& !(value instanceof Array);

			if (isObject) {
				var keyValuePairs = List.Nil;
				for (var key in value) {
					var elmValue = decoder(value[key]);
					var pair = Utils.Tuple2(key, elmValue);
					keyValuePairs = List.Cons(pair, keyValuePairs);
				}
				return keyValuePairs;
			}

			crash("an object", value);
		};
	}

	function decodeObject1(f, d1) {
		return function(value) {
			return f(d1(value));
		};
	}

	function decodeObject2(f, d1, d2) {
		return function(value) {
			return A2( f, d1(value), d2(value) );
		};
	}

	function decodeObject3(f, d1, d2, d3) {
		return function(value) {
			return A3( f, d1(value), d2(value), d3(value) );
		};
	}

	function decodeObject4(f, d1, d2, d3, d4) {
		return function(value) {
			return A4( f, d1(value), d2(value), d3(value), d4(value) );
		};
	}

	function decodeObject5(f, d1, d2, d3, d4, d5) {
		return function(value) {
			return A5( f, d1(value), d2(value), d3(value), d4(value), d5(value) );
		};
	}

	function decodeObject6(f, d1, d2, d3, d4, d5, d6) {
		return function(value) {
			return A6( f,
				d1(value),
				d2(value),
				d3(value),
				d4(value),
				d5(value),
				d6(value)
			);
		};
	}

	function decodeObject7(f, d1, d2, d3, d4, d5, d6, d7) {
		return function(value) {
			return A7( f,
				d1(value),
				d2(value),
				d3(value),
				d4(value),
				d5(value),
				d6(value),
				d7(value)
			);
		};
	}

	function decodeObject8(f, d1, d2, d3, d4, d5, d6, d7, d8) {
		return function(value) {
			return A8( f,
				d1(value),
				d2(value),
				d3(value),
				d4(value),
				d5(value),
				d6(value),
				d7(value),
				d8(value)
			);
		};
	}


	// TUPLES

	function decodeTuple1(f, d1) {
		return function(value) {
			if ( !(value instanceof Array) || value.length !== 1 ) {
				crash('a Tuple of length 1', value);
			}
			return f( d1(value[0]) );
		};
	}

	function decodeTuple2(f, d1, d2) {
		return function(value) {
			if ( !(value instanceof Array) || value.length !== 2 ) {
				crash('a Tuple of length 2', value);
			}
			return A2( f, d1(value[0]), d2(value[1]) );
		};
	}

	function decodeTuple3(f, d1, d2, d3) {
		return function(value) {
			if ( !(value instanceof Array) || value.length !== 3 ) {
				crash('a Tuple of length 3', value);
			}
			return A3( f, d1(value[0]), d2(value[1]), d3(value[2]) );
		};
	}


	function decodeTuple4(f, d1, d2, d3, d4) {
		return function(value) {
			if ( !(value instanceof Array) || value.length !== 4 ) {
				crash('a Tuple of length 4', value);
			}
			return A4( f, d1(value[0]), d2(value[1]), d3(value[2]), d4(value[3]) );
		};
	}


	function decodeTuple5(f, d1, d2, d3, d4, d5) {
		return function(value) {
			if ( !(value instanceof Array) || value.length !== 5 ) {
				crash('a Tuple of length 5', value);
			}
			return A5( f,
				d1(value[0]),
				d2(value[1]),
				d3(value[2]),
				d4(value[3]),
				d5(value[4])
			);
		};
	}


	function decodeTuple6(f, d1, d2, d3, d4, d5, d6) {
		return function(value) {
			if ( !(value instanceof Array) || value.length !== 6 ) {
				crash('a Tuple of length 6', value);
			}
			return A6( f,
				d1(value[0]),
				d2(value[1]),
				d3(value[2]),
				d4(value[3]),
				d5(value[4]),
				d6(value[5])
			);
		};
	}

	function decodeTuple7(f, d1, d2, d3, d4, d5, d6, d7) {
		return function(value) {
			if ( !(value instanceof Array) || value.length !== 7 ) {
				crash('a Tuple of length 7', value);
			}
			return A7( f,
				d1(value[0]),
				d2(value[1]),
				d3(value[2]),
				d4(value[3]),
				d5(value[4]),
				d6(value[5]),
				d7(value[6])
			);
		};
	}


	function decodeTuple8(f, d1, d2, d3, d4, d5, d6, d7, d8) {
		return function(value) {
			if ( !(value instanceof Array) || value.length !== 8 ) {
				crash('a Tuple of length 8', value);
			}
			return A8( f,
				d1(value[0]),
				d2(value[1]),
				d3(value[2]),
				d4(value[3]),
				d5(value[4]),
				d6(value[5]),
				d7(value[6]),
				d8(value[7])
			);
		};
	}


	// CUSTOM DECODERS

	function decodeValue(value) {
		return value;
	}

	function runDecoderValue(decoder, value) {
		try {
			return Result.Ok(decoder(value));
		} catch(e) {
			return Result.Err(e.message);
		}
	}

	function customDecoder(decoder, callback) {
		return function(value) {
			var result = callback(decoder(value));
			if (result.ctor === 'Err') {
				throw new Error('custom decoder failed: ' + result._0);
			}
			return result._0;
		}
	}

	function andThen(decode, callback) {
		return function(value) {
			var result = decode(value);
			return callback(result)(value);
		}
	}

	function fail(msg) {
		return function(value) {
			throw new Error(msg);
		}
	}

	function succeed(successValue) {
		return function(value) {
			return successValue;
		}
	}


	// ONE OF MANY

	function oneOf(decoders) {
		return function(value) {
			var errors = [];
			var temp = decoders;
			while (temp.ctor !== '[]') {
				try {
					return temp._0(value);
				} catch(e) {
					errors.push(e.message);
				}
				temp = temp._1;
			}
			throw new Error('expecting one of the following:\n    ' + errors.join('\n    '));
		}
	}

	function get(decoder, value) {
		try {
			return Result.Ok(decoder(value));
		} catch(e) {
			return Result.Err(e.message);
		}
	}


	// ENCODE / DECODE

	function runDecoderString(decoder, string) {
		try {
			return Result.Ok(decoder(JSON.parse(string)));
		} catch(e) {
			return Result.Err(e.message);
		}
	}

	function encode(indentLevel, value) {
		return JSON.stringify(value, null, indentLevel);
	}

	function identity(value) {
		return value;
	}

	function encodeObject(keyValuePairs) {
		var obj = {};
		while (keyValuePairs.ctor !== '[]') {
			var pair = keyValuePairs._0;
			obj[pair._0] = pair._1;
			keyValuePairs = keyValuePairs._1;
		}
		return obj;
	}

	return localRuntime.Native.Json.values = {
		encode: F2(encode),
		runDecoderString: F2(runDecoderString),
		runDecoderValue: F2(runDecoderValue),

		get: F2(get),
		oneOf: oneOf,

		decodeNull: decodeNull,
		decodeInt: decodeInt,
		decodeFloat: decodeFloat,
		decodeString: decodeString,
		decodeBool: decodeBool,

		decodeMaybe: decodeMaybe,

		decodeList: decodeList,
		decodeArray: decodeArray,

		decodeField: F2(decodeField),

		decodeObject1: F2(decodeObject1),
		decodeObject2: F3(decodeObject2),
		decodeObject3: F4(decodeObject3),
		decodeObject4: F5(decodeObject4),
		decodeObject5: F6(decodeObject5),
		decodeObject6: F7(decodeObject6),
		decodeObject7: F8(decodeObject7),
		decodeObject8: F9(decodeObject8),
		decodeKeyValuePairs: decodeKeyValuePairs,

		decodeTuple1: F2(decodeTuple1),
		decodeTuple2: F3(decodeTuple2),
		decodeTuple3: F4(decodeTuple3),
		decodeTuple4: F5(decodeTuple4),
		decodeTuple5: F6(decodeTuple5),
		decodeTuple6: F7(decodeTuple6),
		decodeTuple7: F8(decodeTuple7),
		decodeTuple8: F9(decodeTuple8),

		andThen: F2(andThen),
		decodeValue: decodeValue,
		customDecoder: F2(customDecoder),
		fail: fail,
		succeed: succeed,

		identity: identity,
		encodeNull: null,
		encodeArray: ElmArray.toJSArray,
		encodeList: List.toArray,
		encodeObject: encodeObject

	};

};

Elm.Native.Keyboard = {};
Elm.Native.Keyboard.make = function(localRuntime) {

	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Keyboard = localRuntime.Native.Keyboard || {};
	if (localRuntime.Native.Keyboard.values)
	{
		return localRuntime.Native.Keyboard.values;
	}

	var NS = Elm.Native.Signal.make(localRuntime);


	function keyEvent(event)
	{
		return {
			_: {},
			alt: event.altKey,
			meta: event.metaKey,
			keyCode: event.keyCode
		};
	}


	function keyStream(node, eventName, handler)
	{
		var stream = NS.input(eventName, '\0');

		localRuntime.addListener([stream.id], node, eventName, function(e) {
			localRuntime.notify(stream.id, handler(e));
		});

		return stream;
	}

	var downs = keyStream(document, 'keydown', keyEvent);
	var ups = keyStream(document, 'keyup', keyEvent);
	var presses = keyStream(document, 'keypress', keyEvent);
	var blurs = keyStream(window, 'blur', function() { return null; });


	return localRuntime.Native.Keyboard.values = {
		downs: downs,
		ups: ups,
		blurs: blurs,
		presses: presses
	};

};

Elm.Native.List = {};
Elm.Native.List.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.List = localRuntime.Native.List || {};
	if (localRuntime.Native.List.values)
	{
		return localRuntime.Native.List.values;
	}
	if ('values' in Elm.Native.List)
	{
		return localRuntime.Native.List.values = Elm.Native.List.values;
	}

	var Utils = Elm.Native.Utils.make(localRuntime);

	var Nil = Utils.Nil;
	var Cons = Utils.Cons;

	function toArray(xs)
	{
		var out = [];
		while (xs.ctor !== '[]')
		{
			out.push(xs._0);
			xs = xs._1;
		}
		return out;
	}

	function fromArray(arr)
	{
		var out = Nil;
		for (var i = arr.length; i--; )
		{
			out = Cons(arr[i], out);
		}
		return out;
	}

	function range(lo,hi)
	{
		var lst = Nil;
		if (lo <= hi)
		{
			do { lst = Cons(hi,lst) } while (hi-->lo);
		}
		return lst
	}

	// f defined similarly for both foldl and foldr (NB: different from Haskell)
	// ie, foldl : (a -> b -> b) -> b -> [a] -> b
	function foldl(f, b, xs)
	{
		var acc = b;
		while (xs.ctor !== '[]')
		{
			acc = A2(f, xs._0, acc);
			xs = xs._1;
		}
		return acc;
	}

	function foldr(f, b, xs)
	{
		var arr = toArray(xs);
		var acc = b;
		for (var i = arr.length; i--; )
		{
			acc = A2(f, arr[i], acc);
		}
		return acc;
	}

	function any(pred, xs)
	{
		while (xs.ctor !== '[]')
		{
			if (pred(xs._0))
			{
				return true;
			}
			xs = xs._1;
		}
		return false;
	}

	function map2(f, xs, ys)
	{
		var arr = [];
		while (xs.ctor !== '[]' && ys.ctor !== '[]')
		{
			arr.push(A2(f, xs._0, ys._0));
			xs = xs._1;
			ys = ys._1;
		}
		return fromArray(arr);
	}

	function map3(f, xs, ys, zs)
	{
		var arr = [];
		while (xs.ctor !== '[]' && ys.ctor !== '[]' && zs.ctor !== '[]')
		{
			arr.push(A3(f, xs._0, ys._0, zs._0));
			xs = xs._1;
			ys = ys._1;
			zs = zs._1;
		}
		return fromArray(arr);
	}

	function map4(f, ws, xs, ys, zs)
	{
		var arr = [];
		while (   ws.ctor !== '[]'
			   && xs.ctor !== '[]'
			   && ys.ctor !== '[]'
			   && zs.ctor !== '[]')
		{
			arr.push(A4(f, ws._0, xs._0, ys._0, zs._0));
			ws = ws._1;
			xs = xs._1;
			ys = ys._1;
			zs = zs._1;
		}
		return fromArray(arr);
	}

	function map5(f, vs, ws, xs, ys, zs)
	{
		var arr = [];
		while (   vs.ctor !== '[]'
			   && ws.ctor !== '[]'
			   && xs.ctor !== '[]'
			   && ys.ctor !== '[]'
			   && zs.ctor !== '[]')
		{
			arr.push(A5(f, vs._0, ws._0, xs._0, ys._0, zs._0));
			vs = vs._1;
			ws = ws._1;
			xs = xs._1;
			ys = ys._1;
			zs = zs._1;
		}
		return fromArray(arr);
	}

	function sortBy(f, xs)
	{
		return fromArray(toArray(xs).sort(function(a,b){
			return Utils.cmp(f(a), f(b));
		}));
	}

	function sortWith(f, xs)
	{
		return fromArray(toArray(xs).sort(function(a,b){
			var ord = f(a)(b).ctor;
			return ord === 'EQ' ? 0 : ord === 'LT' ? -1 : 1;
		}));
	}

	function take(n, xs)
	{
		var arr = [];
		while (xs.ctor !== '[]' && n > 0)
		{
			arr.push(xs._0);
			xs = xs._1;
			--n;
		}
		return fromArray(arr);
	}

	function drop(n, xs)
	{
		while (xs.ctor !== '[]' && n > 0)
		{
			xs = xs._1;
			--n;
		}
		return xs;
	}

	function repeat(n, x)
	{
		var arr = [];
		var pattern = [x];
		while (n > 0)
		{
			if (n & 1)
			{
				arr = arr.concat(pattern);
			}
			n >>= 1, pattern = pattern.concat(pattern);
		}
		return fromArray(arr);
	}


	Elm.Native.List.values = {
		Nil:Nil,
		Cons:Cons,
		cons:F2(Cons),
		toArray:toArray,
		fromArray:fromArray,
		range:range,

		foldl:F3(foldl),
		foldr:F3(foldr),

		any:F2(any),
		map2:F3(map2),
		map3:F4(map3),
		map4:F5(map4),
		map5:F6(map5),
		sortBy:F2(sortBy),
		sortWith:F2(sortWith),
		take:F2(take),
		drop:F2(drop),
		repeat:F2(repeat)
	};
	return localRuntime.Native.List.values = Elm.Native.List.values;

};

Elm.Native = Elm.Native || {};
Elm.Native.Mouse = {};
Elm.Native.Mouse.make = function(localRuntime) {

	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Mouse = localRuntime.Native.Mouse || {};
	if (localRuntime.Native.Mouse.values)
	{
		return localRuntime.Native.Mouse.values;
	}

	var NS = Elm.Native.Signal.make(localRuntime);
	var Utils = Elm.Native.Utils.make(localRuntime);

	var position = NS.input('Mouse.position', Utils.Tuple2(0,0));

	var isDown = NS.input('Mouse.isDown', false);

	var clicks = NS.input('Mouse.clicks', Utils.Tuple0);

	var node = localRuntime.isFullscreen()
		? document
		: localRuntime.node;

	localRuntime.addListener([clicks.id], node, 'click', function click() {
		localRuntime.notify(clicks.id, Utils.Tuple0);
	});
	localRuntime.addListener([isDown.id], node, 'mousedown', function down() {
		localRuntime.notify(isDown.id, true);
	});
	localRuntime.addListener([isDown.id], node, 'mouseup', function up() {
		localRuntime.notify(isDown.id, false);
	});
	localRuntime.addListener([position.id], node, 'mousemove', function move(e) {
		localRuntime.notify(position.id, Utils.getXY(e));
	});

	return localRuntime.Native.Mouse.values = {
		position: position,
		isDown: isDown,
		clicks: clicks
	};
};

Elm.Native.Port = {};
Elm.Native.Port.make = function(localRuntime) {

	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Port = localRuntime.Native.Port || {};
	if (localRuntime.Native.Port.values)
	{
		return localRuntime.Native.Port.values;
	}

	var NS;
	var Utils = Elm.Native.Utils.make(localRuntime);


	// INBOUND

	function inbound(name, type, converter)
	{
		if (!localRuntime.argsTracker[name])
		{
			throw new Error(
				"Port Error:\n" +
				"No argument was given for the port named '" + name + "' with type:\n\n" +
				"    " + type.split('\n').join('\n        ') + "\n\n" +
				"You need to provide an initial value!\n\n" +
				"Find out more about ports here <http://elm-lang.org/learn/Ports.elm>"
			);
		}
		var arg = localRuntime.argsTracker[name];
		arg.used = true;

		return jsToElm(name, type, converter, arg.value);
	}


	function inboundSignal(name, type, converter)
	{
		var initialValue = inbound(name, type, converter);

		if (!NS)
		{
			NS = Elm.Native.Signal.make(localRuntime);
		}
		var signal = NS.input('inbound-port-' + name, initialValue);

		function send(jsValue)
		{
			var elmValue = jsToElm(name, type, converter, jsValue);
			setTimeout(function() {
				localRuntime.notify(signal.id, elmValue);
			}, 0);
		}

		localRuntime.ports[name] = { send: send };

		return signal;
	}


	function jsToElm(name, type, converter, value)
	{
		try
		{
			return converter(value);
		}
		catch(e)
		{
			throw new Error(
				"Port Error:\n" +
				"Regarding the port named '" + name + "' with type:\n\n" +
				"    " + type.split('\n').join('\n        ') + "\n\n" +
				"You just sent the value:\n\n" +
				"    " + JSON.stringify(arg.value) + "\n\n" +
				"but it cannot be converted to the necessary type.\n" +
				e.message
			);
		}
	}


	// OUTBOUND

	function outbound(name, converter, elmValue)
	{
		localRuntime.ports[name] = converter(elmValue);
	}


	function outboundSignal(name, converter, signal)
	{
		var subscribers = [];

		function subscribe(handler)
		{
			subscribers.push(handler);
		}
		function unsubscribe(handler)
		{
			subscribers.pop(subscribers.indexOf(handler));
		}

		function notify(elmValue)
		{
			var jsValue = converter(elmValue);
			var len = subscribers.length;
			for (var i = 0; i < len; ++i)
			{
				subscribers[i](jsValue);
			}
		}

		if (!NS)
		{
			NS = Elm.Native.Signal.make(localRuntime);
		}
		NS.output('outbound-port-' + name, notify, signal);

		localRuntime.ports[name] = {
			subscribe: subscribe,
			unsubscribe: unsubscribe
		};

		return signal;
	}


	return localRuntime.Native.Port.values = {
		inbound: inbound,
		outbound: outbound,
		inboundSignal: inboundSignal,
		outboundSignal: outboundSignal
	};
};


if (!Elm.fullscreen) {

	(function() {
		'use strict';

		var Display = {
			FULLSCREEN: 0,
			COMPONENT: 1,
			NONE: 2
		};

		Elm.fullscreen = function(module, args)
		{
			var container = document.createElement('div');
			document.body.appendChild(container);
			return init(Display.FULLSCREEN, container, module, args || {});
		};

		Elm.embed = function(module, container, args)
		{
			var tag = container.tagName;
			if (tag !== 'DIV')
			{
				throw new Error('Elm.node must be given a DIV, not a ' + tag + '.');
			}
			return init(Display.COMPONENT, container, module, args || {});
		};

		Elm.worker = function(module, args)
		{
			return init(Display.NONE, {}, module, args || {});
		};

		function init(display, container, module, args, moduleToReplace)
		{
			// defining state needed for an instance of the Elm RTS
			var inputs = [];

			/* OFFSET
			 * Elm's time traveling debugger lets you pause time. This means
			 * "now" may be shifted a bit into the past. By wrapping Date.now()
			 * we can manage this.
			 */
			var timer = {
				programStart: Date.now(),
				now: function()
				{
					return Date.now();
				}
			};

			var updateInProgress = false;
			function notify(id, v)
			{
				if (updateInProgress)
				{
					throw new Error(
						'The notify function has been called synchronously!\n' +
						'This can lead to frames being dropped.\n' +
						'Definitely report this to <https://github.com/elm-lang/Elm/issues>\n');
				}
				updateInProgress = true;
				var timestep = timer.now();
				for (var i = inputs.length; i--; )
				{
					inputs[i].notify(timestep, id, v);
				}
				updateInProgress = false;
			}
			function setTimeout(func, delay)
			{
				return window.setTimeout(func, delay);
			}

			var listeners = [];
			function addListener(relevantInputs, domNode, eventName, func)
			{
				domNode.addEventListener(eventName, func);
				var listener = {
					relevantInputs: relevantInputs,
					domNode: domNode,
					eventName: eventName,
					func: func
				};
				listeners.push(listener);
			}

			var argsTracker = {};
			for (var name in args)
			{
				argsTracker[name] = {
					value: args[name],
					used: false
				};
			}

			// create the actual RTS. Any impure modules will attach themselves to this
			// object. This permits many Elm programs to be embedded per document.
			var elm = {
				notify: notify,
				setTimeout: setTimeout,
				node: container,
				addListener: addListener,
				inputs: inputs,
				timer: timer,
				argsTracker: argsTracker,
				ports: {},

				isFullscreen: function() { return display === Display.FULLSCREEN; },
				isEmbed: function() { return display === Display.COMPONENT; },
				isWorker: function() { return display === Display.NONE; }
			};

			function swap(newModule)
			{
				removeListeners(listeners);
				var div = document.createElement('div');
				var newElm = init(display, div, newModule, args, elm);
				inputs = [];
				// elm.swap = newElm.swap;
				return newElm;
			}

			function dispose()
			{
				removeListeners(listeners);
				inputs = [];
			}

			var Module = {};
			try
			{
				Module = module.make(elm);
				checkInputs(elm);
			}
			catch (error)
			{
				if (typeof container.appendChild == 'undefined')
				{
					console.log(error.message);
				}
				else
				{
					container.appendChild(errorNode(error.message));
				}
				throw error;
			}

			if (display !== Display.NONE)
			{
				var graphicsNode = initGraphics(elm, Module);
			}

			var rootNode = { kids: inputs };
			trimDeadNodes(rootNode);
			inputs = rootNode.kids;
			filterListeners(inputs, listeners);

			addReceivers(elm.ports);

			if (typeof moduleToReplace !== 'undefined')
			{
				hotSwap(moduleToReplace, elm);

				// rerender scene if graphics are enabled.
				if (typeof graphicsNode !== 'undefined')
				{
					graphicsNode.notify(0, true, 0);
				}
			}

			return {
				swap: swap,
				ports: elm.ports,
				dispose: dispose
			};
		};

		function checkInputs(elm)
		{
			var argsTracker = elm.argsTracker;
			for (var name in argsTracker)
			{
				if (!argsTracker[name].used)
				{
					throw new Error(
						"Port Error:\nYou provided an argument named '" + name +
						"' but there is no corresponding port!\n\n" +
						"Maybe add a port '" + name + "' to your Elm module?\n" +
						"Maybe remove the '" + name + "' argument from your initialization code in JS?"
					);
				}
			}
		}

		function errorNode(message)
		{
			var code = document.createElement('code');

			var lines = message.split('\n');
			code.appendChild(document.createTextNode(lines[0]));
			code.appendChild(document.createElement('br'));
			code.appendChild(document.createElement('br'));
			for (var i = 1; i < lines.length; ++i)
			{
				code.appendChild(document.createTextNode('\u00A0 \u00A0 ' + lines[i].replace(/  /g, '\u00A0 ')));
				code.appendChild(document.createElement('br'));
			}
			code.appendChild(document.createElement('br'));
			code.appendChild(document.createTextNode("Open the developer console for more details."));
			return code;
		}


		//// FILTER SIGNALS ////

		// TODO: move this code into the signal module and create a function
		// Signal.initializeGraph that actually instantiates everything.

		function filterListeners(inputs, listeners)
		{
			loop:
			for (var i = listeners.length; i--; )
			{
				var listener = listeners[i];
				for (var j = inputs.length; j--; )
				{
					if (listener.relevantInputs.indexOf(inputs[j].id) >= 0)
					{
						continue loop;
					}
				}
				listener.domNode.removeEventListener(listener.eventName, listener.func);
			}
		}

		function removeListeners(listeners)
		{
			for (var i = listeners.length; i--; )
			{
				var listener = listeners[i];
				listener.domNode.removeEventListener(listener.eventName, listener.func);
			}
		}

		// add receivers for built-in ports if they are defined
		function addReceivers(ports)
		{
			if ('title' in ports)
			{
				if (typeof ports.title === 'string')
				{
					document.title = ports.title;
				}
				else
				{
					ports.title.subscribe(function(v) { document.title = v; });
				}
			}
			if ('redirect' in ports)
			{
				ports.redirect.subscribe(function(v) {
					if (v.length > 0)
					{
						window.location = v;
					}
				});
			}
		}


		// returns a boolean representing whether the node is alive or not.
		function trimDeadNodes(node)
		{
			if (node.isOutput)
			{
				return true;
			}

			var liveKids = [];
			for (var i = node.kids.length; i--; )
			{
				var kid = node.kids[i];
				if (trimDeadNodes(kid))
				{
					liveKids.push(kid);
				}
			}
			node.kids = liveKids;

			return liveKids.length > 0;
		}


		////  RENDERING  ////

		function initGraphics(elm, Module)
		{
			if (!('main' in Module))
			{
				throw new Error("'main' is missing! What do I display?!");
			}

			var signalGraph = Module.main;

			// make sure the signal graph is actually a signal & extract the visual model
			if (!('notify' in signalGraph))
			{
				signalGraph = Elm.Signal.make(elm).constant(signalGraph);
			}
			var initialScene = signalGraph.value;

			// Figure out what the render functions should be
			var render;
			var update;
			if (initialScene.props)
			{
				var Element = Elm.Native.Graphics.Element.make(elm);
				render = Element.render;
				update = Element.updateAndReplace;
			}
			else
			{
				var VirtualDom = Elm.Native.VirtualDom.make(elm);
				render = VirtualDom.render;
				update = VirtualDom.updateAndReplace;
			}

			// Add the initialScene to the DOM
			var container = elm.node;
			var node = render(initialScene);
			while (container.firstChild)
			{
				container.removeChild(container.firstChild);
			}
			container.appendChild(node);

			var _requestAnimationFrame =
				typeof requestAnimationFrame !== 'undefined'
					? requestAnimationFrame
					: function(cb) { setTimeout(cb, 1000/60); }
					;

			// domUpdate is called whenever the main Signal changes.
			//
			// domUpdate and drawCallback implement a small state machine in order
			// to schedule only 1 draw per animation frame. This enforces that
			// once draw has been called, it will not be called again until the
			// next frame.
			//
			// drawCallback is scheduled whenever
			// 1. The state transitions from PENDING_REQUEST to EXTRA_REQUEST, or
			// 2. The state transitions from NO_REQUEST to PENDING_REQUEST
			//
			// Invariants:
			// 1. In the NO_REQUEST state, there is never a scheduled drawCallback.
			// 2. In the PENDING_REQUEST and EXTRA_REQUEST states, there is always exactly 1
			//    scheduled drawCallback.
			var NO_REQUEST = 0;
			var PENDING_REQUEST = 1;
			var EXTRA_REQUEST = 2;
			var state = NO_REQUEST;
			var savedScene = initialScene;
			var scheduledScene = initialScene;

			function domUpdate(newScene)
			{
				scheduledScene = newScene;

				switch (state)
				{
					case NO_REQUEST:
						_requestAnimationFrame(drawCallback);
						state = PENDING_REQUEST;
						return;
					case PENDING_REQUEST:
						state = PENDING_REQUEST;
						return;
					case EXTRA_REQUEST:
						state = PENDING_REQUEST;
						return;
				}
			}

			function drawCallback()
			{
				switch (state)
				{
					case NO_REQUEST:
						// This state should not be possible. How can there be no
						// request, yet somehow we are actively fulfilling a
						// request?
						throw new Error(
							"Unexpected draw callback.\n" +
							"Please report this to <https://github.com/elm-lang/core/issues>."
						);

					case PENDING_REQUEST:
						// At this point, we do not *know* that another frame is
						// needed, but we make an extra request to rAF just in
						// case. It's possible to drop a frame if rAF is called
						// too late, so we just do it preemptively.
						_requestAnimationFrame(drawCallback);
						state = EXTRA_REQUEST;

						// There's also stuff we definitely need to draw.
						draw();
						return;

					case EXTRA_REQUEST:
						// Turns out the extra request was not needed, so we will
						// stop calling rAF. No reason to call it all the time if
						// no one needs it.
						state = NO_REQUEST;
						return;
				}
			}

			function draw()
			{
				update(elm.node.firstChild, savedScene, scheduledScene);
				if (elm.Native.Window)
				{
					elm.Native.Window.values.resizeIfNeeded();
				}
				savedScene = scheduledScene;
			}

			var renderer = Elm.Native.Signal.make(elm).output('main', domUpdate, signalGraph);

			// must check for resize after 'renderer' is created so
			// that changes show up.
			if (elm.Native.Window)
			{
				elm.Native.Window.values.resizeIfNeeded();
			}

			return renderer;
		}

		//// HOT SWAPPING ////

		// Returns boolean indicating if the swap was successful.
		// Requires that the two signal graphs have exactly the same
		// structure.
		function hotSwap(from, to)
		{
			function similar(nodeOld,nodeNew)
			{
				if (nodeOld.id !== nodeNew.id)
				{
					return false;
				}
				if (nodeOld.isOutput)
				{
					return nodeNew.isOutput;
				}
				return nodeOld.kids.length === nodeNew.kids.length;
			}
			function swap(nodeOld,nodeNew)
			{
				nodeNew.value = nodeOld.value;
				return true;
			}
			var canSwap = depthFirstTraversals(similar, from.inputs, to.inputs);
			if (canSwap)
			{
				depthFirstTraversals(swap, from.inputs, to.inputs);
			}
			from.node.parentNode.replaceChild(to.node, from.node);

			return canSwap;
		}

		// Returns false if the node operation f ever fails.
		function depthFirstTraversals(f, queueOld, queueNew)
		{
			if (queueOld.length !== queueNew.length)
			{
				return false;
			}
			queueOld = queueOld.slice(0);
			queueNew = queueNew.slice(0);

			var seen = [];
			while (queueOld.length > 0 && queueNew.length > 0)
			{
				var nodeOld = queueOld.pop();
				var nodeNew = queueNew.pop();
				if (seen.indexOf(nodeOld.id) < 0)
				{
					if (!f(nodeOld, nodeNew))
					{
						return false;
					}
					queueOld = queueOld.concat(nodeOld.kids || []);
					queueNew = queueNew.concat(nodeNew.kids || []);
					seen.push(nodeOld.id);
				}
			}
			return true;
		}
	}());

	function F2(fun)
	{
		function wrapper(a) { return function(b) { return fun(a,b) } }
		wrapper.arity = 2;
		wrapper.func = fun;
		return wrapper;
	}

	function F3(fun)
	{
		function wrapper(a) {
			return function(b) { return function(c) { return fun(a,b,c) }}
		}
		wrapper.arity = 3;
		wrapper.func = fun;
		return wrapper;
	}

	function F4(fun)
	{
		function wrapper(a) { return function(b) { return function(c) {
			return function(d) { return fun(a,b,c,d) }}}
		}
		wrapper.arity = 4;
		wrapper.func = fun;
		return wrapper;
	}

	function F5(fun)
	{
		function wrapper(a) { return function(b) { return function(c) {
			return function(d) { return function(e) { return fun(a,b,c,d,e) }}}}
		}
		wrapper.arity = 5;
		wrapper.func = fun;
		return wrapper;
	}

	function F6(fun)
	{
		function wrapper(a) { return function(b) { return function(c) {
			return function(d) { return function(e) { return function(f) {
			return fun(a,b,c,d,e,f) }}}}}
		}
		wrapper.arity = 6;
		wrapper.func = fun;
		return wrapper;
	}

	function F7(fun)
	{
		function wrapper(a) { return function(b) { return function(c) {
			return function(d) { return function(e) { return function(f) {
			return function(g) { return fun(a,b,c,d,e,f,g) }}}}}}
		}
		wrapper.arity = 7;
		wrapper.func = fun;
		return wrapper;
	}

	function F8(fun)
	{
		function wrapper(a) { return function(b) { return function(c) {
			return function(d) { return function(e) { return function(f) {
			return function(g) { return function(h) {
			return fun(a,b,c,d,e,f,g,h)}}}}}}}
		}
		wrapper.arity = 8;
		wrapper.func = fun;
		return wrapper;
	}

	function F9(fun)
	{
		function wrapper(a) { return function(b) { return function(c) {
			return function(d) { return function(e) { return function(f) {
			return function(g) { return function(h) { return function(i) {
			return fun(a,b,c,d,e,f,g,h,i) }}}}}}}}
		}
		wrapper.arity = 9;
		wrapper.func = fun;
		return wrapper;
	}

	function A2(fun,a,b)
	{
		return fun.arity === 2
			? fun.func(a,b)
			: fun(a)(b);
	}
	function A3(fun,a,b,c)
	{
		return fun.arity === 3
			? fun.func(a,b,c)
			: fun(a)(b)(c);
	}
	function A4(fun,a,b,c,d)
	{
		return fun.arity === 4
			? fun.func(a,b,c,d)
			: fun(a)(b)(c)(d);
	}
	function A5(fun,a,b,c,d,e)
	{
		return fun.arity === 5
			? fun.func(a,b,c,d,e)
			: fun(a)(b)(c)(d)(e);
	}
	function A6(fun,a,b,c,d,e,f)
	{
		return fun.arity === 6
			? fun.func(a,b,c,d,e,f)
			: fun(a)(b)(c)(d)(e)(f);
	}
	function A7(fun,a,b,c,d,e,f,g)
	{
		return fun.arity === 7
			? fun.func(a,b,c,d,e,f,g)
			: fun(a)(b)(c)(d)(e)(f)(g);
	}
	function A8(fun,a,b,c,d,e,f,g,h)
	{
		return fun.arity === 8
			? fun.func(a,b,c,d,e,f,g,h)
			: fun(a)(b)(c)(d)(e)(f)(g)(h);
	}
	function A9(fun,a,b,c,d,e,f,g,h,i)
	{
		return fun.arity === 9
			? fun.func(a,b,c,d,e,f,g,h,i)
			: fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
	}
}

Elm.Native.Show = {};
Elm.Native.Show.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Show = localRuntime.Native.Show || {};
	if (localRuntime.Native.Show.values)
	{
		return localRuntime.Native.Show.values;
	}

	var _Array;
	var Dict;
	var List;
	var Utils = Elm.Native.Utils.make(localRuntime);

	var toString = function(v)
	{
		var type = typeof v;
		if (type === "function")
		{
			var name = v.func ? v.func.name : v.name;
			return '<function' + (name === '' ? '' : ': ') + name + '>';
		}
		else if (type === "boolean")
		{
			return v ? "True" : "False";
		}
		else if (type === "number")
		{
			return v + "";
		}
		else if ((v instanceof String) && v.isChar)
		{
			return "'" + addSlashes(v, true) + "'";
		}
		else if (type === "string")
		{
			return '"' + addSlashes(v, false) + '"';
		}
		else if (type === "object" && '_' in v && probablyPublic(v))
		{
			var output = [];
			for (var k in v._)
			{
				for (var i = v._[k].length; i--; )
				{
					output.push(k + " = " + toString(v._[k][i]));
				}
			}
			for (var k in v)
			{
				if (k === '_') continue;
				output.push(k + " = " + toString(v[k]));
			}
			if (output.length === 0)
			{
				return "{}";
			}
			return "{ " + output.join(", ") + " }";
		}
		else if (type === "object" && 'ctor' in v)
		{
			if (v.ctor.substring(0,6) === "_Tuple")
			{
				var output = [];
				for (var k in v)
				{
					if (k === 'ctor') continue;
					output.push(toString(v[k]));
				}
				return "(" + output.join(",") + ")";
			}
			else if (v.ctor === "_Array")
			{
				if (!_Array)
				{
					_Array = Elm.Array.make(localRuntime);
				}
				var list = _Array.toList(v);
				return "Array.fromList " + toString(list);
			}
			else if (v.ctor === "::")
			{
				var output = '[' + toString(v._0);
				v = v._1;
				while (v.ctor === "::")
				{
					output += "," + toString(v._0);
					v = v._1;
				}
				return output + ']';
			}
			else if (v.ctor === "[]")
			{
				return "[]";
			}
			else if (v.ctor === "RBNode" || v.ctor === "RBEmpty")
			{
				if (!Dict)
				{
					Dict = Elm.Dict.make(localRuntime);
				}
				if (!List)
				{
					List = Elm.List.make(localRuntime);
				}
				var list = Dict.toList(v);
				var name = "Dict";
				if (list.ctor === "::" && list._0._1.ctor === "_Tuple0")
				{
					name = "Set";
					list = A2(List.map, function(x){return x._0}, list);
				}
				return name + ".fromList " + toString(list);
			}
			else if (v.ctor.slice(0,5) === "Text:")
			{
				return '<text>'
			}
			else
			{
				var output = "";
				for (var i in v)
				{
					if (i === 'ctor') continue;
					var str = toString(v[i]);
					var parenless = str[0] === '{' || str[0] === '<' || str.indexOf(' ') < 0;
					output += ' ' + (parenless ? str : '(' + str + ')');
				}
				return v.ctor + output;
			}
		}
		if (type === 'object' && 'notify' in v && 'id' in v)
		{
			return 'initialValue' in v
				? '<Signal>'
				: '<Stream>';
		}
		return "<internal structure>";
	};

	function addSlashes(str, isChar)
	{
		var s = str.replace(/\\/g, '\\\\')
				  .replace(/\n/g, '\\n')
				  .replace(/\t/g, '\\t')
				  .replace(/\r/g, '\\r')
				  .replace(/\v/g, '\\v')
				  .replace(/\0/g, '\\0');
		if (isChar)
		{
			return s.replace(/\'/g, "\\'")
		}
		else
		{
			return s.replace(/\"/g, '\\"');
		}
	}

	function probablyPublic(v)
	{
		var keys = Object.keys(v);
		var len = keys.length;
		if (len === 3
			&& 'props' in v
			&& 'element' in v)
		{
			return false;
		}
		else if (len === 5
			&& 'horizontal' in v
			&& 'vertical' in v
			&& 'x' in v
			&& 'y' in v)
		{
			return false;
		}
		else if (len === 7
			&& 'theta' in v
			&& 'scale' in v
			&& 'x' in v
			&& 'y' in v
			&& 'alpha' in v
			&& 'form' in v)
		{
			return false;
		}
		return true;
	}

	return localRuntime.Native.Show.values = {
		toString: toString
	};
};

Elm.Native.Signal = {};
Elm.Native.Signal.make = function(localRuntime) {

	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Signal = localRuntime.Native.Signal || {};
	if (localRuntime.Native.Signal.values)
	{
		return localRuntime.Native.Signal.values;
	}


	var Task = Elm.Native.Task.make(localRuntime);
	var Utils = Elm.Native.Utils.make(localRuntime);


	function broadcastToKids(node, timestamp, update)
	{
		var kids = node.kids;
		for (var i = kids.length; i--; )
		{
			kids[i].notify(timestamp, update, node.id);
		}
	}


	// INPUT

	function input(name, base)
	{
		var node = {
			id: Utils.guid(),
			name: 'input-' + name,
			value: base,
			parents: [],
			kids: []
		};

		node.notify = function(timestamp, targetId, value) {
			var update = targetId === node.id;
			if (update)
			{
				node.value = value;
			}
			broadcastToKids(node, timestamp, update);
			return update;
		};

		localRuntime.inputs.push(node);

		return node;
	}

	function constant(value)
	{
		return input('constant', value);
	}


	// MAILBOX

	function mailbox(base)
	{
		var signal = input('mailbox', base);

		function send(value) {
			return Task.asyncFunction(function(callback) {
				localRuntime.setTimeout(function() {
					localRuntime.notify(signal.id, value);
				}, 0);
				callback(Task.succeed(Utils.Tuple0));
			});
		}

		return {
			_: {},
			signal: signal,
			address: {
				ctor: 'Address',
				_0: send
			}
		};
	}

	function sendMessage(message)
	{
		Task.perform(message._0);
	}


	// OUTPUT

	function output(name, handler, parent)
	{
		var node = {
			id: Utils.guid(),
			name: 'output-' + name,
			parents: [parent],
			isOutput: true
		};

		node.notify = function(timestamp, parentUpdate, parentID)
		{
			if (parentUpdate)
			{
				handler(parent.value);
			}
		};

		parent.kids.push(node);

		return node;
	}


	// MAP

	function mapMany(refreshValue, args)
	{
		var node = {
			id: Utils.guid(),
			name: 'map' + args.length,
			value: refreshValue(),
			parents: args,
			kids: []
		};

		var numberOfParents = args.length;
		var count = 0;
		var update = false;

		node.notify = function(timestamp, parentUpdate, parentID)
		{
			++count;

			update = update || parentUpdate;

			if (count === numberOfParents)
			{
				if (update)
				{
					node.value = refreshValue();
				}
				broadcastToKids(node, timestamp, update);
				update = false;
				count = 0;
			}
		};

		for (var i = numberOfParents; i--; )
		{
			args[i].kids.push(node);
		}

		return node;
	}


	function map(func, a)
	{
		function refreshValue()
		{
			return func(a.value);
		}
		return mapMany(refreshValue, [a]);
	}


	function map2(func, a, b)
	{
		function refreshValue()
		{
			return A2( func, a.value, b.value );
		}
		return mapMany(refreshValue, [a,b]);
	}


	function map3(func, a, b, c)
	{
		function refreshValue()
		{
			return A3( func, a.value, b.value, c.value );
		}
		return mapMany(refreshValue, [a,b,c]);
	}


	function map4(func, a, b, c, d)
	{
		function refreshValue()
		{
			return A4( func, a.value, b.value, c.value, d.value );
		}
		return mapMany(refreshValue, [a,b,c,d]);
	}


	function map5(func, a, b, c, d, e)
	{
		function refreshValue()
		{
			return A5( func, a.value, b.value, c.value, d.value, e.value );
		}
		return mapMany(refreshValue, [a,b,c,d,e]);
	}



	// FOLD

	function foldp(update, state, signal)
	{
		var node = {
			id: Utils.guid(),
			name: 'foldp',
			parents: [signal],
			kids: [],
			value: state
		};

		node.notify = function(timestamp, parentUpdate, parentID)
		{
			if (parentUpdate)
			{
				node.value = A2( update, signal.value, node.value );
			}
			broadcastToKids(node, timestamp, parentUpdate);
		};

		signal.kids.push(node);

		return node;
	}


	// TIME

	function timestamp(signal)
	{
		var node = {
			id: Utils.guid(),
			name: 'timestamp',
			value: Utils.Tuple2(localRuntime.timer.programStart, signal.value),
			parents: [signal],
			kids: []
		};

		node.notify = function(timestamp, parentUpdate, parentID)
		{
			if (parentUpdate)
			{
				node.value = Utils.Tuple2(timestamp, signal.value);
			}
			broadcastToKids(node, timestamp, parentUpdate);
		};

		signal.kids.push(node);

		return node;
	}


	function delay(time, signal)
	{
		var delayed = input('delay-input-' + time, signal.value);

		function handler(value)
		{
			setTimeout(function() {
				localRuntime.notify(delayed.id, value);
			}, time);
		}

		output('delay-output-' + time, handler, signal);

		return delayed;
	}


	// MERGING

	function genericMerge(tieBreaker, leftStream, rightStream)
	{
		var node = {
			id: Utils.guid(),
			name: 'merge',
			value: A2(tieBreaker, leftStream.value, rightStream.value),
			parents: [leftStream, rightStream],
			kids: []
		};

		var left = { touched: false, update: false, value: null };
		var right = { touched: false, update: false, value: null };

		node.notify = function(timestamp, parentUpdate, parentID)
		{
			if (parentID === leftStream.id)
			{
				left.touched = true;
				left.update = parentUpdate;
				left.value = leftStream.value;
			}
			if (parentID === rightStream.id)
			{
				right.touched = true;
				right.update = parentUpdate;
				right.value = rightStream.value;
			}

			if (left.touched && right.touched)
			{
				var update = false;
				if (left.update && right.update)
				{
					node.value = A2(tieBreaker, left.value, right.value);
					update = true;
				}
				else if (left.update)
				{
					node.value = left.value;
					update = true;
				}
				else if (right.update)
				{
					node.value = right.value;
					update = true;
				}
				left.touched = false;
				right.touched = false;

				broadcastToKids(node, timestamp, update);
			}
		};

		leftStream.kids.push(node);
		rightStream.kids.push(node);

		return node;
	}


	// FILTERING

	function filterMap(toMaybe, base, signal)
	{
		var maybe = toMaybe(signal.value);
		var node = {
			id: Utils.guid(),
			name: 'filterMap',
			value: maybe.ctor === 'Nothing' ? base : maybe._0,
			parents: [signal],
			kids: []
		};

		node.notify = function(timestamp, parentUpdate, parentID)
		{
			var update = false;
			if (parentUpdate)
			{
				var maybe = toMaybe(signal.value);
				if (maybe.ctor === 'Just')
				{
					update = true;
					node.value = maybe._0;
				}
			}
			broadcastToKids(node, timestamp, update);
		};

		signal.kids.push(node);

		return node;
	}


	// SAMPLING

	function sampleOn(ticker, signal)
	{
		var node = {
			id: Utils.guid(),
			name: 'sampleOn',
			value: signal.value,
			parents: [ticker, signal],
			kids: []
		};

		var signalTouch = false;
		var tickerTouch = false;
		var tickerUpdate = false;

		node.notify = function(timestamp, parentUpdate, parentID)
		{
			if (parentID === ticker.id)
			{
				tickerTouch = true;
				tickerUpdate = parentUpdate;
			}
			if (parentID === signal.id)
			{
				signalTouch = true;
			}

			if (tickerTouch && signalTouch)
			{
				if (tickerUpdate)
				{
					node.value = signal.value;
				}
				tickerTouch = false;
				signalTouch = false;

				broadcastToKids(node, timestamp, tickerUpdate);
			}
		};

		ticker.kids.push(node);
		signal.kids.push(node);

		return node;
	}


	// DROP REPEATS

	function dropRepeats(signal)
	{
		var node = {
			id: Utils.guid(),
			name: 'dropRepeats',
			value: signal.value,
			parents: [signal],
			kids: []
		};

		node.notify = function(timestamp, parentUpdate, parentID)
		{
			var update = false;
			if (parentUpdate && !Utils.eq(node.value, signal.value))
			{
				node.value = signal.value;
				update = true;
			}
			broadcastToKids(node, timestamp, update);
		};

		signal.kids.push(node);

		return node;
	}


	return localRuntime.Native.Signal.values = {
		input: input,
		constant: constant,
		mailbox: mailbox,
		sendMessage: sendMessage,
		output: output,
		map: F2(map),
		map2: F3(map2),
		map3: F4(map3),
		map4: F5(map4),
		map5: F6(map5),
		foldp: F3(foldp),
		genericMerge: F3(genericMerge),
		filterMap: F3(filterMap),
		sampleOn: F2(sampleOn),
		dropRepeats: dropRepeats,
		timestamp: timestamp,
		delay: F2(delay)
	};
};

Elm.Native.String = {};
Elm.Native.String.make = function(localRuntime) {

	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.String = localRuntime.Native.String || {};
	if (localRuntime.Native.String.values)
	{
		return localRuntime.Native.String.values;
	}
	if ('values' in Elm.Native.String)
	{
		return localRuntime.Native.String.values = Elm.Native.String.values;
	}


	var Char = Elm.Char.make(localRuntime);
	var List = Elm.Native.List.make(localRuntime);
	var Maybe = Elm.Maybe.make(localRuntime);
	var Result = Elm.Result.make(localRuntime);
	var Utils = Elm.Native.Utils.make(localRuntime);

	function isEmpty(str)
	{
		return str.length === 0;
	}
	function cons(chr,str)
	{
		return chr + str;
	}
	function uncons(str)
	{
		var hd;
		return (hd = str[0])
			? Maybe.Just(Utils.Tuple2(Utils.chr(hd), str.slice(1)))
			: Maybe.Nothing;
	}
	function append(a,b)
	{
		return a + b;
	}
	function concat(strs)
	{
		return List.toArray(strs).join('');
	}
	function length(str)
	{
		return str.length;
	}
	function map(f,str)
	{
		var out = str.split('');
		for (var i = out.length; i--; )
		{
			out[i] = f(Utils.chr(out[i]));
		}
		return out.join('');
	}
	function filter(pred,str)
	{
		return str.split('').map(Utils.chr).filter(pred).join('');
	}
	function reverse(str)
	{
		return str.split('').reverse().join('');
	}
	function foldl(f,b,str)
	{
		var len = str.length;
		for (var i = 0; i < len; ++i)
		{
			b = A2(f, Utils.chr(str[i]), b);
		}
		return b;
	}
	function foldr(f,b,str)
	{
		for (var i = str.length; i--; )
		{
			b = A2(f, Utils.chr(str[i]), b);
		}
		return b;
	}

	function split(sep, str)
	{
		return List.fromArray(str.split(sep));
	}
	function join(sep, strs)
	{
		return List.toArray(strs).join(sep);
	}
	function repeat(n, str)
	{
		var result = '';
		while (n > 0)
		{
			if (n & 1)
			{
				result += str;
			}
			n >>= 1, str += str;
		}
		return result;
	}

	function slice(start, end, str)
	{
		return str.slice(start,end);
	}
	function left(n, str)
	{
		return n < 1 ? "" : str.slice(0,n);
	}
	function right(n, str)
	{
		return n < 1 ? "" : str.slice(-n);
	}
	function dropLeft(n, str)
	{
		return n < 1 ? str : str.slice(n);
	}
	function dropRight(n, str)
	{
		return n < 1 ? str : str.slice(0,-n);
	}

	function pad(n,chr,str)
	{
		var half = (n - str.length) / 2;
		return repeat(Math.ceil(half),chr) + str + repeat(half|0,chr);
	}
	function padRight(n,chr,str)
	{
		return str + repeat(n - str.length, chr);
	}
	function padLeft(n,chr,str)
	{
		return repeat(n - str.length, chr) + str;
	}

	function trim(str)
	{
		return str.trim();
	}
	function trimLeft(str)
	{
		return str.trimLeft();
	}
	function trimRight(str)
	{
		return str.trimRight();
	}

	function words(str)
	{
		return List.fromArray(str.trim().split(/\s+/g));
	}
	function lines(str)
	{
		return List.fromArray(str.split(/\r\n|\r|\n/g));
	}

	function toUpper(str)
	{
		return str.toUpperCase();
	}
	function toLower(str)
	{
		return str.toLowerCase();
	}

	function any(pred, str)
	{
		for (var i = str.length; i--; )
		{
			if (pred(Utils.chr(str[i])))
			{
				return true;
			}
		}
		return false;
	}
	function all(pred, str)
	{
		for (var i = str.length; i--; )
		{
			if (!pred(Utils.chr(str[i])))
			{
				return false;
			}
		}
		return true;
	}

	function contains(sub, str)
	{
		return str.indexOf(sub) > -1;
	}
	function startsWith(sub, str)
	{
		return str.indexOf(sub) === 0;
	}
	function endsWith(sub, str)
	{
		return str.length >= sub.length &&
			str.lastIndexOf(sub) === str.length - sub.length;
	}
	function indexes(sub, str)
	{
		var subLen = sub.length;
		var i = 0;
		var is = [];
		while ((i = str.indexOf(sub, i)) > -1)
		{
			is.push(i);
			i = i + subLen;
		}
		return List.fromArray(is);
	}

	function toInt(s)
	{
		var len = s.length;
		if (len === 0)
		{
			return Result.Err("could not convert string '" + s + "' to an Int" );
		}
		var start = 0;
		if (s[0] == '-')
		{
			if (len === 1)
			{
				return Result.Err("could not convert string '" + s + "' to an Int" );
			}
			start = 1;
		}
		for (var i = start; i < len; ++i)
		{
			if (!Char.isDigit(s[i]))
			{
				return Result.Err("could not convert string '" + s + "' to an Int" );
			}
		}
		return Result.Ok(parseInt(s, 10));
	}

	function toFloat(s)
	{
		var len = s.length;
		if (len === 0)
		{
			return Result.Err("could not convert string '" + s + "' to a Float" );
		}
		var start = 0;
		if (s[0] == '-')
		{
			if (len === 1)
			{
				return Result.Err("could not convert string '" + s + "' to a Float" );
			}
			start = 1;
		}
		var dotCount = 0;
		for (var i = start; i < len; ++i)
		{
			if (Char.isDigit(s[i]))
			{
				continue;
			}
			if (s[i] === '.')
			{
				dotCount += 1;
				if (dotCount <= 1)
				{
					continue;
				}
			}
			return Result.Err("could not convert string '" + s + "' to a Float" );
		}
		return Result.Ok(parseFloat(s));
	}

	function toList(str)
	{
		return List.fromArray(str.split('').map(Utils.chr));
	}
	function fromList(chars)
	{
		return List.toArray(chars).join('');
	}

	return Elm.Native.String.values = {
		isEmpty: isEmpty,
		cons: F2(cons),
		uncons: uncons,
		append: F2(append),
		concat: concat,
		length: length,
		map: F2(map),
		filter: F2(filter),
		reverse: reverse,
		foldl: F3(foldl),
		foldr: F3(foldr),

		split: F2(split),
		join: F2(join),
		repeat: F2(repeat),

		slice: F3(slice),
		left: F2(left),
		right: F2(right),
		dropLeft: F2(dropLeft),
		dropRight: F2(dropRight),

		pad: F3(pad),
		padLeft: F3(padLeft),
		padRight: F3(padRight),

		trim: trim,
		trimLeft: trimLeft,
		trimRight: trimRight,

		words: words,
		lines: lines,

		toUpper: toUpper,
		toLower: toLower,

		any: F2(any),
		all: F2(all),

		contains: F2(contains),
		startsWith: F2(startsWith),
		endsWith: F2(endsWith),
		indexes: F2(indexes),

		toInt: toInt,
		toFloat: toFloat,
		toList: toList,
		fromList: fromList
	};
};

Elm.Native.Task = {};
Elm.Native.Task.make = function(localRuntime) {

	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Task = localRuntime.Native.Task || {};
	if (localRuntime.Native.Task.values)
	{
		return localRuntime.Native.Task.values;
	}

	var Result = Elm.Result.make(localRuntime);
	var Signal;
	var Utils = Elm.Native.Utils.make(localRuntime);


	// CONSTRUCTORS

	function succeed(value)
	{
		return {
			tag: 'Succeed',
			value: value
		};
	}

	function fail(error)
	{
		return {
			tag: 'Fail',
			value: error
		};
	}

	function asyncFunction(func)
	{
		return {
			tag: 'Async',
			asyncFunction: func
		};
	}

	function andThen(task, callback)
	{
		return {
			tag: 'AndThen',
			task: task,
			callback: callback
		};
	}

	function catch_(task, callback)
	{
		return {
			tag: 'Catch',
			task: task,
			callback: callback
		};
	}


	// RUNNER

	function perform(task) {
		runTask({ task: task }, function() {});
	}

	function performSignal(name, signal)
	{
		var workQueue = [];

		function onComplete()
		{
			workQueue.shift();

			setTimeout(function() {
				if (workQueue.length > 0)
				{
					runTask(workQueue[0], onComplete);
				}
			}, 0);
		}

		function register(task)
		{
			var root = { task: task };
			workQueue.push(root);
			if (workQueue.length === 1)
			{
				runTask(root, onComplete);
			}
		}

		if (!Signal)
		{
			Signal = Elm.Native.Signal.make(localRuntime);
		}
		Signal.output('perform-tasks-' + name, register, signal);

		register(signal.value);

		return signal;
	}

	function mark(status, task)
	{
		return { status: status, task: task };
	}

	function runTask(root, onComplete)
	{
		var result = mark('runnable', root.task);
		while (result.status === 'runnable')
		{
			result = stepTask(onComplete, root, result.task);
		}

		if (result.status === 'done')
		{
			root.task = result.task;
			onComplete();
		}

		if (result.status === 'blocked')
		{
			root.task = result.task;
		}
	}

	function stepTask(onComplete, root, task)
	{
		var tag = task.tag;

		if (tag === 'Succeed' || tag === 'Fail')
		{
			return mark('done', task);
		}

		if (tag === 'Async')
		{
			var placeHolder = {};
			var couldBeSync = true;
			var wasSync = false;

			task.asyncFunction(function(result) {
				placeHolder.tag = result.tag;
				placeHolder.value = result.value;
				if (couldBeSync)
				{
					wasSync = true;
				}
				else
				{
					runTask(root, onComplete);
				}
			});
			couldBeSync = false;
			return mark(wasSync ? 'done' : 'blocked', placeHolder);
		}

		if (tag === 'AndThen' || tag === 'Catch')
		{
			var result = mark('runnable', task.task);
			while (result.status === 'runnable')
			{
				result = stepTask(onComplete, root, result.task);
			}

			if (result.status === 'done')
			{
				var activeTask = result.task;
				var activeTag = activeTask.tag;

				var succeedChain = activeTag === 'Succeed' && tag === 'AndThen';
				var failChain = activeTag === 'Fail' && tag === 'Catch';

				return (succeedChain || failChain)
					? mark('runnable', task.callback(activeTask.value))
					: mark('runnable', activeTask);
			}
			if (result.status === 'blocked')
			{
				return mark('blocked', {
					tag: tag,
					task: result.task,
					callback: task.callback
				});
			}
		}
	}


	// THREADS

	function sleep(time) {
		return asyncFunction(function(callback) {
			setTimeout(function() {
				callback(succeed(Utils.Tuple0));
			}, time);
		});
	}

	function spawn(task) {
		return asyncFunction(function(callback) {
			var id = setTimeout(function() {
				perform(task);
			}, 0);
			callback(succeed(id));
		});
	}


	return localRuntime.Native.Task.values = {
		succeed: succeed,
		fail: fail,
		asyncFunction: asyncFunction,
		andThen: F2(andThen),
		catch_: F2(catch_),
		perform: perform,
		performSignal: performSignal,
		spawn: spawn,
		sleep: sleep
	};
};

Elm.Native.Text = {};
Elm.Native.Text.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Text = localRuntime.Native.Text || {};
	if (localRuntime.Native.Text.values)
	{
		return localRuntime.Native.Text.values;
	}

	var toCss = Elm.Native.Color.make(localRuntime).toCss;
	var List = Elm.Native.List.make(localRuntime);


	// CONSTRUCTORS

	function fromString(str)
	{
		return {
			ctor: 'Text:Text',
			_0: str
		};
	}

	function append(a, b)
	{
		return {
			ctor: 'Text:Append',
			_0: a,
			_1: b
		};
	}

	function addMeta(field, value, text)
	{
		var newProps = {};
		var newText = {
			ctor: 'Text:Meta',
			_0: newProps,
			_1: text
		};

		if (text.ctor === 'Text:Meta')
		{
			newText._1 = text._1;
			var props = text._0;
			for (var i = metaKeys.length; i--; )
			{
				var key = metaKeys[i];
				var val = props[key];
				if (val)
				{
					newProps[key] = val;
				}
			}
		}
		newProps[field] = value;
		return newText;
	}

	var metaKeys = [
		'font-size',
		'font-family',
		'font-style',
		'font-weight',
		'href',
		'text-decoration',
		'color'
	];


	// conversions from Elm values to CSS

	function toTypefaces(list)
	{
		var typefaces = List.toArray(list);
		for (var i = typefaces.length; i--; )
		{
			var typeface = typefaces[i];
			if (typeface.indexOf(' ') > -1)
			{
				typefaces[i] = "'" + typeface + "'";
			}
		}
		return typefaces.join(',');
	}

	function toLine(line)
	{
		var ctor = line.ctor;
		return ctor === 'Under'
			? 'underline'
			: ctor === 'Over'
				? 'overline'
				: 'line-through';
	}

	// setting styles of Text

	function style(style, text)
	{
		var newText = addMeta('color', toCss(style.color), text);
		var props = newText._0;

		if (style.typeface.ctor !== '[]')
		{
			props['font-family'] = toTypefaces(style.typeface);
		}
		if (style.height.ctor !== "Nothing")
		{
			props['font-size'] = style.height._0 + 'px';
		}
		if (style.bold)
		{
			props['font-weight'] = 'bold';
		}
		if (style.italic)
		{
			props['font-style'] = 'italic';
		}
		if (style.line.ctor !== 'Nothing')
		{
			props['text-decoration'] = toLine(style.line._0);
		}
		return newText;
	}

	function height(px, text)
	{
		return addMeta('font-size', px + 'px', text);
	}

	function typeface(names, text)
	{
		return addMeta('font-family', toTypefaces(names), text);
	}

	function monospace(text)
	{
		return addMeta('font-family', 'monospace', text);
	}

	function italic(text)
	{
		return addMeta('font-style', 'italic', text);
	}

	function bold(text)
	{
		return addMeta('font-weight', 'bold', text);
	}

	function link(href, text)
	{
		return addMeta('href', href, text);
	}

	function line(line, text)
	{
		return addMeta('text-decoration', toLine(line), text);
	}

	function color(color, text)
	{
		return addMeta('color', toCss(color), text);;
	}


	// RENDER

	function renderHtml(text)
	{
		var tag = text.ctor;
		if (tag === 'Text:Append')
		{
			return renderHtml(text._0) + renderHtml(text._1);
		}
		if (tag === 'Text:Text')
		{
			return properEscape(text._0);
		}
		if (tag === 'Text:Meta')
		{
			return renderMeta(text._0, renderHtml(text._1));
		}
	}

	function renderMeta(metas, string)
	{
		var href = metas['href'];
		if (href)
		{
			string = '<a href="' + href + '">' + string + '</a>';
		}
		var styles = '';
		for (var key in metas)
		{
			if (key === 'href')
			{
				continue;
			}
			styles += key + ':' + metas[key] + ';';
		}
		if (styles)
		{
			string = '<span style="' + styles + '">' + string + '</span>';
		}
		return string;
	}

	function properEscape(str)
	{
		if (str.length == 0)
		{
			return str;
		}
		str = str //.replace(/&/g,  "&#38;")
			.replace(/"/g,  '&#34;')
			.replace(/'/g,  "&#39;")
			.replace(/</g,  "&#60;")
			.replace(/>/g,  "&#62;");
		var arr = str.split('\n');
		for (var i = arr.length; i--; )
		{
			arr[i] = makeSpaces(arr[i]);
		}
		return arr.join('<br/>');
	}

	function makeSpaces(s)
	{
		if (s.length == 0)
		{
			return s;
		}
		var arr = s.split('');
		if (arr[0] == ' ')
		{
			arr[0] = "&nbsp;"
		}
		for (var i = arr.length; --i; )
		{
			if (arr[i][0] == ' ' && arr[i-1] == ' ')
			{
				arr[i-1] = arr[i-1] + arr[i];
				arr[i] = '';
			}
		}
		for (var i = arr.length; i--; )
		{
			if (arr[i].length > 1 && arr[i][0] == ' ')
			{
				var spaces = arr[i].split('');
				for (var j = spaces.length - 2; j >= 0; j -= 2)
				{
					spaces[j] = '&nbsp;';
				}
				arr[i] = spaces.join('');
			}
		}
		arr = arr.join('');
		if (arr[arr.length-1] === " ")
		{
			return arr.slice(0,-1) + '&nbsp;';
		}
		return arr;
	}


	return localRuntime.Native.Text.values = {
		fromString: fromString,
		append: F2(append),

		height: F2(height),
		italic: italic,
		bold: bold,
		line: F2(line),
		monospace: monospace,
		typeface: F2(typeface),
		color: F2(color),
		link: F2(link),
		style: F2(style),

		toTypefaces: toTypefaces,
		toLine: toLine,
		renderHtml: renderHtml
	};
};

Elm.Native.Time = {};
Elm.Native.Time.make = function(localRuntime)
{

	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Time = localRuntime.Native.Time || {};
	if (localRuntime.Native.Time.values)
	{
		return localRuntime.Native.Time.values;
	}

	var NS = Elm.Native.Signal.make(localRuntime);
	var Maybe = Elm.Maybe.make(localRuntime);


	// FRAMES PER SECOND

	function fpsWhen(desiredFPS, isOn)
	{
		var msPerFrame = 1000 / desiredFPS;
		var ticker = NS.input('fps-' + desiredFPS, null);

		function notifyTicker()
		{
			localRuntime.notify(ticker.id, null);
		}

		function firstArg(x, y)
		{
			return x;
		}

		// input fires either when isOn changes, or when ticker fires.
		// Its value is a tuple with the current timestamp, and the state of isOn
		var input = NS.timestamp(A3(NS.map2, F2(firstArg), NS.dropRepeats(isOn), ticker));

		var initialState = {
			isOn: false,
			time: localRuntime.timer.programStart,
			delta: 0
		};

		var timeoutId;

		function update(input,state)
		{
			var currentTime = input._0;
			var isOn = input._1;
			var wasOn = state.isOn;
			var previousTime = state.time;

			if (isOn)
			{
				timeoutId = localRuntime.setTimeout(notifyTicker, msPerFrame);
			}
			else if (wasOn)
			{
				clearTimeout(timeoutId);
			}

			return {
				isOn: isOn,
				time: currentTime,
				delta: (isOn && !wasOn) ? 0 : currentTime - previousTime
			};
		}

		return A2(
			NS.map,
			function(state) { return state.delta; },
			A3(NS.foldp, F2(update), update(input.value,initialState), input)
		);
	}


	// EVERY

	function every(t)
	{
		var ticker = NS.input('every-' + t, null);
		function tellTime()
		{
			localRuntime.notify(ticker.id, null);
		}
		var clock = A2( NS.map, fst, NS.timestamp(ticker) );
		setInterval(tellTime, t);
		return clock;
	}


	function fst(pair)
	{
		return pair._0;
	}


	function read(s)
	{
		var t = Date.parse(s);
		return isNaN(t) ? Maybe.Nothing : Maybe.Just(t);
	}

	return localRuntime.Native.Time.values = {
		fpsWhen: F2(fpsWhen),
		every: every,
		toDate: function(t) { return new window.Date(t); },
		read: read
	};

};

Elm.Native.Transform2D = {};
Elm.Native.Transform2D.make = function(localRuntime) {

	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Transform2D = localRuntime.Native.Transform2D || {};
	if (localRuntime.Native.Transform2D.values)
	{
		return localRuntime.Native.Transform2D.values;
	}

	var A;
	if (typeof Float32Array === 'undefined')
	{
		A = function(arr)
		{
			this.length = arr.length;
			this[0] = arr[0];
			this[1] = arr[1];
			this[2] = arr[2];
			this[3] = arr[3];
			this[4] = arr[4];
			this[5] = arr[5];
		};
	}
	else
	{
		A = Float32Array;
	}

	// layout of matrix in an array is
	//
	//   | m11 m12 dx |
	//   | m21 m22 dy |
	//   |  0   0   1 |
	//
	//  new A([ m11, m12, dx, m21, m22, dy ])

	var identity = new A([1,0,0,0,1,0]);
	function matrix(m11, m12, m21, m22, dx, dy)
	{
		return new A([m11, m12, dx, m21, m22, dy]);
	}

	function rotation(t)
	{
		var c = Math.cos(t);
		var s = Math.sin(t);
		return new A([c, -s, 0, s, c, 0]);
	}

	function rotate(t,m)
	{
		var c = Math.cos(t);
		var s = Math.sin(t);
		var m11 = m[0], m12 = m[1], m21 = m[3], m22 = m[4];
		return new A([m11*c + m12*s, -m11*s + m12*c, m[2],
					  m21*c + m22*s, -m21*s + m22*c, m[5]]);
	}
	/*
	function move(xy,m) {
		var x = xy._0;
		var y = xy._1;
		var m11 = m[0], m12 = m[1], m21 = m[3], m22 = m[4];
		return new A([m11, m12, m11*x + m12*y + m[2],
					  m21, m22, m21*x + m22*y + m[5]]);
	}
	function scale(s,m) { return new A([m[0]*s, m[1]*s, m[2], m[3]*s, m[4]*s, m[5]]); }
	function scaleX(x,m) { return new A([m[0]*x, m[1], m[2], m[3]*x, m[4], m[5]]); }
	function scaleY(y,m) { return new A([m[0], m[1]*y, m[2], m[3], m[4]*y, m[5]]); }
	function reflectX(m) { return new A([-m[0], m[1], m[2], -m[3], m[4], m[5]]); }
	function reflectY(m) { return new A([m[0], -m[1], m[2], m[3], -m[4], m[5]]); }

	function transform(m11, m21, m12, m22, mdx, mdy, n) {
		var n11 = n[0], n12 = n[1], n21 = n[3], n22 = n[4], ndx = n[2], ndy = n[5];
		return new A([m11*n11 + m12*n21,
					  m11*n12 + m12*n22,
					  m11*ndx + m12*ndy + mdx,
					  m21*n11 + m22*n21,
					  m21*n12 + m22*n22,
					  m21*ndx + m22*ndy + mdy]);
	}
	*/
	function multiply(m, n)
	{
		var m11 = m[0], m12 = m[1], m21 = m[3], m22 = m[4], mdx = m[2], mdy = m[5];
		var n11 = n[0], n12 = n[1], n21 = n[3], n22 = n[4], ndx = n[2], ndy = n[5];
		return new A([m11*n11 + m12*n21,
					  m11*n12 + m12*n22,
					  m11*ndx + m12*ndy + mdx,
					  m21*n11 + m22*n21,
					  m21*n12 + m22*n22,
					  m21*ndx + m22*ndy + mdy]);
	}

	return localRuntime.Native.Transform2D.values = {
		identity:identity,
		matrix:F6(matrix),
		rotation:rotation,
		multiply:F2(multiply)
		/*
		transform:F7(transform),
		rotate:F2(rotate),
		move:F2(move),
		scale:F2(scale),
		scaleX:F2(scaleX),
		scaleY:F2(scaleY),
		reflectX:reflectX,
		reflectY:reflectY
		*/
	};

};

Elm.Native = Elm.Native || {};
Elm.Native.Utils = {};
Elm.Native.Utils.make = function(localRuntime) {

	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Utils = localRuntime.Native.Utils || {};
	if (localRuntime.Native.Utils.values)
	{
		return localRuntime.Native.Utils.values;
	}

	function eq(l,r)
	{
		var stack = [{'x': l, 'y': r}]
		while (stack.length > 0)
		{
			var front = stack.pop();
			var x = front.x;
			var y = front.y;
			if (x === y)
			{
				continue;
			}
			if (typeof x === "object")
			{
				var c = 0;
				for (var i in x)
				{
					++c;
					if (i in y)
					{
						if (i !== 'ctor')
						{
							stack.push({ 'x': x[i], 'y': y[i] });
						}
					}
					else
					{
						return false;
					}
				}
				if ('ctor' in x)
				{
					stack.push({'x': x.ctor, 'y': y.ctor});
				}
				if (c !== Object.keys(y).length)
				{
					return false;
				}
			}
			else if (typeof x === 'function')
			{
				throw new Error('Equality error: general function equality is ' +
								'undecidable, and therefore, unsupported');
			}
			else
			{
				return false;
			}
		}
		return true;
	}

	// code in Generate/JavaScript.hs depends on the particular
	// integer values assigned to LT, EQ, and GT
	var LT = -1, EQ = 0, GT = 1, ord = ['LT','EQ','GT'];

	function compare(x,y)
	{
		return {
			ctor: ord[cmp(x,y)+1]
		};
	}

	function cmp(x,y) {
		var ord;
		if (typeof x !== 'object')
		{
			return x === y ? EQ : x < y ? LT : GT;
		}
		else if (x.isChar)
		{
			var a = x.toString();
			var b = y.toString();
			return a === b
				? EQ
				: a < b
					? LT
					: GT;
		}
		else if (x.ctor === "::" || x.ctor === "[]")
		{
			while (true)
			{
				if (x.ctor === "[]" && y.ctor === "[]")
				{
					return EQ;
				}
				if (x.ctor !== y.ctor)
				{
					return x.ctor === '[]' ? LT : GT;
				}
				ord = cmp(x._0, y._0);
				if (ord !== EQ)
				{
					return ord;
				}
				x = x._1;
				y = y._1;
			}
		}
		else if (x.ctor.slice(0,6) === '_Tuple')
		{
			var n = x.ctor.slice(6) - 0;
			var err = 'cannot compare tuples with more than 6 elements.';
			if (n === 0) return EQ;
			if (n >= 1) { ord = cmp(x._0, y._0); if (ord !== EQ) return ord;
			if (n >= 2) { ord = cmp(x._1, y._1); if (ord !== EQ) return ord;
			if (n >= 3) { ord = cmp(x._2, y._2); if (ord !== EQ) return ord;
			if (n >= 4) { ord = cmp(x._3, y._3); if (ord !== EQ) return ord;
			if (n >= 5) { ord = cmp(x._4, y._4); if (ord !== EQ) return ord;
			if (n >= 6) { ord = cmp(x._5, y._5); if (ord !== EQ) return ord;
			if (n >= 7) throw new Error('Comparison error: ' + err); } } } } } }
			return EQ;
		}
		else
		{
			throw new Error('Comparison error: comparison is only defined on ints, ' +
							'floats, times, chars, strings, lists of comparable values, ' +
							'and tuples of comparable values.');
		}
	}


	var Tuple0 = {
		ctor: "_Tuple0"
	};

	function Tuple2(x,y)
	{
		return {
			ctor: "_Tuple2",
			_0: x,
			_1: y
		};
	}

	function chr(c)
	{
		var x = new String(c);
		x.isChar = true;
		return x;
	}

	function txt(str)
	{
		var t = new String(str);
		t.text = true;
		return t;
	}

	var count = 0;
	function guid(_)
	{
		return count++
	}

	function copy(oldRecord)
	{
		var newRecord = {};
		for (var key in oldRecord)
		{
			var value = key === '_'
				? copy(oldRecord._)
				: oldRecord[key];
			newRecord[key] = value;
		}
		return newRecord;
	}

	function remove(key, oldRecord)
	{
		var record = copy(oldRecord);
		if (key in record._)
		{
			record[key] = record._[key][0];
			record._[key] = record._[key].slice(1);
			if (record._[key].length === 0)
			{
				delete record._[key];
			}
		}
		else
		{
			delete record[key];
		}
		return record;
	}

	function replace(keyValuePairs, oldRecord)
	{
		var record = copy(oldRecord);
		for (var i = keyValuePairs.length; i--; )
		{
			var pair = keyValuePairs[i];
			record[pair[0]] = pair[1];
		}
		return record;
	}

	function insert(key, value, oldRecord)
	{
		var newRecord = copy(oldRecord);
		if (key in newRecord)
		{
			var values = newRecord._[key];
			var copiedValues = values ? values.slice(0) : [];
			newRecord._[key] = [newRecord[key]].concat(copiedValues);
		}
		newRecord[key] = value;
		return newRecord;
	}

	function getXY(e)
	{
		var posx = 0;
		var posy = 0;
		if (e.pageX || e.pageY)
		{
			posx = e.pageX;
			posy = e.pageY;
		}
		else if (e.clientX || e.clientY)
		{
			posx = e.clientX + document.body.scrollLeft + document.documentElement.scrollLeft;
			posy = e.clientY + document.body.scrollTop + document.documentElement.scrollTop;
		}

		if (localRuntime.isEmbed())
		{
			var rect = localRuntime.node.getBoundingClientRect();
			var relx = rect.left + document.body.scrollLeft + document.documentElement.scrollLeft;
			var rely = rect.top + document.body.scrollTop + document.documentElement.scrollTop;
			// TODO: figure out if there is a way to avoid rounding here
			posx = posx - Math.round(relx) - localRuntime.node.clientLeft;
			posy = posy - Math.round(rely) - localRuntime.node.clientTop;
		}
		return Tuple2(posx, posy);
	}


	//// LIST STUFF ////

	var Nil = { ctor:'[]' };

	function Cons(hd,tl)
	{
		return {
			ctor: "::",
			_0: hd,
			_1: tl
		};
	}

	function append(xs,ys)
	{
		// append Strings
		if (typeof xs === "string")
		{
			return xs + ys;
		}

		// append Text
		if (xs.ctor.slice(0,5) === 'Text:')
		{
			return {
				ctor: 'Text:Append',
				_0: xs,
				_1: ys
			};
		}



		// append Lists
		if (xs.ctor === '[]')
		{
			return ys;
		}
		var root = Cons(xs._0, Nil);
		var curr = root;
		xs = xs._1;
		while (xs.ctor !== '[]')
		{
			curr._1 = Cons(xs._0, Nil);
			xs = xs._1;
			curr = curr._1;
		}
		curr._1 = ys;
		return root;
	}

	//// RUNTIME ERRORS ////

	function indent(lines)
	{
		return '\n' + lines.join('\n');
	}

	function badCase(moduleName, span)
	{
		var msg = indent([
			'Non-exhaustive pattern match in case-expression.',
			'Make sure your patterns cover every case!'
		]);
		throw new Error('Runtime error in module ' + moduleName + ' (' + span + ')' + msg);
	}

	function badIf(moduleName, span)
	{
		var msg = indent([
			'Non-exhaustive pattern match in multi-way-if expression.',
			'It is best to use \'otherwise\' as the last branch of multi-way-if.'
		]);
		throw new Error('Runtime error in module ' + moduleName + ' (' + span + ')' + msg);
	}


	function badPort(expected, received)
	{
		var msg = indent([
			'Expecting ' + expected + ' but was given ',
			JSON.stringify(received)
		]);
		throw new Error('Runtime error when sending values through a port.' + msg);
	}


	return localRuntime.Native.Utils.values = {
		eq: eq,
		cmp: cmp,
		compare: F2(compare),
		Tuple0: Tuple0,
		Tuple2: Tuple2,
		chr: chr,
		txt: txt,
		copy: copy,
		remove: remove,
		replace: replace,
		insert: insert,
		guid: guid,
		getXY: getXY,

		Nil: Nil,
		Cons: Cons,
		append: F2(append),

		badCase: badCase,
		badIf: badIf,
		badPort: badPort
	};
};

Elm.Result = Elm.Result || {};
Elm.Result.make = function (_elm) {
   "use strict";
   _elm.Result = _elm.Result || {};
   if (_elm.Result.values)
   return _elm.Result.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Result",
   $Maybe = Elm.Maybe.make(_elm);
   var toMaybe = function (result) {
      return function () {
         switch (result.ctor)
         {case "Err":
            return $Maybe.Nothing;
            case "Ok":
            return $Maybe.Just(result._0);}
         _U.badCase($moduleName,
         "between lines 164 and 177");
      }();
   };
   var Err = function (a) {
      return {ctor: "Err",_0: a};
   };
   var andThen = F2(function (result,
   callback) {
      return function () {
         switch (result.ctor)
         {case "Err":
            return Err(result._0);
            case "Ok":
            return callback(result._0);}
         _U.badCase($moduleName,
         "between lines 126 and 145");
      }();
   });
   var Ok = function (a) {
      return {ctor: "Ok",_0: a};
   };
   var map = F2(function (func,
   ra) {
      return function () {
         switch (ra.ctor)
         {case "Err": return Err(ra._0);
            case "Ok":
            return Ok(func(ra._0));}
         _U.badCase($moduleName,
         "between lines 41 and 52");
      }();
   });
   var map2 = F3(function (func,
   ra,
   rb) {
      return function () {
         var _v9 = {ctor: "_Tuple2"
                   ,_0: ra
                   ,_1: rb};
         switch (_v9.ctor)
         {case "_Tuple2":
            switch (_v9._0.ctor)
              {case "Err":
                 return Err(_v9._0._0);
                 case "Ok": switch (_v9._1.ctor)
                   {case "Ok": return Ok(A2(func,
                        _v9._0._0,
                        _v9._1._0));}
                   break;}
              switch (_v9._1.ctor)
              {case "Err":
                 return Err(_v9._1._0);}
              break;}
         _U.badCase($moduleName,
         "between lines 55 and 58");
      }();
   });
   var map3 = F4(function (func,
   ra,
   rb,
   rc) {
      return function () {
         var _v16 = {ctor: "_Tuple3"
                    ,_0: ra
                    ,_1: rb
                    ,_2: rc};
         switch (_v16.ctor)
         {case "_Tuple3":
            switch (_v16._0.ctor)
              {case "Err":
                 return Err(_v16._0._0);
                 case "Ok": switch (_v16._1.ctor)
                   {case "Ok":
                      switch (_v16._2.ctor)
                        {case "Ok": return Ok(A3(func,
                             _v16._0._0,
                             _v16._1._0,
                             _v16._2._0));}
                        break;}
                   break;}
              switch (_v16._1.ctor)
              {case "Err":
                 return Err(_v16._1._0);}
              switch (_v16._2.ctor)
              {case "Err":
                 return Err(_v16._2._0);}
              break;}
         _U.badCase($moduleName,
         "between lines 63 and 67");
      }();
   });
   var map4 = F5(function (func,
   ra,
   rb,
   rc,
   rd) {
      return function () {
         var _v26 = {ctor: "_Tuple4"
                    ,_0: ra
                    ,_1: rb
                    ,_2: rc
                    ,_3: rd};
         switch (_v26.ctor)
         {case "_Tuple4":
            switch (_v26._0.ctor)
              {case "Err":
                 return Err(_v26._0._0);
                 case "Ok": switch (_v26._1.ctor)
                   {case "Ok":
                      switch (_v26._2.ctor)
                        {case "Ok":
                           switch (_v26._3.ctor)
                             {case "Ok": return Ok(A4(func,
                                  _v26._0._0,
                                  _v26._1._0,
                                  _v26._2._0,
                                  _v26._3._0));}
                             break;}
                        break;}
                   break;}
              switch (_v26._1.ctor)
              {case "Err":
                 return Err(_v26._1._0);}
              switch (_v26._2.ctor)
              {case "Err":
                 return Err(_v26._2._0);}
              switch (_v26._3.ctor)
              {case "Err":
                 return Err(_v26._3._0);}
              break;}
         _U.badCase($moduleName,
         "between lines 72 and 77");
      }();
   });
   var map5 = F6(function (func,
   ra,
   rb,
   rc,
   rd,
   re) {
      return function () {
         var _v39 = {ctor: "_Tuple5"
                    ,_0: ra
                    ,_1: rb
                    ,_2: rc
                    ,_3: rd
                    ,_4: re};
         switch (_v39.ctor)
         {case "_Tuple5":
            switch (_v39._0.ctor)
              {case "Err":
                 return Err(_v39._0._0);
                 case "Ok": switch (_v39._1.ctor)
                   {case "Ok":
                      switch (_v39._2.ctor)
                        {case "Ok":
                           switch (_v39._3.ctor)
                             {case "Ok":
                                switch (_v39._4.ctor)
                                  {case "Ok": return Ok(A5(func,
                                       _v39._0._0,
                                       _v39._1._0,
                                       _v39._2._0,
                                       _v39._3._0,
                                       _v39._4._0));}
                                  break;}
                             break;}
                        break;}
                   break;}
              switch (_v39._1.ctor)
              {case "Err":
                 return Err(_v39._1._0);}
              switch (_v39._2.ctor)
              {case "Err":
                 return Err(_v39._2._0);}
              switch (_v39._3.ctor)
              {case "Err":
                 return Err(_v39._3._0);}
              switch (_v39._4.ctor)
              {case "Err":
                 return Err(_v39._4._0);}
              break;}
         _U.badCase($moduleName,
         "between lines 82 and 123");
      }();
   });
   var formatError = F2(function (f,
   result) {
      return function () {
         switch (result.ctor)
         {case "Err":
            return Err(f(result._0));
            case "Ok":
            return Ok(result._0);}
         _U.badCase($moduleName,
         "between lines 148 and 161");
      }();
   });
   var fromMaybe = F2(function (err,
   maybe) {
      return function () {
         switch (maybe.ctor)
         {case "Just":
            return Ok(maybe._0);
            case "Nothing":
            return Err(err);}
         _U.badCase($moduleName,
         "between lines 180 and 182");
      }();
   });
   _elm.Result.values = {_op: _op
                        ,map: map
                        ,map2: map2
                        ,map3: map3
                        ,map4: map4
                        ,map5: map5
                        ,andThen: andThen
                        ,toMaybe: toMaybe
                        ,fromMaybe: fromMaybe
                        ,formatError: formatError
                        ,Ok: Ok
                        ,Err: Err};
   return _elm.Result.values;
};
Elm.Set = Elm.Set || {};
Elm.Set.make = function (_elm) {
   "use strict";
   _elm.Set = _elm.Set || {};
   if (_elm.Set.values)
   return _elm.Set.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Set",
   $Dict = Elm.Dict.make(_elm),
   $List = Elm.List.make(_elm);
   var partition = F2(function (p,
   set) {
      return A2($Dict.partition,
      F2(function (k,_v0) {
         return function () {
            return p(k);
         }();
      }),
      set);
   });
   var filter = F2(function (p,
   set) {
      return A2($Dict.filter,
      F2(function (k,_v2) {
         return function () {
            return p(k);
         }();
      }),
      set);
   });
   var foldr = F3(function (f,
   b,
   s) {
      return A3($Dict.foldr,
      F3(function (k,_v4,b) {
         return function () {
            return A2(f,k,b);
         }();
      }),
      b,
      s);
   });
   var foldl = F3(function (f,
   b,
   s) {
      return A3($Dict.foldl,
      F3(function (k,_v6,b) {
         return function () {
            return A2(f,k,b);
         }();
      }),
      b,
      s);
   });
   var toList = $Dict.keys;
   var diff = $Dict.diff;
   var intersect = $Dict.intersect;
   var union = $Dict.union;
   var member = $Dict.member;
   var remove = $Dict.remove;
   var insert = function (k) {
      return A2($Dict.insert,
      k,
      {ctor: "_Tuple0"});
   };
   var singleton = function (k) {
      return A2($Dict.singleton,
      k,
      {ctor: "_Tuple0"});
   };
   var empty = $Dict.empty;
   var fromList = function (xs) {
      return A3($List.foldl,
      insert,
      empty,
      xs);
   };
   var map = F2(function (f,s) {
      return fromList(A2($List.map,
      f,
      toList(s)));
   });
   _elm.Set.values = {_op: _op
                     ,empty: empty
                     ,singleton: singleton
                     ,insert: insert
                     ,remove: remove
                     ,member: member
                     ,foldl: foldl
                     ,foldr: foldr
                     ,map: map
                     ,filter: filter
                     ,partition: partition
                     ,union: union
                     ,intersect: intersect
                     ,diff: diff
                     ,toList: toList
                     ,fromList: fromList};
   return _elm.Set.values;
};
Elm.Signal = Elm.Signal || {};
Elm.Signal.make = function (_elm) {
   "use strict";
   _elm.Signal = _elm.Signal || {};
   if (_elm.Signal.values)
   return _elm.Signal.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Signal",
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Signal = Elm.Native.Signal.make(_elm),
   $Task = Elm.Task.make(_elm);
   var send = F2(function (_v0,
   value) {
      return function () {
         switch (_v0.ctor)
         {case "Address":
            return A2($Task.onError,
              _v0._0(value),
              function (_v3) {
                 return function () {
                    return $Task.succeed({ctor: "_Tuple0"});
                 }();
              });}
         _U.badCase($moduleName,
         "between lines 370 and 371");
      }();
   });
   var Message = function (a) {
      return {ctor: "Message"
             ,_0: a};
   };
   var message = F2(function (_v5,
   value) {
      return function () {
         switch (_v5.ctor)
         {case "Address":
            return Message(_v5._0(value));}
         _U.badCase($moduleName,
         "on line 352, column 5 to 24");
      }();
   });
   var mailbox = $Native$Signal.mailbox;
   var Address = function (a) {
      return {ctor: "Address"
             ,_0: a};
   };
   var forwardTo = F2(function (_v8,
   f) {
      return function () {
         switch (_v8.ctor)
         {case "Address":
            return Address(function (x) {
                 return _v8._0(f(x));
              });}
         _U.badCase($moduleName,
         "on line 339, column 5 to 29");
      }();
   });
   var Mailbox = F2(function (a,
   b) {
      return {_: {}
             ,address: a
             ,signal: b};
   });
   var sampleOn = $Native$Signal.sampleOn;
   var dropRepeats = $Native$Signal.dropRepeats;
   var filterMap = $Native$Signal.filterMap;
   var filter = F3(function (isOk,
   base,
   signal) {
      return A3(filterMap,
      function (value) {
         return isOk(value) ? $Maybe.Just(value) : $Maybe.Nothing;
      },
      base,
      signal);
   });
   var merge = F2(function (left,
   right) {
      return A3($Native$Signal.genericMerge,
      $Basics.always,
      left,
      right);
   });
   var mergeMany = function (signalList) {
      return function () {
         var _v11 = $List.reverse(signalList);
         switch (_v11.ctor)
         {case "::":
            return A3($List.foldl,
              merge,
              _v11._0,
              _v11._1);
            case "[]":
            return $Debug.crash("mergeMany was given an empty list!");}
         _U.badCase($moduleName,
         "between lines 177 and 197");
      }();
   };
   var foldp = $Native$Signal.foldp;
   var map5 = $Native$Signal.map5;
   var map4 = $Native$Signal.map4;
   var map3 = $Native$Signal.map3;
   var map2 = $Native$Signal.map2;
   _op["~"] = F2(function (funcs,
   args) {
      return A3(map2,
      F2(function (f,v) {
         return f(v);
      }),
      funcs,
      args);
   });
   var map = $Native$Signal.map;
   _op["<~"] = map;
   var constant = $Native$Signal.constant;
   var Signal = {ctor: "Signal"};
   _elm.Signal.values = {_op: _op
                        ,merge: merge
                        ,mergeMany: mergeMany
                        ,map: map
                        ,map2: map2
                        ,map3: map3
                        ,map4: map4
                        ,map5: map5
                        ,constant: constant
                        ,dropRepeats: dropRepeats
                        ,filter: filter
                        ,filterMap: filterMap
                        ,sampleOn: sampleOn
                        ,foldp: foldp
                        ,mailbox: mailbox
                        ,send: send
                        ,message: message
                        ,forwardTo: forwardTo
                        ,Mailbox: Mailbox};
   return _elm.Signal.values;
};
Elm.String = Elm.String || {};
Elm.String.make = function (_elm) {
   "use strict";
   _elm.String = _elm.String || {};
   if (_elm.String.values)
   return _elm.String.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "String",
   $Maybe = Elm.Maybe.make(_elm),
   $Native$String = Elm.Native.String.make(_elm),
   $Result = Elm.Result.make(_elm);
   var fromList = $Native$String.fromList;
   var toList = $Native$String.toList;
   var toFloat = $Native$String.toFloat;
   var toInt = $Native$String.toInt;
   var indices = $Native$String.indexes;
   var indexes = $Native$String.indexes;
   var endsWith = $Native$String.endsWith;
   var startsWith = $Native$String.startsWith;
   var contains = $Native$String.contains;
   var all = $Native$String.all;
   var any = $Native$String.any;
   var toLower = $Native$String.toLower;
   var toUpper = $Native$String.toUpper;
   var lines = $Native$String.lines;
   var words = $Native$String.words;
   var trimRight = $Native$String.trimRight;
   var trimLeft = $Native$String.trimLeft;
   var trim = $Native$String.trim;
   var padRight = $Native$String.padRight;
   var padLeft = $Native$String.padLeft;
   var pad = $Native$String.pad;
   var dropRight = $Native$String.dropRight;
   var dropLeft = $Native$String.dropLeft;
   var right = $Native$String.right;
   var left = $Native$String.left;
   var slice = $Native$String.slice;
   var repeat = $Native$String.repeat;
   var join = $Native$String.join;
   var split = $Native$String.split;
   var foldr = $Native$String.foldr;
   var foldl = $Native$String.foldl;
   var reverse = $Native$String.reverse;
   var filter = $Native$String.filter;
   var map = $Native$String.map;
   var length = $Native$String.length;
   var concat = $Native$String.concat;
   var append = $Native$String.append;
   var uncons = $Native$String.uncons;
   var cons = $Native$String.cons;
   var fromChar = function ($char) {
      return A2(cons,$char,"");
   };
   var isEmpty = $Native$String.isEmpty;
   _elm.String.values = {_op: _op
                        ,isEmpty: isEmpty
                        ,length: length
                        ,reverse: reverse
                        ,repeat: repeat
                        ,cons: cons
                        ,uncons: uncons
                        ,fromChar: fromChar
                        ,append: append
                        ,concat: concat
                        ,split: split
                        ,join: join
                        ,words: words
                        ,lines: lines
                        ,slice: slice
                        ,left: left
                        ,right: right
                        ,dropLeft: dropLeft
                        ,dropRight: dropRight
                        ,contains: contains
                        ,startsWith: startsWith
                        ,endsWith: endsWith
                        ,indexes: indexes
                        ,indices: indices
                        ,toInt: toInt
                        ,toFloat: toFloat
                        ,toList: toList
                        ,fromList: fromList
                        ,toUpper: toUpper
                        ,toLower: toLower
                        ,pad: pad
                        ,padLeft: padLeft
                        ,padRight: padRight
                        ,trim: trim
                        ,trimLeft: trimLeft
                        ,trimRight: trimRight
                        ,map: map
                        ,filter: filter
                        ,foldl: foldl
                        ,foldr: foldr
                        ,any: any
                        ,all: all};
   return _elm.String.values;
};
Elm.Task = Elm.Task || {};
Elm.Task.make = function (_elm) {
   "use strict";
   _elm.Task = _elm.Task || {};
   if (_elm.Task.values)
   return _elm.Task.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Task",
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Task = Elm.Native.Task.make(_elm),
   $Result = Elm.Result.make(_elm);
   var sleep = $Native$Task.sleep;
   var spawn = $Native$Task.spawn;
   var ThreadID = function (a) {
      return {ctor: "ThreadID"
             ,_0: a};
   };
   var onError = $Native$Task.catch_;
   var andThen = $Native$Task.andThen;
   var fail = $Native$Task.fail;
   var mapError = F2(function (f,
   promise) {
      return A2(onError,
      promise,
      function (err) {
         return fail(f(err));
      });
   });
   var succeed = $Native$Task.succeed;
   var map = F2(function (func,
   promiseA) {
      return A2(andThen,
      promiseA,
      function (a) {
         return succeed(func(a));
      });
   });
   var map2 = F3(function (func,
   promiseA,
   promiseB) {
      return A2(andThen,
      promiseA,
      function (a) {
         return A2(andThen,
         promiseB,
         function (b) {
            return succeed(A2(func,a,b));
         });
      });
   });
   var map3 = F4(function (func,
   promiseA,
   promiseB,
   promiseC) {
      return A2(andThen,
      promiseA,
      function (a) {
         return A2(andThen,
         promiseB,
         function (b) {
            return A2(andThen,
            promiseC,
            function (c) {
               return succeed(A3(func,
               a,
               b,
               c));
            });
         });
      });
   });
   var map4 = F5(function (func,
   promiseA,
   promiseB,
   promiseC,
   promiseD) {
      return A2(andThen,
      promiseA,
      function (a) {
         return A2(andThen,
         promiseB,
         function (b) {
            return A2(andThen,
            promiseC,
            function (c) {
               return A2(andThen,
               promiseD,
               function (d) {
                  return succeed(A4(func,
                  a,
                  b,
                  c,
                  d));
               });
            });
         });
      });
   });
   var map5 = F6(function (func,
   promiseA,
   promiseB,
   promiseC,
   promiseD,
   promiseE) {
      return A2(andThen,
      promiseA,
      function (a) {
         return A2(andThen,
         promiseB,
         function (b) {
            return A2(andThen,
            promiseC,
            function (c) {
               return A2(andThen,
               promiseD,
               function (d) {
                  return A2(andThen,
                  promiseE,
                  function (e) {
                     return succeed(A5(func,
                     a,
                     b,
                     c,
                     d,
                     e));
                  });
               });
            });
         });
      });
   });
   var andMap = F2(function (promiseFunc,
   promiseValue) {
      return A2(andThen,
      promiseFunc,
      function (func) {
         return A2(andThen,
         promiseValue,
         function (value) {
            return succeed(func(value));
         });
      });
   });
   var sequence = function (promises) {
      return function () {
         switch (promises.ctor)
         {case "::": return A3(map2,
              F2(function (x,y) {
                 return A2($List._op["::"],
                 x,
                 y);
              }),
              promises._0,
              sequence(promises._1));
            case "[]":
            return succeed(_L.fromArray([]));}
         _U.badCase($moduleName,
         "between lines 101 and 106");
      }();
   };
   var toMaybe = function (task) {
      return A2(onError,
      A2(map,$Maybe.Just,task),
      function (_v3) {
         return function () {
            return succeed($Maybe.Nothing);
         }();
      });
   };
   var fromMaybe = F2(function ($default,
   maybe) {
      return function () {
         switch (maybe.ctor)
         {case "Just":
            return succeed(maybe._0);
            case "Nothing":
            return fail($default);}
         _U.badCase($moduleName,
         "between lines 139 and 141");
      }();
   });
   var toResult = function (task) {
      return A2(onError,
      A2(map,$Result.Ok,task),
      function (msg) {
         return succeed($Result.Err(msg));
      });
   };
   var fromResult = function (result) {
      return function () {
         switch (result.ctor)
         {case "Err":
            return fail(result._0);
            case "Ok":
            return succeed(result._0);}
         _U.badCase($moduleName,
         "between lines 151 and 153");
      }();
   };
   var Task = {ctor: "Task"};
   _elm.Task.values = {_op: _op
                      ,succeed: succeed
                      ,fail: fail
                      ,map: map
                      ,map2: map2
                      ,map3: map3
                      ,map4: map4
                      ,map5: map5
                      ,andMap: andMap
                      ,sequence: sequence
                      ,andThen: andThen
                      ,onError: onError
                      ,mapError: mapError
                      ,toMaybe: toMaybe
                      ,fromMaybe: fromMaybe
                      ,toResult: toResult
                      ,fromResult: fromResult
                      ,spawn: spawn
                      ,sleep: sleep};
   return _elm.Task.values;
};
Elm.Text = Elm.Text || {};
Elm.Text.make = function (_elm) {
   "use strict";
   _elm.Text = _elm.Text || {};
   if (_elm.Text.values)
   return _elm.Text.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Text",
   $Color = Elm.Color.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Text = Elm.Native.Text.make(_elm);
   var line = $Native$Text.line;
   var italic = $Native$Text.italic;
   var bold = $Native$Text.bold;
   var color = $Native$Text.color;
   var height = $Native$Text.height;
   var link = $Native$Text.link;
   var monospace = $Native$Text.monospace;
   var typeface = $Native$Text.typeface;
   var style = $Native$Text.style;
   var append = $Native$Text.append;
   var fromString = $Native$Text.fromString;
   var empty = fromString("");
   var concat = function (texts) {
      return A3($List.foldr,
      append,
      empty,
      texts);
   };
   var join = F2(function (seperator,
   texts) {
      return concat(A2($List.intersperse,
      seperator,
      texts));
   });
   var defaultStyle = {_: {}
                      ,bold: false
                      ,color: $Color.black
                      ,height: $Maybe.Nothing
                      ,italic: false
                      ,line: $Maybe.Nothing
                      ,typeface: _L.fromArray([])};
   var Style = F6(function (a,
   b,
   c,
   d,
   e,
   f) {
      return {_: {}
             ,bold: d
             ,color: c
             ,height: b
             ,italic: e
             ,line: f
             ,typeface: a};
   });
   var Through = {ctor: "Through"};
   var Over = {ctor: "Over"};
   var Under = {ctor: "Under"};
   var Text = {ctor: "Text"};
   _elm.Text.values = {_op: _op
                      ,fromString: fromString
                      ,empty: empty
                      ,append: append
                      ,concat: concat
                      ,join: join
                      ,link: link
                      ,style: style
                      ,defaultStyle: defaultStyle
                      ,typeface: typeface
                      ,monospace: monospace
                      ,height: height
                      ,color: color
                      ,bold: bold
                      ,italic: italic
                      ,line: line
                      ,Style: Style
                      ,Under: Under
                      ,Over: Over
                      ,Through: Through};
   return _elm.Text.values;
};
Elm.Time = Elm.Time || {};
Elm.Time.make = function (_elm) {
   "use strict";
   _elm.Time = _elm.Time || {};
   if (_elm.Time.values)
   return _elm.Time.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Time",
   $Basics = Elm.Basics.make(_elm),
   $Native$Signal = Elm.Native.Signal.make(_elm),
   $Native$Time = Elm.Native.Time.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var delay = $Native$Signal.delay;
   var since = F2(function (time,
   signal) {
      return function () {
         var stop = A2($Signal.map,
         $Basics.always(-1),
         A2(delay,time,signal));
         var start = A2($Signal.map,
         $Basics.always(1),
         signal);
         var delaydiff = A3($Signal.foldp,
         F2(function (x,y) {
            return x + y;
         }),
         0,
         A2($Signal.merge,start,stop));
         return A2($Signal.map,
         F2(function (x,y) {
            return !_U.eq(x,y);
         })(0),
         delaydiff);
      }();
   });
   var timestamp = $Native$Signal.timestamp;
   var every = $Native$Time.every;
   var fpsWhen = $Native$Time.fpsWhen;
   var fps = function (targetFrames) {
      return A2(fpsWhen,
      targetFrames,
      $Signal.constant(true));
   };
   var inMilliseconds = function (t) {
      return t;
   };
   var millisecond = 1;
   var second = 1000 * millisecond;
   var minute = 60 * second;
   var hour = 60 * minute;
   var inHours = function (t) {
      return t / hour;
   };
   var inMinutes = function (t) {
      return t / minute;
   };
   var inSeconds = function (t) {
      return t / second;
   };
   _elm.Time.values = {_op: _op
                      ,millisecond: millisecond
                      ,second: second
                      ,minute: minute
                      ,hour: hour
                      ,inMilliseconds: inMilliseconds
                      ,inSeconds: inSeconds
                      ,inMinutes: inMinutes
                      ,inHours: inHours
                      ,fps: fps
                      ,fpsWhen: fpsWhen
                      ,every: every
                      ,timestamp: timestamp
                      ,delay: delay
                      ,since: since};
   return _elm.Time.values;
};
Elm.Transform2D = Elm.Transform2D || {};
Elm.Transform2D.make = function (_elm) {
   "use strict";
   _elm.Transform2D = _elm.Transform2D || {};
   if (_elm.Transform2D.values)
   return _elm.Transform2D.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Transform2D",
   $Native$Transform2D = Elm.Native.Transform2D.make(_elm);
   var multiply = $Native$Transform2D.multiply;
   var rotation = $Native$Transform2D.rotation;
   var matrix = $Native$Transform2D.matrix;
   var translation = F2(function (x,
   y) {
      return A6(matrix,
      1,
      0,
      0,
      1,
      x,
      y);
   });
   var scale = function (s) {
      return A6(matrix,
      s,
      0,
      0,
      s,
      0,
      0);
   };
   var scaleX = function (x) {
      return A6(matrix,
      x,
      0,
      0,
      1,
      0,
      0);
   };
   var scaleY = function (y) {
      return A6(matrix,
      1,
      0,
      0,
      y,
      0,
      0);
   };
   var identity = $Native$Transform2D.identity;
   var Transform2D = {ctor: "Transform2D"};
   _elm.Transform2D.values = {_op: _op
                             ,identity: identity
                             ,matrix: matrix
                             ,multiply: multiply
                             ,rotation: rotation
                             ,translation: translation
                             ,scale: scale
                             ,scaleX: scaleX
                             ,scaleY: scaleY};
   return _elm.Transform2D.values;
};
Elm.Util = Elm.Util || {};
Elm.Util.make = function (_elm) {
   "use strict";
   _elm.Util = _elm.Util || {};
   if (_elm.Util.values)
   return _elm.Util.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   $moduleName = "Util",
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $GameTypes = Elm.GameTypes.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm);
   var isNothing = function (x) {
      return function () {
         switch (x.ctor)
         {case "Just": return false;
            case "Nothing": return true;}
         _U.badCase($moduleName,
         "between lines 43 and 45");
      }();
   };
   var maybe = F3(function (def,
   f,
   m) {
      return $Maybe.withDefault(def)(A2($Maybe.map,
      f,
      m));
   });
   var fromJust = function (_v2) {
      return function () {
         switch (_v2.ctor)
         {case "Just": return _v2._0;}
         _U.badCase($moduleName,
         "on line 37, column 21 to 22");
      }();
   };
   var listFind = F2(function (k,
   xs) {
      return function () {
         switch (xs.ctor)
         {case "::": switch (xs._0.ctor)
              {case "_Tuple2": return _U.eq(k,
                   xs._0._0) ? xs._0._1 : A2(listFind,
                   k,
                   xs._1);}
              break;
            case "[]":
            return $Debug.crash(A2($Basics._op["++"],
              "Key ",
              A2($Basics._op["++"],
              $Basics.toString(k),
              A2($Basics._op["++"],
              " not found in ",
              $Basics.toString(xs)))));}
         _U.badCase($moduleName,
         "between lines 31 and 34");
      }();
   });
   var listModify = F3(function (k,
   f,
   xs) {
      return function () {
         switch (xs.ctor)
         {case "::": switch (xs._0.ctor)
              {case "_Tuple2": return _U.eq(k,
                   xs._0._0) ? A2($List._op["::"],
                   {ctor: "_Tuple2"
                   ,_0: xs._0._0
                   ,_1: f(xs._0._1)},
                   xs._1) : A2($List._op["::"],
                   {ctor: "_Tuple2"
                   ,_0: xs._0._0
                   ,_1: xs._0._1},
                   A3(listModify,k,f,xs._1));}
              break;
            case "[]":
            return $Debug.crash(A2($Basics._op["++"],
              "Key ",
              A2($Basics._op["++"],
              $Basics.toString(k),
              A2($Basics._op["++"],
              " not found in ",
              $Basics.toString(xs)))));}
         _U.badCase($moduleName,
         "between lines 25 and 28");
      }();
   });
   var groupInto = F2(function (n,
   xs) {
      return function () {
         switch (xs.ctor)
         {case "[]":
            return _L.fromArray([]);}
         return A2($List._op["::"],
         A2($List.take,n,xs),
         A2(groupInto,
         n,
         A2($List.drop,n,xs)));
      }();
   });
   var log = F2(function (str,gs) {
      return _U.replace([["debuglog"
                         ,A2($List._op["::"],
                         str,
                         gs.debuglog)]],
      gs);
   });
   var lookupGameInfo = F2(function (game,
   ident) {
      return $List.head(A2($List.filter,
      function (g) {
         return _U.eq(g.ident,ident);
      },
      game.lounge.games));
   });
   var inGame = function (s) {
      return _U.eq(s.status,
      $GameTypes.InGame);
   };
   var atLounge = function (s) {
      return _U.eq(s.status,
      $GameTypes.InLounge);
   };
   _elm.Util.values = {_op: _op
                      ,atLounge: atLounge
                      ,inGame: inGame
                      ,lookupGameInfo: lookupGameInfo
                      ,log: log
                      ,groupInto: groupInto
                      ,listModify: listModify
                      ,listFind: listFind
                      ,fromJust: fromJust
                      ,maybe: maybe
                      ,isNothing: isNothing};
   return _elm.Util.values;
};

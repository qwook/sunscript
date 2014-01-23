
-- SunScript, a transpiled language built ontop of Lua.
-- The goal of SunScript is to allow computers to build an intellisense
-- procedurally during programming, and to make Lua more readable.

-- A bit of caution, the code below was put together while I was still learning how to use LPEG.
-- As such, the code is quite messy and uses really bad LPEG syntax.
-- A rewrite is necessary.

--------------------------------------------------------------------------------
-- The MIT License (MIT)

-- Copyright (c) 2014 Henry Tran

-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:

-- The above copyright notice and this permission notice shall be included in
-- all copies or substantial portions of the Software.

-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
-- THE SOFTWARE.

local path = (...):gsub("%.([A-z0-9]+)$", "");
local lpeg = require(path .. ".lulpeg")
local io = io
local string = string
local print = print
local loadstring = loadstring
local pairs = pairs
local type = type
local tostring = tostring
local pcall = pcall
local error = error
local table = table

module("suncomp")

local V = lpeg.V
local P = lpeg.P

local err_pos = 0

local function update_err_pos (text, pos)
    err_pos = pos
    return pos
end

ERR_POS = P(update_err_pos)

-- Auxiliary Function
local function evalfn(a, b, c)
    if (b == ".") then
        return a .. ":" .. c;
    elseif (b == "::") then
        return a .. "." .. c;
    elseif (b == "||") then
        return a .. "or" .. c;
    end
    return a .. b .. c;
end

local function testfn(a, b, c, d, e, f, g, h)
    return a .. "\n" .. b;
end

local function blockfn(a)
    return a or "";
end

local function block2fn()
    return "";
end

local function headlessscopefn(a)
    return "do\n" .. a .. "\nend"
end

local function opsetfn(_, b, c)
    return "local " .. b .. "=" .. c .. ";";
end

local function opset2fn(b, c)
    return b .. "=" .. c .. ";";
end

local function opaugsetfn(b, op, c)

    if (op == "<<") then
        return b .. "=bitleft(" .. b .. "," .. c .. ");";
    elseif (op == ">>") then
        return b .. "=bitright(" .. b .. "," .. c .. ");";
    elseif (op == "&") then
        return b .. "=bitand(" .. b .. "," .. c .. ");";
    elseif (op == "|") then
        return b .. "=bitor(" .. b .. "," .. c .. ");";
    end
    return b .. "=" .. b .. op .. "(" .. c .. ");";
end

local function parenfn(a, b, c)
    return "(" .. a .. ")";
end

local function stringfn(a, b, c, d)
    local i = 1;
    while (string.sub(a, b-1-i, b-1-i) == "\\") do
        i = i + 1;
    end
    return (i % 2) ~= 0;
end

local function functionfn(_, b, c, d)
    return "function " .. b .. "(" .. ")\n" .. c .. "\nend"
end

local function callfn(a, b)
    return a .. b --.. "(" .. (b or "") .. ")"
end

local function forblockfn(a, b, c, d)
    -- The if loop around it makes the first statement local
    return "do " .. a .. " while (" .. b .. ") do\n" .. d .. "\n" .. c .. "\nend end"
end

-- increment/decrement
local function crementfn(a, b)
    if (b == "++") then
        return a .. "=" .. a .. "+1";
    elseif (b == "--") then
        return a .. "=" .. a .. "-1";
    end
end

local function indexfn(a, b, c)
    if (b == "[") then
        return a .. "[" .. c .. "]";
    elseif (b == "(") then
        return a .. "(" .. c .. ")";
    elseif (b == ".") then
        return a.. "." .. c;
    else
        return a.. "." .. c;
    end
end

local function indexcallfn(a, b, c)
    if b == "." then
        return a .. ":" .. c;
    elseif b == "::" then
        return a .. "." .. c;
    end
end

local function index2fn(a, b)
    if b then
        return a .. ":" .. b;
    else
        return a;
    end
end

local function poopfn(a, b)
    print(a, b);
end

-- Lexical Elements
local Comment =     lpeg.P"/*" * (lpeg.P(1) - lpeg.P"*/")^0 * lpeg.P"*/";
local CommentLine = lpeg.P"//" * (lpeg.P(1) - lpeg.P"\n")^0 * lpeg.P"\n";
local Space =       (lpeg.S(" \n\t") + Comment + CommentLine)^0;
local CSpace =      lpeg.C(Space);
local Hex =         (lpeg.P"0" * lpeg.S"xX" * lpeg.R("af", "AF", "09")^1)
local Number =      lpeg.C(Hex + (lpeg.P"-"^-1 * lpeg.R("09")^1)) * Space;
local Name =        lpeg.C((lpeg.S"$_" + lpeg.R("az", "AZ")) * (lpeg.R("az", "AZ", "09") + lpeg.S"$_" )^0 - Hex) / function(a) a = a:gsub("%$", "__") return a end * Space;
local Operator =    lpeg.C(lpeg.P"<=" + lpeg.P"=<" + lpeg.P"=>" + lpeg.P">=" + lpeg.P"<<" + lpeg.P">>" + lpeg.S("+-*/<>%^") + lpeg.P".." + lpeg.P"." + (lpeg.P"::") + (lpeg.P"==") + (lpeg.P"!=") + (lpeg.P"~=") + (lpeg.P"or" - (lpeg.P"or" * Name)) + (lpeg.P"and" - (lpeg.P"and" * Name)) + (lpeg.P"|" - lpeg.P"||") + (lpeg.P"&" - lpeg.P"&&") + lpeg.P"||" + lpeg.P"&&") * Space;
local EqualOp =     (lpeg.P"=" - lpeg.P"==") * Space;
local CurlyOpen =   lpeg.P"{" * Space;
local CurlyClose =  lpeg.P"}" * Space;
local ParenOpen =   lpeg.P"(" * Space;
local ParenClose =  lpeg.P")" * Space;
local BlockyOpen =  lpeg.P"[" * Space;
local BlockyClose = lpeg.P"]" * Space;
local Semicolon =   lpeg.P";" * Space;
local DoubleColon = lpeg.P"::" * Space;
local SingleColon = lpeg.P":" * Space;
local Dot =         lpeg.P"." * Space;
local Type =        Name;
local String =      (lpeg.C(lpeg.P"\"" * (lpeg.P(1) - lpeg.Cmt(lpeg.P"\"", stringfn))^0 * lpeg.P"\"") + lpeg.C(lpeg.P"'" * (lpeg.P(1) - lpeg.Cmt(lpeg.P"'", stringfn))^0 * lpeg.P"'")) / function(a) return a:gsub("\n", "\\n") end * Space;
local Comma =       lpeg.P"," * Space;

local Crement =     lpeg.C(lpeg.P"++" + lpeg.P"--") * Space;

-- Keyword
local For =         lpeg.P"for" * Space;
local In =          lpeg.P"in" * Space;
local If =          lpeg.P"if" * Space;
local Else =        lpeg.P"else" * Space;
local While =       lpeg.P"while" * Space;
local Return =      lpeg.P"return" * Space;
local Break =       lpeg.P"break" * Space;
local Continue =    lpeg.P"continue" * Space;
local Local =       lpeg.P"local" * Space;
local Class =       lpeg.P"class" * Space;
local New =         lpeg.P"new" * Space;
local Extends =     lpeg.P"extends" * Space;
local Try =         lpeg.P"try" * Space;
local Catch =       lpeg.P"catch" * Space;

local Not =         lpeg.P"!" * Space;
local Anything =    lpeg.R(string.char(0)..string.char(255))^1;

local GLOBAL_ERROR = "";

local PARSER_DEBUG = function(start, expect)
    return (lpeg.Cmt(lpeg.Cp() * start, function(a, b, c, d)
                print(expect)
                GLOBAL_ERROR = "\nERROR, EXPECTED: " .. expect .. "\n" ..
                                "\t> " .. a:sub(c, b-1);
                return true
            end));
end;

-- Grammar
local Gr;
Gr = {
    "Genesis",

    SingleLine =    V"Else" + V"If" + V"OpAugSet" + V"OpSet" + V"Function" + V"LocalFunction" + V"ForBlock" + V"Call" + V"CrementLine" + V"HeadlessScope" + V"ForBlock2" + V"ForBlock3" + V"ForBlock4" + V"ExpressionCall" + V"Declare" + V"Return" + V"Break" + V"Continue" + V"While" + V"Class" + V"New" + V"TryCatch";
    --prototype legacy line detection
    --Genesis =       Space * lpeg.Cf(V"SingleLine" * (Semicolon^0 * V"Genesis"^0), testfn );
    Genesis = Space * lpeg.Ct(((Semicolon^0) * V"SingleLine")^1 * (Semicolon^0)) / function(a) return table.concat(a, "\n") end;

    Block =         (CurlyOpen * V"Genesis" * CurlyClose / blockfn) + (CurlyOpen * Space * CurlyClose / block2fn)
                    + PARSER_DEBUG(CurlyOpen * V"Genesis", "CurlyClose")
                    + PARSER_DEBUG(CurlyOpen * Space, "CurlyClose");

    Keyword =       ((For - (P"for" * Name)) + (In - (P"in" * Name)) + (If - (P"if" * Name)) + (While - (P"while" * Name)) + (Return - (P"return" * Name)) + (Break - (P"break" * Name)) + (Continue - (P"continue" * Name)) + (New - (P"new" * Name)) + (Class - (P"class" * Name)) + (Else - (P"else" * Name)) + (Extends - (P"extends" * Name)) + (Try - (P"try" * Name)) + (Catch - (P"catch" * Name)));
    Type =          (Type - V"Keyword") + Local;
    Name =          Name - V"Keyword";

    -- Get a variable perhaps through indexing or expression
    Variable2 =     V"Name";
                    --lpeg.Cg(V"CallParen" / parenfn)
    Variable =      lpeg.Cf(V"Variable2" * (
                            -- [test + 1]
                            lpeg.Cg(lpeg.C(BlockyOpen) * V"Expression" * BlockyClose) +
                            -- .test, but not when running a function.
                            (lpeg.Cg(lpeg.C(Dot) * V"Variable")) - (lpeg.Cg(lpeg.C(Dot) * V"Variable" * ParenOpen)) +
                            -- :: will always give a dot
                            lpeg.Cg(lpeg.C(DoubleColon) * V"Variable")
                    )^0, indexfn) ;

    -- A headless scope.
    HeadlessScope = V"Block" / headlessscopefn;

    -- Define a function
    Function =      (V"Type" * V"Variable") * ParenOpen * V"FunctionParams"^0 * ParenClose * V"Block" / (function(_, b, c, d)
                        if d then
                            return "function " .. b .. "(" .. c .. ")\n" .. d .. "\nend"
                        else
                            return "function " .. b .. "(" .. ")\n" .. c .. "\nend"
                        end
                    end)
                    + PARSER_DEBUG((V"Type" * V"Variable") * ParenOpen * V"FunctionParams"^0 * ParenClose, "Block")
                    + PARSER_DEBUG((V"Type" * V"Variable") * ParenOpen, "FunctionParams");
    LocalFunction = Local * V"Function" / function(a) return "local " .. a end;
    FunctionParams = lpeg.Cf((V"OptionalType" + lpeg.C(P"...")) * lpeg.Cg(Comma * (V"OptionalType" + lpeg.C(P"...")))^0, function(a, b)
                        return a .. "," .. b
                    end);

    AnonFunction =  V"Type" * ParenOpen * V"FunctionParams"^0 * ParenClose * V"Block" / (function(_, c, d, e)
                        if d then
                            return "function " .. "(" .. c .. ")\n" .. d .. "\nend"
                        else
                            return "function " .. "(" .. ")\n" .. c .. "\nend"
                        end
                    end);

    -- Call a function
    Call =          lpeg.Cf(
                        lpeg.Cg(lpeg.Cf(V"Variable" * (Dot * (Name - V"Keyword"))^0, index2fn))
                            * (V"CallParen") / callfn *
                        (
                            (lpeg.Cg(lpeg.C(Dot) * V"Call")) +
                            (lpeg.Cg(lpeg.C(DoubleColon) * V"Call"))
                        )^0, indexcallfn);
    CallParen =     lpeg.Cf(lpeg.Cg(((ParenOpen * ParenClose) / (function(a) return "()" end)) +
                    (ParenOpen * V"CallArgs" * ParenClose) / (function(a) return "(" .. a .. ")" end)) * V"CallParen"^0, function(a, b)
                        return a .. (b or "")
                    end);
    CallArgs =      lpeg.Cf(V"Expression" * lpeg.Cg(Comma * V"Expression")^0, function(a, b)
                        return a .. "," .. b
                    end);

    OptionalType =  (V"Type" * V"Name" / (function(a, b) return b end)) +
                    V"Name";

    -- For (k;v;c)
    ForBlock =      (For * ParenOpen * (V"SingleLine" + CSpace) * Semicolon * (V"Expression" + CSpace) * Semicolon * (V"SingleLine" + CSpace) * ParenClose * (V"Block" + V"SingleLine")) / forblockfn;
    -- For k, v : tbl
    ForBlock2 =     (For * ParenOpen * V"OptionalType" * Comma * V"OptionalType" * (SingleColon - DoubleColon) * V"Expression" * ParenClose * (V"Block" + V"SingleLine")) / function(a, b, c, d)
        return "for " .. a .. "," .. b .. " in pairs(" .. c .. ") do local __shouldbreak repeat\n" .. d .. "\nuntil true if __shouldbreak then break end end\n"
    end;
    -- For v : tbl
    ForBlock3 =     (For * ParenOpen * V"OptionalType" * (SingleColon - DoubleColon) * V"Expression" * ParenClose * (V"Block" + V"SingleLine")) / function(a, b, c)
        return "for ____tmp____," .. a .. " in pairs(" .. b .. ") do local __shouldbreak repeat\n" .. c .. "\nuntil true if __shouldbreak then break end end\n"
    end;
    -- For k, v in pairs(tbl)
    ForBlock4 =     (For * ParenOpen * V"OptionalType" * Comma * V"OptionalType" * (In - P"in" * V"Name") * V"Expression" * ParenClose * (V"Block" + V"SingleLine")) / function(a, b, c, d)
        return "for " .. a .. "," .. b .. " in " .. c .. " do local __shouldbreak repeat\n" .. d .. "\nuntil true if __shouldbreak then break end end\n"
    end
    +               (For * ParenOpen * V"OptionalType" * (In - P"in" * V"Name") * V"Expression" * ParenClose * (V"Block" + V"SingleLine")) / function(a, b, c)
        return "for " .. a .. " in " .. b .. " do local __shouldbreak repeat\n" .. c .. "\nuntil true if __shouldbreak then break end end\n"
    end;

    -- Local Declare
    Declare =       V"Type" * lpeg.Cf(V"Name" * (Comma * V"Name")^0, function(a, b) return a .. "," .. b end) / function(a, b) return "local " .. b end;

    -- Set operation (also with declaration)
    OpSet =         (V"Variable" * EqualOp * V"Expression" / opset2fn)
                    + (V"Type" * lpeg.Cf(V"Name" * (Comma * V"Name")^0, function(a, b) return a .. "," .. b end) * EqualOp * V"Expression" / opsetfn)
                    + (lpeg.Cf(V"OptionalType" * (Comma * V"Name")^0, function(a, b) return a .. "," .. b end) * EqualOp * V"Expression" / opset2fn)
                    + PARSER_DEBUG(V"OptionalType" * EqualOp, "Expression");
    OpAugSet =      (V"Variable" * lpeg.C(lpeg.P".." + lpeg.P"<<" + lpeg.P">>" + lpeg.S"+-*/^%&|") * EqualOp * V"Expression" / opaugsetfn);
    OpGen =         lpeg.Cf(
                        (lpeg.Cg(ParenOpen * V"Expression" * ParenClose / function(a) return "(" .. a .. ")" end)
                            + V"ExpressionVar")
                        * (lpeg.Cg((Operator * V"Expression") - (Dot * V"Call"))
                            + lpeg.Cg(lpeg.C(Dot) / function() return ":" end * (V"Call" - (V"Call" * V"HeadlessScope")))
                        )^0, function(a, b, c)
                        if (b == ".") then
                            return a .. "." .. c;
                        elseif (b == "::") then
                            return a .. "." .. c;
                        elseif (b == "||" or b == "or") then
                            return a .. " or " .. c;
                        elseif (b == "&&" or b == "and") then
                            return a .. " and " .. c;
                        elseif (b == "!=") then
                            return a .. "~=" .. c
                        elseif (b == "=>") then
                            return a .. ">=" .. c
                        elseif (b == "=<") then
                            return a .. "<=" .. c
                        elseif (b == "<<") then
                            return "bitleft(" .. a .. "," .. c .. ")"
                        elseif (b == ">>") then
                            return "bitright(" .. a .. "," .. c .. ")"
                        elseif (b == "&") then
                            return "bitand(" .. a .. "," .. c .. ")"
                        elseif (b == "|") then
                            return "bitor(" .. a .. "," .. c .. ")"
                        end
                        return a .. b .. c;
                    end);

    -- Expressions
    ExpressionVar = V"New" + Number + V"Table" + V"Array" + (V"Call" - (V"Call" * V"HeadlessScope")) + V"AnonFunction" + String + V"Variable" + V"Name"
                    + (Not * V"ExpressionVar") / function(a) return "not " .. a end
                    + (lpeg.P"#" * V"ExpressionVar") / function(a) return "#" .. a end
                    + (lpeg.P"-" * V"ExpressionVar") / function(a) return "-" .. a end;
    Expression =    V"OpGen" + 
                    lpeg.Cf((ParenOpen * V"Expression" * ParenClose) / parenfn *
                        (lpeg.C(ParenOpen) * V"CallArgs"^0 * ParenClose / (function(a, b) return "(" .. (b or "") .. ")" end))^0, function(a, b, c)
                            return a .. (b or "")
                        end) +
                    (Not * V"Expression" / function(a) return "(not (" .. a .."))" end) +
                    (lpeg.P"#" * (lpeg.Cg(ParenOpen * V"Expression" * ParenClose) + V"Variable" + V"Name") / function(a) return "#".. a .."" end) +
                    (lpeg.P"-" * V"Expression" / function(a) return "(-1*(" .. a .."))" end) +
                    lpeg.C(P"...");
    -- (1 + 1)()
    ExpressionCall =lpeg.Cf((ParenOpen * V"Expression" * ParenClose) / parenfn *
                        (lpeg.C(ParenOpen) * V"CallArgs"^0 * ParenClose / (function(a) return "(" .. a .. ")" end))^1, function(a, b, c)
                            return a .. (b or "")
                        end);

    -- Increment/Decrement
    CrementLine =   (V"Variable" + (Name - V"Keyword")) * Crement / crementfn;

    -- Hardest thing ever, tables
    Table =         lpeg.C(CurlyOpen) * (V"TableEles")^0 * (Comma + Semicolon)^-1 * CurlyClose / function(_, b)
                        return "{" .. (b or "") .. "}"
                    end;
    Array =         lpeg.C(BlockyOpen) * (V"ArrayEles")^0 * (Comma + Semicolon)^-1 * BlockyClose / function(_, b)
                        return "{" .. (b or "") .. "}"
                    end;
    ArrayEles =     lpeg.Cf(
                        V"Expression"
                        * (Comma * V"Expression")^0, function(a, b)
                        return a .. "," .. b
                    end);
    TableKey =      BlockyOpen * V"Expression" * BlockyClose / function(a) return "[" .. a .. "]" end +
                    (String + Number) / function(a) return "[" .. a .. "]" end +
                    Name;
    TableEle =      ((V"TableKey" * (EqualOp+(SingleColon-DoubleColon)) * V"Expression" / function(a, b) return a .. "=" .. b end) + V"Expression");
    TableEles =     lpeg.Cf(
                        V"TableEle"
                        * ((Semicolon+Comma) * V"TableEle")^0, function(a, b)
                        return a .. "," .. b
                    end);

    Return =        (Return * V"ArrayEles") / function(a) return "if(true)then return " .. a .. ";end" end
                    + (Return - (Return * V"ArrayEles")) / function(a) return "if(true)then return;end" end;

    Break =         Break / "if(true)then __shouldbreak=true;break;end";
    Continue =      Continue / "if(true)then break;end";

    If =            If * V"Expression" * (V"Block" + V"SingleLine") / function(a, b) return "if (" .. a .. ") then\n" .. b .. "\nend " end;
    Else =          If * V"Expression" * (V"Block" + V"SingleLine") * Else * (V"Block" + V"SingleLine") / function(a, b, c) return "if (" .. a .. ") then\n" .. b .. "\nelse\n" .. c .. "\nend " end;
    While =         While * V"Expression" * (V"Block" + V"SingleLine") / function(a, b) return "while (" .. a .. ") do local __shouldbreak repeat\n" .. b .. "\nuntil true if __shouldbreak then break end end " end;

    New =           lpeg.Cg((New - (P"new" * V"Name")) * V"Variable" * V"CallParen" / function(a, b) return "(" .. a .. "):new" .. b end) +
                    lpeg.Cg((New - (P"new" * V"Name")) * V"Variable" / function(a) return "(" .. a .. "):new()" end);
    ClassList =     lpeg.Cf(
                        V"Variable" / function(a) return {a}; end
                        * (Comma * V"Variable")^0, function(a, b)
                        table.insert(a, b);
                        return a;
                    end);
    Class =         lpeg.Cg(Class * V"Variable" * CurlyOpen * V"ClassBlock"^0 * CurlyClose / function(a, b)
                        return a .. "={\n" .. (b or "") .."\n}\n" ..
                        a .. ".__constructor = function(s, ...) if " .. a .. ".__super then " .. a .. ".__super.__constructor(s, ...) end if " .. a .. ".constructor then " .. a .. ".constructor(s, ...) end end\n" ..
                        a .. ".__index = function(s, k) return " .. a .. "[k] end\n" ..
                        a .. ".new = function(s, ...) local o = setmetatable({}, s) if o.constructor then o:constructor(...) end return o end\n" ..
                        a .. ".__classname = \"" .. a .. "\"\n"
                    end) +
                    lpeg.Cg(Class * V"Variable" * ((SingleColon - DoubleColon) + Extends) * V"Variable" * CurlyOpen * V"ClassBlock"^0 * CurlyClose / function(a, super, b)
                        return a .. "={\n" .. (b or "") .."\n}\n" ..
                        a .. ".__constructor = function(s, ...) if " .. a .. ".__super then " .. a .. ".__super.__constructor(s, ...) end if " .. a .. ".constructor then " .. a .. ".constructor(s, ...) end end\n" ..
                        a .. ".__index = function(s, k) if (k) == \"super\" then return setmetatable({}, {__index=function(fake, k) local sp = ".. a .. ".__super while (sp[k] == nil and sp.__super) do sp = sp.__super end if type(sp[k]) == \"function\" then return function(fake, ...) return sp[k](s, ...) end end end}) end return " .. a .. "[k] or (" .. a .. ".__super.__index(s, k)) end\n" ..
                        a .. ".new = function(s, ...) local n = {} local o = setmetatable(n, s) o:__constructor(...) return o end\n" ..
                        a .. ".__classname = \"" .. a .. "\"\n" ..
                        a .. ".__super = "..super .. "\n"
                    end) +
                    lpeg.Cg(Class * V"Variable" * ((SingleColon - DoubleColon) + Extends) * V"ClassList" * CurlyOpen * V"ClassBlock"^0 * CurlyClose / function(a, super, b)
                        local super_arr = table.concat(super, ", ");
                        return a .. "={\n" .. (b or "") .."\n}\n" ..
                        a .. ".__constructor = function(s, ...) " ..
                            "local sp_array = ".. a .. ".__super "..
                            "for _, sp in pairs(sp_array) do " ..
                                "sp.__constructor(s, ...) " ..
                            "end " ..
                            "if " .. a .. ".constructor then " ..
                                a .. ".constructor(s, ...) " ..
                            "end " ..
                        "end\n" ..
                        a .. ".__index = function(s, k) if (k) == \"super\" then return setmetatable({}, {__index=function(fake, k) " ..
                            "local sp_array = ".. a .. ".__super "..
                            "for _, _sp in pairs(sp_array) do " ..
                                "local sp = _sp " ..
                                "while (sp[k] == nil and sp.__super) do " ..
                                    "sp = sp.__super " ..
                                "end " ..
                                "if type(sp[k]) == \"function\" then "..
                                    "return function(fake, ...) " ..
                                        "return sp[k](s, ...) " ..
                                    "end " ..
                                "end " ..
                            "end " ..
                        "end}) end " ..
                        "if " .. a .. "[k] == nil then " ..
                            "local sp_array = ".. a .. ".__super " ..
                            "for _, sp in pairs(sp_array) do " ..
                                "local ret = sp.__index(s, k)" ..
                                "if ret ~= nil then " ..
                                    "return ret " ..
                                "end " ..
                            "end " ..
                        "end " ..
                        "return " .. a .. "[k] end\n" ..
                        a .. ".new = function(s, ...) local n = {} local o = setmetatable(n, s) o:__constructor(...) return o end\n" ..
                        a .. ".__classname = \"" .. a .. "\"\n" ..
                        a .. ".__super = {".. super_arr .. "}\n"
                    end);
    ClassFunction = lpeg.Cg((V"OptionalType") * ParenOpen * V"FunctionParams"^0 * ParenClose * V"Block" / function(b, c, d)
                        if d then
                            return b .." = function(self, " .. c .. ")\nlocal super = self.super\n" .. d .. "\nend\n"
                        else
                            return b .." = function(self)\nlocal super = self.super\n" .. c .. "\nend\n"
                        end
                    end)
                    + PARSER_DEBUG((V"OptionalType") * ParenOpen * V"FunctionParams"^0 * ParenClose, "Block")
                    + PARSER_DEBUG((V"OptionalType") * ParenOpen, "FunctionParams")
                    + PARSER_DEBUG((V"OptionalType"), "ParenOpen");
    ClassDeclare =  lpeg.Cg(V"Type" * V"Variable");
    -- ClassBlock =    lpeg.Cf(V"ClassFunction" * (V"ClassBlock")^0, function(a, b)
    --                     return a .. ";\n" .. b
    --                 end)
    --prototype legacy class blocks
    ClassBlock =    lpeg.Ct((V"ClassFunction")^1) / function(a) return table.concat(a, ";\n") end;
    TryCatch = (Try * V"Block" * Catch * ParenOpen * V"Variable" * ParenClose * V"Block") / function(try, e, catch)
        return "local succ, " .. e .. " = pcall(function ()\n" .. try .. " end) if not succ then " .. catch .. " end\n";
    end;
}

--for k,v in pairs (Gr) do
--    Gr[k] = v * ERR_POS
--end

local Grammar = P(Gr)


function print_table(tbl, level)
    level = level or 0;

    print(("\t"):rep(level) .. "{");
    for k,v in pairs(tbl) do
        if type(v) == "table" then
            print( ("\t"):rep(level + 1) .. k .. " =");
            print_table(v, level + 1)
        elseif type(v) == "string" then
            print( ("\t"):rep(level + 1) .. k .. " = \"" .. tostring(v) .. "\"" );
        else
            print( ("\t"):rep(level + 1) .. k .. " = " .. tostring(v) );
        end
    end
    print(("\t"):rep(level) .. "}")
end


-- Example
local code =
[[
/** UglyScript **/

/**
 * Interchange how you want to define tables!
 */
 tabl = { a: 1 + 2, b: {1 + 3}}
 tabl = { a = 1 + 2, b = 1 + 3}
 tabl = { 1 + 2; 1 + 3}
 tabl = { 2, 1 }
 tabl = [ 2, 1, 3, 4, 6 ]

/**
 * The types don't even matter! Name it anything you want!
 * They'll all be defined locally anyways.
 */
 local a
 var b = 3
 int c = 1

/**
 * you can use $ in place of __
 */

meta = {}
meta.$index = function(){ return print("hi") }

function $(quer) {
    print("HI!") // look, I can make some kind of jQuery clone.
}

// Some C++ loops in here
for (int i = 5; i > 0; i--)
{
    //print("oo")
}

// fuck it, define functions with random types (works for anonymous functions as well)
var test() {

    // for k,v in pairs(tabl)
    for (key, value : tabl)
    {
        print('hii "ther"', key, value)
        print("hii 'ther'", key, value)
    }

    // for ____tmp____,v in pairs(tabl)
    for (value : tabl)
    {
        // all the default shit is still here...
        while (false)
            print("hi: ", value)
        if (true) {
            print("poop")
        }
    }

}

local function $(quer) {
    // you can define local functions still
}

// and to screw around with you guys, I made "." into ":"
// and "::" into "."! so tables will act like namespaces.
tbl = {1, 2, 3}
table::insert(tbl, 4)
class = {t: function(self){ print(self) }}
function class.test() {
    
}
class.test()
class.t()
]]

function compile(code)
    GLOBAL_ERROR = ""
    local m, s = pcall(lpeg.match, Grammar, code)
    if not m or GLOBAL_ERROR:len() > 0 then
        return error(GLOBAL_ERROR)
    else
        return s
    end
end


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

local DELIMITER = package.config:sub(1,1);

require("sun.suncomp");

local _require = require;
require = function(...)
    local name = ({...})[1];

    if (package.loaded[name] ~= nil) then return package.loaded[name]; end

    -- this isn't the best way to resolve module names
    local filename = name:gsub("%.", DELIMITER) .. ".sun";
    local foldername = name:gsub("%.", DELIMITER) .. DELIMITER .. "init.sun";

    if (love.filesystem.exists(filename)) then
        local fn = loadstring(
                    suncomp.compile(
                            love.filesystem.read(filename)
                    ),
                    filename
        );
        return fn();
    end

    if (love.filesystem.exists(foldername)) then
        local fn = loadstring(
                    suncomp.compile(
                            love.filesystem.read(foldername)
                    ),
                    foldername
        );
        return fn();
    end

    return _require(...);
end

-- dude you should totally listen to fidlar

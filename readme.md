#sunscript#
a legibility-driven oop scripting language compatible with the **love2d** game framework  

**!!! THIS IS MEANT TO BE A WORKING CONCEPT !!!**  
there is no easy way to debug sunscript right now. errors cannot be traced and sometimes don't even show up explicitly. it should not be used in production *unless you're super crazy*.
  
#how to use#

    require("sun")

then require .sun files as you would with .lua files.

#check out some sunscript#

###classes###

	class Vehicle {
	    constructor(x, y) {
	        self.x = x;
	        self.y = y;
	    }
		    
	    setPosition(x, y) { self.x = x; self.y = y; }
	    getPosition() { return self.x, self.y; }
	    
	    start() { }
	}
	
	local v = new Vehicle(10, 10);
	v.start();
###polymorphism###
	class Motorcycle extends Vehicle {
		start() {
			super.start();
		}
	}

###C++ inspired syntax###

    for (local i = 0; i < 3; i++) {
        local car = new Car(i);
        local motorcycle = new Motorcycle(i);
        continue; // continue keyword compatible with lua 5.1 and 5.2
    }

###new and old operators###

	i = 1 << 1; // bitwise operations
	i = 1 & 1;
	i = 1 | 1;
	i++; i--; // increments and decrements
	i += 1; i-= 1; // self operation

###and more###
this scripting language is still new and the philosophy behind it is still being mulled over. expect features to being constantly added and removed.

the current philosophy is, "be a less strict c++, but still be lua"
#! /bin/python

#  Lisp Engine: Evaluates LISP expressions
#  Translated from QBasic version at https://github.com/skeledrew/lisp-interpreter
#  Copyright (C) 2015 Andrew Phillips <skeledrew@gmail.com>
#
#  This program is free software: you can redistribute it and/or modify
#  it under the terms of the GNU Affero General Public License as published
#  by the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU Affero General Public License for more details.
#
#  You should have received a copy of the GNU Affero General Public License
#  along with this program.  If not, see <http://www.gnu.org/licenses/>.

"""Docstring template.
  
  Created:
   15-10-31
  Last Modified:
   n/a
   
  Params:
   n/a
  Return:
   n/a
  
  Notes:
   n/a
  History:
   n/a
  """
  
# Imports


### Classes:

class Testing:
    """Docstring template.
    
    Created:
    15-10-31
    Last Modified:
    n/a
    
    Params:
    n/a
    Return:
    n/a
    
    Notes:
    n/a
    History:
    n/a
    """
    
    def test_cases():
        pass
    
    def test1(case, result):
        if case == result:
            print "Test passed!"
            
        else:
            print "Test failed..."
            print "Test:", case, "Expected:", result

    def test2(self):
        engine = LispEngine()
        print engine.evalExpr("hello!")
        
class LispEngine:
    """Docstring template.
    
    Created:
    15-10-31
    Last Modified:
    n/a
    
    Params:
    n/a
    Return:
    n/a
    
    Notes:
    n/a
    History:
    n/a
    """
    
    
    def evalExpr(self, expr):
        return "Hi again Lisp!"
        
### Functions

### Testing

myTests = Testing()
myTests.test2()

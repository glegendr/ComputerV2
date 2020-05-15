# computerV2
## Dependencies
The only dependencies you need are haskell and stack.  
To get them, check https://www.haskell.org/platform/ 
## Project
To start the project use:  
``` git clone https://github.com/glegendr/ComputerV2.git; cd ComputerV2; sh compile.sh ```  
You will get a binary call `ComputerV2`
## Variables
### Types
There is 4 types of variables:

|Type|Set|Description|
|:-:|:-:|:-:|
|Rationals|Q|Number that can be expressed as the quotient or fraction `p/q` of two integers, a numerator `p` and a non-zero denominator `q`|
|Imaginary| a + ib -> (a, b) ∈ Q^2|Complex number that can be written as a real number multiplied by the imaginary unit `i`|
|Matrix|A ∈ Mn,p(Q)|Rectangular array of numbers, symbols, or expressions, arranged in rows and columns|
|Functions||Function with one argument|
### Creation
```> {variableName} = {variableValue}```   
To create a variable, you have to give the name followed by the value.   
You can give a computation as value.   
All variables name are not case sensitive.

Rational:
```
> myVar = 4 + 23.5
  27.5
> myVarB = myVar - 50
  -22.5
```

Imaginary:
```
> myIma = 3i + 42
  42.0 + 3.0i
> myImaB = myIma^2  
  1755.0 + 252.0i 
```

Matrix:
```
> mat = [[1,2,3];[4,5,6]]
  [1.0,2.0,3.0]
  [4.0,5.0,6.0]
> matB = mat ** [[1];[2];[3]]
  [14.0]
  [32.0]
```

Functions:
```
> f(x) = 42x + 21
  42.0x + 21.0 
> myFunctionB(myVariableName) = myVariableName * (4 + 2)
  6.0myvariablename 
```

If you create a function that already exist, the older one will be overwritten.
## Computation
```> {computationA} = {computationB} ?```   
The computation part allows you to:
- Display the value of calculation
- Check if the first part of the computation is the same as the second one
- Solve quadratic equation

```
> 43 + 53/2 = ?
  69.5
> [[1.0,2.0,3.0],[4.0,5.0,6.0]]** [[1];[2];[3]] = [[14.0],[32.0]] ?
  True
> 4x + 32 - 2x^2 = 0 ?
  Reduced Form: -2.0x^2 + 4.0x + 32.0 = 0
  The two solutions are:
    5.1231055
    -3.1231055
```
## Commands
### All Commands
```> {commandName}:{commandArg1}:{commandArg2}:...:{commandArgN}```

|Name|Description|
|:-:|:-:|
|help|Display help message|
|list|List all existing variables|
|history|List all given input except commands|
|del|Del a variable|
|replace|Replace variable name by an other|
|show|Create a plot|
|quit|Quit the program|

### Help Command
```> help:{helpArgs}```    
This command displays help

|Short|Medium|Long|Description|
|:-:|:-:|:-:|:-:|
|h||help|Display this message|
|c||commands|List all existing command|
|l||list|Display the command list|
|i||history|Display the command history|
|d||del|Display the command del|
|r||replace|Display the command replace|
|s||show|Display the command show|
|q||quit|Display the command quit|
|o|ope|operators|Display all operators|
|?||computation|Display how to compute|

### List Command
```> list:{listArgs}```    
List all existing variables

|Short|Medium|Long|Description|
|:-:|:-:|:-:|:-:|
|r|rat|rationals|List all exsting rationals|
|i|ima|imaginary|List all exsting imaginary|
|m|mat|matrix|List all exsting matrix|
|f|fct|functions|List all exsting functions|
|c|cst|constants|List all exsting constants|
|a||all|List all exsting var and constants|

### History Command
```> history:{historyArgs}```   
List all given input except commands, excluding `del` and `replace`

The only argument taken is the number of input you want to display.   
- N: List N last input

Default is the list of all input.

### Del Command
```> del:{delArgs}```     
The del command delete a named variable or a pull of variables

|Short|Medium|Long|Description|
|:-:|:-:|:-:|:-:|
|X|||Delete variable with name X|
|a||all|Delete all variables|
|d|dup|duplicate|Delete all duplicates|

### Replace Command
```> replace:{replaceArgs}```    
The replace command replace the name of a variable by another.    
If the chosen name is already in a variable, the names will swap each other.

The only argument taken by replace is the current name of the variable and the new one.
- X:Y Replace var name X by Y. If Y exist the 2 name are swapping

### Show Command
```> show:{showArgs}```    
The show command creates a plot of the given fuction

|Short|Medium|Long|Description|Default|
|:-:|:-:|:-:|:-:|:-:|
|f=x,y..|fct=x,y..|function=x,y..|Function to display||
|o=x|out=x|output=x|Name of created file|Crearion date or `title` if set|
|t=x||title=x|Title of the graph|Crearion date or `output` if set|
|||min=x|Start of the X axis|-100|
|||max=x|End of the X axis|100|
|||down=x|Start of the Y axis|-100|
|||up=x|End of the Y axis|100|
|s=x||scale=x|Scaling between each calculation dot|((min - max) / 2000)|

The plot is a SVG file created at `charts/{output}.svg`

### Quit Command
```> quit```
Self explanatory command.    
It quit the program.

## Author
[@glegendr](https://github.com/glegendr/)
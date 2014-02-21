You should start working on the VM implementation.

Remember that the Resolve structure in OpcodeResolve.sml should be used
for checking that instructions are valid. This is important since the 
assembler also uses this structure and any change to the structure will
influence both what the assembler outputs and what the VM accepts.

The VM will have to contain  a few things:
	One data structure representing the entire state of the VM
	Data structures for registers and stacks etc...
	A program counter.
	An instruction decoder which carries out each instruction
		and increments the PC accordingly.
	A structure for the memory.
		This has to implement a array as its main feature. But
		keep in mind that no semanticly observable side-effects
		should exists. By that is meant that any function handling
		the memory should be able to treat it just like any other
		data structure.
	A method for printing the entire state of the machine in a nice way
		does not have to print all of the RAM. Although a separate
		dump memory function should exist
	A method for typing in values to the memmory manually.
	
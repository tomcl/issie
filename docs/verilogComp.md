---
layout: page
title: "Verilog Component"
permalink: /verilog-comp/
---


# ISSIE Verilog Component

### Syntax

Both old and new style syntax are supported.

#### Old style example

```verilog
module decoder(instr,carry,negative,jump,mux1_sel,mux2_sel);
    input [15:0] instr;
    input carry,negative;
    output jump,mux1_sel,mux2_sel;

    wire [3:0] opc = instr[15:12];

    assign jump = opc[0] ? c|n : c&n|op[1]; 
    assign mux1_sel = (&op[3:2]);
    assign mux2_sel = negative;

endmodule
```

#### New style example

```verilog
module decoder(
    input [15:0] instr,
    input carry,negative,
    output jump,mux1_sel,mux2_sel
);

    wire [3:0] opc = instr[15:12];

    assign jump = opc[0] ? c|n : c&n|op[1]; 
    assign mux1_sel = (&op[3:2]);
    assign mux2_sel = negative;

endmodule
```

### Numbers

Numbers are given in the form: ``{width}'{radix}{value}``, where radix = `'b` or `'d` or `'h`

e.g. 16'h3fa5 , 4'b0101, 16'd154

### Operators

Operators perform an opeation on one or more operands within an expression. An expression combines operands with appropriate operators to produce the desired functional expression.

The table shows the operators in descending order of precedence. Operators with equal precedence are shown grouped.


| Verilog Operator      | Name  | Notes and example     |
| :----:      |    :----:   |    :----:     |
| [ ]      | 	bit-select or part-select       | Used to select specific bits of a bus signal. Example: <code> instr[15:8] </code>  |
| ( )   | parenthesis     | Used to define the order of operations. Example: <code>  (a|b)&c  </code>   |
| ! <br> ~ <br> (&) <br> (\|) <br> (~&) <br> (~\|) | logical negation <br>negation<br>reduction AND <br>reduction OR <br>reduction NAND <br>reduction NOR  | The Verilog reduction operators are used to convert vectors to scalars. <br> They operate on all of the bits in a vector to convert the answer to a single bit <br> AND Reduction of 4'b1101 is: 0 <br> AND Reduction of 4'b1111 is: 1 <br> OR Reduction of 4'b1101 is: 1 <br> OR Reduction of 4'b0000 is: 0 <br> Example: <code> assign out = (&a); </code>|
| { }  | concatenation  | example: <code>{a[2:0],b[3:2],2'b01}</code> -> result is 7 bits  |
| + <br> - | binary plus <br> binary minus  |  The two operands must be of equal width N <br> The result is also N bits    |
| << <br> >> <br> >>  |  logical shift left <br> logical shift right <br> arithmetic shift rigth | The second operand of a shift is an unsigned integer <br> example: <code> assign out[5:0] = in[5:0] << 3; </code> <br> The result has the width of the input    |
| &  | bit-wise AND  | The two operands must have the same width |
| ^ <br> ~^ or ^~  | bit-wise XOR <br> bit-wise XNOR  | As above |
| \|  | bit-wise OR  | As above |
| &&  |  logical AND | logical means that 0->false, anything else -> true <br> Operands can have different width |
| \|\|  | logical OR  | As above    | 
| ? :  | conditional  |  like an if-statement, corresponds to a MUX <br> example: <code> assign jump = opc[0] ? c|n : c&n|op[1]; </code> | 

<br><br><br>

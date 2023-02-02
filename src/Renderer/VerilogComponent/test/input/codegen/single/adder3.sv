        module adder(a, b, ci, sum, co);
        input  bit      ci;
        input bit [7:0] a;
        input bit [7:0] b;
        output bit [7:0] sum;
        output bit      co;
        bit   [8:0] tmp;

           assign tmp = a + b + ci;
           assign sum = tmp [7:0];
           assign co  = tmp [8];

        endmodule
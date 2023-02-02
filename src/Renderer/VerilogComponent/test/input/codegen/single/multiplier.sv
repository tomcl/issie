        module mult(a, b, res);
        input bit [7:0]  a;
        input bit [3:0]  b;
        output bit [11:0] res;

           assign res = a * b;

        endmodule
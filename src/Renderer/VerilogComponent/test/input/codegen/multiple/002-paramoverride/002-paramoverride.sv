module top (
    A, B, C, O
); 

    input bit A, C; 
    input bit [3:0] B; 
    output bit O;
    
    bit tmp;

example  #(.WIDTH(4)) inst_example (.A(A), .B(B), .O(tmp));

assign O = tmp | C;

endmodule
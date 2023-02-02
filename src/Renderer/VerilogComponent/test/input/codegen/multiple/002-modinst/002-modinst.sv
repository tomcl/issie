module top (A, B, C, O); input bit A, B, C; output bit O;
bit tmp;

example inst_example (.A(A), .B(B), .O(tmp));

assign O = tmp | C;

endmodule
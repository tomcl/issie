module top_module;
  bit [0:0] A;
  bit [0:0] A_array [7:0];
  bit [0:0] B;
  bit [0:0] B_array [7:0];
  bit [0:0] C;
  bit [0:0] C_array [7:0];
  bit [0:0] O;
  bit [0:0] O_array [7:0];
bit clk;
integer i_, j_;
  initial begin
   clk=0;
    repeat(20) begin
      #1;
      clk=!clk;
    end 
    $display("[");
    $write("{\"Label\": \"O\", \"Values\": [");
    for(i_=0;i_<7; i_=i_+1) begin $write("%d, ", O_array[i_]); end
    $display("%d]}", O_array[7]);
    $write("]");
    $finish(0);
  end
  initial begin
      A_array[0] = 1'd0;
      A_array[1] = 1'd0;
      A_array[2] = 1'd0;
      A_array[3] = 1'd0;
      A_array[4] = 1'd1;
      A_array[5] = 1'd1;
      A_array[6] = 1'd1;
      A_array[7] = 1'd1;
      B_array[0] = 1'd0;
      B_array[1] = 1'd0;
      B_array[2] = 1'd1;
      B_array[3] = 1'd1;
      B_array[4] = 1'd0;
      B_array[5] = 1'd0;
      B_array[6] = 1'd1;
      B_array[7] = 1'd1;
      C_array[0] = 1'd0;
      C_array[1] = 1'd1;
      C_array[2] = 1'd0;
      C_array[3] = 1'd1;
      C_array[4] = 1'd0;
      C_array[5] = 1'd1;
      C_array[6] = 1'd0;
      C_array[7] = 1'd1;
    for(j_=0; j_<8; j_=j_+1) begin
        A=A_array[j_];
        B=B_array[j_];
        C=C_array[j_];
      #0.5;
        O_array[j_]=O;
      @(negedge clk);
end
  end
  top dut (.A(A), .B(B), .C(C), .O(O));
endmodule
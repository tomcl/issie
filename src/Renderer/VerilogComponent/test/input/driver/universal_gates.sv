module top_module;
  bit [0:0] a;
  bit [0:0] a_array [3:0];
  bit [0:0] b;
  bit [0:0] b_array [3:0];
  bit [0:0] yOR;
  bit [0:0] yOR_array [3:0];
  bit [0:0] yAND;
  bit [0:0] yAND_array [3:0];
bit clk;
integer i_, j_;
  initial begin
   clk=0;
    repeat(12) begin
      #1;
      clk=!clk;
    end 
    $display("[");
    $write("{\"Label\": \"yOR\", \"Values\": [");
    for(i_=0;i_<3; i_=i_+1) begin $write("%d, ", yOR_array[i_]); end
    $display("%d]}, ", yOR_array[3]);
    $write("{\"Label\": \"yAND\", \"Values\": [");
    for(i_=0;i_<3; i_=i_+1) begin $write("%d, ", yAND_array[i_]); end
    $display("%d]}", yAND_array[3]);
    $write("]");
    $finish(0);
  end
  initial begin
      a_array[0] = 1'd0;
      a_array[1] = 1'd0;
      a_array[2] = 1'd1;
      a_array[3] = 1'd1;
      b_array[0] = 1'd0;
      b_array[1] = 1'd1;
      b_array[2] = 1'd1;
      b_array[3] = 1'd0;
    for(j_=0; j_<4; j_=j_+1) begin
        a=a_array[j_];
        b=b_array[j_];
      #0.5;
        yOR_array[j_]=yOR;
        yAND_array[j_]=yAND;
      @(negedge clk);
end
  end
  jbasicgates dut (.a(a), .b(b), .yOR(yOR), .yAND(yAND));
endmodule
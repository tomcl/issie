module top_module;
  bit [0:0] a;
  bit [0:0] a_array [9:0];
  bit [0:0] y;
  bit [0:0] y_array [9:0];
bit clk;
integer i_, j_;
  initial begin
   clk=0;
    repeat(24) begin
      #1;
      clk=!clk;
    end 
    $display("[");
    $write("{\"Label\": \"y\", \"Values\": [");
    for(i_=0;i_<9; i_=i_+1) begin $write("%d, ", y_array[i_]); end
    $display("%d]}", y_array[9]);
    $write("]");
    $finish(0);
  end
  initial begin
      a_array[0] = 1'd0;
      a_array[1] = 1'd0;
      a_array[2] = 1'd1;
      a_array[3] = 1'd1;
      a_array[4] = 1'd1;
      a_array[5] = 1'd0;
      a_array[6] = 1'd1;
      a_array[7] = 1'd0;
      a_array[8] = 1'd1;
      a_array[9] = 1'd0;
    for(j_=0; j_<10; j_=j_+1) begin
        a=a_array[j_];
      #0.5;
        y_array[j_]=y;
      @(negedge clk);
end
  end
  jinverter dut (.a(a), .y(y));
endmodule
module top_module;
  bit [0:0] a;
  bit [0:0] a_array [5:0];
  bit [2:0] d_out;
  bit [2:0] d_out_array [5:0];
bit clk;
integer i_, j_;
  initial begin
   clk=0;
    repeat(16) begin
      #1;
      clk=!clk;
    end 
    $display("[");
    $write("{\"Label\": \"d_out\", \"Values\": [");
    for(i_=0;i_<5; i_=i_+1) begin $write("%d, ", d_out_array[i_]); end
    $display("%d]}", d_out_array[5]);
    $write("]");
    $finish(0);
  end
  initial begin
      a_array[0] = 1'd0;
      a_array[1] = 1'd1;
      a_array[2] = 1'd0;
      a_array[3] = 1'd0;
      a_array[4] = 1'd1;
      a_array[5] = 1'd1;
    for(j_=0; j_<6; j_=j_+1) begin
        a=a_array[j_];
      #0.5;
        d_out_array[j_]=d_out;
      @(negedge clk);
end
  end
  forloop dut (.a(a), .d_out(d_out));
endmodule
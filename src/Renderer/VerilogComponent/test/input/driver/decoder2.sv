module top_module;
  bit [2:0] sel;
  bit [2:0] sel_array [9:0];
  bit [7:0] res;
  bit [7:0] res_array [9:0];
bit clk;
integer i_, j_;
  initial begin
   clk=0;
    repeat(24) begin
      #1;
      clk=!clk;
    end 
    $display("[");
    $write("{\"Label\": \"res\", \"Values\": [");
    for(i_=0;i_<9; i_=i_+1) begin $write("%d, ", res_array[i_]); end
    $display("%d]}", res_array[9]);
    $write("]");
    $finish(0);
  end
  initial begin
      sel_array[0] = 3'd0;
      sel_array[1] = 3'd1;
      sel_array[2] = 3'd2;
      sel_array[3] = 3'd3;
      sel_array[4] = 3'd4;
      sel_array[5] = 3'd5;
      sel_array[6] = 3'd6;
      sel_array[7] = 3'd7;
      sel_array[8] = 3'd6;
      sel_array[9] = 3'd5;
    for(j_=0; j_<10; j_=j_+1) begin
        sel=sel_array[j_];
      #0.5;
        res_array[j_]=res;
      @(negedge clk);
end
  end
  mux dut (.sel(sel), .res(res));
endmodule
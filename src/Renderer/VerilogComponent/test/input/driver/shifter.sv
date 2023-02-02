module top_module;
  bit [7:0] di;
  bit [7:0] di_array [14:0];
  bit [1:0] sel;
  bit [1:0] sel_array [14:0];
  bit [7:0] so;
  bit [7:0] so_array [14:0];
bit clk;
integer i_, j_;
  initial begin
   clk=0;
    repeat(34) begin
      #1;
      clk=!clk;
    end 
    $display("[");
    $write("{\"Label\": \"so\", \"Values\": [");
    for(i_=0;i_<14; i_=i_+1) begin $write("%d, ", so_array[i_]); end
    $display("%d]}", so_array[14]);
    $write("]");
    $finish(0);
  end
  initial begin
      di_array[0] = 8'd1;
      di_array[1] = 8'd2;
      di_array[2] = 8'd3;
      di_array[3] = 8'd4;
      di_array[4] = 8'd5;
      di_array[5] = 8'd6;
      di_array[6] = 8'd7;
      di_array[7] = 8'd8;
      di_array[8] = 8'd9;
      di_array[9] = 8'd124;
      di_array[10] = 8'd100;
      di_array[11] = 8'd78;
      di_array[12] = 8'd34;
      di_array[13] = 8'd20;
      di_array[14] = 8'd55;
      sel_array[0] = 2'd0;
      sel_array[1] = 2'd1;
      sel_array[2] = 2'd2;
      sel_array[3] = 2'd3;
      sel_array[4] = 2'd3;
      sel_array[5] = 2'd2;
      sel_array[6] = 2'd1;
      sel_array[7] = 2'd0;
      sel_array[8] = 2'd1;
      sel_array[9] = 2'd0;
      sel_array[10] = 2'd2;
      sel_array[11] = 2'd3;
      sel_array[12] = 2'd1;
      sel_array[13] = 2'd2;
      sel_array[14] = 2'd3;
    for(j_=0; j_<15; j_=j_+1) begin
        di=di_array[j_];
        sel=sel_array[j_];
      #0.5;
        so_array[j_]=so;
      @(negedge clk);
end
  end
  lshift dut (.di(di), .sel(sel), .so(so));
endmodule
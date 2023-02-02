module top_module;
  bit [0:0] load;
  bit [0:0] load_array [18:0];
  bit [3:0] d;
  bit [3:0] d_array [18:0];
  bit [3:0] q;
  bit [3:0] q_array [18:0];
bit clk;
integer i_, j_;
  initial begin
   clk=0;
    repeat(42) begin
      #1;
      clk=!clk;
    end 
    $display("[");
    $write("{\"Label\": \"q\", \"Values\": [");
    for(i_=0;i_<18; i_=i_+1) begin $write("%d, ", q_array[i_]); end
    $display("%d]}", q_array[18]);
    $write("]");
    $finish(0);
  end
  initial begin
      load_array[0] = 1'd0;
      load_array[1] = 1'd1;
      load_array[2] = 1'd0;
      load_array[3] = 1'd0;
      load_array[4] = 1'd0;
      load_array[5] = 1'd0;
      load_array[6] = 1'd0;
      load_array[7] = 1'd1;
      load_array[8] = 1'd1;
      load_array[9] = 1'd1;
      load_array[10] = 1'd0;
      load_array[11] = 1'd0;
      load_array[12] = 1'd0;
      load_array[13] = 1'd0;
      load_array[14] = 1'd0;
      load_array[15] = 1'd0;
      load_array[16] = 1'd0;
      load_array[17] = 1'd0;
      load_array[18] = 1'd0;
      d_array[0] = 4'd0;
      d_array[1] = 4'd1;
      d_array[2] = 4'd2;
      d_array[3] = 4'd3;
      d_array[4] = 4'd4;
      d_array[5] = 4'd5;
      d_array[6] = 4'd6;
      d_array[7] = 4'd7;
      d_array[8] = 4'd15;
      d_array[9] = 4'd14;
      d_array[10] = 4'd13;
      d_array[11] = 4'd12;
      d_array[12] = 4'd11;
      d_array[13] = 4'd10;
      d_array[14] = 4'd9;
      d_array[15] = 4'd8;
      d_array[16] = 4'd7;
      d_array[17] = 4'd6;
      d_array[18] = 4'd5;
    for(j_=0; j_<19; j_=j_+1) begin
        load=load_array[j_];
        d=d_array[j_];
      #0.5;
        q_array[j_]=q;
      @(negedge clk);
end
  end
  counter dut (.load(load), .d(d), .q(q), .clk(clk));
endmodule
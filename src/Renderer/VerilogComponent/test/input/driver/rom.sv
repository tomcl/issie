module top_module;
  bit [0:0] en;
  bit [0:0] en_array [16:0];
  bit [3:0] addr;
  bit [3:0] addr_array [16:0];
  bit [3:0] data;
  bit [3:0] data_array [16:0];
bit clk;
integer i_, j_;
  initial begin
   clk=0;
    repeat(38) begin
      #1;
      clk=!clk;
    end 
    $display("[");
    $write("{\"Label\": \"data\", \"Values\": [");
    for(i_=0;i_<16; i_=i_+1) begin $write("%d, ", data_array[i_]); end
    $display("%d]}", data_array[16]);
    $write("]");
    $finish(0);
  end
  initial begin
      en_array[0] = 1'd0;
      en_array[1] = 1'd1;
      en_array[2] = 1'd1;
      en_array[3] = 1'd1;
      en_array[4] = 1'd1;
      en_array[5] = 1'd1;
      en_array[6] = 1'd1;
      en_array[7] = 1'd1;
      en_array[8] = 1'd1;
      en_array[9] = 1'd1;
      en_array[10] = 1'd1;
      en_array[11] = 1'd1;
      en_array[12] = 1'd1;
      en_array[13] = 1'd1;
      en_array[14] = 1'd1;
      en_array[15] = 1'd1;
      en_array[16] = 1'd1;
      addr_array[0] = 4'd0;
      addr_array[1] = 4'd0;
      addr_array[2] = 4'd1;
      addr_array[3] = 4'd2;
      addr_array[4] = 4'd3;
      addr_array[5] = 4'd4;
      addr_array[6] = 4'd5;
      addr_array[7] = 4'd6;
      addr_array[8] = 4'd7;
      addr_array[9] = 4'd8;
      addr_array[10] = 4'd9;
      addr_array[11] = 4'd10;
      addr_array[12] = 4'd11;
      addr_array[13] = 4'd12;
      addr_array[14] = 4'd13;
      addr_array[15] = 4'd14;
      addr_array[16] = 4'd15;
    for(j_=0; j_<17; j_=j_+1) begin
        en=en_array[j_];
        addr=addr_array[j_];
      #0.5;
        data_array[j_]=data;
      @(negedge clk);
end
  end
  rominfr dut (.en(en), .addr(addr), .data(data), .clk(clk));
endmodule
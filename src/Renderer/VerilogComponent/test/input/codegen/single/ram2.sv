	

  1 //-----------------------------------------------------
  2 // Design Name : ram_dp_sr_sw
  3 // File Name   : ram_dp_sr_sw.v
  4 // Function    : Synchronous read write RAM
  5 // Coder       : Deepak Kumar Tala
  6 //-----------------------------------------------------
  7 module ram_dp_sr_sw (
  8 clk       , // Clock Input
  9 address_0 , // address_0 Input
 10 data_0    , // data_0 bi-directional
 11 cs_0      , // Chip Select
 12 we_0      , // Write Enable/Read Enable
 13 oe_0      , // Output Enable
 14 address_1 , // address_1 Input
 15 data_1    , // data_1 bi-directional
 16 cs_1      , // Chip Select
 17 we_1      , // Write Enable/Read Enable
 18 oe_1        // Output Enable
 19 ); 
 20 
 21 parameter data_0_WIDTH = 8 ;
 22 parameter ADDR_WIDTH = 8 ;
 23 parameter RAM_DEPTH = 1 << ADDR_WIDTH;
 24 
 25 //--------------Input Ports----------------------- 
 26 input [ADDR_WIDTH-1:0] address_0 ;
 27 input cs_0 ;
 28 input we_0 ;
 29 input oe_0 ; 
 30 input [ADDR_WIDTH-1:0] address_1 ;
 31 input cs_1 ;
 32 input we_1 ;
 33 input oe_1 ; 
 34 
 35 //--------------Inout Ports----------------------- 
 36 inout [data_0_WIDTH-1:0] data_0 ; 
 37 inout [data_0_WIDTH-1:0] data_1 ;
 38 
 39 //--------------Internal variables---------------- 
 40 bit [data_0_WIDTH-1:0] data_0_out ; 
 41 bit [data_0_WIDTH-1:0] data_1_out ;
 42 bit [data_0_WIDTH-1:0] mem [0:RAM_DEPTH-1];
 43 
 44 //--------------Code Starts Here------------------ 
 45 // Memory Write Block 
 46 // Write Operation : When we_0 = 1, cs_0 = 1
 47 always_ff @ (posedge clk)
 48 begin : MEM_WRITE
 49   if ( cs_0 && we_0 ) begin
 50      mem[address_0] <= data_0;
 51   end else if (cs_1 && we_1) begin 
 52      mem[address_1] <= data_1;
 53   end
 54 end
 55 
 56   
 57 
 58 // Tri-State Buffer control 
 59 // output : When we_0 = 0, oe_0 = 1, cs_0 = 1
 60 assign data_0 = (cs_0 && oe_0 &&  ! we_0) ? data_0_out : 8'bz; 
 61 
 62 // Memory Read Block 
 63 // Read Operation : When we_0 = 0, oe_0 = 1, cs_0 = 1
 64 always @ (posedge clk)
 65 begin : MEM_READ_0
 66   if (cs_0 &&  ! we_0 && oe_0) begin
 67     data_0_out <= mem[address_0]; 
 68   end else begin
 69     data_0_out <= 0; 
 70   end  
 71 end 
 72 
 73 //Second Port of RAM
 74 // Tri-State Buffer control 
 75 // output : When we_0 = 0, oe_0 = 1, cs_0 = 1
 76 assign data_1 = (cs_1 && oe_1 &&  ! we_1) ? data_1_out : 8'bz; 
 77 // Memory Read Block 1 
 78 // Read Operation : When we_1 = 0, oe_1 = 1, cs_1 = 1
 79 always @ (posedge clk)
 80 begin : MEM_READ_1
 81   if (cs_1 &&  ! we_1 && oe_1) begin
 82     data_1_out <= mem[address_1]; 
 83   end else begin
 84     data_1_out <= 0;
 85   end
 86 end
 87 
 88 endmodule // End of Module ram_dp_sr_sw
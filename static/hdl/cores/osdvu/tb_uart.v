`timescale 1ns / 1ps
//*************************************************************
//
// Module: tb_uart
//
//** Description **********************************************   
//    
//  Test bench for uart module. 
//
//** HISTORY **************************************************
//
// 07 Apr 13: initial design and test
//
// 11 Apr 13: clean up...
//
//*************************************************************

module tb_uart;

    /* Test bench specific parameters */
        localparam baud_rate = 19200;
        localparam sys_clk_freq = 100000000;

    /* Clock simulation */
        localparam clock_tick = 10;         // in nanoseconds
    
    /* General shortcuts */
        localparam T = 1'b1;
        localparam F = 1'b0;
        
//** SIGNAL DECLARATIONS **************************************
 
    /* UUT signals */
        reg clk;
        reg reset;
        reg rx;
        wire tx;
        reg transmit;
        reg [7:0]tx_byte; 
        wire received; 
        wire [7:0] rx_byte;
        wire is_receiving; 
        wire is_transmitting; 
        wire recv_error; 
        
        
        wire [3:0] rx_samples;
    wire [3:0]rx_sample_countdown;
       
       
     //  reg i;
       
       reg[7:0] cnt;
        reg[7:0] rcv_reg;
       
//** INSTANTIATE THE UNIT UNDER TEST(UUT)**********************

    uart 
        #(.baud_rate(baud_rate),
        .sys_clk_freq(sys_clk_freq)
     )
    uart(
        .clk(clk), 
        .rst(reset), 
        .rx(rx), 
        .tx(tx), 
        .transmit(transmit), 
        .tx_byte(tx_byte), 
        .received(received), 
        .rx_byte(rx_byte), 
        .is_receiving(is_receiving), 
        .is_transmitting(is_transmitting), 
        .recv_error(recv_error),
        .rx_samples(rx_samples),
        .rx_sample_countdown(rx_sample_countdown) 
    );

//** Clock ****************************************************     
 
    always begin
        clk = T;
		#(clock_tick/2);
		clk = F;
		#(clock_tick/2);
	end

//** UUT Tests ************************************************ 

    initial begin
    
        initial_conditions();
        reset_UUT();
        
        test_rcvr(8'h55);
        test_rcvr(8'hAA);
        test_rcvr(8'h45);
        
        
        xmit_char(8'hAA);                   // should transmit one byte
        wait_while_transmitting;
        
        transmit = T;                       // character should be transmitted once
        delay((1e8/baud_rate) * 10 * 2);
        transmit = F;
        
        xmit_char(8'h55);                   // transmit one byte
        wait_while_transmitting;
             
             
        //FIX ME - add tests for receive functions...     
             
        $finish;
  
    end
    
    
    
// The testbench receiver is constantly running...
    always begin
//        while (tx == T)begin
//            @(posedge clk);
//            rcv_reg = 0;
//        end      

    wait(!tx);
        rcv_reg = 0;
        delay(1e8/baud_rate);       // delay for start bit
        
         for(cnt = 0; cnt<8; cnt = cnt + 1) begin
            delay((1e8/baud_rate) / 2);
            rcv_reg = {tx, rcv_reg[7:1]};
            delay((1e8/baud_rate) / 2); 
        end
        
       // delay((1e8/baud_rate)/2);         // delay fro stop bit
       wait(tx);
	end
    
    
    

//** Tasks **************************************************** 
    
    task initial_conditions(); begin
        repeat(5) @(posedge clk)
        reset = F; 
        transmit = F;       
    end endtask  


    task reset_UUT(); begin
        @(posedge clk)
        reset = T;
        @(posedge clk)
        reset = F;
    end endtask  


    task delay(input integer N); begin
        repeat(N) @(posedge clk);
    end endtask  
    
    
    
    task xmit_char(input integer C); begin   
        tx_byte = C;  
        @(posedge clk)
        transmit = T;
        @(posedge clk)
        transmit = F;
    end endtask 
    
    
    task wait_while_transmitting; begin
        @(negedge is_transmitting);
        repeat(10) @(posedge clk);
    end endtask



    task test_rcvr (input reg[7:0] C); 
        integer i;
        reg [7:0] shift_reg;
    begin
        shift_reg = C;
        @(posedge clk)
            rx = F;                         // send start bit
            delay(1e8/baud_rate);
            
        for(i = 0; i<8; i = i + 1) begin    // send serial data LSB first
            rx = shift_reg[0];
            shift_reg = shift_reg >> 1;
            delay(1e8/baud_rate);
        end
        
            rx = T;                         // send stop bit
            delay(1e8/baud_rate);
            
        if(C == rx_byte)begin
            $display("rcvr test pass");
        end else begin
            $display("rcvr test fail %d not equal %d", C, rx_byte);
        end
        
    end endtask









endmodule

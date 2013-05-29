module tb_fpga;

reg fastclk;
reg reset;
reg [3:0] param_data;
reg param_clk;
reg audio_req;
reg audio_ack;
wire dac_bit;

initial begin
	fastclk <= 0;
	reset <= 0;
	param_data <= 0;
	param_clk <= 0;
	audio_req <= 0;
	audio_ack <= 0;
	#10 reset <= 1;
	#50 reset <= 0;
	#20 repeat (2500000) begin
		#25 fastclk <= 1;
		#25 fastclk <= 0;
	end
end

fpga dut(
    fastclk,
    reset,
    param_data,
    param_clk,
    audio_req,
    audio_ack,
    dac_bit
);

endmodule

// Copyright 2024 EPFL
// Solderpad Hardware License, Version 2.1, see LICENSE.md for details.
// SPDX-License-Identifier: Apache-2.0 WITH SHL-2.1
//
// Author: Danilo Cammarata

module quadrilatero_mac_float (
    input  logic        clk_i         ,
    input  logic        rst_ni        ,
    input  logic [31:0] data_i        ,
    input  logic [31:0] weight_i      ,
    input  logic [31:0] acc_i         ,
    input  quadrilatero_pkg::datatype_t   datatype_i    ,
    input  logic        valid_i       ,

    output logic        mac_finished_o,
    output logic [31:0] acc_o
);

  logic               in_ready_out;
  logic               busy_out    ;
  logic               tag_out     ;
  logic               ready       ;
  fpnew_pkg::status_t status_out  ;
  fpnew_pkg::operation_e operation;
  fpnew_pkg::fp_format_e fp_src_fmt;
  logic op_vect;

  // -------------------------------
  // Determine FPU unit signals
  // -------------------------------
  always_comb begin: gen_fpnew_signals
    operation  = fpnew_pkg::FMADD;
    fp_src_fmt = fpnew_pkg::FP32 ;
    op_vect    = 1'b0;

    unique case (datatype_i)
      quadrilatero_pkg::SIZE_32: begin
        operation  = fpnew_pkg::FMADD;
        fp_src_fmt = fpnew_pkg::FP32 ;
        op_vect    = 1'b0;
      end
      quadrilatero_pkg::SIZE_16: begin
        operation  = fpnew_pkg::SDOTP;
        fp_src_fmt = fpnew_pkg::FP16 ;
        op_vect    = 1'b1;
      end
      quadrilatero_pkg::SIZE_8: begin
        operation  = fpnew_pkg::SDOTP;
        fp_src_fmt = fpnew_pkg::FP8  ;
        op_vect    = 1'b1;
      end
      default: begin
      end
    endcase
  end

  // -------------------------------
  // Configure FPU units
  // -------------------------------
  localparam fpnew_pkg::fpu_features_t RV32_QUAD = '{
    Width:         32,
    EnableVectors: 1'b1,
    EnableNanBox:  1'b1,
    FpFmtMask:     6'b101100,
    IntFmtMask:    4'b0010
  };

  localparam fpnew_pkg::fpu_implementation_t FPUImplementation [1] = '{
    '{
        PipeRegs: // FMA Block
                  '{// FP32 FP64 FP16 FP8 FP16alt FP8alt
                    '{   1,   2,   0,  0,   0,      0   },   // FMA Block
                    '{   1,   1,   1,  1,   1,      1   },   // DIVSQRT
                    '{   1,   1,   1,  1,   1,      1   },   // NONCOMP
                    '{   2,   2,   2,  2,   2,      2   },   // CONV
                    '{   1,   1,   1,  1,   1,      1   }    // DOTP
                    },
        UnitTypes: '{'{fpnew_pkg::PARALLEL,
                       fpnew_pkg::DISABLED,
                       fpnew_pkg::DISABLED,
                       fpnew_pkg::DISABLED,
                       fpnew_pkg::DISABLED,
                       fpnew_pkg::DISABLED},  // FMA
                    '{fpnew_pkg::DISABLED,
                        fpnew_pkg::DISABLED,
                        fpnew_pkg::DISABLED,
                        fpnew_pkg::DISABLED,
                        fpnew_pkg::DISABLED,
                        fpnew_pkg::DISABLED}, // DIVSQRT
                    '{fpnew_pkg::DISABLED,
                        fpnew_pkg::DISABLED,
                        fpnew_pkg::DISABLED,
                        fpnew_pkg::DISABLED,
                        fpnew_pkg::DISABLED,
                        fpnew_pkg::DISABLED}, // NONCOMP
                    '{fpnew_pkg::DISABLED,
                        fpnew_pkg::DISABLED,
                        fpnew_pkg::DISABLED,
                        fpnew_pkg::DISABLED,
                        fpnew_pkg::DISABLED,
                        fpnew_pkg::DISABLED},   // CONV
                    '{fpnew_pkg::MERGED,
                        fpnew_pkg::DISABLED,
                        fpnew_pkg::MERGED,
                        fpnew_pkg::MERGED,
                        fpnew_pkg::DISABLED,
                        fpnew_pkg::DISABLED}},  // DOTP
        PipeConfig: fpnew_pkg::BEFORE
    }
  };
  
  // -------------------------------
  // Instantiate FPU units
  // -------------------------------
  fpnew_top #(
    // FPU configuration
    .Features       (RV32_QUAD),
    .Implementation (FPUImplementation[0])
  ) fpu_inst (
    .clk_i          ,
    .rst_ni         ,
    .hart_id_i      (/* Unused */),
    // Input signals
    .operands_i     ({acc_i,weight_i,data_i}),
    .rnd_mode_i     (fpnew_pkg::RNE),
    .op_i           (operation),
    .op_mod_i       (1'b0),
    .src_fmt_i      (fp_src_fmt),
    .dst_fmt_i      (fpnew_pkg::FP32),
    .int_fmt_i      (fpnew_pkg::INT32),
    .vectorial_op_i (op_vect),
    .tag_i          (1'b0),
    .simd_mask_i    ('1),
    // Input Handshake
    .in_valid_i     (valid_i  ),
    .in_ready_o     (in_ready_out),
    .flush_i        (1'b0),
    // Output signals
    .result_o       (acc_o),
    .status_o       (status_out),
    .tag_o          (tag_out),
    // Output handshake
    .out_valid_o    (mac_finished_o),
    .out_ready_i    (ready),
    // Indication of valid data in flight
    .busy_o         (busy_out)
  );

  always_ff @(posedge clk_i or negedge rst_ni) begin : seq_block
    if (!rst_ni) begin
      ready        <= '0;
    end else begin
      ready        <= valid_i;
    end
  end
endmodule

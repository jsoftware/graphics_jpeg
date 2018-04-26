NB. standard tables suggested by JPEG group

cat=: <: 1 2 4 8 16 32 64 128 256 512 1024 2048 4096 8192 16384 32768 65536

std_luminance_quant_tbl=: 8 8$ ". LF -.~ (0 :0)
    16  11  10  16  24  40  51  61
    12  12  14  19  26  58  60  55
    14  13  16  24  40  57  69  56
    14  17  22  29  51  87  80  62
    18  22  37  56  68 109 103  77
    24  35  55  64  81 104 113  92
    49  64  78  87 103 121 120 101
    72  92  95  98 112 100 103  99
)

std_chrominance_quant_tbl=: 8 8$ ". LF -.~ (0 :0)
    17  18  24  47  99  99  99  99
    18  21  26  66  99  99  99  99
    24  26  56  99  99  99  99  99
    47  66  99  99  99  99  99  99
    99  99  99  99  99  99  99  99
    99  99  99  99  99  99  99  99
    99  99  99  99  99  99  99  99
    99  99  99  99  99  99  99  99
)

NB. element 0 for 1 bit, 1 for 2 bits etc.
bits_dc_luminance=: 0 1 5 1 1 1 1 1 1 0 0 0 0 0 0 0

val_dc_luminance=: 0 1 2 3 4 5 6 7 8 9 10 11

bits_dc_chrominance=: 0 3 1 1 1 1 1 1 1 1 1 0 0 0 0 0

val_dc_chrominance=: 0 1 2 3 4 5 6 7 8 9 10 11

bits_ac_luminance=: 0 2 1 3 3 2 4 3 5 5 4 4 0 0 1 16b7d

val_ac_luminance=: ". LF -.~ (0 :0)
      16b01 16b02 16b03 16b00 16b04 16b11 16b05 16b12
      16b21 16b31 16b41 16b06 16b13 16b51 16b61 16b07
      16b22 16b71 16b14 16b32 16b81 16b91 16ba1 16b08
      16b23 16b42 16bb1 16bc1 16b15 16b52 16bd1 16bf0
      16b24 16b33 16b62 16b72 16b82 16b09 16b0a 16b16
      16b17 16b18 16b19 16b1a 16b25 16b26 16b27 16b28
      16b29 16b2a 16b34 16b35 16b36 16b37 16b38 16b39
      16b3a 16b43 16b44 16b45 16b46 16b47 16b48 16b49
      16b4a 16b53 16b54 16b55 16b56 16b57 16b58 16b59
      16b5a 16b63 16b64 16b65 16b66 16b67 16b68 16b69
      16b6a 16b73 16b74 16b75 16b76 16b77 16b78 16b79
      16b7a 16b83 16b84 16b85 16b86 16b87 16b88 16b89
      16b8a 16b92 16b93 16b94 16b95 16b96 16b97 16b98
      16b99 16b9a 16ba2 16ba3 16ba4 16ba5 16ba6 16ba7
      16ba8 16ba9 16baa 16bb2 16bb3 16bb4 16bb5 16bb6
      16bb7 16bb8 16bb9 16bba 16bc2 16bc3 16bc4 16bc5
      16bc6 16bc7 16bc8 16bc9 16bca 16bd2 16bd3 16bd4
      16bd5 16bd6 16bd7 16bd8 16bd9 16bda 16be1 16be2
      16be3 16be4 16be5 16be6 16be7 16be8 16be9 16bea
      16bf1 16bf2 16bf3 16bf4 16bf5 16bf6 16bf7 16bf8
      16bf9 16bfa
)
bits_ac_chrominance=: 0 2 1 2 4 4 3 4 7 5 4 4 0 1 2 16b77

val_ac_chrominance=: ". LF -.~ (0 :0)
      16b00 16b01 16b02 16b03 16b11 16b04 16b05 16b21
      16b31 16b06 16b12 16b41 16b51 16b07 16b61 16b71
      16b13 16b22 16b32 16b81 16b08 16b14 16b42 16b91
      16ba1 16bb1 16bc1 16b09 16b23 16b33 16b52 16bf0
      16b15 16b62 16b72 16bd1 16b0a 16b16 16b24 16b34
      16be1 16b25 16bf1 16b17 16b18 16b19 16b1a 16b26
      16b27 16b28 16b29 16b2a 16b35 16b36 16b37 16b38
      16b39 16b3a 16b43 16b44 16b45 16b46 16b47 16b48
      16b49 16b4a 16b53 16b54 16b55 16b56 16b57 16b58
      16b59 16b5a 16b63 16b64 16b65 16b66 16b67 16b68
      16b69 16b6a 16b73 16b74 16b75 16b76 16b77 16b78
      16b79 16b7a 16b82 16b83 16b84 16b85 16b86 16b87
      16b88 16b89 16b8a 16b92 16b93 16b94 16b95 16b96
      16b97 16b98 16b99 16b9a 16ba2 16ba3 16ba4 16ba5
      16ba6 16ba7 16ba8 16ba9 16baa 16bb2 16bb3 16bb4
      16bb5 16bb6 16bb7 16bb8 16bb9 16bba 16bc2 16bc3
      16bc4 16bc5 16bc6 16bc7 16bc8 16bc9 16bca 16bd2
      16bd3 16bd4 16bd5 16bd6 16bd7 16bd8 16bd9 16bda
      16be2 16be3 16be4 16be5 16be6 16be7 16be8 16be9
      16bea 16bf2 16bf3 16bf4 16bf5 16bf6 16bf7 16bf8
      16bf9 16bfa
)

jpegNaturalOrder=: ". LF-.~ (0 : 0)
 0 1 8   16   9 2 3   10   17   24   32   25   18 11 4 5 12
 19 26   33   40 48   41   34   27   20   13   6 7 14 21 28
 35 42   49   56 57   50   43   36   29   22   15 23 30 37
 44 51   58   59 52   45   38   31   39   46   53 60 61 54
 47 55   62   63
)

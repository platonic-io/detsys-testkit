#!/usr/bin/env Rscript

Input4=("
workload  throughput
journal-4                  7024.48
journal-4                  7144.94
journal-4                  7187.51
journal-4                  7061.21
journal-4                  6826.70
journal-4                  6683.50
journal-4                  6896.12
journal-4                  6919.83
journal-4                  7307.94
journal-4                  7102.50
sqlite-4                  9017.16
sqlite-4                  8809.81
sqlite-4                  8613.81
sqlite-4                  8467.63
sqlite-4                  8600.22
sqlite-4                  8628.48
sqlite-4                  8244.84
sqlite-4                  8075.54
sqlite-4                  8875.07
sqlite-4                  8792.31
")

df4 = read.table(textConnection(Input4),header=TRUE)
bartlett.test(df4$throughput ~ df4$workload, data=df4)

message("If p-value >= 0.05, use var.equal=TRUE below")

t.test(df4$throughput ~ df4$workload, data=df4,
       var.equal=TRUE,
       conf.level=0.95)

Input8=("
workload throughput
journal-8                  6765.83
journal-8                  6977.90
journal-8                  7089.01
journal-8                  6481.04
journal-8                  6717.82
journal-8                  6426.51
journal-8                  6837.21
journal-8                  6454.03
journal-8                  6910.78
journal-8                  6712.71
sqlite-8                 7561.52
sqlite-8                 7811.76
sqlite-8                 7904.59
sqlite-8                 7773.24
sqlite-8                 7411.04
sqlite-8                 7631.94
sqlite-8                 7771.31
sqlite-8                 7641.79
sqlite-8                 8232.62
sqlite-8                 7501.84
")

df8 = read.table(textConnection(Input8),header=TRUE)
bartlett.test(df8$throughput ~ df8$workload, data=df8)

t.test(df8$throughput ~ df8$workload, data=df8,
       var.equal=TRUE,
       conf.level=0.95)

Input16=("
workload throughput
journal-16                  6661.66
journal-16                  6759.78
journal-16                  7032.06
journal-16                  6662.63
journal-16                  6803.18
journal-16                  6579.79
journal-16                  6492.99
journal-16                  6724.89
journal-16                  6595.02
journal-16                  6643.49
sqlite-16                 6165.21
sqlite-16                 6070.99
sqlite-16                 6421.87
sqlite-16                 6536.94
sqlite-16                 6540.05
sqlite-16                 5714.42
sqlite-16                 6162.20
sqlite-16                 6454.41
sqlite-16                 6141.35
sqlite-16                 6010.05
")

df16 = read.table(textConnection(Input16),header=TRUE)
bartlett.test(df16$throughput ~ df16$workload, data=df16)

t.test(df16$throughput ~ df16$workload, data=df16,
       var.equal=TRUE,
       conf.level=0.95)

Input32=("
workload throughput
journal-32                  6745.54
journal-32                  6435.81
journal-32                  6325.01
journal-32                  6511.75
journal-32                  6379.79
journal-32                  6293.02
journal-32                  6595.44
journal-32                  6453.33
journal-32                  6371.21
journal-32                  6298.35
sqlite-32                  6060.26
sqlite-32                  5860.26
sqlite-32                  6105.28
sqlite-32                  6047.47
sqlite-32                  6067.61
sqlite-32                  5815.05
sqlite-32                  6016.67
sqlite-32                  6152.74
sqlite-32                  6077.45
sqlite-32                  5954.26
")

df32 = read.table(textConnection(Input32),header=TRUE)
bartlett.test(df32$throughput ~ df32$workload, data=df32)

message("If p-value >= 0.05, use var.equal=TRUE below")

t.test(df32$throughput ~ df32$workload, data=df32,
       var.equal=TRUE,
       conf.level=0.95)

Input64=("
workload throughput
journal-64   6193.51
journal-64   6492.17
journal-64   6093.18
journal-64   6283.59
journal-64   6253.92
journal-64   6363.34
journal-64   6131.53
journal-64   6468.23
journal-64   6147.20
journal-64   5957.34
sqlite-64    6358.80
sqlite-64    5939.43
sqlite-64    6324.36
sqlite-64    5882.36
sqlite-64    5941.00
sqlite-64    6141.81
sqlite-64    5792.27
sqlite-64    5987.80
sqlite-64    6202.03
sqlite-64    5980.05
")

df64 = read.table(textConnection(Input64),header=TRUE)
bartlett.test(df64$throughput ~ df64$workload, data=df64)

message("If p-value >= 0.05, use var.equal=TRUE below")

t.test(df64$throughput ~ df64$workload, data=df64,
       var.equal=TRUE,
       conf.level=0.95)

Input128=("
workload throughput
journal-128                6140.07
journal-128                6021.51
journal-128                6167.16
journal-128                6446.74
journal-128                6134.16
journal-128                6053.93
journal-128                6236.71
journal-128                6354.00
journal-128                6014.61
journal-128                5914.42
journal-128                  6275.96
journal-128                  6130.14
journal-128                  6069.00
journal-128                  6226.67
journal-128                  6242.63
journal-128                  5969.20
journal-128                  6094.10
journal-128                  6111.73
journal-128                  5845.98
journal-128                  6143.18
sqlite-128               7258.15
sqlite-128               6995.89
sqlite-128               6825.82
sqlite-128               7054.20
sqlite-128               6895.98
sqlite-128               6849.55
sqlite-128               6704.36
sqlite-128               6704.68
sqlite-128               6849.77
sqlite-128               6847.92
sqlite-128                  6886.70
sqlite-128                  6873.38
sqlite-128                  6567.82
sqlite-128                  6879.67
sqlite-128                  6786.69
sqlite-128                  6807.89
sqlite-128                  6940.66
sqlite-128                  6526.45
sqlite-128                  6527.77
sqlite-128                  6842.96
")

df128 = read.table(textConnection(Input128),header=TRUE)
bartlett.test(df128$throughput ~ df128$workload, data=df128)

t.test(df128$throughput ~ df128$workload, data=df128,
       var.equal=TRUE,
       conf.level=0.95)

Input256=("
workload throughput
journal-256                6174.16
journal-256                6009.00
journal-256                6133.48
journal-256                6118.66
journal-256                6251.49
journal-256                6444.50
journal-256                6289.08
journal-256                6090.86
journal-256                6107.82
journal-256                6178.65
sqlite-256               5738.44
sqlite-256               5733.58
sqlite-256               6605.20
sqlite-256               5822.83
sqlite-256               5990.19
sqlite-256               5467.91
sqlite-256               6216.10
sqlite-256               6368.61
sqlite-256               6097.44
sqlite-256               6095.78
")

df256 = read.table(textConnection(Input256),header=TRUE)
bartlett.test(df256$throughput ~ df256$workload, data=df256)

t.test(df256$throughput ~ df256$workload, data=df256,
       var.equal=TRUE,
       conf.level=0.95)

Input512=("
workload throughput
journal-512                5889.80
journal-512                5967.63
journal-512                5871.33
journal-512                6129.90
journal-512                6038.78
journal-512                6000.86
journal-512                5780.60
journal-512                5756.72
journal-512                6042.59
journal-512                5895.52
sqlite-512               4851.46
sqlite-512               4175.24
sqlite-512               5001.94
sqlite-512               6861.95
sqlite-512               4624.16
sqlite-512               6735.40
sqlite-512               6603.62
sqlite-512               6625.08
sqlite-512               6482.68
sqlite-512               6430.65
")

df512 = read.table(textConnection(Input512),header=TRUE)
bartlett.test(df512$throughput ~ df512$workload, data=df512)

t.test(df512$throughput ~ df512$workload, data=df512,
       var.equal=TRUE,
       conf.level=0.95)

Input1024=("
workload throughput
journal-1024                 6060.03
journal-1024                 5713.12
journal-1024                 5984.37
journal-1024                 5642.23
journal-1024                 5733.45
journal-1024                 5664.29
journal-1024                 5667.71
journal-1024                 6007.26
journal-1024                 5949.81
journal-1024                 5859.07
sqlite-1024                4411.89
sqlite-1024                4082.66
sqlite-1024                4422.64
sqlite-1024                3827.05
sqlite-1024                4253.10
sqlite-1024                5100.49
sqlite-1024                3986.22
sqlite-1024                4175.26
sqlite-1024                4204.46
sqlite-1024                4017.86
")

df1024 = read.table(textConnection(Input1024),header=TRUE)
bartlett.test(df1024$throughput ~ df1024$workload, data=df1024)

t.test(df1024$throughput ~ df1024$workload, data=df1024,
       var.equal=TRUE,
       conf.level=0.95)

Input2048=("
workload throughput
journal-2048                 5541.96
journal-2048                 5411.09
journal-2048                 5571.17
journal-2048                 5606.22
journal-2048                 5276.86
journal-2048                 5259.30
journal-2048                 5467.38
journal-2048                 5464.42
journal-2048                 5355.09
journal-2048                 5496.44
sqlite-2048                3504.50
sqlite-2048                3840.96
sqlite-2048                3737.51
sqlite-2048                3874.28
sqlite-2048                3541.43
sqlite-2048                4334.83
sqlite-2048                3773.90
sqlite-2048                3805.36
sqlite-2048                3843.81
sqlite-2048                3773.41
")

df2048 = read.table(textConnection(Input2048),header=TRUE)
bartlett.test(df2048$throughput ~ df2048$workload, data=df2048)

t.test(df2048$throughput ~ df2048$workload, data=df2048,
       var.equal=TRUE,
       conf.level=0.95)

Input4096=("
workload throughput
journal-4096                 4797.12
journal-4096                 5047.38
journal-4096                 4978.69
journal-4096                 4781.63
journal-4096                 5006.89
journal-4096                 4916.87
journal-4096                 4821.37
journal-4096                 4983.61
journal-4096                 5004.28
journal-4096                 4905.44
sqlite-4096                3593.53
sqlite-4096                3377.08
sqlite-4096                3545.17
sqlite-4096                3429.96
sqlite-4096                3763.35
sqlite-4096                3509.91
sqlite-4096                3499.49
sqlite-4096                3393.35
sqlite-4096                3665.23
sqlite-4096                3718.05
")

df4096 = read.table(textConnection(Input4096),header=TRUE)
bartlett.test(df4096$throughput ~ df4096$workload, data=df4096)

t.test(df4096$throughput ~ df4096$workload, data=df4096,
       var.equal=TRUE,
       conf.level=0.95)

c4    <- c(7015.473, 8612.487)
c8    <- c(6737.284, 7724.165)
c16   <- c(6695.549, 6221.749)
c32   <- c(6440.925, 6015.705)
c64   <- c(6238.401, 6054.991)
c128  <- c(6129.595, 6831.315)
c256  <- c(6179.770, 6013.608)
c512  <- c(5937.373, 5839.218)
c1024 <- c(5828.134, 4248.163)
c2048 <- c(5444.993, 3802.999)
c4096 <- c(4924.328, 3549.512)

df <- data.frame(c4, c8, c16, c32, c64, c128, c256, c512, c1024, c2048, c4096)
names(df) <- c("4", "8", "16", "32", "64", "128", "256", "512", "1024", "2048", "4096")

colours=(c("red", "blue"))

# X11()
jpeg("/tmp/journal-benchmark.jpg",
     width=640)

barplot(height=as.matrix(df),
        xlab="# of concurrent clients",
        ylab="Throughput (reqs/s)",
        beside=TRUE,
        col=colours)
legend ("topright", c("journal", "sqlite"), fill=colours)

#message("Press return to continue")
#invisible(readLines("stdin", n=1))
dev.off()

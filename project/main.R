library(EEM)

# names of data files
Arctic_names <- c("1701.txt", "1702.txt", "1704.txt", "1706.txt", "1711.txt", "1712.txt", "1727.txt", "1728.txt", "1729.txt", "1730.txt", "1732.txt", "1733.txt", "1734.txt")
Africa_names <- c("1.1_70..txt", "1.2_21.txt", "1.3_68.txt", "1.4_114.txt", "1.5_11.txt", "1.6_37.txt", "2.3_5.txt", "2.4_7.txt", "3.1_14.txt", "3.2_69.txt", "3.4_20.txt", "3.5_43.txt", "4.1_45.txt", "4.2_80.txt", "4.3_84.txt", "4.4_87.txt", "4.5_108.txt", "4.6_88.txt", "5.1_90.txt", "5.2_2.txt", "5.3_66.txt", "5.4_92.txt", "5.5_28.txt", "5.6_95.txt")

# read data
Arctic_data <- readEEM(Arctic_names)
Africa_data <- readEEM(Africa_names)

# clean data
Arctic_scat <- delScattering2(Arctic_data, rep = 0)
Africa_scat <- delScattering2(Africa_data, rep = 0)


# get C group
Arctic_C_group <- cutEEM(Arctic_scat, cutEX = 351:400, cutEM = 481:600)
Arctic_C_group <- cutEEM(Arctic_C_group, cutEX = 0:319, cutEM = 0:419)
Africa_C_group <- cutEEM(Africa_scat, cutEX = 351:600, cutEM = 481:600)
Africa_C_group <- cutEEM(Africa_C_group, cutEX = 0:319, cutEM = 0:419)

# get A group
Arctic_A_group <- cutEEM(Arctic_scat, cutEX = 261:400, cutEM = 481:600)
Arctic_A_group <- cutEEM(Arctic_A_group, cutEX = 0:249, cutEM = 0:379)
Africa_A_group <- cutEEM(Africa_scat, cutEX = 261:600, cutEM = 481:600)
Africa_A_group <- cutEEM(Africa_A_group, cutEX = 0:249, cutEM = 0:379)

# get M group
Arctic_M_group <- cutEEM(Arctic_scat, cutEX = 321:400, cutEM = 421:600)
Arctic_M_group <- cutEEM(Arctic_M_group, cutEX = 0:309, cutEM = 0:379)
Africa_M_group <- cutEEM(Africa_scat, cutEX = 321:600, cutEM = 421:600)
Africa_M_group <- cutEEM(Africa_M_group, cutEX = 0:309, cutEM = 0:379)

# get B group
Arctic_B_group <- cutEEM(Arctic_scat, cutEX = 281:400, cutEM = 321:600)
Arctic_B_group <- cutEEM(Arctic_B_group, cutEX = 0:269, cutEM = 0:299)
Africa_B_group <- cutEEM(Africa_scat, cutEX = 281:600, cutEM = 321:600)
Africa_B_group <- cutEEM(Africa_B_group, cutEX = 0:269, cutEM = 0:299)

# get T group
Arctic_T_group <- cutEEM(Arctic_scat, cutEX = 281:400, cutEM = 351:600)
Arctic_T_group <- cutEEM(Arctic_T_group, cutEX = 0:269, cutEM = 0:319)
Africa_T_group <- cutEEM(Africa_scat, cutEX = 281:600, cutEM = 351:600)
Africa_T_group <- cutEEM(Africa_T_group, cutEX = 0:269, cutEM = 0:319)


# get unfolded data
Arctic_C_unfold <- unfold(Arctic_C_group)
Africa_C_unfold <- unfold(Africa_C_group)

Arctic_A_unfold <- unfold(Arctic_A_group)
Africa_A_unfold <- unfold(Africa_A_group)

Arctic_M_unfold <- unfold(Arctic_M_group)
Africa_M_unfold <- unfold(Africa_M_group)

Arctic_B_unfold <- unfold(Arctic_B_group)
Africa_B_unfold <- unfold(Africa_B_group)

Arctic_T_unfold <- unfold(Arctic_T_group)
Africa_T_unfold <- unfold(Africa_T_group)


# get sums for each file by groups
Arctic_C_sum <- rowSums(Arctic_C_unfold)
Africa_C_sum <- rowSums(Africa_C_unfold)

Arctic_A_sum <- rowSums(Arctic_A_unfold)
Africa_A_sum <- rowSums(Africa_A_unfold)

Arctic_M_sum <- rowSums(Arctic_M_unfold)
Africa_M_sum <- rowSums(Africa_M_unfold)

Arctic_B_sum <- rowSums(Arctic_B_unfold)
Africa_B_sum <- rowSums(Africa_B_unfold)

Arctic_T_sum <- rowSums(Arctic_T_unfold)
Africa_T_sum <- rowSums(Africa_T_unfold)


# hist
png(filename="Arctic_C.png")
hist(Arctic_C_sum, main = "Arctic C group sums", xlab = "C group sums", breaks = 10, freq = TRUE)
dev.off()
png(filename="Africa_C.png")
hist(Africa_C_sum, main = "Africa C group sums", xlab = "C group sums", breaks = 10, freq = TRUE)
dev.off()

png(filename="Arctic_A.png")
hist(Arctic_A_sum, main = "Arctic A group sums", xlab = "A group sums", breaks = 10, freq = TRUE)
dev.off()
png(filename="Africa_A.png")
hist(Africa_A_sum, main = "Africa A group sums", xlab = "A group sums", breaks = 10, freq = TRUE)
dev.off()

png(filename="Arctic_M.png")
hist(Arctic_M_sum, main = "Arctic M group sums", xlab = "M group sums", breaks = 10, freq = TRUE)
dev.off()
png(filename="Africa_M.png")
hist(Africa_M_sum, main = "Africa M group sums", xlab = "M group sums", breaks = 10, freq = TRUE)
dev.off()

png(filename="Arctic_B.png")
hist(Arctic_B_sum, main = "Arctic B group sums", xlab = "B group sums", breaks = 10, freq = TRUE)
dev.off()
png(filename="Africa_B.png")
hist(Africa_B_sum, main = "Africa B group sums", xlab = "B group sums", breaks = 10, freq = TRUE)
dev.off()

png(filename="Arctic_T.png")
hist(Arctic_T_sum, main = "Arctic T group sums", xlab = "T group sums", breaks = 10, freq = TRUE)
dev.off()
png(filename="Africa_T.png")
hist(Africa_T_sum, main = "Africa T group sums", xlab = "T group sums", breaks = 10, freq = TRUE)
dev.off()
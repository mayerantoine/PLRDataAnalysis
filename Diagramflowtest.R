par(mar = c(1, 1, 1, 1))
openplotmat()
elpos <- coordinates (c(1, 2, 8, 16))
#fromto <- matrix(ncol = 2,byrow = T,data = c(1,2,1,3,3,4,3,5,3,6,3,7,7,9,7,8))
fromto <- matrix(ncol = 2,byrow = T,data = c(1,2,1,3,3,8,3,9,3,10,3,11,11,26,11,27))
nr <- nrow(fromto)
arrpos <- matrix(ncol = 2, nrow = nr)
for(i in 1:nr)
    arrpos[i,] <- straightarrow(to = elpos[fromto[i,2],],
                            from = elpos[fromto[i,1],],
                            lwd = 2,arr.pos = 0.6,
                            arr.length = 0,5)
textround(elpos[1,], 0.04, lab = "34500,(100%) Tot. Contacted", cex = 1)
textround(elpos[2,], 0.07, lab = "500,(88%) No outcomef found", cex = 0.8)
textround(elpos[3,], 0.07, lab = "3,(88%) Outcome found",  cex = 1)
textround(elpos[8,], 0.07, lab = "4,(88%) Dead",  cex = 1)
textround(elpos[9,], 0.02, lab = "5,(23%) Silent transfer",  cex = 1)
textround(elpos[10,], 0.02, lab = "6,(34%) Refuse To Came Back",  cex = 1)
textround(elpos[11,], 0.02, lab = "7,(88%) Return Back in Clinic",  cex = 1)
textround(elpos[26,], 0.02, lab = "8,(88%) Active to Date",  cex = 1)
textround(elpos[27,], 0.02, lab = "9,(88%) ",  cex = 1)


par(mar = c(1, 1, 1, 1))
openplotmat()
elpos <- coordinates (c(1, 2, 4, 2))
fromto <- matrix(ncol = 2,byrow = T,data = c(1,2,1,3,3,4,3,5,3,6,3,7,7,9,7,8))
nr <- nrow(fromto)
arrpos <- matrix(ncol = 2, nrow = nr)
for(i in 1:nr)
    arrpos[i,] <- straightarrow(to = elpos[fromto[i,2],],
                                from = elpos[fromto[i,1],],
                                lwd = 2,arr.pos = 0.6,
                                arr.length = 0.5)
textround(elpos[1,], 0.05, lab = "34500,(100%) Tot. Contacted", cex = 0.8)
textround(elpos[2,], 0.05, lab = "500,(88%) No outcomef found", cex = 0.8)
textround(elpos[3,], 0.05, lab = "4563,(88%) Outcome found",  cex = 0.8)
textround(elpos[4,], 0.05, lab = "234,(88%) Dead",  cex = 0.8)
textround(elpos[5,], 0.05, lab = "1256,(23%) Silent transfer",  cex = 0.8)
textround(elpos[6,], 0.05, lab = "1236,(34%) Refuse To Came Back",  cex = 0.8)
textround(elpos[7,], 0.05, lab = "4557,(88%) Return Back in Clinic",  cex = 0.8)
textround(elpos[9,], 0.05, lab = "3438,(88%) Active to Date",  cex = 0.8)
textround(elpos[8,], 0.05, lab = "322,(88%) ",  cex = 0.8)



par(mar = c(1, 1, 1, 1))
openplotmat()
elpos <- coordinates (c(1, 2, 4, 2))
fromto <- matrix(ncol = 2,byrow = T,data = c(1,2,1,3,3,4,3,5,3,6,3,7,7,9,7,8))
nr <- nrow(fromto)
arrpos <- matrix(ncol = 2, nrow = nr)
for(i in 1:nr)
    arrpos[i,] <- straightarrow(to = elpos[fromto[i,2],],
                                from = elpos[fromto[i,1],],
                                lwd = 2,arr.pos = 0.6,
                                arr.length = 0.5)
textround(elpos[1,], 0.15,0.06, lab = "34,500 (100%) Total Contacted", cex = 0.8)
textround(elpos[2,], 0.15,0.07, lab = "25500 (88%) No Outcomes found", cex = 0.8)
textround(elpos[3,], 0.15,0.07, lab = "4563 (88%) Outcomes found",  cex = 0.8)
textround(elpos[4,], 0.05,0.07, lab = c("Dead","234 (88%)"),  cex = 0.8)
textround(elpos[5,], 0.05,0.07, lab = c("Silent transfer","1256 (23%)"),  cex = 0.8)
textround(elpos[6,], 0.05,0.07, lab = c("Refuse Treatment","1236 (34%)"),  cex = 0.8)
textround(elpos[7,], 0.05,0.07, lab = c("Back in clinic","4557 (88%)"),  cex = 0.8)
textround(elpos[9,], 0.05,0.07, lab = c("active on treatment","3438 (88%)"),  cex = 0.8)
textround(elpos[8,], 0.05,0.07, lab = c("not in care","322 (88%)"),  cex = 0.8)

text(arrpos[3, 2]-0.07 , arrpos[4, 2], "Dead")

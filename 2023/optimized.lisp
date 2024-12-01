(defun part2 ()
  (loop for x from 4000 downto 1 do
    (loop for m from 4000 downto x do
      (loop for a from 4000 downto m do
        (loop for s from 4000 downto a
              for score = (optimized x m a s)
              when score do (print score))))))

(defun optimized (X M A S)
  (DECLARE ((INTEGER 1 4000) X M A S)
           (OPTIMIZE (SPEED 3) (SAFETY 0) (DEBUG 0) (SPACE 0)))
  (BLOCK WORKFLOW
    (TAGBODY
      (GO IN)
     ZM
      (WHEN (< X 1231) (GO R))
      (GO A)
     JZ
      (WHEN (< S 2913) (GO A))
      (WHEN (< X 2108) (GO R))
      (GO R)
     CQV
      (WHEN (> A 2101) (GO R))
      (GO A)
     HSX
      (WHEN (< M 1426) (GO XXN))
      (WHEN (< S 1619) (GO SQ))
      (GO QQB)
     ZBB
      (WHEN (> A 3503) (GO R))
      (WHEN (> S 2984) (GO R))
      (GO A)
     SKK
      (WHEN (> X 1878) (GO A))
      (WHEN (> S 1548) (GO R))
      (GO R)
     MN
      (WHEN (> X 576) (GO R))
      (WHEN (< X 195) (GO A))
      (WHEN (< S 303) (GO R))
      (GO A)
     HBJ
      (WHEN (> A 728) (GO HQQ))
      (GO GGL)
     LQ
      (WHEN (> X 1539) (GO A))
      (WHEN (> M 950) (GO A))
      (WHEN (> M 611) (GO A))
      (GO R)
     GX
      (WHEN (> A 3590) (GO ZMC))
      (GO TG)
     XPR
      (WHEN (< A 3371) (GO R))
      (WHEN (< S 3114) (GO A))
      (GO R)
     NF
      (WHEN (> M 2252) (GO A))
      (WHEN (< M 1915) (GO A))
      (WHEN (< X 1541) (GO A))
      (GO R)
     MFL
      (WHEN (< M 1608) (GO VK))
      (WHEN (> M 2471) (GO MP))
      (WHEN (< X 1842) (GO A))
      (GO A)
     ZP
      (WHEN (> M 1849) (GO R))
      (WHEN (> M 1720) (GO A))
      (WHEN (< S 2907) (GO A))
      (GO R)
     KP
      (WHEN (> X 2295) (GO R))
      (WHEN (> X 2106) (GO R))
      (GO A)
     DM
      (WHEN (> A 3142) (GO RK))
      (WHEN (< X 996) (GO KC))
      (WHEN (> M 2686) (GO LXC))
      (GO SVC)
     CQ
      (WHEN (< A 1392) (GO A))
      (WHEN (> S 2114) (GO R))
      (GO R)
     XLC
      (WHEN (> M 3640) (GO R))
      (WHEN (< S 1981) (GO CK))
      (WHEN (< X 1275) (GO R))
      (GO A)
     QHP
      (WHEN (> A 2255) (GO R))
      (WHEN (< M 2909) (GO A))
      (GO R)
     BMG
      (WHEN (> S 3584) (GO A))
      (GO R)
     TP
      (WHEN (> M 1848) (GO A))
      (GO MDS)
     MDS
      (WHEN (< A 3473) (GO A))
      (WHEN (> A 3501) (GO A))
      (WHEN (< S 3009) (GO A))
      (GO R)
     KDM
      (WHEN (< S 3771) (GO ZX))
      (GO VQ)
     XXN
      (WHEN (> A 1741) (GO FQ))
      (WHEN (< M 899) (GO BKF))
      (GO VM)
     LBM
      (WHEN (> M 3455) (GO A))
      (GO R)
     BQ
      (WHEN (> X 2598) (GO R))
      (GO A)
     VLR
      (WHEN (> M 1824) (GO RSN))
      (WHEN (< A 3330) (GO MR))
      (WHEN (< X 1229) (GO CH))
      (GO GFP)
     RFD
      (WHEN (< X 1380) (GO VXX))
      (WHEN (> M 2215) (GO A))
      (GO A)
     FNP
      (WHEN (< S 2124) (GO BH))
      (WHEN (< A 1450) (GO A))
      (GO A)
     PVH
      (WHEN (> S 1623) (GO A))
      (WHEN (< X 3503) (GO A))
      (WHEN (< S 1454) (GO R))
      (GO R)
     DHH
      (WHEN (> S 1900) (GO A))
      (GO R)
     CV
      (WHEN (> M 2619) (GO A))
      (GO ZBB)
     GZ
      (WHEN (< A 2237) (GO R))
      (GO A)
     HRF
      (WHEN (> A 1467) (GO R))
      (GO A)
     KQP
      (WHEN (> M 2434) (GO QST))
      (WHEN (> X 1969) (GO KGZ))
      (GO R)
     XLG
      (WHEN (< S 1949) (GO A))
      (WHEN (< X 1948) (GO CZJ))
      (GO R)
     XF
      (WHEN (< S 2587) (GO A))
      (WHEN (> M 2612) (GO R))
      (WHEN (> A 381) (GO A))
      (GO R)
     FDJ
      (WHEN (> A 3545) (GO A))
      (WHEN (< A 3532) (GO R))
      (WHEN (< S 2736) (GO R))
      (GO R)
     SHN
      (WHEN (> M 2612) (GO R))
      (WHEN (< M 1284) (GO R))
      (GO A)
     XX
      (WHEN (> A 3575) (GO A))
      (WHEN (< A 3518) (GO A))
      (WHEN (> S 2638) (GO FDJ))
      (GO A)
     SD
      (WHEN (< A 3248) (GO R))
      (WHEN (< M 3422) (GO JS))
      (WHEN (> S 1555) (GO A))
      (GO LZ)
     VD
      (WHEN (> S 1745) (GO TXB))
      (GO FS)
     HM
      (WHEN (> A 1763) (GO PG))
      (WHEN (< S 3711) (GO DVF))
      (GO XPP)
     XHJ
      (WHEN (< A 1056) (GO A))
      (WHEN (> M 3355) (GO A))
      (GO NTZ)
     TD
      (WHEN (> X 705) (GO A))
      (GO R)
     ZNZ
      (WHEN (> A 3929) (GO XG))
      (GO FCF)
     KK
      (WHEN (> S 2920) (GO A))
      (WHEN (> M 3806) (GO R))
      (GO NG)
     MX
      (WHEN (> M 987) (GO R))
      (WHEN (< M 500) (GO A))
      (GO A)
     RK
      (WHEN (< S 2159) (GO PH))
      (WHEN (< S 2256) (GO DXP))
      (WHEN (< M 2823) (GO JQ))
      (GO KM)
     LB
      (WHEN (> A 2647) (GO A))
      (WHEN (< S 1457) (GO A))
      (WHEN (> X 3452) (GO R))
      (GO R)
     SVC
      (WHEN (< S 2017) (GO KGT))
      (GO XFF)
     NK
      (WHEN (> X 1466) (GO A))
      (WHEN (< S 2004) (GO LXG))
      (WHEN (> S 2073) (GO SGD))
      (GO A)
     CZJ
      (WHEN (> M 873) (GO R))
      (WHEN (> X 1527) (GO R))
      (WHEN (> X 1352) (GO R))
      (GO A)
     DJ
      (WHEN (< S 3859) (GO A))
      (WHEN (< X 3298) (GO A))
      (WHEN (> A 730) (GO LL))
      (GO BX)
     LZQ
      (WHEN (< M 3709) (GO A))
      (WHEN (> A 758) (GO R))
      (WHEN (< A 697) (GO R))
      (GO R)
     XJN
      (WHEN (> A 2302) (GO A))
      (WHEN (> M 2041) (GO A))
      (WHEN (> S 3482) (GO R))
      (GO A)
     ZCB
      (WHEN (< A 276) (GO R))
      (WHEN (< M 2219) (GO R))
      (WHEN (> S 2027) (GO A))
      (GO A)
     HMN
      (WHEN (< X 2560) (GO A))
      (WHEN (< M 2069) (GO R))
      (WHEN (> X 3240) (GO R))
      (GO A)
     DXP
      (WHEN (< X 1615) (GO R))
      (WHEN (< A 3653) (GO A))
      (GO R)
     THV
      (WHEN (> M 1443) (GO FNP))
      (GO DNC)
     BPQ
      (WHEN (> A 2566) (GO R))
      (WHEN (> S 2238) (GO R))
      (WHEN (> A 2202) (GO R))
      (GO A)
     XKV
      (WHEN (< S 1708) (GO R))
      (WHEN (< S 1764) (GO A))
      (WHEN (< M 2486) (GO A))
      (GO A)
     XRS
      (WHEN (> S 2780) (GO VN))
      (GO HVH)
     TKQ
      (WHEN (< S 3890) (GO R))
      (WHEN (> S 3905) (GO A))
      (WHEN (< S 3900) (GO R))
      (GO A)
     CXV
      (WHEN (> M 3326) (GO ZL))
      (WHEN (< X 1480) (GO MZ))
      (WHEN (< X 2829) (GO RV))
      (GO LT)
     KZN
      (WHEN (< M 2943) (GO A))
      (WHEN (< X 3690) (GO A))
      (GO R)
     GD
      (WHEN (> S 1936) (GO A))
      (WHEN (> X 3201) (GO R))
      (WHEN (> M 3130) (GO A))
      (GO R)
     QQB
      (WHEN (> X 858) (GO RF))
      (WHEN (< X 392) (GO VD))
      (GO MH)
     MMB
      (WHEN (> S 1505) (GO VL))
      (WHEN (> M 2260) (GO ZZN))
      (GO JD)
     ZLB
      (WHEN (< M 2754) (GO A))
      (WHEN (> M 2824) (GO R))
      (GO R)
     GF
      (WHEN (> S 2007) (GO ZHL))
      (WHEN (> X 742) (GO R))
      (WHEN (< A 2531) (GO NM))
      (GO R)
     LN
      (WHEN (> X 970) (GO XZR))
      (WHEN (> S 3048) (GO CCR))
      (GO LFN)
     LNF
      (WHEN (< M 2433) (GO DS))
      (WHEN (> M 3234) (GO LZQ))
      (GO GKN)
     RPN
      (WHEN (< M 463) (GO R))
      (WHEN (> S 1546) (GO R))
      (WHEN (> S 1448) (GO A))
      (GO R)
     LF
      (WHEN (< A 3216) (GO R))
      (WHEN (< X 1505) (GO A))
      (WHEN (> S 1758) (GO R))
      (GO R)
     JG
      (WHEN (> M 552) (GO VXM))
      (WHEN (< A 3207) (GO NQ))
      (GO FZ)
     RSN
      (WHEN (> A 3358) (GO R))
      (GO GZJ)
     NN
      (WHEN (> X 2059) (GO QQ))
      (WHEN (> A 159) (GO R))
      (GO GFC)
     SKV
      (WHEN (< S 1728) (GO A))
      (WHEN (< A 1776) (GO A))
      (WHEN (< X 555) (GO R))
      (GO R)
     KTV
      (WHEN (> S 3176) (GO R))
      (WHEN (> S 3065) (GO A))
      (GO A)
     ZQ
      (WHEN (> M 3113) (GO A))
      (WHEN (< X 3424) (GO A))
      (GO R)
     VP
      (WHEN (< A 1366) (GO LGH))
      (WHEN (> M 1586) (GO NL))
      (GO JPJ)
     CZ
      (WHEN (> X 1163) (GO R))
      (GO A)
     DXN
      (WHEN (> M 3105) (GO R))
      (WHEN (< M 2018) (GO SKV))
      (WHEN (< X 548) (GO R))
      (GO XKV)
     XTN
      (WHEN (> A 3160) (GO RD))
      (WHEN (> S 2740) (GO TL))
      (GO XJ)
     DV
      (WHEN (< A 3922) (GO BV))
      (WHEN (< S 2651) (GO RL))
      (GO R)
     MLS
      (WHEN (> A 2798) (GO R))
      (WHEN (> X 2352) (GO A))
      (GO R)
     ZTQ
      (WHEN (< A 2008) (GO JHC))
      (WHEN (> M 862) (GO A))
      (GO A)
     NLX
      (WHEN (< A 2137) (GO A))
      (WHEN (> S 2037) (GO A))
      (WHEN (< A 2838) (GO R))
      (GO R)
     JVD
      (WHEN (> S 1469) (GO A))
      (WHEN (< M 3294) (GO ZM))
      (WHEN (< S 1435) (GO R))
      (GO MCB)
     ZL
      (WHEN (< M 3721) (GO ZF))
      (GO A)
     HVH
      (WHEN (< X 1599) (GO FCP))
      (WHEN (> A 256) (GO XTH))
      (WHEN (< X 2400) (GO NN))
      (GO A)
     SF
      (WHEN (> X 1556) (GO ZTQ))
      (GO MZL)
     LCQ
      (WHEN (> M 635) (GO MJP))
      (WHEN (> S 2859) (GO A))
      (WHEN (> M 413) (GO R))
      (GO R)
     XN
      (WHEN (> M 2426) (GO JVD))
      (WHEN (< X 858) (GO PFZ))
      (GO MFV)
     ZGF
      (WHEN (> A 1378) (GO R))
      (WHEN (< M 1291) (GO A))
      (GO R)
     QHV
      (WHEN (> M 1852) (GO A))
      (WHEN (> M 1719) (GO R))
      (WHEN (< S 3007) (GO R))
      (GO A)
     PH
      (WHEN (< A 3497) (GO VV))
      (GO R)
     MPS
      (WHEN (< S 2806) (GO ZPS))
      (GO A)
     BZ
      (WHEN (> S 2864) (GO R))
      (GO A)
     LL
      (WHEN (< S 3917) (GO TKQ))
      (WHEN (< X 3703) (GO R))
      (GO R)
     TM
      (WHEN (< S 3029) (GO SL))
      (WHEN (> A 3664) (GO DG))
      (WHEN (> A 3563) (GO CRG))
      (GO DCM)
     XXR
      (WHEN (< X 2995) (GO A))
      (WHEN (> S 1322) (GO R))
      (WHEN (> A 3132) (GO A))
      (GO A)
     KGZ
      (WHEN (> X 2235) (GO R))
      (GO A)
     PFZ
      (WHEN (< X 300) (GO A))
      (GO R)
     PB
      (WHEN (> M 3682) (GO LPV))
      (GO A)
     PS
      (WHEN (> M 2481) (GO XVP))
      (WHEN (> X 3501) (GO THV))
      (GO BK)
     DVF
      (WHEN (> S 3547) (GO SHN))
      (GO HC)
     ZHJ
      (WHEN (< S 2126) (GO XLC))
      (WHEN (< A 734) (GO PB))
      (WHEN (> X 1387) (GO R))
      (GO RN)
     XG
      (WHEN (< A 3969) (GO A))
      (WHEN (< M 2656) (GO A))
      (GO R)
     SM
      (WHEN (> S 2585) (GO A))
      (GO R)
     ZHL
      (WHEN (< A 2775) (GO R))
      (WHEN (< M 712) (GO A))
      (GO A)
     DF
      (WHEN (< X 2518) (GO R))
      (GO A)
     LFN
      (WHEN (< S 2787) (GO MFC))
      (WHEN (< M 3120) (GO A))
      (GO BMF)
     SDX
      (WHEN (> M 2324) (GO R))
      (GO A)
     TQ
      (WHEN (> M 1287) (GO A))
      (WHEN (< M 798) (GO A))
      (WHEN (> S 3148) (GO R))
      (GO R)
     GQ
      (WHEN (< A 2067) (GO R))
      (WHEN (> A 2180) (GO GZ))
      (WHEN (< M 2530) (GO R))
      (GO LBM)
     JQZ
      (WHEN (> X 800) (GO A))
      (WHEN (< M 2431) (GO R))
      (WHEN (< A 1059) (GO A))
      (GO A)
     HZL
      (WHEN (< M 3210) (GO A))
      (GO R)
     FQD
      (WHEN (< S 3517) (GO A))
      (GO DD)
     XPP
      (WHEN (< M 1921) (GO KFL))
      (WHEN (> X 716) (GO KV))
      (WHEN (> X 273) (GO NTK))
      (GO VMD)
     ST
      (WHEN (> A 2725) (GO R))
      (GO A)
     PD
      (WHEN (< M 3138) (GO A))
      (WHEN (< X 3799) (GO R))
      (GO A)
     CX
      (WHEN (< M 2622) (GO R))
      (WHEN (> X 3616) (GO A))
      (WHEN (< M 3404) (GO ZQ))
      (GO DCS)
     DPM
      (WHEN (> A 2728) (GO A))
      (WHEN (< A 2543) (GO QZB))
      (WHEN (> M 2249) (GO VS))
      (GO FHH)
     ZJN
      (WHEN (< M 1731) (GO R))
      (WHEN (> S 1599) (GO A))
      (GO A)
     DG
      (WHEN (< S 3279) (GO A))
      (WHEN (< M 899) (GO R))
      (WHEN (< A 3806) (GO R))
      (GO R)
     QJ
      (WHEN (< A 2324) (GO A))
      (GO R)
     BM
      (WHEN (< A 2544) (GO A))
      (WHEN (< M 973) (GO A))
      (WHEN (< M 1233) (GO R))
      (GO R)
     DMM
      (WHEN (> X 420) (GO R))
      (WHEN (< A 3938) (GO A))
      (WHEN (< M 1567) (GO A))
      (GO A)
     NXD
      (WHEN (> M 1378) (GO GKL))
      (GO GPS)
     FCF
      (WHEN (< A 3859) (GO R))
      (WHEN (> S 3094) (GO R))
      (GO A)
     KM
      (WHEN (< M 3292) (GO DB))
      (WHEN (> S 2311) (GO NLH))
      (WHEN (> X 949) (GO R))
      (GO R)
     FZ
      (WHEN (> M 224) (GO A))
      (WHEN (> S 427) (GO A))
      (WHEN (> S 193) (GO R))
      (GO R)
     JGN
      (WHEN (> X 3689) (GO A))
      (WHEN (> A 3896) (GO RHX))
      (GO A)
     DQT
      (WHEN (> X 1042) (GO R))
      (GO A)
     XS
      (WHEN (> X 2301) (GO NP))
      (WHEN (> M 2490) (GO A))
      (GO A)
     DNC
      (WHEN (> A 2667) (GO LTX))
      (GO R)
     ZQK
      (WHEN (> S 2813) (GO ZS))
      (WHEN (> M 2905) (GO XX))
      (WHEN (< A 3613) (GO DZC))
      (GO CMS)
     QH
      (WHEN (> M 2483) (GO R))
      (WHEN (< A 1940) (GO R))
      (GO A)
     NM
      (WHEN (> A 2194) (GO A))
      (GO R)
     HC
      (WHEN (< S 3500) (GO R))
      (GO JQZ)
     MT
      (WHEN (< S 1112) (GO DGS))
      (GO CS)
     JNC
      (WHEN (> X 2611) (GO A))
      (WHEN (> X 2231) (GO R))
      (GO R)
     TXN
      (WHEN (> X 2227) (GO A))
      (WHEN (< S 2060) (GO R))
      (GO R)
     TVZ
      (WHEN (< A 2131) (GO A))
      (WHEN (< M 3205) (GO R))
      (WHEN (< M 3641) (GO R))
      (GO A)
     BSH
      (WHEN (< A 439) (GO R))
      (WHEN (< A 528) (GO R))
      (WHEN (> S 2648) (GO A))
      (GO A)
     HB
      (WHEN (< M 340) (GO PMP))
      (WHEN (> X 1343) (GO SMN))
      (GO R)
     GXH
      (WHEN (< M 1015) (GO A))
      (WHEN (< X 1686) (GO A))
      (WHEN (< X 2151) (GO R))
      (GO R)
     XXF
      (WHEN (> X 3458) (GO A))
      (WHEN (> X 3396) (GO R))
      (GO A)
     KS
      (WHEN (< S 3753) (GO A))
      (WHEN (< S 3790) (GO R))
      (WHEN (< A 1055) (GO A))
      (GO A)
     BDB
      (WHEN (> M 1085) (GO KQL))
      (GO QK)
     GKN
      (WHEN (< S 3173) (GO A))
      (GO R)
     QQ
      (WHEN (< M 1365) (GO R))
      (WHEN (> S 2565) (GO A))
      (WHEN (< M 2907) (GO R))
      (GO A)
     DZC
      (WHEN (< M 2655) (GO VNG))
      (GO ZLB)
     HDQ
      (WHEN (< S 878) (GO R))
      (GO A)
     VXM
      (WHEN (> S 304) (GO A))
      (WHEN (> A 3262) (GO R))
      (WHEN (> A 2961) (GO R))
      (GO R)
     JFR
      (WHEN (< A 3476) (GO QT))
      (WHEN (< M 2328) (GO TM))
      (WHEN (< A 3734) (GO ZQK))
      (GO NHH)
     CJ
      (WHEN (< S 2777) (GO MKT))
      (WHEN (< M 1946) (GO R))
      (WHEN (< X 2175) (GO A))
      (GO R)
     NLH
      (WHEN (< M 3735) (GO A))
      (WHEN (< X 878) (GO R))
      (WHEN (> A 3519) (GO R))
      (GO R)
     BS
      (WHEN (< S 2559) (GO R))
      (WHEN (< S 2678) (GO A))
      (GO R)
     MHD
      (WHEN (> A 3461) (GO R))
      (WHEN (> X 1754) (GO A))
      (GO R)
     BF
      (WHEN (< X 1514) (GO A))
      (GO R)
     QZG
      (WHEN (> M 765) (GO R))
      (WHEN (< X 1881) (GO A))
      (WHEN (< M 429) (GO A))
      (GO A)
     LT
      (WHEN (> A 3048) (GO R))
      (WHEN (< S 241) (GO A))
      (WHEN (> S 399) (GO DK))
      (GO R)
     GV
      (WHEN (> X 1712) (GO A))
      (WHEN (> X 1145) (GO R))
      (GO A)
     BBV
      (WHEN (< X 3854) (GO XJN))
      (WHEN (> A 2553) (GO A))
      (GO R)
     RD
      (WHEN (> M 1904) (GO A))
      (WHEN (> A 3229) (GO RB))
      (WHEN (> A 3201) (GO ZBQ))
      (GO QZX)
     XDC
      (WHEN (< S 3559) (GO QKK))
      (WHEN (< X 2257) (GO A))
      (GO R)
     JPJ
      (WHEN (< A 2122) (GO PXJ))
      (WHEN (< M 815) (GO R))
      (GO R)
     HPX
      (WHEN (< X 3160) (GO A))
      (GO R)
     VKP
      (WHEN (< M 3498) (GO VC))
      (GO KK)
     VQ
      (WHEN (> S 3849) (GO A))
      (GO R)
     KC
      (WHEN (< A 2487) (GO R))
      (WHEN (> S 2074) (GO A))
      (GO R)
     NHJ
      (WHEN (< S 1677) (GO R))
      (WHEN (< X 754) (GO A))
      (GO A)
     JMV
      (WHEN (> S 3095) (GO R))
      (WHEN (> M 2659) (GO A))
      (GO A)
     NGP
      (WHEN (> X 318) (GO A))
      (GO R)
     RN
      (WHEN (> M 3717) (GO MV))
      (GO LD)
     RR
      (WHEN (> X 2077) (GO LCS))
      (WHEN (< A 2564) (GO VP))
      (WHEN (< X 1783) (GO KDM))
      (GO XD)
     VV
      (WHEN (< M 2492) (GO A))
      (GO R)
     JGS
      (WHEN (< X 1311) (GO TD))
      (WHEN (> X 1970) (GO FKR))
      (GO LKD)
     ZBQ
      (WHEN (> X 1251) (GO A))
      (WHEN (> X 741) (GO R))
      (WHEN (< A 3214) (GO R))
      (GO A)
     VR
      (WHEN (> S 1490) (GO FKJ))
      (WHEN (> S 1418) (GO R))
      (WHEN (< A 3021) (GO RDH))
      (GO A)
     JS
      (WHEN (> M 3168) (GO R))
      (WHEN (> M 3015) (GO A))
      (GO A)
     MH
      (WHEN (< X 676) (GO DXN))
      (WHEN (> S 1709) (GO JZC))
      (GO NHJ)
     RV
      (WHEN (> A 3103) (GO A))
      (WHEN (< S 282) (GO KVC))
      (GO VNS)
     SG
      (WHEN (> X 1921) (GO RQZ))
      (WHEN (< M 1545) (GO R))
      (GO FDX)
     BDV
      (WHEN (> S 3015) (GO A))
      (WHEN (> X 563) (GO R))
      (WHEN (< A 2129) (GO R))
      (GO A)
     KB
      (WHEN (< A 2399) (GO DNV))
      (WHEN (< A 3059) (GO MMB))
      (WHEN (< M 1918) (GO MRC))
      (GO GX)
     CH
      (WHEN (< S 2743) (GO R))
      (WHEN (> M 1149) (GO TFZ))
      (WHEN (< M 558) (GO XPR))
      (GO A)
     JQ
      (WHEN (> M 2374) (GO A))
      (WHEN (< X 1425) (GO A))
      (GO R)
     NHH
      (WHEN (< X 3487) (GO GNZ))
      (WHEN (> S 3018) (GO JGN))
      (GO TK)
     MFV
      (WHEN (> A 1367) (GO R))
      (WHEN (> A 767) (GO GV))
      (GO NJR)
     TN
      (WHEN (< X 1082) (GO R))
      (WHEN (< S 1473) (GO A))
      (GO A)
     CL
      (WHEN (< A 2428) (GO CQV))
      (GO SKK)
     SMN
      (WHEN (< M 704) (GO R))
      (WHEN (> S 1557) (GO R))
      (GO A)
     VS
      (WHEN (< S 2752) (GO JTG))
      (GO A)
     NDP
      (WHEN (> M 1613) (GO R))
      (WHEN (> A 1668) (GO TQK))
      (WHEN (> A 756) (GO A))
      (GO RTK)
     MM
      (WHEN (< X 2841) (GO A))
      (WHEN (> X 2991) (GO R))
      (GO R)
     GFC
      (WHEN (< S 2651) (GO A))
      (WHEN (> A 87) (GO A))
      (GO R)
     FC
      (WHEN (< S 3768) (GO A))
      (WHEN (> X 2927) (GO A))
      (WHEN (> S 3872) (GO R))
      (GO A)
     QD
      (WHEN (< S 1986) (GO A))
      (WHEN (< A 562) (GO A))
      (GO R)
     VTV
      (WHEN (< X 2948) (GO CJ))
      (WHEN (< S 2996) (GO GQ))
      (GO MFK)
     PC
      (WHEN (< M 2563) (GO R))
      (WHEN (> S 308) (GO R))
      (GO A)
     DB
      (WHEN (> X 1182) (GO R))
      (WHEN (> S 2330) (GO A))
      (GO R)
     LJ
      (WHEN (> A 1208) (GO A))
      (GO SN)
     PKK
      (WHEN (< S 3012) (GO A))
      (WHEN (> M 2572) (GO A))
      (WHEN (> A 3749) (GO R))
      (GO R)
     PHS
      (WHEN (< S 3862) (GO R))
      (WHEN (< M 1390) (GO R))
      (GO KCM)
     GKL
      (WHEN (> X 556) (GO A))
      (GO R)
     QX
      (WHEN (< S 248) (GO JJM))
      (GO KP)
     JJM
      (WHEN (< X 2448) (GO A))
      (WHEN (> M 2222) (GO A))
      (GO R)
     KT
      (WHEN (> S 1768) (GO PS))
      (GO KB)
     LHP
      (WHEN (< M 2508) (GO XTC))
      (WHEN (> X 1732) (GO VKP))
      (WHEN (> A 1157) (GO LN))
      (GO XQ)
     PXJ
      (WHEN (> M 793) (GO R))
      (WHEN (> A 1699) (GO R))
      (GO A)
     FKR
      (WHEN (> X 2275) (GO A))
      (WHEN (< M 2337) (GO A))
      (WHEN (< X 2147) (GO ZLS))
      (GO A)
     MV
      (WHEN (> M 3870) (GO A))
      (WHEN (> M 3778) (GO A))
      (GO R)
     ZH
      (WHEN (< M 1090) (GO A))
      (GO ZT)
     PJ
      (WHEN (> S 2079) (GO R))
      (GO GD)
     TL
      (WHEN (< A 3073) (GO A))
      (WHEN (> M 2621) (GO A))
      (GO LQ)
     MCB
      (WHEN (> X 890) (GO A))
      (WHEN (< S 1455) (GO A))
      (GO R)
     NQK
      (WHEN (< A 2608) (GO A))
      (WHEN (< M 3138) (GO MLS))
      (GO BGH)
     SR
      (WHEN (< M 2392) (GO R))
      (WHEN (> M 2540) (GO A))
      (WHEN (< S 1933) (GO R))
      (GO A)
     JZC
      (WHEN (< A 2239) (GO R))
      (WHEN (< A 3339) (GO A))
      (WHEN (> A 3605) (GO A))
      (GO ZCF)
     TFZ
      (WHEN (< X 779) (GO A))
      (WHEN (< A 3380) (GO A))
      (WHEN (< M 1541) (GO A))
      (GO R)
     PVJ
      (WHEN (< X 2020) (GO R))
      (WHEN (> X 2348) (GO A))
      (GO R)
     BV
      (WHEN (< A 3884) (GO R))
      (WHEN (> X 825) (GO A))
      (GO A)
     HCF
      (WHEN (< A 3816) (GO JGS))
      (WHEN (< S 2909) (GO RBD))
      (WHEN (> X 1198) (GO SG))
      (GO NVV)
     NL
      (WHEN (> A 1904) (GO TZV))
      (GO R)
     RL
      (WHEN (> S 2541) (GO A))
      (WHEN (> M 2863) (GO R))
      (WHEN (> M 2346) (GO A))
      (GO R)
     XSP
      (WHEN (> M 2367) (GO ZG))
      (WHEN (> A 3232) (GO ZP))
      (WHEN (> X 2987) (GO QHV))
      (GO A)
     NV
      (WHEN (> M 1136) (GO JR))
      (WHEN (< X 974) (GO MN))
      (WHEN (< S 299) (GO QG))
      (GO BF)
     DNV
      (WHEN (< X 3133) (GO A))
      (WHEN (> S 1612) (GO CX))
      (GO FXP)
     BVX
      (WHEN (> X 3807) (GO A))
      (WHEN (< M 1978) (GO A))
      (WHEN (< A 829) (GO R))
      (GO KZN)
     VN
      (WHEN (> X 1743) (GO BMX))
      (WHEN (> S 3133) (GO PJC))
      (WHEN (> X 797) (GO RFD))
      (GO DTF)
     SN
      (WHEN (< A 720) (GO R))
      (WHEN (> M 2407) (GO A))
      (WHEN (> S 2263) (GO R))
      (GO R)
     PXN
      (WHEN (< S 3897) (GO A))
      (WHEN (< A 3216) (GO A))
      (GO R)
     BKF
      (WHEN (> A 800) (GO RZG))
      (GO HB)
     ZLS
      (WHEN (> X 2037) (GO R))
      (WHEN (> M 2919) (GO R))
      (GO A)
     XZR
      (WHEN (> A 1432) (GO A))
      (WHEN (> M 3471) (GO A))
      (WHEN (> M 2878) (GO R))
      (GO R)
     PNG
      (WHEN (< S 3217) (GO R))
      (GO A)
     LZP
      (WHEN (> S 2935) (GO LNF))
      (GO HBJ)
     FXS
      (WHEN (< M 2298) (GO A))
      (WHEN (> M 3023) (GO R))
      (GO A)
     RDS
      (WHEN (< X 2136) (GO A))
      (GO GXF)
     MC
      (WHEN (> S 3407) (GO JB))
      (WHEN (> A 1580) (GO TKS))
      (GO DHR)
     LH
      (WHEN (< M 777) (GO A))
      (WHEN (> S 959) (GO A))
      (GO R)
     FQ
      (WHEN (> A 3243) (GO ZZG))
      (WHEN (< X 1126) (GO R))
      (WHEN (< M 680) (GO CL))
      (GO CPK)
     DTF
      (WHEN (< X 432) (GO A))
      (WHEN (< M 2598) (GO ZB))
      (GO A)
     SGD
      (WHEN (< M 377) (GO R))
      (WHEN (< A 1360) (GO A))
      (WHEN (> S 2105) (GO A))
      (GO A)
     FXP
      (WHEN (< S 1476) (GO A))
      (WHEN (> A 1446) (GO R))
      (GO A)
     CGV
      (WHEN (> S 2832) (GO A))
      (WHEN (> A 3725) (GO R))
      (WHEN (< A 3721) (GO R))
      (GO R)
     RB
      (WHEN (> X 1583) (GO R))
      (GO A)
     SXB
      (WHEN (< S 2865) (GO MHD))
      (WHEN (< A 3463) (GO TGC))
      (WHEN (> S 3096) (GO NGZ))
      (GO CV)
     HF
      (WHEN (< S 1198) (GO A))
      (WHEN (< M 1745) (GO A))
      (GO R)
     XFF
      (WHEN (< A 2470) (GO R))
      (WHEN (< A 2902) (GO R))
      (GO BRK)
     PQ
      (WHEN (< M 1019) (GO R))
      (WHEN (< A 3493) (GO R))
      (GO TCN)
     QXH
      (WHEN (< S 3830) (GO JX))
      (WHEN (< A 3002) (GO A))
      (WHEN (< A 3523) (GO PXN))
      (GO DQT)
     CCR
      (WHEN (> A 1369) (GO A))
      (WHEN (< X 540) (GO NGP))
      (GO R)
     LX
      (WHEN (> A 3856) (GO R))
      (GO A)
     GLS
      (WHEN (> S 2827) (GO R))
      (WHEN (> X 1450) (GO A))
      (WHEN (> M 760) (GO R))
      (GO R)
     FS
      (WHEN (> M 2698) (GO R))
      (WHEN (< M 1866) (GO A))
      (GO R)
     QT
      (WHEN (> M 1524) (GO XSP))
      (WHEN (> A 3264) (GO BZ))
      (WHEN (< X 2990) (GO LCQ))
      (GO DDP)
     ZSB
      (WHEN (< S 1967) (GO R))
      (WHEN (> X 1869) (GO TXN))
      (WHEN (> S 2038) (GO R))
      (GO NF)
     BRK
      (WHEN (< S 2250) (GO R))
      (GO A)
     MJP
      (WHEN (> A 3129) (GO R))
      (WHEN (> X 2733) (GO R))
      (WHEN (< S 2923) (GO A))
      (GO R)
     ZZG
      (WHEN (> M 522) (GO ZRF))
      (WHEN (< X 1493) (GO R))
      (WHEN (> X 2078) (GO A))
      (GO A)
     FKJ
      (WHEN (> X 1280) (GO R))
      (WHEN (> M 2156) (GO R))
      (GO A)
     JHC
      (WHEN (> M 746) (GO R))
      (WHEN (> A 1003) (GO A))
      (GO R)
     LZ
      (WHEN (< X 2241) (GO A))
      (WHEN (> X 2443) (GO R))
      (GO A)
     CB
      (WHEN (< S 3619) (GO CFC))
      (WHEN (> A 1549) (GO MDC))
      (GO DJ)
     QM
      (WHEN (< S 3512) (GO ZC))
      (GO R)
     CPK
      (WHEN (< S 1607) (GO BM))
      (GO A)
     FHH
      (WHEN (> A 2658) (GO JZ))
      (WHEN (< S 2865) (GO R))
      (WHEN (> S 3109) (GO A))
      (GO R)
     TGC
      (WHEN (> M 1755) (GO A))
      (WHEN (< S 3212) (GO QZG))
      (GO R)
     CGM
      (WHEN (< S 2172) (GO R))
      (WHEN (> M 3075) (GO R))
      (GO A)
     JGF
      (WHEN (> A 1369) (GO R))
      (WHEN (> A 1337) (GO A))
      (WHEN (> S 2964) (GO R))
      (GO R)
     CMS
      (WHEN (< M 2570) (GO A))
      (WHEN (< M 2752) (GO R))
      (WHEN (< M 2837) (GO R))
      (GO R)
     JX
      (WHEN (> X 548) (GO R))
      (WHEN (< M 2561) (GO R))
      (WHEN (< X 242) (GO A))
      (GO R)
     MRC
      (WHEN (< X 3340) (GO A))
      (WHEN (< M 1213) (GO RPN))
      (GO HVC)
     QZX
      (WHEN (> X 1193) (GO A))
      (GO R)
     MDC
      (WHEN (< X 3104) (GO JDD))
      (WHEN (< X 3684) (GO HJB))
      (GO PHS)
     CF
      (WHEN (< M 2776) (GO VR))
      (WHEN (< X 1619) (GO FNQ))
      (WHEN (< S 1471) (GO DNF))
      (GO SD)
     DS
      (WHEN (< A 815) (GO A))
      (GO TQ)
     VMD
      (WHEN (> S 3862) (GO BJ))
      (GO QB)
     MFC
      (WHEN (> M 3159) (GO R))
      (WHEN (> S 2590) (GO A))
      (WHEN (< S 2498) (GO A))
      (GO A)
     JRK
      (WHEN (< M 3157) (GO SP))
      (GO ZHJ)
     VNG
      (WHEN (> M 2487) (GO R))
      (WHEN (< A 3537) (GO A))
      (GO R)
     PJC
      (WHEN (< S 3295) (GO R))
      (WHEN (> X 730) (GO CZ))
      (GO R)
     PP
      (WHEN (> M 1152) (GO R))
      (WHEN (> A 708) (GO R))
      (GO A)
     LKD
      (WHEN (< A 3716) (GO A))
      (WHEN (> A 3760) (GO BCS))
      (WHEN (> A 3739) (GO PKK))
      (GO CGV)
     TKS
      (WHEN (< A 2969) (GO PXF))
      (WHEN (> X 2465) (GO JFR))
      (WHEN (< A 3555) (GO GP))
      (GO HCF)
     MP
      (WHEN (> X 1865) (GO A))
      (GO R)
     LTX
      (WHEN (< X 3669) (GO A))
      (WHEN (< M 611) (GO R))
      (GO R)
     FB
      (WHEN (> X 527) (GO R))
      (WHEN (< M 1341) (GO A))
      (WHEN (< A 2542) (GO R))
      (GO A)
     GZJ
      (WHEN (< A 3308) (GO A))
      (GO R)
     KBG
      (WHEN (< A 2673) (GO A))
      (WHEN (> X 3099) (GO A))
      (GO R)
     RTK
      (WHEN (< X 1207) (GO R))
      (WHEN (< S 322) (GO R))
      (WHEN (> M 673) (GO R))
      (GO A)
     JD
      (WHEN (< S 1420) (GO KBG))
      (GO SXD)
     DFS
      (WHEN (< A 3210) (GO A))
      (WHEN (> X 3369) (GO R))
      (WHEN (< X 3277) (GO A))
      (GO A)
     NP
      (WHEN (> A 2152) (GO R))
      (GO A)
     TX
      (WHEN (> A 3904) (GO R))
      (WHEN (> M 746) (GO A))
      (WHEN (> A 3848) (GO R))
      (GO R)
     FDX
      (WHEN (< X 1567) (GO R))
      (WHEN (< A 3936) (GO KTV))
      (GO JMV)
     MJV
      (WHEN (< M 2079) (GO A))
      (WHEN (> A 2843) (GO A))
      (WHEN (> X 3591) (GO A))
      (GO QHP)
     ZPS
      (WHEN (> M 3233) (GO A))
      (WHEN (< M 2936) (GO R))
      (GO A)
     VM
      (WHEN (< S 1569) (GO TN))
      (GO BDB)
     PCH
      (WHEN (< M 1670) (GO SMS))
      (WHEN (> A 1946) (GO DM))
      (GO JRK)
     BR
      (WHEN (> X 3300) (GO ZXX))
      (GO R)
     ZF
      (WHEN (> S 242) (GO A))
      (GO R)
     NTK
      (WHEN (< S 3864) (GO R))
      (GO A)
     HS
      (WHEN (> S 2686) (GO GLS))
      (WHEN (> X 1247) (GO A))
      (GO TX)
     XTH
      (WHEN (< M 1407) (GO BSH))
      (WHEN (> A 466) (GO HPX))
      (GO XF)
     KV
      (WHEN (> M 2674) (GO R))
      (WHEN (< M 2325) (GO A))
      (WHEN (< X 1277) (GO R))
      (GO LHZ)
     LXC
      (WHEN (< X 1519) (GO JJT))
      (WHEN (< S 2121) (GO ZN))
      (WHEN (< X 2064) (GO BPQ))
      (GO NQK)
     XVP
      (WHEN (< A 1815) (GO PJ))
      (GO CGM)
     RBD
      (WHEN (< M 1497) (GO HS))
      (GO DV)
     PDN
      (WHEN (< A 3048) (GO ST))
      (WHEN (> S 1293) (GO R))
      (WHEN (< S 1267) (GO A))
      (GO R)
     LG
      (WHEN (> S 413) (GO A))
      (WHEN (< S 242) (GO A))
      (WHEN (< A 1899) (GO A))
      (GO R)
     BGH
      (WHEN (> S 2275) (GO A))
      (GO R)
     BJ
      (WHEN (> A 787) (GO R))
      (WHEN (< M 3108) (GO A))
      (WHEN (< S 3930) (GO A))
      (GO R)
     ZS
      (WHEN (< M 3026) (GO DFV))
      (WHEN (> X 3208) (GO R))
      (WHEN (< S 3193) (GO GT))
      (GO R)
     LPV
      (WHEN (> S 2284) (GO A))
      (WHEN (< M 3804) (GO A))
      (WHEN (< S 2215) (GO A))
      (GO R)
     FRF
      (WHEN (< A 1568) (GO R))
      (GO PF)
     LHZ
      (WHEN (> X 1406) (GO A))
      (WHEN (> S 3831) (GO R))
      (WHEN (> M 2485) (GO A))
      (GO R)
     KFL
      (WHEN (> S 3825) (GO A))
      (WHEN (< X 675) (GO KS))
      (WHEN (> X 1039) (GO A))
      (GO PP)
     BMF
      (WHEN (< A 1331) (GO A))
      (GO R)
     SMS
      (WHEN (> S 2156) (GO SF))
      (WHEN (> A 1737) (GO RPB))
      (WHEN (> A 748) (GO LCP))
      (GO ZH)
     QB
      (WHEN (> S 3774) (GO R))
      (WHEN (< S 3744) (GO A))
      (WHEN (> S 3763) (GO R))
      (GO R)
     GGL
      (WHEN (> A 648) (GO R))
      (WHEN (> A 619) (GO A))
      (GO R)
     PN
      (WHEN (> A 2902) (GO A))
      (WHEN (< S 1543) (GO R))
      (GO A)
     FKP
      (WHEN (> A 782) (GO R))
      (GO ZCB)
     KGT
      (WHEN (< A 2588) (GO PVJ))
      (WHEN (< M 2228) (GO A))
      (GO SR)
     FH
      (WHEN (< M 3554) (GO R))
      (GO A)
     PXF
      (WHEN (> A 2341) (GO DPM))
      (WHEN (> X 1629) (GO VTV))
      (GO NXD)
     ZB
      (WHEN (> M 1090) (GO R))
      (WHEN (< X 661) (GO A))
      (WHEN (< S 2926) (GO A))
      (GO A)
     BK
      (WHEN (> M 1591) (GO HRF))
      (WHEN (> M 843) (GO FRF))
      (WHEN (< X 3052) (GO NLX))
      (GO BR)
     XV
      (WHEN (< S 419) (GO A))
      (WHEN (< X 508) (GO R))
      (GO R)
     NGZ
      (WHEN (> S 3296) (GO FVK))
      (GO A)
     RZH
      (WHEN (> S 2837) (GO R))
      (WHEN (> M 1393) (GO A))
      (GO A)
     DCS
      (WHEN (> A 1227) (GO A))
      (WHEN (< X 3362) (GO R))
      (GO R)
     DDP
      (WHEN (> A 3072) (GO A))
      (GO R)
     CHK
      (WHEN (> X 988) (GO R))
      (GO A)
     BCS
      (WHEN (< A 3795) (GO A))
      (GO R)
     NQ
      (WHEN (< X 2015) (GO R))
      (WHEN (> S 362) (GO R))
      (WHEN (< M 203) (GO A))
      (GO R)
     CRG
      (WHEN (> A 3600) (GO A))
      (WHEN (> X 3090) (GO SJ))
      (WHEN (< X 2861) (GO R))
      (GO R)
     GT
      (WHEN (> M 3484) (GO A))
      (GO R)
     JR
      (WHEN (> S 251) (GO A))
      (WHEN (< X 711) (GO A))
      (GO R)
     SP
      (WHEN (> S 2177) (GO LJ))
      (WHEN (< X 890) (GO FKP))
      (WHEN (< A 1241) (GO HGZ))
      (GO ZSB)
     NTZ
      (WHEN (> S 2967) (GO R))
      (WHEN (> X 686) (GO R))
      (GO A)
     LK
      (WHEN (< X 1915) (GO NDP))
      (WHEN (> X 2905) (GO RKS))
      (WHEN (> A 1647) (GO FD))
      (GO QX)
     MR
      (WHEN (< A 3298) (GO A))
      (WHEN (< A 3316) (GO KLS))
      (GO CRS)
     DFV
      (WHEN (< A 3566) (GO A))
      (WHEN (> X 3241) (GO A))
      (WHEN (> A 3665) (GO A))
      (GO R)
     DD
      (WHEN (> M 2969) (GO A))
      (WHEN (> S 3581) (GO R))
      (GO R)
     VXX
      (WHEN (< A 251) (GO R))
      (GO A)
     HGZ
      (WHEN (> S 1957) (GO KH))
      (WHEN (< A 472) (GO SDX))
      (WHEN (< M 2611) (GO A))
      (GO R)
     JDD
      (WHEN (< A 2697) (GO FC))
      (WHEN (< M 1777) (GO A))
      (GO R)
     XQ
      (WHEN (> A 1074) (GO CHK))
      (WHEN (< A 1022) (GO MPS))
      (GO XHJ)
     TS
      (WHEN (> X 3265) (GO A))
      (WHEN (< M 3294) (GO R))
      (GO A)
     ZC
      (WHEN (> S 3447) (GO A))
      (WHEN (> S 3429) (GO R))
      (WHEN (< M 2140) (GO A))
      (GO R)
     BX
      (WHEN (< X 3604) (GO XXF))
      (WHEN (< A 484) (GO FXS))
      (GO R)
     HJB
      (WHEN (> S 3819) (GO A))
      (WHEN (< X 3453) (GO R))
      (WHEN (< X 3591) (GO A))
      (GO A)
     NJR
      (WHEN (> X 1531) (GO A))
      (WHEN (< S 1460) (GO R))
      (GO R)
     LHX
      (WHEN (< A 2491) (GO MT))
      (WHEN (> M 1715) (GO DL))
      (WHEN (< S 1110) (GO RDS))
      (GO SZ)
     RHX
      (WHEN (< M 2943) (GO A))
      (GO R)
     PTN
      (WHEN (< A 2505) (GO LK))
      (WHEN (< M 2031) (GO LBZ))
      (GO CXV)
     NG
      (WHEN (> S 2739) (GO A))
      (WHEN (> X 3185) (GO A))
      (GO R)
     ZXX
      (WHEN (> M 350) (GO A))
      (WHEN (> S 2187) (GO A))
      (WHEN (< X 3431) (GO A))
      (GO R)
     HQQ
      (WHEN (< A 808) (GO A))
      (WHEN (> S 2584) (GO MS))
      (GO A)
     PF
      (WHEN (> S 2070) (GO A))
      (WHEN (> A 2463) (GO R))
      (WHEN (< M 1154) (GO R))
      (GO A)
     MFK
      (WHEN (< A 1948) (GO R))
      (GO PNG)
     VNS
      (WHEN (< M 2846) (GO R))
      (WHEN (> S 422) (GO R))
      (GO A)
     JMK
      (WHEN (< X 2178) (GO A))
      (WHEN (> A 3930) (GO R))
      (GO A)
     SS
      (WHEN (> S 1952) (GO R))
      (WHEN (> X 1315) (GO A))
      (GO A)
     BC
      (WHEN (< X 1955) (GO A))
      (WHEN (> A 119) (GO R))
      (WHEN (< M 1469) (GO A))
      (GO R)
     KVC
      (WHEN (> A 2746) (GO R))
      (WHEN (> A 2606) (GO A))
      (GO R)
     SVB
      (WHEN (> S 1963) (GO A))
      (GO DHH)
     ZMC
      (WHEN (> S 1542) (GO LX))
      (WHEN (< M 3249) (GO R))
      (WHEN (> A 3822) (GO R))
      (GO A)
     DCM
      (WHEN (> X 3146) (GO SV))
      (WHEN (> X 2890) (GO R))
      (GO BSV)
     DNF
      (WHEN (< M 3282) (GO R))
      (WHEN (> S 1413) (GO A))
      (GO FH)
     QG
      (WHEN (< M 1048) (GO R))
      (WHEN (> S 179) (GO A))
      (GO R)
     PG
      (WHEN (> S 3693) (GO QXH))
      (WHEN (< M 2372) (GO FB))
      (GO FQD)
     SXD
      (WHEN (> X 3164) (GO R))
      (GO A)
     LGH
      (WHEN (> M 2281) (GO R))
      (GO A)
     FNQ
      (WHEN (< S 1485) (GO A))
      (WHEN (< A 3295) (GO PN))
      (GO A)
     JB
      (WHEN (< X 1564) (GO HM))
      (WHEN (> X 2605) (GO CB))
      (GO RR)
     SZ
      (WHEN (< S 1235) (GO PQ))
      (WHEN (> M 756) (GO LNZ))
      (GO PDN)
     ZT
      (WHEN (< X 1498) (GO R))
      (WHEN (> A 306) (GO QD))
      (WHEN (< A 198) (GO BC))
      (GO A)
     FVK
      (WHEN (< A 3520) (GO A))
      (GO R)
     GFP
      (WHEN (> S 2804) (GO R))
      (WHEN (< S 2578) (GO R))
      (GO A)
     TZV
      (WHEN (< A 2166) (GO R))
      (WHEN (< M 2437) (GO A))
      (GO R)
     VC
      (WHEN (< A 1314) (GO R))
      (WHEN (< A 1410) (GO JGF))
      (WHEN (> A 1491) (GO A))
      (GO A)
     VK
      (WHEN (< M 995) (GO A))
      (WHEN (< M 1328) (GO R))
      (GO R)
     PT
      (WHEN (> M 1440) (GO R))
      (WHEN (> X 1184) (GO A))
      (WHEN (< M 1238) (GO A))
      (GO A)
     TXB
      (WHEN (> S 1786) (GO A))
      (WHEN (< X 172) (GO R))
      (WHEN (< S 1766) (GO A))
      (GO QH)
     LLC
      (WHEN (> S 1503) (GO R))
      (WHEN (> S 1417) (GO A))
      (WHEN (> X 2946) (GO R))
      (GO R)
     ZN
      (WHEN (< A 2350) (GO TVZ))
      (WHEN (< X 2058) (GO BB))
      (GO A)
     PMP
      (WHEN (< A 531) (GO A))
      (GO R)
     XTC
      (WHEN (< X 1755) (GO LCN))
      (WHEN (> S 2778) (GO JNC))
      (GO R)
     KQL
      (WHEN (> S 1670) (GO A))
      (GO R)
     ZCF
      (WHEN (> A 3429) (GO R))
      (WHEN (< M 2504) (GO A))
      (GO R)
     GPS
      (WHEN (> A 1924) (GO BDV))
      (WHEN (< S 2811) (GO BS))
      (GO R)
     MS
      (WHEN (< A 887) (GO R))
      (GO A)
     SH
      (WHEN (> M 1213) (GO A))
      (WHEN (< M 628) (GO R))
      (WHEN (< A 278) (GO A))
      (GO A)
     JTG
      (WHEN (< A 2615) (GO A))
      (WHEN (< X 1428) (GO R))
      (GO A)
     QKK
      (WHEN (< S 3538) (GO R))
      (GO A)
     SJ
      (WHEN (> A 3576) (GO A))
      (WHEN (> S 3252) (GO R))
      (WHEN (> X 3403) (GO A))
      (GO R)
     LNZ
      (WHEN (> X 2017) (GO A))
      (WHEN (> A 3320) (GO R))
      (WHEN (< A 3030) (GO R))
      (GO R)
     SVK
      (WHEN (< S 1269) (GO A))
      (WHEN (< X 2459) (GO A))
      (WHEN (> X 3335) (GO R))
      (GO XXR)
     DK
      (WHEN (> X 3445) (GO A))
      (WHEN (> M 2894) (GO A))
      (WHEN (> A 2714) (GO R))
      (GO A)
     CK
      (WHEN (> A 820) (GO R))
      (WHEN (< S 1878) (GO R))
      (GO A)
     MZL
      (WHEN (< M 1102) (GO QJ))
      (WHEN (< X 626) (GO RCN))
      (WHEN (< S 2282) (GO A))
      (GO PT)
     CM
      (WHEN (> M 2496) (GO R))
      (GO A)
     IN
      (WHEN (< S 2403) (GO PXM))
      (GO MC)
     MGR
      (WHEN (> X 1261) (GO A))
      (WHEN (< S 2247) (GO A))
      (WHEN (> S 2332) (GO R))
      (GO R)
     RPB
      (WHEN (< X 1235) (GO GF))
      (WHEN (> A 3150) (GO SVB))
      (GO XLG)
     FCP
      (WHEN (< A 391) (GO SM))
      (WHEN (> X 658) (GO R))
      (WHEN (< X 235) (GO A))
      (GO R)
     MZ
      (WHEN (> A 3217) (GO R))
      (WHEN (< X 568) (GO PC))
      (WHEN (< A 2806) (GO R))
      (GO R)
     GNZ
      (WHEN (> M 3220) (GO R))
      (WHEN (> X 3030) (GO A))
      (WHEN (> M 2804) (GO R))
      (GO A)
     LCN
      (WHEN (< A 1364) (GO BFC))
      (WHEN (> X 713) (GO MX))
      (GO R)
     LD
      (WHEN (> A 1185) (GO R))
      (GO A)
     TK
      (WHEN (< X 3736) (GO HZL))
      (WHEN (> A 3880) (GO R))
      (WHEN (< X 3854) (GO PD))
      (GO A)
     QST
      (WHEN (< S 1649) (GO R))
      (WHEN (> X 1581) (GO A))
      (WHEN (> S 1662) (GO R))
      (GO A)
     ZZN
      (WHEN (> S 1422) (GO LB))
      (WHEN (> A 2822) (GO TS))
      (GO A)
     SQ
      (WHEN (> A 2419) (GO CF))
      (GO XN)
     DL
      (WHEN (< S 1027) (GO HDQ))
      (WHEN (< M 3118) (GO R))
      (WHEN (> S 1207) (GO SVK))
      (GO R)
     DGS
      (WHEN (> S 933) (GO R))
      (WHEN (> A 1219) (GO HMN))
      (GO R)
     TCN
      (WHEN (< M 1316) (GO R))
      (GO A)
     GP
      (WHEN (< A 3265) (GO XTN))
      (WHEN (< A 3409) (GO VLR))
      (WHEN (< X 874) (GO VJ))
      (GO SXB)
     SV
      (WHEN (< X 3623) (GO R))
      (WHEN (< X 3865) (GO A))
      (WHEN (> X 3953) (GO A))
      (GO A)
     VJH
      (WHEN (< S 696) (GO PTN))
      (GO LHX)
     KH
      (WHEN (< M 2223) (GO A))
      (WHEN (> X 1640) (GO A))
      (GO R)
     SL
      (WHEN (> X 3116) (GO R))
      (WHEN (< M 1100) (GO A))
      (WHEN (< M 1681) (GO MM))
      (GO A)
     JV
      (WHEN (> X 3205) (GO R))
      (GO A)
     RF
      (WHEN (< A 2331) (GO TZD))
      (WHEN (< S 1687) (GO KQP))
      (GO LF)
     ZG
      (WHEN (> M 3278) (GO A))
      (WHEN (< M 2822) (GO R))
      (WHEN (> X 3249) (GO R))
      (GO R)
     LCP
      (WHEN (< M 816) (GO NK))
      (WHEN (< S 2030) (GO ZGF))
      (GO CQ)
     HVC
      (WHEN (< X 3758) (GO PVH))
      (WHEN (< M 1480) (GO A))
      (WHEN (< S 1575) (GO R))
      (GO A)
     RKS
      (WHEN (< X 3595) (GO R))
      (WHEN (< M 2372) (GO A))
      (GO A)
     ZX
      (WHEN (> M 2405) (GO A))
      (WHEN (> X 1653) (GO R))
      (GO BMG)
     BFC
      (WHEN (< X 991) (GO A))
      (GO R)
     ZRF
      (WHEN (< X 1430) (GO R))
      (GO A)
     QK
      (WHEN (< M 1019) (GO R))
      (WHEN (< X 994) (GO R))
      (WHEN (< M 1047) (GO R))
      (GO A)
     BSV
      (WHEN (< S 3158) (GO R))
      (WHEN (< X 2672) (GO R))
      (WHEN (> A 3513) (GO R))
      (GO A)
     PXM
      (WHEN (< S 1369) (GO VJH))
      (WHEN (> X 2564) (GO KT))
      (WHEN (> S 1811) (GO PCH))
      (GO HSX)
     GXF
      (WHEN (> A 3398) (GO A))
      (WHEN (< X 3277) (GO BQ))
      (GO LH)
     KLS
      (WHEN (< M 1031) (GO A))
      (WHEN (> S 2876) (GO R))
      (WHEN (> S 2644) (GO A))
      (GO R)
     QQH
      (WHEN (< X 3795) (GO A))
      (GO R)
     NVV
      (WHEN (< S 3241) (GO ZNZ))
      (GO BNL)
     JJT
      (WHEN (< S 2037) (GO SS))
      (GO MGR)
     QZB
      (WHEN (> S 2804) (GO R))
      (GO DF)
     BMX
      (WHEN (> M 1850) (GO A))
      (GO SH)
     TG
      (WHEN (< X 3192) (GO LLC))
      (WHEN (> A 3346) (GO R))
      (WHEN (< X 3633) (GO DFS))
      (GO QQH)
     BNL
      (WHEN (> S 3326) (GO DMM))
      (WHEN (> M 2318) (GO A))
      (WHEN (< A 3895) (GO R))
      (GO R)
     CFC
      (WHEN (< X 3480) (GO QM))
      (WHEN (< A 1363) (GO BVX))
      (WHEN (< X 3745) (GO MJV))
      (GO BBV)
     XLP
      (WHEN (> X 460) (GO A))
      (WHEN (< M 2590) (GO RZH))
      (GO R)
     BB
      (WHEN (< S 1924) (GO R))
      (GO A)
     SHQ
      (WHEN (> X 1246) (GO A))
      (WHEN (> A 3207) (GO TPB))
      (WHEN (< X 826) (GO XV))
      (GO A)
     RZG
      (WHEN (< S 1629) (GO A))
      (WHEN (> A 1245) (GO R))
      (WHEN (> A 1083) (GO R))
      (GO A)
     XGV
      (WHEN (< X 2900) (GO R))
      (GO A)
     BH
      (WHEN (> M 2126) (GO R))
      (WHEN (< S 1916) (GO A))
      (WHEN (> X 3791) (GO A))
      (GO A)
     CS
      (WHEN (> S 1272) (GO A))
      (WHEN (< A 1174) (GO HF))
      (GO R)
     LBZ
      (WHEN (< M 921) (GO JG))
      (WHEN (> X 2079) (GO XGV))
      (WHEN (< M 1295) (GO NV))
      (GO SHQ)
     LXG
      (WHEN (< X 826) (GO R))
      (GO A)
     RDH
      (WHEN (> X 982) (GO R))
      (GO A)
     KCM
      (WHEN (> X 3875) (GO A))
      (WHEN (> M 2918) (GO A))
      (WHEN (> M 2407) (GO A))
      (GO R)
     RQZ
      (WHEN (< A 3893) (GO A))
      (WHEN (> M 1449) (GO JMK))
      (WHEN (> X 2106) (GO A))
      (GO A)
     VL
      (WHEN (> X 3363) (GO ZJN))
      (WHEN (> X 3065) (GO JV))
      (GO A)
     TT
      (WHEN (> X 1484) (GO R))
      (WHEN (> M 2484) (GO A))
      (WHEN (< A 1512) (GO A))
      (GO A)
     DHR
      (WHEN (< A 597) (GO XRS))
      (WHEN (> A 941) (GO LHP))
      (GO LZP)
     CRS
      (WHEN (> A 3323) (GO R))
      (WHEN (> M 1119) (GO A))
      (WHEN (> X 1452) (GO R))
      (GO A)
     RCN
      (WHEN (> M 1445) (GO R))
      (WHEN (> X 347) (GO A))
      (WHEN (> A 1485) (GO A))
      (GO R)
     TZD
      (WHEN (< X 1859) (GO TT))
      (GO A)
     LCS
      (WHEN (> S 3607) (GO R))
      (WHEN (< S 3524) (GO XS))
      (GO XDC)
     TQK
      (WHEN (> X 1006) (GO R))
      (WHEN (< A 2151) (GO A))
      (GO A)
     TPB
      (WHEN (> A 3497) (GO R))
      (GO A)
     XJ
      (WHEN (< A 3083) (GO A))
      (WHEN (> M 1619) (GO R))
      (WHEN (< X 1091) (GO A))
      (GO GXH)
     XD
      (WHEN (> X 1933) (GO R))
      (GO MFL)
     FD
      (WHEN (< X 2466) (GO CM))
      (WHEN (> A 2195) (GO R))
      (WHEN (< X 2715) (GO LG))
      (GO R)
     MKT
      (WHEN (> X 2312) (GO A))
      (GO R)
     VJ
      (WHEN (< X 379) (GO TP))
      (WHEN (> X 561) (GO R))
      (GO XLP)
     A
      (RETURN-FROM WORKFLOW (+ X M A S))
     R)))




(LAMBDA ()
  (LABELS ((A (X M A S)
             (* (SIZE X) (SIZE M) (SIZE A) (SIZE S)))
           (R (X M A S)
             (DECLARE (IGNORE X M A S))
             0)
           (PX (X M A S)
             (+ (APPLY #'QKQ (SLICE X M A S 'A '< 2006))
                (DESTRUCTURING-BIND
                    (X M A S)
                    (SLICE X M A S 'A '>= '2006)
                  (+ (APPLY #'A (SLICE X M A S 'M '> 2090))
                     (DESTRUCTURING-BIND
                         (X M A S)
                         (SLICE X M A S 'M '<= '2090)
                       (RFG X M A S))))))
           (PV (X M A S)
             (+ (APPLY #'R (SLICE X M A S 'A '> 1716))
                (DESTRUCTURING-BIND
                    (X M A S)
                    (SLICE X M A S 'A '<= '1716)
                  (A X M A S))))
           (LNX (X M A S)
             (+ (APPLY #'A (SLICE X M A S 'M '> 1548))
                (DESTRUCTURING-BIND
                    (X M A S)
                    (SLICE X M A S 'M '<= '1548)
                  (A X M A S))))
           (RFG (X M A S)
             (+ (APPLY #'GD (SLICE X M A S 'S '< 537))
                (DESTRUCTURING-BIND
                    (X M A S)
                    (SLICE X M A S 'S '>= '537)
                  (+ (APPLY #'R (SLICE X M A S 'X '> 2440))
                     (DESTRUCTURING-BIND
                         (X M A S)
                         (SLICE X M A S 'X '<= '2440)
                       (A X M A S))))))
           (QS (X M A S)
             (+ (APPLY #'A (SLICE X M A S 'S '> 3448))
                (DESTRUCTURING-BIND
                    (X M A S)
                    (SLICE X M A S 'S '<= '3448)
                  (LNX X M A S))))
           (QKQ (X M A S)
             (+ (APPLY #'A (SLICE X M A S 'X '< 1416))
                (DESTRUCTURING-BIND
                    (X M A S)
                    (SLICE X M A S 'X '>= '1416)
                  (CRN X M A S))))
           (CRN (X M A S)
             (+ (APPLY #'A (SLICE X M A S 'X '> 2662))
                (DESTRUCTURING-BIND
                    (X M A S)
                    (SLICE X M A S 'X '<= '2662)
                  (R X M A S))))
           (IN (X M A S)
             (+ (APPLY #'PX (SLICE X M A S 'S '< 1351))
                (DESTRUCTURING-BIND
                    (X M A S)
                    (SLICE X M A S 'S '>= '1351)
                  (QQZ X M A S))))
           (QQZ (X M A S)
             (+ (APPLY #'QS (SLICE X M A S 'S '> 2770))
                (DESTRUCTURING-BIND
                    (X M A S)
                    (SLICE X M A S 'S '<= '2770)
                  (+ (APPLY #'HDJ (SLICE X M A S 'M '< 1801))
                     (DESTRUCTURING-BIND
                         (X M A S)
                         (SLICE X M A S 'M '>= '1801)
                       (R X M A S))))))
           (GD (X M A S)
             (+ (APPLY #'R (SLICE X M A S 'A '> 3333))
                (DESTRUCTURING-BIND
                    (X M A S)
                    (SLICE X M A S 'A '<= '3333)
                  (R X M A S))))
           (HDJ (X M A S)
             (+ (APPLY #'A (SLICE X M A S 'M '> 838))
                (DESTRUCTURING-BIND
                    (X M A S)
                    (SLICE X M A S 'M '<= '838)
                  (PV X M A S)))))
    (IN '(1 4000) '(1 4000) '(1 4000) '(1 4000))))
;;;; qr-generator.lisp

(in-package #:qr-generator)

(defun assocval (item alist)
  (cdr (assoc item alist))) 

(defvar *encoding-modes* '(:numeric-mode :alphanumeric-mode :byte-mode :kanji-mode))

(defvar *encoding-mode-indicators*
  (pairlis *encoding-modes* (list 1 2 4 8)))

(defparameter *count-indicator-length*
  (let ((low-range (alexandria:iota 9 :start 1))
	(mid-range (alexandria:iota 26 :start 10))
	(high-range (alexandria:iota 40 :start 27))
	(low-table (pairlis *encoding-modes* '(10 9 8 8)))
	(mid-table (pairlis *encoding-modes* '(12 11 16 10)))
	(high-table (pairlis *encoding-modes* '(14 13 16 12))))
    (loop for range in (list low-range mid-range high-range)
	 for table in (list low-table mid-table high-table)
       append (loop for version in range collect (cons version table)))))

(defvar *character-capacities*
  '((1 .
     ((:L . ((:numeric-mode . 41) (:alphanumeric-mode . 25) (:byte-mode . 17) (:kanji-mode . 10)))
      (:M . ((:numeric-mode . 34) (:alphanumeric-mode . 20) (:byte-mode . 14) (:kanji-mode . 8)))
      (:Q . ((:numeric-mode . 27) (:alphanumeric-mode . 16) (:byte-mode . 11) (:kanji-mode . 7)))
      (:H . ((:numeric-mode . 17) (:alphanumeric-mode . 10) (:byte-mode . 7) (:kanji-mode . 4)))))
    (2 .
     ((:L . ((:numeric-mode . 77) (:alphanumeric-mode . 47) (:byte-mode . 32) (:kanji-mode . 20)))
      (:M . ((:numeric-mode . 63) (:alphanumeric-mode . 38) (:byte-mode . 26) (:kanji-mode . 16)))
      (:Q . ((:numeric-mode . 48) (:alphanumeric-mode . 29) (:byte-mode . 20) (:kanji-mode . 12)))
      (:H . ((:numeric-mode . 34) (:alphanumeric-mode . 20) (:byte-mode . 14) (:kanji-mode . 8)))))
    (3 .
     ((:L . ((:numeric-mode . 127) (:alphanumeric-mode . 77) (:byte-mode . 53) (:kanji-mode . 32)))
      (:M . ((:numeric-mode . 101) (:alphanumeric-mode . 61) (:byte-mode . 42) (:kanji-mode . 26)))
      (:Q . ((:numeric-mode . 77) (:alphanumeric-mode . 47) (:byte-mode . 32) (:kanji-mode . 20)))
      (:H . ((:numeric-mode . 58) (:alphanumeric-mode . 35) (:byte-mode . 24) (:kanji-mode . 15)))))
    (4 .
     ((:L . ((:numeric-mode . 187) (:alphanumeric-mode . 114) (:byte-mode . 78) (:kanji-mode . 48)))
      (:M . ((:numeric-mode . 149) (:alphanumeric-mode . 90) (:byte-mode . 62) (:kanji-mode . 38)))
      (:Q . ((:numeric-mode . 111) (:alphanumeric-mode . 67) (:byte-mode . 46) (:kanji-mode . 28)))
      (:H . ((:numeric-mode . 82) (:alphanumeric-mode . 50) (:byte-mode . 34) (:kanji-mode . 21)))))
    (5 .
     ((:L . ((:numeric-mode . 255) (:alphanumeric-mode . 154) (:byte-mode . 106) (:kanji-mode . 65)))
      (:M . ((:numeric-mode . 202) (:alphanumeric-mode . 122) (:byte-mode . 84) (:kanji-mode . 52)))
      (:Q . ((:numeric-mode . 144) (:alphanumeric-mode . 87) (:byte-mode . 60) (:kanji-mode . 37)))
      (:H . ((:numeric-mode . 106) (:alphanumeric-mode . 64) (:byte-mode . 44) (:kanji-mode . 27)))))
    (6 .
     ((:L . ((:numeric-mode . 322) (:alphanumeric-mode . 195) (:byte-mode . 134) (:kanji-mode . 82)))
      (:M . ((:numeric-mode . 255) (:alphanumeric-mode . 154) (:byte-mode . 106) (:kanji-mode . 65)))
      (:Q . ((:numeric-mode . 178) (:alphanumeric-mode . 108) (:byte-mode . 74) (:kanji-mode . 45)))
      (:H . ((:numeric-mode . 139) (:alphanumeric-mode . 84) (:byte-mode . 58) (:kanji-mode . 36)))))
    (7 .
     ((:L . ((:numeric-mode . 370) (:alphanumeric-mode . 224) (:byte-mode . 154) (:kanji-mode . 95)))
      (:M . ((:numeric-mode . 293) (:alphanumeric-mode . 178) (:byte-mode . 122) (:kanji-mode . 75)))
      (:Q . ((:numeric-mode . 207) (:alphanumeric-mode . 125) (:byte-mode . 86) (:kanji-mode . 53)))
      (:H . ((:numeric-mode . 154) (:alphanumeric-mode . 93) (:byte-mode . 64) (:kanji-mode . 39)))))
    (8 .
     ((:L . ((:numeric-mode . 461) (:alphanumeric-mode . 279) (:byte-mode . 192) (:kanji-mode . 118)))
      (:M . ((:numeric-mode . 365) (:alphanumeric-mode . 221) (:byte-mode . 152) (:kanji-mode . 93)))
      (:Q . ((:numeric-mode . 259) (:alphanumeric-mode . 157) (:byte-mode . 108) (:kanji-mode . 66)))
      (:H . ((:numeric-mode . 202) (:alphanumeric-mode . 122) (:byte-mode . 84) (:kanji-mode . 52)))))
    (9 .
     ((:L . ((:numeric-mode . 552) (:alphanumeric-mode . 335) (:byte-mode . 230) (:kanji-mode . 141)))
      (:M . ((:numeric-mode . 432) (:alphanumeric-mode . 262) (:byte-mode . 180) (:kanji-mode . 111)))
      (:Q . ((:numeric-mode . 312) (:alphanumeric-mode . 189) (:byte-mode . 130) (:kanji-mode . 80)))
      (:H . ((:numeric-mode . 235) (:alphanumeric-mode . 143) (:byte-mode . 98) (:kanji-mode . 60)))))
    (10 .
     ((:L . ((:numeric-mode . 652) (:alphanumeric-mode . 395) (:byte-mode . 271) (:kanji-mode . 167)))

      (:M . ((:numeric-mode . 513) (:alphanumeric-mode . 311) (:byte-mode . 213) (:kanji-mode . 131)))

      (:Q . ((:numeric-mode . 364) (:alphanumeric-mode . 221) (:byte-mode . 151) (:kanji-mode . 93)))
      (:H . ((:numeric-mode . 288) (:alphanumeric-mode . 174) (:byte-mode . 119) (:kanji-mode . 74)))))
    (11 .
     ((:L . ((:numeric-mode . 772) (:alphanumeric-mode . 468) (:byte-mode . 321) (:kanji-mode . 198)))
      (:M . ((:numeric-mode . 604) (:alphanumeric-mode . 366) (:byte-mode . 251) (:kanji-mode . 155)))
      (:Q . ((:numeric-mode . 427) (:alphanumeric-mode . 259) (:byte-mode . 177) (:kanji-mode . 109)))
      (:H . ((:numeric-mode . 331) (:alphanumeric-mode . 200) (:byte-mode . 137) (:kanji-mode . 85)))))
    (12 .
     ((:L . ((:numeric-mode . 883) (:alphanumeric-mode . 535) (:byte-mode . 367) (:kanji-mode . 226)))
      (:M . ((:numeric-mode . 691) (:alphanumeric-mode . 419) (:byte-mode . 287) (:kanji-mode . 177)))
      (:Q . ((:numeric-mode . 489) (:alphanumeric-mode . 296) (:byte-mode . 203) (:kanji-mode . 125)))
      (:H . ((:numeric-mode . 374) (:alphanumeric-mode . 227) (:byte-mode . 155) (:kanji-mode . 96)))))
    (13 .
     ((:L . ((:numeric-mode . 1022) (:alphanumeric-mode . 619) (:byte-mode . 425) (:kanji-mode . 262)))
      (:M . ((:numeric-mode . 796) (:alphanumeric-mode . 483) (:byte-mode . 331) (:kanji-mode . 204)))
      (:Q . ((:numeric-mode . 580) (:alphanumeric-mode . 352) (:byte-mode . 241) (:kanji-mode . 149)))
      (:H . ((:numeric-mode . 427) (:alphanumeric-mode . 259) (:byte-mode . 177) (:kanji-mode . 109)))))
    (14 .
     ((:L . ((:numeric-mode . 1101) (:alphanumeric-mode . 667) (:byte-mode . 458) (:kanji-mode . 282)))
      (:M . ((:numeric-mode . 871) (:alphanumeric-mode . 528) (:byte-mode . 362) (:kanji-mode . 223)))
      (:Q . ((:numeric-mode . 621) (:alphanumeric-mode . 376) (:byte-mode . 258) (:kanji-mode . 159)))
      (:H . ((:numeric-mode . 468) (:alphanumeric-mode . 283) (:byte-mode . 194) (:kanji-mode . 120)))))
    (15 .
     ((:L . ((:numeric-mode . 1250) (:alphanumeric-mode . 758) (:byte-mode . 520) (:kanji-mode . 320)))
      (:M . ((:numeric-mode . 991) (:alphanumeric-mode . 600) (:byte-mode . 412) (:kanji-mode . 254)))
      (:Q . ((:numeric-mode . 703) (:alphanumeric-mode . 426) (:byte-mode . 292) (:kanji-mode . 180)))
      (:H . ((:numeric-mode . 530) (:alphanumeric-mode . 321) (:byte-mode . 220) (:kanji-mode . 136)))))
    (16 .
     ((:L . ((:numeric-mode . 1408) (:alphanumeric-mode . 854) (:byte-mode . 586) (:kanji-mode . 361)))
      (:M . ((:numeric-mode . 1082) (:alphanumeric-mode . 656) (:byte-mode . 450) (:kanji-mode . 277)))
      (:Q . ((:numeric-mode . 775) (:alphanumeric-mode . 470) (:byte-mode . 322) (:kanji-mode . 198)))
      (:H . ((:numeric-mode . 602) (:alphanumeric-mode . 365) (:byte-mode . 250) (:kanji-mode . 154)))))
    (17 .
     ((:L . ((:numeric-mode . 1548) (:alphanumeric-mode . 938) (:byte-mode . 644) (:kanji-mode . 397)))
      (:M . ((:numeric-mode . 1212) (:alphanumeric-mode . 734) (:byte-mode . 504) (:kanji-mode . 310)))
      (:Q . ((:numeric-mode . 876) (:alphanumeric-mode . 531) (:byte-mode . 364) (:kanji-mode . 224)))
      (:H . ((:numeric-mode . 674) (:alphanumeric-mode . 408) (:byte-mode . 280) (:kanji-mode . 173)))))
    (18 .
     ((:L . ((:numeric-mode . 1725) (:alphanumeric-mode . 1046) (:byte-mode . 718) (:kanji-mode . 442)))
      (:M . ((:numeric-mode . 1346) (:alphanumeric-mode . 816) (:byte-mode . 560) (:kanji-mode . 345)))
      (:Q . ((:numeric-mode . 948) (:alphanumeric-mode . 574) (:byte-mode . 394) (:kanji-mode . 243)))
      (:H . ((:numeric-mode . 746) (:alphanumeric-mode . 452) (:byte-mode . 310) (:kanji-mode . 191)))))
    (19 .  ((:L . ((:numeric-mode . 1903) (:alphanumeric-mode . 1153) (:byte-mode . 792) (:kanji-mode . 488)))
	    (:M . ((:numeric-mode . 1500) (:alphanumeric-mode . 909) (:byte-mode . 624) (:kanji-mode . 384)))
	    (:Q . ((:numeric-mode . 1063) (:alphanumeric-mode . 644) (:byte-mode . 442) (:kanji-mode . 272)))
	    (:H . ((:numeric-mode . 813) (:alphanumeric-mode . 493) (:byte-mode . 338) (:kanji-mode . 208)))))
    (20 .
     ((:L . ((:numeric-mode . 2061) (:alphanumeric-mode . 1249) (:byte-mode . 858) (:kanji-mode . 528)))
      (:M . ((:numeric-mode . 1600) (:alphanumeric-mode . 970) (:byte-mode . 666) (:kanji-mode . 410)))
      (:Q . ((:numeric-mode . 1159) (:alphanumeric-mode . 702) (:byte-mode . 482) (:kanji-mode . 297)))
      (:H . ((:numeric-mode . 919) (:alphanumeric-mode . 557) (:byte-mode . 382) (:kanji-mode . 235)))))
    (21 .
     ((:L . ((:numeric-mode . 2232) (:alphanumeric-mode . 1352) (:byte-mode . 929) (:kanji-mode . 572)))
      (:M . ((:numeric-mode . 1708) (:alphanumeric-mode . 1035) (:byte-mode . 711) (:kanji-mode . 438)))
      (:Q . ((:numeric-mode . 1224) (:alphanumeric-mode . 742) (:byte-mode . 509) (:kanji-mode . 314)))
      (:H . ((:numeric-mode . 969) (:alphanumeric-mode . 587) (:byte-mode . 403) (:kanji-mode . 248)))))
    (22 .
     ((:L . ((:numeric-mode . 2409) (:alphanumeric-mode . 1460) (:byte-mode . 1003) (:kanji-mode . 618)))
      (:M . ((:numeric-mode . 1872) (:alphanumeric-mode . 1134) (:byte-mode . 779) (:kanji-mode . 480)))
      (:Q . ((:numeric-mode . 1358) (:alphanumeric-mode . 823) (:byte-mode . 565) (:kanji-mode . 348)))
      (:H . ((:numeric-mode . 1056) (:alphanumeric-mode . 640) (:byte-mode . 439) (:kanji-mode . 270)))))
    (23 .
     ((:L . ((:numeric-mode . 2620) (:alphanumeric-mode . 1588) (:byte-mode . 1091) (:kanji-mode . 672)))
      (:M . ((:numeric-mode . 2059) (:alphanumeric-mode . 1248) (:byte-mode . 857) (:kanji-mode . 528)))
      (:Q . ((:numeric-mode . 1468) (:alphanumeric-mode . 890) (:byte-mode . 611) (:kanji-mode . 376)))
      (:H . ((:numeric-mode . 1108) (:alphanumeric-mode . 672) (:byte-mode . 461) (:kanji-mode . 284)))))
    (24 .
     ((:L . ((:numeric-mode . 2812) (:alphanumeric-mode . 1704) (:byte-mode . 1171) (:kanji-mode . 721)))
      (:M . ((:numeric-mode . 2188) (:alphanumeric-mode . 1326) (:byte-mode . 911) (:kanji-mode . 561)))
      (:Q . ((:numeric-mode . 1588) (:alphanumeric-mode . 963) (:byte-mode . 661) (:kanji-mode . 407)))
      (:H . ((:numeric-mode . 1228) (:alphanumeric-mode . 744) (:byte-mode . 511) (:kanji-mode . 315)))))
    (25 .
     ((:L . ((:numeric-mode . 3057) (:alphanumeric-mode . 1853) (:byte-mode . 1273) (:kanji-mode . 784)))
      (:M . ((:numeric-mode . 2395) (:alphanumeric-mode . 1451) (:byte-mode . 997) (:kanji-mode . 614)))
      (:Q . ((:numeric-mode . 1718) (:alphanumeric-mode . 1041) (:byte-mode . 715) (:kanji-mode . 440)))
      (:H . ((:numeric-mode . 1286) (:alphanumeric-mode . 779) (:byte-mode . 535) (:kanji-mode . 330)))))
    (26 .
     ((:L . ((:numeric-mode . 3283) (:alphanumeric-mode . 1990) (:byte-mode . 1367) (:kanji-mode . 842)))
      (:M . ((:numeric-mode . 2544) (:alphanumeric-mode . 1542) (:byte-mode . 1059) (:kanji-mode . 652)))
      (:Q . ((:numeric-mode . 1804) (:alphanumeric-mode . 1094) (:byte-mode . 751) (:kanji-mode . 462)))
      (:H . ((:numeric-mode . 1425) (:alphanumeric-mode . 864) (:byte-mode . 593) (:kanji-mode . 365)))))
    (27 .
     ((:L . ((:numeric-mode . 3517) (:alphanumeric-mode . 2132) (:byte-mode . 1465) (:kanji-mode . 902)))
      (:M . ((:numeric-mode . 2701) (:alphanumeric-mode . 1637) (:byte-mode . 1125) (:kanji-mode . 692)))
      (:Q . ((:numeric-mode . 1933) (:alphanumeric-mode . 1172) (:byte-mode . 805) (:kanji-mode . 496)))
      (:H . ((:numeric-mode . 1501) (:alphanumeric-mode . 910) (:byte-mode . 625) (:kanji-mode . 385)))))
    (28 .
     ((:L . ((:numeric-mode . 3669) (:alphanumeric-mode . 2223) (:byte-mode . 1528) (:kanji-mode . 940)))
      (:M . ((:numeric-mode . 2857) (:alphanumeric-mode . 1732) (:byte-mode . 1190) (:kanji-mode . 732)))
      (:Q . ((:numeric-mode . 2085) (:alphanumeric-mode . 1263) (:byte-mode . 868) (:kanji-mode . 534)))
      (:H . ((:numeric-mode . 1581) (:alphanumeric-mode . 958) (:byte-mode . 658) (:kanji-mode . 405)))))
    (29 .
     ((:L . ((:numeric-mode . 3909) (:alphanumeric-mode . 2369) (:byte-mode . 1628) (:kanji-mode . 1002)))
      (:M . ((:numeric-mode . 3035) (:alphanumeric-mode . 1839) (:byte-mode . 1264) (:kanji-mode . 778)))
      (:Q . ((:numeric-mode . 2181) (:alphanumeric-mode . 1322) (:byte-mode . 908) (:kanji-mode . 559)))
      (:H . ((:numeric-mode . 1677) (:alphanumeric-mode . 1016) (:byte-mode . 698) (:kanji-mode . 430)))))
    (30 .
     ((:L . ((:numeric-mode . 4158) (:alphanumeric-mode . 2520) (:byte-mode . 1732) (:kanji-mode . 1066)))
      (:M . ((:numeric-mode . 3289) (:alphanumeric-mode . 1994) (:byte-mode . 1370) (:kanji-mode . 843)))
      (:Q . ((:numeric-mode . 2358) (:alphanumeric-mode . 1429) (:byte-mode . 982) (:kanji-mode . 604)))
      (:H . ((:numeric-mode . 1782) (:alphanumeric-mode . 1080) (:byte-mode . 742) (:kanji-mode . 457)))))
    (31 .
     ((:L . ((:numeric-mode . 4417) (:alphanumeric-mode . 2677) (:byte-mode . 1840) (:kanji-mode . 1132)))
      (:M . ((:numeric-mode . 3486) (:alphanumeric-mode . 2113) (:byte-mode . 1452) (:kanji-mode . 894)))
      (:Q . ((:numeric-mode . 2473) (:alphanumeric-mode . 1499) (:byte-mode . 1030) (:kanji-mode . 634)))
      (:H . ((:numeric-mode . 1897) (:alphanumeric-mode . 1150) (:byte-mode . 790) (:kanji-mode . 486)))))
    (32 .
     ((:L . ((:numeric-mode . 4686) (:alphanumeric-mode . 2840) (:byte-mode . 1952) (:kanji-mode . 1201)))
      (:M . ((:numeric-mode . 3693) (:alphanumeric-mode . 2238) (:byte-mode . 1538) (:kanji-mode . 947)))
      (:Q . ((:numeric-mode . 2670) (:alphanumeric-mode . 1618) (:byte-mode . 1112) (:kanji-mode . 684)))
      (:H . ((:numeric-mode . 2022) (:alphanumeric-mode . 1226) (:byte-mode . 842) (:kanji-mode . 518)))))
    (33 .
     ((:L . ((:numeric-mode . 4965) (:alphanumeric-mode . 3009) (:byte-mode . 2068) (:kanji-mode . 1273)))
      (:M . ((:numeric-mode . 3909) (:alphanumeric-mode . 2369) (:byte-mode . 1628) (:kanji-mode . 1002)))
      (:Q . ((:numeric-mode . 2805) (:alphanumeric-mode . 1700) (:byte-mode . 1168) (:kanji-mode . 719)))
      (:H . ((:numeric-mode . 2157) (:alphanumeric-mode . 1307) (:byte-mode . 898) (:kanji-mode . 553)))))
    (34 .
     ((:L . ((:numeric-mode . 5253) (:alphanumeric-mode . 3183) (:byte-mode . 2188) (:kanji-mode . 1347)))
      (:M . ((:numeric-mode . 4134) (:alphanumeric-mode . 2506) (:byte-mode . 1722) (:kanji-mode . 1060)))
      (:Q . ((:numeric-mode . 2949) (:alphanumeric-mode . 1787) (:byte-mode . 1228) (:kanji-mode . 756)))
      (:H . ((:numeric-mode . 2301) (:alphanumeric-mode . 1394) (:byte-mode . 958) (:kanji-mode . 590)))))
    (35 .
     ((:L . ((:numeric-mode . 5529) (:alphanumeric-mode . 3351) (:byte-mode . 2303) (:kanji-mode . 1417)))
      (:M . ((:numeric-mode . 4343) (:alphanumeric-mode . 2632) (:byte-mode . 1809) (:kanji-mode . 1113)))
      (:Q . ((:numeric-mode . 3081) (:alphanumeric-mode . 1867) (:byte-mode . 1283) (:kanji-mode . 790)))
      (:H . ((:numeric-mode . 2361) (:alphanumeric-mode . 1431) (:byte-mode . 983) (:kanji-mode . 605)))))
    (36 .
     ((:L . ((:numeric-mode . 5836) (:alphanumeric-mode . 3537) (:byte-mode . 2431) (:kanji-mode . 1496)))
      (:M . ((:numeric-mode . 4588) (:alphanumeric-mode . 2780) (:byte-mode . 1911) (:kanji-mode . 1176)))
      (:Q . ((:numeric-mode . 3244) (:alphanumeric-mode . 1966) (:byte-mode . 1351) (:kanji-mode . 832)))
      (:H . ((:numeric-mode . 2524) (:alphanumeric-mode . 1530) (:byte-mode . 1051) (:kanji-mode . 647)))))
    (37 .
     ((:L . ((:numeric-mode . 6153) (:alphanumeric-mode . 3729) (:byte-mode . 2563) (:kanji-mode . 1577)))
      (:M . ((:numeric-mode . 4775) (:alphanumeric-mode . 2894) (:byte-mode . 1989) (:kanji-mode . 1224)))
      (:Q . ((:numeric-mode . 3417) (:alphanumeric-mode . 2071) (:byte-mode . 1423) (:kanji-mode . 876)))
      (:H . ((:numeric-mode . 2625) (:alphanumeric-mode . 1591) (:byte-mode . 1093) (:kanji-mode . 673)))))
    (38 .
     ((:L . ((:numeric-mode . 6479) (:alphanumeric-mode . 3927) (:byte-mode . 2699) (:kanji-mode . 1661)))
      (:M . ((:numeric-mode . 5039) (:alphanumeric-mode . 3054) (:byte-mode . 2099) (:kanji-mode . 1292)))
      (:Q . ((:numeric-mode . 3599) (:alphanumeric-mode . 2181) (:byte-mode . 1499) (:kanji-mode . 923)))
      (:H . ((:numeric-mode . 2735) (:alphanumeric-mode . 1658) (:byte-mode . 1139) (:kanji-mode . 701)))))
    (39 .
     ((:L . ((:numeric-mode . 6743) (:alphanumeric-mode . 4087) (:byte-mode . 2809) (:kanji-mode . 1729)))
      (:M . ((:numeric-mode . 5313) (:alphanumeric-mode . 3220) (:byte-mode . 2213) (:kanji-mode . 1362)))
      (:Q . ((:numeric-mode . 3791) (:alphanumeric-mode . 2298) (:byte-mode . 1579) (:kanji-mode . 972)))
      (:H . ((:numeric-mode . 2927) (:alphanumeric-mode . 1774) (:byte-mode . 1219) (:kanji-mode . 750)))))
    (40 .
     ((:L . ((:numeric-mode . 7089) (:alphanumeric-mode . 4296) (:byte-mode . 2953) (:kanji-mode . 1817)))
      (:M . ((:numeric-mode . 5596) (:alphanumeric-mode . 3391) (:byte-mode . 2331) (:kanji-mode . 1435)))
      (:Q . ((:numeric-mode . 3993) (:alphanumeric-mode . 2420) (:byte-mode . 1663) (:kanji-mode . 1024)))
      (:H . ((:numeric-mode . 3057) (:alphanumeric-mode . 1852) (:byte-mode . 1273) (:kanji-mode . 784)))))))

(defvar *alphanumeric-encoding*
  '((#\0 . 0) (#\1 . 1) (#\2 . 2) (#\3 . 3) (#\4 . 4) (#\5 . 5) (#\6 . 6) (#\7 . 7)
    (#\8 . 8) (#\9 . 9) (#\A . 10) (#\B . 11) (#\C . 12) (#\D . 13) (#\E . 14) (#\F . 15)
    (#\G . 16) (#\H . 17) (#\I . 18) (#\J . 19) (#\K . 20) (#\L . 21) (#\M . 22) (#\N . 23)
    (#\O . 24) (#\P . 25) (#\Q . 26) (#\R . 27) (#\S . 28) (#\T . 29) (#\U . 30) (#\V . 31)
    (#\W . 32) (#\X . 33) (#\Y . 34) (#\Z . 35) (#\Space . 36) (#\$ . 37) (#\% . 38) (#\* . 39)
    (#\+ . 40) (#\- . 41) (#\. . 42) (#\/ . 43) (#\: . 44)))

(defvar *capacity*
  '((1 . ((:L . 19) (:M . 16) (:Q . 13) (:H . 9)))
    (2 . ((:L . 34) (:M . 28) (:Q . 22) (:H . 16)))
    (3 . ((:L . 55) (:M . 44) (:Q . 34) (:H . 26)))
    (4 . ((:L . 80) (:M . 64) (:Q . 48) (:H . 36)))
    (5 . ((:L . 108) (:M . 86) (:Q . 62) (:H . 46)))
    (6 . ((:L . 136) (:M . 108) (:Q . 76) (:H . 60)))
    (7 . ((:L . 156) (:M . 124) (:Q . 88) (:H . 66)))
    (8 . ((:L . 194) (:M . 154) (:Q . 110) (:H . 86)))
    (9 . ((:L . 232) (:M . 182) (:Q . 132) (:H . 100)))
    (10 . ((:L . 274) (:M . 216) (:Q . 154) (:H . 122)))
    (11 . ((:L . 324) (:M . 254) (:Q . 180) (:H . 140)))
    (12 . ((:L . 370) (:M . 290) (:Q . 206) (:H . 158)))
    (13 . ((:L . 428) (:M . 334) (:Q . 244) (:H . 180)))
    (14 . ((:L . 461) (:M . 365) (:Q . 261) (:H . 197)))
    (15 . ((:L . 523) (:M . 415) (:Q . 295) (:H . 223)))
    (16 . ((:L . 589) (:M . 453) (:Q . 325) (:H . 253)))
    (17 . ((:L . 647) (:M . 507) (:Q . 367) (:H . 283)))
    (18 . ((:L . 721) (:M . 563) (:Q . 397) (:H . 313)))
    (19 . ((:L . 795) (:M . 627) (:Q . 445) (:H . 341)))
    (20 . ((:L . 861) (:M . 669) (:Q . 485) (:H . 385)))
    (21 . ((:L . 932) (:M . 714) (:Q . 512) (:H . 406)))
    (22 . ((:L . 1006) (:M . 782) (:Q . 568) (:H . 442)))
    (23 . ((:L . 1094) (:M . 860) (:Q . 614) (:H . 464)))
    (24 . ((:L . 1174) (:M . 914) (:Q . 664) (:H . 514)))
    (25 . ((:L . 1276) (:M . 1000) (:Q . 718) (:H . 538)))
    (26 . ((:L . 1370) (:M . 1062) (:Q . 754) (:H . 596)))
    (27 . ((:L . 1468) (:M . 1128) (:Q . 808) (:H . 628)))
    (28 . ((:L . 1531) (:M . 1193) (:Q . 871) (:H . 661)))
    (29 . ((:L . 1631) (:M . 1267) (:Q . 911) (:H . 701)))
    (30 . ((:L . 1735) (:M . 1373) (:Q . 985) (:H . 745)))
    (31 . ((:L . 1843) (:M . 1455) (:Q . 1033) (:H . 793)))
    (32 . ((:L . 1955) (:M . 1541) (:Q . 1115) (:H . 845)))
    (33 . ((:L . 2071) (:M . 1631) (:Q . 1171) (:H . 901)))
    (34 . ((:L . 2191) (:M . 1725) (:Q . 1231) (:H . 961)))
    (35 . ((:L . 2306) (:M . 1812) (:Q . 1286) (:H . 986)))
    (36 . ((:L . 2434) (:M . 1914) (:Q . 1354) (:H . 1054)))
    (37 . ((:L . 2566) (:M . 1992) (:Q . 1426) (:H . 1096)))
    (38 . ((:L . 2702) (:M . 2102) (:Q . 1502) (:H . 1142)))
    (39 . ((:L . 2812) (:M . 2216) (:Q . 1582) (:H . 1222)))
    (40 . ((:L . 2956) (:M . 2334) (:Q . 1666) (:H . 1276))))
  "Number of 8-bit wods in a QR code given its version and error correction.")

(defun categorize-string (string &optional (auto-upcase-string nil))
  (let* ((char-sequence (map 'list #'character (if auto-upcase-string
						   (string-upcase string)
						   string)))
	 (numeric-mode (every #'numeric-mode-p char-sequence))
	 (alphanumeric-mode (every #'alphanumeric-mode-p char-sequence))
	 (byte-mode (not alphanumeric-mode)))
    (list (cons :numeric-mode numeric-mode)
	  (cons :alphanumeric-mode alphanumeric-mode)
	  (cons :byte-mode byte-mode))))

(defun get-mode (string)
  (loop with modes = (categorize-string string)
     for try-mode in *encoding-modes*
     for (this-mode . possible) = (assoc try-mode modes) do
       (if possible
	   (return this-mode))))

(defun numeric-mode-p (char)
  (digit-char-p char))

(defun alphanumeric-mode-p (char)
  (let ((alphabet (map 'list #'character "ABCDEFGHIJKLMNOPQRSTUVWXYZ $%*+-./")))
    (or (numeric-mode-p char)
	(member char alphabet))))

(defun choose-error-correction (level)
  (assert (= 1 (length level)))
  (cond ((string-equal level "L") :L)
	((string-equal level "M") :M)
	((string-equal level "Q") :Q)
	((string-equal level "H") :H)
	(t (error "Unrecognized error correction level. Must be one of (L M Q H)"))))

(defun encoding-mode-indicator (encoding-mode)
  (let ((encoding-mode-indicator (assocval encoding-mode *encoding-mode-indicators*)))
    (padded-binary encoding-mode-indicator 4)))

(defun get-version (string-length error-correction-mode encoding-mode)
  (loop for version from 1 upto 40
     for version-level = (assocval version *character-capacities*)
     for correction-level = (assocval error-correction-mode version-level)
     for max-chars = (assocval encoding-mode correction-level) do
       (when (> max-chars string-length)
	   (return version))))

(defun get-qr-version-from-string (string error-correction-mode)
  (let ((length (length string))
	(encoding-mode (get-mode string)))
    (get-version length error-correction-mode encoding-mode)))

(defun padded-binary (decimal-number width)
  (format nil "~v,'0b" width decimal-number))

(defun character-count-indicator-length (version encoding-mode)
  (let ((version-table (assocval version *count-indicator-length*)))
    (assocval encoding-mode version-table)))

(defun character-count-indicator (string-length version encoding-mode)
  (padded-binary string-length
		 (character-count-indicator-length version encoding-mode)))

(defun split-string-into-blocks (string block-size)
  (loop with string-length = (length string)
     for i = 0 then (+ i block-size)
       while (< i (- string-length block-size))
       for substring = (subseq string i (+ i block-size))
       collect substring into split
       finally
       (return (nconc split (list (subseq string i))))))

(defun split-integer-to-blocks (integer &key (block-size 3))
  (let* ((string-data (format nil "~d" integer))
	 (split-string (split-string-into-blocks string-data block-size)))
    (mapcar #'parse-integer split-string)))

(defun represent-substring (substring)
  (ecase (length substring)
    (1 (padded-binary (assocval (char substring 0) *alphanumeric-encoding*)
		      6))
    (2 (padded-binary (+ (* 45 (assocval (char substring 0) *alphanumeric-encoding*))
			 (assocval (char substring 1) *alphanumeric-encoding*))
		      11))))

(defun string-to-message (string correction-mode)
  (let* ((length (length string))
	 (encoding-mode (get-mode string))
	 (error-correction-mode (choose-error-correction correction-mode))
	 (version (get-qr-version-from-string string error-correction-mode))
	 (capacity (qr-capacity version error-correction-mode))
	 (mode-indicator (encoding-mode-indicator encoding-mode))
	 (character-count-indic (character-count-indicator length version encoding-mode))
	 (raw-data (concatenate 'string mode-indicator character-count-indic
				(encode-data string encoding-mode)))
	 (terminated-data (concatenate 'string raw-data (terminator (length raw-data) capacity)))
	 (padded-data (concatenate 'string terminated-data
				   (padding-to-multiple-of-eight (length terminated-data)))))
    (values
     (format nil "~a~a" padded-data (filling-to-capacity (length padded-data) capacity))
     (list :version version :error-correction-mode error-correction-mode))))

(defgeneric encode-data (data encoding-mode)
  (:documentation "Represent the data as a string of binary numbers."))

(defmethod encode-data ((data integer) (encoding-mode (eql :numeric-mode)))
  (format nil "~{~b~}" (split-integer-to-blocks data)))

(defmethod encode-data ((data string) (encoding-mode (eql :numeric-mode)))
  (format nil "~{~b~}" (mapcar #'parse-integer
			       (split-string-into-blocks data 3))))

(defmethod encode-data ((data string) (encoding-mode (eql :alphanumeric-mode)))
  (let ((substrings (split-string-into-blocks data 2)))
    (format nil "~{~b~}" (map 'list #'represent-substring substrings))))

(defmethod encode-data ((data string) (encoding-mode (eql :byte-mode)))
  (format nil "~{~8,'0b~}" (map 'list #'char-code data)))

(defun qr-capacity (version error-correction-mode)
  (* 8 (assocval error-correction-mode
		 (assocval version *capacity*))))

(defun terminator (string-length capacity)
  (if (< string-length capacity)
      (padded-binary 0 (min 4 (- capacity string-length)))
      ""))

(defun padding-to-multiple-of-eight (string-length)
  (unless (zerop (rem string-length 8))
    (padded-binary 0 (- 8 (rem string-length 8)))))

(defun filling-to-capacity (string-length capacity)
  (let ((filling-bytes (alexandria:circular-list (padded-binary 236 8)
						 (padded-binary 17 8)))
	(bytes-to-fill (/ (- capacity string-length) 8)))
    (apply #'concatenate 'string (loop repeat bytes-to-fill
				   for i in filling-bytes
				    collect i))))

(defvar *log-antilog* (loop for i upto 285
			 for j = 1 then (if (< 255 (* 2 j))
					    (logxor (* 2 j) 285)
					    (* 2 j))
			 collect (cons i j))
  "Galois Field")

(defun product (&rest integers)
  (reduce #'logxor integers))

(defun remove-trailing-zeros (list)
  (let ((result nil)
	(seen-nonzero nil))
    (dolist (number (reverse list))
      (when (or seen-nonzero (not (zerop number)))
	(setf seen-nonzero t)
	(push number result)))
    result))

(defun nil-padding (n)
  (loop repeat n collect nil))

(defun degree (polynomial)
  (1- (length (remove-trailing-zeros polynomial))))

(defun multiply (poly-a poly-b)
  (let* ((deg-a (degree poly-a))
	 (deg-b (degree poly-b))
	 (intermediate-products nil))
    (setf intermediate-products
	  (loop for coef upto deg-a collect
	       (append (nil-padding coef)
		       (mapcar (lambda (x) (* x (nth coef poly-a))) poly-b)
		       (nil-padding (- deg-a coef)))))
    (loop for k upto (+ deg-a deg-b) collect
	 (loop for product in intermediate-products
	      sum (or (nth k product) 0)))))

(defun multiply-exponent-list (poly-a poly-b)
  (let ((basis (mapcar (lambda (x) (mod (+ x (first poly-a)) 255)) poly-b))
	(second (mapcar (lambda (x) (mod (+ x (second poly-a)) 255)) poly-b)))
    (loop for a in (append basis (cons nil nil))
       for b in (append (cons nil nil) second) ;; do
       collect (galois-exponent (reduce #'product
					(mapcar #'nth-galois (remove-if #'null (list a b))))))))

(defun galois-exponent (integer)
  (car (rassoc (abs integer) *log-antilog*)))

(defun nth-galois (integer)
  "Return the integer-th element of the Galois field"
  (if (null integer)
      0
      (assocval integer *log-antilog*)))

(defvar *generator-polynomials*
  (loop for i from 0 upto 35
     for pol = (list i 0) then (multiply-exponent-list (list i 0) pol)
     collect (cons (1+ i) pol)))

(defun split-message-string (message)
  (mapcar (lambda (x) (parse-integer x :radix 2))
	  (split-string-into-blocks message 8)))

(defun message-polynomial (message)
  (reverse (split-message-string message)))

(defun shift-polynomial (polynomial n)
  (multiply polynomial
	    (loop for i upto n collect (if (= n i) 1 0))))

(defun prepare-polynomials (message-polynomial generator-galois)
  (let* ((generator-poly (mapcar #'nth-galois generator-galois))
	 (message-shift (degree generator-poly))
	 (generator-shift (degree message-polynomial)))
    (values (shift-polynomial message-polynomial message-shift)
	    (mapcar #'galois-exponent (shift-polynomial generator-poly generator-shift)))))

(defun step1a (message-poly generator-galois)
  (let* ((lead-coef (alexandria:last-elt (mapcar #'galois-exponent message-poly))))
    (mapcar (lambda (x) (multiply-galois-exponents lead-coef x))
	    generator-galois)))

(defun step1b (message-poly generator-galois)
  (let ((generator-poly (mapcar #'nth-galois generator-galois)))
    (remove-trailing-zeros (mapcar #'logxor message-poly generator-poly))))

(defun multiply-galois-exponents (&rest numbers)
  (if (some #'null numbers)
      nil
      (mod (reduce #'+ numbers) 255)))

(defun reed-solomon (message-poly generator-galois)
  (multiple-value-bind (mpo ggo) (prepare-polynomials message-poly generator-galois)
    (loop repeat (count-if-not #'zerop mpo)
       for gg = ggo then (rest gg)
       for mp = mpo then r1b
       for r1a = (step1a mp gg)
       for r1b = (step1b mp r1a)
	 finally (return r1b))))

ALTER TABLE awarded_contracts
    ADD COLUMN preferred_bidder_country_iso_code TEXT;

UPDATE awarded_contracts
SET preferred_bidder_country_iso_code =
        CASE
            WHEN preferred_bidder_country_id = 0
                THEN 'GB'
            WHEN preferred_bidder_country_id = 4
                THEN 'AF'
            WHEN preferred_bidder_country_id = 6
                THEN 'AX'
            WHEN preferred_bidder_country_id = 11
                THEN 'AS'
            WHEN preferred_bidder_country_id = 12
                THEN 'AD'
            WHEN preferred_bidder_country_id = 14
                THEN 'AI'
            WHEN preferred_bidder_country_id = 15
                THEN 'AQ'
            WHEN preferred_bidder_country_id = 27
                THEN 'BH'
            WHEN preferred_bidder_country_id = 31
                THEN 'BY'
            WHEN preferred_bidder_country_id = 38
                THEN 'BQ'
            WHEN preferred_bidder_country_id = 41
                THEN 'BV'
            WHEN preferred_bidder_country_id = 44
                THEN 'IO'
            WHEN preferred_bidder_country_id = 56
                THEN 'KY'
            WHEN preferred_bidder_country_id = 62
                THEN 'CX'
            WHEN preferred_bidder_country_id = 63
                THEN 'CC'
            WHEN preferred_bidder_country_id = 69
                THEN 'CK'
            WHEN preferred_bidder_country_id = 71
                THEN 'CR'
            WHEN preferred_bidder_country_id = 72
                THEN 'HR'
            WHEN preferred_bidder_country_id = 73
                THEN 'CU'
            WHEN preferred_bidder_country_id = 74
                THEN 'CW'
            WHEN preferred_bidder_country_id = 97
                THEN 'PF'
            WHEN preferred_bidder_country_id = 98
                THEN 'TF'
            WHEN preferred_bidder_country_id = 103
                THEN 'GE'
            WHEN preferred_bidder_country_id = 104
                THEN 'DE'
            WHEN preferred_bidder_country_id = 105
                THEN 'GH'
            WHEN preferred_bidder_country_id = 120
                THEN 'HM'
            WHEN preferred_bidder_country_id = 131
                THEN 'IE'
            WHEN preferred_bidder_country_id = 132
                THEN 'IL'
            WHEN preferred_bidder_country_id = 135
                THEN 'JM'
            WHEN preferred_bidder_country_id = 145
                THEN 'KI'
            WHEN preferred_bidder_country_id = 148
                THEN 'XK'
            WHEN preferred_bidder_country_id = 157
                THEN 'LI'
            WHEN preferred_bidder_country_id = 158
                THEN 'LT'
            WHEN preferred_bidder_country_id = 160
                THEN 'MO'
            WHEN preferred_bidder_country_id = 169
                THEN 'IM'
            WHEN preferred_bidder_country_id = 171
                THEN 'MH'
            WHEN preferred_bidder_country_id = 175
                THEN 'YT'
            WHEN preferred_bidder_country_id = 178
                THEN 'FM'
            WHEN preferred_bidder_country_id = 186
                THEN 'MZ'
            WHEN preferred_bidder_country_id = 189
                THEN 'NR'
            WHEN preferred_bidder_country_id = 190
                THEN 'NP'
            WHEN preferred_bidder_country_id = 191
                THEN 'NL'
            WHEN preferred_bidder_country_id = 197
                THEN 'NU'
            WHEN preferred_bidder_country_id = 198
                THEN 'NF'
            WHEN preferred_bidder_country_id = 199
                THEN 'MP'
            WHEN preferred_bidder_country_id = 201
                THEN 'PS'
            WHEN preferred_bidder_country_id = 205
                THEN 'PW'
            WHEN preferred_bidder_country_id = 208
                THEN 'PY'
            WHEN preferred_bidder_country_id = 210
                THEN 'PH'
            WHEN preferred_bidder_country_id = 211
                THEN 'PN'
            WHEN preferred_bidder_country_id = 212
                THEN 'PL'
            WHEN preferred_bidder_country_id = 217
                THEN 'RE'
            WHEN preferred_bidder_country_id = 220
                THEN 'RU'
            WHEN preferred_bidder_country_id = 225
                THEN 'BL'
            WHEN preferred_bidder_country_id = 230
                THEN 'SX'
            WHEN preferred_bidder_country_id = 231
                THEN 'MF'
            WHEN preferred_bidder_country_id = 232
                THEN 'PM'
            WHEN preferred_bidder_country_id = 233
                THEN 'VC'
            WHEN preferred_bidder_country_id = 248
                THEN 'ZA'
            WHEN preferred_bidder_country_id = 249
                THEN 'GS'
            WHEN preferred_bidder_country_id = 250
                THEN 'ES'
            WHEN preferred_bidder_country_id = 251
                THEN 'LK'
            WHEN preferred_bidder_country_id = 266
                THEN 'TK'
            WHEN preferred_bidder_country_id = 267
                THEN 'TO'
            WHEN preferred_bidder_country_id = 274
                THEN 'TV'
            WHEN preferred_bidder_country_id = 278
                THEN 'UM'
            WHEN preferred_bidder_country_id = 286
                THEN 'VI'
            WHEN preferred_bidder_country_id = 288
                THEN 'WF'
            WHEN preferred_bidder_country_id = 289
                THEN 'YE'
            WHEN preferred_bidder_country_id = 292
                THEN 'AL'
            WHEN preferred_bidder_country_id = 293
                THEN 'DZ'
            WHEN preferred_bidder_country_id = 295
                THEN 'AG'
            WHEN preferred_bidder_country_id = 302
                THEN 'BS'
            WHEN preferred_bidder_country_id = 303
                THEN 'BD'
            WHEN preferred_bidder_country_id = 311
                THEN 'BA'
            WHEN preferred_bidder_country_id = 336
                THEN 'DM'
            WHEN preferred_bidder_country_id = 343
                THEN 'ER'
            WHEN preferred_bidder_country_id = 346
                THEN 'FK'
            WHEN preferred_bidder_country_id = 364
                THEN 'GN'
            WHEN preferred_bidder_country_id = 366
                THEN 'GY'
            WHEN preferred_bidder_country_id = 371
                THEN 'IS'
            WHEN preferred_bidder_country_id = 373
                THEN 'ID'
            WHEN preferred_bidder_country_id = 382
                THEN 'KE'
            WHEN preferred_bidder_country_id = 383
                THEN 'KP'
            WHEN preferred_bidder_country_id = 384
                THEN 'KR'
            WHEN preferred_bidder_country_id = 422
                THEN 'PA'
            WHEN preferred_bidder_country_id = 425
                THEN 'PT'
            WHEN preferred_bidder_country_id = 439
                THEN 'RS'
            WHEN preferred_bidder_country_id = 440
                THEN 'SC'
            WHEN preferred_bidder_country_id = 449
                THEN 'SR'
            WHEN preferred_bidder_country_id = 462
                THEN 'TR'
            WHEN preferred_bidder_country_id = 463
                THEN 'TM'
            WHEN preferred_bidder_country_id = 464
                THEN 'TC'
            WHEN preferred_bidder_country_id = 477
                THEN 'AO'
            WHEN preferred_bidder_country_id = 480
                THEN 'AW'
            WHEN preferred_bidder_country_id = 485
                THEN 'BE'
            WHEN preferred_bidder_country_id = 487
                THEN 'BJ'
            WHEN preferred_bidder_country_id = 498
                THEN 'KH'
            WHEN preferred_bidder_country_id = 519
                THEN 'SV'
            WHEN preferred_bidder_country_id = 522
                THEN 'ET'
            WHEN preferred_bidder_country_id = 523
                THEN 'FO'
            WHEN preferred_bidder_country_id = 559
                THEN 'LS'
            WHEN preferred_bidder_country_id = 561
                THEN 'LY'
            WHEN preferred_bidder_country_id = 562
                THEN 'LU'
            WHEN preferred_bidder_country_id = 584
                THEN 'NZ'
            WHEN preferred_bidder_country_id = 585
                THEN 'NI'
            WHEN preferred_bidder_country_id = 603
                THEN 'ST'
            WHEN preferred_bidder_country_id = 615
                THEN 'SE'
            WHEN preferred_bidder_country_id = 617
                THEN 'SY'
            WHEN preferred_bidder_country_id = 624
                THEN 'TT'
            WHEN preferred_bidder_country_id = 626
                THEN 'UG'
            WHEN preferred_bidder_country_id = 629
                THEN 'US'
            WHEN preferred_bidder_country_id = 638
                THEN 'AR'
            WHEN preferred_bidder_country_id = 640
                THEN 'AU'
            WHEN preferred_bidder_country_id = 655
                THEN 'CM'
            WHEN preferred_bidder_country_id = 656
                THEN 'CA'
            WHEN preferred_bidder_country_id = 662
                THEN 'CN'
            WHEN preferred_bidder_country_id = 692
                THEN 'GG'
            WHEN preferred_bidder_country_id = 693
                THEN 'GW'
            WHEN preferred_bidder_country_id = 703
                THEN 'JP'
            WHEN preferred_bidder_country_id = 705
                THEN 'JO'
            WHEN preferred_bidder_country_id = 706
                THEN 'KZ'
            WHEN preferred_bidder_country_id = 746
                THEN 'WS'
            WHEN preferred_bidder_country_id = 762
                THEN 'CH'
            WHEN preferred_bidder_country_id = 763
                THEN 'TW'
            WHEN preferred_bidder_country_id = 774
                THEN 'VU'
            WHEN preferred_bidder_country_id = 781
                THEN 'AT'
            WHEN preferred_bidder_country_id = 787
                THEN 'BO'
            WHEN preferred_bidder_country_id = 793
                THEN 'BF'
            WHEN preferred_bidder_country_id = 811
                THEN 'EG'
            WHEN preferred_bidder_country_id = 815
                THEN 'FI'
            WHEN preferred_bidder_country_id = 829
                THEN 'HT'
            WHEN preferred_bidder_country_id = 830
                THEN 'HN'
            WHEN preferred_bidder_country_id = 836
                THEN 'IT'
            WHEN preferred_bidder_country_id = 838
                THEN 'JE'
            WHEN preferred_bidder_country_id = 839
                THEN 'KW'
            WHEN preferred_bidder_country_id = 840
                THEN 'KG'
            WHEN preferred_bidder_country_id = 869
                THEN 'OM'
            WHEN preferred_bidder_country_id = 873
                THEN 'PR'
            WHEN preferred_bidder_country_id = 892
                THEN 'TJ'
            WHEN preferred_bidder_country_id = 902
                THEN 'VA'
            WHEN preferred_bidder_country_id = 909
                THEN 'BB'
            WHEN preferred_bidder_country_id = 913
                THEN 'BW'
            WHEN preferred_bidder_country_id = 918
                THEN 'BI'
            WHEN preferred_bidder_country_id = 921
                THEN 'CF'
            WHEN preferred_bidder_country_id = 957
                THEN 'LA'
            WHEN preferred_bidder_country_id = 958
                THEN 'LV'
            WHEN preferred_bidder_country_id = 980
                THEN 'NA'
            WHEN preferred_bidder_country_id = 981
                THEN 'NC'
            WHEN preferred_bidder_country_id = 1005
                THEN 'TZ'
            WHEN preferred_bidder_country_id = 1007
                THEN 'TL'
            WHEN preferred_bidder_country_id = 1020
                THEN 'BZ'
            WHEN preferred_bidder_country_id = 1023
                THEN 'BR'
            WHEN preferred_bidder_country_id = 1030
                THEN 'CO'
            WHEN preferred_bidder_country_id = 1053
                THEN 'GD'
            WHEN preferred_bidder_country_id = 1056
                THEN 'GT'
            WHEN preferred_bidder_country_id = 1063
                THEN 'LB'
            WHEN preferred_bidder_country_id = 1071
                THEN 'MT'
            WHEN preferred_bidder_country_id = 1072
                THEN 'MQ'
            WHEN preferred_bidder_country_id = 1073
                THEN 'MR'
            WHEN preferred_bidder_country_id = 1111
                THEN 'UY'
            WHEN preferred_bidder_country_id = 1115
                THEN 'ZM'
            WHEN preferred_bidder_country_id = 1119
                THEN 'BM'
            WHEN preferred_bidder_country_id = 1123
                THEN 'BG'
            WHEN preferred_bidder_country_id = 1142
                THEN 'GF'
            WHEN preferred_bidder_country_id = 1146
                THEN 'GI'
            WHEN preferred_bidder_country_id = 1158
                THEN 'MK'
            WHEN preferred_bidder_country_id = 1183
                THEN 'SH'
            WHEN preferred_bidder_country_id = 1202
                THEN 'VE'
            WHEN preferred_bidder_country_id = 1207
                THEN 'BT'
            WHEN preferred_bidder_country_id = 1209
                THEN 'BN'
            WHEN preferred_bidder_country_id = 1210
                THEN 'CV'
            WHEN preferred_bidder_country_id = 1242
                THEN 'MG'
            WHEN preferred_bidder_country_id = 1245
                THEN 'MV'
            WHEN preferred_bidder_country_id = 1252
                THEN 'ME'
            WHEN preferred_bidder_country_id = 1253
                THEN 'MS'
            WHEN preferred_bidder_country_id = 1254
                THEN 'MA'
            WHEN preferred_bidder_country_id = 1277
                THEN 'TH'
            WHEN preferred_bidder_country_id = 1287
                THEN 'VG'
            WHEN preferred_bidder_country_id = 1288
                THEN 'TD'
            WHEN preferred_bidder_country_id = 1307
                THEN 'GM'
            WHEN preferred_bidder_country_id = 1308
                THEN 'GR'
            WHEN preferred_bidder_country_id = 1328
                THEN 'NE'
            WHEN preferred_bidder_country_id = 1330
                THEN 'NO'
            WHEN preferred_bidder_country_id = 1349
                THEN 'TG'
            WHEN preferred_bidder_country_id = 1358
                THEN 'CL'
            WHEN preferred_bidder_country_id = 1364
                THEN 'DK'
            WHEN preferred_bidder_country_id = 1377
                THEN 'GP'
            WHEN preferred_bidder_country_id = 1379
                THEN 'HK'
            WHEN preferred_bidder_country_id = 1395
                THEN 'NG'
            WHEN preferred_bidder_country_id = 1399
                THEN 'QA'
            WHEN preferred_bidder_country_id = 1418
                THEN 'VN'
            WHEN preferred_bidder_country_id = 1422
                THEN 'KM'
            WHEN preferred_bidder_country_id = 1431
                THEN 'GQ'
            WHEN preferred_bidder_country_id = 1434
                THEN 'FR'
            WHEN preferred_bidder_country_id = 1451
                THEN 'MD'
            WHEN preferred_bidder_country_id = 1452
                THEN 'MC'
            WHEN preferred_bidder_country_id = 1466
                THEN 'SG'
            WHEN preferred_bidder_country_id = 1479
                THEN 'CG'
            WHEN preferred_bidder_country_id = 1492
                THEN 'GL'
            WHEN preferred_bidder_country_id = 1512
                THEN 'LC'
            WHEN preferred_bidder_country_id = 1514
                THEN 'SA'
            WHEN preferred_bidder_country_id = 1531
                THEN 'CY'
            WHEN preferred_bidder_country_id = 1540
                THEN 'GU'
            WHEN preferred_bidder_country_id = 1547
                THEN 'MW'
            WHEN preferred_bidder_country_id = 1559
                THEN 'SM'
            WHEN preferred_bidder_country_id = 1568
                THEN 'TN'
            WHEN preferred_bidder_country_id = 1576
                THEN 'CZ'
            WHEN preferred_bidder_country_id = 1581
                THEN 'EE'
            WHEN preferred_bidder_country_id = 1596
                THEN 'PK'
            WHEN preferred_bidder_country_id = 1616
                THEN 'DJ'
            WHEN preferred_bidder_country_id = 1628
                THEN 'MY'
            WHEN preferred_bidder_country_id = 1646
                THEN 'UA'
            WHEN preferred_bidder_country_id = 1653
                THEN 'DO'
            WHEN preferred_bidder_country_id = 1669
                THEN 'PG'
            WHEN preferred_bidder_country_id = 1687
                THEN 'EC'
            WHEN preferred_bidder_country_id = 1688
                THEN 'FJ'
            WHEN preferred_bidder_country_id = 1701
                THEN 'PE'
            WHEN preferred_bidder_country_id = 1718
                THEN 'GA'
            WHEN preferred_bidder_country_id = 1725
                THEN 'ML'
            WHEN preferred_bidder_country_id = 1746
                THEN 'HU'
            WHEN preferred_bidder_country_id = 1756
                THEN 'RO'
            WHEN preferred_bidder_country_id = 1771
                THEN 'IN'
            WHEN preferred_bidder_country_id = 1776
                THEN 'MU'
            WHEN preferred_bidder_country_id = 1798
                THEN 'MX'
            WHEN preferred_bidder_country_id = 1799
                THEN 'MN'
            WHEN preferred_bidder_country_id = 1821
                THEN 'SN'
            WHEN preferred_bidder_country_id = 1859
                THEN 'SK'
            WHEN preferred_bidder_country_id = 1876
                THEN 'SI'
            WHEN preferred_bidder_country_id = 1877
                THEN 'SB'
            WHEN preferred_bidder_country_id = 1926
                THEN 'AM'
            WHEN preferred_bidder_country_id = 1927
                THEN 'AZ'
            WHEN preferred_bidder_country_id = 1940
                THEN 'CD'
            WHEN preferred_bidder_country_id = 1952
                THEN 'IR'
            WHEN preferred_bidder_country_id = 1963
                THEN 'IQ'
            WHEN preferred_bidder_country_id = 1973
                THEN 'CI'
            WHEN preferred_bidder_country_id = 1981
                THEN 'ZW'
            WHEN preferred_bidder_country_id = 1982
                THEN 'LR'
            WHEN preferred_bidder_country_id = 1989
                THEN 'MM'
            WHEN preferred_bidder_country_id = 1995
                THEN 'RW'
            WHEN preferred_bidder_country_id = 2000
                THEN 'SL'
            WHEN preferred_bidder_country_id = 2004
                THEN 'SO'
            WHEN preferred_bidder_country_id = 2007
                THEN 'SS'
            WHEN preferred_bidder_country_id = 2007
                THEN 'SD'
            WHEN preferred_bidder_country_id = 2009
                THEN 'UZ'
            WHEN preferred_bidder_country_id = 2217
                THEN 'AE'
            WHEN preferred_bidder_country_id = 2218
                THEN 'EH'
            WHEN preferred_bidder_country_id = 2219
                THEN 'KN'
            WHEN preferred_bidder_country_id = 2220
                THEN 'SJ'
            WHEN preferred_bidder_country_id = 2221
                THEN 'SZ'
            END;
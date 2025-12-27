package com.gdn.aggregate.platform.module.product.listener.constants;

public interface PurchasedTypeMap {

  String TRUE_TRUE_TRUE_TRUE = PurchasedType.ONLINE_CNC;          //4000+100+25+50+10 = 4185
  String TRUE_TRUE_TRUE_FALSE = PurchasedType.ONLINE_CNC;         //4000+100+25+50+ 0 = 4175
  String TRUE_FALSE_TRUE_TRUE = PurchasedType.ONLINE_CNC;         //4000+100+ 0+50+10 = 4160
  String TRUE_FALSE_TRUE_FALSE = PurchasedType.ONLINE_CNC;        //4000+100+ 0+50+ 0 = 4150

  String TRUE_TRUE_FALSE_TRUE = PurchasedType.ONLINE;             //3000+100+25+ 0+10 = 3135
  String TRUE_TRUE_FALSE_FALSE = PurchasedType.ONLINE;            //3000+100+25+ 0+ 0 = 3125
  String TRUE_FALSE_FALSE_TRUE = PurchasedType.ONLINE;            //3000+100+ 0+ 0+10 = 3110
  String TRUE_FALSE_FALSE_FALSE = PurchasedType.ONLINE;           //3000+100+ 0+ 0+ 0 = 3100

  String FALSE_TRUE_TRUE_TRUE = PurchasedType.CNC;                //2000+  0+25+50+10 = 2085
  String FALSE_TRUE_TRUE_FALSE = PurchasedType.CNC;               //2000+  0+25+50+ 0 = 2075
  String FALSE_FALSE_TRUE_TRUE = PurchasedType.CNC;               //2000+  0+ 0+50+10 = 2060
  String FALSE_FALSE_TRUE_FALSE = PurchasedType.CNC;              //2000+  0+ 0+50+ 0 = 2050

  String FALSE_TRUE_FALSE_TRUE = PurchasedType.NOT_AVAILABLE;     //1400+  0+25+ 0+10 = 1435
  String FALSE_FALSE_FALSE_TRUE = PurchasedType.NOT_AVAILABLE;    //1400+  0+ 0+ 0+10 = 1410
  String FALSE_TRUE_FALSE_FALSE = PurchasedType.NOT_AVAILABLE;    //1300+  0+25+ 0+ 0 = 1325
  String FALSE_FALSE_FALSE_FALSE = PurchasedType.NOT_AVAILABLE;   //1000+  0+ 0+ 0+ 0 = 1000

}

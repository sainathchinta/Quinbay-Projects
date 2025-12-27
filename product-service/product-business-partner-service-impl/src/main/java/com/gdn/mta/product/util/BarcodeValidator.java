package com.gdn.mta.product.util;

public final class BarcodeValidator {

  public static Boolean validateEan13(String ean13) throws Exception {
    int[] ean13Items = new int[13];
    for (int i = 0; i < ean13Items.length; i++) {
      ean13Items[i] = Integer.valueOf("" + ean13.charAt(i));
    }
    int oddSum = 0;
    int evenSum = 0;
    for (int i = 0; i < ean13Items.length - 1; i++) {
      if (i % 2 == 0) {
        oddSum += ean13Items[i];
      } else {
        evenSum += ean13Items[i];
      }
    }
    int e = 10 - (((oddSum * 1) + (evenSum * 3)) % 10);
    return ean13Items[12] == e;
  }

}

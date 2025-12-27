package com.gdn.mta.product.util;

import java.security.SecureRandom;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.List;

public final class BarcodeGenerator {

  private static final int UPPER_BOUND = 10;
  private static final int UPC_CODE_LENGTH = 17;
  public static String generateBan17() {
    SimpleDateFormat simpleDateFormat = new SimpleDateFormat("yyMMddHHmmssSS");
    String date = simpleDateFormat.format(Calendar.getInstance().getTime());
    int[] ban17Items = new int[17];
    ban17Items[0] = 0;
    ban17Items[1] = 1;
    for (int i = 0; i < date.length() - 1; i++) {
      ban17Items[i + 2] = Integer.valueOf("" + date.charAt(i));
    }
    int oddSum = 0;
    int evenSum = 0;
    for (int i = 0; i < ban17Items.length - 1; i++) {
      if (i % 2 == 0) {
        oddSum += ban17Items[i];
      } else {
        evenSum += ban17Items[i];
      }
    }
    ban17Items[16] = 10 - (((oddSum * 1) + (evenSum * 3)) % 10);
    String ban17 = "";
    for (int ban17Item : ban17Items) {
      ban17 += ban17Item;
    }
    return ban17;
  }

  /**
   * generate upc code
   * @return
   * @throws Exception
   */
  public static String generateUPCCode() {
    StringBuilder upcCode = new StringBuilder();
    upcCode.append(System.currentTimeMillis());
    SecureRandom random = new SecureRandom();
    while(UPC_CODE_LENGTH != upcCode.length()) {
      upcCode.append(random.nextInt(UPPER_BOUND));
    }
    return upcCode.toString();
  }

  public static boolean isValidUPCCode(String upcCode, List<Integer> eanUpcValidLength) {
    try {
      Long.parseLong(upcCode);
    } catch (NumberFormatException e) {
      return false;
    }
    int length = upcCode.length();
    return eanUpcValidLength.contains(length);
  }

  public static boolean isValidUomUPCCode(String upcCode) {
    boolean allDigits = upcCode.chars().allMatch(Character::isDigit);
    if (!allDigits) {
      return false;
    }
    return upcCode.length() <= 255;
  }
}

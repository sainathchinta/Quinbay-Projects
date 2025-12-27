package com.gdn.mta.product.service.util;

import org.springframework.stereotype.Component;

@Component
public class ProductGdnSkuGeneratorUtilBean implements ProductGdnSkuGeneratorUtil {

  private static final String DASH = "-";

  private String appendWithHeadingZero(Number number, int totalDigit) {
    String formatter = "%0" + totalDigit + "d";
    return String.format(formatter, number);
  }

  @Override
  public String appendWithSerial(String base, Number number, int trailingLength) {
    StringBuilder sb = new StringBuilder();
    sb.append(base);
    sb.append(DASH);
    sb.append(this.appendWithHeadingZero(number, trailingLength));
    return sb.toString();
  }

}

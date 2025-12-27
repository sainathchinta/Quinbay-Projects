package com.gdn.x.product.service.util;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;

public class ItemHelperUtil {

  private static final String COMMA_DELIMITER = ",";

  private ItemHelperUtil() {
  }

  public static List<String> combinePickupPointCodeAndPickupPointCodesFilter(
      String pickupPointCodeFilter, List<String> pickupPointCodesFilter) {
    List<String> filter = Objects.isNull(pickupPointCodesFilter) ?
        new ArrayList<>() : new ArrayList<>(pickupPointCodesFilter);
    if (Objects.nonNull(pickupPointCodeFilter)) {
      if (pickupPointCodeFilter.contains(COMMA_DELIMITER)) {
        filter.addAll(Arrays.asList(pickupPointCodeFilter.split(COMMA_DELIMITER)));
      } else {
        filter.add(pickupPointCodeFilter);
      }
    }
    return filter;
  }
}

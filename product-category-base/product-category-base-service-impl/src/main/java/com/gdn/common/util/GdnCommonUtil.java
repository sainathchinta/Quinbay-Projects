package com.gdn.common.util;

import java.util.Collection;
import java.util.Iterator;

public final class GdnCommonUtil {
  private GdnCommonUtil() {}

  public static String generateStringFromStringCollection(Collection<String> collection) {
    final StringBuilder result = new StringBuilder();
    final Iterator<String> itertator = collection.iterator();
    while (itertator.hasNext()) {
      result.append(itertator.next());
      if (itertator.hasNext())
        result.append(", ");
    }
    return result.toString();
  }
}

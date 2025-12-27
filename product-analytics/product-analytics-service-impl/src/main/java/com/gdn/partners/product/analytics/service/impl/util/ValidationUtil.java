package com.gdn.partners.product.analytics.service.impl.util;

import com.gdn.partners.product.analytics.service.impl.exception.ValidationException;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import javax.annotation.Nullable;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

@Slf4j
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class ValidationUtil {

  private static final String PRODUCT_CODE_PATTERN = "^[A-Z0-9]{1,}-[0-9]{5,}$";

  public static boolean isProductCode(String productCode) {
    Pattern pattern = Pattern.compile(PRODUCT_CODE_PATTERN);
    Matcher matcher = pattern.matcher(productCode);
    return matcher.matches();
  }

  public static void checkParameter(boolean expression, @Nullable Object errorMessage) {
    if (!expression) {
      throw new ValidationException(String.valueOf(errorMessage));
    }
  }
}

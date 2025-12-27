package com.gdn.mta.bulk.helper;

import org.apache.commons.lang3.StringUtils;

import com.gdn.mta.bulk.BulkProcessValidationErrorMessages;

public class ProductLevel3ProcessorValidationMessageHelper {
  public static String errorMessage(String prependString, String errorMessageForInternationalMerchant,
      String appendString) {
    if (StringUtils.isNotBlank(prependString) && StringUtils.isNotBlank(appendString)) {
      return errorMessageWithPrependAndAppendString(prependString, errorMessageForInternationalMerchant, appendString);

    } else if (StringUtils.isBlank(prependString) && StringUtils.isNotBlank(appendString)) {

      return errorMessageWithAppendedString(appendString, errorMessageForInternationalMerchant);

    } else if (StringUtils.isNotBlank(prependString) && StringUtils.isBlank(appendString)) {

      return errorMessageWithPrependString(prependString, errorMessageForInternationalMerchant);
    }
    return errorMessageForInternationalMerchant;
  }

  private static String errorMessageWithPrependString(String prependString, String errorMessage) {
    return prependString.concat(errorMessage);
  }

  private static String errorMessageWithAppendedString(String appendString, String errorMessage) {
    return errorMessage.concat(appendString);
  }

  private static String errorMessageWithPrependAndAppendString(String prependString, String errorMessage,
      String appendString) {
    return prependString.concat(errorMessage).concat(appendString);
  }

  public static String errorMessageBasedOnMerchant(boolean isInternationMerchant, String errorMessageEN,
      String errorMessage) {
    if (isInternationMerchant) {
      return errorMessageEN;
    } else {
      return errorMessage;
    }
  }
}

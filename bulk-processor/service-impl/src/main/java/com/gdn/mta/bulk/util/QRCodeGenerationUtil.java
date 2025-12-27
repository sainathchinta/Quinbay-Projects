package com.gdn.mta.bulk.util;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.models.ExcelHeaderNames;
import com.google.common.collect.ImmutableList;
import org.apache.commons.lang.StringUtils;

import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

import static com.gdn.partners.bulk.util.Constant.LOCALE_COUNTRY;
import static com.gdn.partners.bulk.util.Constant.LOCALE_LANGUAGE;

public class QRCodeGenerationUtil {

  private static final String PRODUCT = "PRODUCT";
  private static final String ITEM = "ITEM";
  private static final String ITEM_PICKUP_POINT = "ITEM_PICKUP_POINT";
  private static final String ADD_TO_BAG = "ADD_TO_BAG";

  public static void validateHeadersForExcelUpload(Map<Integer, String> headersFromExcel,
    String qrGenerationType, boolean isCncActivated) {
    List<String> headers = new ArrayList<>(headersFromExcel.values());
    boolean headerMismatch = false;
    if (!isCncActivated && headers.contains(ExcelHeaderNames.FULFILLMENT_TYPE)) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, "Headers mismatch");
    }
    if (PRODUCT.equals(qrGenerationType)) {
      headerMismatch = !headers.contains(ExcelHeaderNames.PRODUCT_SKU);
    } else if (ITEM.equals(qrGenerationType)) {
      headerMismatch = !headers.contains(ExcelHeaderNames.ITEM_SKU);
    } else if (ITEM_PICKUP_POINT.equals(qrGenerationType) || ADD_TO_BAG.equals(qrGenerationType)) {
      headerMismatch = !headers.contains(ExcelHeaderNames.ITEM_SKU) || !headers.contains(
        ExcelHeaderNames.PICKUP_POINT_CODE_EN);
    }
    if (headerMismatch) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, "Headers mismatch");
    }
  }

  public static String convertPriceToIndonesianFormat(Double price) {
    Locale indonesianLocale = new Locale(LOCALE_LANGUAGE, LOCALE_COUNTRY);
    NumberFormat formatter = NumberFormat.getCurrencyInstance(indonesianLocale);
    formatter.setMaximumFractionDigits(0);
    return Optional.ofNullable(price).map(formatter::format).orElse(StringUtils.EMPTY);
  }
}

package com.gdn.partners.bulk.util;

import java.util.Arrays;
import java.util.Collection;

public interface SalesCategoryUpdateConstants {
  String PRODUCT_SKU = "Product SKU";
  String OPERATION_TYPE = "Copy = 1, Delete = 0";
  String CATEGORY_CODE = "Category code (Cn)";
  Collection<String> TEMPLATE_HEADER =
      Arrays.asList(PRODUCT_SKU, OPERATION_TYPE, CATEGORY_CODE);
  String PRODUCT_SKU_OPERATION_TYPE_CATGEORY_CODE_EMPTY = "Product SKU or Operation Type or Category Code is empty";
  String INVALID_OPERATION_TYPE = "Invalid operation type";
  int SALES_CATEGORY_UPDATE_HEADER_SIZE = 4;
}

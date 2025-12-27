package com.gdn.partners.bulk.util;

import java.util.Arrays;
import java.util.Collection;
import java.util.Map;

import com.gdn.mta.bulk.BulkProcessValidationErrorMessages;
import com.google.common.collect.ImmutableMap;

public interface BulkWorkOrderConstants {

  String ASSEMBLY_BULK_PROCESS_TYPE = "ASSEMBLY_REQUEST";
  String DISASSEMBLY_BULK_PROCESS_TYPE = "DISASSEMBLY_REQUEST";
  String TRANSFER_BULK_PROCESS_TYPE = "TRANSFER_REQUEST";
  String INVALID_WORK_ORDER_BULK_PROCESS_TYPE = "Invalid Bulk Process Type";
  String HEADER_MISMATCH = "Header Mismatch : Expected  Excel Headers :  %s";
  String HEADER_MISMATCH_CONSTANT = "Header Mismatch";
  String ITEM_SKU = "Item SKU";
  String ITEM_NAME = "Item Name (Optional)";
  String WAREHOUSE = "Warehouse (dropdown)";
  String STOCK = "Stock";
  String BUNDLING_TYPE = "Bundling Type (dropdown)";
  String CHILD_SKUS = "Child SKUs (comma separated)";
  String COGS = "COGS %(comma separated)";
  String SOURCE_ITEM_SKU = "Source item SKU";
  String SOURCE_ITEM_NAME = "Source Item Name (Optional)";
  String DESTINATION_ITEM_SKU = "Destination Item SKU";
  String DESTINATION_ITEM_NAME = "Destination Item Name (Optional)";
  String PHYSICAL_BUNDLING_TYPE = "Physical";
  String VIRTUAL_BUNDLING_TYPE = "Virtual";

  String FAILURE_REASON = "Failure Reason";
  Collection<String> ASSEMBLY_HEADERS = Arrays.asList(ITEM_SKU, ITEM_NAME, WAREHOUSE, STOCK, BUNDLING_TYPE);
  Collection<String> DISASSEMBLY_HEADERS = Arrays.asList(ITEM_SKU, ITEM_NAME, WAREHOUSE, STOCK, CHILD_SKUS, COGS);
  Collection<String> TRANSFER_REQUEST_HEADERS =
      Arrays.asList(SOURCE_ITEM_SKU, SOURCE_ITEM_NAME, DESTINATION_ITEM_SKU, DESTINATION_ITEM_NAME, WAREHOUSE, STOCK);
  Collection<String> BUNDLING_TYPES = Arrays.asList(PHYSICAL_BUNDLING_TYPE, VIRTUAL_BUNDLING_TYPE);

  String SOURCE_AND_DESTINATION_ITEM_SKU_CANNOT_BE_SAME = "Source SKU %s and destination SKU %s cannot be same";
  String NOT_ELIGIBLE_FOR_WORK_ORDER = "Seller not eligible for work order request";

  Map<Boolean, String> WAREHOUSE_CODE_INVALID =
      ImmutableMap.of(true, BulkProcessValidationErrorMessages.WAREHOUSE_CODE_INVALID_EN, false,
          BulkProcessValidationErrorMessages.WAREHOUSE_CODE_INVALID_ID);
  Map<Boolean, String> ITEM_SKU_INVALID =
      ImmutableMap.of(true, BulkProcessValidationErrorMessages.ITEM_NOT_FOUND_EN, false,
          BulkProcessValidationErrorMessages.ITEM_NOT_FOUND_ID);
  Map<Boolean, String> SOURCE_ITEM_INVALID =
      ImmutableMap.of(true, BulkProcessValidationErrorMessages.SOURCE_ITEM_NOT_FOUND_EN, false,
          BulkProcessValidationErrorMessages.SOURCE_ITEM_NOT_FOUND_ID);
  Map<Boolean, String> DESTINATION_ITEM_INVALID =
      ImmutableMap.of(true, BulkProcessValidationErrorMessages.DESTINATION_ITEM_NOT_FOUND_EN, false,
          BulkProcessValidationErrorMessages.DESTINATION_ITEM_NOT_FOUND_ID);
  Map<Boolean, String> NOT_A_BUNDLE_PRODUCT =
      ImmutableMap.of(true, BulkProcessValidationErrorMessages.NOT_A_BUNDLE_PRODUCT_EN, false,
          BulkProcessValidationErrorMessages.NOT_A_BUNDLE_PRODUCT_ID);
  Map<Boolean, String> CHILD_SKU_INVALID =
      ImmutableMap.of(true, BulkProcessValidationErrorMessages.CHILD_SKU_INVALID_EN, false,
          BulkProcessValidationErrorMessages.CHILD_SKU_INVALID_ID);
  Map<Boolean, String> STOCK_INVALID = ImmutableMap
      .of(true, BulkProcessValidationErrorMessages.MINIMUM_STOCK_1_EN, false,
          BulkProcessValidationErrorMessages.MINIMUM_STOCK_1_ID);
  Map<Boolean, String> STOCK_NOT_INTEGER = ImmutableMap
      .of(true, BulkProcessValidationErrorMessages.STOCK_INVALID_EN, false,
          BulkProcessValidationErrorMessages.STOCK_INVALID);
  Map<Boolean, String> BUNDLING_TYPE_INVALID = ImmutableMap
      .of(true, BulkProcessValidationErrorMessages.BUNDLING_TYPE_INVALID_EN, false,
          BulkProcessValidationErrorMessages.BUNDLING_TYPE_INVALID_ID);

  Map<Boolean, String> COGS_INVALID = ImmutableMap
      .of(true, BulkProcessValidationErrorMessages.COGS_MUST_MATCH_CHILD_SKU_EN, false,
          BulkProcessValidationErrorMessages.COGS_MUST_MATCH_CHILD_SKU_ID);

  Map<Boolean, String> COGS_NOT_SUM_TO_100 = ImmutableMap
      .of(true, BulkProcessValidationErrorMessages.COGS_SUM_TO_100_EN, false,
          BulkProcessValidationErrorMessages.COGS_SUM_TO_100_ID);
}

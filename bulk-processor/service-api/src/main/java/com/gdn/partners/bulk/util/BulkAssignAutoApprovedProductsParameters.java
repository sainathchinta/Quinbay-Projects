package com.gdn.partners.bulk.util;

import java.util.Arrays;
import java.util.Collection;

public interface BulkAssignAutoApprovedProductsParameters {
  String PRODUCT_CODE = "Kode Produk";
  String PRODUCT_NAME = "Nama Produk";
  String CATEGORY_NAME = "Kategori";
  String STORE_NAME = "Nama Toko";

  String PRODUCT_NAME_OPTIONAL = "Nama Produk (opsional)";
  String CATEGORY_NAME_OPTIONAL = "Kategori (opsional)";
  String STORE_NAME_OPTIONAL = "Nama Toko (opsional)";
  String ASSIGNEE = "Produk Assignee";
  String REVIEWERS = "REVIEWERS";
  Collection<String> BULK_ASSIGN_AUTO_APPROVED_PRODUCTS_HEADERS =
      Arrays.asList(PRODUCT_CODE, PRODUCT_NAME, CATEGORY_NAME, STORE_NAME, ASSIGNEE);

  Collection<String> BULK_ASSIGN_AUTO_APPROVED_PRODUCTS_OPTIONAL_HEADERS =
      Arrays.asList(PRODUCT_CODE, PRODUCT_NAME_OPTIONAL, CATEGORY_NAME_OPTIONAL, STORE_NAME_OPTIONAL, ASSIGNEE);
}

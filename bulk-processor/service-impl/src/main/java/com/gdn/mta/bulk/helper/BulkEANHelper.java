package com.gdn.mta.bulk.helper;

import java.util.ArrayList;
import java.util.List;

import com.gdn.mta.bulk.models.download.responsedata.BulkProductEANResponse;


public class BulkEANHelper {
  private BulkEANHelper() {

  }

  public static List<String> toProductValues(BulkProductEANResponse product) {
    List<String> productData = new ArrayList<>();
    productData.add(product.getProductSku());
    productData.add(product.getProductName());
    productData.add(product.getItemSku());
    productData.add(product.getItemName());
    productData.add(product.getUpcCode());
    return productData;
  }

  public static List<List<String>> getAllProductValues(List<BulkProductEANResponse> products) {
    return products.stream()
        .map(BulkEANHelper::toProductValues)
        .toList();
  }
}

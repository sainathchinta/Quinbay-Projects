package com.gdn.x.productcategorybase.solr.thread;

import com.gdn.x.productcategorybase.solr.service.ProductService;

import java.util.Map;

/**
 * Created by Kesha on 25/05/16.
 */
public class ProductThreadExecutor {
  private ProductService productService;
  private Map<String, String> categoryToFinalParentMap;


  public ProductThreadExecutor(ProductService productService, Map<String, String>
      categoryToFinalParentMap) {
    this.productService = productService;
    this.categoryToFinalParentMap = categoryToFinalParentMap;
  }

  public ProductService getProductService() {
    return productService;
  }

  public Map<String, String> getCategoryToFinalParentMap() {
    return categoryToFinalParentMap;
  }

  public void cleanup() {
    this.categoryToFinalParentMap = null;
  }
}

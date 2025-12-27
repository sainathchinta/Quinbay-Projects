package com.gdn.x.productcategorybase.service;

public interface AggregateService {

  /**
   * method for publish all master products
   * @param storeId must not be blank
   * @param startPage must not be blank
   */
  void publishPageOfProducts(String storeId, int startPage);

  /**
   * method for publish all master product categories
   * @param storeId must not be blank
   * @param startPage must not be blank
   */
  void publishPageOfProductCategories(String storeId, int startPage);

  /**
   * method for publish all master product attributes
   * @param storeId must not be blank
   * @param startPage must not be blank
   */
  void publishPageOfProductAttributes(String storeId, int startPage);

  /**
   * method for publish all master product images
   * @param storeId must not be blank
   * @param startPage must not be blank
   */
  void publishPageOfImages(String storeId, int startPage);

  /**
   * method for publish all master product items
   * @param storeId must not be blank
   * @param startPage must not be blank
   */
  void publishPageOfProductItems(String storeId, int startPage);

}

package com.gdn.x.productcategorybase;

public interface AggregateCommandDesc {

  String PRODUCT_CONTROLLER = "publishAllProducts";
  String PRODUCT_CATEGORY_CONTROLLER = "publishAllProductCategories";
  String PRODUCT_ATTRIBUTE_CONTROLLER = "publishAllProductAttributes";
  String IMAGE_CONTROLLER = "publishAllProductImages";
  String PRODUCT_ITEM_CONTROLLER = "publishAllProductItems";
  String PRODUCT = "publishPageOfProducts";
  String PRODUCT_CATEGORY = "publishPageOfProductCategories";
  String PRODUCT_ATTRIBUTE = "publishPageOfProductAttributes";
  String IMAGE = "publishPageOfImages";
  String PRODUCT_ITEM = "publishPageOfProductItems";

}

package com.gdn.x.productcategorybase;

public interface CacheNames {

  String BASE_PATH = "com.gdn.x:productcategorybase:";
  String CATEGORY_HIERARCHY_CACHE = CacheNames.BASE_PATH + "categoryhierarchy";
  String CHILD_CATEGORY_CACHE = CacheNames.BASE_PATH + "childcategories";
  String CATEGORIES_BY_CATALOGTYPE =
      CacheNames.BASE_PATH + "catalogtype-to-categories";
  String CATEGORY_DETAIL = CacheNames.BASE_PATH + "categorydetail";
  String BRAND = CacheNames.BASE_PATH + "brand";
  String ACTIVE_CATEGORY_TREE = CacheNames.BASE_PATH + "activeCategoryTree";
  String PRODUCT_CACHE = CacheNames.BASE_PATH + "product";
  String PRODUCT_ITEMS_CACHE = CacheNames.BASE_PATH + "productItems";
  String PRODUCT_CATEGORIES_CACHE = CacheNames.BASE_PATH + "productCategories";
  String PRODUCT_ATTRIBUTES_CACHE = CacheNames.BASE_PATH + "productAttributes";
  String CATEGORY_CACHE = CacheNames.BASE_PATH + "category";
  String PRODUCT_IMAGES_CACHE = CacheNames.BASE_PATH + "productImages";
  String PRODUCT_ITEM_ATTRIBUTE_VALUES_CACHE = CacheNames.BASE_PATH + "productItemAttributeValues";
  String PRODUCT_ITEM_IMAGES_CACHE = CacheNames.BASE_PATH + "productItemImages";
  String ATTRIBUTE_CACHE = CacheNames.BASE_PATH + "attribute";
  String ALLOWED_ATTRIBUTE_VALUES_CACHE = CacheNames.BASE_PATH + "allowedAttributeValues";
  String PREDEFINED_ALLOWED_ATTRIBUTE_VALUES_CACHE = CacheNames.BASE_PATH + "predefinedAllowedAttributeValues";
  String RESTRICTED_KEYWORDS_CACHE = CacheNames.BASE_PATH + "restrictedKeyword";
  String LOOKUP_CACHE = CacheNames.BASE_PATH + "lookup";
  String GENERIC_CATEGORY_TREE = CacheNames.BASE_PATH + "genericTemplateCategories";
  String SYSTEM_PARAMETER_SWITCHES = BASE_PATH + "systemParameterSwitches";
  String SYSTEM_PARAMETER = BASE_PATH + "systemParameter";
  String BRAND_AUTHORISATION = BASE_PATH + "brandAuthorisation";
  String PROTECTED_BRANDS = BASE_PATH + "protectedBrands";
  String IN_REVIEW_BRANDS = BASE_PATH + "inReviewBrands";
  String ACTIVE_CHILD_CATEGORY_CACHE = CacheNames.BASE_PATH + "activeChildcategories";;
  String BRAND_AUTHORISATION_WIP = BASE_PATH + "brandAuthorisationWip";
}

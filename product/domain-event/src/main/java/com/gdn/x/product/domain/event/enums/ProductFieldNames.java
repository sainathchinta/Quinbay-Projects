package com.gdn.x.product.domain.event.enums;

public interface ProductFieldNames {

  // product
  public static final String MASTER_DATA_PRODUCT = "masterDataProduct";
  public static final String PRODUCT_TYPE = "productType";
  public static final String SETTLEMENT_TYPE = "settlementType";
  public static final String MERCHANT_CODE = "merchantCode";
  public static final String MASTER_CATALOG = "masterCatalog";
  public static final String SALES_CATALOGS = "salesCatalogs";
  public static final String PRODUCT_CATENTRY_ID = "productCatentryId";
  public static final String PRODUCT_CATALOG_ID = "productCatalogId";
  public static final String ITEM_DETAIL = "itemDetail";
  public static final String ORIGIN_ID = "orginId";
  public static final String ITEM_VIEW_CONFIGS = "itemViewConfigs";
  public static final String PRODUCT_ATTRIBUTES = "productAttributes";

  // master data product
  public static final String SPECIFICATION_DETAIL = "specificationDetail";
  public static final String WEIGHT = "weight";
  public static final String HEIGHT = "height";
  public static final String WIDTH = "width";
  public static final String LENGTH = "length";
  public static final String PRODUCT_ITEMS = "productItems";
  public static final String URL = "url";
  public static final String PRODUCT_NAME = "productName";
  public static final String LONG_DESCRIPTION = "longDescription";
  public static final String UNIQUE_SELLING_POINT = "uniqueSellingPoint";
  public static final String PRODUCT_STORY = "productStory";
  public static final String UOM = "uom";
  public static final String MASTER_DATA_PRODUCT_IMAGES = "masterDataProductImages";
  public static final String MASTER_DATA_PRODUCT_ATTRIBUTES = "masterDataProductAttributes";
  public static final String BRAND = "brand";

  // item
  public static final String MERCHANT_SKU = "merchantSku";
  public static final String ITEM_ATTRIBUTE_VALUES = "itemAttributeValues";
  public static final String INVENTORY_TYPE = "inventoryType";
  public static final String DANGEROUS_LEVEL = "dangerousLevel";
  public static final String ITEM_CATENTRY_ID = "itemCatentryId";
  public static final String ITEM_CATGROUP_ID = "itemCatgroupId";
  public static final String MASTER_DATA_ITEM = "masterDataItem";
  public static final String PRICE = "price";
  public static final String PICKUP_POINT_CODE = "pickupPointCode";
  public static final String IS_LATE_FULFILLMENT = "isLatefulfillment";

  // master data item
  public static final String GENERATED_ITEM_NAME = "generatedItemName";
  public static final String UPC_CODE = "upcCode";
  public static final String SKU_CODE = "skuCode";
  public static final String HASH = "hash";
  public static final String MASTER_DATA_ITEM_ATTRIBUTE_VALUES = "masterDataItemAttributeValues";
  public static final String MASTER_DATA_ITEM_IMAGES = "masterDataItemImages";

  // price
  public static final String CURRENCY = "currency";
  public static final String OFFER_PRICE = "offerPrice";
  public static final String LIST_PRICE = "listPrice";

  // price, sale price
  public static final String DISCOUNT_PRICE = "discountPrice";

  // master data item
  public static final String ITEM_DELIVERY_WEIGHT = "itemDeliveryWeight";
  public static final String ITEM_HEIGHT = "itemHeight";
  public static final String ITEM_LENGTH = "itemLength";
  public static final String ITEM_WEIGHT = "itemWeight";
  public static final String ITEM_WIDTH = "itemWidth";

  // product item attribute value, system parameter
  public static final String VALUE = "value";

  // item, product item attribute value
  public static final String ITEM_CODE = "itemCode";

  // product item image
  public static final String PRODUCT_ITEM_ID = "productItemId";

  // master catalog
  public static final String CATEGORY = "category";

  // sales catalog
  public static final String LIST_OF_CATEGORIES = "listOfCategories";

  // special attribute, attribute
  public static final String ATTRIBUTE_VALUE = "attributeValue";

  // product view config
  public static final String ITEM_DISPLAYABLE_SCHEDULES = "itemDisplayableSchedules";
  public static final String ITEM_DISCOVERABLE_SCHEDULES = "itemDiscoverableSchedules";

  // product image, product item image
  public static final String IS_MAIN_IMAGE = "isMainImage";
  public static final String LOCATION_PATH = "locationPath";

  // product attribute
  public static final String IS_OWNED_BY_PRODUCT_ITEM = "isOwnedByProductItem";
  public static final String MASTER_DATA_PRODUCT_ATTRIBUTE_VALUES =
      "masterDataProductAttributeValues";

  // master catalog, sales catalog, catalog
  public static final String PRODUCT_SPECIAL_ATTRIBUTES = "productSpecialAttributes";
  public static final String CATALOG_CODE = "catalogCode";

  // product image, product attribute, category, product item image
  public static final String SEQUENCE = "sequence";

  // product display schedule, product view config
  public static final String IS_BUYABLE = "isBuyable";

  // product discoverable, product view config
  public static final String IS_DISCOVERABLE = "isDiscoverable";

  // product, product view config, item
  public static final String PRODUCT_SKU = "productSku";

  // product discoverable schedule, product displayable schedule, sale price
  public static final String START_DATE_TIME = "startDateTime";
  public static final String END_DATE_TIME = "endDateTime";

  // master data product, product, product image, product attribute
  public static final String PRODUCT_CODE = "productCode";

  // product, item
  public static final String IS_SYNCHRONIZED = "isSynchronized";

  // master data product, item
  public static final String SHIPPING_WEIGHT = "shippingWeight";

  // master data product, master data item, category
  public static final String IS_VIEWABLE = "isViewable";

  // product view config, price
  public static final String CHANNEL = "channel";

  // item, category, master data product, master data item
  public static final String IS_ACTIVATED = "isActivated";

  // product item attribute value, special attribute, product attribute, attribute
  public static final String ATTRIBUTE_CODE = "attributeCode";

  // master data product, system parameter
  public static final String DESCRIPTION = "description";

  // system parameter, gdnBaseMongoEntity
  public static final String MARK_FOR_DELETE = "markForDelete";
  public static final String UPDATED_DATE = "updatedDate";
  public static final String CREATED_DATE = "createdDate";
  public static final String VERSION = "version";
  public static final String STORE_ID = "storeId";
  public static final String CREATED_BY = "createdBy";
  public static final String UPDATED_BY = "updatedBy";
  public static final String ID = "id";
  public static final String VARIABLE = "variable";
  public static final String SYSTEM_PARAMETER = "product_system_parameter";
  public static final String SYSTEM_PARAMETER_INDEX = "product_system_parameter_index";
  public static final String SYSTEM_PARAMETER_HISTORY = "product_system_parameter_history";
  public static final String SYSTEM_PARAMETER_HANDLING_FEE =
      "product_system_parameter_handling_fee";

  // product attribute value
  public static final String DESCRIPTIVE_ATTRIBUTE_VALUE = "descriptiveAttributeValue";
  public static final String DESCRIPTIVE_ATTRIBUTE_VALUE_TYPE = "descriptiveAttributeValueType";
  public static final String ALLOWED_ATTRIBUTE_VALUE_CODE = "allowedAttributeValueCode";
  public static final String PRODUCT_ATTRIBUTE_CODE = "productAttributeCode";
  public static final String PREDEFINED_ALLOWED_ATTRIBUTE_VALUE_CODE =
      "predefinedAllowedAttributeValueCode";

  // category
  public static final String CATGROUP_ID = "catgroupId";
  public static final String CATEGORY_CODE = "categoryCode";

  // Attribute
  public static final String PRODUCT_ATTRIBUTE_DETAILS = "productAttributeDetails";

  // item, attribute
  public static final String ITEM_SKU = "itemSku";

  // master data attribute
  public static final String ATTRIBUTE_TYPE = "attributeType";
  public static final String MANDATORY = "mandatory";
  public static final String ATTRIBUTE_NAME = "attributeName";
  public static final String IS_SEARCHABLE = "isSearchable";
  public static final String EXAMPLE = "example";
  public static final String IS_SKU_VALUE = "isSkuValue";
  public static final String MASTER_DATA_ALLOWED_ATTRIBUTE_VALUES =
      "masterDataAllowedAttributeValues";
  public static final String MASTER_DATA_PREDEFINED_ALLOWED_ATTRIBUTE_VALUES =
      "masterDataPredefinedAllowedAttributeValues";

  // item attribute value, product attribute
  public static final String MASTER_DATA_ATTRIBUTE = "masterDataAttribute";

  // ticket template attribute
  public static final String TICKET_TEMPLATE_CODE = "ticketTemplateCode";
  public static final String NAME = "name";
  public static final String FROM = "from";
  public static final String CC = "cc";
  public static final String BCC = "bcc";
  public static final String ACTIVE = "active";
  public static final String SUBJECT_ORDER_RECEIVED = "subjectOrderReceived";
  public static final String SUBJECT_ORDER_PAID = "subjectOrderPaid";
  public static final String SUBJECT_BARCODE = "subjectBarcode";
  public static final String TEMPLATE_ORDER_RECEIVED = "templateOrderReceived";
  public static final String TEMPLATE_ORDER_PAID = "templateOrderPaid";
  public static final String TEMPLATE_BARCODE = "templateBarcode";
  public static final String BILLINGUAL = "bilingual";
  public static final String TEMPLATE_BARCODE_HEADING_TEXT = "templateBarcodeHeadingText";
  public static final String TEMPLATE_BARCODE_PROCESS_TITLE = "templateBarcodeProcessTitle";
  public static final String TEMPLATE_BARCODE_PROCESS_TEXT = "templateBarcodeProcessText";
  public static final String TEMPLATE_ORDER_PROCESS_TITLE = "templateOrderProcessTitle";
  public static final String TEMPLATE_ORDER_PROCESS_TEXT1 = "templateOrderProcessText1";
  public static final String TEMPLATE_ORDER_PROCESS_TEXT2 = "templateOrderProcessText2";
  public static final String TEMPLATE_PAYMENT_PROCESS_TITLE = "templatePaymentProcessTitle";
  public static final String TEMPLATE_PAYMENT_PROCESS_TEXT1 = "templatePaymentProcessText1";
  public static final String TEMPLATE_PAYMENT_PROCESS_TEXT2 = "templatePaymentProcessText2";

}

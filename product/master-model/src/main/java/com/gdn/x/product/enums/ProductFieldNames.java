package com.gdn.x.product.enums;

public interface ProductFieldNames {

  // product
  String MASTER_DATA_PRODUCT = "masterDataProduct";
  String PRODUCT_NAME_IN_MASTER_DATA_PRODUCT = "masterDataProduct.productName";
  String PRODUCT_TYPE = "productType";
  String SETTLEMENT_TYPE = "settlementType";
  String MERCHANT_CODE = "merchantCode";
  String MASTER_CATALOG = "masterCatalog";
  String SALES_CATALOGS = "salesCatalogs";
  String PRODUCT_CATENTRY_ID = "productCatentryId";
  String PRODUCT_CATALOG_ID = "productCatalogId";
  String DEFINING_ATTRIBUTES = "definingAttributes";
  String SALES_CATEGORY_SEQUENCES = "salesCategorySequences";
  String INSTALLATION_REQUIRED = "installationRequired";
  String TRADING_PRODUCT = "tradingProduct";
  String PICKUP_POINT_CODES = "pickupPointCodes";
  String ONLINE = "online";
  String B2C_ACTIVATED = "b2cActivated";
  String B2B_ACTIVATED = "b2bActivated";
  String BUNDLE_PRODUCT = "bundleProduct";
  String PICKED_FOR_DELETION = "pickedForDeletion";

  // master data product
  String SPECIFICATION_DETAIL = "specificationDetail";
  String WEIGHT = "weight";
  String HEIGHT = "height";
  String WIDTH = "width";
  String LENGTH = "length";
  String URL = "url";
  String PRODUCT_NAME = "productName";
  String LONG_DESCRIPTION = "longDescription";
  String UNIQUE_SELLING_POINT = "uniqueSellingPoint";
  String PRODUCT_STORY = "productStory";
  String UOM = "uom";
  String MASTER_DATA_PRODUCT_IMAGES = "masterDataProductImages";
  String MASTER_DATA_PRODUCT_ATTRIBUTES = "masterDataProductAttributes";
  String BRAND = "brand";
  String BRAND_LOGO_URL = "brandLogoUrl";

  // item
  String MERCHANT_SKU = "merchantSku";
  String OMNI_CHANNEL_SKU = "omniChannelSku";
  String INVENTORY_TYPE = "inventoryType";
  String DANGEROUS_LEVEL = "dangerousLevel";
  String FULL_FIELD_DANGEROUS_LEVEL = "masterDataItem.dangerousLevel";
  String ITEM_CATENTRY_ID = "itemCatentryId";
  String MASTER_DATA_ITEM = "masterDataItem";
  String GENERATED_ITEM_NAME_IN_MASTER_DATA_ITEM = "masterDataItem.generatedItemName";
  String PRICE = "price";
  String PRICE_UPDATED_DATE = "priceUpdatedDate";
  String FIRST_LIST_PRICE = "price.0.listPrice";
  String FIRST_OFFER_PRICE = "price.0.offerPrice";
  String PICKUP_POINT_CODE = "pickupPointCode";
  String EXTERNAL_PICKUP_POINT_CODE = "externalPickupPointCode";
  String IS_LATE_FULFILLMENT = "isLatefulfillment";
  String ITEM_VIEW_CONFIGS = "itemViewConfigs";
  String ITEM_VIEW_CONFIGS_WITH_CHANNEL = "itemViewConfigs.channel";
  String IS_ARCHIVED = "isArchived";
  String PROMO_BUNDLING = "promoBundling";
  String CNC_ACTIVATED = "cncActivated";
  String ACTIVE_PROMO_BUNDLINGS = "activePromoBundlings";
  String IS_FLASH_SALE_ACTIVE = "isFlashSaleActive";
  String MERCHANT_PROMO_DISCOUNT = "merchantPromoDiscount";
  String FORCE_REVIEW = "forceReview";
  String WHOLESALE_PRICE_EXISTS = "wholesalePriceExists";
  String FREE_SAMPLE = "freeSample";
  String PERMANENT_DELETE = "permanentDelete";
  String BUNDLE_RECIPE = "bundleRecipe";
  String MASTER_SKU = "masterSku";

  // master data item
  String GENERATED_ITEM_NAME = "generatedItemName";
  String UPC_CODE = "upcCode";
  String SKU_CODE = "skuCode";
  String HASH = "hash";
  String MASTER_DATA_ITEM_ATTRIBUTE_VALUES = "masterDataItemAttributeValues";
  String MASTER_DATA_ITEM_IMAGES = "masterDataItemImages";

  // price
  String CURRENCY = "currency";
  String OFFER_PRICE = "offerPrice";
  String LIST_PRICE = "listPrice";
  String UPDATE_PRICE_INDEX = "update_price_index";
  String OFFLINE_ITEM_ID = "offlineItemId";

  String INSURED_AMOUNT = "insuredAmount";

  // price, sale price
  String DISCOUNT_PRICE = "discountPrice";
  String MERCHANT_DISCOUNT_PRICE = "merchantDiscountPrice";

  // master data item
  String ITEM_DELIVERY_WEIGHT = "itemDeliveryWeight";
  String ITEM_HEIGHT = "itemHeight";
  String ITEM_LENGTH = "itemLength";
  String ITEM_WEIGHT = "itemWeight";
  String ITEM_WIDTH = "itemWidth";

  // product item attribute value, system parameter
  String VALUE = "value";

  // item, product item attribute value
  String ITEM_CODE = "itemCode";

  //item
  String SOURCE_ITEM_CODE = "sourceItemCode";
  String IS_CONTENT_CHANGED = "isContentChanged";
  String INITIAL_CONTENT_CHANGED = "initialContentChanged";

  // product item image
  String PRODUCT_ITEM_ID = "productItemId";

  // master catalog
  String CATEGORY = "category";

  // sales catalog
  String LIST_OF_CATEGORIES = "listOfCategories";

  // special attribute, attribute
  String ATTRIBUTE_VALUE = "attributeValue";

  // product view config
  String ITEM_DISPLAYABLE_SCHEDULES = "itemDisplayableSchedules";
  String ITEM_DISCOVERABLE_SCHEDULES = "itemDiscoverableSchedules";

  // product image, product item image
  String IS_MAIN_IMAGE = "isMainImage";
  String LOCATION_PATH = "locationPath";

  // product attribute
  String IS_OWNED_BY_PRODUCT_ITEM = "isOwnedByProductItem";
  String MASTER_DATA_PRODUCT_ATTRIBUTE_VALUES =
      "masterDataProductAttributeValues";

  // master catalog, sales catalog, catalog
  String PRODUCT_SPECIAL_ATTRIBUTES = "productSpecialAttributes";
  String CATALOG_CODE = "catalogCode";

  // product image, product attribute, category, product item image
  String SEQUENCE = "sequence";

  // product display schedule, product view config
  String IS_BUYABLE = "isBuyable";

  // product discoverable, product view config
  String IS_DISCOVERABLE = "isDiscoverable";

  // product, product view config, item
  String PRODUCT_SKU = "productSku";

  // product discoverable schedule, product displayable schedule, sale price
  String START_DATE_TIME = "startDateTime";
  String END_DATE_TIME = "endDateTime";

  // master data product, product, product image, product attribute
  String PRODUCT_CODE = "productCode";

  // product, item
  String IS_SYNCHRONIZED = "isSynchronized";
  String LATE_FULFILLMENT_UPDATED_BY = "latefulfillmentUpdatedBy";
  String LATE_FULFILLMENT_UPDATED_DATE = "latefulfillmentUpdatedDate";

  // product
  String IS_SUSPENDED = "isSuspended";

  // item
  String IS_ARCHIVED_BEFORE_SUSPENSION = "isArchivedBeforeSuspension";

  // master data product, item
  String SHIPPING_WEIGHT = "shippingWeight";

  // master data product, master data item, category
  String IS_VIEWABLE = "isViewable";

  // product view config, price
  String CHANNEL = "channel";

  // item, category, master data product, master data item
  String IS_ACTIVATED = "isActivated";

  // product item attribute value, special attribute, product attribute, attribute
  String ATTRIBUTE_CODE = "attributeCode";

  String VARIANT_CREATION = "variantCreation";

  // master data product, system parameter
  String DESCRIPTION = "description";

  // system parameter, gdnBaseMongoEntity
  String MARK_FOR_DELETE = "markForDelete";
  String UPDATED_DATE = "updatedDate";
  String CREATED_DATE = "createdDate";
  String VERSION = "version";
  String STORE_ID = "storeId";
  String CREATED_BY = "createdBy";
  String UPDATED_BY = "updatedBy";
  String LAST_UPDATED_BY = "lastUpdatedBy";
  String LAST_UPDATED_DATE = "lastUpdatedDate";
  String ID = "id";
  String VARIABLE = "variable";
  String SYSTEM_PARAMETER = "product_system_parameter";
  String SYSTEM_PARAMETER_INDEX = "product_system_parameter_index";
  String SYSTEM_PARAMETER_HISTORY = "product_system_parameter_history";

  // product attribute value
  String DESCRIPTIVE_ATTRIBUTE_VALUE = "descriptiveAttributeValue";
  String DESCRIPTIVE_ATTRIBUTE_VALUE_TYPE = "descriptiveAttributeValueType";
  String ALLOWED_ATTRIBUTE_VALUE_CODE = "allowedAttributeValueCode";
  String ALLOWED_ATTRIBUTE_VALUE = "allowedAttributeValue";
  String PREDEFINED_ALLOWED_ATTRIBUTE_VALUE_CODE =
      "predefinedAllowedAttributeValueCode";
  String PREDEFINED_ALLOWED_ATTRIBUTE_VALUE = "predefinedAllowedAttributeValue";

  // category
  String CATGROUP_ID = "catgroupId";
  String CATEGORY_CODE = "categoryCode";

  // Attribute
  String PRODUCT_ATTRIBUTE_DETAILS = "productAttributeDetails";

  // item, attribute
  String ITEM_SKU = "itemSku";

  // deffered reindex
  String REINDEX_TYPE = "reindexType";

  // master data attribute
  String ATTRIBUTE_TYPE = "attributeType";
  String MANDATORY = "mandatory";
  String ATTRIBUTE_NAME = "attributeName";
  String IS_SEARCHABLE = "isSearchable";
  String EXAMPLE = "example";
  String IS_SKU_VALUE = "isSkuValue";
  String IS_BASIC_VIEW = "isBasicView";

  // ticket template attribute
  String ETD_NOTE = "etdNote";
  String TICKET_TEMPLATE_CODE = "ticketTemplateCode";
  String NAME = "name";
  String FROM = "from";
  String CC = "cc";
  String BCC = "bcc";
  String ACTIVE = "active";
  String SUBJECT_ORDER_RECEIVED = "subjectOrderReceived";
  String SUBJECT_ORDER_PAID = "subjectOrderPaid";
  String SUBJECT_BARCODE = "subjectBarcode";
  String TEMPLATE_ORDER_RECEIVED = "templateOrderReceived";
  String TEMPLATE_ORDER_PAID = "templateOrderPaid";
  String TEMPLATE_BARCODE = "templateBarcode";
  String BILLINGUAL = "bilingual";
  String TEMPLATE_BARCODE_HEADING_TEXT = "templateBarcodeHeadingText";
  String TEMPLATE_BARCODE_PROCESS_TITLE = "templateBarcodeProcessTitle";
  String TEMPLATE_BARCODE_PROCESS_TEXT = "templateBarcodeProcessText";
  String TEMPLATE_ORDER_PROCESS_TITLE = "templateOrderProcessTitle";
  String TEMPLATE_ORDER_PROCESS_TEXT1 = "templateOrderProcessText1";
  String TEMPLATE_ORDER_PROCESS_TEXT2 = "templateOrderProcessText2";
  String TEMPLATE_PAYMENT_PROCESS_TITLE = "templatePaymentProcessTitle";
  String TEMPLATE_PAYMENT_PROCESS_TEXT1 = "templatePaymentProcessText1";
  String TEMPLATE_PAYMENT_PROCESS_TEXT2 = "templatePaymentProcessText2";
  String OFF2ON_CHANNEL_ACTIVE = "off2OnChannelActive";
  String OFF2ON_ITEM_COUNT = "off2OnItemCount";

  String ERROR_MESSAGE = "errorMessage";

  // Pristine Master Data Item
  String PRISTINE_DATA_ITEM = "pristineDataItem";
  String PRISTINE_ID = "pristineId";
  String PRISTINE_PRODUCT_NAME = "pristineProductName";
  String PRISTINE_BRAND = "pristineBrand";
  String PRISTINE_CATEGORY = "pristineCategory";
  String PRISTINE_LISTING_ATTRIBUTES = "pristineListingAttributes";
  String PRISTINE_MASTER_ID = "pristineMasterId";
  String PRISTINE_CATEGORIES_HIERARCHY = "pristineCategoriesHierarchy";
  String PRISTINE_PRODUCT_CONDITION = "productCondition";
  String PRISTINE_DEFAULT_PRODUCT_CODE = "defaultProductCode";
  String PRISTINE_SHORT_ID = "pristineShortId";

  // Pristine Master Data Item Update
  String IS_UPDATED = "isUpdated";

  //Product Score
  String PRODUCT_SCORE = "productScore";
  String MANDATORY_ATTRIBUTE_SCORE = "mandatoryAttributeScore";
  String PRODUCT_TITLE_SCORE = "productTitleScore";
  String DESCRIPTION_SCORE = "descriptionScore";
  String USP_SCORE = "uspScore";
  String RECOMMENDED_ATTRIBUTE_SCORE = "recommendedAttributeScore";
  String REMAINING_ATTRIBUTE_SCORE = "remainingAttributeScore";
  String VIDEO_URL_SCORE = "videoUrlScore";
  String IMAGE_SCORE = "imageScore";
  String VARIANT_CREATING_SCORE = "variantCreatingScore";
  String EAN_UPC_SCORE = "eanUpcScore";
  String TOTAL_SCORE = "totalScore";
  String OLD_VALUE = "oldValue";
  String NEW_VALUE = "newValue";

  //item
  String IS_SUBSCRIBABLE = "isSubscribable";

  String SUBSCRIBABLE = "subscribable";
  String PREFERRED_SUBSCRIPTION_TYPE= "preferredSubscriptionType";

  //productCenter
  String PRODUCT_CENTER_UPDATED_DATE = "productCenterUpdatedDate";

  String PRODUCT_REINDEX_STATUS = "productReindexStatus";

  String IS_PREORDER = "isPreOrder";

  String PREORDER_TYPE = "preOrderType";

  String PREORDER_VALUE = "preOrderValue";

  String PREORDER_DATE = "preOrderDate";

  String PREORDER = "preOrder";

  String IS_TAKEN_DOWN = "isTakenDown";

  String IS_ARCHIVED_BEFORE_EDIT = "isArchivedBeforeEdit";

  String TOPIC_NAME = "topicName";

  String IDENTIFIER = "identifier";

  String CACHE_CLEAR = "cacheClear";

  String RETRY_COUNT = "retryCount";

  String RETRY_PUBLISH_STATUS = "retryPublishStatus";

  //Kafka event logger
  String TIMESTAMP = "timestamp";
  String PRIMARY_IDENTIFIER = "primaryIdentifier";
  String SECONDARY_IDENTIFIER = "secondaryIdentifier";
  String EVENT_STATUS = "status";
  String START_TIME = "startTime";
  String END_TIME = "endTime";
  String PAYLOAD = "payload";
  String MAIN_IMAGE_URL = "mainImageUrl";

  //Item pickup point
  String DELIVERY = "delivery";
  String CNC_ACTIVE = "cncActive";
  String FBB_ACTIVATED = "fbbActivated";
  String DISTRIBUTION_STATUS = "distributionStatus";
  String DISTRIBUTION = "distribution";

  // common image
  String COMMON_IMAGE = "commonImage";


  String ITEM_PICKUP_POINT_DISPLAYABLE_SCHEDULES = "itemViewConfigs.0.itemDisplayableSchedules";
  String ITEM_PICKUP_POINT_DISCOVERABLE_SCHEDULES = "itemViewConfigs.0.itemDiscoverableSchedules";

  //Update field names
  String ITEM_PICKUP_POINT_BUYABLE = "itemViewConfigs.$[].isBuyable";
  String ITEM_PICKUP_POINT_DISCOVERABLE = "itemViewConfigs.$[].isDiscoverable";
  String ITEM_PICKUP_POINT_ALL_DISPLAYABLE_SCHEDULES = "itemViewConfigs.$[].itemDisplayableSchedules";
  String ITEM_PICKUP_POINT_ALL_DISCOVERABLE_SCHEDULES = "itemViewConfigs.$[].itemDiscoverableSchedules";

  // Allowed Attribute Values

  String VALUE_SEQUENCE = "valueSequence";

  // b2bFields
  String B2B_FIELDS = "b2bFields";
  String MANAGED = "managed";
  String BASE_PRICE = "basePrice";
  String CURATION_STATUS = "curationStatus";
  String DEFAULT_CHANNEL = "DEFAULT";
  String CNC_CHANNEL = "CNC";

  String ITEM_VIEW_CONFIGS_WITH_IS_BUYABLE = "itemViewConfigs.isBuyable";

  String ITEM_VIEW_CONFIGS_WITH_BUYABLE_SCHEDULES_IS_BUYABLE = "itemViewConfigs.itemDisplayableSchedules.isBuyable";
  String SIZE_CHART_CODE  = "sizeChartCode";
  String DIMENSIONS_MISSING= "dimensionsMissing";
  String SIZE_ATTRIBUTE_CODE = "sizeAttributeCode";
  String MISSING_FIELDS = "missingFields";
  String VIDEO = "video";
  String VIDEO_URL = "finalUrl";
  String VIDEO_COVER_IMAGE_PATH = "coverImagePath";
  String VIDEO_SOURCE_URL = "sourceUrl";
  String VIDEO_ID = "videoId";
  String VIDEO_NAME = "videoName";
  String YOUTUBE_URL = "url";
}

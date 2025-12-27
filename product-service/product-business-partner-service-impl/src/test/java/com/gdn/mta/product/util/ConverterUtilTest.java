package com.gdn.mta.product.util;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.TreeMap;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.gda.mta.product.dto.BulkDownloadProductLevel3Response;
import com.gda.mta.product.dto.ItemPickupPointRequest;
import com.gda.mta.product.dto.PickupPointDeleteRequest;
import com.gda.mta.product.dto.ProductCollectionDTO;
import com.gda.mta.product.dto.ProductLevel3SummaryResponse;
import com.gda.mta.product.dto.ProductVariantPriceStockAndImagesRequest;
import com.gda.mta.product.dto.ProductVariantUpdateRequest;
import com.gda.mta.product.dto.QuickEditV2Request;
import com.gda.mta.product.dto.RestrictedKeywordsByFieldAndActionType;
import com.gda.mta.product.dto.response.ItemSummaryL4Response;
import com.gda.mta.product.dto.response.KeywordRecommendationsResponse;
import com.gda.mta.product.dto.response.KeywordRestrictionModelsResponse;
import com.gdn.mta.domain.event.modal.AddEditedProductToPDTEvent;
import com.gdn.mta.domain.event.modal.ImageQcResponseDomainEvent;
import com.gdn.mta.product.entity.ItemSkuToItemIdMapping;
import com.gda.mta.product.dto.response.InternalProductHistoryEventModel;
import com.gdn.mta.product.entity.KeywordRequestDTO;
import com.gdn.mta.product.entity.ProductBusinessPartner;
import com.gdn.mta.product.entity.ProductBusinessPartnerCounter;
import com.gdn.mta.product.entity.ProductItemBusinessPartner;
import com.gdn.partners.pbp.commons.constants.SaveHistoryConstants;
import com.gdn.partners.pbp.model.productlevel3.ProductLevel3Inventory;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.product.model.vo.BundleRecipeVo;
import com.gdn.x.product.rest.web.model.dto.DiscountPriceDTO;
import com.gdn.x.product.rest.web.model.dto.PreOrderDTO;
import com.gdn.x.product.rest.web.model.dto.PriceDTO;
import com.gdn.x.product.rest.web.model.enums.ProductStatus;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointDeleteRequest;
import com.gdn.x.product.rest.web.model.request.ItemRequest;
import com.gdn.x.product.rest.web.model.request.NeedCorrectionItemActivationRequest;
import com.gdn.x.product.rest.web.model.response.B2BResponse;
import com.gdn.x.product.rest.web.model.response.ItemBasicDetailV2Response;
import com.gdn.x.product.rest.web.model.response.ItemLevel5Response;
import com.gdn.x.product.rest.web.model.response.ItemResponseV2;
import com.gdn.x.product.rest.web.model.response.ItemSummaryListResponse;
import com.google.common.collect.ImmutableList;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang.time.DateUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.solr.common.SolrInputDocument;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.AutoApprovalsDetailDto;
import com.gda.mta.product.dto.BulkDownloadProductLevel3Response;
import com.gda.mta.product.dto.CopyImageEditRequest;
import com.gda.mta.product.dto.DimensionRefreshRequest;
import com.gda.mta.product.dto.DiscountPriceRequest;
import com.gda.mta.product.dto.ImageQcProcessedResponse;
import com.gda.mta.product.dto.ItemImageEditRequest;
import com.gda.mta.product.dto.ItemPickupPointRequest;
import com.gda.mta.product.dto.PickupPointDeleteRequest;
import com.gda.mta.product.dto.ProductCollectionDTO;
import com.gda.mta.product.dto.ProductCreationRequest;
import com.gda.mta.product.dto.ProductImageEditRequest;
import com.gda.mta.product.dto.ProductImagePredictionResponse;
import com.gda.mta.product.dto.ProductItemCreationRequest;
import com.gda.mta.product.dto.ProductItemWholesalePriceRequest;
import com.gda.mta.product.dto.ProductLevel3SummaryDetailsImageRequest;
import com.gda.mta.product.dto.ProductLevel3SummaryResponse;
import com.gda.mta.product.dto.ProductPriceAndWholesaleRequest;
import com.gda.mta.product.dto.ProductPriceStockAndImagesRequest;
import com.gda.mta.product.dto.ProductRevisionInfoResponse;
import com.gda.mta.product.dto.ProductVariantPriceStockAndImagesRequest;
import com.gda.mta.product.dto.ProductVariantUpdateRequest;
import com.gda.mta.product.dto.QuickEditV2Request;
import com.gda.mta.product.dto.RejectedSkuProductResponse;
import com.gda.mta.product.dto.RestrictedKeywordsByField;
import com.gda.mta.product.dto.RestrictedKeywordsByFieldAndActionType;
import com.gda.mta.product.dto.ScreeningProductBulkActionsRequest;
import com.gda.mta.product.dto.response.AutoApprovalRulesListResponse;
import com.gda.mta.product.dto.response.HistoryUpdateResponse;
import com.gda.mta.product.dto.response.ImageQcPredictionResponse;
import com.gda.mta.product.dto.response.ImageQcResponse;
import com.gda.mta.product.dto.response.InternalProductHistoryEventModel;
import com.gda.mta.product.dto.response.ItemSummaryL4Response;
import com.gda.mta.product.dto.response.KeywordRecommendationsResponse;
import com.gda.mta.product.dto.response.KeywordRestrictionModelsResponse;
import com.gda.mta.product.dto.response.ProductFilterResponse;
import com.gda.mta.product.dto.response.RestrictedKeywordsByFieldResponse;
import com.gdn.micro.graphics.domain.event.model.BulkImageProcessResponse;
import com.gdn.micro.graphics.domain.event.model.ImageResponse;
import com.gdn.micro.graphics.web.model.ImageRequest;
import com.gdn.mta.domain.event.modal.AddRevisedProductToPDTEvent;
import com.gdn.mta.domain.event.modal.ImageQcProcessedResponseDomainEvent;
import com.gdn.mta.domain.event.modal.ImageQcRequestDomainEvent;
import com.gdn.mta.domain.event.modal.ImageQcResponseDomainEvent;
import com.gdn.mta.domain.event.modal.PDTDimensionRefreshEventModel;
import com.gdn.mta.domain.event.modal.SolrReviewProductCollectionAddEvent;
import com.gdn.mta.domain.event.modal.SolrReviewProductCollectionAddEventFields;
import com.gdn.mta.product.commons.constant.RestrictedKeywordFieldNames;
import com.gdn.mta.product.entity.AutoApprovalRules;
import com.gdn.mta.product.entity.ItemSkuToItemIdMapping;
import com.gdn.mta.product.entity.KeywordRequestDTO;
import com.gdn.mta.product.entity.NeedCorrectionNotesDto;
import com.gdn.mta.product.entity.ProductBusinessPartner;
import com.gdn.mta.product.entity.ProductBusinessPartnerCounter;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.entity.ProductHistory;
import com.gdn.mta.product.entity.ProductImagePrediction;
import com.gdn.mta.product.entity.ProductImageQcProcessingResponse;
import com.gdn.mta.product.entity.ProductItemBusinessPartner;
import com.gdn.mta.product.entity.ProductItemWholesalePrice;
import com.gdn.mta.product.entity.ProductLevel3;
import com.gdn.mta.product.entity.ProductLevel3Attribute;
import com.gdn.mta.product.entity.RejectedSkuProductCollection;
import com.gdn.mta.product.entity.UpdatedProductHistory;
import com.gdn.mta.product.enums.ApiErrorCode;
import com.gdn.mta.product.enums.AutoApprovalType;
import com.gdn.mta.product.enums.BrandApprovalStatus;
import com.gdn.mta.product.enums.ProductLevel3Status;
import com.gdn.mta.product.valueobject.SolrProductCollectionDTO;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.commons.constants.SaveHistoryConstants;
import com.gdn.partners.pbp.commons.util.SolrFieldNames;
import com.gdn.partners.pbp.model.productlevel3.ProductLevel3Inventory;
import com.gdn.partners.product.pricing.web.model.request.BulkActivateDeactivateRequest;
import com.gdn.partners.product.pricing.web.model.request.WholesalePriceBulkUpdateRequest;
import com.gdn.partners.product.pricing.web.model.request.WholesalePriceRequest;
import com.gdn.partners.product.pricing.web.model.response.WholesalePriceSkuResponse;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.product.enums.AdjustmentTypeEnum;
import com.gdn.x.product.enums.DescriptiveAttributeValueType;
import com.gdn.x.product.enums.MasterDataAttributeType;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.model.vo.BundleRecipeVo;
import com.gdn.x.product.rest.web.model.dto.CategoryDTO;
import com.gdn.x.product.rest.web.model.dto.DiscountPriceDTO;
import com.gdn.x.product.rest.web.model.dto.ItemSummaryResponse;
import com.gdn.x.product.rest.web.model.dto.MasterCatalogDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataAllowedAttributeValueDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataAttributeDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataItemAttributeValueDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataItemDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataItemImageDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductAttributeDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductAttributeValueDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductImageDTO;
import com.gdn.x.product.rest.web.model.dto.PredefinedAllowedAttributeValueDTO;
import com.gdn.x.product.rest.web.model.dto.PriceDTO;
import com.gdn.x.product.rest.web.model.enums.ProductStatus;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointDeleteRequest;
import com.gdn.x.product.rest.web.model.request.ItemRequest;
import com.gdn.x.product.rest.web.model.request.PriceRequest;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.rest.web.model.response.B2BResponse;
import com.gdn.x.product.rest.web.model.response.BusinessPartnerPickupPointResponse;
import com.gdn.x.product.rest.web.model.response.ItemBasicDetailV2Response;
import com.gdn.x.product.rest.web.model.response.ItemLevel5Response;
import com.gdn.x.product.rest.web.model.response.ItemResponse;
import com.gdn.x.product.rest.web.model.response.ItemResponseV2;
import com.gdn.x.product.rest.web.model.response.ItemSkuPickupPointCodeResponse;
import com.gdn.x.product.rest.web.model.response.ItemSummaryListResponse;
import com.gdn.x.product.rest.web.model.response.PriceResponse;
import com.gdn.x.product.rest.web.model.response.ProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.response.ProductResponse;
import com.gdn.x.product.rest.web.model.response.ViewConfigResponse;
import com.gdn.x.productcategorybase.dto.AttributeType;
import com.gdn.x.productcategorybase.dto.CategorySummaryResponse;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.request.AllowedAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.AttributeRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemImageUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemUpcCodeUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import com.gdn.x.productcategorybase.dto.request.SkuCodesRequest;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryRestrictedKeywordResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemImageRequest;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductAttribute;
import com.gdn.x.productcategorybase.entity.ProductItemAttributeValue;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;

public class ConverterUtilTest {

  private static final String PRODUCT_SKU = "product-sku";
  private static final String ITEM_NAME = "item-name";
  private static final String ITEM_CODE = "item-code";
  private static final String ITEM_SKU = "item-sku";
  private static final String ITEM_ID = "item-id";
  private static final String SELLER_SKU = "seller-sku";
  private static final String ITEM_SKU_2= "item-sku-2";
  private static final int QUANTITY = 10;
  private static final int SIZE_VALUE = 1;
  private static final double MAX_PRICE = 100.0d;
  private static final double MIN_PRICE = 10.0d;
  private static final double WHOLESALE_DISCOUNT = 1000;
  private static final String UPC_CODE = "upc-code";
  private static final String SKU_CODE = "sku-code";
  private static final String ATTRIBUTE_CODE = "attribute-code";
  private static final String MAIN_IMAGE_URL = "imageUrl";
  private static final String ATTRIBUTE_VALUE = "S";
  private static final String ATTRIBUTE_VALUE_TYPE = "US";
  private static final String COMBINED_VALUE_TYPE = "US-S";
  private static final String ATTRIBUTE_CODE1 = "attribute_code1";
  private ProductCollection productCollection;
  private SolrReviewProductCollectionAddEvent solrReviewProductCollectionAddEvent;
  private ProductDetailResponse productDetailResponse;
  private SolrReviewProductCollectionAddEventFields solrReviewProductCollectionAddEventFields;
  private SolrProductCollectionDTO solrProductCollectionDTO = new SolrProductCollectionDTO();

  private static final String ID = "id";
  private static final String STORE_ID = "10001";
  private static final String PRODUCT_ID = "productId";
  private static final String PRODUCT_CODE = "productCode";
  private static final String ACTIVITY = "activity";
  private static final String PRODUCT_NAME = "productName";
  private static final String NEW = "new";
  private static final String UPDATE = "update";
  private static final String BRAND = "brand";
  private static final String CATEGORY_CODE = "categoryCode";
  private static final String CATEGORY_NAME = "categoryName";
  private static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private static final String BUSINESS_PARTNER_NAME = "businessPartnerName";
  private static final String BUSINESS_PARTNER_ID = "businessPartnerId";
  private static final String STATE = "DRAFT";
  private static final String CREATED_BY = "createdBy";
  private static final String ASSIGNED_TO = "assignedTo";
  private static final String ASSIGNED_BY = "assignedBy";
  private static final Date CREATED_DATE = new Date();
  private static final String UPDATED_BY = "updatedBy";
  private static final Date SUBMITTED_DATE = new Date();
  private static final Date UPDATED_DATE = new Date();
  private static final Date UPDATED_STEP_DATE = new Date();
  private static final int RESUBMIT_COUNT = 1;
  private static final Date LAST_UPDATED_DATE = new Date();
  private static final String NOTES = "notes";
  private static final String UNIQUE_SELLING_POINT = "unique selling point";
  private static final String DEFAULT_PRODUCT_DESCRIPTION = "desc";
  private static final String ADDITIONAL_NOTES = "additionalNotes";
  private static final String DEFAULT_ATTRIBUTE_NAME = "Attribute name";
  private static final String DEFAULT_ATTRIBUTE_VALUE = "Attribute value";
  private static final String HASH_CODE_1 = "hashcode1";
  private static final String HASH_CODE_2 = "hashcode2";
  private static final String LOCATION_PATH_1 = "locationPath1";
  private static final String LOCATION_PATH_2 = "locationPath2";
  private static final String IMAGE_SOURCE_DIRECTORY = "imageSourceDirectory";
  private static final long NORMAL_PRICE = 1000;
  private static final long SALE_PRICE = 2000;
  private static final Double PRICE = 1000.00;
  private static final double SALE_PRICE_1 = 2000.00;
  private static final String DEFAULT_CURRENCY = "currency";
  private static final String DEFAULT_CHANNEL = "DEFAULT";
  private static final long DISCOUNT_AMOUNT = 100;
  private static final Double DISCOUNT_PRICE = 100.00;
  private static final String ADJUSTMENT_NAME = "adjustment_name";
  private List<String> ACTIVE_PREDICTIONS =
      Arrays.asList("watermark_predictions", "blur_predictions", "text_predictions", "nsfw_predictions");
  private static final String IMAGE_QC_WITHOUT_IMAGES =
      "{\"timestamp\":1595003740556,\"productCode\":\"MTA-0451837\",\"success\":true,\"errorMessage\":\"\"}";
  private static final String VALUE1 = "value1";
  private static final String VALUE2 = "value2";
  private static final List<String> ERROR_FIELDS = Arrays.asList("Description");
  private static final List<String> MERCHANT_MODIFIED_FIELDS = Arrays.asList("packageDimension","shipping");
  private static final List<String> VENDOR_ERROR_NOTES = Arrays.asList("correctionReason");
  private static final String CORRECTION_REASON = "correctionReason";
  private static final String CORRECTION_REASON_1 = "correctionReason1";
  private static final String ADDITIONAL_NOTES_1 = "additionalNotes1";
  private static final String USERNAME = "username";
  private static final String NEED_CORRECTION_NOTES =
      "{\"vendorNotes\":[\"correctionReason\"],\"imageReason\":[\"correctionReason1\"],\"commonImageReason\":[\"correctionReason1\"],\"vendorErrorFields\":[\"Description\"],\"merchantModifiedFields\":[],\"contentAdditionalNotes\":\"additionalNotes\",\"allVariants\":true,\"imagesAdditionalNotes\":\"additionalNotes1\",\"itemNotes\":[],\"allModifiedFields\":null}";
  private static final String REVISION_NOTES =
      "{\"ContentAdditionalNotes\":\"string\",\"assignTo\":\"string\",\"assignedBy\":\"string\",\"vendorNotes\":[\"Incomplete or inappropriate content\"],\"vendorErrorFields\":[\"Video Url\",\"description\"],\"allVariants\":true,\"imagesAdditionalNotes\":\"notes\",\"imageReason\":[\"Blur or inappropriate images\"],\"productCodes\":[\"string\"],\"rejectionReason\":\"string\",\"itemNotes\":[{\"itemSku\":\"TOQ-15126-01980-00007\",\"skuCode\":\"MTA-0451490-00007\",\"itemNumber\":1,\"itemName\":\"itemName-1\",\"vendorNotes\":[\"Blur or inappropriate images\"],\"vendorErrorFields\":[\"images\"]},{\"itemSku\":\"TOQ-15126-01980-00008\",\"skuCode\":\"MTA-0451490-00008\",\"itemNumber\":2,\"itemName\":\"itemName-2\",\"vendorNotes\":[\"Blur or inappropriate images\"],\"vendorErrorFields\":[\"images\"]}]}";
  private static final String WHOLESALE_RULE_1 = "[{\"quantity\":3,\"wholesaleDiscount\":30.0}]";

  private static final Integer SEQUENCE_NUMBER = 0;
  private static final String RULE_NAME = "rule_name";
  private static final String RULE_CONFIG =
      "[{\"keyName\":\"is_edited_by_internal_user\",\"value\":\"@gdn-commerce.com\",\"valueType\":\"string\",\"operator\":\"contains\"}]";
  private static final String IMAGE_QC_CONFIG =
      "[{\"keyName\":\"blur\",\"value\":\"101\",\"valueType\":\"int\",\"operator\":\"<=\"},{\"keyName\":\"watermark\",\"value\":\"101\",\"valueType\":\"int\",\"operator\":\"<=\"},{\"keyName\":\"text\",\"value\":\"0\",\"valueType\":\"int\",\"operator\":\"==\"},{\"keyName\":\"Adult\",\"value\":\"50\",\"valueType\":\"int\",\"operator\":\"<=\"}]";

  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DEFAULT_CREATED_BY = "test_user_create";
  private static final String DEFAULT_UPDATED_BY = "test_user_update";
  private static final String RESTRICTED_KEYWORDS = "restrictedKeywords";
  private static final Long DEFAULT_VERSION = 53535L;
  private static final String PREDICTION_TYPE = "predictionType";
  private static final String ADULT_PREDICTION_TYPE = "Adult";
  private static final String DISPLAY_NAME = "name";
  private static final String PICKUP_POINT_CODE = "pickupPointCode";
  private static final String PICKUP_POINT_NAME = "pickupPointName";

  private static final Double LENGTH = 10.0;
  private static final Double WIDTH = 10.0;
  private static final Double HEIGHT = 10.0;
  private static final Double WEIGHT = 10.0;
  private static final Double SHIPPING_WEIGHT = 10.0;
  private static final Integer DG_LEVEL = 1;
  private static final Integer PRODUCT_TYPE = 1;
  private static final Double NEW_NORMAL_PRICE = 1100.0;
  private static final Double NEW_SALE_PRICE = 1000.0;
  private static final Integer WAREHOUSE_AVAILABLE = 100;
  private static final Integer WAREHOUSE_RESERVED = 10;
  private static final Integer WEB_RESERVED = 100;
  private static final Integer WEB_AVAILABLE = 10;
  private static final Integer WEB_MIN_ALERT = 10;
  private static final Integer NON_DISTRIBUTED_AVAILABLE = 100;
  private static final Integer NON_DISTRIBUTED_RESERVED = 10;
  private static final boolean WEB_SYNC_STOCK = true;
  private static final boolean DISPLAY = true;
  private static final boolean BUYABLE = true;
  private static final String GCS_PREFIX_PATH = "Path2";
  private static final String GCS_SOURCE_URL = "gcsSourceUrl";
  private static final String GCS_SOURCE_URL1 = "gcsSourceUrl/";
  private static final String CATEGORY_CODE1 = "categoryCode1";
  private static final String CATEGORY_CODE2 = "categoryCode2";
  private static final String CATEGORY_NAME1 = "categoryName1";
  private static final String CATEGORY_NAME2 = "categoryName2";
  private static final String MPP_ALLOWED_SELLER = "CC,CM";
  public static final String INVALID_DETECTION = "invalid-detection";
  private static final String BUNDLE_RECIPE = "bundleRecipe";

  private RejectedSkuProductCollection rejectedSkuProductCollection = new RejectedSkuProductCollection();
  private List<ItemLevel5Response> productDatas = new ArrayList<>();
  private ItemLevel5Response itemLevel5Response = new ItemLevel5Response();
  private Map<String, ProductLevel3Inventory> inventoryDatas = new HashMap<>();
  private ProductLevel3Inventory inventory = new ProductLevel3Inventory();
  private List<PriceResponse> prices = new ArrayList<>();
  private List<ViewConfigResponse> viewConfigs = new ArrayList<>();
  private PriceResponse priceResponse = new PriceResponse();
  private ViewConfigResponse viewConfigResponse = new ViewConfigResponse();
  private List<RejectedSkuProductCollection> rejectedSkuProductCollectionList = new ArrayList<>();
  private AttributeRequest attributeRequest = new AttributeRequest();
  private ProductItemAttributeValueRequest productItemAttributeValueRequest = new ProductItemAttributeValueRequest();
  private BulkImageProcessResponse bulkImageProcessResponse;
  private ProductPriceAndWholesaleRequest productPriceAndWholesaleRequest = new ProductPriceAndWholesaleRequest();
  private DiscountPriceRequest discountPriceRequest = new DiscountPriceRequest();
  private ProductImageQcProcessingResponse productImageQcProcessingResponse = new ProductImageQcProcessingResponse();
  private ImageQcProcessedResponse imageQcProcessedResponse = new ImageQcProcessedResponse();
  private ProductAndItemsResponse productAndItemsResponse;
  private ItemResponse itemResponse = new ItemResponse();
  private ProductLevel3 productLevel3;
  private Product product;
  private ScreeningProductBulkActionsRequest screeningProductBulkActionsRequest;
  private List<ProductItemWholesalePrice> productItemWholesalePrices;
  ProductLevel3SummaryDetailsImageRequest image = new ProductLevel3SummaryDetailsImageRequest();
  private Image productImage = new Image();
  private ImageQcResponse imageQcResponse = new ImageQcResponse();

  private RestrictedKeywordsByField restrictedKeywordsByField;
  private BundleRecipeVo bundleRecipeVo;
  private String restrictedKeywordJson;
  private String bundleRecipeJson;
  private ProductImagePredictionResponse productImagePredictionResponse = new ProductImagePredictionResponse();
  private ProductImagePrediction productImagePrediction;
  private List<ProductImagePrediction> productImagePredictionList;
  private UpdatedProductHistory updatedProductHistory = new UpdatedProductHistory();
  private BusinessPartnerPickupPointResponse businessPartnerPickupPointResponse =
    new BusinessPartnerPickupPointResponse();
  private HistoryUpdateResponse historyUpdateResponse = new HistoryUpdateResponse();
  private List<CategoryResponse> categoryResponses = new ArrayList<>();
  CategoryResponse categoryResponse1 = new CategoryResponse();
  CategoryResponse categoryResponse2 = new CategoryResponse();
  private ItemResponseV2 itemResponseV2;

  @BeforeEach
  public void setUp() throws Exception {
    itemLevel5Response.setItemSku(ITEM_SKU);
    itemLevel5Response.setProductSku(PRODUCT_SKU);
    itemLevel5Response.setGeneratedItemName(ITEM_NAME);
    itemLevel5Response.setPickupPointCode(PICKUP_POINT_CODE);
    itemLevel5Response.setItemCode(ITEM_CODE);
    itemLevel5Response.setSellerSku(SELLER_SKU);
    itemLevel5Response.setOff2OnChannelActive(Boolean.TRUE);
    itemLevel5Response.setCncActivated(Boolean.TRUE);
    priceResponse.setSalePrice(SALE_PRICE_1);
    priceResponse.setPrice(PRICE);
    prices.add(priceResponse);
    viewConfigResponse.setDisplay(DISPLAY);
    viewConfigResponse.setBuyable(BUYABLE);
    viewConfigs.add(viewConfigResponse);
    itemLevel5Response.setPrices(prices);
    itemLevel5Response.setViewConfigs(viewConfigs);
    itemLevel5Response.setCncActive(true);
    itemLevel5Response.setProductCode(PRODUCT_CODE);
    itemLevel5Response.setSellerSku(SELLER_SKU);
    productDatas.add(itemLevel5Response);
    inventory.setWarehouseAvailable(WAREHOUSE_AVAILABLE);
    inventory.setWarehouseReserved(WAREHOUSE_RESERVED);
    inventory.setWebAvailable(WEB_AVAILABLE);
    inventory.setWebReserved(WEB_RESERVED);
    inventory.setWebMinAlert(WEB_MIN_ALERT);
    inventory.setWebSyncStock(WEB_SYNC_STOCK);
    inventory.setNonDistributionAvailable(NON_DISTRIBUTED_AVAILABLE);
    inventory.setNonDistributionReserved(NON_DISTRIBUTED_RESERVED);
    inventoryDatas.put(ITEM_SKU, inventory);
    rejectedSkuProductCollection.setProductCode(PRODUCT_CODE);
    rejectedSkuProductCollection.setCategoryName(CATEGORY_NAME);
    rejectedSkuProductCollection.setProductName(PRODUCT_NAME);
    rejectedSkuProductCollectionList.add(rejectedSkuProductCollection);
    attributeRequest.setName(DEFAULT_ATTRIBUTE_NAME);
    attributeRequest.setScreeningMandatory(Boolean.TRUE);
    attributeRequest.setVariantCreatingUI(Boolean.TRUE);
    productItemAttributeValueRequest.setAttribute(attributeRequest);
    productItemAttributeValueRequest.setValue(DEFAULT_ATTRIBUTE_VALUE);
    solrProductCollectionDTO.setProductCode(PRODUCT_CODE);
    solrProductCollectionDTO.setProductName(PRODUCT_NAME);
    solrProductCollectionDTO.setBrand(BRAND);
    solrProductCollectionDTO.setCategoryName(CATEGORY_NAME);

    productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setName(PRODUCT_NAME);
    productDetailResponse.setUniqueSellingPoint(UNIQUE_SELLING_POINT);
    productDetailResponse.setDescription(DEFAULT_PRODUCT_DESCRIPTION.getBytes());

    bulkImageProcessResponse = new BulkImageProcessResponse();
    bulkImageProcessResponse.setGroupCode(PRODUCT_CODE);
    ImageResponse imageResponse = new ImageResponse();
    imageResponse.setHashCode(HASH_CODE_1);
    imageResponse.setImagePathLocation(Constants.DELIMITER_SLASH + LOCATION_PATH_1);
    bulkImageProcessResponse.setImageResponses(new ArrayList<>());
    bulkImageProcessResponse.getImageResponses().add(imageResponse);
    ImageResponse imageResponse1 = new ImageResponse();
    imageResponse1.setHashCode(HASH_CODE_2);
    imageResponse1.setImagePathLocation(Constants.DELIMITER_SLASH + LOCATION_PATH_2);
    bulkImageProcessResponse.getImageResponses().add(imageResponse1);

    discountPriceRequest.setAdjustmentName(ADJUSTMENT_NAME);
    discountPriceRequest.setAdjustmentType(AdjustmentTypeEnum.BLIBLI);
    discountPriceRequest.setDiscountPrice(DISCOUNT_AMOUNT);

    productPriceAndWholesaleRequest.setDiscountPrice(Arrays.asList(discountPriceRequest));
    productPriceAndWholesaleRequest.setListPrice(NORMAL_PRICE);
    productPriceAndWholesaleRequest.setOfferPrice(SALE_PRICE);
    productPriceAndWholesaleRequest.setWholesalePriceActivated(Boolean.TRUE);
    productPriceAndWholesaleRequest.setChannel(DEFAULT_CHANNEL);
    productPriceAndWholesaleRequest.setCurrency(DEFAULT_CURRENCY);

    productAndItemsResponse = new ProductAndItemsResponse();
    ProductResponse productResponse = new ProductResponse();
    MasterDataProductImageDTO masterDataProductImageDTO = new MasterDataProductImageDTO();
    masterDataProductImageDTO.setLocationPath("/MTA-XXXXXXX/image");
    MasterDataProductAttributeDTO masterDataProductAttributeDTO = new MasterDataProductAttributeDTO();
    MasterDataProductAttributeDTO masterDataProductAttributeDTO2 = new MasterDataProductAttributeDTO();
    MasterDataProductAttributeDTO masterDataProductAttributeDTO3 = new MasterDataProductAttributeDTO();
    MasterDataAttributeDTO masterDataAttribute = new MasterDataAttributeDTO();
    masterDataAttribute.setAttributeType(MasterDataAttributeType.DESCRIPTIVE_ATTRIBUTE);
    MasterDataAttributeDTO masterDataAttribute2 = new MasterDataAttributeDTO();
    masterDataAttribute2.setAttributeType(MasterDataAttributeType.PREDEFINED_ATTRIBUTE);
    MasterDataAttributeDTO masterDataAttribute3 = new MasterDataAttributeDTO();
    masterDataAttribute3.setAttributeType(MasterDataAttributeType.DEFINING_ATTRIBUTE);
    masterDataProductAttributeDTO.setMasterDataAttribute(masterDataAttribute);
    masterDataProductAttributeDTO2.setMasterDataAttribute(masterDataAttribute2);
    masterDataProductAttributeDTO3.setMasterDataAttribute(masterDataAttribute3);
    MasterDataProductAttributeValueDTO masterDataProductAttributeValueDTO = new MasterDataProductAttributeValueDTO();
    masterDataProductAttributeValueDTO.setAllowedAttributeValue(new MasterDataAllowedAttributeValueDTO());
    MasterDataProductAttributeValueDTO masterDataProductAttributeValueDTO2 = new MasterDataProductAttributeValueDTO();
    masterDataProductAttributeValueDTO2.setPredefinedAllowedAttributeValue(new PredefinedAllowedAttributeValueDTO());
    MasterDataProductAttributeValueDTO masterDataProductAttributeValueDTO3 = new MasterDataProductAttributeValueDTO();
    masterDataProductAttributeValueDTO3.setDescriptiveAttributeValueType(DescriptiveAttributeValueType.NONE);
    MasterDataProductAttributeValueDTO masterDataProductAttributeValueDTO4 = new MasterDataProductAttributeValueDTO();
    masterDataProductAttributeValueDTO4.setDescriptiveAttributeValueType(DescriptiveAttributeValueType.SINGLE);
    MasterDataProductAttributeValueDTO masterDataProductAttributeValueDTO5 = new MasterDataProductAttributeValueDTO();
    masterDataProductAttributeValueDTO5.setDescriptiveAttributeValueType(DescriptiveAttributeValueType.MULTIPLE);
    MasterDataProductAttributeValueDTO masterDataProductAttributeValueDTO6 = new MasterDataProductAttributeValueDTO();
    masterDataProductAttributeValueDTO6.setDescriptiveAttributeValueType(DescriptiveAttributeValueType.PREDEFINED);
    masterDataProductAttributeDTO.setMasterDataProductAttributeValues(Arrays
        .asList(masterDataProductAttributeValueDTO, masterDataProductAttributeValueDTO2,
            masterDataProductAttributeValueDTO3, masterDataProductAttributeValueDTO4,
            masterDataProductAttributeValueDTO5, masterDataProductAttributeValueDTO6));
    MasterDataProductDTO masterDataProductDTO = new MasterDataProductDTO();
    masterDataProductDTO.setMasterDataProductImages(Arrays.asList(masterDataProductImageDTO));
    masterDataProductDTO.setMasterDataProductAttributes(
        Arrays.asList(masterDataProductAttributeDTO, masterDataProductAttributeDTO2, masterDataProductAttributeDTO3));
    masterDataProductDTO.setDescription("Description");
    masterDataProductDTO.setLongDescription("Description");
    productResponse.setMasterDataProduct(masterDataProductDTO);
    MasterCatalogDTO masterCatalogDTO = new MasterCatalogDTO();
    masterCatalogDTO.setCategory(new CategoryDTO());
    productResponse.setMasterCatalog(masterCatalogDTO);
    MasterDataItemDTO masterDataItemDTO = new MasterDataItemDTO();
    masterDataItemDTO.setHash(new String());
    MasterDataItemImageDTO masterDataItemImageDTO = new MasterDataItemImageDTO();
    masterDataItemImageDTO.setLocationPath("MTA-XXXXXXX/locationPath");
    masterDataItemDTO.setMasterDataItemImages(Arrays.asList(masterDataItemImageDTO));
    MasterDataItemAttributeValueDTO masterDataItemAttributeValueDTO = new MasterDataItemAttributeValueDTO();
    masterDataItemAttributeValueDTO.setMasterDataAttribute(new MasterDataAttributeDTO());
    masterDataItemDTO.setMasterDataItemAttributeValues(Arrays.asList(masterDataItemAttributeValueDTO));
    itemResponse.setMasterDataItem(masterDataItemDTO);
    productAndItemsResponse.setProduct(productResponse);
    productAndItemsResponse.setItems(Arrays.asList(itemResponse));

    productImageQcProcessingResponse.setProductCode(PRODUCT_CODE);
    productImageQcProcessingResponse.setForceReview(true);
    productImageQcProcessingResponse.setProductPredictionScore(100);
    productImageQcProcessingResponse.setImageViolations("blur_predictions");
    productImageQcProcessingResponse.setImageQcResponse(
        "{\"timestamp\":1595003740556,\"productCode\":\"MTA-0451837\",\"images\":[{\"locationPath\":\"/filestore/mta/images/source/MTA-0451837/nike_test_image_qc_full01_uedfzhqd.jpeg\",\"hashCode\":\"2246ef20d75fd39de86a15adb39fc74b\",\"predictions\":[{\"predictionType\":\"watermark_predictions\",\"displayName\":\"Watermark\",\"present\":false,\"confidence\":0},{\"predictionType\":\"blur_predictions\",\"displayName\":\"Blur\",\"present\":true,\"confidence\":78},{\"predictionType\":\"text_predictions\",\"displayName\":\"Text\",\"present\":false,\"confidence\":22},{\"predictionType\":\"nsfw_predictions\",\"displayName\":\"Pornography\",\"present\":false,\"confidence\":4}]},{\"locationPath\":\"/filestore/mta/images/source/MTA-0451837/nike_test_image_qc_full02_gqm86hzf.jpeg\",\"hashCode\":\"d8ed7c1e732a350f9af0ffe0a2e28bca\",\"predictions\":[{\"predictionType\":\"watermark_predictions\",\"displayName\":\"Watermark\",\"present\":false,\"confidence\":0},{\"predictionType\":\"blur_predictions\",\"displayName\":\"Blur\",\"present\":true,\"confidence\":96},{\"predictionType\":\"text_predictions\",\"displayName\":\"Text\",\"present\":false,\"confidence\":5},{\"predictionType\":\"nsfw_predictions\",\"displayName\":\"Pornography\",\"present\":false,\"confidence\":0}]}],\"success\":true,\"errorMessage\":\"\"}");

    image.setLocationPath("/xyz");
    image.setMarkForDelete(false);
    image.setMainImage(true);
    image.setSequence(1);
    image.setReviewType("");

    Attribute attribute = new Attribute();
    attribute.setAttributeCode(ATTRIBUTE_CODE);
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setAttribute(attribute);
    ProductLevel3Attribute productLevel3Attribute = new ProductLevel3Attribute();
    productLevel3Attribute.setAttributeCode(ATTRIBUTE_CODE);
    productLevel3Attribute.setSkuValue(true);
    product = new Product();
    product.setProductAttributes(new ArrayList<>());
    product.getProductAttributes().add(productAttribute);
    productLevel3 = new ProductLevel3();
    productLevel3.setAttributes(Arrays.asList(productLevel3Attribute));
    productLevel3.setDescription(DEFAULT_PRODUCT_DESCRIPTION);

    screeningProductBulkActionsRequest = new ScreeningProductBulkActionsRequest();
    screeningProductBulkActionsRequest.setProductCodes(Arrays.asList(PRODUCT_CODE));
    screeningProductBulkActionsRequest.setAllVariants(true);
    screeningProductBulkActionsRequest.setVendorErrorFields(ERROR_FIELDS);
    screeningProductBulkActionsRequest.setVendorNotes(Arrays.asList(CORRECTION_REASON));
    screeningProductBulkActionsRequest.setImageReason(Arrays.asList(CORRECTION_REASON_1));
    screeningProductBulkActionsRequest.setContentAdditionalNotes(ADDITIONAL_NOTES);
    screeningProductBulkActionsRequest.setImagesAdditionalNotes(ADDITIONAL_NOTES_1);

    ProductItemWholesalePrice productItemWholesalePrice1 = new ProductItemWholesalePrice();
    productItemWholesalePrice1.setItemSku(ITEM_SKU);
    productItemWholesalePrice1.setItemCode(ITEM_CODE);
    productItemWholesalePrice1.setWholesalePriceActivated(true);
    productItemWholesalePrice1.setWholesaleRules(WHOLESALE_RULE_1);

    ProductItemWholesalePrice productItemWholesalePrice2 = new ProductItemWholesalePrice();
    productItemWholesalePrice2.setItemSku(ITEM_SKU);
    productItemWholesalePrice2.setItemCode(ITEM_CODE);
    productItemWholesalePrice2.setWholesalePriceActivated(true);

    productItemWholesalePrices = Arrays.asList(productItemWholesalePrice1, productItemWholesalePrice2);

    productImage.setLocationPath(LOCATION_PATH_1);
    productImage.setHashCode(LOCATION_PATH_1);
    imageQcResponse.setLocationPath(LOCATION_PATH_1);
    imageQcResponse.setPredictions(Arrays.asList(new ImageQcPredictionResponse()));

    restrictedKeywordsByField = new RestrictedKeywordsByField(RestrictedKeywordFieldNames.PRODUCT_NAME.name(),
        Arrays.asList(RESTRICTED_KEYWORDS));
    bundleRecipeVo = new BundleRecipeVo();
    bundleRecipeVo.setItemSku(ITEM_SKU);
    bundleRecipeVo.setQuantity(QUANTITY);
    restrictedKeywordJson = new ObjectMapper().writeValueAsString(Arrays.asList(restrictedKeywordsByField));
    bundleRecipeJson = new ObjectMapper().writeValueAsString(Arrays.asList(bundleRecipeVo));

    productImagePrediction = new ProductImagePrediction();
    productImagePrediction.setDisplayName(DISPLAY_NAME);
    productImagePrediction.setPredictionType(PREDICTION_TYPE);
    productImagePrediction.setConfidenceThreshold(50);
    productImagePrediction.setForceReview(false);
    productImagePredictionList = new ArrayList<>();
    productImagePredictionList.add(productImagePrediction);

    productImagePredictionResponse = new ProductImagePredictionResponse();
    productImagePredictionResponse.setPredictionType(ADULT_PREDICTION_TYPE);
    productImagePredictionResponse.setDisplayName(DISPLAY_NAME);
    productImagePredictionResponse.setConfidenceThreshold(50);
    productImagePredictionResponse.setPredictionConsidered(true);

    updatedProductHistory.setGdnSku(Constants.DEFAULT);

    businessPartnerPickupPointResponse.setCode(PICKUP_POINT_CODE);
    businessPartnerPickupPointResponse.setName(PICKUP_POINT_NAME);
    historyUpdateResponse.setPickupPointCode(PICKUP_POINT_CODE);

    categoryResponse1.setCategoryCode(CATEGORY_CODE1);
    categoryResponse1.setName(CATEGORY_NAME1);
    categoryResponse2.setCategoryCode(CATEGORY_CODE2);
    categoryResponse2.setName(CATEGORY_NAME2);
    categoryResponses.addAll(Arrays.asList(categoryResponse2, categoryResponse1));
    itemResponseV2 = new ItemResponseV2();
    itemResponseV2.setB2BResponse(new B2BResponse());
    itemResponseV2.setProductSku(PRODUCT_SKU);
    itemResponseV2.setProductName(PRODUCT_NAME);
    itemResponseV2.setItemSku(ITEM_SKU);
    itemResponseV2.setProductCode(PRODUCT_CODE);
    itemResponseV2.setPickUpPointCode(PICKUP_POINT_CODE);
    itemResponseV2.setItemName(ITEM_NAME);
    itemResponseV2.setSkuCode(SKU_CODE);
    itemResponseV2.setMerchantCode(SKU_CODE);
    itemResponseV2.setCncActive(true);
    itemResponseV2.setPrices(new ArrayList<>());
    itemResponseV2.setViewConfigs(new ArrayList<>());
  }

  @Test
  public void toProductImagePredictionResponseTest() {
    ProductImagePredictionResponse response = ConverterUtil.toProductImagePredictionResponse(productImagePrediction);
    Assertions.assertEquals(DISPLAY_NAME, response.getDisplayName());
    Assertions.assertEquals(PREDICTION_TYPE, response.getPredictionType());
    Assertions.assertEquals(50, response.getConfidenceThreshold());
    Assertions.assertEquals(false, response.isForceReview());
  }

  @Test
  public void toListOfProductImagePredictionResponseList() {
    List<ProductImagePredictionResponse> response =
        ConverterUtil.toListOfProductImagePredictionResponse(productImagePredictionList);
    Assertions.assertEquals(1, response.size());
  }

  @Test
  public void toScreeningSolrProductCollectionAddEvent() throws Exception {
    productCollection =
        new ProductCollection(PRODUCT_ID, PRODUCT_CODE, PRODUCT_NAME, BRAND, CATEGORY_CODE, CATEGORY_NAME,
            BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_NAME, false, false, STATE, CREATED_BY, CREATED_DATE, STORE_ID);
    productCollection.setAssignedTo(ASSIGNED_TO);
    productCollection.setAssignedBy(ASSIGNED_BY);
    productCollection.setBrandApprovalStatus(BrandApprovalStatus.APPROVED);
    SolrReviewProductCollectionAddEventFields solrReviewProductCollectionAddEvent =
        ConverterUtil.toScreeningSolrProductCollectionAddEvent(productCollection);
    Assertions.assertNotNull(productCollection);
    Assertions.assertEquals(productCollection.getProductCode(), PRODUCT_CODE);
    assertTrue(solrReviewProductCollectionAddEvent.isBrandApproved());
  }

  @Test
  public void toSolrInputDocument() {
    solrReviewProductCollectionAddEventFields =
        SolrReviewProductCollectionAddEventFields.builder().storeId(STORE_ID).productCode(PRODUCT_CODE)
            .productId(PRODUCT_ID).productName(PRODUCT_NAME).brand(BRAND).businessPartnerCode(BUSINESS_PARTNER_CODE)
            .businessPartnerName(BUSINESS_PARTNER_NAME).updatedStepDate(UPDATED_STEP_DATE).updatedDate(UPDATED_DATE)
            .createdDate(CREATED_DATE).createdBy(CREATED_BY).updatedBy(UPDATED_BY).assignedTo(ASSIGNED_TO)
            .activated(false).viewable(false).resubmitCount(RESUBMIT_COUNT)
            .categoryCodes(Collections.singletonList(CATEGORY_CODE))
            .categoryNames(Collections.singletonList(CATEGORY_NAME)).submittedDate(SUBMITTED_DATE).state(STATE).build();
    SolrInputDocument solrInputDocument = ConverterUtil.toSolrInputDocument(solrReviewProductCollectionAddEventFields);
    Assertions.assertNotNull(solrInputDocument);
    Assertions.assertEquals(PRODUCT_CODE, solrInputDocument.getFieldValue(SolrFieldNames.PRODUCT_CODE));
  }

  @Test
  public void toProductRevisionInfoResponseTest() {
    ProductHistory productHistory = mockProductHistory();
    productHistory.setNotes(NOTES + Constants.DASH_DELIMITER + ADDITIONAL_NOTES);
    productHistory.setNeedCorrectionNotes(REVISION_NOTES);
    ProductRevisionInfoResponse productRevisionInfoResponse =
        ConverterUtil.toProductRevisionInfoResponse(productHistory);
    Assertions.assertEquals(ADDITIONAL_NOTES, productRevisionInfoResponse.getAdditionalNotes());
    Assertions.assertEquals(NOTES, productRevisionInfoResponse.getCorrectionReason());
  }
  @Test
  public void toProductRevisionInfoResponseVendorNotesNullTest() {
    String revisionNotes =
        "{\"ContentAdditionalNotes\":\"string\",\"assignTo\":\"string\",\"assignedBy\":\"string\",\"vendorNotes\":null,\"vendorErrorFields\":[\"Video Url\",\"description\"],\"allVariants\":true,\"imagesAdditionalNotes\":\"notes\",\"imageReason\":[\"Blur or inappropriate images\"],\"productCodes\":[\"string\"],\"rejectionReason\":\"string\",\"itemNotes\":[{\"itemSku\":\"TOQ-15126-01980-00007\",\"skuCode\":\"MTA-0451490-00007\",\"itemNumber\":1,\"itemName\":\"itemName-1\",\"vendorNotes\":[\"Blur or inappropriate images\"],\"vendorErrorFields\":[\"images\"]},{\"itemSku\":\"TOQ-15126-01980-00008\",\"skuCode\":\"MTA-0451490-00008\",\"itemNumber\":2,\"itemName\":\"itemName-2\",\"vendorNotes\":[\"Blur or inappropriate images\"],\"vendorErrorFields\":[\"images\"]}]}";
    ProductHistory productHistory = mockProductHistory();
    productHistory.setNotes(NOTES + Constants.DASH_DELIMITER + ADDITIONAL_NOTES);
    productHistory.setNeedCorrectionNotes(revisionNotes);
    ProductRevisionInfoResponse productRevisionInfoResponse =
        ConverterUtil.toProductRevisionInfoResponse(productHistory);
    Assertions.assertEquals(ADDITIONAL_NOTES, productRevisionInfoResponse.getAdditionalNotes());
    Assertions.assertEquals(NOTES, productRevisionInfoResponse.getCorrectionReason());
  }

  @Test
  public void toProductRevisionInfoResponseWithoutAdditionalNotesTest() {
    ProductHistory productHistory = mockProductHistory();
    productHistory.setNotes(NOTES);
    ProductRevisionInfoResponse productRevisionInfoResponse =
        ConverterUtil.toProductRevisionInfoResponse(productHistory);
    Assertions.assertNull(productRevisionInfoResponse.getAdditionalNotes());
    Assertions.assertEquals(NOTES, productRevisionInfoResponse.getCorrectionReason());
  }

  @Test
  public void toProductRevisionInfoResponseWithMoreThanOneDashTest() {
    ProductHistory productHistory = mockProductHistory();
    productHistory
        .setNotes(NOTES + Constants.DASH_DELIMITER + ADDITIONAL_NOTES + Constants.DASH_DELIMITER + ADDITIONAL_NOTES);
    ProductRevisionInfoResponse productRevisionInfoResponse =
        ConverterUtil.toProductRevisionInfoResponse(productHistory);
    Assertions.assertNotNull(productRevisionInfoResponse.getAdditionalNotes());
    Assertions.assertEquals(NOTES, productRevisionInfoResponse.getCorrectionReason());
    Assertions.assertEquals(ADDITIONAL_NOTES + Constants.DASH_DELIMITER + ADDITIONAL_NOTES,
        productRevisionInfoResponse.getAdditionalNotes());
  }

  @Test
  public void toProductRevisionInfoResponse_ErrorTest() {
    ProductHistory productHistory = mockProductHistory();
    productHistory.setNotes(NOTES);
    productHistory.setNeedCorrectionNotes("{");
    ProductRevisionInfoResponse productRevisionInfoResponse = ConverterUtil.toProductRevisionInfoResponse(productHistory);
    Assertions.assertEquals(NOTES, productRevisionInfoResponse.getCorrectionReason());
  }

  @Test
  public void convertRejectedSkuProductCollectionToRejectedSkuProductResponseTest() {
    List<RejectedSkuProductResponse> response =
        ConverterUtil.convertRejectedSkuProductCollectionToRejectedSkuProductResponse(rejectedSkuProductCollectionList);
    Assertions.assertEquals(PRODUCT_CODE, response.get(0).getProductCode());
    Assertions.assertEquals(PRODUCT_NAME, response.get(0).getProductName());
    Assertions.assertEquals(CATEGORY_NAME, response.get(0).getCategoryName());
  }

  @Test
  public void convertProductItemAttributeValueRequestToProductItemAttributeValueTest() {
    ProductItemAttributeValue response = ConverterUtil
        .convertProductItemAttributeValueRequestToProductItemAttributeValue(productItemAttributeValueRequest);
    assertTrue(response.getAttribute().isVariantCreatingUI());
    assertTrue(response.getAttribute().isScreeningMandatory());
    Assertions.assertEquals(DEFAULT_ATTRIBUTE_NAME, response.getAttribute().getName());
    Assertions.assertEquals(DEFAULT_ATTRIBUTE_VALUE, response.getValue());
  }

  private ProductHistory mockProductHistory() {
    ProductHistory productHistory = new ProductHistory();
    productHistory.setCreatedBy(CREATED_BY);
    productHistory.setCreatedDate(CREATED_DATE);
    productHistory.setUpdatedBy(UPDATED_BY);
    productHistory.setUpdatedDate(UPDATED_DATE);
    productHistory.setStoreId(STORE_ID);
    productHistory.setId(ID);
    return productHistory;
  }

  @Test
  public void toProductFilterResponseTest() {
    ProductFilterResponse productFilterResponse = ConverterUtil.toProductFilterResponse(solrProductCollectionDTO);
    Assertions.assertEquals(PRODUCT_CODE, productFilterResponse.getProductCode());
    Assertions.assertEquals(PRODUCT_NAME, productFilterResponse.getProductName());
    Assertions.assertEquals(BRAND, productFilterResponse.getBrandName());
    Assertions.assertEquals(CATEGORY_NAME, productFilterResponse.getCategoryName());
  }

  private List<AutoApprovalRules> generateAutoApprovalRules() {
    AutoApprovalRules autoApprovalRules = new AutoApprovalRules();
    autoApprovalRules.setStoreId(DEFAULT_STORE_ID);
    autoApprovalRules.setCreatedBy(DEFAULT_CREATED_BY);
    autoApprovalRules.setUpdatedDate(new Date());
    autoApprovalRules.setUpdatedBy(DEFAULT_UPDATED_BY);
    autoApprovalRules.setVersion(DEFAULT_VERSION);
    autoApprovalRules.setCreatedDate(new Date());
    autoApprovalRules.setSequenceNumber(SEQUENCE_NUMBER);
    autoApprovalRules.setRuleName(RULE_NAME);
    autoApprovalRules.setRuleConfig(RULE_CONFIG);
    autoApprovalRules.setImageQcConfig(IMAGE_QC_CONFIG);
    autoApprovalRules.setAutoApprovalType(AutoApprovalType.CONTENT_AND_IMAGE);
    autoApprovalRules.setNeedRevisionEnabled(true);
    autoApprovalRules.setNeedRevisionConfig(IMAGE_QC_CONFIG);
    List<AutoApprovalRules> autoApprovalRulesList = new ArrayList<>();
    autoApprovalRulesList.add(autoApprovalRules);
    return autoApprovalRulesList;
  }

  @Test
  public void toAutoApprovalRulesDtoListTest() throws Exception {
    AutoApprovalRulesListResponse autoApprovalRulesListResponse =
        ConverterUtil.toAutoApprovalRulesListResponse(generateAutoApprovalRules(),
            Collections.singletonList(productImagePredictionResponse));
    Assertions.assertEquals(1, autoApprovalRulesListResponse.getAutoApprovalRulesDtoList().size());
    assertTrue(autoApprovalRulesListResponse.getAutoApprovalRulesDtoList().get(0).isNeedRevisionConfigEnabled());
    Assertions.assertEquals(4,
        autoApprovalRulesListResponse.getAutoApprovalRulesDtoList().get(0).getNeedRevisionImageQcConfig().size());
  }

  @Test
  public void toAutoApprovalRulesDtoListPredictionConsideredFalseTest() throws Exception {
    productImagePredictionResponse.setPredictionConsidered(false);
    AutoApprovalRulesListResponse autoApprovalRulesListResponse =
        ConverterUtil.toAutoApprovalRulesListResponse(generateAutoApprovalRules(),
            Collections.singletonList(productImagePredictionResponse));
    Assertions.assertEquals(1, autoApprovalRulesListResponse.getAutoApprovalRulesDtoList().size());
    assertTrue(autoApprovalRulesListResponse.getAutoApprovalRulesDtoList().get(0).isNeedRevisionConfigEnabled());
    Assertions.assertEquals(3,
        autoApprovalRulesListResponse.getAutoApprovalRulesDtoList().get(0).getNeedRevisionImageQcConfig().size());
  }

  @Test
  public void toAutoApprovalRulesDtoListTest_Exception() {
    Exception exception = new Exception();
    try {
      ConverterUtil.toAutoApprovalRulesListResponse(null, new ArrayList<>());
    } catch (Exception e) {
      exception = e;
    } finally {
      assertTrue(exception instanceof NullPointerException);
    }
  }

  @Test
  public void toImageQcRequestTest() {
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setDescription(new byte[1]);
    ImageQcRequestDomainEvent request =
        ConverterUtil.toImageQcRequest(bulkImageProcessResponse, IMAGE_SOURCE_DIRECTORY, productDetailResponse,
            GCS_PREFIX_PATH, GCS_SOURCE_URL, new RestrictedKeywordsByFieldAndActionType());
    Assertions.assertEquals(PRODUCT_CODE, request.getProductCode());
    Assertions.assertEquals(2, request.getImages().size());
    Assertions.assertEquals(IMAGE_SOURCE_DIRECTORY + Constants.DELIMITER_SLASH + LOCATION_PATH_1,
        request.getImages().get(0).getLocationPath());
    Assertions.assertEquals(GCS_SOURCE_URL + Constants.DELIMITER_SLASH + LOCATION_PATH_2,
        request.getImages().get(1).getLocationPath());
    Assertions.assertEquals(HASH_CODE_1, request.getImages().get(0).getHashCode());
    Assertions.assertEquals(HASH_CODE_2, request.getImages().get(1).getHashCode());
  }

  @Test
  public void toImageQcRequestTest1() {
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setDescription(new byte[1]);
    ImageQcRequestDomainEvent request =
        ConverterUtil.toImageQcRequest(bulkImageProcessResponse, IMAGE_SOURCE_DIRECTORY, productDetailResponse,
            GCS_PREFIX_PATH, GCS_SOURCE_URL, null);
    Assertions.assertEquals(PRODUCT_CODE, request.getProductCode());
    Assertions.assertEquals(2, request.getImages().size());
    Assertions.assertEquals(IMAGE_SOURCE_DIRECTORY + Constants.DELIMITER_SLASH + LOCATION_PATH_1,
        request.getImages().get(0).getLocationPath());
    Assertions.assertEquals(GCS_SOURCE_URL + Constants.DELIMITER_SLASH + LOCATION_PATH_2,
        request.getImages().get(1).getLocationPath());
    Assertions.assertEquals(HASH_CODE_1, request.getImages().get(0).getHashCode());
    Assertions.assertEquals(HASH_CODE_2, request.getImages().get(1).getHashCode());
  }

  @Test
  public void toImageQcRequestTest3() {
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setDescription(new byte[1]);
    RestrictedKeywordsByFieldAndActionType restrictedKeywordsByFieldAndActionType =
        new RestrictedKeywordsByFieldAndActionType();
    RestrictedKeywordsByField restrictedKeywordsByField1 = new RestrictedKeywordsByField();
    restrictedKeywordsByField1.setKeywords(Collections.singletonList(SELLER_SKU));
    restrictedKeywordsByFieldAndActionType.setRestrictedKeywordsByFieldList(
        Collections.singletonList(restrictedKeywordsByField1));
    ImageQcRequestDomainEvent request =
        ConverterUtil.toImageQcRequest(bulkImageProcessResponse, IMAGE_SOURCE_DIRECTORY, productDetailResponse,
            GCS_PREFIX_PATH, GCS_SOURCE_URL, restrictedKeywordsByFieldAndActionType);
    Assertions.assertEquals(PRODUCT_CODE, request.getProductCode());
    Assertions.assertEquals(2, request.getImages().size());
    Assertions.assertEquals(IMAGE_SOURCE_DIRECTORY + Constants.DELIMITER_SLASH + LOCATION_PATH_1,
        request.getImages().get(0).getLocationPath());
    Assertions.assertEquals(GCS_SOURCE_URL + Constants.DELIMITER_SLASH + LOCATION_PATH_2,
        request.getImages().get(1).getLocationPath());
    Assertions.assertEquals(HASH_CODE_1, request.getImages().get(0).getHashCode());
    Assertions.assertEquals(HASH_CODE_2, request.getImages().get(1).getHashCode());
  }

  @Test
  public void toImageQcRequestTest5() {
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setDescription(new byte[1]);
    RestrictedKeywordsByFieldAndActionType restrictedKeywordsByFieldAndActionType =
        new RestrictedKeywordsByFieldAndActionType();
    RestrictedKeywordsByField restrictedKeywordsByField1 = new RestrictedKeywordsByField();
    restrictedKeywordsByField1.setKeywords(Collections.singletonList(SELLER_SKU));
    Map<String, KeywordRequestDTO> keywordIdAndKeywordMap = new HashMap<>();
    KeywordRequestDTO keywordRequestDTO = new KeywordRequestDTO();
    keywordRequestDTO.setValidateByDs(true);
    keywordRequestDTO.setKeywordId(SKU_CODE);
    keywordIdAndKeywordMap.put(SELLER_SKU, keywordRequestDTO);
    restrictedKeywordsByFieldAndActionType.setKeywordToKeywordRequestDTOMap(keywordIdAndKeywordMap);
    restrictedKeywordsByFieldAndActionType.setRestrictedKeywordsByFieldList(
        Collections.singletonList(restrictedKeywordsByField1));
    ImageQcRequestDomainEvent request =
        ConverterUtil.toImageQcRequest(bulkImageProcessResponse, IMAGE_SOURCE_DIRECTORY, productDetailResponse,
            GCS_PREFIX_PATH, GCS_SOURCE_URL, restrictedKeywordsByFieldAndActionType);
    Assertions.assertEquals(PRODUCT_CODE, request.getProductCode());
    Assertions.assertEquals(2, request.getImages().size());
    Assertions.assertEquals(IMAGE_SOURCE_DIRECTORY + Constants.DELIMITER_SLASH + LOCATION_PATH_1,
        request.getImages().get(0).getLocationPath());
    Assertions.assertEquals(GCS_SOURCE_URL + Constants.DELIMITER_SLASH + LOCATION_PATH_2,
        request.getImages().get(1).getLocationPath());
    Assertions.assertEquals(HASH_CODE_1, request.getImages().get(0).getHashCode());
    Assertions.assertEquals(HASH_CODE_2, request.getImages().get(1).getHashCode());
    Assertions.assertEquals(SELLER_SKU, request.getRestrictedKeywords().get(0).getKeyword());
    Assertions.assertEquals(SKU_CODE, request.getRestrictedKeywords().get(0).getKeywordId());
    Assertions.assertEquals(true, request.getRestrictedKeywords().get(0).isValidateByDs());
  }

  @Test
  public void toImageQcRequestTest7() {
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setDescription(new byte[1]);
    RestrictedKeywordsByFieldAndActionType restrictedKeywordsByFieldAndActionType =
      new RestrictedKeywordsByFieldAndActionType();
    RestrictedKeywordsByField restrictedKeywordsByField1 = new RestrictedKeywordsByField();
    restrictedKeywordsByField1.setKeywords(Collections.singletonList(SELLER_SKU));
    Map<String, KeywordRequestDTO> keywordIdAndKeywordMap = new HashMap<>();
    keywordIdAndKeywordMap.put(SKU_CODE, new KeywordRequestDTO());
    restrictedKeywordsByFieldAndActionType.setKeywordToKeywordRequestDTOMap(keywordIdAndKeywordMap);
    restrictedKeywordsByFieldAndActionType.setRestrictedKeywordsByFieldList(
      Collections.singletonList(restrictedKeywordsByField1));
    ImageQcRequestDomainEvent request =
      ConverterUtil.toImageQcRequest(bulkImageProcessResponse, IMAGE_SOURCE_DIRECTORY,
        productDetailResponse, GCS_PREFIX_PATH, GCS_SOURCE_URL,
        restrictedKeywordsByFieldAndActionType);
    Assertions.assertEquals(PRODUCT_CODE, request.getProductCode());
    Assertions.assertEquals(2, request.getImages().size());
    Assertions.assertEquals(IMAGE_SOURCE_DIRECTORY + Constants.DELIMITER_SLASH + LOCATION_PATH_1,
      request.getImages().get(0).getLocationPath());
    Assertions.assertEquals(GCS_SOURCE_URL + Constants.DELIMITER_SLASH + LOCATION_PATH_2,
      request.getImages().get(1).getLocationPath());
    Assertions.assertEquals(HASH_CODE_1, request.getImages().get(0).getHashCode());
    Assertions.assertEquals(HASH_CODE_2, request.getImages().get(1).getHashCode());
    assertTrue(request.getRestrictedKeywords().isEmpty());
  }

  @Test
  public void toImageQcRequestTest6() {
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setDescription(new byte[1]);
    RestrictedKeywordsByFieldAndActionType restrictedKeywordsByFieldAndActionType =
        new RestrictedKeywordsByFieldAndActionType();
    restrictedKeywordsByFieldAndActionType.setKeywordAndIdMap(null);
    RestrictedKeywordsByField restrictedKeywordsByField1 = new RestrictedKeywordsByField();
    restrictedKeywordsByField1.setKeywords(Collections.singletonList(SELLER_SKU));
    restrictedKeywordsByFieldAndActionType.setRestrictedKeywordsByFieldList(
        Collections.singletonList(restrictedKeywordsByField1));
    ImageQcRequestDomainEvent request =
        ConverterUtil.toImageQcRequest(bulkImageProcessResponse, IMAGE_SOURCE_DIRECTORY, productDetailResponse,
            GCS_PREFIX_PATH, GCS_SOURCE_URL, restrictedKeywordsByFieldAndActionType);
    Assertions.assertEquals(PRODUCT_CODE, request.getProductCode());
    Assertions.assertEquals(2, request.getImages().size());
    Assertions.assertEquals(IMAGE_SOURCE_DIRECTORY + Constants.DELIMITER_SLASH + LOCATION_PATH_1,
        request.getImages().get(0).getLocationPath());
    Assertions.assertEquals(GCS_SOURCE_URL + Constants.DELIMITER_SLASH + LOCATION_PATH_2,
        request.getImages().get(1).getLocationPath());
    Assertions.assertEquals(HASH_CODE_1, request.getImages().get(0).getHashCode());
    Assertions.assertEquals(HASH_CODE_2, request.getImages().get(1).getHashCode());
    assertTrue(request.getRestrictedKeywords().isEmpty());
  }

  @Test
  public void toImageQcRequestTest4() {
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setDescription(new byte[1]);
    RestrictedKeywordsByFieldAndActionType restrictedKeywordsByFieldAndActionType =
        new RestrictedKeywordsByFieldAndActionType();
    RestrictedKeywordsByField restrictedKeywordsByField1 = new RestrictedKeywordsByField();
    restrictedKeywordsByFieldAndActionType.setRestrictedKeywordsByFieldList(
        Collections.singletonList(restrictedKeywordsByField1));
    ImageQcRequestDomainEvent request =
        ConverterUtil.toImageQcRequest(bulkImageProcessResponse, IMAGE_SOURCE_DIRECTORY, productDetailResponse,
            GCS_PREFIX_PATH, GCS_SOURCE_URL, restrictedKeywordsByFieldAndActionType);
    Assertions.assertEquals(PRODUCT_CODE, request.getProductCode());
    Assertions.assertEquals(2, request.getImages().size());
    Assertions.assertEquals(IMAGE_SOURCE_DIRECTORY + Constants.DELIMITER_SLASH + LOCATION_PATH_1,
        request.getImages().get(0).getLocationPath());
    Assertions.assertEquals(GCS_SOURCE_URL + Constants.DELIMITER_SLASH + LOCATION_PATH_2,
        request.getImages().get(1).getLocationPath());
    Assertions.assertEquals(HASH_CODE_1, request.getImages().get(0).getHashCode());
    Assertions.assertEquals(HASH_CODE_2, request.getImages().get(1).getHashCode());
  }

  @Test
  public void toPriceRequestTest() {
    PriceRequest priceRequest = ConverterUtil.toPriceRequest(productPriceAndWholesaleRequest);
    Assertions.assertEquals(DEFAULT_CHANNEL, priceRequest.getChannel());
    Assertions.assertEquals(SALE_PRICE, priceRequest.getOfferPrice(), 0.0);
    Assertions.assertEquals(NORMAL_PRICE, priceRequest.getListPrice(), 0.0);
    Assertions.assertEquals(DEFAULT_CURRENCY, priceRequest.getCurrency());
    Assertions.assertEquals(AdjustmentTypeEnum.BLIBLI.getDescription(),
        priceRequest.getDiscountPrice().get(0).getAdjustmentType().getDescription());
    Assertions.assertEquals(DISCOUNT_AMOUNT, priceRequest.getDiscountPrice().get(0).getDiscountPrice(), 0.0);
  }

  @Test
  public void getLogAuditTrailUpdatedOfflineProductResponseWrapperTest() {
    UpdatedProductHistory updatedProductHistory = new UpdatedProductHistory();
    updatedProductHistory.setAccessTime(new Date());
    Page<UpdatedProductHistory> updatedProductHistories =
        new PageImpl<>(Collections.singletonList(updatedProductHistory));
    ConverterUtil.toLogAuditTrailUpdatedOfflineProductResponses(updatedProductHistories);
    Assertions.assertNotNull(updatedProductHistories);
  }

  @Test
  public void toImageQcProcessedResponseDomainEventFromProcessedResponseTest() {
    ImageQcProcessedResponse imageQcProcessedResponse = new ImageQcProcessedResponse();
    imageQcProcessedResponse.setProductCode(PRODUCT_CODE);
    imageQcProcessedResponse.setPredictedBrand(BRAND);
    ImageQcProcessedResponseDomainEvent response =
        ConverterUtil.toImageQcProcessedResponseDomainEventFromProcessedResponse(imageQcProcessedResponse);
    Assertions.assertEquals(PRODUCT_CODE, response.getProductCode());
    Assertions.assertEquals(BRAND, response.getPredictedBrand());
  }

  @Test
  public void constructItemL5ListingResponseTest() {
    productDatas.getFirst().setPreOrder(
        PreOrderDTO.builder().isPreOrder(Boolean.TRUE).preOrderDate(DateUtils.addDays(new Date(), 2)).build());
    BulkDownloadProductLevel3Response downloadProductLevel3Response =
        ConverterUtil.constructItemL5ListingResponse(productDatas, inventoryDatas);
    Assertions.assertEquals(PRODUCT_SKU,downloadProductLevel3Response.getProductLevel3SummaryResponses().get(0).getProductSku());
    Assertions.assertEquals(PRICE,downloadProductLevel3Response.getProductLevel3SummaryResponses().get(0).getPrices().get(0).getPrice());
    Assertions.assertEquals(DISPLAY,downloadProductLevel3Response.getProductLevel3SummaryResponses().get(0).getViewConfigs().get(0).getDisplay());
    Assertions.assertEquals(WAREHOUSE_AVAILABLE,downloadProductLevel3Response.getProductLevel3SummaryResponses().get(0).getAvailableStockLevel1());
    Assertions.assertEquals(PRODUCT_CODE, downloadProductLevel3Response.getProductLevel3SummaryResponses().get(0).getProductCode());
    assertTrue(downloadProductLevel3Response.getProductLevel3SummaryResponses().get(0).isPreOrder());
    assertNotNull(downloadProductLevel3Response.getProductLevel3SummaryResponses().get(0).getPreOrderDate());
  }

  @Test
  public void constructItemL5ListingResponse_Null_ItemSkus_Test() {
    itemLevel5Response.setItemSku(ITEM_SKU_2);
    BulkDownloadProductLevel3Response downloadProductLevel3Response =
        ConverterUtil.constructItemL5ListingResponse(productDatas, inventoryDatas);
    Assertions.assertEquals(PRODUCT_SKU,downloadProductLevel3Response.getProductLevel3SummaryResponses().get(0).getProductSku());
    Assertions.assertEquals(PRICE,downloadProductLevel3Response.getProductLevel3SummaryResponses().get(0).getPrices().get(0).getPrice());
    Assertions.assertEquals(DISPLAY,downloadProductLevel3Response.getProductLevel3SummaryResponses().get(0).getViewConfigs().get(0).getDisplay());
  }

  @Test
  public void copyToItemImagesL5Test() {
    List<ProductLevel3SummaryDetailsImageRequest> copyToAllVariantImages = new ArrayList<>();
    ProductLevel3SummaryDetailsImageRequest productLevel3SummaryDetailsImageRequest = new ProductLevel3SummaryDetailsImageRequest();
    productLevel3SummaryDetailsImageRequest.setReviewType(NEW);
    productLevel3SummaryDetailsImageRequest.setMainImage(true);
    productLevel3SummaryDetailsImageRequest.setMarkForDelete(false);
    productLevel3SummaryDetailsImageRequest.setSequence(1);
    productLevel3SummaryDetailsImageRequest.setLocationPath(LOCATION_PATH_1);
    copyToAllVariantImages.add(productLevel3SummaryDetailsImageRequest);
    List<Image> images = ConverterUtil.copyToItemImagesL5(copyToAllVariantImages, false);
    Assertions.assertEquals(1, images.size());
    Assertions.assertEquals(LOCATION_PATH_1, images.get(0).getLocationPath());
    Assertions.assertTrue(images.get(0).isMainImages());
  }

  @Test
  public void getImageQcResponseTest() {
    boolean imageQcResponse = ConverterUtil
        .getImageQcResponse(PRODUCT_CODE, imageQcProcessedResponse, productImageQcProcessingResponse,
            ACTIVE_PREDICTIONS);
    assertTrue(imageQcResponse);
  }

  @Test
  public void getImageQcResponseImagesEmptyTest() {
    productImageQcProcessingResponse.setImageQcResponse(IMAGE_QC_WITHOUT_IMAGES);
    boolean imageQcResponse = ConverterUtil
        .getImageQcResponse(PRODUCT_CODE, imageQcProcessedResponse, productImageQcProcessingResponse,
            ACTIVE_PREDICTIONS);
    assertTrue(imageQcResponse);
  }

  @Test
  public void getImageQcResponseExceptionTest() {
    boolean imageQcResponse = ConverterUtil.getImageQcResponse(PRODUCT_CODE, null, null, ACTIVE_PREDICTIONS);
    Assertions.assertFalse(imageQcResponse);
  }

  @Test
  public void getImageQcResponseWrongQcTest() {
    boolean imageQcResponse =
        ConverterUtil.getImageQcResponse(PRODUCT_CODE, imageQcProcessedResponse, null, ACTIVE_PREDICTIONS);
    Assertions.assertFalse(imageQcResponse);
  }

  @Test
  public void getImageQcResponseWrongQcTest2() {
    productImageQcProcessingResponse.setImageQcResponse(PRODUCT_CODE);
    boolean imageQcResponse = ConverterUtil
        .getImageQcResponse(PRODUCT_CODE, imageQcProcessedResponse, productImageQcProcessingResponse,
            ACTIVE_PREDICTIONS);
    Assertions.assertFalse(imageQcResponse);
  }

  @Test
  public void toProductRequestTest() {
    ProductRequest productRequest = ConverterUtil.toProductRequest(productAndItemsResponse);
    Assertions.assertNotNull(productRequest.getProductAttributes());
    Assertions.assertNotNull(productRequest.getProductCategories());
    Assertions.assertNotNull(productRequest.getImages());
    Assertions.assertNotNull(productRequest.getProductItems());
    Assertions.assertNotNull(productRequest.getProductItems().get(0).getImages());
    Assertions.assertNotNull(productRequest.getProductItems().get(0).getProductItemAttributeValues());
    assertTrue(productRequest.isActivated());
    assertTrue(productRequest.isViewable());
    Assertions.assertNotNull(productRequest.getDescription());
    Assertions.assertNotNull(productRequest.getLongDescription());
  }

  @Test
  public void toProductRequestNullTest() {
    ProductAndItemsResponse productAndItemsResponse = new ProductAndItemsResponse();
    ProductResponse productResponse = new ProductResponse();
    MasterDataProductDTO masterDataProductDTO = new MasterDataProductDTO();
    masterDataProductDTO.setDescription(new String());
    masterDataProductDTO.setLongDescription(new String());
    productAndItemsResponse.setProduct(productResponse);
    productResponse.setMasterDataProduct(masterDataProductDTO);
    ProductRequest productRequest = ConverterUtil.toProductRequest(productAndItemsResponse);
    Assertions.assertEquals(Collections.emptyList(), productRequest.getProductAttributes());
    Assertions.assertEquals(Collections.emptyList(), productRequest.getProductCategories());
    Assertions.assertEquals(0, productRequest.getImages().size());
    Assertions.assertEquals(Collections.emptyList(), productRequest.getProductItems());
    Assertions.assertNull(productRequest.getDescription());
    Assertions.assertNull(productRequest.getLongDescription());
  }

  @Test
  public void toProductRequestForEmptyProductImagesTest() {
    productAndItemsResponse.getProduct().getMasterDataProduct().setMasterDataProductImages(null);
    ProductRequest productRequest = ConverterUtil.toProductRequest(productAndItemsResponse);
    Assertions.assertNotNull(productRequest.getProductAttributes());
    Assertions.assertNotNull(productRequest.getProductCategories());
    Assertions.assertNotNull(productRequest.getImages());
    Assertions.assertNotNull(productRequest.getProductItems());
    Assertions.assertNotNull(productRequest.getProductItems().get(0).getImages());
    Assertions.assertNotNull(productRequest.getProductItems().get(0).getProductItemAttributeValues());
    assertTrue(productRequest.isActivated());
    assertTrue(productRequest.isViewable());
    Assertions.assertNotNull(productRequest.getDescription());
    Assertions.assertNotNull(productRequest.getLongDescription());
  }

  @Test
  public void toProductRequestForEmptyProductImagesTest_multipleItems() {
    productAndItemsResponse.getProduct().getMasterDataProduct().setMasterDataProductImages(null);
    productAndItemsResponse.setItems(Arrays.asList(itemResponse, itemResponse));
    ProductRequest productRequest = ConverterUtil.toProductRequest(productAndItemsResponse);
    Assertions.assertNotNull(productRequest.getProductAttributes());
    Assertions.assertNotNull(productRequest.getProductCategories());
    Assertions.assertNotNull(productRequest.getImages());
    Assertions.assertNotNull(productRequest.getProductItems());
    Assertions.assertNotNull(productRequest.getProductItems().get(0).getImages());
    Assertions.assertNotNull(productRequest.getProductItems().get(0).getProductItemAttributeValues());
    assertTrue(productRequest.isActivated());
    assertTrue(productRequest.isViewable());
    Assertions.assertNotNull(productRequest.getDescription());
    Assertions.assertNotNull(productRequest.getLongDescription());
  }

  @Test
  public void toWholesalePriceBulkUpdateRequestTest() {
    ProductPriceStockAndImagesRequest productPriceStockAndImagesRequest = new ProductPriceStockAndImagesRequest();
    ProductItemWholesalePriceRequest productItemWholesalePriceRequest = new ProductItemWholesalePriceRequest();
    productItemWholesalePriceRequest.setQuantity(QUANTITY);
    productItemWholesalePriceRequest.setWholesaleDiscount(WHOLESALE_DISCOUNT);
    productPriceStockAndImagesRequest.setProductItemWholesalePrices(Arrays.asList(productItemWholesalePriceRequest));
    productPriceStockAndImagesRequest.setWholesalePriceActivated(true);
    ItemSummaryResponse itemSummaryResponse = new ItemSummaryResponse();
    itemSummaryResponse.setProductSku(PRODUCT_SKU);
    itemSummaryResponse.setItemCode(ITEM_CODE);
    itemSummaryResponse.setItemSku(ITEM_SKU);
    WholesalePriceBulkUpdateRequest wholesalePriceBulkUpdateRequest =
        ConverterUtil.toWholesalePriceBulkUpdateRequest(productPriceStockAndImagesRequest, itemSummaryResponse);
    Assertions.assertEquals(PRODUCT_SKU, wholesalePriceBulkUpdateRequest.getProductSku());
    Assertions.assertEquals(ITEM_CODE, wholesalePriceBulkUpdateRequest.getWholesalePriceSkuRequests().get(0).getItemCode());
    Assertions.assertEquals(ITEM_SKU, wholesalePriceBulkUpdateRequest.getWholesalePriceSkuRequests().get(0).getItemSku());
  }

  @Test
  public void convertItemSkusToSkuCodesRequestTest() {
    List<String> itemSkuCodes = new ArrayList<>();
    itemSkuCodes.add(ITEM_CODE);
    SkuCodesRequest skuCodesRequest = ConverterUtil.convertItemSkusToSkuCodesRequest(itemSkuCodes);
    Assertions.assertEquals(1, skuCodesRequest.getSkuCodes().size());
    assertTrue(skuCodesRequest.isFetchArchived());
  }

  @Test
  public void convertToProductItemUpcCodeUpdateRequestTest() {
    ProductItemUpcCodeUpdateRequest productItemUpcCodeUpdateRequest =
        ConverterUtil.convertToProductItemUpcCodeUpdateRequest(SKU_CODE, UPC_CODE);
    Assertions.assertEquals(UPC_CODE, productItemUpcCodeUpdateRequest.getUpcCode());
    Assertions.assertEquals(SKU_CODE, productItemUpcCodeUpdateRequest.getSkuCode());
  }

  @Test
  public void toUpdatedProductItemImageRequestTest() {
    List<ProductLevel3SummaryDetailsImageRequest> images = new ArrayList<>();
    image.setReviewType("update");
    images.add(image);
    ProductItemImageRequest productItemImageRequest = ConverterUtil.toUpdatedProductItemImageRequest(images, ITEM_SKU);
    Assertions.assertEquals(ITEM_SKU, productItemImageRequest.getSkuCode());
    assertTrue(productItemImageRequest.getItemImages().get(0).isMainImages());
  }

  @Test
  public void toUpdatedProductItemImageRequestNotUpdatedTest() {
    List<ProductLevel3SummaryDetailsImageRequest> images = new ArrayList<>();
    image.setReviewType(StringUtils.EMPTY);
    images.add(image);
    ProductItemImageRequest productItemImageRequest = ConverterUtil.toUpdatedProductItemImageRequest(images, ITEM_SKU);
    Assertions.assertEquals(ITEM_SKU, productItemImageRequest.getSkuCode());
    assertTrue(CollectionUtils.isEmpty(productItemImageRequest.getItemImages()));
  }

  @Test
  public void getProductItemBusinessPartnerTest() throws Exception {
    ProductVariantUpdateRequest request = new ProductVariantUpdateRequest();
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setCncBuyable(true);
    itemPickupPointRequest.setCncDisplay(true);
    itemPickupPointRequest.setFbbActive(true);
    ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
    productItemBusinessPartner.setBundleRecipe(BUNDLE_RECIPE);
    ProductItemBusinessPartner result =
        ConverterUtil.getProductItemBusinessPartner(request, itemPickupPointRequest, productItemBusinessPartner,
            new HashMap<>(), false);
    Assertions.assertEquals(BUNDLE_RECIPE, result.getBundleRecipe());
    Assertions.assertTrue(result.isCncBuyable());
    Assertions.assertTrue(result.isCncDiscoverable());
  }

  @Test
  public void getProductItemBusinessPartnerFbbNullTest() throws Exception {
    ProductVariantUpdateRequest request = new ProductVariantUpdateRequest();
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
    ProductItemBusinessPartner result =
        ConverterUtil.getProductItemBusinessPartner(request, itemPickupPointRequest, productItemBusinessPartner,
            new HashMap<>(), false);
    Assertions.assertNotNull(result);
  }

  @Test
  public void getProductItemWholesalePriceTest() throws Exception {
    ProductVariantUpdateRequest request = new ProductVariantUpdateRequest();
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest = new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest.setSkuCode(SKU_CODE);
    request.setProductItems(Arrays.asList(productVariantPriceStockAndImagesRequest));
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setWholesalePriceActivated(true);
    ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
    ProductItemWholesalePrice productItemWholesalePrice =
        ConverterUtil.getProductItemWholesalePrice(request, itemPickupPointRequest, productItemBusinessPartner);
    Assertions.assertNotNull(productItemWholesalePrice);
  }

  @Test
  public void getProductItemWholesalePriceNullTest() throws Exception {
    ProductVariantUpdateRequest request = new ProductVariantUpdateRequest();
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest = new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest.setSkuCode(SKU_CODE);
    request.setProductItems(Arrays.asList(productVariantPriceStockAndImagesRequest));
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setWholesalePriceActivated(null);
    ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
    ProductItemWholesalePrice productItemWholesalePrice =
        ConverterUtil.getProductItemWholesalePrice(request, itemPickupPointRequest, productItemBusinessPartner);
    Assertions.assertNotNull(productItemWholesalePrice);
  }

  @Test
  public void isPurchaseOrderPurchaseTermTrueTest() throws Exception {
    ProfileResponse businessPartner = new ProfileResponse();
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setPurchaseTerm(GdnBaseLookup.PURCHASE_TERM_PURCHASE_ORDER);
    businessPartner.setCompany(companyDTO);
    boolean isPurchaseOrderPurchaseTerm = ConverterUtil.isPurchaseOrderPurchaseTerm(businessPartner);
    assertTrue(isPurchaseOrderPurchaseTerm);
  }

  @Test
  public void isPurchaseOrderPurchaseTermFalseTest() throws Exception {
    ProfileResponse businessPartner = new ProfileResponse();
    CompanyDTO companyDTO = new CompanyDTO();
    businessPartner.setCompany(companyDTO);
    boolean isPurchaseOrderPurchaseTerm = ConverterUtil.isPurchaseOrderPurchaseTerm(businessPartner);
    Assertions.assertFalse(isPurchaseOrderPurchaseTerm);
  }

  @Test
  public void toNewProductItemImageRequestTest() {
    List<ProductLevel3SummaryDetailsImageRequest> images = new ArrayList<>();
    image.setReviewType("new");
    images.add(image);
    ProductItemImageRequest productItemImageRequest =
        ConverterUtil.toNewProductItemImageRequest(images, ITEM_SKU, false);
    Assertions.assertEquals(ITEM_SKU, productItemImageRequest.getSkuCode());
    assertTrue(productItemImageRequest.getItemImages().get(0).isMainImages());
    assertTrue(productItemImageRequest.getItemImages().get(0).isActive());
    assertTrue(productItemImageRequest.getItemImages().get(0).getOriginalImage());
  }

  @Test
  public void toNewProductItemImageRequestWithWrongReviewTypeTest() {
    List<ProductLevel3SummaryDetailsImageRequest> images = new ArrayList<>();
    image.setReviewType("new2");
    images.add(image);
    ProductItemImageRequest productItemImageRequest =
        ConverterUtil.toNewProductItemImageRequest(images, ITEM_SKU, false);
    Assertions.assertEquals(ITEM_SKU, productItemImageRequest.getSkuCode());
  }

  @Test
  public void copyToItemImagesTest() {
    List<ProductLevel3SummaryDetailsImageRequest> copyToAllVariantImages = new ArrayList<>();
    image.setReviewType("new");
    copyToAllVariantImages.add(image);
    List<Image> images = ConverterUtil.copyToItemImages(copyToAllVariantImages, false);
    assertTrue(images.get(0).isMainImages());
  }

  @Test
  public void copyToItemImagesExistingImageTest() {
    List<ProductLevel3SummaryDetailsImageRequest> copyToAllVariantImages = new ArrayList<>();
    image.setReviewType("update");
    copyToAllVariantImages.add(image);
    List<Image> images = ConverterUtil.copyToItemImages(copyToAllVariantImages, false);
    assertTrue(images.get(0).isMainImages());
  }

  @Test
  public void toProductItemImageUpdateRequestTest() {
    List<ProductItemImageRequest> updatedProductImageRequest = new ArrayList<>();
    List<ProductItemImageRequest> newProductImageRequest = new ArrayList<>();
    List<Image> copyImageToAllProductItemsRequest = new ArrayList<>();
    ProductItemImageUpdateRequest productItemImageUpdateRequest = ConverterUtil
        .toProductItemImageUpdateRequest(updatedProductImageRequest, newProductImageRequest,
            copyImageToAllProductItemsRequest, PRODUCT_CODE, true);
    Assertions.assertEquals(PRODUCT_CODE, productItemImageUpdateRequest.getProductCode());
  }

  @Test
  public void convertProductLevel3ToProductTest() {
    ConverterUtil.convertProductLevel3ToProduct(productLevel3, product);
    Assertions.assertEquals(ATTRIBUTE_CODE, product.getProductAttributes().get(0).getAttribute().getAttributeCode());
  }

  @Test
  public void convertProductLevel3ToProductTest2() {
    product.getProductAttributes().get(0).getAttribute().setAttributeCode("attributeCode2");
    ConverterUtil.convertProductLevel3ToProduct(productLevel3, product);
    Assertions.assertEquals(ATTRIBUTE_CODE, product.getProductAttributes().get(1).getAttribute().getAttributeCode());
  }

  @Test
  public void isProductStatusChangeTest_Online() {
    boolean result = ConverterUtil.isProductStatusChange(true, true, ProductLevel3Status.ONLINE.name());
    boolean result1 = ConverterUtil.isProductStatusChange(true, true, ProductLevel3Status.OFFLINE.name());
    boolean result2 = ConverterUtil.isProductStatusChange(true, true, ProductLevel3Status.B2B.name());
    boolean result3 = ConverterUtil.isProductStatusChange(true, true, ProductLevel3Status.TEASER.name());
    boolean result4 = ConverterUtil.isProductStatusChange(true, false, ProductLevel3Status.ONLINE.name());
    boolean result5 = ConverterUtil.isProductStatusChange(true, false, ProductLevel3Status.OFFLINE.name());
    boolean result6 = ConverterUtil.isProductStatusChange(true, false, ProductLevel3Status.B2B.name());
    boolean result7 = ConverterUtil.isProductStatusChange(true, false, ProductLevel3Status.TEASER.name());
    boolean result8 = ConverterUtil.isProductStatusChange(false, false, ProductLevel3Status.ONLINE.name());
    boolean result9 = ConverterUtil.isProductStatusChange(false, false, ProductLevel3Status.OFFLINE.name());
    boolean result10 = ConverterUtil.isProductStatusChange(false, false, ProductLevel3Status.B2B.name());
    boolean result11 = ConverterUtil.isProductStatusChange(false, false, ProductLevel3Status.TEASER.name());
    boolean result12 = ConverterUtil.isProductStatusChange(false, true, ProductLevel3Status.ONLINE.name());
    boolean result13 = ConverterUtil.isProductStatusChange(false, true, ProductLevel3Status.OFFLINE.name());
    boolean result14 = ConverterUtil.isProductStatusChange(false, true, ProductLevel3Status.B2B.name());
    boolean result15 = ConverterUtil.isProductStatusChange(false, true, ProductLevel3Status.TEASER.name());

    Assertions.assertFalse(result);
    Assertions.assertFalse(result1);
    assertTrue(result2);
    assertTrue(result3);
  }

  @Test
  public void toProductDetailResponseTest() {
    ProductLevel3Attribute productLevel3Attribute = getProductLevel3Attribute();
    productLevel3.setAttributes(Collections.singletonList(productLevel3Attribute));
    ProductDetailResponse response = ConverterUtil.toProductDetailResponse(productLevel3);
    Assertions.assertNotNull(response);
    Assertions.assertEquals(VALUE1, response.getProductAttributeResponses().get(0).getProductAttributeValues().get(0)
        .getDescriptiveAttributeValue());
    Assertions.assertEquals(VALUE2, response.getProductAttributeResponses().get(0).getProductAttributeValues().get(1)
        .getDescriptiveAttributeValue());
  }

  @Test
  public void toProductDetailResponseWithEmptyAttributeListTest() {
    ProductLevel3Attribute productLevel3Attribute = new ProductLevel3Attribute();
    productLevel3Attribute.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE.name());
    productLevel3.setAttributes(new ArrayList<>());
    ProductDetailResponse response = ConverterUtil.toProductDetailResponse(productLevel3);
    Assertions.assertNotNull(response);
    assertTrue(CollectionUtils.isEmpty(response.getProductAttributeResponses()));
  }

  @Test
  public void toProductDetailResponseWithVariantCreationTrueTest() {
    ProductLevel3Attribute productLevel3Attribute = getProductLevel3Attribute();
    productLevel3Attribute.setVariantCreation(true);
    productLevel3.setAttributes(Arrays.asList(productLevel3Attribute));
    ProductDetailResponse response = ConverterUtil.toProductDetailResponse(productLevel3);
    Assertions.assertNotNull(response);
    assertTrue(CollectionUtils.isEmpty(response.getProductAttributeResponses()));
  }

  @Test
  public void toProductDetailResponseWithVariantCreationTrue1Test() {
    ProductLevel3Attribute productLevel3Attribute = getProductLevel3Attribute();
    productLevel3Attribute.setVariantCreation(true);
    productLevel3Attribute.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE.name());
    productLevel3.setAttributes(Arrays.asList(productLevel3Attribute));
    ProductDetailResponse response = ConverterUtil.toProductDetailResponse(productLevel3);
    Assertions.assertNotNull(response);
    assertTrue(CollectionUtils.isEmpty(response.getProductAttributeResponses()));
  }

  @Test
  public void toProductDetailResponseWithVariantCreationFalseTest() {
    ProductLevel3Attribute productLevel3Attribute = getProductLevel3Attribute();
    productLevel3Attribute.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE.name());
    productLevel3.setAttributes(Arrays.asList(productLevel3Attribute));
    ProductDetailResponse response = ConverterUtil.toProductDetailResponse(productLevel3);
    Assertions.assertNotNull(response);
    Assertions.assertFalse(CollectionUtils.isEmpty(response.getProductAttributeResponses()));
  }

  @Test
  public void toProductDetailResponseWithVariantCreationFalse1Test() {
    ProductLevel3Attribute productLevel3Attribute = getProductLevel3Attribute();
    productLevel3Attribute.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE.name());
    productLevel3.setAttributes(Arrays.asList(productLevel3Attribute));
    ProductDetailResponse response = ConverterUtil.toProductDetailResponse(productLevel3);
    Assertions.assertNotNull(response);
    assertTrue(CollectionUtils.isEmpty(response.getProductAttributeResponses()));
  }

  @Test
  public void toProductDetailResponseWithNonDescriptiveAttributeTest() {
    ProductLevel3Attribute productLevel3Attribute = getProductLevel3Attribute();
    productLevel3Attribute.setAttributeType(AttributeType.DEFINING_ATTRIBUTE.name());
    ProductDetailResponse response = ConverterUtil.toProductDetailResponse(productLevel3);
    Assertions.assertNotNull(response);
    assertTrue(CollectionUtils.isEmpty(response.getProductAttributeResponses()));
  }

  @Test
  public void toProductDetailResponseWithVariantCreationFalseAndNonDescriptiveAttributeTest() {
    ProductLevel3Attribute productLevel3Attribute = getProductLevel3Attribute();
    productLevel3Attribute.setVariantCreation(true);
    productLevel3Attribute.setAttributeType(AttributeType.DEFINING_ATTRIBUTE.name());
    ProductDetailResponse response = ConverterUtil.toProductDetailResponse(productLevel3);
    Assertions.assertNotNull(response);
    assertTrue(CollectionUtils.isEmpty(response.getProductAttributeResponses()));
  }

  @Test
  public void toNeedCorrectionNotesForScreeningTest() throws Exception {
    screeningProductBulkActionsRequest.setCommonImageReason(Arrays.asList("correctionReason1"));
    String response = ConverterUtil.toNeedCorrectionNotesForScreening(screeningProductBulkActionsRequest);
    Assertions.assertEquals(NEED_CORRECTION_NOTES, response);
  }

  private ProductLevel3Attribute getProductLevel3Attribute() {
    ProductLevel3Attribute productLevel3Attribute = new ProductLevel3Attribute();
    List<String> values = new ArrayList<>();
    values.add(VALUE1);
    values.add(VALUE2);
    productLevel3Attribute.setValues(values);
    productLevel3Attribute.setMandatory(false);
    productLevel3Attribute.setBasicView(true);
    productLevel3Attribute.setVariantCreation(false);
    productLevel3Attribute.setAttributeCode(ATTRIBUTE_CODE);
    productLevel3Attribute.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE.name());
    productLevel3Attribute.setAttributeName(DEFAULT_ATTRIBUTE_NAME);
    productLevel3Attribute.setBasicView(true);
    productLevel3Attribute.setSkuValue(true);
    return productLevel3Attribute;
  }

  @Test
  public void toNewProductItemImageRequestNeedCorrectionTest() {
    List<ProductLevel3SummaryDetailsImageRequest> images = new ArrayList<>();
    image.setReviewType("new");
    images.add(image);
    ProductItemImageRequest productItemImageRequest =
        ConverterUtil.toNewProductItemImageRequest(images, ITEM_SKU, true);
    Assertions.assertEquals(ITEM_SKU, productItemImageRequest.getSkuCode());
    assertTrue(productItemImageRequest.getItemImages().get(0).isMainImages());
    Assertions.assertFalse(productItemImageRequest.getItemImages().get(0).isActive());
    assertTrue(productItemImageRequest.getItemImages().get(0).getOriginalImage());
    assertTrue(productItemImageRequest.getItemImages().get(0).isRevised());
    Assertions.assertFalse(productItemImageRequest.getItemImages().get(0).isEdited());
  }

  @Test
  public void toItemImageEditRequestTest() {
    // Create some sample data for testing
    ProductItemImageRequest imageRequest = new ProductItemImageRequest();
    imageRequest.setSkuCode("123");
    Image image = new Image();
    image.setMainImages(true);
    image.setHashCode("hashcode123");
    imageRequest.setItemImages(ImmutableList.of(image));
    List<ProductItemImageRequest> imageList = ImmutableList.of(imageRequest);
    ImmutableMap<String, List<ProductItemImageRequest>> reviewTypeImageMap = ImmutableMap.of("new",
      imageList);
    List<com.gdn.x.productcategorybase.dto.request.ItemImageEditRequest> result = ConverterUtil.toItemImageEditRequest(reviewTypeImageMap);
    Assertions.assertFalse(result.isEmpty());
    assertEquals(1, result.size());
    com.gdn.x.productcategorybase.dto.request.ItemImageEditRequest itemImageEditRequest = result.get(0);
    assertTrue(itemImageEditRequest.isAdd());
    Assertions.assertFalse(itemImageEditRequest.isCopy());
    assertEquals("123", itemImageEditRequest.getItemCode());
    assertTrue(itemImageEditRequest.isMainImage());
    assertEquals("hashcode123", itemImageEditRequest.getHashCode());
  }

  @Test
  public void toItemImageEditRequestUpdateTest() {
    // Create some sample data for testing
    ProductItemImageRequest imageRequest = new ProductItemImageRequest();
    imageRequest.setSkuCode("123");
    Image image = new Image();
    image.setMainImages(true);
    image.setHashCode("hashcode123");
    imageRequest.setItemImages(ImmutableList.of(image));
    List<ProductItemImageRequest> imageList = ImmutableList.of(imageRequest);
    ImmutableMap<String, List<ProductItemImageRequest>> reviewTypeImageMap = ImmutableMap.of(
      "update",
      imageList);
    List<com.gdn.x.productcategorybase.dto.request.ItemImageEditRequest> result = ConverterUtil.toItemImageEditRequest(reviewTypeImageMap);
    Assertions.assertFalse(result.isEmpty());
    assertEquals(1, result.size());
    com.gdn.x.productcategorybase.dto.request.ItemImageEditRequest itemImageEditRequest = result.get(0);
    assertFalse(itemImageEditRequest.isAdd());
    Assertions.assertFalse(itemImageEditRequest.isCopy());
    assertEquals("123", itemImageEditRequest.getItemCode());
    assertTrue(itemImageEditRequest.isMainImage());
    assertEquals("hashcode123", itemImageEditRequest.getHashCode());
  }


  @Test
  public void toNewProductItemImageRequestWithWrongReviewTypeNeedCorrectionTest() {
    List<ProductLevel3SummaryDetailsImageRequest> images = new ArrayList<>();
    image.setReviewType("new2");
    images.add(image);
    ProductItemImageRequest productItemImageRequest =
        ConverterUtil.toNewProductItemImageRequest(images, ITEM_SKU, true);
    Assertions.assertEquals(ITEM_SKU, productItemImageRequest.getSkuCode());
  }

  @Test
  public void copyToItemImagesNeedCorrectionTest() {
    List<ProductLevel3SummaryDetailsImageRequest> copyToAllVariantImages = new ArrayList<>();
    image.setReviewType("new");
    copyToAllVariantImages.add(image);
    List<Image> images = ConverterUtil.copyToItemImages(copyToAllVariantImages, true);
    assertTrue(images.get(0).isMainImages());
    assertTrue(images.get(0).isRevised());
    Assertions.assertFalse(images.get(0).isEdited());
  }

  @Test
  public void copyToItemImagesExistingImageNeedCorrectionTest() {
    List<ProductLevel3SummaryDetailsImageRequest> copyToAllVariantImages = new ArrayList<>();
    image.setReviewType("update");
    copyToAllVariantImages.add(image);
    List<Image> images = ConverterUtil.copyToItemImages(copyToAllVariantImages, true);
    assertTrue(images.get(0).isMainImages());
    Assertions.assertFalse(images.get(0).isRevised());
    Assertions.assertFalse(images.get(0).isEdited());
  }

  @Test
  public void toAddRevisedProductToPDTEventTest() throws Exception {
    List<String> modifiedFields =
        Stream.concat(ERROR_FIELDS.stream(), MERCHANT_MODIFIED_FIELDS.stream()).collect(Collectors.toList());
    NeedCorrectionNotesDto needCorrectionNotesDto =
        NeedCorrectionNotesDto.builder().merchantModifiedFields(MERCHANT_MODIFIED_FIELDS)
            .allModifiedFields(ERROR_FIELDS).build();
    productCollection =
        new ProductCollection(PRODUCT_ID, PRODUCT_CODE, PRODUCT_NAME, BRAND, CATEGORY_CODE, CATEGORY_NAME,
            BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_NAME, false, false, STATE, CREATED_BY, CREATED_DATE, STORE_ID);
    productCollection.setNeedCorrectionNotes(new ObjectMapper().writeValueAsString(needCorrectionNotesDto));

    AddRevisedProductToPDTEvent addRevisedProductToPDTEvent =
        ConverterUtil.toAddRevisedProductToPDTEvent(productCollection, USERNAME, false,
            new ProductBusinessPartner(), false, null, false, 0);

    Assertions.assertEquals(PRODUCT_CODE, addRevisedProductToPDTEvent.getProductCode());
    Assertions.assertEquals(STORE_ID, addRevisedProductToPDTEvent.getStoreId());
    Assertions.assertEquals(false, addRevisedProductToPDTEvent.isPostLive());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, addRevisedProductToPDTEvent.getMerchantCode());
    Assertions.assertEquals(BUSINESS_PARTNER_NAME, addRevisedProductToPDTEvent.getMerchantName());
    Assertions.assertEquals(USERNAME, addRevisedProductToPDTEvent.getUpdatedBy());
    Assertions.assertEquals(false, addRevisedProductToPDTEvent.isRestrictedKeywordsPresent());
    Assertions.assertEquals(MERCHANT_MODIFIED_FIELDS, addRevisedProductToPDTEvent.getMerchantModifiedFields());
    Assertions.assertFalse(addRevisedProductToPDTEvent.isTrustedSeller());
    Assertions.assertEquals(modifiedFields, addRevisedProductToPDTEvent.getAllModifiedFields());
  }

  @Test
  public void toAddRevisedProductToPDTEventTest_showPriceInfoTrue() throws Exception {
    List<String> modifiedFields =
        Stream.concat(ERROR_FIELDS.stream(), MERCHANT_MODIFIED_FIELDS.stream())
            .collect(Collectors.toList());
    NeedCorrectionNotesDto needCorrectionNotesDto =
        NeedCorrectionNotesDto.builder().merchantModifiedFields(MERCHANT_MODIFIED_FIELDS)
            .allModifiedFields(ERROR_FIELDS).build();
    productCollection =
        new ProductCollection(PRODUCT_ID, PRODUCT_CODE, PRODUCT_NAME, BRAND, CATEGORY_CODE,
            CATEGORY_NAME, BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_NAME, false, false, STATE,
            CREATED_BY, CREATED_DATE, STORE_ID);
    productCollection.setNeedCorrectionNotes(
        new ObjectMapper().writeValueAsString(needCorrectionNotesDto));

    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
    productItemBusinessPartner.setGdnProductItemSku(ITEM_SKU);
    productItemBusinessPartner.setProductItemId(ITEM_ID);
    productItemBusinessPartner.setSalePrice(Double.valueOf(10.0d));
    ProductItemBusinessPartner productItemBusinessPartner2 = new ProductItemBusinessPartner();
    productItemBusinessPartner2.setGdnProductItemSku(ITEM_SKU);
    productItemBusinessPartner2.setProductItemId(ITEM_ID);
    productItemBusinessPartner2.setSalePrice(Double.valueOf(100.0d));
    productBusinessPartner.setBusinessPartnerId(BUSINESS_PARTNER_ID);
    productBusinessPartner.setProductItemBusinessPartners(
        Arrays.asList(productItemBusinessPartner, productItemBusinessPartner2));

    AddRevisedProductToPDTEvent addRevisedProductToPDTEvent =
        ConverterUtil.toAddRevisedProductToPDTEvent(productCollection, USERNAME, false,
            productBusinessPartner, false, null, true, 10);

    Assertions.assertEquals(PRODUCT_CODE, addRevisedProductToPDTEvent.getProductCode());
    Assertions.assertEquals(STORE_ID, addRevisedProductToPDTEvent.getStoreId());
    Assertions.assertEquals(false, addRevisedProductToPDTEvent.isPostLive());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, addRevisedProductToPDTEvent.getMerchantCode());
    Assertions.assertEquals(BUSINESS_PARTNER_NAME, addRevisedProductToPDTEvent.getMerchantName());
    Assertions.assertEquals(USERNAME, addRevisedProductToPDTEvent.getUpdatedBy());
    Assertions.assertEquals(false, addRevisedProductToPDTEvent.isRestrictedKeywordsPresent());
    Assertions.assertEquals(MERCHANT_MODIFIED_FIELDS,
        addRevisedProductToPDTEvent.getMerchantModifiedFields());
    Assertions.assertFalse(addRevisedProductToPDTEvent.isTrustedSeller());
    Assertions.assertEquals(modifiedFields, addRevisedProductToPDTEvent.getAllModifiedFields());
    Assertions.assertEquals(1, addRevisedProductToPDTEvent.getPriceInfo().size());
    Assertions.assertEquals(100.0d,
        addRevisedProductToPDTEvent.getPriceInfo().get(0).getMaxPrice(), 0);
    Assertions.assertEquals(10.0d,
        addRevisedProductToPDTEvent.getPriceInfo().get(0).getMinPrice(), 0);
    Assertions.assertEquals(ITEM_SKU, addRevisedProductToPDTEvent.getPriceInfo().get(0).getItemSku());
    Assertions.assertEquals(ITEM_ID, addRevisedProductToPDTEvent.getPriceInfo().get(0).getItemId());
  }

  @Test
  public void toAddEditedProductToPDTEventTest_showPriceInfoTrue() throws Exception {
    List<String> modifiedFields =
        Stream.concat(ERROR_FIELDS.stream(), MERCHANT_MODIFIED_FIELDS.stream())
            .collect(Collectors.toList());
    NeedCorrectionNotesDto needCorrectionNotesDto =
        NeedCorrectionNotesDto.builder().merchantModifiedFields(MERCHANT_MODIFIED_FIELDS)
            .allModifiedFields(ERROR_FIELDS).build();
    ProfileResponse profileResponse = new ProfileResponse();
    productCollection =
        new ProductCollection(PRODUCT_ID, PRODUCT_CODE, PRODUCT_NAME, BRAND, CATEGORY_CODE,
            CATEGORY_NAME, BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_NAME, false, false, STATE,
            CREATED_BY, CREATED_DATE, STORE_ID);
    productCollection.setNeedCorrectionNotes(
        new ObjectMapper().writeValueAsString(needCorrectionNotesDto));

    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
    productItemBusinessPartner.setGdnProductItemSku(ITEM_SKU);
    productItemBusinessPartner.setProductItemId(ITEM_ID);
    productItemBusinessPartner.setSalePrice(Double.valueOf(10.0d));
    ProductItemBusinessPartner productItemBusinessPartner2 = new ProductItemBusinessPartner();
    productItemBusinessPartner2.setGdnProductItemSku(ITEM_SKU);
    productItemBusinessPartner2.setProductItemId(ITEM_ID);
    productItemBusinessPartner2.setSalePrice(Double.valueOf(100.0d));
    productBusinessPartner.setBusinessPartnerId(BUSINESS_PARTNER_ID);
    productBusinessPartner.setProductItemBusinessPartners(
        Arrays.asList(productItemBusinessPartner, productItemBusinessPartner2));

    AddEditedProductToPDTEvent addEditedProductToPDTEvent =
        ConverterUtil.toAddEditedProductToPDTEvent(STORE_ID, USERNAME, productCollection,
            modifiedFields, profileResponse, productBusinessPartner, true, 10);

    Assertions.assertEquals(PRODUCT_CODE, addEditedProductToPDTEvent.getProductCode());
    Assertions.assertEquals(STORE_ID, addEditedProductToPDTEvent.getStoreId());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, addEditedProductToPDTEvent.getMerchantCode());
    Assertions.assertEquals(BUSINESS_PARTNER_NAME, addEditedProductToPDTEvent.getMerchantName());
    Assertions.assertEquals(1, addEditedProductToPDTEvent.getPriceInfo().size());
    Assertions.assertEquals(100.0d,
        addEditedProductToPDTEvent.getPriceInfo().get(0).getMaxPrice(), 0);
    Assertions.assertEquals(10.0d,
        addEditedProductToPDTEvent.getPriceInfo().get(0).getMinPrice(), 0);
    Assertions.assertEquals(ITEM_SKU, addEditedProductToPDTEvent.getPriceInfo().get(0).getItemSku());
    Assertions.assertEquals(ITEM_ID, addEditedProductToPDTEvent.getPriceInfo().get(0).getItemId());

  }

  @Test
  public void toAddEditedProductToPDTNextEventTest_showPriceInfoTrue() throws Exception {
    List<String> modifiedFields =
        Stream.concat(ERROR_FIELDS.stream(), MERCHANT_MODIFIED_FIELDS.stream())
            .collect(Collectors.toList());
    NeedCorrectionNotesDto needCorrectionNotesDto =
        NeedCorrectionNotesDto.builder().merchantModifiedFields(MERCHANT_MODIFIED_FIELDS)
            .allModifiedFields(ERROR_FIELDS).build();
    productCollection =
        new ProductCollection(PRODUCT_ID, PRODUCT_CODE, PRODUCT_NAME, BRAND, CATEGORY_CODE,
            CATEGORY_NAME, BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_NAME, false, false, STATE,
            CREATED_BY, CREATED_DATE, STORE_ID);
    productCollection.setNeedCorrectionNotes(
        new ObjectMapper().writeValueAsString(needCorrectionNotesDto));

    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
    productItemBusinessPartner.setGdnProductItemSku(ITEM_SKU);
    productItemBusinessPartner.setProductItemId(ITEM_ID);
    productItemBusinessPartner.setSalePrice(Double.valueOf(MIN_PRICE));
    productItemBusinessPartner.setProductBusinessPartner(productBusinessPartner);
    ProductItemBusinessPartner productItemBusinessPartner2 = new ProductItemBusinessPartner();
    productItemBusinessPartner2.setGdnProductItemSku(ITEM_SKU);
    productItemBusinessPartner2.setProductItemId(ITEM_ID);
    productItemBusinessPartner2.setSalePrice(Double.valueOf(MAX_PRICE));
    productItemBusinessPartner2.setProductBusinessPartner(productBusinessPartner);
    productBusinessPartner.setBusinessPartnerId(BUSINESS_PARTNER_ID);
    productBusinessPartner.setProductItemBusinessPartners(
        Arrays.asList(productItemBusinessPartner, productItemBusinessPartner2));

    AddEditedProductToPDTEvent addEditedProductToPDTEvent =
        ConverterUtil.toAddEditedProductToPDTEventForAddAndEdit(STORE_ID, USERNAME, productCollection,
            modifiedFields, productBusinessPartner.getProductItemBusinessPartners(), true, 10);

    Assertions.assertEquals(PRODUCT_CODE, addEditedProductToPDTEvent.getProductCode());
    Assertions.assertEquals(STORE_ID, addEditedProductToPDTEvent.getStoreId());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, addEditedProductToPDTEvent.getMerchantCode());
    Assertions.assertEquals(BUSINESS_PARTNER_NAME, addEditedProductToPDTEvent.getMerchantName());
    Assertions.assertEquals(SIZE_VALUE, addEditedProductToPDTEvent.getPriceInfo().size());
    Assertions.assertEquals(MAX_PRICE,
        addEditedProductToPDTEvent.getPriceInfo().get(0).getMaxPrice(), 0);
    Assertions.assertEquals(MIN_PRICE,
        addEditedProductToPDTEvent.getPriceInfo().get(0).getMinPrice(), 0);
    Assertions.assertEquals(ITEM_SKU, addEditedProductToPDTEvent.getPriceInfo().get(0).getItemSku());
    Assertions.assertEquals(ITEM_ID, addEditedProductToPDTEvent.getPriceInfo().get(0).getItemId());

  }

  @Test
  public void toAddEditedProductToPDTNextEventB2bCheckShowPriceInfoTrue() throws Exception {
    List<String> modifiedFields =
        Stream.concat(ERROR_FIELDS.stream(), MERCHANT_MODIFIED_FIELDS.stream())
            .collect(Collectors.toList());
    NeedCorrectionNotesDto needCorrectionNotesDto =
        NeedCorrectionNotesDto.builder().merchantModifiedFields(MERCHANT_MODIFIED_FIELDS)
            .allModifiedFields(ERROR_FIELDS).build();
    productCollection =
        new ProductCollection(PRODUCT_ID, PRODUCT_CODE, PRODUCT_NAME, BRAND, CATEGORY_CODE,
            CATEGORY_NAME, BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_NAME, false, false, STATE,
            CREATED_BY, CREATED_DATE, STORE_ID);
    productCollection.setNeedCorrectionNotes(
        new ObjectMapper().writeValueAsString(needCorrectionNotesDto));

    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
    productItemBusinessPartner.setGdnProductItemSku(ITEM_SKU);
    productItemBusinessPartner.setProductItemId(ITEM_ID);
    productItemBusinessPartner.setSalePrice(Double.valueOf(MIN_PRICE));
    productBusinessPartner.setB2bActivated(true);
    productBusinessPartner.setB2cActivated(true);
    productItemBusinessPartner.setProductBusinessPartner(productBusinessPartner);
    ProductItemBusinessPartner productItemBusinessPartner2 = new ProductItemBusinessPartner();
    productItemBusinessPartner2.setGdnProductItemSku(ITEM_SKU);
    productItemBusinessPartner2.setProductItemId(ITEM_ID);
    productItemBusinessPartner2.setSalePrice(Double.valueOf(MAX_PRICE));
    productItemBusinessPartner2.setProductBusinessPartner(productBusinessPartner);
    productBusinessPartner.setBusinessPartnerId(BUSINESS_PARTNER_ID);
    productBusinessPartner.setProductItemBusinessPartners(
        Arrays.asList(productItemBusinessPartner, productItemBusinessPartner2));

    AddEditedProductToPDTEvent addEditedProductToPDTEvent =
        ConverterUtil.toAddEditedProductToPDTEventForAddAndEdit(STORE_ID, USERNAME, productCollection,
            modifiedFields, productBusinessPartner.getProductItemBusinessPartners(), true, 10);

    Assertions.assertEquals(PRODUCT_CODE, addEditedProductToPDTEvent.getProductCode());
    Assertions.assertEquals(STORE_ID, addEditedProductToPDTEvent.getStoreId());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, addEditedProductToPDTEvent.getMerchantCode());
    Assertions.assertEquals(BUSINESS_PARTNER_NAME, addEditedProductToPDTEvent.getMerchantName());
    Assertions.assertEquals(SIZE_VALUE, addEditedProductToPDTEvent.getPriceInfo().size());
    Assertions.assertEquals(MAX_PRICE,
        addEditedProductToPDTEvent.getPriceInfo().get(0).getMaxPrice(), 0);
    Assertions.assertEquals(MIN_PRICE,
        addEditedProductToPDTEvent.getPriceInfo().get(0).getMinPrice(), 0);
    Assertions.assertEquals(ITEM_SKU, addEditedProductToPDTEvent.getPriceInfo().get(0).getItemSku());
    Assertions.assertEquals(ITEM_ID, addEditedProductToPDTEvent.getPriceInfo().get(0).getItemId());
    Assertions.assertTrue(addEditedProductToPDTEvent.isB2bActivated());
    Assertions.assertTrue(addEditedProductToPDTEvent.isB2cActivated());
  }


    @Test
  public void toAddRevisedProductToPDTEventTest_showPriceInfoTrue_emptyProductBusinessPartnerItems() throws Exception {
    List<String> modifiedFields =
        Stream.concat(ERROR_FIELDS.stream(), MERCHANT_MODIFIED_FIELDS.stream())
            .collect(Collectors.toList());
    NeedCorrectionNotesDto needCorrectionNotesDto =
        NeedCorrectionNotesDto.builder().merchantModifiedFields(MERCHANT_MODIFIED_FIELDS)
            .allModifiedFields(ERROR_FIELDS).build();
    productCollection =
        new ProductCollection(PRODUCT_ID, PRODUCT_CODE, PRODUCT_NAME, BRAND, CATEGORY_CODE,
            CATEGORY_NAME, BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_NAME, false, false, STATE,
            CREATED_BY, CREATED_DATE, STORE_ID);
    productCollection.setNeedCorrectionNotes(
        new ObjectMapper().writeValueAsString(needCorrectionNotesDto));


    AddRevisedProductToPDTEvent addRevisedProductToPDTEvent =
        ConverterUtil.toAddRevisedProductToPDTEvent(productCollection, USERNAME, false,
            new ProductBusinessPartner(), false, null, true, 0);

    Assertions.assertEquals(PRODUCT_CODE, addRevisedProductToPDTEvent.getProductCode());
    Assertions.assertEquals(STORE_ID, addRevisedProductToPDTEvent.getStoreId());
    Assertions.assertEquals(false, addRevisedProductToPDTEvent.isPostLive());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, addRevisedProductToPDTEvent.getMerchantCode());
    Assertions.assertEquals(BUSINESS_PARTNER_NAME, addRevisedProductToPDTEvent.getMerchantName());
    Assertions.assertEquals(USERNAME, addRevisedProductToPDTEvent.getUpdatedBy());
    Assertions.assertEquals(false, addRevisedProductToPDTEvent.isRestrictedKeywordsPresent());
    Assertions.assertEquals(MERCHANT_MODIFIED_FIELDS,
        addRevisedProductToPDTEvent.getMerchantModifiedFields());
    Assertions.assertFalse(addRevisedProductToPDTEvent.isTrustedSeller());
    Assertions.assertEquals(modifiedFields, addRevisedProductToPDTEvent.getAllModifiedFields());
    Assertions.assertEquals(0, addRevisedProductToPDTEvent.getPriceInfo().size());
  }

  @Test
  public void toAddRevisedProductToPDTEventTest_showPriceInfoTrue_limitBreached() throws Exception {
    List<String> modifiedFields =
        Stream.concat(ERROR_FIELDS.stream(), MERCHANT_MODIFIED_FIELDS.stream())
            .collect(Collectors.toList());
    NeedCorrectionNotesDto needCorrectionNotesDto =
        NeedCorrectionNotesDto.builder().merchantModifiedFields(MERCHANT_MODIFIED_FIELDS)
            .allModifiedFields(ERROR_FIELDS).build();
    productCollection =
        new ProductCollection(PRODUCT_ID, PRODUCT_CODE, PRODUCT_NAME, BRAND, CATEGORY_CODE,
            CATEGORY_NAME, BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_NAME, false, false, STATE,
            CREATED_BY, CREATED_DATE, STORE_ID);
    productCollection.setNeedCorrectionNotes(
        new ObjectMapper().writeValueAsString(needCorrectionNotesDto));

    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
    productItemBusinessPartner.setGdnProductItemSku(ITEM_SKU);
    productItemBusinessPartner.setProductItemId(ITEM_ID);
    productItemBusinessPartner.setSalePrice(Double.valueOf(10));
    ProductItemBusinessPartner productItemBusinessPartner2 = new ProductItemBusinessPartner();
    productItemBusinessPartner2.setGdnProductItemSku(ITEM_SKU);
    productItemBusinessPartner2.setProductItemId(ITEM_ID);
    productItemBusinessPartner2.setSalePrice(Double.valueOf(100));
    productBusinessPartner.setBusinessPartnerId(BUSINESS_PARTNER_ID);
    productBusinessPartner.setProductItemBusinessPartners(
        Arrays.asList(productItemBusinessPartner, productItemBusinessPartner2));

    AddRevisedProductToPDTEvent addRevisedProductToPDTEvent =
        ConverterUtil.toAddRevisedProductToPDTEvent(productCollection, USERNAME, false,
            productBusinessPartner, false, null, true, 0);

    Assertions.assertEquals(PRODUCT_CODE, addRevisedProductToPDTEvent.getProductCode());
    Assertions.assertEquals(STORE_ID, addRevisedProductToPDTEvent.getStoreId());
    Assertions.assertEquals(false, addRevisedProductToPDTEvent.isPostLive());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, addRevisedProductToPDTEvent.getMerchantCode());
    Assertions.assertEquals(BUSINESS_PARTNER_NAME, addRevisedProductToPDTEvent.getMerchantName());
    Assertions.assertEquals(USERNAME, addRevisedProductToPDTEvent.getUpdatedBy());
    Assertions.assertEquals(false, addRevisedProductToPDTEvent.isRestrictedKeywordsPresent());
    Assertions.assertEquals(MERCHANT_MODIFIED_FIELDS,
        addRevisedProductToPDTEvent.getMerchantModifiedFields());
    Assertions.assertFalse(addRevisedProductToPDTEvent.isTrustedSeller());
    Assertions.assertEquals(modifiedFields, addRevisedProductToPDTEvent.getAllModifiedFields());
    Assertions.assertEquals(0, addRevisedProductToPDTEvent.getPriceInfo().size());
  }

  @Test
  public void toAddRevisedProductToPDTEvent_forTrustedSellersTest() throws Exception {
    NeedCorrectionNotesDto needCorrectionNotesDto =
      NeedCorrectionNotesDto.builder().merchantModifiedFields(ERROR_FIELDS).build();
    productCollection =
      new ProductCollection(PRODUCT_ID, PRODUCT_CODE, PRODUCT_NAME, BRAND, CATEGORY_CODE, CATEGORY_NAME,
        BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_NAME, false, false, STATE, CREATED_BY, CREATED_DATE, STORE_ID);
    productCollection.setNeedCorrectionNotes(new ObjectMapper().writeValueAsString(needCorrectionNotesDto));

    AddRevisedProductToPDTEvent addRevisedProductToPDTEvent =
        ConverterUtil.toAddRevisedProductToPDTEvent(productCollection, USERNAME, true,
            new ProductBusinessPartner(), false, null, false, 0);

    Assertions.assertEquals(PRODUCT_CODE, addRevisedProductToPDTEvent.getProductCode());
    Assertions.assertEquals(STORE_ID, addRevisedProductToPDTEvent.getStoreId());
    Assertions.assertEquals(false, addRevisedProductToPDTEvent.isPostLive());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, addRevisedProductToPDTEvent.getMerchantCode());
    Assertions.assertEquals(BUSINESS_PARTNER_NAME, addRevisedProductToPDTEvent.getMerchantName());
    Assertions.assertEquals(USERNAME, addRevisedProductToPDTEvent.getUpdatedBy());
    Assertions.assertEquals(false, addRevisedProductToPDTEvent.isRestrictedKeywordsPresent());
    Assertions.assertEquals(ERROR_FIELDS, addRevisedProductToPDTEvent.getMerchantModifiedFields());
    assertTrue(addRevisedProductToPDTEvent.isTrustedSeller());
    Assertions.assertFalse(addRevisedProductToPDTEvent.isPostLive());
  }


  @Test
  public void toAddRevisedProductToPDTEventNeedCorrectionNotesNullTest() throws Exception {
    productCollection =
        new ProductCollection(PRODUCT_ID, PRODUCT_CODE, PRODUCT_NAME, BRAND, CATEGORY_CODE, CATEGORY_NAME,
            BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_NAME, false, false, STATE, CREATED_BY, CREATED_DATE, STORE_ID);
    AddRevisedProductToPDTEvent addRevisedProductToPDTEvent =
        ConverterUtil.toAddRevisedProductToPDTEvent(productCollection, USERNAME, false,
            new ProductBusinessPartner(), false, null, false, 0);
    Assertions.assertEquals(PRODUCT_CODE, addRevisedProductToPDTEvent.getProductCode());
    Assertions.assertEquals(STORE_ID, addRevisedProductToPDTEvent.getStoreId());
    Assertions.assertFalse(addRevisedProductToPDTEvent.isPostLive());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, addRevisedProductToPDTEvent.getMerchantCode());
    Assertions.assertEquals(BUSINESS_PARTNER_NAME, addRevisedProductToPDTEvent.getMerchantName());
    Assertions.assertEquals(USERNAME, addRevisedProductToPDTEvent.getUpdatedBy());
    Assertions.assertFalse(addRevisedProductToPDTEvent.isRestrictedKeywordsPresent());
    assertTrue(addRevisedProductToPDTEvent.getMerchantModifiedFields().isEmpty());
    Assertions.assertFalse(addRevisedProductToPDTEvent.isTrustedSeller());
  }

  @Test
  public void toWholesalePriceRequestTest() throws Exception {
    WholesalePriceRequest wholesalePriceRequest =
        ConverterUtil.toWholesalePriceRequest(PRODUCT_SKU, BUSINESS_PARTNER_CODE, false, productItemWholesalePrices);
    Assertions.assertEquals(PRODUCT_SKU, wholesalePriceRequest.getProductSku());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, wholesalePriceRequest.getMerchantCode());
    Assertions.assertFalse(wholesalePriceRequest.isNewProduct());
    Assertions.assertEquals(ITEM_SKU, wholesalePriceRequest.getWholesalePriceSkuRequests().get(0).getItemSku());
    Assertions.assertEquals(ITEM_CODE, wholesalePriceRequest.getWholesalePriceSkuRequests().get(0).getItemCode());
    Assertions.assertEquals(ImmutableMap.of(3, 30.0),
        wholesalePriceRequest.getWholesalePriceSkuRequests().get(0).getWholesaleRules());
    assertTrue(wholesalePriceRequest.getWholesalePriceSkuRequests().get(1).getWholesaleRules().isEmpty());
  }

  @Test
  public void getRevisedImageRequestsTest() {
    Image image = new Image();
    image.setLocationPath(LOCATION_PATH_1);
    image.setHashCode(HASH_CODE_1);
    image.setRevised(true);

    Image image1 = new Image();
    image1.setLocationPath(LOCATION_PATH_2);
    image1.setHashCode(HASH_CODE_2);
    image1.setRevised(false);

    List<ImageRequest> imageRequests =
        ConverterUtil.getRevisedImageRequests(Arrays.asList(image, image1), IMAGE_SOURCE_DIRECTORY);

    Assertions.assertEquals(1, imageRequests.size());
    Assertions.assertEquals(LOCATION_PATH_1, imageRequests.get(0).getImageName());
    Assertions.assertEquals(HASH_CODE_1, imageRequests.get(0).getHashCode());
    Assertions.assertEquals(IMAGE_SOURCE_DIRECTORY + File.separator + LOCATION_PATH_1,
        imageRequests.get(0).getAbsoluteImagePath());
  }

  @Test
  public void toImageQcConfidenceDetailsTest() {
    productCollection = new ProductCollection();
    productCollection.setProductId(PRODUCT_ID);
    productCollection.setProductCode(PRODUCT_CODE);
    productCollection.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productCollection.setUpdatedBy(USERNAME);
    AutoApprovalsDetailDto response = ConverterUtil
        .setAutoApprovalDetailDTO(STORE_ID, productCollection, CATEGORY_CODE, Arrays.asList(productImage),
            Collections.singletonMap(LOCATION_PATH_1, imageQcResponse));
    Assertions.assertNotNull(response);
    Assertions.assertEquals(PRODUCT_CODE, response.getProductCode());
    Assertions.assertEquals(USERNAME, response.getUpdatedBy());
    Assertions.assertEquals(CATEGORY_CODE, response.getC1CategoryCode());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, response.getMerchantCode());
    Assertions.assertEquals(LOCATION_PATH_1, response.getImageQcConfidenceDetails().get(0).getLocationPath());
  }

  @Test
  public void toImageQcConfidenceDetailsNoImageQcResponseTest() {
    productCollection = new ProductCollection();
    productCollection.setProductId(PRODUCT_ID);
    productCollection.setProductCode(PRODUCT_CODE);
    productCollection.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productCollection.setUpdatedBy(USERNAME);
    AutoApprovalsDetailDto response = ConverterUtil
        .setAutoApprovalDetailDTO(STORE_ID, productCollection, CATEGORY_CODE, Arrays.asList(productImage),
            Collections.singletonMap(LOCATION_PATH_2, imageQcResponse));
    Assertions.assertNotNull(response);
    Assertions.assertEquals(PRODUCT_CODE, response.getProductCode());
    Assertions.assertEquals(USERNAME, response.getUpdatedBy());
    Assertions.assertEquals(CATEGORY_CODE, response.getC1CategoryCode());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, response.getMerchantCode());
  }

  @Test
  public void toRestrictedKeywordsByFieldResponseFromJsonTest() {
    List<RestrictedKeywordsByFieldResponse> restrictedKeywordsByFieldResponseList =
        ConverterUtil.toRestrictedKeywordsByFieldResponseFromJson(restrictedKeywordJson);
    Assertions.assertEquals(RestrictedKeywordFieldNames.PRODUCT_NAME.name(), restrictedKeywordsByFieldResponseList.get(0).getFieldIdentifier());
    Assertions.assertEquals(RESTRICTED_KEYWORDS, restrictedKeywordsByFieldResponseList.get(0).getKeywords().get(0));
  }

  @Test
  public void toRestrictedKeywordsByFieldResponseFromJsonExceptionTest() {
    List<RestrictedKeywordsByFieldResponse> restrictedKeywordsByFieldResponseList =
        ConverterUtil.toRestrictedKeywordsByFieldResponseFromJson("json");
    assertTrue(restrictedKeywordsByFieldResponseList.isEmpty());
  }

  @Test
  public void toBulkActivateDeactivateRequestTest() {
    WholesalePriceSkuResponse wholesalePriceSkuResponse1 =
        WholesalePriceSkuResponse.builder().itemSku(ITEM_SKU).skuStatus(Constants.ACTIVE_STATUS).build();
    WholesalePriceSkuResponse wholesalePriceSkuResponse2 =
        WholesalePriceSkuResponse.builder().itemSku(ITEM_SKU_2).skuStatus(Constants.INACTIVE_STATUS).build();
    ProductItemWholesalePrice productItemWholesalePrice1 = new ProductItemWholesalePrice();
    ProductItemWholesalePrice productItemWholesalePrice2 = new ProductItemWholesalePrice();
    ProductItemWholesalePrice productItemWholesalePrice3 = new ProductItemWholesalePrice();
    productItemWholesalePrice1.setItemSku(ITEM_SKU);
    productItemWholesalePrice1.setWholesalePriceActivated(false);
    productItemWholesalePrice2.setItemSku(ITEM_SKU_2);
    productItemWholesalePrice2.setWholesalePriceActivated(true);
    List<BulkActivateDeactivateRequest> bulkActivateDeactivateRequests = ConverterUtil.toBulkActivateDeactivateRequest(
        ImmutableMap.of(ITEM_SKU, wholesalePriceSkuResponse1, ITEM_SKU_2, wholesalePriceSkuResponse2),
        Arrays.asList(productItemWholesalePrice1, productItemWholesalePrice2, productItemWholesalePrice3));
    Assertions.assertEquals(2, bulkActivateDeactivateRequests.size());
  }

  @Test
  public void toBulkActivateDeactivateRequestNoChangeTest() {
    WholesalePriceSkuResponse wholesalePriceSkuResponse1 =
        WholesalePriceSkuResponse.builder().itemSku(ITEM_SKU).skuStatus(Constants.ACTIVE_STATUS).build();
    WholesalePriceSkuResponse wholesalePriceSkuResponse2 =
        WholesalePriceSkuResponse.builder().itemSku(ITEM_SKU_2).skuStatus(Constants.INACTIVE_STATUS).build();
    ProductItemWholesalePrice productItemWholesalePrice1 = new ProductItemWholesalePrice();
    ProductItemWholesalePrice productItemWholesalePrice2 = new ProductItemWholesalePrice();
    ProductItemWholesalePrice productItemWholesalePrice3 = new ProductItemWholesalePrice();
    productItemWholesalePrice1.setItemSku(ITEM_SKU);
    productItemWholesalePrice1.setWholesalePriceActivated(true);
    productItemWholesalePrice2.setItemSku(ITEM_SKU_2);
    productItemWholesalePrice2.setWholesalePriceActivated(false);
    List<BulkActivateDeactivateRequest> bulkActivateDeactivateRequests = ConverterUtil.toBulkActivateDeactivateRequest(
        ImmutableMap.of(ITEM_SKU, wholesalePriceSkuResponse1, ITEM_SKU_2, wholesalePriceSkuResponse2),
        Arrays.asList(productItemWholesalePrice1, productItemWholesalePrice2, productItemWholesalePrice3));
    Assertions.assertEquals(0, bulkActivateDeactivateRequests.size());
  }

  @Test
  public void toPDTDimensionRefreshEventModelTest() {
    DimensionRefreshRequest dimensionRefreshRequest =
        DimensionRefreshRequest.builder().length(LENGTH).width(WIDTH).height(HEIGHT).weight(WEIGHT)
            .shippingWeight(SHIPPING_WEIGHT).dangerousGoodsLevel(DG_LEVEL).productType(PRODUCT_TYPE).build();
    PDTDimensionRefreshEventModel pdtDimensionRefreshEventModel =
        ConverterUtil.toPDTDimensionRefreshEventModel(STORE_ID, PRODUCT_CODE, dimensionRefreshRequest);
    Assertions.assertEquals(STORE_ID, pdtDimensionRefreshEventModel.getStoreId());
    Assertions.assertEquals(PRODUCT_CODE, pdtDimensionRefreshEventModel.getProductCode());
    Assertions.assertEquals(LENGTH, pdtDimensionRefreshEventModel.getLength());
    Assertions.assertEquals(WIDTH, pdtDimensionRefreshEventModel.getWidth());
    Assertions.assertEquals(HEIGHT, pdtDimensionRefreshEventModel.getHeight());
    Assertions.assertEquals(WEIGHT, pdtDimensionRefreshEventModel.getWeight());
    Assertions.assertEquals(SHIPPING_WEIGHT, pdtDimensionRefreshEventModel.getShippingWeight());
    Assertions.assertEquals(DG_LEVEL, pdtDimensionRefreshEventModel.getDangerousGoodsLevel());
    Assertions.assertEquals(PRODUCT_TYPE, pdtDimensionRefreshEventModel.getProductType());
  }

  @Test
  public void toDimensionRefreshRequestTest() {
    DimensionRefreshRequest dimensionRefreshRequest =
        ConverterUtil.toDimensionRefreshRequest(LENGTH, WIDTH, HEIGHT, WEIGHT, SHIPPING_WEIGHT, DG_LEVEL, PRODUCT_TYPE);
    Assertions.assertEquals(LENGTH, dimensionRefreshRequest.getLength());
    Assertions.assertEquals(WIDTH, dimensionRefreshRequest.getWidth());
    Assertions.assertEquals(HEIGHT, dimensionRefreshRequest.getHeight());
    Assertions.assertEquals(WEIGHT, dimensionRefreshRequest.getWeight());
    Assertions.assertEquals(SHIPPING_WEIGHT, dimensionRefreshRequest.getShippingWeight());
    Assertions.assertEquals(DG_LEVEL, dimensionRefreshRequest.getDangerousGoodsLevel());
    Assertions.assertEquals(PRODUCT_TYPE, dimensionRefreshRequest.getProductType());
  }

  @Test
  public void toSimpleListStringRequestTest() {
    List<UpdatedProductHistory> updatedProductHistoryList = new ArrayList<>();
    UpdatedProductHistory updatedProductHistory = new UpdatedProductHistory();
    updatedProductHistory.setGdnSku(ITEM_SKU);
    updatedProductHistoryList.add(updatedProductHistory);
    SimpleListStringRequest simpleListStringRequest =
        ConverterUtil.toSimpleListStringRequest(updatedProductHistoryList);
    Assertions.assertEquals(ITEM_SKU, simpleListStringRequest.getValue().get(0));
  }

  @Test
  public void toItemSkuAndPickupPointCodeMapTest() {
    List<ItemSkuPickupPointCodeResponse> itemSkuPickupPointCodeResponseList = new ArrayList<>();
    ItemSkuPickupPointCodeResponse itemSkuPickupPointCodeResponse = new ItemSkuPickupPointCodeResponse();
    itemSkuPickupPointCodeResponse.setPickupPointCode(PICKUP_POINT_CODE);
    itemSkuPickupPointCodeResponse.setItemSku(ITEM_SKU);
    itemSkuPickupPointCodeResponseList.add(itemSkuPickupPointCodeResponse);
    Map<String, String> itemSkuAndPickupPointCodeMap =
        ConverterUtil.toItemSkuAndPickupPointCodeMap(itemSkuPickupPointCodeResponseList);
    Assertions.assertEquals(PICKUP_POINT_CODE, itemSkuAndPickupPointCodeMap.get(ITEM_SKU));
  }

  @Test
  public void toProductImageEditRequestTest() {
    ProductImageEditRequest request = new ProductImageEditRequest();
    request.setImagePath(LOCATION_PATH_1);
    com.gdn.x.productcategorybase.dto.request.ProductImageEditRequest productImageEditRequest =
        new com.gdn.x.productcategorybase.dto.request.ProductImageEditRequest();
    ItemImageEditRequest itemImageEditRequest1 = new ItemImageEditRequest();
    ItemImageEditRequest itemImageEditRequest2 = new ItemImageEditRequest();
    itemImageEditRequest1.setAdd(true);
    itemImageEditRequest1.setCopy(true);
    CopyImageEditRequest copyImageEditRequest = new CopyImageEditRequest();
    copyImageEditRequest.setAdd(true);
    copyImageEditRequest.setCopy(true);
    request.setProductItems(Arrays.asList(itemImageEditRequest1, itemImageEditRequest2));
    request.setCopyToAllVariantImages(copyImageEditRequest);

    ConverterUtil.toProductImageEditRequest(request, PRODUCT_CODE, new HashMap<>(), productImageEditRequest);
    Assertions.assertTrue(request.isImageAdded());
  }

  @Test
  public void toProductImageEditRequestCopyRequestFalseTest() {
    ProductImageEditRequest request = new ProductImageEditRequest();
    request.setImagePath(LOCATION_PATH_1);
    com.gdn.x.productcategorybase.dto.request.ProductImageEditRequest productImageEditRequest =
        new com.gdn.x.productcategorybase.dto.request.ProductImageEditRequest();
    CopyImageEditRequest copyImageEditRequest = new CopyImageEditRequest();
    request.setCopyToAllVariantImages(copyImageEditRequest);

    ConverterUtil.toProductImageEditRequest(request, PRODUCT_CODE, new HashMap<>(), productImageEditRequest);

    Assertions.assertFalse(request.isImageAdded());
    Assertions.assertFalse(request.isImageUpdated());
  }

  @Test
  public void toProductImageEditRequestNullTest() {
    ProductImageEditRequest request = new ProductImageEditRequest();
    request.setImagePath(LOCATION_PATH_1);
    com.gdn.x.productcategorybase.dto.request.ProductImageEditRequest productImageEditRequest =
        new com.gdn.x.productcategorybase.dto.request.ProductImageEditRequest();

    ConverterUtil.toProductImageEditRequest(request, PRODUCT_CODE, new HashMap<>(), productImageEditRequest);

    assertTrue(CollectionUtils.isEmpty(productImageEditRequest.getProductItems()));
    Assertions.assertNull(productImageEditRequest.getCopyToAllVariantImages());
  }

  @Test
  public void getErrorCodeTestImageNotPresent() {
    Assertions.assertEquals(ApiErrorCode.IMAGE_NOT_PRESENT,
        ConverterUtil.getErrorCode(ApiErrorCode.IMAGE_NOT_PRESENT.getDesc()));
  }

  @Test
  public void getErrorCodeTestMaxImageReached() {
    Assertions.assertEquals(ApiErrorCode.MAX_IMAGE_REACHED,
        ConverterUtil.getErrorCode(ApiErrorCode.MAX_IMAGE_REACHED.getDesc()));
  }

  @Test
  public void getErrorCodeTestImageAlreadyExists() {
    Assertions.assertEquals(ApiErrorCode.IMAGE_ALREADY_EXISTS,
        ConverterUtil.getErrorCode(ApiErrorCode.IMAGE_ALREADY_EXISTS.getDesc()));
  }

  @Test
  public void getErrorCodeTestCannotDeleteMainImage() {
    Assertions.assertEquals(ApiErrorCode.CANNOT_DELETE_MAIN_IMAGE,
        ConverterUtil.getErrorCode(ApiErrorCode.CANNOT_DELETE_MAIN_IMAGE.getDesc()));
  }

  @Test
  public void getErrorCodeTestOnlyImageInItem() {
    Assertions.assertEquals(ApiErrorCode.ONLY_IMAGE_IN_ITEM,
        ConverterUtil.getErrorCode(ApiErrorCode.ONLY_IMAGE_IN_ITEM.getDesc()));
  }

  @Test
  public void getErrorCodeTestImage() {
    Assertions.assertNull(
        ConverterUtil.getErrorCode(StringUtils.EMPTY));
  }

  @Test
  public void getCodeFromProductType(){
    ConverterUtil.getCodeFromProductType(ProductType.BIG_PRODUCT);
    ConverterUtil.getCodeFromProductType(ProductType.BOPIS);
  }

  @Test
  public void toHistoryUpdateResponseListTest() {
    List<HistoryUpdateResponse> historyUpdateResponseList =
      ConverterUtil.toHistoryUpdateResponseList(Collections.singletonList(updatedProductHistory));
    Assertions.assertEquals(Constants.ALL_VARIANTS, historyUpdateResponseList.get(0).getGdnSku());
  }

  @Test
  public void toHistoryUpdateResponseList_withGdnSkuTest() {
    updatedProductHistory.setGdnSku(ITEM_SKU);
    updatedProductHistory.setPickupPointCode(Constants.HYPHEN);
    List<HistoryUpdateResponse> historyUpdateResponseList =
      ConverterUtil.toHistoryUpdateResponseList(Collections.singletonList(updatedProductHistory));
    Assertions.assertEquals(ITEM_SKU, historyUpdateResponseList.get(0).getGdnSku());
    Assertions.assertEquals(Constants.ALL_PICKUP_POINTS, historyUpdateResponseList.get(0).getPickupPointCode());
    Assertions.assertEquals(Constants.ALL_PICKUP_POINTS, historyUpdateResponseList.get(0).getPickupPointName());
  }

  @Test
  public void setPickupPointNameTest() {
    Page<HistoryUpdateResponse> response = ConverterUtil.setPickupPointName(
      new PageImpl<>(Collections.singletonList(historyUpdateResponse)),
        Collections.singletonMap(PICKUP_POINT_CODE, businessPartnerPickupPointResponse));
    Assertions.assertEquals(PICKUP_POINT_CODE, response.getContent().get(0).getPickupPointCode());
    Assertions.assertEquals(PICKUP_POINT_NAME, response.getContent().get(0).getPickupPointName());
  }

  @Test
  public void setPickupPointNameAllPPCodesTest() {
    historyUpdateResponse.setPickupPointCode(Constants.ALL_PICKUP_POINTS);
    historyUpdateResponse.setPickupPointName(Constants.ALL_PICKUP_POINTS);
    Page<HistoryUpdateResponse> response = ConverterUtil.setPickupPointName(
        new PageImpl<>(Collections.singletonList(historyUpdateResponse)),
        Collections.singletonMap(PICKUP_POINT_CODE, businessPartnerPickupPointResponse));
    Assertions.assertEquals(Constants.ALL_PICKUP_POINTS, response.getContent().get(0).getPickupPointCode());
    Assertions.assertEquals(Constants.ALL_PICKUP_POINTS, response.getContent().get(0).getPickupPointName());
  }

  @Test
  public void setPickupPointName_nullPickupPointResponseTest() {
    historyUpdateResponse.setPickupPointCode(PICKUP_POINT_NAME);
    Page<HistoryUpdateResponse> response = ConverterUtil.setPickupPointName(
      new PageImpl<>(Collections.singletonList(historyUpdateResponse)),
      Collections.singletonMap(PICKUP_POINT_CODE, businessPartnerPickupPointResponse));
    Assertions.assertEquals(PICKUP_POINT_NAME, response.getContent().get(0).getPickupPointCode());
    Assertions.assertEquals(Constants.DASH_DELIMITER, response.getContent().get(0).getPickupPointName());
  }

  @Test
  public void setOfferPriceForPartialUpdateTest() {
    QuickEditV2Request quickEditV2Request = new QuickEditV2Request();
    quickEditV2Request.setItemSku(ITEM_SKU);
    quickEditV2Request.setPickupPointCode(PICKUP_POINT_CODE);
    ItemSummaryListResponse itemSummaryListResponse = new ItemSummaryListResponse();
    ItemRequest itemRequest = new ItemRequest();
    PriceDTO priceDTO = new PriceDTO();
    priceDTO.setListPrice(NEW_NORMAL_PRICE);
    priceDTO.setOfferPrice(NEW_SALE_PRICE);
    itemSummaryListResponse.setPrice(Collections.singleton(priceDTO));
    ConverterUtil.setOfferPriceForPartialUpdate(quickEditV2Request, itemSummaryListResponse,
      itemRequest);
    Assertions.assertNotNull(itemRequest.getPrice());
  }

  @Test
  public void setOfferPriceForPartialUpdate_withMerchantPromoDiscountTest() {
    QuickEditV2Request quickEditV2Request = new QuickEditV2Request();
    quickEditV2Request.setItemSku(ITEM_SKU);
    quickEditV2Request.setPickupPointCode(PICKUP_POINT_CODE);
    ItemSummaryListResponse itemSummaryListResponse = new ItemSummaryListResponse();
    ItemRequest itemRequest = new ItemRequest();
    PriceDTO priceDTO = new PriceDTO();
    priceDTO.setMerchantPromoDiscountPrice(
      DiscountPriceDTO.builder().discountPrice(DISCOUNT_AMOUNT).campaignCode(CATEGORY_CODE)
        .startDateTime(CREATED_DATE).endDateTime(SUBMITTED_DATE).build());
    priceDTO.setListPrice(NEW_NORMAL_PRICE);
    priceDTO.setOfferPrice(NEW_SALE_PRICE);
    itemSummaryListResponse.setPrice(Collections.singleton(priceDTO));
    ConverterUtil.setOfferPriceForPartialUpdate(quickEditV2Request, itemSummaryListResponse,
      itemRequest);
    Assertions.assertNotNull(itemRequest.getPrice());
    Assertions.assertEquals(DISCOUNT_PRICE, quickEditV2Request.getPrice().getDiscountAmount());
  }

  @Test
  public void toWholesalePriceBulkUpdateRequestL5Test() {
    WholesalePriceBulkUpdateRequest wholesalePriceBulkUpdateRequest = ConverterUtil.toWholesalePriceBulkUpdateRequestL5(PRODUCT_SKU, ITEM_CODE, ITEM_SKU, PICKUP_POINT_CODE, false,
        new ArrayList<>(), false);
    Assertions.assertNotNull(wholesalePriceBulkUpdateRequest);
  }

  @Test
  public void toWholesalePriceBulkUpdateRequestL5PriceTest() {
    ProductItemWholesalePriceRequest productItemWholesalePriceRequest = new ProductItemWholesalePriceRequest();
    productItemWholesalePriceRequest.setQuantity(QUANTITY);
    productItemWholesalePriceRequest.setWholesaleDiscount(WHOLESALE_DISCOUNT);
    WholesalePriceBulkUpdateRequest wholesalePriceBulkUpdateRequest = ConverterUtil.toWholesalePriceBulkUpdateRequestL5(PRODUCT_SKU, ITEM_CODE, ITEM_SKU, PICKUP_POINT_CODE, false,
        Collections.singletonList(productItemWholesalePriceRequest), false);
    Assertions.assertNotNull(wholesalePriceBulkUpdateRequest);
  }

  @Test
  public void copyToItemImagesL5NRTrueTest() {
    List<ProductLevel3SummaryDetailsImageRequest> copyToAllVariantImages = new ArrayList<>();
    ProductLevel3SummaryDetailsImageRequest productLevel3SummaryDetailsImageRequest = new ProductLevel3SummaryDetailsImageRequest();
    productLevel3SummaryDetailsImageRequest.setReviewType(NEW);
    productLevel3SummaryDetailsImageRequest.setMainImage(true);
    productLevel3SummaryDetailsImageRequest.setMarkForDelete(false);
    productLevel3SummaryDetailsImageRequest.setSequence(1);
    productLevel3SummaryDetailsImageRequest.setLocationPath(LOCATION_PATH_1);
    copyToAllVariantImages.add(productLevel3SummaryDetailsImageRequest);
    List<Image> images = ConverterUtil.copyToItemImagesL5(copyToAllVariantImages, true);
    Assertions.assertEquals(1, images.size());
    Assertions.assertEquals(LOCATION_PATH_1, images.get(0).getLocationPath());
  }

  @Test
  public void copyToItemImagesL5UpdateImageTest() {
    List<ProductLevel3SummaryDetailsImageRequest> copyToAllVariantImages = new ArrayList<>();
    ProductLevel3SummaryDetailsImageRequest productLevel3SummaryDetailsImageRequest = new ProductLevel3SummaryDetailsImageRequest();
    productLevel3SummaryDetailsImageRequest.setReviewType(UPDATE);
    productLevel3SummaryDetailsImageRequest.setMainImage(true);
    productLevel3SummaryDetailsImageRequest.setMarkForDelete(false);
    productLevel3SummaryDetailsImageRequest.setSequence(1);
    productLevel3SummaryDetailsImageRequest.setLocationPath(LOCATION_PATH_1);
    copyToAllVariantImages.add(productLevel3SummaryDetailsImageRequest);
    List<Image> images = ConverterUtil.copyToItemImagesL5(copyToAllVariantImages, false);
    Assertions.assertEquals(1, images.size());
    Assertions.assertEquals(LOCATION_PATH_1, images.get(0).getLocationPath());
  }

  @Test
  public void getDeletePickupPointDeleteRequestTest() {
    ProductVariantUpdateRequest request = new ProductVariantUpdateRequest();
    PickupPointDeleteRequest pickupPointDeleteRequest =
      PickupPointDeleteRequest.builder().pickupPointId(PICKUP_POINT_CODE).itemSku(ITEM_SKU).build();
    request.setDeletePickupPoints(Arrays.asList(pickupPointDeleteRequest));
    List<ItemPickupPointDeleteRequest> result =
      ConverterUtil.getDeletePickupPointDeleteRequest(request);
    Assertions.assertEquals(result.size(),1);
  }

  @Test
  public void constructItemL5ListingResponseMppTest() {
    inventoryDatas.put(ITEM_SKU + "-" + PICKUP_POINT_CODE, inventory);
    BulkDownloadProductLevel3Response downloadProductLevel3Response =
      ConverterUtil.constructItemL5ListingResponse(productDatas, inventoryDatas);
    Assertions.assertEquals(PRODUCT_SKU,downloadProductLevel3Response.getProductLevel3SummaryResponses().get(0).getProductSku());
    Assertions.assertEquals(PRICE,downloadProductLevel3Response.getProductLevel3SummaryResponses().get(0).getPrices().get(0).getPrice());
    Assertions.assertEquals(DISPLAY,downloadProductLevel3Response.getProductLevel3SummaryResponses().get(0).getViewConfigs().get(0).getDisplay());
    Assertions.assertEquals(WAREHOUSE_AVAILABLE,downloadProductLevel3Response.getProductLevel3SummaryResponses().get(0).getAvailableStockLevel1());
  }

  @Test
  public void constructItemL5ListingResponseMppItemSkuFalseTest() {
    productDatas.get(0).setProductType(ProductType.BIG_PRODUCT);
    BulkDownloadProductLevel3Response downloadProductLevel3Response =
      ConverterUtil.constructItemL5ListingResponse(productDatas, inventoryDatas);
    Assertions.assertEquals(PRODUCT_SKU,downloadProductLevel3Response.getProductLevel3SummaryResponses().get(0).getProductSku());
    Assertions.assertEquals(PRICE,downloadProductLevel3Response.getProductLevel3SummaryResponses().get(0).getPrices().get(0).getPrice());
    Assertions.assertEquals(DISPLAY,downloadProductLevel3Response.getProductLevel3SummaryResponses().get(0).getViewConfigs().get(0).getDisplay());
    Assertions.assertEquals(WAREHOUSE_AVAILABLE,downloadProductLevel3Response.getProductLevel3SummaryResponses().get(0).getAvailableStockLevel1());
    Assertions.assertEquals(2, downloadProductLevel3Response.getProductLevel3SummaryResponses().get(0).getProductType());
  }

  @Test
  public void getProductRequestForProductDiscardTest() {
    Date date = new Date();
    ProductRequest productRequest = ConverterUtil.getProductRequestForProductDiscard(CREATED_BY, date, PRODUCT_ID);
    Assertions.assertEquals(CREATED_BY, productRequest.getCreatedBy());
    Assertions.assertEquals(PRODUCT_ID, productRequest.getId());
    Assertions.assertEquals(date, productRequest.getCreatedDate());
  }

  @Test
  public void setDimensionDetailsTest() {
    Product product = new Product();
    product.setHeight(10.0);
    product.setLength(10.0);
    product.setWeight(10.0);
    product.setWidth(10.0);
    product.setShippingWeight(10.0);
    ProductLevel3 productLevel3 = new ProductLevel3();
    ConverterUtil.setDimensionDetails(product, productLevel3);
    Assertions.assertEquals(10.0, product.getHeight().doubleValue(), 0);
    Assertions.assertEquals(10.0, product.getLength().doubleValue(), 0);
    Assertions.assertEquals(10.0, product.getWidth().doubleValue(), 0);
    Assertions.assertEquals(10.0, product.getWeight().doubleValue(), 0);
    Assertions.assertEquals(10.0, product.getShippingWeight().doubleValue(), 0);

    productLevel3.setHeight(20.0);
    productLevel3.setLength(20.0);
    productLevel3.setWeight(20.0);
    productLevel3.setWidth(20.0);
    productLevel3.setShippingWeight(20.0);
    ConverterUtil.setDimensionDetails(product, productLevel3);
    Assertions.assertEquals(20.0, product.getHeight().doubleValue(), 0);
    Assertions.assertEquals(20.0, product.getLength().doubleValue(), 0);
    Assertions.assertEquals(20.0, product.getWidth().doubleValue(), 0);
    Assertions.assertEquals(20.0, product.getWeight().doubleValue(), 0);
    Assertions.assertEquals(20.0, product.getShippingWeight().doubleValue(), 0);
  }

  @Test
  public void setProductBusinessPartnerForCategoryChangeTest(){
    ProductCollection productCollection = new ProductCollection();
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    productCollection.setCategoryCode(CATEGORY_CODE);
    productCollection.setCategoryName(CATEGORY_NAME);
    productCollection.setUpdatedDate(null);
    ConverterUtil.setProductBusinessPartnerForCategoryChange(productCollection,productBusinessPartner);
    Assertions.assertEquals(productBusinessPartner.getCategoryCode(), productCollection.getCategoryCode());
    Assertions.assertEquals(productBusinessPartner.getCategoryName(), productCollection.getCategoryName());
    assertTrue(Objects.nonNull(productBusinessPartner.getUpdatedDate()));
  }

  @Test
  public void generateInternalProductHistoryEventModelTest() {
    InternalProductHistoryEventModel internalProductHistoryEventModel =
        ConverterUtil.generateInternalProductHistoryEventModel(PRODUCT_CODE, ACTIVITY, USERNAME, NOTES);
    Assertions.assertEquals(ACTIVITY, internalProductHistoryEventModel.getActivity());
    Assertions.assertEquals(NOTES, internalProductHistoryEventModel.getNotes());
    Assertions.assertEquals(USERNAME, internalProductHistoryEventModel.getUsername());
    Assertions.assertEquals(STORE_ID, internalProductHistoryEventModel.getStoreId());
    Assertions.assertEquals(PRODUCT_CODE, internalProductHistoryEventModel.getProductCode());

  }

  @Test
  public void setProductCollectionDetailsForCategoryChangeTest(){
    ProductCollection productCollection = new ProductCollection();
    CategorySummaryResponse categorySummaryResponse = new CategorySummaryResponse();
    categorySummaryResponse.setCategoryCode(CATEGORY_CODE);
    categorySummaryResponse.setCategoryName(CATEGORY_NAME);
    productCollection.setUpdatedDate(null);
    ConverterUtil.setProductCollectionDetailsForCategoryChange(productCollection,categorySummaryResponse);
    Assertions.assertEquals(categorySummaryResponse.getCategoryCode(), productCollection.getCategoryCode());
    Assertions.assertEquals(categorySummaryResponse.getCategoryName(), productCollection.getCategoryName());
    assertTrue(Objects.nonNull(productCollection.getUpdatedDate()));
  }

  @Test
  public void setProductCollectionDetailsForCategoryChangeNeedRevisionTest(){
    ProductCollection productCollection = new ProductCollection();
    CategorySummaryResponse categorySummaryResponse = new CategorySummaryResponse();
    CategoryRestrictedKeywordResponse messageById = new CategoryRestrictedKeywordResponse();
    categorySummaryResponse.setCategoryName(CATEGORY_NAME);
    messageById.setDestinationCategory(CATEGORY_CODE);
    productCollection.setUpdatedDate(null);
    ConverterUtil.setProductCollectionDetailsForCategoryChangeNeedRevision(productCollection, messageById,
        categorySummaryResponse);
    Assertions.assertEquals(messageById.getDestinationCategory(), productCollection.getCategoryCode());
    Assertions.assertEquals(categorySummaryResponse.getCategoryName(), productCollection.getCategoryName());
    assertTrue(Objects.nonNull(productCollection.getUpdatedDate()));
  }


  @Test
  public void publishImageQcEventForContentEditTest() {
    ImageQcRequestDomainEvent imageQcRequestDomainEvent = ConverterUtil
        .toImageQcRequestForContentEdit(categoryResponses,
            new RestrictedKeywordsByFieldAndActionType(), new ProductLevel3());
    Assertions.assertEquals(CATEGORY_CODE1, imageQcRequestDomainEvent.getCategoryHierarchy().get(0).getCategoryCode());
    Assertions.assertEquals(CATEGORY_CODE2, imageQcRequestDomainEvent.getCategoryHierarchy().get(1).getCategoryCode());
    Assertions.assertEquals(CATEGORY_NAME1, imageQcRequestDomainEvent.getCategoryHierarchy().get(0).getCategoryName());
    Assertions.assertEquals(CATEGORY_NAME2, imageQcRequestDomainEvent.getCategoryHierarchy().get(1).getCategoryName());
    Assertions.assertEquals(CATEGORY_CODE2, imageQcRequestDomainEvent.getCategoryCode());
  }

  @Test
  public void updateCategoryInfoInImageQcRequestCategoryCodesNullTest() {
    ImageQcRequestDomainEvent imageQcRequestDomainEvent = new ImageQcRequestDomainEvent();
    ConverterUtil.updateCategoryInfoInImageQcRequest(imageQcRequestDomainEvent, null, null);
    Assertions.assertNull(imageQcRequestDomainEvent.getCategoryCode());
    Assertions.assertEquals(0, imageQcRequestDomainEvent.getCategoryHierarchy().size());
  }

  @Test
  public void updateCategoryInfoInImageQcRequestCategoryNameNullTest() {
    ImageQcRequestDomainEvent imageQcRequestDomainEvent = new ImageQcRequestDomainEvent();
    ConverterUtil.updateCategoryInfoInImageQcRequest(imageQcRequestDomainEvent,
        Arrays.asList(CATEGORY_CODE1, CATEGORY_CODE2), null);
    Assertions.assertEquals(CATEGORY_CODE2, imageQcRequestDomainEvent.getCategoryCode());
    Assertions.assertEquals(0, imageQcRequestDomainEvent.getCategoryHierarchy().size());
  }

  @Test
  public void updateCategoryInfoInImageQcRequestCategoryCodeCategoryNameDiffSizeTest() {
    ImageQcRequestDomainEvent imageQcRequestDomainEvent = new ImageQcRequestDomainEvent();
    ConverterUtil.updateCategoryInfoInImageQcRequest(imageQcRequestDomainEvent,
        Arrays.asList(CATEGORY_CODE1, CATEGORY_CODE2), Arrays.asList(CATEGORY_NAME1));
    Assertions.assertEquals(CATEGORY_CODE2, imageQcRequestDomainEvent.getCategoryCode());
    Assertions.assertEquals(0, imageQcRequestDomainEvent.getCategoryHierarchy().size());
  }

  @Test
  public void updateCategoryInfoInImageQcRequestTest() {
    ImageQcRequestDomainEvent imageQcRequestDomainEvent = new ImageQcRequestDomainEvent();
    ConverterUtil.updateCategoryInfoInImageQcRequest(imageQcRequestDomainEvent,
        Arrays.asList(CATEGORY_CODE1, CATEGORY_CODE2), Arrays.asList(CATEGORY_NAME1, CATEGORY_NAME2));
    Assertions.assertEquals(2, imageQcRequestDomainEvent.getCategoryHierarchy().size());
    Assertions.assertEquals(CATEGORY_CODE1, imageQcRequestDomainEvent.getCategoryHierarchy().get(0).getCategoryCode());
    Assertions.assertEquals(1, imageQcRequestDomainEvent.getCategoryHierarchy().get(0).getHierarchy());
    Assertions.assertEquals(2, imageQcRequestDomainEvent.getCategoryHierarchy().get(1).getHierarchy());
    Assertions.assertEquals(CATEGORY_CODE2, imageQcRequestDomainEvent.getCategoryHierarchy().get(1).getCategoryCode());
    Assertions.assertEquals(CATEGORY_NAME1, imageQcRequestDomainEvent.getCategoryHierarchy().get(0).getCategoryName());
    Assertions.assertEquals(CATEGORY_NAME2, imageQcRequestDomainEvent.getCategoryHierarchy().get(1).getCategoryName());
    Assertions.assertEquals(CATEGORY_CODE2, imageQcRequestDomainEvent.getCategoryCode());
  }

  @Test
  public void toItemSummaryL4ResponseTest() throws JsonProcessingException {
    String bundleRecipe = "[{\"itemSku\":\"item-sku\",\"quantity\":10}]";
    Map<String, ItemBasicDetailV2Response> itemBasicDetailV2ResponseMap = new HashMap<>();
    itemBasicDetailV2ResponseMap.put(ITEM_SKU,
        ItemBasicDetailV2Response.builder().itemSku(ITEM_SKU).itemCode(ITEM_CODE).sharedProduct(true)
            .mainImageUrl(MAIN_IMAGE_URL).build());
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
    productItemBusinessPartner.setGdnProductItemSku(ITEM_SKU);
    productItemBusinessPartner.setBundleRecipe(bundleRecipe);
    productBusinessPartner.setProductItemBusinessPartners(Arrays.asList(productItemBusinessPartner));
    ProductItemResponse productItemResponse = new ProductItemResponse();
    productItemResponse.setId(UUID.randomUUID().toString());
    Image image = new Image();
    image.setMainImages(true);
    image.setOriginalImage(false);
    image.setLocationPath(GCS_SOURCE_URL);
    productItemResponse.setImages(Arrays.asList(image));
    Map<String, ProductItemResponse> itemIdToProductItemResponseMap = new HashMap<>();
    itemIdToProductItemResponseMap.put(productItemResponse.getId(), productItemResponse);
    ItemSkuToItemIdMapping itemSkuToItemIdMapping = new ItemSkuToItemIdMapping();
    itemSkuToItemIdMapping.setGdnProductItemSku(ITEM_SKU);
    itemSkuToItemIdMapping.setProductItemId(productItemResponse.getId());
    List<ItemSummaryL4Response> responses =
        ConverterUtil.toItemSummaryL4Response(productBusinessPartner, Collections.singletonList(itemSkuToItemIdMapping),
            itemIdToProductItemResponseMap, itemBasicDetailV2ResponseMap);
    Assertions.assertEquals(1,responses.size());
    Assertions.assertEquals(GCS_SOURCE_URL, responses.get(0).getMainImageUrl());
    Assertions.assertEquals(true, responses.get(0).isSharedProduct());
  }

  @Test
  public void getProductStatusTest(){
    ItemBasicDetailV2Response itemBasicDetailV2Response = new ItemBasicDetailV2Response();
    itemBasicDetailV2Response.setArchived(true);
    String result = ConverterUtil.getProductStatus(itemBasicDetailV2Response);
    Assertions.assertEquals(result, ProductStatus.INACTIVE.name());
  }

  @Test
  public void getProductStatuswithMfdTrueTest(){
    ItemBasicDetailV2Response itemBasicDetailV2Response = new ItemBasicDetailV2Response();
    itemBasicDetailV2Response.setMarkForDelete(true);
    String result = ConverterUtil.getProductStatus(itemBasicDetailV2Response);
    Assertions.assertEquals(result, ProductStatus.INACTIVE.name());
  }

  @Test
  public void toItemSummaryL4ResponseTest1() throws JsonProcessingException {
    String bundleRecipe = "[{\"itemSku\":\"itemSku\",\"quantity\":10}]";
    Map<String, ItemBasicDetailV2Response> itemBasicDetailV2ResponseMap = new HashMap<>();
    itemBasicDetailV2ResponseMap.put(ITEM_SKU,
        ItemBasicDetailV2Response.builder().itemSku(ITEM_SKU).itemCode(ITEM_CODE).mainImageUrl(MAIN_IMAGE_URL).build());
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
    productItemBusinessPartner.setGdnProductItemSku(ITEM_SKU);
    productItemBusinessPartner.setBundleRecipe(bundleRecipe);
    productBusinessPartner.setProductItemBusinessPartners(Arrays.asList(productItemBusinessPartner));
    ProductItemResponse productItemResponse = new ProductItemResponse();
    productItemResponse.setId(UUID.randomUUID().toString());
    Image image = new Image();
    image.setMarkForDelete(true);
    productItemResponse.setImages(Arrays.asList(image));
    Map<String, ProductItemResponse> itemIdToProductItemResponseMap = new HashMap<>();
    itemIdToProductItemResponseMap.put(productItemResponse.getId(), productItemResponse);
    ItemSkuToItemIdMapping itemSkuToItemIdMapping = new ItemSkuToItemIdMapping();
    itemSkuToItemIdMapping.setGdnProductItemSku(ITEM_SKU);
    itemSkuToItemIdMapping.setProductItemId(productItemResponse.getId());
    List<ItemSummaryL4Response> responses = ConverterUtil
        .toItemSummaryL4Response(productBusinessPartner, Arrays.asList(itemSkuToItemIdMapping),
            itemIdToProductItemResponseMap, itemBasicDetailV2ResponseMap);
    Assertions.assertEquals(1,responses.size());
  }

  @Test
  public void toItemSummaryL4ResponseTest2() throws JsonProcessingException {
    String bundleRecipe = "[{\"itemSku\":\"itemSku\",\"quantity\":10}]";
    Map<String, ItemBasicDetailV2Response> itemBasicDetailV2ResponseMap = new HashMap<>();
    itemBasicDetailV2ResponseMap.put(ITEM_SKU,
        ItemBasicDetailV2Response.builder().itemSku(ITEM_SKU).itemCode(ITEM_CODE).mainImageUrl(MAIN_IMAGE_URL).build());
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
    productItemBusinessPartner.setGdnProductItemSku(ITEM_SKU);
    productItemBusinessPartner.setBundleRecipe(bundleRecipe);
    productBusinessPartner.setProductItemBusinessPartners(Arrays.asList(productItemBusinessPartner));
    ProductItemResponse productItemResponse = new ProductItemResponse();
    productItemResponse.setId(UUID.randomUUID().toString());
    Image image = new Image();
    image.setMainImages(true);
    image.setEdited(true);
    image.setActive(true);
    image.setLocationPath(GCS_SOURCE_URL);
    productItemResponse.setImages(Arrays.asList(image));
    Map<String, ProductItemResponse> itemIdToProductItemResponseMap = new HashMap<>();
    itemIdToProductItemResponseMap.put(productItemResponse.getId(), productItemResponse);
    ItemSkuToItemIdMapping itemSkuToItemIdMapping = new ItemSkuToItemIdMapping();
    itemSkuToItemIdMapping.setGdnProductItemSku(ITEM_SKU);
    itemSkuToItemIdMapping.setProductItemId(productItemResponse.getId());
    List<ItemSummaryL4Response> responses = ConverterUtil
        .toItemSummaryL4Response(productBusinessPartner, Arrays.asList(itemSkuToItemIdMapping),
            itemIdToProductItemResponseMap, itemBasicDetailV2ResponseMap);
    Assertions.assertEquals(1,responses.size());
    Assertions.assertEquals(GCS_SOURCE_URL, responses.get(0).getMainImageUrl());
  }

  @Test
  public void toItemSummaryL4ResponseTest3() throws JsonProcessingException {
    String bundleRecipe = "[{\"itemSku\":\"itemSku\",\"quantity\":10}]";
    Map<String, ItemBasicDetailV2Response> itemBasicDetailV2ResponseMap = new HashMap<>();
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
    productItemBusinessPartner.setGdnProductItemSku(ITEM_SKU);
    productItemBusinessPartner.setBundleRecipe(bundleRecipe);
    productBusinessPartner.setProductItemBusinessPartners(Arrays.asList(productItemBusinessPartner));
    ProductItemResponse productItemResponse = new ProductItemResponse();
    productItemResponse.setId(UUID.randomUUID().toString());
    Image image = new Image();
    image.setMainImages(true);
    image.setRevised(true);
    image.setActive(true);
    image.setLocationPath(GCS_SOURCE_URL);
    productItemResponse.setImages(Arrays.asList(image));
    Map<String, ProductItemResponse> itemIdToProductItemResponseMap = new HashMap<>();
    itemIdToProductItemResponseMap.put(productItemResponse.getId(), productItemResponse);
    ItemSkuToItemIdMapping itemSkuToItemIdMapping = new ItemSkuToItemIdMapping();
    itemSkuToItemIdMapping.setGdnProductItemSku(ITEM_SKU);
    itemSkuToItemIdMapping.setProductItemId(productItemResponse.getId());
    List<ItemSummaryL4Response> responses = ConverterUtil
        .toItemSummaryL4Response(productBusinessPartner, Arrays.asList(itemSkuToItemIdMapping),
            itemIdToProductItemResponseMap, itemBasicDetailV2ResponseMap);
    Assertions.assertEquals(1,responses.size());
    Assertions.assertEquals(GCS_SOURCE_URL, responses.get(0).getMainImageUrl());
  }

  @Test
  public void toItemSummaryL4ResponseTest4() throws JsonProcessingException {
    Map<String, ItemBasicDetailV2Response> itemBasicDetailV2ResponseMap = new HashMap<>();
    itemBasicDetailV2ResponseMap.put(ITEM_SKU,
        ItemBasicDetailV2Response.builder().itemSku(ITEM_SKU).itemCode(ITEM_CODE).mainImageUrl(MAIN_IMAGE_URL).build());
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
    productItemBusinessPartner.setGdnProductItemSku(ITEM_SKU);
    productBusinessPartner.setProductItemBusinessPartners(Arrays.asList(productItemBusinessPartner));
    ProductItemResponse productItemResponse = new ProductItemResponse();
    productItemResponse.setId(UUID.randomUUID().toString());
    Image image = new Image();
    image.setMainImages(true);
    image.setOriginalImage(false);
    image.setLocationPath(GCS_SOURCE_URL);
    productItemResponse.setImages(Arrays.asList(image));
    Map<String, ProductItemResponse> itemIdToProductItemResponseMap = new HashMap<>();
    itemIdToProductItemResponseMap.put(productItemResponse.getId(), productItemResponse);
    ItemSkuToItemIdMapping itemSkuToItemIdMapping = new ItemSkuToItemIdMapping();
    itemSkuToItemIdMapping.setGdnProductItemSku(ITEM_SKU);
    itemSkuToItemIdMapping.setProductItemId(productItemResponse.getId());
    List<ItemSummaryL4Response> responses = ConverterUtil
        .toItemSummaryL4Response(productBusinessPartner, Arrays.asList(itemSkuToItemIdMapping),
            itemIdToProductItemResponseMap, itemBasicDetailV2ResponseMap);
    Assertions.assertEquals(1,responses.size());
    Assertions.assertEquals(GCS_SOURCE_URL, responses.get(0).getMainImageUrl());
  }

  @Test
  public void toItemSummaryL4ResponseTest5() throws JsonProcessingException {
    String bundleRecipe = "[{\"itemSku\":\"itemSku\",\"quantity\":10}]";
    Map<String, ItemBasicDetailV2Response> itemBasicDetailV2ResponseMap = new HashMap<>();
    itemBasicDetailV2ResponseMap.put(ITEM_SKU,
        ItemBasicDetailV2Response.builder().itemSku(ITEM_SKU).itemCode(ITEM_CODE).mainImageUrl(MAIN_IMAGE_URL).build());
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
    productItemBusinessPartner.setGdnProductItemSku(ITEM_SKU);
    productItemBusinessPartner.setBundleRecipe(bundleRecipe);
    productBusinessPartner.setProductItemBusinessPartners(Arrays.asList(productItemBusinessPartner));
    ProductItemResponse productItemResponse = new ProductItemResponse();
    productItemResponse.setId(UUID.randomUUID().toString());
    Image image = new Image();
    image.setMainImages(true);
    image.setOriginalImage(true);
    image.setLocationPath(GCS_SOURCE_URL);
    productItemResponse.setImages(Arrays.asList(image));
    Map<String, ProductItemResponse> itemIdToProductItemResponseMap = new HashMap<>();
    itemIdToProductItemResponseMap.put(productItemResponse.getId(), productItemResponse);
    ItemSkuToItemIdMapping itemSkuToItemIdMapping = new ItemSkuToItemIdMapping();
    itemSkuToItemIdMapping.setGdnProductItemSku(ITEM_SKU);
    itemSkuToItemIdMapping.setProductItemId(productItemResponse.getId());
    List<ItemSummaryL4Response> responses = ConverterUtil
        .toItemSummaryL4Response(productBusinessPartner, Arrays.asList(itemSkuToItemIdMapping),
            itemIdToProductItemResponseMap, itemBasicDetailV2ResponseMap);
    Assertions.assertEquals(1, responses.size());
    Assertions.assertEquals(null, responses.get(0).getMainImageUrl());
  }

  @Test
  public void createInternalProductHistoryEventModelTest() {
    ProductCollection productCollection2 = new ProductCollection();
    productCollection2.setProductCode(PRODUCT_CODE);
    productCollection2.setStoreId(STORE_ID);
    String oldCategoryName = "OLD_CATEGORY";
    InternalProductHistoryEventModel internalProductHistoryEventModel =
        ConverterUtil.createInternalProductHistoryEventModel(productCollection2, oldCategoryName, StringUtils.EMPTY);
    Assertions.assertEquals(PRODUCT_CODE, internalProductHistoryEventModel.getProductCode());
    Assertions.assertEquals(STORE_ID, internalProductHistoryEventModel.getStoreId());
    Assertions.assertEquals(SaveHistoryConstants.AUTO_CATEGORY_CHANGE,internalProductHistoryEventModel.getUsername());
  }

  @Test
  public void createInternalProductHistoryEventModelKeywordTest() {
    ProductCollection productCollection2 = new ProductCollection();
    productCollection2.setProductCode(PRODUCT_CODE);
    productCollection2.setStoreId(STORE_ID);
    String oldCategoryName = "OLD_CATEGORY";
    String keyword = "keyword";
    InternalProductHistoryEventModel internalProductHistoryEventModel =
        ConverterUtil.createInternalProductHistoryEventModel(productCollection2, oldCategoryName, keyword);
    Assertions.assertEquals(PRODUCT_CODE, internalProductHistoryEventModel.getProductCode());
    Assertions.assertEquals(STORE_ID, internalProductHistoryEventModel.getStoreId());
    Assertions.assertEquals(SaveHistoryConstants.AUTO_CATEGORY_CHANGE, internalProductHistoryEventModel.getUsername());
  }

  @Test
  public void isMppEnabledNullProfileResponseTest() {
    boolean result = ConverterUtil.isMPPEnabled(null, MPP_ALLOWED_SELLER);
    Assertions.assertFalse(result);
  }

  @Test
  public void isMppEnabledNonCncProfileResponseTest() {
    ProfileResponse profileResponse = new ProfileResponse();
    CompanyDTO companyDTO = new CompanyDTO();
    profileResponse.setCompany(companyDTO);
    boolean result = ConverterUtil.isMPPEnabled(profileResponse, MPP_ALLOWED_SELLER);
    Assertions.assertFalse(result);
  }

  @Test
  public void isMppEnabledCncTrueProfileResponseTest() {
    ProfileResponse profileResponse = new ProfileResponse();
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setCncActivated(true);
    profileResponse.setCompany(companyDTO);
    boolean result = ConverterUtil.isMPPEnabled(profileResponse, MPP_ALLOWED_SELLER);
    assertTrue(result);
  }

  @Test
  public void isMppEnabledCncProfileResponseTest() {
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setMultiDefaultAddressFlag(true);
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setMerchantType("TD");
    profileResponse.setCompany(companyDTO);
    boolean result = ConverterUtil.isMPPEnabled(profileResponse, MPP_ALLOWED_SELLER);
    Assertions.assertFalse(result);
  }

  @Test
  public void isMppEnabledProfileResponseTest() {
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setMultiDefaultAddressFlag(true);
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setMerchantType("CC");
    profileResponse.setCompany(companyDTO);
    boolean result = ConverterUtil.isMPPEnabled(profileResponse, MPP_ALLOWED_SELLER);
    assertTrue(result);
  }

  @Test
  public void isMppEnabledProfileResponseNonMultiAddressTest() {
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setMultiDefaultAddressFlag(false);
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setMerchantType("CC");
    profileResponse.setCompany(companyDTO);
    boolean result = ConverterUtil.isMPPEnabled(profileResponse, MPP_ALLOWED_SELLER);
    Assertions.assertFalse(result);
  }

  @Test
  public void toProductLevel3SummaryResponseTest() {
    itemResponseV2.setPreOrder(PreOrderDTO.builder().isPreOrder(Boolean.TRUE).preOrderDate(new Date()).build());
    ProductLevel3SummaryResponse response1 =
        ConverterUtil.toProductLevel3SummaryResponse(itemResponseV2, new ProductLevel3Inventory());
    assertTrue(Objects.nonNull(response1.getB2BResponse()));
    Assertions.assertTrue(response1.isPreOrder());
    Assertions.assertNotNull(response1.getPreOrderDate());
    itemResponseV2.setB2BResponse(null);
    itemResponseV2.setPreOrder(null);
    ProductLevel3SummaryResponse response2 =
        ConverterUtil.toProductLevel3SummaryResponse(itemResponseV2, new ProductLevel3Inventory());
    assertTrue(Objects.nonNull(response2.getB2BResponse()));
    Assertions.assertNull(response2.getB2BResponse().getBasePrice());
    Assertions.assertFalse(response2.isPreOrder());
    Assertions.assertNull(response2.getPreOrderDate());
  }

  @Test
  public void overrideRestrictedKeywordPresentFlagBasedOnDsResponseTest() {
    productCollection = new ProductCollection();
    ConverterUtil.overrideRestrictedKeywordPresentFlagBasedOnDsResponse(null, null, null);
    ConverterUtil.overrideRestrictedKeywordPresentFlagBasedOnDsResponse(new ImageQcResponseDomainEvent(), null, null);
    ImageQcResponseDomainEvent imageQcResponseDomainEvent = new ImageQcResponseDomainEvent();
    imageQcResponseDomainEvent.setKeywordRestrictionModels(new ArrayList<>());
    ConverterUtil.overrideRestrictedKeywordPresentFlagBasedOnDsResponse(imageQcResponseDomainEvent, null, null);
    imageQcResponseDomainEvent.setKeywordRestrictionModels(
        Collections.singletonList(new KeywordRestrictionModelsResponse()));
    ConverterUtil.overrideRestrictedKeywordPresentFlagBasedOnDsResponse(imageQcResponseDomainEvent,
        new AutoApprovalsDetailDto(), null);
    ConverterUtil.overrideRestrictedKeywordPresentFlagBasedOnDsResponse(imageQcResponseDomainEvent,
        new AutoApprovalsDetailDto(), productCollection);
    KeywordRestrictionModelsResponse keywordRestrictionModelsResponse = new KeywordRestrictionModelsResponse();
    KeywordRecommendationsResponse keywordRecommendationsResponse = new KeywordRecommendationsResponse();
    keywordRecommendationsResponse.setRecommendation(PREDICTION_TYPE);
    keywordRestrictionModelsResponse.setKeywordRecommendations(
        Collections.singletonList(keywordRecommendationsResponse));
    imageQcResponseDomainEvent.setKeywordRestrictionModels(Collections.singletonList(keywordRestrictionModelsResponse));
    ConverterUtil.overrideRestrictedKeywordPresentFlagBasedOnDsResponse(imageQcResponseDomainEvent,
        new AutoApprovalsDetailDto(), null);
    imageQcResponseDomainEvent.getKeywordRestrictionModels().get(0).getKeywordRecommendations().get(0)
        .setRecommendation(INVALID_DETECTION);
    ConverterUtil.overrideRestrictedKeywordPresentFlagBasedOnDsResponse(imageQcResponseDomainEvent,
        new AutoApprovalsDetailDto(), null);
    ConverterUtil.overrideRestrictedKeywordPresentFlagBasedOnDsResponse(imageQcResponseDomainEvent,
        new AutoApprovalsDetailDto(), productCollection);
    imageQcResponseDomainEvent.getKeywordRestrictionModels().get(0).getKeywordRecommendations().get(0)
        .setSkipKeyword(true);
    ConverterUtil.overrideRestrictedKeywordPresentFlagBasedOnDsResponse(imageQcResponseDomainEvent,
        new AutoApprovalsDetailDto(), productCollection);
    Assertions.assertFalse(productCollection.isRestrictedKeywordsPresent());
  }

  @Test
  public void skipAllActionsUnlessCategoryChangeTest() {
    RestrictedKeywordsByFieldAndActionType restrictedKeywordsByFieldAndActionType =
        new RestrictedKeywordsByFieldAndActionType();
    ConverterUtil.skipAllActionsUnlessCategoryChange(false, restrictedKeywordsByFieldAndActionType);
    ConverterUtil.skipAllActionsUnlessCategoryChange(true, restrictedKeywordsByFieldAndActionType);
    restrictedKeywordsByFieldAndActionType.setAction(0);
    ConverterUtil.skipAllActionsUnlessCategoryChange(true, restrictedKeywordsByFieldAndActionType);
    ConverterUtil.skipAllActionsUnlessCategoryChange(false, restrictedKeywordsByFieldAndActionType);
    Assertions.assertEquals(0, restrictedKeywordsByFieldAndActionType.getAction());
  }

  @Test
  public void isSkipAllActionTest() {
    Assertions.assertFalse(ConverterUtil.isSkipAllAction(new RestrictedKeywordsByFieldAndActionType()));
    RestrictedKeywordsByFieldAndActionType restrictedKeywordsByFieldAndActionType =
        new RestrictedKeywordsByFieldAndActionType();
    restrictedKeywordsByFieldAndActionType.setAction(2);
    Assertions.assertFalse(ConverterUtil.isSkipAllAction(restrictedKeywordsByFieldAndActionType));
    restrictedKeywordsByFieldAndActionType.setAction(0);
    assertTrue(ConverterUtil.isSkipAllAction(restrictedKeywordsByFieldAndActionType));
    restrictedKeywordsByFieldAndActionType.setSkipAllActions(true);
    assertTrue(ConverterUtil.isSkipAllAction(restrictedKeywordsByFieldAndActionType));
  }

  @Test
  public void convertDtoToProductCollectionTest() {
    ProductCollectionDTO productCollectionDTO = new ProductCollectionDTO();
    productCollectionDTO.setProductCode(PRODUCT_CODE);
    productCollectionDTO.setBrandApprovalStatus(BrandApprovalStatus.APPROVED);
    productCollectionDTO.setAutoApprovalType(AutoApprovalType.CONTENT_AND_IMAGE);
    ProductCollection productCollection = ConverterUtil.convertDtoToProductCollection(productCollectionDTO);
    assertNotNull(productCollection);
    assertEquals(PRODUCT_CODE, productCollection.getProductCode());
    assertEquals(BrandApprovalStatus.APPROVED, productCollection.getBrandApprovalStatus());
    assertEquals(AutoApprovalType.CONTENT_AND_IMAGE, productCollection.getAutoApprovalType());
  }

  @Test
  public void convertDtoToProductCollectionEmptyTest() {
    ProductCollection productCollection = ConverterUtil.convertDtoToProductCollection(null);
    assertNotNull(productCollection);
  }

  @Test
  public void convertProductCollectionToDtoTest() {
    ProductCollection productCollection = new ProductCollection();
    productCollection.setProductCode(PRODUCT_CODE);
    productCollection.setBrandApprovalStatus(BrandApprovalStatus.APPROVED);
    productCollection.setAutoApprovalType(AutoApprovalType.CONTENT_AND_IMAGE);
    ProductCollectionDTO productCollectionDTO = ConverterUtil.convertProductCollectionToDto(productCollection);
    assertNotNull(productCollectionDTO);
    assertEquals(PRODUCT_CODE, productCollectionDTO.getProductCode());
    assertEquals(BrandApprovalStatus.APPROVED, productCollectionDTO.getBrandApprovalStatus());
    assertEquals(AutoApprovalType.CONTENT_AND_IMAGE, productCollectionDTO.getAutoApprovalType());
  }

  @Test
  public void convertProductCollectionToDtoWithNullProductCollectionTest() {
    ProductCollection productCollection = null;
    ProductCollectionDTO productCollectionDTO = ConverterUtil.convertProductCollectionToDto(productCollection);
    assertNotNull(productCollectionDTO);
  }

  @Test
  public void toBundleRecipeByFieldResponseFromJsonTest() {
    BundleRecipeVo bundleRecipeVo =
      ConverterUtil.toBundleRecipeVoResponseFromJson(bundleRecipeJson).iterator().next();
    Assertions.assertEquals(ITEM_SKU, bundleRecipeVo.getItemSku());
    Assertions.assertEquals(QUANTITY, bundleRecipeVo.getQuantity());
  }

  @Test
  public void toBundleRecipeByFieldResponseFromExceptionJsonTest() {
    Set<BundleRecipeVo> bundleRecipeVoSet = ConverterUtil.toBundleRecipeVoResponseFromJson("json");
    assertTrue(bundleRecipeVoSet.isEmpty());
  }

  @Test
  public void toBundleRecipeByFieldResponseFromEmptyRecipeJsonTest() {
    Set<BundleRecipeVo> bundleRecipeVoSet =
      ConverterUtil.toBundleRecipeVoResponseFromJson(StringUtils.EMPTY);
    assertTrue(bundleRecipeVoSet.isEmpty());
  }

  @Test
  public void getNotificationDetailForProductActivationTest() {
    Assertions.assertEquals(String.format(Constants.BUNDLE_ACTIVE_PRODUCT_URL, PRODUCT_SKU),
        ConverterUtil.getNotificationDetailForProductActivation(PRODUCT_SKU, true));
    Assertions.assertEquals(PRODUCT_SKU,
        ConverterUtil.getNotificationDetailForProductActivation(PRODUCT_SKU, false));
  }

  @Test
  public void getItemIdToItemCodeMapTest() {
    ProductItemResponse productItemResponse = new ProductItemResponse();
    productItemResponse.setId(ID);
    productItemResponse.setSkuCode(SKU_CODE);
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setProductItemResponses(ImmutableSet.of(productItemResponse));

    Map<String, String> itemIdToItemCodeMap = ConverterUtil.getItemIdToItemCodeMap(productDetailResponse);

    Assertions.assertEquals(1, itemIdToItemCodeMap.size());
    Assertions.assertTrue(itemIdToItemCodeMap.keySet().contains(ID));
    Assertions.assertEquals(SKU_CODE, itemIdToItemCodeMap.get(ID));
  }

  @Test
  public void toNewProductItemImageRequestFromL5RequestTest() {
    List<ProductLevel3SummaryDetailsImageRequest> images = new ArrayList<>();
    image.setReviewType("new");
    images.add(image);
    ProductItemImageRequest productItemImageRequest =
        ConverterUtil.toNewProductItemImageRequestFromL5Request(images, ITEM_SKU, false);
    Assertions.assertEquals(ITEM_SKU, productItemImageRequest.getSkuCode());
    assertTrue(productItemImageRequest.getItemImages().get(0).isMainImages());
    assertTrue(productItemImageRequest.getItemImages().get(0).isActive());
    assertTrue(productItemImageRequest.getItemImages().get(0).getOriginalImage());
  }

  @Test
  public void generateBusinessPartnerCounterTest() {
    ProductBusinessPartnerCounter partnerCounter =
      ConverterUtil.generateBusinessPartnerCounter(STORE_ID, BUSINESS_PARTNER_CODE);
    Assertions.assertEquals(0, partnerCounter.getAppealedProductCount());
  }

  @Test
  public void unifyProductAttributeAndProductAttributeValueTypeRequestTest() {
    TreeMap<String, String> attributeMap = new TreeMap<>();
    attributeMap.put(ATTRIBUTE_CODE, ATTRIBUTE_VALUE);

    TreeMap<String, String> valueTypeMap = new TreeMap<>();
    valueTypeMap.put(ATTRIBUTE_CODE, ATTRIBUTE_VALUE_TYPE);

    ProductItemCreationRequest productItemCreationRequest = new ProductItemCreationRequest();
    productItemCreationRequest.setAttributesMap(attributeMap);
    productItemCreationRequest.setAttributesValueTypeMap(valueTypeMap);

    AttributeRequest attributeRequest = new AttributeRequest();
    attributeRequest.setAttributeCode(ATTRIBUTE_CODE);
    ProductAttributeValueRequest productAttributeValueRequest = new ProductAttributeValueRequest();
    productAttributeValueRequest.setAllowedAttributeValue(
        new AllowedAttributeValueRequest(ATTRIBUTE_VALUE, null, null));
    ProductAttributeRequest productAttributeRequest = new ProductAttributeRequest();
    productAttributeRequest.setAttribute(attributeRequest);
    productAttributeRequest.setProductAttributeValues(Arrays.asList(productAttributeValueRequest));

    ProductCreationRequest productCreationRequest = new ProductCreationRequest();
    productCreationRequest.setProductItemRequests(Arrays.asList(productItemCreationRequest));
    productCreationRequest.setProductAttributes(Arrays.asList(productAttributeRequest));

    ConverterUtil.unifyProductAttributeAndProductAttributeValueTypeRequest(productCreationRequest, true, "-");

    Assertions.assertEquals(COMBINED_VALUE_TYPE, attributeMap.get(ATTRIBUTE_CODE));
    Assertions.assertEquals(COMBINED_VALUE_TYPE, productAttributeValueRequest.getAllowedAttributeValue().getValue());
  }


  @Test
  public void unifyProductAttributeAndProductAttributeValueTypeRequestSwitchOffTest() {
    TreeMap<String, String> attributeMap = new TreeMap<>();
    attributeMap.put(ATTRIBUTE_CODE, ATTRIBUTE_VALUE);

    TreeMap<String, String> valueTypeMap = new TreeMap<>();
    valueTypeMap.put(ATTRIBUTE_CODE, ATTRIBUTE_VALUE_TYPE);

    ProductItemCreationRequest productItemCreationRequest = new ProductItemCreationRequest();
    productItemCreationRequest.setAttributesMap(attributeMap);
    productItemCreationRequest.setAttributesValueTypeMap(valueTypeMap);

    AttributeRequest attributeRequest = new AttributeRequest();
    attributeRequest.setAttributeCode(ATTRIBUTE_CODE);
    ProductAttributeValueRequest productAttributeValueRequest = new ProductAttributeValueRequest();
    productAttributeValueRequest.setAllowedAttributeValue(
        new AllowedAttributeValueRequest(ATTRIBUTE_VALUE, null, null));
    ProductAttributeRequest productAttributeRequest = new ProductAttributeRequest();
    productAttributeRequest.setAttribute(attributeRequest);
    productAttributeRequest.setProductAttributeValues(Arrays.asList(productAttributeValueRequest));

    ProductCreationRequest productCreationRequest = new ProductCreationRequest();
    productCreationRequest.setProductItemRequests(Arrays.asList(productItemCreationRequest));
    productCreationRequest.setProductAttributes(Arrays.asList(productAttributeRequest));

    ConverterUtil.unifyProductAttributeAndProductAttributeValueTypeRequest(productCreationRequest, false, "-");

    Assertions.assertEquals(ATTRIBUTE_VALUE, attributeMap.get(ATTRIBUTE_CODE));
    Assertions.assertEquals(ATTRIBUTE_VALUE, productAttributeValueRequest.getAllowedAttributeValue().getValue());
  }


  @Test
  public void unifyProductAttributeAndProductAttributeValueTypeRequestEmptyAllowedAttributeValueTest() {
    TreeMap<String, String> attributeMap = new TreeMap<>();
    attributeMap.put(ATTRIBUTE_CODE, ATTRIBUTE_VALUE);

    TreeMap<String, String> valueTypeMap = new TreeMap<>();
    valueTypeMap.put(ATTRIBUTE_CODE, ATTRIBUTE_VALUE_TYPE);

    ProductItemCreationRequest productItemCreationRequest = new ProductItemCreationRequest();
    productItemCreationRequest.setAttributesMap(attributeMap);
    productItemCreationRequest.setAttributesValueTypeMap(valueTypeMap);

    AttributeRequest attributeRequest = new AttributeRequest();
    attributeRequest.setAttributeCode(ATTRIBUTE_CODE);
    ProductAttributeValueRequest productAttributeValueRequest = new ProductAttributeValueRequest();
    productAttributeValueRequest.setAllowedAttributeValue(
        new AllowedAttributeValueRequest("", null, null));
    ProductAttributeRequest productAttributeRequest = new ProductAttributeRequest();
    productAttributeRequest.setAttribute(attributeRequest);
    productAttributeRequest.setProductAttributeValues(Arrays.asList(productAttributeValueRequest));

    ProductCreationRequest productCreationRequest = new ProductCreationRequest();
    productCreationRequest.setProductItemRequests(Arrays.asList(productItemCreationRequest));
    productCreationRequest.setProductAttributes(Arrays.asList(productAttributeRequest));

    ConverterUtil.unifyProductAttributeAndProductAttributeValueTypeRequest(productCreationRequest, true, "-");

    Assertions.assertEquals(COMBINED_VALUE_TYPE, attributeMap.get(ATTRIBUTE_CODE));
  }

  @Test
  public void unifyProductAttributeAndProductAttributeValueTypeRequestNullAllowedAttributeValueTest() {
    TreeMap<String, String> attributeMap = new TreeMap<>();
    attributeMap.put(ATTRIBUTE_CODE, ATTRIBUTE_VALUE);

    TreeMap<String, String> valueTypeMap = new TreeMap<>();
    valueTypeMap.put(ATTRIBUTE_CODE, ATTRIBUTE_VALUE_TYPE);

    ProductItemCreationRequest productItemCreationRequest = new ProductItemCreationRequest();
    productItemCreationRequest.setAttributesMap(attributeMap);
    productItemCreationRequest.setAttributesValueTypeMap(valueTypeMap);

    AttributeRequest attributeRequest = new AttributeRequest();
    attributeRequest.setAttributeCode(ATTRIBUTE_CODE);
    ProductAttributeValueRequest productAttributeValueRequest = new ProductAttributeValueRequest();
    ProductAttributeRequest productAttributeRequest = new ProductAttributeRequest();
    productAttributeRequest.setAttribute(attributeRequest);
    productAttributeRequest.setProductAttributeValues(Arrays.asList(productAttributeValueRequest));

    ProductCreationRequest productCreationRequest = new ProductCreationRequest();
    productCreationRequest.setProductItemRequests(Arrays.asList(productItemCreationRequest));
    productCreationRequest.setProductAttributes(Arrays.asList(productAttributeRequest));

    ConverterUtil.unifyProductAttributeAndProductAttributeValueTypeRequest(productCreationRequest, true, "-");

    Assertions.assertEquals(COMBINED_VALUE_TYPE, attributeMap.get(ATTRIBUTE_CODE));
  }

  @Test
  public void unifyProductAttributeAndProductAttributeValueTypeRequestEmptyProductAttributeValuesTest() {
    TreeMap<String, String> attributeMap = new TreeMap<>();
    attributeMap.put(ATTRIBUTE_CODE, ATTRIBUTE_VALUE);

    TreeMap<String, String> valueTypeMap = new TreeMap<>();
    valueTypeMap.put(ATTRIBUTE_CODE, ATTRIBUTE_VALUE_TYPE);

    ProductItemCreationRequest productItemCreationRequest = new ProductItemCreationRequest();
    productItemCreationRequest.setAttributesMap(attributeMap);
    productItemCreationRequest.setAttributesValueTypeMap(valueTypeMap);

    AttributeRequest attributeRequest = new AttributeRequest();
    attributeRequest.setAttributeCode(ATTRIBUTE_CODE);
    ProductAttributeRequest productAttributeRequest = new ProductAttributeRequest();
    productAttributeRequest.setAttribute(attributeRequest);

    ProductCreationRequest productCreationRequest = new ProductCreationRequest();
    productCreationRequest.setProductItemRequests(Arrays.asList(productItemCreationRequest));
    productCreationRequest.setProductAttributes(Arrays.asList(productAttributeRequest));

    ConverterUtil.unifyProductAttributeAndProductAttributeValueTypeRequest(productCreationRequest, true, "-");

    Assertions.assertEquals(COMBINED_VALUE_TYPE, attributeMap.get(ATTRIBUTE_CODE));
  }

  @Test
  public void unifyProductAttributeAndProductAttributeValueTypeRequestDifferentAttributeMapTest() {
    TreeMap<String, String> attributeMap = new TreeMap<>();
    attributeMap.put(ATTRIBUTE_CODE1, ATTRIBUTE_VALUE);

    TreeMap<String, String> valueTypeMap = new TreeMap<>();
    valueTypeMap.put(ATTRIBUTE_CODE, ATTRIBUTE_VALUE_TYPE);

    ProductItemCreationRequest productItemCreationRequest = new ProductItemCreationRequest();
    productItemCreationRequest.setAttributesMap(attributeMap);
    productItemCreationRequest.setAttributesValueTypeMap(valueTypeMap);

    AttributeRequest attributeRequest = new AttributeRequest();
    attributeRequest.setAttributeCode(ATTRIBUTE_CODE);
    ProductAttributeValueRequest productAttributeValueRequest = new ProductAttributeValueRequest();
    productAttributeValueRequest.setAllowedAttributeValue(
        new AllowedAttributeValueRequest(ATTRIBUTE_VALUE, null, null));
    ProductAttributeRequest productAttributeRequest = new ProductAttributeRequest();
    productAttributeRequest.setAttribute(attributeRequest);
    productAttributeRequest.setProductAttributeValues(Arrays.asList(productAttributeValueRequest));

    ProductCreationRequest productCreationRequest = new ProductCreationRequest();
    productCreationRequest.setProductItemRequests(Arrays.asList(productItemCreationRequest));
    productCreationRequest.setProductAttributes(Arrays.asList(productAttributeRequest));

    ConverterUtil.unifyProductAttributeAndProductAttributeValueTypeRequest(productCreationRequest, true, "-");

    Assertions.assertEquals(ATTRIBUTE_VALUE, attributeMap.get(ATTRIBUTE_CODE1));
    Assertions.assertEquals(COMBINED_VALUE_TYPE, productAttributeValueRequest.getAllowedAttributeValue().getValue());
  }

  @Test
  public void unifyProductAttributeAndProductAttributeValueTypeRequestValueTypeMapEmotyTest() {
    TreeMap<String, String> attributeMap = new TreeMap<>();
    attributeMap.put(ATTRIBUTE_CODE1, ATTRIBUTE_VALUE);

    ProductItemCreationRequest productItemCreationRequest = new ProductItemCreationRequest();
    productItemCreationRequest.setAttributesMap(attributeMap);

    AttributeRequest attributeRequest = new AttributeRequest();
    attributeRequest.setAttributeCode(ATTRIBUTE_CODE);
    ProductAttributeValueRequest productAttributeValueRequest = new ProductAttributeValueRequest();
    productAttributeValueRequest.setAllowedAttributeValue(
        new AllowedAttributeValueRequest(ATTRIBUTE_VALUE, null, null));
    ProductAttributeRequest productAttributeRequest = new ProductAttributeRequest();
    productAttributeRequest.setAttribute(attributeRequest);
    productAttributeRequest.setProductAttributeValues(Arrays.asList(productAttributeValueRequest));

    ProductCreationRequest productCreationRequest = new ProductCreationRequest();
    productCreationRequest.setProductItemRequests(Arrays.asList(productItemCreationRequest));
    productCreationRequest.setProductAttributes(Arrays.asList(productAttributeRequest));

    ConverterUtil.unifyProductAttributeAndProductAttributeValueTypeRequest(productCreationRequest, true, "-");

    Assertions.assertEquals(ATTRIBUTE_VALUE, attributeMap.get(ATTRIBUTE_CODE1));
    Assertions.assertEquals(ATTRIBUTE_VALUE, productAttributeValueRequest.getAllowedAttributeValue().getValue());
  }

  @Test
  public void getNeedRevisionActivationRequest () {
    ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
    productItemBusinessPartner.setGdnProductItemSku(ITEM_SKU);
    productItemBusinessPartner.setCncDiscoverable(true);
    productItemBusinessPartner.setCncBuyable(true);
    Map<String, Boolean> productItemWholesalePriceMap = new HashMap<>();
    productItemWholesalePriceMap.put(ITEM_SKU + "-" + PICKUP_POINT_CODE, false);
    NeedCorrectionItemActivationRequest request =
        ConverterUtil.getNeedRevisionActivationRequest(productItemBusinessPartner, productItemWholesalePriceMap, true);
    assertTrue(request.isCncDiscoverable());
    assertTrue(request.isCncBuyable());
  }

  @Test
  public void getNeedRevisionActivationRequestCnc1pSwitchOff () {
    ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
    productItemBusinessPartner.setGdnProductItemSku(ITEM_SKU);
    productItemBusinessPartner.setCncActive(true);
    Map<String, Boolean> productItemWholesalePriceMap = new HashMap<>();
    productItemWholesalePriceMap.put(ITEM_SKU + "-" + PICKUP_POINT_CODE, false);
    NeedCorrectionItemActivationRequest request =
        ConverterUtil.getNeedRevisionActivationRequest(productItemBusinessPartner, productItemWholesalePriceMap, false);
    assertTrue(request.isCncActive());
  }

  @Test
  void checkBrandCategoryTwentyOnePlusViolation_WithBrandViolation() {
    ProductImageQcProcessingResponse response = new ProductImageQcProcessingResponse();
    response.setPredictedBrand(BRAND);
    response.setImageViolations(StringUtils.EMPTY);
    response.setTextViolations(StringUtils.EMPTY);

    assertTrue(ConverterUtil.checkBrandCategoryTwentyOnePlusViolation(response));
  }

  @Test
  void checkBrandCategoryTwentyOnePlusViolation_WithCategoryMismatch() {
    ProductImageQcProcessingResponse response = new ProductImageQcProcessingResponse();
    response.setPredictedBrand(StringUtils.EMPTY);
    response.setImageViolations(Constants.CATEGORY_MISMATCH);
    response.setTextViolations(StringUtils.EMPTY);

    assertTrue(ConverterUtil.checkBrandCategoryTwentyOnePlusViolation(response));
  }

  @Test
  void checkBrandCategoryTwentyOnePlusViolation_WithTwentyOneProduct() {
    ProductImageQcProcessingResponse response = new ProductImageQcProcessingResponse();
    response.setPredictedBrand(StringUtils.EMPTY);
    response.setImageViolations(StringUtils.EMPTY);
    response.setTextViolations(Constants.TWENTY_ONE_PRODUCT);

    assertTrue(ConverterUtil.checkBrandCategoryTwentyOnePlusViolation(response));
  }

  @Test
  void checkBrandCategoryTwentyOnePlusViolation_WithNoViolations() {
    ProductImageQcProcessingResponse response = new ProductImageQcProcessingResponse();
    response.setPredictedBrand(StringUtils.EMPTY);
    response.setImageViolations(StringUtils.EMPTY);
    response.setTextViolations(StringUtils.EMPTY);

    assertFalse(ConverterUtil.checkBrandCategoryTwentyOnePlusViolation(response));
  }


  @Test
  void checkBrandCategoryTwentyOnePlusViolation_WithMultipleViolations() {
    ProductImageQcProcessingResponse response = new ProductImageQcProcessingResponse();
    response.setPredictedBrand(BRAND);
    response.setImageViolations(Constants.CATEGORY_MISMATCH);
    response.setTextViolations(Constants.TWENTY_ONE_PRODUCT);

    assertTrue(ConverterUtil.checkBrandCategoryTwentyOnePlusViolation(response));
  }
}

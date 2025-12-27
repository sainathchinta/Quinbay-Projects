package com.gdn.mta.product.util;

import java.time.Duration;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.EnumSet;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.TreeMap;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.AttributeCodeValueValueTypeDetails;
import com.gda.mta.product.dto.AuditTrailDto;
import com.gda.mta.product.dto.AutoNeedRevisionAndForceReviewResponse;
import com.gda.mta.product.dto.B2BFields;
import com.gda.mta.product.dto.BuyableScheduleRequest;
import com.gda.mta.product.dto.DimensionAndUomRequest;
import com.gda.mta.product.dto.DiscoverableScheduleRequest;
import com.gda.mta.product.dto.DistributionItemRequest;
import com.gda.mta.product.dto.EditFlagChangesDTO;
import com.gda.mta.product.dto.EditProductResponse;
import com.gda.mta.product.dto.FbbAndCncDataChangeDto;
import com.gda.mta.product.dto.ItemPickupPointRequest;
import com.gda.mta.product.dto.MasterProductEditDTO;
import com.gda.mta.product.dto.PickupPointCreateRequest;
import com.gda.mta.product.dto.PreOrderRequest;
import com.gda.mta.product.dto.ProductAndL5MigrationRequest;
import com.gda.mta.product.dto.ProductItemCreationRequest;
import com.gda.mta.product.dto.ProductItemDistributionInfoRequest;
import com.gda.mta.product.dto.ProductL3UpdateRequest;
import com.gda.mta.product.dto.ProductDetailEditDTO;
import com.gda.mta.product.dto.ProductLevel3PriceRequest;
import com.gda.mta.product.dto.ProductLevel3QuickEditRequest;
import com.gda.mta.product.dto.ProductLevel3SummaryDetailsImageRequest;
import com.gda.mta.product.dto.ProductMasterDataEditRequest;
import com.gda.mta.product.dto.ProductVariantPriceStockAndImagesRequest;
import com.gda.mta.product.dto.ProductVariantUpdateRequest;
import com.gda.mta.product.dto.QuickEditRequest;
import com.gda.mta.product.dto.QuickEditV2Request;
import com.gda.mta.product.dto.RestrictedKeywordsByField;
import com.gda.mta.product.dto.RestrictedKeywordsByFieldAndActionType;
import com.gda.mta.product.dto.response.AuditTrailListRequest;
import com.gda.mta.product.dto.response.SimpleStringResponse;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.micro.graphics.web.model.ImageRequest;
import com.gdn.mta.product.commons.constant.UpdateProductActivity;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.entity.ProductItemBusinessPartner;
import com.gdn.mta.product.entity.ProductItemLevel3;
import com.gdn.mta.product.entity.ProductItemWholesalePrice;
import com.gdn.mta.product.entity.ProductLevel3;
import com.gdn.mta.product.entity.ProductLevel3Attribute;
import com.gdn.mta.product.entity.ProductLevel3Summary;
import com.gdn.mta.product.entity.UpdatedProductHistory;
import com.gdn.mta.product.entity.WorkflowStates;
import com.gdn.mta.product.enums.ApiErrorCode;
import com.gdn.mta.product.enums.AutoApprovalType;
import com.gdn.mta.product.enums.L3InfoUpdateChangeType;
import com.gdn.mta.product.enums.ProductLevel3Status;
import com.gdn.mta.product.enums.RestrictedKeywordActionType;
import com.gdn.mta.product.service.exception.ApiDataNotFoundException;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.commons.constants.EditedReviewTypeConstants;
import com.gdn.partners.pbp.model.productlevel3.ProductLevel3Inventory;
import com.gdn.partners.product.pricing.web.model.dto.WholeSalePriceSkuStatusDto;
import com.gdn.partners.product.pricing.web.model.response.WholesalePriceSkuResponse;
import com.gdn.x.businesspartner.commons.enums.MerchantType;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.PickupPointResponse;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.businesspartner.v2.dto.pickuppoint.PickupPointFilterRequest;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.InventoryBaseRequest;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.InventoryDetailInfoRequestDTO;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.model.entity.ItemBuyableSchedule;
import com.gdn.x.product.model.entity.ItemDiscoverableSchedule;
import com.gdn.x.product.model.entity.ItemViewConfig;
import com.gdn.x.product.rest.web.model.dto.DiscountPriceDTO;
import com.gdn.x.product.rest.web.model.dto.ItemBuyableScheduleDTO;
import com.gdn.x.product.rest.web.model.dto.ItemDiscoverableScheduleDTO;
import com.gdn.x.product.rest.web.model.dto.ItemViewConfigDTO;
import com.gdn.x.product.rest.web.model.dto.PriceDTO;
import com.gdn.x.product.rest.web.model.dto.ProductAttributeDTO;
import com.gdn.x.product.rest.web.model.dto.ProductAttributeDetailDTO;
import com.gdn.x.product.rest.web.model.request.AddDeleteVariantRequest;
import com.gdn.x.product.rest.web.model.request.AddVariantRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointListingRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointQuickEditRequest;
import com.gdn.x.product.rest.web.model.response.B2BResponse;
import com.gdn.x.product.rest.web.model.response.BuyableScheduleResponse;
import com.gdn.x.product.rest.web.model.response.DiscoverableScheduleResponse;
import com.gdn.x.product.rest.web.model.response.ItemLevel5Response;
import com.gdn.x.product.rest.web.model.response.ItemPickupPointBasicResponse;
import com.gdn.x.product.rest.web.model.response.ItemPickupPointListingResponse;
import com.gdn.x.product.rest.web.model.response.ItemSummaryListResponse;
import com.gdn.x.product.rest.web.model.response.ViewConfigResponse;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandResponse;

import com.gdn.x.productcategorybase.dto.response.ProductL1AndL2CodeResponse;
import com.gdn.x.productcategorybase.dto.response.RestrictedKeywordsMappedToCategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ValidOmniChannelSkuResponse;
import com.gdn.x.productcategorybase.entity.Product;
import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.gda.mta.product.dto.ProductCreationRequest;
import com.gda.mta.product.dto.ProductItemWholesalePriceRequest;
import com.gda.mta.product.dto.response.ProductL3SummaryResponse;
import com.gdn.mta.product.entity.ProductBusinessPartner;
import com.gdn.mta.product.enums.ProductCreationType;
import com.gdn.mta.product.service.exception.ApiIncorrectInputDataException;
import com.gdn.x.product.rest.web.model.dto.CategoryDTO;
import com.gdn.x.product.rest.web.model.dto.ItemCatalogDTO;
import com.gdn.x.product.rest.web.model.dto.ItemCategoryDTO;
import com.gdn.x.product.rest.web.model.dto.MasterCatalogDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductDTO;
import com.gdn.x.product.rest.web.model.response.ProductL3Response;
import com.gdn.x.product.rest.web.model.response.ProductSkuSummaryResponse;
import com.gdn.x.productcategorybase.dto.AttributeType;
import com.gdn.x.productcategorybase.dto.request.AllowedAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.AttributeRequest;
import com.gdn.x.productcategorybase.dto.request.PredefinedAllowedAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.MinWholesaleDiscountResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemImageRequest;
import com.gdn.x.productcategorybase.dto.response.WholesaleConfigResponse;
import com.gdn.x.productcategorybase.dto.response.WholesaleMappingResponse;
import com.google.common.collect.ImmutableMap;

import org.springframework.util.CollectionUtils;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class CommonUtilsTest {

  private static final int MAX_WHOLESALE_REQUESTS = 5;
  private static final Integer MINIMUM_PRICE = 1000;
  private static final Double PRICE = 5000.0;
  private static final String ITEM_SKU = "itemSku";
  private static final String ITEM_SKU_3 = "BR6-0009-0009-0001";
  private static final String ITEM_SKU_2 = "itemSku2";
  private static final String PICKUP_POINT_CODE = "pickupPointCode";
  private static final String PERCENTAGE = "PERCENTAGE";
  private static final String PRICE_PERCENTAGE = "PRICE_PERCENTAGE";
  private static final int QUANTITY = 3;
  private static final double DISCOUNT = 10.0;
  private static final int QUANTITY_1 = 5;
  private static final double DISCOUNT_1 = 12.0;
  private static final String PCU_EXTERNAL_APP = "pcu-external-app";
  private static final String MTA_APP = "MTAApp";
  private static final String MTA_API = "mta-api";
  private static final String FLOW1 = "FLOW1";
  private static final String FLOW2 = "FLOW2";
  private static final String BUSINESS_PARTNER_CODE = "BUSINESS_PARTNER_CODE";
  private static final String ATTRIBUTE_VALUE = "attribute_value";
  private static final String ATTRIBUTE_VALUE_1 = "attribute_value1";
  private static final String ATTRIBUTE_VALUE_2 = "attribute_value2";
  private static final String ITEM_NAME = "item-name";
  private static final String L5_CODE = "itemSku-pickupPointCode";
  private static final String SPECIFICATION_DETAIL =
          "<ul><li>attribute_value<ul><li>attribute_value</li></ul></li><li>attribute_value1<ul>" +
                  "<li>attribute_value1</li></ul></li><li>attribute_value2<ul><li>attribute_value2</li></ul></li><li>attribute_value<ul></ul></li></ul>";
  private static final String PRODUCT_NAME = "productName";
  private static final String BRAND = "brand";
  private static final String CATEGORY_CODE = "categoryCode";
  private static final String CATEGORY_NAME = "categoryName";
  private static final String CATEGORY_NAME_1 = "categoryName1";
  private static final String PRODUCT_IMAGE = "productImage";
  private static final String PRODUCT_SKU = "PRODUCT_SKU";
  private static final String PRODUCT_CODE = "productCode";
  private static final String ERROR_MESSAGE = "ERROR_MESSAGE";
  private static final String GDN_SKU = "GDN_SKU";
  private static final String PICKUPPOINT_CODE = "pickupPointCode";
  private static final String BRAND_CODE = "brandCode";
  private static final String BRAND_NAME = "brandName";
  private static final String INTERNAL = "INTERNAL";
  private static final String MERCHANT_CODE = "merchant-code";
  private static final String MPP_ALLOWED_SELLERS = "CM";
  private static final String ACTIVITY = "activity";
  private static final String ATTRIBUTE = "attribute";
  private static final String CHANNEL_ID = "channelId";
  private static final String REQUEST_ID = "requestId";
  private static final String CLIENT_ID = "clientId";
  private static final String CHANGED_BY = "changedBy";
  private static final String WAREHOUSE_SELLER = "BL";
  private static final String NON_WAREHOUSE_SELLER = "AL";
  private static final String ATTRIBUTE_VALUE3 = "-";
  private static final String ATTRIBUTE_CODE = "AttributeCode";
  private static final String ATTRIBUTE_VALUE_WARNA_1 = "green";
  private static final String ATTRIBUTE_VALUE_WARNA_2 = "red";
  private static final String ATTRIBUTE_VALUE_VARIASI = "64GB";
  private static final String PRODUCT_NAME_2 = "iphone 15";
  private static final String PRODUCT_NAME_3 = "MacBook 15";
  private static final String ATTRIBUTE_CODE_2 = "AttributeCode2";
  private static final String FAMILY_COLOR_CODE = "FAMILY_COLOR_CODE";
  private static final String DEFAULT_PICKUP_POINT_CODE = "PP-0000001";
  private static final String PICKUP_POINT_CODE_1 = "PP-0000002";
  private static final String PREDICTION_NAME_1 = "nsfw";
  private static final String PREDICTION_NAME_2 = "blur";
  private static final String FAAS_ACTIVATED = "faasActivated";
  public static final String NEW_URL = "new-url";
  public static final String OLD_URL = "old-url";
  private static final String FIELD_IDENTIFIER_1 = "fieldIdentifier1";
  private static final String CATEGORY_RESTRICTED_KEYWORD_ID = "categoryRestrictedKeywordId";
  private static final String FIELD_IDENTIFIER_2 = "fieldIdentifier2";
  private final String DEFAULT_CATEGORY_CODE = "CA-1234";


  private ProductItemWholesalePriceRequest productItemWholesalePriceRequest = new ProductItemWholesalePriceRequest();
  private ProductItemWholesalePriceRequest productItemWholesalePriceRequest1 = new ProductItemWholesalePriceRequest();
  private List<ProductItemWholesalePriceRequest> productItemWholesalePriceRequestList = new ArrayList<>();
  private WholesaleMappingResponse wholesalePercentageMappingResponse;
  private WholesaleMappingResponse wholesalePricePercentageMappingResponse;
  private ProductCreationRequest productCreationRequest;
  private List<ProductItemCreationRequest> productItemCreationRequests;
  private List<PickupPointResponse> pickupPointResponses;
  private List<PickupPointCreateRequest> pickupPoints;
  private ProductRequest productRequest = new ProductRequest();
  private ProductAttributeRequest productAttributeRequest = new ProductAttributeRequest();
  private ProductAttributeRequest productAttributeRequest1 = new ProductAttributeRequest();
  private ProductAttributeRequest productAttributeRequest2 = new ProductAttributeRequest();
  private ProductAttributeRequest productAttributeRequest3 = new ProductAttributeRequest();
  private ProductAttributeRequest productAttributeRequest4 = new ProductAttributeRequest();
  private AttributeRequest attributeRequest = new AttributeRequest();
  private AttributeRequest attributeRequest1 = new AttributeRequest();
  private AttributeRequest attributeRequest2 = new AttributeRequest();
  private AttributeRequest attributeRequest3 = new AttributeRequest();
  private AttributeRequest attributeRequest4 = new AttributeRequest();
  private ProductAttributeValueRequest productAttributeValueRequest = new ProductAttributeValueRequest();
  private ProductAttributeValueRequest productAttributeValueRequest1 = new ProductAttributeValueRequest();
  private ProductAttributeValueRequest productAttributeValueRequest2 = new ProductAttributeValueRequest();
  private ProductAttributeValueRequest productAttributeValueRequest3 = new ProductAttributeValueRequest();
  private ProductAttributeValueRequest productAttributeValueRequest4 = new ProductAttributeValueRequest();
  private ProductSkuSummaryResponse productSkuSummaryResponse;
  private CategoryResponse categoryResponse = new CategoryResponse();
  private CategoryResponse categoryResponse1 = new CategoryResponse();
  private ItemSummaryListResponse itemSummaryListResponse = new ItemSummaryListResponse();
  private ProductLevel3Inventory productLevel3Inventory = new ProductLevel3Inventory();
  private PriceDTO priceDTO = new PriceDTO();
  private DiscountPriceDTO discountPriceDTO = new DiscountPriceDTO();
  private ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
  private ItemLevel5Response itemLevel5Response = new ItemLevel5Response();
  private WholesalePriceSkuResponse wholesalePriceSkuResponse = new WholesalePriceSkuResponse();

  private ProfileResponse businessPartner;
  private ProductL3UpdateRequest productL3UpdateRequest;
  private ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest;
  private CategoryDetailResponse categoryDetailResponse;
  private AttributeCodeValueValueTypeDetails existingDefiningAttributeDetails;
  private ProfileResponse profileResponse;
  private ProductVariantUpdateRequest productVariantUpdateRequest;
  Map<String, ItemPickupPointListingResponse> itemPickupPointListingResponseMap;
  private MasterProductEditDTO masterProductEditDTO;
  private RestrictedKeywordsByFieldAndActionType restrictedKeywordsByFieldAndActionType;
  private ProductMasterDataEditRequest productMasterDataEditRequest;
  private ConfigurationStatusResponse configurationStatusResponse;
  private ProductCollection productCollectionData;

  @BeforeEach
  public void init() throws Exception {
    productItemWholesalePriceRequest.setQuantity(QUANTITY);
    productItemWholesalePriceRequest.setWholesaleDiscount(DISCOUNT);
    productItemWholesalePriceRequest1.setQuantity(QUANTITY_1);
    productItemWholesalePriceRequest1.setWholesaleDiscount(DISCOUNT_1);

    productItemWholesalePriceRequestList.add(productItemWholesalePriceRequest);
    productItemWholesalePriceRequestList.add(productItemWholesalePriceRequest1);
    wholesalePercentageMappingResponse = new WholesaleMappingResponse();
    wholesalePercentageMappingResponse.setConfigurationType(PERCENTAGE);
    wholesalePercentageMappingResponse.setWholesaleConfig(new ArrayList<>());
    WholesaleConfigResponse wholesaleConfigResponse = new WholesaleConfigResponse();
    wholesaleConfigResponse.setQuantity(QUANTITY);
    wholesaleConfigResponse.setMinWholesaleDiscount(new ArrayList<>());
    wholesaleConfigResponse.getMinWholesaleDiscount().add(new MinWholesaleDiscountResponse(0.0, 6.0));
    WholesaleConfigResponse wholesaleConfigResponse1 = new WholesaleConfigResponse();
    wholesaleConfigResponse1.setQuantity(QUANTITY_1);
    wholesaleConfigResponse1.setMinWholesaleDiscount(new ArrayList<>());
    wholesaleConfigResponse1.getMinWholesaleDiscount().add(new MinWholesaleDiscountResponse(0.0, 7.0));
    wholesalePercentageMappingResponse.setWholesaleConfig(Arrays.asList(wholesaleConfigResponse, wholesaleConfigResponse1));
    wholesalePricePercentageMappingResponse = new WholesaleMappingResponse();
    wholesalePricePercentageMappingResponse.setConfigurationType(PRICE_PERCENTAGE);
    wholesalePricePercentageMappingResponse.setWholesaleConfig(new ArrayList<>());
    WholesaleConfigResponse wholesaleConfigResponse2 = new WholesaleConfigResponse();
    wholesaleConfigResponse2.setQuantity(QUANTITY);
    wholesaleConfigResponse2.setMinWholesaleDiscount(new ArrayList<>());
    wholesaleConfigResponse2.getMinWholesaleDiscount().add(new MinWholesaleDiscountResponse(10000.0, 6.0));
    wholesaleConfigResponse2.getMinWholesaleDiscount().add(new MinWholesaleDiscountResponse(20000.0, 8.0));
    wholesaleConfigResponse2.getMinWholesaleDiscount().add(new MinWholesaleDiscountResponse(30000.0, 10.0));
    wholesaleConfigResponse2.getMinWholesaleDiscount().add(new MinWholesaleDiscountResponse(40000.0, 12.0));
    WholesaleConfigResponse wholesaleConfigResponse3 = new WholesaleConfigResponse();
    wholesaleConfigResponse3.setQuantity(QUANTITY_1);
    wholesaleConfigResponse3.setMinWholesaleDiscount(new ArrayList<>());
    wholesaleConfigResponse3.getMinWholesaleDiscount().add(new MinWholesaleDiscountResponse(10000.0, 7.0));
    wholesaleConfigResponse3.getMinWholesaleDiscount().add(new MinWholesaleDiscountResponse(20000.0, 8.0));
    wholesaleConfigResponse3.getMinWholesaleDiscount().add(new MinWholesaleDiscountResponse(30000.0, 10.0));
    wholesaleConfigResponse3.getMinWholesaleDiscount().add(new MinWholesaleDiscountResponse(40000.0, 12.0));
    wholesalePricePercentageMappingResponse.setWholesaleConfig(Arrays.asList(wholesaleConfigResponse2, wholesaleConfigResponse3));

    PickupPointResponse pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(DEFAULT_PICKUP_POINT_CODE);
    pickupPointResponse.setFbbActivated(true);

    pickupPointResponses = new ArrayList<>();
    pickupPointResponses.add(pickupPointResponse);

    PickupPointCreateRequest pickupPointCreateRequest = new PickupPointCreateRequest();
    pickupPointCreateRequest.setPickupPointId(DEFAULT_PICKUP_POINT_CODE);

    pickupPoints = new ArrayList<>();
    pickupPoints.add(pickupPointCreateRequest);

    ProductItemCreationRequest productItemCreationRequest = new ProductItemCreationRequest();
    productItemCreationRequest.setPickupPoints(pickupPoints);

    productItemCreationRequests = new ArrayList<>();
    productItemCreationRequests.add(productItemCreationRequest);

    productCreationRequest = new ProductCreationRequest();
    productCreationRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productCreationRequest.setProductItemRequests(productItemCreationRequests);

    attributeRequest.setName(ATTRIBUTE_VALUE);
    attributeRequest.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    productAttributeValueRequest.setDescriptiveAttributeValue(ATTRIBUTE_VALUE);
    productAttributeRequest.setAttribute(attributeRequest);
    productAttributeRequest.setProductAttributeValues(Arrays.asList(productAttributeValueRequest));

    attributeRequest1.setName(ATTRIBUTE_VALUE_1);
    attributeRequest1.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    productAttributeValueRequest1.setPredefinedAllowedAttributeValue(new PredefinedAllowedAttributeValueRequest());
    productAttributeValueRequest1.getPredefinedAllowedAttributeValue().setValue(ATTRIBUTE_VALUE_1);
    productAttributeRequest1.setAttribute(attributeRequest1);
    productAttributeRequest1.setProductAttributeValues(Arrays.asList(productAttributeValueRequest1));

    attributeRequest2.setName(ATTRIBUTE_VALUE_2);
    attributeRequest2.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
    productAttributeValueRequest2.setAllowedAttributeValue(new AllowedAttributeValueRequest());
    productAttributeValueRequest2.getAllowedAttributeValue().setValue(ATTRIBUTE_VALUE_2);
    productAttributeRequest2.setAttribute(attributeRequest2);
    productAttributeRequest2.setProductAttributeValues(Arrays.asList(productAttributeValueRequest2));

    attributeRequest3.setName(ATTRIBUTE_VALUE);
    attributeRequest3.setSkuValue(true);
    productAttributeValueRequest3.setAllowedAttributeValue(new AllowedAttributeValueRequest());
    productAttributeValueRequest3.getAllowedAttributeValue().setValue(ATTRIBUTE_VALUE_2);
    productAttributeRequest3.setAttribute(attributeRequest3);
    productAttributeRequest3.setProductAttributeValues(Arrays.asList(productAttributeValueRequest3));

    attributeRequest4.setName(ATTRIBUTE_VALUE);
    productAttributeValueRequest4.setAllowedAttributeValue(new AllowedAttributeValueRequest());
    productAttributeValueRequest4.getAllowedAttributeValue().setValue(ATTRIBUTE_VALUE_2);
    productAttributeRequest4.setAttribute(attributeRequest4);
    productAttributeRequest4.setProductAttributeValues(Arrays.asList(productAttributeValueRequest4));

    productRequest.setProductAttributes(Arrays.asList(productAttributeRequest, productAttributeRequest1,
            productAttributeRequest2, productAttributeRequest3, productAttributeRequest4));

    productSkuSummaryResponse =
        ProductSkuSummaryResponse.builder().productName(PRODUCT_NAME).brand(BRAND)
            .categoryCode(CATEGORY_CODE).productImage(PRODUCT_IMAGE).productSku(PRODUCT_SKU).build();
    categoryResponse.setName(CATEGORY_NAME);
    categoryResponse1.setName(CATEGORY_NAME_1);
    itemSummaryListResponse.setItemSku(ITEM_SKU);
    itemSummaryListResponse.setProductName(PRODUCT_NAME);
    itemSummaryListResponse.setProductType(ProductType.BIG_PRODUCT);
    itemSummaryListResponse.setPrice(Collections.emptySet());
    itemSummaryListResponse.setItemViewConfigs(Collections.emptySet());
    priceDTO.setListOfDiscountPrices(Collections.singletonList(discountPriceDTO));
    businessPartner =
      ProfileResponse.builder().businessPartnerCode(BUSINESS_PARTNER_CODE).trustedSeller(false)
        .build();
    productL3UpdateRequest = new ProductL3UpdateRequest();
    productVariantPriceStockAndImagesRequest = new ProductVariantPriceStockAndImagesRequest();
    productL3UpdateRequest.setProductItems(
        new ArrayList<>(Collections.singleton(productVariantPriceStockAndImagesRequest)));
    categoryDetailResponse = new CategoryDetailResponse();
    CategoryAttributeResponse categoryAttributeResponse = new CategoryAttributeResponse();
    CategoryAttributeResponse categoryAttributeResponse1 = new CategoryAttributeResponse();
    List<CategoryAttributeResponse> categoryAttributeResponses = new ArrayList<>();
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setAttributeCode(ATTRIBUTE_CODE);
    AttributeResponse attributeResponse1 = new AttributeResponse();
    attributeResponse1.setAttributeCode(ATTRIBUTE_CODE_2);
    categoryAttributeResponse.setAttribute(attributeResponse);
    categoryAttributeResponse1.setAttribute(attributeResponse1);
    categoryAttributeResponses.add(categoryAttributeResponse1);
    categoryAttributeResponses.add(categoryAttributeResponse);
    categoryDetailResponse.setCategoryAttributes(categoryAttributeResponses);

    existingDefiningAttributeDetails = new AttributeCodeValueValueTypeDetails();
    profileResponse = new ProfileResponse();
    productVariantUpdateRequest = new ProductVariantUpdateRequest();
    itemPickupPointListingResponseMap = new HashMap<>();
    restrictedKeywordsByFieldAndActionType = new RestrictedKeywordsByFieldAndActionType();
    masterProductEditDTO = new MasterProductEditDTO();
    productMasterDataEditRequest = new ProductMasterDataEditRequest();
    configurationStatusResponse =
      ConfigurationStatusResponse.builder().categoryCode(CATEGORY_CODE).merchantCode(BUSINESS_PARTNER_CODE).build();
    productCollectionData = new ProductCollection();
    productCollectionData.setProductCode(PRODUCT_CODE);
    productCollectionData.setActivated(true);
    productCollectionData.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    masterProductEditDTO.setProductCollection(productCollectionData);
    masterProductEditDTO.setReviewTypeList(new ArrayList<>());
  }

  @Test
  public void validateWholesalePriceTest() {
    CommonUtils
        .validateWholesalePrice(productItemWholesalePriceRequestList, PRICE, MINIMUM_PRICE, MAX_WHOLESALE_REQUESTS,
            ITEM_SKU);
  }

  @Test
  public void validateWholesalePriceTest_duplicateQuantityTest() {
    productItemWholesalePriceRequestList.get(1).setQuantity(QUANTITY);
    Assertions.assertThrows(ApiIncorrectInputDataException.class, () -> {
      CommonUtils
          .validateWholesalePrice(productItemWholesalePriceRequestList, PRICE, MINIMUM_PRICE, MAX_WHOLESALE_REQUESTS,
              ITEM_SKU);
    });
  }

  @Test
  public void validateWholesalePriceTest_duplicateDiscountTest() {
    productItemWholesalePriceRequestList.get(1).setWholesaleDiscount(6.0);
    Assertions.assertThrows(ApiIncorrectInputDataException.class, () -> {
      CommonUtils
          .validateWholesalePrice(productItemWholesalePriceRequestList, PRICE, MINIMUM_PRICE, MAX_WHOLESALE_REQUESTS,
              ITEM_SKU);
    });
  }

  @Test
  public void validateWholesalePriceTest_maxWholesaleRequestsTest() {
    int maxWholesaleRequests = 1;
    Assertions.assertThrows(ApiIncorrectInputDataException.class, () -> {
      CommonUtils
          .validateWholesalePrice(productItemWholesalePriceRequestList, PRICE, MINIMUM_PRICE, maxWholesaleRequests,
              ITEM_SKU);
    });
  }

  @Test
  public void validateWholesalePriceTest_duplicateDiscount_Test() {
    ProductItemWholesalePriceRequest wholesalePriceRequest = new ProductItemWholesalePriceRequest();
    wholesalePriceRequest.setQuantity(50);
    wholesalePriceRequest.setWholesaleDiscount(DISCOUNT);
    productItemWholesalePriceRequestList.add(wholesalePriceRequest);
    Assertions.assertThrows(ApiIncorrectInputDataException.class, () -> {
      CommonUtils
          .validateWholesalePrice(productItemWholesalePriceRequestList, PRICE, MINIMUM_PRICE, MAX_WHOLESALE_REQUESTS,
              ITEM_SKU);
    });
  }

  @Test
  public void validateWholesalePriceTest_maxWholesaleRequestsTest_emptyItemSku() {
    int maxWholesaleRequests = 1;
    Assertions.assertThrows(ApiIncorrectInputDataException.class, () -> {
      CommonUtils
          .validateWholesalePrice(productItemWholesalePriceRequestList, PRICE, MINIMUM_PRICE, maxWholesaleRequests,
              StringUtils.EMPTY);
    });
  }

  @Test
  public void validateWholesalePriceTest_emptyWholeRequestList() {
    int maxWholesaleRequests = 1;
    CommonUtils
        .validateWholesalePrice(new ArrayList<>(), PRICE, MINIMUM_PRICE, maxWholesaleRequests,
            StringUtils.EMPTY);
  }

  @Test
  public void validateWholesalePriceTest_minAllowedQuantityTest() {
    productItemWholesalePriceRequestList.get(0).setQuantity(1);
    Assertions.assertThrows(ApiIncorrectInputDataException.class, () -> {
      CommonUtils
          .validateWholesalePrice(productItemWholesalePriceRequestList, PRICE, MINIMUM_PRICE, MAX_WHOLESALE_REQUESTS,
              ITEM_SKU);
    });
  }

  @Test
  public void validateWholesalePriceTest_wholesaleDiscountLTEZero() {
    productItemWholesalePriceRequestList.get(0).setWholesaleDiscount(-0.1);
    Assertions.assertThrows(ApiIncorrectInputDataException.class, () -> {
      CommonUtils
          .validateWholesalePrice(productItemWholesalePriceRequestList, PRICE, MINIMUM_PRICE, MAX_WHOLESALE_REQUESTS,
              ITEM_SKU);
    });
  }

  @Test
  public void validateWholesalePriceTest_wholesaleTierViolationTest() {
    productItemWholesalePriceRequestList.get(1).setWholesaleDiscount(1.0);
    Assertions.assertThrows(ApiIncorrectInputDataException.class, () -> {
      CommonUtils
          .validateWholesalePrice(productItemWholesalePriceRequestList, PRICE, MINIMUM_PRICE, MAX_WHOLESALE_REQUESTS,
              ITEM_SKU);
    });
  }

  @Test
  public void validateWholesalePriceTest_minPriceViolationTest() {
    Double price = 1050.0;
    Assertions.assertThrows(ApiIncorrectInputDataException.class, () -> {
      CommonUtils
          .validateWholesalePrice(productItemWholesalePriceRequestList, price, MINIMUM_PRICE, MAX_WHOLESALE_REQUESTS,
              ITEM_SKU);
    });
  }
  @Test
  public void validateWholesalePriceTestWithConfig() {
    productItemWholesalePriceRequestList.get(0).setQuantity(4);
    productItemWholesalePriceRequestList.get(0).setWholesaleDiscount(4);
    Double price = 1050.0;
    Assertions.assertThrows(ApiIncorrectInputDataException.class, () -> {
      CommonUtils
          .validateWholesalePriceConfig(productItemWholesalePriceRequestList, price, wholesalePercentageMappingResponse);
    });
  }

  @Test
  public void validateWholesalePriceTestWithConfigWithLeastQuantity() {
    productItemWholesalePriceRequestList.get(0).setQuantity(2);
    productItemWholesalePriceRequestList.get(0).setWholesaleDiscount(4);
    Double price = 1050.0;
    CommonUtils
        .validateWholesalePriceConfig(productItemWholesalePriceRequestList, price, wholesalePercentageMappingResponse);
  }

  @Test
  public void validateWholesalePriceTestWithConfigWithMaxQuantity() {
    productItemWholesalePriceRequestList.get(0).setQuantity(8);
    productItemWholesalePriceRequestList.get(0).setWholesaleDiscount(6);
    Double price = 1050.0;
    Assertions.assertThrows(ApiIncorrectInputDataException.class, () -> {
      CommonUtils
          .validateWholesalePriceConfig(productItemWholesalePriceRequestList, price, wholesalePercentageMappingResponse);
    });
  }

  @Test
  public void validateWholesalePriceTestWithConfigWithMaxQuantitySuccessTest() {
    productItemWholesalePriceRequestList.get(0).setQuantity(8);
    productItemWholesalePriceRequestList.get(0).setWholesaleDiscount(7);
    Double price = 1050.0;
    CommonUtils
        .validateWholesalePriceConfig(productItemWholesalePriceRequestList, price, wholesalePercentageMappingResponse);
  }

  @Test
  public void validateWholesalePriceTestWithConfigWithMaxPrice() {
    productItemWholesalePriceRequestList.get(0).setQuantity(3);
    productItemWholesalePriceRequestList.get(0).setWholesaleDiscount(11);
    Double price = 80000.0;
    Assertions.assertThrows(ApiIncorrectInputDataException.class, () -> {
      CommonUtils
          .validateWholesalePriceConfig(productItemWholesalePriceRequestList, price, wholesalePricePercentageMappingResponse);
    });
  }

  @Test
  public void validateWholesalePriceTestWithPricePercentageConfigWithLessPrice() {
    productItemWholesalePriceRequestList.get(0).setQuantity(4);
    productItemWholesalePriceRequestList.get(0).setWholesaleDiscount(6);
    Double price = 5000.0;
    CommonUtils
        .validateWholesalePriceConfig(productItemWholesalePriceRequestList, price, wholesalePricePercentageMappingResponse);
  }

  @Test
  public void validateWholesalePriceTestWithPricePercentageConfigWithMaxQuantitySuccessTest() {
    productItemWholesalePriceRequestList.get(0).setQuantity(8);
    productItemWholesalePriceRequestList.get(0).setWholesaleDiscount(8);
    Double price = 10001.0;
    CommonUtils
        .validateWholesalePriceConfig(productItemWholesalePriceRequestList, price, wholesalePricePercentageMappingResponse);
  }

  @Test
  public void setProductCreationTypeFlow1PcuExternalApp() {
    CommonUtils.setProductCreationType(productCreationRequest, PCU_EXTERNAL_APP, FLOW1);
    Assertions.assertEquals(ProductCreationType.FLOW1_WEB.getProductCreationType(),
        productCreationRequest.getProductCreationType().getProductCreationType());
  }

  @Test
  public void setProductCreationTypeFlow1MtaApp() {
    CommonUtils.setProductCreationType(productCreationRequest, MTA_APP, FLOW1);
    Assertions.assertEquals(ProductCreationType.FLOW1_APP.getProductCreationType(),
        productCreationRequest.getProductCreationType().getProductCreationType());
  }

  @Test
  public void setProductCreationTypeFlow1MtaApi() {
    CommonUtils.setProductCreationType(productCreationRequest, MTA_API, FLOW1);
    Assertions.assertEquals(ProductCreationType.FLOW1_API.getProductCreationType(),
        productCreationRequest.getProductCreationType().getProductCreationType());
  }

  @Test
  public void setProductCreationTypeFlow2PcuExternalApp() {
    CommonUtils.setProductCreationType(productCreationRequest, PCU_EXTERNAL_APP, FLOW2);
    Assertions.assertEquals(ProductCreationType.FLOW2_WEB.getProductCreationType(),
        productCreationRequest.getProductCreationType().getProductCreationType());
  }

  @Test
  public void setProductCreationTypeFlow2MtaAppApp() {
    CommonUtils.setProductCreationType(productCreationRequest, MTA_APP, FLOW2);
    Assertions.assertEquals(ProductCreationType.FLOW2_APP.getProductCreationType(),
        productCreationRequest.getProductCreationType().getProductCreationType());
  }

  @Test
  public void setProductCreationTypeFlow2PcuExternalApi() {
    CommonUtils.setProductCreationType(productCreationRequest, MTA_API, FLOW2);
    Assertions.assertEquals(ProductCreationType.FLOW2_API.getProductCreationType(),
        productCreationRequest.getProductCreationType().getProductCreationType());
  }


  @Test
  public void setProductCreationTypeFlow3() {
    productCreationRequest.setBusinessPartnerCode(StringUtils.EMPTY);
    CommonUtils.setProductCreationType(productCreationRequest, PCU_EXTERNAL_APP, StringUtils.EMPTY);
    Assertions.assertEquals(ProductCreationType.FLOW3_WEB.getProductCreationType(),
        productCreationRequest.getProductCreationType().getProductCreationType());
  }

  @Test
  public void setProductCreationType_null() {
    productCreationRequest.setProductCreationType(null);
    CommonUtils.setProductCreationType(productCreationRequest, PCU_EXTERNAL_APP, FLOW1);
    Assertions.assertEquals(ProductCreationType.FLOW1_WEB.getProductCreationType(),
        productCreationRequest.getProductCreationType().getProductCreationType());
  }

  @Test
  public void setProductCreationType_unknownBPCode() {
    productCreationRequest.setProductCreationType(ProductCreationType.FLOW3);
    productCreationRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    CommonUtils.setProductCreationType(productCreationRequest, PCU_EXTERNAL_APP, FLOW1);
    Assertions.assertEquals(ProductCreationType.FLOW3.getProductCreationType(),
        productCreationRequest.getProductCreationType().getProductCreationType());
  }

  @Test
  public void setProductCreationTypeWithNonEmptyCreationType() {
    productCreationRequest.setBusinessPartnerCode(StringUtils.EMPTY);
    productCreationRequest.setProductCreationType(ProductCreationType.CATEGORY_BULK_UPLOAD);
    CommonUtils.setProductCreationType(productCreationRequest, PCU_EXTERNAL_APP, StringUtils.EMPTY);
    Assertions.assertEquals(ProductCreationType.CATEGORY_BULK_UPLOAD.getProductCreationType(),
        productCreationRequest.getProductCreationType().getProductCreationType());
  }

  @Test
  public void setProductCreationTypWithRandomClientId() {
    CommonUtils.setProductCreationType(productCreationRequest, BUSINESS_PARTNER_CODE, StringUtils.EMPTY);
    assertNull(productCreationRequest.getProductCreationType());
  }

  @Test
  public void generateSpecificationDetailTest() {
    String result = CommonUtils.generateSpecificationDetail(productRequest);
    Assertions.assertEquals(SPECIFICATION_DETAIL, result);
  }

  @Test
  public void toProductL3SummaryResponseTest() {
    List<ProductL3SummaryResponse> response = CommonUtils
        .toProductL3SummaryResponse(Arrays.asList(productSkuSummaryResponse), Collections
            .singletonMap(CATEGORY_CODE, Arrays.asList(categoryResponse, categoryResponse1)));
    Assertions.assertEquals(PRODUCT_SKU, response.get(0).getProductSku());
    Assertions.assertEquals(CATEGORY_CODE, response.get(0).getCategoryCode());
    Assertions.assertEquals(BRAND, response.get(0).getBrand());
    Assertions.assertEquals(PRODUCT_NAME, response.get(0).getProductSkuName());
    Assertions.assertEquals(PRODUCT_IMAGE, response.get(0).getProductImage());
    Assertions.assertEquals(CATEGORY_NAME, response.get(0).getCategoryName());
    Assertions.assertEquals(String.join(" > ", Arrays.asList(CATEGORY_NAME, CATEGORY_NAME_1)),
        response.get(0).getCategoryHierarchy());
    Assertions.assertFalse(response.get(0).isArchived());
  }

  @Test
  public void setNonNullListValuesForLogsNullTest() {
    productCreationRequest.setProductItems(null);
    productCreationRequest.setProductAttributes(null);
    productCreationRequest.setProductCategories(null);
    CommonUtils.setNonNullListValuesForLogs(productCreationRequest);
    Assertions.assertTrue(Objects.nonNull(productCreationRequest.getProductItems()));
    Assertions.assertTrue(Objects.nonNull(productCreationRequest.getProductAttributes()));
    Assertions.assertTrue(Objects.nonNull(productCreationRequest.getProductCategories()));
  }

  @Test
  public void setNonNullListValuesForLogsTest() {
    productCreationRequest.setProductItems(new ArrayList<>());
    productCreationRequest.setProductAttributes(new ArrayList<>());
    productCreationRequest.setProductCategories(new ArrayList<>());
    CommonUtils.setNonNullListValuesForLogs(productCreationRequest);
    Assertions.assertTrue(productCreationRequest.getProductItems().isEmpty());
    Assertions.assertTrue(productCreationRequest.getProductAttributes().isEmpty());
    Assertions.assertTrue(productCreationRequest.getProductCategories().isEmpty());
  }

  @Test
  public void setMasterDataDetailsFromProductL3ResponseTest() {
    ItemCategoryDTO itemCategoryDTO = new ItemCategoryDTO();
    itemCategoryDTO.setProductCategoryCode(CATEGORY_CODE);
    itemCategoryDTO.setCategory(CATEGORY_NAME);
    ProductL3Response productL3Response = new ProductL3Response();
    productL3Response.setMasterDataProduct(new MasterDataProductDTO());
    productL3Response.setMasterCatalog(
        new MasterCatalogDTO(CATEGORY_CODE, new CategoryDTO(CATEGORY_CODE, CATEGORY_CODE)));
    productL3Response.setItemCatalogs(Arrays.asList(new ItemCatalogDTO(CATEGORY_CODE, Arrays.asList(itemCategoryDTO))));
    ProductBusinessPartner productBusinessPartnerEntity = new ProductBusinessPartner();
    CommonUtils.setMasterDataDetailsFromProductL3Response(productL3Response, productBusinessPartnerEntity);
    Assertions.assertEquals(CATEGORY_NAME, productBusinessPartnerEntity.getCategoryName());
  }

  @Test
  public void setMasterDataDetailsFromProductL3ResponseNoMatchingCategoryTest() {
    ItemCategoryDTO itemCategoryDTO = new ItemCategoryDTO();
    itemCategoryDTO.setProductCategoryCode("");
    itemCategoryDTO.setCategory(CATEGORY_NAME);
    ProductL3Response productL3Response = new ProductL3Response();
    productL3Response.setMasterDataProduct(new MasterDataProductDTO());
    productL3Response.setMasterCatalog(
        new MasterCatalogDTO(CATEGORY_CODE, new CategoryDTO(CATEGORY_CODE, CATEGORY_CODE)));
    productL3Response.setItemCatalogs(Arrays.asList(new ItemCatalogDTO(CATEGORY_CODE, Arrays.asList(itemCategoryDTO))));
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();

    CommonUtils.setMasterDataDetailsFromProductL3Response(productL3Response, productBusinessPartner);

    Assertions.assertNotEquals(CATEGORY_NAME, productBusinessPartner.getCategoryName());
  }

  @Test
  public void setItemNameByItemPickupPointTest() {
    Map<String, ItemPickupPointListingResponse> l5CodeAndResponseMap = new HashMap<>();
    ItemPickupPointListingResponse itemPickupPointListingResponse = new ItemPickupPointListingResponse();
    itemPickupPointListingResponse.setItemName(ITEM_NAME);
    Map<String, String> historyMap = new HashMap<>();
    ItemPickupPointRequest modifiedItemPickupPoint = new ItemPickupPointRequest();
    modifiedItemPickupPoint.setItemSku(ITEM_SKU);
    modifiedItemPickupPoint.setPickupPointId(PICKUP_POINT_CODE);
    l5CodeAndResponseMap.put(CommonUtils.toL5Id(ITEM_SKU, PICKUP_POINT_CODE), itemPickupPointListingResponse);
    CommonUtils.setItemNameByItemPickupPoint(l5CodeAndResponseMap, historyMap, modifiedItemPickupPoint, ITEM_NAME);
    Assertions.assertEquals(ITEM_NAME,
        l5CodeAndResponseMap.get(CommonUtils.toL5Id(ITEM_SKU, PICKUP_POINT_CODE)).getItemName());
  }

  @Test
  public void setMasterDataDetailsFromProductL3ResponseMasterCatalogNullTest() {
    ItemCategoryDTO itemCategoryDTO = new ItemCategoryDTO();
    itemCategoryDTO.setProductCategoryCode("");
    itemCategoryDTO.setCategory(CATEGORY_NAME);
    ProductL3Response productL3Response = new ProductL3Response();
    productL3Response.setMasterDataProduct(new MasterDataProductDTO());
    productL3Response.setMasterCatalog(null);
    productL3Response.setItemCatalogs(Arrays.asList(new ItemCatalogDTO(CATEGORY_CODE, Arrays.asList(itemCategoryDTO))));
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();

    CommonUtils.setMasterDataDetailsFromProductL3Response(productL3Response, productBusinessPartner);

    Assertions.assertNotEquals(CATEGORY_NAME, productBusinessPartner.getCategoryName());
  }

  @Test
  public void setMasterDataDetailsFromProductL3ResponseMasterCategpryNullTest() {
    ItemCategoryDTO itemCategoryDTO = new ItemCategoryDTO();
    itemCategoryDTO.setProductCategoryCode(CATEGORY_CODE);
    itemCategoryDTO.setCategory(CATEGORY_NAME);
    ProductL3Response productL3Response = new ProductL3Response();
    productL3Response.setMasterDataProduct(new MasterDataProductDTO());
    productL3Response.setMasterCatalog(
        new MasterCatalogDTO(CATEGORY_CODE, null));
    productL3Response.setItemCatalogs(Arrays.asList(new ItemCatalogDTO(CATEGORY_CODE, Arrays.asList(itemCategoryDTO))));
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();

    CommonUtils.setMasterDataDetailsFromProductL3Response(productL3Response, productBusinessPartner);
    assertNull(productBusinessPartner.getCategoryCode());
  }

  @Test
  public void setMasterDataDetailsFromProductL3ResponseItemCatalogsTest() {
    ItemCategoryDTO itemCategoryDTO = new ItemCategoryDTO();
    itemCategoryDTO.setProductCategoryCode(CATEGORY_CODE);
    itemCategoryDTO.setCategory(CATEGORY_NAME);
    ProductL3Response productL3Response = new ProductL3Response();
    productL3Response.setMasterCatalog(
        new MasterCatalogDTO(CATEGORY_CODE, new CategoryDTO(CATEGORY_CODE, CATEGORY_CODE)));
    productL3Response.setItemCatalogs(new ArrayList<>());
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();

    CommonUtils.setMasterDataDetailsFromProductL3Response(productL3Response, productBusinessPartner);
    assertNull(productBusinessPartner.getCategoryCode());
  }

  @Test
  public void validateProtectedBrandTrueTest() {
    BrandResponse brandResponse = new BrandResponse();
    brandResponse.setBrandCode(BRAND_CODE);
    brandResponse.setBrandName(BRAND_NAME);
    brandResponse.setProtectedBrand(Boolean.TRUE);
    boolean result = CommonUtils.validateProtectedBrand(brandResponse, BUSINESS_PARTNER_CODE);
    Assertions.assertTrue(result);
  }

  @Test
  public void validateProtectedBrandInternalUserTest() {
    BrandResponse brandResponse = new BrandResponse();
    brandResponse.setBrandCode(BRAND_CODE);
    brandResponse.setBrandName(BRAND_NAME);
    brandResponse.setProtectedBrand(Boolean.TRUE);
    boolean result = CommonUtils.validateProtectedBrand(brandResponse, INTERNAL);
    Assertions.assertFalse(result);
  }

  @Test
  public void validateProtectedBrandFalseTest() {
    BrandResponse brandResponse = new BrandResponse();
    brandResponse.setBrandCode(BRAND_CODE);
    brandResponse.setBrandName(BRAND_NAME);
    brandResponse.setProtectedBrand(Boolean.FALSE);
    boolean result = CommonUtils.validateProtectedBrand(brandResponse, BUSINESS_PARTNER_CODE);
    Assertions.assertFalse(result);
  }

  @Test
  public void checkBooleanEqualsTest() {
    boolean result = CommonUtils.checkBooleanEquals(null, null);
    Assertions.assertTrue(result);
  }

  @Test
  public void checkBooleanEqualsFirstValueNullTest() {
    boolean result = CommonUtils.checkBooleanEquals(null, true);
    Assertions.assertFalse(result);
  }

  @Test
  public void checkBooleanEqualsSecondValueNullTest() {
    boolean result = CommonUtils.checkBooleanEquals(true, null);
    Assertions.assertFalse(result);
  }

  @Test
  public void checkBooleanEqualsFirstValueTrueTest() {
    boolean result = CommonUtils.checkBooleanEquals(true, false);
    Assertions.assertFalse(result);
  }

  @Test
  public void checkBooleanEqualsSecondValueTrueTest() {
    boolean result = CommonUtils.checkBooleanEquals(false, true);
    Assertions.assertFalse(result);
  }

  @Test
  public void checkBooleanEqualsFirstValueTrueAndSecondValueTrueTest() {
    boolean result = CommonUtils.checkBooleanEquals(true, true);
    Assertions.assertTrue(result);
  }

  @Test
  public void generateProductLevel3SummarySingleTest() {
    this.businessPartner.setTrustedSeller(false);
    itemSummaryListResponse.setForceReview(true);
    ProductLevel3Summary productLevel3Summary =
      CommonUtils.generateProductLevel3SummarySingle(itemSummaryListResponse,
      null, null,
      productLevel3Inventory, businessPartner);
    Assertions.assertEquals(PRODUCT_NAME, productLevel3Summary.getProductName());
    Assertions.assertEquals(ITEM_SKU, productLevel3Summary.getItemSku());
    Assertions.assertTrue(productLevel3Summary.isForceReview());
  }

  @Test
  public void generateProductLevel3SummarySingleNullInvTest() {
    this.businessPartner.setTrustedSeller(false);
    itemSummaryListResponse.setForceReview(true);
    ProductLevel3Summary productLevel3Summary =
        CommonUtils.generateProductLevel3SummarySingle(itemSummaryListResponse,
            null, null,
            null, businessPartner);
    Assertions.assertEquals(PRODUCT_NAME, productLevel3Summary.getProductName());
    Assertions.assertEquals(ITEM_SKU, productLevel3Summary.getItemSku());
    Assertions.assertTrue(productLevel3Summary.isForceReview());
  }

  @Test
  public void generateProductLevel3SummarySingleForTrustedSellersTest() {
    this.businessPartner.setTrustedSeller(true);
    itemSummaryListResponse.setForceReview(true);
    ProductLevel3Summary productLevel3Summary =
      CommonUtils.generateProductLevel3SummarySingle(itemSummaryListResponse,
        null, null,
        productLevel3Inventory, businessPartner);
    Assertions.assertEquals(PRODUCT_NAME, productLevel3Summary.getProductName());
    Assertions.assertEquals(ITEM_SKU, productLevel3Summary.getItemSku());
    Assertions.assertFalse(productLevel3Summary.isForceReview());
  }

  @Test
  public void generateProductLevel3SummarySingleForNullBusinessPartnerTest() {
    this.businessPartner = null;
    itemSummaryListResponse.setForceReview(false);
    ProductLevel3Summary productLevel3Summary =
      CommonUtils.generateProductLevel3SummarySingle(itemSummaryListResponse,
        null, null,
        productLevel3Inventory, businessPartner);
    Assertions.assertEquals(PRODUCT_NAME, productLevel3Summary.getProductName());
    Assertions.assertEquals(ITEM_SKU, productLevel3Summary.getItemSku());
    Assertions.assertFalse(productLevel3Summary.isForceReview());
  }

  @Test
  public void generateProductLevel3SummarySingle_withDiscountTest() {
    itemSummaryListResponse.setPrice(Collections.singleton(priceDTO));
    itemSummaryListResponse.setCncActive(true);
    itemSummaryListResponse.setItemViewConfigs(Set.of(new ItemViewConfigDTO()));
    ProductLevel3Summary productLevel3Summary =
      CommonUtils.generateProductLevel3SummarySingle(itemSummaryListResponse,
        null, null,
        productLevel3Inventory, businessPartner);
    Assertions.assertEquals(PRODUCT_NAME, productLevel3Summary.getProductName());
    Assertions.assertEquals(ITEM_SKU, productLevel3Summary.getItemSku());
    Assertions.assertTrue(productLevel3Summary.isCncActive());
  }

  @Test
  public void generateProductLevel3SummarySingle_withDiscountBuyableTrueTest() {
    ItemViewConfigDTO itemViewConfigDTO = new ItemViewConfigDTO();
    itemViewConfigDTO.setItemBuyableSchedules(new ItemBuyableScheduleDTO());
    itemViewConfigDTO.setItemDiscoverableSchedules(new ItemDiscoverableScheduleDTO());
    itemSummaryListResponse.setPrice(Collections.singleton(priceDTO));
    itemSummaryListResponse.setCncActive(true);
    itemSummaryListResponse.setItemViewConfigs(Set.of(itemViewConfigDTO));
    ProductLevel3Summary productLevel3Summary =
        CommonUtils.generateProductLevel3SummarySingle(itemSummaryListResponse,
            null, null,
            productLevel3Inventory, businessPartner);
    Assertions.assertEquals(PRODUCT_NAME, productLevel3Summary.getProductName());
    Assertions.assertEquals(ITEM_SKU, productLevel3Summary.getItemSku());
    Assertions.assertTrue(productLevel3Summary.isCncActive());
  }

  @Test
  public void isPriceEditDisabledTest() {
    Assertions.assertTrue(CommonUtils.isPriceEditDisabled(true, Collections.singleton(priceDTO)));
  }

  @Test
  public void isPriceEditDisabled_withDiscountAndCampaignTest() {
    priceDTO.getListOfDiscountPrices().get(0).setCampaignCode(CATEGORY_CODE);
    priceDTO.getListOfDiscountPrices().get(0).setStartDateTime(
      Date.from(Instant.now().minus(Duration.ofDays(1)).truncatedTo(ChronoUnit.DAYS)));
    priceDTO.getListOfDiscountPrices().get(0).setEndDateTime(
      Date.from(Instant.now().minus(Duration.ofDays(-1)).truncatedTo(ChronoUnit.DAYS)));
    Assertions.assertTrue(CommonUtils.isPriceEditDisabled(false, Collections.singleton(priceDTO)));
  }

  @Test
  public void isPriceEditDisabled_withNoCampaignCodeTest() {
    priceDTO.getListOfDiscountPrices().get(0).setStartDateTime(
      Date.from(Instant.now().minus(Duration.ofDays(1)).truncatedTo(ChronoUnit.DAYS)));
    priceDTO.getListOfDiscountPrices().get(0).setEndDateTime(
      Date.from(Instant.now().minus(Duration.ofDays(-1)).truncatedTo(ChronoUnit.DAYS)));
    Assertions.assertFalse(CommonUtils.isPriceEditDisabled(false, Collections.singleton(priceDTO)));
  }

  @Test
  public void isPriceEditDisabled_withExpiredCampaignTest() {
    priceDTO.getListOfDiscountPrices().get(0).setCampaignCode(CATEGORY_CODE);
    priceDTO.getListOfDiscountPrices().get(0).setStartDateTime(
      Date.from(Instant.now().minus(Duration.ofDays(2)).truncatedTo(ChronoUnit.DAYS)));
    priceDTO.getListOfDiscountPrices().get(0).setEndDateTime(
      Date.from(Instant.now().minus(Duration.ofDays(1)).truncatedTo(ChronoUnit.DAYS)));
    Assertions.assertFalse(CommonUtils.isPriceEditDisabled(false, Collections.singleton(priceDTO)));
  }

  @Test
  public void isPriceEditDisabled_withUpcomingCampaignTest() {
    priceDTO.getListOfDiscountPrices().get(0).setCampaignCode(CATEGORY_CODE);
    priceDTO.getListOfDiscountPrices().get(0).setStartDateTime(
      Date.from(Instant.now().minus(Duration.ofDays(-2)).truncatedTo(ChronoUnit.DAYS)));
    priceDTO.getListOfDiscountPrices().get(0).setEndDateTime(
      Date.from(Instant.now().minus(Duration.ofDays(-3)).truncatedTo(ChronoUnit.DAYS)));
    Assertions.assertFalse(CommonUtils.isPriceEditDisabled(false, Collections.singleton(priceDTO)));
  }

  @Test
  public void isPriceEditDisabled_noDiscountPriceTest() {
    priceDTO.setListOfDiscountPrices(null);
    Assertions.assertFalse(CommonUtils.isPriceEditDisabled(false, Collections.singleton(priceDTO)));
  }

  @Test
  public void generateCategoryNameIdAndHierarchyTest() {
    String[] response =
      CommonUtils.generateCategoryNameIdAndHierarchy(Collections.singletonList(categoryResponse));
    Assertions.assertEquals(CATEGORY_NAME, response[0]);
  }

  @Test
  public void generateCategoryNameIdAndHierarchy_emptyTest() {
    String[] response =
      CommonUtils.generateCategoryNameIdAndHierarchy(Collections.emptyList());
  }

  @Test
  public void generateCategoryNameIdAndHierarchy_multipleCategoriesTest() {
    String[] response =
      CommonUtils.generateCategoryNameIdAndHierarchy(Arrays.asList(categoryResponse, categoryResponse1));
  }

  @Test
  public void isProductNotActivatedBeforeTest() {
    ProductCollection productCollection = new ProductCollection();
    productCollection.setState(WorkflowStates.NEED_CORRECTION.getValue());
    productCollection.setEdited(false);
    productCollection.setPostLive(false);

    Assertions.assertTrue(CommonUtils.isProductNotActivatedBefore(productCollection));
  }

  @Test
  public void isProductNotActivatedBeforePostliveProductTest() {
    ProductCollection productCollection = new ProductCollection();
    productCollection.setState(WorkflowStates.NEED_CORRECTION.getValue());
    productCollection.setEdited(false);
    productCollection.setPostLive(true);

    Assertions.assertFalse(CommonUtils.isProductNotActivatedBefore(productCollection));
  }

  @Test
  public void isProductNotActivatedBeforeEditedProductTest() {
    ProductCollection productCollection = new ProductCollection();
    productCollection.setState(WorkflowStates.NEED_CORRECTION.getValue());
    productCollection.setEdited(true);
    productCollection.setPostLive(false);

    Assertions.assertFalse(CommonUtils.isProductNotActivatedBefore(productCollection));
  }

  @Test
  public void isProductNotActivatedBeforeActiveProductTest() {
    ProductCollection productCollection = new ProductCollection();
    productCollection.setState(WorkflowStates.ACTIVE.getValue());
    productCollection.setEdited(false);
    productCollection.setPostLive(false);

    Assertions.assertFalse(CommonUtils.isProductNotActivatedBefore(productCollection));
  }

  @Test
  public void generateWholesaleSkuStatusMapTest() {
    Map<String, ProductItemWholesalePrice> productItemWholesalePriceMap =
        ImmutableMap.of(ITEM_SKU, new ProductItemWholesalePrice());
    WholeSalePriceSkuStatusDto wholeSalePriceSkuStatusDto2 =
        new WholeSalePriceSkuStatusDto(ITEM_SKU, PICKUP_POINT_CODE, Constants.ACTIVE);
    WholeSalePriceSkuStatusDto wholeSalePriceSkuStatusDto1 =
        new WholeSalePriceSkuStatusDto(ITEM_SKU, PICKUP_POINT_CODE, Constants.AUTO_INACTIVE);
    WholeSalePriceSkuStatusDto wholeSalePriceSkuStatusDto3 =
        new WholeSalePriceSkuStatusDto(ITEM_SKU_2, PICKUP_POINT_CODE, Constants.ACTIVE);
    CommonUtils.generateWholesaleSkuStatusMap(productItemWholesalePriceMap,
        Arrays.asList(wholeSalePriceSkuStatusDto1, wholeSalePriceSkuStatusDto2, wholeSalePriceSkuStatusDto3));
    Assertions.assertFalse(productItemWholesalePriceMap.values().iterator().next().isWholesalePriceActivated());
  }


  @Test
  public void generateWholesaleSkuStatusMapEmptyTestTest() {
    Map<String, ProductItemWholesalePrice> productItemWholesalePriceMap =
        ImmutableMap.of(ITEM_SKU, new ProductItemWholesalePrice());
    CommonUtils.generateWholesaleSkuStatusMap(productItemWholesalePriceMap, new ArrayList<>());
    Assertions.assertFalse(productItemWholesalePriceMap.get(ITEM_SKU).isWholesalePriceActivated());
  }

  @Test
  public void isProductActivatedBeforeFalseTest() {
    ProductCollection productCollection = new ProductCollection();
    productCollection.setState(WorkflowStates.NEED_CORRECTION.getValue());

    Assertions.assertFalse(CommonUtils.isProductActivatedBefore(productCollection));
  }

  @Test
  public void isProductActivatedBeforeTrueTest() {
    ProductCollection productCollection = new ProductCollection();
    productCollection.setState(WorkflowStates.ACTIVE.getValue());
    Assertions.assertTrue(CommonUtils.isProductActivatedBefore(productCollection));
  }

  @Test
  public void toInProgressProductResponseTest() {
    CommonUtils.toInProgressProductResponse(productBusinessPartner);
  }

  @Test
  public void isInternalSellerNullTest() {
    CommonUtils.isInternalSeller(null, Constants.INTERNAL_SELLERS);
  }

  @Test
  public void isInternalSellerCompanyNullTest() {
    CommonUtils.isInternalSeller(new ProfileResponse(), Constants.INTERNAL_SELLERS);
  }

  @Test
  public void isInternalSellerTrueTest() {
    ProfileResponse profileResponse = new ProfileResponse();
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setMerchantType(Constants.TD_MERCHANT);
    profileResponse.setCompany(companyDTO);
    CommonUtils.isInternalSeller(profileResponse, Constants.INTERNAL_SELLERS);
  }

  @Test
  public void isInternalSellerFalseTest() {
    ProfileResponse profileResponse = new ProfileResponse();
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setMerchantType(Constants.CC_MERCHANT);
    profileResponse.setCompany(companyDTO);
    CommonUtils.isInternalSeller(profileResponse, Constants.INTERNAL_SELLERS);
  }
  @Test
  public void isMppEnabledNullTest() {
    CommonUtils.isMppEnabled(null, MPP_ALLOWED_SELLERS);
  }

  @Test
  public void isMppEnabledCompanyNullTest() {
    CommonUtils.isMppEnabled(new ProfileResponse(), MPP_ALLOWED_SELLERS);
  }

  @Test
  public void isMppEnabledCncFalseTest() {
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(new CompanyDTO());
    CommonUtils.isMppEnabled(profileResponse, MPP_ALLOWED_SELLERS);
  }

  @Test
  public void isMppEnabledCncTrueTest() {
    ProfileResponse profileResponse = new ProfileResponse();
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setCncActivated(true);
    profileResponse.setCompany(companyDTO);
    CommonUtils.isMppEnabled(profileResponse, MPP_ALLOWED_SELLERS);
  }

  @Test
  public void isMppEnabledCncMPPTrueTest() {
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setMultiDefaultAddressFlag(true);
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setMerchantType(Constants.CM_MERCHANT);
    profileResponse.setCompany(companyDTO);
    CommonUtils.isMppEnabled(profileResponse, MPP_ALLOWED_SELLERS);
  }

  @Test
  public void isMppEnabledCMTest() {
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setMultiDefaultAddressFlag(true);
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setMerchantType(Constants.CM_MERCHANT);
    profileResponse.setCompany(companyDTO);
    CommonUtils.isMppEnabled(profileResponse, MPP_ALLOWED_SELLERS);
  }

  @Test
  public void toInvDetailInfoRequestTest(){
    itemLevel5Response.setItemSku(ITEM_SKU);
    itemLevel5Response.setPickupPointCode(PICKUP_POINT_CODE);
    itemLevel5Response.setMerchantCode(MERCHANT_CODE);
    InventoryDetailInfoRequestDTO result = CommonUtils.toInvDetailInfoRequest(itemLevel5Response);
    Assertions.assertEquals(ITEM_SKU,result.getWebItemSku());
  }

  @Test
  public void isWholesaleFlagChangedTest() {
    CommonUtils.isWholesaleFlagChanged(wholesalePriceSkuResponse, false);
  }

  @Test
  public void isWholesaleFlagChangedTest1() {
    wholesalePriceSkuResponse.setSkuStatus(Constants.ACTIVE_STATUS);
    CommonUtils.isWholesaleFlagChanged(wholesalePriceSkuResponse, false);
  }

  @Test
  public void isWholesaleFlagChangedTest2() {
    wholesalePriceSkuResponse.setSkuStatus(Constants.ACTIVE_STATUS);
    CommonUtils.isWholesaleFlagChanged(wholesalePriceSkuResponse, true);
  }

  @Test
  public void isWholesaleFlagChangedTest3() {
    wholesalePriceSkuResponse.setSkuStatus(Constants.INACTIVE_STATUS);
    CommonUtils.isWholesaleFlagChanged(wholesalePriceSkuResponse, false);
  }

  @Test
  public void updateProductBusinessPartnerTest() {
    ProductBusinessPartner productBusinessPartnerEntity = new ProductBusinessPartner();
    ProductVariantUpdateRequest request = new ProductVariantUpdateRequest();
    CommonUtils.updateProductBusinessPartner(productBusinessPartnerEntity, request);

    request.setCnc(true);
    CommonUtils.updateProductBusinessPartner(productBusinessPartnerEntity, request);

    request.setCnc(false);
    request.setOnline(true);
    CommonUtils.updateProductBusinessPartner(productBusinessPartnerEntity, request);
    Assertions.assertTrue(productBusinessPartnerEntity.isOnline());
  }

  @Test
  public void generateWholesaleHistoryTest() {
    Map<String, String> historyMap =
        CommonUtils.generateWholesaleHistory(true, false, ITEM_SKU, PICKUP_POINT_CODE, ITEM_NAME);
    Assertions.assertEquals(ITEM_SKU, historyMap.get(Constants.ITEM_SKU));
    Assertions.assertEquals(ITEM_NAME, historyMap.get(Constants.ITEM_NAME));
    Assertions.assertEquals(PICKUP_POINT_CODE, historyMap.get(Constants.PICKUP_POINT_CODE));
    Assertions.assertEquals(Boolean.TRUE.toString(), historyMap.get(Constants.PREVIOUS_VALUE));
    Assertions.assertEquals(Boolean.FALSE.toString(), historyMap.get(Constants.CURRENT_VALUE));
    Assertions.assertEquals(UpdateProductActivity.WHOLE_PRICE_FLAG.getDesc() + Constants.NEED_REVISION,
        historyMap.get(Constants.HISTORY_ACTIVITY));

    historyMap =
        CommonUtils.generateWholesaleHistory(null, false, ITEM_SKU, PICKUP_POINT_CODE, ITEM_NAME);
    Assertions.assertEquals(ITEM_SKU, historyMap.get(Constants.ITEM_SKU));
    Assertions.assertEquals(ITEM_NAME, historyMap.get(Constants.ITEM_NAME));
    Assertions.assertEquals(PICKUP_POINT_CODE, historyMap.get(Constants.PICKUP_POINT_CODE));
    Assertions.assertEquals(StringUtils.EMPTY, historyMap.get(Constants.PREVIOUS_VALUE));
    Assertions.assertEquals(Boolean.FALSE.toString(), historyMap.get(Constants.CURRENT_VALUE));
    Assertions.assertEquals(UpdateProductActivity.WHOLE_PRICE_FLAG.getDesc() + Constants.NEED_REVISION,
        historyMap.get(Constants.HISTORY_ACTIVITY));

    historyMap =
        CommonUtils.generateWholesaleHistory(true, true, ITEM_SKU, PICKUP_POINT_CODE, ITEM_NAME);
    Assertions.assertTrue(MapUtils.isEmpty(historyMap));

    historyMap =
        CommonUtils.generateWholesaleHistory(true, null, ITEM_SKU, PICKUP_POINT_CODE, ITEM_NAME);
    Assertions.assertTrue(MapUtils.isEmpty(historyMap));
  }

  @Test
  public void isDataUpdatedTest() {
    Assertions.assertFalse(CommonUtils.isDataUpdated(false, false, false, false, false, false, false, new ArrayList<>(),
        new ProductLevel3(),false));
    ProductLevel3 productLevel3 = new ProductLevel3();
    ProductLevel3 productLevel31= new ProductLevel3();
    productLevel31.setDeletedItems(new ArrayList<>());
    productLevel3.setDeletedItems(Collections.singletonList(ITEM_SKU));
    Assertions.assertTrue(
        CommonUtils.isDataUpdated(false, false, false, false, false, false, false, new ArrayList<>(), productLevel3,false));
    productLevel3.setDeletedItems(new ArrayList<>());
    ProductItemLevel3 productItemLevel3 = new ProductItemLevel3();
    productItemLevel3.setItemSku(ITEM_SKU);
    productLevel3.setNewlyAddedItems(Collections.singletonList(productItemLevel3));
    Assertions.assertTrue(
        CommonUtils.isDataUpdated(false, false, false, false, false, false, false, new ArrayList<>(), productLevel3,true));
    Assertions.assertTrue(CommonUtils.isDataUpdated(false, false, false, false, false, false, true, new ArrayList<>(),
        new ProductLevel3(),true));
    Assertions.assertTrue(CommonUtils.isDataUpdated(false, false, false, false, false, true, false, new ArrayList<>(),
        new ProductLevel3(),true));
    Assertions.assertTrue(CommonUtils.isDataUpdated(false, false, false, false, true, false, false, new ArrayList<>(),
        new ProductLevel3(),true));
    Assertions.assertTrue(CommonUtils.isDataUpdated(false, false, false, true, false, false, false, new ArrayList<>(),
        new ProductLevel3(),true));
    Assertions.assertTrue(CommonUtils.isDataUpdated(false, false, true, false, false, false, false, new ArrayList<>(),
        new ProductLevel3(),true));
    Assertions.assertTrue(CommonUtils.isDataUpdated(false, true, false, false, false, false, false, new ArrayList<>(),
        new ProductLevel3(),true));
    Assertions.assertTrue(CommonUtils.isDataUpdated(true, false, false, false, false, false, false, new ArrayList<>(),
        new ProductLevel3(),true));
    RestrictedKeywordsByField restrictedKeywordsByField = new RestrictedKeywordsByField();
    restrictedKeywordsByField.setKeywords(Collections.singletonList(CATEGORY_CODE));
    Assertions.assertTrue(CommonUtils.isDataUpdated(false, false, false, false, false, false, false,
        Collections.singletonList(restrictedKeywordsByField), new ProductLevel3(),true));
    CommonUtils.isDataUpdated(false, false, false, false, false, false, false, Collections.emptyList(), productLevel31,
        false);
    CommonUtils.isDataUpdated(false, false, false, false, false, false, false, Collections.emptyList(), productLevel3,
        false);
    CommonUtils.isDataUpdated(false, false, false, false, false, false, false, Collections.emptyList(), productLevel31,
        true);
    CommonUtils.isDataUpdated(false, false, false, false, false, false, false, Collections.emptyList(), productLevel3,
        true);
  }

  @Test
  public void testIsDataUpdated() {
    // Scenario 1: No changes in ProductDetailEditDTO and empty ProductLevel3
    ProductDetailEditDTO productDetailEditDTO = new ProductDetailEditDTO();
    ProductLevel3 product = new ProductLevel3();
    Assertions.assertFalse(CommonUtils.isDataUpdated(productDetailEditDTO, product));
    productDetailEditDTO.setContentChanged(true);
    Assertions.assertTrue(CommonUtils.isDataUpdated(productDetailEditDTO, product));
    productDetailEditDTO.setContentChanged(false);
    product.setDistributionInfoUpdated(true);
    Assertions.assertTrue(CommonUtils.isDataUpdated(productDetailEditDTO, product));
    productDetailEditDTO.setContentChanged(false);
    product.setNewlyAddedItems(Collections.singletonList(new ProductItemLevel3()));
    Assertions.assertTrue(CommonUtils.isDataUpdated(productDetailEditDTO, product));
    product.setNewlyAddedItems(null);
    product.setDeletedItems(Collections.singletonList("ITEM_SKU"));
    Assertions.assertTrue(CommonUtils.isDataUpdated(productDetailEditDTO, product));
    product.setDeletedItems(new ArrayList<>());
    RestrictedKeywordsByField restrictedKeywordsByField = new RestrictedKeywordsByField();
    restrictedKeywordsByField.setKeywords(Collections.singletonList("CATEGORY_CODE"));
    productDetailEditDTO.setRestrictedKeywordsByFieldList(Collections.singletonList(restrictedKeywordsByField));
    Assertions.assertTrue(CommonUtils.isDataUpdated(productDetailEditDTO, product));
    productDetailEditDTO.setRestrictedKeywordsByFieldList(Collections.emptyList());
    productDetailEditDTO.setPreOrderChange(true);
    Assertions.assertTrue(CommonUtils.isDataUpdated(productDetailEditDTO, product));
    product.setCategoryUpdated(true);
    Assertions.assertTrue(CommonUtils.isDataUpdated(productDetailEditDTO, product));
    product.setCategoryUpdated(false);
    productDetailEditDTO.setPreOrderChange(false);
    product.setDistributionInfoUpdated(false);
    product.setBrandUpdated(true);
    Assertions.assertTrue(CommonUtils.isDataUpdated(productDetailEditDTO, product));
    productDetailEditDTO.setContentChanged(true);
    productDetailEditDTO.setOff2OnChannelFlagChanged(true);
    productDetailEditDTO.setPreOrderChange(false);
    product.setDeletedItems(Collections.singletonList("ITEM_SKU"));
    product.setNewlyAddedItems(Collections.singletonList(new ProductItemLevel3()));
    Assertions.assertTrue(CommonUtils.isDataUpdated(productDetailEditDTO, product));
    productDetailEditDTO.setContentChanged(false);
    productDetailEditDTO.setRestrictedKeywordsByFieldList(null);
    productDetailEditDTO.setPreOrderChange(false);
    product.setDeletedItems(new ArrayList<>());
    product.setNewlyAddedItems(null);
    Assertions.assertTrue(CommonUtils.isDataUpdated(productDetailEditDTO, product));
    product.setSizeChartChanged(false);
    Assertions.assertTrue(CommonUtils.isDataUpdated(productDetailEditDTO, product));
    productDetailEditDTO.setVideoUpdated(false);
    Assertions.assertTrue(CommonUtils.isDataUpdated(productDetailEditDTO, product));
  }

  @Test
  public void testIsDataUpdatedForSizeChartChange(){
    ProductDetailEditDTO productDetailEditDTO = new ProductDetailEditDTO();
    ProductLevel3 product = new ProductLevel3();
    product.setSizeChartChanged(true);
    Assertions.assertTrue(CommonUtils.isDataUpdated(productDetailEditDTO, product));
  }

  @Test
  public void testISDataUpdatedForVideoUrl(){
    ProductDetailEditDTO productDetailEditDTO = new ProductDetailEditDTO();
    ProductLevel3 product = new ProductLevel3();
    productDetailEditDTO.setVideoUpdated(true);
    Assertions.assertTrue(CommonUtils.isDataUpdated(productDetailEditDTO, product));
  }


  @Test
  public void testISDataUpdatedFalseForVideoUrl(){
    ProductDetailEditDTO productDetailEditDTO = new ProductDetailEditDTO();
    ProductLevel3 product = new ProductLevel3();
    productDetailEditDTO.setVideoUpdated(null);
    Assertions.assertFalse(CommonUtils.isDataUpdated(productDetailEditDTO, product));
  }

  @Test
  public void publishPDTContentEditEventTest() {
    Assertions.assertFalse(CommonUtils.publishPDTContentEditEvent(false, new ArrayList<>(), false));
    ProductLevel3 productLevel3 = new ProductLevel3();
    ProductItemLevel3 productItemLevel3 = new ProductItemLevel3();
    productLevel3.setNewlyAddedItems(Collections.singletonList(productItemLevel3));
    Assertions.assertFalse(CommonUtils.publishPDTContentEditEvent(false, new ArrayList<>(), false));
    RestrictedKeywordsByField restrictedKeywordsByField = new RestrictedKeywordsByField();
    restrictedKeywordsByField.setKeywords(Collections.singletonList(ITEM_NAME));
    Assertions.assertTrue(
        CommonUtils.publishPDTContentEditEvent(false, Collections.singletonList(restrictedKeywordsByField),
            false));
    Assertions.assertTrue(CommonUtils.publishPDTContentEditEvent(true, new ArrayList<>(), false));
  }

  @Test
  public void eligibleForVariantImageUpdateTest() {
    CommonUtils.eligibleForVariantImageUpdate(new ArrayList<>(), new ArrayList<>(), false);
    CommonUtils.eligibleForVariantImageUpdate(new ArrayList<>(), new ArrayList<>(), true);
    ProductItemImageRequest productItemImageRequest = new ProductItemImageRequest();
    productItemImageRequest.setSkuCode(ITEM_SKU_2);
    CommonUtils.eligibleForVariantImageUpdate(Collections.singletonList(productItemImageRequest), new ArrayList<>(),
        true);
    CommonUtils.eligibleForVariantImageUpdate(new ArrayList<>(), Collections.singletonList(productItemImageRequest),
        true);
  }

  @Test
  public void isEligibleForLogisticsUpdateTest() {
    Assertions.assertFalse(CommonUtils.isEligibleForLogisticsUpdate(null));
    Assertions.assertFalse(CommonUtils.isEligibleForLogisticsUpdate(new AddDeleteVariantRequest()));
    AddDeleteVariantRequest addDeleteVariantRequest = new AddDeleteVariantRequest();
    AddVariantRequest addVariantRequest = new AddVariantRequest();
    addDeleteVariantRequest.setAddVariantsList(Collections.singletonList(addVariantRequest));
    Assertions.assertTrue(CommonUtils.isEligibleForLogisticsUpdate(addDeleteVariantRequest));
  }

  @Test
  public void isEligibleForLogisticsUpdateTestForReorder() {
    Assertions.assertFalse(CommonUtils.isPreOrderChanged(false, null));
    Assertions.assertTrue(CommonUtils.isPreOrderChanged(true, new PreOrderRequest()));
  }

  @Test
  public void checkForChanged() {
    CommonUtils.checkForChangedRequest(false, false, false, false, false, false, false, false);
    CommonUtils.checkForChangedRequest(false, false, false, false, false, true, false, false);
    CommonUtils.checkForChangedRequest(false, false, false, false, true, false, false, false);
    CommonUtils.checkForChangedRequest(false, false, false, true, true, true, false, false);
    CommonUtils.checkForChangedRequest(false, false, true, true, false, false, false, false);
    CommonUtils.checkForChangedRequest(false, true, false, true, false, true, false, false);
    CommonUtils.checkForChangedRequest(true, false, false, true, true, false, false, false);
    Assertions.assertTrue(CommonUtils.checkForChangedRequest(false, false, false, false, false, false, true, false));
  }

  @Test
  void checkForChangedForVideo() {
    Assertions.assertTrue(CommonUtils.checkForChangedRequest(false, false, false, false, false,
      false, false, true));
  }

  @Test
  public void checkIfB2bFieldsChangedTest() {
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    ItemPickupPointListingResponse itemPickupPointListingResponse = new ItemPickupPointListingResponse();
    itemPickupPointListingResponse.setViewConfigs(new ArrayList<>());
    Assertions.assertFalse(
        CommonUtils.checkIfB2bFieldsChanged(itemPickupPointRequest, itemPickupPointListingResponse, false));
    B2BFields b2BFields = new B2BFields();
    b2BFields.setPrice(100.0);
    itemPickupPointRequest.setB2bFields(b2BFields);
    Assertions.assertTrue(
        CommonUtils.checkIfB2bFieldsChanged(itemPickupPointRequest, itemPickupPointListingResponse, false));
    itemPickupPointRequest.setB2bFields(b2BFields);
    B2BResponse b2BResponse = new B2BResponse();
    b2BResponse.setBasePrice(1.0);
    b2BResponse.setManaged(true);
    itemPickupPointListingResponse.setB2bFields(b2BResponse);
    Assertions.assertTrue(
        CommonUtils.checkIfB2bFieldsChanged(itemPickupPointRequest, itemPickupPointListingResponse, false));
    b2BFields.setManaged(true);
    b2BFields.setPrice(10.0);
    itemPickupPointRequest.setB2bFields(b2BFields);
    b2BResponse.setManaged(true);
    b2BResponse.setBasePrice(10.0);
    itemPickupPointListingResponse.setB2bFields(b2BResponse);
    Assertions.assertFalse(
        CommonUtils.checkIfB2bFieldsChanged(itemPickupPointRequest, itemPickupPointListingResponse, false));
    ViewConfigResponse viewConfigResponse = new ViewConfigResponse();
    viewConfigResponse.setBuyable(true);
    viewConfigResponse.setDisplay(true);
    viewConfigResponse.setChannelId(Constants.B2B_CHANNEL);
    itemPickupPointListingResponse.setViewConfigs(Collections.singletonList(viewConfigResponse));
    Assertions.assertTrue(
        CommonUtils.checkIfB2bFieldsChanged(itemPickupPointRequest, itemPickupPointListingResponse, false));
  }

  @Test
  public void salesChannelFromProfileResponseNullProfileResponseTest() {
    List<String> salesChannel = CommonUtils.salesChannelFromProfileResponse(null);
    Assertions.assertEquals(salesChannel, new ArrayList<>());
  }

  @Test
  public void salesChannelFromProfileResponseNullSalesChannelTest() {
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(new CompanyDTO());
    profileResponse.getCompany().setSalesChannel(null);
    List<String> salesChannel = CommonUtils.salesChannelFromProfileResponse(profileResponse);
    Assertions.assertEquals(salesChannel, new ArrayList<>());
  }

  @Test
  public void setB2bAndB2cFLagsTes() {
    CommonUtils.setB2bAndB2cFLags(new ProductVariantUpdateRequest(), new ItemPickupPointListingResponse(),
        new EditFlagChangesDTO());
    ProductVariantUpdateRequest productVariantUpdateRequest = new ProductVariantUpdateRequest();
    productVariantUpdateRequest.setB2cActivated(true);
    productVariantUpdateRequest.setB2bActivated(true);
    CommonUtils.setB2bAndB2cFLags(productVariantUpdateRequest, new ItemPickupPointListingResponse(),
        new EditFlagChangesDTO());
    ItemPickupPointListingResponse itemPickupPointListingResponse = new ItemPickupPointListingResponse();
    itemPickupPointListingResponse.setB2bActivatedAtL3Level(true);
    itemPickupPointListingResponse.setB2cActivatedAtL3Level(true);
    EditFlagChangesDTO editFlagChangesDTO = new EditFlagChangesDTO();
    CommonUtils.setB2bAndB2cFLags(productVariantUpdateRequest, itemPickupPointListingResponse,
        editFlagChangesDTO);
    Assertions.assertFalse(editFlagChangesDTO.isB2bFlagChangedAtL3Level());
  }

  @Test
  public void setB2cActivatedFromProfileResponseTestPureB2bSeller() {
    CommonUtils.setB2cActivatedFromProfileResponse(new ProductVariantUpdateRequest(), new ProfileResponse(), false);
    CommonUtils.setB2cActivatedFromProfileResponse(new ProductVariantUpdateRequest(), new ProfileResponse(), true);
    ProductVariantUpdateRequest productVariantUpdateRequest = new ProductVariantUpdateRequest();
    productVariantUpdateRequest.setProductItems(
        Collections.singletonList(new ProductVariantPriceStockAndImagesRequest()));
    CommonUtils.setB2cActivatedFromProfileResponse(productVariantUpdateRequest, new ProfileResponse(), true);
    ProductVariantPriceStockAndImagesRequest request = new ProductVariantPriceStockAndImagesRequest();
    request.setModifiedItemPickupPoints(
        Collections.singletonList(new ItemPickupPointRequest()));
    productVariantUpdateRequest.setProductItems(Collections.singletonList(request));
    CommonUtils.setB2cActivatedFromProfileResponse(productVariantUpdateRequest, new ProfileResponse(), true);
    productVariantUpdateRequest.setAddPickupPoints(Collections.singletonList(new ItemPickupPointRequest()));
    CommonUtils.setB2cActivatedFromProfileResponse(productVariantUpdateRequest, new ProfileResponse(), true);
    assertNull(
        productVariantUpdateRequest.getProductItems().get(0).getModifiedItemPickupPoints().get(0).getB2bFields());
  }

  @Test
  public void setB2cActivatedFromProfileResponseTestB2bAndB2CSeller() {
    ProductVariantUpdateRequest productVariantUpdateRequest = new ProductVariantUpdateRequest();
    productVariantUpdateRequest.setProductItems(
        Collections.singletonList(new ProductVariantPriceStockAndImagesRequest()));
    ProductVariantPriceStockAndImagesRequest request = new ProductVariantPriceStockAndImagesRequest();
    request.setModifiedItemPickupPoints(
        Collections.singletonList(new ItemPickupPointRequest()));
    productVariantUpdateRequest.setProductItems(Collections.singletonList(request));
    productVariantUpdateRequest.setAddPickupPoints(Collections.singletonList(new ItemPickupPointRequest()));
    ProfileResponse profileResponse = new ProfileResponse();
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setSalesChannel(Arrays.asList(Constants.B2B_SELLER_CHANNEL, Constants.B2C_SELLER_CHANNEL));
    profileResponse.setCompany(companyDTO);
    CommonUtils.setB2cActivatedFromProfileResponse(productVariantUpdateRequest, profileResponse, true);
    assertNull(
        productVariantUpdateRequest.getProductItems().get(0).getModifiedItemPickupPoints().get(0).getB2bFields());
  }

  @Test
  public void overrideAutoNeedRevisionAndTakeDownFlagsTest() {
    CommonUtils.overrideAutoNeedRevisionAndTakeDownFlags(new AutoNeedRevisionAndForceReviewResponse());
    AutoNeedRevisionAndForceReviewResponse autoNeedRevisionAndForceReviewResponse =
        new AutoNeedRevisionAndForceReviewResponse();
    RestrictedKeywordsByFieldAndActionType restrictedKeywordsByFieldAndActionType =
        new RestrictedKeywordsByFieldAndActionType();
    restrictedKeywordsByFieldAndActionType.setAction(1);
    autoNeedRevisionAndForceReviewResponse.setRestrictedKeywordsByFieldAndActionType(restrictedKeywordsByFieldAndActionType);
    CommonUtils.overrideAutoNeedRevisionAndTakeDownFlags(autoNeedRevisionAndForceReviewResponse);
    autoNeedRevisionAndForceReviewResponse.getRestrictedKeywordsByFieldAndActionType().setAction(3);
    CommonUtils.overrideAutoNeedRevisionAndTakeDownFlags(autoNeedRevisionAndForceReviewResponse);
    Assertions.assertFalse(autoNeedRevisionAndForceReviewResponse.isForceReview());
  }

  @Test
  public void validateAddDeleteVariantsRequestTest() {
    assertNull(CommonUtils.validateAddDeleteVariantsRequest(new ProductLevel3(), null, false, businessPartner,
        categoryDetailResponse, false, existingDefiningAttributeDetails, false, true, false));
    assertNull(CommonUtils.validateAddDeleteVariantsRequest(new ProductLevel3(), null, true, businessPartner,
        categoryDetailResponse, false, existingDefiningAttributeDetails, false, true, false));
    ProductLevel3 productLevel3 = new ProductLevel3();
    ProductItemLevel3 productItemLevel3 = new ProductItemLevel3();
    productLevel3.setNewlyAddedItems(Collections.singletonList(productItemLevel3));
    assertNull(CommonUtils.validateAddDeleteVariantsRequest(productLevel3, new ProductL3Response(), false, null,
        categoryDetailResponse, false, existingDefiningAttributeDetails, false, true, false));
  }

  @Test
  public void validateAddDeleteVariantsRequestTest3() {
    ProductLevel3 productLevel3 = new ProductLevel3();
    ProductItemLevel3 productItemLevel3 = new ProductItemLevel3();
    productLevel3.setNewlyAddedItems(Collections.singletonList(productItemLevel3));
    productLevel3.setNewlyAddedItems(null);
    ProductL3Response productL3Response = new ProductL3Response();
    assertNull(CommonUtils.validateAddDeleteVariantsRequest(productLevel3, productL3Response, false, null,
        categoryDetailResponse, false, existingDefiningAttributeDetails, false, true, true));
    productL3Response.setDistributionMappingStatus(Constants.DISTRIBUTION);
    assertNull(CommonUtils.validateAddDeleteVariantsRequest(productLevel3, productL3Response, false, null,
        categoryDetailResponse, false, existingDefiningAttributeDetails, false, true, true));
    productL3Response.setDistributionMappingStatus(Constants.PURE_DISTRIBUTION);
    assertNull(CommonUtils.validateAddDeleteVariantsRequest(productLevel3, productL3Response, false, null,
        categoryDetailResponse, false, existingDefiningAttributeDetails, false, true, true));
    productLevel3.setNewlyAddedItems(Collections.singletonList(productItemLevel3));
    assertNotNull(CommonUtils.validateAddDeleteVariantsRequest(productLevel3, productL3Response, false, null,
        categoryDetailResponse, false, existingDefiningAttributeDetails, false, true, true));
    productLevel3.setNewlyAddedItems(new ArrayList<>());
    productLevel3.setDeletedItems(Collections.singletonList(PCU_EXTERNAL_APP));
    assertNotNull(CommonUtils.validateAddDeleteVariantsRequest(productLevel3, productL3Response, false, null,
        categoryDetailResponse, false, existingDefiningAttributeDetails, false, true, true));
  }

  @Test
  public void validateAddDeleteVariantsRequestErrorTest() {
    ProductLevel3 productLevel3 = new ProductLevel3();
    ProductItemLevel3 productItemLevel3 = new ProductItemLevel3();
    productLevel3.setNewlyAddedItems(Collections.singletonList(productItemLevel3));
    productLevel3.setProductName(PRODUCT_NAME);
    productLevel3.setProductSku(PRODUCT_SKU);
    ProductL3Response productL3Response = new ProductL3Response();
    productL3Response.setDefiningAttributes(Collections.singletonList(new ProductAttributeDTO()));
    productLevel3.setAttributes(null);
    Assertions.assertNotNull(
        CommonUtils.validateAddDeleteVariantsRequest(productLevel3, productL3Response, false, businessPartner, categoryDetailResponse,
            false, existingDefiningAttributeDetails, false, true, false));
  }


  @Test
  public void validateAddDeleteVariantsRequestFalseTest() {
    ProductLevel3 productLevel3 = new ProductLevel3();
    ProductItemLevel3 productItemLevel3 = new ProductItemLevel3();
    productLevel3.setNewlyAddedItems(Collections.singletonList(productItemLevel3));
    productLevel3.setProductName(PRODUCT_NAME);
    productLevel3.setProductSku(PRODUCT_SKU);
    ProductL3Response productL3Response = new ProductL3Response();
    productL3Response.setDefiningAttributes(Collections.singletonList(new ProductAttributeDTO()));
    productLevel3.setAttributes(null);
    CommonUtils.validateAddDeleteVariantsRequest(productLevel3, productL3Response, true, businessPartner, categoryDetailResponse,
        false, existingDefiningAttributeDetails, false, true, false);
  }

  @Test
  public void validateAddDeleteVariantsRequestTest2() {
    ProductLevel3 productLevel3 = new ProductLevel3();
    ProductItemLevel3 productItemLevel3 = new ProductItemLevel3();
    productLevel3.setNewlyAddedItems(Collections.singletonList(productItemLevel3));
    productLevel3.setProductName(PRODUCT_NAME);
    productLevel3.setProductSku(PRODUCT_SKU);
    ProductL3Response productL3Response = new ProductL3Response();
    ProductAttributeDTO productAttributeDTO = new ProductAttributeDTO();
    ProductAttributeDetailDTO productAttributeDetailDTO = new ProductAttributeDetailDTO();
    productAttributeDetailDTO.setAttributeCode(CATEGORY_CODE);
    productAttributeDetailDTO.setAttributeValue(ATTRIBUTE_VALUE);
    productAttributeDTO.setProductAttributeDetails(Collections.singletonList(productAttributeDetailDTO));
    productL3Response.setDefiningAttributes(Collections.singletonList(productAttributeDTO));
    assertNull(
        CommonUtils.validateAddDeleteVariantsRequest(productLevel3, productL3Response, true, businessPartner, categoryDetailResponse,
            false, existingDefiningAttributeDetails, false, true, false));
  }

  @Test
  public void validateAddDeleteVariantsRequestError2() {
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setInventoryFulfillment("BL");
    businessPartner.setCompany(companyDTO);
    ProductLevel3 productLevel3 = new ProductLevel3();
    ProductItemLevel3 productItemLevel3 = new ProductItemLevel3();
    TreeMap<String, String> itemAttributesMap = new TreeMap<>();
    itemAttributesMap.put(CATEGORY_CODE, ATTRIBUTE_VALUE);
    productItemLevel3.setItemAttributesMap(itemAttributesMap);
    productLevel3.setNewlyAddedItems(Collections.singletonList(productItemLevel3));
    productLevel3.setProductName(PRODUCT_NAME);
    productLevel3.setProductSku(PRODUCT_SKU);
    ProductL3Response productL3Response = new ProductL3Response();
    ProductAttributeDTO productAttributeDTO = new ProductAttributeDTO();
    ProductAttributeDetailDTO productAttributeDetailDTO = new ProductAttributeDetailDTO();
    productAttributeDetailDTO.setAttributeCode(CATEGORY_CODE);
    productAttributeDetailDTO.setAttributeValue(ATTRIBUTE_VALUE);
    productAttributeDTO.setProductAttributeDetails(Collections.singletonList(productAttributeDetailDTO));
    productL3Response.setDefiningAttributes(Collections.singletonList(productAttributeDTO));
    Assertions.assertEquals(ApiErrorCode.INVALID_ADD_DELETE_REQUEST,
        CommonUtils.validateAddDeleteVariantsRequest(productLevel3, productL3Response, true, businessPartner, categoryDetailResponse,
            false, existingDefiningAttributeDetails, false, true, false));
  }

  @Test
  public void validateAddDeleteVariantsRequestTestNoError2() {
    ProductLevel3 productLevel3 = new ProductLevel3();
    ProductItemLevel3 productItemLevel3 = new ProductItemLevel3();
    TreeMap<String, String> itemAttributesMap = new TreeMap<>();
    itemAttributesMap.put(CATEGORY_CODE, ATTRIBUTE_VALUE_1);
    productItemLevel3.setItemAttributesMap(itemAttributesMap);
    productLevel3.setNewlyAddedItems(Collections.singletonList(productItemLevel3));
    productLevel3.setProductName(PRODUCT_NAME);
    productLevel3.setProductSku(PRODUCT_SKU);
    ProductL3Response productL3Response = new ProductL3Response();
    ProductAttributeDTO productAttributeDTO = new ProductAttributeDTO();
    ProductAttributeDetailDTO productAttributeDetailDTO = new ProductAttributeDetailDTO();
    productAttributeDetailDTO.setAttributeCode(CATEGORY_CODE);
    productAttributeDetailDTO.setAttributeValue(ATTRIBUTE_VALUE);
    productAttributeDTO.setProductAttributeDetails(Collections.singletonList(productAttributeDetailDTO));
    productL3Response.setDefiningAttributes(Collections.singletonList(productAttributeDTO));
    assertNull(
        CommonUtils.validateAddDeleteVariantsRequest(productLevel3, productL3Response, true, businessPartner, categoryDetailResponse,
            false, existingDefiningAttributeDetails, false, true, false));
  }

  @Test
  public void validateAddDeleteVariantsRequestTestDuplicateError2() {
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setInventoryFulfillment("BL");
    businessPartner.setCompany(companyDTO);
    ProductLevel3 productLevel3 = new ProductLevel3();
    ProductItemLevel3 productItemLevel3 = new ProductItemLevel3();
    TreeMap<String, String> itemAttributesMap = new TreeMap<>();
    itemAttributesMap.put(CATEGORY_CODE, ATTRIBUTE_VALUE_1);
    productItemLevel3.setItemAttributesMap(itemAttributesMap);
    productLevel3.setNewlyAddedItems(Arrays.asList(productItemLevel3, productItemLevel3));
    productLevel3.setProductName(PRODUCT_NAME);
    productLevel3.setProductSku(PRODUCT_SKU);
    ProductL3Response productL3Response = new ProductL3Response();
    ProductAttributeDTO productAttributeDTO = new ProductAttributeDTO();
    ProductAttributeDetailDTO productAttributeDetailDTO = new ProductAttributeDetailDTO();
    productAttributeDetailDTO.setAttributeCode(CATEGORY_CODE);
    productAttributeDetailDTO.setAttributeValue(ATTRIBUTE_VALUE);
    productAttributeDTO.setProductAttributeDetails(Collections.singletonList(productAttributeDetailDTO));
    productL3Response.setDefiningAttributes(Collections.singletonList(productAttributeDTO));
    Assertions.assertEquals(ApiErrorCode.INVALID_ADD_DELETE_DUPLICATE_REQUEST,
        CommonUtils.validateAddDeleteVariantsRequest(productLevel3, productL3Response, true, businessPartner, categoryDetailResponse,
            false, existingDefiningAttributeDetails, false, true, false));
  }

  @Test
  public void validateAddDeleteVariantsRequestWarehouseSellerFalseTest() {
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setInventoryFulfillment(NON_WAREHOUSE_SELLER);
    businessPartner.setCompany(companyDTO);
    ProductLevel3 productLevel3 = new ProductLevel3();
    ProductItemLevel3 productItemLevel3 = new ProductItemLevel3();
    TreeMap<String, String> itemAttributesMap = new TreeMap<>();
    itemAttributesMap.put(CATEGORY_CODE, ATTRIBUTE_VALUE_1);
    productItemLevel3.setItemAttributesMap(itemAttributesMap);
    productLevel3.setNewlyAddedItems(Arrays.asList(productItemLevel3, productItemLevel3));
    productLevel3.setProductName(PRODUCT_NAME);
    productLevel3.setProductSku(PRODUCT_SKU);
    ProductL3Response productL3Response = new ProductL3Response();
    ProductAttributeDTO productAttributeDTO = new ProductAttributeDTO();
    ProductAttributeDetailDTO productAttributeDetailDTO = new ProductAttributeDetailDTO();
    productAttributeDetailDTO.setAttributeCode(CATEGORY_CODE);
    productAttributeDetailDTO.setAttributeValue(ATTRIBUTE_VALUE);
    productAttributeDTO.setProductAttributeDetails(Collections.singletonList(productAttributeDetailDTO));
    productL3Response.setDefiningAttributes(Collections.singletonList(productAttributeDTO));
    CommonUtils.validateAddDeleteVariantsRequest(productLevel3, productL3Response, true, businessPartner, categoryDetailResponse,
        false, existingDefiningAttributeDetails, false, true, false);
  }

  @Test
  public void validateAddDeleteVariantsRequestWarehouseSellerProductAttributeTest() {
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setInventoryFulfillment(WAREHOUSE_SELLER);
    businessPartner.setCompany(companyDTO);
    List<String> attributeValues = new ArrayList<>();
    attributeValues.add(StringUtils.EMPTY);
    ProductLevel3Attribute productLevel3Attribute = new ProductLevel3Attribute();
    productLevel3Attribute.setAttributeCode(ATTRIBUTE_CODE);
    productLevel3Attribute.setVariantCreation(true);
    productLevel3Attribute.setValues(attributeValues);
    ProductLevel3 productLevel3 = new ProductLevel3();
    ProductItemLevel3 productItemLevel3 = new ProductItemLevel3();
    TreeMap<String, String> itemAttributesMap = new TreeMap<>();
    itemAttributesMap.put(CATEGORY_CODE, ATTRIBUTE_VALUE_1);
    productItemLevel3.setItemAttributesMap(itemAttributesMap);
    productLevel3.setNewlyAddedItems(Arrays.asList(productItemLevel3, productItemLevel3));
    productLevel3.setProductName(PRODUCT_NAME);
    productLevel3.setProductSku(PRODUCT_SKU);
    productLevel3.setAttributes(List.of(productLevel3Attribute));
    ProductL3Response productL3Response = new ProductL3Response();
    ProductAttributeDTO productAttributeDTO = new ProductAttributeDTO();
    ProductAttributeDetailDTO productAttributeDetailDTO = new ProductAttributeDetailDTO();
    productAttributeDetailDTO.setAttributeCode(CATEGORY_CODE);
    productAttributeDetailDTO.setAttributeValue(ATTRIBUTE_VALUE);
    productAttributeDTO.setProductAttributeDetails(Collections.singletonList(productAttributeDetailDTO));
    productL3Response.setDefiningAttributes(Collections.singletonList(productAttributeDTO));
    Assertions.assertEquals(ApiErrorCode.INVALID_ADD_DELETE_DUPLICATE_REQUEST,
        CommonUtils.validateAddDeleteVariantsRequest(productLevel3, productL3Response, true, businessPartner, categoryDetailResponse,
            false, existingDefiningAttributeDetails, false, true, false));
  }

  @Test
  public void validateAddDeleteVariantsRequestWarehouseSellerProductAttributeEmptyListTest() {
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setInventoryFulfillment(WAREHOUSE_SELLER);
    businessPartner.setCompany(companyDTO);
    ProductLevel3Attribute productLevel3Attribute = new ProductLevel3Attribute();
    productLevel3Attribute.setAttributeCode(ATTRIBUTE_CODE);
    ProductLevel3 productLevel3 = new ProductLevel3();
    ProductItemLevel3 productItemLevel3 = new ProductItemLevel3();
    TreeMap<String, String> itemAttributesMap = new TreeMap<>();
    itemAttributesMap.put(CATEGORY_CODE, ATTRIBUTE_VALUE_1);
    productItemLevel3.setItemAttributesMap(itemAttributesMap);
    productLevel3.setNewlyAddedItems(Arrays.asList(productItemLevel3, productItemLevel3));
    productLevel3.setProductName(PRODUCT_NAME);
    productLevel3.setProductSku(PRODUCT_SKU);
    productLevel3.setAttributes(List.of(productLevel3Attribute));
    ProductL3Response productL3Response = new ProductL3Response();
    ProductAttributeDTO productAttributeDTO = new ProductAttributeDTO();
    ProductAttributeDetailDTO productAttributeDetailDTO = new ProductAttributeDetailDTO();
    productAttributeDetailDTO.setAttributeCode(CATEGORY_CODE);
    productAttributeDetailDTO.setAttributeValue(ATTRIBUTE_VALUE);
    productAttributeDTO.setProductAttributeDetails(Collections.singletonList(productAttributeDetailDTO));
    productL3Response.setDefiningAttributes(Collections.singletonList(productAttributeDTO));
    CommonUtils.validateAddDeleteVariantsRequest(productLevel3, productL3Response, true, businessPartner, categoryDetailResponse,
        false, existingDefiningAttributeDetails, false, true, false);
  }


  @Test
  public void validateAddDeleteVariantsRequestWarehouseSellerProductAttributeNullTest() {
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setInventoryFulfillment(WAREHOUSE_SELLER);
    businessPartner.setCompany(companyDTO);
    List<String> attributeValues = new ArrayList<>();
    attributeValues.add(ATTRIBUTE_VALUE3);
    ProductLevel3Attribute productLevel3Attribute = new ProductLevel3Attribute();
    productLevel3Attribute.setAttributeCode(ATTRIBUTE_CODE);
    productLevel3Attribute.setVariantCreation(true);
    productLevel3Attribute.setValues(attributeValues);
    ProductLevel3 productLevel3 = new ProductLevel3();
    ProductItemLevel3 productItemLevel3 = new ProductItemLevel3();
    TreeMap<String, String> itemAttributesMap = new TreeMap<>();
    itemAttributesMap.put(CATEGORY_CODE, ATTRIBUTE_VALUE_1);
    productItemLevel3.setItemAttributesMap(itemAttributesMap);
    productLevel3.setNewlyAddedItems(Arrays.asList(productItemLevel3, productItemLevel3));
    productLevel3.setProductName(PRODUCT_NAME);
    productLevel3.setProductSku(PRODUCT_SKU);
    productLevel3.setAttributes(List.of(productLevel3Attribute));
    ProductL3Response productL3Response = new ProductL3Response();
    ProductAttributeDTO productAttributeDTO = new ProductAttributeDTO();
    ProductAttributeDetailDTO productAttributeDetailDTO = new ProductAttributeDetailDTO();
    productAttributeDetailDTO.setAttributeCode(ATTRIBUTE_CODE);
    productAttributeDetailDTO.setAttributeValue(ATTRIBUTE_VALUE);
    productAttributeDTO.setProductAttributeDetails(Collections.singletonList(productAttributeDetailDTO));
    productL3Response.setDefiningAttributes(new ArrayList<>());
    Assertions.assertEquals(ApiErrorCode.INVALID_ADD_DELETE_DUPLICATE_REQUEST,
        CommonUtils.validateAddDeleteVariantsRequest(productLevel3, productL3Response, true, businessPartner, categoryDetailResponse,
            false, existingDefiningAttributeDetails, false, true, false));
  }

  @Test
  public void validateAddDeleteVariantsRequestApiErrorCodeNullTest() {
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setInventoryFulfillment("BL");
    businessPartner.setCompany(companyDTO);
    ProductLevel3 productLevel3 = new ProductLevel3();
    ProductItemLevel3 productItemLevel3 = new ProductItemLevel3();
    List<String> attributeValues = new ArrayList<>();
    attributeValues.add(ATTRIBUTE_VALUE);
    ProductLevel3Attribute productLevel3Attribute = new ProductLevel3Attribute();
    productLevel3Attribute.setAttributeCode(CATEGORY_CODE);
    productLevel3Attribute.setValues(attributeValues);
    productLevel3.setNewlyAddedItems(Collections.singletonList(productItemLevel3));
    productLevel3.setProductName(PRODUCT_NAME);
    productLevel3.setProductSku(PRODUCT_SKU);
    productLevel3.setAttributes(null);
    ProductL3Response productL3Response = new ProductL3Response();
    ProductAttributeDTO productAttributeDTO = new ProductAttributeDTO();
    ProductAttributeDetailDTO productAttributeDetailDTO = new ProductAttributeDetailDTO();
    productAttributeDetailDTO.setAttributeCode(ATTRIBUTE_CODE);
    productAttributeDetailDTO.setAttributeValue(ATTRIBUTE_VALUE);
    productAttributeDTO.setProductAttributeDetails(Collections.singletonList(productAttributeDetailDTO));
    productL3Response.setDefiningAttributes(Collections.singletonList(productAttributeDTO));
    CommonUtils.validateAddDeleteVariantsRequest(productLevel3, productL3Response, true, businessPartner, categoryDetailResponse,
        false, existingDefiningAttributeDetails, false, true, false);
  }


  @Test
  public void validateAddDeleteVariantsRequestDuplicateValueTest() {
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setInventoryFulfillment(WAREHOUSE_SELLER);
    businessPartner.setCompany(companyDTO);
    List<String> attributeValues = new ArrayList<>();
    attributeValues.add(ATTRIBUTE_VALUE);
    ProductLevel3Attribute productLevel3Attribute = new ProductLevel3Attribute();
    productLevel3Attribute.setAttributeCode(CATEGORY_CODE);
    productLevel3Attribute.setValues(attributeValues);
    productLevel3Attribute.setAttributeType(AttributeType.DEFINING_ATTRIBUTE.name());
    ProductLevel3Attribute productLevel3Attribute2 = new ProductLevel3Attribute();
    productLevel3Attribute2.setAttributeCode(CATEGORY_CODE);
    productLevel3Attribute2.setValues(attributeValues);
    productLevel3Attribute2.setAttributeType(AttributeType.DEFINING_ATTRIBUTE.name());
    ProductLevel3 productLevel3 = new ProductLevel3();
    ProductItemLevel3 productItemLevel3 = new ProductItemLevel3();
    TreeMap<String, String> itemAttributesMap = new TreeMap<>();
    itemAttributesMap.put(CATEGORY_CODE, ATTRIBUTE_VALUE_1);
    productItemLevel3.setItemAttributesMap(itemAttributesMap);
    productLevel3.setNewlyAddedItems(Arrays.asList(productItemLevel3, productItemLevel3));
    productLevel3.setProductName(PRODUCT_NAME);
    productLevel3.setProductSku(PRODUCT_SKU);
    productLevel3.setAttributes(List.of(productLevel3Attribute, productLevel3Attribute2));
    ProductL3Response productL3Response = new ProductL3Response();
    ProductAttributeDTO productAttributeDTO = new ProductAttributeDTO();
    ProductAttributeDetailDTO productAttributeDetailDTO = new ProductAttributeDetailDTO();
    productAttributeDetailDTO.setAttributeCode(CATEGORY_CODE);
    productAttributeDetailDTO.setAttributeValue(ATTRIBUTE_VALUE);
    productAttributeDTO.setProductAttributeDetails(Collections.singletonList(productAttributeDetailDTO));
    productL3Response.setDefiningAttributes(Collections.singletonList(productAttributeDTO));
    existingDefiningAttributeDetails.setValueTypeAdditionForDefiningAttributes(true);
    ApiErrorCode apiErrorCode = CommonUtils.validateAddDeleteVariantsRequest(productLevel3, productL3Response, true, businessPartner, categoryDetailResponse,
        false, existingDefiningAttributeDetails, false, true, false);
    Assertions.assertEquals(ApiErrorCode.DUPLICATE_ATTRIBUTE_VALUE_NOT_ALLOWED, apiErrorCode);
  }

  @Test
  public void validateAddDeleteVariantsRequestWarehouseSellerProductAttributeAddRequestTest() {
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setInventoryFulfillment(WAREHOUSE_SELLER);
    businessPartner.setCompany(companyDTO);
    List<String> attributeValues = new ArrayList<>();
    attributeValues.add(ATTRIBUTE_VALUE);
    ProductLevel3Attribute productLevel3Attribute = new ProductLevel3Attribute();
    productLevel3Attribute.setAttributeCode(CATEGORY_CODE);
    productLevel3Attribute.setValues(attributeValues);
    ProductLevel3 productLevel3 = new ProductLevel3();
    ProductItemLevel3 productItemLevel3 = new ProductItemLevel3();
    TreeMap<String, String> itemAttributesMap = new TreeMap<>();
    itemAttributesMap.put(CATEGORY_CODE, ATTRIBUTE_VALUE_1);
    productItemLevel3.setItemAttributesMap(itemAttributesMap);
    productLevel3.setNewlyAddedItems(Arrays.asList(productItemLevel3, productItemLevel3));
    productLevel3.setProductName(PRODUCT_NAME);
    productLevel3.setProductSku(PRODUCT_SKU);
    productLevel3.setAttributes(List.of(productLevel3Attribute));
    ProductL3Response productL3Response = new ProductL3Response();
    ProductAttributeDTO productAttributeDTO = new ProductAttributeDTO();
    ProductAttributeDetailDTO productAttributeDetailDTO = new ProductAttributeDetailDTO();
    productAttributeDetailDTO.setAttributeCode(CATEGORY_CODE);
    productAttributeDetailDTO.setAttributeValue(ATTRIBUTE_VALUE);
    productAttributeDTO.setProductAttributeDetails(Collections.singletonList(productAttributeDetailDTO));
    productL3Response.setDefiningAttributes(Collections.singletonList(productAttributeDTO));
    CommonUtils.validateAddDeleteVariantsRequest(productLevel3, productL3Response, true, businessPartner, categoryDetailResponse,
        false, existingDefiningAttributeDetails, false, true, false);
  }

  @Test
  public void validateAddDeleteVariantsRequestWarehouseLastAttributeValueTest() {
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setInventoryFulfillment(WAREHOUSE_SELLER);
    businessPartner.setCompany(companyDTO);
    List<String> attributeValues = new ArrayList<>();
    attributeValues.add(ATTRIBUTE_VALUE);
    ProductLevel3Attribute productLevel3Attribute = new ProductLevel3Attribute();
    productLevel3Attribute.setAttributeCode(CATEGORY_CODE);
    productLevel3Attribute.setValues(attributeValues);
    ProductLevel3 productLevel3 = new ProductLevel3();
    ProductItemLevel3 productItemLevel3 = new ProductItemLevel3();
    TreeMap<String, String> itemAttributesMap = new TreeMap<>();
    itemAttributesMap.put(CATEGORY_CODE, ATTRIBUTE_VALUE_1);
    productItemLevel3.setItemAttributesMap(itemAttributesMap);
    productLevel3.setNewlyAddedItems(Arrays.asList(productItemLevel3, productItemLevel3));
    productLevel3.setProductName(PRODUCT_NAME);
    productLevel3.setProductSku(PRODUCT_SKU);
    productLevel3.setAttributes(List.of(productLevel3Attribute));
    ProductL3Response productL3Response = new ProductL3Response();
    ProductAttributeDTO productAttributeDTO = new ProductAttributeDTO();
    ProductAttributeDetailDTO productAttributeDetailDTO = new ProductAttributeDetailDTO();
    productAttributeDetailDTO.setAttributeCode(CATEGORY_CODE);
    productAttributeDetailDTO.setAttributeValue(ATTRIBUTE_VALUE);
    productAttributeDTO.setProductAttributeDetails(Collections.singletonList(productAttributeDetailDTO));
    productLevel3.setDeletedItems(Collections.singletonList(ATTRIBUTE_CODE));
    productLevel3.setNewlyAddedItems(new ArrayList<>());
    productL3Response.setDefiningAttributes(Collections.singletonList(productAttributeDTO));
    Assertions.assertEquals(ApiErrorCode.DELETE_NOT_ALLOWED_FOR_LAST_ATTRIBUTE_VALUE,
        CommonUtils.validateAddDeleteVariantsRequest(productLevel3, productL3Response, true, businessPartner, categoryDetailResponse,
            false, existingDefiningAttributeDetails, false, true, false));
  }

  @Test
  public void validateAddDeleteVariantsRequestWarehouseLastAttributeValueFalseTest() {
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setInventoryFulfillment(WAREHOUSE_SELLER);
    businessPartner.setCompany(companyDTO);
    List<String> attributeValues = new ArrayList<>();
    attributeValues.add(ATTRIBUTE_VALUE);
    ProductLevel3Attribute productLevel3Attribute = new ProductLevel3Attribute();
    productLevel3Attribute.setAttributeCode(CATEGORY_CODE);
    productLevel3Attribute.setValues(attributeValues);
    ProductLevel3 productLevel3 = new ProductLevel3();
    ProductItemLevel3 productItemLevel3 = new ProductItemLevel3();
    TreeMap<String, String> itemAttributesMap = new TreeMap<>();
    itemAttributesMap.put(CATEGORY_CODE, ATTRIBUTE_VALUE_1);
    productItemLevel3.setItemAttributesMap(itemAttributesMap);
    productLevel3.setNewlyAddedItems(Arrays.asList(productItemLevel3, productItemLevel3));
    productLevel3.setProductName(PRODUCT_NAME);
    productLevel3.setProductSku(PRODUCT_SKU);
    productLevel3.setAttributes(List.of(productLevel3Attribute));
    ProductL3Response productL3Response = new ProductL3Response();
    ProductAttributeDTO productAttributeDTO = new ProductAttributeDTO();
    ProductAttributeDetailDTO productAttributeDetailDTO = new ProductAttributeDetailDTO();
    productAttributeDetailDTO.setAttributeCode(CATEGORY_CODE);
    productAttributeDetailDTO.setAttributeValue(ATTRIBUTE_VALUE);
    productAttributeDTO.setProductAttributeDetails(Collections.singletonList(productAttributeDetailDTO));
    productLevel3.setDeletedItems(Collections.singletonList(ATTRIBUTE_CODE));
    productLevel3.setNewlyAddedItems(new ArrayList<>());
    productL3Response.setDefiningAttributes(new ArrayList<>());
    CommonUtils.validateAddDeleteVariantsRequest(productLevel3, productL3Response, true, businessPartner, categoryDetailResponse,
        false, existingDefiningAttributeDetails, true, true, false);
  }

  @Test
  public void validateAddDeleteVariantsRequestDeleteAllItemsTest() {
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setInventoryFulfillment(WAREHOUSE_SELLER);
    businessPartner.setCompany(companyDTO);
    List<String> attributeValues = new ArrayList<>();
    attributeValues.add(ATTRIBUTE_VALUE);
    ProductLevel3Attribute productLevel3Attribute = new ProductLevel3Attribute();
    productLevel3Attribute.setAttributeCode(CATEGORY_CODE);
    productLevel3Attribute.setValues(attributeValues);
    ProductLevel3 productLevel3 = new ProductLevel3();
    ProductItemLevel3 productItemLevel3 = new ProductItemLevel3();
    TreeMap<String, String> itemAttributesMap = new TreeMap<>();
    itemAttributesMap.put(CATEGORY_CODE, ATTRIBUTE_VALUE_1);
    productItemLevel3.setItemAttributesMap(itemAttributesMap);
    productLevel3.setNewlyAddedItems(Arrays.asList(productItemLevel3, productItemLevel3));
    productLevel3.setProductName(PRODUCT_NAME);
    productLevel3.setProductSku(PRODUCT_SKU);
    productLevel3.setAttributes(List.of(productLevel3Attribute));
    ProductL3Response productL3Response = new ProductL3Response();
    productL3Response.setItemCount(10);
    ProductAttributeDTO productAttributeDTO = new ProductAttributeDTO();
    ProductAttributeDetailDTO productAttributeDetailDTO = new ProductAttributeDetailDTO();
    productAttributeDetailDTO.setAttributeCode(CATEGORY_CODE);
    productAttributeDetailDTO.setAttributeValue(ATTRIBUTE_VALUE);
    productAttributeDTO.setProductAttributeDetails(Collections.singletonList(productAttributeDetailDTO));
    productLevel3.setDeletedItems(Collections.singletonList(ATTRIBUTE_CODE));
    productLevel3.setNewlyAddedItems(new ArrayList<>());
    productL3Response.setDefiningAttributes(new ArrayList<>());
    CommonUtils.validateAddDeleteVariantsRequest(productLevel3, productL3Response, true, businessPartner,
        categoryDetailResponse, false, existingDefiningAttributeDetails, true, true, false);
  }

  @Test
  public void validateAddDeleteVariantsRequestDeleteAllItemsNoDeleteTest() {
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setInventoryFulfillment(WAREHOUSE_SELLER);
    businessPartner.setCompany(companyDTO);
    List<String> attributeValues = new ArrayList<>();
    attributeValues.add(ATTRIBUTE_VALUE);
    ProductLevel3Attribute productLevel3Attribute = new ProductLevel3Attribute();
    productLevel3Attribute.setAttributeCode(CATEGORY_CODE);
    productLevel3Attribute.setValues(attributeValues);
    ProductLevel3 productLevel3 = new ProductLevel3();
    ProductItemLevel3 productItemLevel3 = new ProductItemLevel3();
    TreeMap<String, String> itemAttributesMap = new TreeMap<>();
    itemAttributesMap.put(CATEGORY_CODE, ATTRIBUTE_VALUE_1);
    productItemLevel3.setItemAttributesMap(itemAttributesMap);
    productLevel3.setNewlyAddedItems(Arrays.asList(productItemLevel3, productItemLevel3));
    productLevel3.setProductName(PRODUCT_NAME);
    productLevel3.setProductSku(PRODUCT_SKU);
    productLevel3.setAttributes(List.of(productLevel3Attribute));
    ProductL3Response productL3Response = new ProductL3Response();
    productL3Response.setItemCount(10);
    ProductAttributeDTO productAttributeDTO = new ProductAttributeDTO();
    ProductAttributeDetailDTO productAttributeDetailDTO = new ProductAttributeDetailDTO();
    productAttributeDetailDTO.setAttributeCode(CATEGORY_CODE);
    productAttributeDetailDTO.setAttributeValue(ATTRIBUTE_VALUE);
    productAttributeDTO.setProductAttributeDetails(Collections.singletonList(productAttributeDetailDTO));
    productLevel3.setDeletedItems(new ArrayList<>());
    productLevel3.setNewlyAddedItems(new ArrayList<>());
    productL3Response.setDefiningAttributes(new ArrayList<>());
    CommonUtils.validateAddDeleteVariantsRequest(productLevel3, productL3Response, true, businessPartner,
        categoryDetailResponse, false, existingDefiningAttributeDetails, true, true, false);
  }

  @Test
  public void validateAddDeleteVariantsRequestWarehouseNewAddedItemsEmptyTest() {
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setInventoryFulfillment(WAREHOUSE_SELLER);
    businessPartner.setCompany(companyDTO);
    List<String> attributeValues = new ArrayList<>();
    attributeValues.add(ATTRIBUTE_VALUE);
    ProductLevel3Attribute productLevel3Attribute = new ProductLevel3Attribute();
    productLevel3Attribute.setAttributeCode(CATEGORY_CODE);
    productLevel3Attribute.setValues(attributeValues);
    ProductLevel3 productLevel3 = new ProductLevel3();
    ProductItemLevel3 productItemLevel3 = new ProductItemLevel3();
    TreeMap<String, String> itemAttributesMap = new TreeMap<>();
    itemAttributesMap.put(CATEGORY_CODE, ATTRIBUTE_VALUE_1);
    productItemLevel3.setItemAttributesMap(itemAttributesMap);
    productLevel3.setNewlyAddedItems(Arrays.asList(productItemLevel3, productItemLevel3));
    productLevel3.setProductName(PRODUCT_NAME);
    productLevel3.setProductSku(PRODUCT_SKU);
    productLevel3.setAttributes(List.of(productLevel3Attribute));
    ProductL3Response productL3Response = new ProductL3Response();
    ProductAttributeDTO productAttributeDTO = new ProductAttributeDTO();
    ProductAttributeDetailDTO productAttributeDetailDTO = new ProductAttributeDetailDTO();
    productAttributeDetailDTO.setAttributeCode(CATEGORY_CODE);
    productAttributeDetailDTO.setAttributeValue(ATTRIBUTE_VALUE);
    productAttributeDTO.setProductAttributeDetails(Collections.singletonList(productAttributeDetailDTO));
    productLevel3.setDeletedItems(Collections.singletonList(ATTRIBUTE_CODE));
    productLevel3.setNewlyAddedItems(Collections.singletonList(productItemLevel3));
    productL3Response.setDefiningAttributes(new ArrayList<>());
    CommonUtils.validateAddDeleteVariantsRequest(productLevel3, productL3Response, true, businessPartner, null, true, existingDefiningAttributeDetails,
        false, true, false);
  }

  @Test
  public void validateAddDeleteVariantsRequestWarehouseCategoryDetailedResponseTest() {
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setInventoryFulfillment(WAREHOUSE_SELLER);
    businessPartner.setCompany(companyDTO);
    List<String> attributeValues = new ArrayList<>();
    CategoryDetailResponse response = new CategoryDetailResponse();
    CategoryAttributeResponse categoryAttributeResponse = new CategoryAttributeResponse();
    CategoryAttributeResponse categoryAttributeResponse2 = new CategoryAttributeResponse();
    List<CategoryAttributeResponse> categoryAttributeResponses = new ArrayList<>();
    AttributeResponse attributeResponse = new AttributeResponse();
    AttributeResponse attributeResponse2 = new AttributeResponse();
    attributeResponse.setAttributeCode(ATTRIBUTE_CODE);
    attributeResponse2.setAttributeCode(ATTRIBUTE_CODE_2);
    categoryAttributeResponse.setAttribute(attributeResponse);
    categoryAttributeResponse2.setAttribute(attributeResponse2);
    categoryAttributeResponses.add(categoryAttributeResponse);
    categoryAttributeResponses.add(categoryAttributeResponse2);
    response.setCategoryAttributes(categoryAttributeResponses);
    attributeValues.add(ATTRIBUTE_VALUE);
    ProductLevel3Attribute productLevel3Attribute = new ProductLevel3Attribute();
    productLevel3Attribute.setAttributeCode(ATTRIBUTE_CODE);
    productLevel3Attribute.setValues(attributeValues);
    ProductLevel3 productLevel3 = new ProductLevel3();
    ProductItemLevel3 productItemLevel3 = new ProductItemLevel3();
    TreeMap<String, String> itemAttributesMap = new TreeMap<>();
    itemAttributesMap.put(ATTRIBUTE_CODE, ATTRIBUTE_VALUE_1);
    productItemLevel3.setItemAttributesMap(itemAttributesMap);
    productLevel3.setNewlyAddedItems(Arrays.asList(productItemLevel3, productItemLevel3));
    productLevel3.setProductName(PRODUCT_NAME);
    productLevel3.setProductSku(PRODUCT_SKU);
    productLevel3.setAttributes(List.of(productLevel3Attribute));
    ProductL3Response productL3Response = new ProductL3Response();
    ProductAttributeDTO productAttributeDTO = new ProductAttributeDTO();
    ProductAttributeDetailDTO productAttributeDetailDTO = new ProductAttributeDetailDTO();
    productAttributeDetailDTO.setAttributeCode(ATTRIBUTE_CODE_2);
    productAttributeDetailDTO.setAttributeValue(ATTRIBUTE_VALUE);
    productAttributeDTO.setProductAttributeDetails(Collections.singletonList(productAttributeDetailDTO));
    productLevel3.setDeletedItems(Collections.singletonList(ATTRIBUTE_CODE));
    productLevel3.setNewlyAddedItems(Collections.singletonList(productItemLevel3));
    productL3Response.setDefiningAttributes(Collections.singletonList(productAttributeDTO));
    CommonUtils.validateAddDeleteVariantsRequest(productLevel3, productL3Response, true, businessPartner,
        response, true, existingDefiningAttributeDetails, false, true, false);
  }

  @Test
  public void validateAddDeleteVariantsRequestWarehouseCategoryDetailedResponseWithDifferentAttributeTest() {
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setInventoryFulfillment(WAREHOUSE_SELLER);
    businessPartner.setCompany(companyDTO);
    List<String> attributeValues = new ArrayList<>();
    CategoryDetailResponse response = new CategoryDetailResponse();
    CategoryAttributeResponse categoryAttributeResponse = new CategoryAttributeResponse();
    CategoryAttributeResponse categoryAttributeResponse2 = new CategoryAttributeResponse();
    List<CategoryAttributeResponse> categoryAttributeResponses = new ArrayList<>();
    AttributeResponse attributeResponse = new AttributeResponse();
    AttributeResponse attributeResponse2 = new AttributeResponse();
    attributeResponse.setAttributeCode(ATTRIBUTE_CODE);
    attributeResponse2.setAttributeCode(ATTRIBUTE_CODE_2);
    categoryAttributeResponse.setAttribute(attributeResponse);
    categoryAttributeResponse2.setAttribute(attributeResponse2);
    categoryAttributeResponses.add(categoryAttributeResponse);
    categoryAttributeResponses.add(categoryAttributeResponse2);
    response.setCategoryAttributes(categoryAttributeResponses);
    attributeValues.add(ATTRIBUTE_VALUE);
    ProductLevel3Attribute productLevel3Attribute = new ProductLevel3Attribute();
    productLevel3Attribute.setAttributeCode(ATTRIBUTE_CODE);
    productLevel3Attribute.setVariantCreation(true);
    productLevel3Attribute.setValues(attributeValues);
    ProductLevel3 productLevel3 = new ProductLevel3();
    ProductItemLevel3 productItemLevel3 = new ProductItemLevel3();
    TreeMap<String, String> itemAttributesMap = new TreeMap<>();
    itemAttributesMap.put(ATTRIBUTE_CODE, ATTRIBUTE_VALUE_1);
    productItemLevel3.setItemAttributesMap(itemAttributesMap);
    productLevel3.setNewlyAddedItems(Arrays.asList(productItemLevel3, productItemLevel3));
    productLevel3.setProductName(PRODUCT_NAME);
    productLevel3.setProductSku(PRODUCT_SKU);
    productLevel3.setAttributes(List.of(productLevel3Attribute));
    ProductL3Response productL3Response = new ProductL3Response();
    ProductAttributeDTO productAttributeDTO = new ProductAttributeDTO();
    ProductAttributeDetailDTO productAttributeDetailDTO = new ProductAttributeDetailDTO();
    productAttributeDetailDTO.setAttributeCode(ATTRIBUTE_CODE_2);
    productAttributeDetailDTO.setAttributeValue(ATTRIBUTE_VALUE);
    productAttributeDTO.setProductAttributeDetails(Collections.singletonList(productAttributeDetailDTO));
    productLevel3.setDeletedItems(Collections.singletonList(ATTRIBUTE_CODE));
    productLevel3.setNewlyAddedItems(Collections.singletonList(productItemLevel3));
    productL3Response.setDefiningAttributes(Collections.singletonList(productAttributeDTO));
    CommonUtils.validateAddDeleteVariantsRequest(productLevel3, productL3Response, true, businessPartner,
        response, true, existingDefiningAttributeDetails, false, true, false);
  }

  @Test
  public void validateAddDeleteVariantsRequestWarehouseCategoryDetailedResponseInvalidAttributeTest() {
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setInventoryFulfillment(WAREHOUSE_SELLER);
    businessPartner.setCompany(companyDTO);
    List<String> attributeValues = new ArrayList<>();
    CategoryDetailResponse response = new CategoryDetailResponse();
    CategoryAttributeResponse categoryAttributeResponse = new CategoryAttributeResponse();
    List<CategoryAttributeResponse> categoryAttributeResponses = new ArrayList<>();
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setAttributeCode(ATTRIBUTE_CODE_2);
    categoryAttributeResponse.setAttribute(attributeResponse);
    categoryAttributeResponses.add(categoryAttributeResponse);
    response.setCategoryAttributes(categoryAttributeResponses);
    attributeValues.add(ATTRIBUTE_VALUE);
    ProductLevel3Attribute productLevel3Attribute = new ProductLevel3Attribute();
    productLevel3Attribute.setAttributeCode(ATTRIBUTE_CODE);
    productLevel3Attribute.setValues(attributeValues);
    ProductLevel3 productLevel3 = new ProductLevel3();
    ProductItemLevel3 productItemLevel3 = new ProductItemLevel3();
    TreeMap<String, String> itemAttributesMap = new TreeMap<>();
    itemAttributesMap.put(ATTRIBUTE_CODE, ATTRIBUTE_VALUE_1);
    productItemLevel3.setItemAttributesMap(itemAttributesMap);
    productLevel3.setNewlyAddedItems(Arrays.asList(productItemLevel3, productItemLevel3));
    productLevel3.setProductName(PRODUCT_NAME);
    productLevel3.setProductSku(PRODUCT_SKU);
    productLevel3.setAttributes(List.of(productLevel3Attribute));
    ProductL3Response productL3Response = new ProductL3Response();
    ProductAttributeDTO productAttributeDTO = new ProductAttributeDTO();
    ProductAttributeDetailDTO productAttributeDetailDTO = new ProductAttributeDetailDTO();
    productAttributeDetailDTO.setAttributeCode(ATTRIBUTE_CODE);
    productAttributeDetailDTO.setAttributeValue(ATTRIBUTE_VALUE);
    productAttributeDTO.setProductAttributeDetails(Collections.singletonList(productAttributeDetailDTO));
    productLevel3.setDeletedItems(Collections.singletonList(ATTRIBUTE_CODE));
    productLevel3.setNewlyAddedItems(Collections.singletonList(productItemLevel3));
    productL3Response.setDefiningAttributes(Collections.singletonList(productAttributeDTO));
    CommonUtils.validateAddDeleteVariantsRequest(productLevel3, productL3Response, true, businessPartner,
        response, true, existingDefiningAttributeDetails, true, true, false);
  }

  @Test
  public void validateAddDeleteVariantsRequestWarehouseCategoryDetailedResponseSwitchOffTest() {
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setInventoryFulfillment(WAREHOUSE_SELLER);
    businessPartner.setCompany(companyDTO);
    List<String> attributeValues = new ArrayList<>();
    CategoryDetailResponse response = new CategoryDetailResponse();
    CategoryAttributeResponse categoryAttributeResponse = new CategoryAttributeResponse();
    List<CategoryAttributeResponse> categoryAttributeResponses = new ArrayList<>();
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setAttributeCode(ATTRIBUTE_CODE_2);
    categoryAttributeResponse.setAttribute(attributeResponse);
    categoryAttributeResponses.add(categoryAttributeResponse);
    response.setCategoryAttributes(categoryAttributeResponses);
    attributeValues.add(ATTRIBUTE_VALUE);
    ProductLevel3Attribute productLevel3Attribute = new ProductLevel3Attribute();
    productLevel3Attribute.setAttributeCode(ATTRIBUTE_CODE);
    productLevel3Attribute.setValues(attributeValues);
    ProductLevel3 productLevel3 = new ProductLevel3();
    ProductItemLevel3 productItemLevel3 = new ProductItemLevel3();
    TreeMap<String, String> itemAttributesMap = new TreeMap<>();
    itemAttributesMap.put(ATTRIBUTE_CODE, ATTRIBUTE_VALUE_1);
    productItemLevel3.setItemAttributesMap(itemAttributesMap);
    productLevel3.setNewlyAddedItems(Arrays.asList(productItemLevel3, productItemLevel3));
    productLevel3.setProductName(PRODUCT_NAME);
    productLevel3.setProductSku(PRODUCT_SKU);
    productLevel3.setAttributes(List.of(productLevel3Attribute));
    ProductL3Response productL3Response = new ProductL3Response();
    ProductAttributeDTO productAttributeDTO = new ProductAttributeDTO();
    ProductAttributeDetailDTO productAttributeDetailDTO = new ProductAttributeDetailDTO();
    productAttributeDetailDTO.setAttributeCode(ATTRIBUTE_CODE);
    productAttributeDetailDTO.setAttributeValue(ATTRIBUTE_VALUE);
    productAttributeDTO.setProductAttributeDetails(Collections.singletonList(productAttributeDetailDTO));
    productLevel3.setDeletedItems(Collections.singletonList(ATTRIBUTE_CODE));
    productLevel3.setNewlyAddedItems(Collections.singletonList(productItemLevel3));
    productL3Response.setDefiningAttributes(Collections.singletonList(productAttributeDTO));
    CommonUtils.validateAddDeleteVariantsRequest(productLevel3, productL3Response, true, businessPartner,
        response, false, existingDefiningAttributeDetails, false, true, false);
  }

    @Test
  public void isEligibleForAutoRejectTest() {
    Assertions.assertFalse(CommonUtils.isEligibleForAutoReject(new EditProductResponse()));
    EditProductResponse editProductResponse = new EditProductResponse();
    editProductResponse.setAction(3);
    Assertions.assertFalse(CommonUtils.isEligibleForAutoReject(editProductResponse));
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setTrustedSeller(false);
    editProductResponse.setProfileResponse(profileResponse);
    Assertions.assertTrue(CommonUtils.isEligibleForAutoReject(editProductResponse));
    profileResponse.setTrustedSeller(true);
    Assertions.assertFalse(CommonUtils.isEligibleForAutoReject(editProductResponse));
  }

  @Test
  public void isEligibleForAutoNeedRevisionTest() {
    Assertions.assertFalse(CommonUtils.isEligibleForAutoNeedRevision(new EditProductResponse()));
    EditProductResponse editProductResponse = new EditProductResponse();
    editProductResponse.setAction(2);
    Assertions.assertFalse(CommonUtils.isEligibleForAutoNeedRevision(editProductResponse));
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setTrustedSeller(false);
    editProductResponse.setProfileResponse(profileResponse);
    Assertions.assertTrue(CommonUtils.isEligibleForAutoNeedRevision(editProductResponse));
    profileResponse.setTrustedSeller(true);
    Assertions.assertFalse(CommonUtils.isEligibleForAutoNeedRevision(editProductResponse));
  }

  @Test
  public void getUpdateHistoryFromAuditTrailRequestTest() {
    AuditTrailDto auditTrailDto = new AuditTrailDto();
    auditTrailDto.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    auditTrailDto.setName(PRODUCT_NAME);
    auditTrailDto.setActionKey(ACTIVITY);
    auditTrailDto.setPickupPointCode(PICKUP_POINT_CODE);
    auditTrailDto.setOnlineStatus(true);
    auditTrailDto.setAttributeName(ATTRIBUTE);
    auditTrailDto.setNewValue(ATTRIBUTE_VALUE_1);
    auditTrailDto.setOldValue(ATTRIBUTE_VALUE_2);
    auditTrailDto.setProductSku(PRODUCT_SKU);
    auditTrailDto.setGdnSku(GDN_SKU);
    auditTrailDto.setPickupPointCode(PICKUPPOINT_CODE);
    AuditTrailListRequest auditTrailListRequest = new AuditTrailListRequest();
    auditTrailListRequest.setRequestId(REQUEST_ID);
    auditTrailListRequest.setClientId(CLIENT_ID);
    auditTrailListRequest.setChangedBy(CHANGED_BY);
    auditTrailListRequest.setUpdateDirectly(true);
    auditTrailListRequest.setUpdateDirectlyToDB(true);
    auditTrailListRequest.setAuditTrailResponseList(Arrays.asList(auditTrailDto));
    List<UpdatedProductHistory> updatedProductHistoryList =
        CommonUtils.getUpdateHistoryFromAuditTrailRequest(auditTrailListRequest);
    Assertions.assertEquals(updatedProductHistoryList.get(0).getProductSku(), PRODUCT_SKU);
    Assertions.assertEquals(updatedProductHistoryList.get(0).getActivity(), ACTIVITY);
    Assertions.assertEquals(updatedProductHistoryList.get(0).getClientHost(), CLIENT_ID);
    Assertions.assertEquals(updatedProductHistoryList.get(0).getBusinessPartnerCode(), BUSINESS_PARTNER_CODE);
    Assertions.assertEquals(updatedProductHistoryList.get(0).getGdnSku(), GDN_SKU);
    Assertions.assertEquals(updatedProductHistoryList.get(0).getPickupPointCode(), PICKUP_POINT_CODE);
  }


  @Test
  public void getUpdateHistoryFromAuditTrailRequestTestwithAuditTrailDtoNull() {
    AuditTrailListRequest auditTrailListRequest = new AuditTrailListRequest();
    auditTrailListRequest.setRequestId(REQUEST_ID);
    auditTrailListRequest.setClientId(CLIENT_ID);
    auditTrailListRequest.setChangedBy(CHANGED_BY);
    auditTrailListRequest.setAuditTrailResponseList(null);
    List<UpdatedProductHistory> updatedProductHistoryList =
        CommonUtils.getUpdateHistoryFromAuditTrailRequest(auditTrailListRequest);
    Assertions.assertNotNull(updatedProductHistoryList);
  }

  @Test
  public void validateCreateRequestForFbbCount1AndMppWhEnabledTest(){

    boolean mppForWhEnabled = true;

    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setCncActivated(true);
    companyDTO.setMerchantType(Constants.CM_MERCHANT);

    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(companyDTO);
    profileResponse.setMultiDefaultAddressFlag(true);

    CommonUtils.validateCreateRequestForFbb(productCreationRequest, pickupPointResponses, profileResponse,
        MPP_ALLOWED_SELLERS, mppForWhEnabled);
  }

  @Test
  public void validateCreateRequestForFbbCount1AndMppWhDisabledTest(){

    boolean mppForWhEnabled = false;

    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setCncActivated(true);
    companyDTO.setMerchantType(Constants.CM_MERCHANT);

    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(companyDTO);
    profileResponse.setMultiDefaultAddressFlag(true);

    CommonUtils.validateCreateRequestForFbb(productCreationRequest, pickupPointResponses, profileResponse,
        MPP_ALLOWED_SELLERS, mppForWhEnabled);
  }

  @Test
  public void validateCreateRequestForFbbCountGreaterThan1AndMppWhDisabledTest(){

    boolean mppForWhEnabled = false;

    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setCncActivated(true);
    companyDTO.setMerchantType(Constants.CM_MERCHANT);

    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(companyDTO);
    profileResponse.setMultiDefaultAddressFlag(true);

    PickupPointResponse pickupPointResponse1 = new PickupPointResponse();
    pickupPointResponse1.setCode(PICKUP_POINT_CODE_1);
    pickupPointResponse1.setFbbActivated(true);

    pickupPointResponses.add(pickupPointResponse1);

    PickupPointCreateRequest pickupPointCreateRequest1 = new PickupPointCreateRequest();
    pickupPointCreateRequest1.setPickupPointId(PICKUP_POINT_CODE_1);

    pickupPoints.add(pickupPointCreateRequest1);

    ProductItemCreationRequest productItemCreationRequest = new ProductItemCreationRequest();
    productItemCreationRequest.setPickupPoints(pickupPoints);

    List<ProductItemCreationRequest> creationRequests = new ArrayList<>();
    creationRequests.add(productItemCreationRequest);

    productCreationRequest = new ProductCreationRequest();
    productCreationRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productCreationRequest.setProductItemRequests(creationRequests);

    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      CommonUtils.validateCreateRequestForFbb(productCreationRequest, pickupPointResponses, profileResponse,
          MPP_ALLOWED_SELLERS, mppForWhEnabled);
    });
  }

  @Test
  public void validateCreateRequestForFbbCountGreaterThan1AndMppWhEnabledTest(){

    boolean mppForWhEnabled = true;

    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setCncActivated(true);
    companyDTO.setMerchantType(Constants.CM_MERCHANT);

    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(companyDTO);
    profileResponse.setMultiDefaultAddressFlag(true);

    PickupPointResponse pickupPointResponse1 = new PickupPointResponse();
    pickupPointResponse1.setCode(PICKUP_POINT_CODE_1);
    pickupPointResponse1.setFbbActivated(true);

    pickupPointResponses.add(pickupPointResponse1);

    PickupPointCreateRequest pickupPointCreateRequest1 = new PickupPointCreateRequest();
    pickupPointCreateRequest1.setPickupPointId(PICKUP_POINT_CODE_1);

    pickupPoints.add(pickupPointCreateRequest1);

    ProductItemCreationRequest productItemCreationRequest = new ProductItemCreationRequest();
    productItemCreationRequest.setPickupPoints(pickupPoints);

    List<ProductItemCreationRequest> itemCreationRequests = new ArrayList<>();
    itemCreationRequests.add(productItemCreationRequest);

    productCreationRequest = new ProductCreationRequest();
    productCreationRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productCreationRequest.setProductItemRequests(itemCreationRequests);

    CommonUtils.validateCreateRequestForFbb(productCreationRequest, pickupPointResponses, profileResponse,
        MPP_ALLOWED_SELLERS, mppForWhEnabled);
  }

  @Test
  public void validateCreateRequestForFbbCountGreaterThan1AndMppWhEnabledAndNonMppSellerTest(){

    boolean mppForWhEnabled = true;

    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setCncActivated(false);
    companyDTO.setMerchantType("NON_MPP_SELLER");

    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(companyDTO);
    profileResponse.setMultiDefaultAddressFlag(true);

    PickupPointResponse pickupPointResponse1 = new PickupPointResponse();
    pickupPointResponse1.setCode(PICKUP_POINT_CODE_1);
    pickupPointResponse1.setFbbActivated(true);

    pickupPointResponses.add(pickupPointResponse1);

    PickupPointCreateRequest pickupPointCreateRequest1 = new PickupPointCreateRequest();
    pickupPointCreateRequest1.setPickupPointId(PICKUP_POINT_CODE_1);

    pickupPoints.add(pickupPointCreateRequest1);

    ProductItemCreationRequest productItemCreationRequest = new ProductItemCreationRequest();
    productItemCreationRequest.setPickupPoints(pickupPoints);

    List<ProductItemCreationRequest> itemCreationRequests = new ArrayList<>();
    itemCreationRequests.add(productItemCreationRequest);

    productCreationRequest = new ProductCreationRequest();
    productCreationRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productCreationRequest.setProductItemRequests(itemCreationRequests);

    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      CommonUtils.validateCreateRequestForFbb(productCreationRequest, pickupPointResponses, profileResponse,
          MPP_ALLOWED_SELLERS, mppForWhEnabled);
    });
  }

  @Test
  public void isItemNameEmptyForAnyNewlyAddedItemTest() {
    boolean result1 = CommonUtils.isItemNameEmptyForAnyNewlyAddedItem(null,
      false);
    Assertions.assertFalse(result1);
    boolean result2 = CommonUtils.isItemNameEmptyForAnyNewlyAddedItem(productL3UpdateRequest,
      false);
    Assertions.assertFalse(result2);
    productVariantPriceStockAndImagesRequest.setNewlyAddedItem(true);
    productVariantPriceStockAndImagesRequest.setItemName(ITEM_NAME);
    boolean result3 = CommonUtils.isItemNameEmptyForAnyNewlyAddedItem(productL3UpdateRequest,
      true);
    Assertions.assertTrue(result3);
    productVariantPriceStockAndImagesRequest.setItemName(null);
    boolean result4 = CommonUtils.isItemNameEmptyForAnyNewlyAddedItem(productL3UpdateRequest,
      true);
    Assertions.assertTrue(result4);
  }

  @Test
  public void getNewlyAddedAttributeCodesTest() {
    Map<String, String> attributeMapRequest = new HashMap<>();
    productL3UpdateRequest.getProductItems().get(0).getAttributesMap().put(ATTRIBUTE_CODE, ATTRIBUTE_VALUE);
    Set<String> result1 =
        CommonUtils.getNewlyAddedAttributeCodes(productL3UpdateRequest,attributeMapRequest, FAMILY_COLOR_CODE);
    attributeMapRequest.put(ATTRIBUTE_CODE, ATTRIBUTE_VALUE);
    Set<String> result2 =
        CommonUtils.getNewlyAddedAttributeCodes(productL3UpdateRequest,attributeMapRequest, FAMILY_COLOR_CODE);
    attributeMapRequest.remove(ATTRIBUTE_CODE);
    Set<String> result3 =
        CommonUtils.getNewlyAddedAttributeCodes(productL3UpdateRequest,attributeMapRequest, ATTRIBUTE_CODE);
    Assertions.assertNotNull(result3);
  }

  @Test
  public void isCombinedEditAndAutoRejectTest() {
    boolean result1 = CommonUtils.isCombinedEditAndAutoReject(false, true);
    Assertions.assertTrue(result1);
    boolean result2 = CommonUtils.isCombinedEditAndAutoReject(true, true);
    Assertions.assertFalse(result2);
    boolean result3 = CommonUtils.isCombinedEditAndAutoReject(false, false);
    Assertions.assertFalse(result3);
  }

  @Test
  public void getPickupPointCodesWithFbbActivatedValueTest() throws Exception {
    CommonUtils.getPickupPointCodesWithFbbActivatedValue(new ArrayList<>());
    CommonUtils.getPickupPointCodesWithFbbActivatedValue(Collections.singletonList(new PickupPointResponse()));
  }
  
  @Test
  public void isCncActiveTest(){
    ProductVariantUpdateRequest productVariantUpdateRequest = new ProductVariantUpdateRequest();
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setCncActive(true);
    ItemPickupPointRequest itemPickupPointRequest1 = new ItemPickupPointRequest();
    itemPickupPointRequest1.setCncActive(false);
    List<ItemPickupPointRequest> itemPickupPointRequests = new ArrayList<>();
    itemPickupPointRequests.add(itemPickupPointRequest);
    itemPickupPointRequests.add(itemPickupPointRequest1);
    productVariantUpdateRequest.setAddPickupPoints(itemPickupPointRequests);
    boolean cncActive = false;
    cncActive = CommonUtils.isCncActive(productVariantUpdateRequest,cncActive, false);
    Assertions.assertEquals(true,cncActive);
  }

  @Test
  public void isCncActiveCncTruePPEmptyTest(){
    ProductVariantUpdateRequest productVariantUpdateRequest = new ProductVariantUpdateRequest();
    productVariantUpdateRequest.setAddPickupPoints(new ArrayList<>());
    boolean cncActive = false;
    boolean result = CommonUtils.isCncActive(productVariantUpdateRequest,cncActive, false);
    Assertions.assertFalse(result);
  }

  @Test
  public void isCncActiveEmptyTest(){
    ProductVariantUpdateRequest productVariantUpdateRequest = new ProductVariantUpdateRequest();
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setCncActive(false);
    productVariantUpdateRequest.setAddPickupPoints(Collections.singletonList(itemPickupPointRequest));
    boolean cncActive = true;
    boolean result = CommonUtils.isCncActive(productVariantUpdateRequest,cncActive, false);
    Assertions.assertTrue(result);
  }

  @Test
  public void isCncActiveCncFalseEmptyTest(){
    ProductVariantUpdateRequest productVariantUpdateRequest = new ProductVariantUpdateRequest();
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setCncActive(false);
    productVariantUpdateRequest.setAddPickupPoints(Collections.singletonList(itemPickupPointRequest));
    boolean cncActive = false;
    boolean result = CommonUtils.isCncActive(productVariantUpdateRequest,cncActive, false);
    Assertions.assertFalse(result);
  }

  @Test
  public void isCncActive_cncSwitchOn_cncDisplayTest(){
    ProductVariantUpdateRequest productVariantUpdateRequest = new ProductVariantUpdateRequest();
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setCncDisplay(true);
    productVariantUpdateRequest.setAddPickupPoints(Collections.singletonList(itemPickupPointRequest));
    boolean cncActive = false;
    cncActive = CommonUtils.isCncActive(productVariantUpdateRequest,cncActive, true);
    Assertions.assertTrue(cncActive);
  }

  @Test
  public void isCncActive_cncSwitchOn_cncBuyableTest(){
    ProductVariantUpdateRequest productVariantUpdateRequest = new ProductVariantUpdateRequest();
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setCncBuyable(true);
    productVariantUpdateRequest.setAddPickupPoints(Collections.singletonList(itemPickupPointRequest));
    boolean cncActive = false;
    cncActive = CommonUtils.isCncActive(productVariantUpdateRequest,cncActive, true);
    Assertions.assertTrue(cncActive);
  }

  @Test
  public void isDefinitiveActionToBeSkippedTest() {
    boolean result = CommonUtils.isDefinitiveActionToBeSkipped(0);
    Assertions.assertTrue(result);
  }

  @Test
  public void isDefinitiveActionToBeSkippedRejectionActionTest() {
    boolean result = CommonUtils.isDefinitiveActionToBeSkipped(3);
    Assertions.assertTrue(result);
  }

  @Test
  public void isDefinitiveActionToBeSkippedNotEligibleActionTest() {
    boolean result = CommonUtils.isDefinitiveActionToBeSkipped(1);
    Assertions.assertFalse(result);
  }

  @Test
  public void testOverrideVariantNameForNewlyAddedVariants() {
    TreeMap<String, String> attributesMap = new TreeMap<>();
    attributesMap.put(ATTRIBUTE_CODE, ATTRIBUTE_VALUE_WARNA_1);
    attributesMap.put(ATTRIBUTE_CODE_2, ATTRIBUTE_VALUE_VARIASI);
    ProductL3Response productL3Response = new ProductL3Response();
    productL3UpdateRequest.setProductName(PRODUCT_NAME_2);
    Map<String, String> attributeIdAndValueMapFromPCB = new HashMap<>();
    attributeIdAndValueMapFromPCB.put(ATTRIBUTE_CODE, "id1");
    attributeIdAndValueMapFromPCB.put(ATTRIBUTE_CODE_2, "id2");
    productVariantPriceStockAndImagesRequest.setAttributesMap(attributesMap);
    CommonUtils.overrideVariantNameForNewlyAddedVariants(productL3UpdateRequest,
      productVariantPriceStockAndImagesRequest, attributeIdAndValueMapFromPCB, productL3Response);
    Assertions.assertEquals("iphone 15 green 64GB", productVariantPriceStockAndImagesRequest.getItemName());
  }

  @Test
  public void testOverrideVariantNameForNewlyAddedVariantsMissingValuesNoOverride() {
    TreeMap<String, String> attributesMap = new TreeMap<>();
    attributesMap.put(ATTRIBUTE_CODE, ATTRIBUTE_VALUE_WARNA_2);
    attributesMap.put(ATTRIBUTE_CODE_2, ATTRIBUTE_VALUE_VARIASI);
    ProductL3Response productL3Response = new ProductL3Response();
    productL3UpdateRequest.setProductName("");
    productVariantPriceStockAndImagesRequest.setItemName(
      PRODUCT_NAME_3.concat(" ").concat(ATTRIBUTE_VALUE_VARIASI).concat(" ")
        .concat(ATTRIBUTE_VALUE_WARNA_1));
    MasterDataProductDTO masterDataProduct = new MasterDataProductDTO();
    masterDataProduct.setProductName(PRODUCT_NAME);
    productL3Response.setMasterDataProduct(masterDataProduct);
    Map<String, String> attributeIdAndValueMapFromPCB = new HashMap<>();
    attributeIdAndValueMapFromPCB.put(ATTRIBUTE, "id1");
    attributeIdAndValueMapFromPCB.put(ATTRIBUTE_VALUE3, "id2");
    productVariantPriceStockAndImagesRequest.setAttributesMap(attributesMap);
    CommonUtils.overrideVariantNameForNewlyAddedVariants(productL3UpdateRequest,
      productVariantPriceStockAndImagesRequest, attributeIdAndValueMapFromPCB, productL3Response);
    Assertions.assertEquals("productName",
      productVariantPriceStockAndImagesRequest.getItemName());
  }

  @Test
  public void testOverrideVariantNameForNewlyAddedVariantsForEmptyNameInRequest() {
    TreeMap<String, String> attributesMap = new TreeMap<>();
    attributesMap.put(ATTRIBUTE_CODE, ATTRIBUTE_VALUE_WARNA_2);
    attributesMap.put(ATTRIBUTE_CODE_2, ATTRIBUTE_VALUE_VARIASI);
    ProductL3Response productL3Response = new ProductL3Response();
    productL3UpdateRequest.setProductName("");
    MasterDataProductDTO masterDataProduct = new MasterDataProductDTO();
    masterDataProduct.setProductName(PRODUCT_NAME_3);
    productL3Response.setMasterDataProduct(masterDataProduct);
    Map<String, String> attributeIdAndValueMapFromPCB = new HashMap<>();
    attributeIdAndValueMapFromPCB.put(ATTRIBUTE_CODE, "id1");
    attributeIdAndValueMapFromPCB.put(ATTRIBUTE_CODE_2, "id2");
    productVariantPriceStockAndImagesRequest.setAttributesMap(attributesMap);
    CommonUtils.overrideVariantNameForNewlyAddedVariants(productL3UpdateRequest,
      productVariantPriceStockAndImagesRequest, attributeIdAndValueMapFromPCB, productL3Response);
    Assertions.assertEquals("MacBook 15 red 64GB",
      productVariantPriceStockAndImagesRequest.getItemName());
  }

  @Test
  public void testOverrideVariantNameForNewlyAddedVariantsForEmptyName() {
    TreeMap<String, String> attributesMap = new TreeMap<>();
    attributesMap.put(ATTRIBUTE_CODE, ATTRIBUTE_VALUE_WARNA_2);
    attributesMap.put(ATTRIBUTE_CODE_2, ATTRIBUTE_VALUE_VARIASI);
    ProductL3Response productL3Response = new ProductL3Response();
    productL3UpdateRequest.setProductName("");
    MasterDataProductDTO masterDataProduct = new MasterDataProductDTO();
    masterDataProduct.setProductName("");
    productL3Response.setMasterDataProduct(masterDataProduct);
    Map<String, String> attributeIdAndValueMapFromPCB = new HashMap<>();
    attributeIdAndValueMapFromPCB.put(ATTRIBUTE_CODE, "id1");
    attributeIdAndValueMapFromPCB.put(ATTRIBUTE_CODE_2, "id2");
    productVariantPriceStockAndImagesRequest.setAttributesMap(attributesMap);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      CommonUtils.overrideVariantNameForNewlyAddedVariants(productL3UpdateRequest,
          productVariantPriceStockAndImagesRequest, attributeIdAndValueMapFromPCB, productL3Response);
    });
  }
  @Test
  public void isItemSkuValidSkuTest() {
    String sku = "HEH-12345-12345-12345";
    boolean result = CommonUtils.isItemSku(sku);
    Assertions.assertTrue(result);
  }

  @Test
  public void isItemSkuInvalidSkuTest() {
    String sku = "HEH-123-123-123-123-123";
    boolean result = CommonUtils.isItemSku(sku);
    Assertions.assertFalse(result);
  }

  @Test
  public void isItemSkuBlankTest(){
    String sku = "";
    boolean result = CommonUtils.isItemSku(sku);
    Assertions.assertFalse(result);
  }

  @Test
  public void isProductSkuValidSkuTest(){
    String sku = "HEH-12345-12345";
    boolean result = CommonUtils.isProductSku(sku);
    Assertions.assertTrue(result);
  }

  @Test
  public void isProductSkuInvalidSkuTest(){
    String sku = "HEH-12345-12345-123-123-123";
    boolean result = CommonUtils.isProductSku(sku);
    Assertions.assertFalse(result);
  }

  @Test
  public void isProductSkuBlankSkuTest(){
    String sku = "";
    boolean result = CommonUtils.isProductSku(sku);
    Assertions.assertFalse(result);
  }

  @Test
  public void testSetPriceForQuickEditRequestSet() {
    Set<PriceDTO> price = new HashSet<>();
    PriceDTO priceDataTransferObject = new PriceDTO();
    priceDataTransferObject.setListPrice(10.0);
    priceDataTransferObject.setOfferPrice(8.0);
    price.add(priceDataTransferObject);
    ProductLevel3PriceRequest oldProductLevel3PriceRequest = new ProductLevel3PriceRequest();
    oldProductLevel3PriceRequest.setPrice(11.00);
    oldProductLevel3PriceRequest.setSalePrice(6.00);
    QuickEditV2Request quickEditV2Request =
      QuickEditV2Request.builder().price(oldProductLevel3PriceRequest).build();

    // Call the method
    CommonUtils.setPriceForQuickEditRequestSet(price, quickEditV2Request);
    Assertions.assertEquals(Optional.of(10.0).get(), quickEditV2Request.getPrice().getPrice());
  }

  @Test
  public void testSetPriceForQuickEditRequestSetNotPresent() {
    Set<PriceDTO> price = new HashSet<>();
    ProductLevel3PriceRequest oldProductLevel3PriceRequest = new ProductLevel3PriceRequest();
    oldProductLevel3PriceRequest.setPrice(11.00);
    oldProductLevel3PriceRequest.setSalePrice(6.00);
    QuickEditV2Request quickEditV2Request =
      QuickEditV2Request.builder().price(oldProductLevel3PriceRequest).build();
    CommonUtils.setPriceForQuickEditRequestSet(price, quickEditV2Request);
    Assertions.assertEquals(Optional.of(11.0).get(), quickEditV2Request.getPrice().getPrice());
  }

  @Test
  public void sanitizeSellerSkuTest() {
    ProductLevel3QuickEditRequest productLevel3QuickEditRequest = new ProductLevel3QuickEditRequest();
    CommonUtils.sanitizeSellerSku(false,productLevel3QuickEditRequest);
    assertNull(productLevel3QuickEditRequest.getQuickEditRequests());
    CommonUtils.sanitizeSellerSku(true,productLevel3QuickEditRequest);
    assertNull(productLevel3QuickEditRequest.getQuickEditRequests());
    productLevel3QuickEditRequest.setQuickEditRequests(List.of(new QuickEditRequest()));
    CommonUtils.sanitizeSellerSku(true,productLevel3QuickEditRequest);
    assertNull(productLevel3QuickEditRequest.getQuickEditRequests().get(0).getSellerSku());
    productLevel3QuickEditRequest.getQuickEditRequests().get(0).setSellerSku(WAREHOUSE_SELLER);
    CommonUtils.sanitizeSellerSku(true,productLevel3QuickEditRequest);
    Assertions.assertNotNull(productLevel3QuickEditRequest.getQuickEditRequests().get(0).getSellerSku());
  }

  @Test
  public void testSetSchedulesForL5EditRequest(){
    Set<String> l5sWithActiveSchedules = new HashSet<>();
    l5sWithActiveSchedules.add(ITEM_SKU.concat(Constants.HYPHEN).concat(PICKUP_POINT_CODE));
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setBuyableSchedule(
      BuyableScheduleRequest.builder().endDateTime(new Date()).buyable(true).build());
    itemPickupPointRequest.setDiscoverableSchedule(
      DiscoverableScheduleRequest.builder().discoverable(true).endDateTime(new Date()).build());
    ItemPickupPointQuickEditRequest itemPickupPointQuickEditRequest =
      new ItemPickupPointQuickEditRequest();
    itemPickupPointQuickEditRequest.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointQuickEditRequest.setItemSku(ITEM_SKU);
    itemPickupPointQuickEditRequest.setStatus(ProductLevel3Status.B2B.name());
    Boolean scheduleRemovedForStatusUpdate =
      CommonUtils.setSchedulesForL5EditRequest(itemPickupPointRequest,
        itemPickupPointQuickEditRequest, l5sWithActiveSchedules, null, false);
    assertNull(itemPickupPointRequest.getBuyableSchedule());
    assertNull(itemPickupPointRequest.getDiscoverableSchedule());
    Assertions.assertTrue(scheduleRemovedForStatusUpdate);
  }

  @Test
  public void testSetSchedulesForL5EditRequestItemSkuEmpty(){
    Set<String> l5sWithActiveSchedules = new HashSet<>();
    l5sWithActiveSchedules.add(ITEM_SKU.concat(Constants.HYPHEN).concat(PICKUP_POINT_CODE));
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setBuyableSchedule(
        BuyableScheduleRequest.builder().endDateTime(new Date()).buyable(true).build());
    itemPickupPointRequest.setDiscoverableSchedule(
        DiscoverableScheduleRequest.builder().discoverable(true).endDateTime(new Date()).build());
    ItemPickupPointQuickEditRequest itemPickupPointQuickEditRequest =
        new ItemPickupPointQuickEditRequest();
    itemPickupPointQuickEditRequest.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointQuickEditRequest.setItemSku(null);
    itemPickupPointQuickEditRequest.setStatus(ProductLevel3Status.B2B.name());
    Boolean scheduleRemovedForStatusUpdate =
        CommonUtils.setSchedulesForL5EditRequest(itemPickupPointRequest,
            itemPickupPointQuickEditRequest, l5sWithActiveSchedules, null, false);
    Assertions.assertEquals(itemPickupPointQuickEditRequest.getBuyableSchedule().isBuyable(),
        itemPickupPointRequest.getBuyableSchedule().isBuyable());
    assertNull(scheduleRemovedForStatusUpdate);
  }

  @Test
  public void testSetSchedulesForL5EditOfflineRequest(){
    Set<String> l5sWithActiveSchedules = new HashSet<>();
    l5sWithActiveSchedules.add(ITEM_SKU.concat(Constants.HYPHEN).concat(PICKUP_POINT_CODE));
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setBuyableSchedule(
      BuyableScheduleRequest.builder().endDateTime(new Date()).buyable(true).build());
    itemPickupPointRequest.setDiscoverableSchedule(
      DiscoverableScheduleRequest.builder().discoverable(true).endDateTime(new Date()).build());
    ItemPickupPointQuickEditRequest itemPickupPointQuickEditRequest =
      new ItemPickupPointQuickEditRequest();
    itemPickupPointQuickEditRequest.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointQuickEditRequest.setItemSku(ITEM_SKU);
    itemPickupPointQuickEditRequest.setStatus(ProductLevel3Status.OFFLINE.name());
    Boolean scheduleRemovedForStatusUpdate =
      CommonUtils.setSchedulesForL5EditRequest(itemPickupPointRequest,
        itemPickupPointQuickEditRequest, l5sWithActiveSchedules, null, false);
    Assertions.assertEquals(itemPickupPointQuickEditRequest.getBuyableSchedule().isBuyable(),
      itemPickupPointRequest.getBuyableSchedule().isBuyable());
    assertNull(scheduleRemovedForStatusUpdate);
  }

  @Test
  public void testSetSchedulesForL5EditDeliveryOfflineCncOnlineRequest(){
    Set<String> l5sWithActiveSchedules = new HashSet<>();
    l5sWithActiveSchedules.add(ITEM_SKU.concat(Constants.HYPHEN).concat(PICKUP_POINT_CODE));
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setBuyableSchedule(
        BuyableScheduleRequest.builder().endDateTime(new Date()).buyable(true).build());
    itemPickupPointRequest.setDiscoverableSchedule(
        DiscoverableScheduleRequest.builder().discoverable(true).startDateTime(new Date(0)).endDateTime(new Date(0)).build());
    ItemPickupPointQuickEditRequest itemPickupPointQuickEditRequest =
        new ItemPickupPointQuickEditRequest();
    itemPickupPointQuickEditRequest.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointQuickEditRequest.setItemSku(ITEM_SKU);
    itemPickupPointQuickEditRequest.setStatus(ProductLevel3Status.OFFLINE.name());
    itemPickupPointQuickEditRequest.setCncStatus(ProductLevel3Status.ONLINE.name());
    Boolean scheduleRemovedForStatusUpdate =
        CommonUtils.setSchedulesForL5EditRequest(itemPickupPointRequest,
            itemPickupPointQuickEditRequest, l5sWithActiveSchedules, null, true);
    Assertions.assertEquals(itemPickupPointQuickEditRequest.getBuyableSchedule().getStartDateTime(),
        itemPickupPointRequest.getDiscoverableSchedule().getStartDateTime());
    Assertions.assertEquals(itemPickupPointQuickEditRequest.getBuyableSchedule().getEndDateTime(),
        itemPickupPointRequest.getDiscoverableSchedule().getEndDateTime());
    assertNull(scheduleRemovedForStatusUpdate);
  }

  @Test
  public void testSetSchedulesForL5EditDeliveryOfflineCncOnline_nullBuyableScheduleRequest(){
    Set<String> l5sWithActiveSchedules = new HashSet<>();
    l5sWithActiveSchedules.add(ITEM_SKU.concat(Constants.HYPHEN).concat(PICKUP_POINT_CODE));
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setDiscoverableSchedule(
        DiscoverableScheduleRequest.builder().discoverable(true).startDateTime(new Date(0)).endDateTime(new Date(0)).build());
    ItemPickupPointQuickEditRequest itemPickupPointQuickEditRequest =
        new ItemPickupPointQuickEditRequest();
    itemPickupPointQuickEditRequest.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointQuickEditRequest.setItemSku(ITEM_SKU);
    itemPickupPointQuickEditRequest.setStatus(ProductLevel3Status.OFFLINE.name());
    itemPickupPointQuickEditRequest.setCncStatus(ProductLevel3Status.ONLINE.name());
    Boolean scheduleRemovedForStatusUpdate =
        CommonUtils.setSchedulesForL5EditRequest(itemPickupPointRequest,
            itemPickupPointQuickEditRequest, l5sWithActiveSchedules, null, true);
    Assertions.assertEquals(itemPickupPointQuickEditRequest.getBuyableSchedule().getStartDateTime(),
        itemPickupPointRequest.getDiscoverableSchedule().getStartDateTime());
    Assertions.assertEquals(itemPickupPointQuickEditRequest.getBuyableSchedule().getEndDateTime(),
        itemPickupPointRequest.getDiscoverableSchedule().getEndDateTime());
    assertNull(scheduleRemovedForStatusUpdate);
  }

  @Test
  public void testSetSchedulesForL5EditDeliveryOfflineCncOnline_nullScheduleRequest(){
    Set<String> l5sWithActiveSchedules = new HashSet<>();
    l5sWithActiveSchedules.add(ITEM_SKU.concat(Constants.HYPHEN).concat(PICKUP_POINT_CODE));
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    ItemPickupPointQuickEditRequest itemPickupPointQuickEditRequest =
        new ItemPickupPointQuickEditRequest();
    itemPickupPointQuickEditRequest.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointQuickEditRequest.setItemSku(ITEM_SKU);
    itemPickupPointQuickEditRequest.setStatus(ProductLevel3Status.OFFLINE.name());
    itemPickupPointQuickEditRequest.setCncStatus(ProductLevel3Status.ONLINE.name());
    Boolean scheduleRemovedForStatusUpdate =
        CommonUtils.setSchedulesForL5EditRequest(itemPickupPointRequest,
            itemPickupPointQuickEditRequest, l5sWithActiveSchedules, null, true);
    assertNull(scheduleRemovedForStatusUpdate);
  }

  @Test
  public void testSetSchedulesForL5EditDeliveryOfflineCncOnline_schedulesRemovedRequest(){
    Set<String> l5sWithActiveSchedules = new HashSet<>();
    l5sWithActiveSchedules.add(ITEM_SKU.concat(Constants.HYPHEN).concat(PICKUP_POINT_CODE));
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setBuyableSchedule(
        BuyableScheduleRequest.builder().endDateTime(new Date()).buyable(true).build());
    itemPickupPointRequest.setDiscoverableSchedule(
        DiscoverableScheduleRequest.builder().discoverable(true).startDateTime(new Date(0)).endDateTime(new Date(0)).build());
    ItemPickupPointQuickEditRequest itemPickupPointQuickEditRequest =
        new ItemPickupPointQuickEditRequest();
    itemPickupPointQuickEditRequest.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointQuickEditRequest.setItemSku(ITEM_SKU);
    itemPickupPointQuickEditRequest.setStatus(ProductLevel3Status.ONLINE.name());
    itemPickupPointQuickEditRequest.setCncStatus(ProductLevel3Status.ONLINE.name());
    Boolean scheduleRemovedForStatusUpdate =
        CommonUtils.setSchedulesForL5EditRequest(itemPickupPointRequest,
            itemPickupPointQuickEditRequest, l5sWithActiveSchedules, null, true);
    assertNull(itemPickupPointQuickEditRequest.getBuyableSchedule());
    assertNull(itemPickupPointQuickEditRequest.getDiscoverableSchedule());
    Assertions.assertTrue(scheduleRemovedForStatusUpdate);
  }

  @Test
  public void testSetSchedulesForL5EditDeliveryOfflineCncOfflineRequest(){
    Set<String> l5sWithActiveSchedules = new HashSet<>();
    l5sWithActiveSchedules.add(ITEM_SKU.concat(Constants.HYPHEN).concat(PICKUP_POINT_CODE));
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setBuyableSchedule(
        BuyableScheduleRequest.builder().endDateTime(new Date()).buyable(true).build());
    itemPickupPointRequest.setDiscoverableSchedule(
        DiscoverableScheduleRequest.builder().discoverable(true).startDateTime(new Date(0)).endDateTime(new Date(0)).build());
    ItemPickupPointQuickEditRequest itemPickupPointQuickEditRequest =
        new ItemPickupPointQuickEditRequest();
    itemPickupPointQuickEditRequest.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointQuickEditRequest.setItemSku(ITEM_SKU);
    itemPickupPointQuickEditRequest.setStatus(ProductLevel3Status.OFFLINE.name());
    itemPickupPointQuickEditRequest.setCncStatus(ProductLevel3Status.OFFLINE.name());
    Boolean scheduleRemovedForStatusUpdate =
        CommonUtils.setSchedulesForL5EditRequest(itemPickupPointRequest,
            itemPickupPointQuickEditRequest, l5sWithActiveSchedules, null, true);
    assertNull(scheduleRemovedForStatusUpdate);
  }

  @Test
  public void testSetSchedulesForL5EditDeliveryOfflineCncTeaserRequest(){
    Set<String> l5sWithActiveSchedules = new HashSet<>();
    l5sWithActiveSchedules.add(ITEM_SKU.concat(Constants.HYPHEN).concat(PICKUP_POINT_CODE));
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setBuyableSchedule(
        BuyableScheduleRequest.builder().endDateTime(new Date()).buyable(true).build());
    itemPickupPointRequest.setDiscoverableSchedule(
        DiscoverableScheduleRequest.builder().discoverable(true).startDateTime(new Date(0)).endDateTime(new Date(0)).build());
    ItemPickupPointQuickEditRequest itemPickupPointQuickEditRequest =
        new ItemPickupPointQuickEditRequest();
    itemPickupPointQuickEditRequest.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointQuickEditRequest.setItemSku(ITEM_SKU);
    itemPickupPointQuickEditRequest.setStatus(ProductLevel3Status.OFFLINE.name());
    itemPickupPointQuickEditRequest.setCncStatus(ProductLevel3Status.TEASER.name());
    Boolean scheduleRemovedForStatusUpdate =
        CommonUtils.setSchedulesForL5EditRequest(itemPickupPointRequest,
            itemPickupPointQuickEditRequest, l5sWithActiveSchedules, null, true);
    Assertions.assertTrue(scheduleRemovedForStatusUpdate);
  }

  @Test
  public void testSetSchedulesForL5EditNullBuyableRequest(){
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    Set<String> offlineItemId = new HashSet<>();
    offlineItemId.add(ITEM_SKU.concat(Constants.HYPHEN).concat(PICKUP_POINT_CODE_1));
    itemPickupPointRequest.setBuyableSchedule(null);
    itemPickupPointRequest.setDiscoverableSchedule(
      DiscoverableScheduleRequest.builder().discoverable(true).endDateTime(new Date()).build());
    ItemPickupPointQuickEditRequest itemPickupPointQuickEditRequest =
      new ItemPickupPointQuickEditRequest();
    itemPickupPointQuickEditRequest.setItemSku(ITEM_SKU);
    itemPickupPointQuickEditRequest.setPickupPointCode(PICKUP_POINT_CODE);
    Boolean scheduleRemovedForStatusUpdate =
      CommonUtils.setSchedulesForL5EditRequest(itemPickupPointRequest,
        itemPickupPointQuickEditRequest, offlineItemId, null, true);
    Assertions.assertEquals(itemPickupPointQuickEditRequest.getDiscoverableSchedule().isDiscoverable(),
      itemPickupPointRequest.getDiscoverableSchedule().isDiscoverable());
    assertNull(itemPickupPointQuickEditRequest.getBuyableSchedule());
    assertNull(scheduleRemovedForStatusUpdate);
  }

  @Test
  public void testSetSchedulesForL5EditNullDiscoverableRequest(){
    Set<String> l5sWithActiveSchedules = new HashSet<>();
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setBuyableSchedule(
      BuyableScheduleRequest.builder().endDateTime(new Date()).buyable(true).build());
   itemPickupPointRequest.setDiscoverableSchedule(
      null);
    ItemPickupPointQuickEditRequest itemPickupPointQuickEditRequest =
      new ItemPickupPointQuickEditRequest();
    itemPickupPointQuickEditRequest.setItemSku(ITEM_SKU);
    itemPickupPointQuickEditRequest.setPickupPointCode(PICKUP_POINT_CODE);
    CommonUtils.setSchedulesForL5EditRequest(itemPickupPointRequest, itemPickupPointQuickEditRequest,
      l5sWithActiveSchedules, null, false);
    Assertions.assertEquals(itemPickupPointQuickEditRequest.getBuyableSchedule().isBuyable(),
      itemPickupPointRequest.getBuyableSchedule().isBuyable());
    assertNull(itemPickupPointQuickEditRequest.getDiscoverableSchedule());
  }

  @Test
  public void testGetSchedulesUpdatedL5_withUpdateScheduleTrue() {
    Map<String, Set<String>> scheduleUpdatedL5s = new HashMap<>();
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setItemSku(ITEM_SKU);
    itemPickupPointRequest.setPickupPointId(PICKUP_POINT_CODE);
    itemPickupPointRequest.setScheduleRemoval(true);
    ItemPickupPointListingResponse itemPickupPointListingResponse = new ItemPickupPointListingResponse();
    itemPickupPointListingResponse.setViewConfigs(Collections.emptyList());
    CommonUtils.getSchedulesUpdatedL5(scheduleUpdatedL5s, itemPickupPointListingResponse,
      itemPickupPointRequest, true, false);

    Assertions.assertFalse(scheduleUpdatedL5s.containsKey(L5_CODE));
  }

  @Test
  public void testGetSchedulesUpdatedL5_withRemovalScheduleAndNullBuyableTrue() {
    Map<String, Set<String>> scheduleUpdatedL5s = new HashMap<>();
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setItemSku(ITEM_SKU);
    itemPickupPointRequest.setPickupPointId(PICKUP_POINT_CODE);
    itemPickupPointRequest.setScheduleRemoval(true);
    itemPickupPointRequest.setDiscoverableSchedule(DiscoverableScheduleRequest.builder().startDateTime(new Date()).build());
    ItemPickupPointListingResponse itemPickupPointListingResponse = new ItemPickupPointListingResponse();
    ViewConfigResponse viewConfigResponse =
      ViewConfigResponse.builder().buyableScheduleResponse(null)
        .discoverableScheduleResponse(new DiscoverableScheduleResponse(true, new Date(), new Date()))
        .channelId(Constants.DEFAULT).build();
    itemPickupPointListingResponse.setViewConfigs(Collections.singletonList(viewConfigResponse));
    CommonUtils.getSchedulesUpdatedL5(scheduleUpdatedL5s, itemPickupPointListingResponse,
      itemPickupPointRequest, true, false);

    Assertions.assertTrue(scheduleUpdatedL5s.containsKey(L5_CODE));
  }

  @Test
  public void getSchedulesUpdatedL5ItemPickupPointListingResponseNullTest() {
    Map<String, Set<String>> scheduleUpdatedL5s = new HashMap<>();
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setItemSku(ITEM_SKU);
    itemPickupPointRequest.setPickupPointId(PICKUP_POINT_CODE);
    ViewConfigResponse viewConfigResponse =
        ViewConfigResponse.builder().buyableScheduleResponse(null)
            .discoverableScheduleResponse(new DiscoverableScheduleResponse(true, new Date(), new Date()))
            .channelId(Constants.DEFAULT).build();
    CommonUtils.getSchedulesUpdatedL5(scheduleUpdatedL5s, null,
        itemPickupPointRequest, true, false);
    Assertions.assertEquals(0, scheduleUpdatedL5s.size());
  }

  @Test
  public void testGetSchedulesUpdatedL5_withRemovalScheduleAndNullDiscoverableTrue() {
    Map<String, Set<String>> scheduleUpdatedL5s = new HashMap<>();
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setItemSku(ITEM_SKU);
    itemPickupPointRequest.setPickupPointId(PICKUP_POINT_CODE);
    itemPickupPointRequest.setScheduleRemoval(true);
    ItemPickupPointListingResponse itemPickupPointListingResponse = new ItemPickupPointListingResponse();
    ViewConfigResponse viewConfigResponse =
      ViewConfigResponse.builder().buyableScheduleResponse(new BuyableScheduleResponse(true, new Date(), new Date()))
        .discoverableScheduleResponse(null)
        .channelId(Constants.DEFAULT).build();
    itemPickupPointListingResponse.setViewConfigs(Collections.singletonList(viewConfigResponse));
    CommonUtils.getSchedulesUpdatedL5(scheduleUpdatedL5s, itemPickupPointListingResponse,
      itemPickupPointRequest, true, false);

    Assertions.assertTrue(scheduleUpdatedL5s.containsKey(L5_CODE));
  }

  @Test
  public void testGetSchedulesUpdatedL5_withUpdateScheduleTrueAndSwitchOff() {
    Map<String, Set<String>> scheduleUpdatedL5s = new HashMap<>();
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setItemSku(ITEM_SKU);
    itemPickupPointRequest.setPickupPointId(PICKUP_POINT_CODE);
    itemPickupPointRequest.setScheduleRemoval(true);

    CommonUtils.getSchedulesUpdatedL5(scheduleUpdatedL5s, null, itemPickupPointRequest, false, false);

    Assertions.assertFalse(scheduleUpdatedL5s.containsKey(L5_CODE));
    Assertions.assertTrue(scheduleUpdatedL5s.isEmpty());
  }

  @Test
  public void testGetSchedulesUpdatedL5_withNullScheduleRequests() {
    Map<String, Set<String>> scheduleUpdatedL5s = new HashMap<>();
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setItemSku(ITEM_SKU);
    itemPickupPointRequest.setPickupPointId(PICKUP_POINT_CODE);

    ItemPickupPointListingResponse itemPickupPointListingResponse = new ItemPickupPointListingResponse();
    itemPickupPointListingResponse.setViewConfigs(Collections.emptyList());

    CommonUtils.getSchedulesUpdatedL5(scheduleUpdatedL5s, itemPickupPointListingResponse, itemPickupPointRequest,
      true, false);

    Assertions.assertTrue(scheduleUpdatedL5s.isEmpty());
  }

  @Test
  public void testGetSchedulesUpdatedL5_withNullScheduleResponses() {
    Map<String, Set<String>> scheduleUpdatedL5s = new HashMap<>();
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setItemSku(ITEM_SKU);
    itemPickupPointRequest.setPickupPointId(PICKUP_POINT_CODE);
    itemPickupPointRequest.setBuyableSchedule(new BuyableScheduleRequest());
    itemPickupPointRequest.setDiscoverableSchedule(new DiscoverableScheduleRequest());

    ItemPickupPointListingResponse itemPickupPointListingResponse = new ItemPickupPointListingResponse();
    itemPickupPointListingResponse.setViewConfigs(Collections.singletonList(new ViewConfigResponse()));

    CommonUtils.getSchedulesUpdatedL5(scheduleUpdatedL5s, itemPickupPointListingResponse, itemPickupPointRequest,
      true, false);

    Assertions.assertTrue(scheduleUpdatedL5s.containsKey(L5_CODE));
    Assertions.assertTrue(scheduleUpdatedL5s.get(L5_CODE).contains(Constants.BUYABLE_SCHEDULE_UPDATE));
    Assertions.assertTrue(scheduleUpdatedL5s.get(L5_CODE).contains(Constants.DISCOVERABLE_SCHEDULE_UPDATE));
  }

  @Test
  public void testGetSchedulesUpdatedL5_withScheduleChanges() {
    Map<String, Set<String>> scheduleUpdatedL5s = new HashMap<>();
    Date oldDate = new Date(System.currentTimeMillis() - 10000);
    Date newDate = new Date();
    BuyableScheduleRequest buyableScheduleRequest = BuyableScheduleRequest.builder()
      .startDateTime(newDate)
      .endDateTime(newDate)
      .build();
    DiscoverableScheduleRequest discoverableScheduleRequest = DiscoverableScheduleRequest.builder()
      .startDateTime(newDate)
      .endDateTime(newDate)
      .build();

    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setItemSku(ITEM_SKU);
    itemPickupPointRequest.setPickupPointId(PICKUP_POINT_CODE);
    itemPickupPointRequest.setBuyableSchedule(buyableScheduleRequest);
    itemPickupPointRequest.setDiscoverableSchedule(discoverableScheduleRequest);

    BuyableScheduleResponse buyableScheduleResponse = new BuyableScheduleResponse();
    buyableScheduleResponse.setStartDateTime(oldDate);
    buyableScheduleResponse.setEndDateTime(oldDate);
    DiscoverableScheduleResponse discoverableScheduleResponse = new DiscoverableScheduleResponse();
    discoverableScheduleResponse.setStartDateTime(oldDate);
    discoverableScheduleResponse.setEndDateTime(oldDate);

    ViewConfigResponse viewConfigResponse = new ViewConfigResponse();
    viewConfigResponse.setBuyableScheduleResponse(buyableScheduleResponse);
    viewConfigResponse.setDiscoverableScheduleResponse(discoverableScheduleResponse);
    ItemPickupPointListingResponse itemPickupPointListingResponse = new ItemPickupPointListingResponse();
    itemPickupPointListingResponse.setViewConfigs(Collections.singletonList(viewConfigResponse));

    CommonUtils.getSchedulesUpdatedL5(scheduleUpdatedL5s, itemPickupPointListingResponse, itemPickupPointRequest,
      true, false);

    Assertions.assertTrue(scheduleUpdatedL5s.containsKey(L5_CODE));
    Assertions.assertTrue(scheduleUpdatedL5s.get(L5_CODE).contains(Constants.BUYABLE_SCHEDULE_UPDATE));
    Assertions.assertTrue(scheduleUpdatedL5s.get(L5_CODE).contains(Constants.DISCOVERABLE_SCHEDULE_UPDATE));
  }

  @Test
  public void testGetSchedulesUpdatedL5_withScheduleWithOnlyEndDateChangeChanges() {
    Map<String, Set<String>> scheduleUpdatedL5s = new HashMap<>();
    Date oldDate = new Date(System.currentTimeMillis() - 10000);
    Date newDate = new Date();
    BuyableScheduleRequest buyableScheduleRequest = BuyableScheduleRequest.builder()
      .startDateTime(oldDate)
      .endDateTime(newDate)
      .build();
    DiscoverableScheduleRequest discoverableScheduleRequest = DiscoverableScheduleRequest.builder()
      .startDateTime(oldDate)
      .endDateTime(newDate)
      .build();

    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setItemSku(ITEM_SKU);
    itemPickupPointRequest.setPickupPointId(PICKUP_POINT_CODE);
    itemPickupPointRequest.setBuyableSchedule(buyableScheduleRequest);
    itemPickupPointRequest.setDiscoverableSchedule(discoverableScheduleRequest);

    BuyableScheduleResponse buyableScheduleResponse = new BuyableScheduleResponse();
    buyableScheduleResponse.setStartDateTime(oldDate);
    buyableScheduleResponse.setEndDateTime(oldDate);
    DiscoverableScheduleResponse discoverableScheduleResponse = new DiscoverableScheduleResponse();
    discoverableScheduleResponse.setStartDateTime(oldDate);
    discoverableScheduleResponse.setEndDateTime(oldDate);

    ViewConfigResponse viewConfigResponse = new ViewConfigResponse();
    viewConfigResponse.setBuyableScheduleResponse(buyableScheduleResponse);
    viewConfigResponse.setDiscoverableScheduleResponse(discoverableScheduleResponse);
    ItemPickupPointListingResponse itemPickupPointListingResponse = new ItemPickupPointListingResponse();
    itemPickupPointListingResponse.setViewConfigs(Collections.singletonList(viewConfigResponse));

    CommonUtils.getSchedulesUpdatedL5(scheduleUpdatedL5s, itemPickupPointListingResponse, itemPickupPointRequest,
      true, false);

    Assertions.assertTrue(scheduleUpdatedL5s.containsKey(L5_CODE));
    Assertions.assertTrue(scheduleUpdatedL5s.get(L5_CODE).contains(Constants.BUYABLE_SCHEDULE_UPDATE));
    Assertions.assertTrue(scheduleUpdatedL5s.get(L5_CODE).contains(Constants.DISCOVERABLE_SCHEDULE_UPDATE));
  }

  @Test
  public void testGetSchedulesUpdatedL5_withScheduleWithOnlyEndDateChange() {
    Map<String, Set<String>> scheduleUpdatedL5s = new HashMap<>();
    Date oldDate = new Date(System.currentTimeMillis() - 10000);
    Date newDate = new Date();
    BuyableScheduleRequest buyableScheduleRequest = BuyableScheduleRequest.builder()
      .startDateTime(oldDate)
      .endDateTime(oldDate)
      .build();
    DiscoverableScheduleRequest discoverableScheduleRequest = DiscoverableScheduleRequest.builder()
      .startDateTime(newDate)
      .endDateTime(oldDate)
      .build();

    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setItemSku(ITEM_SKU);
    itemPickupPointRequest.setPickupPointId(PICKUP_POINT_CODE);
    itemPickupPointRequest.setBuyableSchedule(buyableScheduleRequest);
    itemPickupPointRequest.setDiscoverableSchedule(discoverableScheduleRequest);

    BuyableScheduleResponse buyableScheduleResponse = new BuyableScheduleResponse();
    buyableScheduleResponse.setStartDateTime(oldDate);
    buyableScheduleResponse.setEndDateTime(oldDate);
    DiscoverableScheduleResponse discoverableScheduleResponse = new DiscoverableScheduleResponse();
    discoverableScheduleResponse.setStartDateTime(oldDate);
    discoverableScheduleResponse.setEndDateTime(oldDate);

    ViewConfigResponse viewConfigResponse = new ViewConfigResponse();
    viewConfigResponse.setBuyableScheduleResponse(buyableScheduleResponse);
    viewConfigResponse.setDiscoverableScheduleResponse(discoverableScheduleResponse);
    ItemPickupPointListingResponse itemPickupPointListingResponse = new ItemPickupPointListingResponse();
    itemPickupPointListingResponse.setViewConfigs(Collections.singletonList(viewConfigResponse));

    CommonUtils.getSchedulesUpdatedL5(scheduleUpdatedL5s, itemPickupPointListingResponse, itemPickupPointRequest,
      true, false);

    Assertions.assertTrue(scheduleUpdatedL5s.containsKey(L5_CODE));
    Assertions.assertFalse(scheduleUpdatedL5s.get(L5_CODE).contains(Constants.BUYABLE_SCHEDULE_UPDATE));
    Assertions.assertTrue(scheduleUpdatedL5s.get(L5_CODE).contains(Constants.DISCOVERABLE_SCHEDULE_UPDATE));
  }

  @Test
  public void testGetSchedulesUpdatedL5_withScheduleWithOnlyEndDateChangeForBuyable() {
    Map<String, Set<String>> scheduleUpdatedL5s = new HashMap<>();
    Date oldDate = new Date(System.currentTimeMillis() - 10000);
    Date newDate = new Date();
    BuyableScheduleRequest buyableScheduleRequest = BuyableScheduleRequest.builder()
      .startDateTime(oldDate)
      .endDateTime(oldDate)
      .build();
    DiscoverableScheduleRequest discoverableScheduleRequest = DiscoverableScheduleRequest.builder()
      .startDateTime(oldDate)
      .endDateTime(oldDate)
      .build();

    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setItemSku(ITEM_SKU);
    itemPickupPointRequest.setPickupPointId(PICKUP_POINT_CODE);
    itemPickupPointRequest.setBuyableSchedule(buyableScheduleRequest);
    itemPickupPointRequest.setDiscoverableSchedule(discoverableScheduleRequest);

    BuyableScheduleResponse buyableScheduleResponse = new BuyableScheduleResponse();
    buyableScheduleResponse.setStartDateTime(oldDate);
    buyableScheduleResponse.setEndDateTime(newDate);
    DiscoverableScheduleResponse discoverableScheduleResponse = new DiscoverableScheduleResponse();
    discoverableScheduleResponse.setStartDateTime(oldDate);
    discoverableScheduleResponse.setEndDateTime(oldDate);

    ViewConfigResponse viewConfigResponse = new ViewConfigResponse();
    viewConfigResponse.setBuyableScheduleResponse(buyableScheduleResponse);
    viewConfigResponse.setDiscoverableScheduleResponse(discoverableScheduleResponse);
    ItemPickupPointListingResponse itemPickupPointListingResponse = new ItemPickupPointListingResponse();
    itemPickupPointListingResponse.setViewConfigs(Collections.singletonList(viewConfigResponse));

    CommonUtils.getSchedulesUpdatedL5(scheduleUpdatedL5s, itemPickupPointListingResponse, itemPickupPointRequest,
      true, false);

    Assertions.assertTrue(scheduleUpdatedL5s.containsKey(L5_CODE));
    Assertions.assertTrue(scheduleUpdatedL5s.get(L5_CODE).contains(Constants.BUYABLE_SCHEDULE_UPDATE));
    Assertions.assertFalse(scheduleUpdatedL5s.get(L5_CODE).contains(Constants.DISCOVERABLE_SCHEDULE_UPDATE));
  }

  @Test
  public void testGetSchedulesUpdatedL5_withScheduleWithOnlyEndDateChangeForBuyableNoChange() {
    Map<String, Set<String>> scheduleUpdatedL5s = new HashMap<>();
    Date oldDate = new Date(System.currentTimeMillis() - 10000);
    BuyableScheduleRequest buyableScheduleRequest = BuyableScheduleRequest.builder()
      .startDateTime(oldDate)
      .endDateTime(oldDate)
      .build();
    DiscoverableScheduleRequest discoverableScheduleRequest = DiscoverableScheduleRequest.builder()
      .startDateTime(oldDate)
      .endDateTime(oldDate)
      .build();

    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setItemSku(ITEM_SKU);
    itemPickupPointRequest.setPickupPointId(PICKUP_POINT_CODE);
    itemPickupPointRequest.setBuyableSchedule(buyableScheduleRequest);
    itemPickupPointRequest.setDiscoverableSchedule(discoverableScheduleRequest);

    BuyableScheduleResponse buyableScheduleResponse = new BuyableScheduleResponse();
    buyableScheduleResponse.setStartDateTime(oldDate);
    buyableScheduleResponse.setEndDateTime(oldDate);
    DiscoverableScheduleResponse discoverableScheduleResponse = new DiscoverableScheduleResponse();
    discoverableScheduleResponse.setStartDateTime(oldDate);
    discoverableScheduleResponse.setEndDateTime(oldDate);

    ViewConfigResponse viewConfigResponse = new ViewConfigResponse();
    viewConfigResponse.setBuyableScheduleResponse(buyableScheduleResponse);
    viewConfigResponse.setDiscoverableScheduleResponse(discoverableScheduleResponse);
    ItemPickupPointListingResponse itemPickupPointListingResponse = new ItemPickupPointListingResponse();
    itemPickupPointListingResponse.setViewConfigs(Collections.singletonList(viewConfigResponse));

    CommonUtils.getSchedulesUpdatedL5(scheduleUpdatedL5s, itemPickupPointListingResponse, itemPickupPointRequest,
      true, false);

    Assertions.assertFalse(scheduleUpdatedL5s.containsKey(L5_CODE));
  }

  @Test
  public void testGetSchedulesUpdatedL5_withoutScheduleChanges() {
    Map<String, Set<String>> scheduleUpdatedL5s = new HashMap<>();
    Date date = new Date();
    Date oldDate = new Date(System.currentTimeMillis() - 10000);
    BuyableScheduleRequest buyableScheduleRequest = BuyableScheduleRequest.builder()
      .startDateTime(oldDate)
      .endDateTime(date)
      .build();
    DiscoverableScheduleRequest discoverableScheduleRequest = DiscoverableScheduleRequest.builder()
      .startDateTime(date)
      .endDateTime(date)
      .build();

    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setItemSku(ITEM_SKU);
    itemPickupPointRequest.setPickupPointId(PICKUP_POINT_CODE);
    itemPickupPointRequest.setBuyableSchedule(buyableScheduleRequest);
    itemPickupPointRequest.setDiscoverableSchedule(discoverableScheduleRequest);

    BuyableScheduleResponse buyableScheduleResponse = new BuyableScheduleResponse();
    buyableScheduleResponse.setStartDateTime(date);
    buyableScheduleResponse.setEndDateTime(date);
    DiscoverableScheduleResponse discoverableScheduleResponse = new DiscoverableScheduleResponse();
    discoverableScheduleResponse.setStartDateTime(date);
    discoverableScheduleResponse.setEndDateTime(date);

    ViewConfigResponse viewConfigResponse = new ViewConfigResponse();
    viewConfigResponse.setBuyableScheduleResponse(buyableScheduleResponse);
    viewConfigResponse.setDiscoverableScheduleResponse(discoverableScheduleResponse);
    ItemPickupPointListingResponse itemPickupPointListingResponse = new ItemPickupPointListingResponse();
    itemPickupPointListingResponse.setViewConfigs(Collections.singletonList(viewConfigResponse));

    CommonUtils.getSchedulesUpdatedL5(scheduleUpdatedL5s, itemPickupPointListingResponse, itemPickupPointRequest,
      true, false);

    Assertions.assertTrue(scheduleUpdatedL5s.containsKey(L5_CODE));
  }

  @Test
  public void testGetSchedulesUpdatedL5_withoutScheduleChanges2() {
    Map<String, Set<String>> scheduleUpdatedL5s = new HashMap<>();
    Date date = new Date();
    Date oldDate = new Date(System.currentTimeMillis() - 10000);
    BuyableScheduleRequest buyableScheduleRequest = BuyableScheduleRequest.builder()
      .startDateTime(null)
      .endDateTime(oldDate)
      .build();
    DiscoverableScheduleRequest discoverableScheduleRequest = DiscoverableScheduleRequest.builder()
      .startDateTime(date)
      .endDateTime(date)
      .build();

    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setItemSku(ITEM_SKU);
    itemPickupPointRequest.setPickupPointId(PICKUP_POINT_CODE);
    itemPickupPointRequest.setBuyableSchedule(buyableScheduleRequest);
    itemPickupPointRequest.setDiscoverableSchedule(discoverableScheduleRequest);

    BuyableScheduleResponse buyableScheduleResponse = new BuyableScheduleResponse();
    buyableScheduleResponse.setStartDateTime(date);
    buyableScheduleResponse.setEndDateTime(date);
    DiscoverableScheduleResponse discoverableScheduleResponse = new DiscoverableScheduleResponse();
    discoverableScheduleResponse.setStartDateTime(date);
    discoverableScheduleResponse.setEndDateTime(date);

    ViewConfigResponse viewConfigResponse = new ViewConfigResponse();
    viewConfigResponse.setBuyableScheduleResponse(buyableScheduleResponse);
    viewConfigResponse.setDiscoverableScheduleResponse(discoverableScheduleResponse);
    ItemPickupPointListingResponse itemPickupPointListingResponse = new ItemPickupPointListingResponse();
    itemPickupPointListingResponse.setViewConfigs(Collections.singletonList(viewConfigResponse));

    CommonUtils.getSchedulesUpdatedL5(scheduleUpdatedL5s, itemPickupPointListingResponse, itemPickupPointRequest,
      true, false);

    Assertions.assertTrue(scheduleUpdatedL5s.containsKey(L5_CODE));
  }

  @Test
  public void testGetSchedulesUpdatedL5_withoutScheduleChangesWithNullResponseNonNullRequest() {
    Map<String, Set<String>> scheduleUpdatedL5s = new HashMap<>();
    Date date = new Date();

    BuyableScheduleRequest buyableScheduleRequest = BuyableScheduleRequest.builder()
      .startDateTime(date)
      .endDateTime(date)
      .build();
    DiscoverableScheduleRequest discoverableScheduleRequest = DiscoverableScheduleRequest.builder()
      .startDateTime(date)
      .endDateTime(date)
      .build();

    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setItemSku(ITEM_SKU);
    itemPickupPointRequest.setPickupPointId(PICKUP_POINT_CODE);
    itemPickupPointRequest.setBuyableSchedule(null);
    itemPickupPointRequest.setDiscoverableSchedule(discoverableScheduleRequest);

    BuyableScheduleResponse buyableScheduleResponse = new BuyableScheduleResponse();
    buyableScheduleResponse.setStartDateTime(date);
    buyableScheduleResponse.setEndDateTime(date);
    DiscoverableScheduleResponse discoverableScheduleResponse = new DiscoverableScheduleResponse();
    discoverableScheduleResponse.setStartDateTime(date);
    discoverableScheduleResponse.setEndDateTime(date);

    ViewConfigResponse viewConfigResponse = new ViewConfigResponse();
    viewConfigResponse.setBuyableScheduleResponse(buyableScheduleResponse);
    viewConfigResponse.setDiscoverableScheduleResponse(discoverableScheduleResponse);
    ItemPickupPointListingResponse itemPickupPointListingResponse = new ItemPickupPointListingResponse();
    itemPickupPointListingResponse.setViewConfigs(Collections.singletonList(viewConfigResponse));

    CommonUtils.getSchedulesUpdatedL5(scheduleUpdatedL5s, itemPickupPointListingResponse, itemPickupPointRequest,
      true, false);

    Assertions.assertTrue(scheduleUpdatedL5s.containsKey(L5_CODE));
  }

  @Test
  public void testGetSchedulesUpdatedL5_withoutScheduleChangesWithNullBuyableResponseNullRequest() {
    Map<String, Set<String>> scheduleUpdatedL5s = new HashMap<>();
    Date date = new Date();
    DiscoverableScheduleRequest discoverableScheduleRequest = DiscoverableScheduleRequest.builder()
      .startDateTime(date)
      .endDateTime(date)
      .build();

    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setItemSku(ITEM_SKU);
    itemPickupPointRequest.setPickupPointId(PICKUP_POINT_CODE);
    itemPickupPointRequest.setBuyableSchedule(null);
    itemPickupPointRequest.setDiscoverableSchedule(discoverableScheduleRequest);

    DiscoverableScheduleResponse discoverableScheduleResponse = new DiscoverableScheduleResponse();
    discoverableScheduleResponse.setStartDateTime(date);
    discoverableScheduleResponse.setEndDateTime(date);

    ViewConfigResponse viewConfigResponse = new ViewConfigResponse();
    viewConfigResponse.setBuyableScheduleResponse(null);
    viewConfigResponse.setDiscoverableScheduleResponse(discoverableScheduleResponse);
    ItemPickupPointListingResponse itemPickupPointListingResponse = new ItemPickupPointListingResponse();
    itemPickupPointListingResponse.setViewConfigs(Collections.singletonList(viewConfigResponse));

    CommonUtils.getSchedulesUpdatedL5(scheduleUpdatedL5s, itemPickupPointListingResponse, itemPickupPointRequest,
      true, false);

    Assertions.assertFalse(scheduleUpdatedL5s.containsKey(L5_CODE));
  }

  @Test
  public void testGetSchedulesUpdatedL5_withoutScheduleChangesWithNullDiscoverableResponseNullRequest() {
    Map<String, Set<String>> scheduleUpdatedL5s = new HashMap<>();
    Date date = new Date();
    BuyableScheduleRequest buyableScheduleRequest = BuyableScheduleRequest.builder()
      .startDateTime(date)
      .endDateTime(date)
      .build();

    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setItemSku(ITEM_SKU);
    itemPickupPointRequest.setPickupPointId(PICKUP_POINT_CODE);
    itemPickupPointRequest.setBuyableSchedule(buyableScheduleRequest);
    itemPickupPointRequest.setDiscoverableSchedule(null);

    DiscoverableScheduleResponse discoverableScheduleResponse = new DiscoverableScheduleResponse();
    discoverableScheduleResponse.setStartDateTime(date);
    discoverableScheduleResponse.setEndDateTime(date);

    ViewConfigResponse viewConfigResponse = new ViewConfigResponse();
    viewConfigResponse.setBuyableScheduleResponse(null);
    viewConfigResponse.setDiscoverableScheduleResponse(discoverableScheduleResponse);
    ItemPickupPointListingResponse itemPickupPointListingResponse = new ItemPickupPointListingResponse();
    itemPickupPointListingResponse.setViewConfigs(Collections.singletonList(viewConfigResponse));

    CommonUtils.getSchedulesUpdatedL5(scheduleUpdatedL5s, itemPickupPointListingResponse, itemPickupPointRequest,
      true, false);

    Assertions.assertTrue(scheduleUpdatedL5s.containsKey(L5_CODE));
  }

  @Test
  public void testGetBuyableScheduleWithEmptyList() {
    List<ViewConfigResponse> emptyList = Collections.emptyList();
    String result = CommonUtils.getBuyableSchedule(emptyList);
    Assertions.assertEquals(Constants.HYPHEN, result);
  }

  @Test
  public void testGetBuyableScheduleWithNoDefaultChannel() {
    List<ViewConfigResponse> viewConfigResponses = new ArrayList<>();
    ViewConfigResponse viewConfigResponse = ViewConfigResponse.builder().channelId(CHANNEL_ID)
      .buyableScheduleResponse(BuyableScheduleResponse.builder().build()).build();
    viewConfigResponses.add(viewConfigResponse);
    String result = CommonUtils.getBuyableSchedule(viewConfigResponses);
    Assertions.assertEquals(Constants.HYPHEN, result);
  }

  @Test
  public void testGetBuyableScheduleWithDefaultChannelAndNullBuyableSchedule() {
    List<ViewConfigResponse> viewConfigResponses = new ArrayList<>();
    ViewConfigResponse viewConfigResponse =
      ViewConfigResponse.builder().channelId(CHANNEL_ID).build();
    viewConfigResponses.add(viewConfigResponse);
    String result = CommonUtils.getBuyableSchedule(viewConfigResponses);
    Assertions.assertEquals(Constants.HYPHEN, result);
  }

  @Test
  public void testGetBuyableScheduleWithDefaultChannelAndValidBuyableSchedule() {
    Date date = new Date();
    BuyableScheduleResponse buyableScheduleResponse =
      BuyableScheduleResponse.builder().startDateTime(date).endDateTime(date).build();
    List<ViewConfigResponse> viewConfigResponses = new ArrayList<>();
    ViewConfigResponse viewConfigResponse =
      ViewConfigResponse.builder().channelId(Constants.DEFAULT).buyableScheduleResponse(buyableScheduleResponse).build();
    viewConfigResponses.add(viewConfigResponse);
    String result = CommonUtils.getBuyableSchedule(viewConfigResponses);
    Assertions.assertEquals(
      buyableScheduleResponse.getStartDateTime().toString().concat(Constants.SPACE)
        .concat(Constants.HYPHEN).concat(Constants.SPACE)
        .concat(buyableScheduleResponse.getEndDateTime().toString()), result);
  }

  @Test
  public void testGetDiscoverableScheduleWithEmptyList() {
    List<ViewConfigResponse> emptyList = Collections.emptyList();
    String result = CommonUtils.getDiscoverableSchedule(emptyList);
    Assertions.assertEquals(Constants.HYPHEN, result);
  }

  @Test
  public void testGetDiscoverableScheduleWithNoDefaultChannel() {
    List<ViewConfigResponse> viewConfigResponses = new ArrayList<>();
    ViewConfigResponse viewConfigResponse =
      ViewConfigResponse.builder().channelId(Constants.DEFAULT_CHANNEL_ID).discoverableScheduleResponse(new DiscoverableScheduleResponse()).build();
    viewConfigResponses.add(viewConfigResponse);
    String result = CommonUtils.getDiscoverableSchedule(viewConfigResponses);
    Assertions.assertEquals(Constants.HYPHEN, result);
  }

  @Test
  public void testGetDiscoverableScheduleWithDefaultChannelAndNullDiscoverableSchedule() {
    List<ViewConfigResponse> viewConfigResponses = new ArrayList<>();
    ViewConfigResponse viewConfigResponse =
      ViewConfigResponse.builder().channelId(Constants.DEFAULT).discoverableScheduleResponse(null).build();
    viewConfigResponses.add(viewConfigResponse);
    String result = CommonUtils.getDiscoverableSchedule(viewConfigResponses);
    Assertions.assertEquals(Constants.HYPHEN, result);
  }

  @Test
  public void testGetDiscoverableScheduleWithDefaultChannelAndValidDiscoverableSchedule() {
    Date date = new Date();
    DiscoverableScheduleResponse discoverableScheduleResponse =
      DiscoverableScheduleResponse.builder().startDateTime(date).endDateTime(date).build();
    List<ViewConfigResponse> viewConfigResponses = new ArrayList<>();
    ViewConfigResponse viewConfigResponse =
      ViewConfigResponse.builder().channelId(Constants.DEFAULT).discoverableScheduleResponse(discoverableScheduleResponse).build();
    viewConfigResponses.add(viewConfigResponse);
    String result = CommonUtils.getDiscoverableSchedule(viewConfigResponses);
    Assertions.assertEquals(
      discoverableScheduleResponse.getStartDateTime().toString().concat(Constants.SPACE)
        .concat(Constants.HYPHEN).concat(Constants.SPACE)
        .concat(discoverableScheduleResponse.getEndDateTime().toString()), result);
  }

  @Test
  public void testGetDiscoverableScheduleWithEmptySet() {
    Set<ItemViewConfig> emptySet = Collections.emptySet();
    String result = CommonUtils.getDiscoverableSchedule(emptySet);
    Assertions.assertEquals(Constants.HYPHEN, result);
  }

  @Test
  public void testGetDiscoverableScheduleWithNoDefaultChannelSet() {
    Set<ItemViewConfig> itemViewConfigs = new HashSet<>();
    ItemViewConfig itemViewConfig =
      ItemViewConfig.builder().channel(Constants.B2B_CHANNEL).itemDiscoverableSchedules(new ItemDiscoverableSchedule()).build();
    itemViewConfigs.add(itemViewConfig);
    String result = CommonUtils.getDiscoverableSchedule(itemViewConfigs);
    Assertions.assertEquals(Constants.HYPHEN, result);
  }

  @Test
  public void testGetDiscoverableScheduleWithDefaultChannelAndNullDiscoverableScheduleSet() {
    Set<ItemViewConfig> itemViewConfigs = new HashSet<>();
    ItemViewConfig itemViewConfig =
      ItemViewConfig.builder().channel(Constants.DEFAULT).itemDiscoverableSchedules(null).build();
    itemViewConfigs.add(itemViewConfig);
    String result = CommonUtils.getDiscoverableSchedule(itemViewConfigs);
    Assertions.assertEquals(Constants.HYPHEN, result);
  }

  @Test
  public void testGetDiscoverableScheduleWithDefaultChannelAndValidDiscoverableScheduleSet() {
    Date date = new Date();
    ItemDiscoverableSchedule discoverableScheduleResponse =
      new ItemDiscoverableSchedule(true, date, date);
    Set<ItemViewConfig> itemViewConfigs = new HashSet<>();
    ItemViewConfig itemViewConfig =
      ItemViewConfig.builder().channel(Constants.DEFAULT).itemDiscoverableSchedules(discoverableScheduleResponse).build();
    itemViewConfigs.add(itemViewConfig);
    String result = CommonUtils.getDiscoverableSchedule(itemViewConfigs);
    Assertions.assertEquals(
      discoverableScheduleResponse.getStartDateTime().toString().concat(Constants.SPACE)
        .concat(Constants.HYPHEN).concat(Constants.SPACE)
        .concat(discoverableScheduleResponse.getEndDateTime().toString()), result);
  }

  @Test
  public void testGetBuyableScheduleWithEmptySet() {
    Set<ItemViewConfig> emptySet = Collections.emptySet();
    String result = CommonUtils.getBuyableSchedule(emptySet);
    Assertions.assertEquals(Constants.HYPHEN, result);
  }

  @Test
  public void testGetBuyableScheduleWithNoDefaultChannelSet() {
    Set<ItemViewConfig> itemViewConfigs = new HashSet<>();
    ItemViewConfig itemViewConfig =
      ItemViewConfig.builder().channel(Constants.B2B_CHANNEL).itemBuyableSchedules(new ItemBuyableSchedule()).build();
    itemViewConfigs.add(itemViewConfig);
    String result = CommonUtils.getBuyableSchedule(itemViewConfigs);
    Assertions.assertEquals(Constants.HYPHEN, result);
  }

  @Test
  public void testGetBuyableScheduleWithDefaultChannelAndNullBuyableScheduleSet() {
    Set<ItemViewConfig> itemViewConfigs = new HashSet<>();
    ItemViewConfig itemViewConfig =
      ItemViewConfig.builder().channel(Constants.DEFAULT).itemBuyableSchedules(null).build();
    itemViewConfigs.add(itemViewConfig);
    String result = CommonUtils.getBuyableSchedule(itemViewConfigs);
    Assertions.assertEquals(Constants.HYPHEN, result);
  }

  @Test
  public void testGetBuyableScheduleWithDefaultChannelAndValidBuyableScheduleSet() {
    Date date = new Date();
    ItemBuyableSchedule buyableScheduleResponse =
      new ItemBuyableSchedule(true, date, date);
    Set<ItemViewConfig> itemViewConfigs = new HashSet<>();
    ItemViewConfig itemViewConfig =
      ItemViewConfig.builder().channel(Constants.DEFAULT).itemBuyableSchedules(buyableScheduleResponse).build();
    itemViewConfigs.add(itemViewConfig);
    String result = CommonUtils.getBuyableSchedule(itemViewConfigs);
    Assertions.assertEquals(
      buyableScheduleResponse.getStartDateTime().toString().concat(Constants.SPACE)
        .concat(Constants.HYPHEN).concat(Constants.SPACE)
        .concat(buyableScheduleResponse.getEndDateTime().toString()), result);
  }

  @Test
  public void isBuyableScheduleChangedTest() {
    ItemBuyableScheduleDTO itemBuyableScheduleDTO = new ItemBuyableScheduleDTO();
    ItemBuyableScheduleDTO newItemBuyableScheduleDTO = new ItemBuyableScheduleDTO();
    Assertions.assertFalse(CommonUtils.isBuyableScheduleChanged(null, newItemBuyableScheduleDTO));
    Assertions.assertFalse(CommonUtils.isBuyableScheduleChanged(itemBuyableScheduleDTO, newItemBuyableScheduleDTO));
    itemBuyableScheduleDTO.setStartDateTime(new Date());
    Assertions.assertFalse(CommonUtils.isBuyableScheduleChanged(itemBuyableScheduleDTO, newItemBuyableScheduleDTO));
    itemBuyableScheduleDTO.setEndDateTime(new Date());
    Assertions.assertFalse(CommonUtils.isBuyableScheduleChanged(itemBuyableScheduleDTO, newItemBuyableScheduleDTO));
    Assertions.assertTrue(CommonUtils.isBuyableScheduleChanged(itemBuyableScheduleDTO, null));
  }

  @Test
  public void isDiscoverableScheduleChangedTest() {
    ItemDiscoverableScheduleDTO itemDiscoverableScheduleDTO = new ItemDiscoverableScheduleDTO();
    ItemDiscoverableScheduleDTO newItemDiscoverableScheduleDTO = new ItemDiscoverableScheduleDTO();
    Assertions.assertFalse(CommonUtils.isDiscoverableScheduleChanged(null, newItemDiscoverableScheduleDTO));
    Assertions.assertFalse(
        CommonUtils.isDiscoverableScheduleChanged(itemDiscoverableScheduleDTO, newItemDiscoverableScheduleDTO));
    itemDiscoverableScheduleDTO.setStartDateTime(new Date());
    Assertions.assertFalse(
        CommonUtils.isDiscoverableScheduleChanged(itemDiscoverableScheduleDTO, newItemDiscoverableScheduleDTO));
    itemDiscoverableScheduleDTO.setEndDateTime(new Date());
    Assertions.assertFalse(
        CommonUtils.isDiscoverableScheduleChanged(itemDiscoverableScheduleDTO, newItemDiscoverableScheduleDTO));
    Assertions.assertTrue(CommonUtils.isDiscoverableScheduleChanged(itemDiscoverableScheduleDTO, null));
  }

  @Test
  public void setWaitingDeletionForDeletePickupPointTest() {
    PickupPointFilterRequest pickupPointFilterRequest = new PickupPointFilterRequest();
    CommonUtils.setWaitingDeletionForDeletePickupPoint(false, pickupPointFilterRequest);
    assertNull(pickupPointFilterRequest.getWaitingDeletion());
    CommonUtils.setWaitingDeletionForDeletePickupPoint(true, pickupPointFilterRequest);
    Assertions.assertFalse(pickupPointFilterRequest.getWaitingDeletion());
  }

  @Test
  public void isPickupPointInvalidTest() {
    PickupPointResponse pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(DEFAULT_PICKUP_POINT_CODE);
    pickupPointResponses.add(pickupPointResponse);
    pickupPointResponses.forEach(response -> response.setBusinessPartnerCode(BUSINESS_PARTNER_CODE));
    Assertions.assertFalse(
        CommonUtils.isPickupPointInvalid(Set.of(PRODUCT_SKU), pickupPointResponses, BUSINESS_PARTNER_CODE));
    Assertions.assertTrue(
        CommonUtils.isPickupPointInvalid(Set.of(PRODUCT_SKU), new ArrayList<>(), BUSINESS_PARTNER_CODE));
    Assertions.assertTrue(CommonUtils.isPickupPointInvalid(Set.of(PRODUCT_SKU, PICKUP_POINT_CODE_1, BUSINESS_PARTNER_CODE),
        pickupPointResponses, BUSINESS_PARTNER_CODE));
    pickupPointResponse.setArchived(true);
    Assertions.assertTrue(
        CommonUtils.isPickupPointInvalid(Set.of(PRODUCT_SKU), pickupPointResponses, BUSINESS_PARTNER_CODE));
    pickupPointResponse.setArchived(false);
    Assertions.assertTrue(
        CommonUtils.isPickupPointInvalid(Set.of(PRODUCT_SKU), pickupPointResponses, PRODUCT_IMAGE));
    pickupPointResponse.setWaitingDeletion(true);
    Assertions.assertTrue(
        CommonUtils.isPickupPointInvalid(Set.of(PRODUCT_SKU), pickupPointResponses, BUSINESS_PARTNER_CODE));
  }

  @Test
  public void validatePickupPointsForCreationTest() {
    SimpleStringResponse error = new SimpleStringResponse();
    pickupPointResponses.forEach(response -> response.setBusinessPartnerCode(BUSINESS_PARTNER_CODE));
    error.setResult(ERROR_MESSAGE);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      CommonUtils.validatePickupPointsAndWaitingDeletion(PRODUCT_SKU, Set.of(PRODUCT_SKU), pickupPointResponses, error,
          PRODUCT_SKU);
    });
    Assertions.assertEquals(ApiErrorCode.PICKUP_POINT_IS_NOT_VALID.getCode(), error.getResult());
  }

  @Test
  public void fetchL5WithActiveSchedulesTest_BuyableScheduleActive() {
    Calendar calendar = Calendar.getInstance();
    Date startDate = calendar.getTime();
    calendar.add(Calendar.DAY_OF_MONTH, 10);
    Date endDate = calendar.getTime();
    ProductL3UpdateRequest productVariantUpdateRequest = new ProductL3UpdateRequest();
    productVariantUpdateRequest.setProductSku(PRODUCT_SKU);
    productVariantUpdateRequest.setProductName(Constants.PRODUCT_NAME);
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest =
      new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest.setItemName(ITEM_NAME);
    productVariantPriceStockAndImagesRequest.setItemSku(ITEM_SKU);
    productVariantPriceStockAndImagesRequest.setItemName(ITEM_NAME);
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setPickupPointId(PICKUP_POINT_CODE);
    itemPickupPointRequest.setBuyableSchedule(BuyableScheduleRequest.builder().startDateTime(startDate).endDateTime(endDate).build());
    productVariantPriceStockAndImagesRequest
      .setModifiedItemPickupPoints(Collections.singletonList(itemPickupPointRequest));
    productVariantUpdateRequest.setProductItems(new ArrayList<>(
      List.of(productVariantPriceStockAndImagesRequest)));
    ViewConfigResponse viewConfigResponse = ViewConfigResponse.builder()
      .buyableScheduleResponse(BuyableScheduleResponse.builder().startDateTime(startDate).endDateTime(endDate).build())
      .build();
    ItemPickupPointBasicResponse itemPickupPointBasicResponse =
      ItemPickupPointBasicResponse.builder().itemSku(ITEM_SKU).pickupPointCode(PICKUP_POINT_CODE)
        .build();
    itemPickupPointBasicResponse.setViewConfigResponse(viewConfigResponse);
    Map<String, Set<String>> l5WithActiveSchedules = CommonUtils.fetchL5WithActiveAndUnModifiedSchedules(
      Collections.singletonList(itemPickupPointBasicResponse), productVariantUpdateRequest);
    Assertions.assertTrue(MapUtils.isNotEmpty(l5WithActiveSchedules));
    Assertions.assertTrue(l5WithActiveSchedules.get(Constants.BUYABLE_SCHEDULE_UPDATE)
      .contains(ITEM_SKU.concat(Constants.HYPHEN).concat(PICKUP_POINT_CODE)));
  }

  @Test
  public void fetchL5WithActiveSchedulesTest_BuyableScheduleActiveAndItemSkuNull() {
    Calendar calendar = Calendar.getInstance();
    Date startDate = calendar.getTime();
    calendar.add(Calendar.DAY_OF_MONTH, 10);
    Date endDate = calendar.getTime();
    ProductL3UpdateRequest productVariantUpdateRequest = new ProductL3UpdateRequest();
    productVariantUpdateRequest.setProductSku(PRODUCT_SKU);
    productVariantUpdateRequest.setProductName(Constants.PRODUCT_NAME);
    ProductVariantPriceStockAndImagesRequest variantPriceStockAndImagesRequest =
      new ProductVariantPriceStockAndImagesRequest();
    variantPriceStockAndImagesRequest.setItemName(ITEM_NAME);
    variantPriceStockAndImagesRequest.setItemName(ITEM_NAME);
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setPickupPointId(PICKUP_POINT_CODE);
    itemPickupPointRequest.setBuyableSchedule(BuyableScheduleRequest.builder().startDateTime(startDate).endDateTime(endDate).build());
    variantPriceStockAndImagesRequest
      .setModifiedItemPickupPoints(Collections.singletonList(itemPickupPointRequest));
    productVariantUpdateRequest.setProductItems(new ArrayList<>(
      List.of(variantPriceStockAndImagesRequest)));
    ViewConfigResponse viewConfigResponse = ViewConfigResponse.builder()
      .buyableScheduleResponse(BuyableScheduleResponse.builder().startDateTime(startDate).endDateTime(endDate).build())
      .build();
    ItemPickupPointBasicResponse itemPickupPointBasicResponse =
      ItemPickupPointBasicResponse.builder().itemSku(ITEM_SKU).pickupPointCode(PICKUP_POINT_CODE)
        .build();
    itemPickupPointBasicResponse.setViewConfigResponse(viewConfigResponse);
    Map<String, Set<String>> l5WithActiveSchedules = CommonUtils.fetchL5WithActiveAndUnModifiedSchedules(
      Collections.singletonList(itemPickupPointBasicResponse), productVariantUpdateRequest);
    Assertions.assertTrue(MapUtils.isNotEmpty(l5WithActiveSchedules));
  }

  @Test
  public void fetchL5WithActiveSchedulesTest_BuyableScheduleActiveAndDifferent() {
    Calendar calendar = Calendar.getInstance();
    Date startDate = calendar.getTime();
    calendar.add(Calendar.DAY_OF_MONTH, 10);
    Date endDate = calendar.getTime();
    Date startDateNew = calendar.getTime();
    calendar.add(Calendar.DAY_OF_MONTH, 11);
    Date endDateNew = calendar.getTime();
    ProductL3UpdateRequest productVariantUpdateRequest = new ProductL3UpdateRequest();
    productVariantUpdateRequest.setProductSku(PRODUCT_SKU);
    productVariantUpdateRequest.setProductName(Constants.PRODUCT_NAME);
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest =
      new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest.setItemName(ITEM_NAME);
    productVariantPriceStockAndImagesRequest.setItemSku(ITEM_SKU);
    productVariantPriceStockAndImagesRequest.setItemName(ITEM_NAME);
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setPickupPointId(PICKUP_POINT_CODE);
    itemPickupPointRequest.setBuyableSchedule(BuyableScheduleRequest.builder().startDateTime(startDate).endDateTime(endDateNew).build());
    productVariantPriceStockAndImagesRequest
      .setModifiedItemPickupPoints(Collections.singletonList(itemPickupPointRequest));
    productVariantUpdateRequest.setProductItems(new ArrayList<>(
      List.of(productVariantPriceStockAndImagesRequest)));
    ViewConfigResponse viewConfigResponse = ViewConfigResponse.builder()
      .buyableScheduleResponse(BuyableScheduleResponse.builder().startDateTime(startDate).endDateTime(endDate).build())
      .build();
    ItemPickupPointBasicResponse itemPickupPointBasicResponse =
      ItemPickupPointBasicResponse.builder().itemSku(ITEM_SKU).pickupPointCode(PICKUP_POINT_CODE)
        .build();
    itemPickupPointBasicResponse.setViewConfigResponse(viewConfigResponse);
    Map<String, Set<String>> l5WithActiveSchedules = CommonUtils.fetchL5WithActiveAndUnModifiedSchedules(
      Collections.singletonList(itemPickupPointBasicResponse), productVariantUpdateRequest);
    Assertions.assertTrue(MapUtils.isNotEmpty(l5WithActiveSchedules));
    Assertions.assertFalse(l5WithActiveSchedules.get(Constants.BUYABLE_SCHEDULE_UPDATE)
      .contains(ITEM_SKU.concat(Constants.HYPHEN).concat(PICKUP_POINT_CODE)));
  }



  @Test
  public void fetchL5WithActiveSchedulesTest_DiscoverableScheduleActive() {
    Calendar calendar = Calendar.getInstance();
    Date startDate = calendar.getTime();
    calendar.add(Calendar.DAY_OF_MONTH, 10);
    Date endDate = calendar.getTime();
    ProductL3UpdateRequest productVariantUpdateRequest = new ProductL3UpdateRequest();
    productVariantUpdateRequest.setProductSku(PRODUCT_SKU);
    productVariantUpdateRequest.setProductName(Constants.PRODUCT_NAME);
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest =
      new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest.setItemName(ITEM_NAME);
    productVariantPriceStockAndImagesRequest.setItemSku(ITEM_SKU);
    productVariantPriceStockAndImagesRequest.setItemName(ITEM_NAME);
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setPickupPointId(PICKUP_POINT_CODE);
    itemPickupPointRequest.setDiscoverableSchedule(DiscoverableScheduleRequest.builder().startDateTime(startDate).endDateTime(endDate).build());
    productVariantPriceStockAndImagesRequest
      .setModifiedItemPickupPoints(Collections.singletonList(itemPickupPointRequest));
    productVariantUpdateRequest.setProductItems(new ArrayList<>(
      List.of(productVariantPriceStockAndImagesRequest)));
    ViewConfigResponse viewConfigResponse = ViewConfigResponse.builder()
      .discoverableScheduleResponse(DiscoverableScheduleResponse.builder().startDateTime(startDate).endDateTime(endDate).build())
      .build();
    ItemPickupPointBasicResponse itemPickupPointBasicResponse =
      ItemPickupPointBasicResponse.builder().itemSku(ITEM_SKU).pickupPointCode(PICKUP_POINT_CODE)
        .build();
    itemPickupPointBasicResponse.setViewConfigResponse(viewConfigResponse);
    Map<String, Set<String>> l5WithActiveSchedules = CommonUtils.fetchL5WithActiveAndUnModifiedSchedules(
      Collections.singletonList(itemPickupPointBasicResponse), productVariantUpdateRequest);
    Assertions.assertTrue(MapUtils.isNotEmpty(l5WithActiveSchedules));
    Assertions.assertTrue(l5WithActiveSchedules.get(Constants.DISCOVERABLE_SCHEDULE_UPDATE)
      .contains(ITEM_SKU.concat(Constants.HYPHEN).concat(PICKUP_POINT_CODE)));
  }
  @Test
  public void fetchL5WithActiveSchedulesTest_DiscoverableScheduleActiveDifferntEndDate() {
    Calendar calendar = Calendar.getInstance();
    Date startDate = calendar.getTime();
    calendar.add(Calendar.DAY_OF_MONTH, 10);
    Date endDate = calendar.getTime();
    calendar.add(Calendar.DAY_OF_MONTH, 12);
    Date newEndDate = calendar.getTime();
    ProductL3UpdateRequest productVariantUpdateRequest = new ProductL3UpdateRequest();
    productVariantUpdateRequest.setProductSku(PRODUCT_SKU);
    productVariantUpdateRequest.setProductName(Constants.PRODUCT_NAME);
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest =
      new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest.setItemName(ITEM_NAME);
    productVariantPriceStockAndImagesRequest.setItemSku(ITEM_SKU);
    productVariantPriceStockAndImagesRequest.setItemName(ITEM_NAME);
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setPickupPointId(PICKUP_POINT_CODE);
    itemPickupPointRequest.setDiscoverableSchedule(DiscoverableScheduleRequest.builder().startDateTime(startDate).endDateTime(newEndDate).build());
    productVariantPriceStockAndImagesRequest
      .setModifiedItemPickupPoints(Collections.singletonList(itemPickupPointRequest));
    productVariantUpdateRequest.setProductItems(new ArrayList<>(
      List.of(productVariantPriceStockAndImagesRequest)));
    ViewConfigResponse viewConfigResponse = ViewConfigResponse.builder()
      .discoverableScheduleResponse(DiscoverableScheduleResponse.builder().startDateTime(startDate).endDateTime(endDate).build())
      .build();
    ItemPickupPointBasicResponse itemPickupPointBasicResponse =
      ItemPickupPointBasicResponse.builder().itemSku(ITEM_SKU).pickupPointCode(PICKUP_POINT_CODE)
        .build();
    itemPickupPointBasicResponse.setViewConfigResponse(viewConfigResponse);
    Map<String, Set<String>> l5WithActiveSchedules = CommonUtils.fetchL5WithActiveAndUnModifiedSchedules(
      Collections.singletonList(itemPickupPointBasicResponse), productVariantUpdateRequest);
    Assertions.assertTrue(MapUtils.isNotEmpty(l5WithActiveSchedules));
    Assertions.assertFalse(l5WithActiveSchedules.get(Constants.DISCOVERABLE_SCHEDULE_UPDATE)
      .contains(ITEM_SKU.concat(Constants.HYPHEN).concat(PICKUP_POINT_CODE)));
  }

  @Test
  public void fetchL5WithActiveSchedulesTest_DiscoverableScheduleActiveAndDifferntDates() {
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DAY_OF_MONTH, 1);
    Date newStartDate = calendar.getTime();
    calendar.add(Calendar.DAY_OF_MONTH, 10);
    Date endDate = calendar.getTime();
    calendar.add(Calendar.DAY_OF_MONTH, 10);
    Date newEndDate = calendar.getTime();
    calendar.add(Calendar.DAY_OF_MONTH, 30);
    Date startDate = calendar.getTime();
    ProductL3UpdateRequest productVariantUpdateRequest = new ProductL3UpdateRequest();
    productVariantUpdateRequest.setProductSku(PRODUCT_SKU);
    productVariantUpdateRequest.setProductName(Constants.PRODUCT_NAME);
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest =
      new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest.setItemName(ITEM_NAME);
    productVariantPriceStockAndImagesRequest.setItemSku(ITEM_SKU);
    productVariantPriceStockAndImagesRequest.setItemName(ITEM_NAME);
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setPickupPointId(PICKUP_POINT_CODE);
    itemPickupPointRequest.setDiscoverableSchedule(DiscoverableScheduleRequest.builder().startDateTime(newStartDate).endDateTime(newEndDate).build());
    itemPickupPointRequest.setBuyableSchedule(BuyableScheduleRequest.builder().startDateTime(newStartDate).endDateTime(newEndDate).build());
    productVariantPriceStockAndImagesRequest
      .setModifiedItemPickupPoints(Collections.singletonList(itemPickupPointRequest));
    productVariantUpdateRequest.setProductItems(new ArrayList<>(
      List.of(productVariantPriceStockAndImagesRequest)));
    ViewConfigResponse viewConfigResponse = ViewConfigResponse.builder()
      .discoverableScheduleResponse(
        DiscoverableScheduleResponse.builder().startDateTime(startDate).endDateTime(endDate)
          .build()).buyableScheduleResponse(
        BuyableScheduleResponse.builder().startDateTime(startDate).endDateTime(endDate).build())
      .build();
    ItemPickupPointBasicResponse itemPickupPointBasicResponse =
      ItemPickupPointBasicResponse.builder().itemSku(ITEM_SKU).pickupPointCode(PICKUP_POINT_CODE)
        .build();
    itemPickupPointBasicResponse.setViewConfigResponse(viewConfigResponse);
    Map<String, Set<String>> l5WithActiveSchedules = CommonUtils.fetchL5WithActiveAndUnModifiedSchedules(
      Collections.singletonList(itemPickupPointBasicResponse), productVariantUpdateRequest);
    Assertions.assertTrue(MapUtils.isNotEmpty(l5WithActiveSchedules));
    Assertions.assertFalse(l5WithActiveSchedules.get(Constants.DISCOVERABLE_SCHEDULE_UPDATE)
      .contains(ITEM_SKU.concat(Constants.HYPHEN).concat(PICKUP_POINT_CODE)));
  }

  @Test
  public void fetchL5WithActiveSchedulesTest_newSchedulesAdded() {
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DAY_OF_MONTH, 10);
    Date startDate = calendar.getTime();
    Date endDate = calendar.getTime();
    ProductL3UpdateRequest productVariantUpdateRequest = new ProductL3UpdateRequest();
    productVariantUpdateRequest.setProductSku(PRODUCT_SKU);
    productVariantUpdateRequest.setProductName(Constants.PRODUCT_NAME);
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest =
      new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest.setItemName(ITEM_NAME);
    productVariantPriceStockAndImagesRequest.setItemSku(ITEM_SKU);
    productVariantPriceStockAndImagesRequest.setItemName(ITEM_NAME);
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setPickupPointId(PICKUP_POINT_CODE);
    itemPickupPointRequest.setDiscoverableSchedule(
      DiscoverableScheduleRequest.builder().startDateTime(startDate).endDateTime(endDate).build());
    itemPickupPointRequest.setBuyableSchedule(BuyableScheduleRequest.builder().startDateTime(startDate).endDateTime(endDate).build());
    productVariantPriceStockAndImagesRequest
      .setModifiedItemPickupPoints(Collections.singletonList(itemPickupPointRequest));
    productVariantUpdateRequest.setProductItems(new ArrayList<>(
      List.of(productVariantPriceStockAndImagesRequest)));
    ItemPickupPointBasicResponse itemPickupPointBasicResponse =
      ItemPickupPointBasicResponse.builder().itemSku(ITEM_SKU).pickupPointCode(PICKUP_POINT_CODE)
        .build();
    itemPickupPointBasicResponse.setViewConfigResponse(null);
    Map<String, Set<String>> l5WithActiveSchedules = CommonUtils.fetchL5WithActiveAndUnModifiedSchedules(
      Collections.singletonList(itemPickupPointBasicResponse), productVariantUpdateRequest);
    Assertions.assertFalse(l5WithActiveSchedules.get(Constants.DISCOVERABLE_SCHEDULE_UPDATE)
      .contains(ITEM_SKU.concat(Constants.HYPHEN).concat(PICKUP_POINT_CODE)));
  }

  @Test
  public void testGetExistingSchedulesFromRequest_BothSchedulesPresent() {
    ProductL3UpdateRequest productL3UpdateRequest = new ProductL3UpdateRequest();
    ProductVariantPriceStockAndImagesRequest variantRequest = new ProductVariantPriceStockAndImagesRequest();
    variantRequest.setItemSku(ITEM_SKU);

    ItemPickupPointRequest pickupPointRequest = new ItemPickupPointRequest();
    pickupPointRequest.setPickupPointId(PICKUP_POINT_CODE);
    pickupPointRequest.setDiscoverableSchedule(new DiscoverableScheduleRequest());
    pickupPointRequest.setBuyableSchedule(new BuyableScheduleRequest());

    variantRequest.setModifiedItemPickupPoints(Collections.singletonList(pickupPointRequest));
    productL3UpdateRequest.setProductItems(Collections.singletonList(variantRequest));

    List<com.gdn.x.product.rest.web.model.request.ItemPickupPointRequest> result =
      CommonUtils.getExistingSchedulesFromRequest(productL3UpdateRequest);

    Assertions.assertEquals(1, result.size());
    Assertions.assertEquals(ITEM_SKU, result.get(0).getItemSku());
    Assertions.assertEquals(PICKUPPOINT_CODE, result.get(0).getPickupPointCode());
  }

  @Test
  public void testGetExistingSchedulesFromRequest_DiscoverableSchedulePresent() {
    ProductL3UpdateRequest productL3UpdateRequest = new ProductL3UpdateRequest();
    ProductVariantPriceStockAndImagesRequest variantRequest = new ProductVariantPriceStockAndImagesRequest();
    variantRequest.setItemSku(ITEM_SKU);

    ItemPickupPointRequest pickupPointRequest = new ItemPickupPointRequest();
    pickupPointRequest.setPickupPointId(PICKUP_POINT_CODE);
    pickupPointRequest.setDiscoverableSchedule(new DiscoverableScheduleRequest());

    variantRequest.setModifiedItemPickupPoints(Collections.singletonList(pickupPointRequest));
    productL3UpdateRequest.setProductItems(Collections.singletonList(variantRequest));

    List<com.gdn.x.product.rest.web.model.request.ItemPickupPointRequest> result =
      CommonUtils.getExistingSchedulesFromRequest(productL3UpdateRequest);

    Assertions.assertEquals(1, result.size());
    Assertions.assertEquals(ITEM_SKU, result.get(0).getItemSku());
    Assertions.assertEquals(PICKUPPOINT_CODE, result.get(0).getPickupPointCode());
  }

  @Test
  public void testGetExistingSchedulesFromRequest_BuyableSchedulePresent() {
    ProductL3UpdateRequest productL3UpdateRequest = new ProductL3UpdateRequest();
    ProductVariantPriceStockAndImagesRequest variantRequest = new ProductVariantPriceStockAndImagesRequest();
    variantRequest.setItemSku(ITEM_SKU);

    ItemPickupPointRequest pickupPointRequest = new ItemPickupPointRequest();
    pickupPointRequest.setPickupPointId(PICKUP_POINT_CODE);
    pickupPointRequest.setBuyableSchedule(new BuyableScheduleRequest());

    variantRequest.setModifiedItemPickupPoints(Collections.singletonList(pickupPointRequest));
    productL3UpdateRequest.setProductItems(Collections.singletonList(variantRequest));

    List<com.gdn.x.product.rest.web.model.request.ItemPickupPointRequest> result =
      CommonUtils.getExistingSchedulesFromRequest(productL3UpdateRequest);

    Assertions.assertEquals(1, result.size());
    Assertions.assertEquals(ITEM_SKU, result.get(0).getItemSku());
    Assertions.assertEquals(PICKUPPOINT_CODE, result.get(0).getPickupPointCode());
  }

  @Test
  public void testGetExistingSchedulesFromRequest_NoSchedulesPresent() {
    ProductL3UpdateRequest productL3UpdateRequest = new ProductL3UpdateRequest();
    ProductVariantPriceStockAndImagesRequest variantRequest = new ProductVariantPriceStockAndImagesRequest();
    variantRequest.setItemSku(ITEM_SKU);

    ItemPickupPointRequest pickupPointRequest = new ItemPickupPointRequest();
    pickupPointRequest.setPickupPointId(PICKUP_POINT_CODE);

    variantRequest.setModifiedItemPickupPoints(Collections.singletonList(pickupPointRequest));
    productL3UpdateRequest.setProductItems(Collections.singletonList(variantRequest));

    List<com.gdn.x.product.rest.web.model.request.ItemPickupPointRequest> result =
      CommonUtils.getExistingSchedulesFromRequest(productL3UpdateRequest);

    Assertions.assertTrue(result.isEmpty());
  }

  @Test
  public void testSetSchedulesForQuickEditRequest_NoDefaultChannel() {
    ItemSummaryListResponse itemSummaryListResponse = new ItemSummaryListResponse();
    itemSummaryListResponse.setItemViewConfigs(Collections.emptySet());
    ItemPickupPointQuickEditRequest itemPickupPointQuickEditRequest = new ItemPickupPointQuickEditRequest();
    CommonUtils.setSchedulesForQuickEditRequest(itemSummaryListResponse, itemPickupPointQuickEditRequest, false);
    assertNull(itemPickupPointQuickEditRequest.getBuyableSchedule());
    assertNull(itemPickupPointQuickEditRequest.getDiscoverableSchedule());
  }

  @Test
  public void testSetSchedulesForQuickEditRequest_DefaultChannelNoSchedules() {
    ItemSummaryListResponse itemSummaryListResponse = new ItemSummaryListResponse();
    ItemViewConfigDTO itemViewConfigDTO = new ItemViewConfigDTO();
    itemViewConfigDTO.setChannel(Constants.DEFAULT);
    itemSummaryListResponse.setItemViewConfigs(Set.of(itemViewConfigDTO));

    ItemPickupPointQuickEditRequest itemPickupPointQuickEditRequest = new ItemPickupPointQuickEditRequest();
    itemPickupPointQuickEditRequest.setItemSku(ITEM_SKU);
    itemPickupPointQuickEditRequest.setPickupPointCode(PICKUP_POINT_CODE);
    CommonUtils.setSchedulesForQuickEditRequest(itemSummaryListResponse, itemPickupPointQuickEditRequest, false);
    assertNull(itemPickupPointQuickEditRequest.getBuyableSchedule());
    assertNull(itemPickupPointQuickEditRequest.getDiscoverableSchedule());
  }

  @Test
  public void testSetSchedulesForQuickEditRequest_DefaultChannelWithBuyableSchedules() {
    ItemSummaryListResponse itemSummaryListResponse = new ItemSummaryListResponse();
    ItemViewConfigDTO itemViewConfigDTO = new ItemViewConfigDTO();
    itemViewConfigDTO.setChannel(Constants.DEFAULT);

    ItemBuyableScheduleDTO itemBuyableScheduleDTO = new ItemBuyableScheduleDTO();
    itemBuyableScheduleDTO.setBuyable(true);
    itemBuyableScheduleDTO.setStartDateTime(new Date());
    itemBuyableScheduleDTO.setEndDateTime(new Date());

    itemViewConfigDTO.setItemBuyableSchedules(itemBuyableScheduleDTO);
    itemSummaryListResponse.setItemViewConfigs(Set.of(itemViewConfigDTO));

    ItemPickupPointQuickEditRequest itemPickupPointQuickEditRequest = new ItemPickupPointQuickEditRequest();
    itemPickupPointQuickEditRequest.setItemSku(ITEM_SKU);
    itemPickupPointQuickEditRequest.setPickupPointCode(PICKUP_POINT_CODE);
    CommonUtils.setSchedulesForQuickEditRequest(itemSummaryListResponse, itemPickupPointQuickEditRequest, false);

    Assertions.assertNotNull(itemPickupPointQuickEditRequest.getBuyableSchedule());
    Assertions.assertTrue(itemPickupPointQuickEditRequest.getBuyableSchedule().isBuyable());
    assertNull(itemPickupPointQuickEditRequest.getDiscoverableSchedule());
  }
  @Test
  public void testSetSchedulesForQuickEditRequest_DefaultChannelWithDiscoverableSchedules() {
    ItemSummaryListResponse itemSummaryListResponse = new ItemSummaryListResponse();
    ItemViewConfigDTO itemViewConfigDTO = new ItemViewConfigDTO();
    itemViewConfigDTO.setChannel(Constants.DEFAULT);

    ItemDiscoverableScheduleDTO itemDiscoverableScheduleDTO = new ItemDiscoverableScheduleDTO();
    itemDiscoverableScheduleDTO.setDiscoverable(true);
    itemDiscoverableScheduleDTO.setStartDateTime(new Date());
    itemDiscoverableScheduleDTO.setEndDateTime(new Date());

    itemViewConfigDTO.setItemDiscoverableSchedules(itemDiscoverableScheduleDTO);
    itemSummaryListResponse.setItemViewConfigs(Set.of(itemViewConfigDTO));

    ItemPickupPointQuickEditRequest itemPickupPointQuickEditRequest = new ItemPickupPointQuickEditRequest();
    itemPickupPointQuickEditRequest.setItemSku(ITEM_SKU);
    itemPickupPointQuickEditRequest.setPickupPointCode(PICKUP_POINT_CODE);
    CommonUtils.setSchedulesForQuickEditRequest(itemSummaryListResponse, itemPickupPointQuickEditRequest, false);

    Assertions.assertNotNull(itemPickupPointQuickEditRequest.getDiscoverableSchedule());
    Assertions.assertTrue(itemPickupPointQuickEditRequest.getDiscoverableSchedule().isDiscoverable());
    assertNull(itemPickupPointQuickEditRequest.getBuyableSchedule());
  }

  @Test
  public void setForceReviewFlagTest() {
    Assertions.assertTrue(CommonUtils.setForceReviewFlag(true, true));
    Assertions.assertTrue(CommonUtils.setForceReviewFlag(true, false));
    Assertions.assertTrue(CommonUtils.setForceReviewFlag(false, true));
    Assertions.assertFalse(CommonUtils.setForceReviewFlag(false, false));
  }

  @Test
  public void setImageQcViolationTest(){
    String violation = CommonUtils.setImageQcViolation(PREDICTION_NAME_1, PREDICTION_NAME_2);
    Assertions.assertEquals(violation, PREDICTION_NAME_1.concat(Constants.COMMA).concat(PREDICTION_NAME_2));
  }

  @Test
  public void setImageQcViolationExistingNullTest(){
    String violation = CommonUtils.setImageQcViolation(null, PREDICTION_NAME_2);
    Assertions.assertEquals(PREDICTION_NAME_2, violation);
  }

  @Test
  public void setImageQcViolationBothNullTest(){
    String violation = CommonUtils.setImageQcViolation(null, null);
    Assertions.assertEquals(violation, StringUtils.EMPTY);
  }

  @Test
  public void testSetSchedulesForQuickEditRequestsWithStatusChange() {
    ItemSummaryListResponse itemSummaryListResponse = new ItemSummaryListResponse();
    ItemViewConfigDTO itemViewConfigDTO = new ItemViewConfigDTO();
    itemViewConfigDTO.setChannel(Constants.DEFAULT);

    ItemDiscoverableScheduleDTO itemDiscoverableScheduleDTO = new ItemDiscoverableScheduleDTO();
    itemDiscoverableScheduleDTO.setDiscoverable(true);
    itemDiscoverableScheduleDTO.setStartDateTime(new Date());
    itemDiscoverableScheduleDTO.setEndDateTime(new Date());

    itemViewConfigDTO.setItemDiscoverableSchedules(itemDiscoverableScheduleDTO);
    itemSummaryListResponse.setItemViewConfigs(Set.of(itemViewConfigDTO));

    ItemPickupPointQuickEditRequest itemPickupPointQuickEditRequest = new ItemPickupPointQuickEditRequest();
    itemPickupPointQuickEditRequest.setItemSku(ITEM_SKU);
    itemPickupPointQuickEditRequest.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointQuickEditRequest.setStatus(ProductLevel3Status.ONLINE.name());
    CommonUtils.setSchedulesForQuickEditRequest(itemSummaryListResponse, itemPickupPointQuickEditRequest, false);
    Assertions.assertTrue(itemPickupPointQuickEditRequest.isScheduleUpdate());
  }

  @Test
  public void testSetSchedulesForQuickEditRequestsWithStatusChangeAndNoExistingSchedules() {
    ItemSummaryListResponse itemSummaryListResponse = new ItemSummaryListResponse();
    ItemViewConfigDTO itemViewConfigDTO = new ItemViewConfigDTO();
    itemViewConfigDTO.setChannel(Constants.DEFAULT);
    itemViewConfigDTO.setItemDiscoverableSchedules(null);
    itemSummaryListResponse.setItemViewConfigs(Set.of(itemViewConfigDTO));

    ItemPickupPointQuickEditRequest itemPickupPointQuickEditRequest = new ItemPickupPointQuickEditRequest();
    itemPickupPointQuickEditRequest.setItemSku(ITEM_SKU);
    itemPickupPointQuickEditRequest.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointQuickEditRequest.setStatus(ProductLevel3Status.ONLINE.name());
    CommonUtils.setSchedulesForQuickEditRequest(itemSummaryListResponse, itemPickupPointQuickEditRequest, false);
    Assertions.assertFalse(itemPickupPointQuickEditRequest.isScheduleUpdate());
  }

  @Test
  public void testGetSchedulesUpdatedL5_withScheduleRemovalOnOfflineStatusChange() {
    Map<String, Set<String>> scheduleUpdatedL5s = new HashMap<>();
    Date oldDate = new Date(System.currentTimeMillis() - 10000);
    Date newDate = new Date();
    BuyableScheduleRequest buyableScheduleRequest = BuyableScheduleRequest.builder()
      .startDateTime(newDate)
      .endDateTime(newDate)
      .build();
    DiscoverableScheduleRequest discoverableScheduleRequest = DiscoverableScheduleRequest.builder()
      .startDateTime(newDate)
      .endDateTime(newDate)
      .build();

    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setItemSku(ITEM_SKU);
    itemPickupPointRequest.setPickupPointId(PICKUP_POINT_CODE);
    itemPickupPointRequest.setBuyableSchedule(buyableScheduleRequest);
    itemPickupPointRequest.setDiscoverableSchedule(discoverableScheduleRequest);
    itemPickupPointRequest.setBuyable(true);
    itemPickupPointRequest.setDisplay(true);

    BuyableScheduleResponse buyableScheduleResponse = new BuyableScheduleResponse();
    buyableScheduleResponse.setStartDateTime(oldDate);
    buyableScheduleResponse.setEndDateTime(oldDate);
    DiscoverableScheduleResponse discoverableScheduleResponse = new DiscoverableScheduleResponse();
    discoverableScheduleResponse.setStartDateTime(oldDate);
    discoverableScheduleResponse.setEndDateTime(oldDate);

    ViewConfigResponse viewConfigResponse = new ViewConfigResponse();
    viewConfigResponse.setBuyableScheduleResponse(buyableScheduleResponse);
    viewConfigResponse.setDiscoverableScheduleResponse(discoverableScheduleResponse);
    ItemPickupPointListingResponse itemPickupPointListingResponse = new ItemPickupPointListingResponse();
    itemPickupPointListingResponse.setViewConfigs(Collections.singletonList(viewConfigResponse));

    CommonUtils.getSchedulesUpdatedL5(scheduleUpdatedL5s, itemPickupPointListingResponse, itemPickupPointRequest,
      true, false);

    Assertions.assertTrue(itemPickupPointRequest.isScheduleRemoval());
    Assertions.assertTrue(scheduleUpdatedL5s.containsKey(L5_CODE));
    Assertions.assertTrue(scheduleUpdatedL5s.get(L5_CODE).contains(Constants.BUYABLE_SCHEDULE_UPDATE));
    Assertions.assertTrue(scheduleUpdatedL5s.get(L5_CODE).contains(Constants.DISCOVERABLE_SCHEDULE_UPDATE));
  }

  @Test
  public void testGetSchedulesUpdatedL5_withScheduleRemovalOnOnlineStatusAndNoActiveSchedulesChange() {
    Map<String, Set<String>> scheduleUpdatedL5s = new HashMap<>();
    Date oldDate = new Date(System.currentTimeMillis() - 10000);
    Date newDate = new Date();
    BuyableScheduleRequest buyableScheduleRequest = BuyableScheduleRequest.builder()
      .startDateTime(newDate)
      .endDateTime(newDate)
      .build();
    DiscoverableScheduleRequest discoverableScheduleRequest = DiscoverableScheduleRequest.builder()
      .startDateTime(newDate)
      .endDateTime(newDate)
      .build();

    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setItemSku(ITEM_SKU);
    itemPickupPointRequest.setPickupPointId(PICKUP_POINT_CODE);
    itemPickupPointRequest.setBuyableSchedule(buyableScheduleRequest);
    itemPickupPointRequest.setDiscoverableSchedule(discoverableScheduleRequest);
    itemPickupPointRequest.setBuyable(true);
    itemPickupPointRequest.setDisplay(false);
    ViewConfigResponse viewConfigResponse = new ViewConfigResponse();
    ItemPickupPointListingResponse itemPickupPointListingResponse = new ItemPickupPointListingResponse();
    itemPickupPointListingResponse.setViewConfigs(Collections.singletonList(viewConfigResponse));

    CommonUtils.getSchedulesUpdatedL5(scheduleUpdatedL5s, itemPickupPointListingResponse, itemPickupPointRequest,
      true, false);
    Assertions.assertFalse(itemPickupPointRequest.isScheduleRemoval());
  }

  @Test
  public void testGetSchedulesUpdatedL5_withScheduleRemovalOnBuyableStatusChange() {
    Map<String, Set<String>> scheduleUpdatedL5s = new HashMap<>();
    Date oldDate = new Date(System.currentTimeMillis() - 10000);
    Date newDate = new Date();
    BuyableScheduleRequest buyableScheduleRequest = BuyableScheduleRequest.builder()
      .startDateTime(newDate)
      .endDateTime(newDate)
      .build();
    DiscoverableScheduleRequest discoverableScheduleRequest = DiscoverableScheduleRequest.builder()
      .startDateTime(newDate)
      .endDateTime(newDate)
      .build();

    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setItemSku(ITEM_SKU);
    itemPickupPointRequest.setPickupPointId(PICKUP_POINT_CODE);
    itemPickupPointRequest.setBuyableSchedule(buyableScheduleRequest);
    itemPickupPointRequest.setDiscoverableSchedule(discoverableScheduleRequest);
    itemPickupPointRequest.setBuyable(true);
    itemPickupPointRequest.setDisplay(false);

    BuyableScheduleResponse buyableScheduleResponse = new BuyableScheduleResponse();
    buyableScheduleResponse.setStartDateTime(oldDate);
    buyableScheduleResponse.setEndDateTime(oldDate);
    DiscoverableScheduleResponse discoverableScheduleResponse = new DiscoverableScheduleResponse();
    discoverableScheduleResponse.setStartDateTime(oldDate);
    discoverableScheduleResponse.setEndDateTime(oldDate);

    ViewConfigResponse viewConfigResponse = new ViewConfigResponse();
    viewConfigResponse.setBuyableScheduleResponse(buyableScheduleResponse);
    viewConfigResponse.setDiscoverableScheduleResponse(discoverableScheduleResponse);
    ItemPickupPointListingResponse itemPickupPointListingResponse = new ItemPickupPointListingResponse();
    itemPickupPointListingResponse.setViewConfigs(Collections.singletonList(viewConfigResponse));

    CommonUtils.getSchedulesUpdatedL5(scheduleUpdatedL5s, itemPickupPointListingResponse, itemPickupPointRequest,
      true, false);

    Assertions.assertTrue(itemPickupPointRequest.isScheduleRemoval());
    Assertions.assertTrue(scheduleUpdatedL5s.containsKey(L5_CODE));
    Assertions.assertTrue(scheduleUpdatedL5s.get(L5_CODE).contains(Constants.BUYABLE_SCHEDULE_UPDATE));
    Assertions.assertTrue(scheduleUpdatedL5s.get(L5_CODE).contains(Constants.DISCOVERABLE_SCHEDULE_UPDATE));
  }

  @Test
  public void testGetSchedulesUpdatedL5_withScheduleRemovalOnDisplayableStatusChange() {
    Map<String, Set<String>> scheduleUpdatedL5s = new HashMap<>();
    Date oldDate = new Date(System.currentTimeMillis() - 10000);
    Date newDate = new Date();
    BuyableScheduleRequest buyableScheduleRequest = BuyableScheduleRequest.builder()
      .startDateTime(newDate)
      .endDateTime(newDate)
      .build();
    DiscoverableScheduleRequest discoverableScheduleRequest = DiscoverableScheduleRequest.builder()
      .startDateTime(newDate)
      .endDateTime(newDate)
      .build();

    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setItemSku(ITEM_SKU);
    itemPickupPointRequest.setPickupPointId(PICKUP_POINT_CODE);
    itemPickupPointRequest.setBuyableSchedule(buyableScheduleRequest);
    itemPickupPointRequest.setDiscoverableSchedule(discoverableScheduleRequest);
    itemPickupPointRequest.setBuyable(false);
    itemPickupPointRequest.setDisplay(true);

    BuyableScheduleResponse buyableScheduleResponse = new BuyableScheduleResponse();
    buyableScheduleResponse.setStartDateTime(oldDate);
    buyableScheduleResponse.setEndDateTime(oldDate);
    DiscoverableScheduleResponse discoverableScheduleResponse = new DiscoverableScheduleResponse();
    discoverableScheduleResponse.setStartDateTime(oldDate);
    discoverableScheduleResponse.setEndDateTime(oldDate);

    ViewConfigResponse viewConfigResponse = new ViewConfigResponse();
    viewConfigResponse.setBuyableScheduleResponse(buyableScheduleResponse);
    viewConfigResponse.setDiscoverableScheduleResponse(discoverableScheduleResponse);
    ItemPickupPointListingResponse itemPickupPointListingResponse = new ItemPickupPointListingResponse();
    itemPickupPointListingResponse.setViewConfigs(Collections.singletonList(viewConfigResponse));

    CommonUtils.getSchedulesUpdatedL5(scheduleUpdatedL5s, itemPickupPointListingResponse, itemPickupPointRequest,
      true, false);

    Assertions.assertTrue(itemPickupPointRequest.isScheduleRemoval());
    Assertions.assertTrue(scheduleUpdatedL5s.containsKey(L5_CODE));
    Assertions.assertTrue(scheduleUpdatedL5s.get(L5_CODE).contains(Constants.BUYABLE_SCHEDULE_UPDATE));
    Assertions.assertTrue(scheduleUpdatedL5s.get(L5_CODE).contains(Constants.DISCOVERABLE_SCHEDULE_UPDATE));
  }

  @Test
  public void testPopulateL5WithActiveSchedules_EmptyMap() {
    Set<String> l5sWithActiveSchedules = new HashSet<>();
    Map<String, ItemPickupPointListingResponse> savedItemPickupPointDataMap = new HashMap<>();

    CommonUtils.populateL5WithActiveSchedules(l5sWithActiveSchedules, savedItemPickupPointDataMap);

    Assertions.assertTrue(l5sWithActiveSchedules.isEmpty());
  }

  @Test
  public void testPopulateL5WithActiveSchedulesNullValueMap() {
    Set<String> l5sWithActiveSchedules = new HashSet<>();
    Map<String, ItemPickupPointListingResponse> savedItemPickupPointDataMap = new HashMap<>();
    savedItemPickupPointDataMap.put(PICKUP_POINT_CODE, null);
    CommonUtils.populateL5WithActiveSchedules(l5sWithActiveSchedules, savedItemPickupPointDataMap);

    Assertions.assertTrue(l5sWithActiveSchedules.isEmpty());
  }

  @Test
  public void testPopulateL5WithActiveSchedules_NoActiveSchedules() {
    Set<String> l5sWithActiveSchedules = new HashSet<>();
    Map<String, ItemPickupPointListingResponse> savedItemPickupPointDataMap = new HashMap<>();

    List<ViewConfigResponse> viewConfigResponses = Collections.singletonList(
      ViewConfigResponse.builder().channelId(Constants.DEFAULT).buyableScheduleResponse(
        BuyableScheduleResponse.builder().endDateTime(new Date(System.currentTimeMillis() - 10000))
          .build()).discoverableScheduleResponse(DiscoverableScheduleResponse.builder()
        .endDateTime(new Date(System.currentTimeMillis() - 10000)).build()).build());

    ItemPickupPointListingResponse itemPickupPointListingResponse =
      new ItemPickupPointListingResponse();
      itemPickupPointListingResponse.setViewConfigs(viewConfigResponses);
      itemPickupPointListingResponse.setItemSku(ITEM_SKU);
      itemPickupPointListingResponse.setPickUpPointCode(PICKUP_POINT_CODE);
    savedItemPickupPointDataMap.put(ITEM_SKU.concat(Constants.HYPHEN).concat(PICKUP_POINT_CODE),
      itemPickupPointListingResponse);
    CommonUtils.populateL5WithActiveSchedules(l5sWithActiveSchedules, savedItemPickupPointDataMap);
    Assertions.assertTrue(l5sWithActiveSchedules.isEmpty());
  }

  @Test
  public void testPopulateL5WithActiveSchedules_ActiveBuyableSchedule() {
    Set<String> l5sWithActiveSchedules = new HashSet<>();
    Map<String, ItemPickupPointListingResponse> savedItemPickupPointDataMap = new HashMap<>();

    List<ViewConfigResponse> viewConfigResponses = Arrays.asList(
      ViewConfigResponse.builder().channelId(Constants.DEFAULT).buyableScheduleResponse(
        BuyableScheduleResponse.builder().endDateTime(new Date(System.currentTimeMillis() + 10000))
          .build()).build());
    ItemPickupPointListingResponse itemPickupPointListingResponse =
      new ItemPickupPointListingResponse();
    itemPickupPointListingResponse.setViewConfigs(viewConfigResponses);
    itemPickupPointListingResponse.setItemSku(ITEM_SKU);
    itemPickupPointListingResponse.setPickUpPointCode(PICKUP_POINT_CODE);
    savedItemPickupPointDataMap.put(ITEM_SKU.concat(Constants.HYPHEN).concat(PICKUP_POINT_CODE),
      itemPickupPointListingResponse);
    CommonUtils.populateL5WithActiveSchedules(l5sWithActiveSchedules, savedItemPickupPointDataMap);
    Assertions.assertTrue(l5sWithActiveSchedules.contains(ITEM_SKU.concat(Constants.HYPHEN).concat(PICKUP_POINT_CODE)));
  }

  @Test
  public void testPopulateL5WithActiveSchedules_ActiveBuyableAndDiscoverableSchedules() {
    Set<String> l5sWithActiveSchedules = new HashSet<>();
    Map<String, ItemPickupPointListingResponse> savedItemPickupPointDataMap = new HashMap<>();

    List<ViewConfigResponse> viewConfigResponses = Collections.singletonList(
      ViewConfigResponse.builder().channelId(Constants.DEFAULT).buyableScheduleResponse(
        BuyableScheduleResponse.builder().endDateTime(new Date(System.currentTimeMillis() + 10000))
          .build()).discoverableScheduleResponse(DiscoverableScheduleResponse.builder()
        .endDateTime(new Date(System.currentTimeMillis() + 10000)).build()).build());
    ItemPickupPointListingResponse itemPickupPointListingResponse =
      new ItemPickupPointListingResponse();
    itemPickupPointListingResponse.setViewConfigs(viewConfigResponses);
    itemPickupPointListingResponse.setItemSku(ITEM_SKU);
    itemPickupPointListingResponse.setPickUpPointCode(PICKUP_POINT_CODE);
    savedItemPickupPointDataMap.put(ITEM_SKU.concat(Constants.HYPHEN).concat(PICKUP_POINT_CODE),
      itemPickupPointListingResponse);

    CommonUtils.populateL5WithActiveSchedules(l5sWithActiveSchedules, savedItemPickupPointDataMap);

    Assertions.assertTrue(l5sWithActiveSchedules.contains(ITEM_SKU.concat(Constants.HYPHEN).concat(PICKUP_POINT_CODE)));
  }

  @Test
  public void testPopulateL5WithActiveSchedules_NonDefaultChannelId() {
    Set<String> l5sWithActiveSchedules = new HashSet<>();
    Map<String, ItemPickupPointListingResponse> savedItemPickupPointDataMap = new HashMap<>();

    List<ViewConfigResponse> viewConfigResponses = Collections.singletonList(
      ViewConfigResponse.builder().channelId(Constants.B2B_CHANNEL).buyableScheduleResponse(
        BuyableScheduleResponse.builder().endDateTime(new Date(System.currentTimeMillis() + 10000))
          .build()).discoverableScheduleResponse(DiscoverableScheduleResponse.builder()
        .endDateTime(new Date(System.currentTimeMillis() + 10000)).build()).build());
    ItemPickupPointListingResponse itemPickupPointListingResponse =
      new ItemPickupPointListingResponse();
    itemPickupPointListingResponse.setViewConfigs(viewConfigResponses);
    itemPickupPointListingResponse.setItemSku(ITEM_SKU);
    itemPickupPointListingResponse.setPickUpPointCode(PICKUP_POINT_CODE);
    savedItemPickupPointDataMap.put(ITEM_SKU.concat(Constants.HYPHEN).concat(PICKUP_POINT_CODE),
      itemPickupPointListingResponse);

    CommonUtils.populateL5WithActiveSchedules(l5sWithActiveSchedules, savedItemPickupPointDataMap);

    Assertions.assertFalse(l5sWithActiveSchedules.contains(ITEM_SKU.concat(Constants.HYPHEN).concat(PICKUP_POINT_CODE)));
  }


  @Test
  public void getMerchantTypeFromProfileResponseTest() {
    ProfileResponse profileResponse = new ProfileResponse();
    Assertions.assertEquals(StringUtils.EMPTY, CommonUtils.getMerchantTypeFromProfileResponse(null));
    Assertions.assertEquals(StringUtils.EMPTY, CommonUtils.getMerchantTypeFromProfileResponse(profileResponse));
    profileResponse.setCompany(new CompanyDTO());
    Assertions.assertEquals(StringUtils.EMPTY, CommonUtils.getMerchantTypeFromProfileResponse(profileResponse));
    profileResponse.getCompany().setMerchantType(MERCHANT_CODE);
    Assertions.assertEquals(MERCHANT_CODE, CommonUtils.getMerchantTypeFromProfileResponse(profileResponse));
  }


  @Test
  public void validateCncUpdateForBopisTest() {
    QuickEditV2Request quickEditV2Request = new QuickEditV2Request();
    itemSummaryListResponse.setProductType(null);
    itemSummaryListResponse.setDimensionsMissing(null);
    Assertions.assertFalse(CommonUtils.validateCncUpdateForBopis(quickEditV2Request, itemSummaryListResponse, false, false));
    quickEditV2Request.setCncActive(true);
    Assertions.assertFalse(CommonUtils.validateCncUpdateForBopis(quickEditV2Request, itemSummaryListResponse, false, false));
    Assertions.assertFalse(CommonUtils.validateCncUpdateForBopis(quickEditV2Request, itemSummaryListResponse, true, false));
    itemSummaryListResponse.setProductType(ProductType.BOPIS);
    Assertions.assertTrue(CommonUtils.validateCncUpdateForBopis(quickEditV2Request, itemSummaryListResponse, true, false));
  }

  @Test
  public void validateStatusUpdateForBopisTest() {
    QuickEditV2Request quickEditV2Request = new QuickEditV2Request();
    itemSummaryListResponse.setProductType(null);
    itemSummaryListResponse.setDimensionsMissing(null);
    List<String> bopisUnsupportedMerchantTypes = new ArrayList<>();
    MerchantType merchantType = MerchantType.CC;
    Assertions.assertFalse(
        CommonUtils.validateStatusUpdateForBopis(quickEditV2Request, merchantType.name(), itemSummaryListResponse,
            false, bopisUnsupportedMerchantTypes));
    quickEditV2Request.setStatus(ProductLevel3Status.ONLINE);
    Assertions.assertFalse(
        CommonUtils.validateStatusUpdateForBopis(quickEditV2Request, merchantType.name(), itemSummaryListResponse,
            false, bopisUnsupportedMerchantTypes));
    Assertions.assertFalse(
        CommonUtils.validateStatusUpdateForBopis(quickEditV2Request, merchantType.name(), itemSummaryListResponse, true,
            bopisUnsupportedMerchantTypes));
    bopisUnsupportedMerchantTypes.add(MerchantType.CC.name());
    Assertions.assertFalse(
        CommonUtils.validateStatusUpdateForBopis(quickEditV2Request, merchantType.name(), itemSummaryListResponse, true,
            bopisUnsupportedMerchantTypes));
    itemSummaryListResponse.setDimensionsMissing(true);
    Assertions.assertTrue(
        CommonUtils.validateStatusUpdateForBopis(quickEditV2Request, merchantType.name(), itemSummaryListResponse,
            true, bopisUnsupportedMerchantTypes));
  }

  @Test
  public void validateShippingTypeAndDimensionMissingForBopisTest() {
    itemSummaryListResponse.setProductType(null);
    itemSummaryListResponse.setDimensionsMissing(null);
    Assertions.assertFalse(CommonUtils.validateShippingTypeAndDimensionMissingForBopis(false, itemSummaryListResponse));
    Assertions.assertFalse(CommonUtils.validateShippingTypeAndDimensionMissingForBopis(true, itemSummaryListResponse));
    itemSummaryListResponse.setProductType(ProductType.BOPIS);
    Assertions.assertFalse(CommonUtils.validateShippingTypeAndDimensionMissingForBopis(true, itemSummaryListResponse));
    itemSummaryListResponse.setDimensionsMissing(true);
    Assertions.assertTrue(CommonUtils.validateShippingTypeAndDimensionMissingForBopis(true, itemSummaryListResponse));
  }

  @Test
  public void validateShippingTypeAndDimensionMissingForBopis_AllCombinationsTest() {
    ItemSummaryListResponse itemSummaryListResponse = new ItemSummaryListResponse();
    itemSummaryListResponse.setProductType(null);
    itemSummaryListResponse.setDimensionsMissing(null);
    Assertions.assertFalse(
        CommonUtils.validateShippingTypeAndDimensionMissingForBopis(false, itemSummaryListResponse));
    Assertions.assertFalse(
        CommonUtils.validateShippingTypeAndDimensionMissingForBopis(false, itemSummaryListResponse));
    Assertions.assertFalse(
        CommonUtils.validateShippingTypeAndDimensionMissingForBopis(true, itemSummaryListResponse));
    Assertions.assertFalse(
        CommonUtils.validateShippingTypeAndDimensionMissingForBopis(true, itemSummaryListResponse));
    itemSummaryListResponse.setProductType(ProductType.BOPIS);
    Assertions.assertFalse(
        CommonUtils.validateShippingTypeAndDimensionMissingForBopis(true, itemSummaryListResponse));
    Assertions.assertFalse(
        CommonUtils.validateShippingTypeAndDimensionMissingForBopis(true, itemSummaryListResponse));
    itemSummaryListResponse.setDimensionsMissing(true);
    Assertions.assertTrue(
        CommonUtils.validateShippingTypeAndDimensionMissingForBopis(true, itemSummaryListResponse));
    Assertions.assertTrue(CommonUtils.validateShippingTypeAndDimensionMissingForBopis(true, itemSummaryListResponse));
    Assertions.assertFalse(
        CommonUtils.validateShippingTypeAndDimensionMissingForBopis(false, itemSummaryListResponse));
    Assertions.assertFalse(
        CommonUtils.validateShippingTypeAndDimensionMissingForBopis(false, itemSummaryListResponse));
    itemSummaryListResponse.setDimensionsMissing(false);
    Assertions.assertFalse(
        CommonUtils.validateShippingTypeAndDimensionMissingForBopis(true, itemSummaryListResponse));
    Assertions.assertFalse(
        CommonUtils.validateShippingTypeAndDimensionMissingForBopis(true, itemSummaryListResponse));
  }

  @Test
  public void testPopulateHistoryAuditMapForSchedulesUpdate_ActiveSchedules() {
    // Setup
    Map<String, ItemPickupPointListingResponse> l5CodeAndResponseMap = new HashMap<>();
    List<Map<String, String>> historyAuditMap = new ArrayList<>();
    FbbAndCncDataChangeDto fbbAndCncDataChangeDto = new FbbAndCncDataChangeDto();
    ItemPickupPointRequest modifiedItemPickupPoint = new ItemPickupPointRequest();
    modifiedItemPickupPoint.setItemSku(ITEM_SKU);
    modifiedItemPickupPoint.setPickupPointId(PICKUP_POINT_CODE);
    modifiedItemPickupPoint.setBuyable(false);
    modifiedItemPickupPoint.setDisplay(true);

    Date now = new Date();
    ViewConfigResponse viewConfigResponse = ViewConfigResponse.builder()
      .channelId(Constants.DEFAULT)
      .buyableScheduleResponse(BuyableScheduleResponse.builder()
        .endDateTime(new Date(now.getTime() + 10000))
        .build())
      .discoverableScheduleResponse(DiscoverableScheduleResponse.builder()
        .endDateTime(new Date(now.getTime() + 10000))
        .build())
      .build();

    ItemPickupPointListingResponse itemPickupPointListingResponse = new ItemPickupPointListingResponse();
    itemPickupPointListingResponse.setViewConfigs(Collections.singletonList(viewConfigResponse));
    itemPickupPointListingResponse.setItemSku(ITEM_SKU);
    itemPickupPointListingResponse.setPickUpPointCode(PICKUP_POINT_CODE);
    l5CodeAndResponseMap.put(ITEM_SKU.concat(Constants.HYPHEN).concat(PICKUP_POINT_CODE),
      itemPickupPointListingResponse);
    CommonUtils.populateHistoryAuditMapForSchedulesUpdate(l5CodeAndResponseMap, true, historyAuditMap,
        modifiedItemPickupPoint, fbbAndCncDataChangeDto, true, false);
    Assertions.assertEquals(2, historyAuditMap.size());
  }

  @Test
  public void testPopulateHistoryAuditMapForSchedulesUpdate_NoSchedules() {
    // Setup
    Map<String, ItemPickupPointListingResponse> l5CodeAndResponseMap = new HashMap<>();
    List<Map<String, String>> historyAuditMap = new ArrayList<>();
    FbbAndCncDataChangeDto fbbAndCncDataChangeDto = new FbbAndCncDataChangeDto();
    ItemPickupPointRequest modifiedItemPickupPoint = new ItemPickupPointRequest();
    modifiedItemPickupPoint.setItemSku(ITEM_SKU);
    modifiedItemPickupPoint.setPickupPointId(PICKUP_POINT_CODE);
    modifiedItemPickupPoint.setBuyable(false);
    modifiedItemPickupPoint.setDisplay(true);

    Date now = new Date();
    ViewConfigResponse viewConfigResponse = ViewConfigResponse.builder()
      .channelId(Constants.DEFAULT)
      .buyableScheduleResponse(null)
      .discoverableScheduleResponse(null)
      .build();

    ItemPickupPointListingResponse itemPickupPointListingResponse = new ItemPickupPointListingResponse();
    itemPickupPointListingResponse.setViewConfigs(Collections.singletonList(viewConfigResponse));
    itemPickupPointListingResponse.setItemSku(ITEM_SKU);
    itemPickupPointListingResponse.setPickUpPointCode(PICKUP_POINT_CODE);
    l5CodeAndResponseMap.put(ITEM_SKU.concat(Constants.HYPHEN).concat(PICKUP_POINT_CODE),
      itemPickupPointListingResponse);
    CommonUtils.populateHistoryAuditMapForSchedulesUpdate(
      l5CodeAndResponseMap, true, historyAuditMap, modifiedItemPickupPoint, fbbAndCncDataChangeDto, true, false);
    Assertions.assertTrue(CollectionUtils.isEmpty(historyAuditMap));
  }

  @Test
  public void testPopulateHistoryAuditMapForSchedulesUpdate_ActiveSchedulesBuyable() {
    Map<String, ItemPickupPointListingResponse> l5CodeAndResponseMap = new HashMap<>();
    List<Map<String, String>> historyAuditMap = new ArrayList<>();
    FbbAndCncDataChangeDto fbbAndCncDataChangeDto = new FbbAndCncDataChangeDto();
    ItemPickupPointRequest modifiedItemPickupPoint = new ItemPickupPointRequest();
    modifiedItemPickupPoint.setItemSku(ITEM_SKU);
    modifiedItemPickupPoint.setPickupPointId(PICKUP_POINT_CODE);
    modifiedItemPickupPoint.setBuyable(true);
    modifiedItemPickupPoint.setDisplay(false);

    Date now = new Date();
    ViewConfigResponse viewConfigResponse = ViewConfigResponse.builder()
      .channelId(Constants.DEFAULT)
      .buyableScheduleResponse(BuyableScheduleResponse.builder()
        .endDateTime(new Date(now.getTime() + 10000))
        .build())
      .discoverableScheduleResponse(DiscoverableScheduleResponse.builder()
        .endDateTime(new Date(now.getTime() + 10000))
        .build())
      .build();

    ItemPickupPointListingResponse itemPickupPointListingResponse = new ItemPickupPointListingResponse();
    itemPickupPointListingResponse.setViewConfigs(Collections.singletonList(viewConfigResponse));
    itemPickupPointListingResponse.setItemSku(ITEM_SKU);
    itemPickupPointListingResponse.setPickUpPointCode(PICKUP_POINT_CODE);
    l5CodeAndResponseMap.put(ITEM_SKU.concat(Constants.HYPHEN).concat(PICKUP_POINT_CODE),
      itemPickupPointListingResponse);
    CommonUtils.populateHistoryAuditMapForSchedulesUpdate(
      l5CodeAndResponseMap, true, historyAuditMap, modifiedItemPickupPoint, fbbAndCncDataChangeDto, true, false);
    Assertions.assertEquals(2, historyAuditMap.size());
  }

  @Test
  public void testPopulateHistoryAuditMapForSchedulesUpdate_ActiveSchedulesNonBuyable() {
    Map<String, ItemPickupPointListingResponse> l5CodeAndResponseMap = new HashMap<>();
    List<Map<String, String>> historyAuditMap = new ArrayList<>();
    FbbAndCncDataChangeDto fbbAndCncDataChangeDto = new FbbAndCncDataChangeDto();
    ItemPickupPointRequest modifiedItemPickupPoint = new ItemPickupPointRequest();
    modifiedItemPickupPoint.setItemSku(ITEM_SKU);
    modifiedItemPickupPoint.setPickupPointId(PICKUP_POINT_CODE);
    modifiedItemPickupPoint.setBuyable(false);
    modifiedItemPickupPoint.setDisplay(false);

    Date now = new Date();
    ViewConfigResponse viewConfigResponse = ViewConfigResponse.builder()
      .channelId(Constants.DEFAULT)
      .buyableScheduleResponse(BuyableScheduleResponse.builder()
        .endDateTime(new Date(now.getTime() + 10000))
        .build())
      .discoverableScheduleResponse(DiscoverableScheduleResponse.builder()
        .endDateTime(new Date(now.getTime() + 10000))
        .build())
      .build();

    ItemPickupPointListingResponse itemPickupPointListingResponse = new ItemPickupPointListingResponse();
    itemPickupPointListingResponse.setViewConfigs(Collections.singletonList(viewConfigResponse));
    itemPickupPointListingResponse.setItemSku(ITEM_SKU);
    itemPickupPointListingResponse.setPickUpPointCode(PICKUP_POINT_CODE);
    l5CodeAndResponseMap.put(ITEM_SKU.concat(Constants.HYPHEN).concat(PICKUP_POINT_CODE),
      itemPickupPointListingResponse);
    CommonUtils.populateHistoryAuditMapForSchedulesUpdate(
      l5CodeAndResponseMap, true, historyAuditMap, modifiedItemPickupPoint, fbbAndCncDataChangeDto, true, false);
    Assertions.assertEquals(0, historyAuditMap.size());

  }



  @Test
  public void testPopulateHistoryAuditMapForSchedulesUpdate_ActiveSchedulesButSwitchOff() {
    // Setup
    Map<String, ItemPickupPointListingResponse> l5CodeAndResponseMap = new HashMap<>();
    List<Map<String, String>> historyAuditMap = new ArrayList<>();
    FbbAndCncDataChangeDto fbbAndCncDataChangeDto = new FbbAndCncDataChangeDto();
    ItemPickupPointRequest modifiedItemPickupPoint = new ItemPickupPointRequest();
    modifiedItemPickupPoint.setItemSku(ITEM_SKU);
    modifiedItemPickupPoint.setPickupPointId(PICKUP_POINT_CODE);
    modifiedItemPickupPoint.setBuyable(true);
    modifiedItemPickupPoint.setDisplay(true);

    Date now = new Date();
    ViewConfigResponse viewConfigResponse = ViewConfigResponse.builder()
      .channelId(Constants.DEFAULT)
      .buyableScheduleResponse(BuyableScheduleResponse.builder()
        .endDateTime(new Date(now.getTime() + 10000))
        .build())
      .discoverableScheduleResponse(DiscoverableScheduleResponse.builder()
        .endDateTime(new Date(now.getTime() + 10000))
        .build())
      .build();

    ItemPickupPointListingResponse itemPickupPointListingResponse = new ItemPickupPointListingResponse();
    itemPickupPointListingResponse.setViewConfigs(Collections.singletonList(viewConfigResponse));
    itemPickupPointListingResponse.setItemSku(ITEM_SKU);
    itemPickupPointListingResponse.setPickUpPointCode(PICKUP_POINT_CODE);
    l5CodeAndResponseMap.put(ITEM_SKU.concat(Constants.HYPHEN).concat(PICKUP_POINT_CODE),
      itemPickupPointListingResponse);
    CommonUtils.populateHistoryAuditMapForSchedulesUpdate(
      l5CodeAndResponseMap, false, historyAuditMap, modifiedItemPickupPoint, fbbAndCncDataChangeDto,
      true, false);
    Assertions.assertEquals(0, historyAuditMap.size());
  }


  @Test
  public void testPopulateHistoryAuditMapForSchedulesUpdate_ActiveSchedulesNonMpp() {
    // Setup
    Map<String, ItemPickupPointListingResponse> l5CodeAndResponseMap = new HashMap<>();
    List<Map<String, String>> historyAuditMap = new ArrayList<>();
    FbbAndCncDataChangeDto fbbAndCncDataChangeDto = new FbbAndCncDataChangeDto();
    ItemPickupPointRequest modifiedItemPickupPoint = new ItemPickupPointRequest();
    modifiedItemPickupPoint.setItemSku(ITEM_SKU);
    modifiedItemPickupPoint.setPickupPointId(PICKUP_POINT_CODE);
    modifiedItemPickupPoint.setBuyable(true);
    modifiedItemPickupPoint.setDisplay(true);

    Date now = new Date();
    ViewConfigResponse viewConfigResponse = ViewConfigResponse.builder()
      .channelId(Constants.DEFAULT)
      .buyableScheduleResponse(BuyableScheduleResponse.builder()
        .endDateTime(new Date(now.getTime() + 10000))
        .build())
      .discoverableScheduleResponse(DiscoverableScheduleResponse.builder()
        .endDateTime(new Date(now.getTime() + 10000))
        .build())
      .build();

    ItemPickupPointListingResponse itemPickupPointListingResponse = new ItemPickupPointListingResponse();
    itemPickupPointListingResponse.setViewConfigs(Collections.singletonList(viewConfigResponse));
    itemPickupPointListingResponse.setItemSku(ITEM_SKU);
    itemPickupPointListingResponse.setPickUpPointCode(PICKUP_POINT_CODE);
    l5CodeAndResponseMap.put(ITEM_SKU.concat(Constants.HYPHEN).concat(PICKUP_POINT_CODE),
      itemPickupPointListingResponse);
    CommonUtils.populateHistoryAuditMapForSchedulesUpdate(
      l5CodeAndResponseMap, true, historyAuditMap, modifiedItemPickupPoint,
      fbbAndCncDataChangeDto, false, false);
    Assertions.assertEquals(0, historyAuditMap.size());
  }

  @Test
  public void testPopulateHistoryAuditMapForSchedulesUpdate_InactiveSchedules() {
    Map<String, ItemPickupPointListingResponse> l5CodeAndResponseMap = new HashMap<>();
    List<Map<String, String>> historyAuditMap = new ArrayList<>();
    FbbAndCncDataChangeDto fbbAndCncDataChangeDto = new FbbAndCncDataChangeDto();
    ItemPickupPointRequest modifiedItemPickupPoint = new ItemPickupPointRequest();
    modifiedItemPickupPoint.setItemSku(ITEM_SKU);
    modifiedItemPickupPoint.setPickupPointId(PICKUP_POINT_CODE);
    modifiedItemPickupPoint.setBuyable(true);
    modifiedItemPickupPoint.setDisplay(true);

    Date now = new Date();
    ViewConfigResponse viewConfigResponse = ViewConfigResponse.builder()
      .channelId(Constants.DEFAULT)
      .buyableScheduleResponse(BuyableScheduleResponse.builder()
        .endDateTime(new Date(now.getTime() - 10000))
        .build())
      .discoverableScheduleResponse(DiscoverableScheduleResponse.builder()
        .endDateTime(new Date(now.getTime() - 10000))
        .build())
      .build();

    ItemPickupPointListingResponse itemPickupPointListingResponse = new ItemPickupPointListingResponse();
    itemPickupPointListingResponse.setViewConfigs(Collections.singletonList(viewConfigResponse));
    itemPickupPointListingResponse.setItemSku(ITEM_SKU);
    itemPickupPointListingResponse.setPickUpPointCode(PICKUP_POINT_CODE);

    l5CodeAndResponseMap.put(ITEM_SKU.concat(Constants.HYPHEN).concat(PICKUP_POINT_CODE),
      itemPickupPointListingResponse);
    CommonUtils.populateHistoryAuditMapForSchedulesUpdate(
      l5CodeAndResponseMap, true, historyAuditMap, modifiedItemPickupPoint, fbbAndCncDataChangeDto, true, false);
    Assertions.assertTrue(historyAuditMap.isEmpty());
  }

  @Test
  public void testPopulateHistoryAuditMapForSchedulesUpdate_NonDefaultChannelId() {
    Map<String, ItemPickupPointListingResponse> l5CodeAndResponseMap = new HashMap<>();
    List<Map<String, String>> historyAuditMap = new ArrayList<>();
    FbbAndCncDataChangeDto fbbAndCncDataChangeDto = new FbbAndCncDataChangeDto();
    ItemPickupPointRequest modifiedItemPickupPoint = new ItemPickupPointRequest();
    modifiedItemPickupPoint.setItemSku(ITEM_SKU);
    modifiedItemPickupPoint.setPickupPointId(PICKUP_POINT_CODE);
    modifiedItemPickupPoint.setBuyable(true);
    modifiedItemPickupPoint.setDisplay(true);
    Date now = new Date();
    ViewConfigResponse viewConfigResponse = ViewConfigResponse.builder()
      .channelId(Constants.B2B_CHANNEL)
      .buyableScheduleResponse(BuyableScheduleResponse.builder()
        .endDateTime(new Date(now.getTime() + 10000))
        .build())
      .discoverableScheduleResponse(DiscoverableScheduleResponse.builder()
        .endDateTime(new Date(now.getTime() + 10000))
        .build())
      .build();

    ItemPickupPointListingResponse itemPickupPointListingResponse = new ItemPickupPointListingResponse();
    itemPickupPointListingResponse.setViewConfigs(Collections.singletonList(viewConfigResponse));
    itemPickupPointListingResponse.setItemSku(ITEM_SKU);
    itemPickupPointListingResponse.setPickUpPointCode(PICKUP_POINT_CODE);

    l5CodeAndResponseMap.put(ITEM_SKU.concat(Constants.HYPHEN).concat(PICKUP_POINT_CODE),
      itemPickupPointListingResponse);

    // Action
    CommonUtils.populateHistoryAuditMapForSchedulesUpdate(
      l5CodeAndResponseMap, true, historyAuditMap, modifiedItemPickupPoint, fbbAndCncDataChangeDto, true, false);

    // Assert
    Assertions.assertTrue(historyAuditMap.isEmpty());
  }

  @Test
  public void testPopulateHistoryAuditMapForSchedulesUpdate_NoResponseFound() {
    // Setup
    Map<String, ItemPickupPointListingResponse> l5CodeAndResponseMap = new HashMap<>();
    List<Map<String, String>> historyAuditMap = new ArrayList<>();
    FbbAndCncDataChangeDto fbbAndCncDataChangeDto = new FbbAndCncDataChangeDto();
    ItemPickupPointRequest modifiedItemPickupPoint = new ItemPickupPointRequest();
    modifiedItemPickupPoint.setItemSku(ITEM_SKU);
    modifiedItemPickupPoint.setPickupPointId(PICKUP_POINT_CODE);
    modifiedItemPickupPoint.setBuyable(true);
    modifiedItemPickupPoint.setDisplay(true);
    CommonUtils.populateHistoryAuditMapForSchedulesUpdate(
      l5CodeAndResponseMap, true, historyAuditMap, modifiedItemPickupPoint, fbbAndCncDataChangeDto, true, false);
    Assertions.assertTrue(historyAuditMap.isEmpty());
  }

  @Test
  public void testMigrateProductAndL5DetailsByProductSku_AllScenarios() {
    List<ProductItemBusinessPartner> productItemBusinessPartners = new ArrayList<>();
    ProductItemBusinessPartner productItemBusinessPartner1 = new ProductItemBusinessPartner();
    ProductItemBusinessPartner productItemBusinessPartner2 = new ProductItemBusinessPartner();
    productItemBusinessPartner1.setProductType(1);
    productItemBusinessPartner1.setCncActive(false);
    productItemBusinessPartner1.setCncActivated(false);
    productItemBusinessPartner1.setBuyable(false);
    productItemBusinessPartner1.setDisplay(false);
    productItemBusinessPartner1.setB2bBuyable(false);
    productItemBusinessPartner1.setB2bDiscoverable(false);
    productItemBusinessPartner2.setProductType(2);
    productItemBusinessPartner2.setCncActive(true);
    productItemBusinessPartner2.setCncActivated(true);
    productItemBusinessPartner2.setBuyable(false);
    productItemBusinessPartner2.setDisplay(false);
    productItemBusinessPartner2.setB2bBuyable(false);
    productItemBusinessPartner2.setB2bDiscoverable(false);
    productItemBusinessPartners.add(productItemBusinessPartner1);
    productItemBusinessPartners.add(productItemBusinessPartner2);

    ProductAndL5MigrationRequest migrationRequest = new ProductAndL5MigrationRequest();
    migrationRequest.setProductType(com.gdn.mta.product.enums.ProductType.REGULAR);
    migrationRequest.setCncActiveAtL3(true);
    migrationRequest.setCncActiveAtL5(true);
    migrationRequest.setBuyable(true);
    migrationRequest.setDiscoverable(true);
    String username = Constants.DEFAULT_USERNAME;
    boolean b2cActivated = true;
    boolean b2bActivated = true;

    CommonUtils.migrateProductAndL5DetailsByProductSku(productItemBusinessPartners, migrationRequest, b2cActivated, b2bActivated, username);
    Assertions.assertTrue(productItemBusinessPartner1.isCncActive());
    Assertions.assertTrue(productItemBusinessPartner1.isCncActivated());
    Assertions.assertTrue(productItemBusinessPartner1.isBuyable());
    Assertions.assertTrue(productItemBusinessPartner1.isDisplay());
    Assertions.assertTrue(productItemBusinessPartner1.isB2bBuyable());
    Assertions.assertTrue(productItemBusinessPartner1.isB2bDiscoverable());
    Assertions.assertEquals(username, productItemBusinessPartner1.getUpdatedBy());
    Assertions.assertTrue(productItemBusinessPartner2.isCncActive());
    Assertions.assertTrue(productItemBusinessPartner2.isCncActivated());
    Assertions.assertTrue(productItemBusinessPartner2.isBuyable());
    Assertions.assertTrue(productItemBusinessPartner2.isDisplay());
    Assertions.assertTrue(productItemBusinessPartner2.isB2bBuyable());
    Assertions.assertTrue(productItemBusinessPartner2.isB2bDiscoverable());
    Assertions.assertEquals(username, productItemBusinessPartner2.getUpdatedBy());
  }

  @Test
  public void testMigrateProductAndL5DetailsByProductSku_NoRequest_NoChanges() {
    // Setup
    List<ProductItemBusinessPartner> productItemBusinessPartners = new ArrayList<>();
    ProductItemBusinessPartner productItemBusinessPartner1 = new ProductItemBusinessPartner();
    productItemBusinessPartner1.setProductType(3);
    productItemBusinessPartner1.setCncActive(false);
    productItemBusinessPartner1.setCncActivated(false);
    productItemBusinessPartner1.setBuyable(false);
    productItemBusinessPartner1.setDisplay(false);
    productItemBusinessPartner1.setB2bBuyable(false);
    productItemBusinessPartner1.setB2bDiscoverable(false);

    productItemBusinessPartners.add(productItemBusinessPartner1);
    ProductAndL5MigrationRequest migrationRequest = null;
    String username = Constants.DEFAULT_USERNAME;
    boolean b2cActivated = false;
    boolean b2bActivated = false;
    CommonUtils.migrateProductAndL5DetailsByProductSku(productItemBusinessPartners, migrationRequest, b2cActivated, b2bActivated, username);
    Assertions.assertEquals(Optional.of(3).get(), productItemBusinessPartner1.getProductType());
    Assertions.assertFalse(productItemBusinessPartner1.isCncActive());
    Assertions.assertFalse(productItemBusinessPartner1.isCncActivated());
    Assertions.assertFalse(productItemBusinessPartner1.isBuyable());
    Assertions.assertFalse(productItemBusinessPartner1.isDisplay());
    Assertions.assertFalse(productItemBusinessPartner1.isB2bBuyable());
    Assertions.assertFalse(productItemBusinessPartner1.isB2bDiscoverable());
    Assertions.assertEquals(username, productItemBusinessPartner1.getUpdatedBy());
  }

  @Test
  public void testMigrateProductAndL5DetailsByProductSku_PartialActivation() {
    List<ProductItemBusinessPartner> productItemBusinessPartners = new ArrayList<>();
    ProductItemBusinessPartner productItemBusinessPartner1 = new ProductItemBusinessPartner();
    productItemBusinessPartner1.setProductType(2);
    productItemBusinessPartner1.setCncActive(false);
    productItemBusinessPartner1.setCncActivated(false);
    productItemBusinessPartner1.setBuyable(false);
    productItemBusinessPartner1.setDisplay(false);
    productItemBusinessPartner1.setB2bBuyable(false);
    productItemBusinessPartner1.setB2bDiscoverable(false);

    productItemBusinessPartners.add(productItemBusinessPartner1);
    ProductAndL5MigrationRequest migrationRequest = new ProductAndL5MigrationRequest();
    migrationRequest.setProductType(com.gdn.mta.product.enums.ProductType.REGULAR);
    migrationRequest.setCncActiveAtL3(true);
    migrationRequest.setCncActiveAtL5(true);
    migrationRequest.setBuyable(true);
    migrationRequest.setDiscoverable(true);

    String username = Constants.DEFAULT_USERNAME;
    boolean b2cActivated = true;
    boolean b2bActivated = false;
    CommonUtils.migrateProductAndL5DetailsByProductSku(productItemBusinessPartners, migrationRequest, b2cActivated, b2bActivated, username);
    Assertions.assertTrue(productItemBusinessPartner1.isCncActive());
    Assertions.assertTrue(productItemBusinessPartner1.isCncActivated());
    Assertions.assertTrue(productItemBusinessPartner1.isBuyable());
    Assertions.assertTrue(productItemBusinessPartner1.isDisplay());
    Assertions.assertFalse(productItemBusinessPartner1.isB2bBuyable());
    Assertions.assertFalse(productItemBusinessPartner1.isB2bDiscoverable());
    Assertions.assertEquals(username, productItemBusinessPartner1.getUpdatedBy());
  }
  @Test
  public void testAllDimensionsNonZero() {
    Assertions.assertFalse(CommonUtils.isDimensionLess(10.0, 5.0, 15.0, 2.0));
  }

  @Test
  public void testisDimensionLessHeightZero() {
    Assertions.assertTrue(CommonUtils.isDimensionLess(0.0, 5.0, 15.0, 2.0));
  }

  @Test
  public void testisDimensionLessHeightNull() {
    Assertions.assertTrue(CommonUtils.isDimensionLess(null, 5.0, 15.0, 2.0));
  }

  @Test
  public void testisDimensionLessAllDimensionsZero() {
    Assertions.assertTrue(CommonUtils.isDimensionLess(0.0, 0.0, 0.0, 0.0));
  }

  @Test
  public void testisDimensionLessMixedZeroAndNonZero() {
    Assertions.assertTrue(CommonUtils.isDimensionLess(0.0, 0.0, 0.0, 1.0));
  }

  @Test
  public void testisDimensionLessAllDimensionsNull() {
    Assertions.assertTrue(CommonUtils.isDimensionLess(null, null, null, null));
  }

  @Test
  public void updateProductItemBusinessPartnerForAutoCategoryChangeTest() {
    ProductItemBusinessPartner productItemBusinessPartner1 = new ProductItemBusinessPartner();
    ProductItemBusinessPartner productItemBusinessPartner2 = new ProductItemBusinessPartner();
    List<ProductItemBusinessPartner> productItemBusinessPartners = List.of(productItemBusinessPartner1, productItemBusinessPartner2);
    EditFlagChangesDTO editFlagChangesDTO = new EditFlagChangesDTO();
    boolean autoCategoryChange = true;
    boolean bopisEligible = false;
    boolean bopisCategoryActionOnCategoryChangeSwitch = true;
    Integer productType = ProductType.BOPIS.getCode();
    CommonUtils.updateProductItemBusinessPartnerForAutoCategoryChange(
      productItemBusinessPartners, autoCategoryChange, bopisEligible,
      bopisCategoryActionOnCategoryChangeSwitch, productType, editFlagChangesDTO
    );
    productItemBusinessPartners.forEach(productItemBusinessPartner -> {
      Assertions.assertFalse(productItemBusinessPartner.isDisplay());
      Assertions.assertFalse(productItemBusinessPartner.isBuyable());
      Assertions.assertEquals(Optional.of(ProductType.REGULAR.getCode()).get(),
        productItemBusinessPartner.getProductType());
    });
    Assertions.assertTrue(editFlagChangesDTO.isTakeActionOnShippingForAutoCategoryChange());
  }

  @Test
  public void updateProductItemBusinessPartnerForAutoCategoryChangeSwithOffTest() {
    ProductItemBusinessPartner productItemBusinessPartner1 = new ProductItemBusinessPartner();
    productItemBusinessPartner1.setBuyable(true);
    productItemBusinessPartner1.setDisplay(true);
    ProductItemBusinessPartner productItemBusinessPartner2 = new ProductItemBusinessPartner();
    productItemBusinessPartner2.setBuyable(true);
    productItemBusinessPartner2.setDisplay(true);
    productItemBusinessPartner2.setProductType(1);
    productItemBusinessPartner1.setProductType(1);
    List<ProductItemBusinessPartner> productItemBusinessPartners = List.of(productItemBusinessPartner1, productItemBusinessPartner2);
    EditFlagChangesDTO editFlagChangesDTO = new EditFlagChangesDTO();
    boolean autoCategoryChange = true;
    boolean bopisEligible = false;
    boolean bopisCategoryActionOnCategoryChangeSwitch = false;
    Integer productType = ProductType.BOPIS.getCode();
    CommonUtils.updateProductItemBusinessPartnerForAutoCategoryChange(
      productItemBusinessPartners, autoCategoryChange, bopisEligible,
      bopisCategoryActionOnCategoryChangeSwitch, productType, editFlagChangesDTO
    );
    productItemBusinessPartners.forEach(productItemBusinessPartner -> {
      Assertions.assertTrue(productItemBusinessPartner.isDisplay());
      Assertions.assertTrue(productItemBusinessPartner.isBuyable());
      Assertions.assertEquals(Optional.of(ProductType.REGULAR.getCode()).get(),
        productItemBusinessPartner.getProductType());
    });
    Assertions.assertFalse(editFlagChangesDTO.isTakeActionOnShippingForAutoCategoryChange());
  }

  @Test
  public void validateSyncStockForFaasSellerTest() {
    validateSyncStockForFaasSellerTestImpl(true, true, true);
    validateSyncStockForFaasSellerTestImpl(true, false, true);
    validateSyncStockForFaasSellerTestImpl(false, true, true);
    validateSyncStockForFaasSellerTestImpl(false, false, true);
    validateSyncStockForFaasSellerTestImpl(true, true, false);
  }

  public void validateSyncStockForFaasSellerTestImpl(boolean warehouse, boolean faas,
    boolean faasFeatureSwitch) {
    QuickEditV2Request quickEditV2Request = new QuickEditV2Request();
    quickEditV2Request.setUseWarehouseStock(warehouse);
    ProfileResponse profileResponse = new ProfileResponse();
    Map<String, Object> flags = new HashMap<>();
    flags.put("faasActivated", faas);
    profileResponse.setFlags(flags);
    ItemSummaryListResponse itemSummaryListResponse = new ItemSummaryListResponse();
    try {
      CommonUtils.validateSyncStockForFaasSeller(quickEditV2Request, profileResponse, itemSummaryListResponse,
        faasFeatureSwitch);
    } catch(ApplicationRuntimeException e) {
      Assertions.assertEquals(e.getErrorMessage(), ErrorCategory.VALIDATION.getMessage() + ApiErrorCode.FAAS_SELLER_SYNC_STOCK_CHANGE_ERROR.getDesc());
      Assertions.assertEquals(ErrorCategory.VALIDATION, e.getErrorCodes());
    }
  }

  @Test
  public void updateProductItemBusinessPartnerForAutoCategoryChangeBopisEligibkleTest() {
    ProductItemBusinessPartner productItemBusinessPartner1 = new ProductItemBusinessPartner();
    productItemBusinessPartner1.setBuyable(true);
    productItemBusinessPartner1.setDisplay(true);
    ProductItemBusinessPartner productItemBusinessPartner2 = new ProductItemBusinessPartner();
    productItemBusinessPartner2.setBuyable(true);
    productItemBusinessPartner2.setDisplay(true);
    productItemBusinessPartner2.setProductType(1);
    productItemBusinessPartner1.setProductType(1);
    List<ProductItemBusinessPartner> productItemBusinessPartners = List.of(productItemBusinessPartner1, productItemBusinessPartner2);
    EditFlagChangesDTO editFlagChangesDTO = new EditFlagChangesDTO();
    boolean autoCategoryChange = true;
    boolean bopisEligible = true;
    boolean bopisCategoryActionOnCategoryChangeSwitch = true;
    Integer productType = ProductType.BOPIS.getCode();
    CommonUtils.updateProductItemBusinessPartnerForAutoCategoryChange(
      productItemBusinessPartners, autoCategoryChange, bopisEligible,
      bopisCategoryActionOnCategoryChangeSwitch, productType, editFlagChangesDTO
    );
    productItemBusinessPartners.forEach(productItemBusinessPartner -> {
      Assertions.assertTrue(productItemBusinessPartner.isDisplay());
      Assertions.assertTrue(productItemBusinessPartner.isBuyable());
      Assertions.assertEquals(Optional.of(ProductType.REGULAR.getCode()).get(),
        productItemBusinessPartner.getProductType());
    });
    Assertions.assertFalse(editFlagChangesDTO.isTakeActionOnShippingForAutoCategoryChange());
  }

  @Test
  public void updateProductItemBusinessPartnerForAutoCategoryChangDiffTypeeTest() {
    ProductItemBusinessPartner productItemBusinessPartner1 = new ProductItemBusinessPartner();
    productItemBusinessPartner1.setBuyable(true);
    productItemBusinessPartner1.setDisplay(true);
    ProductItemBusinessPartner productItemBusinessPartner2 = new ProductItemBusinessPartner();
    productItemBusinessPartner2.setBuyable(true);
    productItemBusinessPartner2.setDisplay(true);
    productItemBusinessPartner2.setProductType(1);
    productItemBusinessPartner1.setProductType(1);
    List<ProductItemBusinessPartner> productItemBusinessPartners = List.of(productItemBusinessPartner1, productItemBusinessPartner2);
    EditFlagChangesDTO editFlagChangesDTO = new EditFlagChangesDTO();
    boolean autoCategoryChange = true;
    boolean bopisEligible = false;
    boolean bopisCategoryActionOnCategoryChangeSwitch = true;
    Integer productType = ProductType.REGULAR.getCode();
    CommonUtils.updateProductItemBusinessPartnerForAutoCategoryChange(
      productItemBusinessPartners, autoCategoryChange, bopisEligible,
      bopisCategoryActionOnCategoryChangeSwitch, productType, editFlagChangesDTO
    );
    productItemBusinessPartners.forEach(productItemBusinessPartner -> {
      Assertions.assertTrue(productItemBusinessPartner.isDisplay());
      Assertions.assertTrue(productItemBusinessPartner.isBuyable());
      Assertions.assertEquals(Optional.of(ProductType.REGULAR.getCode()).get(),
        productItemBusinessPartner.getProductType());
    });
    Assertions.assertFalse(editFlagChangesDTO.isTakeActionOnShippingForAutoCategoryChange());
  }

  @Test
  public void noActionWhenConditionsAreNotMet() {
    ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
    List<ProductItemBusinessPartner> productItemBusinessPartners = List.of(productItemBusinessPartner);
    EditFlagChangesDTO editFlagChangesDTO = new EditFlagChangesDTO();
    boolean autoCategoryChange = false;
    boolean bopisEligible = true;
    boolean bopisCategoryActionOnCategoryChangeSwitch = true;
    Integer productType = ProductType.BOPIS.getCode();
    CommonUtils.updateProductItemBusinessPartnerForAutoCategoryChange(
      productItemBusinessPartners, autoCategoryChange, bopisEligible,
      bopisCategoryActionOnCategoryChangeSwitch, productType, editFlagChangesDTO
    );
    productItemBusinessPartners.forEach(productItemBusinessPartnerInstance -> {
      assertNull(productItemBusinessPartnerInstance.getProductType());
    });
    Assertions.assertFalse(editFlagChangesDTO.isTakeActionOnShippingForAutoCategoryChange());
  }

  @Test
  public void noActionWhenProductTypeIsDifferent() {
    ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
    List<ProductItemBusinessPartner> productItemBusinessPartners = List.of(productItemBusinessPartner);
    EditFlagChangesDTO editFlagChangesDTO = new EditFlagChangesDTO();
    boolean autoCategoryChange = true;
    boolean bopisEligible = false;
    boolean bopisCategoryActionOnCategoryChangeSwitch = true;
    Integer productType = ProductType.REGULAR.getCode();
    CommonUtils.updateProductItemBusinessPartnerForAutoCategoryChange(
      productItemBusinessPartners, autoCategoryChange, bopisEligible,
      bopisCategoryActionOnCategoryChangeSwitch, productType, editFlagChangesDTO
    );
    productItemBusinessPartners.forEach(productItemBusinessPartnerInstance -> {
      assertNull(productItemBusinessPartnerInstance.getProductType());
    });
    Assertions.assertFalse(editFlagChangesDTO.isTakeActionOnShippingForAutoCategoryChange());
  }

  @Test
  public void setCncViewConfigFromProductVariantUpdateRequestTest_switchOff() {
    ProductVariantUpdateRequest productVariantUpdateRequest = new ProductVariantUpdateRequest();
    ItemPickupPointRequest itemPickupPointRequest = ItemPickupPointRequest.builder().cncActive(true).build();
    productVariantPriceStockAndImagesRequest.setModifiedItemPickupPoints(
        Collections.singletonList(itemPickupPointRequest));
    productVariantUpdateRequest.setAddPickupPoints(Collections.singletonList(itemPickupPointRequest));
    productVariantUpdateRequest.setProductItems(Collections.singletonList(productVariantPriceStockAndImagesRequest));
    CommonUtils.setCncViewConfigFromProductVariantUpdateRequest(productVariantUpdateRequest, false);
    Assertions.assertTrue(productVariantUpdateRequest.getAddPickupPoints().get(0).isCncBuyable());
    Assertions.assertTrue(productVariantUpdateRequest.getAddPickupPoints().get(0).isCncDisplay());
    Assertions.assertTrue(
        productVariantUpdateRequest.getProductItems().get(0).getModifiedItemPickupPoints().get(0).isCncBuyable());
    Assertions.assertTrue(
        productVariantUpdateRequest.getProductItems().get(0).getModifiedItemPickupPoints().get(0).isCncDisplay());
  }

  @Test
  public void setCncViewConfigFromProductVariantUpdateRequestTest_switchOn() {
    ProductVariantUpdateRequest productVariantUpdateRequest = new ProductVariantUpdateRequest();
    ItemPickupPointRequest itemPickupPointRequest = ItemPickupPointRequest.builder().cncActive(true).build();
    productVariantPriceStockAndImagesRequest.setModifiedItemPickupPoints(
        Collections.singletonList(itemPickupPointRequest));
    productVariantUpdateRequest.setAddPickupPoints(Collections.singletonList(itemPickupPointRequest));
    productVariantUpdateRequest.setProductItems(Collections.singletonList(productVariantPriceStockAndImagesRequest));
    CommonUtils.setCncViewConfigFromProductVariantUpdateRequest(productVariantUpdateRequest, true);
    Assertions.assertTrue(productVariantUpdateRequest.getAddPickupPoints().get(0).isCncBuyable());
    Assertions.assertTrue(productVariantUpdateRequest.getAddPickupPoints().get(0).isCncDisplay());
    Assertions.assertTrue(
        productVariantUpdateRequest.getProductItems().get(0).getModifiedItemPickupPoints().get(0).isCncBuyable());
    Assertions.assertTrue(
        productVariantUpdateRequest.getProductItems().get(0).getModifiedItemPickupPoints().get(0).isCncDisplay());
  }

  @Test
  public void setCncViewConfigFromProductVariantUpdateRequestTest_switchOn_b2bConfig() {
    ProductVariantUpdateRequest productVariantUpdateRequest = new ProductVariantUpdateRequest();
    ItemPickupPointRequest itemPickupPointRequest = ItemPickupPointRequest.builder().cncActive(true).cncBuyable(true).build();
    productVariantPriceStockAndImagesRequest.setModifiedItemPickupPoints(
        Collections.singletonList(itemPickupPointRequest));
    productVariantUpdateRequest.setAddPickupPoints(Collections.singletonList(itemPickupPointRequest));
    productVariantUpdateRequest.setProductItems(Collections.singletonList(productVariantPriceStockAndImagesRequest));
    CommonUtils.setCncViewConfigFromProductVariantUpdateRequest(productVariantUpdateRequest, true);
    Assertions.assertTrue(productVariantUpdateRequest.getAddPickupPoints().get(0).isCncBuyable());
    Assertions.assertFalse(productVariantUpdateRequest.getAddPickupPoints().get(0).isCncDisplay());
    Assertions.assertTrue(
        productVariantUpdateRequest.getProductItems().get(0).getModifiedItemPickupPoints().get(0).isCncBuyable());
    Assertions.assertFalse(
        productVariantUpdateRequest.getProductItems().get(0).getModifiedItemPickupPoints().get(0).isCncDisplay());
  }

  @Test
  public void setCncViewConfigFromProductVariantUpdateRequestTest_switchOn_teaserConfig() {
    ProductVariantUpdateRequest productVariantUpdateRequest = new ProductVariantUpdateRequest();
    ItemPickupPointRequest itemPickupPointRequest = ItemPickupPointRequest.builder().cncActive(true).cncDisplay(true).build();
    productVariantPriceStockAndImagesRequest.setModifiedItemPickupPoints(
        Collections.singletonList(itemPickupPointRequest));
    productVariantUpdateRequest.setAddPickupPoints(Collections.singletonList(itemPickupPointRequest));
    productVariantUpdateRequest.setProductItems(Collections.singletonList(productVariantPriceStockAndImagesRequest));
    CommonUtils.setCncViewConfigFromProductVariantUpdateRequest(productVariantUpdateRequest, true);
    Assertions.assertFalse(productVariantUpdateRequest.getAddPickupPoints().get(0).isCncBuyable());
    Assertions.assertTrue(productVariantUpdateRequest.getAddPickupPoints().get(0).isCncDisplay());
    Assertions.assertFalse(
        productVariantUpdateRequest.getProductItems().get(0).getModifiedItemPickupPoints().get(0).isCncBuyable());
    Assertions.assertTrue(
        productVariantUpdateRequest.getProductItems().get(0).getModifiedItemPickupPoints().get(0).isCncDisplay());
  }

  @Test
  public void testFetchRequestForActiveProductMigration() {
    ProductAndL5MigrationRequest l5MigrationRequest = new ProductAndL5MigrationRequest();
    l5MigrationRequest.setProductSku(PRODUCT_SKU);
    l5MigrationRequest.setL5Updated(true);
    l5MigrationRequest.setBuyable(false);
    l5MigrationRequest.setDiscoverable(false);
    l5MigrationRequest.setProductType(com.gdn.mta.product.enums.ProductType.BIG_PRODUCT);

    com.gdn.x.product.rest.web.model.request.ProductAndL5MigrationRequest request =
      CommonUtils.fetchRequestForActiveProductMigration(l5MigrationRequest);
    Assertions.assertEquals(l5MigrationRequest.getProductSku(), request.getProductSku());
    Assertions.assertEquals(l5MigrationRequest.isL5Updated(), request.isL5Updated());
    Assertions.assertEquals(l5MigrationRequest.isBuyable(), request.isBuyable());
    Assertions.assertEquals(l5MigrationRequest.isDiscoverable(), request.isDiscoverable());
    Assertions.assertEquals(ProductType.BIG_PRODUCT, request.getProductType());
  }

  @Test
  public void testGetSchedulesUpdatedL5_withScheduleRemovalOnCncStatusChange() {
    Map<String, Set<String>> scheduleUpdatedL5s = new HashMap<>();
    Date oldDate = new Date(System.currentTimeMillis() - 10000);
    Date newDate = new Date();
    BuyableScheduleRequest buyableScheduleRequest = BuyableScheduleRequest.builder()
        .startDateTime(newDate)
        .endDateTime(newDate)
        .build();
    DiscoverableScheduleRequest discoverableScheduleRequest = DiscoverableScheduleRequest.builder()
        .startDateTime(newDate)
        .endDateTime(newDate)
        .build();

    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setItemSku(ITEM_SKU);
    itemPickupPointRequest.setPickupPointId(PICKUP_POINT_CODE);
    itemPickupPointRequest.setBuyableSchedule(buyableScheduleRequest);
    itemPickupPointRequest.setDiscoverableSchedule(discoverableScheduleRequest);
    itemPickupPointRequest.setBuyable(false);
    itemPickupPointRequest.setDisplay(false);
    itemPickupPointRequest.setCncDisplay(true);

    BuyableScheduleResponse buyableScheduleResponse = new BuyableScheduleResponse();
    buyableScheduleResponse.setStartDateTime(oldDate);
    buyableScheduleResponse.setEndDateTime(oldDate);
    DiscoverableScheduleResponse discoverableScheduleResponse = new DiscoverableScheduleResponse();
    discoverableScheduleResponse.setStartDateTime(oldDate);
    discoverableScheduleResponse.setEndDateTime(oldDate);

    ViewConfigResponse viewConfigResponse = new ViewConfigResponse();
    viewConfigResponse.setBuyableScheduleResponse(buyableScheduleResponse);
    viewConfigResponse.setDiscoverableScheduleResponse(discoverableScheduleResponse);
    ItemPickupPointListingResponse itemPickupPointListingResponse = new ItemPickupPointListingResponse();
    itemPickupPointListingResponse.setViewConfigs(Collections.singletonList(viewConfigResponse));

    CommonUtils.getSchedulesUpdatedL5(scheduleUpdatedL5s, itemPickupPointListingResponse, itemPickupPointRequest,
        true, true);

    Assertions.assertTrue(itemPickupPointRequest.isScheduleRemoval());
    Assertions.assertTrue(scheduleUpdatedL5s.containsKey(L5_CODE));
    Assertions.assertTrue(scheduleUpdatedL5s.get(L5_CODE).contains(Constants.BUYABLE_SCHEDULE_UPDATE));
    Assertions.assertTrue(scheduleUpdatedL5s.get(L5_CODE).contains(Constants.DISCOVERABLE_SCHEDULE_UPDATE));
  }
  @Test
  public void validateSyncStockForFAASMerchantsInEditRequest_withFaasFlagAndSyncStockTrue_returnsErrorCode() {
    ProductL3UpdateRequest productL3UpdateRequest = new ProductL3UpdateRequest();
    ProfileResponse profileResponse = new ProfileResponse();
    ApiErrorCode apiErrorCode = null;
    Map<String, Object> flags = new HashMap<>();
    flags.put(Constants.FAAS_ACTIVATED, true);
    profileResponse.setFlags(flags);
    ProductVariantPriceStockAndImagesRequest item = new ProductVariantPriceStockAndImagesRequest();
    ItemPickupPointRequest pickupPointRequest = new ItemPickupPointRequest();
    pickupPointRequest.setSynchronizeStock(true);
    item.setModifiedItemPickupPoints(Collections.singletonList(pickupPointRequest));
    productL3UpdateRequest.setProductItems(Collections.singletonList(item));
    productL3UpdateRequest.setAddPickupPoints(Collections.emptyList());
    ApiErrorCode result = CommonUtils.validateSyncStockForFAASMerchantsInEditRequest(
      productL3UpdateRequest, profileResponse, apiErrorCode
    );
    Assertions.assertEquals(ApiErrorCode.FAAS_SELLER_SYNC_STOCK_CHANGE_ERROR, result);
  }

  @Test
  public void validateSyncStockForFAASMerchantsInEditRequest_withFaasFlagAndSyncStockTrueForNonFAASSeller() {
    ProductL3UpdateRequest productL3UpdateRequest = new ProductL3UpdateRequest();
    ProfileResponse profileResponse = new ProfileResponse();
    ApiErrorCode apiErrorCode = null;
    Map<String, Object> flags = new HashMap<>();
    flags.put(Constants.CM_MERCHANT, true);
    profileResponse.setFlags(flags);
    ProductVariantPriceStockAndImagesRequest item = new ProductVariantPriceStockAndImagesRequest();
    ItemPickupPointRequest pickupPointRequest = new ItemPickupPointRequest();
    pickupPointRequest.setSynchronizeStock(true);
    item.setModifiedItemPickupPoints(Collections.singletonList(pickupPointRequest));
    productL3UpdateRequest.setProductItems(Collections.singletonList(item));
    productL3UpdateRequest.setAddPickupPoints(Collections.emptyList());
    ApiErrorCode result = CommonUtils.validateSyncStockForFAASMerchantsInEditRequest(
      productL3UpdateRequest, profileResponse, apiErrorCode
    );
    assertNull(result);
  }

  @Test
  public void validateSyncStockForFAASMerchantsInEditRequest_withNoSyncStock_returnsNoError() {
    ProductL3UpdateRequest productL3UpdateRequest = new ProductL3UpdateRequest();
    ProfileResponse profileResponse = new ProfileResponse();
    ApiErrorCode apiErrorCode = null;
    Map<String, Object> flags = new HashMap<>();
    flags.put(Constants.FAAS_ACTIVATED, true);
    profileResponse.setFlags(flags);
    ProductVariantPriceStockAndImagesRequest item = new ProductVariantPriceStockAndImagesRequest();
    ItemPickupPointRequest pickupPointRequest = new ItemPickupPointRequest();
    pickupPointRequest.setSynchronizeStock(false);
    item.setModifiedItemPickupPoints(Collections.singletonList(pickupPointRequest));
    productL3UpdateRequest.setProductItems(Collections.singletonList(item));
    productL3UpdateRequest.setAddPickupPoints(Collections.emptyList());
    ApiErrorCode result = CommonUtils.validateSyncStockForFAASMerchantsInEditRequest(
      productL3UpdateRequest, profileResponse, apiErrorCode
    );
    assertNull(result);
  }

  @Test
  public void validateSyncStockForFAASMerchantsInEditRequest_withFaasFlagAndNewSyncStockTrue_returnsErrorCode() {
    ProductL3UpdateRequest productL3UpdateRequest = new ProductL3UpdateRequest();
    ProfileResponse profileResponse = new ProfileResponse();
    ApiErrorCode apiErrorCode = null;
    Map<String, Object> flags = new HashMap<>();
    flags.put(Constants.FAAS_ACTIVATED, true);
    profileResponse.setFlags(flags);
    ItemPickupPointRequest newPickupPointRequest = new ItemPickupPointRequest();
    newPickupPointRequest.setSynchronizeStock(true);
    productL3UpdateRequest.setAddPickupPoints(Collections.singletonList(newPickupPointRequest));

    productL3UpdateRequest.setProductItems(Collections.emptyList());
    ApiErrorCode result = CommonUtils.validateSyncStockForFAASMerchantsInEditRequest(
      productL3UpdateRequest, profileResponse, apiErrorCode
    );
    Assertions.assertEquals(ApiErrorCode.FAAS_SELLER_SYNC_STOCK_CHANGE_ERROR, result);
  }

  @Test
  public void getSyncStockValueForFASSMerchants_faasFeatureSwitchAndFaasActivated_returnsFalse() {
    boolean faasFeatureSwitch = true;
    boolean fbbActivated = true;
    ProfileResponse profileResponse = new ProfileResponse();
    Map<String, Object> flags = new HashMap<>();
    flags.put(Constants.FAAS_ACTIVATED, true);
    profileResponse.setFlags(flags);
    boolean result = CommonUtils.getSyncStockValueForFASSMerchants(faasFeatureSwitch, fbbActivated, profileResponse);
    Assertions.assertFalse(result);
  }

  @Test
  public void getSyncStockValueForFASSMerchants_faasFeatureSwitchAndFaasActivatedFalse() {
    boolean faasFeatureSwitch = true;
    boolean fbbActivated = true;
    ProfileResponse profileResponse = new ProfileResponse();
    Map<String, Object> flags = new HashMap<>();
    flags.put(Constants.BUYABLE_SCHEDULE_UPDATE, true);
    profileResponse.setFlags(flags);
    boolean result = CommonUtils.getSyncStockValueForFASSMerchants(faasFeatureSwitch, fbbActivated, profileResponse);
    Assertions.assertTrue(result);
  }

  @Test
  public void getSyncStockValueForFASSMerchants_faasFeatureSwitchFalse_returnsFbbActivatedValue() {
    boolean faasFeatureSwitch = false;
    boolean fbbActivated = true;
    ProfileResponse profileResponse = new ProfileResponse();
    Map<String, Object> flags = new HashMap<>();
    flags.put(Constants.FAAS_ACTIVATED, true);
    profileResponse.setFlags(flags);
    boolean result = CommonUtils.getSyncStockValueForFASSMerchants(faasFeatureSwitch, fbbActivated, profileResponse);
    Assertions.assertTrue(result);
  }

  @Test
  public void getSyncStockValueForFASSMerchants_fbbActivatedFalse_returnsFalse() {
    boolean faasFeatureSwitch = false;
    boolean fbbActivated = false;
    ProfileResponse profileResponse = new ProfileResponse();
    boolean result = CommonUtils.getSyncStockValueForFASSMerchants(faasFeatureSwitch, fbbActivated, profileResponse);
    Assertions.assertFalse(result);
  }

  @Test
  public void isFaasEligibleSellerTest() {
    Assertions.assertFalse(CommonUtils.isFaasEligibleSeller(false, profileResponse));
    Assertions.assertFalse(CommonUtils.isFaasEligibleSeller(true, profileResponse));
    profileResponse.setFlags(Map.of(FAAS_ACTIVATED, true));
    Assertions.assertTrue(CommonUtils.isFaasEligibleSeller(true, profileResponse));
  }

  @Test
  public void testPopulateHistoryAuditMapForSchedulesUpdate_cncSwitchOn_ActiveSchedules() {
    // Setup
    Map<String, ItemPickupPointListingResponse> l5CodeAndResponseMap = new HashMap<>();
    List<Map<String, String>> historyAuditMap = new ArrayList<>();
    FbbAndCncDataChangeDto fbbAndCncDataChangeDto = new FbbAndCncDataChangeDto();
    ItemPickupPointRequest modifiedItemPickupPoint = new ItemPickupPointRequest();
    modifiedItemPickupPoint.setItemSku(ITEM_SKU);
    modifiedItemPickupPoint.setPickupPointId(PICKUP_POINT_CODE);
    modifiedItemPickupPoint.setBuyable(false);
    modifiedItemPickupPoint.setDisplay(false);
    modifiedItemPickupPoint.setCncDisplay(true);

    Date now = new Date();
    ViewConfigResponse viewConfigResponse = ViewConfigResponse.builder()
        .channelId(Constants.DEFAULT)
        .buyableScheduleResponse(BuyableScheduleResponse.builder()
            .endDateTime(new Date(now.getTime() + 10000))
            .build())
        .discoverableScheduleResponse(DiscoverableScheduleResponse.builder()
            .endDateTime(new Date(now.getTime() + 10000))
            .build())
        .build();

    ItemPickupPointListingResponse itemPickupPointListingResponse = new ItemPickupPointListingResponse();
    itemPickupPointListingResponse.setViewConfigs(Collections.singletonList(viewConfigResponse));
    itemPickupPointListingResponse.setItemSku(ITEM_SKU);
    itemPickupPointListingResponse.setPickUpPointCode(PICKUP_POINT_CODE);
    l5CodeAndResponseMap.put(ITEM_SKU.concat(Constants.HYPHEN).concat(PICKUP_POINT_CODE),
        itemPickupPointListingResponse);
    CommonUtils.populateHistoryAuditMapForSchedulesUpdate(l5CodeAndResponseMap, true, historyAuditMap,
        modifiedItemPickupPoint, fbbAndCncDataChangeDto, true, true);
    Assertions.assertEquals(2, historyAuditMap.size());
  }

  @Test
  public void updateItemPickupPointSchedules_nonOfflineStatusTest() {
    ItemPickupPointRequest itemPickupPointRequest = ItemPickupPointRequest.builder().cncBuyable(true).discoverableSchedule(
        DiscoverableScheduleRequest.builder().build()).build();
    CommonUtils.updateItemPickupPointSchedules(itemPickupPointRequest);
    assertNull(itemPickupPointRequest.getBuyableSchedule());
    Assertions.assertNotNull(itemPickupPointRequest.getDiscoverableSchedule());
  }

  @Test
  public void testSetSchedulesForQuickEditRequestsWithStatus_noCncChange() {
    ItemSummaryListResponse itemSummaryListResponse = new ItemSummaryListResponse();
    ItemViewConfigDTO itemViewConfigDTO = new ItemViewConfigDTO();
    itemViewConfigDTO.setChannel(Constants.DEFAULT);

    ItemDiscoverableScheduleDTO itemDiscoverableScheduleDTO = new ItemDiscoverableScheduleDTO();
    itemDiscoverableScheduleDTO.setDiscoverable(true);
    itemDiscoverableScheduleDTO.setStartDateTime(new Date());
    itemDiscoverableScheduleDTO.setEndDateTime(new Date());

    itemViewConfigDTO.setItemDiscoverableSchedules(itemDiscoverableScheduleDTO);
    itemSummaryListResponse.setItemViewConfigs(Set.of(itemViewConfigDTO));

    ItemPickupPointQuickEditRequest itemPickupPointQuickEditRequest = new ItemPickupPointQuickEditRequest();
    itemPickupPointQuickEditRequest.setItemSku(ITEM_SKU);
    itemPickupPointQuickEditRequest.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointQuickEditRequest.setStatus(ProductLevel3Status.OFFLINE.name());
    itemPickupPointQuickEditRequest.setCncStatus(ProductLevel3Status.OFFLINE.name());
    CommonUtils.setSchedulesForQuickEditRequest(itemSummaryListResponse, itemPickupPointQuickEditRequest, true);
    Assertions.assertFalse(itemPickupPointQuickEditRequest.isScheduleUpdate());
  }

  @Test
  public void testSetSchedulesForQuickEditRequestsWithStatus_cncChangeToOnline() {
    ItemSummaryListResponse itemSummaryListResponse = new ItemSummaryListResponse();
    ItemViewConfigDTO itemViewConfigDTO = new ItemViewConfigDTO();
    itemViewConfigDTO.setChannel(Constants.DEFAULT);

    ItemDiscoverableScheduleDTO itemDiscoverableScheduleDTO = new ItemDiscoverableScheduleDTO();
    itemDiscoverableScheduleDTO.setDiscoverable(true);
    itemDiscoverableScheduleDTO.setStartDateTime(new Date());
    itemDiscoverableScheduleDTO.setEndDateTime(new Date());

    itemViewConfigDTO.setItemDiscoverableSchedules(itemDiscoverableScheduleDTO);
    itemSummaryListResponse.setItemViewConfigs(Set.of(itemViewConfigDTO));

    ItemPickupPointQuickEditRequest itemPickupPointQuickEditRequest = new ItemPickupPointQuickEditRequest();
    itemPickupPointQuickEditRequest.setItemSku(ITEM_SKU);
    itemPickupPointQuickEditRequest.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointQuickEditRequest.setStatus(ProductLevel3Status.OFFLINE.name());
    itemPickupPointQuickEditRequest.setCncStatus(ProductLevel3Status.ONLINE.name());
    CommonUtils.setSchedulesForQuickEditRequest(itemSummaryListResponse, itemPickupPointQuickEditRequest, true);
    Assertions.assertTrue(itemPickupPointQuickEditRequest.isScheduleUpdate());
  }

  @Test
  public void testSetSchedulesForQuickEditRequestsWithStatus_cncChangeToOnline_existingCncStatus() {
    ItemSummaryListResponse itemSummaryListResponse = new ItemSummaryListResponse();
    ItemViewConfigDTO itemViewConfigDTO = new ItemViewConfigDTO();
    itemViewConfigDTO.setChannel(Constants.DEFAULT);

    ItemViewConfigDTO cncItemViewConfigDTO = new ItemViewConfigDTO();
    cncItemViewConfigDTO.setChannel(Constants.CNC_CHANNEL);

    ItemDiscoverableScheduleDTO itemDiscoverableScheduleDTO = new ItemDiscoverableScheduleDTO();
    itemDiscoverableScheduleDTO.setDiscoverable(true);
    itemDiscoverableScheduleDTO.setStartDateTime(new Date());
    itemDiscoverableScheduleDTO.setEndDateTime(new Date());

    itemViewConfigDTO.setItemDiscoverableSchedules(itemDiscoverableScheduleDTO);
    itemSummaryListResponse.setItemViewConfigs(Set.of(itemViewConfigDTO));

    ItemPickupPointQuickEditRequest itemPickupPointQuickEditRequest = new ItemPickupPointQuickEditRequest();
    itemPickupPointQuickEditRequest.setItemSku(ITEM_SKU);
    itemPickupPointQuickEditRequest.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointQuickEditRequest.setStatus(ProductLevel3Status.OFFLINE.name());
    itemPickupPointQuickEditRequest.setCncStatus(ProductLevel3Status.ONLINE.name());
    CommonUtils.setSchedulesForQuickEditRequest(itemSummaryListResponse, itemPickupPointQuickEditRequest, true);
    Assertions.assertTrue(itemPickupPointQuickEditRequest.isScheduleUpdate());
  }

  @Test
  public void testSetSchedulesForQuickEditRequestsWithStatus_noCncChange_requestCncNull() {
    ItemSummaryListResponse itemSummaryListResponse = new ItemSummaryListResponse();
    ItemViewConfigDTO itemViewConfigDTO = new ItemViewConfigDTO();
    itemViewConfigDTO.setChannel(Constants.DEFAULT);

    ItemDiscoverableScheduleDTO itemDiscoverableScheduleDTO = new ItemDiscoverableScheduleDTO();
    itemDiscoverableScheduleDTO.setDiscoverable(true);
    itemDiscoverableScheduleDTO.setStartDateTime(new Date());
    itemDiscoverableScheduleDTO.setEndDateTime(new Date());

    itemViewConfigDTO.setItemDiscoverableSchedules(itemDiscoverableScheduleDTO);
    itemSummaryListResponse.setItemViewConfigs(Set.of(itemViewConfigDTO));

    ItemPickupPointQuickEditRequest itemPickupPointQuickEditRequest = new ItemPickupPointQuickEditRequest();
    itemPickupPointQuickEditRequest.setItemSku(ITEM_SKU);
    itemPickupPointQuickEditRequest.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointQuickEditRequest.setStatus(ProductLevel3Status.OFFLINE.name());
    CommonUtils.setSchedulesForQuickEditRequest(itemSummaryListResponse, itemPickupPointQuickEditRequest, true);
    Assertions.assertFalse(itemPickupPointQuickEditRequest.isScheduleUpdate());
  }

  @Test
  public void isL5CncActive_switchOffTest() {
    Assertions.assertFalse(CommonUtils.isL5CncActive(new ProductItemBusinessPartner(), false));
  }

  @Test
  public void isL5CncActive_switchOff_cncActiveTest() {
    ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
    productItemBusinessPartner.setCncActive(true);
    Assertions.assertTrue(CommonUtils.isL5CncActive(productItemBusinessPartner, false));
  }

  @Test
  public void isL5CncActive_switchOn_offlineTest() {
    Assertions.assertFalse(CommonUtils.isL5CncActive(new ProductItemBusinessPartner(), true));
  }

  @Test
  public void isL5CncActive_switchOn_buyableTest() {
    ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
    productItemBusinessPartner.setCncBuyable(true);
    Assertions.assertTrue(CommonUtils.isL5CncActive(productItemBusinessPartner, true));
  }

  @Test
  public void isL5CncActive_switchOn_teaserTest() {
    ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
    productItemBusinessPartner.setCncDiscoverable(true);
    Assertions.assertTrue(CommonUtils.isL5CncActive(productItemBusinessPartner, true));
  }

  @Test
  public void isL5CncActive_switchOn_onlineTest() {
    ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
    productItemBusinessPartner.setCncDiscoverable(true);
    productItemBusinessPartner.setCncBuyable(true);
    Assertions.assertTrue(CommonUtils.isL5CncActive(productItemBusinessPartner, true));
  }

  @Test
  public void validateSyncStockForFAASMerchantsInEditPriceStock_withFaasFlagAndSyncStockTrue_switchOff() {
    ProductVariantUpdateRequest productL3UpdateRequest = new ProductVariantUpdateRequest();
    ProfileResponse profileResponse = new ProfileResponse();
    Map<String, Object> flags = new HashMap<>();
    flags.put(Constants.FAAS_ACTIVATED, true);
    profileResponse.setFlags(flags);
    ProductVariantPriceStockAndImagesRequest item = new ProductVariantPriceStockAndImagesRequest();
    ItemPickupPointRequest pickupPointRequest = new ItemPickupPointRequest();
    pickupPointRequest.setSynchronizeStock(true);
    item.setModifiedItemPickupPoints(Collections.singletonList(pickupPointRequest));
    productL3UpdateRequest.setProductItems(Collections.singletonList(item));
    productL3UpdateRequest.setAddPickupPoints(Collections.emptyList());
    CommonUtils.validateSyncStockForFAASMerchantsInEditRequest(productL3UpdateRequest, profileResponse, false);
  }

  @Test
  public void validateSyncStockForFAASMerchantsInEditPriceStock_withFaasFlagAndSyncStockTrue_returnsErrorCode() {
    ProductVariantUpdateRequest productL3UpdateRequest = new ProductVariantUpdateRequest();
    ProfileResponse profileResponse = new ProfileResponse();
    Map<String, Object> flags = new HashMap<>();
    flags.put(Constants.FAAS_ACTIVATED, true);
    profileResponse.setFlags(flags);
    ProductVariantPriceStockAndImagesRequest item = new ProductVariantPriceStockAndImagesRequest();
    ItemPickupPointRequest pickupPointRequest = new ItemPickupPointRequest();
    pickupPointRequest.setSynchronizeStock(true);
    item.setModifiedItemPickupPoints(Collections.singletonList(pickupPointRequest));
    productL3UpdateRequest.setProductItems(Collections.singletonList(item));
    productL3UpdateRequest.setAddPickupPoints(Collections.emptyList());
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      CommonUtils.validateSyncStockForFAASMerchantsInEditRequest(productL3UpdateRequest, profileResponse, true);
    });
  }

  @Test
  public void validateSyncStockForFAASMerchantsInEditPriceStock_withFaasFlagAndSyncStockTrueForNonFAASSeller() {
    ProductVariantUpdateRequest productL3UpdateRequest = new ProductVariantUpdateRequest();
    ProfileResponse profileResponse = new ProfileResponse();
    Map<String, Object> flags = new HashMap<>();
    flags.put(Constants.CM_MERCHANT, true);
    profileResponse.setFlags(flags);
    ProductVariantPriceStockAndImagesRequest item = new ProductVariantPriceStockAndImagesRequest();
    ItemPickupPointRequest pickupPointRequest = new ItemPickupPointRequest();
    pickupPointRequest.setSynchronizeStock(true);
    item.setModifiedItemPickupPoints(Collections.singletonList(pickupPointRequest));
    productL3UpdateRequest.setProductItems(Collections.singletonList(item));
    productL3UpdateRequest.setAddPickupPoints(Collections.emptyList());
    CommonUtils.validateSyncStockForFAASMerchantsInEditRequest(productL3UpdateRequest, profileResponse, true);
  }

  @Test
  public void validateSyncStockForFAASMerchantsInEditPriceStock_withNoSyncStock_returnsNoError() {
    ProductVariantUpdateRequest productL3UpdateRequest = new ProductVariantUpdateRequest();
    ProfileResponse profileResponse = new ProfileResponse();
    Map<String, Object> flags = new HashMap<>();
    flags.put(Constants.FAAS_ACTIVATED, true);
    profileResponse.setFlags(flags);
    ProductVariantPriceStockAndImagesRequest item = new ProductVariantPriceStockAndImagesRequest();
    ItemPickupPointRequest pickupPointRequest = new ItemPickupPointRequest();
    pickupPointRequest.setSynchronizeStock(false);
    item.setModifiedItemPickupPoints(Collections.singletonList(pickupPointRequest));
    productL3UpdateRequest.setProductItems(Collections.singletonList(item));
    productL3UpdateRequest.setAddPickupPoints(Collections.emptyList());
    CommonUtils.validateSyncStockForFAASMerchantsInEditRequest(productL3UpdateRequest, profileResponse, true);
  }

  @Test
  public void validateSyncStockForFAASMerchantsInEditPriceStock_withFaasFlagAndNewSyncStockTrue_returnsErrorCode() {
    ProductVariantUpdateRequest productL3UpdateRequest = new ProductVariantUpdateRequest();
    ProfileResponse profileResponse = new ProfileResponse();
    Map<String, Object> flags = new HashMap<>();
    flags.put(Constants.FAAS_ACTIVATED, true);
    profileResponse.setFlags(flags);
    ItemPickupPointRequest newPickupPointRequest = new ItemPickupPointRequest();
    newPickupPointRequest.setSynchronizeStock(true);
    productL3UpdateRequest.setAddPickupPoints(Collections.singletonList(newPickupPointRequest));
    productL3UpdateRequest.setProductItems(Collections.emptyList());
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      CommonUtils.validateSyncStockForFAASMerchantsInEditRequest(productL3UpdateRequest, profileResponse, true);
    });
  }

  @Test
  public void testOverrideImageExtensionToWebp_commonImagesConvertedToWebp() {
    ProductLevel3SummaryDetailsImageRequest image1 =
        ProductLevel3SummaryDetailsImageRequest.builder().locationPath("/images/test1.jpg").reviewType(Constants.NEW)
            .build();
    ProductLevel3SummaryDetailsImageRequest image2 =
        ProductLevel3SummaryDetailsImageRequest.builder().locationPath("/images/test2.png").reviewType(Constants.NEW)
            .build();
    ProductLevel3SummaryDetailsImageRequest image3 =
        ProductLevel3SummaryDetailsImageRequest.builder().locationPath("/images/test3.webp").reviewType(Constants.NEW)
            .build();
    ProductL3UpdateRequest request =
        ProductL3UpdateRequest.builder().commonImages(new ArrayList<>(Arrays.asList(image1, image2, image3))).build();

    CommonUtils.overrideImageExtensionToWebp(request);

    assertEquals("/images/test1.webp", request.getCommonImages().get(0).getLocationPath());
    assertEquals("/images/test2.webp", request.getCommonImages().get(1).getLocationPath());
    assertEquals("/images/test3.webp", request.getCommonImages().get(2).getLocationPath());
  }

  @Test
  public void testOverrideImageExtensionToWebp_productItemsImagesConvertedToWebp() {
    ProductLevel3SummaryDetailsImageRequest itemImage1 =
        ProductLevel3SummaryDetailsImageRequest.builder().locationPath("/item/test4.jpeg").reviewType(Constants.NEW)
            .build();
    ProductLevel3SummaryDetailsImageRequest itemImage3 =
        ProductLevel3SummaryDetailsImageRequest.builder().locationPath("/item/test4.webp").reviewType(Constants.NEW)
            .build();
    ProductLevel3SummaryDetailsImageRequest itemImage2 =
        ProductLevel3SummaryDetailsImageRequest.builder().locationPath("").reviewType(Constants.NEW).build();
    ProductVariantPriceStockAndImagesRequest productItem = ProductVariantPriceStockAndImagesRequest.builder()
        .images(new ArrayList<>(Arrays.asList(itemImage1, itemImage2, itemImage3))).build();
    ProductL3UpdateRequest request =
        ProductL3UpdateRequest.builder().productItems(new ArrayList<>(Collections.singletonList(productItem))).build();

    CommonUtils.overrideImageExtensionToWebp(request);

    assertEquals("/item/test4.webp", request.getProductItems().get(0).getImages().get(0).getLocationPath());
  }

  @Test
  public void testOverrideImageExtensionToWebp_ignoresNonNewReviewType() {
    ProductLevel3SummaryDetailsImageRequest image =
        ProductLevel3SummaryDetailsImageRequest.builder().locationPath("/images/test6.jpg").reviewType("old").build();
    ProductL3UpdateRequest request =
        ProductL3UpdateRequest.builder().commonImages(new ArrayList<>(Collections.singletonList(image))).build();

    CommonUtils.overrideImageExtensionToWebp(request);

    assertEquals("/images/test6.jpg", request.getCommonImages().get(0).getLocationPath());
  }

  @Test
  public void testOverrideImageExtensionToWebp_handlesNullAndEmptyLists() {
    ProductL3UpdateRequest request = ProductL3UpdateRequest.builder().commonImages(null).productItems(null).build();
    CommonUtils.overrideImageExtensionToWebp(request);
    request = ProductL3UpdateRequest.builder().commonImages(new ArrayList<>()).productItems(new ArrayList<>()).build();
    CommonUtils.overrideImageExtensionToWebp(request);
    Assertions.assertNotNull(request);
  }

  @Test
  public void testOverrideImageExtensionToWebp_handlesNullLocationPath() {
    ProductLevel3SummaryDetailsImageRequest image =
        ProductLevel3SummaryDetailsImageRequest.builder().locationPath(null).reviewType(Constants.NEW).build();
    ProductL3UpdateRequest request =
        ProductL3UpdateRequest.builder().commonImages(new ArrayList<>(Collections.singletonList(image))).build();
    CommonUtils.overrideImageExtensionToWebp(request);
    assertNull(request.getCommonImages().get(0).getLocationPath());
  }

  @Test
  public void getInventoryDetailInfoRequestDTOList_withValidInputs_returnsExpectedDTOList() {
    QuickEditV2Request quickEdit1 = new QuickEditV2Request();
    quickEdit1.setItemSku(ITEM_SKU);
    quickEdit1.setPickupPointCode(PICKUP_POINT_CODE);
    QuickEditV2Request quickEdit2 = new QuickEditV2Request();
    quickEdit2.setItemSku(ITEM_SKU_2);
    quickEdit2.setPickupPointCode(PICKUP_POINT_CODE_1);
    List<QuickEditV2Request> quickEditV2Requests = Arrays.asList(quickEdit1, quickEdit2);
    List<InventoryDetailInfoRequestDTO> result =
      CommonUtils.getInventoryDetailInfoRequestDTOList(quickEditV2Requests, BUSINESS_PARTNER_CODE);
    Assertions.assertEquals(2, result.size());
    InventoryDetailInfoRequestDTO dto1 = result.get(0);
    Assertions.assertEquals(ITEM_SKU, dto1.getWebItemSku());
    InventoryDetailInfoRequestDTO dto2 = result.get(1);
    Assertions.assertEquals(PICKUP_POINT_CODE_1, dto2.getPickupPointCode());
  }

  @Test
  public void isPureInstoreProduct1() {
    Assertions.assertFalse(CommonUtils.isPureInstoreProduct(false, false, false));
  }
  @Test
  public void isPureInstoreProduct2() {
    Assertions.assertFalse(CommonUtils.isPureInstoreProduct(false, false, true));
  }
  @Test
  public void isPureInstoreProduct3() {
    Assertions.assertTrue(CommonUtils.isPureInstoreProduct(true, false, true));
  }
  @Test
  public void isPureInstoreProduct4() {
    Assertions.assertFalse(CommonUtils.isPureInstoreProduct(true, true, true));
  }
  @Test
  public void isPureInstoreProduct5() {
    Assertions.assertFalse(CommonUtils.isPureInstoreProduct(true, null, true));
  }

  @Test
  public void setMissingFieldsForPureInstoreProduct_withValidInputsAndPureInstoreProduct_returnsExpectedMissingFields() {
    ProductLevel3 product = new ProductLevel3();
    product.setOff2OnChannelActive(true);
    product.setB2cActivated(false);
    product.setHeight(Constants.DOUBLE_ZERO);
    product.setLength(Constants.DOUBLE_ZERO);
    product.setWeight(Constants.DOUBLE_ZERO);
    product.setShippingWeight(Constants.DOUBLE_ZERO);
    product.setProductType(ProductType.REGULAR.getCode());
    product.setDescription("");

    com.gdn.x.product.rest.web.model.request.ProductRequest productRequest = new  com.gdn.x.product.rest.web.model.request.ProductRequest();
    boolean instore2FlowSwitch = true;
    CommonUtils.setMissingFieldsForPureInstoreProduct(product, productRequest, instore2FlowSwitch);
    Set<String> missingFields = productRequest.getMissingFields();
    Assertions.assertNotNull(missingFields);
    Assertions.assertEquals(2, missingFields.size());
    Assertions.assertTrue(missingFields.contains(Constants.DIMENSIONS_MISSING));
    Assertions.assertTrue(missingFields.contains(Constants.DESCRIPTION_MISSING));
  }

  @Test
  public void setMissingFieldsForPureInstoreProduct_withValidInputsAndPureInstoreProduct_BOPIS_returnsExpectedMissingFields() {
    ProductLevel3 product = new ProductLevel3();
    product.setOff2OnChannelActive(true);
    product.setB2cActivated(false);
    product.setHeight(Constants.DOUBLE_ZERO);
    product.setLength(Constants.DOUBLE_ZERO);
    product.setWeight(Constants.DOUBLE_ZERO);
    product.setShippingWeight(Constants.DOUBLE_ZERO);
    product.setProductType(ProductType.BOPIS.getCode());
    product.setDescription("");

    com.gdn.x.product.rest.web.model.request.ProductRequest productRequest = new  com.gdn.x.product.rest.web.model.request.ProductRequest();
    boolean instore2FlowSwitch = true;
    CommonUtils.setMissingFieldsForPureInstoreProduct(product, productRequest, instore2FlowSwitch);
    Set<String> missingFields = productRequest.getMissingFields();
    Assertions.assertNotNull(missingFields);
  }


  @Test
  public void setMissingFieldsForPureInstoreProduct_withNonPureInstoreProduct_doesNotAddMissingFields() {
    ProductLevel3 product = new ProductLevel3();
    product.setOff2OnChannelActive(false);
    product.setB2cActivated(true);
    product.setHeight(1.0);
    product.setLength(1.0);
    product.setWeight(1.0);
    product.setShippingWeight(1.0);
    product.setProductType(ProductType.REGULAR.getCode());
    product.setDescription(DEFAULT_PICKUP_POINT_CODE);

    com.gdn.x.product.rest.web.model.request.ProductRequest productRequest = new  com.gdn.x.product.rest.web.model.request.ProductRequest();
    boolean instore2FlowSwitch = false;
    CommonUtils.setMissingFieldsForPureInstoreProduct(product, productRequest, instore2FlowSwitch);
    Set<String> missingFields = productRequest.getMissingFields();
    Assertions.assertNotNull(missingFields);
    Assertions.assertTrue(missingFields.isEmpty());
  }

  @Test
  public void setMissingFieldsForPureInstoreProduct_withMissingDimensionsForBOPISProduct_addsDimensionsMissingField() {
    ProductLevel3 product = new ProductLevel3();
    product.setOff2OnChannelActive(true);
    product.setB2cActivated(false);
    product.setHeight(Constants.DOUBLE_ZERO);
    product.setLength(Constants.DOUBLE_ZERO);
    product.setWeight(Constants.DOUBLE_ZERO);
    product.setShippingWeight(Constants.DOUBLE_ZERO);
    product.setProductType(ProductType.REGULAR.getCode());
    product.setDescription(PICKUP_POINT_CODE);
    com.gdn.x.product.rest.web.model.request.ProductRequest productRequest = new  com.gdn.x.product.rest.web.model.request.ProductRequest();
    boolean instore2FlowSwitch = true;
    CommonUtils.setMissingFieldsForPureInstoreProduct(product, productRequest, instore2FlowSwitch);
    Set<String> missingFields = productRequest.getMissingFields();
    Assertions.assertNotNull(missingFields);
    Assertions.assertEquals(1, missingFields.size()); // Only DIMENSIONS_MISSING should be added
    Assertions.assertTrue(missingFields.contains(Constants.DIMENSIONS_MISSING));
  }

  @Test
  public void setMissingFieldsForPureInstoreProduct_withValidProduct_noMissingFields() {
    ProductLevel3 product = new ProductLevel3();
    product.setOff2OnChannelActive(true);
    product.setB2cActivated(false);
    product.setHeight(1.0);
    product.setLength(1.0);
    product.setWeight(1.0);
    product.setShippingWeight(1.0);
    product.setProductType(ProductType.REGULAR.getCode());
    product.setDescription("Valid Description");
    com.gdn.x.product.rest.web.model.request.ProductRequest productRequest = new  com.gdn.x.product.rest.web.model.request.ProductRequest();
    boolean instore2FlowSwitch = true;
    CommonUtils.setMissingFieldsForPureInstoreProduct(product, productRequest, instore2FlowSwitch);
    Set<String> missingFields = productRequest.getMissingFields();
    Assertions.assertNotNull(missingFields);
  }

  @Test
  public void eligibleToCheckIfDescriptionIsChangedTest() {
    Assertions.assertFalse(
        CommonUtils.eligibleToCheckIfDescriptionIsChanged(new ProductLevel3(), new ProductLevel3(), true));
    ProductLevel3 productLevel3 = new ProductLevel3();
    productLevel3.setDescription(DEFAULT_PICKUP_POINT_CODE);
    ProductLevel3 productLevel3new = new ProductLevel3();
    Assertions.assertTrue(CommonUtils.eligibleToCheckIfDescriptionIsChanged(productLevel3, productLevel3, true));
    Assertions.assertTrue(CommonUtils.eligibleToCheckIfDescriptionIsChanged(productLevel3, productLevel3new, true));
    Assertions.assertFalse(
        CommonUtils.eligibleToCheckIfDescriptionIsChanged(new ProductLevel3(), new ProductLevel3(), false));
    Assertions.assertTrue(CommonUtils.eligibleToCheckIfDescriptionIsChanged(productLevel3, productLevel3, false));
    productLevel3.setDescription(DEFAULT_PICKUP_POINT_CODE);
    productLevel3new.setDescription(null);
    Assertions.assertFalse(CommonUtils.eligibleToCheckIfDescriptionIsChanged(productLevel3, productLevel3new, false));
    productLevel3new.setDescription(DEFAULT_PICKUP_POINT_CODE);
    productLevel3.setDescription(null);
    Assertions.assertFalse(CommonUtils.eligibleToCheckIfDescriptionIsChanged(productLevel3, productLevel3new, false));
  }

  @Test
  public void isDescriptionChangedTest() {
    ProductLevel3 productLevel = new ProductLevel3();
    ProductLevel3 productLevel3 = new ProductLevel3();
    productLevel.setDescription(StringUtils.EMPTY);
    productLevel3.setDescription(StringUtils.EMPTY);
    Assertions.assertFalse(CommonUtils.isDescriptionChanged(productLevel, productLevel3));
    productLevel3.setDescription(DEFAULT_PICKUP_POINT_CODE);
    Assertions.assertTrue(CommonUtils.isDescriptionChanged(productLevel, productLevel3));
  }

  @Test
  public void checkBrandModelForMerchantTypeTest() {
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setMerchantType(Constants.TD_MERCHANT);
    profileResponse.setCompany(companyDTO);
    Assertions.assertFalse(CommonUtils.skipBrandModelForMerchantType(profileResponse, "TD,TC"));
  }

  @Test
  public void checkBrandModelForMerchantTypeCompanyNullTest() {
    profileResponse.setCompany(null);
    Assertions.assertTrue(CommonUtils.skipBrandModelForMerchantType(profileResponse, "TD,TC"));
  }

  @Test
  public void checkBrandModelForMerchantTypeNullProfileResponseTest() {
    Assertions.assertTrue(CommonUtils.skipBrandModelForMerchantType(null, "TD,TC"));
  }

  @Test
  public void checkBrandModelForMerchantTypeTrueTest() {
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setMerchantType(Constants.CM_MERCHANT);
    profileResponse.setCompany(companyDTO);
    Assertions.assertTrue(CommonUtils.skipBrandModelForMerchantType(profileResponse, "TD,TC"));
  }

  @Test
  public void getItemPickupPointListingRequestsFromAddPickupPointListTest() {
    ItemPickupPointRequest addItemPickupPoint = new ItemPickupPointRequest();
    addItemPickupPoint.setItemSku(ITEM_SKU);
    addItemPickupPoint.setPickupPointId(PICKUP_POINT_CODE);
    List<ItemPickupPointListingRequest> response =
        CommonUtils.getItemPickupPointListingRequestsFromAddPickupPointList(BUSINESS_PARTNER_CODE,
            productVariantUpdateRequest, itemPickupPointListingResponseMap);
    Assertions.assertEquals(0, response.size());
    productVariantUpdateRequest.setAddPickupPoints(List.of(new ItemPickupPointRequest()));
    response = CommonUtils.getItemPickupPointListingRequestsFromAddPickupPointList(BUSINESS_PARTNER_CODE,
        productVariantUpdateRequest, itemPickupPointListingResponseMap);
    Assertions.assertEquals(1, response.size());
    productVariantUpdateRequest.setAddPickupPoints(List.of(addItemPickupPoint));
    response = CommonUtils.getItemPickupPointListingRequestsFromAddPickupPointList(BUSINESS_PARTNER_CODE,
        productVariantUpdateRequest, itemPickupPointListingResponseMap);
    Assertions.assertEquals(1, response.size());
    itemPickupPointListingResponseMap.put(ITEM_SKU.concat(Constants.HYPHEN).concat(PICKUP_POINT_CODE),
        new ItemPickupPointListingResponse());
    response = CommonUtils.getItemPickupPointListingRequestsFromAddPickupPointList(BUSINESS_PARTNER_CODE,
        productVariantUpdateRequest, itemPickupPointListingResponseMap);
    Assertions.assertEquals(0, response.size());
  }

  @Test
  public void rResetRestrictedKeywordToActionTypeMapForMultipleCategoryChangeKeywordsTest() {
    // Test Case 1: Empty Map
    Map<String, RestrictedKeywordsMappedToCategoryResponse> emptyMap = new HashMap<>();
    CommonUtils.resetRestrictedKeywordToActionTypeMapForMultipleCategoryChangeKeywords(emptyMap);
    Assertions.assertTrue(emptyMap.isEmpty());

    // Test Case 2: Single Entry Map
    Map<String, RestrictedKeywordsMappedToCategoryResponse> singleEntryMap = new HashMap<>();
    RestrictedKeywordsMappedToCategoryResponse response1 =
      new RestrictedKeywordsMappedToCategoryResponse();
    response1.setAction(
      RestrictedKeywordActionType.CHANGE_CATEGORY_AND_AUTO_APPROVE.getRestrictedKeywordActionType());
    response1.setDestinationCategory("category1");
    singleEntryMap.put("keyword1", response1);

    CommonUtils.resetRestrictedKeywordToActionTypeMapForMultipleCategoryChangeKeywords(
      singleEntryMap);
    Assertions.assertEquals("category1", singleEntryMap.get("keyword1").getDestinationCategory());
    Assertions.assertEquals(
      RestrictedKeywordActionType.CHANGE_CATEGORY_AND_AUTO_APPROVE.getRestrictedKeywordActionType(),
      singleEntryMap.get("keyword1").getAction());

    // Test Case 3: Multiple Entries with Same Destination Category
    Map<String, RestrictedKeywordsMappedToCategoryResponse> sameDestMap = new HashMap<>();
    RestrictedKeywordsMappedToCategoryResponse response2 =
      new RestrictedKeywordsMappedToCategoryResponse();
    response2.setAction(
      RestrictedKeywordActionType.CHANGE_CATEGORY_AND_AUTO_APPROVE.getRestrictedKeywordActionType());
    response2.setDestinationCategory("category1");

    RestrictedKeywordsMappedToCategoryResponse response3 =
      new RestrictedKeywordsMappedToCategoryResponse();
    response3.setAction(
      RestrictedKeywordActionType.CHANGE_CATEGORY_AND_AUTO_APPROVE.getRestrictedKeywordActionType());
    response3.setDestinationCategory("category1");

    sameDestMap.put("keyword1", response2);
    sameDestMap.put("keyword2", response3);

    CommonUtils.resetRestrictedKeywordToActionTypeMapForMultipleCategoryChangeKeywords(sameDestMap);
    Assertions.assertEquals("category1", sameDestMap.get("keyword1").getDestinationCategory());
    Assertions.assertEquals("category1", sameDestMap.get("keyword2").getDestinationCategory());
    Assertions.assertEquals(
      RestrictedKeywordActionType.CHANGE_CATEGORY_AND_AUTO_APPROVE.getRestrictedKeywordActionType(),
      sameDestMap.get("keyword1").getAction());
    Assertions.assertEquals(
      RestrictedKeywordActionType.CHANGE_CATEGORY_AND_AUTO_APPROVE.getRestrictedKeywordActionType(),
      sameDestMap.get("keyword2").getAction());

    // Test Case 4: Multiple Entries with Different Destination Categories
    Map<String, RestrictedKeywordsMappedToCategoryResponse> diffDestMap = new HashMap<>();
    RestrictedKeywordsMappedToCategoryResponse response4 =
      new RestrictedKeywordsMappedToCategoryResponse();
    response4.setAction(
      RestrictedKeywordActionType.CHANGE_CATEGORY_AND_AUTO_APPROVE.getRestrictedKeywordActionType());
    response4.setDestinationCategory("category1");

    RestrictedKeywordsMappedToCategoryResponse response5 =
      new RestrictedKeywordsMappedToCategoryResponse();
    response5.setAction(
      RestrictedKeywordActionType.CHANGE_CATEGORY_AND_AUTO_APPROVE.getRestrictedKeywordActionType());
    response5.setDestinationCategory("category2");

    diffDestMap.put("keyword1", response4);
    diffDestMap.put("keyword2", response5);

    CommonUtils.resetRestrictedKeywordToActionTypeMapForMultipleCategoryChangeKeywords(diffDestMap);
    Assertions.assertEquals("", diffDestMap.get("keyword1").getDestinationCategory());
    Assertions.assertEquals("", diffDestMap.get("keyword2").getDestinationCategory());
    Assertions.assertEquals(Constants.ONE, diffDestMap.get("keyword1").getAction());
    Assertions.assertEquals(Constants.ONE, diffDestMap.get("keyword2").getAction());

    // Test Case 5: Mixed Actions
    Map<String, RestrictedKeywordsMappedToCategoryResponse> mixedMap = new HashMap<>();
    RestrictedKeywordsMappedToCategoryResponse response6 =
      new RestrictedKeywordsMappedToCategoryResponse();
    response6.setAction(
      RestrictedKeywordActionType.CHANGE_CATEGORY_AND_AUTO_APPROVE.getRestrictedKeywordActionType());
    response6.setDestinationCategory("category1");

    RestrictedKeywordsMappedToCategoryResponse response7 =
      new RestrictedKeywordsMappedToCategoryResponse();
    response7.setAction(RestrictedKeywordActionType.AUTO_REJECT.getRestrictedKeywordActionType());
    response7.setDestinationCategory("category2");

    mixedMap.put("keyword1", response6);
    mixedMap.put("keyword2", response7);

    CommonUtils.resetRestrictedKeywordToActionTypeMapForMultipleCategoryChangeKeywords(mixedMap);
    Assertions.assertEquals("category1", mixedMap.get("keyword1").getDestinationCategory());
    Assertions.assertEquals("category2", mixedMap.get("keyword2").getDestinationCategory());
    Assertions.assertEquals(
      RestrictedKeywordActionType.CHANGE_CATEGORY_AND_AUTO_APPROVE.getRestrictedKeywordActionType(),
      mixedMap.get("keyword1").getAction());
    Assertions.assertEquals(
      RestrictedKeywordActionType.AUTO_REJECT.getRestrictedKeywordActionType(),
      mixedMap.get("keyword2").getAction());
  }

  @Test
  public void testResetRestrictedKeywordToActionTypeMapWithBlankDestinationCategory() {
    // Test case for category change action with blank destination category
    Map<String, RestrictedKeywordsMappedToCategoryResponse> mapWithBlankDest = new HashMap<>();

    // Entry 1: Category change with blank destination
    RestrictedKeywordsMappedToCategoryResponse response1 = new RestrictedKeywordsMappedToCategoryResponse();
    response1.setAction(RestrictedKeywordActionType.CHANGE_CATEGORY_AND_AUTO_APPROVE.getRestrictedKeywordActionType());
    response1.setDestinationCategory(""); // blank destination

    // Entry 2: Category change with valid destination
    RestrictedKeywordsMappedToCategoryResponse response2 = new RestrictedKeywordsMappedToCategoryResponse();
    response2.setAction(RestrictedKeywordActionType.CHANGE_CATEGORY_AND_AUTO_APPROVE.getRestrictedKeywordActionType());
    response2.setDestinationCategory("category1");

    mapWithBlankDest.put("keyword1", response1);
    mapWithBlankDest.put("keyword2", response2);

    CommonUtils.resetRestrictedKeywordToActionTypeMapForMultipleCategoryChangeKeywords(mapWithBlankDest);

    // Verify:
    // 1. Entry with blank destination should remain unchanged
    Assertions.assertEquals("", mapWithBlankDest.get("keyword1").getDestinationCategory());
    Assertions.assertEquals(RestrictedKeywordActionType.CHANGE_CATEGORY_AND_AUTO_APPROVE.getRestrictedKeywordActionType(),
      mapWithBlankDest.get("keyword1").getAction());

    // 2. Entry with valid destination should remain unchanged since only one valid destination exists
    Assertions.assertEquals("category1", mapWithBlankDest.get("keyword2").getDestinationCategory());
    Assertions.assertEquals(RestrictedKeywordActionType.CHANGE_CATEGORY_AND_AUTO_APPROVE.getRestrictedKeywordActionType(),
      mapWithBlankDest.get("keyword2").getAction());
  }

  @Test
  void isProductScoreGenerationNeededFalseTest() {
    Assertions.assertFalse(CommonUtils.isProductScoreGenerationNeeded(false,
      false, false));
  }

  @Test
  void isProductScoreGenerationNeededTest() {
    Assertions.assertTrue(CommonUtils.isProductScoreGenerationNeeded(true,
      false, false));

    Assertions.assertTrue(CommonUtils.isProductScoreGenerationNeeded(false,
      true, false));

    Assertions.assertTrue(CommonUtils.isProductScoreGenerationNeeded(false,
      false, true));
  }

  @Test
  void testSetYouTubeUrlUpdated_urlIsUpdated() {
    ProductLevel3 product = new ProductLevel3();
    product.setUrl(NEW_URL);

    Product savedProduct = new Product();
    savedProduct.setUrl(OLD_URL);

    EditProductResponse editProductResponse = new EditProductResponse();
    editProductResponse.setProduct(savedProduct);

    CommonUtils.setYouTubeUrlUpdated(product, editProductResponse, new ProductLevel3());

    Assertions.assertTrue(editProductResponse.isYouTubeUrlUpdated());
  }

  @Test
  void testSetYouTubeUrlUpdated_urlIsNotUpdated() {
    ProductLevel3 product = new ProductLevel3();
    product.setUrl(OLD_URL);
    Product savedProduct = new Product();
    savedProduct.setUrl(OLD_URL);

    EditProductResponse editProductResponse = new EditProductResponse();
    editProductResponse.setProduct(savedProduct);

    CommonUtils.setYouTubeUrlUpdated(product, editProductResponse, new ProductLevel3());

    Assertions.assertFalse(editProductResponse.isYouTubeUrlUpdated());
  }

  @Test
  void testSetYouTubeUrlUpdated_urlUpdatedFromNull() {
    ProductLevel3 product = new ProductLevel3();
    product.setUrl(NEW_URL);

    Product savedProduct = new Product();
    savedProduct.setUrl(null);

    EditProductResponse editProductResponse = new EditProductResponse();
    editProductResponse.setProduct(savedProduct);

    CommonUtils.setYouTubeUrlUpdated(product, editProductResponse, new ProductLevel3());

    Assertions.assertTrue(editProductResponse.isYouTubeUrlUpdated());
  }

  @Test
  void testSetYouTubeUrlUpdated_urlUpdatedToNull() {
    ProductLevel3 product = new ProductLevel3();
    product.setUrl(null);

    Product savedProduct = new Product();
    savedProduct.setUrl(OLD_URL);

    EditProductResponse editProductResponse = new EditProductResponse();
    editProductResponse.setProduct(savedProduct);

    CommonUtils.setYouTubeUrlUpdated(product, editProductResponse, new ProductLevel3());

    Assertions.assertTrue(editProductResponse.isYouTubeUrlUpdated());
  }

  @Test
  void testSetYouTubeUrlUpdated_bothUrlsNull() {
    ProductLevel3 product = new ProductLevel3();
    product.setUrl(null);

    Product savedProduct = new Product();
    savedProduct.setUrl(null);

    EditProductResponse editProductResponse = new EditProductResponse();
    editProductResponse.setProduct(savedProduct);

    CommonUtils.setYouTubeUrlUpdated(product, editProductResponse, new ProductLevel3());

    Assertions.assertFalse(editProductResponse.isYouTubeUrlUpdated());
  }

  @Test
  void testSetYouTubeUrlUpdated_editProductResponseProductNull_urlIsUpdated() {
    ProductLevel3 product = new ProductLevel3();
    product.setUrl(NEW_URL);

    ProductLevel3 productLevel3 = new ProductLevel3();
    productLevel3.setUrl(OLD_URL);

    EditProductResponse editProductResponse = new EditProductResponse();
    editProductResponse.setProduct(null);

    CommonUtils.setYouTubeUrlUpdated(product, editProductResponse, productLevel3);

    Assertions.assertTrue(editProductResponse.isYouTubeUrlUpdated());
  }

  @Test
  void testSetYouTubeUrlUpdated_editProductResponseProductNull_urlIsNotUpdated() {
    ProductLevel3 product = new ProductLevel3();
    product.setUrl(OLD_URL);

    ProductLevel3 productLevel3 = new ProductLevel3();
    productLevel3.setUrl(OLD_URL);

    EditProductResponse editProductResponse = new EditProductResponse();
    editProductResponse.setProduct(null);

    CommonUtils.setYouTubeUrlUpdated(product, editProductResponse, productLevel3);

    Assertions.assertFalse(editProductResponse.isYouTubeUrlUpdated());
  }

  @Test
  void testSetYouTubeUrlUpdated_editProductResponseProductNull_urlUpdatedFromNull() {
    ProductLevel3 product = new ProductLevel3();
    product.setUrl(NEW_URL);

    ProductLevel3 productLevel3 = new ProductLevel3();
    productLevel3.setUrl(null);

    EditProductResponse editProductResponse = new EditProductResponse();
    editProductResponse.setProduct(null);

    CommonUtils.setYouTubeUrlUpdated(product, editProductResponse, productLevel3);

    Assertions.assertTrue(editProductResponse.isYouTubeUrlUpdated());
  }

  @Test
  void testSetYouTubeUrlUpdated_editProductResponseProductNull_urlUpdatedToNull() {
    ProductLevel3 product = new ProductLevel3();
    product.setUrl(null);

    ProductLevel3 productLevel3 = new ProductLevel3();
    productLevel3.setUrl(OLD_URL);

    EditProductResponse editProductResponse = new EditProductResponse();
    editProductResponse.setProduct(null);

    CommonUtils.setYouTubeUrlUpdated(product, editProductResponse, productLevel3);

    Assertions.assertTrue(editProductResponse.isYouTubeUrlUpdated());
  }

  @Test
  void testSetYouTubeUrlUpdated_editProductResponseProductNull_bothUrlsNull() {
    ProductLevel3 product = new ProductLevel3();
    product.setUrl(null);

    ProductLevel3 productLevel3 = new ProductLevel3();
    productLevel3.setUrl(null);

    EditProductResponse editProductResponse = new EditProductResponse();
    editProductResponse.setProduct(null);

    CommonUtils.setYouTubeUrlUpdated(product, editProductResponse, productLevel3);

    Assertions.assertFalse(editProductResponse.isYouTubeUrlUpdated());
  }

  @Test
  void testProcessRestrictedKeywords_hasRestrictedKeywordsAndTrustedSellerPostLiveReview()
    throws JsonProcessingException {
    List<RestrictedKeywordsByField> restrictedKeywords = new ArrayList<>();
    RestrictedKeywordsByField field1 = new RestrictedKeywordsByField();
    field1.setFieldIdentifier(FIELD_IDENTIFIER_1);
    restrictedKeywords.add(field1);
    masterProductEditDTO.setRestrictedKeywordsByFieldList(restrictedKeywords);

    restrictedKeywordsByFieldAndActionType.setAction(RestrictedKeywordActionType.AUTO_REJECT.getRestrictedKeywordActionType());
    restrictedKeywordsByFieldAndActionType.setCategoryRestrictedKeywordId(CATEGORY_RESTRICTED_KEYWORD_ID);
    restrictedKeywordsByFieldAndActionType.setRestrictedKeywordsByFieldList(restrictedKeywords);

    productMasterDataEditRequest.setTrustedSeller(true);

    configurationStatusResponse.setReviewConfig(Constants.POST_LIVE);
    masterProductEditDTO.setConfigurationStatusResponseList(Collections.singletonList(configurationStatusResponse));
    productCollectionData.setReviewType("SOME_OTHER_REVIEW_TYPE");
    masterProductEditDTO.setRestrictedKeywordsByFieldAndActionType(restrictedKeywordsByFieldAndActionType);
    masterProductEditDTO.setRestrictedKeywordsByFieldList(restrictedKeywords);
    // When
    CommonUtils.processRestrictedKeywords(masterProductEditDTO, productMasterDataEditRequest);

    // Then
    Assertions.assertTrue(productCollectionData.isPostLive());
    Assertions.assertTrue(productCollectionData.isRestrictedKeywordsPresent());
    Assertions.assertEquals(new ObjectMapper().writeValueAsString(restrictedKeywords),
        productCollectionData.getRestrictedKeywordsDetected());
    Assertions.assertEquals(RestrictedKeywordActionType.AUTO_REJECT.getRestrictedKeywordActionType(),
      masterProductEditDTO.getAction());
    Assertions.assertEquals(CATEGORY_RESTRICTED_KEYWORD_ID, masterProductEditDTO.getCategoryRestrictedKeywordId());
    Assertions.assertTrue(masterProductEditDTO.getVendorErrorFields().contains(FIELD_IDENTIFIER_1));
    Assertions.assertTrue(masterProductEditDTO.getReviewTypeList().contains("SOME_OTHER_REVIEW_TYPE"));
  }

  @Test
  public void testProcessRestrictedKeywords_hasRestrictedKeywordsAndNotTrustedSeller() throws JsonProcessingException {
    // Given
    List<RestrictedKeywordsByField> restrictedKeywords = new ArrayList<>();
    RestrictedKeywordsByField field1 = new RestrictedKeywordsByField();
    field1.setFieldIdentifier(FIELD_IDENTIFIER_1);
    restrictedKeywords.add(field1);
    masterProductEditDTO.setRestrictedKeywordsByFieldList(restrictedKeywords);

    restrictedKeywordsByFieldAndActionType.setAction(RestrictedKeywordActionType.AUTO_REJECT.getRestrictedKeywordActionType());
    restrictedKeywordsByFieldAndActionType.setCategoryRestrictedKeywordId(CATEGORY_RESTRICTED_KEYWORD_ID);
    restrictedKeywordsByFieldAndActionType.setRestrictedKeywordsByFieldList(restrictedKeywords);
    masterProductEditDTO.setRestrictedKeywordsByFieldAndActionType(restrictedKeywordsByFieldAndActionType);
    masterProductEditDTO.setRestrictedKeywordsByFieldList(restrictedKeywords);
    productMasterDataEditRequest.setTrustedSeller(false);

    configurationStatusResponse.setReviewConfig(Constants.POST_LIVE);
    masterProductEditDTO.setConfigurationStatusResponseList(Collections.singletonList(configurationStatusResponse));
    productCollectionData.setReviewType("SOME_OTHER_REVIEW_TYPE");

    // When
    CommonUtils.processRestrictedKeywords(masterProductEditDTO, productMasterDataEditRequest);

    // Then
    Assertions.assertFalse(productCollectionData.isPostLive()); // reviewConfiguration should be false
    Assertions.assertTrue(productCollectionData.isRestrictedKeywordsPresent());
    Assertions.assertEquals(new ObjectMapper().writeValueAsString(restrictedKeywords),
        productCollectionData.getRestrictedKeywordsDetected());
    Assertions.assertEquals(RestrictedKeywordActionType.AUTO_REJECT.getRestrictedKeywordActionType(),
      masterProductEditDTO.getAction());
    Assertions.assertEquals(CATEGORY_RESTRICTED_KEYWORD_ID, masterProductEditDTO.getCategoryRestrictedKeywordId());
    Assertions.assertTrue(masterProductEditDTO.getVendorErrorFields().contains(FIELD_IDENTIFIER_1));
    Assertions.assertTrue(masterProductEditDTO.getReviewTypeList().contains("SOME_OTHER_REVIEW_TYPE"));
  }

  @Test
  public void testProcessRestrictedKeywords_hasRestrictedKeywordsAndReviewConfigNotPostLive() throws JsonProcessingException {
    // Given
    List<RestrictedKeywordsByField> restrictedKeywords = new ArrayList<>();
    RestrictedKeywordsByField field1 = new RestrictedKeywordsByField();
    field1.setFieldIdentifier(FIELD_IDENTIFIER_1);
    restrictedKeywords.add(field1);
    masterProductEditDTO.setRestrictedKeywordsByFieldList(restrictedKeywords);
    restrictedKeywordsByFieldAndActionType = new RestrictedKeywordsByFieldAndActionType();
    restrictedKeywordsByFieldAndActionType.setAction(RestrictedKeywordActionType.AUTO_REJECT.getRestrictedKeywordActionType());
    restrictedKeywordsByFieldAndActionType.setCategoryRestrictedKeywordId(CATEGORY_RESTRICTED_KEYWORD_ID);
    restrictedKeywordsByFieldAndActionType.setRestrictedKeywordsByFieldList(restrictedKeywords);
    masterProductEditDTO.setRestrictedKeywordsByFieldAndActionType(restrictedKeywordsByFieldAndActionType);
    masterProductEditDTO.setRestrictedKeywordsByFieldList(restrictedKeywords);
    productMasterDataEditRequest.setTrustedSeller(true);
    masterProductEditDTO.setRestrictedKeywordsByFieldAndActionType(restrictedKeywordsByFieldAndActionType);
    masterProductEditDTO.setRestrictedKeywordsByFieldList(restrictedKeywords);
    configurationStatusResponse.setReviewConfig("SOME_OTHER_CONFIG");
    masterProductEditDTO.setConfigurationStatusResponseList(Collections.singletonList(configurationStatusResponse));
    productCollectionData.setReviewType("SOME_OTHER_REVIEW_TYPE");

    // When
    CommonUtils.processRestrictedKeywords(masterProductEditDTO, productMasterDataEditRequest);

    // Then
    Assertions.assertFalse(productCollectionData.isPostLive()); // reviewConfiguration should be false
    Assertions.assertTrue(productCollectionData.isRestrictedKeywordsPresent());
    Assertions.assertEquals(new ObjectMapper().writeValueAsString(restrictedKeywords),
        productCollectionData.getRestrictedKeywordsDetected());
    Assertions.assertEquals(RestrictedKeywordActionType.AUTO_REJECT.getRestrictedKeywordActionType(),
      masterProductEditDTO.getAction());
    Assertions.assertEquals(CATEGORY_RESTRICTED_KEYWORD_ID, masterProductEditDTO.getCategoryRestrictedKeywordId());
    Assertions.assertTrue(masterProductEditDTO.getVendorErrorFields().contains(FIELD_IDENTIFIER_1));
    Assertions.assertTrue(masterProductEditDTO.getReviewTypeList().contains("SOME_OTHER_REVIEW_TYPE"));
  }

  @Test
  public void testProcessRestrictedKeywords_hasRestrictedKeywordsByFieldAndActionTypeOnly() throws JsonProcessingException {
    // Given
    List<RestrictedKeywordsByField> restrictedKeywordsByFieldAction = new ArrayList<>();
    RestrictedKeywordsByField field2 = new RestrictedKeywordsByField();
    field2.setFieldIdentifier(FIELD_IDENTIFIER_2);
    restrictedKeywordsByFieldAction.add(field2);

    masterProductEditDTO.setRestrictedKeywordsByFieldList(Collections.emptyList());
    restrictedKeywordsByFieldAndActionType.setRestrictedKeywordsByFieldList(restrictedKeywordsByFieldAction);
    productCollectionData.setReviewType("SOME_OTHER_REVIEW_TYPE");
    masterProductEditDTO.setRestrictedKeywordsByFieldAndActionType(restrictedKeywordsByFieldAndActionType);
    // When
    CommonUtils.processRestrictedKeywords(masterProductEditDTO, productMasterDataEditRequest);

    // Then
    Assertions.assertFalse(productCollectionData.isPostLive()); // Should remain false as review config not set
    Assertions.assertTrue(productCollectionData.isRestrictedKeywordsPresent());
    Assertions.assertEquals(new ObjectMapper().writeValueAsString(restrictedKeywordsByFieldAction),
        productCollectionData.getRestrictedKeywordsDetected());
    Assertions.assertEquals(1, masterProductEditDTO.getAction()); // Default value as action not
    // set from hasRestrictedKeywords
    assertNull(masterProductEditDTO.getCategoryRestrictedKeywordId()); // Should be null
    Assertions.assertTrue(masterProductEditDTO.getVendorErrorFields().isEmpty()); // Should be empty
    Assertions.assertTrue(masterProductEditDTO.getReviewTypeList().contains("SOME_OTHER_REVIEW_TYPE"));
  }

  @Test
  public void testProcessRestrictedKeywords_noRestrictedKeywords() throws JsonProcessingException {
    // Given
    masterProductEditDTO.setRestrictedKeywordsByFieldList(Collections.emptyList());
    restrictedKeywordsByFieldAndActionType.setRestrictedKeywordsByFieldList(Collections.emptyList());
    productCollectionData.setReviewType("SOME_OTHER_REVIEW_TYPE");

    // When
    CommonUtils.processRestrictedKeywords(masterProductEditDTO, productMasterDataEditRequest);

    // Then
    Assertions.assertFalse(productCollectionData.isPostLive());
    Assertions.assertFalse(productCollectionData.isRestrictedKeywordsPresent());
    Assertions.assertNull(productCollectionData.getRestrictedKeywordsDetected());
    Assertions.assertEquals(1, masterProductEditDTO.getAction());
    assertNull(masterProductEditDTO.getCategoryRestrictedKeywordId());
    Assertions.assertTrue(masterProductEditDTO.getVendorErrorFields().isEmpty());
    Assertions.assertTrue(masterProductEditDTO.getReviewTypeList().contains("SOME_OTHER_REVIEW_TYPE"));
  }

  @Test
  public void testProcessRestrictedKeywords_nullReviewType() throws JsonProcessingException {
    // Given
    List<RestrictedKeywordsByField> restrictedKeywords = new ArrayList<>();
    RestrictedKeywordsByField field1 = new RestrictedKeywordsByField();
    field1.setFieldIdentifier(FIELD_IDENTIFIER_1);
    restrictedKeywords.add(field1);
    masterProductEditDTO.setRestrictedKeywordsByFieldList(restrictedKeywords);

    restrictedKeywordsByFieldAndActionType.setAction(RestrictedKeywordActionType.AUTO_REJECT.getRestrictedKeywordActionType());
    restrictedKeywordsByFieldAndActionType.setCategoryRestrictedKeywordId(CATEGORY_RESTRICTED_KEYWORD_ID);
    restrictedKeywordsByFieldAndActionType.setRestrictedKeywordsByFieldList(restrictedKeywords);

    productMasterDataEditRequest.setTrustedSeller(true);
    configurationStatusResponse.setReviewConfig(Constants.POST_LIVE);
    masterProductEditDTO.setConfigurationStatusResponseList(Collections.singletonList(configurationStatusResponse));
    productCollectionData.setReviewType(null);

    // When
    CommonUtils.processRestrictedKeywords(masterProductEditDTO, productMasterDataEditRequest);

    // Then
    Assertions.assertTrue(masterProductEditDTO.getReviewTypeList().isEmpty());
  }

  @Test
  void testHandleAutoApprovalOnMasterDataEdit_contentAndImageAutoApprovalType_reviewPending() {
    masterProductEditDTO.setAutoApprovalType(AutoApprovalType.CONTENT_AND_IMAGE);
    productCollectionData.setReviewPending(true);
    masterProductEditDTO.setProductCollection(productCollectionData);

    CommonUtils.handleAutoApprovalOnMasterDataEdit(masterProductEditDTO);

    Assertions.assertTrue(masterProductEditDTO.isSaveInternalHistory());
    Assertions.assertTrue(masterProductEditDTO.isAutoApprovalEligible());
    Assertions.assertEquals(EditedReviewTypeConstants.CONTENT_REFRESH, masterProductEditDTO.getContentType());
    Assertions.assertTrue(productCollectionData.isEdited());
  }

  @Test
  void testHandleAutoApprovalOnMasterDataEdit_contentAndImageAutoApprovalType_notReviewPending() {
    masterProductEditDTO.setAutoApprovalType(AutoApprovalType.CONTENT_AND_IMAGE);
    productCollectionData.setReviewPending(false);
    masterProductEditDTO.setProductCollection(productCollectionData);

    CommonUtils.handleAutoApprovalOnMasterDataEdit(masterProductEditDTO);

    Assertions.assertTrue(masterProductEditDTO.isSaveInternalHistory());
    Assertions.assertTrue(masterProductEditDTO.isAutoApprovalEligible());
    Assertions.assertNull(masterProductEditDTO.getContentType()); // Should remain null or its initial value
    Assertions.assertFalse(productCollectionData.isEdited()); // Should remain false or its initial value
  }

  @Test
  void testHandleAutoApprovalOnMasterDataEdit_shouldTriggerReview_notPostLive() {
    masterProductEditDTO.setAutoApprovalType(AutoApprovalType.NA);
    masterProductEditDTO.setContentChanged(true);
    masterProductEditDTO.setRestrictedKeywordsByFieldList(Collections.singletonList(new RestrictedKeywordsByField())); // Make publishPDTContentEditEvent return true
    productCollectionData.setPostLive(false);
    productCollectionData.setReviewPending(false);
    masterProductEditDTO.setProductCollection(productCollectionData);
    masterProductEditDTO.setReviewTypeList(new ArrayList<>()); // Initialize to avoid NPE

    CommonUtils.handleAutoApprovalOnMasterDataEdit(masterProductEditDTO);

    Assertions.assertTrue(productCollectionData.isReviewPending());
    Assertions.assertTrue(productCollectionData.isEdited());
    Assertions.assertFalse(masterProductEditDTO.isAutoApprovalEligible());
    Assertions.assertEquals(EditedReviewTypeConstants.CONTENT_EDIT, masterProductEditDTO.getContentType());
    Assertions.assertTrue(masterProductEditDTO.getReviewTypeList().contains(EditedReviewTypeConstants.CONTENT_EDIT));
    Assertions.assertTrue(masterProductEditDTO.isTakenDownProduct());
  }

  @Test
  void testHandleAutoApprovalOnMasterDataEdit_shouldTriggerReview_postLive() {
    masterProductEditDTO.setAutoApprovalType(AutoApprovalType.NA);
    masterProductEditDTO.setContentChanged(true);
    masterProductEditDTO.setRestrictedKeywordsByFieldList(Collections.singletonList(new RestrictedKeywordsByField())); // Make publishPDTContentEditEvent return true
    productCollectionData.setPostLive(true);
    productCollectionData.setReviewPending(false);
    masterProductEditDTO.setProductCollection(productCollectionData);
    masterProductEditDTO.setReviewTypeList(new ArrayList<>()); // Initialize to avoid NPE

    CommonUtils.handleAutoApprovalOnMasterDataEdit(masterProductEditDTO);

    Assertions.assertTrue(productCollectionData.isReviewPending());
    Assertions.assertTrue(productCollectionData.isEdited());
    Assertions.assertFalse(masterProductEditDTO.isAutoApprovalEligible());
    Assertions.assertEquals(EditedReviewTypeConstants.CONTENT_EDIT, masterProductEditDTO.getContentType());
    Assertions.assertTrue(masterProductEditDTO.getReviewTypeList().contains(EditedReviewTypeConstants.CONTENT_EDIT));
    Assertions.assertFalse(masterProductEditDTO.isTakenDownProduct()); // Should not be taken down if post live
  }

  @Test
  void testHandleAutoApprovalOnMasterDataEdit_ContentEdit() {
    masterProductEditDTO.setAutoApprovalType(AutoApprovalType.NA);
    masterProductEditDTO.setContentChanged(true);
    masterProductEditDTO.setRestrictedKeywordsByFieldList(Collections.singletonList(new RestrictedKeywordsByField())); // Make publishPDTContentEditEvent return true
    productCollectionData.setPostLive(true);
    productCollectionData.setReviewPending(false);
    masterProductEditDTO.setProductCollection(productCollectionData);
    masterProductEditDTO.setReviewTypeList(Collections.singletonList(EditedReviewTypeConstants.CONTENT_EDIT)); // Initialize to avoid
    // NPE

    CommonUtils.handleAutoApprovalOnMasterDataEdit(masterProductEditDTO);

    Assertions.assertTrue(productCollectionData.isReviewPending());
    Assertions.assertTrue(productCollectionData.isEdited());
    Assertions.assertFalse(masterProductEditDTO.isAutoApprovalEligible());
    Assertions.assertEquals(EditedReviewTypeConstants.CONTENT_EDIT, masterProductEditDTO.getContentType());
    Assertions.assertTrue(masterProductEditDTO.getReviewTypeList().contains(EditedReviewTypeConstants.CONTENT_EDIT));
    Assertions.assertFalse(masterProductEditDTO.isTakenDownProduct()); // Should not be taken down if post live
  }

  @Test
  void testHandleAutoApprovalOnMasterDataEdit_shouldNotTriggerReview_reviewPending_notPostLive() {
    masterProductEditDTO.setAutoApprovalType(AutoApprovalType.NA);
    masterProductEditDTO.setContentChanged(false);
    masterProductEditDTO.setRestrictedKeywordsByFieldList(Collections.emptyList()); // Make publishPDTContentEditEvent return false
    productCollectionData.setReviewPending(true);
    masterProductEditDTO.setPostLive(false);
    masterProductEditDTO.setProductCollection(productCollectionData);

    CommonUtils.handleAutoApprovalOnMasterDataEdit(masterProductEditDTO);

    Assertions.assertTrue(productCollectionData.isEdited());
    Assertions.assertEquals(EditedReviewTypeConstants.CONTENT_REFRESH, masterProductEditDTO.getContentType());
    Assertions.assertFalse(masterProductEditDTO.isAutoApprovalEligible());
    Assertions.assertTrue(masterProductEditDTO.isTakenDownProduct());
  }

  @Test
  void testHandleAutoApprovalOnMasterDataEdit_shouldNotTriggerReview_reviewPending_postLive() {
    masterProductEditDTO.setAutoApprovalType(AutoApprovalType.NA);
    masterProductEditDTO.setContentChanged(false);
    masterProductEditDTO.setRestrictedKeywordsByFieldList(Collections.emptyList()); // Make publishPDTContentEditEvent return false
    productCollectionData.setReviewPending(true);
    masterProductEditDTO.setPostLive(true);
    masterProductEditDTO.setProductCollection(productCollectionData);

    CommonUtils.handleAutoApprovalOnMasterDataEdit(masterProductEditDTO);

    Assertions.assertTrue(productCollectionData.isEdited());
    Assertions.assertEquals(EditedReviewTypeConstants.CONTENT_REFRESH, masterProductEditDTO.getContentType());
    Assertions.assertFalse(masterProductEditDTO.isAutoApprovalEligible());
    Assertions.assertFalse(masterProductEditDTO.isTakenDownProduct()); // Should not be taken down if post live
  }

  @Test
  void testHandleAutoApprovalOnMasterDataEdit_shouldNotTriggerReview_notReviewPending() {
    masterProductEditDTO.setAutoApprovalType(AutoApprovalType.NA);
    masterProductEditDTO.setContentChanged(false);
    masterProductEditDTO.setRestrictedKeywordsByFieldList(Collections.emptyList()); // Make publishPDTContentEditEvent return false
    productCollectionData.setReviewPending(false);
    masterProductEditDTO.setProductCollection(productCollectionData);

    CommonUtils.handleAutoApprovalOnMasterDataEdit(masterProductEditDTO);

    Assertions.assertFalse(productCollectionData.isEdited());
    Assertions.assertNull(masterProductEditDTO.getContentType());
    Assertions.assertFalse(masterProductEditDTO.isAutoApprovalEligible());
    Assertions.assertFalse(masterProductEditDTO.isTakenDownProduct());
  }

  @Test
  void testGenerateModifiedFieldsForMasterDataEdit_withDescriptionUpdate() {
    // Given
    Set<L3InfoUpdateChangeType> changeTypes = EnumSet.of(L3InfoUpdateChangeType.DESCRIPTION_UPDATE);
    MasterProductEditDTO masterProductEditDTO = new MasterProductEditDTO();
    masterProductEditDTO.setModifiedFields(new HashSet<>());

    // When
    CommonUtils.generateModifiedFieldsForMasterDataEdit(changeTypes, masterProductEditDTO);

    // Then
    Assertions.assertEquals(1, masterProductEditDTO.getModifiedFields().size());
    Assertions.assertTrue(masterProductEditDTO.getModifiedFields().contains(UpdateProductActivity.PRODUCT_DESC.name()));
  }

  @Test
  void testGenerateModifiedFieldsForMasterDataEdit_withProductNameUpdate() {
    // Given
    Set<L3InfoUpdateChangeType> changeTypes = EnumSet.of(L3InfoUpdateChangeType.PRODUCT_NAME_UPDATE);
    MasterProductEditDTO masterProductEditDTO = new MasterProductEditDTO();
    masterProductEditDTO.setModifiedFields(new HashSet<>());

    // When
    CommonUtils.generateModifiedFieldsForMasterDataEdit(changeTypes, masterProductEditDTO);

    // Then
    Assertions.assertEquals(1, masterProductEditDTO.getModifiedFields().size());
    Assertions.assertTrue(masterProductEditDTO.getModifiedFields().contains(UpdateProductActivity.PRODUCT_NAME.name()));
  }

  @Test
  void testGenerateModifiedFieldsForMasterDataEdit_withYouTubeUrlUpdate() {
    // Given
    Set<L3InfoUpdateChangeType> changeTypes = EnumSet.of(L3InfoUpdateChangeType.YOUTUBE_URL_UPDATE);
    MasterProductEditDTO masterProductEditDTO = new MasterProductEditDTO();
    masterProductEditDTO.setModifiedFields(new HashSet<>());

    // When
    CommonUtils.generateModifiedFieldsForMasterDataEdit(changeTypes, masterProductEditDTO);

    // Then
    Assertions.assertEquals(1, masterProductEditDTO.getModifiedFields().size());
    Assertions.assertTrue(masterProductEditDTO.getModifiedFields().contains(Constants.URL_VIDEO_EDITED));
  }

  @Test
  void testGenerateModifiedFieldsForMasterDataEdit_withProductTypeUpdate() {
    // Given
    Set<L3InfoUpdateChangeType> changeTypes = EnumSet.of(L3InfoUpdateChangeType.PRODUCT_TYPE_UPDATE);
    MasterProductEditDTO masterProductEditDTO = new MasterProductEditDTO();
    masterProductEditDTO.setModifiedFields(new HashSet<>());

    // When
    CommonUtils.generateModifiedFieldsForMasterDataEdit(changeTypes, masterProductEditDTO);

    // Then
    Assertions.assertEquals(1, masterProductEditDTO.getModifiedFields().size());
    Assertions.assertTrue(masterProductEditDTO.getModifiedFields().contains(UpdateProductActivity.PRODUCT_TYPE.name()));
  }

  @Test
  void testGenerateModifiedFieldsForMasterDataEdit_withMultipleUpdates() {
    // Given
    Set<L3InfoUpdateChangeType> changeTypes = EnumSet.of(
        L3InfoUpdateChangeType.DESCRIPTION_UPDATE,
        L3InfoUpdateChangeType.PRODUCT_NAME_UPDATE,
        L3InfoUpdateChangeType.YOUTUBE_URL_UPDATE,
        L3InfoUpdateChangeType.PRODUCT_TYPE_UPDATE
    );
    MasterProductEditDTO masterProductEditDTO = new MasterProductEditDTO();
    masterProductEditDTO.setModifiedFields(new HashSet<>());

    // When
    CommonUtils.generateModifiedFieldsForMasterDataEdit(changeTypes, masterProductEditDTO);

    // Then
    Assertions.assertEquals(4, masterProductEditDTO.getModifiedFields().size());
    Assertions.assertTrue(masterProductEditDTO.getModifiedFields().contains(UpdateProductActivity.PRODUCT_DESC.name()));
    Assertions.assertTrue(masterProductEditDTO.getModifiedFields().contains(UpdateProductActivity.PRODUCT_NAME.name()));
    Assertions.assertTrue(masterProductEditDTO.getModifiedFields().contains(Constants.URL_VIDEO_EDITED));
    Assertions.assertTrue(masterProductEditDTO.getModifiedFields().contains(UpdateProductActivity.PRODUCT_TYPE.name()));
  }
  @Test
  void testGenerateModifiedFieldsForMasterDataEditNull() {
    // Given
    Set<L3InfoUpdateChangeType> changeTypes = EnumSet.of(
      L3InfoUpdateChangeType.DESCRIPTION_UPDATE,
      L3InfoUpdateChangeType.PRODUCT_NAME_UPDATE,
      L3InfoUpdateChangeType.YOUTUBE_URL_UPDATE,
      L3InfoUpdateChangeType.PRODUCT_TYPE_UPDATE
    );
    MasterProductEditDTO masterProductEditDTO = new MasterProductEditDTO();
    masterProductEditDTO.setModifiedFields(null);
    CommonUtils.generateModifiedFieldsForMasterDataEdit(changeTypes, masterProductEditDTO);

    // Then
    Assertions.assertEquals(4, masterProductEditDTO.getModifiedFields().size());
  }


  @Test
  void testGenerateModifiedFieldsForMasterDataEdit_withEmptyChangeTypes() {
    // Given
    Set<L3InfoUpdateChangeType> changeTypes = EnumSet.noneOf(L3InfoUpdateChangeType.class);
    MasterProductEditDTO masterProductEditDTO = new MasterProductEditDTO();
    masterProductEditDTO.setModifiedFields(new HashSet<>());

    // When
    CommonUtils.generateModifiedFieldsForMasterDataEdit(changeTypes, masterProductEditDTO);

    // Then
    Assertions.assertEquals(0, masterProductEditDTO.getModifiedFields().size());
  }

  @Test
  void testGenerateModifiedFieldsForMasterDataEdit_withUnsupportedChangeTypes() {
    // Given - using change types that are not handled in the switch statement
    Set<L3InfoUpdateChangeType> changeTypes = EnumSet.of(
        L3InfoUpdateChangeType.INSTORE_UPDATE,
        L3InfoUpdateChangeType.COMMON_IMAGE_UPDATE,
        L3InfoUpdateChangeType.VIDEO_UPDATE,
        L3InfoUpdateChangeType.DIMENSIONS_UPDATE,
        L3InfoUpdateChangeType.SIZE_CHART_UPDATE
    );
    MasterProductEditDTO masterProductEditDTO = new MasterProductEditDTO();
    masterProductEditDTO.setModifiedFields(new HashSet<>());

    // When
    CommonUtils.generateModifiedFieldsForMasterDataEdit(changeTypes, masterProductEditDTO);

    // Then - no fields should be added as these change types are not handled
    Assertions.assertEquals(0, masterProductEditDTO.getModifiedFields().size());
  }

  @Test
  void testGenerateModifiedFieldsForMasterDataEdit_withExistingModifiedFields() {
    // Given
    Set<L3InfoUpdateChangeType> changeTypes = EnumSet.of(L3InfoUpdateChangeType.DESCRIPTION_UPDATE);
    MasterProductEditDTO masterProductEditDTO = new MasterProductEditDTO();
    Set<String> existingModifiedFields = new HashSet<>();
    existingModifiedFields.add("EXISTING_FIELD");
    masterProductEditDTO.setModifiedFields(existingModifiedFields);

    // When
    CommonUtils.generateModifiedFieldsForMasterDataEdit(changeTypes, masterProductEditDTO);

    // Then
    Assertions.assertEquals(2, masterProductEditDTO.getModifiedFields().size());
    Assertions.assertTrue(masterProductEditDTO.getModifiedFields().contains("EXISTING_FIELD"));
    Assertions.assertTrue(masterProductEditDTO.getModifiedFields().contains(UpdateProductActivity.PRODUCT_DESC.name()));
  }

  @Test
  void testGenerateModifiedFieldsForMasterDataEdit_withDuplicateFields() {
    // Given
    Set<L3InfoUpdateChangeType> changeTypes = EnumSet.of(L3InfoUpdateChangeType.DESCRIPTION_UPDATE);
    MasterProductEditDTO masterProductEditDTO = new MasterProductEditDTO();
    Set<String> existingModifiedFields = new HashSet<>();
    existingModifiedFields.add(UpdateProductActivity.PRODUCT_DESC.name()); // Already contains the field
    masterProductEditDTO.setModifiedFields(existingModifiedFields);

    // When
    CommonUtils.generateModifiedFieldsForMasterDataEdit(changeTypes, masterProductEditDTO);

    // Then - should not add duplicate, Set should maintain uniqueness
    Assertions.assertEquals(1, masterProductEditDTO.getModifiedFields().size());
    Assertions.assertTrue(masterProductEditDTO.getModifiedFields().contains(UpdateProductActivity.PRODUCT_DESC.name()));
  }

  @Test
  void testGenerateModifiedFieldsForMasterDataEdit_withMixedSupportedAndUnsupportedChangeTypes() {
    // Given
    Set<L3InfoUpdateChangeType> changeTypes = EnumSet.of(
        L3InfoUpdateChangeType.DESCRIPTION_UPDATE, // supported
        L3InfoUpdateChangeType.INSTORE_UPDATE,     // not supported
        L3InfoUpdateChangeType.PRODUCT_NAME_UPDATE, // supported
        L3InfoUpdateChangeType.COMMON_IMAGE_UPDATE  // not supported
    );
    MasterProductEditDTO masterProductEditDTO = new MasterProductEditDTO();
    masterProductEditDTO.setModifiedFields(new HashSet<>());

    // When
    CommonUtils.generateModifiedFieldsForMasterDataEdit(changeTypes, masterProductEditDTO);

    // Then - only the supported change types should add fields
    Assertions.assertEquals(2, masterProductEditDTO.getModifiedFields().size());
    Assertions.assertTrue(masterProductEditDTO.getModifiedFields().contains(UpdateProductActivity.PRODUCT_DESC.name()));
    Assertions.assertTrue(masterProductEditDTO.getModifiedFields().contains(UpdateProductActivity.PRODUCT_NAME.name()));
  }

  @Test
  void testGenerateModifiedFieldsForMasterDataEdit_withAllSupportedChangeTypes() {
    // Given - all change types that are handled in the switch statement
    Set<L3InfoUpdateChangeType> changeTypes = EnumSet.of(
        L3InfoUpdateChangeType.DESCRIPTION_UPDATE,
        L3InfoUpdateChangeType.PRODUCT_NAME_UPDATE,
        L3InfoUpdateChangeType.YOUTUBE_URL_UPDATE,
        L3InfoUpdateChangeType.PRODUCT_TYPE_UPDATE
    );
    MasterProductEditDTO masterProductEditDTO = new MasterProductEditDTO();
    masterProductEditDTO.setModifiedFields(new HashSet<>());

    // When
    CommonUtils.generateModifiedFieldsForMasterDataEdit(changeTypes, masterProductEditDTO);

    // Then
    Assertions.assertEquals(4, masterProductEditDTO.getModifiedFields().size());
    Assertions.assertTrue(masterProductEditDTO.getModifiedFields().contains(UpdateProductActivity.PRODUCT_DESC.name()));
    Assertions.assertTrue(masterProductEditDTO.getModifiedFields().contains(UpdateProductActivity.PRODUCT_NAME.name()));
    Assertions.assertTrue(masterProductEditDTO.getModifiedFields().contains(Constants.URL_VIDEO_EDITED));
    Assertions.assertTrue(masterProductEditDTO.getModifiedFields().contains(UpdateProductActivity.PRODUCT_TYPE.name()));
  }

  @Test
  void testEvaluateLateFulfillmentOnShippingTypeChange_productTypeNotChanged_returnOriginalValue() {
    Integer productType = ProductType.REGULAR.getCode();
    boolean isProductTypeChanged = false;
    Boolean lateFulfillment = true;
    Assertions.assertEquals(lateFulfillment, CommonUtils.evaluateLateFulfillmentOnShippingTypeChange(productType, isProductTypeChanged, lateFulfillment));
  }

  @Test
  void testEvaluateLateFulfillmentOnShippingTypeChange_productTypeNotChanged_returnOriginalValueFalse() {
    Integer productType = ProductType.BIG_PRODUCT.getCode();
    boolean isProductTypeChanged = false;
    Boolean lateFulfillment = false;
    Assertions.assertEquals(lateFulfillment, CommonUtils.evaluateLateFulfillmentOnShippingTypeChange(productType, isProductTypeChanged, lateFulfillment));
  }

  @Test
  void testEvaluateLateFulfillmentOnShippingTypeChange_productTypeNull_returnOriginalValue() {
    Integer productType = null;
    boolean isProductTypeChanged = true;
    Boolean lateFulfillment = true;
    Assertions.assertEquals(lateFulfillment, CommonUtils.evaluateLateFulfillmentOnShippingTypeChange(productType, isProductTypeChanged, lateFulfillment));
  }

  @Test
  void testEvaluateLateFulfillmentOnShippingTypeChange_productTypeNull_returnOriginalValueFalse() {
    Integer productType = null;
    boolean isProductTypeChanged = true;
    Boolean lateFulfillment = false;
    Assertions.assertEquals(lateFulfillment, CommonUtils.evaluateLateFulfillmentOnShippingTypeChange(productType, isProductTypeChanged, lateFulfillment));
  }

  @Test
  void testEvaluateLateFulfillmentOnShippingTypeChange_productTypeChangedToRegular_returnFalse() {
    Integer productType = ProductType.REGULAR.getCode();
    boolean isProductTypeChanged = true;
    Boolean lateFulfillment = true;
    Assertions.assertFalse(CommonUtils.evaluateLateFulfillmentOnShippingTypeChange(productType, isProductTypeChanged, lateFulfillment));
  }

  @Test
  void testEvaluateLateFulfillmentOnShippingTypeChange_productTypeChangedToRegular_returnFalseWhenOriginalFalse() {
    Integer productType = ProductType.REGULAR.getCode();
    boolean isProductTypeChanged = true;
    Boolean lateFulfillment = false;
    Assertions.assertFalse(CommonUtils.evaluateLateFulfillmentOnShippingTypeChange(productType, isProductTypeChanged, lateFulfillment));
  }

  @Test
  void testEvaluateLateFulfillmentOnShippingTypeChange_productTypeChangedToBigProduct_returnTrue() {
    Integer productType = ProductType.BIG_PRODUCT.getCode();
    boolean isProductTypeChanged = true;
    Boolean lateFulfillment = false;
    Assertions.assertTrue(CommonUtils.evaluateLateFulfillmentOnShippingTypeChange(productType, isProductTypeChanged, lateFulfillment));
  }

  @Test
  void testEvaluateLateFulfillmentOnShippingTypeChange_productTypeChangedToBigProduct_returnTrueWhenOriginalTrue() {
    Integer productType = ProductType.BIG_PRODUCT.getCode();
    boolean isProductTypeChanged = true;
    Boolean lateFulfillment = true;
    Boolean result = CommonUtils.evaluateLateFulfillmentOnShippingTypeChange(productType, isProductTypeChanged, lateFulfillment);
    Assertions.assertTrue(result);
  }

  @Test
  void testEvaluateLateFulfillmentOnShippingTypeChange_productTypeChangedToBopis_returnTrue() {
    Integer productType = ProductType.BOPIS.getCode();
    boolean isProductTypeChanged = true;
    Boolean lateFulfillment = false; // original value
    Boolean result = CommonUtils.evaluateLateFulfillmentOnShippingTypeChange(productType, isProductTypeChanged, lateFulfillment);
    Assertions.assertTrue(result);
  }

  @Test
  void testEvaluateLateFulfillmentOnShippingTypeChange_productTypeChangedToBopis_returnTrueWhenOriginalTrue() {
    // Given
    Integer productType = ProductType.BOPIS.getCode();
    boolean isProductTypeChanged = true;
    Boolean lateFulfillment = true; // original value

    // When
    Boolean result = CommonUtils.evaluateLateFulfillmentOnShippingTypeChange(productType, isProductTypeChanged, lateFulfillment);

    // Then
    Assertions.assertTrue(result);
  }

  @Test
  void testEvaluateLateFulfillmentOnShippingTypeChange_productTypeNotChangedAndNull_returnOriginalValue() {
    // Given
    Integer productType = null;
    boolean isProductTypeChanged = false;
    Boolean lateFulfillment = true;

    // When
    Boolean result = CommonUtils.evaluateLateFulfillmentOnShippingTypeChange(productType, isProductTypeChanged, lateFulfillment);

    // Then
    Assertions.assertEquals(lateFulfillment, result);
  }

  @Test
  void testEvaluateLateFulfillmentOnShippingTypeChange_lateFulfillmentNull_productTypeChangedToRegular() {
    // Given
    Integer productType = ProductType.REGULAR.getCode();
    boolean isProductTypeChanged = true;
    Boolean lateFulfillment = null; // original value is null

    // When
    Boolean result = CommonUtils.evaluateLateFulfillmentOnShippingTypeChange(productType, isProductTypeChanged, lateFulfillment);

    // Then
    Assertions.assertFalse(result);
  }

  @Test
  void testEvaluateLateFulfillmentOnShippingTypeChange_lateFulfillmentNull_productTypeChangedToBigProduct() {
    // Given
    Integer productType = ProductType.BIG_PRODUCT.getCode();
    boolean isProductTypeChanged = true;
    Boolean lateFulfillment = null; // original value is null

    // When
    Boolean result = CommonUtils.evaluateLateFulfillmentOnShippingTypeChange(productType, isProductTypeChanged, lateFulfillment);

    // Then
    Assertions.assertTrue(result);
  }

  @Test
  void testEvaluateLateFulfillmentOnShippingTypeChange_lateFulfillmentNull_productTypeChangedToBopis() {
    Integer productType = ProductType.BOPIS.getCode();
    boolean isProductTypeChanged = true;
    Boolean lateFulfillment = null; // original value is null
    Assertions.assertTrue(CommonUtils.evaluateLateFulfillmentOnShippingTypeChange(productType, isProductTypeChanged, lateFulfillment));
  }

  @Test
  void testEvaluateLateFulfillmentOnShippingTypeChange_lateFulfillmentNull_productTypeNotChanged() {
    Integer productType = ProductType.REGULAR.getCode();
    boolean isProductTypeChanged = false;
    Boolean lateFulfillment = null;
    assertNull(CommonUtils.evaluateLateFulfillmentOnShippingTypeChange(productType, isProductTypeChanged, lateFulfillment));
  }

  @Test
  void testIsEligibleForAutoRejectOrAutoNeedRevision_AutoRejectActionWithTrustedSeller() {
    // Test AUTO_REJECT action with trusted seller - should return false
    boolean result = CommonUtils.isEligibleForAutoRejectOrAutoNeedRevision(RestrictedKeywordActionType.AUTO_REJECT.getRestrictedKeywordActionType(), true);
    Assertions.assertFalse(result);
  }

  @Test
  void testIsEligibleForAutoRejectOrAutoNeedRevision_AutoRejectActionWithNonTrustedSeller() {
    // Test AUTO_REJECT action with non-trusted seller - should return true
    boolean result = CommonUtils.isEligibleForAutoRejectOrAutoNeedRevision(RestrictedKeywordActionType.AUTO_REJECT.getRestrictedKeywordActionType(), false);
    Assertions.assertTrue(result);
  }

  @Test
  void testIsEligibleForAutoRejectOrAutoNeedRevision_AutoNeedRevisionActionWithTrustedSeller() {
    // Test AUTO_NEED_REVISION action with trusted seller - should return false
    boolean result = CommonUtils.isEligibleForAutoRejectOrAutoNeedRevision(RestrictedKeywordActionType.AUTO_NEED_REVISION.getRestrictedKeywordActionType(), true);
    Assertions.assertFalse(result);
  }

  @Test
  void testIsEligibleForAutoRejectOrAutoNeedRevision_AutoNeedRevisionActionWithNonTrustedSeller() {
    // Test AUTO_NEED_REVISION action with non-trusted seller - should return true
    boolean result = CommonUtils.isEligibleForAutoRejectOrAutoNeedRevision(RestrictedKeywordActionType.AUTO_NEED_REVISION.getRestrictedKeywordActionType(), false);
    Assertions.assertTrue(result);
  }

  @Test
  void testIsEligibleForAutoRejectOrAutoNeedRevision_ManualReviewDefaultActionWithTrustedSeller() {
    // Test MANUAL_REVIEW_DEFAULT action with trusted seller - should return false
    boolean result = CommonUtils.isEligibleForAutoRejectOrAutoNeedRevision(RestrictedKeywordActionType.MANUAL_REVIEW_DEFAULT.getRestrictedKeywordActionType(), true);
    Assertions.assertFalse(result);
  }

  @Test
  public void testGetImageRequestForResizeEvent_withNewImages() throws Exception {
    // Arrange
    ProductMasterDataEditRequest request = new ProductMasterDataEditRequest();
    List<ProductLevel3SummaryDetailsImageRequest> imageRequests = new ArrayList<>();

    ProductLevel3SummaryDetailsImageRequest newImage = new ProductLevel3SummaryDetailsImageRequest();
    newImage.setReviewType("NEW");
    newImage.setMainImage(true);
    newImage.setLocationPath("/path/to/image.jpg");
    imageRequests.add(newImage);

    request.setProductLevel3SummaryDetailsImageRequests(imageRequests);

    // Act
    List<ImageRequest> result = CommonUtils.getImageRequestForResizeEvent(request);

    // Assert
    assertNotNull(result);
    assertEquals(1, result.size());
    ImageRequest imageRequest = result.get(0);
    assertEquals("/path/to/image.jpg", imageRequest.getAbsoluteImagePath());
    assertTrue(imageRequest.isCommonImage());
    assertFalse(imageRequest.isEdited());
    assertNotNull(imageRequest.getHashCode());
  }

  @Test
  public void testGetImageRequestForResizeEvent_withNoNewImages() throws Exception {
    // Arrange
    ProductMasterDataEditRequest request = new ProductMasterDataEditRequest();
    List<ProductLevel3SummaryDetailsImageRequest> imageRequests = new ArrayList<>();

    ProductLevel3SummaryDetailsImageRequest existingImage = new ProductLevel3SummaryDetailsImageRequest();
    existingImage.setReviewType("UPDATE");
    existingImage.setMainImage(true);
    existingImage.setLocationPath("/path/to/image.jpg");
    imageRequests.add(existingImage);

    request.setProductLevel3SummaryDetailsImageRequests(imageRequests);

    // Act
    List<ImageRequest> result = CommonUtils.getImageRequestForResizeEvent(request);

    // Assert
    assertNotNull(result);
    assertEquals(0, result.size());
  }

  @Test
  public void testGetImageRequestForResizeEvent_withMultipleImages() throws Exception {
    // Arrange
    ProductMasterDataEditRequest request = new ProductMasterDataEditRequest();
    List<ProductLevel3SummaryDetailsImageRequest> imageRequests = new ArrayList<>();

    ProductLevel3SummaryDetailsImageRequest newImage1 = new ProductLevel3SummaryDetailsImageRequest();
    newImage1.setReviewType("NEW");
    newImage1.setMainImage(true);
    newImage1.setLocationPath("/path/to/image1.jpg");
    imageRequests.add(newImage1);

    ProductLevel3SummaryDetailsImageRequest newImage2 = new ProductLevel3SummaryDetailsImageRequest();
    newImage2.setReviewType("NEW");
    newImage2.setMainImage(false);
    newImage2.setLocationPath("/path/to/image2.jpg");
    imageRequests.add(newImage2);

    ProductLevel3SummaryDetailsImageRequest existingImage = new ProductLevel3SummaryDetailsImageRequest();
    existingImage.setReviewType("UPDATE");
    existingImage.setMainImage(true);
    existingImage.setLocationPath("/path/to/image3.jpg");
    imageRequests.add(existingImage);

    request.setProductLevel3SummaryDetailsImageRequests(imageRequests);

    // Act
    List<ImageRequest> result = CommonUtils.getImageRequestForResizeEvent(request);

    // Assert
    assertNotNull(result);
    assertEquals(2, result.size());

    // Verify first new image
    ImageRequest imageRequest1 = result.get(0);
    assertEquals("/path/to/image1.jpg", imageRequest1.getAbsoluteImagePath());
    assertTrue(imageRequest1.isCommonImage());
    assertFalse(imageRequest1.isEdited());
    assertNotNull(imageRequest1.getHashCode());

    // Verify second new image
    ImageRequest imageRequest2 = result.get(1);
    assertEquals("/path/to/image2.jpg", imageRequest2.getAbsoluteImagePath());
    assertTrue(imageRequest2.isCommonImage());
    assertFalse(imageRequest2.isEdited());
    assertNotNull(imageRequest2.getHashCode());
  }

  @Test
  public void testGetImageRequestForResizeEvent_withEmptyImageRequests() throws Exception {
    // Arrange
    ProductMasterDataEditRequest request = new ProductMasterDataEditRequest();
    request.setProductLevel3SummaryDetailsImageRequests(new ArrayList<>());

    // Act
    List<ImageRequest> result = CommonUtils.getImageRequestForResizeEvent(request);

    // Assert
    assertNotNull(result);
    assertEquals(0, result.size());
  }

  @Test
  void testIsEligibleForTakeDown_contentNotChanged() {
    // Given
    MasterProductEditDTO masterProductEditDTO = new MasterProductEditDTO();
    masterProductEditDTO.setContentChanged(false);
    ProductMasterDataEditRequest productMasterDataEditRequest = new ProductMasterDataEditRequest();
    
    // When
    CommonUtils.isEligibleForTakeDown(masterProductEditDTO, productMasterDataEditRequest);
    
    // Then
    assertFalse(masterProductEditDTO.isTakenDownProduct());
  }

  @Test
  void testIsEligibleForTakeDown_notPostLive() {
    // Given
    MasterProductEditDTO masterProductEditDTO = new MasterProductEditDTO();
    masterProductEditDTO.setContentChanged(true);
    masterProductEditDTO.setPostLive(false);
    RestrictedKeywordsByFieldAndActionType restrictedKeywordsByFieldAndActionType = new RestrictedKeywordsByFieldAndActionType();
    restrictedKeywordsByFieldAndActionType.setAction(RestrictedKeywordActionType.AUTO_REJECT.getRestrictedKeywordActionType());
    masterProductEditDTO.setRestrictedKeywordsByFieldAndActionType(restrictedKeywordsByFieldAndActionType);
    ProductMasterDataEditRequest productMasterDataEditRequest = new ProductMasterDataEditRequest();
    
    // When
    CommonUtils.isEligibleForTakeDown(masterProductEditDTO, productMasterDataEditRequest);
    
    // Then
    assertTrue(masterProductEditDTO.isTakenDownProduct());
  }

  @Test
  void testIsEligibleForTakeDown_notPostLiveWithImageAdded() {
    // Given
    MasterProductEditDTO masterProductEditDTO = new MasterProductEditDTO();
    masterProductEditDTO.setContentChanged(false);
    masterProductEditDTO.setPostLive(false);
    RestrictedKeywordsByFieldAndActionType restrictedKeywordsByFieldAndActionType = new RestrictedKeywordsByFieldAndActionType();
    restrictedKeywordsByFieldAndActionType.setAction(RestrictedKeywordActionType.AUTO_REJECT.getRestrictedKeywordActionType());
    masterProductEditDTO.setRestrictedKeywordsByFieldAndActionType(restrictedKeywordsByFieldAndActionType);
    ProductMasterDataEditRequest productMasterDataEditRequest = new ProductMasterDataEditRequest();
    ProductLevel3SummaryDetailsImageRequest newImage2 = new ProductLevel3SummaryDetailsImageRequest();
    newImage2.setReviewType("NEW");
    newImage2.setMainImage(false);
    newImage2.setLocationPath("/path/to/image2.jpg");
    productMasterDataEditRequest.setProductLevel3SummaryDetailsImageRequests(Collections.singletonList(newImage2));

    // When
    CommonUtils.isEligibleForTakeDown(masterProductEditDTO, productMasterDataEditRequest);

    // Then
    assertTrue(masterProductEditDTO.isTakenDownProduct());
  }

  @Test
  void testIsEligibleForTakeDown_postLive_manualReviewDefault_notTrustedSeller() {
    // Given
    MasterProductEditDTO masterProductEditDTO = new MasterProductEditDTO();
    masterProductEditDTO.setContentChanged(true);
    masterProductEditDTO.setPostLive(true);
    RestrictedKeywordsByFieldAndActionType restrictedKeywordsByFieldAndActionType = new RestrictedKeywordsByFieldAndActionType();
    restrictedKeywordsByFieldAndActionType.setAction(RestrictedKeywordActionType.MANUAL_REVIEW_DEFAULT.getRestrictedKeywordActionType());
    masterProductEditDTO.setRestrictedKeywordsByFieldAndActionType(restrictedKeywordsByFieldAndActionType);
    ProductMasterDataEditRequest productMasterDataEditRequest = new ProductMasterDataEditRequest();
    productMasterDataEditRequest.setTrustedSeller(false);
    
    // When
    CommonUtils.isEligibleForTakeDown(masterProductEditDTO, productMasterDataEditRequest);
    
    // Then
    assertTrue(masterProductEditDTO.isTakenDownProduct());
  }

  @Test
  void testIsEligibleForTakeDown_postLive_manualReviewDefault_trustedSeller() {
    // Given
    MasterProductEditDTO masterProductEditDTO = new MasterProductEditDTO();
    masterProductEditDTO.setContentChanged(true);
    masterProductEditDTO.setPostLive(true);
    RestrictedKeywordsByFieldAndActionType restrictedKeywordsByFieldAndActionType = new RestrictedKeywordsByFieldAndActionType();
    restrictedKeywordsByFieldAndActionType.setAction(RestrictedKeywordActionType.MANUAL_REVIEW_DEFAULT.getRestrictedKeywordActionType());
    masterProductEditDTO.setRestrictedKeywordsByFieldAndActionType(restrictedKeywordsByFieldAndActionType);
    ProductMasterDataEditRequest productMasterDataEditRequest = new ProductMasterDataEditRequest();
    productMasterDataEditRequest.setTrustedSeller(true);
    
    // When
    CommonUtils.isEligibleForTakeDown(masterProductEditDTO, productMasterDataEditRequest);
    
    // Then
    assertFalse(masterProductEditDTO.isTakenDownProduct());
  }

  @Test
  void testIsEligibleForTakeDown_postLive_notManualReviewDefault() {
    // Given
    MasterProductEditDTO masterProductEditDTO = new MasterProductEditDTO();
    masterProductEditDTO.setContentChanged(true);
    masterProductEditDTO.setPostLive(true);
    RestrictedKeywordsByFieldAndActionType restrictedKeywordsByFieldAndActionType = new RestrictedKeywordsByFieldAndActionType();
    restrictedKeywordsByFieldAndActionType.setAction(RestrictedKeywordActionType.AUTO_REJECT.getRestrictedKeywordActionType());
    masterProductEditDTO.setRestrictedKeywordsByFieldAndActionType(restrictedKeywordsByFieldAndActionType);
    ProductMasterDataEditRequest productMasterDataEditRequest = new ProductMasterDataEditRequest();
    productMasterDataEditRequest.setTrustedSeller(false);
    
    // When
    CommonUtils.isEligibleForTakeDown(masterProductEditDTO, productMasterDataEditRequest);
    
    // Then
    assertFalse(masterProductEditDTO.isTakenDownProduct());
  }



  @Test
  void testIsEligibleForDuplicateAttributeValidation_validationDisabledWithNullAttributes() {
    // Given
    boolean validateAttributeAdditionForOnlyNewlyAddedAttributes = false;
    ProductLevel3 request = new ProductLevel3();
    request.setNewlyAddedItems(Collections.singletonList(new ProductItemLevel3()));
    boolean result = CommonUtils.isEligibleForDuplicateAttributeValidation(
      validateAttributeAdditionForOnlyNewlyAddedAttributes, request);
    
    // Then
    assertTrue(result);
  }


  @Test
  void testIsEligibleForDuplicateAttributeValidation_validationDisabledwithOnlyDeletedItemSwithcOFf() {
    // Given
    boolean validateAttributeAdditionForOnlyNewlyAddedAttributes = false;
    ProductLevel3 request = new ProductLevel3();
    request.setDeletedItems(Collections.singletonList(PRODUCT_CODE));
    boolean result = CommonUtils.isEligibleForDuplicateAttributeValidation(
      validateAttributeAdditionForOnlyNewlyAddedAttributes, request);

    // Then
    assertTrue(result);
  }

  @Test
  void testIsEligibleForDuplicateAttributeValidation_validationDisabledwithOnlyDeletedItem() {
    // Given
    boolean validateAttributeAdditionForOnlyNewlyAddedAttributes = true;
    ProductLevel3 request = new ProductLevel3();
    request.setDeletedItems(Collections.singletonList(PRODUCT_CODE));
    boolean result = CommonUtils.isEligibleForDuplicateAttributeValidation(
      validateAttributeAdditionForOnlyNewlyAddedAttributes, request);

    // Then
    assertTrue(result);
  }

  @Test
  void testIsEligibleForDuplicateAttributeValidation_validationDeletedAttributes() {
    // Given
    boolean validateAttributeAdditionForOnlyNewlyAddedAttributes = true;
    ProductLevel3 request = new ProductLevel3();
    request.setNewlyAddedItems(Collections.singletonList(new ProductItemLevel3()));
    request.setDeletedItems(Collections.singletonList(PRODUCT_CODE));
    boolean result = CommonUtils.isEligibleForDuplicateAttributeValidation(
      validateAttributeAdditionForOnlyNewlyAddedAttributes, request);
    assertTrue(result);
  }

  @Test
  void testIsEligibleForDuplicateAttributeValidation_validationEnabledWithMultipleAttributes() {
    // Given
    boolean validateAttributeAdditionForOnlyNewlyAddedAttributes = false;
    ProductLevel3 request = new ProductLevel3();
   request.setNewlyAddedItems(Collections.singletonList(new ProductItemLevel3()));
   request.setDeletedItems(Collections.singletonList(PRODUCT_CODE));
    boolean result = CommonUtils.isEligibleForDuplicateAttributeValidation(
      validateAttributeAdditionForOnlyNewlyAddedAttributes, request);
    assertTrue(result);
  }

  @Test
  void testIsEligibleForDuplicateAttributeValidation_validationEnabledWithNOItem() {
    // Given
    boolean validateAttributeAdditionForOnlyNewlyAddedAttributes = true;
    ProductLevel3 request = new ProductLevel3();
    request.setNewlyAddedItems(Collections.emptyList());
    boolean result = CommonUtils.isEligibleForDuplicateAttributeValidation(
      validateAttributeAdditionForOnlyNewlyAddedAttributes, request);
    assertFalse(result);
  }

  @Test
  public void testSanitiseDimensionsForBopisProductType_bopisType_shouldResetDimensions() {
    ProductMasterDataEditRequest request = new ProductMasterDataEditRequest();
    request.setProductType(ProductType.BOPIS.getCode());
    request.setMasterDataEditChangeTypes(Set.of(L3InfoUpdateChangeType.DIMENSIONS_UPDATE));
    request.setHeight(10.0);
    request.setWidth(5.0);
    request.setLength(20.0);
    request.setWeight(2.5);
    request.setShippingWeight(3.0);

    CommonUtils.sanitiseDimensionsForBopisProductType(request);

    // Assuming the method sets dimensions to null for BOPIS
    Assertions.assertEquals(0.0, request.getHeight());
    Assertions.assertEquals(0.0, request.getWidth());
    Assertions.assertEquals(0.0, request.getLength());
    Assertions.assertEquals(0.0, request.getWeight());
    Assertions.assertEquals(0.0, request.getShippingWeight());
  }

  @Test
  public void testSanitiseDimensionsForBopisProductType_bopisType_shouldNotResetDimensions() {
    ProductMasterDataEditRequest request = new ProductMasterDataEditRequest();
    request.setProductType(ProductType.BOPIS.getCode());
    request.setMasterDataEditChangeTypes(Set.of(L3InfoUpdateChangeType.DESCRIPTION_UPDATE));
    request.setHeight(10.0);
    request.setWidth(5.0);
    request.setLength(20.0);
    request.setWeight(2.5);
    request.setShippingWeight(3.0);

    CommonUtils.sanitiseDimensionsForBopisProductType(request);

    // Assuming the method sets dimensions to null for BOPIS
    Assertions.assertEquals(10.0, request.getHeight());
  }

  @Test
  public void testSanitiseDimensionsForBopisProductType_nonBopisType_shouldNotResetDimensions() {
    ProductMasterDataEditRequest request = new ProductMasterDataEditRequest();
    request.setProductType(ProductType.REGULAR.getCode());
    request.setMasterDataEditChangeTypes(Set.of(L3InfoUpdateChangeType.DIMENSIONS_UPDATE));
    request.setHeight(10.0);
    request.setWidth(5.0);
    request.setLength(20.0);
    request.setWeight(2.5);
    request.setShippingWeight(3.0);

    CommonUtils.sanitiseDimensionsForBopisProductType(request);

    Assertions.assertEquals(10.0, request.getHeight());
    Assertions.assertEquals(5.0, request.getWidth());
    Assertions.assertEquals(20.0, request.getLength());
    Assertions.assertEquals(2.5, request.getWeight());
    Assertions.assertEquals(3.0, request.getShippingWeight());
  }

  @Test
  public void overrideFlagsBasedOnDistributionPPTest() {
    PickupPointResponse pickupPointResponse = new PickupPointResponse();
    CommonUtils.overrideFlagsBasedOnDistributionPP(new ProductCreationRequest(), new ArrayList<>());
    CommonUtils.overrideFlagsBasedOnDistributionPP(new ProductCreationRequest(),
        Collections.singletonList(pickupPointResponse));
    Map<String, Object> map = new HashMap<>();
    map.put(PICKUP_POINT_CODE, PICKUP_POINT_CODE);
    pickupPointResponse.setFlags(map);
    CommonUtils.overrideFlagsBasedOnDistributionPP(new ProductCreationRequest(),
        Collections.singletonList(pickupPointResponse));
    map.put(Constants.DISTRIBUTION_FLAG_KEY, Boolean.FALSE);
    CommonUtils.overrideFlagsBasedOnDistributionPP(new ProductCreationRequest(),
        Collections.singletonList(pickupPointResponse));
    map.put(Constants.DISTRIBUTION_FLAG_KEY, Boolean.TRUE);
    CommonUtils.overrideFlagsBasedOnDistributionPP(productCreationRequest,
        Collections.singletonList(pickupPointResponse));
    Assertions.assertNotNull(productCreationRequest);
    pickupPointResponse.setCode(DEFAULT_PICKUP_POINT_CODE);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      CommonUtils.overrideFlagsBasedOnDistributionPP(productCreationRequest,
          Collections.singletonList(pickupPointResponse));
    });
    productCreationRequest.getProductItemRequests()
        .forEach(productItemCreationRequest -> productItemCreationRequest.setMerchantSku(MERCHANT_CODE));
    CommonUtils.overrideFlagsBasedOnDistributionPP(productCreationRequest,
        Collections.singletonList(pickupPointResponse));
  }

  @Test
  public void testOverrideImageExtensionToWebpInMoreThan50VCase_productItems_onlyValidImagesConverted() {
    ProductLevel3SummaryDetailsImageRequest img1 = ProductLevel3SummaryDetailsImageRequest.builder()
        .locationPath("/img/test1.jpg").reviewType(Constants.NEW).build();
    ProductLevel3SummaryDetailsImageRequest img2 = ProductLevel3SummaryDetailsImageRequest.builder()
        .locationPath("/img/test2.webp").reviewType(Constants.NEW).build();
    ProductLevel3SummaryDetailsImageRequest img3 = ProductLevel3SummaryDetailsImageRequest.builder()
        .locationPath("").reviewType(Constants.NEW).build();
    ProductLevel3SummaryDetailsImageRequest img4 = ProductLevel3SummaryDetailsImageRequest.builder()
        .locationPath(null).reviewType(Constants.NEW).build();
    ProductLevel3SummaryDetailsImageRequest img5 = ProductLevel3SummaryDetailsImageRequest.builder()
        .locationPath("/img/test3.png").reviewType("old").build();

    ProductVariantPriceStockAndImagesRequest item = ProductVariantPriceStockAndImagesRequest.builder()
        .images(new ArrayList<>(Arrays.asList(img1, img2, img3, img4, img5))).build();
    ProductVariantUpdateRequest req = ProductVariantUpdateRequest.builder()
        .productItems(new ArrayList<>(Collections.singletonList(item))).build();

    CommonUtils.overrideImageExtensionToWebpInMoreThan50VCase(req);

    assertEquals("/img/test1.webp", req.getProductItems().get(0).getImages().get(0).getLocationPath());
    assertEquals("/img/test2.webp", req.getProductItems().get(0).getImages().get(1).getLocationPath()); // already webp
    assertEquals("", req.getProductItems().get(0).getImages().get(2).getLocationPath()); // blank, unchanged
    assertNull(req.getProductItems().get(0).getImages().get(3).getLocationPath()); // null, unchanged
    assertEquals("/img/test3.png", req.getProductItems().get(0).getImages().get(4).getLocationPath()); // not \"new\"
  }

  @Test
  public void testOverrideImageExtensionToWebpInMoreThan50VCase_copyToAllVariantImages_onlyValidImagesConverted() {
    ProductLevel3SummaryDetailsImageRequest img1 = ProductLevel3SummaryDetailsImageRequest.builder()
        .locationPath("/img/test4.jpg").reviewType(Constants.NEW).build();
    ProductLevel3SummaryDetailsImageRequest img2 = ProductLevel3SummaryDetailsImageRequest.builder()
        .locationPath("/img/test5.webp").reviewType(Constants.NEW).build();
    ProductLevel3SummaryDetailsImageRequest img3 = ProductLevel3SummaryDetailsImageRequest.builder()
        .locationPath(null).reviewType(Constants.NEW).build();
    ProductLevel3SummaryDetailsImageRequest img4 = ProductLevel3SummaryDetailsImageRequest.builder()
        .locationPath("").reviewType(Constants.NEW).build();
    ProductLevel3SummaryDetailsImageRequest img5 = ProductLevel3SummaryDetailsImageRequest.builder()
        .locationPath("/img/test6.png").reviewType("old").build();

    ProductVariantUpdateRequest req = ProductVariantUpdateRequest.builder()
        .copyToAllVariantImages(new ArrayList<>(Arrays.asList(img1, img2, img3, img4, img5))).build();

    CommonUtils.overrideImageExtensionToWebpInMoreThan50VCase(req);

    assertEquals("/img/test4.webp", req.getCopyToAllVariantImages().get(0).getLocationPath());
    assertEquals("/img/test5.webp", req.getCopyToAllVariantImages().get(1).getLocationPath()); // already webp
    assertNull(req.getCopyToAllVariantImages().get(2).getLocationPath()); // null, unchanged
    assertEquals("", req.getCopyToAllVariantImages().get(3).getLocationPath()); // blank, unchanged
    assertEquals("/img/test6.png", req.getCopyToAllVariantImages().get(4).getLocationPath()); // not \"new\"
  }

  @Test
  public void testOverrideImageExtensionToWebpInMoreThan50VCase_handlesNullAndEmptyLists() {
    ProductVariantUpdateRequest req = ProductVariantUpdateRequest.builder()
        .productItems(null).copyToAllVariantImages(null).build();
    CommonUtils.overrideImageExtensionToWebpInMoreThan50VCase(req);

    req = ProductVariantUpdateRequest.builder()
        .productItems(new ArrayList<>()).copyToAllVariantImages(new ArrayList<>()).build();
    CommonUtils.overrideImageExtensionToWebpInMoreThan50VCase(req);
    Assertions.assertNotNull(req);
  }

  @Test
  public void testOverrideImageExtensionToWebpInMoreThan50VCase_handlesWhitespaceLocationPath() {
    ProductLevel3SummaryDetailsImageRequest img =
        ProductLevel3SummaryDetailsImageRequest.builder().locationPath(" ").reviewType(Constants.NEW).build();
    ProductVariantPriceStockAndImagesRequest item =
        ProductVariantPriceStockAndImagesRequest.builder().images(new ArrayList<>(Collections.singletonList(img)))
            .build();
    ProductVariantUpdateRequest req =
        ProductVariantUpdateRequest.builder().productItems(new ArrayList<>(Collections.singletonList(item))).build();
    CommonUtils.overrideImageExtensionToWebpInMoreThan50VCase(req);
    assertEquals(" ", req.getProductItems().get(0).getImages().get(0).getLocationPath());
  }

  @Test
  public void generateInventoryBaseRequest_Test() {
    InventoryBaseRequest request = CommonUtils.generateInventoryBaseRequest(Constants.UPDATE_PO_DATE_BY_L3);
    Assertions.assertEquals(Constants.UPDATE_PO_DATE_BY_L3, request.getActionKey());
  }

  @Test
  public void shouldPopulatePreOrderDetails_Test() {
    Map<String, Object> flag = new HashMap<>();
    flag.put(Constants.BLIBLI_OMG, true);
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DATE, 1);
    Date oneDayInFuture = calendar.getTime();
    boolean result =
        CommonUtils.shouldPopulatePreOrderDetails(ProfileResponse.builder().flags(flag).build(),
            oneDayInFuture);
    assertTrue(result);
  }

  @Test
  public void shouldPopulatePreOrderDetails_flag_OmgFalse_Test() {
    Map<String, Object> flag = new HashMap<>();
    flag.put(Constants.BLIBLI_OMG, false);
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DATE, 1);
    Date oneDayInFuture = calendar.getTime();
    boolean result =
        CommonUtils.shouldPopulatePreOrderDetails(ProfileResponse.builder().flags(flag).build(),
            oneDayInFuture);
    assertFalse(result);
  }

  @Test
  public void shouldPopulatePreOrderDetails_flag_preOrderDate_null_Test() {
    Map<String, Object> flag = new HashMap<>();
    flag.put(Constants.BLIBLI_OMG, true);
    boolean result =
        CommonUtils.shouldPopulatePreOrderDetails(ProfileResponse.builder().flags(flag).build(),
            null);
    assertFalse(result);
  }

  @Test
  public void shouldPopulatePreOrderDetails_flag_preOrderDate_past_Test() {
    Map<String, Object> flag = new HashMap<>();
    flag.put(Constants.BLIBLI_OMG, false);
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DATE, -1);
    Date oneDayInPast = calendar.getTime();
    boolean result =
        CommonUtils.shouldPopulatePreOrderDetails(ProfileResponse.builder().flags(flag).build(),
            oneDayInPast);
    assertFalse(result);
  }

  @Test
  public void setPreOrderFields_featureSwitchTrue_validConditions_shouldSetFields() {
    Map<String, Object> flags = new HashMap<>();
    flags.put(Constants.BLIBLI_OMG, true);
    ProfileResponse profileResponse = ProfileResponse.builder().flags(flags).build();
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DATE, 1);
    Date futureDate = calendar.getTime();

    ProductLevel3Inventory inventory = new ProductLevel3Inventory();
    Integer preOrderQuota = 100;

    CommonUtils.setPreOrderFields(true, profileResponse, futureDate, inventory, preOrderQuota);
    assertEquals(futureDate, inventory.getPreOrderDate());
    assertEquals(preOrderQuota, inventory.getInitialPreOrderQuota());
  }

  @Test
  public void setPreOrderFields_featureSwitchFalse_shouldNotSetFields() {
    Map<String, Object> flags = new HashMap<>();
    flags.put(Constants.BLIBLI_OMG, true);
    ProfileResponse profileResponse = ProfileResponse.builder().flags(flags).build();

    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DATE, 1);
    Date futureDate = calendar.getTime();

    ProductLevel3Inventory inventory = new ProductLevel3Inventory();
    Integer preOrderQuota = 100;

    CommonUtils.setPreOrderFields(false, profileResponse, futureDate, inventory, preOrderQuota);
    Assertions.assertNull(inventory.getPreOrderDate());
    Assertions.assertNull(inventory.getInitialPreOrderQuota());
  }

  @Test
  public void setPreOrderFields_omgFlagFalse_shouldNotSetFields() {
    Map<String, Object> flags = new HashMap<>();
    flags.put(Constants.BLIBLI_OMG, false);
    ProfileResponse profileResponse = ProfileResponse.builder().flags(flags).build();
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DATE, 1);
    Date futureDate = calendar.getTime();

    ProductLevel3Inventory inventory = new ProductLevel3Inventory();
    Integer preOrderQuota = 100;

    CommonUtils.setPreOrderFields(true, profileResponse, futureDate, inventory, preOrderQuota);
    Assertions.assertNull(inventory.getPreOrderDate());
    Assertions.assertNull(inventory.getInitialPreOrderQuota());
  }

  @Test
  public void setPreOrderFields_nullPreOrderDate_shouldNotSetFields() {
    Map<String, Object> flags = new HashMap<>();
    flags.put(Constants.BLIBLI_OMG, true);
    ProfileResponse profileResponse = ProfileResponse.builder().flags(flags).build();

    ProductLevel3Inventory inventory = new ProductLevel3Inventory();
    Integer preOrderQuota = 100;

    CommonUtils.setPreOrderFields(true, profileResponse, null, inventory, preOrderQuota);
    Assertions.assertNull(inventory.getPreOrderDate());
    Assertions.assertNull(inventory.getInitialPreOrderQuota());
  }

  @Test
  public void setPreOrderFields_pastPreOrderDate_shouldNotSetFields() {
    Map<String, Object> flags = new HashMap<>();
    flags.put(Constants.BLIBLI_OMG, true);
    ProfileResponse profileResponse = ProfileResponse.builder().flags(flags).build();

    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DATE, -1);
    Date pastDate = calendar.getTime();

    ProductLevel3Inventory inventory = new ProductLevel3Inventory();
    Integer preOrderQuota = 100;

    CommonUtils.setPreOrderFields(true, profileResponse, pastDate, inventory, preOrderQuota);
    Assertions.assertNull(inventory.getPreOrderDate());
    Assertions.assertNull(inventory.getInitialPreOrderQuota());
  }

  @Test
  public void setPreOrderFields_nullPreOrderQuota_shouldSetFieldsWithZeroQuota() {
    Map<String, Object> flags = new HashMap<>();
    flags.put(Constants.BLIBLI_OMG, true);
    ProfileResponse profileResponse = ProfileResponse.builder().flags(flags).build();
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DATE, 1);
    Date futureDate = calendar.getTime();
    ProductLevel3Inventory inventory = new ProductLevel3Inventory();

    CommonUtils.setPreOrderFields(true, profileResponse, futureDate, inventory, null);
    assertEquals(futureDate, inventory.getPreOrderDate());
    assertEquals(Constants.ZERO, inventory.getInitialPreOrderQuota());
  }

  @Test
  public void validateOmgSellerAndPreOrderType_withDateType () {
    assertTrue(CommonUtils.validateOmgSellerAndPreOrderType("DATE"));
  }

  @Test
  public void validateOmgSellerAndPreOrderType_withDayType () {
    assertFalse(CommonUtils.validateOmgSellerAndPreOrderType("DAY"));
  }

  @Test
  public void validateOmgSellerAndPreOrderType_withBlankType () {
    assertFalse(CommonUtils.validateOmgSellerAndPreOrderType(null));
  }

  @Test
  public void validatePreOrderStockForEdit () {
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setStock(0);
    ItemPickupPointRequest itemPickupPointRequest1 = new ItemPickupPointRequest();
    itemPickupPointRequest1.setStock(10);
    List<ItemPickupPointRequest> itemPickupPointRequests = new ArrayList<>();
    itemPickupPointRequests.add(itemPickupPointRequest);
    itemPickupPointRequests.add(itemPickupPointRequest1);
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DATE, 1);
    PreOrderRequest preOrderRequest = PreOrderRequest.builder()
        .isPreOrder(true)
        .preOrderType("DATE")
        .preOrderValue(null)
        .preOrderDate(calendar.getTime())
        .build();
    ApiErrorCode apiErrorCode = CommonUtils.validatePreOrderStockForEdit(itemPickupPointRequests, preOrderRequest, null);
    assertEquals(ApiErrorCode.INVALID_STOCK_UPDATE_PREORDER_OMG, apiErrorCode);
  }

  @Test
  public void validatePreOrderStockForEditValidStock () {
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setStock(0);
    ItemPickupPointRequest itemPickupPointRequest1 = new ItemPickupPointRequest();
    itemPickupPointRequest1.setStock(0);
    List<ItemPickupPointRequest> itemPickupPointRequests = new ArrayList<>();
    itemPickupPointRequests.add(itemPickupPointRequest);
    itemPickupPointRequests.add(itemPickupPointRequest1);
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DATE, 1);
    PreOrderRequest preOrderRequest = PreOrderRequest.builder()
        .isPreOrder(true)
        .preOrderType("DATE")
        .preOrderValue(null)
        .preOrderDate(calendar.getTime())
        .build();
    ApiErrorCode apiErrorCode = CommonUtils.validatePreOrderStockForEdit(itemPickupPointRequests, preOrderRequest, null);
    assertNull(apiErrorCode);
  }

  @Test
  public void validatePreOrderStockForEditInvalidDate () {
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setStock(0);
    ItemPickupPointRequest itemPickupPointRequest1 = new ItemPickupPointRequest();
    itemPickupPointRequest1.setStock(0);
    List<ItemPickupPointRequest> itemPickupPointRequests = new ArrayList<>();
    itemPickupPointRequests.add(itemPickupPointRequest);
    itemPickupPointRequests.add(itemPickupPointRequest1);
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DATE, -1);
    PreOrderRequest preOrderRequest = PreOrderRequest.builder()
        .isPreOrder(true)
        .preOrderType("DATE")
        .preOrderValue(null)
        .preOrderDate(calendar.getTime())
        .build();
    ApiErrorCode apiErrorCode = CommonUtils.validatePreOrderStockForEdit(itemPickupPointRequests, preOrderRequest, null);
    assertEquals(ApiErrorCode.INVALID_STOCK_UPDATE_PREORDER_OMG, apiErrorCode);
  }

  @Test
  public void validatePreOrderStockForEditNullDate () {
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setStock(0);
    ItemPickupPointRequest itemPickupPointRequest1 = new ItemPickupPointRequest();
    itemPickupPointRequest1.setStock(0);
    List<ItemPickupPointRequest> itemPickupPointRequests = new ArrayList<>();
    itemPickupPointRequests.add(itemPickupPointRequest);
    itemPickupPointRequests.add(itemPickupPointRequest1);
    PreOrderRequest preOrderRequest = PreOrderRequest.builder()
        .isPreOrder(true)
        .preOrderType("DATE")
        .preOrderValue(null)
        .preOrderDate(null)
        .build();
    ApiErrorCode apiErrorCode = CommonUtils.validatePreOrderStockForEdit(itemPickupPointRequests, preOrderRequest, null);
    assertEquals(ApiErrorCode.INVALID_STOCK_UPDATE_PREORDER_OMG, apiErrorCode);
  }

  @Test
  public void extractProductSkuFromItemSkuTest() {
    CommonUtils.extractProductSkuFromItemSku(StringUtils.EMPTY);
    Assertions.assertEquals(StringUtils.EMPTY, CommonUtils.extractProductSkuFromItemSku(ITEM_SKU));
    CommonUtils.extractProductSkuFromItemSku(ITEM_SKU_3);
  }

  @Test
  public void convertToOmniChannelSkuResponseTest() {
    ValidOmniChannelSkuResponse validOmniChannelSkuResponse = new ValidOmniChannelSkuResponse();
    Assertions.assertTrue(CommonUtils.convertToOmniChannelSkuResponse(null, new HashMap<>()).isEmpty());
    Assertions.assertTrue(
        CommonUtils.convertToOmniChannelSkuResponse(new ValidOmniChannelSkuResponse(), new HashMap<>()).isEmpty());
    Map<String, ProductL1AndL2CodeResponse> map = new HashMap<>();
    map.put(CATEGORY_CODE, new ProductL1AndL2CodeResponse());
    validOmniChannelSkuResponse.setExistingOmniChannelSkusAndProductDetailsMap(map);
    CommonUtils.convertToOmniChannelSkuResponse(validOmniChannelSkuResponse, new HashMap<>());
  }

  @Test
  public void validateStockIncrementForSellerPenalty_true_whenAllConditionsMet() {
    Map<String, Object> flags = new HashMap<>();
    flags.put(Constants.PRODUCT_CONSEQUENCE_LIMITATION, true);
    ProfileResponse bp = ProfileResponse.builder().flags(flags).build();

    boolean result = CommonUtils.validateStockIncrementForSellerPenalty(bp, true, 5);
    Assertions.assertTrue(result);
  }

  @Test
  public void validateStockIncrementForSellerPenalty_false_whenFeatureDisabled() {
    Map<String, Object> flags = new HashMap<>();
    flags.put(Constants.PRODUCT_CONSEQUENCE_LIMITATION, true);
    ProfileResponse bp = ProfileResponse.builder().flags(flags).build();

    Assertions.assertFalse(CommonUtils.validateStockIncrementForSellerPenalty(bp, false, null));
  }

  @Test
  void validateStockIncrementForSellerPenaltyForNullStockTest() {
    Map<String, Object> flags = new HashMap<>();
    flags.put(Constants.PRODUCT_CONSEQUENCE_LIMITATION, true);
    ProfileResponse bp = ProfileResponse.builder().flags(flags).build();
    Assertions.assertFalse(CommonUtils.validateStockIncrementForSellerPenalty(bp, true, null));
  }

  @Test
  public void validateStockIncrementForSellerPenalty_false_whenFlagMissingOrFalse() {
    ProfileResponse bpNoFlags = new ProfileResponse();
    Assertions.assertFalse(
        CommonUtils.validateStockIncrementForSellerPenalty(bpNoFlags, true, 2));

    Map<String, Object> flagsFalse = new HashMap<>();
    flagsFalse.put(Constants.PRODUCT_CONSEQUENCE_LIMITATION, false);
    ProfileResponse bpFlagFalse = ProfileResponse.builder().flags(flagsFalse).build();
    Assertions.assertFalse(
        CommonUtils.validateStockIncrementForSellerPenalty(bpFlagFalse, true, 2));
  }

  @Test
  public void validateStockIncrementForSellerPenalty_false_whenDeltaNonPositive() {
    Map<String, Object> flags = new HashMap<>();
    flags.put(Constants.PRODUCT_CONSEQUENCE_LIMITATION, true);
    ProfileResponse bp = ProfileResponse.builder().flags(flags).build();

    Assertions.assertFalse(CommonUtils.validateStockIncrementForSellerPenalty(bp, true, 0));
    Assertions.assertFalse(
        CommonUtils.validateStockIncrementForSellerPenalty(bp, true, -5));
  }
  @Test
  public void checkCategoryBopisEligibilityTestWithBopisAndCategoryNotEligible() {
    com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse category =
        new com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse();
    category.setBopisEligible(false);
    category.setCategoryCode(this.DEFAULT_CATEGORY_CODE);
    ApiErrorCode apiErrorCode =
        CommonUtils.checkCategoryBopisEligibility(3, category, PRODUCT_CODE);
    Assertions.assertEquals(ApiErrorCode.BOPIS_CATEGORY_ELIGIBILTY_ERROR, apiErrorCode);
  }
  @Test
  public void checkCategoryBopisEligibilityTestWithBopisAndCategoryEligible() {
    com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse category =
        new com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse();
    category.setBopisEligible(true);
    category.setCategoryCode(this.DEFAULT_CATEGORY_CODE);
    ApiErrorCode apiErrorCode =
        CommonUtils.checkCategoryBopisEligibility(3, category, PRODUCT_CODE);
    Assertions.assertNull(apiErrorCode);
  }
  @Test
  public void checkCategoryBopisEligibilityTestWithNonBopisProduct() {
    com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse category =
        new com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse();
    category.setBopisEligible(false);
    category.setCategoryCode(this.DEFAULT_CATEGORY_CODE);
    ApiErrorCode apiErrorCode =
        CommonUtils.checkCategoryBopisEligibility(2, category, PRODUCT_CODE);
    Assertions.assertNull(apiErrorCode);
  }
  @Test
  public void checkCategoryBopisEligibilityTestWithProductTypeNull() {
    com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse category =
        new com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse();
    category.setBopisEligible(false);
    category.setCategoryCode(this.DEFAULT_CATEGORY_CODE);
    ApiErrorCode apiErrorCode =
        CommonUtils.checkCategoryBopisEligibility(null, category, PRODUCT_CODE);
    Assertions.assertNull(apiErrorCode);
  }

  @Test
  public void checkCategoryBopisEligibilityTestWithCategoryNull() {
    ApiErrorCode apiErrorCode =
        CommonUtils.checkCategoryBopisEligibility(3, null, PRODUCT_CODE);
    Assertions.assertNull(apiErrorCode);
  }

  @Test
  public void validateIfDistributionInfoIsMissingTest() {
    ProductItemDistributionInfoRequest productItemDistributionInfoRequest = new ProductItemDistributionInfoRequest();
    productItemDistributionInfoRequest.setDimensionsAndUOMRequest(
        Collections.singletonList(new DimensionAndUomRequest()));
    productItemDistributionInfoRequest.setDistributionItemInfoRequest(new DistributionItemRequest());
    Map<String, String> distributionInfoRequest = new HashMap<>();
    distributionInfoRequest.put(PICKUP_POINT_CODE, PICKUP_POINT_CODE);
    CommonUtils.validateIfDistributionInfoIsMissing(Collections.singletonList(productItemDistributionInfoRequest),
        distributionInfoRequest, PRODUCT_CODE);
    productItemDistributionInfoRequest.setDimensionsAndUOMRequest(null);
    Assertions.assertThrows(ApiDataNotFoundException.class, () -> {CommonUtils.validateIfDistributionInfoIsMissing(Collections.singletonList(productItemDistributionInfoRequest),
        distributionInfoRequest, PRODUCT_CODE);});
    productItemDistributionInfoRequest.setDistributionItemInfoRequest(null);
    Assertions.assertThrows(ApiDataNotFoundException.class, () -> {CommonUtils.validateIfDistributionInfoIsMissing(Collections.singletonList(productItemDistributionInfoRequest),
        distributionInfoRequest, PRODUCT_CODE);});
    Assertions.assertThrows(ApiDataNotFoundException.class, () -> {CommonUtils.validateIfDistributionInfoIsMissing(Collections.singletonList(productItemDistributionInfoRequest),
        null, PRODUCT_CODE);});
  }

  @Test
  public void testIsBrandUpdated_WhenBrandCodesAreDifferent() {
    String newBrandCode = "BRAND001";
    String oldBrandCode = "BRAND002";
    boolean result = CommonUtils.isBrandUpdated(newBrandCode, oldBrandCode);
    Assertions.assertTrue(result);
  }

  @Test
  public void testIsBrandUpdated_WhenBrandCodesAreSame() {
    String newBrandCode = "BRAND001";
    String oldBrandCode = "BRAND001";
    boolean result = CommonUtils.isBrandUpdated(newBrandCode, oldBrandCode);
    Assertions.assertFalse(result);
  }

  @Test
  public void testIsBrandUpdated_WhenNewBrandCodeIsEmpty() {
    String newBrandCode = "";
    String oldBrandCode = "BRAND001";
    boolean result = CommonUtils.isBrandUpdated(newBrandCode, oldBrandCode);
    Assertions.assertFalse(result);
  }

  @Test
  public void testIsBrandUpdated_WhenOldBrandCodeIsEmpty() {
    String newBrandCode = "BRAND001";
    String oldBrandCode = "";
    boolean result = CommonUtils.isBrandUpdated(newBrandCode, oldBrandCode);
    Assertions.assertFalse(result);
  }

  @Test
  public void testIsBrandUpdated_WhenBothBrandCodesAreEmpty() {
    String newBrandCode = "";
    String oldBrandCode = "";
    boolean result = CommonUtils.isBrandUpdated(newBrandCode, oldBrandCode);
    Assertions.assertFalse(result);
  }

  @Test
  public void testIsBrandUpdated_WhenBrandCodesAreNull() {
    String newBrandCode = null;
    String oldBrandCode = null;
    boolean result = CommonUtils.isBrandUpdated(newBrandCode, oldBrandCode);
    Assertions.assertFalse(result);
  }

  @Test
  public void testApiErrorCodeBrandEditNotAllowed() {
    ApiErrorCode errorCode = ApiErrorCode.BRAND_EDIT_NOT_ALLOWED_SINCE_ORDER_IS_PRESENT;
    Assertions.assertEquals("ERR-PBP100135", errorCode.getCode());
    Assertions.assertEquals(400, errorCode.getHttpStatus());
    Assertions.assertEquals("You can't change the brand because you have ongoing order right now.", errorCode.getDesc());
  }

  @Test
  public void testEligibleForAutoNeedRevision_AllConditionsTrue_NotWarehouseSeller() {
    AutoNeedRevisionAndForceReviewResponse autoResponse = new AutoNeedRevisionAndForceReviewResponse();
    autoResponse.setBrandTakeDown(true);
    autoResponse.setCategoryTakeDown(false);
    ProfileResponse profileResponse = new ProfileResponse();
    CompanyDTO company = new CompanyDTO();
    company.setMerchantType("CM");
    profileResponse.setCompany(company);
    String whMerchantTypes = "BL,AL";
    boolean result = CommonUtils.eligibleForAutoNeedRevision(true, autoResponse, profileResponse, whMerchantTypes);
    Assertions.assertTrue(result);
  }

  @Test
  public void testEligibleForAutoNeedRevision_WithCategoryTakeDown() {
    boolean sendProductToAutoNROnBrandOrCategoryTakeDown = true;
    AutoNeedRevisionAndForceReviewResponse autoResponse = new AutoNeedRevisionAndForceReviewResponse();
    autoResponse.setBrandTakeDown(false);
    autoResponse.setCategoryTakeDown(true);
    ProfileResponse profileResponse = new ProfileResponse();
    CompanyDTO company = new CompanyDTO();
    company.setMerchantType("CC");
    profileResponse.setCompany(company);
    String whMerchantTypes = "BL,AL";
    boolean result = CommonUtils.eligibleForAutoNeedRevision(sendProductToAutoNROnBrandOrCategoryTakeDown, autoResponse,
        profileResponse, whMerchantTypes);
    Assertions.assertTrue(result);
  }

  @Test
  public void testEligibleForAutoNeedRevision_WhenSendProductFlagIsFalse() {
    AutoNeedRevisionAndForceReviewResponse autoResponse = new AutoNeedRevisionAndForceReviewResponse();
    autoResponse.setBrandTakeDown(true);
    autoResponse.setCategoryTakeDown(false);
    ProfileResponse profileResponse = new ProfileResponse();
    CompanyDTO company = new CompanyDTO();
    company.setMerchantType("CM");
    profileResponse.setCompany(company);
    String whMerchantTypes = "BL,AL";
    boolean result = CommonUtils.eligibleForAutoNeedRevision(false,
        autoResponse, profileResponse, whMerchantTypes);
    Assertions.assertFalse(result);
  }

  @Test
  public void testEligibleForAutoNeedRevision_WhenBothBrandAndCategoryTakeDownAreFalse() {
    AutoNeedRevisionAndForceReviewResponse autoResponse = new AutoNeedRevisionAndForceReviewResponse();
    autoResponse.setBrandTakeDown(false);
    autoResponse.setCategoryTakeDown(false);
    ProfileResponse profileResponse = new ProfileResponse();
    CompanyDTO company = new CompanyDTO();
    company.setMerchantType("CM");
    profileResponse.setCompany(company);
    String whMerchantTypes = "BL,AL";
    boolean result = CommonUtils.eligibleForAutoNeedRevision(true,
        autoResponse, profileResponse, whMerchantTypes);
    Assertions.assertFalse(result);
  }

  @Test
  public void testEligibleForAutoNeedRevision_WhenSellerIsWarehouseSeller() {
    AutoNeedRevisionAndForceReviewResponse autoResponse = new AutoNeedRevisionAndForceReviewResponse();
    autoResponse.setBrandTakeDown(true);
    autoResponse.setCategoryTakeDown(false);
    ProfileResponse profileResponse = new ProfileResponse();
    CompanyDTO company = new CompanyDTO();
    company.setMerchantType("BL");
    profileResponse.setCompany(company);
    String whMerchantTypes = "BL,AL";
    boolean result = CommonUtils.eligibleForAutoNeedRevision(true,
        autoResponse, profileResponse, whMerchantTypes);
    Assertions.assertFalse(result);
  }

  @Test
  public void testEligibleForAutoNeedRevision_WhenCompanyIsNull() {
    AutoNeedRevisionAndForceReviewResponse autoResponse = new AutoNeedRevisionAndForceReviewResponse();
    autoResponse.setBrandTakeDown(true);
    autoResponse.setCategoryTakeDown(false);
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(null);
    String whMerchantTypes = "BL,AL";
    boolean result = CommonUtils.eligibleForAutoNeedRevision(true, autoResponse, profileResponse, whMerchantTypes);
    Assertions.assertTrue(result);
  }

  @Test
  public void testEligibleForAutoNeedRevision_WhenCompanyHasNoMerchantType() {
    AutoNeedRevisionAndForceReviewResponse autoResponse = new AutoNeedRevisionAndForceReviewResponse();
    autoResponse.setBrandTakeDown(true);
    autoResponse.setCategoryTakeDown(false);
    ProfileResponse profileResponse = new ProfileResponse();
    CompanyDTO company = new CompanyDTO();
    company.setMerchantType(null);
    profileResponse.setCompany(company);
    String whMerchantTypes = "BL,AL";
    boolean result = CommonUtils.eligibleForAutoNeedRevision(true,
        autoResponse, profileResponse, whMerchantTypes);
    Assertions.assertTrue(result);
  }

  @Test
  public void testIsWareHouseSeller_WhenCommissionTypeIsInList() {
    String wHMerchantTypes = "BL,AL,CM";
    String commissionType = "BL";
    boolean result = CommonUtils.isWareHouseSeller(wHMerchantTypes, commissionType);
    Assertions.assertTrue(result);
  }

  @Test
  public void testIsWareHouseSeller_WhenCommissionTypeIsNotInList() {
    String wHMerchantTypes = "BL,AL,CM";
    String commissionType = "CC";
    boolean result = CommonUtils.isWareHouseSeller(wHMerchantTypes, commissionType);
    Assertions.assertFalse(result);
  }

  @Test
  public void testIsWareHouseSeller_WithSingleMerchantType() {
    String wHMerchantTypes = "BL";
    String commissionType = "BL";
    boolean result = CommonUtils.isWareHouseSeller(wHMerchantTypes, commissionType);
    Assertions.assertTrue(result);
  }

  @Test
  public void testIsWareHouseSeller_WithSingleMerchantType_NotInList() {
    String wHMerchantTypes = "BL";
    String commissionType = "CM";
    boolean result = CommonUtils.isWareHouseSeller(wHMerchantTypes, commissionType);
    Assertions.assertFalse(result);
  }

  @Test
  public void testIsWareHouseSeller_WithCommaInMerchantTypes() {
    String wHMerchantTypes = "TD,TC,BL";
    String commissionType = "TD";
    boolean result = CommonUtils.isWareHouseSeller(wHMerchantTypes, commissionType);
    Assertions.assertTrue(result);
  }

  @Test
  public void testIsWareHouseSeller_WithMultipleTypesIncludingTarget() {
    String wHMerchantTypes = "AL,BL,CM,CC,TD,TC";
    String commissionType = "CM";
    boolean result = CommonUtils.isWareHouseSeller(wHMerchantTypes, commissionType);
    Assertions.assertTrue(result);
  }

  @Test
  public void testIsWareHouseSeller_CaseSensitiveCheck() {
    String wHMerchantTypes = "bl,al";
    String commissionType = "BL";
    boolean result = CommonUtils.isWareHouseSeller(wHMerchantTypes, commissionType);
    Assertions.assertFalse(result);
  }

  @Test
  public void testIsWareHouseSeller_WithEmptyString() {
    String wHMerchantTypes = "";
    String commissionType = "BL";
    boolean result = CommonUtils.isWareHouseSeller(wHMerchantTypes, commissionType);
    Assertions.assertFalse(result);
  }

  @Test
  public void testIsWareHouseSeller_AtStartOfList() {
    String wHMerchantTypes = "AL,BL,CM";
    String commissionType = "AL";
    boolean result = CommonUtils.isWareHouseSeller(wHMerchantTypes, commissionType);
    Assertions.assertTrue(result);
  }

  @Test
  public void testIsWareHouseSeller_AtEndOfList() {
    String wHMerchantTypes = "AL,BL,CM";
    String commissionType = "CM";
    boolean result = CommonUtils.isWareHouseSeller(wHMerchantTypes, commissionType);
    Assertions.assertTrue(result);
  }

  @Test
  public void testIsWareHouseSeller_WithWhitespaceInList() {
    String wHMerchantTypes = "AL, BL, CM";
    String commissionType = "BL";
    boolean result = CommonUtils.isWareHouseSeller(wHMerchantTypes, commissionType);
    Assertions.assertFalse(result);
  }

  @Test
  public void testIsBrandNameUpdated_whenDifferentNames_returnsTrue() {
    boolean result = CommonUtils.isBrandNameUpdated("NewBrand", "OldBrand");
    Assertions.assertTrue(result);
  }

  @Test
  public void testIsBrandNameUpdated_whenSameNames_returnsFalse() {
    boolean result = CommonUtils.isBrandNameUpdated("SameBrand", "SameBrand");
    Assertions.assertFalse(result);
  }

  @Test
  public void testIsBrandNameUpdated_whenNewBrandNameEmpty_returnsFalse() {
    boolean result = CommonUtils.isBrandNameUpdated("", "OldBrand");
    Assertions.assertFalse(result);
  }

  @Test
  public void testIsBrandNameUpdated_whenOldBrandNameEmpty_returnsFalse() {
    boolean result = CommonUtils.isBrandNameUpdated("NewBrand", "");
    Assertions.assertFalse(result);
  }

  @Test
  public void testIsBrandNameUpdated_whenBothEmpty_returnsFalse() {
    boolean result = CommonUtils.isBrandNameUpdated("", "");
    Assertions.assertFalse(result);
  }

  @Test
  public void testIsBrandNameUpdated_whenNewBrandNameNull_returnsFalse() {
    boolean result = CommonUtils.isBrandNameUpdated(null, "OldBrand");
    Assertions.assertFalse(result);
  }

  @Test
  public void testIsBrandNameUpdated_whenOldBrandNameNull_returnsFalse() {
    boolean result = CommonUtils.isBrandNameUpdated("NewBrand", null);
    Assertions.assertFalse(result);
  }

  @Test
  public void testIsCategoryCodeUpdated_whenDifferentCodes_returnsTrue() {
    boolean result = CommonUtils.isCategoryCodeUpdated("CAT001", "CAT002");
    Assertions.assertTrue(result);
  }

  @Test
  public void testIsCategoryCodeUpdated_whenSameCodes_returnsFalse() {
    boolean result = CommonUtils.isCategoryCodeUpdated("CAT001", "CAT001");
    Assertions.assertFalse(result);
  }

  @Test
  public void testIsCategoryCodeUpdated_whenNewCategoryCodeEmpty_returnsFalse() {
    boolean result = CommonUtils.isCategoryCodeUpdated("", "OLD_CAT");
    Assertions.assertFalse(result);
  }

  @Test
  public void testIsCategoryCodeUpdated_whenOldCategoryCodeEmpty_returnsFalse() {
    boolean result = CommonUtils.isCategoryCodeUpdated("NEW_CAT", "");
    Assertions.assertFalse(result);
  }

  @Test
  public void testIsCategoryCodeUpdated_whenBothEmpty_returnsFalse() {
    boolean result = CommonUtils.isCategoryCodeUpdated("", "");
    Assertions.assertFalse(result);
  }

  @Test
  public void testIsCategoryCodeUpdated_whenNewCategoryCodeNull_returnsFalse() {
    boolean result = CommonUtils.isCategoryCodeUpdated(null, "OLD_CAT");
    Assertions.assertFalse(result);
  }

  @Test
  public void testIsCategoryCodeUpdated_whenOldCategoryCodeNull_returnsFalse() {
    boolean result = CommonUtils.isCategoryCodeUpdated("NEW_CAT", null);
    Assertions.assertFalse(result);
  }

  @Test
  public void testConvertToUtc_whenInputIsNull_returnsNull() {
    Date result = CommonUtils.convertToUtc(null);
    Assertions.assertNull(result);
  }

  @Test
  public void testConvertToUtc_withDifferentDates_handlesCorrectly() {
    Calendar cal1 = Calendar.getInstance();
    cal1.set(Calendar.HOUR_OF_DAY, 23);
    cal1.set(Calendar.MINUTE, 59);
    cal1.set(Calendar.SECOND, 59);
    cal1.set(Calendar.MILLISECOND, 0);
    Date date1 = cal1.getTime();
    Calendar cal2 = Calendar.getInstance();
    cal2.setTime(date1);
    cal2.add(Calendar.SECOND, 1);
    Date date2 = cal2.getTime();
    Date result1 = CommonUtils.convertToUtc(date1);
    Date result2 = CommonUtils.convertToUtc(date2);
    Assertions.assertNotNull(result1);
    Assertions.assertNotNull(result2);
    long diff = Math.abs(result2.getTime() - result1.getTime());
    Assertions.assertTrue(diff >= 0 && diff < 2000,
        "Results should represent the correct time difference");
  }

  @Test
  public void testConvertToUtc_preservesTimeAccuracy() {
    Date originalDate = new Date();
    Date convertedDate = CommonUtils.convertToUtc(originalDate);
    Assertions.assertNotNull(convertedDate);
    long timeDifference = Math.abs(convertedDate.getTime() - originalDate.getTime());
    Assertions.assertTrue(timeDifference < 25 * 60 * 60 * 1000, 
        "Time difference should be within 24 hours due to timezone offset");
  }

  @Test
  public void testConvertToUtc_convertsBasedOnSystemTimezone() {
    // Test that conversion actually works based on system timezone
    ZoneId systemZone = ZoneId.systemDefault();

    // If system timezone is UTC, the conversion should return the same time
    // If system timezone is not UTC, the conversion will adjust
    Calendar cal = Calendar.getInstance();
    cal.set(Calendar.HOUR_OF_DAY, 10);
    cal.set(Calendar.MINUTE, 0);
    cal.set(Calendar.SECOND, 0);
    cal.set(Calendar.MILLISECOND, 0);
    Date inputDate = cal.getTime();

    Date outputDate = CommonUtils.convertToUtc(inputDate);
    Assertions.assertNotNull(outputDate);
    
    // Expected: Add timezone offset to convert local time to UTC
    int offsetSeconds = systemZone.getRules().getOffset(inputDate.toInstant()).getTotalSeconds();
    Date expectedDate = Date.from(inputDate.toInstant().plusSeconds(offsetSeconds));

    long timeDifference = Math.abs(outputDate.getTime() - expectedDate.getTime());
    Assertions.assertTrue(timeDifference < 1000,
        "Conversion should match expected UTC time based on system timezone");
  }

  @Test
  public void testConvertToUtc_handlesTimeZoneOffsetCorrectly() {
    // Test that the method properly handles timezone offsets
    // Create a date at noon local time
    Calendar cal = Calendar.getInstance();
    cal.set(Calendar.HOUR_OF_DAY, 12);
    cal.set(Calendar.MINUTE, 0);
    cal.set(Calendar.SECOND, 0);
    cal.set(Calendar.MILLISECOND, 0);
    Date localNoon = cal.getTime();

    Date utcResult = CommonUtils.convertToUtc(localNoon);
    Assertions.assertNotNull(utcResult);

    // Expected: Add timezone offset to convert local time to UTC
    ZoneId systemZone = ZoneId.systemDefault();
    int offsetSeconds = systemZone.getRules().getOffset(localNoon.toInstant()).getTotalSeconds();
    Instant utcInstant = localNoon.toInstant().plusSeconds(offsetSeconds);
    
    // The result should match the UTC instant for noon in local timezone
    long difference = Math.abs(utcResult.getTime() - utcInstant.toEpochMilli());
    Assertions.assertTrue(difference < 1000,
        "Noon in local timezone should be correctly converted to UTC");
  }
}

package com.gdn.mta.product.service;

import static com.gdn.partners.product.pricing.model.PromoFieldNames.ITEM_CODE;
import static com.gdn.partners.product.pricing.model.PromoFieldNames.PICKUP_POINT_CODE;
import static com.gdn.partners.product.pricing.model.PromoFieldNames.UPDATED_BY;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.eq;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import com.gda.mta.product.dto.DimensionAndUomRequest;
import com.gda.mta.product.dto.DistributionItemRequest;
import com.gda.mta.product.dto.PickupPointDeleteRequest;
import com.gda.mta.product.dto.ProductItemDistributionInfoRequest;
import com.gda.mta.product.dto.response.L2StockAvailabilityDTO;
import com.gdn.mta.product.entity.ProductLevel3;
import com.gdn.partners.pbp.dao.InventoryDetailInfoResponseV2DTO;
import com.gdn.partners.pbp.helper.ResponseHelper;
import com.gdn.partners.pbp.outbound.inventory.InventoryOutbound;
import com.gdn.partners.pbp.util.ProductLevel3InventoryUtil;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.InventoryDetailInfoRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.ListRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.InventoryDetailInfoResponseDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.WarehouseInventoryResponseDTO;
import com.gdn.x.product.rest.web.model.response.ItemPickupPointL5Response;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Assertions;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.data.domain.PageImpl;
import org.springframework.test.util.ReflectionTestUtils;

import com.gda.mta.product.dto.B2BFields;
import com.gda.mta.product.dto.BuyableScheduleRequest;
import com.gda.mta.product.dto.DeletedProductItems;
import com.gda.mta.product.dto.DiscoverableScheduleRequest;
import com.gda.mta.product.dto.EditProductResponse;
import com.gda.mta.product.dto.ItemPickupPointRequest;
import com.gda.mta.product.dto.ProductBundleRecipeRequest;
import com.gda.mta.product.dto.ProductEditValidationDTO;
import com.gda.mta.product.dto.ProductItemWholesalePriceRequest;
import com.gda.mta.product.dto.ProductL3UpdateRequest;
import com.gda.mta.product.dto.ProductLevel3PriceRequest;
import com.gda.mta.product.dto.ProductLevel3SummaryDetailsImageRequest;
import com.gda.mta.product.dto.ProductPriceStockAndImagesRequest;
import com.gda.mta.product.dto.ProductVariantPriceStockAndImagesRequest;
import com.gda.mta.product.dto.ProductVariantUpdateRequest;
import com.gda.mta.product.dto.response.SimpleStringResponse;
import com.gda.mta.product.dto.response.VariantsErrorListResponse;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.product.entity.ProductBundleRecipe;
import com.gdn.mta.product.entity.ProductSystemParameter;
import com.gdn.mta.product.enums.ApiErrorCode;
import com.gdn.mta.product.repository.BusinessPartnerRepository;
import com.gdn.mta.product.repository.ProductBusinessPartnerRepository;
import com.gdn.mta.product.repository.ProductLevel3Repository;
import com.gdn.mta.product.service.util.WholesaleValidationUtil;
import com.gdn.mta.product.util.ConverterUtil;
import com.gdn.mta.product.valueobject.SortOrder;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.commons.constants.SystemParameterConstants;
import com.gdn.partners.pbp.outbound.campaign.CampaignOutbound;
import com.gdn.partners.pbp.outbound.pickuppoint.PickupPointOutbound;
import com.gdn.partners.pbp.outbound.product.ProductOutbound;
import com.gdn.partners.pbp.outbound.product.feign.PCBFeign;
import com.gdn.partners.pbp.outbound.productPricing.ProductPricingOutbound;
import com.gdn.partners.pbp.outbound.xProduct.XProductOutbound;
import com.gdn.partners.product.pricing.web.model.response.WholesalePriceSkuResponse;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.PickupPointResponse;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.businesspartner.v2.dto.pickuppoint.PickupPointFilterRequest;
import com.gdn.x.campaign.rest.web.model.request.CampaignUpdateDiscountRequest;
import com.gdn.x.campaign.rest.web.model.response.CampaignUpdateDiscountResponse;
import com.gdn.x.product.rest.web.model.dto.CategoryDTO;
import com.gdn.x.product.rest.web.model.dto.ItemSummaryResponse;
import com.gdn.x.product.rest.web.model.dto.MasterCatalogDTO;
import com.gdn.x.product.rest.web.model.request.ItemRequest;
import com.gdn.x.product.rest.web.model.request.ItemSummaryRequest;
import com.gdn.x.product.rest.web.model.response.BusinessPartnerPickupPointResponse;
import com.gdn.x.product.rest.web.model.response.ItemBasicDetailV2Response;
import com.gdn.x.product.rest.web.model.response.ItemPickupPointListingResponse;
import com.gdn.x.product.rest.web.model.response.ItemResponse;
import com.gdn.x.product.rest.web.model.response.ProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.response.ProductBasicResponse;
import com.gdn.x.product.rest.web.model.response.ProductL3Response;
import com.gdn.x.product.rest.web.model.response.ProductResponse;
import com.gdn.x.productcategorybase.dto.request.ProductItemUpcCodesSkuCodesRequest;
import com.gdn.x.productcategorybase.dto.request.SkuCodesRequest;
import com.gdn.x.productcategorybase.dto.response.MinWholesaleDiscountResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import com.gdn.x.productcategorybase.dto.response.SingleObjectResponse;
import com.gdn.x.productcategorybase.dto.response.WholesaleConfigResponse;
import com.gdn.x.productcategorybase.dto.response.WholesaleMappingResponse;
import com.google.common.collect.ImmutableSet;

public class VariantEditValidationServiceTest {

  private static final String ITEM_SKU = "ITEM_SKU";

  private static final String ITEM_SKU_1 = "DTA-100001-10000-00001";
  private static final String ITEM_SKU_2 = "DTA-100001-10000-00002"; // Added for pagination test
  private static final String ITEM_SKU_3 = "DTA-100001-10000-00003"; // Added for pagination test
  private static final String ITEM_SKU_4 = "DTA-100001-10000-00004"; // Added for pagination test
  private static final String ITEM_SKU_5 = "DTA-100001-10000-00005"; // Added for pagination test
  private static final String PRODUCT_SKU_1 = "DTA-100001-10000";
  private static final String PRODUCT_SKU = "PRODUCT_SKU";
  private static final String PRODUCT_CODE = "PRODUCT_CODE";
  private static final String ITEM_NAME = "ITEM_NAME";
  private static final String CATEGORY_CODE = "CATEGORY_CODE";
  private static final String REQUEST_ID = "REQUEST_ID";
  private static final String CHANNEL_ID = "CHANNEL_ID";
  private static final String LOCATION_PATH = "LOCATION_PATH.jpg";
  private static final String MERCHANT_CODE = "MERCHANT_CODE";
  private static final String SOURCE_DIRECTORY = "/tmp";
  private static final String SKU_CODE = "sku-code";
  private static final String DEFAULT_STORE_ID = "10001";
  private static final String UPC_CODE = "upcCode";
  private static final String UPC_CODE_UPDATED = "upcCodeUpdated";
  private static final String SKU_CODE_1 = "skuCode";
  private static final String PICKUP_POINT_ID = "pickupPointId";
  private static final String BUSINESS_PARTNER_CODE = "bpCode";
  private static final Double PRICE = 1000.0;
  private static final String DELIVERY_API_ERROR = "ERR-PBP100124";
  private static final String CNC_API_ERROR = "ERR-PBP100123";
  private static final String INVALID_PICKUP_POINT_CODE = "ERR-PBP100042";

  private static final String BUSINESS_PARTNER_TYPE = "TD";

  private static final String BUSINESS_PARTNER_TYPE_CC = "CC";
  private static final String PP_CODE = "ppCode";
  private static final String PP_CODE_1 = "ppCode1";
  private static final String DEFAULT_BUSINESS_PARTNER_CODE = "merchantCode";
  private static final String MERCHANT_SKU_LENGTH_GREATER_THAN_255 =
      "MerchantSkuMerchantSkuMerchantSkuMerchantSkuMerchantSkuMerchantSkuMerchantSkuMerchantSkuMerchantSkuMerchantSkuMerchantSkuMerchantSku"
          + "MerchantSkuMerchantSkuMerchantSkuMerchantSkuMerchantSkuMerchantSkuMerchantSkuMerchantSkuMerchantSkuMerchantSkuMerchantSkuMerchantSku"
          + "MerchantSkuMerchantSkuMerchantSkuMerchantSkuMerchantSkuMerchantSkuMerchantSkuMerchantSkuMerchantSkuMerchantSkuMerchantSkuMerchantSku";
  private static final String FAAS_ACTIVATED = "faasActivated";
  private static final String UPDATED_BY = "testUser"; // Added constant

  private ProductPriceStockAndImagesRequest productPriceStockAndImagesRequest;
  private ProductVariantPriceStockAndImagesRequest productPriceStockAndImagesRequestL5;
  private ItemSummaryResponse itemSummaryResponse;
  private WholesalePriceSkuResponse wholesalePriceSkuResponse;
  private Map<String, WholesalePriceSkuResponse> wholesalePriceSkuResponseMap = new HashMap<>();
  private Map<String, ItemPickupPointListingResponse> itemPickupPointListingResponseMap = new HashMap<>();
  private ItemRequest itemRequest;
  private Map<String, ItemSummaryResponse> itemSummaryResponseMap = new HashMap<>();
  private ProductVariantUpdateRequest productVariantUpdateRequest = new ProductVariantUpdateRequest();
  private ProfileResponse profileResponse = new ProfileResponse();
  ProductItemResponse productItemResponse = new ProductItemResponse();
  SkuCodesRequest skuCodesRequest = new SkuCodesRequest();

  ProductL3UpdateRequest productL3UpdateRequest = new ProductL3UpdateRequest();
  private BusinessPartnerPickupPointResponse businessPartnerPickupPointResponse =
      new BusinessPartnerPickupPointResponse();


  @InjectMocks
  private VariantEditValidationServiceImpl variantEditValidationService;

  @Mock
  private ProductLevel3Repository productLevel3Repository;

  @Mock
  private ProductBusinessPartnerRepository productBusinessPartnerRepository;

  @Mock
  private ProductPricingOutbound productPricingOutbound;

  @Mock
  private PCBFeign pcbFeign;

  @Mock
  private WholesaleValidationUtil wholesaleValidationUtil;

  @Mock
  private ProductSystemParameterService productSystemParameterService;

  @Mock
  private CampaignOutbound campaignOutbound;

  @Mock
  private ProductOutbound productOutbound;

  @Mock
  private BusinessPartnerRepository businessPartnerRepository;

  @Mock
  private PickupPointOutbound pickupPointOutbound;

  @Mock
  private FileStorageService fileStorageService;

  @Mock
  private ProductInventoryService productInventoryService;

  @Mock
  private XProductOutbound xProductOutbound;

  @Mock
  private ProductLevel3V2Service productLevel3V2Service;

  @Mock
  private InventoryOutbound inventoryOutbound;

  @Captor
  private ArgumentCaptor<ItemRequest> itemRequestArgumentCaptor;

  @Captor
  private ArgumentCaptor<List<ProductItemWholesalePriceRequest>> listArgumentCaptor;

  @BeforeEach
  public void init() throws Exception {
    MockitoAnnotations.initMocks(this);

    productPriceStockAndImagesRequest = new ProductPriceStockAndImagesRequest();
    productPriceStockAndImagesRequest.setItemSku(ITEM_SKU);
    productPriceStockAndImagesRequest.setItemName(ITEM_NAME);
    productPriceStockAndImagesRequest.setProductSku(PRODUCT_SKU);
    productPriceStockAndImagesRequest.setWholesalePriceActivated(true);
    ProductLevel3PriceRequest productLevel3PriceRequest = new ProductLevel3PriceRequest();
    productLevel3PriceRequest.setChannelId(CHANNEL_ID);
    productLevel3PriceRequest.setPrice(10000.0);
    productLevel3PriceRequest.setSalePrice(10000.0);
    productPriceStockAndImagesRequest.setPrices(Arrays.asList(productLevel3PriceRequest));
    ProductLevel3SummaryDetailsImageRequest image = new ProductLevel3SummaryDetailsImageRequest();
    image.setLocationPath(LOCATION_PATH);
    image.setMainImage(true);
    image.setSequence(Integer.valueOf(0));
    productPriceStockAndImagesRequest.setImages(Arrays.asList(image));
    productPriceStockAndImagesRequest.setMerchantCode(MERCHANT_CODE);
    productPriceStockAndImagesRequest.setSynchronizeStock(true);

    itemSummaryResponse = new ItemSummaryResponse();
    itemSummaryResponse.setMasterCatalog(new MasterCatalogDTO());
    itemSummaryResponse.getMasterCatalog().setCategory(new CategoryDTO());
    itemSummaryResponse.getMasterCatalog().getCategory().setCategoryCode(CATEGORY_CODE);

    Map<Integer, Double> map = new HashMap<>();
    map.put(100, 10.0);
    wholesalePriceSkuResponse = new WholesalePriceSkuResponse();
    wholesalePriceSkuResponse.setItemSku(ITEM_SKU);
    wholesalePriceSkuResponse.setWholesaleRules(map);

    wholesalePriceSkuResponseMap.putIfAbsent(ITEM_SKU + Constants.HYPHEN + PICKUP_POINT_ID, wholesalePriceSkuResponse);

    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(new CompanyDTO());
    profileResponse.getCompany().setInventoryFulfillment("BL");

    itemRequest = new ItemRequest();
    itemRequest.setWholesalePriceActivated(true);
    itemRequest.setPrice(ConverterUtil.setOfferPrice(Arrays.asList(productLevel3PriceRequest)));

    productPriceStockAndImagesRequestL5 = new ProductVariantPriceStockAndImagesRequest();
    productPriceStockAndImagesRequestL5.setItemSku(ITEM_SKU);
    productPriceStockAndImagesRequestL5.setItemName(ITEM_NAME);
    productPriceStockAndImagesRequestL5.setProductSku(PRODUCT_SKU);
    ItemPickupPointRequest modifiedItemPickupPoint = new ItemPickupPointRequest();
    modifiedItemPickupPoint.setWholesalePriceActivated(true);
    modifiedItemPickupPoint.setPrice(10000.0);
    modifiedItemPickupPoint.setSalePrice(10000.0);
    modifiedItemPickupPoint.setSynchronizeStock(true);

    productPriceStockAndImagesRequestL5.setModifiedItemPickupPoints(Arrays.asList(modifiedItemPickupPoint));
    ProductLevel3SummaryDetailsImageRequest imageL5 = new ProductLevel3SummaryDetailsImageRequest();
    imageL5.setLocationPath(LOCATION_PATH);
    imageL5.setMainImage(true);
    imageL5.setSequence(Integer.valueOf(0));
    productPriceStockAndImagesRequestL5.setImages(Arrays.asList(imageL5));
    productPriceStockAndImagesRequestL5.setMerchantCode(MERCHANT_CODE);

    generateDummyImageFiles(LOCATION_PATH);
    ReflectionTestUtils.setField(variantEditValidationService, "maxWholesalePriceRequests", 0);
    ReflectionTestUtils.setField(variantEditValidationService, "mppAllowedSellers", Constants.CM_MERCHANT);
    ReflectionTestUtils.setField(variantEditValidationService, "inventoryL5SyncStockEnabled", false);
    Mockito.when(productLevel3Repository.findSummaryByFilter(Mockito.any(ItemSummaryRequest.class), Mockito.any(),
        Mockito.any(SortOrder.class))).thenReturn(new PageImpl<>(Arrays.asList(itemSummaryResponse)));
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE))
        .thenReturn(new ProductSystemParameter(Constants.MINIMUM_PRICE, "100", "min-price", false));
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID,
        SystemParameterConstants.VALID_IMAGE_EXTENSION)).thenReturn(new ProductSystemParameter(SystemParameterConstants.VALID_IMAGE_EXTENSION, ".jpg,.jpeg,.png",
            SystemParameterConstants.VALID_IMAGE_EXTENSION, false));
    Mockito.when(wholesaleValidationUtil.validateWholesaleConfigOnUpdate(eq(CATEGORY_CODE), listArgumentCaptor.capture(),
            itemRequestArgumentCaptor.capture(), eq(100), eq(ITEM_SKU), eq(true), Mockito.eq(null)))
        .thenReturn(ApiErrorCode.WHOLESALE_VALIDATION_FAILED);
    Mockito.when(productPricingOutbound.getWholesalePrice(eq(ITEM_SKU), Mockito.any()))
        .thenReturn(wholesalePriceSkuResponse);
    Mockito.when(productOutbound.getProductItemBySkuCodes(any(SkuCodesRequest.class))).thenReturn(new ArrayList<>());
    itemSummaryResponseMap.put(ITEM_SKU, itemSummaryResponse);

    ItemPickupPointListingResponse itemPickupPointListingResponse = new ItemPickupPointListingResponse();
    itemPickupPointListingResponse.setCategoryCode(CATEGORY_CODE);
    itemPickupPointListingResponse.setItemSku(ITEM_SKU);
    itemPickupPointListingResponse.setPickUpPointCode(PICKUP_POINT_ID);
    itemPickupPointListingResponseMap.putIfAbsent(ITEM_SKU + Constants.HYPHEN + PICKUP_POINT_ID,
        itemPickupPointListingResponse);


    productVariantUpdateRequest.setBusinessPartnerCode(MERCHANT_CODE);

    productItemResponse = new ProductItemResponse();
    productItemResponse.setSkuCode(SKU_CODE);
    productItemResponse.setUpcCode(UPC_CODE);
    skuCodesRequest = new SkuCodesRequest();
    skuCodesRequest.setFetchArchived(true);
    skuCodesRequest.setSkuCodes(Collections.singletonList(SKU_CODE));
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(productLevel3Repository);
    Mockito.verifyNoMoreInteractions(productBusinessPartnerRepository);
    Mockito.verifyNoMoreInteractions(productPricingOutbound);
    Mockito.verifyNoMoreInteractions(pcbFeign);
    Mockito.verifyNoMoreInteractions(productSystemParameterService);
    Mockito.verifyNoMoreInteractions(wholesaleValidationUtil);
    Mockito.verifyNoMoreInteractions(productSystemParameterService);
    Mockito.verifyNoMoreInteractions(pickupPointOutbound);
    Mockito.verifyNoMoreInteractions(productInventoryService);
    Mockito.verifyNoMoreInteractions(xProductOutbound);
    deleteDummyFiles(LOCATION_PATH);
  }

  @Test
  public void validateListOfVariantsTest() throws Exception {
    List<ProductPriceStockAndImagesRequest> successList = new ArrayList<>();
    List<VariantsErrorListResponse> failureList = new ArrayList<>();
    variantEditValidationService.validateListOfVariants(Arrays.asList(productPriceStockAndImagesRequest), successList,
        failureList, itemSummaryResponseMap);
    Mockito.verify(productSystemParameterService, Mockito.times(2))
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
    Mockito.verify(wholesaleValidationUtil).validateWholesaleConfigOnUpdate(eq(CATEGORY_CODE),
        eq(wholesalePriceSkuResponse.getWholesaleRules().keySet().stream().map(key -> new ProductItemWholesalePriceRequest(key, wholesalePriceSkuResponse.getWholesaleRules().get(key)))
            .collect(Collectors.toList())), Mockito.any(ItemRequest.class), eq(100), eq(ITEM_SKU), eq(true), eq(null));
    Mockito.verify(productPricingOutbound).getWholesalePrice(eq(ITEM_SKU), Mockito.any());
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.VALID_IMAGE_EXTENSION);
    Assertions.assertEquals(1, successList.size());
    Assertions.assertEquals(0, failureList.size());
  }

  @Test
  public void validateAndUpdateWholesalePriceTest() throws Exception {
    List<ProductPriceStockAndImagesRequest> successList = new ArrayList<>();
    List<VariantsErrorListResponse> failureList = new ArrayList<>();
    variantEditValidationService.validateListOfVariants(Arrays.asList(productPriceStockAndImagesRequest), successList,
        failureList, itemSummaryResponseMap);
    Mockito.verify(productSystemParameterService, Mockito.times(2))
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
    Mockito.verify(wholesaleValidationUtil).validateWholesaleConfigOnUpdate(eq(CATEGORY_CODE),
        eq(wholesalePriceSkuResponse.getWholesaleRules().keySet().stream().map(key -> new ProductItemWholesalePriceRequest(key, wholesalePriceSkuResponse.getWholesaleRules().get(key)))
            .collect(Collectors.toList())), eq(itemRequestArgumentCaptor.getValue()), eq(100), eq(ITEM_SKU), eq(true),
        eq(null));
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.VALID_IMAGE_EXTENSION);
    Mockito.verify(productPricingOutbound).getWholesalePrice(eq(ITEM_SKU), Mockito.any());
  }

  @Test
  public void validateAndUpdateWholesalePriceSuccessTest() throws Exception {
    ReflectionTestUtils.setField(variantEditValidationService, "maxWholesalePriceRequests", 5);
    List<ProductPriceStockAndImagesRequest> successList = new ArrayList<>();
    List<VariantsErrorListResponse> failureList = new ArrayList<>();
    variantEditValidationService.validateListOfVariants(Arrays.asList(productPriceStockAndImagesRequest), successList,
        failureList, itemSummaryResponseMap);
    Mockito.verify(productSystemParameterService, Mockito.times(2))
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
    Mockito.verify(wholesaleValidationUtil).validateWholesaleConfigOnUpdate(eq(CATEGORY_CODE),
        eq(wholesalePriceSkuResponse.getWholesaleRules().keySet().stream().map(key -> new ProductItemWholesalePriceRequest(key, wholesalePriceSkuResponse.getWholesaleRules().get(key)))
            .collect(Collectors.toList())), eq(itemRequestArgumentCaptor.getValue()), eq(100), eq(ITEM_SKU), eq(true),
        eq(null));
    Mockito.verify(productPricingOutbound).getWholesalePrice(eq(ITEM_SKU), Mockito.any());
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.VALID_IMAGE_EXTENSION);
    Assertions.assertTrue(CollectionUtils.isNotEmpty(successList));
  }

  @Test
  public void validateAndUpdateWholesalePriceNullTest() throws Exception {
    productPriceStockAndImagesRequest.setWholesalePriceActivated(false);
    Mockito.when(pcbFeign.getWholesaleConfigToCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE))
        .thenReturn(new GdnRestSingleResponse<>(null, REQUEST_ID));
    List<ProductPriceStockAndImagesRequest> successList = new ArrayList<>();
    List<VariantsErrorListResponse> failureList = new ArrayList<>();
    variantEditValidationService.validateListOfVariants(Arrays.asList(productPriceStockAndImagesRequest), successList,
        failureList, itemSummaryResponseMap);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.VALID_IMAGE_EXTENSION);
    Assertions.assertTrue(CollectionUtils.isNotEmpty(successList));
  }

  @Test
  public void validateAndUpdateWholesalePriceActivatedNullTest() throws Exception {
    List<ProductPriceStockAndImagesRequest> successList = new ArrayList<>();
    List<VariantsErrorListResponse> failureList = new ArrayList<>();
    productPriceStockAndImagesRequest.setWholesalePriceActivated(null);
    variantEditValidationService.validateListOfVariants(Arrays.asList(productPriceStockAndImagesRequest), successList,
        failureList, itemSummaryResponseMap);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.VALID_IMAGE_EXTENSION);
    Assertions.assertTrue(CollectionUtils.isNotEmpty(successList));
  }

  @Test
  public void validatePriceRequestNullPriceTest() throws Exception {
    productPriceStockAndImagesRequest.getPrices().get(0).setPrice(null);
    productPriceStockAndImagesRequest.setWholesalePriceActivated(false);
    List<ProductPriceStockAndImagesRequest> successList = new ArrayList<>();
    List<VariantsErrorListResponse> failureList = new ArrayList<>();
    variantEditValidationService.validateListOfVariants(Arrays.asList(productPriceStockAndImagesRequest), successList,
        failureList, itemSummaryResponseMap);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.VALID_IMAGE_EXTENSION);
    Assertions.assertTrue(CollectionUtils.isEmpty(successList));
    Assertions.assertEquals(ITEM_SKU, failureList.get(0).getItemSku());
    Assertions.assertEquals(ITEM_NAME, failureList.get(0).getItemName());
    Assertions.assertEquals(ApiErrorCode.PRICE_UPDATE_FAILED.getCode(), failureList.get(0).getCode());
    Assertions.assertEquals(ApiErrorCode.PRICE_UPDATE_FAILED.getDesc(), failureList.get(0).getMessage());
  }

  @Test
  public void validatePriceRequestMinimumPriceFailureTest() throws Exception {
    productPriceStockAndImagesRequest.getPrices().get(0).setPrice(80.0);
    List<ProductPriceStockAndImagesRequest> successList = new ArrayList<>();
    List<VariantsErrorListResponse> failureList = new ArrayList<>();
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE))
        .thenReturn(new ProductSystemParameter(Constants.MINIMUM_PRICE, "100", "min-price", false));
    variantEditValidationService.validateListOfVariants(Arrays.asList(productPriceStockAndImagesRequest), successList,
        failureList, itemSummaryResponseMap);
    Mockito.verify(wholesaleValidationUtil).validateWholesaleConfigOnUpdate(eq(CATEGORY_CODE),
        eq(wholesalePriceSkuResponse.getWholesaleRules().keySet().stream().map(
            key -> new ProductItemWholesalePriceRequest(key, wholesalePriceSkuResponse.getWholesaleRules().get(key))).collect(Collectors.toList())), eq(itemRequestArgumentCaptor.getValue()), eq(100), eq(ITEM_SKU), eq(true),
        eq(null));
    Mockito.verify(productSystemParameterService, Mockito.times(2)).findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
    Mockito.verify(productPricingOutbound).getWholesalePrice(eq(ITEM_SKU), Mockito.any());
    Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.VALID_IMAGE_EXTENSION);
    Assertions.assertTrue(CollectionUtils.isEmpty(successList));
    Assertions.assertEquals(ITEM_SKU, failureList.get(0).getItemSku());
    Assertions.assertEquals(ITEM_NAME, failureList.get(0).getItemName());
    Assertions.assertEquals(ApiErrorCode.PRICE_UPDATE_FAILED.getCode(), failureList.get(0).getCode());
    Assertions.assertEquals(ApiErrorCode.PRICE_UPDATE_FAILED.getDesc(), failureList.get(0).getMessage());
  }

  @Test
  public void validatePriceRequestMinimumBasePriceTestForBFB() throws Exception {
    productPriceStockAndImagesRequest.getPrices().get(0).setPrice(80.0);
    List<ProductPriceStockAndImagesRequest> successList = new ArrayList<>();
    List<VariantsErrorListResponse> failureList = new ArrayList<>();
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE))
        .thenReturn(new ProductSystemParameter(Constants.MINIMUM_PRICE, "100", "min-price", false));
    variantEditValidationService.validateListOfVariants(Arrays.asList(productPriceStockAndImagesRequest), successList,
        failureList, itemSummaryResponseMap);
    Mockito.verify(wholesaleValidationUtil).validateWholesaleConfigOnUpdate(eq(CATEGORY_CODE),
        eq(wholesalePriceSkuResponse.getWholesaleRules().keySet().stream().map(
            key -> new ProductItemWholesalePriceRequest(key, wholesalePriceSkuResponse.getWholesaleRules().get(key))).collect(Collectors.toList())), eq(itemRequestArgumentCaptor.getValue()), eq(100), eq(ITEM_SKU), eq(true),
        eq(null));
    Mockito.verify(productSystemParameterService, Mockito.times(2)).findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
    Mockito.verify(productPricingOutbound).getWholesalePrice(eq(ITEM_SKU), Mockito.any());
    Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.VALID_IMAGE_EXTENSION);
    Assertions.assertTrue(CollectionUtils.isEmpty(successList));
    Assertions.assertEquals(ITEM_SKU, failureList.get(0).getItemSku());
    Assertions.assertEquals(ITEM_NAME, failureList.get(0).getItemName());
    Assertions.assertEquals(ApiErrorCode.PRICE_UPDATE_FAILED.getCode(), failureList.get(0).getCode());
    Assertions.assertEquals(ApiErrorCode.PRICE_UPDATE_FAILED.getDesc(), failureList.get(0).getMessage());
  }

  @Test
  public void validatePriceRequestMinimumSalePriceFailureTest() throws Exception {
    productPriceStockAndImagesRequest.getPrices().get(0).setSalePrice(80.0);
    List<ProductPriceStockAndImagesRequest> successList = new ArrayList<>();
    List<VariantsErrorListResponse> failureList = new ArrayList<>();
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE))
        .thenReturn(new ProductSystemParameter(Constants.MINIMUM_PRICE, "100", "min-price", false));
    variantEditValidationService.validateListOfVariants(Arrays.asList(productPriceStockAndImagesRequest), successList,
        failureList, itemSummaryResponseMap);
    Mockito.verify(productSystemParameterService, Mockito.times(2))
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
    Mockito.verify(wholesaleValidationUtil).validateWholesaleConfigOnUpdate(eq(CATEGORY_CODE),
        eq(wholesalePriceSkuResponse.getWholesaleRules().keySet().stream().map(key -> new ProductItemWholesalePriceRequest(key, wholesalePriceSkuResponse.getWholesaleRules().get(key)))
            .collect(Collectors.toList())), eq(itemRequestArgumentCaptor.getValue()), eq(100), eq(ITEM_SKU), eq(true),
        eq(null));
    Mockito.verify(productPricingOutbound).getWholesalePrice(eq(ITEM_SKU), Mockito.any());
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.VALID_IMAGE_EXTENSION);
    Assertions.assertTrue(CollectionUtils.isEmpty(successList));
    Assertions.assertEquals(ITEM_SKU, failureList.get(0).getItemSku());
    Assertions.assertEquals(ITEM_NAME, failureList.get(0).getItemName());
    Assertions.assertEquals(ApiErrorCode.PRICE_UPDATE_FAILED.getCode(), failureList.get(0).getCode());
    Assertions.assertEquals(ApiErrorCode.PRICE_UPDATE_FAILED.getDesc(), failureList.get(0).getMessage());
  }

  @Test
  public void validatePriceRequestEmptyChannelIdFailureTest() throws Exception {
    productPriceStockAndImagesRequest.getPrices().get(0).setChannelId(StringUtils.EMPTY);
    List<ProductPriceStockAndImagesRequest> successList = new ArrayList<>();
    List<VariantsErrorListResponse> failureList = new ArrayList<>();
    variantEditValidationService.validateListOfVariants(Arrays.asList(productPriceStockAndImagesRequest), successList,
        failureList, itemSummaryResponseMap);
    Mockito.verify(productSystemParameterService, Mockito.times(2))
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
    Mockito.verify(wholesaleValidationUtil).validateWholesaleConfigOnUpdate(CATEGORY_CODE,
        wholesalePriceSkuResponse.getWholesaleRules().keySet().stream().map(key -> new ProductItemWholesalePriceRequest(key, wholesalePriceSkuResponse.getWholesaleRules().get(key)))
            .collect(Collectors.toList()), itemRequest, 100, ITEM_SKU, true, null);
    Mockito.verify(productPricingOutbound).getWholesalePrice(eq(ITEM_SKU), Mockito.any());
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.VALID_IMAGE_EXTENSION);
    Assertions.assertTrue(CollectionUtils.isEmpty(successList));
    Assertions.assertEquals(ITEM_SKU, failureList.get(0).getItemSku());
    Assertions.assertEquals(ITEM_NAME, failureList.get(0).getItemName());
    Assertions.assertEquals(ApiErrorCode.PRICE_UPDATE_FAILED.getCode(), failureList.get(0).getCode());
    Assertions.assertEquals(ApiErrorCode.PRICE_UPDATE_FAILED.getDesc(), failureList.get(0).getMessage());
  }

  @Test
  public void validatePriceLockCampaignRequestTest() throws Exception {
    productPriceStockAndImagesRequest.getPrices().get(0).setPrice(80000.0);
    productPriceStockAndImagesRequest.setCategoryCode(CATEGORY_CODE);
    List<VariantsErrorListResponse> failureList = new ArrayList<>();
    Map<String, String> itemSkuStatusMap = new HashMap<>();
    itemSkuStatusMap.put(ITEM_SKU,
        "New price does not satisfy the max discount criteria for Flashsale or campaign. Please make sure price is lower than Rp30000.0");
    CampaignUpdateDiscountResponse campaignUpdateDiscountResponse =
        CampaignUpdateDiscountResponse.builder().itemSkuStatusMap(itemSkuStatusMap).build();
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.CAMPAIGN_PRICE_VALIDATION)).thenReturn(
        new ProductSystemParameter(Constants.CAMPAIGN_PRICE_VALIDATION, "true", "campaignPriceValidationSwitch", false));
    Mockito.when(campaignOutbound.validateDiscountPrice(anyBoolean(), any(CampaignUpdateDiscountRequest.class)))
        .thenReturn(campaignUpdateDiscountResponse);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        variantEditValidationService.validatePriceLockCampaignRequest(productPriceStockAndImagesRequest, failureList);
      });
    } finally {
      Mockito.verify(productSystemParameterService, Mockito.times(1))
          .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.CAMPAIGN_PRICE_VALIDATION);
    }
  }

  @Test
  public void validatePriceLockCampaignSuccessRequestTest() throws Exception {
    productPriceStockAndImagesRequest.getPrices().get(0).setPrice(80000.0);
    productPriceStockAndImagesRequest.setCategoryCode(CATEGORY_CODE);
    List<VariantsErrorListResponse> failureList = new ArrayList<>();
    Map<String, String> itemSkuStatusMap = new HashMap<>();
    CampaignUpdateDiscountResponse campaignUpdateDiscountResponse =
        CampaignUpdateDiscountResponse.builder().itemSkuStatusMap(itemSkuStatusMap).build();
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.CAMPAIGN_PRICE_VALIDATION)).thenReturn(
        new ProductSystemParameter(Constants.CAMPAIGN_PRICE_VALIDATION, "true", "campaignPriceValidationSwitch", false));
    Mockito.when(campaignOutbound.validateAndUpdateDiscountPrice(anyBoolean(), any(CampaignUpdateDiscountRequest.class)))
        .thenReturn(campaignUpdateDiscountResponse);
    variantEditValidationService.validatePriceLockCampaignRequest(productPriceStockAndImagesRequest, failureList);
    Mockito.verify(productSystemParameterService, Mockito.times(1))
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.CAMPAIGN_PRICE_VALIDATION);
  }

  @Test
  public void validatePriceLockCampaignFailsRequestTest() throws Exception {
    productPriceStockAndImagesRequest.getPrices().get(0).setPrice(80000.0);
    productPriceStockAndImagesRequest.setCategoryCode(CATEGORY_CODE);
    List<ProductPriceStockAndImagesRequest> successList = new ArrayList<>();
    List<VariantsErrorListResponse> failureList = new ArrayList<>();
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.CAMPAIGN_PRICE_VALIDATION)).thenReturn(
        new ProductSystemParameter(Constants.CAMPAIGN_PRICE_VALIDATION, "true", "campaignPriceValidationSwitch", false));
    Mockito.when(campaignOutbound.validateAndUpdateDiscountPrice(anyBoolean(), any(CampaignUpdateDiscountRequest.class)))
        .thenReturn(new CampaignUpdateDiscountResponse());
    variantEditValidationService.validatePriceLockCampaignRequest(productPriceStockAndImagesRequest, failureList);
    Mockito.verify(productSystemParameterService, Mockito.times(1))
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.CAMPAIGN_PRICE_VALIDATION);
  }

  @Test
  public void validatePriceLockCampaign_nullSystemParameterTest() throws Exception {
    List<VariantsErrorListResponse> failureList = new ArrayList<>();
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.CAMPAIGN_PRICE_VALIDATION)).thenReturn(null);
    Mockito.when(campaignOutbound.validateAndUpdateDiscountPrice(anyBoolean(), any(CampaignUpdateDiscountRequest.class)))
        .thenReturn(new CampaignUpdateDiscountResponse());
    variantEditValidationService.validatePriceLockCampaignRequest(productPriceStockAndImagesRequest, failureList);
    Mockito.verify(productSystemParameterService, Mockito.times(1))
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.CAMPAIGN_PRICE_VALIDATION);
  }

  @Test
  public void validatePriceLockCampaignFailRequestTest() throws Exception {
    productPriceStockAndImagesRequest.getPrices().get(0).setPrice(80000.0);
    productPriceStockAndImagesRequest.setCategoryCode(CATEGORY_CODE);
    List<ProductPriceStockAndImagesRequest> successList = new ArrayList<>();
    List<VariantsErrorListResponse> failureList = new ArrayList<>();
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.CAMPAIGN_PRICE_VALIDATION)).thenReturn(
        new ProductSystemParameter(Constants.CAMPAIGN_PRICE_VALIDATION, "true", "campaignPriceValidationSwitch", false));
    Mockito.when(campaignOutbound.validateAndUpdateDiscountPrice(anyBoolean(), any(CampaignUpdateDiscountRequest.class)))
        .thenReturn(null);
    variantEditValidationService.validatePriceLockCampaignRequest(productPriceStockAndImagesRequest, failureList);
    Mockito.verify(productSystemParameterService, Mockito.times(1))
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.CAMPAIGN_PRICE_VALIDATION);
  }

  @Test
  public void validatePriceLockCampaignSwitchOffRequestTest() throws Exception {
    productPriceStockAndImagesRequest.getPrices().get(0).setPrice(80000.0);
    productPriceStockAndImagesRequest.setCategoryCode(CATEGORY_CODE);
    List<VariantsErrorListResponse> failureList = new ArrayList<>();
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.CAMPAIGN_PRICE_VALIDATION)).thenReturn(
        new ProductSystemParameter(Constants.CAMPAIGN_PRICE_VALIDATION, "false", "campaignPriceValidationSwitch", false));
    variantEditValidationService.validatePriceLockCampaignRequest(productPriceStockAndImagesRequest, failureList);
    Mockito.verify(productSystemParameterService, Mockito.times(1))
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.CAMPAIGN_PRICE_VALIDATION);
  }


  @Test
  public void validateImageRequestLocationPathNullTest() throws Exception {
    productPriceStockAndImagesRequest.getImages().get(0).setLocationPath(null);
    List<ProductPriceStockAndImagesRequest> successList = new ArrayList<>();
    List<VariantsErrorListResponse> failureList = new ArrayList<>();
    variantEditValidationService.validateListOfVariants(Arrays.asList(productPriceStockAndImagesRequest), successList,
        failureList, itemSummaryResponseMap);
    Mockito.verify(productSystemParameterService, Mockito.times(2))
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
    Mockito.verify(wholesaleValidationUtil).validateWholesaleConfigOnUpdate(CATEGORY_CODE,
        wholesalePriceSkuResponse.getWholesaleRules().keySet().stream().map(key -> new ProductItemWholesalePriceRequest(key, wholesalePriceSkuResponse.getWholesaleRules().get(key)))
            .collect(Collectors.toList()), itemRequest, 100, ITEM_SKU, true, null);
    Mockito.verify(productPricingOutbound).getWholesalePrice(eq(ITEM_SKU), Mockito.any());
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.VALID_IMAGE_EXTENSION);
    Assertions.assertTrue(CollectionUtils.isEmpty(successList));
    Assertions.assertEquals(ITEM_SKU, failureList.get(0).getItemSku());
    Assertions.assertEquals(ITEM_NAME, failureList.get(0).getItemName());
    Assertions.assertEquals(ApiErrorCode.IMAGE_UPDATE_FAILED.getCode(), failureList.get(0).getCode());
    Assertions.assertEquals(ApiErrorCode.IMAGE_UPDATE_FAILED.getDesc(), failureList.get(0).getMessage());
  }

  @Test
  public void validateImageRequestNotAvailableTest() throws Exception {
    productPriceStockAndImagesRequest.getImages().get(0).setLocationPath("location");
    List<ProductPriceStockAndImagesRequest> successList = new ArrayList<>();
    List<VariantsErrorListResponse> failureList = new ArrayList<>();
    Mockito.when(fileStorageService.checkImageAvailability(Mockito.anyList(), Mockito.anyBoolean())).thenReturn("abc");
    variantEditValidationService.validateListOfVariants(Arrays.asList(productPriceStockAndImagesRequest), successList,
        failureList, itemSummaryResponseMap);
    Assertions.assertTrue(CollectionUtils.isEmpty(successList));
    Mockito.verify(productSystemParameterService, Mockito.times(2))
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
    Mockito.verify(wholesaleValidationUtil).validateWholesaleConfigOnUpdate(eq(CATEGORY_CODE),
        eq(wholesalePriceSkuResponse.getWholesaleRules().keySet().stream().map(key -> new ProductItemWholesalePriceRequest(key, wholesalePriceSkuResponse.getWholesaleRules().get(key)))
            .collect(Collectors.toList())), Mockito.any(ItemRequest.class), eq(100), eq(ITEM_SKU), eq(true), eq(null));
    Mockito.verify(productPricingOutbound).getWholesalePrice(eq(ITEM_SKU), Mockito.any());
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.VALID_IMAGE_EXTENSION);
    Assertions.assertEquals(ITEM_SKU, failureList.get(0).getItemSku());
    Assertions.assertEquals(ITEM_NAME, failureList.get(0).getItemName());
    Assertions.assertEquals(ApiErrorCode.IMAGE_UPDATE_FAILED.getCode(), failureList.get(0).getCode());
    Assertions.assertEquals(ApiErrorCode.IMAGE_UPDATE_FAILED.getDesc(), failureList.get(0).getMessage());
  }

  @Test
  public void validateImageRequestMainImageNullTest() throws Exception {
    productPriceStockAndImagesRequest.getImages().get(0).setMainImage(null);
    List<ProductPriceStockAndImagesRequest> successList = new ArrayList<>();
    List<VariantsErrorListResponse> failureList = new ArrayList<>();
    variantEditValidationService.validateListOfVariants(Arrays.asList(productPriceStockAndImagesRequest), successList,
        failureList, itemSummaryResponseMap);
    Assertions.assertTrue(CollectionUtils.isEmpty(successList));
    Mockito.verify(productSystemParameterService, Mockito.times(2))
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
    Mockito.verify(wholesaleValidationUtil).validateWholesaleConfigOnUpdate(eq(CATEGORY_CODE),
        eq(wholesalePriceSkuResponse.getWholesaleRules().keySet().stream().map(key -> new ProductItemWholesalePriceRequest(key, wholesalePriceSkuResponse.getWholesaleRules().get(key)))
            .collect(Collectors.toList())), Mockito.any(ItemRequest.class), eq(100), eq(ITEM_SKU), eq(true), eq(null));
    Mockito.verify(productPricingOutbound).getWholesalePrice(eq(ITEM_SKU), Mockito.any());
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.VALID_IMAGE_EXTENSION);
    Assertions.assertEquals(ITEM_SKU, failureList.get(0).getItemSku());
    Assertions.assertEquals(ITEM_NAME, failureList.get(0).getItemName());
    Assertions.assertEquals(ApiErrorCode.IMAGE_UPDATE_FAILED.getCode(), failureList.get(0).getCode());
    Assertions.assertEquals(ApiErrorCode.IMAGE_UPDATE_FAILED.getDesc(), failureList.get(0).getMessage());
  }

  @Test
  public void validateImageRequestMMerchantSkuExceededTest() throws Exception {
    productPriceStockAndImagesRequest.setMerchantSku(new String(new char[300]).replace('\0', ' '));
    List<ProductPriceStockAndImagesRequest> successList = new ArrayList<>();
    List<VariantsErrorListResponse> failureList = new ArrayList<>();
    variantEditValidationService.validateListOfVariants(Arrays.asList(productPriceStockAndImagesRequest), successList,
        failureList, itemSummaryResponseMap);
    Assertions.assertTrue(CollectionUtils.isEmpty(successList));
    Mockito.verify(productSystemParameterService, Mockito.times(2))
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
    Mockito.verify(wholesaleValidationUtil).validateWholesaleConfigOnUpdate(eq(CATEGORY_CODE),
        eq(wholesalePriceSkuResponse.getWholesaleRules().keySet().stream().map(key -> new ProductItemWholesalePriceRequest(key, wholesalePriceSkuResponse.getWholesaleRules().get(key)))
            .collect(Collectors.toList())), Mockito.any(ItemRequest.class), eq(100), eq(ITEM_SKU), eq(true), eq(null));
    Mockito.verify(productPricingOutbound).getWholesalePrice(eq(ITEM_SKU), Mockito.any());
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.VALID_IMAGE_EXTENSION);
    Assertions.assertEquals(ITEM_SKU, failureList.get(0).getItemSku());
    Assertions.assertEquals(ITEM_NAME, failureList.get(0).getItemName());
    Assertions.assertEquals(ApiErrorCode.MAXIMUM_SELLER_SKU_EXCEEDED.getCode(), failureList.get(0).getCode());
    Assertions.assertEquals(ApiErrorCode.MAXIMUM_SELLER_SKU_EXCEEDED.getDesc(), failureList.get(0).getMessage());
  }

  @Test
  public void validateImageRequestMerchantSkuNotNullTest() throws Exception {
    productPriceStockAndImagesRequest.setMerchantSku(new String(new char[30]).replace('\0', ' '));
    List<ProductPriceStockAndImagesRequest> successList = new ArrayList<>();
    List<VariantsErrorListResponse> failureList = new ArrayList<>();
    variantEditValidationService.validateListOfVariants(Arrays.asList(productPriceStockAndImagesRequest), successList,
        failureList, itemSummaryResponseMap);
    Assertions.assertTrue(CollectionUtils.isNotEmpty(successList));
    Mockito.verify(productSystemParameterService, Mockito.times(2))
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
    Mockito.verify(wholesaleValidationUtil).validateWholesaleConfigOnUpdate(eq(CATEGORY_CODE),
        eq(wholesalePriceSkuResponse.getWholesaleRules().keySet().stream().map(key -> new ProductItemWholesalePriceRequest(key, wholesalePriceSkuResponse.getWholesaleRules().get(key)))
            .collect(Collectors.toList())), Mockito.any(ItemRequest.class), eq(100), eq(ITEM_SKU), eq(true), eq(null));
    Mockito.verify(productPricingOutbound).getWholesalePrice(eq(ITEM_SKU), Mockito.any());
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.VALID_IMAGE_EXTENSION);
    Assertions.assertTrue(CollectionUtils.isEmpty(failureList));
  }

  @Test
  public void validateImageRequestSequenceNullTest() throws Exception {
    productPriceStockAndImagesRequest.getImages().get(0).setSequence(null);
    List<ProductPriceStockAndImagesRequest> successList = new ArrayList<>();
    List<VariantsErrorListResponse> failureList = new ArrayList<>();
    variantEditValidationService.validateListOfVariants(Arrays.asList(productPriceStockAndImagesRequest), successList,
        failureList, itemSummaryResponseMap);
    Mockito.verify(productSystemParameterService, Mockito.times(2))
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
    Mockito.verify(wholesaleValidationUtil).validateWholesaleConfigOnUpdate(CATEGORY_CODE,
        wholesalePriceSkuResponse.getWholesaleRules().keySet().stream().map(key -> new ProductItemWholesalePriceRequest(key, wholesalePriceSkuResponse.getWholesaleRules().get(key)))
            .collect(Collectors.toList()), itemRequest, 100, ITEM_SKU, true, null);
    Mockito.verify(productPricingOutbound).getWholesalePrice(eq(ITEM_SKU), Mockito.any());
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.VALID_IMAGE_EXTENSION);
    Assertions.assertTrue(CollectionUtils.isEmpty(successList));
    Assertions.assertEquals(ITEM_SKU, failureList.get(0).getItemSku());
    Assertions.assertEquals(ITEM_NAME, failureList.get(0).getItemName());
    Assertions.assertEquals(ApiErrorCode.IMAGE_UPDATE_FAILED.getCode(), failureList.get(0).getCode());
    Assertions.assertEquals(ApiErrorCode.IMAGE_UPDATE_FAILED.getDesc(), failureList.get(0).getMessage());
  }

  @Test
  public void validateStockRequestNotBLTest() throws Exception {
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(CompanyDTO.builder().inventoryFulfillment("BL").build());
    productPriceStockAndImagesRequest.setSynchronizeStock(true);
    List<ProductPriceStockAndImagesRequest> successList = new ArrayList<>();
    List<VariantsErrorListResponse> failureList = new ArrayList<>();
    variantEditValidationService.validateListOfVariants(Arrays.asList(productPriceStockAndImagesRequest), successList,
        failureList, itemSummaryResponseMap);
    Mockito.verify(productSystemParameterService, Mockito.times(2))
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
    Mockito.verify(wholesaleValidationUtil).validateWholesaleConfigOnUpdate(CATEGORY_CODE,
        wholesalePriceSkuResponse.getWholesaleRules().keySet().stream().map(key -> new ProductItemWholesalePriceRequest(key, wholesalePriceSkuResponse.getWholesaleRules().get(key)))
            .collect(Collectors.toList()), itemRequest, 100, ITEM_SKU, true, null);
    Mockito.verify(productPricingOutbound).getWholesalePrice(eq(ITEM_SKU), Mockito.any());
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.VALID_IMAGE_EXTENSION);
    Assertions.assertFalse(CollectionUtils.isEmpty(successList));
  }

  @Test
  public void validateStockRequestSynchronizeStockTest() throws Exception {
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(CompanyDTO.builder().inventoryFulfillment("BL").build());
    productPriceStockAndImagesRequest.setSynchronizeStock(null);
    List<ProductPriceStockAndImagesRequest> successList = new ArrayList<>();
    List<VariantsErrorListResponse> failureList = new ArrayList<>();
    variantEditValidationService.validateListOfVariants(Arrays.asList(productPriceStockAndImagesRequest), successList,
        failureList, itemSummaryResponseMap);
    Mockito.verify(productSystemParameterService, Mockito.times(2))
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
    Mockito.verify(wholesaleValidationUtil).validateWholesaleConfigOnUpdate(CATEGORY_CODE,
        wholesalePriceSkuResponse.getWholesaleRules().keySet().stream().map(key -> new ProductItemWholesalePriceRequest(key, wholesalePriceSkuResponse.getWholesaleRules().get(key)))
            .collect(Collectors.toList()), itemRequest, 100, ITEM_SKU, true, null);
    Mockito.verify(productPricingOutbound).getWholesalePrice(eq(ITEM_SKU), Mockito.any());
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.VALID_IMAGE_EXTENSION);
    Assertions.assertFalse(CollectionUtils.isEmpty(successList));
  }

  @Test
  public void validatePriceLockCampaignRequestL5Test() throws Exception {
    productPriceStockAndImagesRequestL5.getModifiedItemPickupPoints().get(0).setPrice(80000.0);
    List<VariantsErrorListResponse> failureList = new ArrayList<>();
    Map<String, String> itemSkuStatusMap = new HashMap<>();
    itemSkuStatusMap.put(ITEM_SKU,
        "New price does not satisfy the max discount criteria for Flashsale or campaign. Please make sure price is lower than Rp30000.0");
    CampaignUpdateDiscountResponse campaignUpdateDiscountResponse =
        CampaignUpdateDiscountResponse.builder().itemSkuStatusMap(itemSkuStatusMap).build();
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.CAMPAIGN_PRICE_VALIDATION)).thenReturn(
        new ProductSystemParameter(Constants.CAMPAIGN_PRICE_VALIDATION, "true", "campaignPriceValidationSwitch", false));
    Mockito.when(campaignOutbound.validateDiscountPrice(anyBoolean(), any(CampaignUpdateDiscountRequest.class)))
        .thenReturn(campaignUpdateDiscountResponse);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        variantEditValidationService.validatePriceLockCampaignRequestL5(productPriceStockAndImagesRequestL5, failureList,
            CATEGORY_CODE, productPriceStockAndImagesRequestL5.getModifiedItemPickupPoints().get(0));
      });
    } finally {
      Mockito.verify(productSystemParameterService, Mockito.times(1))
          .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.CAMPAIGN_PRICE_VALIDATION);
    }
  }

  @Test()
  public void validatePriceLockCampaignRequestForEmptyMapL5Test() throws Exception {
    productPriceStockAndImagesRequestL5.getModifiedItemPickupPoints().get(0).setPrice(80000.0);
    List<VariantsErrorListResponse> failureList = new ArrayList<>();
    CampaignUpdateDiscountResponse campaignUpdateDiscountResponse =
        CampaignUpdateDiscountResponse.builder().itemSkuStatusMap(new HashMap<>()).build();
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.CAMPAIGN_PRICE_VALIDATION)).thenReturn(
        new ProductSystemParameter(Constants.CAMPAIGN_PRICE_VALIDATION, "true", "campaignPriceValidationSwitch", false));
    Mockito.when(campaignOutbound.validateDiscountPrice(anyBoolean(), any(CampaignUpdateDiscountRequest.class))).thenReturn(campaignUpdateDiscountResponse);
    try {
      variantEditValidationService.validatePriceLockCampaignRequestL5(productPriceStockAndImagesRequestL5, failureList,
          CATEGORY_CODE, productPriceStockAndImagesRequestL5.getModifiedItemPickupPoints().get(0));
    } finally {
      Mockito.verify(productSystemParameterService, Mockito.times(1)).findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.CAMPAIGN_PRICE_VALIDATION);
    }
  }

  @Test()
  public void updatePriceLockCampaignRequestForNullResponseL5Test() throws Exception {
    productPriceStockAndImagesRequestL5.getModifiedItemPickupPoints().get(0).setPrice(80000.0);
    List<VariantsErrorListResponse> failureList = new ArrayList<>();
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.CAMPAIGN_PRICE_VALIDATION)).thenReturn(
        new ProductSystemParameter(Constants.CAMPAIGN_PRICE_VALIDATION, "true", "campaignPriceValidationSwitch", false));
    Mockito.when(campaignOutbound.validateDiscountPrice(anyBoolean(), any(CampaignUpdateDiscountRequest.class))).thenReturn(null);
    try {
      variantEditValidationService.validatePriceLockCampaignRequestL5(productPriceStockAndImagesRequestL5, failureList,
          CATEGORY_CODE, productPriceStockAndImagesRequestL5.getModifiedItemPickupPoints().get(0));
    } finally {
      Mockito.verify(productSystemParameterService, Mockito.times(1)).findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.CAMPAIGN_PRICE_VALIDATION);
    }
  }

  @Test()
  public void validatePriceLockCampaignRequestForNullResponseL5Test() throws Exception {
    productPriceStockAndImagesRequestL5.getModifiedItemPickupPoints().get(0).setPrice(80000.0);
    List<VariantsErrorListResponse> failureList = new ArrayList<>();
    Map<String, String> itemSkuStatusMap = new HashMap<>();
    itemSkuStatusMap.put(ITEM_SKU,
        "New price does not satisfy the max discount criteria for Flashsale or campaign. Please make sure price is lower than Rp30000.0");
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.CAMPAIGN_PRICE_VALIDATION)).thenReturn(
        new ProductSystemParameter(Constants.CAMPAIGN_PRICE_VALIDATION, "true", "campaignPriceValidationSwitch", false));
    Mockito.when(campaignOutbound.validateAndUpdateDiscountPrice(anyBoolean(), any(CampaignUpdateDiscountRequest.class)))
        .thenReturn(CampaignUpdateDiscountResponse.builder().itemSkuStatusMap(itemSkuStatusMap).build());
    try {
      variantEditValidationService.validatePriceLockCampaignRequestL5(productPriceStockAndImagesRequestL5, failureList,
          CATEGORY_CODE, productPriceStockAndImagesRequestL5.getModifiedItemPickupPoints().get(0));
    } finally {
      Mockito.verify(productSystemParameterService, Mockito.times(1)).findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.CAMPAIGN_PRICE_VALIDATION);
    }
  }

  @Test
  public void validatePriceLockCampaignRequestEmptyMap() throws Exception {
    productPriceStockAndImagesRequestL5.getModifiedItemPickupPoints().get(0).setPrice(80000.0);
    List<VariantsErrorListResponse> failureList = new ArrayList<>();
    Map<String, String> itemSkuStatusMap = new HashMap<>();
    itemSkuStatusMap.put(ITEM_NAME,
        "New price does not satisfy the max discount criteria for Flashsale or campaign. Please make sure price is lower than Rp30000.0");
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.CAMPAIGN_PRICE_VALIDATION)).thenReturn(
        new ProductSystemParameter(Constants.CAMPAIGN_PRICE_VALIDATION, "true", "campaignPriceValidationSwitch", false));
    Mockito.when(campaignOutbound.validateAndUpdateDiscountPrice(anyBoolean(), any(CampaignUpdateDiscountRequest.class)))
        .thenReturn(CampaignUpdateDiscountResponse.builder().itemSkuStatusMap(itemSkuStatusMap).build());
    try {
      variantEditValidationService.updatePriceLockCampaignRequestL5(productPriceStockAndImagesRequestL5, failureList,
          CATEGORY_CODE, productPriceStockAndImagesRequestL5.getModifiedItemPickupPoints().get(0));
    } finally {
      Mockito.verify(productSystemParameterService, Mockito.times(1)).findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.CAMPAIGN_PRICE_VALIDATION);
    }
  }

  @Test
  public void validatePriceLockCampaignSuccessRequestL5Test() throws Exception {
    productPriceStockAndImagesRequestL5.getModifiedItemPickupPoints().get(0).setPrice(80000.0);
    List<VariantsErrorListResponse> failureList = new ArrayList<>();
    Map<String, String> itemSkuStatusMap = new HashMap<>();
    itemSkuStatusMap.put(ITEM_NAME, "0");
    CampaignUpdateDiscountResponse campaignUpdateDiscountResponse =
        CampaignUpdateDiscountResponse.builder().itemSkuStatusMap(itemSkuStatusMap).build();
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.CAMPAIGN_PRICE_VALIDATION)).thenReturn(
        new ProductSystemParameter(Constants.CAMPAIGN_PRICE_VALIDATION, "true", "campaignPriceValidationSwitch", false));
    Mockito.when(campaignOutbound.validateDiscountPrice(anyBoolean(), any(CampaignUpdateDiscountRequest.class))).thenReturn(campaignUpdateDiscountResponse);
    Mockito.when(campaignOutbound.validateAndUpdateDiscountPrice(anyBoolean(), any(CampaignUpdateDiscountRequest.class)))
        .thenReturn(campaignUpdateDiscountResponse);
    variantEditValidationService.validatePriceLockCampaignRequestL5(productPriceStockAndImagesRequestL5, failureList,
        CATEGORY_CODE, productPriceStockAndImagesRequestL5.getModifiedItemPickupPoints().get(0));
    Mockito.verify(productSystemParameterService, Mockito.times(1))
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.CAMPAIGN_PRICE_VALIDATION);
  }

  @Test
  public void updatePriceLockCampaignSuccessRequestL5Test() throws Exception {
    productPriceStockAndImagesRequestL5.getModifiedItemPickupPoints().get(0).setPrice(80000.0);
    List<VariantsErrorListResponse> failureList = new ArrayList<>();
    Map<String, String> itemSkuStatusMap = new HashMap<>();
    CampaignUpdateDiscountResponse campaignUpdateDiscountResponse =
        CampaignUpdateDiscountResponse.builder().itemSkuStatusMap(itemSkuStatusMap).build();
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.CAMPAIGN_PRICE_VALIDATION)).thenReturn(
        new ProductSystemParameter(Constants.CAMPAIGN_PRICE_VALIDATION, "true", "campaignPriceValidationSwitch", false));
    Mockito.when(campaignOutbound.validateAndUpdateDiscountPrice(anyBoolean(), any(CampaignUpdateDiscountRequest.class)))
        .thenReturn(campaignUpdateDiscountResponse);
    variantEditValidationService.updatePriceLockCampaignRequestL5(productPriceStockAndImagesRequestL5, failureList,
        CATEGORY_CODE, productPriceStockAndImagesRequestL5.getModifiedItemPickupPoints().get(0));
    Mockito.verify(productSystemParameterService, Mockito.times(1)).findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.CAMPAIGN_PRICE_VALIDATION);
  }

  @Test
  public void updatePriceLockCampaignFailsRequestL5Test() throws Exception {
    productPriceStockAndImagesRequestL5.getModifiedItemPickupPoints().get(0).setPrice(80000.0);
    List<ProductPriceStockAndImagesRequest> successList = new ArrayList<>();
    List<VariantsErrorListResponse> failureList = new ArrayList<>();
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.CAMPAIGN_PRICE_VALIDATION)).thenReturn(
        new ProductSystemParameter(Constants.CAMPAIGN_PRICE_VALIDATION, "true", "campaignPriceValidationSwitch", false));
    Mockito.when(campaignOutbound.validateAndUpdateDiscountPrice(anyBoolean(), any(CampaignUpdateDiscountRequest.class)))
        .thenReturn(new CampaignUpdateDiscountResponse());
    variantEditValidationService.updatePriceLockCampaignRequestL5(productPriceStockAndImagesRequestL5, failureList,
        CATEGORY_CODE, productPriceStockAndImagesRequestL5.getModifiedItemPickupPoints().get(0));
    Mockito.verify(productSystemParameterService, Mockito.times(1))
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.CAMPAIGN_PRICE_VALIDATION);
  }

  @Test
  public void updatePriceLockCampaignRequestL5Test() throws Exception {
    productPriceStockAndImagesRequestL5.getModifiedItemPickupPoints().get(0).setPrice(80000.0);
    List<VariantsErrorListResponse> failureList = new ArrayList<>();
    Map<String, String> itemSkuStatusMap = new HashMap<>();
    itemSkuStatusMap.put(ITEM_SKU,
        "New price does not satisfy the max discount criteria for Flashsale or campaign. Please make sure price is lower than Rp30000.0");
    CampaignUpdateDiscountResponse campaignUpdateDiscountResponse =
        CampaignUpdateDiscountResponse.builder().itemSkuStatusMap(itemSkuStatusMap).build();
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.CAMPAIGN_PRICE_VALIDATION)).thenReturn(
        new ProductSystemParameter(Constants.CAMPAIGN_PRICE_VALIDATION, "true", "campaignPriceValidationSwitch", false));
    Mockito.when(campaignOutbound.validateAndUpdateDiscountPrice(anyBoolean(), any(CampaignUpdateDiscountRequest.class)))
        .thenReturn(campaignUpdateDiscountResponse);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        variantEditValidationService.updatePriceLockCampaignRequestL5(productPriceStockAndImagesRequestL5, failureList,
            CATEGORY_CODE, productPriceStockAndImagesRequestL5.getModifiedItemPickupPoints().get(0));
      });
    } finally {
      Mockito.verify(productSystemParameterService, Mockito.times(1)).findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.CAMPAIGN_PRICE_VALIDATION);
    }
  }

  @Test()
  public void updatePriceLockCampaignFailRequestL5Test() throws Exception {
    productPriceStockAndImagesRequestL5.getModifiedItemPickupPoints().get(0).setPrice(80000.0);
    List<ProductPriceStockAndImagesRequest> successList = new ArrayList<>();
    List<VariantsErrorListResponse> failureList = new ArrayList<>();
    Map<String, String> itemSkuStatusMap = new HashMap<>();
    itemSkuStatusMap.put(ITEM_SKU, "0");
    CampaignUpdateDiscountResponse campaignUpdateDiscountResponse =
        CampaignUpdateDiscountResponse.builder().itemSkuStatusMap(itemSkuStatusMap).build();
    CampaignUpdateDiscountResponse campaignUpdateDiscountResponse1 =
        CampaignUpdateDiscountResponse.builder().itemSkuStatusMap(new HashMap<>()).build();
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.CAMPAIGN_PRICE_VALIDATION)).thenReturn(
        new ProductSystemParameter(Constants.CAMPAIGN_PRICE_VALIDATION, "true", "campaignPriceValidationSwitch", false));
    Mockito.when(campaignOutbound.validateDiscountPrice(anyBoolean(), any(CampaignUpdateDiscountRequest.class))).thenReturn(campaignUpdateDiscountResponse);
    Mockito.when(campaignOutbound.validateAndUpdateDiscountPrice(anyBoolean(), any(CampaignUpdateDiscountRequest.class)))
        .thenReturn(campaignUpdateDiscountResponse1);
    try {
      variantEditValidationService.updatePriceLockCampaignRequestL5(productPriceStockAndImagesRequestL5, failureList,
          CATEGORY_CODE, productPriceStockAndImagesRequestL5.getModifiedItemPickupPoints().get(0));
    } finally {
      Mockito.verify(productSystemParameterService, Mockito.times(1))
          .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.CAMPAIGN_PRICE_VALIDATION);
    }
  }

  @Test
  public void validatePriceLockCampaignFailsRequestL5Test() throws Exception {
    productPriceStockAndImagesRequestL5.getModifiedItemPickupPoints().get(0).setPrice(80000.0);
    List<ProductPriceStockAndImagesRequest> successList = new ArrayList<>();
    List<VariantsErrorListResponse> failureList = new ArrayList<>();
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.CAMPAIGN_PRICE_VALIDATION)).thenReturn(
        new ProductSystemParameter(Constants.CAMPAIGN_PRICE_VALIDATION, "true", "campaignPriceValidationSwitch", false));
    Mockito.when(campaignOutbound.validateAndUpdateDiscountPrice(anyBoolean(), any(CampaignUpdateDiscountRequest.class)))
        .thenReturn(new CampaignUpdateDiscountResponse());
    variantEditValidationService.validatePriceLockCampaignRequestL5(productPriceStockAndImagesRequestL5, failureList,
        CATEGORY_CODE, productPriceStockAndImagesRequestL5.getModifiedItemPickupPoints().get(0));
    Mockito.verify(productSystemParameterService, Mockito.times(1))
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.CAMPAIGN_PRICE_VALIDATION);
  }

  @Test
  public void validatePriceLockCampaign_nullSystemParameterL5Test() throws Exception {
    List<VariantsErrorListResponse> failureList = new ArrayList<>();
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.CAMPAIGN_PRICE_VALIDATION)).thenReturn(null);
    Mockito.when(campaignOutbound.validateAndUpdateDiscountPrice(anyBoolean(), any(CampaignUpdateDiscountRequest.class)))
        .thenReturn(new CampaignUpdateDiscountResponse());
    variantEditValidationService.validatePriceLockCampaignRequestL5(productPriceStockAndImagesRequestL5, failureList,
        CATEGORY_CODE, productPriceStockAndImagesRequestL5.getModifiedItemPickupPoints().get(0));
    Mockito.verify(productSystemParameterService, Mockito.times(1))
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.CAMPAIGN_PRICE_VALIDATION);
  }

  @Test
  public void validatePriceLockCampaignFailRequestL5Test() throws Exception {
    productPriceStockAndImagesRequestL5.getModifiedItemPickupPoints().get(0).setPrice(80000.0);
    List<ProductPriceStockAndImagesRequest> successList = new ArrayList<>();
    List<VariantsErrorListResponse> failureList = new ArrayList<>();
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.CAMPAIGN_PRICE_VALIDATION)).thenReturn(
        new ProductSystemParameter(Constants.CAMPAIGN_PRICE_VALIDATION, "true", "campaignPriceValidationSwitch", false));
    Mockito.when(campaignOutbound.validateAndUpdateDiscountPrice(anyBoolean(), any(CampaignUpdateDiscountRequest.class)))
        .thenReturn(null);
    variantEditValidationService.validatePriceLockCampaignRequestL5(productPriceStockAndImagesRequestL5, failureList,
        CATEGORY_CODE, productPriceStockAndImagesRequestL5.getModifiedItemPickupPoints().get(0));
    Mockito.verify(productSystemParameterService, Mockito.times(1))
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.CAMPAIGN_PRICE_VALIDATION);
  }

  @Test
  public void validatePriceLockCampaignSwitchOffRequestL5Test() throws Exception {
    productPriceStockAndImagesRequestL5.getModifiedItemPickupPoints().get(0).setPrice(80000.0);
    List<VariantsErrorListResponse> failureList = new ArrayList<>();
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.CAMPAIGN_PRICE_VALIDATION)).thenReturn(
        new ProductSystemParameter(Constants.CAMPAIGN_PRICE_VALIDATION, "false", "campaignPriceValidationSwitch", false));
    variantEditValidationService.validatePriceLockCampaignRequestL5(productPriceStockAndImagesRequestL5, failureList,
        CATEGORY_CODE, productPriceStockAndImagesRequestL5.getModifiedItemPickupPoints().get(0));
    Mockito.verify(productSystemParameterService, Mockito.times(1))
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.CAMPAIGN_PRICE_VALIDATION);
  }

  @Test
  public void addToFailureListTest() {
    List<VariantsErrorListResponse> failureList = new ArrayList<>();
    variantEditValidationService.addToFailureList(productPriceStockAndImagesRequest, failureList, "ERROR-CODE",
        "ERROR-MESSAGE");
    Assertions.assertEquals(ITEM_SKU, failureList.get(0).getItemSku());
    Assertions.assertEquals(ITEM_NAME, failureList.get(0).getItemName());
    Assertions.assertEquals("ERROR-CODE", failureList.get(0).getCode());
    Assertions.assertEquals("ERROR-MESSAGE", failureList.get(0).getMessage());
  }

  @Test
  public void checkArchivedSuspendedRejectedFaultyImageForUpdateIsArchivedTest() {
    ProductAndItemsResponse savedProductData = new ProductAndItemsResponse();
    ItemResponse itemResponse = new ItemResponse();
    itemResponse.setArchived(true);
    savedProductData.setItems(Arrays.asList(itemResponse));
    ApiErrorCode apiErrorCode = variantEditValidationService.checkArchivedSuspendedRejectedFaultyImageForUpdate(savedProductData);
    Assertions.assertEquals(ApiErrorCode.ITEM_IS_ARCHIVED.getCode(), apiErrorCode.getCode());
    Assertions.assertEquals(ApiErrorCode.ITEM_IS_ARCHIVED.getDesc(), apiErrorCode.getDesc());
  }

  @Test
  public void checkArchivedSuspendedRejectedFaultyImageForUpdateIsSuspendedTest() {
    ProductAndItemsResponse savedProductData = new ProductAndItemsResponse();
    ItemResponse itemResponse = new ItemResponse();
    itemResponse.setArchived(false);
    ProductResponse productResponse = new ProductResponse();
    productResponse.setSuspended(true);
    savedProductData.setItems(Arrays.asList(itemResponse));
    savedProductData.setProduct(productResponse);
    ApiErrorCode apiErrorCode = variantEditValidationService.checkArchivedSuspendedRejectedFaultyImageForUpdate(savedProductData);
    Assertions.assertEquals(ApiErrorCode.ITEM_IS_SUSPENDED.getCode(), apiErrorCode.getCode());
    Assertions.assertEquals(ApiErrorCode.ITEM_IS_SUSPENDED.getDesc(), apiErrorCode.getDesc());
  }

  @Test
  public void checkArchivedSuspendedRejectedFaultyImageForUpdateIsRejectedTest() {
    ProductAndItemsResponse savedProductData = new ProductAndItemsResponse();
    ItemResponse itemResponse = new ItemResponse();
    itemResponse.setArchived(false);
    itemResponse.setMarkForDelete(true);
    savedProductData.setItems(Arrays.asList(itemResponse));
    ApiErrorCode apiErrorCode = variantEditValidationService.checkArchivedSuspendedRejectedFaultyImageForUpdate(savedProductData);
    Assertions.assertEquals(ApiErrorCode.ITEM_IS_REJECTED.getCode(), apiErrorCode.getCode());
    Assertions.assertEquals(ApiErrorCode.ITEM_IS_REJECTED.getDesc(), apiErrorCode.getDesc());
  }

  @Test
  public void checkArchivedSuspendedRejectedFaultyImageForUpdateFaultyImagesTest() {
    ProductAndItemsResponse savedProductData = new ProductAndItemsResponse();
    ItemResponse itemResponse = new ItemResponse();
    itemResponse.setArchived(false);
    ProductResponse productResponse = new ProductResponse();
    productResponse.setSuspended(false);
    productResponse.setForceReview(true);
    savedProductData.setItems(Arrays.asList(itemResponse));
    savedProductData.setProduct(productResponse);
    ApiErrorCode apiErrorCode = variantEditValidationService.checkArchivedSuspendedRejectedFaultyImageForUpdate(savedProductData);
    Assertions.assertEquals(ApiErrorCode.FAULTY_IMAGE_FORCE_REVIEW.getCode(), apiErrorCode.getCode());
    Assertions.assertEquals(ApiErrorCode.FAULTY_IMAGE_FORCE_REVIEW.getDesc(), apiErrorCode.getDesc());
  }

  @Test
  public void checkArchivedSuspendedRejectedFaultyImageForUpdateFaultyImagesForceReviewFalseTest() {
    ProductAndItemsResponse savedProductData = new ProductAndItemsResponse();
    ItemResponse itemResponse = new ItemResponse();
    itemResponse.setArchived(false);
    ProductResponse productResponse = new ProductResponse();
    productResponse.setSuspended(false);
    productResponse.setForceReview(false);
    savedProductData.setItems(Arrays.asList(itemResponse));
    savedProductData.setProduct(productResponse);
    ApiErrorCode apiErrorCode = variantEditValidationService.checkArchivedSuspendedRejectedFaultyImageForUpdate(savedProductData);
    Assertions.assertNull(apiErrorCode);
  }

  private WholesaleMappingResponse getWholeSaleMapping() {
    WholesaleMappingResponse wholesaleMappingResponse = new WholesaleMappingResponse();
    wholesaleMappingResponse.setWholesalePriceConfigEnabled(true);
    MinWholesaleDiscountResponse minWholesaleDiscountResponse = new MinWholesaleDiscountResponse();
    minWholesaleDiscountResponse.setPercentage(20.0);
    minWholesaleDiscountResponse.setPrice(10000.0);
    MinWholesaleDiscountResponse minWholesaleDiscountResponse1 = new MinWholesaleDiscountResponse();
    minWholesaleDiscountResponse1.setPercentage(30.0);
    minWholesaleDiscountResponse1.setPrice(50000.0);
    WholesaleConfigResponse wholesaleConfigResponse = new WholesaleConfigResponse();
    wholesaleConfigResponse.setQuantity(200);
    wholesaleConfigResponse.setMinWholesaleDiscount(new ArrayList<>());
    wholesaleConfigResponse.getMinWholesaleDiscount().add(minWholesaleDiscountResponse);
    WholesaleConfigResponse wholesaleConfigResponse1 = new WholesaleConfigResponse();
    wholesaleConfigResponse1.setQuantity(300);
    wholesaleConfigResponse1.setMinWholesaleDiscount(new ArrayList<>());
    wholesaleConfigResponse1.getMinWholesaleDiscount().add(minWholesaleDiscountResponse1);
    wholesaleMappingResponse.setWholesaleConfig(new ArrayList<>());
    wholesaleMappingResponse.getWholesaleConfig().add(wholesaleConfigResponse);
    wholesaleMappingResponse.getWholesaleConfig().add(wholesaleConfigResponse1);
    wholesaleMappingResponse.setConfigurationType("PERCENTAGE");
    return wholesaleMappingResponse;
  }

  private void generateDummyImageFiles(String location) throws Exception {
    new File(SOURCE_DIRECTORY, FilenameUtils.getName(location)).createNewFile();
  }

  private void deleteDummyFiles(String location) throws Exception {
    new File(SOURCE_DIRECTORY, FilenameUtils.getName(location)).delete();
  }

  @Test
  public void isSameThresholdTest() {
    Mockito.when(pcbFeign.getWholesaleConfigToCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE))
        .thenReturn(new GdnRestSingleResponse<>(getWholeSaleMapping(), REQUEST_ID));
    boolean response =
        variantEditValidationService.isSameThreshold(productPriceStockAndImagesRequest, CATEGORY_CODE, ITEM_SKU,
            wholesalePriceSkuResponse, null);
    Mockito.verify(pcbFeign).getWholesaleConfigToCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
    Assertions.assertFalse(response);
  }

  @Test
  public void isSameThresholdWholesalePriceConfigDisabledTest() {
    WholesaleMappingResponse wholesaleMappingResponse = getWholeSaleMapping();
    wholesaleMappingResponse.setWholesalePriceConfigEnabled(false);
    Mockito.when(pcbFeign.getWholesaleConfigToCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE))
        .thenReturn(new GdnRestSingleResponse<>(wholesaleMappingResponse, REQUEST_ID));
    boolean response =
        variantEditValidationService.isSameThreshold(productPriceStockAndImagesRequest, CATEGORY_CODE, ITEM_SKU,
            wholesalePriceSkuResponse, null);
    Mockito.verify(pcbFeign).getWholesaleConfigToCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
    Assertions.assertTrue(response);
  }

  @Test
  public void isSameThresholdWholesaleRulesNullTest() {
    wholesalePriceSkuResponse.setWholesaleRules(null);
    WholesaleMappingResponse wholesaleMappingResponse = getWholeSaleMapping();
    Mockito.when(pcbFeign.getWholesaleConfigToCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE))
        .thenReturn(new GdnRestSingleResponse<>(wholesaleMappingResponse, REQUEST_ID));
    boolean response =
        variantEditValidationService.isSameThreshold(productPriceStockAndImagesRequest, CATEGORY_CODE, ITEM_SKU,
            wholesalePriceSkuResponse, null);
    Mockito.verify(pcbFeign).getWholesaleConfigToCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
    Assertions.assertTrue(response);
  }

  @Test
  public void isSameThresholdWholesalePriceSkuResponseNullTest() {
    WholesaleMappingResponse wholesaleMappingResponse = getWholeSaleMapping();
    Mockito.when(pcbFeign.getWholesaleConfigToCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE))
        .thenReturn(new GdnRestSingleResponse<>(wholesaleMappingResponse, REQUEST_ID));
    boolean response =
        variantEditValidationService.isSameThreshold(productPriceStockAndImagesRequest, CATEGORY_CODE, ITEM_SKU, null,
            null);
    Mockito.verify(pcbFeign).getWholesaleConfigToCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
    Assertions.assertTrue(response);
  }

  @Test
  public void isSameThresholdExceptionTest() {
    Mockito.when(pcbFeign.getWholesaleConfigToCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE))
        .thenReturn(new GdnRestSingleResponse<>(getWholeSaleMapping(), REQUEST_ID));
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE))
        .thenThrow(RuntimeException.class);
      boolean response =
          variantEditValidationService.isSameThreshold(productPriceStockAndImagesRequest, CATEGORY_CODE, ITEM_SKU,
              wholesalePriceSkuResponse, null);
      Assertions.assertFalse(response);
    Mockito.verify(pcbFeign).getWholesaleConfigToCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
  }

  @Test
  public void isSameThresholdL5Test() {
    Mockito.when(pcbFeign.getWholesaleConfigToCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE))
        .thenReturn(new GdnRestSingleResponse<>(getWholeSaleMapping(), REQUEST_ID));
    Boolean response = variantEditValidationService.isSameThresholdL5(
        productPriceStockAndImagesRequestL5.getModifiedItemPickupPoints().get(0), CATEGORY_CODE, ITEM_SKU,
        wholesalePriceSkuResponse);
    Mockito.verify(pcbFeign).getWholesaleConfigToCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
    Assertions.assertFalse(response);
  }

  @Test
  public void isSameThresholdL5WholesaleMappingNullTest() {
    Mockito.when(pcbFeign.getWholesaleConfigToCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE))
        .thenReturn(new GdnRestSingleResponse<>(null, REQUEST_ID));
    Boolean response = variantEditValidationService.isSameThresholdL5(
        productPriceStockAndImagesRequestL5.getModifiedItemPickupPoints().get(0), CATEGORY_CODE, ITEM_SKU,
        wholesalePriceSkuResponse);
    Mockito.verify(pcbFeign).getWholesaleConfigToCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
    Assertions.assertNull(response);
  }

  @Test
  public void isSameThresholdL5WholesalePriceConfigDisabledTest() {
    WholesaleMappingResponse wholesaleMappingResponse = getWholeSaleMapping();
    wholesaleMappingResponse.setWholesalePriceConfigEnabled(false);
    Mockito.when(pcbFeign.getWholesaleConfigToCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE))
        .thenReturn(new GdnRestSingleResponse<>(wholesaleMappingResponse, REQUEST_ID));
    boolean response = variantEditValidationService.isSameThresholdL5(
        productPriceStockAndImagesRequestL5.getModifiedItemPickupPoints().get(0), CATEGORY_CODE, ITEM_SKU,
        wholesalePriceSkuResponse);
    Mockito.verify(pcbFeign).getWholesaleConfigToCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
    Assertions.assertTrue(response);
  }

  @Test
  public void isSameThresholdL5WholesaleRulesNullTest() {
    wholesalePriceSkuResponse.setWholesaleRules(null);
    WholesaleMappingResponse wholesaleMappingResponse = getWholeSaleMapping();
    Mockito.when(pcbFeign.getWholesaleConfigToCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE))
        .thenReturn(new GdnRestSingleResponse<>(wholesaleMappingResponse, REQUEST_ID));
    boolean response = variantEditValidationService.isSameThresholdL5(
        productPriceStockAndImagesRequestL5.getModifiedItemPickupPoints().get(0), CATEGORY_CODE, ITEM_SKU,
        wholesalePriceSkuResponse);
    Mockito.verify(pcbFeign).getWholesaleConfigToCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
    Assertions.assertTrue(response);
  }

  @Test
  public void isSameThresholdL5WholesalePriceSkuResponseNullTest() {
    WholesaleMappingResponse wholesaleMappingResponse = getWholeSaleMapping();
    Mockito.when(pcbFeign.getWholesaleConfigToCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE))
        .thenReturn(new GdnRestSingleResponse<>(wholesaleMappingResponse, REQUEST_ID));
    boolean response = variantEditValidationService.isSameThresholdL5(
        productPriceStockAndImagesRequestL5.getModifiedItemPickupPoints().get(0), CATEGORY_CODE, ITEM_SKU, null);
    Mockito.verify(pcbFeign).getWholesaleConfigToCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
    Assertions.assertTrue(response);
  }

  @Test
  public void isSameThresholdL5ExceptionTest() {
    Mockito.when(pcbFeign.getWholesaleConfigToCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE))
        .thenReturn(new GdnRestSingleResponse<>(getWholeSaleMapping(), REQUEST_ID));
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE))
        .thenThrow(RuntimeException.class);

      boolean response = variantEditValidationService.isSameThresholdL5(
          productPriceStockAndImagesRequestL5.getModifiedItemPickupPoints().get(0), CATEGORY_CODE, ITEM_SKU,
          wholesalePriceSkuResponse);
      Assertions.assertFalse(response);
    Mockito.verify(pcbFeign).getWholesaleConfigToCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
  }

  @Test
  public void validateListOfVariantsForMultiUsedProductTest() throws Exception {
    List<ProductPriceStockAndImagesRequest> successList = new ArrayList<>();
    List<VariantsErrorListResponse> failureList = new ArrayList<>();
    successList = variantEditValidationService.validateListOfVariantsForMultiUsedProduct(Arrays.asList(productPriceStockAndImagesRequest), failureList,
        itemSummaryResponseMap);
    Mockito.verify(productSystemParameterService, Mockito.times(2))
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
    Mockito.verify(wholesaleValidationUtil).validateWholesaleConfigOnUpdate(eq(CATEGORY_CODE),
        eq(wholesalePriceSkuResponse.getWholesaleRules().keySet().stream().map(key -> new ProductItemWholesalePriceRequest(key, wholesalePriceSkuResponse.getWholesaleRules().get(key)))
            .collect(Collectors.toList())), Mockito.any(ItemRequest.class), eq(100), eq(ITEM_SKU), eq(true), eq(null));
    Mockito.verify(productPricingOutbound).getWholesalePrice(eq(ITEM_SKU), Mockito.any());
    Assertions.assertEquals(1, successList.size());
    Assertions.assertEquals(0, failureList.size());
  }

  @Test
  public void validateVersionTest() {
    List<VariantsErrorListResponse> failedRequests = new ArrayList<>();
    variantEditValidationService.validateVersion(productPriceStockAndImagesRequest, failedRequests, itemSummaryResponse);
  }

  @Test
  public void validateVersionNullVersionInRequestTest() {
    List<VariantsErrorListResponse> failedRequests = new ArrayList<>();
    itemSummaryResponse.setVersion(1L);
    productPriceStockAndImagesRequest.setVersion(null);
    variantEditValidationService.validateVersion(productPriceStockAndImagesRequest, failedRequests, itemSummaryResponse);
  }

  @Test
  public void validateVersionMissMatchTest() {
    List<VariantsErrorListResponse> failedRequests = new ArrayList<>();
    productPriceStockAndImagesRequest.setVersion(10L);
    itemSummaryResponse.setVersion(11L);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      variantEditValidationService.validateVersion(productPriceStockAndImagesRequest, failedRequests, itemSummaryResponse);
    });
    Assertions.assertEquals(1, failedRequests.size());
  }

  @Test
  public void validateVersionSameVersionTest() {
    List<VariantsErrorListResponse> failedRequests = new ArrayList<>();
    productPriceStockAndImagesRequest.setVersion(10L);
    itemSummaryResponse.setVersion(10L);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      variantEditValidationService.validateVersion(productPriceStockAndImagesRequest, failedRequests, itemSummaryResponse);
    });
    Assertions.assertEquals(1, failedRequests.size());
  }

  @Test
  public void validateVersionCorrectValuesTest() {
    List<VariantsErrorListResponse> failedRequests = new ArrayList<>();
    productPriceStockAndImagesRequest.setVersion(11L);
    itemSummaryResponse.setVersion(10L);
    variantEditValidationService.validateVersion(productPriceStockAndImagesRequest, failedRequests, itemSummaryResponse);
  }

  @Test
  public void validateVersionMissMatchValueTest() {
    List<VariantsErrorListResponse> failedRequests = new ArrayList<>();
    productPriceStockAndImagesRequest.setVersion(10L);
    itemSummaryResponse.setVersion(null);
    variantEditValidationService.validateVersion(productPriceStockAndImagesRequest, failedRequests, itemSummaryResponse);
  }

  @Test
  public void validateUpcCodeTest() {
    Map<String, String> skuCodeUpcCodeMap = new HashMap<>();
    skuCodeUpcCodeMap.put(SKU_CODE, "upc-code1");
    List<VariantsErrorListResponse> failedRequests = new ArrayList<>();
    productPriceStockAndImagesRequest.setUpcCode("upc-code");
    productPriceStockAndImagesRequest.setSkuCode(SKU_CODE);
    Mockito.when(pcbFeign.getItemNameByUpcCodeAndProductCode(any(), any(), any(), any(), any(), any(), any(), any()))
        .thenReturn(new GdnRestListResponse<>(null, null, true,
            Arrays.asList(new SingleObjectResponse<>(Arrays.asList(ITEM_NAME))), null, Constants.DEFAULT_REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        variantEditValidationService.validateUpcCode(productPriceStockAndImagesRequest, failedRequests, skuCodeUpcCodeMap);
      });
    } finally {
      Mockito.verify(pcbFeign)
          .getItemNameByUpcCodeAndProductCode(any(), any(), any(), any(), any(), any(), any(), any());
    }
    Assertions.assertEquals(1, failedRequests.size());
    Assertions.assertEquals(ApiErrorCode.UPC_CODE_UPDATE_FAILED.getCode(), failedRequests.get(0).getCode());
  }

  @Test
  public void validateUpcCodeEmptyUpcCodeTest() {
    List<VariantsErrorListResponse> failedRequests = new ArrayList<>();
    Map<String, String> skuCodeUpcCodeMap = new HashMap<>();
    Mockito.when(pcbFeign.getItemNameByUpcCodeAndProductCode(any(), any(), any(), any(), any(), any(), any(), any()))
        .thenReturn(new GdnRestListResponse<>(null, null, true,
            Arrays.asList(new SingleObjectResponse<>(Arrays.asList(ITEM_NAME))), null, Constants.DEFAULT_REQUEST_ID));
    variantEditValidationService.validateUpcCode(productPriceStockAndImagesRequest, failedRequests, skuCodeUpcCodeMap);
  }

  @Test
  public void validateUpcCodeEmptyResponseTest() {
    Map<String, String> skuCodeUpcCodeMap = new HashMap<>();
    skuCodeUpcCodeMap.put(SKU_CODE, "upc-code1");
    List<VariantsErrorListResponse> failedRequests = new ArrayList<>();
    productPriceStockAndImagesRequest.setUpcCode("upc-code");
    productPriceStockAndImagesRequest.setSkuCode(SKU_CODE);
    Mockito.when(pcbFeign.getItemNameByUpcCodeAndProductCode(any(), any(), any(), any(), any(), any(), any(), any()))
        .thenReturn(new GdnRestListResponse<>(null, null, true, Arrays.asList(new SingleObjectResponse<>(new ArrayList<>())),
            null, Constants.DEFAULT_REQUEST_ID));
    variantEditValidationService.validateUpcCode(productPriceStockAndImagesRequest, failedRequests, skuCodeUpcCodeMap);
    Mockito.verify(pcbFeign).getItemNameByUpcCodeAndProductCode(any(), any(), any(), any(), any(), any(), any(), any());
  }

  @Test
  public void validateUpcCodeEmptyResponse1Test() {
    Map<String, String> skuCodeUpcCodeMap = new HashMap<>();
    skuCodeUpcCodeMap.put(SKU_CODE, "upc-code1");
    List<VariantsErrorListResponse> failedRequests = new ArrayList<>();
    productPriceStockAndImagesRequest.setUpcCode("upc-code");
    productPriceStockAndImagesRequest.setSkuCode(SKU_CODE + 1);
    Mockito.when(pcbFeign.getItemNameByUpcCodeAndProductCode(any(), any(), any(), any(), any(), any(), any(), any()))
        .thenReturn(new GdnRestListResponse<>(null, null, true, Arrays.asList(new SingleObjectResponse<>(new ArrayList<>())),
            null, Constants.DEFAULT_REQUEST_ID));
    variantEditValidationService.validateUpcCode(productPriceStockAndImagesRequest, failedRequests, skuCodeUpcCodeMap);
  }


  @Test
  public void validateUpcCodeEmptyResponse2Test() {
    Map<String, String> skuCodeUpcCodeMap = new HashMap<>();
    skuCodeUpcCodeMap.put(SKU_CODE, "upc-code1");
    List<VariantsErrorListResponse> failedRequests = new ArrayList<>();
    productPriceStockAndImagesRequest.setUpcCode("upc-code1");
    productPriceStockAndImagesRequest.setSkuCode(SKU_CODE);
    Mockito.when(pcbFeign.getItemNameByUpcCodeAndProductCode(any(), any(), any(), any(), any(), any(), any(), any()))
        .thenReturn(new GdnRestListResponse<>(null, null, true, Arrays.asList(new SingleObjectResponse<>(new ArrayList<>())),
            null, Constants.DEFAULT_REQUEST_ID));
    variantEditValidationService.validateUpcCode(productPriceStockAndImagesRequest, failedRequests, skuCodeUpcCodeMap);
  }

  @Test
  public void validateUpcCodeEmptyStringResponseTest() {
    Map<String, String> skuCodeUpcCodeMap = new HashMap<>();
    skuCodeUpcCodeMap.put(SKU_CODE, "upc-code1");
    List<VariantsErrorListResponse> failedRequests = new ArrayList<>();
    productPriceStockAndImagesRequest.setUpcCode(StringUtils.EMPTY);
    productPriceStockAndImagesRequest.setSkuCode(SKU_CODE);
    Mockito.when(pcbFeign.getItemNameByUpcCodeAndProductCode(any(), any(), any(), any(), any(), any(), any(), any()))
        .thenReturn(new GdnRestListResponse<>(null, null, true, Arrays.asList(new SingleObjectResponse<>(new ArrayList<>())),
            null, Constants.DEFAULT_REQUEST_ID));
    variantEditValidationService.validateUpcCode(productPriceStockAndImagesRequest, failedRequests, skuCodeUpcCodeMap);
  }

  @Test
  public void getExistingUpcCodesForSkuCodeTest() throws Exception {
    productPriceStockAndImagesRequest.setUpcCode("upc-code");
    productPriceStockAndImagesRequest.setSkuCode(SKU_CODE);
    ProductItemResponse productItemResponse = new ProductItemResponse();
    productItemResponse.setUpcCode("upc-code1");
    productItemResponse.setSkuCode(SKU_CODE);
    Mockito.when(productOutbound.getProductItemBySkuCodes(any(SkuCodesRequest.class)))
        .thenReturn(Arrays.asList(productItemResponse));
    variantEditValidationService.getExistingUpcCodesForSkuCode(Arrays.asList(productPriceStockAndImagesRequest));
    Mockito.verifyNoMoreInteractions(pickupPointOutbound);
  }

  @Test
  public void getExistingUpcCodesForSkuCodeUpcCodeNullTest() throws Exception {
    productPriceStockAndImagesRequest.setUpcCode("upc-code");
    productPriceStockAndImagesRequest.setSkuCode(SKU_CODE);
    ProductItemResponse productItemResponse = new ProductItemResponse();
    productItemResponse.setUpcCode(null);
    productItemResponse.setSkuCode(SKU_CODE);
    Mockito.when(productOutbound.getProductItemBySkuCodes(any(SkuCodesRequest.class)))
        .thenReturn(Arrays.asList(productItemResponse));
    Map<String, String> existingUpcCodesForSkuCode = variantEditValidationService.getExistingUpcCodesForSkuCode(Arrays.asList(productPriceStockAndImagesRequest));
    Assertions.assertTrue(MapUtils.isNotEmpty(existingUpcCodesForSkuCode));
  }

  @Test
  public void validateListOfVariantsForNeedCorrectionL5Test() throws Exception {
    List<VariantsErrorListResponse> failureList = new ArrayList<>();
    variantEditValidationService.validateListOfVariantsForNeedCorrectionL5(
        Arrays.asList(productPriceStockAndImagesRequestL5), failureList);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.VALID_IMAGE_EXTENSION);
  }

  @Test
  public void validateListOfVariantsForNeedCorrectionL5_cncSwitchOn_deliveryB2BTest() throws Exception {
    ReflectionTestUtils.setField(variantEditValidationService, "cncForWarehouseFeatureSwitch", true);
    List<VariantsErrorListResponse> failureList = new ArrayList<>();
    productPriceStockAndImagesRequestL5.getModifiedItemPickupPoints().get(0).setBuyable(true);
    Mockito.when(xProductOutbound.getPickupPointDetailsByListOfPickupPointCodes(Mockito.anyList()))
        .thenReturn(Collections.singletonList(businessPartnerPickupPointResponse));
    variantEditValidationService.validateListOfVariantsForNeedCorrectionL5(
        Arrays.asList(productPriceStockAndImagesRequestL5), failureList);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.VALID_IMAGE_EXTENSION);
    Mockito.verify(xProductOutbound).getPickupPointDetailsByListOfPickupPointCodes(Mockito.anyList());
    Assertions.assertEquals(DELIVERY_API_ERROR, failureList.get(0).getCode());
  }

  @Test
  public void validateListOfVariantsForNeedCorrectionL5_cncSwitchOn_emptyModifiedPickupPointsTest() throws Exception {
    ReflectionTestUtils.setField(variantEditValidationService, "cncForWarehouseFeatureSwitch", true);
    List<VariantsErrorListResponse> failureList = new ArrayList<>();
    productPriceStockAndImagesRequestL5.setModifiedItemPickupPoints(new ArrayList<>());
    List<ProductVariantPriceStockAndImagesRequest> productVariantPriceStockAndImagesRequests =
        variantEditValidationService.validateListOfVariantsForNeedCorrectionL5(
            Arrays.asList(productPriceStockAndImagesRequestL5), failureList);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.VALID_IMAGE_EXTENSION);
    Assertions.assertEquals(1, productVariantPriceStockAndImagesRequests.size());
  }

  @Test
  public void validateListOfVariantsForNeedCorrectionL5_cncSwitchOn_pickupPointNullTest() throws Exception {
    ReflectionTestUtils.setField(variantEditValidationService, "cncForWarehouseFeatureSwitch", true);
    List<VariantsErrorListResponse> failureList = new ArrayList<>();
    productPriceStockAndImagesRequestL5.getModifiedItemPickupPoints().get(0).setBuyable(true);
    Mockito.when(xProductOutbound.getPickupPointDetailsByListOfPickupPointCodes(Mockito.anyList()))
        .thenReturn(null);
    variantEditValidationService.validateListOfVariantsForNeedCorrectionL5(
        Arrays.asList(productPriceStockAndImagesRequestL5), failureList);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.VALID_IMAGE_EXTENSION);
    Mockito.verify(xProductOutbound).getPickupPointDetailsByListOfPickupPointCodes(Mockito.anyList());
    Assertions.assertEquals(INVALID_PICKUP_POINT_CODE, failureList.get(0).getCode());
  }

  @Test
  public void validateListOfVariantsForNeedCorrectionL5_cncSwitchOn_cncB2BTest() throws Exception {
    ReflectionTestUtils.setField(variantEditValidationService, "cncForWarehouseFeatureSwitch", true);
    List<VariantsErrorListResponse> failureList = new ArrayList<>();
    productPriceStockAndImagesRequestL5.getModifiedItemPickupPoints().get(0).setCncBuyable(true);
    Mockito.when(xProductOutbound.getPickupPointDetailsByListOfPickupPointCodes(Mockito.anyList()))
        .thenReturn(Collections.singletonList(businessPartnerPickupPointResponse));
    variantEditValidationService.validateListOfVariantsForNeedCorrectionL5(
        Arrays.asList(productPriceStockAndImagesRequestL5), failureList);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.VALID_IMAGE_EXTENSION);
    Mockito.verify(xProductOutbound).getPickupPointDetailsByListOfPickupPointCodes(Mockito.anyList());
    Assertions.assertEquals(CNC_API_ERROR, failureList.get(0).getCode());
  }

  @Test
  public void validateListOfVariantsForNeedCorrectionL5_cncSwitchOn_deliveryTeaserTest() throws Exception {
    ReflectionTestUtils.setField(variantEditValidationService, "cncForWarehouseFeatureSwitch", true);
    List<VariantsErrorListResponse> failureList = new ArrayList<>();
    productPriceStockAndImagesRequestL5.getModifiedItemPickupPoints().get(0).setDisplay(true);
    Mockito.when(xProductOutbound.getPickupPointDetailsByListOfPickupPointCodes(Mockito.anyList()))
        .thenReturn(Collections.singletonList(businessPartnerPickupPointResponse));
    variantEditValidationService.validateListOfVariantsForNeedCorrectionL5(
        Arrays.asList(productPriceStockAndImagesRequestL5), failureList);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.VALID_IMAGE_EXTENSION);
    Mockito.verify(xProductOutbound).getPickupPointDetailsByListOfPickupPointCodes(Mockito.anyList());
    Assertions.assertEquals(DELIVERY_API_ERROR, failureList.get(0).getCode());
  }

  @Test
  public void validateListOfVariantsForNeedCorrectionL5_cncSwitchOn_cncTeaserTest() throws Exception {
    ReflectionTestUtils.setField(variantEditValidationService, "cncForWarehouseFeatureSwitch", true);
    List<VariantsErrorListResponse> failureList = new ArrayList<>();
    productPriceStockAndImagesRequestL5.getModifiedItemPickupPoints().get(0).setCncDisplay(true);
    Mockito.when(xProductOutbound.getPickupPointDetailsByListOfPickupPointCodes(Mockito.anyList()))
        .thenReturn(Collections.singletonList(businessPartnerPickupPointResponse));
    variantEditValidationService.validateListOfVariantsForNeedCorrectionL5(
        Arrays.asList(productPriceStockAndImagesRequestL5), failureList);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.VALID_IMAGE_EXTENSION);
    Mockito.verify(xProductOutbound).getPickupPointDetailsByListOfPickupPointCodes(Mockito.anyList());
    Assertions.assertEquals(CNC_API_ERROR, failureList.get(0).getCode());
  }

  @Test
  public void validateListOfVariantsForNeedCorrectionL5_cncSwitchOn_successTest() throws Exception {
    ReflectionTestUtils.setField(variantEditValidationService, "cncForWarehouseFeatureSwitch", true);
    List<VariantsErrorListResponse> failureList = new ArrayList<>();
    businessPartnerPickupPointResponse.setDelivery(true);
    businessPartnerPickupPointResponse.setCncActivated(true);
    productPriceStockAndImagesRequestL5.getModifiedItemPickupPoints().get(0).setDisplay(true);
    productPriceStockAndImagesRequestL5.getModifiedItemPickupPoints().get(0).setCncDisplay(true);
    Mockito.when(xProductOutbound.getPickupPointDetailsByListOfPickupPointCodes(Mockito.anyList()))
        .thenReturn(Collections.singletonList(businessPartnerPickupPointResponse));
    variantEditValidationService.validateListOfVariantsForNeedCorrectionL5(
        Arrays.asList(productPriceStockAndImagesRequestL5), failureList);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.VALID_IMAGE_EXTENSION);
    Mockito.verify(xProductOutbound).getPickupPointDetailsByListOfPickupPointCodes(Mockito.anyList());
  }

  @Test
  public void validateListOfVariantsForNeedCorrectionL5_cncSwitchOn_success_nonCncTest() throws Exception {
    ReflectionTestUtils.setField(variantEditValidationService, "cncForWarehouseFeatureSwitch", true);
    List<VariantsErrorListResponse> failureList = new ArrayList<>();
    businessPartnerPickupPointResponse.setDelivery(true);
    businessPartnerPickupPointResponse.setCncActivated(true);
    productPriceStockAndImagesRequestL5.getModifiedItemPickupPoints().get(0).setDisplay(true);
    Mockito.when(xProductOutbound.getPickupPointDetailsByListOfPickupPointCodes(Mockito.anyList()))
        .thenReturn(Collections.singletonList(businessPartnerPickupPointResponse));
    variantEditValidationService.validateListOfVariantsForNeedCorrectionL5(
        Arrays.asList(productPriceStockAndImagesRequestL5), failureList);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.VALID_IMAGE_EXTENSION);
    Mockito.verify(xProductOutbound).getPickupPointDetailsByListOfPickupPointCodes(Mockito.anyList());
  }

  @Test
  public void validateListOfVariantsForNeedCorrectionL5UpcCodeFailTest() throws Exception {
    List<ProductItemResponse> productItemResponseList = new ArrayList<>();
    productItemResponseList.add(productItemResponse);
    Mockito.when(productOutbound.getProductItemBySkuCodes(Mockito.any())).thenReturn(productItemResponseList);
    List<VariantsErrorListResponse> failureList = new ArrayList<>();
    productPriceStockAndImagesRequestL5.setUpcCode(SKU_CODE);
    productPriceStockAndImagesRequestL5.setSkuCode(SKU_CODE);
    Mockito.when(
        pcbFeign.getItemNameByUpcCodeAndProductCode(eq(Constants.DEFAULT_STORE_ID), eq(Constants.DEFAULT_CHANNEL_ID),
            eq(Constants.DEFAULT_CLIENT_ID), eq(Constants.DEFAULT_REQUEST_ID), eq(Constants.DEFAULT_USERNAME),
            Mockito.any(), Mockito.any(), Mockito.any())).thenReturn(new GdnRestListResponse<>(null, null, true,
        Collections.singletonList(new SingleObjectResponse<>(Collections.singletonList(ITEM_NAME))), null, Constants.DEFAULT_REQUEST_ID));

    variantEditValidationService.validateListOfVariantsForNeedCorrectionL5(
        Arrays.asList(productPriceStockAndImagesRequestL5), failureList);

    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.VALID_IMAGE_EXTENSION);
    Mockito.verify(productOutbound).getProductItemBySkuCodes(Mockito.any());
    Mockito.verify(pcbFeign)
        .getItemNameByUpcCodeAndProductCode(eq(Constants.DEFAULT_STORE_ID), eq(Constants.DEFAULT_CHANNEL_ID), eq(Constants.DEFAULT_CLIENT_ID), eq(Constants.DEFAULT_REQUEST_ID), eq(Constants.DEFAULT_USERNAME),
            Mockito.any(), Mockito.any(), Mockito.any());
  }

  @Test
  public void validateListOfVariantsForNeedCorrectionL5MerchantSkuNullTest() throws Exception {
    List<VariantsErrorListResponse> failureList = new ArrayList<>();
    productPriceStockAndImagesRequestL5.setMerchantSku(null);

    variantEditValidationService.validateListOfVariantsForNeedCorrectionL5(
        Arrays.asList(productPriceStockAndImagesRequestL5), failureList);

    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.VALID_IMAGE_EXTENSION);
  }

  @Test
  public void validateListOfVariantsForNeedCorrectionL5MerchantSkuFailsTest() throws Exception {
    List<VariantsErrorListResponse> failureList = new ArrayList<>();
    productPriceStockAndImagesRequestL5.setMerchantSku(MERCHANT_SKU_LENGTH_GREATER_THAN_255);
    variantEditValidationService.validateListOfVariantsForNeedCorrectionL5(
        Arrays.asList(productPriceStockAndImagesRequestL5), failureList);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.VALID_IMAGE_EXTENSION);
  }

  @Test
  public void validateListOfVariantsForNeedCorrectionL5SalePriceNullTest() throws Exception {
    List<VariantsErrorListResponse> failureList = new ArrayList<>();
    List<ItemPickupPointRequest> modifiedItemPickupPoints = new ArrayList<>();
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setPrice(6.0);
    itemPickupPointRequest.setSalePrice(null);
    modifiedItemPickupPoints.add(itemPickupPointRequest);
    productPriceStockAndImagesRequestL5.setModifiedItemPickupPoints(modifiedItemPickupPoints);

    ProductSystemParameter productSystemParameter = new ProductSystemParameter();
    productSystemParameter.setValue("5");
    Mockito.when(
            productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE))
        .thenReturn(productSystemParameter);

    variantEditValidationService.validateListOfVariantsForNeedCorrectionL5(
        Arrays.asList(productPriceStockAndImagesRequestL5), failureList);

    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.VALID_IMAGE_EXTENSION);
  }

  @Test
  public void validateListOfVariantsForNeedCorrectionL5PriceNullTest() throws Exception {
    List<VariantsErrorListResponse> failureList = new ArrayList<>();
    List<ItemPickupPointRequest> modifiedItemPickupPoints = new ArrayList<>();
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setPrice(null);
    itemPickupPointRequest.setSalePrice(6.0);
    modifiedItemPickupPoints.add(itemPickupPointRequest);
    productPriceStockAndImagesRequestL5.setModifiedItemPickupPoints(modifiedItemPickupPoints);

    ProductSystemParameter productSystemParameter = new ProductSystemParameter();
    productSystemParameter.setValue("5");
    Mockito.when(
            productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE))
        .thenReturn(productSystemParameter);

    variantEditValidationService.validateListOfVariantsForNeedCorrectionL5(
        Arrays.asList(productPriceStockAndImagesRequestL5), failureList);

    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.VALID_IMAGE_EXTENSION);
  }

  @Test
  public void validateListOfVariantsForNeedCorrectionL5ExceptionTest() throws Exception {
    List<VariantsErrorListResponse> failureList = new ArrayList<>();
    productPriceStockAndImagesRequestL5.getModifiedItemPickupPoints().get(0).setPrice(0.0);
    try {
      variantEditValidationService.validateListOfVariantsForNeedCorrectionL5(
          Arrays.asList(productPriceStockAndImagesRequestL5), failureList);
    } finally {
      Mockito.verify(productSystemParameterService)
          .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
      Mockito.verify(productSystemParameterService)
          .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.VALID_IMAGE_EXTENSION);
    }
  }

  @Test
  public void validateListOfVariantsForNeedCorrectionL5LocationPathNullTest() throws Exception {
    productPriceStockAndImagesRequestL5.getImages().get(0).setLocationPath(null);
    List<VariantsErrorListResponse> failureList = new ArrayList<>();
    variantEditValidationService.validateListOfVariantsForNeedCorrectionL5(
        Arrays.asList(productPriceStockAndImagesRequestL5), failureList);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.VALID_IMAGE_EXTENSION);
  }

  @Test
  public void validateListOfVariantsForNeedCorrectionL5SequenceNullTest() throws Exception {
    productPriceStockAndImagesRequestL5.getImages().get(0).setSequence(null);
    List<VariantsErrorListResponse> failureList = new ArrayList<>();
    variantEditValidationService.validateListOfVariantsForNeedCorrectionL5(
        Arrays.asList(productPriceStockAndImagesRequestL5), failureList);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.VALID_IMAGE_EXTENSION);
  }

  @Test
  public void validateListOfVariantsForNeedCorrectionL5MainImageNullTest() throws Exception {
    productPriceStockAndImagesRequestL5.getImages().get(0).setMainImage(null);
    List<VariantsErrorListResponse> failureList = new ArrayList<>();
    variantEditValidationService.validateListOfVariantsForNeedCorrectionL5(
        Arrays.asList(productPriceStockAndImagesRequestL5), failureList);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.VALID_IMAGE_EXTENSION);
  }

  @Test
  public void validateListOfVariantsForNeedCorrectionL5ImageExtensionNullTest() throws Exception {
    productPriceStockAndImagesRequestL5.getImages().get(0).setLocationPath("location.pdf");
    List<VariantsErrorListResponse> failureList = new ArrayList<>();
    variantEditValidationService.validateListOfVariantsForNeedCorrectionL5(
        Arrays.asList(productPriceStockAndImagesRequestL5), failureList);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.VALID_IMAGE_EXTENSION);
  }

  @Test
  public void validateListOfVariantsForNeedCorrectionTest() throws Exception {
    List<VariantsErrorListResponse> failureList = new ArrayList<>();
    variantEditValidationService.validateListOfVariantsForNeedCorrection(
        Arrays.asList(productPriceStockAndImagesRequest), failureList);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.VALID_IMAGE_EXTENSION);
  }

  @Test
  public void validateListOfVariantsForNeedCorrectionExceptionTest() throws Exception {
    List<VariantsErrorListResponse> failureList = new ArrayList<>();
    productPriceStockAndImagesRequest.getPrices().get(0).setPrice(0.0);
    try {
      variantEditValidationService.validateListOfVariantsForNeedCorrection(
          Arrays.asList(productPriceStockAndImagesRequest), failureList);
    } finally {
      Mockito.verify(productSystemParameterService)
          .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
      Mockito.verify(productSystemParameterService)
          .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.VALID_IMAGE_EXTENSION);
    }
  }

  @Test
  public void validateListOfVariantsForNeedCorrectionLocationPathNullTest() throws Exception {
    productPriceStockAndImagesRequest.getImages().get(0).setLocationPath(null);
    List<VariantsErrorListResponse> failureList = new ArrayList<>();
    variantEditValidationService.validateListOfVariantsForNeedCorrection(
        Arrays.asList(productPriceStockAndImagesRequest), failureList);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.VALID_IMAGE_EXTENSION);
  }

  @Test
  public void validateListOfVariantsForNeedCorrectionSequenceNullTest() throws Exception {
    productPriceStockAndImagesRequest.getImages().get(0).setSequence(null);
    List<VariantsErrorListResponse> failureList = new ArrayList<>();
    variantEditValidationService.validateListOfVariantsForNeedCorrection(
        Arrays.asList(productPriceStockAndImagesRequest), failureList);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.VALID_IMAGE_EXTENSION);
  }

  @Test
  public void validateListOfVariantsForNeedCorrectionMainImageNullTest() throws Exception {
    productPriceStockAndImagesRequest.getImages().get(0).setMainImage(null);
    List<VariantsErrorListResponse> failureList = new ArrayList<>();
    variantEditValidationService.validateListOfVariantsForNeedCorrection(
        Arrays.asList(productPriceStockAndImagesRequest), failureList);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.VALID_IMAGE_EXTENSION);
  }

  @Test
  public void validateListOfVariantsForNeedCorrectionImageExtensionNullTest() throws Exception {
    productPriceStockAndImagesRequest.getImages().get(0).setLocationPath("location.pdf");
    List<VariantsErrorListResponse> failureList = new ArrayList<>();
    variantEditValidationService.validateListOfVariantsForNeedCorrection(
        Arrays.asList(productPriceStockAndImagesRequest), failureList);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.VALID_IMAGE_EXTENSION);
  }

  @Test
  public void validateL5UpdateRequestTrueNullTest() throws Exception {
    variantEditValidationService.validateL5UpdateRequest(productVariantUpdateRequest, new SimpleStringResponse(), null);
    Mockito.verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(MERCHANT_CODE);
  }

  @Test
  public void validateL5UpdateRequestTrueCompanyNullTest() throws Exception {
    ProfileResponse profileResponseNew = new ProfileResponse();
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(MERCHANT_CODE)).thenReturn(profileResponseNew);
    variantEditValidationService.validateL5UpdateRequest(productVariantUpdateRequest, new SimpleStringResponse(), null);
    Mockito.verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(MERCHANT_CODE);
  }

  @Test
  public void validateL5UpdateRequestTrueCncFalseTest() throws Exception {
    ProfileResponse profileResponseNew = new ProfileResponse();
    CompanyDTO companyDTO = new CompanyDTO();
    profileResponseNew.setCompany(companyDTO);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(MERCHANT_CODE)).thenReturn(profileResponseNew);
    variantEditValidationService.validateL5UpdateRequest(productVariantUpdateRequest, new SimpleStringResponse(), null);
    Mockito.verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(MERCHANT_CODE);
  }

  @Test
  public void validateL5UpdateRequestTrueCncTrueTest() throws Exception {
    ProfileResponse profileResponse = new ProfileResponse();
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setCncActivated(true);
    profileResponse.setCompany(companyDTO);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(MERCHANT_CODE)).thenReturn(profileResponse);
    variantEditValidationService.validateL5UpdateRequest(productVariantUpdateRequest, new SimpleStringResponse(), null);
    Mockito.verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(MERCHANT_CODE);
  }

  @Test
  public void validateL5UpdateRequestTrueTest() throws Exception {
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setMultiDefaultAddressFlag(true);
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setMerchantType(Constants.CM_MERCHANT);
    profileResponse.setCompany(companyDTO);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(MERCHANT_CODE)).thenReturn(profileResponse);
    variantEditValidationService.validateL5UpdateRequest(productVariantUpdateRequest, new SimpleStringResponse(), null);
    Mockito.verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(MERCHANT_CODE);
  }

  @Test
  public void validateL5UpdateRequestStatusUpdateWithAddL5Test() throws Exception {
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setDisplay(true);
    itemPickupPointRequest.setBuyable(true);
    itemPickupPointRequest.setPickupPointId(PICKUP_POINT_CODE);
    itemPickupPointRequest.setItemSku(ITEM_SKU);
    productVariantUpdateRequest.setFreeSample(true);
    productVariantUpdateRequest.setAddPickupPoints(Collections.singletonList(itemPickupPointRequest));
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      variantEditValidationService.validateL5UpdateRequest(productVariantUpdateRequest, new SimpleStringResponse(),
          profileResponse);
    });
  }

  @Test()
  public void validateL5UpdateRequestDisplayUpdateWithAddL5Test() throws Exception {
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setMultiDefaultAddressFlag(false);
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setCncActivated(true);
    profileResponse.setCompany(companyDTO);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(MERCHANT_CODE)).thenReturn(profileResponse);
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setDisplay(false);
    itemPickupPointRequest.setBuyable(false);
    itemPickupPointRequest.setPickupPointId(PICKUP_POINT_CODE);
    itemPickupPointRequest.setItemSku(ITEM_SKU);
    productVariantUpdateRequest.setFreeSample(true);
    productVariantUpdateRequest.setAddPickupPoints(Collections.singletonList(itemPickupPointRequest));
    variantEditValidationService.validateL5UpdateRequest(productVariantUpdateRequest, new SimpleStringResponse(), null);
    Mockito.verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(MERCHANT_CODE);
  }

  @Test
  public void validateL5UpdateRequestDisplayUpdateWithEmptyProductItemL5Test() throws Exception {
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setMultiDefaultAddressFlag(false);
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setCncActivated(true);
    profileResponse.setCompany(companyDTO);
    productVariantUpdateRequest.setProductItems(new ArrayList<>());
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(MERCHANT_CODE)).thenReturn(profileResponse);
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setDisplay(false);
    itemPickupPointRequest.setBuyable(true);
    itemPickupPointRequest.setPickupPointId(PICKUP_POINT_CODE);
    itemPickupPointRequest.setItemSku(ITEM_SKU);
    productVariantUpdateRequest.setFreeSample(true);
    productVariantUpdateRequest.setAddPickupPoints(Collections.singletonList(itemPickupPointRequest));
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      variantEditValidationService.validateL5UpdateRequest(productVariantUpdateRequest, new SimpleStringResponse(), null);
    });
  }

  @Test
  public void validateL5UpdateRequestStatusUpdateWithCNCL5Test() throws Exception {
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setDisplay(false);
    itemPickupPointRequest.setBuyable(false);
    itemPickupPointRequest.setPickupPointId(PICKUP_POINT_CODE);
    itemPickupPointRequest.setItemSku(ITEM_SKU);
    itemPickupPointRequest.setCncActive(true);
    productVariantUpdateRequest.setFreeSample(true);
    productVariantUpdateRequest.setAddPickupPoints(Collections.singletonList(itemPickupPointRequest));
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      variantEditValidationService.validateL5UpdateRequest(productVariantUpdateRequest, new SimpleStringResponse(),
          profileResponse);
    });
  }

  @Test()
  public void validateL5UpdateRequestDisplayUpdateWithEmptyProductItemL5_TrueTest() throws Exception {
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setMultiDefaultAddressFlag(false);
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setCncActivated(true);
    profileResponse.setCompany(companyDTO);
    productVariantUpdateRequest.setProductItems(new ArrayList<>());
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(MERCHANT_CODE)).thenReturn(profileResponse);
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setDisplay(false);
    itemPickupPointRequest.setBuyable(false);
    itemPickupPointRequest.setPickupPointId(PICKUP_POINT_CODE);
    itemPickupPointRequest.setItemSku(ITEM_SKU);
    productVariantUpdateRequest.setFreeSample(true);
    productVariantUpdateRequest.setAddPickupPoints(Collections.singletonList(itemPickupPointRequest));
    variantEditValidationService.validateL5UpdateRequest(productVariantUpdateRequest, new SimpleStringResponse(), null);
    Mockito.verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(MERCHANT_CODE);
  }

  @Test
  public void validateL5UpdateRequestSellerTypeCMTest() throws Exception {
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setMultiDefaultAddressFlag(true);
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setMerchantType(Constants.CM_MERCHANT);
    profileResponse.setCompany(companyDTO);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(MERCHANT_CODE)).thenReturn(profileResponse);
    variantEditValidationService.validateL5UpdateRequest(productVariantUpdateRequest, new SimpleStringResponse(), null);
    Mockito.verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(MERCHANT_CODE);
  }

  @Test
  public void validateL5UpdateRequestWithErrorCodeTest() throws Exception {
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setMultiDefaultAddressFlag(true);
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setMerchantType(Constants.CM_MERCHANT);
    profileResponse.setCompany(companyDTO);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(MERCHANT_CODE)).thenReturn(profileResponse);

    variantEditValidationService.validateL5UpdateRequestWithErrorCode(productVariantUpdateRequest);

    Mockito.verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(MERCHANT_CODE);
  }

  @Test
  public void validateL5UpdateRequestWithErrorCodeApplicationRuntimeExceptionTest() throws Exception {
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(MERCHANT_CODE)).thenThrow(new ApplicationRuntimeException(ErrorCategory.VALIDATION, ApiErrorCode.OPERATION_NOT_PERMITTED.getDesc()));

    variantEditValidationService.validateL5UpdateRequestWithErrorCode(productVariantUpdateRequest);

    Mockito.verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(MERCHANT_CODE);
  }

  @Test
  public void validateL5UpdateRequestWithErrorCodeApplicationRuntimeExceptionNewTest() throws Exception {
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(MERCHANT_CODE)).thenThrow(new ApplicationRuntimeException(ErrorCategory.VALIDATION, ApiErrorCode.ALL_VARIANTS_ERROR.getDesc()));

    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      variantEditValidationService.validateL5UpdateRequestWithErrorCode(productVariantUpdateRequest);
    });

    Mockito.verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(MERCHANT_CODE);
  }

  @Test
  public void validateL5UpdateRequestWithErrorCodeExceptionTest() throws Exception {
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(MERCHANT_CODE)).thenThrow(new NullPointerException());

    Assertions.assertThrows(Exception.class, () -> {
      variantEditValidationService.validateL5UpdateRequestWithErrorCode(productVariantUpdateRequest);
    });

    Mockito.verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(MERCHANT_CODE);
  }

  @Test
  public void validateListOfVariantsL5WithSuccessTest() throws Exception {
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE))
        .thenReturn(new ProductSystemParameter(Constants.MINIMUM_PRICE, "100", "min-price", false));
    variantEditValidationService.validateListOfVariantsL5WithSuccess(new ArrayList<>(), new HashMap<>(), new HashMap<>(), new ArrayList<>(),
        new SimpleStringResponse(), true, null);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.VALID_IMAGE_EXTENSION);
  }

  @Test
  public void validateListOfVariantsL5WithUPC1Test() throws Exception {
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE))
        .thenReturn(new ProductSystemParameter(Constants.MINIMUM_PRICE, "100", "min-price", false));
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest =
        new ProductVariantPriceStockAndImagesRequest();
    variantEditValidationService.validateListOfVariantsL5WithSuccess(Collections.singletonList(productVariantPriceStockAndImagesRequest),
        new HashMap<>(), new HashMap<>(), new ArrayList<>(), new SimpleStringResponse(), true, null);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.VALID_IMAGE_EXTENSION);
  }

  @Test
  public void validateListOfVariantsL5WithUPC2Test() throws Exception {
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE))
        .thenReturn(new ProductSystemParameter(Constants.MINIMUM_PRICE, "100", "min-price", false));
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest =
        new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest.setUpcCode(UPC_CODE);
    variantEditValidationService.validateListOfVariantsL5WithSuccess(Collections.singletonList(productVariantPriceStockAndImagesRequest),
        new HashMap<>(), new HashMap<>(), new ArrayList<>(), new SimpleStringResponse(), true, null);

    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.VALID_IMAGE_EXTENSION);
  }

  @Test
  public void validateListOfVariantsL5WithUPC3Test() throws Exception {
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE))
        .thenReturn(new ProductSystemParameter(Constants.MINIMUM_PRICE, "100", "min-price", false));
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest =
        new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest.setUpcCode(UPC_CODE);
    productVariantPriceStockAndImagesRequest.setSkuCode(SKU_CODE);
    ProductItemResponse productItemResponse = new ProductItemResponse();
    productItemResponse.setSkuCode(SKU_CODE);
    productItemResponse.setUpcCode(UPC_CODE);
    SkuCodesRequest skuCodesRequest = new SkuCodesRequest();
    skuCodesRequest.setFetchArchived(true);
    skuCodesRequest.setSkuCodes(Collections.singletonList(SKU_CODE));
    Mockito.when(productOutbound.getProductItemBySkuCodes(skuCodesRequest)).thenReturn(Collections.singletonList(productItemResponse));
    variantEditValidationService.validateListOfVariantsL5WithSuccess(Collections.singletonList(productVariantPriceStockAndImagesRequest),
        new HashMap<>(), new HashMap<>(), new ArrayList<>(), new SimpleStringResponse(), true, null);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.VALID_IMAGE_EXTENSION);
  }

  @Disabled
  @Test
  public void validateListOfVariantsL5WithUPCCodeUpdateExceptionTest() throws Exception {
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE))
        .thenReturn(new ProductSystemParameter(Constants.MINIMUM_PRICE, "100", "min-price", false));
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest = new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest.setUpcCode(UPC_CODE_UPDATED);
    productVariantPriceStockAndImagesRequest.setSkuCode(SKU_CODE);
    productVariantPriceStockAndImagesRequest.setProductCode(PRODUCT_CODE);
    ProductItemResponse productItemResponse = new ProductItemResponse();
    productItemResponse.setSkuCode(SKU_CODE);
    productItemResponse.setUpcCode(UPC_CODE);
    SkuCodesRequest skuCodesRequest = new SkuCodesRequest();
    skuCodesRequest.setFetchArchived(true);
    skuCodesRequest.setSkuCodes(Collections.singletonList(SKU_CODE));
    Mockito.when(productOutbound.getProductItemBySkuCodes(skuCodesRequest)).thenReturn(Collections.singletonList(productItemResponse));
    Mockito.when(pcbFeign.getItemNameByUpcCodeAndProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, UPC_CODE_UPDATED,
        PRODUCT_CODE, SKU_CODE)).thenReturn(new GdnRestListResponse<>(null, null, true, Collections.singletonList(new SingleObjectResponse<>(Collections.singletonList(ITEM_NAME))), null,
        Constants.DEFAULT_REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        variantEditValidationService.validateListOfVariantsL5WithSuccess(Collections.singletonList(productVariantPriceStockAndImagesRequest),
            new HashMap<>(), new HashMap<>(), new ArrayList<>(), new SimpleStringResponse(), true, null);
      });
    } finally {
      Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
      Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.VALID_IMAGE_EXTENSION);
      Mockito.verify(pcbFeign)
          .getItemNameByUpcCodeAndProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, UPC_CODE_UPDATED,
              PRODUCT_CODE, SKU_CODE);
    }
  }

  @Test
  public void validateListOfVariantsL5WithUPCCodeUpdateTest() throws Exception {
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE))
        .thenReturn(new ProductSystemParameter(Constants.MINIMUM_PRICE, "100", "min-price", false));
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest =
        new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest.setUpcCode(UPC_CODE_UPDATED);
    productVariantPriceStockAndImagesRequest.setSkuCode(SKU_CODE);
    productVariantPriceStockAndImagesRequest.setProductCode(PRODUCT_CODE);
    Mockito.when(productOutbound.getProductItemBySkuCodes(skuCodesRequest))
        .thenReturn(Collections.singletonList(productItemResponse));
    Mockito.when(pcbFeign.getItemNameByUpcCodeAndProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, UPC_CODE_UPDATED,
        PRODUCT_CODE, SKU_CODE)).thenReturn(new GdnRestListResponse<>(null, null, true,
        Collections.singletonList(new SingleObjectResponse<>(Collections.emptyList())), null,
        Constants.DEFAULT_REQUEST_ID));
    variantEditValidationService.validateListOfVariantsL5WithSuccess(Collections.singletonList(productVariantPriceStockAndImagesRequest),
        new HashMap<>(), new HashMap<>(), new ArrayList<>(), new SimpleStringResponse(), true, null);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.VALID_IMAGE_EXTENSION);
  }

  @Test
  public void validateListOfVariantsL5WholesaleFlagFalseTest() throws Exception {
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE))
        .thenReturn(new ProductSystemParameter(Constants.MINIMUM_PRICE, "100", "min-price", false));
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest =
        new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest.setUpcCode(UPC_CODE_UPDATED);
    productVariantPriceStockAndImagesRequest.setSkuCode(SKU_CODE);
    productVariantPriceStockAndImagesRequest.setProductCode(PRODUCT_CODE);
    productVariantPriceStockAndImagesRequest.setItemSku(ITEM_SKU);
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setPrice(1000.0);
    itemPickupPointRequest.setSalePrice(1000.0);
    itemPickupPointRequest.setWholesalePriceActivated(true);
    ProductItemWholesalePriceRequest productItemWholesalePriceRequest = new ProductItemWholesalePriceRequest();
    productItemWholesalePriceRequest.setWholesaleDiscount(10);
    productItemWholesalePriceRequest.setQuantity(2);
    itemPickupPointRequest.setProductItemWholesalePriceRequests(Collections.singletonList(productItemWholesalePriceRequest));
    itemPickupPointRequest.setItemSku(ITEM_SKU);
    itemPickupPointRequest.setPickupPointId(PICKUP_POINT_ID);
    productVariantPriceStockAndImagesRequest.setModifiedItemPickupPoints(Collections.singletonList(itemPickupPointRequest));
    Mockito.when(productOutbound.getProductItemBySkuCodes(skuCodesRequest))
        .thenReturn(Collections.singletonList(productItemResponse));
    Mockito.when(pcbFeign.getItemNameByUpcCodeAndProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, UPC_CODE_UPDATED,
        PRODUCT_CODE, SKU_CODE)).thenReturn(new GdnRestListResponse<>(null, null, true,
        Collections.singletonList(new SingleObjectResponse<>(Collections.emptyList())), null,
        Constants.DEFAULT_REQUEST_ID));
    variantEditValidationService.validateListOfVariantsL5WithSuccess(Collections.singletonList(productVariantPriceStockAndImagesRequest),
        itemPickupPointListingResponseMap, wholesalePriceSkuResponseMap, new ArrayList<>(), new SimpleStringResponse(), true, null);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.VALID_IMAGE_EXTENSION);
    Mockito.verify(wholesaleValidationUtil)
        .validateWholesaleConfigOnUpdate(eq(CATEGORY_CODE), listArgumentCaptor.capture(),
            itemRequestArgumentCaptor.capture(), eq(100), eq(ITEM_SKU + Constants.HYPHEN + PICKUP_POINT_ID), eq(true),
            eq(null));
    Assertions.assertEquals(100, listArgumentCaptor.getValue().get(0).getQuantity());
    Assertions.assertEquals(10, listArgumentCaptor.getValue().get(0).getWholesaleDiscount(), 0);
  }

  @Test
  public void validateListOfVariantsL5WholesaleFlagFalse1Test() throws Exception {
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE))
        .thenReturn(new ProductSystemParameter(Constants.MINIMUM_PRICE, "100", "min-price", false));
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest =
        new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest.setUpcCode(UPC_CODE_UPDATED);
    productVariantPriceStockAndImagesRequest.setSkuCode(SKU_CODE);
    productVariantPriceStockAndImagesRequest.setProductCode(PRODUCT_CODE);
    productVariantPriceStockAndImagesRequest.setItemSku(ITEM_SKU);
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setPrice(1000.0);
    itemPickupPointRequest.setSalePrice(1000.0);
    itemPickupPointRequest.setWholesalePriceActivated(true);
    ProductItemWholesalePriceRequest productItemWholesalePriceRequest = new ProductItemWholesalePriceRequest();
    productItemWholesalePriceRequest.setWholesaleDiscount(10);
    productItemWholesalePriceRequest.setQuantity(2);
    itemPickupPointRequest.setProductItemWholesalePriceRequests(Collections.singletonList(productItemWholesalePriceRequest));
    itemPickupPointRequest.setItemSku(ITEM_SKU);
    itemPickupPointRequest.setPickupPointId(PICKUP_POINT_ID);
    productVariantPriceStockAndImagesRequest.setModifiedItemPickupPoints(Collections.singletonList(itemPickupPointRequest));
    Mockito.when(productOutbound.getProductItemBySkuCodes(skuCodesRequest))
        .thenReturn(Collections.singletonList(productItemResponse));
    Map<String, WholesalePriceSkuResponse> wholesalePriceSkuResponseMap1 = new HashMap<>();
    wholesalePriceSkuResponseMap1.putIfAbsent(PRODUCT_CODE + Constants.HYPHEN + PICKUP_POINT_ID,
        wholesalePriceSkuResponse);
    variantEditValidationService.validateListOfVariantsL5WithSuccess(Collections.singletonList(productVariantPriceStockAndImagesRequest),
        itemPickupPointListingResponseMap, wholesalePriceSkuResponseMap1, new ArrayList<>(), new SimpleStringResponse(), true, null);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.VALID_IMAGE_EXTENSION);
  }

  @Test
  public void validateListOfVariantsL5WholesaleFlagTest() throws Exception {
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE))
        .thenReturn(new ProductSystemParameter(Constants.MINIMUM_PRICE, "100", "min-price", false));
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest =
        new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest.setUpcCode(UPC_CODE_UPDATED);
    productVariantPriceStockAndImagesRequest.setSkuCode(SKU_CODE);
    productVariantPriceStockAndImagesRequest.setProductCode(PRODUCT_CODE);
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setPrice(1000.0);
    itemPickupPointRequest.setSalePrice(1000.0);
    itemPickupPointRequest.setWholesalePriceActivated(null);
    itemPickupPointRequest.setItemSku(ITEM_SKU);
    itemPickupPointRequest.setPickupPointId(PICKUP_POINT_ID);
    ItemPickupPointRequest itemPickupPointRequest1 = new ItemPickupPointRequest();
    itemPickupPointRequest1.setPrice(1000.0);
    itemPickupPointRequest1.setSalePrice(1000.0);
    itemPickupPointRequest1.setWholesalePriceActivated(false);
    itemPickupPointRequest1.setItemSku(ITEM_SKU);
    itemPickupPointRequest1.setPickupPointId(PICKUP_POINT_ID);
    productVariantPriceStockAndImagesRequest.setModifiedItemPickupPoints(Arrays.asList(itemPickupPointRequest, itemPickupPointRequest1));
    Mockito.when(productOutbound.getProductItemBySkuCodes(skuCodesRequest))
        .thenReturn(Collections.singletonList(productItemResponse));
    variantEditValidationService.validateListOfVariantsL5WithSuccess(Collections.singletonList(productVariantPriceStockAndImagesRequest),
        new HashMap<>(), new HashMap<>(), new ArrayList<>(), new SimpleStringResponse(), true, null);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.VALID_IMAGE_EXTENSION);
  }

  @Test
  public void validateListOfVariantsL5PriceNullTest() throws Exception {
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE))
        .thenReturn(new ProductSystemParameter(Constants.MINIMUM_PRICE, "100", "min-price", false));
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest =
        new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest.setUpcCode(UPC_CODE_UPDATED);
    productVariantPriceStockAndImagesRequest.setSkuCode(SKU_CODE);
    productVariantPriceStockAndImagesRequest.setProductCode(PRODUCT_CODE);
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setPrice(null);
    itemPickupPointRequest.setSalePrice(1000.0);
    itemPickupPointRequest.setWholesalePriceActivated(null);
    itemPickupPointRequest.setItemSku(ITEM_SKU);
    itemPickupPointRequest.setPickupPointId(PICKUP_POINT_ID);
    productVariantPriceStockAndImagesRequest.setModifiedItemPickupPoints(Collections.singletonList(itemPickupPointRequest));
    Mockito.when(productOutbound.getProductItemBySkuCodes(skuCodesRequest))
        .thenReturn(Collections.singletonList(productItemResponse));
    Mockito.when(pcbFeign.getItemNameByUpcCodeAndProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, UPC_CODE_UPDATED,
        PRODUCT_CODE, SKU_CODE)).thenReturn(new GdnRestListResponse<>(null, null, true,
        Collections.singletonList(new SingleObjectResponse<>(Collections.emptyList())), null,
        Constants.DEFAULT_REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        variantEditValidationService.validateListOfVariantsL5WithSuccess(Collections.singletonList(productVariantPriceStockAndImagesRequest),
            new HashMap<>(), new HashMap<>(), new ArrayList<>(), new SimpleStringResponse(), true, null);
      });
    } finally {

      Mockito.verify(productSystemParameterService)
          .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
      Mockito.verify(productSystemParameterService)
          .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.VALID_IMAGE_EXTENSION);
    }
  }

  @Test
  public void validateListOfVariantsL5SalePriceNullTest() throws Exception {
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE))
        .thenReturn(new ProductSystemParameter(Constants.MINIMUM_PRICE, "100", "min-price", false));
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest =
        new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest.setUpcCode(UPC_CODE_UPDATED);
    productVariantPriceStockAndImagesRequest.setSkuCode(SKU_CODE);
    productVariantPriceStockAndImagesRequest.setProductCode(PRODUCT_CODE);
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setPrice(1000.0);
    itemPickupPointRequest.setSalePrice(null);
    itemPickupPointRequest.setWholesalePriceActivated(null);
    itemPickupPointRequest.setItemSku(ITEM_SKU);
    itemPickupPointRequest.setPickupPointId(PICKUP_POINT_ID);
    productVariantPriceStockAndImagesRequest.setModifiedItemPickupPoints(Collections.singletonList(itemPickupPointRequest));
    Mockito.when(productOutbound.getProductItemBySkuCodes(skuCodesRequest))
        .thenReturn(Collections.singletonList(productItemResponse));
    Mockito.when(pcbFeign.getItemNameByUpcCodeAndProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, UPC_CODE_UPDATED,
        PRODUCT_CODE, SKU_CODE)).thenReturn(new GdnRestListResponse<>(null, null, true,
        Collections.singletonList(new SingleObjectResponse<>(Collections.emptyList())), null,
        Constants.DEFAULT_REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        variantEditValidationService.validateListOfVariantsL5WithSuccess(Collections.singletonList(productVariantPriceStockAndImagesRequest),
            new HashMap<>(), new HashMap<>(), new ArrayList<>(), new SimpleStringResponse(), true, null);
      });
    } finally {
      Mockito.verify(productSystemParameterService)
          .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
      Mockito.verify(productSystemParameterService)
          .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.VALID_IMAGE_EXTENSION);
    }
  }

  @Test
  public void validateListOfVariantsL5PriceMinimumNullTest() throws Exception {
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE))
        .thenReturn(new ProductSystemParameter(Constants.MINIMUM_PRICE, "100", "min-price", false));
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest =
        new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest.setUpcCode(UPC_CODE_UPDATED);
    productVariantPriceStockAndImagesRequest.setSkuCode(SKU_CODE);
    productVariantPriceStockAndImagesRequest.setProductCode(PRODUCT_CODE);
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setPrice(10.0);
    itemPickupPointRequest.setWholesalePriceActivated(null);
    itemPickupPointRequest.setItemSku(ITEM_SKU);
    itemPickupPointRequest.setPickupPointId(PICKUP_POINT_ID);
    productVariantPriceStockAndImagesRequest.setModifiedItemPickupPoints(Collections.singletonList(itemPickupPointRequest));
    Mockito.when(productOutbound.getProductItemBySkuCodes(skuCodesRequest))
        .thenReturn(Collections.singletonList(productItemResponse));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        variantEditValidationService.validateListOfVariantsL5WithSuccess(Collections.singletonList(productVariantPriceStockAndImagesRequest),
            new HashMap<>(), new HashMap<>(), new ArrayList<>(), new SimpleStringResponse(), true, null);
      });
    } finally {
      Mockito.verify(productSystemParameterService)
          .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
      Mockito.verify(productSystemParameterService)
          .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.VALID_IMAGE_EXTENSION);
    }
  }

  @Test
  public void validateListOfVariantsL5SalePriceMinimumNullTest() throws Exception {
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE))
        .thenReturn(new ProductSystemParameter(Constants.MINIMUM_PRICE, "100", "min-price", false));
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest =
        new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest.setUpcCode(UPC_CODE_UPDATED);
    productVariantPriceStockAndImagesRequest.setSkuCode(SKU_CODE);
    productVariantPriceStockAndImagesRequest.setProductCode(PRODUCT_CODE);
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setPrice(1000.0);
    itemPickupPointRequest.setSalePrice(10.0);
    itemPickupPointRequest.setWholesalePriceActivated(null);
    itemPickupPointRequest.setItemSku(ITEM_SKU);
    itemPickupPointRequest.setPickupPointId(PICKUP_POINT_ID);
    itemPickupPointRequest.setB2bFields(B2BFields.builder().price(400.0).buyable(true).display(true).build());
    productVariantPriceStockAndImagesRequest.setModifiedItemPickupPoints(Collections.singletonList(itemPickupPointRequest));
    Mockito.when(productOutbound.getProductItemBySkuCodes(skuCodesRequest))
        .thenReturn(Collections.singletonList(productItemResponse));
    Mockito.when(pcbFeign.getItemNameByUpcCodeAndProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, UPC_CODE_UPDATED,
        PRODUCT_CODE, SKU_CODE)).thenReturn(new GdnRestListResponse<>(null, null, true,
        Collections.singletonList(new SingleObjectResponse<>(Collections.emptyList())), null,
        Constants.DEFAULT_REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        variantEditValidationService.validateListOfVariantsL5WithSuccess(Collections.singletonList(productVariantPriceStockAndImagesRequest),
            new HashMap<>(), new HashMap<>(), new ArrayList<>(), new SimpleStringResponse(), true, null);
      });
    } finally {
      Mockito.verify(productSystemParameterService)
          .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
      Mockito.verify(productSystemParameterService)
          .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.VALID_IMAGE_EXTENSION);
    }
  }


  @Test
  public void validateListOfVariantsL5SalePriceMinimumForNull_forBFBTest() throws Exception {
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE))
        .thenReturn(new ProductSystemParameter(Constants.MINIMUM_PRICE, "1", "min-price", false));
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest = new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest.setUpcCode(UPC_CODE_UPDATED);
    productVariantPriceStockAndImagesRequest.setSkuCode(SKU_CODE);
    productVariantPriceStockAndImagesRequest.setProductCode(PRODUCT_CODE);
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setPrice(1000.0);
    itemPickupPointRequest.setB2bFields(B2BFields.builder().price(0.0).buyable(true).display(true).build());
    itemPickupPointRequest.setSalePrice(10.0);
    itemPickupPointRequest.setWholesalePriceActivated(null);
    itemPickupPointRequest.setItemSku(ITEM_SKU);
    itemPickupPointRequest.setPickupPointId(PICKUP_POINT_ID);
    productVariantPriceStockAndImagesRequest.setModifiedItemPickupPoints(Collections.singletonList(itemPickupPointRequest));
    Mockito.when(productOutbound.getProductItemBySkuCodes(skuCodesRequest)).thenReturn(Collections.singletonList(productItemResponse));
    Mockito.when(pcbFeign.getItemNameByUpcCodeAndProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, UPC_CODE_UPDATED,
        PRODUCT_CODE, SKU_CODE)).thenReturn(new GdnRestListResponse<>(null, null, true, Collections.singletonList(new SingleObjectResponse<>(Collections.emptyList())), null,
        Constants.DEFAULT_REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        variantEditValidationService.validateListOfVariantsL5WithSuccess(Collections.singletonList(productVariantPriceStockAndImagesRequest),
            new HashMap<>(), new HashMap<>(), new ArrayList<>(), new SimpleStringResponse(), true, null);
      });
    } finally {
      Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
      Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.VALID_IMAGE_EXTENSION);
    }
  }

  @Test()
  public void validateListOfVariantsL5SalePrice_forBFBTest() throws Exception {
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE))
        .thenReturn(new ProductSystemParameter(Constants.MINIMUM_PRICE, "1", "min-price", false));
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest = new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest.setUpcCode(UPC_CODE_UPDATED);
    productVariantPriceStockAndImagesRequest.setSkuCode(SKU_CODE);
    productVariantPriceStockAndImagesRequest.setProductCode(PRODUCT_CODE);
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setPrice(1000.0);
    itemPickupPointRequest.setB2bFields(B2BFields.builder().price(400.0).buyable(true).display(true).build());
    itemPickupPointRequest.setSalePrice(10.0);
    itemPickupPointRequest.setWholesalePriceActivated(null);
    itemPickupPointRequest.setItemSku(ITEM_SKU);
    itemPickupPointRequest.setPickupPointId(PICKUP_POINT_ID);
    productVariantPriceStockAndImagesRequest.setModifiedItemPickupPoints(Collections.singletonList(itemPickupPointRequest));
    Mockito.when(productOutbound.getProductItemBySkuCodes(skuCodesRequest)).thenReturn(Collections.singletonList(productItemResponse));
    Mockito.when(pcbFeign.getItemNameByUpcCodeAndProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, UPC_CODE_UPDATED,
        PRODUCT_CODE, SKU_CODE)).thenReturn(new GdnRestListResponse<>(null, null, true, Collections.singletonList(new SingleObjectResponse<>(Collections.emptyList())), null,
        Constants.DEFAULT_REQUEST_ID));
    try {
      variantEditValidationService.validateListOfVariantsL5WithSuccess(Collections.singletonList(productVariantPriceStockAndImagesRequest),
          new HashMap<>(), new HashMap<>(), new ArrayList<>(), new SimpleStringResponse(), true, null);
    } finally {
      Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
      Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.VALID_IMAGE_EXTENSION);
    }
  }

  @Test
  public void validateListOfVariantsL5SalePrice_forNegetivePriceBFBTest() throws Exception {
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE))
        .thenReturn(new ProductSystemParameter(Constants.MINIMUM_PRICE, "1", "min-price", false));
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest = new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest.setUpcCode(UPC_CODE_UPDATED);
    productVariantPriceStockAndImagesRequest.setSkuCode(SKU_CODE);
    productVariantPriceStockAndImagesRequest.setProductCode(PRODUCT_CODE);
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setPrice(1000.0);
    itemPickupPointRequest.setB2bFields(B2BFields.builder().price(-4.0).buyable(true).display(true).build());
    itemPickupPointRequest.setSalePrice(10.0);
    itemPickupPointRequest.setWholesalePriceActivated(null);
    itemPickupPointRequest.setItemSku(ITEM_SKU);
    itemPickupPointRequest.setPickupPointId(PICKUP_POINT_ID);
    productVariantPriceStockAndImagesRequest.setModifiedItemPickupPoints(Collections.singletonList(itemPickupPointRequest));
    Mockito.when(productOutbound.getProductItemBySkuCodes(skuCodesRequest)).thenReturn(Collections.singletonList(productItemResponse));
    Mockito.when(pcbFeign.getItemNameByUpcCodeAndProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, UPC_CODE_UPDATED,
        PRODUCT_CODE, SKU_CODE)).thenReturn(new GdnRestListResponse<>(null, null, true, Collections.singletonList(new SingleObjectResponse<>(Collections.emptyList())), null,
        Constants.DEFAULT_REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        variantEditValidationService.validateListOfVariantsL5WithSuccess(Collections.singletonList(productVariantPriceStockAndImagesRequest),
            new HashMap<>(), new HashMap<>(), new ArrayList<>(), new SimpleStringResponse(), true, null);
      });
    } finally {
      Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
      Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.VALID_IMAGE_EXTENSION);
    }
  }


  @Test
  public void validateListOfVariantsL5ImageValidTest() throws Exception {
    generateDummyImageFiles(LOCATION_PATH);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE))
        .thenReturn(new ProductSystemParameter(Constants.MINIMUM_PRICE, "100", "min-price", false));
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest =
        new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest.setUpcCode(UPC_CODE_UPDATED);
    productVariantPriceStockAndImagesRequest.setSkuCode(SKU_CODE);
    productVariantPriceStockAndImagesRequest.setProductCode(PRODUCT_CODE);
    ProductLevel3SummaryDetailsImageRequest productLevel3SummaryDetailsImageRequest =
        new ProductLevel3SummaryDetailsImageRequest();
    productLevel3SummaryDetailsImageRequest.setMainImage(true);
    productLevel3SummaryDetailsImageRequest.setMarkForDelete(false);
    productLevel3SummaryDetailsImageRequest.setLocationPath(LOCATION_PATH);
    productLevel3SummaryDetailsImageRequest.setSequence(0);
    productVariantPriceStockAndImagesRequest.setImages(Collections.singletonList(productLevel3SummaryDetailsImageRequest));
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setPrice(1000.0);
    itemPickupPointRequest.setSalePrice(1000.0);
    itemPickupPointRequest.setWholesalePriceActivated(null);
    itemPickupPointRequest.setItemSku(ITEM_SKU);
    itemPickupPointRequest.setSellerSku(MERCHANT_CODE);
    itemPickupPointRequest.setPickupPointId(PICKUP_POINT_ID);
    productVariantPriceStockAndImagesRequest.setModifiedItemPickupPoints(Collections.singletonList(itemPickupPointRequest));
    Mockito.when(productOutbound.getProductItemBySkuCodes(skuCodesRequest))
        .thenReturn(Collections.singletonList(productItemResponse));
    Mockito.when(pcbFeign.getItemNameByUpcCodeAndProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, UPC_CODE_UPDATED,
        PRODUCT_CODE, SKU_CODE)).thenReturn(new GdnRestListResponse<>(null, null, true,
        Collections.singletonList(new SingleObjectResponse<>(Collections.emptyList())), null,
        Constants.DEFAULT_REQUEST_ID));
    variantEditValidationService.validateListOfVariantsL5WithSuccess(Collections.singletonList(productVariantPriceStockAndImagesRequest),
        new HashMap<>(), new HashMap<>(), new ArrayList<>(), new SimpleStringResponse(), true, null);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.VALID_IMAGE_EXTENSION);
    deleteDummyFiles(LOCATION_PATH);
  }

  @Test
  public void validateListOfVariantsL5ImageImageInvalidTest() throws Exception {
    generateDummyImageFiles(LOCATION_PATH);
    Mockito.when(
            productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE))
        .thenReturn(new ProductSystemParameter(Constants.MINIMUM_PRICE, "100", "min-price", false));
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest =
        new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest.setUpcCode(UPC_CODE_UPDATED);
    productVariantPriceStockAndImagesRequest.setSkuCode(SKU_CODE);
    productVariantPriceStockAndImagesRequest.setProductCode(PRODUCT_CODE);
    ProductLevel3SummaryDetailsImageRequest productLevel3SummaryDetailsImageRequest =
        new ProductLevel3SummaryDetailsImageRequest();
    productLevel3SummaryDetailsImageRequest.setMainImage(true);
    productLevel3SummaryDetailsImageRequest.setMarkForDelete(false);
    productLevel3SummaryDetailsImageRequest.setLocationPath(LOCATION_PATH);
    productLevel3SummaryDetailsImageRequest.setSequence(0);
    productLevel3SummaryDetailsImageRequest.setReviewType(Constants.NEW);
    productVariantPriceStockAndImagesRequest.setImages(Collections.singletonList(productLevel3SummaryDetailsImageRequest));
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setPrice(1000.0);
    itemPickupPointRequest.setSalePrice(1000.0);
    itemPickupPointRequest.setWholesalePriceActivated(null);
    itemPickupPointRequest.setItemSku(ITEM_SKU);
    itemPickupPointRequest.setSellerSku(MERCHANT_CODE);
    itemPickupPointRequest.setPickupPointId(PICKUP_POINT_ID);
    productVariantPriceStockAndImagesRequest.setModifiedItemPickupPoints(Collections.singletonList(itemPickupPointRequest));
    Mockito.when(productOutbound.getProductItemBySkuCodes(skuCodesRequest))
        .thenReturn(Collections.singletonList(productItemResponse));
    Mockito.when(pcbFeign.getItemNameByUpcCodeAndProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, UPC_CODE_UPDATED,
        PRODUCT_CODE, SKU_CODE)).thenReturn(new GdnRestListResponse<>(null, null, true,
        Collections.singletonList(new SingleObjectResponse<>(Collections.emptyList())), null,
        Constants.DEFAULT_REQUEST_ID));
    variantEditValidationService.validateListOfVariantsL5WithSuccess(Collections.singletonList(productVariantPriceStockAndImagesRequest),
        new HashMap<>(), new HashMap<>(), new ArrayList<>(), new SimpleStringResponse(), true, null);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.VALID_IMAGE_EXTENSION);
    deleteDummyFiles(LOCATION_PATH);
  }

  @Test
  public void validateListOfVariantsL5SellerSkuTest() throws Exception {
    generateDummyImageFiles(LOCATION_PATH);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE))
        .thenReturn(new ProductSystemParameter(Constants.MINIMUM_PRICE, "100", "min-price", false));
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest =
        new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest.setUpcCode(UPC_CODE_UPDATED);
    productVariantPriceStockAndImagesRequest.setSkuCode(SKU_CODE);
    productVariantPriceStockAndImagesRequest.setProductCode(PRODUCT_CODE);
    ProductLevel3SummaryDetailsImageRequest productLevel3SummaryDetailsImageRequest =
        new ProductLevel3SummaryDetailsImageRequest();
    productLevel3SummaryDetailsImageRequest.setMainImage(true);
    productLevel3SummaryDetailsImageRequest.setMarkForDelete(false);
    productLevel3SummaryDetailsImageRequest.setLocationPath(LOCATION_PATH);
    productLevel3SummaryDetailsImageRequest.setSequence(0);
    productVariantPriceStockAndImagesRequest.setImages(
        Collections.singletonList(productLevel3SummaryDetailsImageRequest));
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setPrice(1000.0);
    itemPickupPointRequest.setSalePrice(1000.0);
    itemPickupPointRequest.setWholesalePriceActivated(null);
    itemPickupPointRequest.setItemSku(ITEM_SKU);
    itemPickupPointRequest.setSellerSku(MERCHANT_CODE);
    itemPickupPointRequest.setPickupPointId(PICKUP_POINT_ID);
    productVariantPriceStockAndImagesRequest.setModifiedItemPickupPoints(Collections.singletonList(itemPickupPointRequest));
    Mockito.when(productOutbound.getProductItemBySkuCodes(skuCodesRequest))
        .thenReturn(Collections.singletonList(productItemResponse));
    try {
      variantEditValidationService.validateListOfVariantsL5WithSuccess(Collections.singletonList(productVariantPriceStockAndImagesRequest),
          new HashMap<>(), new HashMap<>(), new ArrayList<>(), new SimpleStringResponse(), true, null);
    } finally {
      Mockito.verify(productSystemParameterService)
          .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
      Mockito.verify(productSystemParameterService)
          .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.VALID_IMAGE_EXTENSION);
      deleteDummyFiles(LOCATION_PATH);
    }
  }

  @Test
  public void validateListOfVariantsL5SellerSkuInValidTest() throws Exception {
    generateDummyImageFiles(LOCATION_PATH);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE))
        .thenReturn(new ProductSystemParameter(Constants.MINIMUM_PRICE, "100", "min-price", false));
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest =
        new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest.setUpcCode(UPC_CODE_UPDATED);
    productVariantPriceStockAndImagesRequest.setSkuCode(SKU_CODE);
    productVariantPriceStockAndImagesRequest.setProductCode(PRODUCT_CODE);
    ProductLevel3SummaryDetailsImageRequest productLevel3SummaryDetailsImageRequest =
        new ProductLevel3SummaryDetailsImageRequest();
    productLevel3SummaryDetailsImageRequest.setMainImage(true);
    productLevel3SummaryDetailsImageRequest.setMarkForDelete(false);
    productLevel3SummaryDetailsImageRequest.setLocationPath(LOCATION_PATH);
    productLevel3SummaryDetailsImageRequest.setSequence(0);
    productVariantPriceStockAndImagesRequest.setImages(
        Collections.singletonList(productLevel3SummaryDetailsImageRequest));
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setPrice(1000.0);
    itemPickupPointRequest.setSalePrice(1000.0);
    itemPickupPointRequest.setWholesalePriceActivated(null);
    itemPickupPointRequest.setItemSku(ITEM_SKU);
    itemPickupPointRequest.setSellerSku(StringUtils.repeat(LOCATION_PATH, 50));
    itemPickupPointRequest.setPickupPointId(PICKUP_POINT_ID);
    productVariantPriceStockAndImagesRequest.setModifiedItemPickupPoints(Collections.singletonList(itemPickupPointRequest));
    Mockito.when(productOutbound.getProductItemBySkuCodes(skuCodesRequest))
        .thenReturn(Collections.singletonList(productItemResponse));
    Mockito.when(pcbFeign.getItemNameByUpcCodeAndProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, UPC_CODE_UPDATED,
        PRODUCT_CODE, SKU_CODE)).thenReturn(new GdnRestListResponse<>(null, null, true,
        Collections.singletonList(new SingleObjectResponse<>(Collections.emptyList())), null,
        Constants.DEFAULT_REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        variantEditValidationService.validateListOfVariantsL5WithSuccess(Collections.singletonList(productVariantPriceStockAndImagesRequest),
            new HashMap<>(), new HashMap<>(), new ArrayList<>(), new SimpleStringResponse(), true, null);
      });
    } finally {
      Mockito.verify(productSystemParameterService)
          .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
      Mockito.verify(productSystemParameterService)
          .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.VALID_IMAGE_EXTENSION);
      deleteDummyFiles(LOCATION_PATH);
    }
  }

  @Test
  public void validateNewL5AdditionRequestsTest() throws Exception {
    ReflectionTestUtils.setField(variantEditValidationService, "validateNewlyAddedItems", true);
    ReflectionTestUtils.setField(variantEditValidationService, "ranchIntegrationEnabled", true);
    ReflectionTestUtils.setField(variantEditValidationService, "relaxDistributionL5AdditionFromBulk", true);
    PickupPointResponse pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_ID);
    pickupPointResponse.setFbbActivated(true);
    pickupPointResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    profileResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    PickupPointResponse pickupPointResponse1 = new PickupPointResponse();
    pickupPointResponse1.setCode(PICKUP_POINT_ID);
    pickupPointResponse1.setFbbActivated(false);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE))
        .thenReturn(new ProductSystemParameter(Constants.MINIMUM_PRICE, "100", "min-price", false));
    Mockito.when(businessPartnerRepository.filterPickupPointsByPickupPointRequest(Mockito.any(PickupPointFilterRequest.class)))
        .thenReturn(Arrays.asList(pickupPointResponse, pickupPointResponse1));
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setSalePrice(1000.0);
    itemPickupPointRequest.setPrice(1000.0);
    itemPickupPointRequest.setPickupPointId(PICKUP_POINT_ID);

    List<ProductVariantPriceStockAndImagesRequest> productVariantPriceStockAndImagesRequests = new ArrayList<>();
    List<ProductVariantPriceStockAndImagesRequest> newlyAddedVariantPriceStockAndImagesRequests = new ArrayList<>();
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest =
        new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest.setModifiedItemPickupPoints(
        Collections.singletonList(itemPickupPointRequest));
    productVariantPriceStockAndImagesRequest.setProductCode(PRODUCT_CODE);
    productVariantPriceStockAndImagesRequests.add(productVariantPriceStockAndImagesRequest);
    newlyAddedVariantPriceStockAndImagesRequests.add(productVariantPriceStockAndImagesRequest);
    productVariantUpdateRequest.setProductItems(productVariantPriceStockAndImagesRequests);
    productVariantUpdateRequest.setOnlyL5Update(true);
    Map<String, Object> flags = new HashMap<>();
    flags.put(Constants.DISTRIBUTION_FLAG_KEY, true);
    pickupPointResponse.setFlags(flags);
    Mockito.when(pickupPointOutbound.getByPickupPointCodes(Constants.DEFAULT_REQUEST_ID, Collections.singletonList(PICKUP_POINT_ID))).thenReturn(Collections.singletonList(pickupPointResponse));
    productVariantUpdateRequest.setAddPickupPoints(Collections.singletonList(itemPickupPointRequest));
    Mockito.when(productInventoryService.isSyncedToLevel1Inventory(profileResponse)).thenReturn(true);
    variantEditValidationService.validateNewL5AdditionRequests(productVariantUpdateRequest, new SimpleStringResponse(),
        profileResponse, newlyAddedVariantPriceStockAndImagesRequests, new HashMap<>());
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
    Mockito.verify(pickupPointOutbound)
        .getByPickupPointCodes(Constants.DEFAULT_REQUEST_ID, Collections.singletonList(PICKUP_POINT_ID));
    Mockito.verify(productInventoryService, Mockito.times(2)).isSyncedToLevel1Inventory(profileResponse);
  }

  @Test
  public void validateNewL5AdditionRequestsDistributionAddedFromBulkTest() throws Exception {
    ReflectionTestUtils.setField(variantEditValidationService, "validateNewlyAddedItems", true);
    ReflectionTestUtils.setField(variantEditValidationService, "ranchIntegrationEnabled", true);
    ReflectionTestUtils.setField(variantEditValidationService, "relaxDistributionL5AdditionFromBulk", false);
    PickupPointResponse pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_ID);
    pickupPointResponse.setFbbActivated(true);
    pickupPointResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    profileResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    PickupPointResponse pickupPointResponse1 = new PickupPointResponse();
    pickupPointResponse1.setCode(PICKUP_POINT_ID);
    pickupPointResponse1.setFbbActivated(false);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE))
        .thenReturn(new ProductSystemParameter(Constants.MINIMUM_PRICE, "100", "min-price", false));
    Mockito.when(businessPartnerRepository.filterPickupPointsByPickupPointRequest(Mockito.any(PickupPointFilterRequest.class)))
        .thenReturn(Arrays.asList(pickupPointResponse, pickupPointResponse1));
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setSalePrice(1000.0);
    itemPickupPointRequest.setPrice(1000.0);
    itemPickupPointRequest.setPickupPointId(PICKUP_POINT_ID);

    List<ProductVariantPriceStockAndImagesRequest> productVariantPriceStockAndImagesRequests = new ArrayList<>();
    List<ProductVariantPriceStockAndImagesRequest> newlyAddedVariantPriceStockAndImagesRequests = new ArrayList<>();
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest =
        new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest.setModifiedItemPickupPoints(
        Collections.singletonList(itemPickupPointRequest));
    productVariantPriceStockAndImagesRequest.setProductCode(PRODUCT_CODE);
    productVariantPriceStockAndImagesRequests.add(productVariantPriceStockAndImagesRequest);
    newlyAddedVariantPriceStockAndImagesRequests.add(productVariantPriceStockAndImagesRequest);
    productVariantUpdateRequest.setProductItems(productVariantPriceStockAndImagesRequests);
    productVariantUpdateRequest.setOnlyL5Update(true);
    Map<String, Object> flags = new HashMap<>();
    flags.put(Constants.DISTRIBUTION_FLAG_KEY, true);
    pickupPointResponse.setFlags(flags);
    Mockito.when(pickupPointOutbound.getByPickupPointCodes(Constants.DEFAULT_REQUEST_ID, Collections.singletonList(PICKUP_POINT_ID))).thenReturn(Collections.singletonList(pickupPointResponse));
    productVariantUpdateRequest.setAddPickupPoints(Collections.singletonList(itemPickupPointRequest));
    Mockito.when(productInventoryService.isSyncedToLevel1Inventory(profileResponse)).thenReturn(true);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      variantEditValidationService.validateNewL5AdditionRequests(productVariantUpdateRequest,
          new SimpleStringResponse(), profileResponse, newlyAddedVariantPriceStockAndImagesRequests, new HashMap<>());
    });
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
    Mockito.verify(pickupPointOutbound)
        .getByPickupPointCodes(Constants.DEFAULT_REQUEST_ID, Collections.singletonList(PICKUP_POINT_ID));
    Mockito.verify(productInventoryService).isSyncedToLevel1Inventory(profileResponse);
  }

  @Test
  public void validateNewL5AdditionRequestsValidateItemSkusTest() throws Exception {
    ReflectionTestUtils.setField(variantEditValidationService, "validateNewlyAddedItems", true);
    ReflectionTestUtils.setField(variantEditValidationService, "validateItemSkuInAddL5Requests", true);
    PickupPointResponse pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_ID);
    pickupPointResponse.setFbbActivated(true);
    pickupPointResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    profileResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    PickupPointResponse pickupPointResponse1 = new PickupPointResponse();
    pickupPointResponse1.setCode(PICKUP_POINT_ID);
    pickupPointResponse1.setFbbActivated(false);
    Mockito.when(
            productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE))
        .thenReturn(new ProductSystemParameter(Constants.MINIMUM_PRICE, "100", "min-price", false));
    Mockito.when(businessPartnerRepository
            .filterPickupPointsByPickupPointRequest(Mockito.any(PickupPointFilterRequest.class)))
        .thenReturn(Arrays.asList(pickupPointResponse, pickupPointResponse1));
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setItemSku(ITEM_SKU);
    itemPickupPointRequest.setSalePrice(1000.0);
    itemPickupPointRequest.setPrice(1000.0);
    itemPickupPointRequest.setPickupPointId(PICKUP_POINT_ID);

    List<ProductVariantPriceStockAndImagesRequest> productVariantPriceStockAndImagesRequests = new ArrayList<>();
    List<ProductVariantPriceStockAndImagesRequest> newlyAddedVariantPriceStockAndImagesRequests = new ArrayList<>();
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest =
        new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest.setModifiedItemPickupPoints(
        Collections.singletonList(itemPickupPointRequest));
    productVariantPriceStockAndImagesRequest.setProductCode(PRODUCT_CODE);
    productVariantPriceStockAndImagesRequests.add(productVariantPriceStockAndImagesRequest);
    newlyAddedVariantPriceStockAndImagesRequests.add(productVariantPriceStockAndImagesRequest);
    productVariantUpdateRequest.setProductItems(productVariantPriceStockAndImagesRequests);
    Mockito.when(pickupPointOutbound
            .getByPickupPointCodes(Constants.DEFAULT_REQUEST_ID, Collections.singletonList(PICKUP_POINT_ID)))
        .thenReturn(Collections.singletonList(pickupPointResponse));
    productVariantUpdateRequest.setAddPickupPoints(Collections.singletonList(itemPickupPointRequest));
    Mockito.when(productInventoryService.isSyncedToLevel1Inventory(profileResponse)).thenReturn(true);
    ItemPickupPointListingResponse itemPickupPointListingResponse = new ItemPickupPointListingResponse();
    itemPickupPointListingResponse.setItemSku(ITEM_SKU);
    HashMap<String, ItemPickupPointListingResponse> itemPickupPointListingResponseHashMap = new HashMap<>();
    itemPickupPointListingResponseHashMap.put(ITEM_SKU, itemPickupPointListingResponse);
    variantEditValidationService.validateNewL5AdditionRequests(productVariantUpdateRequest, new SimpleStringResponse(),
        profileResponse, newlyAddedVariantPriceStockAndImagesRequests, itemPickupPointListingResponseHashMap);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
    Mockito.verify(pickupPointOutbound)
        .getByPickupPointCodes(Constants.DEFAULT_REQUEST_ID, Collections.singletonList(PICKUP_POINT_ID));
    Mockito.verify(productInventoryService, Mockito.times(2)).isSyncedToLevel1Inventory(profileResponse);
  }

  @Test
  public void validateNewL5AdditionRequestsValidateItemSkusFalseTest() throws Exception {
    ReflectionTestUtils.setField(variantEditValidationService, "validateNewlyAddedItems", true);
    ReflectionTestUtils.setField(variantEditValidationService, "validateItemSkuInAddL5Requests", true);
    PickupPointResponse pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_ID);
    pickupPointResponse.setFbbActivated(true);
    pickupPointResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    profileResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    PickupPointResponse pickupPointResponse1 = new PickupPointResponse();
    pickupPointResponse1.setCode(PICKUP_POINT_ID);
    pickupPointResponse1.setFbbActivated(false);
    Mockito.when(
            productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE))
        .thenReturn(new ProductSystemParameter(Constants.MINIMUM_PRICE, "100", "min-price", false));
    Mockito.when(
            businessPartnerRepository.filterPickupPointsByPickupPointRequest(Mockito.any(PickupPointFilterRequest.class)))
        .thenReturn(Arrays.asList(pickupPointResponse, pickupPointResponse1));
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setItemSku(ITEM_SKU);
    itemPickupPointRequest.setSalePrice(1000.0);
    itemPickupPointRequest.setPrice(1000.0);
    itemPickupPointRequest.setPickupPointId(PICKUP_POINT_ID);

    List<ProductVariantPriceStockAndImagesRequest> productVariantPriceStockAndImagesRequests = new ArrayList<>();
    List<ProductVariantPriceStockAndImagesRequest> newlyAddedVariantPriceStockAndImagesRequests = new ArrayList<>();
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest =
        new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest.setModifiedItemPickupPoints(
        Collections.singletonList(itemPickupPointRequest));
    productVariantPriceStockAndImagesRequest.setProductCode(PRODUCT_CODE);
    productVariantPriceStockAndImagesRequests.add(productVariantPriceStockAndImagesRequest);
    newlyAddedVariantPriceStockAndImagesRequests.add(productVariantPriceStockAndImagesRequest);
    productVariantUpdateRequest.setProductItems(productVariantPriceStockAndImagesRequests);
    Mockito.when(pickupPointOutbound.getByPickupPointCodes(Constants.DEFAULT_REQUEST_ID,
        Collections.singletonList(PICKUP_POINT_ID))).thenReturn(Collections.singletonList(pickupPointResponse));
    productVariantUpdateRequest.setAddPickupPoints(Collections.singletonList(itemPickupPointRequest));
    Mockito.when(productInventoryService.isSyncedToLevel1Inventory(profileResponse)).thenReturn(true);
    ItemPickupPointListingResponse itemPickupPointListingResponse = new ItemPickupPointListingResponse();
    itemPickupPointListingResponse.setItemSku(PRODUCT_SKU);
    HashMap<String, ItemPickupPointListingResponse> itemPickupPointListingResponseHashMap = new HashMap<>();
    itemPickupPointListingResponseHashMap.put(ITEM_SKU, itemPickupPointListingResponse);
    try {Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      variantEditValidationService.validateNewL5AdditionRequests(productVariantUpdateRequest,
          new SimpleStringResponse(), profileResponse, newlyAddedVariantPriceStockAndImagesRequests,
          itemPickupPointListingResponseHashMap);
    });
    } finally {
      Mockito.verify(productSystemParameterService)
          .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
      Mockito.verify(pickupPointOutbound)
          .getByPickupPointCodes(Constants.DEFAULT_REQUEST_ID, Collections.singletonList(PICKUP_POINT_ID));
      Mockito.verify(productInventoryService).isSyncedToLevel1Inventory(profileResponse);
    }
  }

  @Test
  public void validateNewL5AdditionRequestsValidateItemSkusFalseFaasSwitchOnFaasSellerTest() throws Exception {
    ReflectionTestUtils.setField(variantEditValidationService, "validateNewlyAddedItems", true);
    ReflectionTestUtils.setField(variantEditValidationService, "validateItemSkuInAddL5Requests", true);
    ReflectionTestUtils.setField(variantEditValidationService, "faasFeatureSwitch", true);
    profileResponse.setFlags(Map.of(FAAS_ACTIVATED, true));
    PickupPointResponse pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_ID);
    pickupPointResponse.setFbbActivated(true);
    pickupPointResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    profileResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    PickupPointResponse pickupPointResponse1 = new PickupPointResponse();
    pickupPointResponse1.setCode(PICKUP_POINT_ID);
    pickupPointResponse1.setFbbActivated(false);
    Mockito.when(
            productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE))
        .thenReturn(new ProductSystemParameter(Constants.MINIMUM_PRICE, "100", "min-price", false));
    Mockito.when(
            businessPartnerRepository.filterPickupPointsByPickupPointRequest(Mockito.any(PickupPointFilterRequest.class)))
        .thenReturn(Arrays.asList(pickupPointResponse, pickupPointResponse1));
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setItemSku(ITEM_SKU);
    itemPickupPointRequest.setSalePrice(1000.0);
    itemPickupPointRequest.setPrice(1000.0);
    itemPickupPointRequest.setPickupPointId(PICKUP_POINT_ID);
    List<ProductVariantPriceStockAndImagesRequest> productVariantPriceStockAndImagesRequests = new ArrayList<>();
    List<ProductVariantPriceStockAndImagesRequest> newlyAddedVariantPriceStockAndImagesRequests = new ArrayList<>();
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest =
        new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest.setModifiedItemPickupPoints(
        Collections.singletonList(itemPickupPointRequest));
    productVariantPriceStockAndImagesRequest.setProductCode(PRODUCT_CODE);
    productVariantPriceStockAndImagesRequests.add(productVariantPriceStockAndImagesRequest);
    newlyAddedVariantPriceStockAndImagesRequests.add(productVariantPriceStockAndImagesRequest);
    productVariantUpdateRequest.setProductItems(productVariantPriceStockAndImagesRequests);
    Mockito.when(pickupPointOutbound.getByPickupPointCodes(Constants.DEFAULT_REQUEST_ID,
        Collections.singletonList(PICKUP_POINT_ID))).thenReturn(Collections.singletonList(pickupPointResponse));
    productVariantUpdateRequest.setAddPickupPoints(Collections.singletonList(itemPickupPointRequest));
    ItemPickupPointListingResponse itemPickupPointListingResponse = new ItemPickupPointListingResponse();
    itemPickupPointListingResponse.setItemSku(PRODUCT_SKU);
    HashMap<String, ItemPickupPointListingResponse> itemPickupPointListingResponseHashMap = new HashMap<>();
    itemPickupPointListingResponseHashMap.put(ITEM_SKU, itemPickupPointListingResponse);
    try {Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      variantEditValidationService.validateNewL5AdditionRequests(productVariantUpdateRequest,
          new SimpleStringResponse(), profileResponse, newlyAddedVariantPriceStockAndImagesRequests,
          itemPickupPointListingResponseHashMap);
    });

    } finally {
      Mockito.verify(productSystemParameterService)
          .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
      Mockito.verify(pickupPointOutbound)
          .getByPickupPointCodes(Constants.DEFAULT_REQUEST_ID, Collections.singletonList(PICKUP_POINT_ID));
    }
  }

  @Test
  public void validateNewL5AdditionRequestsValidateItemSkusFalseSetWaitingDeletionTest() throws Exception {
    ReflectionTestUtils.setField(variantEditValidationService, "validateNewlyAddedItems", true);
    ReflectionTestUtils.setField(variantEditValidationService, "validateItemSkuInAddL5Requests", true);
    ReflectionTestUtils.setField(variantEditValidationService, "setWaitingDeletionForDeletePickupPoint", true);
    PickupPointResponse pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_ID);
    pickupPointResponse.setFbbActivated(true);
    pickupPointResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    profileResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    PickupPointResponse pickupPointResponse1 = new PickupPointResponse();
    pickupPointResponse1.setCode(PICKUP_POINT_ID);
    pickupPointResponse1.setFbbActivated(false);
    Mockito.when(
            productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE))
        .thenReturn(new ProductSystemParameter(Constants.MINIMUM_PRICE, "100", "min-price", false));
    Mockito.when(
            businessPartnerRepository.filterPickupPointsByPickupPointRequest(Mockito.any(PickupPointFilterRequest.class)))
        .thenReturn(Arrays.asList(pickupPointResponse, pickupPointResponse1));
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setItemSku(ITEM_SKU);
    itemPickupPointRequest.setSalePrice(1000.0);
    itemPickupPointRequest.setPrice(1000.0);
    itemPickupPointRequest.setPickupPointId(PICKUP_POINT_ID);
    List<ProductVariantPriceStockAndImagesRequest> productVariantPriceStockAndImagesRequests = new ArrayList<>();
    List<ProductVariantPriceStockAndImagesRequest> newlyAddedVariantPriceStockAndImagesRequests = new ArrayList<>();
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest =
        new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest.setModifiedItemPickupPoints(
        Collections.singletonList(itemPickupPointRequest));
    productVariantPriceStockAndImagesRequest.setProductCode(PRODUCT_CODE);
    productVariantPriceStockAndImagesRequests.add(productVariantPriceStockAndImagesRequest);
    newlyAddedVariantPriceStockAndImagesRequests.add(productVariantPriceStockAndImagesRequest);
    productVariantUpdateRequest.setProductItems(productVariantPriceStockAndImagesRequests);
    Mockito.when(pickupPointOutbound.getByPickupPointCodes(Constants.DEFAULT_REQUEST_ID,
        Collections.singletonList(PICKUP_POINT_ID))).thenReturn(Collections.singletonList(pickupPointResponse));
    productVariantUpdateRequest.setAddPickupPoints(Collections.singletonList(itemPickupPointRequest));
    Mockito.when(productInventoryService.isSyncedToLevel1Inventory(profileResponse)).thenReturn(true);
    ItemPickupPointListingResponse itemPickupPointListingResponse = new ItemPickupPointListingResponse();
    itemPickupPointListingResponse.setItemSku(PRODUCT_SKU);
    HashMap<String, ItemPickupPointListingResponse> itemPickupPointListingResponseHashMap = new HashMap<>();
    itemPickupPointListingResponseHashMap.put(ITEM_SKU, itemPickupPointListingResponse);
        try {
          Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
            variantEditValidationService.validateNewL5AdditionRequests(productVariantUpdateRequest,
                new SimpleStringResponse(), profileResponse, newlyAddedVariantPriceStockAndImagesRequests,
                itemPickupPointListingResponseHashMap);
          });
    } finally {
      Mockito.verify(productSystemParameterService)
          .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
      Mockito.verify(pickupPointOutbound)
          .getByPickupPointCodes(Constants.DEFAULT_REQUEST_ID, Collections.singletonList(PICKUP_POINT_ID));
      Mockito.verify(productInventoryService).isSyncedToLevel1Inventory(profileResponse);
    }
  }

  @Test
  public void validateNewL5AdditionRequestsPriceNull2Test() throws Exception {
    PickupPointResponse pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_ID);
    pickupPointResponse.setFbbActivated(true);
    PickupPointResponse pickupPointResponse1 = new PickupPointResponse();
    pickupPointResponse1.setCode(PICKUP_POINT_ID);
    pickupPointResponse1.setFbbActivated(false);
    Mockito.when(
            productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE))
        .thenReturn(new ProductSystemParameter(Constants.MINIMUM_PRICE, "100", "min-price", false));
    Mockito.when(
            businessPartnerRepository.filterPickupPointsByPickupPointRequest(Mockito.any(PickupPointFilterRequest.class)))
        .thenReturn(Arrays.asList(pickupPointResponse, pickupPointResponse1));
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setSalePrice(1000.0);
    itemPickupPointRequest.setPrice(1.0);
    itemPickupPointRequest.setPickupPointId(PICKUP_POINT_ID);

    List<ProductVariantPriceStockAndImagesRequest> productVariantPriceStockAndImagesRequests = new ArrayList<>();
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest =
        new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest.setModifiedItemPickupPoints(
        Collections.singletonList(itemPickupPointRequest));
    productVariantPriceStockAndImagesRequest.setProductCode(PRODUCT_CODE);
    productVariantPriceStockAndImagesRequests.add(productVariantPriceStockAndImagesRequest);
    productVariantUpdateRequest.setProductItems(productVariantPriceStockAndImagesRequests);
    Mockito.when(pickupPointOutbound.getByPickupPointCodes(Constants.DEFAULT_REQUEST_ID,
        Collections.singletonList(PICKUP_POINT_ID))).thenReturn(Collections.singletonList(pickupPointResponse));
    productVariantUpdateRequest.setAddPickupPoints(Collections.singletonList(itemPickupPointRequest));
    Mockito.when(productInventoryService.isSyncedToLevel1Inventory(profileResponse)).thenReturn(true);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        ;variantEditValidationService.validateNewL5AdditionRequests(productVariantUpdateRequest,
            new SimpleStringResponse(), profileResponse, new ArrayList<>(), new HashMap<>());
      });
    } finally {
      Mockito.verify(productSystemParameterService)
          .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
    }
  }

  @Test
  public void validateNewL5AdditionRequestsFbbActivatedTest() throws Exception {
    PickupPointResponse pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_ID);
    pickupPointResponse.setFbbActivated(false);
    pickupPointResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    profileResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    Mockito.when(
            productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE))
        .thenReturn(new ProductSystemParameter(Constants.MINIMUM_PRICE, "100", "min-price", false));
    Mockito.when(businessPartnerRepository.filterPickupPointsByPickupPointRequest(Mockito.any(PickupPointFilterRequest.class)))
        .thenReturn(List.of(pickupPointResponse));
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setSalePrice(1000.0);
    itemPickupPointRequest.setPrice(1000.0);
    itemPickupPointRequest.setPickupPointId(PICKUP_POINT_ID);

    List<ProductVariantPriceStockAndImagesRequest> productVariantPriceStockAndImagesRequests = new ArrayList<>();
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest =
        new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest.setModifiedItemPickupPoints(
        Collections.singletonList(itemPickupPointRequest));
    productVariantPriceStockAndImagesRequest.setProductCode(PRODUCT_CODE);
    productVariantPriceStockAndImagesRequests.add(productVariantPriceStockAndImagesRequest);
    productVariantUpdateRequest.setProductItems(productVariantPriceStockAndImagesRequests);
    Mockito.when(pickupPointOutbound.getByPickupPointCodes(Constants.DEFAULT_REQUEST_ID, Collections.singletonList(PICKUP_POINT_ID))).thenReturn(Collections.singletonList(pickupPointResponse));
    productVariantUpdateRequest.setAddPickupPoints(Collections.singletonList(itemPickupPointRequest));
    variantEditValidationService.validateNewL5AdditionRequests(productVariantUpdateRequest, new SimpleStringResponse(),
        profileResponse, new ArrayList<>(), new HashMap<>());
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
    Mockito.verify(pickupPointOutbound)
        .getByPickupPointCodes(Constants.DEFAULT_REQUEST_ID, Collections.singletonList(PICKUP_POINT_ID));
    Mockito.verify(productInventoryService).isSyncedToLevel1Inventory(profileResponse);
  }


  @Test
  public void validateNewL5AdditionRequestsFbbActivatedAndPickupPointValidationFalseTest() throws Exception {
    try {
      PickupPointResponse pickupPointResponse = new PickupPointResponse();
      pickupPointResponse.setCode(PICKUP_POINT_ID);
      pickupPointResponse.setFbbActivated(false);
      pickupPointResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
      profileResponse.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
      Mockito.when(productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE))
          .thenReturn(new ProductSystemParameter(Constants.MINIMUM_PRICE, "100", "min-price", false));
      Mockito.when(businessPartnerRepository.filterPickupPointsByPickupPointRequest(Mockito.any(PickupPointFilterRequest.class)))
          .thenReturn(List.of(pickupPointResponse));
      ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
      itemPickupPointRequest.setSalePrice(1000.0);
      itemPickupPointRequest.setPrice(1000.0);
      itemPickupPointRequest.setPickupPointId(PICKUP_POINT_ID);

      List<ProductVariantPriceStockAndImagesRequest> productVariantPriceStockAndImagesRequests = new ArrayList<>();
      ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest = new ProductVariantPriceStockAndImagesRequest();
      productVariantPriceStockAndImagesRequest.setModifiedItemPickupPoints(Collections.singletonList(itemPickupPointRequest));
      productVariantPriceStockAndImagesRequest.setProductCode(PRODUCT_CODE);
      productVariantPriceStockAndImagesRequests.add(productVariantPriceStockAndImagesRequest);
      productVariantUpdateRequest.setProductItems(productVariantPriceStockAndImagesRequests);
      Mockito.when(pickupPointOutbound.getByPickupPointCodes(Constants.DEFAULT_REQUEST_ID, Collections.singletonList(PICKUP_POINT_ID))).thenReturn(Collections.singletonList(pickupPointResponse));
      productVariantUpdateRequest.setAddPickupPoints(Collections.singletonList(itemPickupPointRequest));
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        variantEditValidationService.validateNewL5AdditionRequests(productVariantUpdateRequest, new SimpleStringResponse(),
            profileResponse, new ArrayList<>(), new HashMap<>());
      });
    } finally {
      Mockito.verify(productSystemParameterService)
          .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
      Mockito.verify(pickupPointOutbound)
          .getByPickupPointCodes(Constants.DEFAULT_REQUEST_ID, Collections.singletonList(PICKUP_POINT_ID));
    }
  }

  @Test
  public void validateNewL5AdditionRequestsFbbActivatedTestInvSwitchTrue() throws Exception {
    ReflectionTestUtils.setField(variantEditValidationService, "inventoryL5SyncStockEnabled", true);
    ReflectionTestUtils.setField(variantEditValidationService, "ranchIntegrationEnabled", true);
    PickupPointResponse pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_ID);
    pickupPointResponse.setFbbActivated(false);
    pickupPointResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    profileResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    Mockito.when(
            productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE))
        .thenReturn(new ProductSystemParameter(Constants.MINIMUM_PRICE, "100", "min-price", false));
    Mockito.when(businessPartnerRepository.filterPickupPointsByPickupPointRequest(Mockito.any(PickupPointFilterRequest.class)))
        .thenReturn(List.of(pickupPointResponse));
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setSalePrice(1000.0);
    itemPickupPointRequest.setPrice(1000.0);
    itemPickupPointRequest.setPickupPointId(PICKUP_POINT_ID);

    List<ProductVariantPriceStockAndImagesRequest> productVariantPriceStockAndImagesRequests = new ArrayList<>();
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest =
        new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest.setModifiedItemPickupPoints(
        Collections.singletonList(itemPickupPointRequest));
    productVariantPriceStockAndImagesRequest.setProductCode(PRODUCT_CODE);
    productVariantPriceStockAndImagesRequests.add(productVariantPriceStockAndImagesRequest);
    productVariantUpdateRequest.setProductItems(productVariantPriceStockAndImagesRequests);
    Mockito.when(pickupPointOutbound.getByPickupPointCodes(Constants.DEFAULT_REQUEST_ID, Collections.singletonList(PICKUP_POINT_ID))).thenReturn(Collections.singletonList(pickupPointResponse));
    productVariantUpdateRequest.setAddPickupPoints(Collections.singletonList(itemPickupPointRequest));
    variantEditValidationService.validateNewL5AdditionRequests(productVariantUpdateRequest, new SimpleStringResponse(),
        profileResponse, new ArrayList<>(), new HashMap<>());
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
    Mockito.verify(pickupPointOutbound)
        .getByPickupPointCodes(Constants.DEFAULT_REQUEST_ID, Collections.singletonList(PICKUP_POINT_ID));
  }

  @Test
  public void validateNewL5AdditionRequestsFbbActivatedTrueTestInvSwitchTrue() throws Exception {
    ReflectionTestUtils.setField(variantEditValidationService, "inventoryL5SyncStockEnabled", true);
    ReflectionTestUtils.setField(variantEditValidationService, "ranchIntegrationEnabled", true);
    PickupPointResponse pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_ID);
    pickupPointResponse.setFbbActivated(true);
    pickupPointResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    profileResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    Mockito.when(
            productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE))
        .thenReturn(new ProductSystemParameter(Constants.MINIMUM_PRICE, "100", "min-price", false));
    Mockito.when(businessPartnerRepository.filterPickupPointsByPickupPointRequest(Mockito.any(PickupPointFilterRequest.class)))
        .thenReturn(List.of(pickupPointResponse));
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setSalePrice(1000.0);
    itemPickupPointRequest.setPrice(1000.0);
    itemPickupPointRequest.setPickupPointId(PICKUP_POINT_ID);

    List<ProductVariantPriceStockAndImagesRequest> productVariantPriceStockAndImagesRequests = new ArrayList<>();
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest =
        new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest.setModifiedItemPickupPoints(
        Collections.singletonList(itemPickupPointRequest));
    productVariantPriceStockAndImagesRequest.setProductCode(PRODUCT_CODE);
    productVariantPriceStockAndImagesRequests.add(productVariantPriceStockAndImagesRequest);
    productVariantUpdateRequest.setProductItems(productVariantPriceStockAndImagesRequests);
    productVariantUpdateRequest.setOnlyL5Update(true);
    Mockito.when(pickupPointOutbound.getByPickupPointCodes(Constants.DEFAULT_REQUEST_ID,
        Collections.singletonList(PICKUP_POINT_ID))).thenReturn(Collections.singletonList(pickupPointResponse));
    productVariantUpdateRequest.setAddPickupPoints(Collections.singletonList(itemPickupPointRequest));
    variantEditValidationService.validateNewL5AdditionRequests(productVariantUpdateRequest, new SimpleStringResponse(),
        profileResponse, new ArrayList<>(), new HashMap<>());
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
    Mockito.verify(pickupPointOutbound)
        .getByPickupPointCodes(Constants.DEFAULT_REQUEST_ID, Collections.singletonList(PICKUP_POINT_ID));
    Mockito.verify(productInventoryService).isSyncedToLevel1Inventory(profileResponse);
  }

  @Test
  public void validateNewL5AdditionRequestsFbbActivatedTrueTestInvSwitchTrue2() throws Exception {
    ReflectionTestUtils.setField(variantEditValidationService, "inventoryL5SyncStockEnabled", false);
    PickupPointResponse pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_ID);
    pickupPointResponse.setFbbActivated(true);
    pickupPointResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    profileResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    Mockito.when(
            productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE))
        .thenReturn(new ProductSystemParameter(Constants.MINIMUM_PRICE, "100", "min-price", false));
    Mockito.when(businessPartnerRepository.filterPickupPointsByPickupPointRequest(Mockito.any(PickupPointFilterRequest.class)))
        .thenReturn(List.of(pickupPointResponse));
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setSalePrice(1000.0);
    itemPickupPointRequest.setPrice(1000.0);
    itemPickupPointRequest.setPickupPointId(PICKUP_POINT_ID);

    List<ProductVariantPriceStockAndImagesRequest> productVariantPriceStockAndImagesRequests = new ArrayList<>();
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest =
        new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest.setModifiedItemPickupPoints(
        Collections.singletonList(itemPickupPointRequest));
    productVariantPriceStockAndImagesRequest.setProductCode(PRODUCT_CODE);
    productVariantPriceStockAndImagesRequests.add(productVariantPriceStockAndImagesRequest);
    productVariantUpdateRequest.setProductItems(productVariantPriceStockAndImagesRequests);
    Mockito.when(productInventoryService.isSyncedToLevel1Inventory(profileResponse)).thenReturn(false);
    Mockito.when(pickupPointOutbound.getByPickupPointCodes(Constants.DEFAULT_REQUEST_ID, Collections.singletonList(PICKUP_POINT_ID))).thenReturn(Collections.singletonList(pickupPointResponse));
    productVariantUpdateRequest.setAddPickupPoints(Collections.singletonList(itemPickupPointRequest));
    variantEditValidationService.validateNewL5AdditionRequests(productVariantUpdateRequest, new SimpleStringResponse(),
        profileResponse, new ArrayList<>(), new HashMap<>());
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
    Mockito.verify(pickupPointOutbound)
        .getByPickupPointCodes(Constants.DEFAULT_REQUEST_ID, Collections.singletonList(PICKUP_POINT_ID));
    Mockito.verify(productInventoryService).isSyncedToLevel1Inventory(profileResponse);
  }

  @Test
  public void validateNewL5AdditionRequestsEmptyTest() throws Exception {
    productVariantUpdateRequest.setAddPickupPoints(new ArrayList<>());
    productVariantUpdateRequest.setProductItems(new ArrayList<>());
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest =
        new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest.setModifiedItemPickupPoints(new ArrayList<>());
    variantEditValidationService.validateNewL5AdditionRequests(productVariantUpdateRequest, new SimpleStringResponse(),
        profileResponse, new ArrayList<>(), new HashMap<>());
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
    Mockito.verify(pickupPointOutbound).getByPickupPointCodes(Constants.DEFAULT_REQUEST_ID, new ArrayList<>());
  }

  @Test
  public void validateNewL5AdditionRequestsExceptionTest() throws Exception {
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE))
        .thenReturn(new ProductSystemParameter(Constants.MINIMUM_PRICE, "100", "min-price", false));
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setSalePrice(1000.0);
    itemPickupPointRequest.setPrice(1000.0);
    itemPickupPointRequest.setPickupPointId(PICKUP_POINT_ID);
    productVariantUpdateRequest.setAddPickupPoints(Collections.singletonList(itemPickupPointRequest));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        variantEditValidationService
            .validateNewL5AdditionRequests(productVariantUpdateRequest, new SimpleStringResponse(), profileResponse,
                new ArrayList<>(), new HashMap<>());
      });
    } finally {
      Mockito.verify(productSystemParameterService)
          .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
      Mockito.verify(pickupPointOutbound)
          .getByPickupPointCodes(Constants.DEFAULT_REQUEST_ID, Collections.singletonList(PICKUP_POINT_ID));
    }
  }

  @Test
  public void validateNewL5AdditionRequestsPriceNullTest() throws Exception {
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE))
        .thenReturn(new ProductSystemParameter(Constants.MINIMUM_PRICE, "100", "min-price", false));
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setPrice(null);
    itemPickupPointRequest.setSalePrice(1000.0);
    productVariantUpdateRequest.setAddPickupPoints(Collections.singletonList(itemPickupPointRequest));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        variantEditValidationService.validateNewL5AdditionRequests(productVariantUpdateRequest,
            new SimpleStringResponse(), profileResponse, new ArrayList<>(), new HashMap<>());
      });
    } finally {
      Mockito.verify(productSystemParameterService)
          .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
    }
  }


  @Test
  public void validateNewL5AdditionRequestsSalePriceNullTest() throws Exception {
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE))
        .thenReturn(new ProductSystemParameter(Constants.MINIMUM_PRICE, "100", "min-price", false));
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setSalePrice(null);
    itemPickupPointRequest.setPrice(1000.0);
    productVariantUpdateRequest.setAddPickupPoints(Collections.singletonList(itemPickupPointRequest));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        variantEditValidationService
            .validateNewL5AdditionRequests(productVariantUpdateRequest, new SimpleStringResponse(), profileResponse,
                new ArrayList<>(), new HashMap<>());
      });
    } finally {
      Mockito.verify(productSystemParameterService)
          .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
    }
  }

  @Test
  public void validateListOfVariantsL5ForMultiUsedProductTest() throws Exception {
    generateDummyImageFiles(LOCATION_PATH);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE))
        .thenReturn(new ProductSystemParameter(Constants.MINIMUM_PRICE, "100", "min-price", false));
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest =
        new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest.setUpcCode(UPC_CODE_UPDATED);
    productVariantPriceStockAndImagesRequest.setSkuCode(SKU_CODE);
    productVariantPriceStockAndImagesRequest.setProductCode(PRODUCT_CODE);
    ProductLevel3SummaryDetailsImageRequest productLevel3SummaryDetailsImageRequest =
        new ProductLevel3SummaryDetailsImageRequest();
    productLevel3SummaryDetailsImageRequest.setMainImage(true);
    productLevel3SummaryDetailsImageRequest.setMarkForDelete(false);
    productLevel3SummaryDetailsImageRequest.setLocationPath(LOCATION_PATH);
    productLevel3SummaryDetailsImageRequest.setSequence(0);
    productVariantPriceStockAndImagesRequest.setImages(Collections.singletonList(productLevel3SummaryDetailsImageRequest));
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setPrice(1000.0);
    itemPickupPointRequest.setSalePrice(1000.0);
    itemPickupPointRequest.setWholesalePriceActivated(null);
    itemPickupPointRequest.setItemSku(ITEM_SKU);
    itemPickupPointRequest.setSellerSku(MERCHANT_CODE);
    itemPickupPointRequest.setPickupPointId(PICKUP_POINT_ID);
    productVariantPriceStockAndImagesRequest.setModifiedItemPickupPoints(Collections.singletonList(itemPickupPointRequest));
    Mockito.when(productOutbound.getProductItemBySkuCodes(skuCodesRequest))
        .thenReturn(Collections.singletonList(productItemResponse));
    Mockito.when(pcbFeign.getItemNameByUpcCodeAndProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, UPC_CODE_UPDATED,
        PRODUCT_CODE, SKU_CODE)).thenReturn(new GdnRestListResponse<>(null, null, true,
        Collections.singletonList(new SingleObjectResponse<>(Collections.emptyList())), null,
        Constants.DEFAULT_REQUEST_ID));
    variantEditValidationService.validateListOfVariantsL5ForMultiUsedProduct(Collections.singletonList(productVariantPriceStockAndImagesRequest),
        new HashMap<>(), new HashMap<>(), new ArrayList<>(), new SimpleStringResponse(), true, null);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
    deleteDummyFiles(LOCATION_PATH);
  }

  @Test
  public void validateListOfVariantsL5ForMultiUsedProductNonMppTest() throws Exception {
    generateDummyImageFiles(LOCATION_PATH);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE))
        .thenReturn(new ProductSystemParameter(Constants.MINIMUM_PRICE, "100", "min-price", false));
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest =
        new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest.setUpcCode(UPC_CODE_UPDATED);
    productVariantPriceStockAndImagesRequest.setSkuCode(SKU_CODE);
    productVariantPriceStockAndImagesRequest.setProductCode(PRODUCT_CODE);
    ProductLevel3SummaryDetailsImageRequest productLevel3SummaryDetailsImageRequest =
        new ProductLevel3SummaryDetailsImageRequest();
    productLevel3SummaryDetailsImageRequest.setMainImage(true);
    productLevel3SummaryDetailsImageRequest.setMarkForDelete(false);
    productLevel3SummaryDetailsImageRequest.setLocationPath(LOCATION_PATH);
    productLevel3SummaryDetailsImageRequest.setSequence(0);
    productVariantPriceStockAndImagesRequest.setImages(Collections.singletonList(productLevel3SummaryDetailsImageRequest));
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setPrice(1000.0);
    itemPickupPointRequest.setSalePrice(1000.0);
    itemPickupPointRequest.setWholesalePriceActivated(null);
    itemPickupPointRequest.setItemSku(ITEM_SKU);
    itemPickupPointRequest.setSellerSku(MERCHANT_CODE);
    itemPickupPointRequest.setPickupPointId(PICKUP_POINT_ID);
    productVariantPriceStockAndImagesRequest.setModifiedItemPickupPoints(Collections.singletonList(itemPickupPointRequest));
    Mockito.when(productOutbound.getProductItemBySkuCodes(skuCodesRequest))
        .thenReturn(Collections.singletonList(productItemResponse));
    Mockito.when(pcbFeign.getItemNameByUpcCodeAndProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, UPC_CODE_UPDATED,
        PRODUCT_CODE, SKU_CODE)).thenReturn(new GdnRestListResponse<>(null, null, true,
        Collections.singletonList(new SingleObjectResponse<>(Collections.emptyList())), null,
        Constants.DEFAULT_REQUEST_ID));
    variantEditValidationService.validateListOfVariantsL5ForMultiUsedProduct(Collections.singletonList(productVariantPriceStockAndImagesRequest),
        new HashMap<>(), new HashMap<>(), new ArrayList<>(), new SimpleStringResponse(), false, null);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
    deleteDummyFiles(LOCATION_PATH);
  }

  @Test
  public void validateUpcCodeL5LevelTest() {
    GdnRestListResponse<SingleObjectResponse<List<String>>> response =
        new GdnRestListResponse<>(null, null, true, new ArrayList<>(), new PageMetaData(1, 0, 1), REQUEST_ID);
    Map<String, String> skuCodeToUpcCodeMap = new HashMap<>();
    skuCodeToUpcCodeMap.put(SKU_CODE, UPC_CODE);
    ProductPriceStockAndImagesRequest productPriceStockAndImagesRequest = new ProductPriceStockAndImagesRequest();
    productPriceStockAndImagesRequest.setItemSku(ITEM_SKU);
    productPriceStockAndImagesRequest.setProductCode(PRODUCT_CODE);
    productPriceStockAndImagesRequest.setSkuCode(SKU_CODE);
    productPriceStockAndImagesRequest.setUpcCode(UPC_CODE_UPDATED);
    Mockito.when(pcbFeign.getItemNameByUpcCodeAndProductCode(any(), any(), any(), any(), any(), any(), any(), any()))
        .thenReturn(new GdnRestListResponse<>(null, null, true, Arrays.asList(new SingleObjectResponse<>()), null,
            Constants.DEFAULT_REQUEST_ID));
    variantEditValidationService.validateUpcCodeL5Level(skuCodeToUpcCodeMap, new SimpleStringResponse(),
        productPriceStockAndImagesRequest);
    Mockito.verify(pcbFeign)
        .getItemNameByUpcCodeAndProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, UPC_CODE_UPDATED,
            PRODUCT_CODE, SKU_CODE);
  }

  @Test
  public void validateUpcCodeL5LevelNoChangeTest() {
    GdnRestListResponse<SingleObjectResponse<List<String>>> response =
        new GdnRestListResponse<>(null, null, true, new ArrayList<>(), new PageMetaData(1, 0, 1), REQUEST_ID);
    Map<String, String> skuCodeToUpcCodeMap = new HashMap<>();
    skuCodeToUpcCodeMap.put(SKU_CODE, UPC_CODE);
    ProductPriceStockAndImagesRequest productPriceStockAndImagesRequest = new ProductPriceStockAndImagesRequest();
    productPriceStockAndImagesRequest.setItemSku(ITEM_SKU);
    productPriceStockAndImagesRequest.setProductCode(PRODUCT_CODE);
    productPriceStockAndImagesRequest.setSkuCode(SKU_CODE);
    productPriceStockAndImagesRequest.setUpcCode(UPC_CODE);
    Mockito.when(pcbFeign.getItemNameByUpcCodeAndProductCode(any(), any(), any(), any(), any(), any(), any(), any()))
        .thenReturn(new GdnRestListResponse<>(null, null, true, Arrays.asList(new SingleObjectResponse<>()), null,
            Constants.DEFAULT_REQUEST_ID));
    variantEditValidationService.validateUpcCodeL5Level(skuCodeToUpcCodeMap, new SimpleStringResponse(),
        productPriceStockAndImagesRequest);
  }

  @Test
  public void validateUpcCodeL5LevelMapTest() {
    GdnRestListResponse<SingleObjectResponse<List<String>>> response =
        new GdnRestListResponse<>(null, null, true, new ArrayList<>(), new PageMetaData(1, 0, 1), REQUEST_ID);
    Map<String, String> skuCodeToUpcCodeMap = new HashMap<>();
    skuCodeToUpcCodeMap.put(ITEM_SKU, UPC_CODE);
    ProductPriceStockAndImagesRequest productPriceStockAndImagesRequest = new ProductPriceStockAndImagesRequest();
    productPriceStockAndImagesRequest.setItemSku(ITEM_SKU);
    productPriceStockAndImagesRequest.setProductCode(PRODUCT_CODE);
    productPriceStockAndImagesRequest.setSkuCode(SKU_CODE);
    productPriceStockAndImagesRequest.setUpcCode(UPC_CODE_UPDATED);
    Mockito.when(pcbFeign.getItemNameByUpcCodeAndProductCode(any(), any(), any(), any(), any(), any(), any(), any()))
        .thenReturn(new GdnRestListResponse<>(null, null, true, Arrays.asList(new SingleObjectResponse<>()), null,
            Constants.DEFAULT_REQUEST_ID));
    variantEditValidationService.validateUpcCodeL5Level(skuCodeToUpcCodeMap, new SimpleStringResponse(),
        productPriceStockAndImagesRequest);
  }

  @Test
  public void validateUpcCodeL5LevelEmptyUpcTest() {
    GdnRestListResponse<SingleObjectResponse<List<String>>> response =
        new GdnRestListResponse<>(null, null, true, new ArrayList<>(), new PageMetaData(1, 0, 1), REQUEST_ID);
    Map<String, String> skuCodeToUpcCodeMap = new HashMap<>();
    skuCodeToUpcCodeMap.put(SKU_CODE, UPC_CODE);
    ProductPriceStockAndImagesRequest productPriceStockAndImagesRequest = new ProductPriceStockAndImagesRequest();
    productPriceStockAndImagesRequest.setItemSku(ITEM_SKU);
    productPriceStockAndImagesRequest.setProductCode(PRODUCT_CODE);
    productPriceStockAndImagesRequest.setSkuCode(SKU_CODE);
    productPriceStockAndImagesRequest.setUpcCode(StringUtils.EMPTY);
    Mockito.when(pcbFeign.getItemNameByUpcCodeAndProductCode(any(), any(), any(), any(), any(), any(), any(), any()))
        .thenReturn(new GdnRestListResponse<>(null, null, true, Arrays.asList(new SingleObjectResponse<>()), null,
            Constants.DEFAULT_REQUEST_ID));
    variantEditValidationService.validateUpcCodeL5Level(skuCodeToUpcCodeMap, new SimpleStringResponse(),
        productPriceStockAndImagesRequest);
  }

  @Test
  public void validateUpcCodeL5LevelExceptionTest() {
    GdnRestListResponse<SingleObjectResponse<List<String>>> response =
        new GdnRestListResponse<>(null, null, true, new ArrayList<>(), new PageMetaData(1, 0, 1), REQUEST_ID);
    Map<String, String> skuCodeToUpcCodeMap = new HashMap<>();
    skuCodeToUpcCodeMap.put(SKU_CODE, UPC_CODE);
    ProductPriceStockAndImagesRequest productPriceStockAndImagesRequest = new ProductPriceStockAndImagesRequest();
    productPriceStockAndImagesRequest.setItemSku(ITEM_SKU);
    productPriceStockAndImagesRequest.setProductCode(PRODUCT_CODE);
    productPriceStockAndImagesRequest.setSkuCode(SKU_CODE);
    productPriceStockAndImagesRequest.setUpcCode(UPC_CODE_UPDATED);
    Mockito.when(pcbFeign.getItemNameByUpcCodeAndProductCode(any(), any(), any(), any(), any(), any(), any(), any()))
        .thenReturn(new GdnRestListResponse<>(null, null, true,
            Arrays.asList(new SingleObjectResponse<>(Arrays.asList(ITEM_NAME))), null, Constants.DEFAULT_REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        variantEditValidationService.validateUpcCodeL5Level(skuCodeToUpcCodeMap, new SimpleStringResponse(),
            productPriceStockAndImagesRequest);
      });
    } finally {
      Mockito.verify(pcbFeign)
          .getItemNameByUpcCodeAndProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
              Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, UPC_CODE_UPDATED,
              PRODUCT_CODE, SKU_CODE);
    }
  }

  @Test
  public void validateAndSetFbbActiveFlagAtL5Test() throws Exception {
    ReflectionTestUtils.setField(variantEditValidationService, "setWaitingDeletionForDeletePickupPoint", true);
    ItemPickupPointRequest modifiedItemPickupPoint = new ItemPickupPointRequest();
    modifiedItemPickupPoint.setPickupPointId(PP_CODE);
    PickupPointResponse pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PP_CODE);
    pickupPointResponse.setFbbActivated(true);
    PickupPointResponse pickupPointResponse1 = new PickupPointResponse();
    pickupPointResponse1.setCode(PP_CODE_1);
    pickupPointResponse1.setFbbActivated(false);
    PickupPointFilterRequest pickupPointFilterRequest = new PickupPointFilterRequest();
    pickupPointFilterRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    pickupPointFilterRequest.setCodes(new HashSet<>(Collections.singletonList(PP_CODE)));
    pickupPointFilterRequest.setFbbActivated(true);
    pickupPointFilterRequest.setWaitingDeletion(false);
    Mockito.when(businessPartnerRepository.filterPickupPointsByPickupPointRequest(pickupPointFilterRequest)).thenReturn(Arrays.asList(pickupPointResponse, pickupPointResponse1));
    Mockito.when(productInventoryService.isSyncedToLevel1Inventory(new ProfileResponse())).thenReturn(false);
    productVariantUpdateRequest.setAddPickupPoints(Arrays.asList(modifiedItemPickupPoint));
    variantEditValidationService.validateAndSetFbbActiveFlagAtL5(productVariantUpdateRequest, BUSINESS_PARTNER_CODE,
        null);
    Mockito.verify(businessPartnerRepository).filterPickupPointsByPickupPointRequest(pickupPointFilterRequest);
    Mockito.verify(productInventoryService).isSyncedToLevel1Inventory(null);
  }

  @Test
  public void validateAndSetFbbActiveFlagAtL5FalseTest() throws Exception {
    ReflectionTestUtils.setField(variantEditValidationService, "setWaitingDeletionForDeletePickupPoint", true);
    ItemPickupPointRequest modifiedItemPickupPoint = new ItemPickupPointRequest();
    modifiedItemPickupPoint.setPickupPointId(PP_CODE);
    modifiedItemPickupPoint.setBuyable(true);
    PickupPointResponse pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PP_CODE_1);
    pickupPointResponse.setFbbActivated(true);
    PickupPointResponse pickupPointResponse1 = new PickupPointResponse();
    pickupPointResponse1.setCode(PP_CODE_1);
    pickupPointResponse1.setFbbActivated(true);
    PickupPointFilterRequest pickupPointFilterRequest = new PickupPointFilterRequest();
    pickupPointFilterRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    pickupPointFilterRequest.setCodes(new HashSet<>(Collections.singletonList(PP_CODE)));
    pickupPointFilterRequest.setFbbActivated(true);
    Mockito.when(businessPartnerRepository.filterPickupPointsByPickupPointRequest(pickupPointFilterRequest)).thenReturn(Arrays.asList(pickupPointResponse, pickupPointResponse1));
    ProfileResponse profileResponse1 = new ProfileResponse();
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setSalesChannel(Arrays.asList(Constants.B2C_SELLER_CHANNEL));
    profileResponse1.setCompany(companyDTO);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse1);
    Mockito.when(productInventoryService.isSyncedToLevel1Inventory(profileResponse1)).thenReturn(true);
    productVariantUpdateRequest.setAddPickupPoints(Arrays.asList(modifiedItemPickupPoint));
    List<ProductVariantPriceStockAndImagesRequest> productItems = new ArrayList<>();
    ProductVariantPriceStockAndImagesRequest itemPickupPoints = new ProductVariantPriceStockAndImagesRequest();
    modifiedItemPickupPoint.setBuyable(true);
    pickupPointFilterRequest.setWaitingDeletion(false);
    itemPickupPoints.setModifiedItemPickupPoints(Arrays.asList(modifiedItemPickupPoint));
    productItems.add(itemPickupPoints);
    productVariantUpdateRequest.setProductItems(productItems);
    productVariantUpdateRequest.setB2cActivated(false);
    variantEditValidationService.validateAndSetFbbActiveFlagAtL5(productVariantUpdateRequest, BUSINESS_PARTNER_CODE,
        profileResponse1);
    Mockito.verify(businessPartnerRepository).filterPickupPointsByPickupPointRequest(pickupPointFilterRequest);
    Mockito.verify(productInventoryService).isSyncedToLevel1Inventory(profileResponse1);
  }

  @Test
  public void validateAndSetFbbActiveFlagAtL5FalseDisplayableTrueTest() throws Exception {
    ReflectionTestUtils.setField(variantEditValidationService, "setWaitingDeletionForDeletePickupPoint", true);
    ItemPickupPointRequest modifiedItemPickupPoint = new ItemPickupPointRequest();
    modifiedItemPickupPoint.setPickupPointId(PP_CODE);
    modifiedItemPickupPoint.setBuyable(false);
    modifiedItemPickupPoint.setDisplay(true);
    PickupPointResponse pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PP_CODE_1);
    pickupPointResponse.setFbbActivated(true);
    PickupPointResponse pickupPointResponse1 = new PickupPointResponse();
    pickupPointResponse1.setCode(PP_CODE_1);
    pickupPointResponse1.setFbbActivated(true);
    PickupPointFilterRequest pickupPointFilterRequest = new PickupPointFilterRequest();
    pickupPointFilterRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    pickupPointFilterRequest.setCodes(new HashSet<>(Collections.singletonList(PP_CODE)));
    pickupPointFilterRequest.setFbbActivated(true);
    pickupPointFilterRequest.setWaitingDeletion(false);
    Mockito.when(businessPartnerRepository.filterPickupPointsByPickupPointRequest(pickupPointFilterRequest))
        .thenReturn(Arrays.asList(pickupPointResponse, pickupPointResponse1));
    ProfileResponse profileResponse1 = new ProfileResponse();
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setSalesChannel(Arrays.asList(Constants.B2C_SELLER_CHANNEL));
    profileResponse1.setCompany(companyDTO);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse1);
    Mockito.when(productInventoryService.isSyncedToLevel1Inventory(profileResponse1)).thenReturn(true);
    productVariantUpdateRequest.setAddPickupPoints(Arrays.asList(modifiedItemPickupPoint));
    List<ProductVariantPriceStockAndImagesRequest> productItems = new ArrayList<>();
    ProductVariantPriceStockAndImagesRequest itemPickupPoints = new ProductVariantPriceStockAndImagesRequest();
    itemPickupPoints.setModifiedItemPickupPoints(Arrays.asList(modifiedItemPickupPoint));
    productItems.add(itemPickupPoints);
    productVariantUpdateRequest.setProductItems(productItems);
    productVariantUpdateRequest.setB2cActivated(false);
    variantEditValidationService.validateAndSetFbbActiveFlagAtL5(productVariantUpdateRequest, BUSINESS_PARTNER_CODE,
        profileResponse1);
    Mockito.verify(businessPartnerRepository).filterPickupPointsByPickupPointRequest(pickupPointFilterRequest);
    Mockito.verify(productInventoryService).isSyncedToLevel1Inventory(profileResponse1);
  }

  @Test
  public void validateAndSetFbbActiveFlagAtL5FalsecncTrueTest() throws Exception {
    ReflectionTestUtils.setField(variantEditValidationService, "setWaitingDeletionForDeletePickupPoint", true);
    ItemPickupPointRequest modifiedItemPickupPoint = new ItemPickupPointRequest();
    modifiedItemPickupPoint.setPickupPointId(PP_CODE);
    modifiedItemPickupPoint.setBuyable(false);
    modifiedItemPickupPoint.setDisplay(false);
    modifiedItemPickupPoint.setCncActive(true);
    PickupPointResponse pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PP_CODE_1);
    pickupPointResponse.setFbbActivated(true);
    PickupPointResponse pickupPointResponse1 = new PickupPointResponse();
    pickupPointResponse1.setCode(PP_CODE_1);
    pickupPointResponse1.setFbbActivated(true);
    PickupPointFilterRequest pickupPointFilterRequest = new PickupPointFilterRequest();
    pickupPointFilterRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    pickupPointFilterRequest.setCodes(new HashSet<>(Collections.singletonList(PP_CODE)));
    pickupPointFilterRequest.setFbbActivated(true);
    pickupPointFilterRequest.setWaitingDeletion(false);
    Mockito.when(businessPartnerRepository.filterPickupPointsByPickupPointRequest(pickupPointFilterRequest))
        .thenReturn(Arrays.asList(pickupPointResponse, pickupPointResponse1));
    ProfileResponse profileResponse1 = new ProfileResponse();
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setSalesChannel(Arrays.asList(Constants.B2C_SELLER_CHANNEL));
    profileResponse1.setCompany(companyDTO);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse1);
    Mockito.when(productInventoryService.isSyncedToLevel1Inventory(profileResponse1)).thenReturn(true);
    productVariantUpdateRequest.setAddPickupPoints(Arrays.asList(modifiedItemPickupPoint));
    List<ProductVariantPriceStockAndImagesRequest> productItems = new ArrayList<>();
    ProductVariantPriceStockAndImagesRequest itemPickupPoints = new ProductVariantPriceStockAndImagesRequest();
    itemPickupPoints.setModifiedItemPickupPoints(Arrays.asList(modifiedItemPickupPoint));
    productItems.add(itemPickupPoints);
    productVariantUpdateRequest.setProductItems(productItems);
    productVariantUpdateRequest.setB2cActivated(false);
    variantEditValidationService.validateAndSetFbbActiveFlagAtL5(productVariantUpdateRequest, BUSINESS_PARTNER_CODE,
        profileResponse1);
    Mockito.verify(businessPartnerRepository).filterPickupPointsByPickupPointRequest(pickupPointFilterRequest);
    Mockito.verify(productInventoryService).isSyncedToLevel1Inventory(profileResponse1);
  }

  @Test
  public void validateAndSetFbbActiveFlagAtL5FalsecncTrueB2bTest() throws Exception {
    ReflectionTestUtils.setField(variantEditValidationService, "setWaitingDeletionForDeletePickupPoint", true);
    ItemPickupPointRequest modifiedItemPickupPoint = new ItemPickupPointRequest();
    modifiedItemPickupPoint.setPickupPointId(PP_CODE);
    modifiedItemPickupPoint.setBuyable(false);
    modifiedItemPickupPoint.setDisplay(false);
    modifiedItemPickupPoint.setCncActive(true);
    PickupPointResponse pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PP_CODE_1);
    pickupPointResponse.setFbbActivated(true);
    PickupPointResponse pickupPointResponse1 = new PickupPointResponse();
    pickupPointResponse1.setCode(PP_CODE_1);
    pickupPointResponse1.setFbbActivated(true);
    PickupPointFilterRequest pickupPointFilterRequest = new PickupPointFilterRequest();
    pickupPointFilterRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    pickupPointFilterRequest.setCodes(new HashSet<>(Collections.singletonList(PP_CODE)));
    pickupPointFilterRequest.setFbbActivated(true);
    pickupPointFilterRequest.setWaitingDeletion(false);
    Mockito.when(businessPartnerRepository.filterPickupPointsByPickupPointRequest(pickupPointFilterRequest))
        .thenReturn(Arrays.asList(pickupPointResponse, pickupPointResponse1));
    ProfileResponse profileResponse1 = new ProfileResponse();
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setSalesChannel(Arrays.asList(Constants.B2B_SELLER_CHANNEL));
    profileResponse1.setCompany(companyDTO);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse1);
    Mockito.when(productInventoryService.isSyncedToLevel1Inventory(profileResponse1)).thenReturn(true);
    productVariantUpdateRequest.setAddPickupPoints(Arrays.asList(modifiedItemPickupPoint));
    List<ProductVariantPriceStockAndImagesRequest> productItems = new ArrayList<>();
    ProductVariantPriceStockAndImagesRequest itemPickupPoints = new ProductVariantPriceStockAndImagesRequest();
    itemPickupPoints.setModifiedItemPickupPoints(Arrays.asList(modifiedItemPickupPoint));
    productItems.add(itemPickupPoints);
    productVariantUpdateRequest.setProductItems(productItems);
    productVariantUpdateRequest.setB2cActivated(false);
    variantEditValidationService.validateAndSetFbbActiveFlagAtL5(productVariantUpdateRequest, BUSINESS_PARTNER_CODE,
        profileResponse1);
    Mockito.verify(businessPartnerRepository).filterPickupPointsByPickupPointRequest(pickupPointFilterRequest);
    Mockito.verify(productInventoryService).isSyncedToLevel1Inventory(profileResponse1);
  }

  @Test
  public void validateAndSetFbbActiveFlagAtL5FalseB2bSellerTest() throws Exception {
    ReflectionTestUtils.setField(variantEditValidationService, "setWaitingDeletionForDeletePickupPoint", true);
    ItemPickupPointRequest modifiedItemPickupPoint = new ItemPickupPointRequest();
    modifiedItemPickupPoint.setPickupPointId(PP_CODE);
    B2BFields b2BFields = new B2BFields();
    b2BFields.setBuyable(true);
    b2BFields.setDisplay(false);
    modifiedItemPickupPoint.setB2bFields(b2BFields);
    PickupPointResponse pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PP_CODE_1);
    pickupPointResponse.setFbbActivated(true);
    PickupPointResponse pickupPointResponse1 = new PickupPointResponse();
    pickupPointResponse1.setCode(PP_CODE_1);
    pickupPointResponse1.setFbbActivated(true);
    PickupPointFilterRequest pickupPointFilterRequest = new PickupPointFilterRequest();
    pickupPointFilterRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    pickupPointFilterRequest.setCodes(new HashSet<>(Collections.singletonList(PP_CODE)));
    pickupPointFilterRequest.setFbbActivated(true);
    pickupPointFilterRequest.setWaitingDeletion(false);
    Mockito.when(businessPartnerRepository.filterPickupPointsByPickupPointRequest(pickupPointFilterRequest))
        .thenReturn(Arrays.asList(pickupPointResponse, pickupPointResponse1));
    ProfileResponse profileResponse1 = new ProfileResponse();
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setSalesChannel(Arrays.asList(Constants.B2B_SELLER_CHANNEL));
    profileResponse1.setCompany(companyDTO);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse1);
    Mockito.when(productInventoryService.isSyncedToLevel1Inventory(profileResponse1)).thenReturn(true);
    productVariantUpdateRequest.setAddPickupPoints(Arrays.asList(modifiedItemPickupPoint));
    List<ProductVariantPriceStockAndImagesRequest> productItems = new ArrayList<>();
    ProductVariantPriceStockAndImagesRequest itemPickupPoints = new ProductVariantPriceStockAndImagesRequest();
    itemPickupPoints.setModifiedItemPickupPoints(Arrays.asList(modifiedItemPickupPoint));
    productItems.add(itemPickupPoints);
    productVariantUpdateRequest.setProductItems(productItems);
    productVariantUpdateRequest.setB2cActivated(false);
    variantEditValidationService.validateAndSetFbbActiveFlagAtL5(productVariantUpdateRequest, BUSINESS_PARTNER_CODE,
        profileResponse1);
    Mockito.verify(businessPartnerRepository).filterPickupPointsByPickupPointRequest(pickupPointFilterRequest);
    Mockito.verify(productInventoryService).isSyncedToLevel1Inventory(profileResponse1);
  }

  @Test
  public void validateAndSetFbbActiveFlagAtL5FalseB2bSellerBuyableFalseTest() throws Exception {
    ReflectionTestUtils.setField(variantEditValidationService, "setWaitingDeletionForDeletePickupPoint", true);
    ItemPickupPointRequest modifiedItemPickupPoint = new ItemPickupPointRequest();
    modifiedItemPickupPoint.setPickupPointId(PP_CODE);
    B2BFields b2BFields = new B2BFields();
    b2BFields.setBuyable(false);
    b2BFields.setDisplay(true);
    modifiedItemPickupPoint.setB2bFields(b2BFields);
    PickupPointResponse pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PP_CODE_1);
    pickupPointResponse.setFbbActivated(true);
    PickupPointResponse pickupPointResponse1 = new PickupPointResponse();
    pickupPointResponse1.setCode(PP_CODE_1);
    pickupPointResponse1.setFbbActivated(true);
    PickupPointFilterRequest pickupPointFilterRequest = new PickupPointFilterRequest();
    pickupPointFilterRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    pickupPointFilterRequest.setCodes(new HashSet<>(Collections.singletonList(PP_CODE)));
    pickupPointFilterRequest.setFbbActivated(true);
    pickupPointFilterRequest.setWaitingDeletion(false);
    Mockito.when(businessPartnerRepository.filterPickupPointsByPickupPointRequest(pickupPointFilterRequest))
        .thenReturn(Arrays.asList(pickupPointResponse, pickupPointResponse1));
    ProfileResponse profileResponse1 = new ProfileResponse();
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setSalesChannel(Arrays.asList(Constants.B2B_SELLER_CHANNEL));
    profileResponse1.setCompany(companyDTO);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse1);
    Mockito.when(productInventoryService.isSyncedToLevel1Inventory(profileResponse1)).thenReturn(true);
    productVariantUpdateRequest.setAddPickupPoints(Arrays.asList(modifiedItemPickupPoint));
    List<ProductVariantPriceStockAndImagesRequest> productItems = new ArrayList<>();
    ProductVariantPriceStockAndImagesRequest itemPickupPoints = new ProductVariantPriceStockAndImagesRequest();
    itemPickupPoints.setModifiedItemPickupPoints(Arrays.asList(modifiedItemPickupPoint));
    productItems.add(itemPickupPoints);
    productVariantUpdateRequest.setProductItems(productItems);
    productVariantUpdateRequest.setB2cActivated(false);
    variantEditValidationService.validateAndSetFbbActiveFlagAtL5(productVariantUpdateRequest, BUSINESS_PARTNER_CODE,
        profileResponse1);
    Mockito.verify(businessPartnerRepository).filterPickupPointsByPickupPointRequest(pickupPointFilterRequest);
    Mockito.verify(productInventoryService).isSyncedToLevel1Inventory(profileResponse1);
  }

  @Test
  public void validateAndSetFbbActiveFlagAtL5FalseB2bSellerB2bFieldsFalseTest() throws Exception {
    ReflectionTestUtils.setField(variantEditValidationService, "setWaitingDeletionForDeletePickupPoint", true);
    ItemPickupPointRequest modifiedItemPickupPoint = new ItemPickupPointRequest();
    modifiedItemPickupPoint.setPickupPointId(PP_CODE);
    B2BFields b2BFields = new B2BFields();
    b2BFields.setBuyable(false);
    b2BFields.setDisplay(false);
    modifiedItemPickupPoint.setB2bFields(b2BFields);
    PickupPointResponse pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PP_CODE_1);
    pickupPointResponse.setFbbActivated(true);
    PickupPointResponse pickupPointResponse1 = new PickupPointResponse();
    pickupPointResponse1.setCode(PP_CODE_1);
    pickupPointResponse1.setFbbActivated(true);
    PickupPointFilterRequest pickupPointFilterRequest = new PickupPointFilterRequest();
    pickupPointFilterRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    pickupPointFilterRequest.setCodes(new HashSet<>(Collections.singletonList(PP_CODE)));
    pickupPointFilterRequest.setFbbActivated(true);
    pickupPointFilterRequest.setWaitingDeletion(false);
    Mockito.when(businessPartnerRepository.filterPickupPointsByPickupPointRequest(pickupPointFilterRequest))
        .thenReturn(Arrays.asList(pickupPointResponse, pickupPointResponse1));
    ProfileResponse profileResponse1 = new ProfileResponse();
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setSalesChannel(Arrays.asList(Constants.B2B_SELLER_CHANNEL));
    profileResponse1.setCompany(companyDTO);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse1);
    Mockito.when(productInventoryService.isSyncedToLevel1Inventory(profileResponse1)).thenReturn(true);
    productVariantUpdateRequest.setAddPickupPoints(Arrays.asList(modifiedItemPickupPoint));
    List<ProductVariantPriceStockAndImagesRequest> productItems = new ArrayList<>();
    ProductVariantPriceStockAndImagesRequest itemPickupPoints = new ProductVariantPriceStockAndImagesRequest();
    itemPickupPoints.setModifiedItemPickupPoints(Arrays.asList(modifiedItemPickupPoint));
    productItems.add(itemPickupPoints);
    productVariantUpdateRequest.setProductItems(productItems);
    productVariantUpdateRequest.setB2cActivated(false);
    variantEditValidationService.validateAndSetFbbActiveFlagAtL5(productVariantUpdateRequest, BUSINESS_PARTNER_CODE,
        profileResponse1);
    Mockito.verify(businessPartnerRepository).filterPickupPointsByPickupPointRequest(pickupPointFilterRequest);
    Mockito.verify(productInventoryService).isSyncedToLevel1Inventory(profileResponse1);
  }

  @Test
  public void validateAndSetFbbActiveFlagAtL5SyncStockAtL5Test() throws Exception {
    ReflectionTestUtils.setField(variantEditValidationService, "setWaitingDeletionForDeletePickupPoint", true);
    ReflectionTestUtils.setField(variantEditValidationService, "inventoryL5SyncStockEnabled", true);
    ItemPickupPointRequest modifiedItemPickupPoint = new ItemPickupPointRequest();
    modifiedItemPickupPoint.setPickupPointId(PP_CODE);
    PickupPointResponse pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PP_CODE_1);
    pickupPointResponse.setFbbActivated(false);
    PickupPointResponse pickupPointResponse1 = new PickupPointResponse();
    pickupPointResponse1.setCode(PP_CODE_1);
    pickupPointResponse1.setFbbActivated(true);
    PickupPointFilterRequest pickupPointFilterRequest = new PickupPointFilterRequest();
    pickupPointFilterRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    pickupPointFilterRequest.setCodes(new HashSet<>(Collections.singletonList(PP_CODE)));
    pickupPointFilterRequest.setFbbActivated(true);
    pickupPointFilterRequest.setWaitingDeletion(false);
    Mockito.when(businessPartnerRepository.filterPickupPointsByPickupPointRequest(pickupPointFilterRequest))
        .thenReturn(Arrays.asList(pickupPointResponse, pickupPointResponse1));
    ProfileResponse profileResponse1 = new ProfileResponse();
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setSalesChannel(null);
    profileResponse1.setCompany(companyDTO);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse1);
    Mockito.when(productInventoryService.isSyncedToLevel1Inventory(profileResponse1)).thenReturn(true);
    productVariantUpdateRequest.setAddPickupPoints(Arrays.asList(modifiedItemPickupPoint));
    variantEditValidationService.validateAndSetFbbActiveFlagAtL5(productVariantUpdateRequest, BUSINESS_PARTNER_CODE,
        null);
    Mockito.verify(businessPartnerRepository).filterPickupPointsByPickupPointRequest(pickupPointFilterRequest);
  }

  @Test
  public void validateAndSetFbbActiveFlagAtL5SyncStockAtL5NoneTest() throws Exception {
    ReflectionTestUtils.setField(variantEditValidationService, "setWaitingDeletionForDeletePickupPoint", true);
    ReflectionTestUtils.setField(variantEditValidationService, "inventoryL5SyncStockEnabled", true);
    ItemPickupPointRequest modifiedItemPickupPoint = new ItemPickupPointRequest();
    modifiedItemPickupPoint.setPickupPointId(PP_CODE);
    PickupPointResponse pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PP_CODE);
    pickupPointResponse.setFbbActivated(true);
    PickupPointResponse pickupPointResponse1 = new PickupPointResponse();
    pickupPointResponse1.setCode(PP_CODE_1);
    pickupPointResponse1.setFbbActivated(false);
    PickupPointFilterRequest pickupPointFilterRequest = new PickupPointFilterRequest();
    pickupPointFilterRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    pickupPointFilterRequest.setCodes(new HashSet<>(Collections.singletonList(PP_CODE)));
    pickupPointFilterRequest.setFbbActivated(true);
    pickupPointFilterRequest.setWaitingDeletion(false);
    Mockito.when(businessPartnerRepository.filterPickupPointsByPickupPointRequest(pickupPointFilterRequest))
        .thenReturn(Arrays.asList(pickupPointResponse, pickupPointResponse1));
    Mockito.when(productInventoryService.isSyncedToLevel1Inventory(new ProfileResponse())).thenReturn(false);
    productVariantUpdateRequest.setAddPickupPoints(Arrays.asList(modifiedItemPickupPoint));
    variantEditValidationService.validateAndSetFbbActiveFlagAtL5(productVariantUpdateRequest, BUSINESS_PARTNER_CODE,
        null);
    Mockito.verify(businessPartnerRepository).filterPickupPointsByPickupPointRequest(pickupPointFilterRequest);
  }

  @Test
  public void validateAndSetFbbActiveFlagAtSyncStockFalseL5Test() throws Exception {
    ReflectionTestUtils.setField(variantEditValidationService, "setWaitingDeletionForDeletePickupPoint", true);
    ReflectionTestUtils.setField(variantEditValidationService, "inventoryL5SyncStockEnabled", false);
    ItemPickupPointRequest modifiedItemPickupPoint = new ItemPickupPointRequest();
    modifiedItemPickupPoint.setPickupPointId(PP_CODE);
    PickupPointResponse pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PP_CODE);
    pickupPointResponse.setFbbActivated(true);
    PickupPointResponse pickupPointResponse1 = new PickupPointResponse();
    pickupPointResponse1.setCode(PP_CODE_1);
    pickupPointResponse1.setFbbActivated(false);
    PickupPointFilterRequest pickupPointFilterRequest = new PickupPointFilterRequest();
    pickupPointFilterRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    pickupPointFilterRequest.setCodes(new HashSet<>(Collections.singletonList(PP_CODE)));
    pickupPointFilterRequest.setFbbActivated(true);
    pickupPointFilterRequest.setWaitingDeletion(false);
    Mockito.when(businessPartnerRepository.filterPickupPointsByPickupPointRequest(pickupPointFilterRequest))
        .thenReturn(Arrays.asList(pickupPointResponse, pickupPointResponse1));
    Mockito.when(productInventoryService.isSyncedToLevel1Inventory(null)).thenReturn(true);
    productVariantUpdateRequest.setAddPickupPoints(Arrays.asList(modifiedItemPickupPoint));
    variantEditValidationService.validateAndSetFbbActiveFlagAtL5(productVariantUpdateRequest, BUSINESS_PARTNER_CODE,
        null);
    Mockito.verify(businessPartnerRepository).filterPickupPointsByPickupPointRequest(pickupPointFilterRequest);
    Mockito.verify(productInventoryService).isSyncedToLevel1Inventory(null);
  }

  @Test
  public void validateAndSetFbbActiveFlagAtSyncStockFalseL5PpCodeEmptyListResponseTest() throws Exception {
    ReflectionTestUtils.setField(variantEditValidationService, "setWaitingDeletionForDeletePickupPoint", true);
    ReflectionTestUtils.setField(variantEditValidationService, "inventoryL5SyncStockEnabled", false);
    ItemPickupPointRequest modifiedItemPickupPoint = new ItemPickupPointRequest();
    modifiedItemPickupPoint.setPickupPointId(PP_CODE);
    PickupPointResponse pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PP_CODE);
    pickupPointResponse.setFbbActivated(true);
    PickupPointResponse pickupPointResponse1 = new PickupPointResponse();
    pickupPointResponse1.setCode(PP_CODE_1);
    pickupPointResponse1.setFbbActivated(false);
    PickupPointFilterRequest pickupPointFilterRequest = new PickupPointFilterRequest();
    pickupPointFilterRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    pickupPointFilterRequest.setCodes(new HashSet<>(Collections.singletonList(PP_CODE)));
    pickupPointFilterRequest.setFbbActivated(true);
    pickupPointFilterRequest.setWaitingDeletion(false);
    Mockito.when(productInventoryService.isSyncedToLevel1Inventory(null)).thenReturn(true);
    productVariantUpdateRequest.setAddPickupPoints(Arrays.asList(modifiedItemPickupPoint));
    variantEditValidationService.validateAndSetFbbActiveFlagAtL5(productVariantUpdateRequest, BUSINESS_PARTNER_CODE,
        null);
    Mockito.verify(businessPartnerRepository).filterPickupPointsByPickupPointRequest(pickupPointFilterRequest);
    Mockito.verify(productInventoryService).isSyncedToLevel1Inventory(null);
  }

  @Test
  public void validateAndSetFbbActiveFlagAtSyncStockFalseEmptyTest() throws Exception {
    ReflectionTestUtils.setField(variantEditValidationService, "inventoryL5SyncStockEnabled", false);
    productVariantUpdateRequest.setAddPickupPoints(new ArrayList<>());
    variantEditValidationService.validateAndSetFbbActiveFlagAtL5(productVariantUpdateRequest, BUSINESS_PARTNER_CODE,
        null);
  }

  @Test
  public void validateNewL5AdditionRequests_salePriceMoreThanNormalPriceTest() throws Exception {
    Mockito.when(
            productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE))
        .thenReturn(new ProductSystemParameter(Constants.MINIMUM_PRICE, "100", "min-price", false));
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setPrice(900.0);
    itemPickupPointRequest.setSalePrice(1000.0);
    productVariantUpdateRequest.setAddPickupPoints(Collections.singletonList(itemPickupPointRequest));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        variantEditValidationService.validateNewL5AdditionRequests(productVariantUpdateRequest,
            new SimpleStringResponse(), profileResponse, new ArrayList<>(), new HashMap<>());
      });
    } finally {
      Mockito.verify(productSystemParameterService)
          .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
    }
  }

  @Test
  public void validateNewL5AdditionRequests_requestNormalPriceTest() throws Exception {
    Mockito.when(
            productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE))
        .thenReturn(new ProductSystemParameter(Constants.MINIMUM_PRICE, "100", "min-price", false));
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setPrice(null);
    itemPickupPointRequest.setSalePrice(null);
    productVariantUpdateRequest.setAddPickupPoints(Collections.singletonList(itemPickupPointRequest));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        variantEditValidationService.validateNewL5AdditionRequests(productVariantUpdateRequest,
            new SimpleStringResponse(), profileResponse, new ArrayList<>(), new HashMap<>());
      });
    } finally {
      Mockito.verify(productSystemParameterService)
          .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
    }
  }

  @Test
  public void validateUpcCodeL5LevelNew1Test() {
    variantEditValidationService.validateUpcCodeL5LevelNew(new HashMap<>(), new SimpleStringResponse(), null,
        new ArrayList<>());
    ProductPriceStockAndImagesRequest productPriceStockAndImagesRequest = new ProductPriceStockAndImagesRequest();
    variantEditValidationService.validateUpcCodeL5LevelNew(new HashMap<>(), new SimpleStringResponse(), null,
        Collections.singletonList(productPriceStockAndImagesRequest));
    productPriceStockAndImagesRequest.setUpcCode(UPC_CODE);
    variantEditValidationService.validateUpcCodeL5LevelNew(new HashMap<>(), new SimpleStringResponse(), null,
        Collections.singletonList(productPriceStockAndImagesRequest));
  }

  @Test
  public void validateUpcCodeL5LevelNew2Test() {
    Map<String, String> skuCodeUpcCodeMap = new HashMap<>();
    skuCodeUpcCodeMap.putIfAbsent(SKU_CODE, UPC_CODE);
    ProductPriceStockAndImagesRequest productPriceStockAndImagesRequest = new ProductPriceStockAndImagesRequest();
    productPriceStockAndImagesRequest.setSkuCode(SKU_CODE);
    productPriceStockAndImagesRequest.setUpcCode(UPC_CODE);
    variantEditValidationService.validateUpcCodeL5LevelNew(skuCodeUpcCodeMap, new SimpleStringResponse(), null,
        Collections.singletonList(productPriceStockAndImagesRequest));
  }

  @Test
  public void validateUpcCodeL5LevelNew3Test() {
    Map<String, String> skuCodeUpcCodeMap = new HashMap<>();
    skuCodeUpcCodeMap.putIfAbsent(SKU_CODE, UPC_CODE);
    ProductPriceStockAndImagesRequest productPriceStockAndImagesRequest = new ProductPriceStockAndImagesRequest();
    productPriceStockAndImagesRequest.setSkuCode(SKU_CODE);
    productPriceStockAndImagesRequest.setUpcCode(UPC_CODE_UPDATED);
    ProductVariantUpdateRequest productVariantUpdateRequest = new ProductVariantUpdateRequest();
    productVariantUpdateRequest.setProductCode(PRODUCT_CODE);
    variantEditValidationService.validateUpcCodeL5LevelNew(skuCodeUpcCodeMap, new SimpleStringResponse(),
        productVariantUpdateRequest, Collections.singletonList(productPriceStockAndImagesRequest));
    ProductItemUpcCodesSkuCodesRequest productItemUpcCodesSkuCodesRequest = new ProductItemUpcCodesSkuCodesRequest();
    productItemUpcCodesSkuCodesRequest.setSkuCodes(Collections.singletonList(SKU_CODE));
    productItemUpcCodesSkuCodesRequest.setUpcCodes(Collections.singletonList(UPC_CODE_UPDATED));
    Mockito.verify(productOutbound)
        .getItemCodesByUpcCodeAndProductCode(PRODUCT_CODE, productItemUpcCodesSkuCodesRequest);
  }

  @Test
  public void validateUpcCodeL5LevelNew4Test() {
    Map<String, String> skuCodeUpcCodeMap = new HashMap<>();
    skuCodeUpcCodeMap.putIfAbsent(SKU_CODE, UPC_CODE);
    ProductPriceStockAndImagesRequest productPriceStockAndImagesRequest = new ProductPriceStockAndImagesRequest();
    productPriceStockAndImagesRequest.setSkuCode(SKU_CODE);
    productPriceStockAndImagesRequest.setUpcCode(UPC_CODE_UPDATED);
    ProductVariantUpdateRequest productVariantUpdateRequest = new ProductVariantUpdateRequest();
    productVariantUpdateRequest.setProductCode(PRODUCT_CODE);
    ProductItemUpcCodesSkuCodesRequest productItemUpcCodesSkuCodesRequest = new ProductItemUpcCodesSkuCodesRequest();
    productItemUpcCodesSkuCodesRequest.setSkuCodes(Collections.singletonList(SKU_CODE));
    productItemUpcCodesSkuCodesRequest.setUpcCodes(Collections.singletonList(UPC_CODE_UPDATED));
    Mockito.when(productOutbound.getItemCodesByUpcCodeAndProductCode(PRODUCT_CODE, productItemUpcCodesSkuCodesRequest))
        .thenReturn(Collections.singletonList(SKU_CODE));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        variantEditValidationService.validateUpcCodeL5LevelNew(skuCodeUpcCodeMap, new SimpleStringResponse(),
            productVariantUpdateRequest, Collections.singletonList(productPriceStockAndImagesRequest));
      });
    } finally {
      Mockito.verify(productOutbound)
          .getItemCodesByUpcCodeAndProductCode(PRODUCT_CODE, productItemUpcCodesSkuCodesRequest);
    }
  }

  @Test
  public void validateUpcCodeL5LevelNew5Test() {
    Map<String, String> skuCodeUpcCodeMap = new HashMap<>();
    skuCodeUpcCodeMap.putIfAbsent(SKU_CODE, UPC_CODE);
    ProductPriceStockAndImagesRequest productPriceStockAndImagesRequest = new ProductPriceStockAndImagesRequest();
    productPriceStockAndImagesRequest.setSkuCode(SKU_CODE);
    productPriceStockAndImagesRequest.setUpcCode(UPC_CODE_UPDATED);
    ProductVariantUpdateRequest productVariantUpdateRequest = new ProductVariantUpdateRequest();
    productVariantUpdateRequest.setProductCode(PRODUCT_CODE);
    productVariantUpdateRequest.setDeletedProductItems(
        Collections.singletonList(new DeletedProductItems(ITEM_SKU, SKU_CODE_1)));
    ProductItemUpcCodesSkuCodesRequest productItemUpcCodesSkuCodesRequest = new ProductItemUpcCodesSkuCodesRequest();
    productItemUpcCodesSkuCodesRequest.setSkuCodes(Collections.singletonList(SKU_CODE));
    productItemUpcCodesSkuCodesRequest.setUpcCodes(Collections.singletonList(UPC_CODE_UPDATED));
    Mockito.when(productOutbound.getItemCodesByUpcCodeAndProductCode(PRODUCT_CODE, productItemUpcCodesSkuCodesRequest))
        .thenReturn(Collections.singletonList(SKU_CODE_1));
    variantEditValidationService.validateUpcCodeL5LevelNew(skuCodeUpcCodeMap, new SimpleStringResponse(),
        productVariantUpdateRequest, Collections.singletonList(productPriceStockAndImagesRequest));
    Mockito.verify(productOutbound)
        .getItemCodesByUpcCodeAndProductCode(PRODUCT_CODE, productItemUpcCodesSkuCodesRequest);
  }

  @Test
  public void validateL5UpdateRequestNewTest() throws Exception {
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(
        productVariantUpdateRequest.getBusinessPartnerCode())).thenReturn(profileResponse);
    variantEditValidationService.validateL5UpdateRequestNew(productVariantUpdateRequest, new SimpleStringResponse(),
        new ArrayList<>(), profileResponse);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
  }

  @Test
  public void validateL5UpdateRequestNew2Test() throws Exception {
    ReflectionTestUtils.setField(variantEditValidationService, "schedulesAddEditEnabled", true);
    productVariantUpdateRequest.setFreeSample(true);
    productVariantUpdateRequest.setProductItems(
        Collections.singletonList(new ProductVariantPriceStockAndImagesRequest()));
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(
        productVariantUpdateRequest.getBusinessPartnerCode())).thenReturn(profileResponse);
    variantEditValidationService.validateL5UpdateRequestNew(productVariantUpdateRequest, new SimpleStringResponse(),
        new ArrayList<>(), profileResponse);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
  }

  @Test
  public void validateL5UpdateRequestNew3Test() throws Exception {
    productVariantUpdateRequest.setFreeSample(true);
    productVariantUpdateRequest.setProductItems(
        Collections.singletonList(new ProductVariantPriceStockAndImagesRequest()));
    ItemPickupPointRequest addPickupPoint =
        ItemPickupPointRequest.builder().itemSku(ITEM_SKU).pickupPointId(PICKUP_POINT_ID).build();
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(
        productVariantUpdateRequest.getBusinessPartnerCode())).thenReturn(profileResponse);
    productVariantUpdateRequest.setAddPickupPoints(Collections.singletonList(addPickupPoint));
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      variantEditValidationService.validateL5UpdateRequestNew(productVariantUpdateRequest, new SimpleStringResponse(),
          new ArrayList<>(), null);
    });
    Mockito.verify(businessPartnerRepository)
        .filterDetailByBusinessPartnerCode(productVariantUpdateRequest.getBusinessPartnerCode());
  }

  @Test
  public void validateL5UpdateRequestForBuyableSchedulesTest() throws Exception {
    ReflectionTestUtils.setField(variantEditValidationService, "schedulesAddEditEnabled", true);
    productVariantUpdateRequest.setFreeSample(true);
    ItemPickupPointRequest addPickupPoint =
        ItemPickupPointRequest.builder().buyableSchedule(BuyableScheduleRequest.builder().buyable(true).build()).itemSku(ITEM_SKU).pickupPointId(PICKUP_POINT_ID).build();
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest = new ProductVariantPriceStockAndImagesRequest();
    productVariantUpdateRequest.setAddPickupPoints(Collections.singletonList(addPickupPoint));
    productVariantUpdateRequest.setProductItems(Collections.singletonList(productVariantPriceStockAndImagesRequest));
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(productVariantUpdateRequest.getBusinessPartnerCode())).thenReturn(profileResponse);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      variantEditValidationService.validateL5UpdateRequestNew(productVariantUpdateRequest, new SimpleStringResponse(), new ArrayList<>(), null);
    });
    Mockito.verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(productVariantUpdateRequest.getBusinessPartnerCode());
  }

  @Test
  public void validateL5UpdateRequestForDiscoverableSchedulesTest() throws Exception {
    ReflectionTestUtils.setField(variantEditValidationService, "schedulesAddEditEnabled", true);
    productVariantUpdateRequest.setFreeSample(true);
    ItemPickupPointRequest addPickupPoint = ItemPickupPointRequest.builder().discoverableSchedule(DiscoverableScheduleRequest.builder().discoverable(true).build()).itemSku(ITEM_SKU)
        .pickupPointId(PICKUP_POINT_ID).build();
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest = new ProductVariantPriceStockAndImagesRequest();
    productVariantUpdateRequest.setAddPickupPoints(Collections.singletonList(addPickupPoint));
    productVariantUpdateRequest.setProductItems(Collections.singletonList(productVariantPriceStockAndImagesRequest));
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(productVariantUpdateRequest.getBusinessPartnerCode())).thenReturn(profileResponse);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      variantEditValidationService.validateL5UpdateRequestNew(productVariantUpdateRequest, new SimpleStringResponse(), new ArrayList<>(), null);
    });
    Mockito.verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(productVariantUpdateRequest.getBusinessPartnerCode());
  }

  @Test
  public void validateL5UpdateRequestForSchedulesSwitchOffTest() throws Exception {
    ReflectionTestUtils.setField(variantEditValidationService, "schedulesAddEditEnabled", false);
    productVariantUpdateRequest.setFreeSample(true);
    ItemPickupPointRequest addPickupPoint =
        ItemPickupPointRequest.builder().buyableSchedule(BuyableScheduleRequest.builder().buyable(true).build()).itemSku(ITEM_SKU).pickupPointId(PICKUP_POINT_ID).build();
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest = new ProductVariantPriceStockAndImagesRequest();
    productVariantUpdateRequest.setAddPickupPoints(Collections.singletonList(addPickupPoint));
    productVariantUpdateRequest.setProductItems(Collections.singletonList(productVariantPriceStockAndImagesRequest));
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(productVariantUpdateRequest.getBusinessPartnerCode())).thenReturn(profileResponse);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      variantEditValidationService.validateL5UpdateRequestNew(productVariantUpdateRequest, new SimpleStringResponse(), new ArrayList<>(), null);
    });
    Mockito.verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(productVariantUpdateRequest.getBusinessPartnerCode());
  }

  @Test
  public void validateL5UpdateRequestForCncBuyableTest() throws Exception {
    ReflectionTestUtils.setField(variantEditValidationService, "cncForWarehouseFeatureSwitch", true);
    productVariantUpdateRequest.setFreeSample(true);
    ItemPickupPointRequest addPickupPoint =
        ItemPickupPointRequest.builder().cncBuyable(true).itemSku(ITEM_SKU)
            .pickupPointId(PICKUP_POINT_ID).build();
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest = new ProductVariantPriceStockAndImagesRequest();
    productVariantUpdateRequest.setAddPickupPoints(Collections.singletonList(addPickupPoint));
    productVariantUpdateRequest.setProductItems(Collections.singletonList(productVariantPriceStockAndImagesRequest));
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(productVariantUpdateRequest.getBusinessPartnerCode())).thenReturn(profileResponse);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      variantEditValidationService.validateL5UpdateRequestNew(productVariantUpdateRequest, new SimpleStringResponse(), new ArrayList<>(), null);
    });
    Mockito.verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(productVariantUpdateRequest.getBusinessPartnerCode());
  }

  @Test
  public void validateL5UpdateRequestForCncActive_switchOffTest() throws Exception {
    ReflectionTestUtils.setField(variantEditValidationService, "cncForWarehouseFeatureSwitch", false);
    productVariantUpdateRequest.setFreeSample(true);
    ItemPickupPointRequest addPickupPoint =
        ItemPickupPointRequest.builder().cncActive(true).itemSku(ITEM_SKU)
            .pickupPointId(PICKUP_POINT_ID).build();
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest = new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest.setModifiedItemPickupPoints(Collections.singletonList(addPickupPoint));
    productVariantUpdateRequest.setProductItems(Collections.singletonList(productVariantPriceStockAndImagesRequest));
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(productVariantUpdateRequest.getBusinessPartnerCode())).thenReturn(profileResponse);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      variantEditValidationService.validateL5UpdateRequestNew(productVariantUpdateRequest, new SimpleStringResponse(), new ArrayList<>(), null);
    });
    Mockito.verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(productVariantUpdateRequest.getBusinessPartnerCode());
  }

  @Test
  public void validateL5UpdateRequestForCncBuyable_switchOnTest() throws Exception {
    ReflectionTestUtils.setField(variantEditValidationService, "cncForWarehouseFeatureSwitch", true);
    productVariantUpdateRequest.setFreeSample(true);
    ItemPickupPointRequest addPickupPoint =
        ItemPickupPointRequest.builder().cncBuyable(true).itemSku(ITEM_SKU)
            .pickupPointId(PICKUP_POINT_ID).build();
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest = new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest.setModifiedItemPickupPoints(Collections.singletonList(addPickupPoint));
    productVariantUpdateRequest.setProductItems(Collections.singletonList(productVariantPriceStockAndImagesRequest));
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(productVariantUpdateRequest.getBusinessPartnerCode())).thenReturn(profileResponse);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      variantEditValidationService.validateL5UpdateRequestNew(productVariantUpdateRequest, new SimpleStringResponse(), new ArrayList<>(), null);
    });
    Mockito.verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(productVariantUpdateRequest.getBusinessPartnerCode());
  }

  @Test
  public void validateL5UpdateRequestForCncDisplay_switchOnTest() throws Exception {
    ReflectionTestUtils.setField(variantEditValidationService, "cncForWarehouseFeatureSwitch", true);
    productVariantUpdateRequest.setFreeSample(true);
    ItemPickupPointRequest addPickupPoint =
        ItemPickupPointRequest.builder().cncDisplay(true).itemSku(ITEM_SKU)
            .pickupPointId(PICKUP_POINT_ID).build();
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest = new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest.setModifiedItemPickupPoints(Collections.singletonList(addPickupPoint));
    productVariantUpdateRequest.setProductItems(Collections.singletonList(productVariantPriceStockAndImagesRequest));
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(productVariantUpdateRequest.getBusinessPartnerCode())).thenReturn(profileResponse);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      variantEditValidationService.validateL5UpdateRequestNew(productVariantUpdateRequest, new SimpleStringResponse(), new ArrayList<>(), null);
    });
    Mockito.verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(productVariantUpdateRequest.getBusinessPartnerCode());
  }

  @Test
  public void validateL5UpdateRequestValidCnc_switchOffTest() throws Exception {
    ReflectionTestUtils.setField(variantEditValidationService, "cncForWarehouseFeatureSwitch", false);
    productVariantUpdateRequest.setFreeSample(true);
    ItemPickupPointRequest addPickupPoint =
        ItemPickupPointRequest.builder().cncDisplay(true).itemSku(ITEM_SKU)
            .price(PRICE)
            .salePrice(PRICE)
            .pickupPointId(PICKUP_POINT_ID).build();
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest = new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest.setModifiedItemPickupPoints(Collections.singletonList(addPickupPoint));
    productVariantUpdateRequest.setProductItems(Collections.singletonList(productVariantPriceStockAndImagesRequest));
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(productVariantUpdateRequest.getBusinessPartnerCode())).thenReturn(profileResponse);
    variantEditValidationService.validateL5UpdateRequestNew(productVariantUpdateRequest, new SimpleStringResponse(), new ArrayList<>(), null);
    Mockito.verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(productVariantUpdateRequest.getBusinessPartnerCode());
    Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
  }

  @Test
  public void validateL5UpdateRequestValidCnc_switchOnTest() throws Exception {
    ReflectionTestUtils.setField(variantEditValidationService, "cncForWarehouseFeatureSwitch", true);
    productVariantUpdateRequest.setFreeSample(true);
    ItemPickupPointRequest addPickupPoint =
        ItemPickupPointRequest.builder().itemSku(ITEM_SKU)
            .price(PRICE)
            .salePrice(PRICE)
            .pickupPointId(PICKUP_POINT_ID).build();
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest = new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest.setModifiedItemPickupPoints(Collections.singletonList(addPickupPoint));
    productVariantUpdateRequest.setProductItems(Collections.singletonList(productVariantPriceStockAndImagesRequest));
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(productVariantUpdateRequest.getBusinessPartnerCode())).thenReturn(profileResponse);
    variantEditValidationService.validateL5UpdateRequestNew(productVariantUpdateRequest, new SimpleStringResponse(), new ArrayList<>(), null);
    Mockito.verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(productVariantUpdateRequest.getBusinessPartnerCode());
    Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
  }

  @Test
  public void validateL5UpdateRequestForCncDisplayTest() throws Exception {
    ReflectionTestUtils.setField(variantEditValidationService, "cncForWarehouseFeatureSwitch", true);
    productVariantUpdateRequest.setFreeSample(true);
    ItemPickupPointRequest addPickupPoint =
        ItemPickupPointRequest.builder().cncDisplay(true).itemSku(ITEM_SKU)
            .pickupPointId(PICKUP_POINT_ID).build();
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest = new ProductVariantPriceStockAndImagesRequest();
    productVariantUpdateRequest.setAddPickupPoints(Collections.singletonList(addPickupPoint));
    productVariantPriceStockAndImagesRequest.setModifiedItemPickupPoints(Collections.singletonList(addPickupPoint));
    productVariantUpdateRequest.setProductItems(Collections.singletonList(productVariantPriceStockAndImagesRequest));
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(productVariantUpdateRequest.getBusinessPartnerCode())).thenReturn(profileResponse);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      variantEditValidationService.validateL5UpdateRequestNew(productVariantUpdateRequest, new SimpleStringResponse(), new ArrayList<>(), null);
    });
    Mockito.verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(productVariantUpdateRequest.getBusinessPartnerCode());
  }

  @Test
  public void validateL5UpdateRequestForBuyableSchedulesModifiedPPTest() throws Exception {
    ReflectionTestUtils.setField(variantEditValidationService, "schedulesAddEditEnabled", true);
    productVariantUpdateRequest.setFreeSample(true);
    ItemPickupPointRequest modifiedPP =
        ItemPickupPointRequest.builder().buyableSchedule(BuyableScheduleRequest.builder().buyable(true).build()).itemSku(ITEM_SKU).pickupPointId(PICKUP_POINT_ID).build();
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest = new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest.setModifiedItemPickupPoints(Collections.singletonList(modifiedPP));
    productVariantUpdateRequest.setProductItems(Collections.singletonList(productVariantPriceStockAndImagesRequest));
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(productVariantUpdateRequest.getBusinessPartnerCode())).thenReturn(profileResponse);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      variantEditValidationService.validateL5UpdateRequestNew(productVariantUpdateRequest, new SimpleStringResponse(), new ArrayList<>(), null);
    });
    Mockito.verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(productVariantUpdateRequest.getBusinessPartnerCode());
  }

  @Test
  public void validateL5UpdateRequestForDiscoverableSchedulesModifiedPPTest() throws Exception {
    ReflectionTestUtils.setField(variantEditValidationService, "schedulesAddEditEnabled", true);
    productVariantUpdateRequest.setFreeSample(true);
    ItemPickupPointRequest modifiedPP = ItemPickupPointRequest.builder().discoverableSchedule(DiscoverableScheduleRequest.builder().discoverable(true).build()).itemSku(ITEM_SKU)
        .pickupPointId(PICKUP_POINT_ID).build();
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest = new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest.setModifiedItemPickupPoints(Collections.singletonList(modifiedPP));
    productVariantUpdateRequest.setProductItems(Collections.singletonList(productVariantPriceStockAndImagesRequest));
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(productVariantUpdateRequest.getBusinessPartnerCode())).thenReturn(profileResponse);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      variantEditValidationService.validateL5UpdateRequestNew(productVariantUpdateRequest, new SimpleStringResponse(), new ArrayList<>(), null);
    });
    Mockito.verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(productVariantUpdateRequest.getBusinessPartnerCode());
  }

  @Test
  public void validateL5UpdateRequestForSchedulesSwitchOffModifiedPPTest() throws Exception {
    ReflectionTestUtils.setField(variantEditValidationService, "schedulesAddEditEnabled", false);
    productVariantUpdateRequest.setFreeSample(true);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE))
        .thenReturn(new ProductSystemParameter(Constants.MINIMUM_PRICE, "100", "min-price", false));
    ItemPickupPointRequest modifiedPP =
        ItemPickupPointRequest.builder().buyableSchedule(BuyableScheduleRequest.builder().buyable(true).build()).itemSku(ITEM_SKU).pickupPointId(PICKUP_POINT_ID).build();
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest = new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest.setModifiedItemPickupPoints(Collections.singletonList(modifiedPP));
      productVariantUpdateRequest.setProductItems(Collections.singletonList(productVariantPriceStockAndImagesRequest));
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(productVariantUpdateRequest.getBusinessPartnerCode())).thenReturn(profileResponse);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        variantEditValidationService.validateL5UpdateRequestNew(productVariantUpdateRequest, new SimpleStringResponse(),
            new ArrayList<>(), null);
      });
    } finally {
      Mockito.verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(productVariantUpdateRequest.getBusinessPartnerCode());
      Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
    }

  }


  @Test
  public void pickupPointCodeValidationTest() throws Exception {
    ReflectionTestUtils.setField(variantEditValidationService, "ranchIntegrationEnabled", true);
    PickupPointResponse pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_ID);
    pickupPointResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);

    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setPickupPointId(PICKUP_POINT_ID);
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest = new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest.setModifiedItemPickupPoints(Arrays.asList(itemPickupPointRequest));

    ProductL3UpdateRequest productL3UpdateRequest = new ProductL3UpdateRequest();
    productL3UpdateRequest.setProductSku(PRODUCT_SKU);
    productL3UpdateRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productL3UpdateRequest.setProductItems(Arrays.asList(productVariantPriceStockAndImagesRequest));
    productL3UpdateRequest.setAddPickupPoints(Arrays.asList(itemPickupPointRequest));
    ProductItemDistributionInfoRequest productItemDistributionInfoRequest = new ProductItemDistributionInfoRequest();
    productItemDistributionInfoRequest.setDistributionItemInfoRequest(new DistributionItemRequest());
    productItemDistributionInfoRequest.setDimensionsAndUOMRequest(
        Collections.singletonList(new DimensionAndUomRequest()));
    productL3UpdateRequest.setDistributionAndUOMRequest(
        Collections.singletonList(productItemDistributionInfoRequest));
    Map<String, String> mapData = new HashMap<>();
    mapData.put(PP_CODE, PP_CODE);
    productL3UpdateRequest.setDistributionInfoRequest(mapData);
    Map<String, Object> map = new HashMap<>();
    map.put(Constants.DISTRIBUTION_FLAG_KEY, true);
    pickupPointResponse.setFlags(map);
    Mockito.when(
            pickupPointOutbound.getByPickupPointCodes(Constants.DEFAULT_REQUEST_ID, Arrays.asList(PICKUP_POINT_ID)))
        .thenReturn(Arrays.asList(pickupPointResponse));
    variantEditValidationService.pickupPointCodeValidation(productL3UpdateRequest);
    Mockito.verify(pickupPointOutbound).getByPickupPointCodes(Constants.DEFAULT_REQUEST_ID, Arrays.asList(PICKUP_POINT_ID));
  }

  @Test
  public void pickupPointCodeValidationTest2() throws Exception {
    ReflectionTestUtils.setField(variantEditValidationService, "ranchIntegrationEnabled", true);
    PickupPointResponse pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_ID);
    pickupPointResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);

    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setPickupPointId(PICKUP_POINT_ID);
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest = new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest.setModifiedItemPickupPoints(Arrays.asList(itemPickupPointRequest));

    ProductL3UpdateRequest productL3UpdateRequest = new ProductL3UpdateRequest();
    productL3UpdateRequest.setProductSku(PRODUCT_SKU);
    productL3UpdateRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productL3UpdateRequest.setProductItems(Arrays.asList(productVariantPriceStockAndImagesRequest));
    productL3UpdateRequest.setAddPickupPoints(Arrays.asList(itemPickupPointRequest));

    Mockito.when(
            pickupPointOutbound.getByPickupPointCodes(Constants.DEFAULT_REQUEST_ID, Arrays.asList(PICKUP_POINT_ID)))
        .thenReturn(Arrays.asList(pickupPointResponse));
    variantEditValidationService.pickupPointCodeValidation(productL3UpdateRequest);
    Mockito.verify(pickupPointOutbound).getByPickupPointCodes(Constants.DEFAULT_REQUEST_ID, Arrays.asList(PICKUP_POINT_ID));
  }

  @Test
  public void pickupPointCodeValidationSetWaitingDeletionTest() throws Exception {
    ReflectionTestUtils.setField(variantEditValidationService, "setWaitingDeletionForDeletePickupPoint", true);
    PickupPointResponse pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_ID);
    pickupPointResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setPickupPointId(PICKUP_POINT_ID);
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest = new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest.setModifiedItemPickupPoints(Arrays.asList(itemPickupPointRequest));
    ProductL3UpdateRequest productL3UpdateRequest = new ProductL3UpdateRequest();
    productL3UpdateRequest.setProductSku(PRODUCT_SKU);
    productL3UpdateRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productL3UpdateRequest.setProductItems(Arrays.asList(productVariantPriceStockAndImagesRequest));
    Mockito.when(
            pickupPointOutbound.getByPickupPointCodes(Constants.DEFAULT_REQUEST_ID, Arrays.asList(PICKUP_POINT_ID)))
        .thenReturn(Arrays.asList(pickupPointResponse));
    variantEditValidationService.pickupPointCodeValidation(productL3UpdateRequest);
    Mockito.verify(pickupPointOutbound).getByPickupPointCodes(Constants.DEFAULT_REQUEST_ID, Arrays.asList(PICKUP_POINT_ID));
  }

  @Test
  public void pickupPointCodeValidationSetWaitingDeletionAddPickupPointsTest() throws Exception {
    ReflectionTestUtils.setField(variantEditValidationService, "setWaitingDeletionForDeletePickupPoint", true);
    PickupPointResponse pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_ID);
    pickupPointResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setPickupPointId(PICKUP_POINT_ID);
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest = new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest.setModifiedItemPickupPoints(Arrays.asList(itemPickupPointRequest));
    ProductL3UpdateRequest productL3UpdateRequest = new ProductL3UpdateRequest();
    productL3UpdateRequest.setProductSku(PRODUCT_SKU);
    productL3UpdateRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productL3UpdateRequest.setProductItems(Arrays.asList(productVariantPriceStockAndImagesRequest));
    productL3UpdateRequest.setAddPickupPoints(Arrays.asList(itemPickupPointRequest));
    Mockito.when(
            pickupPointOutbound.getByPickupPointCodes(Constants.DEFAULT_REQUEST_ID, Arrays.asList(PICKUP_POINT_ID)))
        .thenReturn(Arrays.asList(pickupPointResponse));
    variantEditValidationService.pickupPointCodeValidation(productL3UpdateRequest);
    Mockito.verify(pickupPointOutbound).getByPickupPointCodes(Constants.DEFAULT_REQUEST_ID, Arrays.asList(PICKUP_POINT_ID));
  }

  @Test
  public void pickupPointCodeValidationSetWaitingDeletionAddPickupPointsExceptionTest() throws Exception {
    ReflectionTestUtils.setField(variantEditValidationService, "setWaitingDeletionForDeletePickupPoint", true);
    PickupPointResponse pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_ID);
    pickupPointResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    pickupPointResponse.setWaitingDeletion(true);
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setPickupPointId(PICKUP_POINT_ID);
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest =
        new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest.setModifiedItemPickupPoints(Arrays.asList(itemPickupPointRequest));
    ProductL3UpdateRequest productL3UpdateRequest = new ProductL3UpdateRequest();
    productL3UpdateRequest.setProductSku(PRODUCT_SKU);
    productL3UpdateRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productL3UpdateRequest.setProductItems(Arrays.asList(productVariantPriceStockAndImagesRequest));
    productL3UpdateRequest.setAddPickupPoints(Arrays.asList(itemPickupPointRequest));
    Mockito.when(
            pickupPointOutbound.getByPickupPointCodes(Constants.DEFAULT_REQUEST_ID, Arrays.asList(PICKUP_POINT_ID)))
        .thenReturn(Arrays.asList(pickupPointResponse));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        variantEditValidationService.pickupPointCodeValidation(productL3UpdateRequest);
      });
    } finally {
      Mockito.verify(pickupPointOutbound)
          .getByPickupPointCodes(Constants.DEFAULT_REQUEST_ID, Arrays.asList(PICKUP_POINT_ID));
    }
  }

  @Test
  public void pickupPointCodeValidationFailureTest() throws Exception {
    PickupPointResponse pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);
    pickupPointResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);

    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setPickupPointId(PICKUP_POINT_ID);
    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest =
        new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest.setModifiedItemPickupPoints(Arrays.asList(itemPickupPointRequest));

    ProductL3UpdateRequest productL3UpdateRequest = new ProductL3UpdateRequest();
    productL3UpdateRequest.setProductSku(PRODUCT_SKU);
    productL3UpdateRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productL3UpdateRequest.setProductItems(Arrays.asList(productVariantPriceStockAndImagesRequest));
    productL3UpdateRequest.setAddPickupPoints(Arrays.asList(itemPickupPointRequest));
    Mockito.when(
            pickupPointOutbound.getByPickupPointCodes(Constants.DEFAULT_REQUEST_ID, Arrays.asList(PICKUP_POINT_ID)))
        .thenReturn(Arrays.asList(pickupPointResponse));
    try {
      variantEditValidationService.pickupPointCodeValidation(productL3UpdateRequest);
    } finally {
      Mockito.verify(pickupPointOutbound)
          .getByPickupPointCodes(Constants.DEFAULT_REQUEST_ID, Arrays.asList(PICKUP_POINT_ID));
    }
  }

  @Test
  public void pickupPointCodeValidationEmptyPickupPointTest() throws Exception {
    PickupPointResponse pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_ID);
    pickupPointResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);

    ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest = new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest.setModifiedItemPickupPoints(new ArrayList<>());

    ProductL3UpdateRequest productL3UpdateRequest = new ProductL3UpdateRequest();
    productL3UpdateRequest.setProductSku(PRODUCT_SKU);
    productL3UpdateRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productL3UpdateRequest.setProductItems(Arrays.asList(productVariantPriceStockAndImagesRequest));
    productL3UpdateRequest.setAddPickupPoints(new ArrayList<>());
    variantEditValidationService.pickupPointCodeValidation(productL3UpdateRequest);
    Mockito.verifyNoMoreInteractions(pickupPointOutbound);
  }

  @Test
  public void validateBundleProductTest() {
    productL3UpdateRequest.setProductSku(PRODUCT_SKU);
    ProductEditValidationDTO productEditValidationDTO = new ProductEditValidationDTO();
    productEditValidationDTO.setEditProductResponse(new EditProductResponse());
    Assertions.assertNull(
        variantEditValidationService.validateBundleProduct(productL3UpdateRequest, new ProductL3Response(),
            productEditValidationDTO, profileResponse).getEditProductResponse().getApiErrorCode());
    ReflectionTestUtils.setField(variantEditValidationService, "productBundlingEnabled", true);
    ReflectionTestUtils.setField(variantEditValidationService, "productBundlingEligibleMerchantTypes", PRODUCT_CODE);
    Assertions.assertNull(
        variantEditValidationService.validateBundleProduct(productL3UpdateRequest, new ProductL3Response(),
            productEditValidationDTO, profileResponse).getEditProductResponse().getApiErrorCode());
    productL3UpdateRequest.setBundleProduct(true);
    Assertions.assertNull(
        variantEditValidationService.validateBundleProduct(productL3UpdateRequest, new ProductL3Response(),
            productEditValidationDTO, profileResponse).getEditProductResponse().getApiErrorCode());
    productL3UpdateRequest.setProductBundleRecipe(Collections.singletonList(new ProductBundleRecipeRequest()));
    Assertions.assertNull(
        variantEditValidationService.validateBundleProduct(productL3UpdateRequest, new ProductL3Response(),
            productEditValidationDTO, profileResponse).getEditProductResponse().getApiErrorCode());
    ReflectionTestUtils.setField(variantEditValidationService, "productBundlingEligibleMerchantTypes",
        BUSINESS_PARTNER_TYPE);
    profileResponse.setCompany(new CompanyDTO());
    profileResponse.getCompany().setMerchantType(BUSINESS_PARTNER_TYPE);
    Assertions.assertNull(
        variantEditValidationService.validateBundleProduct(productL3UpdateRequest, new ProductL3Response(),
            productEditValidationDTO, profileResponse).getEditProductResponse().getApiErrorCode());
  }

  @Test
  public void validateBundleProductQuantityLessThanZeroTest() {
    ProductEditValidationDTO productEditValidationDTO = new ProductEditValidationDTO();
    productEditValidationDTO.setEditProductResponse(new EditProductResponse());
    ReflectionTestUtils.setField(variantEditValidationService, "productBundlingEnabled", true);
    ReflectionTestUtils.setField(variantEditValidationService, "productBundlingEligibleMerchantTypes",
        BUSINESS_PARTNER_TYPE);
    ReflectionTestUtils.setField(variantEditValidationService, "productBundlingMaxNumberOfSkus", 5);
    profileResponse.setCompany(new CompanyDTO());
    profileResponse.getCompany().setMerchantType(BUSINESS_PARTNER_TYPE);
    productL3UpdateRequest.setBundleProduct(true);
    productL3UpdateRequest.setProductBundleRecipe(Arrays.asList(new ProductBundleRecipeRequest("DR6-00001-00001-00002",
        ImmutableSet.of(new ProductBundleRecipe("DR6-00001-00001-00001", 0)))));
    Assertions.assertNotNull(
        variantEditValidationService.validateBundleProduct(productL3UpdateRequest, new ProductL3Response(),
            productEditValidationDTO, profileResponse).getEditProductResponse().getApiErrorCode());
  }

  @Test
  public void validateBundleProductSameParentSkuTest() {
    productL3UpdateRequest.setProductSku("DR6-00001-00001");
    ProductEditValidationDTO productEditValidationDTO = new ProductEditValidationDTO();
    productEditValidationDTO.setEditProductResponse(new EditProductResponse());
    ReflectionTestUtils.setField(variantEditValidationService, "productBundlingEnabled", true);
    ReflectionTestUtils.setField(variantEditValidationService, "productBundlingEligibleMerchantTypes",
        BUSINESS_PARTNER_TYPE);
    ReflectionTestUtils.setField(variantEditValidationService, "productBundlingMaxNumberOfSkus", 5);
    profileResponse.setCompany(new CompanyDTO());
    profileResponse.getCompany().setMerchantType(BUSINESS_PARTNER_TYPE);
    productL3UpdateRequest.setBundleProduct(true);
    productL3UpdateRequest.setProductBundleRecipe(Arrays.asList(new ProductBundleRecipeRequest("DR6-00001-00001-00002",
        ImmutableSet.of(new ProductBundleRecipe("DR6-00001-00001-00001", 1)))));
    Assertions.assertNotNull(
        variantEditValidationService.validateBundleProduct(productL3UpdateRequest, new ProductL3Response(),
            productEditValidationDTO, profileResponse).getEditProductResponse().getApiErrorCode());
  }

  @Test
  public void validateBundleProductTest1() {
    ProductEditValidationDTO productEditValidationDTO = new ProductEditValidationDTO();
    productEditValidationDTO.setEditProductResponse(new EditProductResponse());
    ReflectionTestUtils.setField(variantEditValidationService, "productBundlingEnabled", true);
    ReflectionTestUtils.setField(variantEditValidationService, "productBundlingEligibleMerchantTypes", PRODUCT_CODE);
    productL3UpdateRequest.setBundleProduct(true);
    Assertions.assertNull(
        variantEditValidationService.validateBundleProduct(productL3UpdateRequest, new ProductL3Response(),
            productEditValidationDTO, profileResponse).getEditProductResponse().getApiErrorCode());
  }

  @Test
  public void validateBundleProductInvalidSizeTest() {
    ProductEditValidationDTO productEditValidationDTO = new ProductEditValidationDTO();
    productEditValidationDTO.setEditProductResponse(new EditProductResponse());
    ReflectionTestUtils.setField(variantEditValidationService, "productBundlingEnabled", true);
    ReflectionTestUtils.setField(variantEditValidationService, "productBundlingEligibleMerchantTypes", PRODUCT_CODE);
    productL3UpdateRequest.setBundleProduct(true);
    productL3UpdateRequest.setProductBundleRecipe(Collections.singletonList(new ProductBundleRecipeRequest()));
    ReflectionTestUtils.setField(variantEditValidationService, "productBundlingEligibleMerchantTypes",
        BUSINESS_PARTNER_TYPE);
    profileResponse.setCompany(new CompanyDTO());
    profileResponse.getCompany().setMerchantType(BUSINESS_PARTNER_TYPE);
    ReflectionTestUtils.setField(variantEditValidationService, "productBundlingMaxNumberOfSkus", 1);
    ProductBundleRecipeRequest productBundleRecipeRequest = new ProductBundleRecipeRequest();
    productBundleRecipeRequest.setItemSku(ITEM_SKU);
    ProductBundleRecipe productBundleRecipe = new ProductBundleRecipe();
    productBundleRecipe.setItemSku(ITEM_SKU);
    ProductBundleRecipe productBundleRecipe1 = new ProductBundleRecipe();
    productBundleRecipe1.setItemSku(ITEM_SKU_1);
    productBundleRecipeRequest.setBundleRecipe(new HashSet<>(Arrays.asList(productBundleRecipe, productBundleRecipe1)));
    productL3UpdateRequest.setProductBundleRecipe(
        new ArrayList<>(Collections.singletonList(productBundleRecipeRequest)));
    Assertions.assertNotNull(
        variantEditValidationService.validateBundleProduct(productL3UpdateRequest, new ProductL3Response(),
            productEditValidationDTO, profileResponse).getEditProductResponse().getApiErrorCode());
  }


  @Test
  public void validateBundleProductEmptyRecipeTest() {
    ProductEditValidationDTO productEditValidationDTO = new ProductEditValidationDTO();
    productEditValidationDTO.setEditProductResponse(new EditProductResponse());
    profileResponse.setCompany(new CompanyDTO());
    profileResponse.getCompany().setMerchantType("TD");
    ReflectionTestUtils.setField(variantEditValidationService, "productBundlingEnabled", true);
    ReflectionTestUtils.setField(variantEditValidationService, "productBundlingEligibleMerchantTypes", "TD");
    productL3UpdateRequest.setBundleProduct(true);
    productL3UpdateRequest.setProductBundleRecipe(new ArrayList<>(Arrays.asList(new ProductBundleRecipeRequest(ITEM_SKU, new HashSet<>()))));
    Mockito.when(xProductOutbound.getItemBasicDetailV2Response(new ArrayList<>(), false)).thenReturn(new ArrayList<>());
    Assertions.assertNull(
        variantEditValidationService.validateBundleProduct(productL3UpdateRequest, new ProductL3Response(),
            productEditValidationDTO, profileResponse).getEditProductResponse().getApiErrorCode());
    Mockito.verify(xProductOutbound).getProductBasicDetails(Mockito.anyList());
    Mockito.verify(xProductOutbound).getItemBasicDetailV2Response(new ArrayList<>(), false);
  }

  @Test
  public void validateBundleProductTestInvalidItemSku() {
    ProductEditValidationDTO productEditValidationDTO = new ProductEditValidationDTO();
    productEditValidationDTO.setEditProductResponse(new EditProductResponse());
    ReflectionTestUtils.setField(variantEditValidationService, "productBundlingEnabled", true);
    productL3UpdateRequest.setBundleProduct(true);
    ReflectionTestUtils.setField(variantEditValidationService, "productBundlingEligibleMerchantTypes",
        BUSINESS_PARTNER_TYPE);
    profileResponse.setCompany(new CompanyDTO());
    profileResponse.getCompany().setMerchantType(BUSINESS_PARTNER_TYPE);
    ReflectionTestUtils.setField(variantEditValidationService, "productBundlingMaxNumberOfSkus", 10);
    ProductBundleRecipeRequest productBundleRecipeRequest = new ProductBundleRecipeRequest();
    productBundleRecipeRequest.setItemSku(ITEM_SKU);
    ProductBundleRecipe productBundleRecipe = new ProductBundleRecipe();
    productBundleRecipe.setItemSku(ITEM_SKU);
    ProductBundleRecipe productBundleRecipe1 = new ProductBundleRecipe();
    productBundleRecipe1.setItemSku(PRODUCT_SKU);
    productBundleRecipeRequest.setBundleRecipe(new HashSet<>(Arrays.asList(productBundleRecipe, productBundleRecipe1)));
    productL3UpdateRequest.setProductBundleRecipe(
        new ArrayList<>(Collections.singletonList(productBundleRecipeRequest)));
    Assertions.assertNotNull(
        variantEditValidationService.validateBundleProduct(productL3UpdateRequest, new ProductL3Response(),
            productEditValidationDTO, profileResponse).getEditProductResponse().getApiErrorCode());
  }

  @Test
  public void validateBundleProductTestInvalidChildProductSkuDoesNotExists() {
    productL3UpdateRequest.setProductSku(PRODUCT_SKU);
    ProductEditValidationDTO productEditValidationDTO = new ProductEditValidationDTO();
    productEditValidationDTO.setEditProductResponse(new EditProductResponse());
    ReflectionTestUtils.setField(variantEditValidationService, "productBundlingEnabled", true);
    productL3UpdateRequest.setBundleProduct(true);
    ReflectionTestUtils.setField(variantEditValidationService, "productBundlingEligibleMerchantTypes",
        BUSINESS_PARTNER_TYPE);
    profileResponse.setCompany(new CompanyDTO());
    profileResponse.getCompany().setMerchantType(BUSINESS_PARTNER_TYPE);
    ReflectionTestUtils.setField(variantEditValidationService, "productBundlingMaxNumberOfSkus", 10);
    ProductBundleRecipeRequest productBundleRecipeRequest = new ProductBundleRecipeRequest();
    productBundleRecipeRequest.setItemSku(ITEM_SKU_1);
    ProductBundleRecipe productBundleRecipe = new ProductBundleRecipe();
    productBundleRecipe.setItemSku(ITEM_SKU_1);
    productBundleRecipe.setQuantity(1);
    productBundleRecipeRequest.setBundleRecipe(new HashSet<>(Arrays.asList(productBundleRecipe)));
    productL3UpdateRequest.setProductBundleRecipe(
        new ArrayList<>(Collections.singletonList(productBundleRecipeRequest)));
    ProductBasicResponse productBasicResponse = new ProductBasicResponse();
    productBasicResponse.setProductSku(PRODUCT_SKU_1);
    Mockito.when(xProductOutbound.getProductBasicDetails(Collections.singletonList(PRODUCT_SKU_1)))
        .thenReturn(Collections.singletonList(productBasicResponse));
    Assertions.assertNotNull(
        variantEditValidationService.validateBundleProduct(productL3UpdateRequest, new ProductL3Response(),
            productEditValidationDTO, profileResponse).getEditProductResponse().getApiErrorCode());
    Mockito.verify(xProductOutbound).getProductBasicDetails(Collections.singletonList(PRODUCT_SKU_1));
  }

  @Test
  public void validateBundleProductTestInvalidChildProductSkuMFDTrue() {
    productL3UpdateRequest.setProductSku(PRODUCT_SKU);
    ProductEditValidationDTO productEditValidationDTO = new ProductEditValidationDTO();
    productEditValidationDTO.setEditProductResponse(new EditProductResponse());
    ReflectionTestUtils.setField(variantEditValidationService, "productBundlingEnabled", true);
    productL3UpdateRequest.setBundleProduct(true);
    ReflectionTestUtils.setField(variantEditValidationService, "productBundlingEligibleMerchantTypes",
        BUSINESS_PARTNER_TYPE);
    profileResponse.setCompany(new CompanyDTO());
    profileResponse.getCompany().setMerchantType(BUSINESS_PARTNER_TYPE);
    ReflectionTestUtils.setField(variantEditValidationService, "productBundlingMaxNumberOfSkus", 10);
    ProductBundleRecipeRequest productBundleRecipeRequest = new ProductBundleRecipeRequest();
    productBundleRecipeRequest.setItemSku(ITEM_SKU_1);
    ProductBundleRecipe productBundleRecipe = new ProductBundleRecipe();
    productBundleRecipe.setItemSku(ITEM_SKU_1);
    productBundleRecipe.setQuantity(1);
    productBundleRecipeRequest.setBundleRecipe(new HashSet<>(Arrays.asList(productBundleRecipe)));
    productL3UpdateRequest.setProductBundleRecipe(
        new ArrayList<>(Collections.singletonList(productBundleRecipeRequest)));
    ProductBasicResponse productBasicResponse = new ProductBasicResponse();
    productBasicResponse.setProductSku(PRODUCT_SKU_1);
    productBasicResponse.setProductExists(true);
    productBasicResponse.setMarkForDelete(true);
    Mockito.when(xProductOutbound.getProductBasicDetails(Collections.singletonList(PRODUCT_SKU_1)))
        .thenReturn(Collections.singletonList(productBasicResponse));
    Assertions.assertNotNull(
        variantEditValidationService.validateBundleProduct(productL3UpdateRequest, new ProductL3Response(),
            productEditValidationDTO, profileResponse).getEditProductResponse().getApiErrorCode());
    Mockito.verify(xProductOutbound).getProductBasicDetails(Collections.singletonList(PRODUCT_SKU_1));
  }

  @Test
  public void validateBundleProductTestInvalidChildProductSkuSuspendedTrue() {
    productL3UpdateRequest.setProductSku(PRODUCT_SKU);
    ProductEditValidationDTO productEditValidationDTO = new ProductEditValidationDTO();
    productEditValidationDTO.setEditProductResponse(new EditProductResponse());
    ReflectionTestUtils.setField(variantEditValidationService, "productBundlingEnabled", true);
    productL3UpdateRequest.setBundleProduct(true);
    ReflectionTestUtils.setField(variantEditValidationService, "productBundlingEligibleMerchantTypes",
        BUSINESS_PARTNER_TYPE);
    profileResponse.setCompany(new CompanyDTO());
    profileResponse.getCompany().setMerchantType(BUSINESS_PARTNER_TYPE);
    ReflectionTestUtils.setField(variantEditValidationService, "productBundlingMaxNumberOfSkus", 10);
    ProductBundleRecipeRequest productBundleRecipeRequest = new ProductBundleRecipeRequest();
    productBundleRecipeRequest.setItemSku(ITEM_SKU_1);
    ProductBundleRecipe productBundleRecipe = new ProductBundleRecipe();
    productBundleRecipe.setItemSku(ITEM_SKU_1);
    productBundleRecipe.setQuantity(1);
    productBundleRecipeRequest.setBundleRecipe(new HashSet<>(Arrays.asList(productBundleRecipe)));
    productL3UpdateRequest.setProductBundleRecipe(
        new ArrayList<>(Collections.singletonList(productBundleRecipeRequest)));
    ProductBasicResponse productBasicResponse = new ProductBasicResponse();
    productBasicResponse.setProductSku(PRODUCT_SKU_1);
    productBasicResponse.setProductExists(true);
    productBasicResponse.setSuspended(true);
    Mockito.when(xProductOutbound.getProductBasicDetails(Collections.singletonList(PRODUCT_SKU_1)))
        .thenReturn(Collections.singletonList(productBasicResponse));
    Assertions.assertNotNull(
        variantEditValidationService.validateBundleProduct(productL3UpdateRequest, new ProductL3Response(),
            productEditValidationDTO, profileResponse).getEditProductResponse().getApiErrorCode());
    Mockito.verify(xProductOutbound).getProductBasicDetails(Collections.singletonList(PRODUCT_SKU_1));
  }

  @Test
  public void validateBundleProductTestInvalidChildProductSkuArchivedTrue() {
    productL3UpdateRequest.setProductSku(PRODUCT_SKU);
    ProductEditValidationDTO productEditValidationDTO = new ProductEditValidationDTO();
    productEditValidationDTO.setEditProductResponse(new EditProductResponse());
    ReflectionTestUtils.setField(variantEditValidationService, "productBundlingEnabled", true);
    productL3UpdateRequest.setBundleProduct(true);
    ReflectionTestUtils.setField(variantEditValidationService, "productBundlingEligibleMerchantTypes",
        BUSINESS_PARTNER_TYPE);
    profileResponse.setCompany(new CompanyDTO());
    profileResponse.getCompany().setMerchantType(BUSINESS_PARTNER_TYPE);
    ReflectionTestUtils.setField(variantEditValidationService, "productBundlingMaxNumberOfSkus", 10);
    ProductBundleRecipeRequest productBundleRecipeRequest = new ProductBundleRecipeRequest();
    productBundleRecipeRequest.setItemSku(ITEM_SKU_1);
    ProductBundleRecipe productBundleRecipe = new ProductBundleRecipe();
    productBundleRecipe.setItemSku(ITEM_SKU_1);
    productBundleRecipe.setQuantity(1);
    productBundleRecipeRequest.setBundleRecipe(new HashSet<>(Arrays.asList(productBundleRecipe)));
    productL3UpdateRequest.setProductBundleRecipe(
        new ArrayList<>(Collections.singletonList(productBundleRecipeRequest)));
    ProductBasicResponse productBasicResponse = new ProductBasicResponse();
    productBasicResponse.setProductSku(PRODUCT_SKU_1);
    productBasicResponse.setProductExists(true);
    productBasicResponse.setArchived(true);
    Mockito.when(xProductOutbound.getProductBasicDetails(Collections.singletonList(PRODUCT_SKU_1)))
        .thenReturn(Collections.singletonList(productBasicResponse));
    Assertions.assertNotNull(
        variantEditValidationService.validateBundleProduct(productL3UpdateRequest, new ProductL3Response(),
            productEditValidationDTO, profileResponse).getEditProductResponse().getApiErrorCode());
    Mockito.verify(xProductOutbound).getProductBasicDetails(Collections.singletonList(PRODUCT_SKU_1));
  }

  @Test
  public void validateBundleProductTestInvalidChildProductSku() {
    productL3UpdateRequest.setProductSku(PRODUCT_SKU);
    ProductEditValidationDTO productEditValidationDTO = new ProductEditValidationDTO();
    productEditValidationDTO.setEditProductResponse(new EditProductResponse());
    ReflectionTestUtils.setField(variantEditValidationService, "productBundlingEnabled", true);
    productL3UpdateRequest.setBundleProduct(true);
    ReflectionTestUtils.setField(variantEditValidationService, "productBundlingEligibleMerchantTypes",
        BUSINESS_PARTNER_TYPE);
    profileResponse.setCompany(new CompanyDTO());
    profileResponse.getCompany().setMerchantType(BUSINESS_PARTNER_TYPE);
    ReflectionTestUtils.setField(variantEditValidationService, "productBundlingMaxNumberOfSkus", 10);
    ProductBundleRecipeRequest productBundleRecipeRequest = new ProductBundleRecipeRequest();
    productBundleRecipeRequest.setItemSku(ITEM_SKU_1);
    ProductBundleRecipe productBundleRecipe = new ProductBundleRecipe();
    productBundleRecipe.setItemSku(ITEM_SKU_1);
    productBundleRecipe.setQuantity(1);
    productBundleRecipeRequest.setBundleRecipe(new HashSet<>(Arrays.asList(productBundleRecipe)));
    productL3UpdateRequest.setProductBundleRecipe(
        new ArrayList<>(Collections.singletonList(productBundleRecipeRequest)));
    ProductBasicResponse productBasicResponse = new ProductBasicResponse();
    productBasicResponse.setProductSku(PRODUCT_SKU_1);
    productBasicResponse.setProductExists(true);
    productBasicResponse.setArchived(true);
    Mockito.when(xProductOutbound.getProductBasicDetails(Collections.singletonList(PRODUCT_SKU_1)))
        .thenReturn(Collections.singletonList(productBasicResponse));
    Assertions.assertNotNull(
        variantEditValidationService.validateBundleProduct(productL3UpdateRequest, new ProductL3Response(),
            productEditValidationDTO, profileResponse).getEditProductResponse().getApiErrorCode());
    Mockito.verify(xProductOutbound).getProductBasicDetails(Collections.singletonList(PRODUCT_SKU_1));
  }

  @Test
  public void validateBundleProductTestEmptyChildProductSku() {
    productL3UpdateRequest.setProductSku(PRODUCT_SKU);
    ProductEditValidationDTO productEditValidationDTO = new ProductEditValidationDTO();
    productEditValidationDTO.setEditProductResponse(new EditProductResponse());
    ReflectionTestUtils.setField(variantEditValidationService, "productBundlingEnabled", true);
    productL3UpdateRequest.setBundleProduct(true);
    ReflectionTestUtils.setField(variantEditValidationService, "productBundlingEligibleMerchantTypes",
        BUSINESS_PARTNER_TYPE);
    profileResponse.setCompany(new CompanyDTO());
    profileResponse.getCompany().setMerchantType(BUSINESS_PARTNER_TYPE);
    ReflectionTestUtils.setField(variantEditValidationService, "productBundlingMaxNumberOfSkus", 10);
    ProductBundleRecipeRequest productBundleRecipeRequest = new ProductBundleRecipeRequest();
    productBundleRecipeRequest.setItemSku(ITEM_SKU_1);
    ProductBundleRecipe productBundleRecipe = new ProductBundleRecipe();
    productBundleRecipe.setItemSku(ITEM_SKU_1);
    productBundleRecipe.setQuantity(1);
    productBundleRecipeRequest.setBundleRecipe(new HashSet<>(Arrays.asList(productBundleRecipe)));
    productL3UpdateRequest.setProductBundleRecipe(
        new ArrayList<>(Collections.singletonList(productBundleRecipeRequest)));
    ProductBasicResponse productBasicResponse = new ProductBasicResponse();
    productBasicResponse.setProductSku(PRODUCT_SKU);
    productBasicResponse.setProductExists(true);
    productBasicResponse.setArchived(true);
    Mockito.when(xProductOutbound.getProductBasicDetails(Collections.singletonList(PRODUCT_SKU_1)))
        .thenReturn(Collections.singletonList(productBasicResponse));
    Assertions.assertNotNull(
        variantEditValidationService.validateBundleProduct(productL3UpdateRequest, new ProductL3Response(),
            productEditValidationDTO, profileResponse).getEditProductResponse().getApiErrorCode());
    Mockito.verify(xProductOutbound).getProductBasicDetails(Collections.singletonList(PRODUCT_SKU_1));
  }

  @Test
  public void validateBundleProductTestValid() {
    productL3UpdateRequest.setProductSku(PRODUCT_SKU);
    ProductEditValidationDTO productEditValidationDTO = new ProductEditValidationDTO();
    productEditValidationDTO.setEditProductResponse(new EditProductResponse());
    ReflectionTestUtils.setField(variantEditValidationService, "productBundlingEnabled", true);
    productL3UpdateRequest.setBundleProduct(true);
    ReflectionTestUtils.setField(variantEditValidationService, "productBundlingEligibleMerchantTypes",
        BUSINESS_PARTNER_TYPE);
    profileResponse.setCompany(new CompanyDTO());
    profileResponse.getCompany().setMerchantType(BUSINESS_PARTNER_TYPE);
    ReflectionTestUtils.setField(variantEditValidationService, "productBundlingMaxNumberOfSkus", 10);
    ProductBundleRecipeRequest productBundleRecipeRequest = new ProductBundleRecipeRequest();
    productBundleRecipeRequest.setItemSku(ITEM_SKU_1);
    ProductBundleRecipe productBundleRecipe = new ProductBundleRecipe();
    productBundleRecipe.setItemSku(ITEM_SKU_1);
    productBundleRecipe.setQuantity(1);
    productBundleRecipeRequest.setBundleRecipe(new HashSet<>(Arrays.asList(productBundleRecipe)));
    productL3UpdateRequest.setProductBundleRecipe(
        new ArrayList<>(Collections.singletonList(productBundleRecipeRequest)));
    ProductBasicResponse productBasicResponse = new ProductBasicResponse();
    productBasicResponse.setProductSku(PRODUCT_SKU_1);
    productBasicResponse.setProductExists(true);
    productBasicResponse.setArchived(false);
    Mockito.when(xProductOutbound.getProductBasicDetails(Collections.singletonList(PRODUCT_SKU_1)))
        .thenReturn(Collections.singletonList(productBasicResponse));
    Assertions.assertNotNull(
        variantEditValidationService.validateBundleProduct(productL3UpdateRequest, new ProductL3Response(),
            productEditValidationDTO, profileResponse).getEditProductResponse().getApiErrorCode());
    Mockito.verify(xProductOutbound).getProductBasicDetails(Collections.singletonList(PRODUCT_SKU_1));
  }

  @Test
  public void validateBundleProductTestValidCCSeller() {
    productL3UpdateRequest.setProductSku(PRODUCT_SKU);
    ProductEditValidationDTO productEditValidationDTO = new ProductEditValidationDTO();
    productEditValidationDTO.setEditProductResponse(new EditProductResponse());
    ReflectionTestUtils.setField(variantEditValidationService, "productBundlingEnabled", true);
    productL3UpdateRequest.setBundleProduct(true);
    productL3UpdateRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    ReflectionTestUtils.setField(variantEditValidationService, "productBundlingEligibleMerchantTypes",
        BUSINESS_PARTNER_TYPE_CC);
    profileResponse.setCompany(new CompanyDTO());
    profileResponse.setBusinessPartnerCode("DTA-100001");
    profileResponse.getCompany().setMerchantType(BUSINESS_PARTNER_TYPE_CC);
    ReflectionTestUtils.setField(variantEditValidationService, "productBundlingMaxNumberOfSkus", 10);
    ProductBundleRecipeRequest productBundleRecipeRequest = new ProductBundleRecipeRequest();
    productBundleRecipeRequest.setItemSku(ITEM_SKU_1);
    ProductBundleRecipe productBundleRecipe = new ProductBundleRecipe();
    productBundleRecipe.setItemSku(ITEM_SKU_1);
    productBundleRecipe.setQuantity(1);
    productBundleRecipeRequest.setBundleRecipe(new HashSet<>(Arrays.asList(productBundleRecipe)));
    productL3UpdateRequest.setProductBundleRecipe(
        new ArrayList<>(Collections.singletonList(productBundleRecipeRequest)));
    ProductBasicResponse productBasicResponse = new ProductBasicResponse();
    productBasicResponse.setProductSku(PRODUCT_SKU_1);
    productBasicResponse.setProductExists(true);
    productBasicResponse.setArchived(false);
    Mockito.when(xProductOutbound.getProductBasicDetails(Collections.singletonList(PRODUCT_SKU_1)))
        .thenReturn(Collections.singletonList(productBasicResponse));
    Mockito.when(xProductOutbound.getItemBasicDetailV2Response(Arrays.asList(ITEM_SKU_1), false))
        .thenReturn(Arrays.asList(new ItemBasicDetailV2Response()));
    Assertions.assertNull(
        variantEditValidationService.validateBundleProduct(productL3UpdateRequest, new ProductL3Response(),
            productEditValidationDTO, profileResponse).getEditProductResponse().getApiErrorCode());
    Mockito.verify(xProductOutbound).getProductBasicDetails(Collections.singletonList(PRODUCT_SKU_1));
    Mockito.verify(xProductOutbound).getItemBasicDetailV2Response(Arrays.asList(ITEM_SKU_1), false);
  }


  @Test
  public void validateBundleProductTestValidCCSellerDeletedSku() {
    productL3UpdateRequest.setProductSku(PRODUCT_SKU);
    ProductEditValidationDTO productEditValidationDTO = new ProductEditValidationDTO();
    productEditValidationDTO.setEditProductResponse(new EditProductResponse());
    ReflectionTestUtils.setField(variantEditValidationService, "productBundlingEnabled", true);
    productL3UpdateRequest.setBundleProduct(true);
    productL3UpdateRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    ReflectionTestUtils.setField(variantEditValidationService, "productBundlingEligibleMerchantTypes",
        BUSINESS_PARTNER_TYPE_CC);
    profileResponse.setCompany(new CompanyDTO());
    profileResponse.setBusinessPartnerCode("DTA-100001");
    profileResponse.getCompany().setMerchantType(BUSINESS_PARTNER_TYPE_CC);
    ItemBasicDetailV2Response itemBasicDetailV2Response = new ItemBasicDetailV2Response();
    itemBasicDetailV2Response.setMarkForDelete(true);
    ReflectionTestUtils.setField(variantEditValidationService, "productBundlingMaxNumberOfSkus", 10);
    ProductBundleRecipeRequest productBundleRecipeRequest = new ProductBundleRecipeRequest();
    productBundleRecipeRequest.setItemSku(ITEM_SKU_1);
    ProductBundleRecipe productBundleRecipe = new ProductBundleRecipe();
    productBundleRecipe.setItemSku(ITEM_SKU_1);
    productBundleRecipe.setQuantity(1);
    productBundleRecipeRequest.setBundleRecipe(new HashSet<>(Arrays.asList(productBundleRecipe)));
    productL3UpdateRequest.setProductBundleRecipe(
        new ArrayList<>(Collections.singletonList(productBundleRecipeRequest)));
    ProductBasicResponse productBasicResponse = new ProductBasicResponse();
    productBasicResponse.setProductSku(PRODUCT_SKU_1);
    productBasicResponse.setProductExists(true);
    productBasicResponse.setArchived(false);
    Mockito.when(xProductOutbound.getProductBasicDetails(Collections.singletonList(PRODUCT_SKU_1)))
        .thenReturn(Collections.singletonList(productBasicResponse));
    Mockito.when(xProductOutbound.getItemBasicDetailV2Response(Arrays.asList(ITEM_SKU_1), false))
        .thenReturn(Arrays.asList(itemBasicDetailV2Response));
    Assertions.assertNotNull(
        variantEditValidationService.validateBundleProduct(productL3UpdateRequest, new ProductL3Response(),
            productEditValidationDTO, profileResponse).getEditProductResponse().getApiErrorCode());
    Mockito.verify(xProductOutbound).getProductBasicDetails(Collections.singletonList(PRODUCT_SKU_1));
    Mockito.verify(xProductOutbound).getItemBasicDetailV2Response(Arrays.asList(ITEM_SKU_1), false);
  }

  @Test
  public void validateBundleProductTestValidCCSellerSkuNumberMismatch() {
    productL3UpdateRequest.setProductSku(PRODUCT_SKU);
    ProductEditValidationDTO productEditValidationDTO = new ProductEditValidationDTO();
    productEditValidationDTO.setEditProductResponse(new EditProductResponse());
    ReflectionTestUtils.setField(variantEditValidationService, "productBundlingEnabled", true);
    productL3UpdateRequest.setBundleProduct(true);
    productL3UpdateRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    ReflectionTestUtils.setField(variantEditValidationService, "productBundlingEligibleMerchantTypes",
        BUSINESS_PARTNER_TYPE_CC);
    profileResponse.setCompany(new CompanyDTO());
    profileResponse.setBusinessPartnerCode("DTA-100001");
    profileResponse.getCompany().setMerchantType(BUSINESS_PARTNER_TYPE_CC);
    ReflectionTestUtils.setField(variantEditValidationService, "productBundlingMaxNumberOfSkus", 10);
    ProductBundleRecipeRequest productBundleRecipeRequest = new ProductBundleRecipeRequest();
    productBundleRecipeRequest.setItemSku(ITEM_SKU_1);
    ProductBundleRecipe productBundleRecipe = new ProductBundleRecipe();
    productBundleRecipe.setItemSku(ITEM_SKU_1);
    productBundleRecipe.setQuantity(1);
    productBundleRecipeRequest.setBundleRecipe(new HashSet<>(Arrays.asList(productBundleRecipe)));
    productL3UpdateRequest.setProductBundleRecipe(
        new ArrayList<>(Collections.singletonList(productBundleRecipeRequest)));
    ProductBasicResponse productBasicResponse = new ProductBasicResponse();
    productBasicResponse.setProductSku(PRODUCT_SKU_1);
    productBasicResponse.setProductExists(true);
    productBasicResponse.setArchived(false);
    Mockito.when(xProductOutbound.getProductBasicDetails(Collections.singletonList(PRODUCT_SKU_1)))
        .thenReturn(Collections.singletonList(productBasicResponse));
    Mockito.when(xProductOutbound.getItemBasicDetailV2Response(Arrays.asList(ITEM_SKU_1), false))
        .thenReturn(Arrays.asList(new ItemBasicDetailV2Response(), new ItemBasicDetailV2Response()));
    Assertions.assertNotNull(
        variantEditValidationService.validateBundleProduct(productL3UpdateRequest, new ProductL3Response(),
            productEditValidationDTO, profileResponse).getEditProductResponse().getApiErrorCode());
    Mockito.verify(xProductOutbound).getProductBasicDetails(Collections.singletonList(PRODUCT_SKU_1));
    Mockito.verify(xProductOutbound).getItemBasicDetailV2Response(Arrays.asList(ITEM_SKU_1), false);
  }

  @Test
  public void validateBundleProductTesInvalidSeller() {
    ProductEditValidationDTO productEditValidationDTO = new ProductEditValidationDTO();
    productEditValidationDTO.setEditProductResponse(new EditProductResponse());
    ReflectionTestUtils.setField(variantEditValidationService, "productBundlingEnabled", true);
    productL3UpdateRequest.setBundleProduct(true);
    ReflectionTestUtils.setField(variantEditValidationService, "productBundlingEligibleMerchantTypes",
        BUSINESS_PARTNER_TYPE);
    profileResponse.setCompany(new CompanyDTO());
    profileResponse.getCompany().setMerchantType(ITEM_SKU_1);
    ReflectionTestUtils.setField(variantEditValidationService, "productBundlingMaxNumberOfSkus", 10);
    ProductBundleRecipeRequest productBundleRecipeRequest = new ProductBundleRecipeRequest();
    productBundleRecipeRequest.setItemSku(ITEM_SKU_1);
    ProductBundleRecipe productBundleRecipe = new ProductBundleRecipe();
    productBundleRecipe.setItemSku(ITEM_SKU_1);
    productBundleRecipeRequest.setBundleRecipe(new HashSet<>(Arrays.asList(productBundleRecipe)));
    productL3UpdateRequest.setProductBundleRecipe(
        new ArrayList<>(Collections.singletonList(productBundleRecipeRequest)));
    ProductBasicResponse productBasicResponse = new ProductBasicResponse();
    productBasicResponse.setProductSku(PRODUCT_SKU);
    productBasicResponse.setProductExists(true);
    productBasicResponse.setArchived(true);
    Assertions.assertNull(
        variantEditValidationService.validateBundleProduct(productL3UpdateRequest, new ProductL3Response(),
            productEditValidationDTO, profileResponse).getEditProductResponse().getApiErrorCode());
  }

  @Test
  public void validateBundleProductEmptyRequestSeller() {
    ProductEditValidationDTO productEditValidationDTO = new ProductEditValidationDTO();
    productEditValidationDTO.setEditProductResponse(new EditProductResponse());
    ReflectionTestUtils.setField(variantEditValidationService, "productBundlingEnabled", true);
    productL3UpdateRequest.setBundleProduct(true);
    ReflectionTestUtils.setField(variantEditValidationService, "productBundlingEligibleMerchantTypes",
        BUSINESS_PARTNER_TYPE);
    profileResponse.setCompany(new CompanyDTO());
    profileResponse.getCompany().setMerchantType(BUSINESS_PARTNER_TYPE);
    ReflectionTestUtils.setField(variantEditValidationService, "productBundlingMaxNumberOfSkus", 10);
    ProductBundleRecipeRequest productBundleRecipeRequest = new ProductBundleRecipeRequest();
    ProductBundleRecipe productBundleRecipe = new ProductBundleRecipe();
    productBundleRecipe.setItemSku(ITEM_SKU_1);
    productBundleRecipeRequest.setBundleRecipe(new HashSet<>(Arrays.asList(productBundleRecipe)));
    productL3UpdateRequest.setProductBundleRecipe(
        new ArrayList<>(Collections.singletonList(productBundleRecipeRequest)));
    ProductBasicResponse productBasicResponse = new ProductBasicResponse();
    productBasicResponse.setProductSku(PRODUCT_SKU);
    productBasicResponse.setProductExists(true);
    productBasicResponse.setArchived(true);
    Mockito.when(xProductOutbound.getItemBasicDetailV2Response(new ArrayList<>(), false)).thenReturn(new ArrayList<>());
    Assertions.assertNull(
        variantEditValidationService.validateBundleProduct(productL3UpdateRequest, new ProductL3Response(),
            productEditValidationDTO, profileResponse).getEditProductResponse().getApiErrorCode());
    Mockito.verify(xProductOutbound).getProductBasicDetails(new ArrayList<>());
    Mockito.verify(xProductOutbound).getItemBasicDetailV2Response(new ArrayList<>(), false);
  }

  @Test
  public void validateBundleProductTestValidCCSeller2() {
    productL3UpdateRequest.setProductSku(PRODUCT_SKU);
    ProductEditValidationDTO productEditValidationDTO = new ProductEditValidationDTO();
    productEditValidationDTO.setEditProductResponse(new EditProductResponse());
    ReflectionTestUtils.setField(variantEditValidationService, "productBundlingEnabled", true);
    productL3UpdateRequest.setBundleProduct(true);
    ReflectionTestUtils.setField(variantEditValidationService, "productBundlingEligibleMerchantTypes",
        BUSINESS_PARTNER_TYPE_CC);
    profileResponse.setCompany(new CompanyDTO());
    profileResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    profileResponse.getCompany().setMerchantType(BUSINESS_PARTNER_TYPE_CC);
    ReflectionTestUtils.setField(variantEditValidationService, "productBundlingMaxNumberOfSkus", 10);
    ProductBundleRecipeRequest productBundleRecipeRequest = new ProductBundleRecipeRequest();
    productBundleRecipeRequest.setItemSku(ITEM_SKU_1);
    ProductBundleRecipe productBundleRecipe = new ProductBundleRecipe();
    productBundleRecipe.setItemSku(ITEM_SKU_1);
    productBundleRecipe.setQuantity(1);
    productBundleRecipeRequest.setBundleRecipe(new HashSet<>(Arrays.asList(productBundleRecipe)));
    productL3UpdateRequest.setProductBundleRecipe(
        new ArrayList<>(Collections.singletonList(productBundleRecipeRequest)));
    ProductBasicResponse productBasicResponse = new ProductBasicResponse();
    productBasicResponse.setProductSku(PRODUCT_SKU_1);
    productBasicResponse.setProductExists(true);
    productBasicResponse.setArchived(false);
    Mockito.when(xProductOutbound.getProductBasicDetails(Collections.singletonList(PRODUCT_SKU_1)))
        .thenReturn(Collections.singletonList(productBasicResponse));
    Assertions.assertNotNull(
        variantEditValidationService.validateBundleProduct(productL3UpdateRequest, new ProductL3Response(),
            productEditValidationDTO, profileResponse).getEditProductResponse().getApiErrorCode());
    Mockito.verify(xProductOutbound).getProductBasicDetails(Collections.singletonList(PRODUCT_SKU_1));
  }

  @Test
  public void validateBundleProductBomCreationTest() {
    ProductEditValidationDTO productEditValidationDTO = new ProductEditValidationDTO();
    productEditValidationDTO.setEditProductResponse(new EditProductResponse());
    ReflectionTestUtils.setField(variantEditValidationService, "productBundlingEnabled", true);
    productL3UpdateRequest.setBundleProduct(true);
    ReflectionTestUtils.setField(variantEditValidationService, "productBundlingEligibleMerchantTypes",
        BUSINESS_PARTNER_TYPE);
    ReflectionTestUtils.setField(variantEditValidationService, "warehouseBomActivated",
        true);
    profileResponse.setCompany(new CompanyDTO());
    profileResponse.getCompany().setMerchantType(BUSINESS_PARTNER_TYPE);
    ReflectionTestUtils.setField(variantEditValidationService, "productBundlingMaxNumberOfSkus", 10);
    ProductBundleRecipeRequest productBundleRecipeRequest = new ProductBundleRecipeRequest();
    ProductBundleRecipe productBundleRecipe = new ProductBundleRecipe();
    productBundleRecipe.setItemSku(ITEM_SKU_1);
    productBundleRecipeRequest.setBundleRecipe(new HashSet<>(Arrays.asList(productBundleRecipe)));
    productL3UpdateRequest.setProductBundleRecipe(
        new ArrayList<>(Collections.singletonList(productBundleRecipeRequest)));
    ProductBasicResponse productBasicResponse = new ProductBasicResponse();
    productBasicResponse.setProductSku(PRODUCT_SKU);
    productBasicResponse.setProductExists(true);
    productBasicResponse.setArchived(true);
    Mockito.when(xProductOutbound.getItemBasicDetailV2Response(new ArrayList<>(), false)).thenReturn(new ArrayList<>());
    Assertions.assertNull(
        variantEditValidationService.validateBundleProduct(productL3UpdateRequest, new ProductL3Response(),
            productEditValidationDTO, profileResponse).getEditProductResponse().getApiErrorCode());
    Mockito.verify(xProductOutbound).getProductBasicDetails(new ArrayList<>());
    Mockito.verify(xProductOutbound).getItemBasicDetailV2Response(new ArrayList<>(), false);
    Mockito.verify(productLevel3V2Service).editRequestToBillOfMaterialRecipeRequest(productL3UpdateRequest, new ProductL3Response());
  }


  @Test
  public void testValidateEligibilityForVariantAndL5Deletion_MerchantTypeNotEligible() throws Exception {
    ReflectionTestUtils.setField(variantEditValidationService,
      "validateWarehouseVariantDeletionEnabled", true);
    ReflectionTestUtils.setField(variantEditValidationService,
      "validateWarehouseDeletionEligibleSellers",
      BUSINESS_PARTNER_CODE.concat(Constants.COMMA).concat(BUSINESS_PARTNER_TYPE_CC));
    profileResponse.setCompany(new CompanyDTO());
    profileResponse.getCompany().setMerchantType(BUSINESS_PARTNER_TYPE);
    productL3UpdateRequest.setDeletePickupPoints(new ArrayList<>());
    variantEditValidationService.validateEligibilityForVariantAndL5Deletion(profileResponse,
      productL3UpdateRequest, new ProductLevel3());
  }

  @Test
  public void testValidateEligibilityForVariantAndL5Deletion_VariantDeletionDisabled()
    throws Exception {
    ReflectionTestUtils.setField(variantEditValidationService,
      "validateWarehouseDeletionEligibleSellers",
      BUSINESS_PARTNER_CODE.concat(Constants.COMMA).concat(BUSINESS_PARTNER_TYPE_CC));
    ReflectionTestUtils.setField(variantEditValidationService,
      "validateWarehouseVariantDeletionEnabled", false);
    profileResponse.setCompany(new CompanyDTO());
    profileResponse.getCompany().setMerchantType(BUSINESS_PARTNER_TYPE_CC);
    productL3UpdateRequest.setDeletePickupPoints(new ArrayList<>());
    variantEditValidationService.validateEligibilityForVariantAndL5Deletion(profileResponse,
      productL3UpdateRequest, new ProductLevel3());
  }

  @Test
  public void testValidateEligibilityForVariantAndL5Deletion_NoDeletedPickupPoints() throws Exception {
    ReflectionTestUtils.setField(variantEditValidationService,
      "validateWarehouseDeletionEligibleSellers",
      BUSINESS_PARTNER_CODE.concat(Constants.COMMA).concat(BUSINESS_PARTNER_TYPE_CC));
    ReflectionTestUtils.setField(variantEditValidationService,
      "validateWarehouseVariantDeletionEnabled", true);
    profileResponse.setCompany(new CompanyDTO());
    profileResponse.getCompany().setMerchantType(BUSINESS_PARTNER_TYPE_CC);
    productL3UpdateRequest.setDeletePickupPoints(Collections.emptyList());
    variantEditValidationService.validateEligibilityForVariantAndL5Deletion(profileResponse, productL3UpdateRequest,
      new ProductLevel3());
  }

  @Test
  public void testValidateEligibilityForVariantAndL5Deletion_ValidDeletedPickupPoints() throws Exception {
    ReflectionTestUtils.setField(variantEditValidationService,
      "validateWarehouseDeletionEligibleSellers",
      BUSINESS_PARTNER_CODE.concat(Constants.COMMA).concat(BUSINESS_PARTNER_TYPE_CC));
    ReflectionTestUtils.setField(variantEditValidationService,
      "validateWarehouseVariantDeletionEnabled", true);
    ReflectionTestUtils.setField(variantEditValidationService, "l5InventoryFetchBatchSize", 2);
    profileResponse.setCompany(new CompanyDTO());
    profileResponse.getCompany().setMerchantType(BUSINESS_PARTNER_TYPE_CC);
    productL3UpdateRequest.setUpdatedBy(UPDATED_BY);
    List<PickupPointDeleteRequest> deleteRequests = new ArrayList<>();
    PickupPointDeleteRequest pickupPointDeleteRequest =
      PickupPointDeleteRequest.builder().pickupPointId(PICKUP_POINT_ID).itemSku(ITEM_SKU).build();
    deleteRequests.add(pickupPointDeleteRequest);
    productL3UpdateRequest.setDeletePickupPoints(deleteRequests);
    PickupPointResponse pickupPointResponse =
      PickupPointResponse.builder().code(PICKUP_POINT_ID).fbbActivated(false).build();
    Mockito.when(pickupPointOutbound.getByPickupPointCodes(Constants.PBP,
      Collections.singletonList(PICKUP_POINT_ID))).thenReturn(Collections.singletonList(pickupPointResponse));
    variantEditValidationService.validateEligibilityForVariantAndL5Deletion(profileResponse, productL3UpdateRequest,
      new ProductLevel3());
    Mockito.verify(pickupPointOutbound).getByPickupPointCodes(Constants.PBP,
      Collections.singletonList(PICKUP_POINT_ID));
  }

  @Test
  public void testValidateEligibilityForVariantAndL5Deletion_ItemsEligibleForDeletion() throws Exception {
    ReflectionTestUtils.setField(variantEditValidationService,
      "inventoryBatchUpdateDeleteBatchSize", 2);
    ReflectionTestUtils.setField(variantEditValidationService,
      "validateWarehouseDeletionEligibleSellers",
      BUSINESS_PARTNER_CODE.concat(Constants.COMMA).concat(BUSINESS_PARTNER_TYPE_CC));
    ReflectionTestUtils.setField(variantEditValidationService,
      "validateWarehouseVariantDeletionEnabled", true);
    ProductL3Response productL3Response = new ProductL3Response();
    productL3Response.setPickupPointCodes(List.of(PICKUP_POINT_ID));
    productL3UpdateRequest.setUpdatedBy(UPDATED_BY);
    ReflectionTestUtils.setField(variantEditValidationService, "l5InventoryFetchBatchSize", 2);
    profileResponse.setCompany(new CompanyDTO());
    profileResponse.getCompany().setMerchantType(BUSINESS_PARTNER_TYPE_CC);
    profileResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    ListRequestDTO<InventoryDetailInfoRequestDTO> inventoryDetailInfoRequestDTO =
      new ListRequestDTO<>();
    GdnRestListResponse<ItemPickupPointL5Response> itemResponse = new GdnRestListResponse();
    GdnRestListResponse<InventoryDetailInfoResponseV2DTO> inventoryResponseList =
      new GdnRestListResponse<>();
    ItemPickupPointL5Response l5Response1 =
      ItemPickupPointL5Response.builder().itemSku(ITEM_SKU).pickUpPointCode(PICKUP_POINT_ID)
        .build();
    ItemPickupPointL5Response l5Response2 =
      ItemPickupPointL5Response.builder().itemSku(ITEM_SKU_1).pickUpPointCode(PICKUP_POINT_ID)
        .build();
    itemResponse.setContent(List.of(l5Response1, l5Response2));
    PickupPointResponse pickupPointResponse =
      PickupPointResponse.builder().code(PICKUP_POINT_ID).fbbActivated(true).build();
    Mockito.when(pickupPointOutbound.getByPickupPointCodes(Constants.PBP,
      Collections.singletonList(PICKUP_POINT_ID))).thenReturn(Collections.singletonList(pickupPointResponse));
    List<String> deletedItems = List.of(ITEM_SKU, ITEM_SKU_1);
    InventoryDetailInfoResponseV2DTO inventoryResponse = new InventoryDetailInfoResponseV2DTO();
    WarehouseInventoryResponseDTO distributed = new WarehouseInventoryResponseDTO();
    distributed.setAvailableStock(0);
    inventoryResponse.setWarehouseInventoryResponseList(Collections.singletonList(distributed));
    inventoryResponseList.setContent(Collections.singletonList(inventoryResponse));
    inventoryResponseList.setSuccess(true);
    inventoryResponseList.setErrorMessage(null);
    itemResponse.setSuccess(true);
    InventoryDetailInfoRequestDTO inventoryDetail = new InventoryDetailInfoRequestDTO();
    inventoryDetail.setWebMerchantCode(BUSINESS_PARTNER_CODE);
    inventoryDetail.setWebItemSku(ITEM_SKU);
    inventoryDetail.setPickupPointCode(PICKUP_POINT_ID);
    inventoryDetailInfoRequestDTO.setList(Collections.singletonList(inventoryDetail));
    List<DeletedProductItems> deletedProductItems = new ArrayList<>();
    DeletedProductItems item1 = new DeletedProductItems();
    item1.setItemSku(ITEM_SKU);
    DeletedProductItems item2 = new DeletedProductItems();
    item2.setItemSku(ITEM_SKU_1);
    deletedProductItems.add(item1);
    deletedProductItems.add(item2);
    productL3UpdateRequest.setDeletedProductItems(deletedProductItems);
    List<PickupPointDeleteRequest> deleteRequests = new ArrayList<>();
    PickupPointDeleteRequest pickupPointDeleteRequest =
      PickupPointDeleteRequest.builder().pickupPointId(PICKUP_POINT_ID).itemSku(ITEM_SKU).build();
    deleteRequests.add(pickupPointDeleteRequest);
    productL3UpdateRequest.setDeletePickupPoints(deleteRequests);
    Mockito.when(xProductOutbound.fetchL5ResponsesByItemSkus(deletedItems, 0, deletedItems.size())).thenReturn(itemResponse);
    Mockito.when(inventoryOutbound.findDetailByWebMerchantCodeAndWebItemSku(
      ProductLevel3InventoryUtil.generateMandatoryRequestParam(), inventoryDetailInfoRequestDTO)).thenReturn(inventoryResponseList);
    variantEditValidationService.validateEligibilityForVariantAndL5Deletion(profileResponse, productL3UpdateRequest,
      new ProductLevel3());
    Mockito.verify(xProductOutbound).fetchL5ResponsesByItemSkus(deletedItems, 0, 2);
    Mockito.verify(pickupPointOutbound).getByPickupPointCodes(Constants.PBP,
      Collections.singletonList(PICKUP_POINT_ID));
  }


  @Test
  public void testValidateEligibilityForVariantAndL5Deletion_ExceptionThrownWhileFetchingL5Responses() throws Exception {
    ReflectionTestUtils.setField(variantEditValidationService, "inventoryBatchUpdateDeleteBatchSize", 2);
    ReflectionTestUtils.setField(variantEditValidationService,
      "validateWarehouseDeletionEligibleSellers",
      BUSINESS_PARTNER_CODE.concat(Constants.COMMA).concat(BUSINESS_PARTNER_TYPE_CC));
    ReflectionTestUtils.setField(variantEditValidationService, "validateWarehouseVariantDeletionEnabled", true);
    ReflectionTestUtils.setField(variantEditValidationService, "l5InventoryFetchBatchSize", 2);

    productL3UpdateRequest.setUpdatedBy(UPDATED_BY);
    profileResponse.setCompany(new CompanyDTO());
    profileResponse.getCompany().setMerchantType(BUSINESS_PARTNER_TYPE_CC);
    profileResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);

    DeletedProductItems item1 = new DeletedProductItems();
    item1.setItemSku(ITEM_SKU);
    DeletedProductItems item2 = new DeletedProductItems();
    item2.setItemSku(ITEM_SKU_1);
    productL3UpdateRequest.setDeletedProductItems(List.of(item1, item2));

    PickupPointDeleteRequest deleteRequest = PickupPointDeleteRequest.builder()
      .pickupPointId(PICKUP_POINT_ID)
      .itemSku(ITEM_SKU)
      .build();
    PickupPointResponse pickupPointResponse =
      PickupPointResponse.builder().code(PICKUP_POINT_ID).fbbActivated(true).build();
    Mockito.when(pickupPointOutbound.getByPickupPointCodes(Constants.PBP,
      Collections.singletonList(PICKUP_POINT_ID))).thenReturn(Collections.singletonList(pickupPointResponse));
    productL3UpdateRequest.setDeletePickupPoints(List.of(deleteRequest));
    Mockito.when(xProductOutbound.fetchL5ResponsesByItemSkus(List.of(ITEM_SKU, ITEM_SKU_1), 0, 2))
      .thenThrow(new ApplicationRuntimeException());
    Assertions.assertDoesNotThrow(() -> {
      variantEditValidationService.validateEligibilityForVariantAndL5Deletion(
        profileResponse, productL3UpdateRequest, new ProductLevel3());
    });
    Mockito.verify(xProductOutbound).fetchL5ResponsesByItemSkus(List.of(ITEM_SKU, ITEM_SKU_1), 0, 2);
    Mockito.verify(pickupPointOutbound).getByPickupPointCodes(Constants.PBP,
      Collections.singletonList(PICKUP_POINT_ID));
  }



  @Test
  public void testValidateEligibilityForVariantAndL5Deletion_ItemsEligibleForDeletionEmptyContent() throws Exception {
    ReflectionTestUtils.setField(variantEditValidationService,
      "inventoryBatchUpdateDeleteBatchSize", 2);
    ReflectionTestUtils.setField(variantEditValidationService,
      "validateWarehouseDeletionEligibleSellers",
      BUSINESS_PARTNER_CODE.concat(Constants.COMMA).concat(BUSINESS_PARTNER_TYPE_CC));
    ReflectionTestUtils.setField(variantEditValidationService,
      "validateWarehouseVariantDeletionEnabled", true);
    ProductL3Response productL3Response = new ProductL3Response();
    productL3Response.setPickupPointCodes(List.of(PICKUP_POINT_ID));
    productL3UpdateRequest.setUpdatedBy(UPDATED_BY);
    ReflectionTestUtils.setField(variantEditValidationService, "l5InventoryFetchBatchSize", 2);
    profileResponse.setCompany(new CompanyDTO());
    profileResponse.getCompany().setMerchantType(BUSINESS_PARTNER_TYPE_CC);
    profileResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    ListRequestDTO<InventoryDetailInfoRequestDTO> inventoryDetailInfoRequestDTO =
      new ListRequestDTO<>();
    GdnRestListResponse<ItemPickupPointL5Response> itemResponse = new GdnRestListResponse();
    GdnRestListResponse<InventoryDetailInfoResponseV2DTO> inventoryResponseList =
      new GdnRestListResponse<>();
    ItemPickupPointL5Response l5Response1 =
      ItemPickupPointL5Response.builder().itemSku(ITEM_SKU).pickUpPointCode(PICKUP_POINT_ID)
        .build();
    ItemPickupPointL5Response l5Response2 =
      ItemPickupPointL5Response.builder().itemSku(ITEM_SKU_1).pickUpPointCode(PICKUP_POINT_ID)
        .build();
    itemResponse.setContent(List.of(l5Response1, l5Response2));
    PickupPointResponse pickupPointResponse =
      PickupPointResponse.builder().code(PICKUP_POINT_ID).fbbActivated(true).build();
    Mockito.when(pickupPointOutbound.getByPickupPointCodes(Constants.PBP,
      Collections.singletonList(PICKUP_POINT_ID))).thenReturn(Collections.singletonList(pickupPointResponse));
    List<String> deletedItems = List.of(ITEM_SKU, ITEM_SKU_1);
    InventoryDetailInfoResponseV2DTO inventoryResponse = new InventoryDetailInfoResponseV2DTO();
    WarehouseInventoryResponseDTO distributed = new WarehouseInventoryResponseDTO();
    distributed.setAvailableStock(0);
    inventoryResponse.setWarehouseInventoryResponseList(Collections.singletonList(distributed));
    inventoryResponseList.setContent(Collections.singletonList(inventoryResponse));
    inventoryResponseList.setSuccess(true);
    inventoryResponseList.setErrorMessage(null);
    itemResponse.setSuccess(true);
    itemResponse.setContent(Collections.emptyList());
    InventoryDetailInfoRequestDTO inventoryDetail = new InventoryDetailInfoRequestDTO();
    inventoryDetail.setWebMerchantCode(BUSINESS_PARTNER_CODE);
    inventoryDetail.setWebItemSku(ITEM_SKU);
    inventoryDetail.setPickupPointCode(PICKUP_POINT_ID);
    inventoryDetailInfoRequestDTO.setList(Collections.singletonList(inventoryDetail));
    List<DeletedProductItems> deletedProductItems = new ArrayList<>();
    DeletedProductItems item1 = new DeletedProductItems();
    item1.setItemSku(ITEM_SKU);
    DeletedProductItems item2 = new DeletedProductItems();
    item2.setItemSku(ITEM_SKU_1);
    deletedProductItems.add(item1);
    deletedProductItems.add(item2);
    productL3UpdateRequest.setDeletedProductItems(deletedProductItems);
    List<PickupPointDeleteRequest> deleteRequests = new ArrayList<>();
    PickupPointDeleteRequest pickupPointDeleteRequest =
      PickupPointDeleteRequest.builder().pickupPointId(PICKUP_POINT_ID).itemSku(ITEM_SKU).build();
    deleteRequests.add(pickupPointDeleteRequest);
    productL3UpdateRequest.setDeletePickupPoints(deleteRequests);
    Mockito.when(xProductOutbound.fetchL5ResponsesByItemSkus(deletedItems, 0, deletedItems.size())).thenReturn(itemResponse);
    Mockito.when(inventoryOutbound.findDetailByWebMerchantCodeAndWebItemSku(
      ProductLevel3InventoryUtil.generateMandatoryRequestParam(), inventoryDetailInfoRequestDTO)).thenReturn(inventoryResponseList);
    variantEditValidationService.validateEligibilityForVariantAndL5Deletion(profileResponse, productL3UpdateRequest,
      new ProductLevel3());
    Mockito.verify(xProductOutbound).fetchL5ResponsesByItemSkus(deletedItems, 0, 2);
    Mockito.verify(pickupPointOutbound).getByPickupPointCodes(Constants.PBP,
      Collections.singletonList(PICKUP_POINT_ID));
  }

  @Test
  public void testValidateEligibilityForVariantAndL5Deletion_ItemsEligibleForDeletionFailedResponse() throws Exception {
    ReflectionTestUtils.setField(variantEditValidationService,
      "inventoryBatchUpdateDeleteBatchSize", 2);
    ReflectionTestUtils.setField(variantEditValidationService,
      "validateWarehouseDeletionEligibleSellers",
      BUSINESS_PARTNER_CODE.concat(Constants.COMMA).concat(BUSINESS_PARTNER_TYPE_CC));
    ReflectionTestUtils.setField(variantEditValidationService,
      "validateWarehouseVariantDeletionEnabled", true);
    ProductL3Response productL3Response = new ProductL3Response();
    productL3Response.setPickupPointCodes(List.of(PICKUP_POINT_ID));
    productL3UpdateRequest.setUpdatedBy(UPDATED_BY);
    ReflectionTestUtils.setField(variantEditValidationService, "l5InventoryFetchBatchSize", 2);
    profileResponse.setCompany(new CompanyDTO());
    profileResponse.getCompany().setMerchantType(BUSINESS_PARTNER_TYPE_CC);
    profileResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    ListRequestDTO<InventoryDetailInfoRequestDTO> inventoryDetailInfoRequestDTO =
      new ListRequestDTO<>();
    GdnRestListResponse<ItemPickupPointL5Response> itemResponse = new GdnRestListResponse();
    GdnRestListResponse<InventoryDetailInfoResponseV2DTO> inventoryResponseList =
      new GdnRestListResponse<>();
    ItemPickupPointL5Response l5Response1 =
      ItemPickupPointL5Response.builder().itemSku(ITEM_SKU).pickUpPointCode(PICKUP_POINT_ID)
        .build();
    ItemPickupPointL5Response l5Response2 =
      ItemPickupPointL5Response.builder().itemSku(ITEM_SKU_1).pickUpPointCode(PICKUP_POINT_ID)
        .build();
    itemResponse.setContent(List.of(l5Response1, l5Response2));
    PickupPointResponse pickupPointResponse =
      PickupPointResponse.builder().code(PICKUP_POINT_ID).fbbActivated(true).build();
    Mockito.when(pickupPointOutbound.getByPickupPointCodes(Constants.PBP,
      Collections.singletonList(PICKUP_POINT_ID))).thenReturn(Collections.singletonList(pickupPointResponse));
    List<String> deletedItems = List.of(ITEM_SKU, ITEM_SKU_1);
    InventoryDetailInfoResponseV2DTO inventoryResponse = new InventoryDetailInfoResponseV2DTO();
    WarehouseInventoryResponseDTO distributed = new WarehouseInventoryResponseDTO();
    distributed.setAvailableStock(0);
    inventoryResponse.setWarehouseInventoryResponseList(Collections.singletonList(distributed));
    inventoryResponseList.setContent(Collections.singletonList(inventoryResponse));
    inventoryResponseList.setSuccess(true);
    inventoryResponseList.setErrorMessage(null);
    itemResponse.setSuccess(false);
    InventoryDetailInfoRequestDTO inventoryDetail = new InventoryDetailInfoRequestDTO();
    inventoryDetail.setWebMerchantCode(BUSINESS_PARTNER_CODE);
    inventoryDetail.setWebItemSku(ITEM_SKU);
    inventoryDetail.setPickupPointCode(PICKUP_POINT_ID);
    inventoryDetailInfoRequestDTO.setList(Collections.singletonList(inventoryDetail));
    List<DeletedProductItems> deletedProductItems = new ArrayList<>();
    DeletedProductItems item1 = new DeletedProductItems();
    item1.setItemSku(ITEM_SKU);
    DeletedProductItems item2 = new DeletedProductItems();
    item2.setItemSku(ITEM_SKU_1);
    deletedProductItems.add(item1);
    deletedProductItems.add(item2);
    productL3UpdateRequest.setDeletedProductItems(deletedProductItems);
    List<PickupPointDeleteRequest> deleteRequests = new ArrayList<>();
    PickupPointDeleteRequest pickupPointDeleteRequest =
      PickupPointDeleteRequest.builder().pickupPointId(PICKUP_POINT_ID).itemSku(ITEM_SKU).build();
    deleteRequests.add(pickupPointDeleteRequest);
    productL3UpdateRequest.setDeletePickupPoints(deleteRequests);
    Mockito.when(xProductOutbound.fetchL5ResponsesByItemSkus(deletedItems, 0, deletedItems.size())).thenReturn(itemResponse);
    Mockito.when(inventoryOutbound.findDetailByWebMerchantCodeAndWebItemSku(
      ProductLevel3InventoryUtil.generateMandatoryRequestParam(), inventoryDetailInfoRequestDTO)).thenReturn(inventoryResponseList);
    variantEditValidationService.validateEligibilityForVariantAndL5Deletion(profileResponse, productL3UpdateRequest,
      new ProductLevel3());
    Mockito.verify(xProductOutbound).fetchL5ResponsesByItemSkus(deletedItems, 0, 2);
    Mockito.verify(pickupPointOutbound).getByPickupPointCodes(Constants.PBP,
      Collections.singletonList(PICKUP_POINT_ID));
  }


  @Test
  public void testValidateEligibilityForVariantAndL5Deletion_ItemsInEligibleForDeletion() throws Exception {
    ReflectionTestUtils.setField(variantEditValidationService,
      "inventoryBatchUpdateDeleteBatchSize", 10);
    ReflectionTestUtils.setField(variantEditValidationService,
      "validateWarehouseDeletionEligibleSellers",
      BUSINESS_PARTNER_CODE.concat(Constants.COMMA).concat(BUSINESS_PARTNER_TYPE_CC));
    ReflectionTestUtils.setField(variantEditValidationService,
      "validateWarehouseVariantDeletionEnabled", true);
    productL3UpdateRequest.setUpdatedBy(UPDATED_BY);
    ReflectionTestUtils.setField(variantEditValidationService, "l5InventoryFetchBatchSize", 2);
    profileResponse.setCompany(new CompanyDTO());
    profileResponse.getCompany().setMerchantType(BUSINESS_PARTNER_TYPE_CC);
    profileResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    ListRequestDTO<InventoryDetailInfoRequestDTO> inventoryDetailInfoRequestDTO =
      new ListRequestDTO<>();
    GdnRestListResponse<InventoryDetailInfoResponseV2DTO> inventoryResponseList =
      new GdnRestListResponse<>();
    InventoryDetailInfoResponseV2DTO inventoryResponse = new InventoryDetailInfoResponseV2DTO();
    WarehouseInventoryResponseDTO distributed = new WarehouseInventoryResponseDTO();
    distributed.setAvailableStock(2);
    inventoryResponse.setWarehouseInventoryResponseList(Collections.singletonList(distributed));
    inventoryResponseList.setContent(Collections.singletonList(inventoryResponse));
    inventoryResponseList.setSuccess(true);
    InventoryDetailInfoRequestDTO inventoryDetail = new InventoryDetailInfoRequestDTO();
    inventoryDetail.setWebMerchantCode(BUSINESS_PARTNER_CODE);
    inventoryDetail.setWebItemSku(ITEM_SKU);
    inventoryDetail.setPickupPointCode(PICKUP_POINT_ID);
    inventoryDetailInfoRequestDTO.setList(Collections.singletonList(inventoryDetail));
    List<DeletedProductItems> deletedProductItems = new ArrayList<>();
    DeletedProductItems item1 = new DeletedProductItems();
    item1.setItemSku(ITEM_SKU);
    DeletedProductItems item2 = new DeletedProductItems();
    item2.setItemSku(ITEM_SKU_1);
    deletedProductItems.add(item1);
    deletedProductItems.add(item2);
    productL3UpdateRequest.setDeletedProductItems(deletedProductItems);
    List<PickupPointDeleteRequest> deleteRequests = new ArrayList<>();
    PickupPointDeleteRequest pickupPointDeleteRequest =
      PickupPointDeleteRequest.builder().pickupPointId(PICKUP_POINT_ID).itemSku(ITEM_SKU).build();
    deleteRequests.add(pickupPointDeleteRequest);
    productL3UpdateRequest.setDeletePickupPoints(deleteRequests);
    PickupPointResponse pickupPointResponse =
      PickupPointResponse.builder().code(PICKUP_POINT_ID).fbbActivated(true).build();
    Mockito.when(pickupPointOutbound.getByPickupPointCodes(Constants.PBP,
        Collections.singletonList(PICKUP_POINT_ID)))
      .thenReturn(Collections.singletonList(pickupPointResponse));

    Mockito.when(inventoryOutbound.findDetailByWebMerchantCodeAndWebItemSku(
        ProductLevel3InventoryUtil.generateMandatoryRequestParam(), inventoryDetailInfoRequestDTO))
      .thenReturn(inventoryResponseList);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        variantEditValidationService.validateEligibilityForVariantAndL5Deletion(profileResponse,
          productL3UpdateRequest, new ProductLevel3());
      });
    } finally {
      Mockito.verify(pickupPointOutbound)
        .getByPickupPointCodes(Constants.PBP, Collections.singletonList(PICKUP_POINT_ID));
      Mockito.verify(inventoryOutbound).findDetailByWebMerchantCodeAndWebItemSku(
        ProductLevel3InventoryUtil.generateMandatoryRequestParam(), inventoryDetailInfoRequestDTO);
    }

  }

  @Test
  public void testValidateEligibilityForVariantAndL5Deletion_ValidateVariantsDeletion() throws Exception {
    ReflectionTestUtils.setField(variantEditValidationService,
      "inventoryBatchUpdateDeleteBatchSize", 1);
    ReflectionTestUtils.setField(variantEditValidationService,
      "validateWarehouseDeletionEligibleSellers",
      BUSINESS_PARTNER_CODE.concat(Constants.COMMA).concat(BUSINESS_PARTNER_TYPE_CC));
    ReflectionTestUtils.setField(variantEditValidationService,
      "validateWarehouseVariantDeletionEnabled", true);
    ProductLevel3 response = new ProductLevel3();
    GdnRestListResponse<ItemPickupPointL5Response> l5Response = new GdnRestListResponse();
    l5Response.setContent(Collections.emptyList());
    l5Response.setSuccess(true);
    profileResponse.setBusinessPartnerCode(MERCHANT_CODE);
    response.setPickupPointCodes(List.of(PICKUP_POINT_ID));
    ReflectionTestUtils.setField(variantEditValidationService, "l5InventoryFetchBatchSize", 2);
    profileResponse.setCompany(new CompanyDTO());
    profileResponse.getCompany().setMerchantType(BUSINESS_PARTNER_TYPE_CC);
    productL3UpdateRequest.setUpdatedBy(UPDATED_BY);
    DeletedProductItems productItems =
      DeletedProductItems.builder().itemCode(ITEM_CODE).itemSku(ITEM_SKU).build();
    productL3UpdateRequest.setDeletedProductItems(Collections.singletonList(productItems));
    List<String> deletedItems = List.of(ITEM_SKU);
    Mockito.when(xProductOutbound.fetchL5ResponsesByItemSkus(deletedItems, 0, deletedItems.size())).thenReturn(l5Response);
    variantEditValidationService.validateEligibilityForVariantAndL5Deletion(profileResponse, productL3UpdateRequest,
      response);
    Mockito.verify(xProductOutbound).fetchL5ResponsesByItemSkus(deletedItems, 0, 2);
  }

  @Test
  public void testValidateEligibilityForVariantAndL5Deletion_ValidateVariantsDeletionWithInventoryResponse() throws Exception {
    ReflectionTestUtils.setField(variantEditValidationService,
      "inventoryBatchUpdateDeleteBatchSize", 10);
    ReflectionTestUtils.setField(variantEditValidationService,
      "validateWarehouseDeletionEligibleSellers",
      BUSINESS_PARTNER_CODE.concat(Constants.COMMA).concat(BUSINESS_PARTNER_TYPE_CC));
    ReflectionTestUtils.setField(variantEditValidationService,
      "validateWarehouseVariantDeletionEnabled", true);
    ProductLevel3 response = new ProductLevel3();
    GdnRestListResponse<ItemPickupPointL5Response> l5Response = new GdnRestListResponse<>();
    response.setPickupPointCodes(List.of(PICKUP_POINT_ID));
    ReflectionTestUtils.setField(variantEditValidationService, "l5InventoryFetchBatchSize", 2);
    profileResponse.setCompany(new CompanyDTO());
    profileResponse.getCompany().setMerchantType(BUSINESS_PARTNER_TYPE_CC);
    profileResponse.setBusinessPartnerCode(MERCHANT_CODE);
    productL3UpdateRequest.setUpdatedBy(UPDATED_BY);
    DeletedProductItems productItems =
      DeletedProductItems.builder().itemCode(ITEM_CODE).itemSku(ITEM_SKU).build();
    productL3UpdateRequest.setDeletedProductItems(Collections.singletonList(productItems));
    ItemPickupPointL5Response l5Response1 =
      ItemPickupPointL5Response.builder().itemSku(ITEM_SKU).pickUpPointCode(PICKUP_POINT_ID)
        .build();
    l5Response.setSuccess(true);
    l5Response.setContent(List.of(l5Response1));
    List<String> deletedItems = List.of(ITEM_SKU);
    Mockito.lenient().when(xProductOutbound.fetchL5ResponsesByItemSkus(deletedItems, 0, 2)).thenReturn(l5Response);
    L2StockAvailabilityDTO zeroStockDto = new L2StockAvailabilityDTO();
    zeroStockDto.setDistributionWarehouseAvailable(false);

    Mockito.when(inventoryOutbound.findStockAvailabilityByWarehouseItemSku(
        ProductLevel3InventoryUtil.generateMandatoryRequestParam(), MERCHANT_CODE, ITEM_CODE))
      .thenReturn(zeroStockDto);
    variantEditValidationService.validateEligibilityForVariantAndL5Deletion(profileResponse, productL3UpdateRequest,
      response);
    Mockito.verify(xProductOutbound).fetchL5ResponsesByItemSkus(deletedItems, 0, 2);
    Mockito.verify(inventoryOutbound).findStockAvailabilityByWarehouseItemSku(
        ProductLevel3InventoryUtil.generateMandatoryRequestParam(), MERCHANT_CODE, ITEM_CODE);
  }

  @Test
  public void testValidateEligibilityForVariantAndL5Deletion_MultipleL5Pages() throws Exception {
    ReflectionTestUtils.setField(variantEditValidationService, "inventoryBatchUpdateDeleteBatchSize", 2);
    ReflectionTestUtils.setField(variantEditValidationService, "validateWarehouseDeletionEligibleSellers",
      BUSINESS_PARTNER_CODE.concat(Constants.COMMA).concat(BUSINESS_PARTNER_TYPE_CC));
    ReflectionTestUtils.setField(variantEditValidationService, "validateWarehouseVariantDeletionEnabled", true);
    ReflectionTestUtils.setField(variantEditValidationService, "l5InventoryFetchBatchSize", 2);

    profileResponse.setCompany(new CompanyDTO());
    profileResponse.getCompany().setMerchantType(BUSINESS_PARTNER_TYPE_CC);
    profileResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);

    productL3UpdateRequest.setUpdatedBy(UPDATED_BY);

    List<DeletedProductItems> deletedProductItems = new ArrayList<>();
    DeletedProductItems item1 = new DeletedProductItems();
    item1.setItemSku(ITEM_SKU);
    item1.setItemCode("ITEM_CODE_1");
    DeletedProductItems item2 = new DeletedProductItems();
    item2.setItemSku(ITEM_SKU_1);
    item2.setItemCode("ITEM_CODE_2");
    deletedProductItems.add(item1);
    deletedProductItems.add(item2);
    productL3UpdateRequest.setDeletedProductItems(deletedProductItems);

    List<PickupPointDeleteRequest> deleteRequests = new ArrayList<>();
    PickupPointDeleteRequest pickupPointDeleteRequest =
      PickupPointDeleteRequest.builder().pickupPointId(PICKUP_POINT_ID).itemSku(ITEM_SKU).build();
    deleteRequests.add(pickupPointDeleteRequest);
    productL3UpdateRequest.setDeletePickupPoints(deleteRequests);

    // ---- Page 0 Response
    ItemPickupPointL5Response l5Response1 = ItemPickupPointL5Response.builder()
      .itemSku(ITEM_SKU).pickUpPointCode(PICKUP_POINT_ID).build();

    GdnRestListResponse<ItemPickupPointL5Response> page0 = new GdnRestListResponse<>();
    page0.setContent(List.of(l5Response1));
    page0.setSuccess(true);
    PageMetaData meta0 = new PageMetaData();
    meta0.setPageNumber(1); // page 0 (0-based) + 1 for hasNextPage math
    meta0.setPageSize(2);
    meta0.setTotalRecords(3); // 2 < 3 → should go for another page
    page0.setPageMetaData(meta0);

    // ---- Page 1 Response
    ItemPickupPointL5Response l5Response2 = ItemPickupPointL5Response.builder()
      .itemSku(ITEM_SKU_1).pickUpPointCode(PICKUP_POINT_ID).build();

    GdnRestListResponse<ItemPickupPointL5Response> page1 = new GdnRestListResponse<>();
    page1.setContent(List.of(l5Response2));
    page1.setSuccess(true);
    PageMetaData meta1 = new PageMetaData();
    meta1.setPageNumber(2); // page 1 (0-based) + 1
    meta1.setPageSize(2);
    meta1.setTotalRecords(3); // 2 * 2 = 4 > 3 → no next page
    page1.setPageMetaData(meta1);

    // Set up mock paged responses
    List<String> deletedItems = List.of(ITEM_SKU, ITEM_SKU_1);
    Mockito.when(xProductOutbound.fetchL5ResponsesByItemSkus(deletedItems, 0, 2)).thenReturn(page0);
    Mockito.when(xProductOutbound.fetchL5ResponsesByItemSkus(deletedItems, 1, 2)).thenReturn(page1);

    // Pickup Point Mock
    PickupPointResponse pickupPointResponse = PickupPointResponse.builder()
      .code(PICKUP_POINT_ID).fbbActivated(true).build();
    Mockito.when(pickupPointOutbound.getByPickupPointCodes(Constants.PBP,
      Collections.singletonList(PICKUP_POINT_ID))).thenReturn(Collections.singletonList(pickupPointResponse));

    // Inventory Mock
    InventoryDetailInfoResponseV2DTO inventoryResponse = new InventoryDetailInfoResponseV2DTO();
    WarehouseInventoryResponseDTO distributed = new WarehouseInventoryResponseDTO();
    distributed.setAvailableStock(0);
    inventoryResponse.setWarehouseInventoryResponseList(Collections.singletonList(distributed));

    GdnRestListResponse<InventoryDetailInfoResponseV2DTO> inventoryResponseList = new GdnRestListResponse<>();
    inventoryResponseList.setContent(Collections.singletonList(inventoryResponse));
    inventoryResponseList.setSuccess(true);

    ListRequestDTO<InventoryDetailInfoRequestDTO> inventoryDetailInfoRequestDTO = new ListRequestDTO<>();
    InventoryDetailInfoRequestDTO inventoryDetail = new InventoryDetailInfoRequestDTO();
    inventoryDetail.setWebMerchantCode(BUSINESS_PARTNER_CODE);
    inventoryDetail.setWebItemSku(ITEM_SKU);
    inventoryDetail.setPickupPointCode(PICKUP_POINT_ID);
    inventoryDetailInfoRequestDTO.setList(Collections.singletonList(inventoryDetail));

    Mockito.when(inventoryOutbound.findDetailByWebMerchantCodeAndWebItemSku(
        ProductLevel3InventoryUtil.generateMandatoryRequestParam(), inventoryDetailInfoRequestDTO))
      .thenReturn(inventoryResponseList);

    // Run the method
    variantEditValidationService.validateEligibilityForVariantAndL5Deletion(
      profileResponse, productL3UpdateRequest, new ProductLevel3());

    // Verify both pages fetched
    Mockito.verify(xProductOutbound).fetchL5ResponsesByItemSkus(deletedItems, 0, 2);
    Mockito.verify(xProductOutbound).fetchL5ResponsesByItemSkus(deletedItems, 1, 2);

    Mockito.verify(pickupPointOutbound).getByPickupPointCodes(Constants.PBP,
      Collections.singletonList(PICKUP_POINT_ID));
  }



}

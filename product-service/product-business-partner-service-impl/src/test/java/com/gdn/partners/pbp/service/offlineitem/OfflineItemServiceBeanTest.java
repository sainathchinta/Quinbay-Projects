package com.gdn.partners.pbp.service.offlineitem;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.initMocks;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.log4j.MDC;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.mta.product.service.config.KafkaPublisher;
import com.gda.mta.product.dto.response.AuditTrailListRequest;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.product.entity.ProductSystemParameter;
import com.gdn.mta.product.entity.UpdatedProductHistory;
import com.gdn.mta.product.repository.BusinessPartnerRepository;
import com.gdn.mta.product.repository.CategoryRepository;
import com.gdn.mta.product.repository.OfflineItemRepository;
import com.gdn.mta.product.repository.PickupPointRepository;
import com.gdn.mta.product.service.ProductLevel3Service;
import com.gdn.mta.product.service.ProductService;
import com.gdn.mta.product.service.ProductSystemParameterService;
import com.gdn.mta.product.service.UpdatedProductHistoryService;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.commons.constants.SystemParameterConstants;
import com.gdn.partners.pbp.converter.OfflineItemConverter;
import com.gdn.partners.pbp.dto.offlineitem.DeleteOfflineItemDetailResponse;
import com.gdn.partners.pbp.dto.offlineitem.FailedOfflineItemResponse;
import com.gdn.partners.pbp.dto.offlineitem.OfflineItemInstantPickupV2Response;
import com.gdn.partners.pbp.dto.offlineitem.OfflineProductResponse;
import com.gdn.partners.pbp.dto.offlineitem.UpsertOfflineItemDetailResponse;
import com.gdn.partners.pbp.dto.offlineitem.UpsertOfflineItemFailedResponse;
import com.gdn.partners.pbp.model.offlineitem.DeleteOfflineItem;
import com.gdn.partners.pbp.model.offlineitem.OfflineItem;
import com.gdn.partners.pbp.model.offlineitem.UpsertOfflineItem;
import com.gdn.partners.pbp.model.offlineitem.ValidOfflineItem;
import com.gdn.partners.pbp.outbound.inventory.InventoryOutbound;
import com.gdn.partners.pbp.outbound.pickuppoint.PickupPointOutbound;
import com.gdn.partners.pbp.outbound.product.ProductOutbound;
import com.gdn.partners.pbp.outbound.xProduct.XProductOutbound;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.ExternalPickupPointCodeResponse;
import com.gdn.x.businesspartner.dto.ExternalPickupPointCodeResponseDetail;
import com.gdn.x.businesspartner.dto.PickupPointDTO;
import com.gdn.x.businesspartner.dto.PickupPointResponse;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.product.rest.web.model.dto.CategoryDTO;
import com.gdn.x.product.rest.web.model.dto.ItemSummaryResponse;
import com.gdn.x.product.rest.web.model.dto.MasterCatalogDTO;
import com.gdn.x.product.rest.web.model.dto.OfflineItemPriceDTO;
import com.gdn.x.product.rest.web.model.request.DeleteOfflineItemRequest;
import com.gdn.x.product.rest.web.model.request.ItemSummaryRequest;
import com.gdn.x.product.rest.web.model.request.UpdateOfflineItemPriceRequest;
import com.gdn.x.product.rest.web.model.response.DeleteOfflineItemResponse;
import com.gdn.x.product.rest.web.model.response.OfflineItemPriceResponse;
import com.gdn.x.product.rest.web.model.response.OfflineItemResponse;
import com.gdn.x.product.rest.web.model.response.OfflineItemResponseDetail;
import com.gdn.x.product.rest.web.model.response.UpsertOfflineItemPriceResponse;

public class OfflineItemServiceBeanTest {

  @InjectMocks
  private OfflineItemServiceBean offlineItemService;

  @Mock
  private ProductOutbound productOutbound;

  @Mock
  private PickupPointOutbound pickupPointOutbound;

  @Mock
  private InventoryOutbound inventoryOutbound;

  @Mock
  private OfflineItemConverter offlineItemConverter;

  @Mock
  protected CategoryRepository categoryRepository;

  @Mock
  protected PickupPointRepository pickupPointRepository;

  @Mock
  private OfflineItemRepository offlineItemRepository;

  @Mock
  private BusinessPartnerRepository businessPartnerRepository;

  @Mock
  private OfflineItemHelperService offlineItemHelperService;

  @Mock
  private XProductOutbound xProductOutbound;

  @Mock
  private ProductSystemParameterService productSystemParameterService;

  @Mock
  private ProductService productService;

  @Mock
  private ProductLevel3Service productLevel3Service;

  @Mock
  private UpdatedProductHistoryService updatedProductHistoryService;

  @Mock
  private KafkaPublisher kafkaProducer;

  private List<OfflineItem> offlineItems = new ArrayList();
  private List<OfflineItemResponseDetail> offlineItemResponseDetails = new ArrayList();
  private List<ExternalPickupPointCodeResponseDetail> externalPickupPointCodeResponseDetails =
      new ArrayList<>();
  private GdnRestSingleResponse<OfflineItemResponse> productClientResponse;
  private GdnRestSingleResponse<ExternalPickupPointCodeResponse> pickupPointClientResponse;
  private List<ValidOfflineItem> validOfflineItems = new ArrayList();
  private GdnBaseRestResponse successResponse;
  private List<UpsertOfflineItemPriceResponse> upsertOfflineItemPriceResponses;
  private MandatoryRequestParam mandatoryRequestParam;

  private FailedOfflineItemResponse errorXProductResponse;
  private FailedOfflineItemResponse errorXBPResponse;

  private OfflineItemPriceResponse offlineItemPriceResponse;
  private PickupPointDTO pickupPointDTO;
  private ProfileResponse profileResponse;
  private ValidOfflineItem validOfflineItem;

  private DeleteOfflineItem deleteOfflineItem;
  private List<DeleteOfflineItem> deleteOfflineItems;
  private DeleteOfflineItemRequest deleteOfflineItemRequest;
  private List<DeleteOfflineItemRequest> deleteOfflineItemRequests;
  private OfflineItem duplicateMerchantSku;
  private OfflineItem dataNotFoundMerchantSku;
  private PickupPointResponse pickupPointResponse;
  private PickupPointResponse pickupPointResponse2;
  private List<PickupPointResponse> pickupPointResponses;

  private ItemSummaryResponse itemSummaryResponse;
  private List<ItemSummaryResponse> itemSummaryResponses;
  private OfflineItemPriceDTO offlineItemPriceDTO;
  private List<OfflineItemPriceDTO> offlinePrices;

  private CompanyDTO company;
  private List<String> validPickupPointCodes;
  private UpsertOfflineItem upsertOfflineItem;
  private List<UpsertOfflineItem> upsertOfflineItems;
  private List<UpsertOfflineItemFailedResponse> failedUpsertOfflineItemResponses;

  private List<String> merchantSkus;
  private OfflineItemResponse offlineItemResponse;
  private OfflineItemResponseDetail offlineItemResponseDetail;

  public static final String COMMA_DELIMITER = ",";
  public static final String NOT_FOUND_IN_XPRODUCT_ERROR = "NOT_FOUND_IN_XPRODUCT";
  public static final String NOT_FOUND_IN_BUSINESS_PARTNER_ERROR = "NOT_FOUND_IN_BUSINESS_PARTNER";

  private static final String STORE_ID = "storeId";
  private static final String CHANNEL_ID = "channelId";
  private static final String CLIENT_ID = "clientId";
  private static final String AUTHENTICATOR = "authenticator";

  private static final String REQUEST_ID = "request-id";
  private static final String USERNAME = "username";
  private static final String MERCHANT_CODE = "merchant-code";
  private static final String MERCHANT_SKU = "merchant-sku";
  private static final String MERCHANT_SKU2 = "merchant-sku2";
  private static final List<String> MERCHANT_SKUS = Collections.singletonList(MERCHANT_SKU);
  private static final String EXTERNAL_PICKUP_POINT_CODE = "external-pickup-point-code";
  private static final String FAILED_MERCHANT_SKU = "failed-merchant-sku";
  private static final String DUPLICATE_MERCHANT_SKU = "duplicate-merchant-sku";
  private static final String FAILED_EXTERNAL_PICKUP_POINT_CODE =
      "failed-external-pickup-point-code";
  private static final String ITEM_SKU = "item-sku";
  private static final String PRODUCT_SKU = "product-sku";
  private static final String ITEM_SKU2 = "item-sku2";
  private static final String PICKUP_POINT_CODE = "pickup-point-code";
  private static final String PICKUP_POINT_CODE2 = "pickup-point-code2";
  private static final String PICKUP_POINT_CODES = PICKUP_POINT_CODE + COMMA_DELIMITER + PICKUP_POINT_CODE2;
  private static final List<String> PICKUP_POINT_CODE_LIST = Arrays.asList(PICKUP_POINT_CODE, PICKUP_POINT_CODE2);
  private static final int SAFETY_STOCK = 1;
  private static final String FILE_NAME = "file-name";
  private static final String ERROR_MESSAGE = "errorMessage";
  private static final String ERROR_DUPLICATE_MERCHANT_SKU = "ERROR_DUPLICATE_MERCHANT_SKU";
  private static final String ERROR_DUPLICATE_PICKUP_POINT_CODE = "ERROR_DUPLICATE_PICKUP_POINT_CODE";
  private static final String DUPLICATE_EXTERNAL_PICKUP_POINT_CODE = "duplicate-pickup-point-code";
  private static final String OFFLINE_INVENTORY_ID = ITEM_SKU + "-" + PICKUP_POINT_CODE;

  private static final Integer PAGE = 0;
  private static final Integer SIZE = 10;
  private static final String ORDER_BY = "orderBy";
  private static final String SORT_BY = "sortBy";
  private static final String ITEM_NAME = "item-name";
  private static final String CATEGORY_CODE = "category-code";
  private static final String GDN_SKU = "gdn-sku";
  private static final String GDN_NAME = "gdn-name";
  private static final String ACTIVITY = "Activity";
  private static final String OLD_VALUES = "oldValues";
  private static final String NEW_VALUES = "oldValues";
  private static final String CHANGED_BY = "changedBy";
  private static final String ATTRIBUTE = "attribute";
  private static final List<String> GDN_SKUS = Collections.singletonList(GDN_SKU);
  private static final String CATEGORY_GROUP_ID = "category-group-id";
  private static final String CATEGORY_NAME = "category-name";

  private static final String BUSINESS_PARTNER_CODE = "business-partner-code";
  private static final String PICKUP_POINT_NAME = "pickup-point-name";
  private static final String PICKUP_POINT_NAME2 = "pickup-point-name2";
  private static final Double LIST_PRICE = 222222.0;
  private static final Double OFFER_PRICE = 111111.0;
  private static final Integer ORIGINAL_STOCK = 11;
  private static final Integer STOCK = 11;

  private static final String TRUE_VALUE = "true";
  private static final String FALSE_VALUE = "false";

  @BeforeEach
  public void setUp() throws Exception {
    initMocks(this);

    ReflectionTestUtils.setField(offlineItemService, "pickupPointCodeFetchSize", 1000);

    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER,
        OfflineItemServiceBeanTest.STORE_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
        OfflineItemServiceBeanTest.CHANNEL_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER,
        OfflineItemServiceBeanTest.CLIENT_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
        OfflineItemServiceBeanTest.REQUEST_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER,
        OfflineItemServiceBeanTest.USERNAME);
    MDC.put(GdnMandatoryRequestParameterUtil.AUTHENTICATOR_KEY,
        OfflineItemServiceBeanTest.AUTHENTICATOR);

    this.mandatoryRequestParam = MandatoryRequestParam
        .generateMandatoryRequestParam(OfflineItemServiceBeanTest.STORE_ID,
            OfflineItemServiceBeanTest.CHANNEL_ID, OfflineItemServiceBeanTest.CLIENT_ID,
            OfflineItemServiceBeanTest.REQUEST_ID, OfflineItemServiceBeanTest.USERNAME,
            OfflineItemServiceBeanTest.AUTHENTICATOR);

    //success request
    OfflineItem offlineItem = new OfflineItem();
    offlineItem.setMerchantSku(MERCHANT_SKU);
    offlineItem.setExternalPickupPointCode(EXTERNAL_PICKUP_POINT_CODE);
    offlineItem.setFileName(FILE_NAME);
    offlineItems.add(offlineItem);

    //duplicate merchant sku
    duplicateMerchantSku = new OfflineItem();
    duplicateMerchantSku.setMerchantSku(DUPLICATE_MERCHANT_SKU);
    duplicateMerchantSku.setExternalPickupPointCode(DUPLICATE_EXTERNAL_PICKUP_POINT_CODE);
    duplicateMerchantSku.setFileName(FILE_NAME);
    offlineItems.add(duplicateMerchantSku);

    //failed request
    dataNotFoundMerchantSku = new OfflineItem();
    dataNotFoundMerchantSku.setMerchantSku(FAILED_MERCHANT_SKU);
    dataNotFoundMerchantSku.setExternalPickupPointCode(FAILED_EXTERNAL_PICKUP_POINT_CODE);
    dataNotFoundMerchantSku.setFileName(FILE_NAME);
    offlineItems.add(dataNotFoundMerchantSku);

    //response
    OfflineItemResponseDetail offlineItemResponseDetail = new OfflineItemResponseDetail();
    offlineItemResponseDetail.setItemSku(ITEM_SKU);
    offlineItemResponseDetail.setMerchantSku(MERCHANT_SKU);
    offlineItemResponseDetails.add(offlineItemResponseDetail);

    offlineItemResponseDetail = new OfflineItemResponseDetail();
    offlineItemResponseDetail.setItemSku(ITEM_SKU);
    offlineItemResponseDetail.setMerchantSku(DUPLICATE_MERCHANT_SKU);
    offlineItemResponseDetail.setErrorCode(ERROR_DUPLICATE_MERCHANT_SKU);
    offlineItemResponseDetails.add(offlineItemResponseDetail);

    //failed response
    offlineItemResponseDetail = new OfflineItemResponseDetail();
    offlineItemResponseDetail.setItemSku(null);
    offlineItemResponseDetail.setMerchantSku(FAILED_MERCHANT_SKU);
    offlineItemResponseDetails.add(offlineItemResponseDetail);

    OfflineItemResponse offlineItemResponse = new OfflineItemResponse();
    offlineItemResponse.setOfflineProducts(offlineItemResponseDetails);

    //BP response
    ExternalPickupPointCodeResponseDetail externalPickupPointCodeResponseDetail =
        new ExternalPickupPointCodeResponseDetail();
    externalPickupPointCodeResponseDetail.setCode(PICKUP_POINT_CODE);
    externalPickupPointCodeResponseDetail.setExternalPickupPointCode(EXTERNAL_PICKUP_POINT_CODE);
    externalPickupPointCodeResponseDetails.add(externalPickupPointCodeResponseDetail);

    //failed BP Respons duplicate pickup point code
    externalPickupPointCodeResponseDetail = new ExternalPickupPointCodeResponseDetail();
    externalPickupPointCodeResponseDetail.setCode(PICKUP_POINT_CODE);
    externalPickupPointCodeResponseDetail
        .setExternalPickupPointCode(DUPLICATE_EXTERNAL_PICKUP_POINT_CODE);
    externalPickupPointCodeResponseDetail.setErrorCode(ERROR_DUPLICATE_PICKUP_POINT_CODE);
    externalPickupPointCodeResponseDetails.add(externalPickupPointCodeResponseDetail);

    //failed BP Respons
    externalPickupPointCodeResponseDetail = new ExternalPickupPointCodeResponseDetail();
    externalPickupPointCodeResponseDetail.setCode(null);
    externalPickupPointCodeResponseDetail
        .setExternalPickupPointCode(FAILED_EXTERNAL_PICKUP_POINT_CODE);
    externalPickupPointCodeResponseDetails.add(externalPickupPointCodeResponseDetail);

    ExternalPickupPointCodeResponse externalPickupPointCodeResponse =
        new ExternalPickupPointCodeResponse();
    externalPickupPointCodeResponse
        .setExternalPickupPointCodeResponseDetails(externalPickupPointCodeResponseDetails);

    productClientResponse = new GdnRestSingleResponse<>(offlineItemResponse, REQUEST_ID);
    pickupPointClientResponse =
        new GdnRestSingleResponse<>(externalPickupPointCodeResponse, REQUEST_ID);

    validOfflineItem = new ValidOfflineItem();
    validOfflineItem.setMerchantSku(MERCHANT_SKU);
    validOfflineItem.setPickupPointCode(PICKUP_POINT_CODE);
    validOfflineItem.setItemSku(ITEM_SKU);
    validOfflineItem.setExternalPickupPointCode(EXTERNAL_PICKUP_POINT_CODE);
    validOfflineItem.setListPrice(LIST_PRICE);
    validOfflineItem.setPrice(OFFER_PRICE);
    validOfflineItem.setFileName(FILE_NAME);
    validOfflineItem.setStock(1);

    validOfflineItems.add(validOfflineItem);

    successResponse = new GdnBaseRestResponse();
    successResponse.setSuccess(true);

    UpsertOfflineItemPriceResponse offlineItemPriceResponse = new UpsertOfflineItemPriceResponse();
    offlineItemPriceResponse.setSuccess(true);
    offlineItemPriceResponse.setItemSku(ITEM_SKU);
    offlineItemPriceResponse.setMerchantSku(MERCHANT_SKU);
    offlineItemPriceResponse.setPickupPointCode(PICKUP_POINT_CODE);
    offlineItemPriceResponse.setExternalPickupPointCode(EXTERNAL_PICKUP_POINT_CODE);
    offlineItemPriceResponse.setListPrice(LIST_PRICE);
    offlineItemPriceResponse.setOfferPrice(OFFER_PRICE);

    upsertOfflineItemPriceResponses = new ArrayList<>();
    upsertOfflineItemPriceResponses.add(offlineItemPriceResponse);


    errorXProductResponse = new FailedOfflineItemResponse();
    errorXProductResponse.setErrorCode(NOT_FOUND_IN_XPRODUCT_ERROR);

    errorXBPResponse = new FailedOfflineItemResponse();
    errorXBPResponse.setMerchantSku(MERCHANT_SKU);
    errorXBPResponse.setExternalPickupPointCode(EXTERNAL_PICKUP_POINT_CODE);
    errorXBPResponse.setErrorCode(NOT_FOUND_IN_BUSINESS_PARTNER_ERROR);

    this.offlineItemPriceResponse = new OfflineItemPriceResponse();
    this.offlineItemPriceResponse.setPickupPointCode(PICKUP_POINT_CODE);
    this.offlineItemPriceResponse.setListPrice(LIST_PRICE);
    this.offlineItemPriceResponse.setOfferPrice(OFFER_PRICE);

    this.pickupPointDTO = new PickupPointDTO();
    this.pickupPointDTO.setCode(PICKUP_POINT_CODE);
    this.pickupPointDTO.setName(PICKUP_POINT_NAME);

    this.profileResponse = new ProfileResponse();
    this.profileResponse.setPickupPoints(Arrays.asList(this.pickupPointDTO));

    this.deleteOfflineItems = new ArrayList<>();
    this.deleteOfflineItem = new DeleteOfflineItem();
    this.deleteOfflineItem.setItemSku(ITEM_SKU);
    this.deleteOfflineItem.setMerchantSku(MERCHANT_SKU);
    this.deleteOfflineItem.setPickupPointCode(PICKUP_POINT_CODE);
    this.deleteOfflineItems.add(this.deleteOfflineItem);

    this.deleteOfflineItemRequests = new ArrayList<>();
    this.deleteOfflineItemRequest = new DeleteOfflineItemRequest();
    this.deleteOfflineItemRequest.setItemSku(ITEM_SKU);
    this.deleteOfflineItemRequest.setMerchantSku(MERCHANT_SKU);
    this.deleteOfflineItemRequest.setPickupPointCode(PICKUP_POINT_CODE);
    this.deleteOfflineItemRequests.add(this.deleteOfflineItemRequest);

    this.pickupPointResponses = new ArrayList<>();
    this.pickupPointResponse = new PickupPointResponse();
    this.pickupPointResponse.setCode(PICKUP_POINT_CODE);
    this.pickupPointResponse.setName(PICKUP_POINT_NAME);
    this.pickupPointResponse2 = new PickupPointResponse();
    this.pickupPointResponse2.setCode(PICKUP_POINT_CODE2);
    this.pickupPointResponse2.setName(PICKUP_POINT_NAME2);
    this.pickupPointResponses.add(pickupPointResponse);
    this.pickupPointResponses.add(pickupPointResponse2);

    this.itemSummaryResponse = new ItemSummaryResponse();
    this.itemSummaryResponse.setMerchantCode(OfflineItemServiceBeanTest.MERCHANT_CODE);
    this.itemSummaryResponse.setItemSku(OfflineItemServiceBeanTest.ITEM_SKU);
    this.itemSummaryResponse.setMerchantSku(OfflineItemServiceBeanTest.MERCHANT_SKU);
    this.itemSummaryResponse.setProductSku(PRODUCT_SKU);

    this.offlineItemPriceDTO = new OfflineItemPriceDTO();
    this.offlineItemPriceDTO.setPickupPointCode(PICKUP_POINT_CODE);
    this.offlineItemPriceDTO.setListPrice(LIST_PRICE);
    this.offlineItemPriceDTO.setOfferPrice(OFFER_PRICE);

    this.offlinePrices = new ArrayList<>();
    this.offlinePrices.add(offlineItemPriceDTO);

    this.itemSummaryResponse.setOfflinePrices(offlinePrices);

    this.itemSummaryResponses = new ArrayList<>();
    this.itemSummaryResponses.add(itemSummaryResponse);

    this.merchantSkus = new ArrayList<>();
    this.merchantSkus.add(MERCHANT_SKU);

    this.offlineItemResponse = new OfflineItemResponse();
    this.offlineItemResponseDetail = new OfflineItemResponseDetail();
    this.offlineItemResponse.setOfflineProducts(Arrays.asList(this.offlineItemResponseDetail));

    ProductSystemParameter productSystemParameter = ProductSystemParameter.builder()
        .value(FALSE_VALUE)
        .build();
    Mockito.when(this.productSystemParameterService.findByStoreIdAndVariable(STORE_ID,
        SystemParameterConstants.AVOID_INVENTORY_UPDATE_FLAG)).thenReturn(productSystemParameter);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(productOutbound);
    Mockito.verifyNoMoreInteractions(pickupPointOutbound);
    Mockito.verifyNoMoreInteractions(inventoryOutbound);
    Mockito.verifyNoMoreInteractions(offlineItemConverter);
    Mockito.verifyNoMoreInteractions(offlineItemRepository);
    Mockito.verifyNoMoreInteractions(businessPartnerRepository);
    Mockito.verifyNoMoreInteractions(offlineItemHelperService);
    Mockito.verifyNoMoreInteractions(xProductOutbound);
    Mockito.verifyNoMoreInteractions(productSystemParameterService);
    Mockito.verifyNoMoreInteractions(productService);
    Mockito.verifyNoMoreInteractions(productLevel3Service);
    Mockito.verifyNoMoreInteractions(kafkaProducer);
  }


  @Test
  public void updateOfflineItemPriceByItemSku_BlankMerchantCode() throws Exception {
    try {
      boolean result = this.offlineItemService.updateOfflineItemPriceByItemSku(null, ITEM_SKU,
          LIST_PRICE, OFFER_PRICE);
      assertFalse(result);
    } catch (ApplicationRuntimeException e) {
      assertEquals(ErrorCategory.VALIDATION.getMessage()
          + OfflineItemServiceBean.MERCHANT_CODE_MUST_NOT_BE_EMPTY, e.getErrorMessage());
    }
  }

  @Test
  public void updateOfflineItemPriceByItemSku_BlankItemSku() throws Exception {
    try {
      boolean result = this.offlineItemService.updateOfflineItemPriceByItemSku(MERCHANT_CODE, null,
          LIST_PRICE, OFFER_PRICE);
      assertFalse(result);
    } catch (ApplicationRuntimeException e) {
      assertEquals(
          ErrorCategory.VALIDATION.getMessage() + OfflineItemServiceBean.ITEM_SKU_MUST_NOT_BE_EMPTY,
          e.getErrorMessage());
    }
  }

  @Test
  public void updateOfflineItemPriceByItemSku_NullOfferPrice() throws Exception {
    try {
      boolean result = this.offlineItemService.updateOfflineItemPriceByItemSku(MERCHANT_CODE,
          ITEM_SKU, LIST_PRICE, null);
      assertFalse(result);
    } catch (ApplicationRuntimeException e) {
      assertEquals(
          ErrorCategory.VALIDATION.getMessage() + OfflineItemServiceBean.OFFER_PRICE_MUST_BE_FILLED,
          e.getErrorMessage());
    }
  }

  @Test
  public void updateOfflineItemPriceByItemSku_Success() throws Exception {
    UpdateOfflineItemPriceRequest request =
        new UpdateOfflineItemPriceRequest(ITEM_SKU, LIST_PRICE, OFFER_PRICE);
    Mockito.when(this.offlineItemRepository
        .updateOfflineItemPriceByItemSku(Mockito.eq(MERCHANT_CODE), Mockito.eq(request)))
        .thenReturn(new GdnBaseRestResponse(true));
    boolean result = this.offlineItemService.updateOfflineItemPriceByItemSku(MERCHANT_CODE,
        ITEM_SKU, LIST_PRICE, OFFER_PRICE);
    assertTrue(result);
    Mockito.verify(this.offlineItemRepository)
        .updateOfflineItemPriceByItemSku(Mockito.eq(MERCHANT_CODE), Mockito.eq(request));
  }

  private OfflineItemInstantPickupV2Response generateOfflineItemInstantPickupV2Response() {
    return OfflineItemInstantPickupV2Response.builder()
        .categoryName(CATEGORY_NAME)
        .itemName(ITEM_NAME)
        .itemSku(ITEM_SKU)
        .merchantSku(MERCHANT_SKU)
        .productSku(PRODUCT_SKU)
        .offlineProducts(Collections.singletonList(
            OfflineProductResponse.builder()
                .pickupPointCode(PICKUP_POINT_CODE)
                .offlineSafetyStock(SAFETY_STOCK)
                .offlineOriginalStock(ORIGINAL_STOCK)
                .offlineAvailableStock(STOCK)
                .listPrice(LIST_PRICE)
                .price(OFFER_PRICE)
                .build()))
            .build();
  }


  @Test
  public void findOfflineItemProductByMerchantSkusTest_nullBusinessPartnerCode() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      this.offlineItemService.findOfflineItemProductByMerchantSkus(null, merchantSkus);
    });
  }

  @Test
  public void findOfflineItemProductByMerchantSkusTest_emptyMerchantSkus() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      this.offlineItemService.findOfflineItemProductByMerchantSkus(BUSINESS_PARTNER_CODE, null);
    });
  }

  @Test
  public void findOfflineItemProductByMerchantSkusTest_success() throws Exception {
    when(this.offlineItemRepository
        .findOfflineItemByBusinessPartnerCodeAndMerchantSkus(BUSINESS_PARTNER_CODE, merchantSkus))
        .thenReturn(this.offlineItemResponse);

    List<OfflineItemResponseDetail> responses = this.offlineItemService
        .findOfflineItemProductByMerchantSkus(BUSINESS_PARTNER_CODE, merchantSkus);

    verify(this.offlineItemRepository).findOfflineItemByBusinessPartnerCodeAndMerchantSkus(
        BUSINESS_PARTNER_CODE, merchantSkus);

    assertEquals(offlineItemResponseDetail, responses.get(0));
  }

  private DeleteOfflineItemResponse constructDeleteOfflineItemResponse(String itemSku, boolean success) {
    DeleteOfflineItemResponse response = new DeleteOfflineItemResponse();
    response.setItemSku(itemSku);
    response.setPickupPointCode(PICKUP_POINT_CODE);
    response.setSuccess(success);
    response.setErrorMessage(success ? StringUtils.EMPTY : ERROR_MESSAGE);
    return response;
  }

  @Test
  public void bulkDeleteOfflineItemTest_allSuccessMppTrue() throws Exception {
    List<DeleteOfflineItemResponse> responses = new ArrayList<>();
    responses.add(constructDeleteOfflineItemResponse(ITEM_SKU, Boolean.TRUE));
    responses.add(constructDeleteOfflineItemResponse(ITEM_SKU2, Boolean.TRUE));

    when(offlineItemConverter.convertToDeleteOfflineItemRequest(deleteOfflineItems))
        .thenReturn(deleteOfflineItemRequests);
    when(xProductOutbound.bulkDeleteOfflineItem(MERCHANT_CODE, deleteOfflineItemRequests))
        .thenReturn(responses);

    Pair<List<DeleteOfflineItemDetailResponse>, List<DeleteOfflineItemDetailResponse>>
        pair = offlineItemService.bulkDeleteOfflineItem(REQUEST_ID, USERNAME, MERCHANT_CODE, deleteOfflineItems);

    verify(offlineItemConverter).convertToDeleteOfflineItemRequest(Mockito.eq(deleteOfflineItems));
    verify(xProductOutbound).bulkDeleteOfflineItem(MERCHANT_CODE, deleteOfflineItemRequests);
    verify(xProductOutbound).updateCncActivationFlag(Arrays.asList(ITEM_SKU));
    verify(updatedProductHistoryService).createAudit(any(), Mockito.eq(false));

    assertEquals(2, pair.getLeft().size());
    assertEquals(0, pair.getRight().size());
  }

  @Test
  public void bulkDeleteOfflineItemTest_allSuccess() throws Exception {
    List<DeleteOfflineItemResponse> responses = new ArrayList<>();
    responses.add(constructDeleteOfflineItemResponse(ITEM_SKU, Boolean.TRUE));
    responses.add(constructDeleteOfflineItemResponse(ITEM_SKU2, Boolean.TRUE));

    when(offlineItemConverter.convertToDeleteOfflineItemRequest(deleteOfflineItems))
        .thenReturn(deleteOfflineItemRequests);
    when(xProductOutbound.bulkDeleteOfflineItem(MERCHANT_CODE, deleteOfflineItemRequests))
        .thenReturn(responses);

    Pair<List<DeleteOfflineItemDetailResponse>, List<DeleteOfflineItemDetailResponse>>
        pair = offlineItemService.bulkDeleteOfflineItem(REQUEST_ID, USERNAME, MERCHANT_CODE, deleteOfflineItems);

    verify(offlineItemConverter).convertToDeleteOfflineItemRequest(Mockito.eq(deleteOfflineItems));
    verify(xProductOutbound).bulkDeleteOfflineItem(MERCHANT_CODE, deleteOfflineItemRequests);
    verify(xProductOutbound).updateCncActivationFlag(Arrays.asList(ITEM_SKU));
    verify(updatedProductHistoryService).createAudit(any(), Mockito.eq(false));

    assertEquals(2, pair.getLeft().size());
    assertEquals(0, pair.getRight().size());
  }

  @Test
  public void bulkDeleteOfflineItemTestWithHistoryEventSwitchOn() throws Exception {
    ReflectionTestUtils.setField(offlineItemService, "productHistoryUpdateThroughEvent", true);
    UpdatedProductHistory updatedProductHistory = new UpdatedProductHistory();
    updatedProductHistory.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    updatedProductHistory.setProductSku(PRODUCT_SKU);
    updatedProductHistory.setGdnName(GDN_NAME);
    updatedProductHistory.setActivity(ACTIVITY);
    updatedProductHistory.setOldValues(OLD_VALUES);
    updatedProductHistory.setNewValues(NEW_VALUES);
    updatedProductHistory.setChangedBy(CHANGED_BY);
    updatedProductHistory.setRequestId(REQUEST_ID);
    updatedProductHistory.setClientHost(CLIENT_ID);
    List<DeleteOfflineItemResponse> responses = new ArrayList<>();
    responses.add(constructDeleteOfflineItemResponse(ITEM_SKU, Boolean.TRUE));
    responses.add(constructDeleteOfflineItemResponse(ITEM_SKU2, Boolean.TRUE));
    responses.get(0).setProductSku(PRODUCT_SKU);
    responses.get(0).setItemName(ITEM_NAME);
    responses.get(1).setProductSku(PRODUCT_SKU);
    responses.get(1).setItemName(ITEM_NAME);

    when(offlineItemConverter.convertToDeleteOfflineItemRequest(deleteOfflineItems))
        .thenReturn(deleteOfflineItemRequests);
    when(xProductOutbound.bulkDeleteOfflineItem(MERCHANT_CODE, deleteOfflineItemRequests)).thenReturn(responses);
    when(updatedProductHistoryService
        .addToUpdatedProductHistory(anyString(), anyString(), anyString(), anyString(), anyString(), anyString(),
            anyString(), anyString(), anyString(), anyString(), anyBoolean())).thenReturn(updatedProductHistory);

    Pair<List<DeleteOfflineItemDetailResponse>, List<DeleteOfflineItemDetailResponse>> pair =
        offlineItemService.bulkDeleteOfflineItem(REQUEST_ID, USERNAME, MERCHANT_CODE, deleteOfflineItems);

    verify(offlineItemConverter).convertToDeleteOfflineItemRequest(Mockito.eq(deleteOfflineItems));
    verify(xProductOutbound).bulkDeleteOfflineItem(MERCHANT_CODE, deleteOfflineItemRequests);
    verify(xProductOutbound).updateCncActivationFlag(Arrays.asList(ITEM_SKU));
    verify(kafkaProducer, times(2)).send(anyString(), any(), any(AuditTrailListRequest.class));
  }

  @Test
  public void bulkDeleteOfflineItemTest_partiallySuccess() throws Exception {
    List<DeleteOfflineItemResponse> responses = new ArrayList<>();
    responses.add(constructDeleteOfflineItemResponse(ITEM_SKU, Boolean.TRUE));
    responses.add(constructDeleteOfflineItemResponse(ITEM_SKU2, Boolean.FALSE));

    when(offlineItemConverter.convertToDeleteOfflineItemRequest(deleteOfflineItems))
        .thenReturn(deleteOfflineItemRequests);
    when(xProductOutbound.bulkDeleteOfflineItem(MERCHANT_CODE, deleteOfflineItemRequests))
        .thenReturn(responses);

    Pair<List<DeleteOfflineItemDetailResponse>, List<DeleteOfflineItemDetailResponse>>
        pair = offlineItemService.bulkDeleteOfflineItem(REQUEST_ID, USERNAME, MERCHANT_CODE, deleteOfflineItems);

    verify(offlineItemConverter).convertToDeleteOfflineItemRequest(Mockito.eq(deleteOfflineItems));
    verify(xProductOutbound).bulkDeleteOfflineItem(MERCHANT_CODE, deleteOfflineItemRequests);
    verify(xProductOutbound).updateCncActivationFlag(Arrays.asList(ITEM_SKU));
    verify(updatedProductHistoryService).createAudit(any(), Mockito.eq(false));

    assertEquals(1, pair.getLeft().size());
    assertEquals(1, pair.getRight().size());
  }

  @Test
  public void bulkDeleteOfflineItemTest_allFailed() throws Exception {
    List<DeleteOfflineItemResponse> responses = new ArrayList<>();
    responses.add(constructDeleteOfflineItemResponse(ITEM_SKU, Boolean.FALSE));
    responses.add(constructDeleteOfflineItemResponse(ITEM_SKU2, Boolean.FALSE));

    when(offlineItemConverter.convertToDeleteOfflineItemRequest(deleteOfflineItems))
        .thenReturn(deleteOfflineItemRequests);
    when(xProductOutbound.bulkDeleteOfflineItem(MERCHANT_CODE, deleteOfflineItemRequests))
        .thenReturn(responses);

    Pair<List<DeleteOfflineItemDetailResponse>, List<DeleteOfflineItemDetailResponse>>
        pair = offlineItemService.bulkDeleteOfflineItem(REQUEST_ID, USERNAME, MERCHANT_CODE, deleteOfflineItems);

    verify(offlineItemConverter).convertToDeleteOfflineItemRequest(Mockito.eq(deleteOfflineItems));
    verify(xProductOutbound).bulkDeleteOfflineItem(MERCHANT_CODE, deleteOfflineItemRequests);
    verify(xProductOutbound).updateCncActivationFlag(Arrays.asList(ITEM_SKU));
    verify(updatedProductHistoryService).createAudit(any(), Mockito.eq(false));

    assertEquals(0, pair.getLeft().size());
    assertEquals(2, pair.getRight().size());
  }

  @Test
  public void bulkDeleteOfflineItemTest_allFailedMppTrue() throws Exception {
    List<DeleteOfflineItemResponse> responses = new ArrayList<>();
    responses.add(constructDeleteOfflineItemResponse(ITEM_SKU, Boolean.FALSE));
    DeleteOfflineItemResponse response = constructDeleteOfflineItemResponse(ITEM_SKU2, Boolean.FALSE);
    response.setCncUpdated(true);
    response.setBuyableUpdated(true);
    response.setDiscoverableUpdated(true);
    responses.add(response);

    when(offlineItemConverter.convertToDeleteOfflineItemRequest(deleteOfflineItems))
        .thenReturn(deleteOfflineItemRequests);
    when(xProductOutbound.bulkDeleteOfflineItem(MERCHANT_CODE, deleteOfflineItemRequests))
        .thenReturn(responses);

    Pair<List<DeleteOfflineItemDetailResponse>, List<DeleteOfflineItemDetailResponse>>
        pair = offlineItemService.bulkDeleteOfflineItem(REQUEST_ID, USERNAME, MERCHANT_CODE, deleteOfflineItems);

    verify(offlineItemConverter).convertToDeleteOfflineItemRequest(Mockito.eq(deleteOfflineItems));
    verify(xProductOutbound).bulkDeleteOfflineItem(MERCHANT_CODE, deleteOfflineItemRequests);
    verify(xProductOutbound).updateCncActivationFlag(Arrays.asList(ITEM_SKU));
    verify(updatedProductHistoryService).createAudit(any(), Mockito.eq(false));

    assertEquals(0, pair.getLeft().size());
    assertEquals(2, pair.getRight().size());
  }

  private void mockForUpsertOfflineItems() {
    company = new CompanyDTO();
    company.setCncActivated(true);

    profileResponse = new ProfileResponse();
    profileResponse.setCompany(company);

    validPickupPointCodes = Arrays.asList(PICKUP_POINT_CODE);

    upsertOfflineItem = new UpsertOfflineItem();
    upsertOfflineItem.setItemSku(MERCHANT_CODE + Constants.HYPHEN + ITEM_SKU);
    upsertOfflineItem.setPickupPointCode(PICKUP_POINT_CODE);
    upsertOfflineItem.setListPrice(LIST_PRICE);
    upsertOfflineItem.setOfferPrice(OFFER_PRICE);
    upsertOfflineItem.setStock(STOCK);
    upsertOfflineItem.setFileName(FILE_NAME);

    upsertOfflineItems = new ArrayList<>();
    upsertOfflineItems.add(upsertOfflineItem);

    validOfflineItem = new ValidOfflineItem();
    validOfflineItem.setItemSku(ITEM_SKU);
    validOfflineItem.setPickupPointCode(PICKUP_POINT_CODE);
    validOfflineItem.setListPrice(LIST_PRICE);
    validOfflineItem.setPrice(OFFER_PRICE);
    validOfflineItem.setStock(STOCK);
    validOfflineItem.setFileName(FILE_NAME);

    validOfflineItems = new ArrayList<>();
    validOfflineItems.add(validOfflineItem);

    failedUpsertOfflineItemResponses = new ArrayList<>();
  }

  @Test
  public void upsertL5ItemsSuccessTest() throws Exception {
    mockForUpsertOfflineItems();
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(MERCHANT_CODE)).thenReturn(profileResponse);
    Mockito.when(productService.checkIfMPPIsAllowed(profileResponse)).thenReturn(true);
    Mockito.when(
        offlineItemHelperService.upsertL5ItemsInXProduct(REQUEST_ID, USERNAME, MERCHANT_CODE, upsertOfflineItems,
            new ArrayList<>())).thenReturn(upsertOfflineItems);
    Mockito.when(productLevel3Service.upsertL5StockInInventory(MERCHANT_CODE, upsertOfflineItems, new ArrayList<>(),
        profileResponse))
        .thenReturn(upsertOfflineItems);
    upsertOfflineItems.get(0).setCncActive(true);
    Pair<List<UpsertOfflineItemDetailResponse>, List<UpsertOfflineItemFailedResponse>> response =
        this.offlineItemService.upsertL5Items(REQUEST_ID, USERNAME, MERCHANT_CODE, upsertOfflineItems, true);
    Mockito.verify(productService).checkIfMPPIsAllowed(profileResponse);
    Mockito.verify(offlineItemHelperService)
        .upsertL5ItemsInXProduct(REQUEST_ID, USERNAME, MERCHANT_CODE, upsertOfflineItems, new ArrayList<>());
    Mockito.verify(productLevel3Service).upsertL5StockInInventory(MERCHANT_CODE, upsertOfflineItems, new ArrayList<>(),
        profileResponse);
    Mockito.verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(MERCHANT_CODE);
    Assertions.assertEquals(1, response.getLeft().size());
    Assertions.assertEquals(0, response.getRight().size());
  }

  @Test
  public void upsertL5ItemsSuccessStockUpdateFalseTest() throws Exception {
    mockForUpsertOfflineItems();
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(MERCHANT_CODE)).thenReturn(profileResponse);
    Mockito.when(productService.checkIfMPPIsAllowed(profileResponse)).thenReturn(true);
    Mockito.when(
        offlineItemHelperService.upsertL5ItemsInXProduct(REQUEST_ID, USERNAME, MERCHANT_CODE, upsertOfflineItems,
            new ArrayList<>())).thenReturn(upsertOfflineItems);
    Pair<List<UpsertOfflineItemDetailResponse>, List<UpsertOfflineItemFailedResponse>> response =
        this.offlineItemService.upsertL5Items(REQUEST_ID, USERNAME, MERCHANT_CODE, upsertOfflineItems, false);
    Mockito.verify(productService).checkIfMPPIsAllowed(profileResponse);
    Mockito.verify(offlineItemHelperService)
        .upsertL5ItemsInXProduct(REQUEST_ID, USERNAME, MERCHANT_CODE, upsertOfflineItems, new ArrayList<>());
    Mockito.verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(MERCHANT_CODE);
    Assertions.assertEquals(1, response.getLeft().size());
    Assertions.assertEquals(0, response.getRight().size());
  }

  @Test
  public void upsertL5ItemsMerchantNotAllowedForMPPTest() throws Exception {
    mockForUpsertOfflineItems();
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(MERCHANT_CODE)).thenReturn(profileResponse);
    Mockito.when(productService.checkIfMPPIsAllowed(profileResponse)).thenReturn(false);
    Mockito.when(
        offlineItemHelperService.upsertL5ItemsInXProduct(REQUEST_ID, USERNAME, MERCHANT_CODE, upsertOfflineItems,
            new ArrayList<>())).thenReturn(upsertOfflineItems);
    Mockito.when(productLevel3Service.upsertL5StockInInventory(MERCHANT_CODE, upsertOfflineItems, new ArrayList<>(),
        profileResponse))
        .thenReturn(upsertOfflineItems);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        this.offlineItemService.upsertL5Items(REQUEST_ID, USERNAME, MERCHANT_CODE, upsertOfflineItems, true);
      });
    } catch (Exception e) {
      throw e;
    } finally {
      Mockito.verify(productService).checkIfMPPIsAllowed(profileResponse);
      Mockito.verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(MERCHANT_CODE);
    }
  }

  @Test
  public void upsertL5ItemsValidationFailTest1() throws Exception {
    upsertOfflineItem = new UpsertOfflineItem();
    upsertOfflineItems = new ArrayList<>();
    upsertOfflineItems.add(upsertOfflineItem);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(MERCHANT_CODE)).thenReturn(profileResponse);
    Mockito.when(productService.checkIfMPPIsAllowed(profileResponse)).thenReturn(true);
    Pair<List<UpsertOfflineItemDetailResponse>, List<UpsertOfflineItemFailedResponse>> response =
        this.offlineItemService.upsertL5Items(REQUEST_ID, USERNAME, MERCHANT_CODE, upsertOfflineItems, true);
    Mockito.verify(productService).checkIfMPPIsAllowed(profileResponse);
    Mockito.verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(MERCHANT_CODE);
    Assertions.assertEquals(0, response.getLeft().size());
    Assertions.assertEquals(1, response.getRight().size());
  }

  @Test
  public void upsertL5ItemsValidationFailTest2() throws Exception {
    upsertOfflineItem = new UpsertOfflineItem();
    upsertOfflineItem.setItemSku(ITEM_SKU2);
    upsertOfflineItems = new ArrayList<>();
    upsertOfflineItems.add(upsertOfflineItem);
    Mockito.when(productService.checkIfMPPIsAllowed(profileResponse)).thenReturn(true);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(MERCHANT_CODE)).thenReturn(profileResponse);
    Pair<List<UpsertOfflineItemDetailResponse>, List<UpsertOfflineItemFailedResponse>> response =
        this.offlineItemService.upsertL5Items(REQUEST_ID, USERNAME, MERCHANT_CODE, upsertOfflineItems, false);
    Mockito.verify(productService).checkIfMPPIsAllowed(profileResponse);
    Mockito.verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(MERCHANT_CODE);
    Assertions.assertEquals(0, response.getLeft().size());
    Assertions.assertEquals(1, response.getRight().size());
  }

  @Test
  public void upsertL5ItemsXproductEmptyListTest() throws Exception {
    mockForUpsertOfflineItems();
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(MERCHANT_CODE)).thenReturn(profileResponse);
    Mockito.when(productService.checkIfMPPIsAllowed(profileResponse)).thenReturn(true);
    Mockito.when(
        offlineItemHelperService.upsertL5ItemsInXProduct(REQUEST_ID, USERNAME, MERCHANT_CODE, upsertOfflineItems,
            new ArrayList<>())).thenReturn(new ArrayList<>());
    Pair<List<UpsertOfflineItemDetailResponse>, List<UpsertOfflineItemFailedResponse>> response =
        this.offlineItemService.upsertL5Items(REQUEST_ID, USERNAME, MERCHANT_CODE, upsertOfflineItems, false);
    Mockito.verify(productService).checkIfMPPIsAllowed(profileResponse);
    Mockito.verify(offlineItemHelperService)
        .upsertL5ItemsInXProduct(REQUEST_ID, USERNAME, MERCHANT_CODE, upsertOfflineItems, new ArrayList<>());
    Mockito.verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(MERCHANT_CODE);
    Assertions.assertEquals(0, response.getLeft().size());
    Assertions.assertEquals(0, response.getRight().size());
  }


  @Test
  public void upsertL5ItemsFailedInXInventoryTest() throws Exception {
    mockForUpsertOfflineItems();
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(MERCHANT_CODE)).thenReturn(profileResponse);
    Mockito.when(productService.checkIfMPPIsAllowed(profileResponse)).thenReturn(true);
    Mockito.when(
        offlineItemHelperService.upsertL5ItemsInXProduct(REQUEST_ID, USERNAME, MERCHANT_CODE, upsertOfflineItems,
            new ArrayList<>())).thenReturn(upsertOfflineItems);
    Mockito.when(productLevel3Service.upsertL5StockInInventory(MERCHANT_CODE, upsertOfflineItems, new ArrayList<>(),
        profileResponse))
        .thenReturn(new ArrayList<>());
    Pair<List<UpsertOfflineItemDetailResponse>, List<UpsertOfflineItemFailedResponse>> response =
        this.offlineItemService.upsertL5Items(REQUEST_ID, USERNAME, MERCHANT_CODE, upsertOfflineItems, true);
    Mockito.verify(productService).checkIfMPPIsAllowed(profileResponse);
    Mockito.verify(offlineItemHelperService)
        .upsertL5ItemsInXProduct(REQUEST_ID, USERNAME, MERCHANT_CODE, upsertOfflineItems, new ArrayList<>());
    Mockito.verify(productLevel3Service).upsertL5StockInInventory(MERCHANT_CODE, upsertOfflineItems, new ArrayList<>(),
        profileResponse);
    Mockito.verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(MERCHANT_CODE);
    Assertions.assertEquals(0, response.getLeft().size());
    Assertions.assertEquals(0, response.getRight().size());
  }
}

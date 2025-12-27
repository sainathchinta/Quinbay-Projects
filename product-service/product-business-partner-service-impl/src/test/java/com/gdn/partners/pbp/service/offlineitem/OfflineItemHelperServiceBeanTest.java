package com.gdn.partners.pbp.service.offlineitem;

import static org.mockito.MockitoAnnotations.initMocks;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.log4j.MDC;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.product.repository.BusinessPartnerRepository;
import com.gdn.partners.pbp.converter.OfflineItemConverter;
import com.gdn.partners.pbp.dto.offlineitem.FailedOfflineItemResponse;
import com.gdn.partners.pbp.dto.offlineitem.SuccessOfflineItemResponse;
import com.gdn.partners.pbp.dto.offlineitem.UpsertOfflineItemFailedResponse;
import com.gdn.partners.pbp.model.offlineitem.OfflineItem;
import com.gdn.partners.pbp.model.offlineitem.UpsertOfflineItem;
import com.gdn.partners.pbp.model.offlineitem.ValidOfflineItem;
import com.gdn.partners.pbp.outbound.inventory.InventoryOutbound;
import com.gdn.partners.pbp.outbound.product.ProductOutbound;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.ExternalPickupPointCodeResponseDetail;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.product.rest.web.model.request.OfflineItemRequest;
import com.gdn.x.product.rest.web.model.request.UpsertOfflineItemRequest;
import com.gdn.x.product.rest.web.model.response.OfflineItemResponseDetail;
import com.gdn.x.product.rest.web.model.response.UpsertOfflineItemPriceResponse;

public class OfflineItemHelperServiceBeanTest {

  @Mock
  private OfflineItemConverter offlineItemConverter;

  @Mock
  private InventoryOutbound inventoryOutbound;

  @Mock
  private ProductOutbound productOutbound;
  
  @Mock
  private BusinessPartnerRepository businessPartnerRepository;

  @InjectMocks
  private OfflineItemHelperServiceBean serviceBean;

  private static List<OfflineItemResponseDetail> offlineItemResponseDetails;
  private static List<ExternalPickupPointCodeResponseDetail> externalPickupPointCodeResponseDetails;
  private static Map<String, OfflineItemResponseDetail> offlineItemResponseByMerchantSkuMap;
  private static Map<String, ExternalPickupPointCodeResponseDetail> externalPickupPointResponseByExternalPickupPointCodeMa;
  private static List<OfflineItem> offlineItems;
  private static List<ValidOfflineItem> validOfflineItems;
  private static List<FailedOfflineItemResponse> failedOfflineItemResponses;
  private static FailedOfflineItemResponse failedOfflineItemResponse;
  private static List<SuccessOfflineItemResponse> successOfflineItemResponses;
  private static SuccessOfflineItemResponse successOfflineItemResponse;
  private static ValidOfflineItem validOfflineItem;
  private MandatoryRequestParam mandatoryRequestParam;

  private UpsertOfflineItemFailedResponse upsertOfflineItemFailedResponse;
  private List<UpsertOfflineItemFailedResponse> failedUpsertOfflineItemResponses;
  private UpsertOfflineItemRequest upsertOfflineItemRequest;
  private List<UpsertOfflineItemRequest> upsertOfflineItemRequests;
  private ValidOfflineItem validOfflineItemforUpsert;
  private List<ValidOfflineItem> validOfflineItemsforUpsert;
  private List<String> validPickupPointCodes;
  private UpsertOfflineItem upsertOfflineItem;
  private List<UpsertOfflineItem> upsertOfflineItems;
  private UpsertOfflineItemPriceResponse upsertOfflineItemPriceResponse;
  private UpsertOfflineItemPriceResponse successUpsertOfflineItemPriceResponse;
  private List<UpsertOfflineItemPriceResponse> upsertOfflineItemPriceResponses;

  private static final String STOREID = "storeId";
  private static final String CHANNELID = "channelId";
  private static final String CLIENTID = "clientId";
  private static final String REQUESTID = "requestId";
  private static final String USERNAME = "username";
  private static final String AUTHENTICATOR = "authenticator";
  private static final Integer STOCK = 1;
  private static final Double LIST_PRICE = 20000.0;
  private static final Double OFFER_PRICE = 10000.0;
  private static final String FILE_NAME = "file-name";
  private static final Integer STOCK_2 = 2;
  private static final Double LIST_PRICE_2 = 30000.0;
  private static final Double OFFER_PRICE_2 = 20000.0;
  private static final String FILE_NAME_2 = "file-name-2";
  private static final String ITEM_SKU = "item-sku";
  private static final String MERCHANT_SKU = "merchant-sku";
  private static final String ERROR_CODE = "error-code";
  private static final String ITEM_SKU_2 = "item-sku-2";
  private static final String MERCHANT_SKU_2 = "merchant-sku-2";
  private static final String ERROR_CODE_2 = "error-code-2";
  private static final String CODE = "code";
  private static final String EXTERNAL_PICLKUP_POINT_CODE = "external-pickup-point-code";
  private static final String CODE_2 = "code-2";
  private static final String EXTERNAL_PICLKUP_POINT_CODE_2 = "external-pickup-point-code-2";
  private static final String MERCHANT_CODE = "merchant-code";
  private static final String PICKUP_POINT_CODE = "pickup-point-code";
  private static final String FAILED_TO_UPDATE_PRICE_ERROR = "FAILED_TO_UPDATE_PRICE_ERROR";
  private static final String ITEM_SKU_3 = "AAA-xxxxx-xxxxx-xxxxx";
  private static final String MERCHANT_CODE_2 = "AAA-xxxxx";
  private static final String DASH_SEPARATOR = "-";
  private static final String OFFLINE_ITEM_ID =
      ITEM_SKU_3.concat(DASH_SEPARATOR).concat(PICKUP_POINT_CODE);
  private static final String ITEM_SKU_IS_NOT_VALID = "Blibli SKU tidak sesuai";
  private static final String PICKUP_POINT_IS_NOT_VALID = "Kode lokasi tidak sesuai";
  private static final String PICKUP_POINT_CODE_2 = "pickup-point-code-2";

  @BeforeEach
  public void setUp() throws Exception {
    initMocks(this);

    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER,
        OfflineItemHelperServiceBeanTest.STOREID);
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
        OfflineItemHelperServiceBeanTest.CHANNELID);
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER,
        OfflineItemHelperServiceBeanTest.CLIENTID);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
        OfflineItemHelperServiceBeanTest.REQUESTID);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER,
        OfflineItemHelperServiceBeanTest.USERNAME);
    MDC.put(GdnMandatoryRequestParameterUtil.AUTHENTICATOR_KEY,
        OfflineItemHelperServiceBeanTest.AUTHENTICATOR);


    mandatoryRequestParam = MandatoryRequestParam.generateMandatoryRequestParam(STOREID, CHANNELID,
        CLIENTID, REQUESTID, USERNAME, AUTHENTICATOR);

    failedOfflineItemResponses = new ArrayList<>();
    failedOfflineItemResponse = new FailedOfflineItemResponse();
    failedOfflineItemResponse.setMerchantSku(MERCHANT_SKU);
    failedOfflineItemResponse.setExternalPickupPointCode(EXTERNAL_PICLKUP_POINT_CODE);
    failedOfflineItemResponses.add(failedOfflineItemResponse);

    successOfflineItemResponses = new ArrayList<>();
    successOfflineItemResponse = new SuccessOfflineItemResponse();
    successOfflineItemResponse.setItemSku(ITEM_SKU);
    successOfflineItemResponse.setMerchantSku(MERCHANT_SKU);
    successOfflineItemResponse.setPickupPointCode(PICKUP_POINT_CODE);
    successOfflineItemResponse.setExternalPickupPointCode(EXTERNAL_PICLKUP_POINT_CODE);
    successOfflineItemResponse.setListPrice(LIST_PRICE);
    successOfflineItemResponse.setPrice(OFFER_PRICE);
    successOfflineItemResponses.add(successOfflineItemResponse);

    offlineItems = new ArrayList<>();
    OfflineItem offlineItem = new OfflineItem();
    offlineItem.setExternalPickupPointCode(EXTERNAL_PICLKUP_POINT_CODE);
    offlineItem.setFileName(FILE_NAME);
    offlineItem.setListPrice(LIST_PRICE);
    offlineItem.setPrice(OFFER_PRICE);
    offlineItem.setMerchantSku(MERCHANT_SKU);
    offlineItem.setStock(STOCK);
    offlineItems.add(offlineItem);

    OfflineItem offlineItem2 = new OfflineItem();
    offlineItem2.setExternalPickupPointCode(EXTERNAL_PICLKUP_POINT_CODE_2);
    offlineItem2.setFileName(FILE_NAME_2);
    offlineItem2.setListPrice(LIST_PRICE_2);
    offlineItem2.setPrice(OFFER_PRICE_2);
    offlineItem2.setMerchantSku(MERCHANT_SKU_2);
    offlineItem2.setStock(STOCK_2);
    offlineItems.add(offlineItem);

    offlineItemResponseDetails = new ArrayList<>();
    OfflineItemResponseDetail offlineItemResponseDetail = new OfflineItemResponseDetail();
    offlineItemResponseDetail.setItemSku(ITEM_SKU);
    offlineItemResponseDetail.setMerchantSku(MERCHANT_SKU);
    offlineItemResponseDetails.add(offlineItemResponseDetail);

    OfflineItemResponseDetail offlineItemResponseDetail2 = new OfflineItemResponseDetail();
    offlineItemResponseDetail2.setItemSku(ITEM_SKU_2);
    offlineItemResponseDetail2.setMerchantSku(MERCHANT_SKU_2);
    offlineItemResponseDetails.add(offlineItemResponseDetail2);

    externalPickupPointCodeResponseDetails = new ArrayList<>();
    ExternalPickupPointCodeResponseDetail externalPickupPointCodeResponseDetail = new ExternalPickupPointCodeResponseDetail();
    externalPickupPointCodeResponseDetail.setCode(CODE);
    externalPickupPointCodeResponseDetail.setExternalPickupPointCode(EXTERNAL_PICLKUP_POINT_CODE);
    externalPickupPointCodeResponseDetails.add(externalPickupPointCodeResponseDetail);

    ExternalPickupPointCodeResponseDetail externalPickupPointCodeResponseDetail2 = new ExternalPickupPointCodeResponseDetail();
    externalPickupPointCodeResponseDetail2.setCode(CODE_2);
    externalPickupPointCodeResponseDetail2.setExternalPickupPointCode(EXTERNAL_PICLKUP_POINT_CODE_2);
    externalPickupPointCodeResponseDetails.add(externalPickupPointCodeResponseDetail);

    offlineItemResponseByMerchantSkuMap = offlineItemResponseDetails.stream()
        .collect(HashMap::new, (map, o) -> map.put(o.getMerchantSku(), o), HashMap::putAll);

    externalPickupPointResponseByExternalPickupPointCodeMa = externalPickupPointCodeResponseDetails.stream()
        .collect(HashMap::new, (map, o) -> map.put(o.getExternalPickupPointCode(), o), HashMap::putAll);

    validOfflineItems = new ArrayList<>();
    validOfflineItem = new ValidOfflineItem();
    validOfflineItem.setMerchantSku(MERCHANT_SKU);
    validOfflineItem.setExternalPickupPointCode(EXTERNAL_PICLKUP_POINT_CODE);
    validOfflineItem.setItemSku(ITEM_SKU);
    validOfflineItem.setFileName(FILE_NAME);
    validOfflineItem.setPickupPointCode(PICKUP_POINT_CODE);
    validOfflineItem.setListPrice(LIST_PRICE);
    validOfflineItem.setPrice(OFFER_PRICE);
    validOfflineItem.setStock(STOCK);
    validOfflineItems.add(validOfflineItem);

    this.upsertOfflineItemRequest = new UpsertOfflineItemRequest();
    this.upsertOfflineItemRequest.setItemSku(ITEM_SKU_3);
    this.upsertOfflineItemRequest.setListPrice(LIST_PRICE);
    this.upsertOfflineItemRequest.setOfferPrice(OFFER_PRICE);
    this.upsertOfflineItemRequest.setPickupPointCode(PICKUP_POINT_CODE);
    this.upsertOfflineItemRequest.setOfflineItemId(OFFLINE_ITEM_ID);
    this.upsertOfflineItemRequest.setCncActive(false);

    this.upsertOfflineItemRequests = new ArrayList<>();
    this.upsertOfflineItemRequests.add(this.upsertOfflineItemRequest);

    this.validOfflineItemforUpsert = new ValidOfflineItem();
    this.validOfflineItemforUpsert.setItemSku(ITEM_SKU_3);
    this.validOfflineItemforUpsert.setListPrice(LIST_PRICE);
    this.validOfflineItemforUpsert.setPrice(OFFER_PRICE);
    this.validOfflineItemforUpsert.setPickupPointCode(PICKUP_POINT_CODE);
    this.validOfflineItemforUpsert.setFileName(FILE_NAME);
    this.validOfflineItemforUpsert.setStock(STOCK);

    this.validOfflineItemsforUpsert = new ArrayList<>();
    this.validOfflineItemsforUpsert.add(this.validOfflineItemforUpsert);

    this.upsertOfflineItemFailedResponse = new UpsertOfflineItemFailedResponse();
    this.upsertOfflineItemFailedResponse.setItemSku(ITEM_SKU_3);
    this.upsertOfflineItemFailedResponse.setPickupPointCode(PICKUP_POINT_CODE);
    this.upsertOfflineItemFailedResponse.setErrorCode(ITEM_SKU_IS_NOT_VALID);

    this.failedUpsertOfflineItemResponses = new ArrayList<>();

    this.validPickupPointCodes = Arrays.asList(PICKUP_POINT_CODE);

    this.upsertOfflineItem = new UpsertOfflineItem();
    this.upsertOfflineItem.setItemSku(ITEM_SKU_3);
    this.upsertOfflineItem.setListPrice(LIST_PRICE);
    this.upsertOfflineItem.setOfferPrice(OFFER_PRICE);
    this.upsertOfflineItem.setPickupPointCode(PICKUP_POINT_CODE);
    this.upsertOfflineItem.setFileName(FILE_NAME);
    this.upsertOfflineItem.setStock(STOCK);

    this.upsertOfflineItems = new ArrayList<>();
    this.upsertOfflineItems.add(this.upsertOfflineItem);

    this.upsertOfflineItemPriceResponse = new UpsertOfflineItemPriceResponse();
    this.upsertOfflineItemPriceResponse.setItemSku(ITEM_SKU_3);
    this.upsertOfflineItemPriceResponse.setListPrice(LIST_PRICE);
    this.upsertOfflineItemPriceResponse.setOfferPrice(OFFER_PRICE);
    this.upsertOfflineItemPriceResponse.setPickupPointCode(PICKUP_POINT_CODE);
    this.upsertOfflineItemPriceResponse.setOfflineItemId(OFFLINE_ITEM_ID);
    this.upsertOfflineItemPriceResponse.setSuccess(false);
    this.upsertOfflineItemPriceResponse.setErrorMessage(ERROR_CODE);

    this.successUpsertOfflineItemPriceResponse = new UpsertOfflineItemPriceResponse();
    this.successUpsertOfflineItemPriceResponse.setItemSku(ITEM_SKU_3);
    this.successUpsertOfflineItemPriceResponse.setListPrice(LIST_PRICE);
    this.successUpsertOfflineItemPriceResponse.setOfferPrice(OFFER_PRICE);
    this.successUpsertOfflineItemPriceResponse.setPickupPointCode(PICKUP_POINT_CODE);
    this.successUpsertOfflineItemPriceResponse.setOfflineItemId(OFFLINE_ITEM_ID);
    this.successUpsertOfflineItemPriceResponse.setSuccess(true);
    this.successUpsertOfflineItemPriceResponse.setErrorMessage(ERROR_CODE);

    this.upsertOfflineItemPriceResponses = new ArrayList<>();
    this.upsertOfflineItemPriceResponses.add(this.upsertOfflineItemPriceResponse);
    this.upsertOfflineItemPriceResponses.add(this.successUpsertOfflineItemPriceResponse);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(businessPartnerRepository, offlineItemConverter,
        inventoryOutbound, productOutbound);
  }

  @Test
  public void mapFailedOfflineItemResponses_noHasError_success(){
    List<FailedOfflineItemResponse> expected = new ArrayList();

    List<FailedOfflineItemResponse> result = this.serviceBean.mapFailedOfflineItemResponses(
        offlineItems, offlineItemResponseByMerchantSkuMap, externalPickupPointResponseByExternalPickupPointCodeMa);

    Assertions.assertEquals(expected, result);
  }

  @Test
  public void mapFailedOfflineItemResponses_ErrorCodeNotEmpty_success(){
    offlineItemResponseDetails.forEach(o -> o.setErrorCode(ERROR_CODE));
    externalPickupPointCodeResponseDetails.forEach(e -> e.setErrorCode(ERROR_CODE_2));

    Mockito.when(this.offlineItemConverter.convertOfflineItemToFailedItem(
        Mockito.any(OfflineItem.class), Mockito.anyString())).thenReturn(failedOfflineItemResponse);

    List<FailedOfflineItemResponse> result = this.serviceBean.mapFailedOfflineItemResponses(
        offlineItems, offlineItemResponseByMerchantSkuMap, externalPickupPointResponseByExternalPickupPointCodeMa);

    Mockito.verify(this.offlineItemConverter, Mockito.times(2))
        .convertOfflineItemToFailedItem(Mockito.any(OfflineItem.class), Mockito.anyString());
    Assertions.assertEquals(2, result.size());
  }

  @Test
  public void mapOfflineItemResponseByMerchantSku_offlineItemResponseDetailsIsEmpty_success(){
    Map<String, OfflineItemResponseDetail> expectedResult = new HashMap<>();

    Map<String, OfflineItemResponseDetail> actualResult = this.serviceBean
        .mapOfflineItemResponseByMerchantSku(null);

    Assertions.assertEquals(expectedResult.size(), actualResult.size());
    Assertions.assertEquals(expectedResult, actualResult);
  }

  @Test
  public void mapOfflineItemResponseByMerchantSku_valid_success(){
    Map<String, OfflineItemResponseDetail> expectedResult = offlineItemResponseDetails.stream()
        .collect(HashMap::new, (map, o) -> map.put(o.getMerchantSku(), o), HashMap::putAll);

    Map<String, OfflineItemResponseDetail> actualResult = this.serviceBean
        .mapOfflineItemResponseByMerchantSku(offlineItemResponseDetails);

    Assertions.assertEquals(expectedResult.size(), actualResult.size());
    Assertions.assertEquals(expectedResult, actualResult);
  }

  @Test
  public void mapExternalPickupPointResponseByExternalPickupPointCode_externalPickupPointCodeResponseDetailsEmpty_success(){
    Map<String, ExternalPickupPointCodeResponseDetail> expectedResult = new HashMap<>();

    Map<String, ExternalPickupPointCodeResponseDetail> actualResult = this.serviceBean
        .mapExternalPickupPointResponseByExternalPickupPointCode(null);

    Assertions.assertEquals(expectedResult.size(), actualResult.size());
    Assertions.assertEquals(expectedResult, actualResult);
  }

  @Test
  public void mapExternalPickupPointResponseByExternalPickupPointCode_valid_success(){
    Map<String, ExternalPickupPointCodeResponseDetail> expectedResult = externalPickupPointCodeResponseDetails.stream()
        .collect(HashMap::new, (map, o) -> map.put(o.getExternalPickupPointCode(), o), HashMap::putAll);

    Map<String, ExternalPickupPointCodeResponseDetail> actualResult = this.serviceBean
        .mapExternalPickupPointResponseByExternalPickupPointCode(externalPickupPointCodeResponseDetails);

    Assertions.assertEquals(expectedResult.size(), actualResult.size());
    Assertions.assertEquals(expectedResult, actualResult);
  }

  @Test
  public void updateOfflineItemPriceAndAppendSuccessAndFailedOfflineItemFromXProduct_offlineItemRequestsIsEmpty_success() throws Exception {
    List<OfflineItemRequest> offlineItemRequests = new ArrayList<>();
    List<UpsertOfflineItemPriceResponse> itemUpdateResponses = new ArrayList<>();

    Mockito.when(offlineItemConverter.convertOfflineItemsToOfflineItemRequests(validOfflineItems))
        .thenReturn(offlineItemRequests);
    Mockito.when(productOutbound.upsertOfflineItemPrice(REQUESTID, USERNAME, MERCHANT_CODE, offlineItemRequests))
        .thenReturn(itemUpdateResponses);

    this.serviceBean.upsertOfflineItemPriceAndAppendSuccessAndFailedOfflineItemFromXProduct(
        REQUESTID, USERNAME, MERCHANT_CODE, validOfflineItems, successOfflineItemResponses,
        failedOfflineItemResponses);

    Mockito.verify(productOutbound).upsertOfflineItemPrice(REQUESTID, USERNAME, MERCHANT_CODE, offlineItemRequests);
    Mockito.verify(offlineItemConverter).convertOfflineItemsToOfflineItemRequests(validOfflineItems);
  }

  @Test
  public void updateOfflineItemPriceAndAppendSuccessAndFailedOfflineItemFromXProduct_valid_success() throws Exception {
    List<OfflineItemRequest> offlineItemRequests = new ArrayList<>();
    List<UpsertOfflineItemPriceResponse> itemUpdateResponses = new ArrayList<>();
    UpsertOfflineItemPriceResponse upsertOfflineItemPriceResponse = new UpsertOfflineItemPriceResponse();
    upsertOfflineItemPriceResponse.setSuccess(Boolean.FALSE);
    upsertOfflineItemPriceResponse.setExternalPickupPointCode(EXTERNAL_PICLKUP_POINT_CODE);
    upsertOfflineItemPriceResponse.setMerchantSku(MERCHANT_SKU);
    upsertOfflineItemPriceResponse.setItemSku(ITEM_SKU);
    upsertOfflineItemPriceResponse.setPickupPointCode(PICKUP_POINT_CODE);
    upsertOfflineItemPriceResponse.setListPrice(LIST_PRICE);
    upsertOfflineItemPriceResponse.setOfferPrice(OFFER_PRICE);
    upsertOfflineItemPriceResponse.setErrorMessage(ERROR_CODE);
    itemUpdateResponses.add(upsertOfflineItemPriceResponse);
    UpsertOfflineItemPriceResponse upsertOfflineItemPriceResponse2 = new UpsertOfflineItemPriceResponse();
    upsertOfflineItemPriceResponse2.setSuccess(Boolean.TRUE);
    upsertOfflineItemPriceResponse2.setExternalPickupPointCode(EXTERNAL_PICLKUP_POINT_CODE_2);
    upsertOfflineItemPriceResponse2.setMerchantSku(MERCHANT_SKU);
    upsertOfflineItemPriceResponse2.setItemSku(ITEM_SKU_2);
    upsertOfflineItemPriceResponse2.setPickupPointCode(PICKUP_POINT_CODE_2);
    upsertOfflineItemPriceResponse2.setListPrice(LIST_PRICE);
    upsertOfflineItemPriceResponse2.setOfferPrice(OFFER_PRICE);
    itemUpdateResponses.add(upsertOfflineItemPriceResponse2);

    Mockito.when(offlineItemConverter.convertOfflineItemsToOfflineItemRequests(validOfflineItems))
        .thenReturn(offlineItemRequests);
    Mockito.when(productOutbound.upsertOfflineItemPrice(REQUESTID, USERNAME, MERCHANT_CODE, offlineItemRequests))
        .thenReturn(itemUpdateResponses);
    Mockito.when(this.offlineItemConverter.convertUpdateOfflineItemPriceToFailedItem(
        upsertOfflineItemPriceResponse, FAILED_TO_UPDATE_PRICE_ERROR+"-"+ERROR_CODE))
        .thenReturn(failedOfflineItemResponse);
    Mockito.when(this.offlineItemConverter.convertUpdateOfflineItemPriceToSuccessItem(
        upsertOfflineItemPriceResponse2))
        .thenReturn(successOfflineItemResponse);

    this.serviceBean.upsertOfflineItemPriceAndAppendSuccessAndFailedOfflineItemFromXProduct(
        REQUESTID, USERNAME, MERCHANT_CODE, validOfflineItems, successOfflineItemResponses,
        failedOfflineItemResponses);

    Mockito.verify(productOutbound).upsertOfflineItemPrice(REQUESTID, USERNAME, MERCHANT_CODE, offlineItemRequests);
    Mockito.verify(offlineItemConverter).convertOfflineItemsToOfflineItemRequests(validOfflineItems);
    Mockito.verify(this.offlineItemConverter).convertUpdateOfflineItemPriceToFailedItem(
        upsertOfflineItemPriceResponse, FAILED_TO_UPDATE_PRICE_ERROR+"-"+ERROR_CODE);
    Mockito.verify(this.offlineItemConverter).convertUpdateOfflineItemPriceToSuccessItem(
        upsertOfflineItemPriceResponse2);
  }

  @Test
  public void testValidateCncMerchant_withTrueCncActivated() throws Exception {
    ProfileResponse profileResponse = new ProfileResponse();
    CompanyDTO company = new CompanyDTO();
    company.setCncActivated(true);
    profileResponse.setCompany(company);

    Mockito.when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(MERCHANT_CODE))
        .thenReturn(profileResponse);
    this.serviceBean.validateCncMerchant(MERCHANT_CODE);
    Mockito.verify(this.businessPartnerRepository).filterDetailByBusinessPartnerCode(MERCHANT_CODE);
  }

  @Test
  public void testValidateCncMerchant_withFalseCncActivated() throws Exception {
    ProfileResponse profileResponse = new ProfileResponse();
    CompanyDTO company = new CompanyDTO();
    profileResponse.setCompany(company);

    Mockito.when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(MERCHANT_CODE))
        .thenReturn(profileResponse);
    try {
      this.serviceBean.validateCncMerchant(MERCHANT_CODE);
    } catch (Exception e) {
      Mockito.verify(this.businessPartnerRepository).filterDetailByBusinessPartnerCode(MERCHANT_CODE);
      Assertions.assertEquals("Can not process invalid input data :Merchant is not a cnc merchant", e.getMessage());
    }
  }

  @Test
  public void mapFailedOfflineItemResponsesFromUpsertOfflineItemsTest_success_hasNoError() {
    List<UpsertOfflineItemFailedResponse> result = this.serviceBean
        .mapFailedOfflineItemResponsesFromUpsertOfflineItems(this.upsertOfflineItems,
            MERCHANT_CODE_2, this.validPickupPointCodes);

    Assertions.assertEquals(result.size(), 0);
  }

  @Test
  public void mapFailedOfflineItemResponsesFromUpsertOfflineItemsTest_success_invalidItemSku() {
    List<UpsertOfflineItemFailedResponse> result = this.serviceBean
        .mapFailedOfflineItemResponsesFromUpsertOfflineItems(this.upsertOfflineItems, MERCHANT_CODE,
            this.validPickupPointCodes);

    Mockito.verify(this.offlineItemConverter)
        .convertUpsertOfflineItemToUpsertOfflineItemFailedResponse(this.upsertOfflineItem,
            ITEM_SKU_IS_NOT_VALID);
    Assertions.assertEquals(result.size(), 1);
  }

  @Test
  public void mapFailedOfflineItemResponsesFromUpsertOfflineItemsTest_success_invalidPickupPointCode() {
    this.validPickupPointCodes = Arrays.asList(PICKUP_POINT_CODE_2);

    List<UpsertOfflineItemFailedResponse> result = this.serviceBean
        .mapFailedOfflineItemResponsesFromUpsertOfflineItems(this.upsertOfflineItems,
            MERCHANT_CODE_2, this.validPickupPointCodes);

    Mockito.verify(this.offlineItemConverter)
        .convertUpsertOfflineItemToUpsertOfflineItemFailedResponse(this.upsertOfflineItem,
            PICKUP_POINT_IS_NOT_VALID);
    Assertions.assertEquals(1, result.size());
  }

  @Test
  public void upsertOfflineItemPriceAndAppendFailedOfflineItemFromXProductTest_success()
      throws Exception {

    Mockito.when(offlineItemConverter
        .convertValidOfflineItemsToUpsertOfflineItemRequests(this.validOfflineItemsforUpsert))
        .thenReturn(this.upsertOfflineItemRequests);
    Mockito.when(productOutbound
        .upsertOfflineItem(REQUESTID, USERNAME, MERCHANT_CODE_2, this.upsertOfflineItemRequests))
        .thenReturn(this.upsertOfflineItemPriceResponses);

    this.serviceBean
        .upsertOfflineItemAndAppendFailedUpsertOfflineItemsFromXProduct(REQUESTID, USERNAME,
            MERCHANT_CODE_2, this.validOfflineItemsforUpsert,
            this.failedUpsertOfflineItemResponses);

    Mockito.verify(offlineItemConverter)
        .convertValidOfflineItemsToUpsertOfflineItemRequests(this.validOfflineItemsforUpsert);
    Mockito.verify(productOutbound)
        .upsertOfflineItem(REQUESTID, USERNAME, MERCHANT_CODE_2, this.upsertOfflineItemRequests);
    Mockito.verify(offlineItemConverter)
        .convertToUpsertOfflineItemFailedResponse(this.upsertOfflineItemPriceResponse,
            FAILED_TO_UPDATE_PRICE_ERROR.concat(DASH_SEPARATOR).concat(ERROR_CODE));

    Assertions.assertEquals(this.failedUpsertOfflineItemResponses.size(), 1);
  }

  @Test
  public void upsertOfflineItemPriceAndAppendFailedOfflineItemFromXProductTest_success_emptyResponse()
      throws Exception {

    Mockito.when(offlineItemConverter
        .convertValidOfflineItemsToUpsertOfflineItemRequests(this.validOfflineItemsforUpsert))
        .thenReturn(this.upsertOfflineItemRequests);
    Mockito.when(productOutbound
        .upsertOfflineItem(REQUESTID, USERNAME, MERCHANT_CODE_2, this.upsertOfflineItemRequests))
        .thenReturn(Collections.emptyList());

    this.serviceBean
        .upsertOfflineItemAndAppendFailedUpsertOfflineItemsFromXProduct(REQUESTID, USERNAME,
            MERCHANT_CODE_2, this.validOfflineItemsforUpsert,
            this.failedUpsertOfflineItemResponses);

    Mockito.verify(offlineItemConverter)
        .convertValidOfflineItemsToUpsertOfflineItemRequests(this.validOfflineItemsforUpsert);
    Mockito.verify(productOutbound)
        .upsertOfflineItem(REQUESTID, USERNAME, MERCHANT_CODE_2, this.upsertOfflineItemRequests);
  }

  @Test
  public void upsertOfflineItemPriceAndAppendFailedOfflineItemFromXProductTest_success_noError()
      throws Exception {

    this.upsertOfflineItemPriceResponse.setSuccess(true);
    this.upsertOfflineItemPriceResponse.setErrorMessage(null);

    this.upsertOfflineItemPriceResponses = new ArrayList<>();
    this.upsertOfflineItemPriceResponses.add(this.upsertOfflineItemPriceResponse);

    Mockito.when(offlineItemConverter
        .convertValidOfflineItemsToUpsertOfflineItemRequests(this.validOfflineItemsforUpsert))
        .thenReturn(this.upsertOfflineItemRequests);
    Mockito.when(productOutbound
        .upsertOfflineItem(REQUESTID, USERNAME, MERCHANT_CODE_2, this.upsertOfflineItemRequests))
        .thenReturn(this.upsertOfflineItemPriceResponses);

    this.serviceBean
        .upsertOfflineItemAndAppendFailedUpsertOfflineItemsFromXProduct(REQUESTID, USERNAME,
            MERCHANT_CODE_2, this.validOfflineItemsforUpsert,
            this.failedUpsertOfflineItemResponses);

    Mockito.verify(offlineItemConverter)
        .convertValidOfflineItemsToUpsertOfflineItemRequests(this.validOfflineItemsforUpsert);
    Mockito.verify(productOutbound)
        .upsertOfflineItem(REQUESTID, USERNAME, MERCHANT_CODE_2, this.upsertOfflineItemRequests);
  }

  @Test
  public void upsertL5ItemsInXProductTest_success() throws Exception {
    Mockito.when(
            productOutbound.upsertOfflineItem(REQUESTID, USERNAME, MERCHANT_CODE_2, this.upsertOfflineItemRequests))
        .thenReturn(this.upsertOfflineItemPriceResponses);

    this.serviceBean.upsertL5ItemsInXProduct(REQUESTID, USERNAME, MERCHANT_CODE_2, this.upsertOfflineItems,
        this.failedUpsertOfflineItemResponses);

    Mockito.verify(productOutbound)
        .upsertOfflineItem(REQUESTID, USERNAME, MERCHANT_CODE_2, this.upsertOfflineItemRequests);
    Mockito.verify(offlineItemConverter).convertToUpsertOfflineItemFailedResponse(this.upsertOfflineItemPriceResponse,
        FAILED_TO_UPDATE_PRICE_ERROR.concat(DASH_SEPARATOR).concat(ERROR_CODE));

    Assertions.assertEquals(this.failedUpsertOfflineItemResponses.size(), 1);
  }

  @Test
  public void upsertL5ItemsInXProduct_success_emptyResponse() throws Exception {
    Mockito.when(
            productOutbound.upsertOfflineItem(REQUESTID, USERNAME, MERCHANT_CODE_2, this.upsertOfflineItemRequests))
        .thenReturn(Collections.emptyList());

    this.serviceBean.upsertL5ItemsInXProduct(REQUESTID, USERNAME, MERCHANT_CODE_2, this.upsertOfflineItems,
        this.failedUpsertOfflineItemResponses);

    Mockito.verify(productOutbound)
        .upsertOfflineItem(REQUESTID, USERNAME, MERCHANT_CODE_2, this.upsertOfflineItemRequests);
  }

  @Test
  public void upsertL5ItemsInXProduct_success_noError() throws Exception {

    this.upsertOfflineItemPriceResponse.setSuccess(true);
    this.upsertOfflineItemPriceResponse.setErrorMessage(null);

    this.upsertOfflineItemPriceResponses = new ArrayList<>();
    this.upsertOfflineItemPriceResponses.add(this.upsertOfflineItemPriceResponse);

    Mockito.when(
            productOutbound.upsertOfflineItem(REQUESTID, USERNAME, MERCHANT_CODE_2, this.upsertOfflineItemRequests))
        .thenReturn(this.upsertOfflineItemPriceResponses);

    this.serviceBean.upsertL5ItemsInXProduct(REQUESTID, USERNAME, MERCHANT_CODE_2, this.upsertOfflineItems,
        this.failedUpsertOfflineItemResponses);

    Mockito.verify(productOutbound)
        .upsertOfflineItem(REQUESTID, USERNAME, MERCHANT_CODE_2, this.upsertOfflineItemRequests);
  }
}

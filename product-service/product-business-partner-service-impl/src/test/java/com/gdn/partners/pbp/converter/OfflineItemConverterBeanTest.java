package com.gdn.partners.pbp.converter;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.WeakHashMap;

import org.apache.commons.collections.CollectionUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;

import com.gdn.partners.pbp.dto.offlineitem.FailedOfflineItemResponse;
import com.gdn.partners.pbp.dto.offlineitem.SuccessOfflineItemResponse;
import com.gdn.partners.pbp.dto.offlineitem.OfflineItemInstantPickupRequest;
import com.gdn.partners.pbp.dto.offlineitem.OfflineItemInstantPickupV2Request;
import com.gdn.partners.pbp.dto.offlineitem.UpsertOfflineItemFailedResponse;
import com.gdn.partners.pbp.model.offlineitem.DeleteOfflineItem;
import com.gdn.partners.pbp.model.offlineitem.OfflineItem;
import com.gdn.partners.pbp.model.offlineitem.UpsertOfflineItem;
import com.gdn.partners.pbp.model.offlineitem.ValidOfflineItem;
import com.gdn.x.businesspartner.dto.ExternalPickupPointCodeResponseDetail;
import com.gdn.x.product.rest.web.model.dto.ItemSummaryResponse;
import com.gdn.x.product.rest.web.model.dto.OfflineItemPriceDTO;
import com.gdn.x.product.rest.web.model.request.DeleteOfflineItemRequest;
import com.gdn.x.product.rest.web.model.request.ItemSummaryRequest;
import com.gdn.x.product.rest.web.model.request.OfflineItemRequest;
import com.gdn.x.product.rest.web.model.request.UpsertOfflineItemRequest;
import com.gdn.x.product.rest.web.model.response.OfflineItemResponseDetail;
import com.gdn.x.product.rest.web.model.response.UpsertOfflineItemPriceResponse;

public class OfflineItemConverterBeanTest {

  @InjectMocks
  OfflineItemConverterBean offlineItemConverter;

  private List<OfflineItem> offlineItems;
  private List<FailedOfflineItemResponse> failedOfflineItemResponses;
  private List<SuccessOfflineItemResponse> successOfflineItemResponses;

  private Map<String, OfflineItemResponseDetail> merchantSkuAndItemSkuMap;
  private Map<String, ExternalPickupPointCodeResponseDetail> externalPickupPointAndPickupPointCodeMap;

  private List<OfflineItemResponseDetail> offlineItemResponseDetails;
  private List<ExternalPickupPointCodeResponseDetail> externalPickupPointCodeResponseDetails;

  private List<ValidOfflineItem> validOfflineItems;

  private DeleteOfflineItem deleteOfflineItem;
  private List<DeleteOfflineItem> deleteOfflineItems;

  private ExternalPickupPointCodeResponseDetail externalPickupPointCodeResponseDetail;
  private OfflineItemResponseDetail offlineItemResponseDetail;
  private ValidOfflineItem validOfflineItem;
  private FailedOfflineItemResponse failedOfflineItemResponse;
  private SuccessOfflineItemResponse successOfflineItemResponse;

  private UpsertOfflineItemRequest upsertOfflineItemRequest;
  private List<UpsertOfflineItemRequest> upsertOfflineItemRequests;
  private ValidOfflineItem validOfflineItemforUpsert;
  private List<ValidOfflineItem> validOfflineItemsforUpsert;
  private UpsertOfflineItem upsertOfflineItem;
  private List<UpsertOfflineItem> upsertOfflineItems;
  private List<String> validPickupPointCodes;
  private UpsertOfflineItemFailedResponse upsertOfflineItemFailedResponse;
  private UpsertOfflineItemPriceResponse upsertOfflineItemPriceResponse;
  private List<UpsertOfflineItemFailedResponse> failedUpsertOfflineItemResponses;

  private static final String DASH_SEPARATOR = "-";
  private static final String ITEM_SKU = "item-sku";
  private static final List<String> ITEM_SKUS = Collections.singletonList(ITEM_SKU);
  private static final String MERCHANT_SKU = "merchant-sku";
  private static final List<String> MERCHANT_SKUS = Collections.singletonList(MERCHANT_SKU);
  private static final String FAILED_MERCHANT_SKU = "failed-merchant-code";
  private static final String PICKUP_POINT_CODE = "pp-code";
  private static final List<String> PICKUP_POINT_CODE_LIST = Collections.singletonList(PICKUP_POINT_CODE);
  private static final String EXTERNAL_PICKUP_POINT_CODE = "external-pp-code";
  private static final String FAILED_EXTERNAL_PICKUP_POINT_CODE = "failed-external-pp-code";
  private static final String MERCHANT_CODE = "merchant-code";
  private static final String OFFLINE_ITEM_ID = ITEM_SKU.concat("-").concat(PICKUP_POINT_CODE);
  private static final String OFFLINE_INVENTORY_ID = ITEM_SKU.concat(DASH_SEPARATOR).concat(PICKUP_POINT_CODE);
  private static final String ERROR_MESSAGE = "error-message";
  private static final Double LIST_PRICE = 20000.00;
  private static final Double OFFER_PRICE = 10000.00;
  private static final String ERROR_CODE = "error";
  private static final int SAFETY_STOCK = 1;
  private static final String FILE_NAME = "file-name";
  private static final String INVENTORY_UNIQUE_ID_SEPARATOR = "_";
  private static final int STOCK = 1;
  private static final String ITEM_NAME = "item-name";
  private static final String CATEGORY_CODE = "category-code";
  private static final String GDN_SKU = "gdn-sku";
  private static final List<String> GDN_SKUS = Collections.singletonList(GDN_SKU);
  private static final String WEB_ITEM_SKU = "web-item-sku";
  private static final String WEB_MERCHANT_CODE = "web-merchant-code";
  private static final String OFFLINE_ITEM_KEY = WEB_ITEM_SKU + "_" + MERCHANT_CODE + "_" + PICKUP_POINT_CODE;
  private static final String FAILED_TO_UPDATE_STOCK_ERROR = "FAILED_TO_UPDATE_STOCK_ERROR";
  private static final String MERCHANT_CODE_2 = "AAA-xxxxx";
  private static final String ITEM_SKU_2 = "AAA-xxxxx-xxxxx-xxxxx";
  private static final String OFFLINE_ITEM_ID_2 = ITEM_SKU_2.concat("-").concat(PICKUP_POINT_CODE);
  private static final String OFFLINE_INVENTORY_ID_2 =
      ITEM_SKU_2.concat(DASH_SEPARATOR).concat(PICKUP_POINT_CODE);

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);

    OfflineItemResponseDetail offlineItemResponseDetail = new OfflineItemResponseDetail();
    offlineItemResponseDetail.setItemSku(ITEM_SKU);
    offlineItemResponseDetail.setMerchantSku(MERCHANT_SKU);

    OfflineItemResponseDetail nullItemSkuOfflineItem = new OfflineItemResponseDetail();
    nullItemSkuOfflineItem.setItemSku(null);
    nullItemSkuOfflineItem.setMerchantSku(FAILED_MERCHANT_SKU);

    offlineItemResponseDetails = new ArrayList<>();
    offlineItemResponseDetails.add(offlineItemResponseDetail);
    offlineItemResponseDetails.add(nullItemSkuOfflineItem);


    ExternalPickupPointCodeResponseDetail externalPickupPointCodeResponseDetail =
        new ExternalPickupPointCodeResponseDetail();
    externalPickupPointCodeResponseDetail.setExternalPickupPointCode(EXTERNAL_PICKUP_POINT_CODE);
    externalPickupPointCodeResponseDetail.setCode(PICKUP_POINT_CODE);

    ExternalPickupPointCodeResponseDetail nullCodeResponse =
        new ExternalPickupPointCodeResponseDetail();
    nullCodeResponse.setCode(null);
    nullCodeResponse.setExternalPickupPointCode(FAILED_EXTERNAL_PICKUP_POINT_CODE);

    externalPickupPointCodeResponseDetails = new ArrayList<>();
    externalPickupPointCodeResponseDetails.add(externalPickupPointCodeResponseDetail);
    externalPickupPointCodeResponseDetails.add(nullCodeResponse);

    offlineItemResponseDetail = new OfflineItemResponseDetail();
    offlineItemResponseDetail.setItemSku(ITEM_SKU);
    externalPickupPointCodeResponseDetail = new ExternalPickupPointCodeResponseDetail();
    externalPickupPointCodeResponseDetail.setCode(PICKUP_POINT_CODE);

    merchantSkuAndItemSkuMap = new HashMap();
    merchantSkuAndItemSkuMap.put(MERCHANT_SKU, offlineItemResponseDetail);

    externalPickupPointAndPickupPointCodeMap = new HashMap();
    externalPickupPointAndPickupPointCodeMap.put(EXTERNAL_PICKUP_POINT_CODE, externalPickupPointCodeResponseDetail);

    offlineItems = new ArrayList();
    OfflineItem offlineItem = new OfflineItem();
    offlineItem.setMerchantSku(MERCHANT_SKU);
    offlineItem.setExternalPickupPointCode(EXTERNAL_PICKUP_POINT_CODE);
    offlineItem.setFileName(FILE_NAME);
    offlineItems.add(offlineItem);

    offlineItem = new OfflineItem();
    offlineItem.setMerchantSku(FAILED_MERCHANT_SKU);
    offlineItem.setExternalPickupPointCode(FAILED_EXTERNAL_PICKUP_POINT_CODE);
    offlineItem.setFileName(FILE_NAME);
    offlineItems.add(offlineItem);

    validOfflineItem = new ValidOfflineItem();
    validOfflineItem.setMerchantSku(MERCHANT_SKU);
    validOfflineItem.setStock(STOCK);
    validOfflineItem.setExternalPickupPointCode(EXTERNAL_PICKUP_POINT_CODE);
    validOfflineItem.setItemSku(ITEM_SKU);
    validOfflineItem.setPickupPointCode(PICKUP_POINT_CODE);
    validOfflineItem.setListPrice(LIST_PRICE);
    validOfflineItem.setPrice(OFFER_PRICE);
    validOfflineItem.setFileName(FILE_NAME);

    validOfflineItems = new ArrayList<>();
    validOfflineItems.add(validOfflineItem);

    failedOfflineItemResponses = new ArrayList<>();

    failedOfflineItemResponse = new FailedOfflineItemResponse();
    failedOfflineItemResponse.setMerchantSku(FAILED_MERCHANT_SKU);
    failedOfflineItemResponse.setExternalPickupPointCode(EXTERNAL_PICKUP_POINT_CODE);

    failedOfflineItemResponses.add(failedOfflineItemResponse);

    successOfflineItemResponses = new ArrayList<>();
    successOfflineItemResponse = SuccessOfflineItemResponse.builder()
        .itemSku(ITEM_SKU)
        .merchantSku(MERCHANT_SKU)
        .pickupPointCode(PICKUP_POINT_CODE)
        .externalPickupPointCode(EXTERNAL_PICKUP_POINT_CODE)
        .listPrice(LIST_PRICE)
        .price(OFFER_PRICE)
        .build();
    successOfflineItemResponses.add(successOfflineItemResponse);

    this.deleteOfflineItems = new ArrayList<>();
    this.deleteOfflineItem = new DeleteOfflineItem();
    this.deleteOfflineItem.setItemSku(ITEM_SKU);
    this.deleteOfflineItem.setMerchantSku(MERCHANT_SKU);
    this.deleteOfflineItem.setPickupPointCode(PICKUP_POINT_CODE);
    this.deleteOfflineItems.add(this.deleteOfflineItem);

    this.upsertOfflineItemRequest = new UpsertOfflineItemRequest();
    this.upsertOfflineItemRequest.setItemSku(ITEM_SKU_2);
    this.upsertOfflineItemRequest.setListPrice(LIST_PRICE);
    this.upsertOfflineItemRequest.setOfferPrice(OFFER_PRICE);
    this.upsertOfflineItemRequest.setPickupPointCode(PICKUP_POINT_CODE);
    this.upsertOfflineItemRequest.setOfflineItemId(OFFLINE_ITEM_ID_2);

    this.upsertOfflineItemRequests = new ArrayList<>();
    this.upsertOfflineItemRequests.add(this.upsertOfflineItemRequest);

    this.validOfflineItemforUpsert = new ValidOfflineItem();
    this.validOfflineItemforUpsert.setItemSku(ITEM_SKU_2);
    this.validOfflineItemforUpsert.setListPrice(LIST_PRICE);
    this.validOfflineItemforUpsert.setPrice(OFFER_PRICE);
    this.validOfflineItemforUpsert.setPickupPointCode(PICKUP_POINT_CODE);
    this.validOfflineItemforUpsert.setFileName(FILE_NAME);
    this.validOfflineItemforUpsert.setStock(STOCK);

    this.validOfflineItemsforUpsert = new ArrayList<>();
    this.validOfflineItemsforUpsert.add(this.validOfflineItemforUpsert);

    this.upsertOfflineItem = new UpsertOfflineItem();
    this.upsertOfflineItem.setItemSku(ITEM_SKU_2);
    this.upsertOfflineItem.setListPrice(LIST_PRICE);
    this.upsertOfflineItem.setOfferPrice(OFFER_PRICE);
    this.upsertOfflineItem.setPickupPointCode(PICKUP_POINT_CODE);
    this.upsertOfflineItem.setFileName(FILE_NAME);
    this.upsertOfflineItem.setStock(STOCK);

    this.upsertOfflineItems = new ArrayList<>();
    this.upsertOfflineItems.add(this.upsertOfflineItem);

    this.validPickupPointCodes = Arrays.asList(PICKUP_POINT_CODE);

    this.upsertOfflineItemFailedResponse = new UpsertOfflineItemFailedResponse();
    this.upsertOfflineItemFailedResponse.setItemSku(ITEM_SKU_2);
    this.upsertOfflineItemFailedResponse.setPickupPointCode(PICKUP_POINT_CODE);
    this.upsertOfflineItemFailedResponse.setErrorCode(ERROR_CODE);

    this.upsertOfflineItemPriceResponse = new UpsertOfflineItemPriceResponse();
    this.upsertOfflineItemPriceResponse.setItemSku(ITEM_SKU_2);
    this.upsertOfflineItemPriceResponse.setOfferPrice(OFFER_PRICE);
    this.upsertOfflineItemPriceResponse.setPickupPointCode(PICKUP_POINT_CODE);
    this.upsertOfflineItemPriceResponse.setOfflineItemId(OFFLINE_ITEM_ID_2);
    this.upsertOfflineItemPriceResponse.setSuccess(false);
    this.upsertOfflineItemPriceResponse.setErrorMessage(ERROR_CODE);

    this.failedUpsertOfflineItemResponses = new ArrayList<>();
  }

  @AfterEach
  public void tearDown() throws Exception {
  }

  @Test
  public void constructValidItems_Valid_Success() {
    List<ValidOfflineItem> actual = offlineItemConverter
        .constructValidItems(offlineItems, merchantSkuAndItemSkuMap,
            externalPickupPointAndPickupPointCodeMap);

    Assertions.assertEquals(1, actual.size());
  }

  @Test
  public void constructValidItems_ValidItemSkuNull_Success() {
    this.offlineItems.get(1).setItemSku(null);
    this.offlineItems.get(1).setPickupPointCode(PICKUP_POINT_CODE);
    List<ValidOfflineItem> actual = offlineItemConverter
        .constructValidItems(offlineItems, merchantSkuAndItemSkuMap,
            externalPickupPointAndPickupPointCodeMap);

    Assertions.assertEquals(1, actual.size());
  }

  @Test
  public void constructValidItems_ValidProductSkuNull_Success() {
    this.offlineItems.get(1).setItemSku(ITEM_SKU);
    this.offlineItems.get(1).setPickupPointCode(null);
    List<ValidOfflineItem> actual = offlineItemConverter
        .constructValidItems(offlineItems, merchantSkuAndItemSkuMap,
            externalPickupPointAndPickupPointCodeMap);

    Assertions.assertEquals(1, actual.size());
  }

  @Test
  public void constructValidItems_Valid_Success_Without_Map() {
    offlineItems.get(0).setItemSku(ITEM_SKU);
    offlineItems.get(0).setPickupPointCode(PICKUP_POINT_CODE);
    List<ValidOfflineItem> actual = offlineItemConverter
        .constructValidItems(offlineItems, new HashMap<>(), new HashMap<>());

    Assertions.assertEquals(1, actual.size());
  }

  @Test
  public void constructValidItems_Map_Errors() {
    merchantSkuAndItemSkuMap.get(MERCHANT_SKU).setErrorCode(ERROR_CODE);
    externalPickupPointAndPickupPointCodeMap.get(EXTERNAL_PICKUP_POINT_CODE).setErrorCode(ERROR_CODE);
    List<ValidOfflineItem> actual = offlineItemConverter
        .constructValidItems(offlineItems, merchantSkuAndItemSkuMap,
            externalPickupPointAndPickupPointCodeMap);

    Assertions.assertEquals(0, actual.size());
  }
  @Test
  public void constructValidItems_Valid_Success2() {
    merchantSkuAndItemSkuMap.keySet().stream().forEach(s -> merchantSkuAndItemSkuMap.get(s).setErrorCode(ERROR_CODE));
    externalPickupPointAndPickupPointCodeMap.keySet().forEach(s -> externalPickupPointAndPickupPointCodeMap.get(s).setErrorCode(ERROR_CODE));

    List<ValidOfflineItem> actual = offlineItemConverter
        .constructValidItems(offlineItems, merchantSkuAndItemSkuMap,
            externalPickupPointAndPickupPointCodeMap);

    Assertions.assertEquals(0, actual.size());
  }

  @Test
  public void constructValidItems_emptyOfflineItems() {
    List<ValidOfflineItem> actual = offlineItemConverter
        .constructValidItems(null, merchantSkuAndItemSkuMap,
            externalPickupPointAndPickupPointCodeMap);

    Assertions.assertEquals(0, actual.size());
  }

  @Test
  public void convertOfflineItemssToOfflineItemRequests_Valid_Success() {
    List<OfflineItemRequest> actual = offlineItemConverter
        .convertOfflineItemsToOfflineItemRequests(validOfflineItems);

    Assertions.assertEquals(1, actual.size());
    Assertions.assertEquals(OFFLINE_ITEM_ID, actual.get(0).getOfflineItemId());
  }

  @Test
  public void convertOfflineItemssToOfflineItemRequests_emptyOfflineItems() {
    List<OfflineItemRequest> actual = offlineItemConverter.convertOfflineItemsToOfflineItemRequests(null);

    Assertions.assertEquals(0, actual.size());
  }

  @Test
  public void convertOfflineItemToFailedItem_Valid_Success() {
    FailedOfflineItemResponse actual = offlineItemConverter
        .convertOfflineItemToFailedItem(offlineItems.get(0), ERROR_CODE);

    Assertions.assertEquals(MERCHANT_SKU, actual.getMerchantSku());
  }

  @Test
  public void convertUpdateOfflineItemPriceToFailedItem_Valid_Success() {
    UpsertOfflineItemPriceResponse upsertOfflineItemPriceResponse = new UpsertOfflineItemPriceResponse();
    upsertOfflineItemPriceResponse.setMerchantSku(MERCHANT_SKU);
    upsertOfflineItemPriceResponse.setExternalPickupPointCode(EXTERNAL_PICKUP_POINT_CODE);
    upsertOfflineItemPriceResponse.setErrorMessage("error");

    FailedOfflineItemResponse actual = offlineItemConverter
        .convertUpdateOfflineItemPriceToFailedItem(upsertOfflineItemPriceResponse, ERROR_CODE);

    Assertions.assertEquals(MERCHANT_SKU, actual.getMerchantSku());
    Assertions.assertEquals(EXTERNAL_PICKUP_POINT_CODE, actual.getExternalPickupPointCode());
    Assertions.assertEquals(ERROR_CODE, actual.getErrorCode());
  }

  @Test
  public void convertOfflineItemInstantPickupRequestToItemSummaryRequest_valid_success(){
    OfflineItemInstantPickupRequest request = new OfflineItemInstantPickupRequest();
    request.setItemName(OfflineItemConverterBeanTest.ITEM_NAME);
    request.setCategoryCode(OfflineItemConverterBeanTest.CATEGORY_CODE);
    request.setMerchantSku(OfflineItemConverterBeanTest.MERCHANT_SKU);
    request.setGdnSku(OfflineItemConverterBeanTest.GDN_SKU);
    request.setMerchantCode(OfflineItemConverterBeanTest.MERCHANT_CODE);
    request.setPickupPointCode(OfflineItemConverterBeanTest.PICKUP_POINT_CODE);
    ItemSummaryRequest itemSummaryRequest = offlineItemConverter
        .convertOfflineItemInstantPickupRequestToItemSummaryRequest(request);
    Assertions.assertEquals(ITEM_NAME, itemSummaryRequest.getProductItemName());
    Assertions.assertEquals(CATEGORY_CODE, itemSummaryRequest.getMasterCategoryCode());
    Assertions.assertEquals(MERCHANT_SKU, itemSummaryRequest.getMerchantSku());
    Assertions.assertEquals(PICKUP_POINT_CODE, itemSummaryRequest.getPickupPointCode());
    Assertions.assertEquals(MERCHANT_CODE, itemSummaryRequest.getMerchantCode());
    Assertions.assertEquals(true, itemSummaryRequest.isCncActivated());
  }

  @Test
  public void convertOfflineItemInstantPickupV2RequestToItemSummaryRequest_valid_success() {
    OfflineItemInstantPickupV2Request request = OfflineItemInstantPickupV2Request.builder()
        .merchantCode(MERCHANT_CODE)
        .gdnSkus(GDN_SKUS)
        .merchantSkus(MERCHANT_SKUS)
        .pickupPointCodes(PICKUP_POINT_CODE_LIST)
        .itemName(ITEM_NAME)
        .categoryCode(CATEGORY_CODE)
        .build();
    ItemSummaryRequest itemSummaryRequest =
        offlineItemConverter.convertOfflineItemInstantPickupV2RequestToItemSummaryRequest(request);
    Assertions.assertEquals(GDN_SKUS, itemSummaryRequest.getItemSkus());
    Assertions.assertEquals(MERCHANT_SKUS, itemSummaryRequest.getMerchantSkus());
    Assertions.assertEquals(PICKUP_POINT_CODE_LIST, itemSummaryRequest.getPickupPointCodes());
    Assertions.assertEquals(ITEM_NAME, itemSummaryRequest.getProductItemName());
    Assertions.assertEquals(CATEGORY_CODE, itemSummaryRequest.getMasterCategoryCode());
    Assertions.assertEquals(Boolean.TRUE, itemSummaryRequest.isCncActivated());
    Assertions.assertEquals(Boolean.FALSE, itemSummaryRequest.getArchived());
  }

  @Test
  public void convertItemSummaryResponseToListOfGdnSku_valid_success() {
    List<ItemSummaryResponse> itemSummaryResponses = new ArrayList<>();
    ItemSummaryResponse itemSummaryResponse = new ItemSummaryResponse();
    itemSummaryResponse.setItemSku(ITEM_SKU);
    itemSummaryResponses.add(itemSummaryResponse);
    List<String> gdnSkus =
        offlineItemConverter.convertItemSummaryResponseToListOfGdnSku(itemSummaryResponses);
    Assertions.assertNotNull(gdnSkus);
    Assertions.assertEquals(1, gdnSkus.size());
    Assertions.assertEquals(OfflineItemConverterBeanTest.ITEM_SKU, gdnSkus.get(0));
  }

  @Test
  public void convertItemSummaryResponseToMapOfPickupPointCodeAndListOfGdnSku_valid_success() {
    OfflineItemPriceDTO offlinePrice = new OfflineItemPriceDTO();
    offlinePrice.setPickupPointCode(PICKUP_POINT_CODE);
    List<OfflineItemPriceDTO> offlinePrices = new ArrayList<>();
    offlinePrices.add(offlinePrice);

    ItemSummaryResponse itemSummaryResponse = new ItemSummaryResponse();
    itemSummaryResponse.setItemSku(ITEM_SKU);
    itemSummaryResponse.setOfflinePrices(offlinePrices);
    List<ItemSummaryResponse> itemSummaryResponses = new ArrayList<>();
    itemSummaryResponses.add(itemSummaryResponse);

    Map<String, List<String>> pickupPointCodesAndListOfGdnSkus = offlineItemConverter
        .convertItemSummaryResponseToMapOfPickupPointCodeAndListOfGdnSku(itemSummaryResponses);
    Assertions.assertNotNull(pickupPointCodesAndListOfGdnSkus);
    Assertions.assertEquals(1, pickupPointCodesAndListOfGdnSkus.size());
    Assertions.assertEquals(ITEM_SKUS, pickupPointCodesAndListOfGdnSkus.get(PICKUP_POINT_CODE));
  }

  @Test
  public void convertItemSummaryResponseToMapOfPickupPointCodeAndListOfGdnSku_multipleSkuSamePickupPoint_success() {
    OfflineItemPriceDTO offlinePrice = new OfflineItemPriceDTO();
    offlinePrice.setPickupPointCode(PICKUP_POINT_CODE);
    List<OfflineItemPriceDTO> offlinePrices = new ArrayList<>();
    offlinePrices.add(offlinePrice);

    ItemSummaryResponse itemSummaryResponse = new ItemSummaryResponse();
    itemSummaryResponse.setItemSku(ITEM_SKU);
    itemSummaryResponse.setOfflinePrices(offlinePrices);
    ItemSummaryResponse itemSummaryResponse2 = new ItemSummaryResponse();
    itemSummaryResponse2.setItemSku(ITEM_SKU_2);
    itemSummaryResponse2.setOfflinePrices(offlinePrices);
    List<ItemSummaryResponse> itemSummaryResponses = new ArrayList<>();
    itemSummaryResponses.add(itemSummaryResponse);
    itemSummaryResponses.add(itemSummaryResponse2);

    List<String> expectedSkus = Arrays.asList(ITEM_SKU, ITEM_SKU_2);

    Map<String, List<String>> pickupPointCodesAndListOfGdnSkus = offlineItemConverter
          .convertItemSummaryResponseToMapOfPickupPointCodeAndListOfGdnSku(itemSummaryResponses);
    Assertions.assertNotNull(pickupPointCodesAndListOfGdnSkus);
    Assertions.assertEquals(1, pickupPointCodesAndListOfGdnSkus.size());
    Assertions.assertEquals(expectedSkus, pickupPointCodesAndListOfGdnSkus.get(PICKUP_POINT_CODE));
  }

  @Test
  public void convertItemSummaryResponseToListOfGdnSku_emptySource_success() {
    List<String> gdnSkus =
        offlineItemConverter.convertItemSummaryResponseToListOfGdnSku(null);
    Assertions.assertNotNull(gdnSkus);
    Assertions.assertEquals(0, gdnSkus.size());
  }


  @Test
  public void testConvertToDeleteOfflineItemRequest() {
    List<DeleteOfflineItemRequest> result =
        this.offlineItemConverter.convertToDeleteOfflineItemRequest(this.deleteOfflineItems);
    Assertions.assertNotNull(result);
    Assertions.assertEquals(1, result.size());
    Assertions.assertEquals(ITEM_SKU, result.get(0).getItemSku());
    Assertions.assertEquals(MERCHANT_SKU, result.get(0).getMerchantSku());
    Assertions.assertEquals(PICKUP_POINT_CODE, result.get(0).getPickupPointCode());
  }

  @Test
  public void convertOfflineItemToFailedOfflineItemResponse_valid_success(){

    FailedOfflineItemResponse response = this.offlineItemConverter
        .convertOfflineItemToFailedOfflineItemResponse(validOfflineItem, ERROR_MESSAGE);

    Assertions.assertEquals(response.getMerchantSku(), MERCHANT_SKU);
    Assertions.assertEquals(response.getErrorCode(), ERROR_MESSAGE);
    Assertions.assertEquals(response.getExternalPickupPointCode(), EXTERNAL_PICKUP_POINT_CODE);
  }

  @Test
  public void convertValidOfflineItemsToUpsertOfflineItemRequestsTest_success() {
    List<UpsertOfflineItemRequest> result = this.offlineItemConverter
        .convertValidOfflineItemsToUpsertOfflineItemRequests(this.validOfflineItemsforUpsert);

    Assertions.assertEquals(result, this.upsertOfflineItemRequests);
  }

  @Test
  public void convertValidOfflineItemsToUpsertOfflineItemRequestsTest_emptyRequest() {
    List<UpsertOfflineItemRequest> result = this.offlineItemConverter
        .convertValidOfflineItemsToUpsertOfflineItemRequests(Collections.emptyList());

    Assertions.assertEquals(result, Collections.emptyList());
  }

  @Test
  public void constructValidOfflineItemsTest_success() {
    List<ValidOfflineItem> result = this.offlineItemConverter
        .constructValidOfflineItems(this.upsertOfflineItems, MERCHANT_CODE_2,
            this.validPickupPointCodes);

    Assertions.assertEquals(result.size(), 1);
    Assertions.assertEquals(result.get(0).getItemSku(), ITEM_SKU_2);
    Assertions.assertEquals(result.get(0).getPickupPointCode(), PICKUP_POINT_CODE);
    Assertions.assertEquals(result.get(0).getFileName(), FILE_NAME);
    Assertions.assertEquals(result.get(0).getStock(), Integer.valueOf(STOCK));
    Assertions.assertEquals(result.get(0).getListPrice(), LIST_PRICE);
    Assertions.assertEquals(result.get(0).getPrice(), OFFER_PRICE);
  }

  @Test
  public void constructValidOfflineItemsTest_invalidItemSku() {
    List<ValidOfflineItem> result = this.offlineItemConverter
        .constructValidOfflineItems(this.upsertOfflineItems, MERCHANT_CODE,
            this.validPickupPointCodes);

    Assertions.assertEquals(result.size(), 0);
  }

  @Test
  public void convertToUpsertOfflineItemFailedResponseTest_success() {
    UpsertOfflineItemFailedResponse result = this.offlineItemConverter
        .convertToUpsertOfflineItemFailedResponse(this.upsertOfflineItemPriceResponse, ERROR_CODE);

    Assertions.assertEquals(result.getErrorCode(), ERROR_CODE);
    Assertions.assertEquals(result.getItemSku(), ITEM_SKU_2);
    Assertions.assertEquals(result.getPickupPointCode(), PICKUP_POINT_CODE);
  }

  @Test
  public void convertUpsertOfflineItemToUpsertOfflineItemDetailResponseTest_success() {
    UpsertOfflineItemFailedResponse result = this.offlineItemConverter
        .convertUpsertOfflineItemToUpsertOfflineItemFailedResponse(this.upsertOfflineItem,
            ERROR_CODE);

    Assertions.assertEquals(result.getErrorCode(), ERROR_CODE);
    Assertions.assertEquals(result.getItemSku(), ITEM_SKU_2);
    Assertions.assertEquals(result.getPickupPointCode(), PICKUP_POINT_CODE);
  }

  @Test
  public void convertOfflineItemToUpsertOfflineItemDetailResponseTest_success() {
    UpsertOfflineItemFailedResponse result = this.offlineItemConverter
        .convertOfflineItemToUpsertOfflineItemDetailResponse(this.validOfflineItemforUpsert,
            ERROR_CODE);

    Assertions.assertEquals(result.getErrorCode(), ERROR_CODE);
    Assertions.assertEquals(result.getItemSku(), ITEM_SKU_2);
    Assertions.assertEquals(result.getPickupPointCode(), PICKUP_POINT_CODE);
  }
}

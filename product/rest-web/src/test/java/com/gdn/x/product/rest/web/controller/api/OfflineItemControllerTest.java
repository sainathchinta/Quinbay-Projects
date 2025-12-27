package com.gdn.x.product.rest.web.controller.api;

import static com.gdn.x.product.enums.Constants.REGULAR_MERCHANT_CLIENT_ID;
import static org.hamcrest.Matchers.equalTo;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyMap;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.openMocks;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.setup.MockMvcBuilders.standaloneSetup;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.io.FileUtils;
import org.hamcrest.CoreMatchers;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.OfflineItem;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.vo.DeleteOfflineItemVO;
import com.gdn.x.product.model.vo.ItemPickupPointPriceVo;
import com.gdn.x.product.model.vo.ProductAndItemsVO;
import com.gdn.x.product.model.vo.UpsertOfflineItemPriceResponseVO;
import com.gdn.x.product.rest.web.model.ProductApiPath;
import com.gdn.x.product.rest.web.model.enums.ProductErrorCodesEnum;
import com.gdn.x.product.rest.web.model.request.DeleteOfflineItemRequest;
import com.gdn.x.product.rest.web.model.request.OfflineItemRequest;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.rest.web.model.request.UpdateOfflineItemPriceRequest;
import com.gdn.x.product.rest.web.model.request.UpsertOfflineItemRequest;
import com.gdn.x.product.rest.web.model.response.DeleteOfflineItemResponse;
import com.gdn.x.product.rest.web.model.response.OfflineItemPriceResponse;
import com.gdn.x.product.rest.web.model.response.OfflineItemResponseDetail;
import com.gdn.x.product.rest.web.model.response.ProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.response.UpsertOfflineItemPriceResponse;
import com.gdn.x.product.service.api.ItemPickupPointService;
import com.gdn.x.product.service.api.ItemService;
import com.gdn.x.product.service.api.OfflineItemService;
import com.gdn.x.product.service.api.ProductService;
import com.gdn.x.product.service.util.ModelConverter;

public class OfflineItemControllerTest {

  private static final String STORE_ID = "store-id";
  private static final String CHANNEL_ID = "channel-id";
  private static final String CLIENT_ID = "client-id";
  private static final String REQUEST_ID = "request-id";
  private static final String USERNAME = "username";
  private static final String ITEM_SKU = "item-sku";
  private static final String ITEM_SKU_2 = "TEM-00001-00001-00001";
  private static final Set<String> ITEM_SKU_SET = new HashSet<>(Arrays.asList(ITEM_SKU));
  private static final String MERCHANT_SKU = "merchant-sku";
  private static final String MERCHANT_CODE = "merchant-code";
  private static final String MERCHANT_CODE_2 = "TEM-00001";
  private static final String PICKUP_POINT_CODE = "pickup-point-code";
  private static final String PICKUP_POINT_CODE_2 = "PP-1752591";
  private static final String EXTERNAL_PICKUP_POINT_CODE = "external-pickup-point-code";
  private static final String PRODUCT_SKU = "product-sku";
  private static final Double LIST_PRICE = 150000.0;
  private static final Double OFFER_PRICE = 100000.0;
  private static final String BLANK = "";
  private static final String UNIQUE_ID = "UNIQUE_ID";
  private static final String BATCH_SIZE = "10";
  private static final String OFFLINE_ITEM_ID = "item-sku-pickup-point-code";


  private static final List<String> MERCHANT_CODE_LIST = Arrays.asList(MERCHANT_CODE_2);

  @InjectMocks
  private OfflineItemController offlineItemController;

  @Mock
  private ModelConverter modelConverter;

  @Mock
  private OfflineItemService offlineItemService;

  @Mock
  private ItemPickupPointService itemPickupPointService;

  @Mock
  private ItemService itemService;

  @Mock
  private ProductService productService;

  private MockMvc mockMvc;
  private ObjectMapper objectMapper;
  private OfflineItemRequest offlineItemRequest;
  private List<OfflineItemRequest> offlineItemRequestList;
  private OfflineItem offlineItem;
  private OfflineItem offlineItemForUpdatePriceByItemSku;
  private OfflineItem offlineItemForDelete;
  private String offlineItemRequestJson;
  private Product product;
  private List<OfflineItem> offlineItemList;
  private DeleteOfflineItemRequest deleteOfflineItemRequest;
  private String deleteOfflineItemRequestJson;
  private List<DeleteOfflineItemVO> deleteOfflineItemVOs;
  private List<DeleteOfflineItemResponse> deleteOfflineItemResponses;
  private List<UpsertOfflineItemPriceResponseVO> upsertOfflineItemPriceResponseVOs;
  private List<UpsertOfflineItemPriceResponse> content;
  private MandatoryRequestParam mandatoryRequestParam;
  private Map<String, ProductType> productTypeByProductSkuMap;
  private Set<String> productSkus;
  private UpsertOfflineItemRequest upsertOfflineItemRequest;
  private List<UpsertOfflineItemRequest> upsertOfflineItemRequests;
  private String upsertOfflineItemRequestJson;
  private OfflineItem offlineItemForUpsert;
  private List<OfflineItem> offlineItemsForUpsert;
  private UpdateOfflineItemPriceRequest updateOfflineItemPriceRequest;
  private String updateOfflineItemPriceRequestJson;
  private ItemPickupPoint itemPickupPointForCncInactive;
  private List<ItemPickupPoint> itemPickupPointList;
  private List<DeleteOfflineItemRequest> deleteOfflineItemRequestList;

  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);
    this.mockMvc = standaloneSetup(this.offlineItemController).build();
    this.objectMapper = new ObjectMapper();

    this.offlineItemRequestJson =
        FileUtils.readFileToString(new File("src/test/resources/offlineItemRequest.json"));
    this.deleteOfflineItemRequestJson =
        FileUtils.readFileToString(new File("src/test/resources/deleteOfflineItemsRequest.json"));
    this.updateOfflineItemPriceRequestJson =
      FileUtils.readFileToString(new File("src/test/resources/updateOfflineItemPriceRequest.json"));

    this.offlineItemRequest = new OfflineItemRequest();
    this.offlineItemRequest.setItemSku(OfflineItemControllerTest.ITEM_SKU);
    this.offlineItemRequest.setMerchantSku(OfflineItemControllerTest.MERCHANT_SKU);
    this.offlineItemRequest.setPickupPointCode(OfflineItemControllerTest.PICKUP_POINT_CODE);
    this.offlineItemRequest.setExternalPickupPointCode(
        OfflineItemControllerTest.EXTERNAL_PICKUP_POINT_CODE);
    this.offlineItemRequest.setListPrice(OfflineItemControllerTest.LIST_PRICE);
    this.offlineItemRequest.setOfferPrice(OfflineItemControllerTest.OFFER_PRICE);

    this.offlineItemRequestList = new ArrayList<>();
    this.offlineItemRequestList.add(offlineItemRequest);

    this.offlineItemRequestJson = objectMapper.writeValueAsString(this.offlineItemRequestList);

    this.offlineItem = new OfflineItem();
    this.offlineItem.setItemSku(OfflineItemControllerTest.ITEM_SKU);
    this.offlineItem.setMerchantSku(OfflineItemControllerTest.MERCHANT_SKU);
    this.offlineItem.setPickupPointCode(OfflineItemControllerTest.PICKUP_POINT_CODE);
    this.offlineItem.setExternalPickupPointCode(
        OfflineItemControllerTest.EXTERNAL_PICKUP_POINT_CODE);
    this.offlineItem.setListPrice(OfflineItemControllerTest.LIST_PRICE);
    this.offlineItem.setOfferPrice(OfflineItemControllerTest.OFFER_PRICE);

    this.offlineItemForUpdatePriceByItemSku = new OfflineItem();
    this.offlineItemForUpdatePriceByItemSku.setItemSku(OfflineItemControllerTest.ITEM_SKU);
    this.offlineItemForUpdatePriceByItemSku.setListPrice(OfflineItemControllerTest.LIST_PRICE);
    this.offlineItemForUpdatePriceByItemSku.setOfferPrice(OfflineItemControllerTest.OFFER_PRICE);

    this.offlineItemList = new ArrayList<>();
    this.offlineItemList.add(offlineItem);

    this.offlineItemForDelete = new OfflineItem();
    this.offlineItemForDelete.setItemSku(OfflineItemControllerTest.ITEM_SKU_2);
    this.offlineItemForDelete.setMerchantSku(OfflineItemControllerTest.MERCHANT_SKU);
    this.offlineItemForDelete.setPickupPointCode(OfflineItemControllerTest.PICKUP_POINT_CODE_2);
    this.offlineItemForDelete.setExternalPickupPointCode(
        OfflineItemControllerTest.EXTERNAL_PICKUP_POINT_CODE);
    this.offlineItemForDelete.setListPrice(OfflineItemControllerTest.LIST_PRICE);
    this.offlineItemForDelete.setOfferPrice(OfflineItemControllerTest.OFFER_PRICE);
    this.offlineItemForDelete.setMerchantCode(OfflineItemControllerTest.MERCHANT_CODE_2);

    this.product = new Product();
    this.productSkus = new HashSet<>();

    this.deleteOfflineItemRequest = new DeleteOfflineItemRequest();
    this.deleteOfflineItemRequest.setItemSku(ITEM_SKU_2);
    this.deleteOfflineItemRequest.setMerchantSku(MERCHANT_SKU);
    this.deleteOfflineItemRequest.setPickupPointCode(PICKUP_POINT_CODE_2);

    DeleteOfflineItemVO deleteOfflineItemVO = new DeleteOfflineItemVO();
    deleteOfflineItemVO.setItemSku(ITEM_SKU_2);
    deleteOfflineItemVO.setPickupPointCode(PICKUP_POINT_CODE_2);
    this.deleteOfflineItemVOs = Collections.singletonList(deleteOfflineItemVO);

    DeleteOfflineItemResponse deleteOfflineItemResponse = new DeleteOfflineItemResponse();
    deleteOfflineItemResponse.setItemSku(ITEM_SKU);
    deleteOfflineItemResponse.setPickupPointCode(PICKUP_POINT_CODE);
    deleteOfflineItemResponse.setSuccess(Boolean.TRUE);
    this.deleteOfflineItemResponses = Collections.singletonList(deleteOfflineItemResponse);

    UpsertOfflineItemPriceResponseVO
        upsertOfflineItemPriceResponseVO = new UpsertOfflineItemPriceResponseVO();
    upsertOfflineItemPriceResponseVO.setPickupPointCode(offlineItem.getPickupPointCode());
    upsertOfflineItemPriceResponseVO.setListPrice(offlineItem.getListPrice());
    upsertOfflineItemPriceResponseVO.setOfferPrice(offlineItem.getOfferPrice());
    upsertOfflineItemPriceResponseVO.setMerchantSku(offlineItem.getMerchantSku());
    upsertOfflineItemPriceResponseVO.setExternalPickupPointCode(offlineItem.getExternalPickupPointCode());
    upsertOfflineItemPriceResponseVO.setItemSku(offlineItem.getItemSku());

    this.upsertOfflineItemPriceResponseVOs = new ArrayList<>();
    this.upsertOfflineItemPriceResponseVOs.add(upsertOfflineItemPriceResponseVO);

    UpsertOfflineItemPriceResponse
        upsertOfflineItemPriceResponse = new UpsertOfflineItemPriceResponse();
    upsertOfflineItemPriceResponse.setPickupPointCode(offlineItem.getPickupPointCode());
    upsertOfflineItemPriceResponse.setListPrice(offlineItem.getListPrice());
    upsertOfflineItemPriceResponse.setOfferPrice(offlineItem.getOfferPrice());
    upsertOfflineItemPriceResponse.setMerchantSku(offlineItem.getMerchantSku());
    upsertOfflineItemPriceResponse.setExternalPickupPointCode(offlineItem.getExternalPickupPointCode());
    upsertOfflineItemPriceResponse.setItemSku(offlineItem.getItemSku());

    this.content = new ArrayList<>();
    content.add(upsertOfflineItemPriceResponse);

    this.mandatoryRequestParam = MandatoryRequestParam
        .generateMandatoryRequestParam(OfflineItemControllerTest.STORE_ID, OfflineItemControllerTest.CHANNEL_ID,
            OfflineItemControllerTest.CLIENT_ID, OfflineItemControllerTest.REQUEST_ID,
            OfflineItemControllerTest.USERNAME, OfflineItemControllerTest.BLANK);

    this.upsertOfflineItemRequest = new UpsertOfflineItemRequest();
    this.upsertOfflineItemRequest.setOfflineItemId(OfflineItemControllerTest.OFFLINE_ITEM_ID);
    this.upsertOfflineItemRequest.setItemSku(OfflineItemControllerTest.ITEM_SKU);
    this.upsertOfflineItemRequest.setPickupPointCode(OfflineItemControllerTest.PICKUP_POINT_CODE);
    this.upsertOfflineItemRequest.setListPrice(OfflineItemControllerTest.LIST_PRICE);
    this.upsertOfflineItemRequest.setOfferPrice(OfflineItemControllerTest.OFFER_PRICE);

    this.upsertOfflineItemRequests = new ArrayList<>();
    this.upsertOfflineItemRequests.add(this.upsertOfflineItemRequest);

    this.upsertOfflineItemRequestJson = this.objectMapper.writeValueAsString(this.upsertOfflineItemRequests);

    this.updateOfflineItemPriceRequest = new UpdateOfflineItemPriceRequest();
    this.updateOfflineItemPriceRequest.setItemSku(ITEM_SKU);
    this.updateOfflineItemPriceRequest.setListPrice(LIST_PRICE);
    this.updateOfflineItemPriceRequest.setOfferPrice(OFFER_PRICE);

    this.offlineItemForUpsert = new OfflineItem();
    this.offlineItemForUpsert.setOfflineItemId(OfflineItemControllerTest.OFFLINE_ITEM_ID);
    this.offlineItemForUpsert.setItemSku(OfflineItemControllerTest.ITEM_SKU);
    this.offlineItemForUpsert.setPickupPointCode(OfflineItemControllerTest.PICKUP_POINT_CODE);
    this.offlineItemForUpsert.setListPrice(OfflineItemControllerTest.LIST_PRICE);
    this.offlineItemForUpsert.setOfferPrice(OfflineItemControllerTest.OFFER_PRICE);
    this.offlineItemForUpsert.setMerchantCode(OfflineItemControllerTest.MERCHANT_CODE);

    this.offlineItemsForUpsert = new ArrayList<>();
    this.offlineItemsForUpsert.add(this.offlineItemForUpsert);

    itemPickupPointList = new ArrayList<>();
    this.itemPickupPointForCncInactive = new ItemPickupPoint();
    this.itemPickupPointForCncInactive.setItemSku(ITEM_SKU);
    this.itemPickupPointForCncInactive.setMerchantCode(MERCHANT_CODE_2);
    itemPickupPointList.add(this.itemPickupPointForCncInactive);
    deleteOfflineItemRequestList = new ArrayList<>();
    deleteOfflineItemRequestList.add(deleteOfflineItemRequest);
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(this.modelConverter);
    verifyNoMoreInteractions(this.itemService);
    verifyNoMoreInteractions(this.offlineItemService);
    verifyNoMoreInteractions(this.productService);
  }

  @Test
  public void upsertOfflineItemPriceSuccess() throws Exception {
    when(this.modelConverter.convertToOfflineItemList(this.offlineItemRequestList, MERCHANT_CODE, STORE_ID))
        .thenReturn(this.upsertOfflineItemRequests);

    when(this.offlineItemService.upsertOfflineItem(this.mandatoryRequestParam, USERNAME, MERCHANT_CODE, upsertOfflineItemRequests))
        .thenReturn(upsertOfflineItemPriceResponseVOs);

    when(this.modelConverter.convertListToResponse(upsertOfflineItemPriceResponseVOs, UpsertOfflineItemPriceResponse.class))
        .thenReturn(content);

    this.mockMvc.perform(
        post(ProductApiPath.OFFLINE_ITEM + ProductApiPath.UPSERT_OFFLINE_ITEM_PRICE)
            .accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).content(offlineItemRequestJson)
            .param("storeId", STORE_ID)
            .param("channelId", CHANNEL_ID)
            .param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID)
            .param("username", USERNAME)
            .param("merchantCode", MERCHANT_CODE))
        .andExpect(status().isOk());

    verify(this.modelConverter).convertToOfflineItemList(this.offlineItemRequestList, MERCHANT_CODE, STORE_ID);
    verify(this.offlineItemService).upsertOfflineItem(this.mandatoryRequestParam, USERNAME, MERCHANT_CODE, upsertOfflineItemRequests);
    verify(this.modelConverter).convertListToResponse(upsertOfflineItemPriceResponseVOs, UpsertOfflineItemPriceResponse.class);
  }

  @Test
  public void upsertOfflineItemPriceException() throws Exception {
    when(this.modelConverter.convertToOfflineItemList(this.offlineItemRequestList, MERCHANT_CODE, STORE_ID))
        .thenReturn(this.upsertOfflineItemRequests);

    when(this.offlineItemService.upsertOfflineItem(this.mandatoryRequestParam, USERNAME, MERCHANT_CODE, upsertOfflineItemRequests))
        .thenReturn(upsertOfflineItemPriceResponseVOs);

    when(this.modelConverter.convertListToResponse(upsertOfflineItemPriceResponseVOs, UpsertOfflineItemPriceResponse.class))
        .thenThrow(new RuntimeException("error"));

    this.mockMvc.perform(
        post(ProductApiPath.OFFLINE_ITEM + ProductApiPath.UPSERT_OFFLINE_ITEM_PRICE)
            .accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).content(offlineItemRequestJson)
            .param("storeId", STORE_ID)
            .param("channelId", CHANNEL_ID)
            .param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID)
            .param("username", USERNAME)
            .param("merchantCode", MERCHANT_CODE))
        .andExpect(status().isOk());

    verify(this.modelConverter).convertToOfflineItemList(this.offlineItemRequestList, MERCHANT_CODE, STORE_ID);
    verify(this.offlineItemService).upsertOfflineItem(this.mandatoryRequestParam, USERNAME, MERCHANT_CODE, upsertOfflineItemRequests);
    verify(this.modelConverter).convertListToResponse(upsertOfflineItemPriceResponseVOs, UpsertOfflineItemPriceResponse.class);
  }

  private List<UpsertOfflineItemPriceResponseVO> mockUpsertOfflineItemPriceResponseVOs() {
    UpsertOfflineItemPriceResponseVO upsertOfflineItemPriceResponseVO = new UpsertOfflineItemPriceResponseVO();
    upsertOfflineItemPriceResponseVO.setOfflineItemId(OfflineItemControllerTest.OFFLINE_ITEM_ID);
    upsertOfflineItemPriceResponseVO.setItemSku(OfflineItemControllerTest.ITEM_SKU);
    upsertOfflineItemPriceResponseVO.setPickupPointCode(OfflineItemControllerTest.PICKUP_POINT_CODE);
    upsertOfflineItemPriceResponseVO.setListPrice(OfflineItemControllerTest.LIST_PRICE);
    upsertOfflineItemPriceResponseVO.setOfferPrice(OfflineItemControllerTest.OFFER_PRICE);

    List<UpsertOfflineItemPriceResponseVO> upsertOfflineItemPriceResponseVOs = new ArrayList<>();
    upsertOfflineItemPriceResponseVOs.add(upsertOfflineItemPriceResponseVO);

    return upsertOfflineItemPriceResponseVOs;
  }

  private List<UpsertOfflineItemPriceResponse> mockUpsertOfflineItemPriceResponses() {
    UpsertOfflineItemPriceResponse upsertOfflineItemPriceResponse = new UpsertOfflineItemPriceResponse();
    upsertOfflineItemPriceResponse.setOfflineItemId(OfflineItemControllerTest.OFFLINE_ITEM_ID);
    upsertOfflineItemPriceResponse.setItemSku(OfflineItemControllerTest.ITEM_SKU);
    upsertOfflineItemPriceResponse.setPickupPointCode(OfflineItemControllerTest.PICKUP_POINT_CODE);
    upsertOfflineItemPriceResponse.setListPrice(OfflineItemControllerTest.LIST_PRICE);
    upsertOfflineItemPriceResponse.setOfferPrice(OfflineItemControllerTest.OFFER_PRICE);

    List<UpsertOfflineItemPriceResponse> upsertOfflineItemPriceResponses = new ArrayList<>();
    upsertOfflineItemPriceResponses.add(upsertOfflineItemPriceResponse);

    return upsertOfflineItemPriceResponses;
  }

  @Test
  public void upsertOfflineItemSuccess() throws Exception {
    this.upsertOfflineItemPriceResponseVOs = this.mockUpsertOfflineItemPriceResponseVOs();
    this.content = this.mockUpsertOfflineItemPriceResponses();
    this.mandatoryRequestParam.setClientId(REGULAR_MERCHANT_CLIENT_ID);

    when(this.offlineItemService
        .upsertOfflineItem(this.mandatoryRequestParam, USERNAME, MERCHANT_CODE, upsertOfflineItemRequests))
        .thenReturn(this.upsertOfflineItemPriceResponseVOs);
    content.get(0).setItemCode(ITEM_SKU);
    when(this.modelConverter.convertListToResponse(this.upsertOfflineItemPriceResponseVOs,
        UpsertOfflineItemPriceResponse.class)).thenReturn(this.content);

    this.mockMvc.perform(post(ProductApiPath.OFFLINE_ITEM + ProductApiPath.UPSERT_OFFLINE_ITEM)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .content(this.upsertOfflineItemRequestJson).param("storeId", STORE_ID)
        .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
        .param("username", USERNAME).param("merchantCode", MERCHANT_CODE)).andExpect(status().isOk())
        .andExpect(jsonPath("$.content[0].itemCode", CoreMatchers.equalTo(ITEM_SKU)));

    verify(this.offlineItemService).upsertOfflineItem(this.mandatoryRequestParam, USERNAME, MERCHANT_CODE,
        upsertOfflineItemRequests);
    verify(this.modelConverter).convertListToResponse(this.upsertOfflineItemPriceResponseVOs,
        UpsertOfflineItemPriceResponse.class);
  }

  @Test
  public void upsertOfflineItemExceptionTest() throws Exception {
    this.upsertOfflineItemPriceResponseVOs = this.mockUpsertOfflineItemPriceResponseVOs();
    this.content = this.mockUpsertOfflineItemPriceResponses();
    when(this.modelConverter.convertListToResponse(Mockito.anyList(),
        eq(UpsertOfflineItemPriceResponse.class))).thenThrow(ApplicationRuntimeException.class);
    when(this.offlineItemService.upsertOfflineItem(eq(this.mandatoryRequestParam), eq(USERNAME), eq(MERCHANT_CODE),
        Mockito.anyList())).thenReturn(upsertOfflineItemPriceResponseVOs);

    this.mockMvc.perform(
        post(ProductApiPath.OFFLINE_ITEM + ProductApiPath.UPSERT_OFFLINE_ITEM).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).content(this.upsertOfflineItemRequestJson)
            .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID).param("username", USERNAME).param("merchantCode", MERCHANT_CODE));
    verify(this.offlineItemService).upsertOfflineItem(Mockito.any(MandatoryRequestParam.class), eq(USERNAME), eq(MERCHANT_CODE),
        Mockito.anyList());
    verify(this.modelConverter).convertListToResponse(Mockito.anyList(),
        eq(UpsertOfflineItemPriceResponse.class));
  }

  @Test
  public void upsertOfflineItemException() throws Exception {

    when(this.modelConverter.convertToOfflineItemList(Arrays.asList(offlineItemRequest), MERCHANT_CODE, STORE_ID)).thenReturn(upsertOfflineItemRequests);
    when(this.offlineItemService.upsertOfflineItem(this.mandatoryRequestParam, USERNAME, MERCHANT_CODE, upsertOfflineItemRequests))
        .thenReturn(upsertOfflineItemPriceResponseVOs);
    when(this.modelConverter.convertListToResponse(Mockito.anyList(),
        eq(UpsertOfflineItemPriceResponse.class))).thenThrow(ApplicationRuntimeException.class);

    this.mockMvc.perform(
        post(ProductApiPath.OFFLINE_ITEM + ProductApiPath.UPSERT_OFFLINE_ITEM_PRICE)
            .accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).content(offlineItemRequestJson)
            .param("storeId", STORE_ID)
            .param("channelId", CHANNEL_ID)
            .param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID)
            .param("username", USERNAME)
            .param("merchantCode", MERCHANT_CODE))
        .andExpect(status().isOk());

    verify(this.modelConverter).convertToOfflineItemList(Arrays.asList(offlineItemRequest), MERCHANT_CODE, STORE_ID);
    verify(this.offlineItemService).upsertOfflineItem(eq(this.mandatoryRequestParam), eq(USERNAME), eq(MERCHANT_CODE),
        Mockito.anyList());
    verify(this.modelConverter).convertListToResponse(Mockito.anyList(),
        eq(UpsertOfflineItemPriceResponse.class));
  }

  @Test
  public void publishOfflineItemsTest() throws Exception {
    this.mockMvc.perform(
        get(ProductApiPath.OFFLINE_ITEM + ProductApiPath.OFFLINE_ITEM_PUBLISH_IN_BATCH)
            .contentType(MediaType.APPLICATION_JSON)
            .param("storeId", STORE_ID)
            .param("channelId", CHANNEL_ID)
            .param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID)
            .param("username", USERNAME)
            .param("merchantCode", MERCHANT_CODE)
            .param("batchSize", BATCH_SIZE)
            .param("itemSkus", ITEM_SKU))
        .andExpect(status().isOk());

    verify(this.offlineItemService).publishOfflineItems(STORE_ID, Integer.parseInt(BATCH_SIZE), Arrays.asList(ITEM_SKU));
  }

  @Test
  public void publishOfflineItems_expectException() throws Exception {
    Mockito.doThrow(Exception.class).when(this.offlineItemService)
        .publishOfflineItems(STORE_ID, Integer.parseInt(BATCH_SIZE), Arrays.asList(ITEM_SKU));
    this.mockMvc.perform(
        get(ProductApiPath.OFFLINE_ITEM + ProductApiPath.OFFLINE_ITEM_PUBLISH_IN_BATCH)
            .contentType(MediaType.APPLICATION_JSON)
            .param("storeId", STORE_ID)
            .param("channelId", CHANNEL_ID)
            .param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID)
            .param("username", USERNAME)
            .param("merchantCode", MERCHANT_CODE)
            .param("batchSize", BATCH_SIZE)
            .param("itemSkus", ITEM_SKU))
        .andExpect(status().isOk());

    verify(this.offlineItemService).publishOfflineItems(STORE_ID, Integer.parseInt(BATCH_SIZE), Arrays.asList(ITEM_SKU));
  }

  @Test
  public void republishOfflineItemByMerchantCodesTest() throws Exception {
    SimpleListStringRequest bodyObject =
        new SimpleListStringRequest(OfflineItemControllerTest.MERCHANT_CODE_LIST);
    String bodyJson = new ObjectMapper().writeValueAsString(bodyObject);
    this.mockMvc.perform(post(ProductApiPath.OFFLINE_ITEM
        + ProductApiPath.OFFLINE_ITEM_REPUBLISH_BY_MERCHANT_CODES_IN_BATCH)
        .contentType(MediaType.APPLICATION_JSON)
        .param("storeId", STORE_ID)
        .param("channelId", CHANNEL_ID)
        .param("clientId", CLIENT_ID)
        .param("requestId", REQUEST_ID)
        .param("username", USERNAME)
        .param("batchSize", BATCH_SIZE)
        .content(bodyJson))
        .andExpect(status().isOk());

    verify(this.offlineItemService)
        .republishOfflineItemsByMerchantCodes(STORE_ID, Integer.parseInt(BATCH_SIZE),
            Arrays.asList(MERCHANT_CODE_2));
  }

  @Test
  public void republishOfflineItemByMerchantCodes_expectException() throws Exception {
    SimpleListStringRequest bodyObject =
        new SimpleListStringRequest(OfflineItemControllerTest.MERCHANT_CODE_LIST);
    String bodyJson = new ObjectMapper().writeValueAsString(bodyObject);
    Mockito.doThrow(Exception.class).when(this.offlineItemService)
        .republishOfflineItemsByMerchantCodes(STORE_ID, Integer.parseInt(BATCH_SIZE),
            Arrays.asList(MERCHANT_CODE_2));
    this.mockMvc.perform(
        post(ProductApiPath.OFFLINE_ITEM
            + ProductApiPath.OFFLINE_ITEM_REPUBLISH_BY_MERCHANT_CODES_IN_BATCH)
            .contentType(MediaType.APPLICATION_JSON)
            .param("storeId", STORE_ID)
            .param("channelId", CHANNEL_ID)
            .param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID)
            .param("username", USERNAME)
            .param("batchSize", BATCH_SIZE)
            .content(bodyJson))
        .andExpect(status().isOk());

    verify(this.offlineItemService)
        .republishOfflineItemsByMerchantCodes(STORE_ID, Integer.parseInt(BATCH_SIZE),
            Arrays.asList(MERCHANT_CODE_2));
  }

  @Test
  public void deleteOfflineItemTest() throws Exception {
    this.offlineItemList = Arrays.asList(this.offlineItemForDelete);
    this.mockMvc.perform(
        post(ProductApiPath.OFFLINE_ITEM + ProductApiPath.DELETE_OFFLINE_ITEM)
            .accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).content(this.deleteOfflineItemRequestJson)
            .param("storeId", STORE_ID)
            .param("channelId", CHANNEL_ID)
            .param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID)
            .param("username", USERNAME)
            .param("merchantCode", MERCHANT_CODE_2))
        .andExpect(status().isOk());

    verify(this.itemPickupPointService)
        .delete(STORE_ID, MERCHANT_CODE_2, deleteOfflineItemRequestList, USERNAME);
  }

  @Test
  public void deleteOfflineItemExceptionTest() throws Exception {
    doThrow(ApplicationRuntimeException.class).when(itemPickupPointService).delete(STORE_ID,
      MERCHANT_CODE_2,deleteOfflineItemRequestList,USERNAME);
    this.mockMvc.perform(
        post(ProductApiPath.OFFLINE_ITEM + ProductApiPath.DELETE_OFFLINE_ITEM)
            .accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).content(this.deleteOfflineItemRequestJson)
            .param("storeId", STORE_ID)
            .param("channelId", CHANNEL_ID)
            .param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID)
            .param("username", USERNAME)
            .param("merchantCode", MERCHANT_CODE_2))
        .andExpect(status().isOk());
    verify(this.itemPickupPointService).delete(
      (STORE_ID), (MERCHANT_CODE_2), deleteOfflineItemRequestList, (USERNAME));
  }

  @Test
  public void bulkDeleteOfflineItemTest() throws Exception {
    when(this.itemPickupPointService.bulkDelete(STORE_ID, MERCHANT_CODE, deleteOfflineItemVOs, USERNAME))
        .thenReturn(deleteOfflineItemVOs);
    when(this.modelConverter.convertListToResponse(deleteOfflineItemVOs, DeleteOfflineItemResponse.class))
        .thenReturn(deleteOfflineItemResponses);
    this.mockMvc.perform(
        post(ProductApiPath.OFFLINE_ITEM + ProductApiPath.BULK_DELETE_OFFLINE_ITEM)
            .accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).content(this.deleteOfflineItemRequestJson)
            .param("storeId", STORE_ID)
            .param("channelId", CHANNEL_ID)
            .param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID)
            .param("username", USERNAME)
            .param("merchantCode", MERCHANT_CODE))
        .andExpect(status().isOk());

    verify(this.itemPickupPointService).bulkDelete(STORE_ID, MERCHANT_CODE, deleteOfflineItemVOs, USERNAME);
    verify(this.modelConverter).convertListToResponse(deleteOfflineItemVOs, DeleteOfflineItemResponse.class);
  }

  @Test
  public void bulkDeleteOfflineItemExceptionTest() throws Exception {
    when(this.itemPickupPointService.bulkDelete(
        eq(STORE_ID), eq(MERCHANT_CODE), anyList(), eq(USERNAME)))
        .thenThrow(new ApplicationRuntimeException());
    this.mockMvc.perform(
        post(ProductApiPath.OFFLINE_ITEM + ProductApiPath.BULK_DELETE_OFFLINE_ITEM)
            .accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).content(this.deleteOfflineItemRequestJson)
            .param("storeId", STORE_ID)
            .param("channelId", CHANNEL_ID)
            .param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID)
            .param("username", USERNAME)
            .param("merchantCode", MERCHANT_CODE))
        .andExpect(status().isOk());

    verify(this.itemPickupPointService).bulkDelete(
        eq(STORE_ID), eq(MERCHANT_CODE), anyList(), eq(USERNAME));
  }

  @Test
  public void findByMerchantCodeAndMerchantSkusTest() throws Exception {
    List<String> merchantSkus = new ArrayList<>();
    merchantSkus.add(MERCHANT_SKU);

    SimpleListStringRequest merchantSkuRequest = new SimpleListStringRequest();
    merchantSkuRequest.setValue(merchantSkus);

    Item item = new Item();
    item.setItemSku(ITEM_SKU);
    item.setProductSku(PRODUCT_SKU);
    item.setMerchantCode(MERCHANT_CODE);
    item.setMerchantSku(MERCHANT_SKU);

    List<Item> items = new ArrayList<>();
    items.add(item);

    OfflineItemResponseDetail offlineItemResponseDetail = new OfflineItemResponseDetail();
    offlineItemResponseDetail.setMerchantSku(MERCHANT_SKU);
    offlineItemResponseDetail.setItemSku(ITEM_SKU);

    this.productSkus.add(PRODUCT_SKU);
    this.product.setProductSku(PRODUCT_SKU);
    this.product.setProductType(ProductType.REGULAR);

    List<Product> products = new ArrayList<>();
    products.add(product);

    List<OfflineItemResponseDetail> offlineItemResponseDetails = new ArrayList<>();
    offlineItemResponseDetails.add(offlineItemResponseDetail);

    when(this.itemService.getItemsByMerchantCodeAndMerchantSkus(STORE_ID, REQUEST_ID, USERNAME, MERCHANT_CODE, merchantSkus))
        .thenReturn(items);
    when(this.modelConverter.convertItemToOfflineProductResponse(eq(merchantSkus), anyList(), anyMap()))
        .thenReturn(offlineItemResponseDetails);
    when(this.productService.getProducts(STORE_ID, productSkus))
        .thenReturn(products);

    this.mockMvc.perform(
        post(ProductApiPath.OFFLINE_ITEM + ProductApiPath.FIND_BY_MERCHANT_CODE_AND_MERCHANT_SKUS)
            .accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON)
            .content(this.objectMapper.writeValueAsString(merchantSkuRequest))
            .param("storeId", OfflineItemControllerTest.STORE_ID)
            .param("channelId", OfflineItemControllerTest.CHANNEL_ID)
            .param("clientId", OfflineItemControllerTest.CLIENT_ID)
            .param("requestId", OfflineItemControllerTest.REQUEST_ID)
            .param("username", OfflineItemControllerTest.USERNAME)
            .param("merchantCode", OfflineItemControllerTest.MERCHANT_CODE))
        .andExpect(status().isOk());

    verify(this.itemService).getItemsByMerchantCodeAndMerchantSkus(STORE_ID, REQUEST_ID, USERNAME, MERCHANT_CODE, merchantSkus);
    verify(this.modelConverter).convertItemToOfflineProductResponse(eq(merchantSkus), anyList(), anyMap());
    verify(this.productService).getProducts(STORE_ID, productSkus);
  }

  @Test
  public void findByMerchantCodeAndMerchantSkusTestInvalidMerchantSkus() throws Exception {
    String invalidMerchantSku = "invalid merchantSku";
    List<String> merchantSkus = new ArrayList<>();
    merchantSkus.add(invalidMerchantSku);

    SimpleListStringRequest merchantSkuRequest = new SimpleListStringRequest();
    merchantSkuRequest.setValue(merchantSkus);

    List<Item> items = new ArrayList<>();

    OfflineItemResponseDetail offlineItemResponseDetail = new OfflineItemResponseDetail();
    offlineItemResponseDetail.setMerchantSku(invalidMerchantSku);
    offlineItemResponseDetail.setItemSku(null);
    offlineItemResponseDetail.setErrorCode(ProductErrorCodesEnum.MERCHANT_SKU_NOT_FOUND.getCode());

    List<OfflineItemResponseDetail> offlineItemResponseDetails = new ArrayList<>();
    offlineItemResponseDetails.add(offlineItemResponseDetail);

    when(this.itemService.getItemsByMerchantCodeAndMerchantSkus(STORE_ID, REQUEST_ID, USERNAME, MERCHANT_CODE, merchantSkus))
        .thenReturn(items);
    when(this.modelConverter.convertItemToOfflineProductResponse(eq(merchantSkus), anyList(), anyMap()))
        .thenReturn(offlineItemResponseDetails);

    this.mockMvc.perform(
            post(ProductApiPath.OFFLINE_ITEM + ProductApiPath.FIND_BY_MERCHANT_CODE_AND_MERCHANT_SKUS)
                .accept(MediaType.APPLICATION_JSON)
                .contentType(MediaType.APPLICATION_JSON)
                .content(this.objectMapper.writeValueAsString(merchantSkuRequest))
                .param("storeId", OfflineItemControllerTest.STORE_ID)
                .param("channelId", OfflineItemControllerTest.CHANNEL_ID)
                .param("clientId", OfflineItemControllerTest.CLIENT_ID)
                .param("requestId", OfflineItemControllerTest.REQUEST_ID)
                .param("username", OfflineItemControllerTest.USERNAME)
                .param("merchantCode", OfflineItemControllerTest.MERCHANT_CODE))
        .andExpect(status().isOk());

    verify(this.itemService).getItemsByMerchantCodeAndMerchantSkus(STORE_ID, REQUEST_ID, USERNAME, MERCHANT_CODE, merchantSkus);
    verify(this.modelConverter).convertItemToOfflineProductResponse(eq(merchantSkus), anyList(), anyMap());
  }

  @Test
  public void findByMerchantCodeAndMerchantSkusTestException() throws Exception {
    List<String> merchantSkus = new ArrayList<>();
    merchantSkus.add(MERCHANT_SKU);

    SimpleListStringRequest merchantSkuRequest = new SimpleListStringRequest();
    merchantSkuRequest.setValue(merchantSkus);

    when(this.itemService
        .getItemsByMerchantCodeAndMerchantSkus(STORE_ID, REQUEST_ID, USERNAME, MERCHANT_CODE,
            merchantSkus)).thenThrow(new ApplicationRuntimeException());

    this.mockMvc.perform(
        post(ProductApiPath.OFFLINE_ITEM + ProductApiPath.FIND_BY_MERCHANT_CODE_AND_MERCHANT_SKUS)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(this.objectMapper.writeValueAsString(merchantSkuRequest))
            .param("storeId", OfflineItemControllerTest.STORE_ID)
            .param("channelId", OfflineItemControllerTest.CHANNEL_ID)
            .param("clientId", OfflineItemControllerTest.CLIENT_ID)
            .param("requestId", OfflineItemControllerTest.REQUEST_ID)
            .param("username", OfflineItemControllerTest.USERNAME)
            .param("merchantCode", OfflineItemControllerTest.MERCHANT_CODE))
        .andExpect(status().isOk());

    verify(this.itemService)
        .getItemsByMerchantCodeAndMerchantSkus(STORE_ID, REQUEST_ID, USERNAME, MERCHANT_CODE,
            merchantSkus);
  }

  @Test
  public void findByMerchantCodeAndItemSkuTest() throws Exception {
    List<ItemPickupPointPriceVo> itemPickupPointPriceVoList = new ArrayList<>();
    List<OfflineItemPriceResponse> offlineItemPriceResponses = new ArrayList<>();

    when(this.offlineItemService.findItemPickupPointByMerchantCodeAndItemSku(STORE_ID, MERCHANT_CODE, ITEM_SKU))
        .thenReturn(itemPickupPointPriceVoList);
    when(this.modelConverter.convertListToResponse(itemPickupPointPriceVoList, OfflineItemPriceResponse.class))
        .thenReturn(offlineItemPriceResponses);

    this.mockMvc.perform(
        get(ProductApiPath.OFFLINE_ITEM + ProductApiPath.FIND_BY_MERCHANT_CODE_AND_ITEM_SKU)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .param("storeId", OfflineItemControllerTest.STORE_ID)
            .param("channelId", OfflineItemControllerTest.CHANNEL_ID)
            .param("clientId", OfflineItemControllerTest.CLIENT_ID)
            .param("requestId", OfflineItemControllerTest.REQUEST_ID)
            .param("username", OfflineItemControllerTest.USERNAME)
            .param("merchantCode", OfflineItemControllerTest.MERCHANT_CODE)
            .param("itemSku", OfflineItemControllerTest.ITEM_SKU)).andExpect(status().isOk());

    verify(this.offlineItemService).findItemPickupPointByMerchantCodeAndItemSku(STORE_ID,
      MERCHANT_CODE, ITEM_SKU);
    verify(this.modelConverter).convertListToResponse(itemPickupPointPriceVoList, OfflineItemPriceResponse.class);
  }

  @Test
  public void findByMerchantCodeAndItemSkuTestException() throws Exception {
    when(this.offlineItemService.findItemPickupPointByMerchantCodeAndItemSku(STORE_ID, MERCHANT_CODE, ITEM_SKU))
        .thenThrow(new ApplicationRuntimeException());

    this.mockMvc.perform(
        get(ProductApiPath.OFFLINE_ITEM + ProductApiPath.FIND_BY_MERCHANT_CODE_AND_ITEM_SKU)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .param("storeId", OfflineItemControllerTest.STORE_ID)
            .param("channelId", OfflineItemControllerTest.CHANNEL_ID)
            .param("clientId", OfflineItemControllerTest.CLIENT_ID)
            .param("requestId", OfflineItemControllerTest.REQUEST_ID)
            .param("username", OfflineItemControllerTest.USERNAME)
            .param("merchantCode", OfflineItemControllerTest.MERCHANT_CODE)
            .param("itemSku", OfflineItemControllerTest.ITEM_SKU)).andExpect(status().isOk());

    verify(this.offlineItemService).findItemPickupPointByMerchantCodeAndItemSku(STORE_ID, MERCHANT_CODE, ITEM_SKU);
  }

  @Test
  public void findOfflineItemByProductSkusTest() throws Exception {
    List<String> productSkus = new ArrayList<>();
    List<ProductAndItemsVO> productAndItemsVOs = new ArrayList<>();
    List<ProductAndItemsResponse> responses = new ArrayList<>();

    when(this.offlineItemService.findOfflineItemsByProductSkus(STORE_ID, USERNAME, REQUEST_ID, productSkus))
        .thenReturn(productAndItemsVOs);
    when(this.modelConverter.convertToProductAndItemsDTOs(productAndItemsVOs))
        .thenReturn(responses);

    this.mockMvc.perform(post(ProductApiPath.OFFLINE_ITEM + ProductApiPath.FIND_BY_PRODUCT_SKUS)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .content(this.objectMapper.writeValueAsString(productSkus))
        .param("storeId", OfflineItemControllerTest.STORE_ID)
        .param("channelId", OfflineItemControllerTest.CHANNEL_ID)
        .param("clientId", OfflineItemControllerTest.CLIENT_ID)
        .param("requestId", OfflineItemControllerTest.REQUEST_ID)
        .param("username", OfflineItemControllerTest.USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));

    verify(this.offlineItemService).findOfflineItemsByProductSkus(STORE_ID, USERNAME, REQUEST_ID, productSkus);
    verify(this.modelConverter).convertToProductAndItemsDTOs(productAndItemsVOs);
  }

  @Test
  public void findOfflineItemByProductSkusTestException() throws Exception {
    List<String> productSkus = new ArrayList<>();
    when(this.offlineItemService.findOfflineItemsByProductSkus(STORE_ID, USERNAME, REQUEST_ID, productSkus))
        .thenThrow(new ApplicationRuntimeException());

    this.mockMvc.perform(post(ProductApiPath.OFFLINE_ITEM + ProductApiPath.FIND_BY_PRODUCT_SKUS)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .content(this.objectMapper.writeValueAsString(productSkus))
        .param("storeId", OfflineItemControllerTest.STORE_ID)
        .param("channelId", OfflineItemControllerTest.CHANNEL_ID)
        .param("clientId", OfflineItemControllerTest.CLIENT_ID)
        .param("requestId", OfflineItemControllerTest.REQUEST_ID)
        .param("username", OfflineItemControllerTest.USERNAME)
        .param("merchantCode", OfflineItemControllerTest.MERCHANT_CODE)
        .param("itemSku", OfflineItemControllerTest.ITEM_SKU)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));

    verify(this.offlineItemService).findOfflineItemsByProductSkus(STORE_ID, USERNAME, REQUEST_ID, productSkus);
  }

  @Test
  public void findAllOfflineItemsOfMultipleMerchantsTest() throws Exception {
    List<ItemPickupPointPriceVo> itemPickupPointPriceVoList = new ArrayList<>();
    List<OfflineItemPriceResponse> offlineItemPriceResponses = new ArrayList<>();

    when(this.offlineItemService.findMultipleMerchantsOfflineItemByItemSku(STORE_ID, ITEM_SKU))
        .thenReturn(itemPickupPointPriceVoList);
    when(this.modelConverter.convertListToResponse(itemPickupPointPriceVoList, OfflineItemPriceResponse.class))
        .thenReturn(offlineItemPriceResponses);

    this.mockMvc.perform(get(ProductApiPath.OFFLINE_ITEM
        + ProductApiPath.FIND_ALL_OFFLINE_ITEMS_OF_MULTIPLE_MERCHANTS)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .param("storeId", OfflineItemControllerTest.STORE_ID)
        .param("channelId", OfflineItemControllerTest.CHANNEL_ID)
        .param("clientId", OfflineItemControllerTest.CLIENT_ID)
        .param("requestId", OfflineItemControllerTest.REQUEST_ID)
        .param("username", OfflineItemControllerTest.USERNAME)
        .param("itemSku", OfflineItemControllerTest.ITEM_SKU)).andExpect(status().isOk());

    verify(this.offlineItemService).findMultipleMerchantsOfflineItemByItemSku(STORE_ID, ITEM_SKU);
    verify(this.modelConverter).convertListToResponse(itemPickupPointPriceVoList, OfflineItemPriceResponse.class);
  }

  @Test
  public void findAllOfflineItemsOfMultipleMerchantsTestException() throws Exception {
    when(this.offlineItemService.findMultipleMerchantsOfflineItemByItemSku(STORE_ID, ITEM_SKU))
        .thenThrow(new ApplicationRuntimeException());

    this.mockMvc.perform(get(ProductApiPath.OFFLINE_ITEM
        + ProductApiPath.FIND_ALL_OFFLINE_ITEMS_OF_MULTIPLE_MERCHANTS)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .param("storeId", OfflineItemControllerTest.STORE_ID)
        .param("channelId", OfflineItemControllerTest.CHANNEL_ID)
        .param("clientId", OfflineItemControllerTest.CLIENT_ID)
        .param("requestId", OfflineItemControllerTest.REQUEST_ID)
        .param("username", OfflineItemControllerTest.USERNAME)
        .param("itemSku", OfflineItemControllerTest.ITEM_SKU)).andExpect(status().isOk());

    verify(this.offlineItemService).findMultipleMerchantsOfflineItemByItemSku(STORE_ID, ITEM_SKU);
  }

  @Test
  public void findAllOfflineItemsOfSpecificMerchantTest() throws Exception {
    List<OfflineItem> offlineItems = new ArrayList<>();
    List<OfflineItemPriceResponse> offlineItemPriceResponses = new ArrayList<>();

    when(this.offlineItemService.findByItemSkusAndMarkForDeleteFalseWithLimit(STORE_ID, ITEM_SKU_SET))
        .thenReturn(offlineItems);
    when(this.modelConverter.convertListToResponse(offlineItems, OfflineItemPriceResponse.class))
        .thenReturn(offlineItemPriceResponses);

    this.mockMvc.perform(get(ProductApiPath.OFFLINE_ITEM
        + ProductApiPath.FIND_ALL_OFFLINE_ITEMS_OF_SPECIFIC_MERCHANT)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .param("storeId", OfflineItemControllerTest.STORE_ID)
        .param("channelId", OfflineItemControllerTest.CHANNEL_ID)
        .param("clientId", OfflineItemControllerTest.CLIENT_ID)
        .param("requestId", OfflineItemControllerTest.REQUEST_ID)
        .param("username", OfflineItemControllerTest.USERNAME)
        .param("itemSku", OfflineItemControllerTest.ITEM_SKU)).andExpect(status().isOk());

    verify(this.offlineItemService).findByItemSkusAndMarkForDeleteFalseWithLimit(STORE_ID, ITEM_SKU_SET);
    verify(this.modelConverter).convertListToResponse(offlineItems, OfflineItemPriceResponse.class);
  }

  @Test
  public void findAllOfflineItemsOfSpecificMerchantTestException() throws Exception {
    when(this.offlineItemService.findByItemSkusAndMarkForDeleteFalseWithLimit(STORE_ID, ITEM_SKU_SET))
        .thenThrow(new ApplicationRuntimeException());

    this.mockMvc.perform(get(ProductApiPath.OFFLINE_ITEM
        + ProductApiPath.FIND_ALL_OFFLINE_ITEMS_OF_SPECIFIC_MERCHANT)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .param("storeId", OfflineItemControllerTest.STORE_ID)
        .param("channelId", OfflineItemControllerTest.CHANNEL_ID)
        .param("clientId", OfflineItemControllerTest.CLIENT_ID)
        .param("requestId", OfflineItemControllerTest.REQUEST_ID)
        .param("username", OfflineItemControllerTest.USERNAME)
        .param("itemSku", OfflineItemControllerTest.ITEM_SKU)).andExpect(status().isOk());

    verify(this.offlineItemService).findByItemSkusAndMarkForDeleteFalseWithLimit(STORE_ID, ITEM_SKU_SET);
  }

  @Test
  public void findByOfflineItemIdTest() throws Exception {
    String offlineItemId = "offlineItemId1";

    OfflineItemPriceResponse offlineItemPriceResponse = new OfflineItemPriceResponse();
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    when(this.itemPickupPointService.findByStoreIdAndOfflineItemId(STORE_ID, offlineItemId))
        .thenReturn(itemPickupPoint);
    when(this.modelConverter.convertToResponse(offlineItem, OfflineItemPriceResponse.class))
        .thenReturn(offlineItemPriceResponse);

    this.mockMvc.perform(get(ProductApiPath.OFFLINE_ITEM_FIND_BY_OFFLINE_ITEM_ID).accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON).param("storeId", OfflineItemControllerTest.STORE_ID)
        .param("channelId", OfflineItemControllerTest.CHANNEL_ID).param("clientId", OfflineItemControllerTest.CLIENT_ID)
        .param("requestId", OfflineItemControllerTest.REQUEST_ID).param("username", OfflineItemControllerTest.USERNAME)
        .param("offlineItemId", offlineItemId)).andExpect(status().isOk());

    verify(this.itemPickupPointService).findByStoreIdAndOfflineItemId(STORE_ID, offlineItemId);
    verify(this.modelConverter).convertToResponse(itemPickupPoint, OfflineItemPriceResponse.class);
  }

  @Test
  public void findByOfflineItemIdTest_Exception() throws Exception {
    String offlineItemId = "offlineItemId-1";
    when(this.itemPickupPointService.findByStoreIdAndOfflineItemId(STORE_ID, offlineItemId))
        .thenThrow(new RuntimeException());

    this.mockMvc.perform(get(ProductApiPath.OFFLINE_ITEM_FIND_BY_OFFLINE_ITEM_ID).accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON).param("storeId", OfflineItemControllerTest.STORE_ID)
        .param("channelId", OfflineItemControllerTest.CHANNEL_ID).param("clientId", OfflineItemControllerTest.CLIENT_ID)
        .param("requestId", OfflineItemControllerTest.REQUEST_ID).param("username", OfflineItemControllerTest.USERNAME)
        .param("offlineItemId", offlineItemId)).andExpect(status().isOk());

    verify(this.itemPickupPointService).findByStoreIdAndOfflineItemId(STORE_ID, offlineItemId);
  }

  @Test
  public void checkUniqueIdTypeTest() throws Exception {
    when(this.offlineItemService.isOfflineItem(STORE_ID, UNIQUE_ID))
        .thenReturn(true);

    this.mockMvc.perform(
        get(ProductApiPath.OFFLINE_ITEM_CHECK_UNIQUE_ID_TYPE).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON)
            .param("storeId", OfflineItemControllerTest.STORE_ID)
            .param("channelId", OfflineItemControllerTest.CHANNEL_ID)
            .param("clientId", OfflineItemControllerTest.CLIENT_ID)
            .param("requestId", OfflineItemControllerTest.REQUEST_ID)
            .param("username", OfflineItemControllerTest.USERNAME)
            .param("uniqueId", UNIQUE_ID)).andExpect(status().isOk());

    verify(this.offlineItemService).isOfflineItem(STORE_ID, UNIQUE_ID);
  }

  @Test
  public void checkUniqueIdTypeTest_applicationRuntimeException_returnsSuccessFalseAndErrorCode() throws Exception {
    when(this.offlineItemService.isOfflineItem(STORE_ID, UNIQUE_ID))
        .thenThrow(new ApplicationRuntimeException());

    this.mockMvc.perform(
        get(ProductApiPath.OFFLINE_ITEM_CHECK_UNIQUE_ID_TYPE).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON)
            .param("storeId", OfflineItemControllerTest.STORE_ID)
            .param("channelId", OfflineItemControllerTest.CHANNEL_ID)
            .param("clientId", OfflineItemControllerTest.CLIENT_ID)
            .param("requestId", OfflineItemControllerTest.REQUEST_ID)
            .param("username", OfflineItemControllerTest.USERNAME)
            .param("uniqueId", UNIQUE_ID)).andExpect(status().isOk())
        .andExpect(jsonPath("success").value(false))
        .andExpect(jsonPath("errorCode").value(ProductErrorCodesEnum.CHECK_UNIQUE_ID_TYPE.getCode()))
        .andExpect(jsonPath("errorMessage").value(ProductErrorCodesEnum.CHECK_UNIQUE_ID_TYPE.getMessage()));
    verify(this.offlineItemService).isOfflineItem(STORE_ID, UNIQUE_ID);
  }

  @Test
  public void checkUniqueIdTypeTest_exception_returnsSuccessFalseAndErrorCode() throws Exception {
    when(this.offlineItemService.isOfflineItem(STORE_ID, UNIQUE_ID))
        .thenThrow(new Exception());

    this.mockMvc.perform(
        get(ProductApiPath.OFFLINE_ITEM_CHECK_UNIQUE_ID_TYPE).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON)
            .param("storeId", OfflineItemControllerTest.STORE_ID)
            .param("channelId", OfflineItemControllerTest.CHANNEL_ID)
            .param("clientId", OfflineItemControllerTest.CLIENT_ID)
            .param("requestId", OfflineItemControllerTest.REQUEST_ID)
            .param("username", OfflineItemControllerTest.USERNAME)
            .param("uniqueId", UNIQUE_ID)).andExpect(status().isOk())
        .andExpect(jsonPath("success").value(false))
        .andExpect(jsonPath("errorCode").value(ProductErrorCodesEnum.CHECK_UNIQUE_ID_TYPE.getCode()))
        .andExpect(jsonPath("errorMessage").value(ProductErrorCodesEnum.CHECK_UNIQUE_ID_TYPE.getMessage()));
    verify(this.offlineItemService).isOfflineItem(STORE_ID, UNIQUE_ID);
  }

  @Test
  public void testUpdateOfflineItemPriceByItemSku() throws Exception {

    this.mockMvc.perform(
        post(ProductApiPath.OFFLINE_ITEM + ProductApiPath.UPDATE_OFFLINE_ITEM_PRICE_BY_ITEM_SKU).accept(
                MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(this.updateOfflineItemPriceRequestJson).param("storeId", STORE_ID).param("channelId", CHANNEL_ID)
            .param("clientId", CLIENT_ID).param("requestId", REQUEST_ID).param("username", USERNAME)
            .param("merchantCode", MERCHANT_CODE)).andExpect(status().isOk());

    verify(this.offlineItemService).updateOfflineItemPriceByItemSku(mandatoryRequestParam, MERCHANT_CODE,
        updateOfflineItemPriceRequest);
  }

  @Test
  public void testUpdateOfflineItemPriceByItemSku_throwException() throws Exception {
    Mockito.doThrow(Exception.class).when(this.offlineItemService).updateOfflineItemPriceByItemSku(
        mandatoryRequestParam, MERCHANT_CODE, updateOfflineItemPriceRequest);

    this.mockMvc.perform(
        post(ProductApiPath.OFFLINE_ITEM + ProductApiPath.UPDATE_OFFLINE_ITEM_PRICE_BY_ITEM_SKU)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(this.updateOfflineItemPriceRequestJson).param("storeId", STORE_ID)
            .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID).param("username", USERNAME)
            .param("merchantCode", MERCHANT_CODE))
        .andExpect(status().isOk());

    verify(this.offlineItemService).updateOfflineItemPriceByItemSku(mandatoryRequestParam,
        MERCHANT_CODE, updateOfflineItemPriceRequest);
  }
}

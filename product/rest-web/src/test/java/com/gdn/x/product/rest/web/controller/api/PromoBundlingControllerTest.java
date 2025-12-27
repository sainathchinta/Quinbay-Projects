package com.gdn.x.product.rest.web.controller.api;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.openMocks;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.setup.MockMvcBuilders.standaloneSetup;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.MasterDataAttribute;
import com.gdn.x.product.model.entity.MasterDataItem;
import com.gdn.x.product.model.entity.MasterDataItemAttributeValue;
import com.gdn.x.product.model.entity.PristineDataItem;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.vo.ActiveComboRequestVO;
import com.gdn.x.product.model.vo.ComboDetailVo;
import com.gdn.x.product.model.vo.ComboItemVO;
import com.gdn.x.product.model.vo.ComboResponseVO;
import com.gdn.x.product.model.vo.ComboVO;
import com.gdn.x.product.model.vo.WholesaleVO;
import com.gdn.x.product.rest.web.model.ComboItem;
import com.gdn.x.product.rest.web.model.ProductApiPath;
import com.gdn.x.product.rest.web.model.request.ActiveComboRequest;
import com.gdn.x.product.rest.web.model.response.ComboDetailResponse;
import com.gdn.x.product.rest.web.model.response.ComboResponse;
import com.gdn.x.product.rest.web.model.response.WholesaleResponse;
import com.gdn.x.product.service.impl.PromoBundlingServiceImpl;
import com.gdn.x.product.service.util.ModelConverter;

public class PromoBundlingControllerTest {

  private static final String STORE_ID = "store-id";
  private static final String CHANNEL_ID = "channel-id";
  private static final String CLIENT_ID = "client-id";
  private static final String REQUEST_ID = "request-id";
  private static final String USERNAME = "username";
  private static final String PAGE = "0";
  private static final String SIZE = "10";
  private static final String SORT_BY = "promoBundlingName";
  private static final String SORT_TYPE = "desc";
  private static final String ITEM_CODE = "BLI-0001";
  private static final String ITEM_SKU = "IT-0001-0001-0001";
  private static final String PRISTINE_ID = "PR-0001";
  private static final String ATTRIBUTE_VALUE = "42";
  private static final String ATTRIBUTE_CODE = "SZ";
  private static final String ATTRIBUTE_NAME = "SIZE";
  private static final String PROMO_BUNDLING_NAME = "promo-bundling-123";
  private static final String PROMO_BUNDLING_TYPE = "COMBO";
  private static final String PROMO_BUNDLING_ID = "PRM-0001";
  private static final String PRODUCT_NAME = "product-name";
  private static final String IS_PRISTINE = "true";
  private static final int QUANTITY = 10;

  @InjectMocks
  private PromoBundlingController promoBundlingController;

  @Mock
  private PromoBundlingServiceImpl promoBundlingServiceImpl;

  @Mock
  private ModelConverter modelConverter;

  private MockMvc mockMvc;
  private ObjectMapper mapper;

  private Item item;
  private MasterDataItem masterDataItem;
  private List<MasterDataItemAttributeValue> masterDataItemAttributeValueList;
  private MasterDataItemAttributeValue masterDataItemAttributeValue;
  private MasterDataAttribute masterDataAttribute;
  private PristineDataItem pristineDataItem;

  private Product product;
  private ActiveComboRequest activeComboRequest;
  private ActiveComboRequestVO activeComboRequestVO;

  private ComboResponseVO comboResponseVO;

  private ComboVO comboVO;
  private List<ComboVO> comboVOList;

  private ComboDetailVo comboDetailVo;
  private ComboDetailResponse comboDetailResponse;
  private ComboItemVO comboItemVO;
  private List<ComboItemVO> comboItemVOList;

  private List<ComboResponse> comboResponseList;
  private ComboResponse comboResponse;

  private ComboItem comboItem;
  private List<ComboItem> comboItemList;

  private WholesaleVO wholesaleVO;
  private WholesaleResponse wholesaleResponse;

  @Test
  public void getActiveCombosUsing_throwRuntimeException() throws Exception {
    activeComboRequest = new ActiveComboRequest();
    String content = mapper.writeValueAsString(new ActiveComboRequestVO());
    when(this.modelConverter
        .convertActiveComboRequestToActiveComboRequestVO(activeComboRequest))
        .thenReturn(new ActiveComboRequestVO());
    doThrow(new RuntimeException()).when(this.promoBundlingServiceImpl)
        .getActiveCombos(STORE_ID, CHANNEL_ID, CLIENT_ID,
            REQUEST_ID, USERNAME, new ActiveComboRequestVO(), Integer.valueOf(PAGE),
            Integer.valueOf(SIZE), SORT_BY, SORT_TYPE);

    this.mockMvc.perform(post(ProductApiPath.PROMO_BUNDLING + ProductApiPath.GET_ACTIVE_COMBOS)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).content(content)
        .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
        .param("requestId", REQUEST_ID).param("username", USERNAME).param("page", PAGE)
        .param("size", SIZE).param("sortBy", SORT_BY).param("sortType", SORT_TYPE))
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.content", nullValue()));

    verify(this.modelConverter)
        .convertActiveComboRequestToActiveComboRequestVO(new ActiveComboRequest());
    verify(this.promoBundlingServiceImpl)
        .getActiveCombos(STORE_ID, CHANNEL_ID, CLIENT_ID,
            REQUEST_ID, USERNAME,  new ActiveComboRequestVO(), Integer.valueOf(PAGE),
            Integer.valueOf(SIZE), SORT_BY, SORT_TYPE);
  }

  @Test
  public void getActiveCombos_throwApplicationRuntimeException() throws Exception {
    activeComboRequest = new ActiveComboRequest();
    String content = mapper.writeValueAsString(new ActiveComboRequestVO());
    when(this.modelConverter
        .convertActiveComboRequestToActiveComboRequestVO(activeComboRequest))
        .thenReturn(new ActiveComboRequestVO());
    doThrow(new ApplicationRuntimeException()).when(this.promoBundlingServiceImpl)
        .getActiveCombos(STORE_ID, CHANNEL_ID, CLIENT_ID,
            REQUEST_ID, USERNAME, new ActiveComboRequestVO(), Integer.valueOf(PAGE),
            Integer.valueOf(SIZE), SORT_BY, SORT_TYPE);

    this.mockMvc.perform(post(ProductApiPath.PROMO_BUNDLING + ProductApiPath.GET_ACTIVE_COMBOS)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).content(content)
        .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
        .param("requestId", REQUEST_ID).param("username", USERNAME).param("page", PAGE)
        .param("size", SIZE).param("sortBy", SORT_BY).param("sortType", SORT_TYPE))
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.content", nullValue()));

    verify(this.modelConverter)
        .convertActiveComboRequestToActiveComboRequestVO(new ActiveComboRequest());
    verify(this.promoBundlingServiceImpl)
        .getActiveCombos(STORE_ID, CHANNEL_ID, CLIENT_ID,
            REQUEST_ID, USERNAME, new ActiveComboRequestVO(), Integer.valueOf(PAGE),
            Integer.valueOf(SIZE), SORT_BY, SORT_TYPE);
  }

  @Test
  public void getAllActiveComboUsingItemCodes_success() throws Exception {this.activeComboRequest.setItemCode(ITEM_CODE);
    this.activeComboRequestVO.setItemCode(ITEM_CODE);
    String content = mapper.writeValueAsString(this.activeComboRequestVO);
    when(this.promoBundlingServiceImpl
        .getActiveCombos(STORE_ID, CHANNEL_ID, CLIENT_ID,
            REQUEST_ID, USERNAME, this.activeComboRequestVO, Integer.valueOf(PAGE),
            Integer.valueOf(SIZE), SORT_BY, SORT_TYPE)).thenReturn(this.comboResponseVO);
    when(this.modelConverter
        .convertActiveComboRequestToActiveComboRequestVO(activeComboRequest))
        .thenReturn(activeComboRequestVO);
    when(this.modelConverter
        .convertListToResponse(comboResponseVO.getComboList(), ComboResponse.class))
        .thenReturn(Arrays.asList(comboResponse));

    this.mockMvc.perform(post(ProductApiPath.PROMO_BUNDLING + ProductApiPath.GET_ACTIVE_COMBOS)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).content(content)
        .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
        .param("requestId", REQUEST_ID).param("username", USERNAME).param("page", PAGE)
        .param("size", SIZE).param("sortBy", SORT_BY).param("sortType", SORT_TYPE))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.content", notNullValue()));

    verify(this.promoBundlingServiceImpl)
        .getActiveCombos(STORE_ID, CHANNEL_ID, CLIENT_ID,
            REQUEST_ID, USERNAME, this.activeComboRequestVO, Integer.valueOf(PAGE),
            Integer.valueOf(SIZE), SORT_BY, SORT_TYPE);
    verify(this.modelConverter)
        .convertActiveComboRequestToActiveComboRequestVO(activeComboRequest);
    verify(this.modelConverter).convertListToResponse(comboResponseVO.getComboList(), ComboResponse.class);
  }

  @Test
  public void getAllActiveComboUsingItemSkus_success() throws Exception {this.activeComboRequest.setItemSku(ITEM_SKU);
    this.activeComboRequestVO.setItemSku(ITEM_SKU);
    String content = mapper.writeValueAsString(this.activeComboRequestVO);
    when(this.promoBundlingServiceImpl
        .getActiveCombos(STORE_ID, CHANNEL_ID, CLIENT_ID,
            REQUEST_ID, USERNAME, this.activeComboRequestVO, Integer.valueOf(PAGE),
            Integer.valueOf(SIZE), SORT_BY, SORT_TYPE)).thenReturn(this.comboResponseVO);
    when(this.modelConverter
        .convertActiveComboRequestToActiveComboRequestVO(activeComboRequest))
        .thenReturn(activeComboRequestVO);
    when(this.modelConverter
        .convertListToResponse(comboResponseVO.getComboList(), ComboResponse.class))
        .thenReturn(Arrays.asList(comboResponse));

    this.mockMvc.perform(post(ProductApiPath.PROMO_BUNDLING + ProductApiPath.GET_ACTIVE_COMBOS)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).content(content)
        .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
        .param("requestId", REQUEST_ID).param("username", USERNAME).param("page", PAGE)
        .param("size", SIZE).param("sortBy", SORT_BY).param("sortType", SORT_TYPE))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.content", notNullValue()));

    verify(this.promoBundlingServiceImpl)
        .getActiveCombos(STORE_ID, CHANNEL_ID, CLIENT_ID,
            REQUEST_ID, USERNAME, this.activeComboRequestVO, Integer.valueOf(PAGE),
            Integer.valueOf(SIZE), SORT_BY, SORT_TYPE);
    verify(this.modelConverter)
        .convertActiveComboRequestToActiveComboRequestVO(activeComboRequest);
    verify(this.modelConverter).convertListToResponse(comboResponseVO.getComboList(), ComboResponse.class);
  }

  @Test
  public void getAllActiveComboUsingPristineId_success() throws Exception {
    this.activeComboRequest.setPristineId(PRISTINE_ID);
    this.activeComboRequestVO.setPristineId(PRISTINE_ID);
    String content = mapper.writeValueAsString(this.activeComboRequestVO);
    when(this.promoBundlingServiceImpl
        .getActiveCombos(STORE_ID, CHANNEL_ID, CLIENT_ID,
            REQUEST_ID, USERNAME, this.activeComboRequestVO, Integer.valueOf(PAGE),
            Integer.valueOf(SIZE), SORT_BY, SORT_TYPE)).thenReturn(this.comboResponseVO);
    when(this.modelConverter
        .convertActiveComboRequestToActiveComboRequestVO(activeComboRequest))
        .thenReturn(activeComboRequestVO);
    when(this.modelConverter
        .convertListToResponse(comboResponseVO.getComboList(), ComboResponse.class))
        .thenReturn(Arrays.asList(comboResponse));

    this.mockMvc.perform(post(ProductApiPath.PROMO_BUNDLING + ProductApiPath.GET_ACTIVE_COMBOS)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).content(content)
        .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
        .param("requestId", REQUEST_ID).param("username", USERNAME).param("page", PAGE)
        .param("size", SIZE).param("sortBy", SORT_BY).param("sortType", SORT_TYPE))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.content", notNullValue()));

    verify(this.promoBundlingServiceImpl)
        .getActiveCombos(STORE_ID, CHANNEL_ID, CLIENT_ID,
            REQUEST_ID, USERNAME, this.activeComboRequestVO, Integer.valueOf(PAGE),
            Integer.valueOf(SIZE), SORT_BY, SORT_TYPE);
    verify(this.modelConverter)
        .convertActiveComboRequestToActiveComboRequestVO(activeComboRequest);
    verify(this.modelConverter)
        .convertListToResponse(comboResponseVO.getComboList(), ComboResponse.class);
  }

  @Test
  public void testGetComboDetailSuccess() throws Exception {
    when(this.promoBundlingServiceImpl.getComboDetailByItemSku(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID,
        USERNAME, ITEM_SKU, true, false)).thenReturn(this.comboDetailVo);
    when(this.modelConverter.convertComboDetailVoToComboDetailResponse(eq(comboDetailVo)))
        .thenReturn(comboDetailResponse);

    this.mockMvc
        .perform(get(ProductApiPath.PROMO_BUNDLING + ProductApiPath.GET_COMBO_DETAIL)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .param("storeId", STORE_ID).param("channelId", CHANNEL_ID)
            .param("clientId", CLIENT_ID).param("requestId", REQUEST_ID).param("username", USERNAME)
            .param("itemSku", ITEM_SKU).param("pristine", IS_PRISTINE))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.value", notNullValue()))
        .andExpect(jsonPath("$.value.promoBundlingName", equalTo(PROMO_BUNDLING_NAME)));

    verify(this.promoBundlingServiceImpl).getComboDetailByItemSku(STORE_ID, CHANNEL_ID, CLIENT_ID,
        REQUEST_ID, USERNAME, ITEM_SKU, true, false);
    verify(this.modelConverter, times(1))
        .convertComboDetailVoToComboDetailResponse(eq(comboDetailVo));
  }

  @Test
  public void testGetComboDetailThrowApplicationRuntimeException() throws Exception {
    when(this.promoBundlingServiceImpl.getComboDetailByItemSku(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID,
            USERNAME, ITEM_SKU, true, false)).thenThrow(new ApplicationRuntimeException());

    this.mockMvc.perform(get(ProductApiPath.PROMO_BUNDLING + ProductApiPath.GET_COMBO_DETAIL)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
        .param("requestId", REQUEST_ID).param("username", USERNAME).param("itemSku", ITEM_SKU)
        .param("pristine", IS_PRISTINE)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));

    verify(this.promoBundlingServiceImpl).getComboDetailByItemSku(STORE_ID, CHANNEL_ID, CLIENT_ID,
            REQUEST_ID, USERNAME, ITEM_SKU, true, false);
  }

  @Test
  public void testGetComboDetailThrowException() throws Exception {
    when(this.promoBundlingServiceImpl.getComboDetailByItemSku(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID,
            USERNAME, ITEM_SKU, true, false)).thenThrow(new Exception());

    this.mockMvc.perform(get(ProductApiPath.PROMO_BUNDLING + ProductApiPath.GET_COMBO_DETAIL)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
        .param("requestId", REQUEST_ID).param("username", USERNAME).param("itemSku", ITEM_SKU)
        .param("pristine", IS_PRISTINE)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));

    verify(this.promoBundlingServiceImpl).getComboDetailByItemSku(STORE_ID, CHANNEL_ID, CLIENT_ID,
            REQUEST_ID, USERNAME, ITEM_SKU, true, false);
  }

  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);
    this.mockMvc = standaloneSetup(this.promoBundlingController).build();
    this.mapper = new ObjectMapper();

    this.activeComboRequest = new ActiveComboRequest();
    this.activeComboRequestVO = new ActiveComboRequestVO();

    this.masterDataAttribute = new MasterDataAttribute();
    this.masterDataAttribute.setAttributeCode(ATTRIBUTE_CODE);
    this.masterDataAttribute.setAttributeName(ATTRIBUTE_NAME);

    this.masterDataItemAttributeValue = new MasterDataItemAttributeValue();
    this.masterDataItemAttributeValue.setAttributeValue(ATTRIBUTE_VALUE);
    this.masterDataItemAttributeValue.setMasterDataAttribute(this.masterDataAttribute);

    this.masterDataItemAttributeValueList = new ArrayList<>();
    this.masterDataItemAttributeValueList.add(this.masterDataItemAttributeValue);

    this.masterDataItem = new MasterDataItem();
    this.masterDataItem.setMasterDataItemAttributeValues(this.masterDataItemAttributeValueList);

    this.pristineDataItem = new PristineDataItem();
    this.pristineDataItem.setPristineId(PRISTINE_ID);

    this.item = new Item();
    this.item.setItemSku(ITEM_SKU);
    this.item.setItemCode(ITEM_CODE);
    this.item.setMasterDataItem(this.masterDataItem);
    this.item.setPristineDataItem(this.pristineDataItem);

    this.comboItemVO = new ComboItemVO();
    this.comboItemVO.setProductName(PRODUCT_NAME);
    this.comboItemVO.setItemCode(ITEM_CODE);
    this.comboItemVO.setItemSku(ITEM_SKU);
    this.comboItemVO.setQuantity(QUANTITY);

    this.comboItemVOList = new ArrayList<>();
    this.comboItemVOList.add(this.comboItemVO);

    this.comboVO = new ComboVO();
    this.comboVO.setPromoBundlingName(PROMO_BUNDLING_NAME);
    this.comboVO.setPromoBundlingId(PROMO_BUNDLING_ID);
    this.comboVO.setPromoBundlingType(PROMO_BUNDLING_TYPE);
    this.comboVO.setComboItems(comboItemVOList);

    this.comboVOList = new ArrayList<>();
    this.comboVOList.add(comboVO);

    this.comboResponseVO = new ComboResponseVO();
    this.comboResponseVO.setComboList(comboVOList);

    this.comboItem = new ComboItem();
    this.comboItem.setProductName(PRODUCT_NAME);
    this.comboItem.setItemCode(ITEM_CODE);
    this.comboItem.setItemSku(ITEM_SKU);
    this.comboItem.setQuantity(QUANTITY);

    this.comboItemList = new ArrayList<>();
    this.comboItemList.add(comboItem);

    this.comboResponse = new ComboResponse();

    this.comboResponseList = new ArrayList<>();
    this.comboResponseList.add(comboResponse);

    this.wholesaleVO = new WholesaleVO();
    this.wholesaleVO.setItemSku(ITEM_SKU);

    this.wholesaleResponse = new WholesaleResponse();
    this.wholesaleResponse.setItemSku(ITEM_SKU);
    this.wholesaleResponse.setItemCode(ITEM_CODE);

    this.comboDetailVo = new ComboDetailVo();
    this.comboDetailResponse = new ComboDetailResponse();
    this.comboDetailResponse.setPromoBundlingName(PROMO_BUNDLING_NAME);
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(this.promoBundlingServiceImpl, this.modelConverter);
  }

  @Test
  public void getWholesaleDetail_success() throws Exception {
    when(this.promoBundlingServiceImpl
        .getWholesaleByItemSku(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, ITEM_SKU, true, false))
        .thenReturn(this.wholesaleVO);
    when(this.modelConverter.convertWholesaleVOToWholesaleResponse(wholesaleVO))
        .thenReturn(wholesaleResponse);

    this.mockMvc.perform(get(ProductApiPath.PROMO_BUNDLING + ProductApiPath.GET_WHOLESALE_DETAIL)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .param("storeId", STORE_ID).param("requestId", REQUEST_ID).param("channelId", CHANNEL_ID)
        .param("clientId", CLIENT_ID).param("username", USERNAME).param("itemSku", ITEM_SKU)
        .param("pristine", IS_PRISTINE)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));

    verify(this.promoBundlingServiceImpl)
        .getWholesaleByItemSku(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, ITEM_SKU, true, false);
    verify(this.modelConverter).convertWholesaleVOToWholesaleResponse(wholesaleVO);
  }

  @Test
  public void getWholesaleDetail_expectApplicationRuntimeException() throws Exception {
    when(this.promoBundlingServiceImpl
        .getWholesaleByItemSku(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, ITEM_SKU, true, false))
        .thenThrow(ApplicationRuntimeException.class);
    when(this.modelConverter.convertWholesaleVOToWholesaleResponse(wholesaleVO))
        .thenReturn(wholesaleResponse);

    this.mockMvc.perform(get(ProductApiPath.PROMO_BUNDLING + ProductApiPath.GET_WHOLESALE_DETAIL)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .param("storeId", STORE_ID).param("requestId", REQUEST_ID).param("channelId", CHANNEL_ID)
        .param("clientId", CLIENT_ID).param("username", USERNAME).param("itemSku", ITEM_SKU)
        .param("pristine", IS_PRISTINE)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));

    verify(this.promoBundlingServiceImpl)
        .getWholesaleByItemSku(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, ITEM_SKU, true, false);
  }

  @Test
  public void getWholesaleDetail_expectException() throws Exception {
    when(this.promoBundlingServiceImpl
        .getWholesaleByItemSku(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, ITEM_SKU, true, false))
        .thenThrow(Exception.class);
    when(this.modelConverter.convertWholesaleVOToWholesaleResponse(wholesaleVO))
        .thenReturn(wholesaleResponse);

    this.mockMvc.perform(get(ProductApiPath.PROMO_BUNDLING + ProductApiPath.GET_WHOLESALE_DETAIL)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .param("storeId", STORE_ID).param("requestId", REQUEST_ID).param("channelId", CHANNEL_ID)
        .param("clientId", CLIENT_ID).param("username", USERNAME).param("itemSku", ITEM_SKU)
        .param("pristine", IS_PRISTINE)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));

    verify(this.promoBundlingServiceImpl)
        .getWholesaleByItemSku(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, ITEM_SKU, true, false);
  }
}

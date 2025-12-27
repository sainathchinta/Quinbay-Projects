package com.gdn.x.product.rest.web.controller.api;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.Matchers.notNullValue;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.openMocks;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.setup.MockMvcBuilders.standaloneSetup;

import java.io.File;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.apache.commons.io.FileUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.ResultActions;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.ObjectWriter;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.x.product.enums.ChannelName;
import com.gdn.x.product.enums.SystemParameterNames;
import com.gdn.x.product.model.entity.SystemParameter;
import com.gdn.x.product.model.vo.ItemPriceVO;
import com.gdn.x.product.model.vo.ItemSkuVO;
import com.gdn.x.product.model.vo.ProductForTransactionVO;
import com.gdn.x.product.rest.web.model.ProductApiPath;
import com.gdn.x.product.rest.web.model.dto.ItemSkuDTO;
import com.gdn.x.product.rest.web.model.enums.ProductErrorCodesEnum;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.rest.web.model.response.ProductForTransactionResponse;
import com.gdn.x.product.rest.web.model.response.SimpleItemResponse;
import com.gdn.x.product.service.api.ProductForTransactionService;
import com.gdn.x.product.service.api.SystemParameterService;
import com.gdn.x.product.service.util.ModelConverter;

public class ProductForTransactionControllerTest {
  private static final String ITEM_SKU = "item-sku";
  private static final String ITEM_SKU_1 = "sku-1";
  private static final String ITEM_SKU_2 = "sku-2";
  private static final String USERNAME = "username";
  private static final String REQUEST_ID = "request-id";
  private static final String CLIENT_ID = "client-id";
  private static final String CHANNEL_ID = "channel-id";
  private static final String STORE_ID = "store-id";
  private static final String CHANNEL = ChannelName.MOBILE_WEB.toString();
  private static final String STORE_ID_BLANK = "";
  private static final String STORE_ID_MUST_NOT_BE_BLANK = "Store id must not be blank";
  private static final String STORE_ID_MUST_NOT_BE_BLANK_APP_RUNTIME_EXCEPTION =
      "Can not process invalid input data :"
          + ProductForTransactionControllerTest.STORE_ID_MUST_NOT_BE_BLANK;
  private static final String PICKUP_POINT_CODE = "pickup-point-code";
  private static final String BUSINESS_PARTNER_CODE = "business-partner-code";

  private ProductForTransactionResponse productResponseRestWeb;
  private GdnRestListResponse<SimpleItemResponse> itemPriceRestListResponse;
  private ProductForTransactionVO productVO;
  private String itemSkuListStr;
  private SimpleListStringRequest itemSkuList;
  private List<String> itemSkuListOfStr;
  private ArrayList<ItemPriceVO> itemPriceVoList;
  private ArrayList<SimpleItemResponse> itemPriceRestWebList;
  private ObjectMapper mapper;
  private List<ItemSkuDTO> itemSkuDTOList;
  private ItemSkuDTO itemSkuDTO;
  private String itemSkuDtoListStr;
  private List<ItemSkuVO> itemSkuVOList;
  private ItemSkuVO itemSkuVO;
  private List<ItemPriceVO> itemSku;
  private List<ItemPriceVO> itemPriceVOList;
  private ItemPriceVO itemPriceVO;
  private SystemParameter systemParameter = new SystemParameter();

  @InjectMocks
  private ProductForTransactionController productForTransactionController;

  @Mock
  private ProductForTransactionService productForTransactionService;

  @Mock
  private ModelConverter modelConverter;

  @Mock
  private SystemParameterService systemParameterService;

  private MockMvc mockMvc;

  @Test
  public void getProductPriceByItemSkuTest() throws Exception {
    ResultActions resultActions =
        this.mockMvc
            .perform(
                post(ProductApiPath.GET_PRICE_BY_ITEM_SKU)
                    .accept(MediaType.APPLICATION_JSON)
                    .contentType(MediaType.APPLICATION_JSON)
                    .param(ProductApiPath.STORE_ID, ProductForTransactionControllerTest.STORE_ID)
                    .param(ProductApiPath.CHANNEL_ID,
                        ProductForTransactionControllerTest.CHANNEL_ID)
                    .param(ProductApiPath.CLIENT_ID, ProductForTransactionControllerTest.CLIENT_ID)
                    .param(ProductApiPath.REQUEST_ID,
                        ProductForTransactionControllerTest.REQUEST_ID)
                    .param(ProductApiPath.USERNAME, ProductForTransactionControllerTest.USERNAME)
                    .param("channel", ProductForTransactionControllerTest.CHANNEL)
                    .content(this.itemSkuListStr)).andExpect(status().isOk())
            .andExpect(jsonPath("$.success", equalTo(true)))
            .andExpect(jsonPath("$.content", notNullValue()));

    for (int i = 0; i < this.itemPriceVoList.size(); i++) {
      resultActions.andExpect(jsonPath("$.content[" + i + "].itemSku",
          equalTo(this.itemPriceRestWebList.get(i).getItemSku())));
      resultActions.andExpect(jsonPath("$.content[" + i + "].offerPrice",
          equalTo(this.itemPriceRestWebList.get(i).getOfferPrice())));
    }

    verify(this.productForTransactionService).getProductPriceForTransaction(
        ProductForTransactionControllerTest.STORE_ID, this.itemSkuListOfStr,
        ProductForTransactionControllerTest.CHANNEL);
    verify(this.modelConverter).convertToItemPriceRestWebGdnRestListResponse(this.itemPriceVoList,
        ProductForTransactionControllerTest.REQUEST_ID);
  }

  @Test
  public void getProductPriceByItemSkuTestWithApplicationRuntimeException() throws Exception {
    doThrow(
        new ApplicationRuntimeException(ErrorCategory.VALIDATION,
            ProductForTransactionControllerTest.STORE_ID_MUST_NOT_BE_BLANK)).when(
        this.productForTransactionService).getProductPriceForTransaction(
        ProductForTransactionControllerTest.STORE_ID_BLANK, this.itemSkuListOfStr,
        ProductForTransactionControllerTest.CHANNEL);
    MvcResult result =
        this.mockMvc
            .perform(
                post(ProductApiPath.GET_PRICE_BY_ITEM_SKU)
                    .accept(MediaType.APPLICATION_JSON)
                    .contentType(MediaType.APPLICATION_JSON)
                    .param(ProductApiPath.STORE_ID,
                        ProductForTransactionControllerTest.STORE_ID_BLANK)
                    .param(ProductApiPath.CHANNEL_ID,
                        ProductForTransactionControllerTest.CHANNEL_ID)
                    .param(ProductApiPath.CLIENT_ID, ProductForTransactionControllerTest.CLIENT_ID)
                    .param(ProductApiPath.REQUEST_ID,
                        ProductForTransactionControllerTest.REQUEST_ID)
                    .param(ProductApiPath.USERNAME, ProductForTransactionControllerTest.USERNAME)
                    .param("channel", ProductForTransactionControllerTest.CHANNEL)
                    .content(this.itemSkuListStr))
            .andExpect(status().isOk())
            .andExpect(
                jsonPath("$.errorCode",
                    equalTo(ProductErrorCodesEnum.GET_PRODUCT_PRICE_BY_ITEM_SKU.getCode())))
            .andExpect(
                jsonPath(
                    "$.errorMessage",
                    equalTo(ProductForTransactionControllerTest.STORE_ID_MUST_NOT_BE_BLANK_APP_RUNTIME_EXCEPTION)))
            .andExpect(jsonPath("$.success", equalTo(false)))
            .andExpect(jsonPath("$.content", equalTo(new ArrayList<SimpleItemResponse>())))
            .andReturn();

    verify(this.productForTransactionService).getProductPriceForTransaction(
        ProductForTransactionControllerTest.STORE_ID_BLANK, this.itemSkuListOfStr,
        ProductForTransactionControllerTest.CHANNEL);
  }


  @Test
  public void getProductPriceByItemSkuTestWithOthersException() throws Exception {
    doThrow(new Exception(ProductForTransactionControllerTest.STORE_ID_MUST_NOT_BE_BLANK)).when(
        this.productForTransactionService).getProductPriceForTransaction(
        ProductForTransactionControllerTest.STORE_ID_BLANK, this.itemSkuListOfStr,
        ProductForTransactionControllerTest.CHANNEL);
    MvcResult result =
        this.mockMvc
            .perform(
                post(ProductApiPath.GET_PRICE_BY_ITEM_SKU)
                    .accept(MediaType.APPLICATION_JSON)
                    .contentType(MediaType.APPLICATION_JSON)
                    .param(ProductApiPath.STORE_ID,
                        ProductForTransactionControllerTest.STORE_ID_BLANK)
                    .param(ProductApiPath.CHANNEL_ID,
                        ProductForTransactionControllerTest.CHANNEL_ID)
                    .param(ProductApiPath.CLIENT_ID, ProductForTransactionControllerTest.CLIENT_ID)
                    .param(ProductApiPath.REQUEST_ID,
                        ProductForTransactionControllerTest.REQUEST_ID)
                    .param(ProductApiPath.USERNAME, ProductForTransactionControllerTest.USERNAME)
                    .param("channel", ProductForTransactionControllerTest.CHANNEL)
                    .content(this.itemSkuListStr))
            .andExpect(status().isOk())
            .andExpect(
                jsonPath("$.errorCode",
                    equalTo(ProductErrorCodesEnum.GET_PRODUCT_PRICE_BY_ITEM_SKU.getCode())))
            .andExpect(
                jsonPath("$.errorMessage",
                    equalTo(ProductForTransactionControllerTest.STORE_ID_MUST_NOT_BE_BLANK)))
            .andExpect(jsonPath("$.success", equalTo(false)))
            .andExpect(jsonPath("$.content", equalTo(new ArrayList<SimpleItemResponse>())))
            .andReturn();

    verify(this.productForTransactionService).getProductPriceForTransaction(
        ProductForTransactionControllerTest.STORE_ID_BLANK, this.itemSkuListOfStr,
        ProductForTransactionControllerTest.CHANNEL);
  }

  @Test
  public void filterBuyableAndOfferPriceByOnlineAndOfflineItemSkuTest_success() throws Exception {
    when(this.modelConverter.convertRequestListToModel(this.itemSkuDTOList, ItemSkuVO.class))
        .thenReturn(this.itemSkuVOList);
    when(this.productForTransactionService.findProductPriceForOnlineAndOfflineTransaction(
        ProductForTransactionControllerTest.STORE_ID, this.itemSkuVOList, ProductForTransactionControllerTest.CHANNEL,
        ProductForTransactionControllerTest.REQUEST_ID, ProductForTransactionControllerTest.USERNAME))
        .thenReturn(this.itemPriceVoList);

    this.mockMvc
        .perform(
            post(ProductApiPath.FILTER_PRICE_BY_ONLINE_AND_OFFLINE_ITEM_SKU)
                .accept(MediaType.APPLICATION_JSON)
                .contentType(MediaType.APPLICATION_JSON)
                .param(ProductApiPath.STORE_ID, ProductForTransactionControllerTest.STORE_ID)
                .param(ProductApiPath.CHANNEL_ID,
                    ProductForTransactionControllerTest.CHANNEL_ID)
                .param(ProductApiPath.CLIENT_ID, ProductForTransactionControllerTest.CLIENT_ID)
                .param(ProductApiPath.REQUEST_ID,
                    ProductForTransactionControllerTest.REQUEST_ID)
                .param(ProductApiPath.USERNAME, ProductForTransactionControllerTest.USERNAME)
                .param("channel", ProductForTransactionControllerTest.CHANNEL)
                .content(this.itemSkuDtoListStr)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.content", notNullValue()));

    verify(this.modelConverter).convertRequestListToModel(this.itemSkuDTOList, ItemSkuVO.class);
    verify(this.productForTransactionService).findProductPriceForOnlineAndOfflineTransaction(
        ProductForTransactionControllerTest.STORE_ID, this.itemSkuVOList,
        ProductForTransactionControllerTest.CHANNEL, ProductForTransactionControllerTest.REQUEST_ID,
        ProductForTransactionControllerTest.USERNAME);
    verify(this.productForTransactionService).findProductPriceForOnlineAndOfflineTransaction(
        ProductForTransactionControllerTest.STORE_ID, this.itemSkuVOList, ProductForTransactionControllerTest.CHANNEL,
        ProductForTransactionControllerTest.REQUEST_ID, ProductForTransactionControllerTest.USERNAME);
    verify(this.modelConverter).convertToItemPriceRestWebGdnRestListResponse(this.itemPriceVoList, ProductForTransactionControllerTest.REQUEST_ID);
  }

  @Test
  public void filterBuyableAndOfferPriceByOnlineAndOfflineItemSkuTest_runTimeException() throws Exception {
    this.itemSkuVOList = new ArrayList<>();
    when(this.modelConverter.convertRequestListToModel(this.itemSkuDTOList, ItemSkuVO.class))
        .thenReturn(this.itemSkuVOList);
    doThrow(
        new ApplicationRuntimeException(ErrorCategory.VALIDATION,
            ProductForTransactionControllerTest.STORE_ID_MUST_NOT_BE_BLANK)).when(
        this.productForTransactionService).findProductPriceForOnlineAndOfflineTransaction(
        ProductForTransactionControllerTest.STORE_ID_BLANK, this.itemSkuVOList,
        ProductForTransactionControllerTest.CHANNEL, ProductForTransactionControllerTest.REQUEST_ID,
        ProductForTransactionControllerTest.USERNAME);

    MvcResult result =
        this.mockMvc
            .perform(
                post(ProductApiPath.FILTER_PRICE_BY_ONLINE_AND_OFFLINE_ITEM_SKU)
                    .accept(MediaType.APPLICATION_JSON)
                    .contentType(MediaType.APPLICATION_JSON)
                    .param(ProductApiPath.STORE_ID, ProductForTransactionControllerTest.STORE_ID_BLANK)
                    .param(ProductApiPath.CHANNEL_ID,
                        ProductForTransactionControllerTest.CHANNEL_ID)
                    .param(ProductApiPath.CLIENT_ID, ProductForTransactionControllerTest.CLIENT_ID)
                    .param(ProductApiPath.REQUEST_ID,
                        ProductForTransactionControllerTest.REQUEST_ID)
                    .param(ProductApiPath.USERNAME, ProductForTransactionControllerTest.USERNAME)
                    .param("channel", ProductForTransactionControllerTest.CHANNEL)
                    .content(this.itemSkuDtoListStr)).andExpect(status().isOk())
            .andExpect(jsonPath("$.success", equalTo(false)))
            .andExpect(jsonPath("$.content", equalTo(new ArrayList<SimpleItemResponse>())))
            .andReturn();

    verify(this.modelConverter).convertRequestListToModel(this.itemSkuDTOList, ItemSkuVO.class);
    verify(this.productForTransactionService).findProductPriceForOnlineAndOfflineTransaction(
        ProductForTransactionControllerTest.STORE_ID_BLANK, this.itemSkuVOList,
        ProductForTransactionControllerTest.CHANNEL, ProductForTransactionControllerTest.REQUEST_ID,
        ProductForTransactionControllerTest.USERNAME);
  }

  @Test
  public void filterBuyableAndOfferPriceByOnlineAndOfflineItemSkuTest_otherException() throws Exception {
    this.itemSkuVOList = new ArrayList<>();
    when(this.modelConverter.convertRequestListToModel(this.itemSkuDTOList, ItemSkuVO.class))
        .thenReturn(this.itemSkuVOList);
    doThrow(new Exception(ProductForTransactionControllerTest.STORE_ID_MUST_NOT_BE_BLANK)).when(
        this.productForTransactionService).findProductPriceForOnlineAndOfflineTransaction(
        ProductForTransactionControllerTest.STORE_ID_BLANK, this.itemSkuVOList,
        ProductForTransactionControllerTest.CHANNEL, ProductForTransactionControllerTest.REQUEST_ID,
        ProductForTransactionControllerTest.USERNAME);

    MvcResult result =
        this.mockMvc
            .perform(
                post(ProductApiPath.FILTER_PRICE_BY_ONLINE_AND_OFFLINE_ITEM_SKU)
                    .accept(MediaType.APPLICATION_JSON)
                    .contentType(MediaType.APPLICATION_JSON)
                    .param(ProductApiPath.STORE_ID, ProductForTransactionControllerTest.STORE_ID_BLANK)
                    .param(ProductApiPath.CHANNEL_ID,
                        ProductForTransactionControllerTest.CHANNEL_ID)
                    .param(ProductApiPath.CLIENT_ID, ProductForTransactionControllerTest.CLIENT_ID)
                    .param(ProductApiPath.REQUEST_ID,
                        ProductForTransactionControllerTest.REQUEST_ID)
                    .param(ProductApiPath.USERNAME, ProductForTransactionControllerTest.USERNAME)
                    .param("channel", ProductForTransactionControllerTest.CHANNEL)
                    .content(this.itemSkuDtoListStr)).andExpect(status().isOk())
            .andExpect(jsonPath("$.success", equalTo(false)))
            .andExpect(jsonPath("$.content", equalTo(new ArrayList<SimpleItemResponse>())))
            .andReturn();

    verify(this.modelConverter).convertRequestListToModel(this.itemSkuDTOList, ItemSkuVO.class);
    verify(this.productForTransactionService).findProductPriceForOnlineAndOfflineTransaction(
        ProductForTransactionControllerTest.STORE_ID_BLANK, this.itemSkuVOList,
        ProductForTransactionControllerTest.CHANNEL, ProductForTransactionControllerTest.REQUEST_ID,
        ProductForTransactionControllerTest.USERNAME);
  }

  @Test
  public void getProductByItemSku_switchFalseTest() throws Exception {
    when(this.productForTransactionService.findProductForTransactionByItemSkus(STORE_ID, REQUEST_ID,
        USERNAME, itemSkuListOfStr)).thenReturn(Arrays.asList(productVO));
    this.mockMvc
        .perform(
            post(ProductApiPath.GET_PRODUCT_BY_ITEM_SKU).accept(MediaType.APPLICATION_JSON)
                .contentType(MediaType.APPLICATION_JSON)
                .param(ProductApiPath.STORE_ID, ProductForTransactionControllerTest.STORE_ID)
                .param(ProductApiPath.CHANNEL_ID, ProductForTransactionControllerTest.CHANNEL_ID)
                .param(ProductApiPath.CLIENT_ID, ProductForTransactionControllerTest.CLIENT_ID)
                .param(ProductApiPath.REQUEST_ID, ProductForTransactionControllerTest.REQUEST_ID)
                .param(ProductApiPath.USERNAME, ProductForTransactionControllerTest.USERNAME)
                .content(this.itemSkuListStr)).andExpect(status().isOk())
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));

    verify(this.productForTransactionService).findProductForTransactionByItemSkus(STORE_ID,
        REQUEST_ID, USERNAME, itemSkuListOfStr);
    verify(this.modelConverter).convertToProductForTransactionResponse(
        Arrays.asList(this.productVO));
  }

  @Test
  public void getProductByItemSku_ExceptionTest() throws Exception {
    when(this.productForTransactionService.findProductForTransactionByItemSkus(STORE_ID, REQUEST_ID,
      USERNAME, itemSkuListOfStr)).thenThrow(Exception.class);
    this.mockMvc
        .perform(
            post(ProductApiPath.GET_PRODUCT_BY_ITEM_SKU).accept(MediaType.APPLICATION_JSON)
                .contentType(MediaType.APPLICATION_JSON)
                .param(ProductApiPath.STORE_ID, ProductForTransactionControllerTest.STORE_ID)
                .param(ProductApiPath.CHANNEL_ID, ProductForTransactionControllerTest.CHANNEL_ID)
                .param(ProductApiPath.CLIENT_ID, ProductForTransactionControllerTest.CLIENT_ID)
                .param(ProductApiPath.REQUEST_ID, ProductForTransactionControllerTest.REQUEST_ID)
                .param(ProductApiPath.USERNAME, ProductForTransactionControllerTest.USERNAME)
                .content(this.itemSkuListStr)).andExpect(status().isOk())
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(false)));
    verify(this.productForTransactionService).findProductForTransactionByItemSkus(STORE_ID, REQUEST_ID,
      USERNAME, itemSkuListOfStr);
  }

  @Test
  public void getProductByItemSku_applicationRuntimeExceptionTest() throws Exception {
    when(this.productForTransactionService.findProductForTransactionByItemSkus(STORE_ID, REQUEST_ID,
      USERNAME, itemSkuListOfStr)).thenThrow(ApplicationRuntimeException.class);
    this.mockMvc
        .perform(
            post(ProductApiPath.GET_PRODUCT_BY_ITEM_SKU).accept(MediaType.APPLICATION_JSON)
                .contentType(MediaType.APPLICATION_JSON)
                .param(ProductApiPath.STORE_ID, ProductForTransactionControllerTest.STORE_ID)
                .param(ProductApiPath.CHANNEL_ID, ProductForTransactionControllerTest.CHANNEL_ID)
                .param(ProductApiPath.CLIENT_ID, ProductForTransactionControllerTest.CLIENT_ID)
                .param(ProductApiPath.REQUEST_ID, ProductForTransactionControllerTest.REQUEST_ID)
                .param(ProductApiPath.USERNAME, ProductForTransactionControllerTest.USERNAME)
                .content(this.itemSkuListStr)).andExpect(status().isOk())
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(false)));
    verify(this.productForTransactionService).findProductForTransactionByItemSkus(STORE_ID, REQUEST_ID,
      USERNAME, itemSkuListOfStr);
  }

  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);

    this.mockMvc = standaloneSetup(this.productForTransactionController).build();

    this.mapper = new ObjectMapper();

    this.itemSkuListStr =
        FileUtils.readFileToString(new File("src/test/resources/itemSkuList.json"),
            String.valueOf(StandardCharsets.UTF_8));

    this.itemSkuList = this.mapper.readValue(this.itemSkuListStr, SimpleListStringRequest.class);
    this.itemSkuListOfStr = this.itemSkuList.getValue();
    this.itemPriceVoList = new ArrayList<ItemPriceVO>();

    this.itemPriceRestWebList = new ArrayList<SimpleItemResponse>();

    for (int i = 0; i < this.itemSkuListOfStr.size(); i++) {
      String itemSku = this.itemSkuListOfStr.get(i);
      this.itemPriceVoList.add(new ItemPriceVO.ItemPriceBuilder().setItemSku(itemSku)
          .setOfferPrice(i).build());
      this.itemPriceRestWebList.add(new SimpleItemResponse.SimpleItemResponseWebBuilder()
          .setItemSku(itemSku).setOfferPrice(i).build());
    }

    this.itemPriceRestListResponse =
        new GdnRestListResponse<SimpleItemResponse>(
            null,
            null,
            true,
            this.itemPriceRestWebList,
            new PageMetaData(this.itemPriceRestWebList.size(), 1, this.itemPriceRestWebList.size()),
            ProductForTransactionControllerTest.REQUEST_ID);

    this.productVO = new ProductForTransactionVO();
    this.productVO.setItemSku(ProductForTransactionControllerTest.ITEM_SKU);

    this.productResponseRestWeb = new ProductForTransactionResponse();
    this.productResponseRestWeb.setItemSku(ProductForTransactionControllerTest.ITEM_SKU);

    this.itemSkuDTO = new ItemSkuDTO();
    this.itemSkuDTO.setPickupPointCode(ProductForTransactionControllerTest.PICKUP_POINT_CODE);
    this.itemSkuDTO.setBusinessPartnerCode(ProductForTransactionControllerTest.BUSINESS_PARTNER_CODE);
    this.itemSkuDTO.setInstantPickup(Boolean.TRUE);
    this.itemSkuDTO.setItemSku(ProductForTransactionControllerTest.ITEM_SKU);

    this.itemSkuDTOList = new ArrayList<>();
    this.itemSkuDTOList.add(this.itemSkuDTO);
    ObjectWriter objectWriter = new ObjectMapper().writer().withDefaultPrettyPrinter();
    this.itemSkuDtoListStr = objectWriter.writeValueAsString(itemSkuDTOList);

    this.itemSkuVO = new ItemSkuVO();
    this.itemSkuVO.setBusinessPartnerCode(ProductForTransactionControllerTest.BUSINESS_PARTNER_CODE);
    this.itemSkuVO.setInstantPickup(Boolean.TRUE);
    this.itemSkuVO.setItemSku(ProductForTransactionControllerTest.ITEM_SKU);
    this.itemSkuVO.setPickupPointCode(ProductForTransactionControllerTest.PICKUP_POINT_CODE);
    this.itemSkuVOList = new ArrayList<>();
    this.itemSkuVOList.add(this.itemSkuVO);

    this.itemPriceVO = new ItemPriceVO();
    this.itemPriceVO.setItemSku(ProductForTransactionControllerTest.ITEM_SKU_1);
    this.itemPriceVO.setOfferPrice(0.0);
    this.itemPriceVO.setBuyable(0);
    this.itemPriceVO.setPickupPointCode(null);
    this.itemPriceVoList = new ArrayList<>();
    this.itemPriceVoList.add(this.itemPriceVO);
    when(this.modelConverter.convertToProductForTransactionResponse(Arrays.asList(this.productVO)))
        .thenReturn(Arrays.asList(this.productResponseRestWeb));

    when(
        this.modelConverter.convertToItemPriceRestWebGdnRestListResponse(this.itemPriceVoList,
            ProductForTransactionControllerTest.REQUEST_ID)).thenReturn(
        this.itemPriceRestListResponse);

    when(
        this.productForTransactionService.getProductPriceForTransaction(
            ProductForTransactionControllerTest.STORE_ID, this.itemSkuListOfStr,
            ProductForTransactionControllerTest.CHANNEL)).thenReturn(this.itemPriceVoList);

    systemParameter.setValue(String.valueOf(true));
    when(this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.REVERT_TRANSACTION_API_SWITCH)).thenReturn(systemParameter);
    when(this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID_BLANK,
        SystemParameterNames.REVERT_TRANSACTION_API_SWITCH)).thenReturn(systemParameter);

  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(this.productForTransactionService);
    verifyNoMoreInteractions(this.modelConverter);
    verifyNoMoreInteractions(systemParameterService);
  }

}

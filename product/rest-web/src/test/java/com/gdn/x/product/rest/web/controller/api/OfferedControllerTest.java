package com.gdn.x.product.rest.web.controller.api;

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.notNullValue;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.openMocks;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.setup.MockMvcBuilders.standaloneSetup;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.MasterDataProduct;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.vo.MasterDataProductAndItemsVO;
import com.gdn.x.product.model.vo.OfferedSummaryVo;
import com.gdn.x.product.rest.web.model.ProductApiPath;
import com.gdn.x.product.rest.web.model.request.OfferedSummaryRequest;
import com.gdn.x.product.rest.web.model.response.OfferedComboSummaryResponse;
import com.gdn.x.product.service.api.OfferedService;
import com.gdn.x.product.service.util.ModelConverter;

/**
 * Created by w.william on 2/27/2018.
 */
public class OfferedControllerTest {

  private static final String STORE_ID = "storeId";
  private static final String USERNAME = "username";
  private static final String REQUEST_ID = "requestId";
  private static final String ITEM_CODE = "itemCode";
  private static final String CLIENT_ID = "clientId";
  private static final String CHANNEL_ID = "channelId";
  private static final String PRISTINE_ID = "pristineId";
  private static final String ITEM_SKU = "itemSku";
  private static final String DEFAULT_SKU = "defaultSku";

  @InjectMocks
  private OfferedController offeredController;

  @Mock
  private OfferedService offeredService;

  @Mock
  private ModelConverter modelConverter;

  private MockMvc mockMvc;

  private ObjectMapper objectMapper;

  private OfferedSummaryVo offeredSummaryVo;

  private OfferedComboSummaryResponse offeredComboSummaryResponse;

  private MandatoryRequestParam param;

  @Test
  public void getOfferPageHeaderByItemCodeTest() throws Exception {

    OfferedSummaryRequest offeredSummaryRequest =
        new OfferedSummaryRequest();
    offeredSummaryRequest.setItemCode(ITEM_CODE);
    offeredSummaryRequest.setItemSku(ITEM_SKU);
    offeredSummaryRequest.setPristineId(PRISTINE_ID);
    offeredSummaryRequest.setDefaultSku(DEFAULT_SKU);
    String offeredComboPageHeaderRequestString =
        objectMapper.writeValueAsString(offeredSummaryRequest);

    when(offeredService.getOfferedSummary(param, PRISTINE_ID, ITEM_CODE, ITEM_SKU, DEFAULT_SKU))
        .thenReturn(offeredSummaryVo);

    when(modelConverter.convertToResponse(offeredSummaryVo, OfferedComboSummaryResponse.class))
        .thenReturn(offeredComboSummaryResponse);

    this.mockMvc.perform(post(ProductApiPath.OFFERED + ProductApiPath.OFFERED_SUMMARY)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .content(offeredComboPageHeaderRequestString)
        .param("storeId", OfferedControllerTest.STORE_ID)
        .param("channelId", OfferedControllerTest.CHANNEL_ID)
        .param("clientId", OfferedControllerTest.CLIENT_ID)
        .param("requestId", OfferedControllerTest.REQUEST_ID)
        .param("username", OfferedControllerTest.USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)));

    verify(offeredService).getOfferedSummary(param, PRISTINE_ID, ITEM_CODE, ITEM_SKU, DEFAULT_SKU);

    verify(modelConverter).convertToResponse(offeredSummaryVo, OfferedComboSummaryResponse.class);
  }

  @Test
  public void getOfferPageHeaderByItemCodeTest_Exception() throws Exception {

    String productSku = "productSku";
    Item item = new Item();
    item.setProductSku(productSku);

    OfferedSummaryRequest offeredSummaryRequest =
        new OfferedSummaryRequest();
    offeredSummaryRequest.setItemCode(ITEM_CODE);
    offeredSummaryRequest.setItemSku(ITEM_SKU);
    offeredSummaryRequest.setPristineId(PRISTINE_ID);
    String offeredComboPageHeaderRequestString =
        objectMapper.writeValueAsString(offeredSummaryRequest);

    when(offeredService.getOfferedSummary(param, PRISTINE_ID, ITEM_CODE, ITEM_SKU, null))
        .thenThrow(new ApplicationRuntimeException());

    this.mockMvc.perform(post(ProductApiPath.OFFERED + ProductApiPath.OFFERED_SUMMARY)
        .contentType(MediaType.APPLICATION_JSON).content(offeredComboPageHeaderRequestString)
        .param("storeId", OfferedControllerTest.STORE_ID)
        .param("channelId", OfferedControllerTest.CHANNEL_ID)
        .param("clientId", OfferedControllerTest.CLIENT_ID)
        .param("requestId", OfferedControllerTest.REQUEST_ID)
        .param("username", OfferedControllerTest.USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", notNullValue()))
        .andExpect(jsonPath("$.errorCode", notNullValue()))
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(OfferedControllerTest.REQUEST_ID)));

    verify(offeredService).getOfferedSummary(param, PRISTINE_ID, ITEM_CODE, ITEM_SKU, null);
  }

  @Test
  public void getAttributeSummaryTest() throws Exception {

    OfferedSummaryRequest offeredSummaryRequest =
        new OfferedSummaryRequest();
    offeredSummaryRequest.setItemCode(ITEM_CODE);
    offeredSummaryRequest.setItemSku(ITEM_SKU);
    offeredSummaryRequest.setPristineId(PRISTINE_ID);
    String offeredComboPageHeaderRequestString =
        objectMapper.writeValueAsString(offeredSummaryRequest);

    when(offeredService.getOfferedComboSummary(param, PRISTINE_ID, ITEM_CODE, ITEM_SKU, null))
        .thenReturn(offeredSummaryVo);

    when(modelConverter.convertToResponse(offeredSummaryVo, OfferedComboSummaryResponse.class))
        .thenReturn(offeredComboSummaryResponse);

    this.mockMvc.perform(post(ProductApiPath.OFFERED + ProductApiPath.OFFERED_COMBO_SUMMARY)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .content(offeredComboPageHeaderRequestString)
        .param("storeId", OfferedControllerTest.STORE_ID)
        .param("channelId", OfferedControllerTest.CHANNEL_ID)
        .param("clientId", OfferedControllerTest.CLIENT_ID)
        .param("requestId", OfferedControllerTest.REQUEST_ID)
        .param("username", OfferedControllerTest.USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.value", notNullValue()));

    verify(offeredService).getOfferedComboSummary(param, PRISTINE_ID, ITEM_CODE, ITEM_SKU, null);

    verify(modelConverter).convertToResponse(offeredSummaryVo, OfferedComboSummaryResponse.class);
  }

  @Test
  public void getAttributeSummaryTest_Exception() throws Exception {

    String productSku = "productSku";
    Item item = new Item();
    item.setProductSku(productSku);

    OfferedSummaryRequest offeredSummaryRequest =
        new OfferedSummaryRequest();
    offeredSummaryRequest.setItemCode(ITEM_CODE);
    offeredSummaryRequest.setItemSku(ITEM_SKU);
    offeredSummaryRequest.setPristineId(PRISTINE_ID);
    String offeredComboPageHeaderRequestString =
        objectMapper.writeValueAsString(offeredSummaryRequest);

    when(offeredService.getOfferedComboSummary(param, PRISTINE_ID, ITEM_CODE, ITEM_SKU, null))
        .thenThrow(new RuntimeException());

    this.mockMvc.perform(post(ProductApiPath.OFFERED + ProductApiPath.OFFERED_COMBO_SUMMARY)
        .contentType(MediaType.APPLICATION_JSON).content(offeredComboPageHeaderRequestString)
        .param("storeId", OfferedControllerTest.STORE_ID)
        .param("channelId", OfferedControllerTest.CHANNEL_ID)
        .param("clientId", OfferedControllerTest.CLIENT_ID)
        .param("requestId", OfferedControllerTest.REQUEST_ID)
        .param("username", OfferedControllerTest.USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", notNullValue()))
        .andExpect(jsonPath("$.errorCode", notNullValue()))
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(OfferedControllerTest.REQUEST_ID)));

    verify(offeredService).getOfferedComboSummary(param, PRISTINE_ID, ITEM_CODE, ITEM_SKU, null);
  }

  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);
    this.mockMvc = standaloneSetup(this.offeredController).build();

    param = MandatoryRequestParam
        .generateMandatoryRequestParam(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID);

    objectMapper = new ObjectMapper();

    String productSku = "productSku";
    Item item = new Item();
    item.setProductSku(productSku);
    String productCode = "productCode";
    Product product = new Product();
    product.setProductCode(productCode);

    Set<String> productCodes = new HashSet<>();
    productCodes.add(productCode);

    MasterDataProduct masterDataProduct = new MasterDataProduct();
    masterDataProduct.setBrand("brand");
    masterDataProduct.setMasterDataProductAttributes(new ArrayList<>());

    Map<String, MasterDataProductAndItemsVO> masterDataProductAndItemVOs = new HashMap<>();
    MasterDataProductAndItemsVO masterDataProductAndItemsVO = new MasterDataProductAndItemsVO();
    masterDataProductAndItemsVO.setMasterDataItems(new HashMap<>());
    masterDataProductAndItemsVO.setMasterDataProduct(masterDataProduct);
    masterDataProductAndItemVOs.put("itemCode", masterDataProductAndItemsVO);

    offeredComboSummaryResponse = new OfferedComboSummaryResponse();
    offeredComboSummaryResponse.setId("id");
    offeredComboSummaryResponse.setAttributes(new HashMap<>());
    offeredComboSummaryResponse.setImageUrl("imageUrl");
    offeredComboSummaryResponse.setOtherAttributes(new ArrayList<>());
    offeredComboSummaryResponse.setName("name");
    offeredComboSummaryResponse.setBrand("brand");

    offeredSummaryVo = new OfferedSummaryVo();
    offeredSummaryVo.setId("id");
    offeredSummaryVo.setAttributes(new HashMap<>());
    offeredSummaryVo.setImageUrl("imageUrl");
    offeredSummaryVo.setOtherAttributes(new ArrayList<>());
    offeredSummaryVo.setName("name");
    offeredSummaryVo.setBrand("brand");
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(this.offeredService);
    verifyNoMoreInteractions(this.modelConverter);
  }
}

package com.gdn.x.product.rest.web.controller.api;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.openMocks;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.setup.MockMvcBuilders.standaloneSetup;

import org.apache.commons.lang3.StringUtils;
import org.hamcrest.CoreMatchers;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

import com.gdn.common.base.mapper.GdnMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.x.product.model.vo.DefaultItemSkuVO;
import com.gdn.x.product.model.vo.PristineItemDetailAndMappingVo;
import com.gdn.x.product.rest.web.model.ProductApiPath;
import com.gdn.x.product.rest.web.model.enums.ProductErrorCodesEnum;
import com.gdn.x.product.rest.web.model.response.PristineItemDetailAndMapping;
import com.gdn.x.product.service.PristineService;

/**
 * Created by govind on 27/12/2017 AD.
 */
public class PristineControllerTest {

  private static final String STORE_ID = "store-id";

  private static final String USERNAME = "username";

  private static final String PRISTINE_ID = "pristineId";

  private static final String PRODUCT_SKU = "productSku";

  private static final String PRODUCT_CODE = "productCode";

  private static final String PRISTINE_MASTER_ID = "pristineMasterId";

  private static final String CHANNEL_ID = "channel-id";

  private static final String CLIENT_ID = "client-id";

  private static final String REQUEST_ID = "request-id";

  private static final String DEFAULT_SKU = "defaultSku";

  @InjectMocks
  private PristineController pristineController;

  @Mock
  private PristineService pristineService;

  @Mock
  private GdnMapper gdnMapper;

  private MockMvc mockMvc;
  private MandatoryRequestParam mandatoryRequestParam;

  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);

    mandatoryRequestParam = MandatoryRequestParam
        .generateMandatoryRequestParam(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID);
    mandatoryRequestParam.setUsername(USERNAME);

    this.mockMvc = standaloneSetup(this.pristineController).build();
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(this.pristineService);
    verifyNoMoreInteractions(this.gdnMapper);
  }

  @Test
  public void getMapForPristineItemsByIdTest() throws Exception {

    PristineItemDetailAndMappingVo pristineItemDetailAndMappingVo = new PristineItemDetailAndMappingVo();
    when(this.pristineService
        .getPristineItemsMappingByPristineId(any(MandatoryRequestParam.class), eq("pristineId"),
            anyString()))
        .thenReturn(pristineItemDetailAndMappingVo);
    when(gdnMapper.deepCopy(Mockito.isNull(), eq(PristineItemDetailAndMapping.class)))
        .thenReturn(new PristineItemDetailAndMapping());
    this.mockMvc.perform(get(ProductApiPath.PRISTINE + ProductApiPath.PRISTINE_ITEM_MAP_BY_ID)
        .contentType(MediaType.APPLICATION_JSON)
        .param("storeId", this.STORE_ID)
        .param("channelId", this.CHANNEL_ID)
        .param("clientId", this.CLIENT_ID)
        .param("requestId", this.REQUEST_ID)
        .param("pristineId", "pristineId")).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", nullValue()))
        .andExpect(jsonPath("$.errorCode", nullValue()))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.value", notNullValue()))
        .andExpect(jsonPath("$.requestId", equalTo(this.REQUEST_ID)));
    verify(this.pristineService)
        .getPristineItemsMappingByPristineId(any(MandatoryRequestParam.class), eq("pristineId"),
            eq(null));
    verify(gdnMapper).deepCopy(Mockito.isNull(), eq(PristineItemDetailAndMapping.class));
  }

  @Test
  public void getMapForPristineItemsById_WithDefaultSku() throws Exception {

    PristineItemDetailAndMappingVo pristineItemDetailAndMappingVo = new PristineItemDetailAndMappingVo();
    when(this.pristineService
        .getPristineItemsMappingByPristineId(any(MandatoryRequestParam.class), eq("pristineId"),
            anyString()))
        .thenReturn(pristineItemDetailAndMappingVo);
    when(gdnMapper.deepCopy(any(PristineItemDetailAndMappingVo.class), eq(PristineItemDetailAndMapping.class)))
        .thenReturn(new PristineItemDetailAndMapping());
    this.mockMvc.perform(get(ProductApiPath.PRISTINE + ProductApiPath.PRISTINE_ITEM_MAP_BY_ID)
        .contentType(MediaType.APPLICATION_JSON)
        .param("storeId", this.STORE_ID)
        .param("channelId", this.CHANNEL_ID)
        .param("clientId", this.CLIENT_ID)
        .param("requestId", this.REQUEST_ID)
        .param("pristineId", "pristineId")
        .param("defaultSku", DEFAULT_SKU))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", nullValue()))
        .andExpect(jsonPath("$.errorCode", nullValue()))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.value", notNullValue()))
        .andExpect(jsonPath("$.requestId", equalTo(this.REQUEST_ID)));
    verify(gdnMapper).deepCopy(any(PristineItemDetailAndMappingVo.class), eq(PristineItemDetailAndMapping.class));
    verify(this.pristineService)
        .getPristineItemsMappingByPristineId(any(MandatoryRequestParam.class),
            eq("pristineId"), eq(DEFAULT_SKU));
  }

  @Test
  public void getMapForPristineItemsById_ExceptionTest() throws Exception {

    PristineItemDetailAndMappingVo pristineItemDetailAndMappingVo = new PristineItemDetailAndMappingVo();
    doThrow(RuntimeException.class).when(this.pristineService)
        .getPristineItemsMappingByPristineId(any(MandatoryRequestParam.class), eq("pristineId"),
            Mockito.isNull());
    this.mockMvc.perform(get(ProductApiPath.PRISTINE + ProductApiPath.PRISTINE_ITEM_MAP_BY_ID)
        .contentType(MediaType.APPLICATION_JSON)
        .param("storeId", this.STORE_ID)
        .param("channelId", this.CHANNEL_ID)
        .param("clientId", this.CLIENT_ID)
        .param("requestId", this.REQUEST_ID)
        .param("pristineId", "pristineId")).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", notNullValue()))
        .andExpect(jsonPath("$.errorCode", notNullValue()))
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.value", notNullValue()))
        .andExpect(jsonPath("$.requestId", equalTo(this.REQUEST_ID)));
    verify(this.pristineService)
        .getPristineItemsMappingByPristineId(any(MandatoryRequestParam.class), eq("pristineId"),
            eq(null));
  }

  @Test
  public void getPristineMasterIdByPristineIdOrProductCodeOrSkuTest() throws Exception {
    when(this.pristineService.getPristineMasterIdByPristineIdOrProductCodeOrSku(this.STORE_ID, this.USERNAME,
        this.REQUEST_ID, this.PRISTINE_ID))
        .thenReturn(PRISTINE_MASTER_ID);
    this.mockMvc
        .perform(
            get(ProductApiPath.PRISTINE + ProductApiPath
                .GET_PRISTINE_MASTER_ID_BY_PRISTINE_ID_OR_PRODUCT_CODE_OR_SKU)
                .contentType(MediaType.APPLICATION_JSON)
                .param("storeId", this.STORE_ID)
                .param("channelId", this.CHANNEL_ID)
                .param("clientId", this.CLIENT_ID)
                .param("requestId", this.REQUEST_ID)
                .param("username", this.USERNAME)
                .param("pristineIdOrProductCodeOrSku", this.PRISTINE_ID))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", CoreMatchers.nullValue()))
        .andExpect(jsonPath("$.errorCode", CoreMatchers.nullValue()))
        .andExpect(jsonPath("$.success", Matchers.equalTo(true)))
        .andExpect(jsonPath("$.value", notNullValue()))
        .andExpect(jsonPath("$.requestId", Matchers.equalTo(PristineControllerTest.REQUEST_ID)));
    verify(this.pristineService).getPristineMasterIdByPristineIdOrProductCodeOrSku(
        PristineControllerTest.STORE_ID, PristineControllerTest.USERNAME,
        PristineControllerTest.REQUEST_ID, PristineControllerTest.PRISTINE_ID);
  }

  @Test
  public void getPristineMasterIdByPristineIdOrProductSku_ExceptionTest() throws Exception {
    doThrow(RuntimeException.class).when(this.pristineService)
        .getPristineMasterIdByPristineIdOrProductCodeOrSku(this.STORE_ID, this.USERNAME,
            this.REQUEST_ID, this.PRISTINE_ID);
    this.mockMvc
        .perform(
            get(ProductApiPath.PRISTINE + ProductApiPath
                .GET_PRISTINE_MASTER_ID_BY_PRISTINE_ID_OR_PRODUCT_CODE_OR_SKU)
                .contentType(MediaType.APPLICATION_JSON)
                .param("storeId", this.STORE_ID)
                .param("channelId", this.CHANNEL_ID)
                .param("clientId", this.CLIENT_ID)
                .param("requestId", this.REQUEST_ID)
                .param("username", this.USERNAME)
                .param("pristineIdOrProductCodeOrSku", this.PRISTINE_ID))
        .andExpect(jsonPath("$.errorMessage",
            Matchers.equalTo(ProductErrorCodesEnum.INTERNAL_SERVER.getMessage()))).andExpect(
        jsonPath("$.errorCode",
            Matchers.equalTo(ProductErrorCodesEnum.INTERNAL_SERVER.getCode())))
        .andExpect(jsonPath("$.success", Matchers.equalTo(false)))
        .andExpect(jsonPath("$.value", CoreMatchers.nullValue()))
        .andExpect(jsonPath("$.requestId", Matchers.equalTo(PristineControllerTest.REQUEST_ID)));
    verify(this.pristineService).getPristineMasterIdByPristineIdOrProductCodeOrSku(this.STORE_ID, this.USERNAME,
        this.REQUEST_ID, this.PRISTINE_ID);
  }

  @Test
  public void getPristineMasterIdByPristineIdOrProductSku_ApplicationRuntimeExceptionTest() throws Exception {
    doThrow(new ApplicationRuntimeException()).when(this.pristineService)
        .getPristineMasterIdByPristineIdOrProductCodeOrSku(this.STORE_ID, this.USERNAME,
            this.REQUEST_ID, this.PRISTINE_ID);
    this.mockMvc
        .perform(
            get(ProductApiPath.PRISTINE + ProductApiPath
                .GET_PRISTINE_MASTER_ID_BY_PRISTINE_ID_OR_PRODUCT_CODE_OR_SKU)
                .contentType(MediaType.APPLICATION_JSON)
                .param("storeId", this.STORE_ID)
                .param("channelId", this.CHANNEL_ID)
                .param("clientId", this.CLIENT_ID)
                .param("requestId", this.REQUEST_ID)
                .param("username", this.USERNAME)
                .param("pristineIdOrProductCodeOrSku", this.PRISTINE_ID))
        .andExpect(jsonPath("$.errorMessage",
            Matchers.equalTo(ProductErrorCodesEnum.GET_PRISTINE_MASTER_ID.getMessage()))).andExpect(
        jsonPath("$.errorCode",
            Matchers.equalTo(ProductErrorCodesEnum.GET_PRISTINE_MASTER_ID.getCode())))
        .andExpect(jsonPath("$.success", Matchers.equalTo(false)))
        .andExpect(jsonPath("$.value", CoreMatchers.nullValue()))
        .andExpect(jsonPath("$.requestId", Matchers.equalTo(PristineControllerTest.REQUEST_ID)));
    verify(this.pristineService).getPristineMasterIdByPristineIdOrProductCodeOrSku(this.STORE_ID, this.USERNAME,
        this.REQUEST_ID, this.PRISTINE_ID);
  }

  @Test
  public void getDefaultItemSkuByPristineIdTest() throws Exception {
    DefaultItemSkuVO defaultSkuVo = new DefaultItemSkuVO();
    defaultSkuVo.setDefaultItemSku(DEFAULT_SKU);
    when(this.pristineService.getDefaultItemSkuByPristineId(mandatoryRequestParam, PRISTINE_ID))
        .thenReturn(defaultSkuVo);
    this.mockMvc
        .perform(
            get(ProductApiPath.PRISTINE + ProductApiPath
                .GET_DEFAULT_ITEM_SKU_BY_PRISTINE_ID)
                .contentType(MediaType.APPLICATION_JSON)
                .param("storeId", this.STORE_ID)
                .param("channelId", this.CHANNEL_ID)
                .param("clientId", this.CLIENT_ID)
                .param("requestId", this.REQUEST_ID)
                .param("username", this.USERNAME)
                .param("pristineId", this.PRISTINE_ID))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", Matchers.equalTo(StringUtils.EMPTY)))
        .andExpect(jsonPath("$.errorCode", Matchers.equalTo(StringUtils.EMPTY)))
        .andExpect(jsonPath("$.success", Matchers.equalTo(true)))
        .andExpect(jsonPath("$.value", notNullValue()))
        .andExpect(jsonPath("$.requestId", Matchers.equalTo(PristineControllerTest.REQUEST_ID)));
    verify(this.pristineService).getDefaultItemSkuByPristineId(this.mandatoryRequestParam, this.PRISTINE_ID);
  }

  @Test
  public void getDefaultItemSkuByPristineIdTest_ApplicationRumtimeExceptionTest() throws Exception {
    doThrow(new ApplicationRuntimeException()).when(this.pristineService)
        .getDefaultItemSkuByPristineId(this.mandatoryRequestParam, this.PRISTINE_ID);
    this.mockMvc
        .perform(
            get(ProductApiPath.PRISTINE + ProductApiPath
                .GET_DEFAULT_ITEM_SKU_BY_PRISTINE_ID)
                .contentType(MediaType.APPLICATION_JSON)
                .param("storeId", this.STORE_ID)
                .param("channelId", this.CHANNEL_ID)
                .param("clientId", this.CLIENT_ID)
                .param("requestId", this.REQUEST_ID)
                .param("username", this.USERNAME)
                .param("pristineId", this.PRISTINE_ID))
        .andExpect(jsonPath("$.errorMessage",
            Matchers.equalTo(ProductErrorCodesEnum.GET_DEFAULT_ITEM_SKU_BY_PRISTINE_ID.getMessage())))
        .andExpect(jsonPath("$.errorCode",
            Matchers.equalTo(ProductErrorCodesEnum.GET_DEFAULT_ITEM_SKU_BY_PRISTINE_ID.getCode())))
        .andExpect(jsonPath("$.success", Matchers.equalTo(false)))
        .andExpect(jsonPath("$.value", CoreMatchers.nullValue()))
        .andExpect(jsonPath("$.requestId", Matchers.equalTo(PristineControllerTest.REQUEST_ID)));
    verify(this.pristineService).getDefaultItemSkuByPristineId(this.mandatoryRequestParam, this.PRISTINE_ID);
  }

  @Test
  public void getDefaultItemSkuByPristineIdTest_ExceptionTest() throws Exception {
    doThrow(Exception.class).when(this.pristineService)
        .getDefaultItemSkuByPristineId(this.mandatoryRequestParam, this.PRISTINE_ID);
    this.mockMvc
        .perform(
            get(ProductApiPath.PRISTINE + ProductApiPath
                .GET_DEFAULT_ITEM_SKU_BY_PRISTINE_ID)
                .contentType(MediaType.APPLICATION_JSON)
                .param("storeId", this.STORE_ID)
                .param("channelId", this.CHANNEL_ID)
                .param("clientId", this.CLIENT_ID)
                .param("requestId", this.REQUEST_ID)
                .param("username", this.USERNAME)
                .param("pristineId", this.PRISTINE_ID))
        .andExpect(jsonPath("$.errorMessage",
            Matchers.equalTo(ProductErrorCodesEnum.INTERNAL_SERVER.getMessage()))).andExpect(
        jsonPath("$.errorCode",
            Matchers.equalTo(ProductErrorCodesEnum.INTERNAL_SERVER.getCode())))
        .andExpect(jsonPath("$.success", Matchers.equalTo(false)))
        .andExpect(jsonPath("$.value", CoreMatchers.nullValue()))
        .andExpect(jsonPath("$.requestId", Matchers.equalTo(PristineControllerTest.REQUEST_ID)));
    verify(this.pristineService).getDefaultItemSkuByPristineId(this.mandatoryRequestParam, this.PRISTINE_ID);
  }
}

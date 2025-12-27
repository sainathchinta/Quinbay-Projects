package com.gdn.x.productcategorybase.controller;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.productcategorybase.DistributionInfoApiPath;
import com.gdn.x.productcategorybase.dto.request.DimensionAndUomDTO;
import com.gdn.x.productcategorybase.dto.request.DistributionInfoUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.DistributionItemInfoRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemUomInfoDTO;
import com.gdn.x.productcategorybase.dto.response.DistributionInfoPerSkuResponse;
import com.gdn.x.productcategorybase.service.DistributionInfoWrapperService;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

import java.util.Arrays;
import java.util.Collections;
import java.util.Map;

import static org.hamcrest.Matchers.equalTo;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.initMocks;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.setup.MockMvcBuilders.standaloneSetup;

public class DistributionInfoControllerTest {

  private static final String CLIENT_ID = "client-id";
  private static final String CHANNEL_ID = "channel-id";
  private static final String REQUEST_ID = "REQ-001";
  private static final String STORE_ID = "10001";
  private static final String USERNAME = "developer";
  private static final String PRODUCT_CODE = "PROD-001";
  private static final String SELLER_CODE = "SELLER-001";
  private static final String SKU_CODE_1 = "SKU-001";
  private static final String SKU_CODE_2 = "SKU-002";

  @InjectMocks
  private DistributionInfoController controller;

  @Mock
  private DistributionInfoWrapperService distributionInfoWrapperService;

  private static final ObjectMapper objectMapper = new ObjectMapper();

  private MockMvc mockMvc;
  private DistributionInfoUpdateRequest validRequest;
  private DimensionAndUomDTO dimensionAndUOMDTO;

  @BeforeEach
  public void setUp() {
    initMocks(this);
    this.mockMvc = standaloneSetup(this.controller).build();

    // Create dimension and UOM DTO
    dimensionAndUOMDTO =
        DimensionAndUomDTO.builder().uomCode("CM").uomType("LENGTH").length(10.0).width(5.0)
            .height(2.0).conversion(1.0).build();

    // Create distribution item info request
    DistributionItemInfoRequest distributionItemInfoRequest1 =
        DistributionItemInfoRequest.builder().omniChannelSku("OMNI-001").origin("LOCAL")
            .expiry(false).build();

    DistributionItemInfoRequest distributionItemInfoRequest2 =
        DistributionItemInfoRequest.builder().omniChannelSku("OMNI-002").origin("IMPORT")
            .expiry(true).build();

    // Create product item UOM info DTOs
    ProductItemUomInfoDTO productItemUomInfoDTO1 =
        ProductItemUomInfoDTO.builder().skuCode(SKU_CODE_1)
            .distributionItemInfoRequest(distributionItemInfoRequest1)
            .dimensionAndUomDTOList(Arrays.asList(dimensionAndUOMDTO)).build();

    ProductItemUomInfoDTO productItemUomInfoDTO2 =
        ProductItemUomInfoDTO.builder().skuCode(SKU_CODE_2)
            .distributionItemInfoRequest(distributionItemInfoRequest2)
            .dimensionAndUomDTOList(Arrays.asList(dimensionAndUOMDTO)).build();

    // Create valid request
    validRequest = DistributionInfoUpdateRequest.builder().sellerCode(SELLER_CODE)
        .productItemUomInfoDTOS(Arrays.asList(productItemUomInfoDTO1, productItemUomInfoDTO2))
        .distributionInfoRequest(Map.of("key1", "value1", "key2", "value2")).build();
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(distributionInfoWrapperService);
  }

  @Test
  public void updateDistributionInfo_success() throws Exception {
    this.mockMvc.perform(
            post(DistributionInfoApiPath.BASE_PATH + DistributionInfoApiPath.DISTRIBUTION_INFO,
                PRODUCT_CODE).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(validRequest)).param("storeId", STORE_ID)
                .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
                .param("requestId", REQUEST_ID).param("username", USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));

    Mockito.verify(distributionInfoWrapperService)
        .updateDistributionInfoAndPublishProduct(STORE_ID, PRODUCT_CODE, validRequest);
  }

  @Test
  public void updateDistributionInfo_serviceThrowsException_returnsFalse() throws Exception {
    Mockito.doThrow(new RuntimeException("Service error")).when(distributionInfoWrapperService)
        .updateDistributionInfoAndPublishProduct(STORE_ID, PRODUCT_CODE, validRequest);

    this.mockMvc.perform(
            post(DistributionInfoApiPath.BASE_PATH + DistributionInfoApiPath.DISTRIBUTION_INFO,
                PRODUCT_CODE).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(validRequest)).param("storeId", STORE_ID)
                .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
                .param("requestId", REQUEST_ID).param("username", USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));

    Mockito.verify(distributionInfoWrapperService)
        .updateDistributionInfoAndPublishProduct(STORE_ID, PRODUCT_CODE, validRequest);
  }

  @Test
  public void getDistributionInfo_success() throws Exception {
    Page<DistributionInfoPerSkuResponse> pageData =
        new PageImpl<>(Collections.singletonList(new DistributionInfoPerSkuResponse()),
            PageRequest.of(0, 50, Sort.by("skuCode").ascending()), 1);
    when(distributionInfoWrapperService.getDistributionInfo(STORE_ID, PRODUCT_CODE,
        PageRequest.of(0, 50, Sort.by("skuCode").ascending()))).thenReturn(pageData);
    mockMvc.perform(
            get(DistributionInfoApiPath.BASE_PATH + DistributionInfoApiPath.DISTRIBUTION_INFO,
                PRODUCT_CODE).param("storeId", STORE_ID).param("requestId", REQUEST_ID)
                .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
                .param("needDistributionInfoResponse", "false").param("page", "0").param("size",
                    "50")
                .accept(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(distributionInfoWrapperService).getDistributionInfo(anyString(), anyString(),
         any(Pageable.class));
  }

  @Test
  public void getDistributionInfo_applicationRuntimeException() throws Exception {
    when(distributionInfoWrapperService.getDistributionInfo(anyString(), anyString(), 
        any(Pageable.class))).thenThrow(new ApplicationRuntimeException());
    mockMvc.perform(
            get(DistributionInfoApiPath.BASE_PATH + DistributionInfoApiPath.DISTRIBUTION_INFO,
                PRODUCT_CODE).param("storeId", STORE_ID).param("requestId", REQUEST_ID)
                .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
                .accept(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
    verify(distributionInfoWrapperService).getDistributionInfo(anyString(), anyString(),
         any(Pageable.class));
  }

  @Test
  public void getDistributionInfo_genericException() throws Exception {
    when(distributionInfoWrapperService.getDistributionInfo(anyString(), anyString(), 
        any(Pageable.class))).thenThrow(new RuntimeException("Unexpected Error"));
    mockMvc.perform(
            get(DistributionInfoApiPath.BASE_PATH + DistributionInfoApiPath.DISTRIBUTION_INFO,
                PRODUCT_CODE).param("storeId", STORE_ID).param("requestId", REQUEST_ID)
                .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("page", "1")
                .param("size", "20").accept(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
    verify(distributionInfoWrapperService).getDistributionInfo(anyString(), anyString(),
         any(Pageable.class));
  }
} 
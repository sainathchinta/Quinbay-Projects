package com.gdn.mta.product.controller;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

import java.util.*;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.DimensionAndUomRequest;
import com.gda.mta.product.dto.DistributionInfoRequest;
import com.gda.mta.product.dto.DistributionItemRequest;
import com.gda.mta.product.dto.ProductItemDistributionInfoRequest;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.product.enums.ApiErrorCode;
import com.gdn.mta.product.enums.UomType;
import com.gdn.mta.product.service.DistributionInfoService;
import com.gdn.mta.product.service.exception.ApiDataNotFoundException;
import com.gdn.x.productcategorybase.dto.response.DistributionInfoPerSkuResponse;

/**
 * Comprehensive test class for DistributionInfoController
 * Tests all endpoints and various scenarios
 */
@ExtendWith(MockitoExtension.class)
class DistributionInfoControllerTest {

    private static final String BASE_URL = "/api/distribution-info";
    private static final String UPDATE_ENDPOINT = "/{productCode}";
    private static final String STORE_ID = "TEST_STORE";
    private static final String REQUEST_ID = "REQ_12345";
    private static final String CHANNEL_ID = "WEB";
    private static final String CLIENT_ID = "CLIENT_001";
    private static final String USERNAME = "testuser";
    private static final String PRODUCT_CODE = "PROD_001";
    private static final String SELLER_CODE = "SELLER_001";
    private static final String SKU_CODE = "SKU_001";
    private static final String OMNICHANNEL_SKU = "OMNI_001";
    private static final String UOM_CODE = "PCS";
    private static final String PRODUCT_NAME = "Test Product";
    private static final String CATEGORY_NAME = "Test Category";
    private static final String ORIGIN = "LOCAL";

    @Mock
    private DistributionInfoService distributionInfoService;

    @InjectMocks
    private DistributionInfoController distributionInfoController;

    private MockMvc mockMvc;
    private ObjectMapper objectMapper;
    private DistributionInfoRequest distributionInfoRequest;

    @BeforeEach
    void setUp() {
        mockMvc = MockMvcBuilders.standaloneSetup(distributionInfoController).build();
        objectMapper = new ObjectMapper();
        setupTestData();
    }

    private void setupTestData() {
        // Setup DistributionInfoRequest
        Map<String, String> productDistributionInfo = new HashMap<>();
        productDistributionInfo.put("productName", PRODUCT_NAME);
        productDistributionInfo.put("categoryName", CATEGORY_NAME);

        Set<String> upcEanSet = new HashSet<>();
        upcEanSet.add("123456789");

        DimensionAndUomRequest dimensionAndUomRequest = DimensionAndUomRequest.builder()
            .uomCode(UOM_CODE)
            .uomType(UomType.Base.name())
            .conversion(1.0)
            .length(10.0)
            .width(5.0)
            .height(2.0)
            .weight(1.0)
            .upcEanList(upcEanSet)
            .build();

        DistributionItemRequest distributionItemRequest = DistributionItemRequest.builder()
            .omniChannelSku(OMNICHANNEL_SKU)
            .origin(ORIGIN)
            .expiry(false)
            .build();

        ProductItemDistributionInfoRequest productItemDistributionInfoRequest = ProductItemDistributionInfoRequest.builder()
            .skuCode(SKU_CODE)
            .distributionItemInfoRequest(distributionItemRequest)
            .dimensionsAndUOMRequest(new ArrayList<>(Arrays.asList(dimensionAndUomRequest)))
            .build();

        List<ProductItemDistributionInfoRequest> productItemRequests = new ArrayList<>();
        productItemRequests.add(productItemDistributionInfoRequest);
        
        distributionInfoRequest = DistributionInfoRequest.builder()
            .sellerCode(SELLER_CODE)
            .distributionInfoRequest(productDistributionInfo)
            .productItems(productItemRequests)
            .build();
    }

    // ================ Success Scenarios ================

    @Test
    void testUpdateDistributionInfo_Success() throws Exception {
        // Given
        doNothing().when(distributionInfoService).validateAndUpdateDistributionInfo(eq(PRODUCT_CODE), any(DistributionInfoRequest.class),
            eq(USERNAME));

        // When & Then
        mockMvc.perform(post(BASE_URL + UPDATE_ENDPOINT, PRODUCT_CODE)
                .param("storeId", STORE_ID)
                .param("requestId", REQUEST_ID)
                .param("channelId", CHANNEL_ID)
                .param("clientId", CLIENT_ID)
                .param("username", USERNAME)
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(distributionInfoRequest)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.success").value(true));

        verify(distributionInfoService).validateAndUpdateDistributionInfo(eq(PRODUCT_CODE), any(DistributionInfoRequest.class),
            eq(USERNAME));
    }

    @Test
    void testUpdateDistributionInfo_ApplicationRuntimeException_ValidationError() throws Exception {
        // Given
        ApplicationRuntimeException exception = new ApplicationRuntimeException(
            ErrorCategory.VALIDATION,
            ApiErrorCode.UPC_CODE_UPDATE_FAILED.getDesc());
        doThrow(exception).when(distributionInfoService).validateAndUpdateDistributionInfo(eq(PRODUCT_CODE), any(DistributionInfoRequest.class),
            eq(USERNAME));

        // When & Then
        mockMvc.perform(post(BASE_URL + UPDATE_ENDPOINT, PRODUCT_CODE)
                .param("storeId", STORE_ID)
                .param("requestId", REQUEST_ID)
                .param("channelId", CHANNEL_ID)
                .param("clientId", CLIENT_ID)
                .param("username", USERNAME)
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(distributionInfoRequest)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.success").value(false))
                .andExpect(jsonPath("$.errorMessage").value("Can not process invalid input data :"+ApiErrorCode.UPC_CODE_UPDATE_FAILED.getDesc()))
                .andExpect(jsonPath("$.errorCode").exists())
                .andExpect(jsonPath("$.requestId").value(REQUEST_ID));

        verify(distributionInfoService).validateAndUpdateDistributionInfo(eq(PRODUCT_CODE), any(DistributionInfoRequest.class),
            eq(USERNAME));
    }

    @Test
    void testUpdateDistributionInfo_Exception_ValidationError() throws Exception {
        // Given
        ApiDataNotFoundException exception =
            new ApiDataNotFoundException(ErrorCategory.VALIDATION.getMessage(), ApiErrorCode.UPC_CODE_UPDATE_FAILED);
        doThrow(exception).when(distributionInfoService)
            .validateAndUpdateDistributionInfo(eq(PRODUCT_CODE), any(DistributionInfoRequest.class), eq(USERNAME));

        // When & Then
        mockMvc.perform(post(BASE_URL + UPDATE_ENDPOINT, PRODUCT_CODE)
                .param("storeId", STORE_ID)
                .param("requestId", REQUEST_ID)
                .param("channelId", CHANNEL_ID)
                .param("clientId", CLIENT_ID)
                .param("username", USERNAME)
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(distributionInfoRequest)))
            .andExpect(status().isOk())
            .andExpect(jsonPath("$.success").value(false))
            .andExpect(jsonPath("$.errorCode").exists())
            .andExpect(jsonPath("$.requestId").value(REQUEST_ID));

        verify(distributionInfoService).validateAndUpdateDistributionInfo(eq(PRODUCT_CODE), any(DistributionInfoRequest.class),
            eq(USERNAME));
        Assertions.assertNotNull(SELLER_CODE);
    }


    @Test
    void testUpdateDistributionInfo_GeneralException_ServiceError() throws Exception {
        // Given
        RuntimeException exception = new RuntimeException("Unexpected service error occurred");
        doThrow(exception).when(distributionInfoService).validateAndUpdateDistributionInfo(eq(PRODUCT_CODE), any(DistributionInfoRequest.class),
            eq(USERNAME));

        // When & Then
        mockMvc.perform(post(BASE_URL + UPDATE_ENDPOINT, PRODUCT_CODE)
                .param("storeId", STORE_ID)
                .param("requestId", REQUEST_ID)
                .param("channelId", CHANNEL_ID)
                .param("clientId", CLIENT_ID)
                .param("username", USERNAME)
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(distributionInfoRequest)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.success").value(false))
                .andExpect(jsonPath("$.errorMessage").value("Unexpected service error occurred"))
                .andExpect(jsonPath("$.errorCode").value("Unexpected service error occurred"))
                .andExpect(jsonPath("$.requestId").value(REQUEST_ID));

        verify(distributionInfoService).validateAndUpdateDistributionInfo(eq(PRODUCT_CODE), any(DistributionInfoRequest.class),
            eq(USERNAME));
    }

    @Test
    void testUpdateDistributionInfo_VerifyResponseStructure() throws Exception {
        doNothing().when(distributionInfoService).validateAndUpdateDistributionInfo(eq(PRODUCT_CODE), any(DistributionInfoRequest.class),
            eq(USERNAME));

        String responseContent = mockMvc.perform(post(BASE_URL + UPDATE_ENDPOINT, PRODUCT_CODE)
                .param("storeId", STORE_ID)
                .param("requestId", REQUEST_ID)
                .param("channelId", CHANNEL_ID)
                .param("clientId", CLIENT_ID)
                .param("username", USERNAME)
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(distributionInfoRequest)))
                .andExpect(status().isOk())
                .andReturn().getResponse().getContentAsString();

        GdnBaseRestResponse response = objectMapper.readValue(responseContent, GdnBaseRestResponse.class);
        assertTrue(response.isSuccess());
        assertNull(response.getErrorMessage());
        assertNull(response.getErrorCode());

        verify(distributionInfoService).validateAndUpdateDistributionInfo(eq(PRODUCT_CODE), any(DistributionInfoRequest.class),
            eq(USERNAME));
    }

    @Test
    void fetchDistributionInfoTest() throws Exception {
        GdnRestListResponse<DistributionInfoPerSkuResponse> distributionInfoPerSkuResponseGdnRestListResponse =
            new GdnRestListResponse<>();
        distributionInfoPerSkuResponseGdnRestListResponse.setContent(
            Collections.singletonList(new DistributionInfoPerSkuResponse()));
        distributionInfoPerSkuResponseGdnRestListResponse.setPageMetaData(new PageMetaData());
        when(distributionInfoService.fetchDistributionInfoByProductCode(STORE_ID, PRODUCT_CODE, 0, 100)).thenReturn(
            distributionInfoPerSkuResponseGdnRestListResponse);

        mockMvc.perform(
            get(BASE_URL + UPDATE_ENDPOINT, PRODUCT_CODE).param("storeId", STORE_ID).param("requestId", REQUEST_ID)
                .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("username", USERNAME)
                .param("productCode", PRODUCT_CODE).param("needDistributionInfoResponse", String.valueOf(false))
                .param("page", String.valueOf(0)).param("size", String.valueOf(100))
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(distributionInfoRequest))).andExpect(status().isOk());

        verify(distributionInfoService).fetchDistributionInfoByProductCode(STORE_ID, PRODUCT_CODE, 0, 100);
        Assertions.assertNotNull(distributionInfoRequest);
    }

}

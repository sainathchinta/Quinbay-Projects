package com.gdn.mta.product.service;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.test.util.ReflectionTestUtils;

import com.gda.mta.product.dto.AuditTrailDto;
import com.gda.mta.product.dto.DimensionAndUomRequest;
import com.gda.mta.product.dto.DistributionInfoRequest;
import com.gda.mta.product.dto.DistributionItemRequest;
import com.gda.mta.product.dto.ProductItemDistributionInfoRequest;
import com.gda.mta.product.dto.UomStockValidationRequest;
import com.gda.mta.product.dto.response.UomStockValidationResponse;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.enums.UomType;
import com.gdn.mta.product.service.config.KafkaPublisher;
import com.gdn.mta.product.service.config.KafkaTopicProperties;
import com.gdn.mta.product.service.exception.ApiDataNotFoundException;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.outbound.inventory.InventoryOutbound;
import com.gdn.partners.pbp.outbound.product.ProductOutbound;
import com.gdn.partners.pbp.outbound.warehouse.OMSOutBound;
import com.gdn.x.productcategorybase.dto.response.DimensionsAndUomResponse;
import com.gdn.x.productcategorybase.dto.response.DistributionInfoPerSkuResponse;
import com.gdn.x.productcategorybase.dto.response.DistributionInfoResponse;
import com.gdn.x.productcategorybase.dto.response.DistributionItemInfoResponse;
import com.gdn.x.productcategorybase.dto.response.ProductL1AndL2CodeResponse;

@ExtendWith(MockitoExtension.class)
class DistributionInfoServiceBeanTest {

    private static final String STORE_ID = "STORE_ID";
    private static final String PRODUCT_CODE = "TEST_PRODUCT_001";
    private static final String SELLER_CODE = "SELLER_001";
    private static final String SKU_CODE = "SKU_001";
    private static final String SKU_CODE_2 = "SKU_002";
    private static final String OMNICHANNEL_SKU = "OMNI_001";
    private static final String OMNICHANNEL_SKU_2 = "OMNI_002";
    private static final String UOM_CODE = "ZC1";
    private static final String UOM_CODE_1 = "CAR";
    private static final String PRODUCT_NAME = "Test Product";
    private static final String PRODUCT_NAME_2 = "Test Product 2";
    private static final String CATEGORY_NAME = "Test Category";
    private static final String ORIGIN = "LOCAL";
    private static final String ORIGIN_2 = "IMPORT";

    @Mock
    private InventoryOutbound inventoryOutbound;

    @Mock
    private ProductOutbound productOutbound;

    @Mock
    private OMSOutBound omsOutBound;

    @Mock
    private KafkaPublisher kafkaProducer;

    @Mock
    private KafkaTopicProperties kafkaTopicProperties;

    @InjectMocks
    private DistributionInfoServiceBean distributionInfoServiceBean;

    private DistributionInfoRequest distributionInfoRequest;
    private List<DistributionInfoPerSkuResponse> existingDistributionInfo;
    private Map<String, ProductL1AndL2CodeResponse> omniChannelSkuToResponseMap;
    private List<ProductItemDistributionInfoRequest> productItemRequests = new ArrayList<>();
    ProductItemDistributionInfoRequest productItemDistributionInfoRequest;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
        setupTestData();
        ReflectionTestUtils.setField(distributionInfoServiceBean, "validateUomInfo", Boolean.TRUE);
        ReflectionTestUtils.setField(distributionInfoServiceBean, "ranchIntegrationEnabled", Boolean.TRUE);
        HashSet<String> sellerCodeList = new HashSet<>();
        sellerCodeList.add(SELLER_CODE);
        ReflectionTestUtils.setField(distributionInfoServiceBean, "distributionSellerList", sellerCodeList);
    }

    private void setupTestData() {
        // Setup DistributionInfoRequest
        Map<String, String> productDistributionInfo = new HashMap<>();
        productDistributionInfo.put("productName", PRODUCT_NAME);
        productDistributionInfo.put("categoryName", CATEGORY_NAME);

        Set<String> upcEanSet = new HashSet<>();
        upcEanSet.add("12345");

        DimensionAndUomRequest dimensionAndUomRequest = DimensionAndUomRequest.builder()
            .uomCode(UOM_CODE)
            .uomType(UomType.Base.name())
            .conversion(1.0)
            .length(10.0)
            .width(5.0)
            .weight(5.0)
            .height(2.0)
            .upcEanList(upcEanSet)
            .build();

        DistributionItemRequest distributionItemRequest = DistributionItemRequest.builder()
            .omniChannelSku(OMNICHANNEL_SKU)
            .origin(ORIGIN)
            .expiry(false)
            .build();

        productItemDistributionInfoRequest=
            ProductItemDistributionInfoRequest.builder()
            .skuCode(SKU_CODE)
            .distributionItemInfoRequest(distributionItemRequest)
            .dimensionsAndUOMRequest(new ArrayList<>(Arrays.asList(dimensionAndUomRequest)))
            .build();

        productItemRequests.add(productItemDistributionInfoRequest);
        
        distributionInfoRequest = DistributionInfoRequest.builder()
            .sellerCode(SELLER_CODE)
            .distributionInfoRequest(productDistributionInfo)
            .productItems(productItemRequests)
            .build();


        // Setup existing distribution info
        DistributionInfoResponse distributionInfoResponse = new DistributionInfoResponse();
        distributionInfoResponse.setProductName(PRODUCT_NAME);
        distributionInfoResponse.setCategoryName(CATEGORY_NAME);

        DistributionItemInfoResponse distributionItemInfoResponse = new DistributionItemInfoResponse();
        distributionItemInfoResponse.setOmniChannelSku(OMNICHANNEL_SKU);
        distributionItemInfoResponse.setOrigin(ORIGIN);
        distributionItemInfoResponse.setExpiry(false);

        DimensionsAndUomResponse dimensionsAndUomResponse = new DimensionsAndUomResponse();
        dimensionsAndUomResponse.setUomCode(UOM_CODE);
        dimensionsAndUomResponse.setConversion(1.0);
        dimensionsAndUomResponse.setLength(10.0);
        dimensionsAndUomResponse.setWidth(5.0);
        dimensionsAndUomResponse.setHeight(2.0);
        dimensionsAndUomResponse.setUpcEanList(Arrays.asList("12345"));

        DistributionInfoPerSkuResponse distributionInfoPerSkuResponse = new DistributionInfoPerSkuResponse();
        distributionInfoPerSkuResponse.setSkuCode(SKU_CODE);
        distributionInfoPerSkuResponse.setDistributionInfoResponse(distributionInfoResponse);
        distributionInfoPerSkuResponse.setDistributionItemInfoResponse(distributionItemInfoResponse);
        distributionInfoPerSkuResponse.setDimensionsAndUomResponse(new ArrayList<>(Arrays.asList(dimensionsAndUomResponse)));

        existingDistributionInfo = new ArrayList<>(Arrays.asList(distributionInfoPerSkuResponse));

        // Setup omniChannelSkuToResponseMap
        ProductL1AndL2CodeResponse productL1AndL2CodeResponse = new ProductL1AndL2CodeResponse();
        productL1AndL2CodeResponse.setSkuCode(SKU_CODE);
        omniChannelSkuToResponseMap = new HashMap<>();
        omniChannelSkuToResponseMap.put(OMNICHANNEL_SKU, productL1AndL2CodeResponse);
    }

    @Test
    public void validateAndUpdateDistributionInfo_successUpdateOnlyProductName() throws Exception {
        existingDistributionInfo.get(0).getDistributionInfoResponse().setProductName(PRODUCT_NAME_2);
        when(productOutbound.getDistributionInfo(eq(PRODUCT_CODE))).thenReturn(
            existingDistributionInfo);
        when(productOutbound.getOmniChannelSkuToItemCode(eq(SELLER_CODE),
            eq(List.of(OMNICHANNEL_SKU)))).thenReturn(omniChannelSkuToResponseMap);
        distributionInfoServiceBean.validateAndUpdateDistributionInfo(PRODUCT_CODE,
            distributionInfoRequest, Constants.DEFAULT_USERNAME);
        verify(productOutbound, times(1)).getDistributionInfo(eq(PRODUCT_CODE));
        verify(productOutbound).getOmniChannelSkuToItemCode(eq(SELLER_CODE),
            eq(List.of(OMNICHANNEL_SKU)));
    }

    @Test
    public void validateAndUpdateDistributionInfo_successUpdateOnlyCategoryName() throws Exception {
        existingDistributionInfo.get(0).getDistributionInfoResponse().setCategoryName(PRODUCT_NAME_2);
        when(productOutbound.getDistributionInfo(eq(PRODUCT_CODE))).thenReturn(
            existingDistributionInfo);
        when(productOutbound.getOmniChannelSkuToItemCode(eq(SELLER_CODE),
            eq(List.of(OMNICHANNEL_SKU)))).thenReturn(omniChannelSkuToResponseMap);
        distributionInfoServiceBean.validateAndUpdateDistributionInfo(PRODUCT_CODE,
            distributionInfoRequest, Constants.DEFAULT_USERNAME);
        verify(productOutbound, times(1)).getDistributionInfo(eq(PRODUCT_CODE));
        verify(productOutbound).getOmniChannelSkuToItemCode(eq(SELLER_CODE),
            eq(List.of(OMNICHANNEL_SKU)));
    }

    @Test
    public void validateAndUpdateDistributionInfo_successUpdateOnlyOrigin() throws Exception {
        existingDistributionInfo.get(0).getDistributionItemInfoResponse().setOrigin(ORIGIN_2);
        when(productOutbound.getDistributionInfo(eq(PRODUCT_CODE))).thenReturn(
            existingDistributionInfo);
        when(productOutbound.getOmniChannelSkuToItemCode(eq(SELLER_CODE),
            eq(List.of(OMNICHANNEL_SKU)))).thenReturn(omniChannelSkuToResponseMap);
        distributionInfoServiceBean.validateAndUpdateDistributionInfo(PRODUCT_CODE,
            distributionInfoRequest, Constants.DEFAULT_USERNAME);
        verify(productOutbound, times(1)).getDistributionInfo(eq(PRODUCT_CODE));
        verify(productOutbound).getOmniChannelSkuToItemCode(eq(SELLER_CODE),
            eq(List.of(OMNICHANNEL_SKU)));
    }

    @Test
    public void validateAndUpdateDistributionInfo_successUpdateOnlyOmniChannelSku() throws Exception {
        distributionInfoRequest.getProductItems().get(0)
            .getDistributionItemInfoRequest().setOmniChannelSku(OMNICHANNEL_SKU_2);
        when(productOutbound.getDistributionInfo(eq(PRODUCT_CODE))).thenReturn(
            existingDistributionInfo);
        when(productOutbound.getOmniChannelSkuToItemCode(eq(SELLER_CODE),
            eq(List.of(OMNICHANNEL_SKU_2)))).thenReturn(omniChannelSkuToResponseMap);
        distributionInfoServiceBean.validateAndUpdateDistributionInfo(PRODUCT_CODE,
            distributionInfoRequest, Constants.DEFAULT_USERNAME);
        verify(productOutbound, times(1)).getDistributionInfo(eq(PRODUCT_CODE));
        verify(productOutbound).getOmniChannelSkuToItemCode(eq(SELLER_CODE),
            eq(List.of(OMNICHANNEL_SKU_2)));
    }

    @Test
    public void validateAndUpdateDistributionInfo_failUpdateDuplicateOmniChannelSku() throws Exception {
        distributionInfoRequest.getProductItems().get(0)
            .getDistributionItemInfoRequest().setOmniChannelSku(OMNICHANNEL_SKU_2);
        ProductL1AndL2CodeResponse productL1AndL2CodeResponse = new ProductL1AndL2CodeResponse();
        productL1AndL2CodeResponse.setSkuCode(SKU_CODE_2);
        omniChannelSkuToResponseMap.put(OMNICHANNEL_SKU_2, productL1AndL2CodeResponse);
        when(productOutbound.getDistributionInfo(eq(PRODUCT_CODE))).thenReturn(
            existingDistributionInfo);
        when(productOutbound.getOmniChannelSkuToItemCode(eq(SELLER_CODE),
            eq(List.of(OMNICHANNEL_SKU_2)))).thenReturn(omniChannelSkuToResponseMap);
        try {
            distributionInfoServiceBean.validateAndUpdateDistributionInfo(PRODUCT_CODE,
                distributionInfoRequest, Constants.DEFAULT_USERNAME);
        } catch (ApiDataNotFoundException e) {
            Assertions.assertNotNull(e.getMessage());
        }
        verify(productOutbound, times(1)).getDistributionInfo(eq(PRODUCT_CODE));
        verify(productOutbound).getOmniChannelSkuToItemCode(eq(SELLER_CODE),
            eq(List.of(OMNICHANNEL_SKU_2)));
    }

    @Test
    public void validateAndUpdateDistributionInfo_failUpdateDuplicateOmniChannelSkuInRequest() throws Exception {
        distributionInfoRequest.getProductItems().get(0)
            .getDistributionItemInfoRequest().setOmniChannelSku(OMNICHANNEL_SKU);
        distributionInfoRequest.getProductItems()
            .add(productItemDistributionInfoRequest);
        when(productOutbound.getDistributionInfo(eq(PRODUCT_CODE))).thenReturn(
            existingDistributionInfo);
        when(productOutbound.getOmniChannelSkuToItemCode(eq(SELLER_CODE),
            eq(List.of(OMNICHANNEL_SKU, OMNICHANNEL_SKU)))).thenReturn(omniChannelSkuToResponseMap);
        try {
            distributionInfoServiceBean.validateAndUpdateDistributionInfo(PRODUCT_CODE,
                distributionInfoRequest, Constants.DEFAULT_USERNAME);
        } catch (ApiDataNotFoundException e) {
            Assertions.assertNotNull(e.getMessage());
        }
        verify(productOutbound, times(1)).getDistributionInfo(eq(PRODUCT_CODE));
        verify(productOutbound).getOmniChannelSkuToItemCode(eq(SELLER_CODE),
            eq(List.of(OMNICHANNEL_SKU, OMNICHANNEL_SKU)));
    }

    @Test
    public void validateAndUpdateDistributionInfo_successUpdateOnlyExpiry() throws Exception {
        distributionInfoRequest.getProductItems().get(0)
            .getDistributionItemInfoRequest().setExpiry(true);
        when(productOutbound.getDistributionInfo(eq(PRODUCT_CODE))).thenReturn(
            existingDistributionInfo);
        when(productOutbound.getOmniChannelSkuToItemCode(eq(SELLER_CODE),
            eq(List.of(OMNICHANNEL_SKU)))).thenReturn(omniChannelSkuToResponseMap);
        distributionInfoServiceBean.validateAndUpdateDistributionInfo(PRODUCT_CODE,
            distributionInfoRequest, Constants.DEFAULT_USERNAME);
        verify(productOutbound, times(1)).getDistributionInfo(eq(PRODUCT_CODE));
        verify(productOutbound).getOmniChannelSkuToItemCode(eq(SELLER_CODE),
            eq(List.of(OMNICHANNEL_SKU)));
    }

    @Test
    public void validateAndUpdateDistributionInfo_successUpdateOnlyWidth() throws Exception {
        existingDistributionInfo.get(0).getDimensionsAndUomResponse().get(0).setWidth(11.0);
        existingDistributionInfo.get(0).getDimensionsAndUomResponse().get(0).setUomType(null);
        when(productOutbound.getDistributionInfo(eq(PRODUCT_CODE))).thenReturn(
            existingDistributionInfo);
        when(productOutbound.getOmniChannelSkuToItemCode(eq(SELLER_CODE),
            eq(List.of(OMNICHANNEL_SKU)))).thenReturn(omniChannelSkuToResponseMap);
        distributionInfoServiceBean.validateAndUpdateDistributionInfo(PRODUCT_CODE,
            distributionInfoRequest, Constants.DEFAULT_USERNAME);
        verify(productOutbound, times(1)).getDistributionInfo(eq(PRODUCT_CODE));
        verify(productOutbound).getOmniChannelSkuToItemCode(eq(SELLER_CODE),
            eq(List.of(OMNICHANNEL_SKU)));
    }

    @Test
    public void validateAndUpdateDistributionInfo_successUpdateOnlyEanUpc() throws Exception {
        existingDistributionInfo.get(0).getDimensionsAndUomResponse().get(0).setUpcEanList(
            Collections.singletonList("12345"));
        existingDistributionInfo.get(0).getDimensionsAndUomResponse().get(0).setUpcEanList(new ArrayList<>());
        when(productOutbound.getDistributionInfo(eq(PRODUCT_CODE))).thenReturn(
            existingDistributionInfo);
        when(productOutbound.getOmniChannelSkuToItemCode(eq(SELLER_CODE),
            eq(List.of(OMNICHANNEL_SKU)))).thenReturn(omniChannelSkuToResponseMap);
        distributionInfoServiceBean.validateAndUpdateDistributionInfo(PRODUCT_CODE,
            distributionInfoRequest, Constants.DEFAULT_USERNAME);
        verify(productOutbound, times(1)).getDistributionInfo(eq(PRODUCT_CODE));
        verify(productOutbound).getOmniChannelSkuToItemCode(eq(SELLER_CODE),
            eq(List.of(OMNICHANNEL_SKU)));
    }

    @Test
    public void validateAndUpdateDistributionInfo_successUpdateOnlyLength() throws Exception {
        existingDistributionInfo.get(0).getDimensionsAndUomResponse().get(0).setLength(11.0);
        when(productOutbound.getDistributionInfo(eq(PRODUCT_CODE))).thenReturn(
            existingDistributionInfo);
        when(productOutbound.getOmniChannelSkuToItemCode(eq(SELLER_CODE),
            eq(List.of(OMNICHANNEL_SKU)))).thenReturn(omniChannelSkuToResponseMap);
        distributionInfoServiceBean.validateAndUpdateDistributionInfo(PRODUCT_CODE,
            distributionInfoRequest, Constants.DEFAULT_USERNAME);
        verify(productOutbound, times(1)).getDistributionInfo(eq(PRODUCT_CODE));
        verify(productOutbound).getOmniChannelSkuToItemCode(eq(SELLER_CODE),
            eq(List.of(OMNICHANNEL_SKU)));
    }

    @Test
    public void validateAndUpdateDistributionInfo_successUpdateOnlyHeight() throws Exception {
        existingDistributionInfo.get(0).getDimensionsAndUomResponse().get(0).setHeight(11.0);
        when(productOutbound.getDistributionInfo(eq(PRODUCT_CODE))).thenReturn(
            existingDistributionInfo);
        when(productOutbound.getOmniChannelSkuToItemCode(eq(SELLER_CODE),
            eq(List.of(OMNICHANNEL_SKU)))).thenReturn(omniChannelSkuToResponseMap);
        distributionInfoServiceBean.validateAndUpdateDistributionInfo(PRODUCT_CODE,
            distributionInfoRequest, Constants.DEFAULT_USERNAME);
        verify(productOutbound, times(1)).getDistributionInfo(eq(PRODUCT_CODE));
        verify(productOutbound).getOmniChannelSkuToItemCode(eq(SELLER_CODE),
            eq(List.of(OMNICHANNEL_SKU)));
    }

    @Test
    public void validateAndUpdateDistributionInfo_successUpdateOnlyUOMCode() throws Exception {
        existingDistributionInfo.get(0).getDimensionsAndUomResponse().get(0).setUomCode(UOM_CODE_1);
        when(productOutbound.getDistributionInfo(eq(PRODUCT_CODE))).thenReturn(
            existingDistributionInfo);
        when(productOutbound.getOmniChannelSkuToItemCode(eq(SELLER_CODE),
            eq(List.of(OMNICHANNEL_SKU)))).thenReturn(omniChannelSkuToResponseMap);
        distributionInfoServiceBean.validateAndUpdateDistributionInfo(PRODUCT_CODE,
            distributionInfoRequest, Constants.DEFAULT_USERNAME);
        verify(productOutbound, times(1)).getDistributionInfo(eq(PRODUCT_CODE));
        verify(productOutbound).getOmniChannelSkuToItemCode(eq(SELLER_CODE),
            eq(List.of(OMNICHANNEL_SKU)));
    }

    @Test
    public void validateAndUpdateDistributionInfo_successUpdateOnlyConversion() throws Exception {
        existingDistributionInfo.get(0).getDimensionsAndUomResponse().get(0).setConversion(2.0);
        UomStockValidationRequest uomStockValidationRequest =
            UomStockValidationRequest.builder().itemCodes(List.of(SKU_CODE)).build();
        when(productOutbound.getDistributionInfo(eq(PRODUCT_CODE))).thenReturn(
            existingDistributionInfo);
        when(productOutbound.getOmniChannelSkuToItemCode(eq(SELLER_CODE),
            eq(List.of(OMNICHANNEL_SKU)))).thenReturn(omniChannelSkuToResponseMap);
        when(inventoryOutbound.isWarehouseStockPresent(any(), eq(SKU_CODE))).thenReturn(Boolean.FALSE);
        when(omsOutBound.validateUomEditable(eq(uomStockValidationRequest))).thenReturn(List.of(
            UomStockValidationResponse.builder().hasExistingStock(false).hasPendingDocuments(false)
                .success(true).build()));
        distributionInfoServiceBean.validateAndUpdateDistributionInfo(PRODUCT_CODE,
            distributionInfoRequest, Constants.DEFAULT_USERNAME);
        verify(productOutbound, times(1)).getDistributionInfo(eq(PRODUCT_CODE));
        verify(productOutbound).getOmniChannelSkuToItemCode(eq(SELLER_CODE),
            eq(List.of(OMNICHANNEL_SKU)));
        verify(inventoryOutbound).isWarehouseStockPresent(any(), eq(SKU_CODE));
    }

    @Test
    public void validateAndUpdateDistributionInfo_failUpdateConversionWithInventoryStock() throws Exception {
        existingDistributionInfo.get(0).getDimensionsAndUomResponse().get(0).setConversion(2.0);
        when(productOutbound.getDistributionInfo(eq(PRODUCT_CODE))).thenReturn(
            existingDistributionInfo);
        when(productOutbound.getOmniChannelSkuToItemCode(eq(SELLER_CODE),
            eq(List.of(OMNICHANNEL_SKU)))).thenReturn(omniChannelSkuToResponseMap);
        when(inventoryOutbound.isWarehouseStockPresent(any(), eq(SKU_CODE))).thenReturn(Boolean.TRUE);
        try {
            distributionInfoServiceBean.validateAndUpdateDistributionInfo(PRODUCT_CODE,
                distributionInfoRequest, Constants.DEFAULT_USERNAME);
        } catch (ApiDataNotFoundException ex){
            Assertions.assertNotNull(ex);
        }
        verify(productOutbound, times(1)).getDistributionInfo(eq(PRODUCT_CODE));
        verify(productOutbound).getOmniChannelSkuToItemCode(eq(SELLER_CODE),
            eq(List.of(OMNICHANNEL_SKU)));
        verify(inventoryOutbound).isWarehouseStockPresent(any(), eq(SKU_CODE));
    }

    @Test
    public void validateAndUpdateDistributionInfo_failUpdateConversionWithOMSHasExistingStockTrue() throws Exception {
        existingDistributionInfo.get(0).getDimensionsAndUomResponse().get(0).setConversion(2.0);
        UomStockValidationRequest uomStockValidationRequest =
            UomStockValidationRequest.builder().itemCodes(List.of(SKU_CODE)).build();

        when(productOutbound.getDistributionInfo(eq(PRODUCT_CODE))).thenReturn(
            existingDistributionInfo);
        when(productOutbound.getOmniChannelSkuToItemCode(eq(SELLER_CODE),
            eq(List.of(OMNICHANNEL_SKU)))).thenReturn(omniChannelSkuToResponseMap);
        when(inventoryOutbound.isWarehouseStockPresent(any(), eq(SKU_CODE))).thenReturn(
            Boolean.FALSE);
        when(omsOutBound.validateUomEditable(eq(uomStockValidationRequest))).thenReturn(List.of(
            UomStockValidationResponse.builder().itemCode(SKU_CODE).hasExistingStock(true)
                .build()));
        try {
            distributionInfoServiceBean.validateAndUpdateDistributionInfo(PRODUCT_CODE,
                distributionInfoRequest, Constants.DEFAULT_USERNAME);
        } catch (ApiDataNotFoundException ex){
            Assertions.assertNotNull(ex);
        }
        verify(productOutbound, times(1)).getDistributionInfo(eq(PRODUCT_CODE));
        verify(productOutbound).getOmniChannelSkuToItemCode(eq(SELLER_CODE),
            eq(List.of(OMNICHANNEL_SKU)));
        verify(inventoryOutbound).isWarehouseStockPresent(any(), eq(SKU_CODE));
        verify(omsOutBound).validateUomEditable(eq(uomStockValidationRequest));
    }

    @Test
    public void validateAndUpdateDistributionInfo_failUpdateConversionWithOMSHasPendingDocumentTrue() throws Exception {
        existingDistributionInfo.get(0).getDimensionsAndUomResponse().get(0).setConversion(2.0);
        UomStockValidationRequest uomStockValidationRequest =
            UomStockValidationRequest.builder().itemCodes(List.of(SKU_CODE)).build();

        when(productOutbound.getDistributionInfo(eq(PRODUCT_CODE))).thenReturn(
            existingDistributionInfo);
        when(productOutbound.getOmniChannelSkuToItemCode(eq(SELLER_CODE),
            eq(List.of(OMNICHANNEL_SKU)))).thenReturn(omniChannelSkuToResponseMap);
        when(inventoryOutbound.isWarehouseStockPresent(any(), eq(SKU_CODE))).thenReturn(
            Boolean.FALSE);
        when(omsOutBound.validateUomEditable(eq(uomStockValidationRequest))).thenReturn(List.of(
            UomStockValidationResponse.builder().itemCode(SKU_CODE).hasPendingDocuments(true)
                .build()));
        try {
            distributionInfoServiceBean.validateAndUpdateDistributionInfo(PRODUCT_CODE,
                distributionInfoRequest, Constants.DEFAULT_USERNAME);
        } catch (ApiDataNotFoundException ex){
            Assertions.assertNotNull(ex);
        }
        verify(productOutbound, times(1)).getDistributionInfo(eq(PRODUCT_CODE));
        verify(productOutbound).getOmniChannelSkuToItemCode(eq(SELLER_CODE),
            eq(List.of(OMNICHANNEL_SKU)));
        verify(inventoryOutbound).isWarehouseStockPresent(any(), eq(SKU_CODE));
        verify(omsOutBound).validateUomEditable(eq(uomStockValidationRequest));
    }

    @Test
    public void validateAndUpdateDistributionInfo_failUpdateConversionWithOMSHasSuccessFalse() throws Exception {
        existingDistributionInfo.get(0).getDimensionsAndUomResponse().get(0).setConversion(2.0);
        UomStockValidationRequest uomStockValidationRequest =
            UomStockValidationRequest.builder().itemCodes(List.of(SKU_CODE)).build();

        when(productOutbound.getDistributionInfo(eq(PRODUCT_CODE))).thenReturn(
            existingDistributionInfo);
        when(productOutbound.getOmniChannelSkuToItemCode(eq(SELLER_CODE),
            eq(List.of(OMNICHANNEL_SKU)))).thenReturn(omniChannelSkuToResponseMap);
        when(inventoryOutbound.isWarehouseStockPresent(any(), eq(SKU_CODE))).thenReturn(
            Boolean.FALSE);
        when(omsOutBound.validateUomEditable(eq(uomStockValidationRequest))).thenReturn(List.of(
            UomStockValidationResponse.builder().itemCode(SKU_CODE).success(false)
                .build()));
        try {
            distributionInfoServiceBean.validateAndUpdateDistributionInfo(PRODUCT_CODE,
                distributionInfoRequest, Constants.DEFAULT_USERNAME);
        } catch (ApiDataNotFoundException ex){
            Assertions.assertNotNull(ex);
        }
        verify(productOutbound, times(1)).getDistributionInfo(eq(PRODUCT_CODE));
        verify(productOutbound).getOmniChannelSkuToItemCode(eq(SELLER_CODE),
            eq(List.of(OMNICHANNEL_SKU)));
        verify(inventoryOutbound).isWarehouseStockPresent(any(), eq(SKU_CODE));
        verify(omsOutBound).validateUomEditable(eq(uomStockValidationRequest));
    }

    @Test
    public void validateAndUpdateDistributionInfo_noUpdate() throws Exception {
        when(productOutbound.getDistributionInfo(eq(PRODUCT_CODE))).thenReturn(
            existingDistributionInfo);
        distributionInfoServiceBean.validateAndUpdateDistributionInfo(PRODUCT_CODE,
                distributionInfoRequest, Constants.DEFAULT_USERNAME);
        verify(productOutbound, times(1)).getDistributionInfo(eq(PRODUCT_CODE));
    }

    @Test
    public void validateAndUpdateDistributionInfoExistingSkuCodeDifferent() throws Exception {
        existingDistributionInfo.get(0).setSkuCode(PRODUCT_NAME);
        when(productOutbound.getDistributionInfo(eq(PRODUCT_CODE))).thenReturn(
            existingDistributionInfo);
        distributionInfoServiceBean.validateAndUpdateDistributionInfo(PRODUCT_CODE,
            distributionInfoRequest, Constants.DEFAULT_USERNAME);
        verify(productOutbound, times(1)).getDistributionInfo(eq(PRODUCT_CODE));
    }

    @Test
    public void validateAndUpdateDistributionInfo_addNewUOM() throws Exception {
        distributionInfoRequest.getProductItems().get(0)
            .getDimensionsAndUOMRequest().add(DimensionAndUomRequest.builder().uomCode(UOM_CODE_1)
                .uomType(UomType.Alternate.toString()).length(10.0).width(10.0).height(10.0).weight(10.0).upcEanList(Set.of("12345678")).build());
        existingDistributionInfo.get(0).getDimensionsAndUomResponse().get(0).setUomType(UomType.Base.name());
        when(productOutbound.getDistributionInfo(eq(PRODUCT_CODE))).thenReturn(
            existingDistributionInfo);
        when(productOutbound.getOmniChannelSkuToItemCode(eq(SELLER_CODE),
            eq(List.of(OMNICHANNEL_SKU)))).thenReturn(omniChannelSkuToResponseMap);
        distributionInfoServiceBean.validateAndUpdateDistributionInfo(PRODUCT_CODE,
            distributionInfoRequest, Constants.DEFAULT_USERNAME);
        verify(productOutbound, times(1)).getDistributionInfo(eq(PRODUCT_CODE));
        verify(productOutbound).getOmniChannelSkuToItemCode(eq(SELLER_CODE),
            eq(List.of(OMNICHANNEL_SKU)));
    }

    @Test
    public void validateAndUpdateDistributionInfoInValidUom() throws Exception {
        distributionInfoRequest.getProductItems().get(0).getDimensionsAndUOMRequest().add(
            DimensionAndUomRequest.builder().uomCode("Adarsh").uomType(UomType.Alternate.toString()).length(10.0)
                .width(10.0).height(10.0).weight(10.0).upcEanList(Set.of("12345678")).build());
        existingDistributionInfo.get(0).getDimensionsAndUomResponse().get(0).setUomType(UomType.Base.name());
        when(productOutbound.getDistributionInfo(eq(PRODUCT_CODE))).thenReturn(existingDistributionInfo);
        when(productOutbound.getOmniChannelSkuToItemCode(eq(SELLER_CODE), eq(List.of(OMNICHANNEL_SKU)))).thenReturn(
            omniChannelSkuToResponseMap);
        try {
            distributionInfoServiceBean.validateAndUpdateDistributionInfo(PRODUCT_CODE, distributionInfoRequest,
                Constants.DEFAULT_USERNAME);
        } catch (ApplicationRuntimeException runtimeException) {
            Assertions.assertTrue(
                runtimeException.getErrorMessage().contains("Invalid uomCode, allowed values are [ZC1, EA, CAR]"));
        }
    }

    @Test
    public void validateAndUpdateDistributionInfoNoExistingInfo() throws Exception {
        when(productOutbound.getDistributionInfo(eq(PRODUCT_CODE))).thenReturn(
            new ArrayList<>());
        when(productOutbound.getOmniChannelSkuToItemCode(eq(SELLER_CODE),
            eq(List.of(OMNICHANNEL_SKU)))).thenReturn(omniChannelSkuToResponseMap);
        distributionInfoServiceBean.validateAndUpdateDistributionInfo(PRODUCT_CODE,
            distributionInfoRequest, Constants.DEFAULT_USERNAME);
        verify(productOutbound, times(1)).getDistributionInfo(eq(PRODUCT_CODE));
        verify(productOutbound).getOmniChannelSkuToItemCode(eq(SELLER_CODE),
            eq(List.of(OMNICHANNEL_SKU)));
    }

    @Test
    public void validateAndUpdateDistributionInfoBaseUomCannotBeChanged() {
        distributionInfoRequest.getProductItems().get(0)
            .getDimensionsAndUOMRequest().add(DimensionAndUomRequest.builder().uomCode(UOM_CODE_1)
                .uomType(UomType.Base.toString()).length(10.0).width(10.0).height(10.0).weight(10.0).upcEanList(Set.of("82345671")).build());
        existingDistributionInfo.get(0).getDimensionsAndUomResponse().get(0).setUomType(UomType.Base.name());
        when(productOutbound.getDistributionInfo(eq(PRODUCT_CODE))).thenReturn(
            existingDistributionInfo);
        Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
            distributionInfoServiceBean.validateAndUpdateDistributionInfo(PRODUCT_CODE, distributionInfoRequest,
                Constants.DEFAULT_USERNAME);
        });
        verify(productOutbound, times(1)).getDistributionInfo(eq(PRODUCT_CODE));
    }

    @Test
    public void validateAndUpdateResponseNull() {
        distributionInfoRequest.getProductItems().get(0).getDimensionsAndUOMRequest().add(
            DimensionAndUomRequest.builder().uomCode(UOM_CODE_1).uomType(UomType.Base.toString()).length(10.0)
                .width(10.0).height(10.0).weight(10.0).upcEanList(Set.of("82345671")).build());
        existingDistributionInfo.get(0).getDimensionsAndUomResponse().get(0).setUomType(UomType.Base.name());
        existingDistributionInfo.get(0).setDistributionInfoResponse(null);
        when(productOutbound.getDistributionInfo(eq(PRODUCT_CODE))).thenReturn(existingDistributionInfo);
        Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
            distributionInfoServiceBean.validateAndUpdateDistributionInfo(PRODUCT_CODE, distributionInfoRequest,
                Constants.DEFAULT_USERNAME);
        });
        verify(productOutbound, times(1)).getDistributionInfo(eq(PRODUCT_CODE));
    }

    @Test
    public void validateAndUpdateDistributionInfoDuplicateUOM() {
        distributionInfoRequest.getProductItems().get(0)
            .getDimensionsAndUOMRequest().add(DimensionAndUomRequest.builder().uomCode(UOM_CODE_1)
                .uomType(UomType.Base.toString()).length(10.0).width(10.0).height(10.0).weight(10.0).upcEanList(Set.of("12345678")).build());
        distributionInfoRequest.getProductItems().get(0)
            .getDimensionsAndUOMRequest().add(DimensionAndUomRequest.builder().uomCode(UOM_CODE_1)
                .uomType(UomType.Base.toString()).length(10.0).width(10.0).height(10.0).weight(10.0).upcEanList(Set.of("12345678")).build());
        when(productOutbound.getDistributionInfo(eq(PRODUCT_CODE))).thenReturn(
            existingDistributionInfo);
        Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
            distributionInfoServiceBean.validateAndUpdateDistributionInfo(PRODUCT_CODE, distributionInfoRequest,
                Constants.DEFAULT_USERNAME);
        });
        verify(productOutbound, times(1)).getDistributionInfo(eq(PRODUCT_CODE));
    }

    @Test
    public void validateAndUpdateDistributionInfoInValidWeight() {
        distributionInfoRequest.getProductItems().get(0)
            .getDimensionsAndUOMRequest().add(DimensionAndUomRequest.builder().uomCode(UOM_CODE_1)
                .uomType(UomType.Alternate.toString()).length(10.0).width(10.0).height(10.0).weight(10.0).upcEanList(Set.of("12345678")).build());
        distributionInfoRequest.getProductItems().get(0).getDimensionsAndUOMRequest().get(0).setWeight(0d);
        when(productOutbound.getDistributionInfo(eq(PRODUCT_CODE))).thenReturn(
            existingDistributionInfo);
        Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
            distributionInfoServiceBean.validateAndUpdateDistributionInfo(PRODUCT_CODE, distributionInfoRequest,
                Constants.DEFAULT_USERNAME);
        });
        verify(productOutbound, times(1)).getDistributionInfo(eq(PRODUCT_CODE));
    }

    @Test
    public void validateAndUpdateDistributionInfoInValidHeight() {
        distributionInfoRequest.getProductItems().get(0)
            .getDimensionsAndUOMRequest().add(DimensionAndUomRequest.builder().uomCode(UOM_CODE_1)
                .uomType(UomType.Alternate.toString()).length(10.0).width(10.0).height(10.0).weight(10.0).upcEanList(Set.of("12345678")).build());
        distributionInfoRequest.getProductItems().get(0).getDimensionsAndUOMRequest().get(0).setHeight(0d);
        when(productOutbound.getDistributionInfo(eq(PRODUCT_CODE))).thenReturn(
            existingDistributionInfo);
        Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
            distributionInfoServiceBean.validateAndUpdateDistributionInfo(PRODUCT_CODE, distributionInfoRequest,
                Constants.DEFAULT_USERNAME);
        });
        verify(productOutbound, times(1)).getDistributionInfo(eq(PRODUCT_CODE));
    }

    @Test
    public void validateAndUpdateDistributionInfoInValidWidth() {
        distributionInfoRequest.getProductItems().get(0)
            .getDimensionsAndUOMRequest().add(DimensionAndUomRequest.builder().uomCode(UOM_CODE_1)
                .uomType(UomType.Alternate.toString()).length(10.0).width(10.0).height(10.0).weight(10.0).upcEanList(Set.of("12345678")).build());
        existingDistributionInfo.get(0).getDimensionsAndUomResponse()
            .add(existingDistributionInfo.get(0).getDimensionsAndUomResponse().get(0));
        distributionInfoRequest.getProductItems().get(0).getDimensionsAndUOMRequest().get(0).setWidth(0d);
        when(productOutbound.getDistributionInfo(eq(PRODUCT_CODE))).thenReturn(
            existingDistributionInfo);
        Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
            distributionInfoServiceBean.validateAndUpdateDistributionInfo(PRODUCT_CODE, distributionInfoRequest,
                Constants.DEFAULT_USERNAME);
        });
        verify(productOutbound, times(1)).getDistributionInfo(eq(PRODUCT_CODE));
    }

    @Test
    public void validateAndUpdateDistributionInfoInValid̉Length() {
        distributionInfoRequest.getProductItems().get(0)
            .getDimensionsAndUOMRequest().add(DimensionAndUomRequest.builder().uomCode(UOM_CODE_1)
                .uomType(UomType.Alternate.toString()).length(10.0).width(10.0).height(10.0).weight(10.0).upcEanList(Set.of("12345678")).build());
        distributionInfoRequest.getProductItems().get(0).getDimensionsAndUOMRequest().get(0).setLength(0d);
        when(productOutbound.getDistributionInfo(eq(PRODUCT_CODE))).thenReturn(
            existingDistributionInfo);
        Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
            distributionInfoServiceBean.validateAndUpdateDistributionInfo(PRODUCT_CODE, distributionInfoRequest,
                Constants.DEFAULT_USERNAME);
        });
        verify(productOutbound, times(1)).getDistributionInfo(eq(PRODUCT_CODE));
    }

    @Test
    public void validateAndUpdateDistributionInfoValidationOff() throws Exception {
        ReflectionTestUtils.setField(distributionInfoServiceBean, "validateUomInfo", Boolean.FALSE);
        distributionInfoServiceBean.validateAndUpdateDistributionInfo(PRODUCT_CODE,
            distributionInfoRequest, Constants.DEFAULT_USERNAME);
        Assertions.assertNotNull(distributionInfoRequest);
    }

    @Test
    public void fetchDistributionInfoByProductCodeTest() {
        distributionInfoServiceBean.fetchDistributionInfoByProductCode(STORE_ID, PRODUCT_CODE, 0 , 10);
        Mockito.verify(productOutbound).getDistributionInfoPerSkuResponse(PRODUCT_CODE, 0 , 10);
    }

    @Test
    public void publishProductLevelHistoryToPcbForDistributionUpdateTest() {
        distributionInfoServiceBean.publishProductLevelHistoryToPcbForDistributionUpdate(PRODUCT_CODE, SELLER_CODE,
            new ArrayList<>(), Constants.DEFAULT_USERNAME);
        Mockito.when(kafkaTopicProperties.getPopulateL3HistoryByProductCodeEvent())
            .thenReturn(Constants.SEND_EVENT_FOR_DISTRIBUTION_HISTORY);
        distributionInfoServiceBean.publishProductLevelHistoryToPcbForDistributionUpdate(PRODUCT_CODE, SELLER_CODE,
            Collections.singletonList(new AuditTrailDto()), Constants.DEFAULT_USERNAME);
        Mockito.verify(kafkaProducer)
            .send(eq(Constants.SEND_EVENT_FOR_DISTRIBUTION_HISTORY), eq(PRODUCT_CODE), Mockito.any());
        Mockito.verify(kafkaTopicProperties)
            .getPopulateL3HistoryByProductCodeEvent();
    }

}
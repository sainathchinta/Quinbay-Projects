package com.gdn.x.productcategorybase.service.impl;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.dto.request.DimensionAndUomDTO;
import com.gdn.x.productcategorybase.dto.request.DistributionInfoUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.DistributionItemInfoRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemUomInfoDTO;
import com.gdn.x.productcategorybase.dto.response.DistributionInfoPerSkuResponse;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductItem;
import com.gdn.x.productcategorybase.entity.ProductItemUomInfo;
import com.gdn.x.productcategorybase.entity.Origin;
import com.gdn.x.productcategorybase.repository.ProductItemUomInfoRepository;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.productcategorybase.config.MandatoryParameterHelper;

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.*;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class DistributionInfoServiceBeanTest {

  @Mock
  private ProductItemUomInfoRepository productItemUomInfoRepository;

  @Mock(lenient = true)
  private ObjectMapper objectMapper;

  @Mock
  private MandatoryParameterHelper mandatoryParameterHelper;

  @InjectMocks
  private DistributionInfoServiceBean distributionInfoServiceBean;

  private static final String STORE_ID = "store123";
  private static final String PRODUCT_CODE = "PROD001";
  private static final String SELLER_CODE = "SELLER001";
  private static final String SKU_CODE_1 = "SKU001";
  private static final String SKU_CODE_2 = "SKU002";
  private static final String PRODUCT_ITEM_ID_1 = "ITEM001";
  private static final String PRODUCT_ITEM_ID_2 = "ITEM002";
  private static final String OMNI_CHANNEL_SKU_1 = "OMNI001";
  private static final String OMNI_CHANNEL_SKU_2 = "OMNI002";

  private Product product;
  private ProductItem productItem1;
  private ProductItem productItem2;
  private ProductItemUomInfo existingProductItemUomInfo;
  private DistributionInfoUpdateRequest validRequest;
  private List<DimensionAndUomDTO> dimensionAndUomDTOS;

  @BeforeEach
  void setUp() throws JsonProcessingException {
    // Setup Product and ProductItems
    productItem1 = new ProductItem();
    productItem1.setId(PRODUCT_ITEM_ID_1);
    productItem1.setSkuCode(SKU_CODE_1);

    productItem2 = new ProductItem();
    productItem2.setId(PRODUCT_ITEM_ID_2);
    productItem2.setSkuCode(SKU_CODE_2);

    product = new Product.Builder().productCode(PRODUCT_CODE)
        .productItems(Arrays.asList(productItem1, productItem2)).build();

    // Setup existing ProductItemUomInfo
    existingProductItemUomInfo =
        ProductItemUomInfo.builder().productCode(PRODUCT_CODE).productItem(productItem1)
            .skuCode(SKU_CODE_1).sellerCode(SELLER_CODE).origin(Origin.LOCAL)
            .uom("old_uom_json").expiry(false).build();
    productItem1.setProductItemUomInfo(existingProductItemUomInfo);
    existingProductItemUomInfo.setId("UOM001");
    existingProductItemUomInfo.setStoreId(STORE_ID);

    // Setup DimensionAndUOMDTO
    dimensionAndUomDTOS = Arrays.asList(
        DimensionAndUomDTO.builder().uomCode("CM").uomType("LENGTH").length(10.0).width(5.0)
            .height(2.0).conversion(1.0).build());

    // Setup valid request
    DistributionItemInfoRequest distributionItemInfoRequest1 =
        DistributionItemInfoRequest.builder().omniChannelSku(OMNI_CHANNEL_SKU_1).origin("LOCAL")
            .expiry(false).build();

    DistributionItemInfoRequest distributionItemInfoRequest2 =
        DistributionItemInfoRequest.builder().omniChannelSku(OMNI_CHANNEL_SKU_2).origin("IMPORT")
            .expiry(true).build();

    ProductItemUomInfoDTO productItemUomInfoDTO1 =
        ProductItemUomInfoDTO.builder().skuCode(SKU_CODE_1)
            .distributionItemInfoRequest(distributionItemInfoRequest1)
            .dimensionAndUomDTOList(dimensionAndUomDTOS).build();

    ProductItemUomInfoDTO productItemUomInfoDTO2 =
        ProductItemUomInfoDTO.builder().skuCode(SKU_CODE_2)
            .distributionItemInfoRequest(distributionItemInfoRequest2)
            .dimensionAndUomDTOList(dimensionAndUomDTOS).build();

    validRequest = DistributionInfoUpdateRequest.builder().sellerCode(SELLER_CODE)
        .productItemUomInfoDTOS(Arrays.asList(productItemUomInfoDTO1, productItemUomInfoDTO2))
        .build();

        // Setup ObjectMapper mock
    when(objectMapper.writeValueAsString(any())).thenReturn("{\"test\":\"json\"}");

    ReflectionTestUtils.setField(distributionInfoServiceBean, "sellerApiClientId", "mta-api");
  }

  @Test
  void testUpdateDistributionInfo_Success_UpdateExistingAndCreateNew() throws Exception {
    productItem1.setOmniChannelSku(OMNI_CHANNEL_SKU_1);
    productItem1.setSkuCode(SKU_CODE_1);
    productItem2.setOmniChannelSku(OMNI_CHANNEL_SKU_2);
    productItem2.setSkuCode(SKU_CODE_2);
    product.setProductItems(Arrays.asList(productItem1, productItem2));
    when(productItemUomInfoRepository.save(any(ProductItemUomInfo.class))).thenReturn(
        existingProductItemUomInfo);
    distributionInfoServiceBean.updateDistributionInfo(StringUtils.EMPTY, PRODUCT_CODE, validRequest, product);
    verify(productItemUomInfoRepository, times(2)).save(any(ProductItemUomInfo.class));
    verify(objectMapper, times(2)).writeValueAsString(dimensionAndUomDTOS);
  }

  @Test
  void testUpdateDistributionInfo_Success_AllNewRecords() throws Exception {
    when(productItemUomInfoRepository.save(any(ProductItemUomInfo.class))).thenReturn(
        existingProductItemUomInfo);
    distributionInfoServiceBean.updateDistributionInfo(StringUtils.EMPTY, PRODUCT_CODE, validRequest, product);
    verify(productItemUomInfoRepository, times(2)).save(any(ProductItemUomInfo.class));
  }

  @Test
  void testUpdateDistributionInfo_ValidationFailure_BlankProductCode() {
    // Given
    String blankProductCode = "";

    // When & Then
    ApplicationRuntimeException exception = assertThrows(ApplicationRuntimeException.class,
        () -> distributionInfoServiceBean.updateDistributionInfo(StringUtils.EMPTY, blankProductCode, validRequest,
            product));
    assertEquals("Can not process invalid input data :"
        + ErrorMessage.PRODUCT_CODE_MUST_NOT_BE_BLANK.getMessage(), exception.getMessage());
  }

  @Test
  void testUpdateDistributionInfo_ValidationFailure_NullProductCode() {
    // Given
    String nullProductCode = null;

    // When & Then
    ApplicationRuntimeException exception = assertThrows(ApplicationRuntimeException.class,
        () -> distributionInfoServiceBean.updateDistributionInfo(StringUtils.EMPTY, nullProductCode, validRequest,
            product));
    assertEquals("Can not process invalid input data :"
        + ErrorMessage.PRODUCT_CODE_MUST_NOT_BE_BLANK.getMessage(), exception.getMessage());
  }

  @Test
  void testUpdateDistributionInfo_ValidationFailure_BlankSellerCode() {
    // Given
    DistributionInfoUpdateRequest requestWithBlankSellerCode =
        DistributionInfoUpdateRequest.builder().sellerCode("")
            .productItemUomInfoDTOS(validRequest.getProductItemUomInfoDTOS()).build();

    // When & Then
    ApplicationRuntimeException exception = assertThrows(ApplicationRuntimeException.class,
        () -> distributionInfoServiceBean.updateDistributionInfo(StringUtils.EMPTY, PRODUCT_CODE,
            requestWithBlankSellerCode, product));
    assertEquals("Can not process invalid input data :"
        + ErrorMessage.SELLER_CODE_MUST_NOT_BE_BLANK.getMessage(), exception.getMessage());
  }

  @Test
  void testUpdateDistributionInfo_ValidationFailure_NullSellerCode() {
    // Given
    DistributionInfoUpdateRequest requestWithNullSellerCode =
        DistributionInfoUpdateRequest.builder().sellerCode(null)
            .productItemUomInfoDTOS(validRequest.getProductItemUomInfoDTOS()).build();

    // When & Then
    ApplicationRuntimeException exception = assertThrows(ApplicationRuntimeException.class,
        () -> distributionInfoServiceBean.updateDistributionInfo(StringUtils.EMPTY, PRODUCT_CODE,
            requestWithNullSellerCode, product));
    assertEquals("Can not process invalid input data :"
        + ErrorMessage.SELLER_CODE_MUST_NOT_BE_BLANK.getMessage(), exception.getMessage());
  }

  @Test
  void testUpdateDistributionInfo_ValidationFailure_EmptyProductItemUomInfoDTOS() {
    // Given
    DistributionInfoUpdateRequest requestWithEmptyList =
        DistributionInfoUpdateRequest.builder().sellerCode(SELLER_CODE)
            .productItemUomInfoDTOS(Collections.emptyList()).build();

    // When & Then
    ApplicationRuntimeException exception = assertThrows(ApplicationRuntimeException.class,
        () -> distributionInfoServiceBean.updateDistributionInfo(StringUtils.EMPTY, PRODUCT_CODE, requestWithEmptyList,
            product));
    assertEquals("Can not process invalid input data :"
            + ErrorMessage.PRODUCT_ITEM_UOM_INFO_REQUESTS_MUST_NOT_BE_EMPTY.getMessage(),
        exception.getMessage());
  }

  @Test
  void testUpdateDistributionInfo_ValidationFailure_NullProductItemUomInfoDTOS() {
    // Given
    DistributionInfoUpdateRequest requestWithNullList =
        DistributionInfoUpdateRequest.builder().sellerCode(SELLER_CODE).productItemUomInfoDTOS(null)
            .build();

    // When & Then
    ApplicationRuntimeException exception = assertThrows(ApplicationRuntimeException.class,
        () -> distributionInfoServiceBean.updateDistributionInfo(StringUtils.EMPTY, PRODUCT_CODE, requestWithNullList,
            product));
    assertEquals("Can not process invalid input data :"
            + ErrorMessage.PRODUCT_ITEM_UOM_INFO_REQUESTS_MUST_NOT_BE_EMPTY.getMessage(),
        exception.getMessage());
  }

  @Test
  void testUpdateDistributionInfo_ValidationFailure_BlankSkuCode() {
    // Given
    ProductItemUomInfoDTO productItemUomInfoDTOWithBlankSku =
        ProductItemUomInfoDTO.builder().skuCode("").distributionItemInfoRequest(
                validRequest.getProductItemUomInfoDTOS().get(0).getDistributionItemInfoRequest())
            .dimensionAndUomDTOList(dimensionAndUomDTOS).build();

    DistributionInfoUpdateRequest requestWithBlankSku =
        DistributionInfoUpdateRequest.builder().sellerCode(SELLER_CODE)
            .productItemUomInfoDTOS(Arrays.asList(productItemUomInfoDTOWithBlankSku)).build();

    // When & Then
    ApplicationRuntimeException exception = assertThrows(ApplicationRuntimeException.class,
        () -> distributionInfoServiceBean.updateDistributionInfo(StringUtils.EMPTY, PRODUCT_CODE, requestWithBlankSku,
            product));
    assertEquals("Can not process invalid input data :"
        + ErrorMessage.ITEM_CODE_MUST_NOT_BE_BLANK.getMessage(), exception.getMessage());
  }

  @Test
  void testUpdateDistributionInfo_ValidationFailure_NullSkuCode() {
    // Given
    ProductItemUomInfoDTO productItemUomInfoDTOWithNullSku =
        ProductItemUomInfoDTO.builder().skuCode(null).distributionItemInfoRequest(
                validRequest.getProductItemUomInfoDTOS().get(0).getDistributionItemInfoRequest())
            .dimensionAndUomDTOList(dimensionAndUomDTOS).build();

    DistributionInfoUpdateRequest requestWithNullSku =
        DistributionInfoUpdateRequest.builder().sellerCode(SELLER_CODE)
            .productItemUomInfoDTOS(Arrays.asList(productItemUomInfoDTOWithNullSku)).build();

    // When & Then
    ApplicationRuntimeException exception = assertThrows(ApplicationRuntimeException.class,
        () -> distributionInfoServiceBean.updateDistributionInfo(StringUtils.EMPTY, PRODUCT_CODE, requestWithNullSku,
            product));
    assertEquals("Can not process invalid input data :"
        + ErrorMessage.ITEM_CODE_MUST_NOT_BE_BLANK.getMessage(), exception.getMessage());
  }

  @Test
  void testUpdateDistributionInfo_ValidationFailure_EmptyDimensionAndUOMDTOS() {
    // Given
    ProductItemUomInfoDTO productItemUomInfoDTOWithEmptyDimensions =
        ProductItemUomInfoDTO.builder().skuCode(SKU_CODE_1).distributionItemInfoRequest(
                validRequest.getProductItemUomInfoDTOS().get(0).getDistributionItemInfoRequest())
            .dimensionAndUomDTOList(Collections.emptyList()).build();

    DistributionInfoUpdateRequest requestWithEmptyDimensions =
        DistributionInfoUpdateRequest.builder().sellerCode(SELLER_CODE)
            .productItemUomInfoDTOS(Arrays.asList(productItemUomInfoDTOWithEmptyDimensions))
            .build();

    // When & Then
    ApplicationRuntimeException exception = assertThrows(ApplicationRuntimeException.class,
        () -> distributionInfoServiceBean.updateDistributionInfo(StringUtils.EMPTY, PRODUCT_CODE,
            requestWithEmptyDimensions, product));
    assertEquals("Can not process invalid input data :"
            + ErrorMessage.DIMENSION_AND_UOM_UPDATE_REQUESTS_MUST_NOT_BE_EMPTY.getMessage(),
        exception.getMessage());
  }

  @Test
  void testUpdateDistributionInfo_ValidationFailure_NullDimensionAndUOMDTOS() {
    // Given
    ProductItemUomInfoDTO productItemUomInfoDTOWithNullDimensions =
        ProductItemUomInfoDTO.builder().skuCode(SKU_CODE_1).distributionItemInfoRequest(
                validRequest.getProductItemUomInfoDTOS().get(0).getDistributionItemInfoRequest())
            .dimensionAndUomDTOList(null).build();

    DistributionInfoUpdateRequest requestWithNullDimensions =
        DistributionInfoUpdateRequest.builder().sellerCode(SELLER_CODE)
            .productItemUomInfoDTOS(Arrays.asList(productItemUomInfoDTOWithNullDimensions)).build();

    // When & Then
    ApplicationRuntimeException exception = assertThrows(ApplicationRuntimeException.class,
        () -> distributionInfoServiceBean.updateDistributionInfo(StringUtils.EMPTY, PRODUCT_CODE,
            requestWithNullDimensions, product));
    assertEquals("Can not process invalid input data :"
            + ErrorMessage.DIMENSION_AND_UOM_UPDATE_REQUESTS_MUST_NOT_BE_EMPTY.getMessage(),
        exception.getMessage());
  }

  @Test
  void testUpdateDistributionInfo_ValidationFailure_SkuCodeNotFoundInProduct() {
    productItem1.setOmniChannelSku(OMNI_CHANNEL_SKU_1);
    productItem1.setSkuCode(SKU_CODE_1);
    productItem2.setOmniChannelSku(OMNI_CHANNEL_SKU_2);
    productItem2.setSkuCode(SKU_CODE_2);
    product.setProductItems(Arrays.asList(productItem1, productItem2));
    String nonExistentSkuCode = "NON_EXISTENT_SKU";
    ProductItemUomInfoDTO productItemUomInfoDTOWithNonExistentSku =
        ProductItemUomInfoDTO.builder().skuCode(nonExistentSkuCode).distributionItemInfoRequest(
                validRequest.getProductItemUomInfoDTOS().get(0).getDistributionItemInfoRequest())
            .dimensionAndUomDTOList(dimensionAndUomDTOS).build();
    DistributionInfoUpdateRequest requestWithNonExistentSku =
        DistributionInfoUpdateRequest.builder().sellerCode(SELLER_CODE)
            .productItemUomInfoDTOS(Arrays.asList(productItemUomInfoDTOWithNonExistentSku)).build();
    ApplicationRuntimeException exception = assertThrows(ApplicationRuntimeException.class,
        () -> distributionInfoServiceBean.updateDistributionInfo(StringUtils.EMPTY, PRODUCT_CODE,
            requestWithNonExistentSku, product));
    assertEquals("Can not process invalid input data :"
        + ErrorMessage.ITEM_NOT_FOUND_FOR_SKU_CODE.getMessage(), exception.getMessage());
  }

  @Test
  void testUpdateDistributionInfo_JsonProcessingException() throws Exception {
    productItem1.setOmniChannelSku(OMNI_CHANNEL_SKU_1);
    productItem1.setSkuCode(SKU_CODE_1);
    productItem2.setOmniChannelSku(OMNI_CHANNEL_SKU_2);
    productItem2.setSkuCode(SKU_CODE_2);
    product.setProductItems(Arrays.asList(productItem1, productItem2));
    when(objectMapper.writeValueAsString(any())).thenThrow(
        new JsonProcessingException("JSON processing error") {});

    assertThrows(Exception.class,
        () -> distributionInfoServiceBean.updateDistributionInfo(StringUtils.EMPTY, PRODUCT_CODE, validRequest,
            product));
  }

  @Test
  void testUpdateDistributionInfo_Success_UpdateExistingRecord() throws Exception {
    productItem1.setOmniChannelSku(OMNI_CHANNEL_SKU_1);
    productItem1.setSkuCode(SKU_CODE_1);
    productItem2.setOmniChannelSku(OMNI_CHANNEL_SKU_2);
    productItem2.setSkuCode(SKU_CODE_2);
    product.setProductItems(Arrays.asList(productItem1, productItem2));
    when(productItemUomInfoRepository.save(any(ProductItemUomInfo.class))).thenReturn(
        existingProductItemUomInfo);
    DistributionInfoUpdateRequest singleItemRequest =
        DistributionInfoUpdateRequest.builder().sellerCode(SELLER_CODE)
            .productItemUomInfoDTOS(Arrays.asList(validRequest.getProductItemUomInfoDTOS().get(0)))
            .build();
    distributionInfoServiceBean.updateDistributionInfo(StringUtils.EMPTY, PRODUCT_CODE, singleItemRequest, product);
    verify(productItemUomInfoRepository, times(1)).save(any(ProductItemUomInfo.class));
    verify(objectMapper, times(1)).writeValueAsString(dimensionAndUomDTOS);
  }

  @Test
  void testUpdateDistributionInfo_Success_CreateNewRecord() throws Exception {
    when(productItemUomInfoRepository.save(any(ProductItemUomInfo.class))).thenReturn(
        existingProductItemUomInfo);

    DistributionInfoUpdateRequest singleItemRequest =
        DistributionInfoUpdateRequest.builder().sellerCode(SELLER_CODE)
            .productItemUomInfoDTOS(Collections.singletonList(validRequest.getProductItemUomInfoDTOS().get(1)))
            .build();

    distributionInfoServiceBean.updateDistributionInfo(StringUtils.EMPTY, PRODUCT_CODE, singleItemRequest, product);

    verify(productItemUomInfoRepository, times(1)).save(any(ProductItemUomInfo.class));
  }

  @Test
  void testUpdateDistributionInfo_Success_MixedScenario() throws Exception {
    when(mandatoryParameterHelper.getClientId()).thenReturn("mta-api");
    productItem1.setOmniChannelSku(null);
    productItem1.setSkuCode(SKU_CODE_1);
    productItem2.setOmniChannelSku(OMNI_CHANNEL_SKU_2);
    productItem2.setSkuCode(SKU_CODE_2);
    product.setProductItems(Arrays.asList(productItem1, productItem2));
    when(productItemUomInfoRepository.save(any(ProductItemUomInfo.class))).thenReturn(
        existingProductItemUomInfo);
    distributionInfoServiceBean.updateDistributionInfo(StringUtils.EMPTY, PRODUCT_CODE, validRequest, product);
    verify(productItemUomInfoRepository, times(2)).save(any(ProductItemUomInfo.class));
    verify(objectMapper, times(2)).writeValueAsString(dimensionAndUomDTOS);
  }

  @Test
  void testUpdateDistributionInfo_RepositorySaveFailure() throws Exception {
    productItem1.setOmniChannelSku(OMNI_CHANNEL_SKU_1);
    productItem1.setSkuCode(SKU_CODE_1);
    productItem2.setOmniChannelSku(OMNI_CHANNEL_SKU_2);
    productItem2.setSkuCode(SKU_CODE_2);
    product.setProductItems(Arrays.asList(productItem1, productItem2));
    when(productItemUomInfoRepository.save(any(ProductItemUomInfo.class))).thenThrow(
        new RuntimeException("Database error"));

    // When & Then
    assertThrows(RuntimeException.class,
        () -> distributionInfoServiceBean.updateDistributionInfo(StringUtils.EMPTY, PRODUCT_CODE, validRequest,
            product));
  }

  @Test
  void testGetDistributionInfo_ReturnsPagedMappedResponse() {
    // Given
    String storeId = "STORE1";
    String productCode = "PROD1";
    PageRequest pageable = PageRequest.of(0, 10, Sort.by("skuCode"));
    Product product = new Product();
    product.setDistributionInfo("{\"productName\":\"P1\",\"categoryName\":\"C1\"}");
    ProductItemUomInfo entity1 = new ProductItemUomInfo();
    entity1.setSkuCode("SKU1");
    entity1.setOrigin(Origin.LOCAL);
    productItem1.setOmniChannelSku("OC1");
    entity1.setExpiry(false);
    entity1.setUom(
        "[{\"uomCode\":\"U1\",\"uomType\":\"T1\",\"length\":1,\"width\":2,\"height\":3,"
            + "\"conversion\":4.5,\"upcEanList\":[\"111\"]}]");
    ProductItemUomInfo entity2 = new ProductItemUomInfo();
    entity2.setSkuCode("SKU2");
    entity2.setOrigin(Origin.IMPORT);
    productItem2.setOmniChannelSku("OC2");
    entity2.setExpiry(true);
    entity2.setUom(
        "[{\"uomCode\":\"U2\",\"uomType\":\"T2\",\"length\":2,\"width\":3,\"height\":4,"
            + "\"conversion\":5.5,\"upcEanList\":[\"222\"]}]");
    List<ProductItemUomInfo> entityList = Arrays.asList(entity1, entity2);
    Page<ProductItemUomInfo> entityPage = new PageImpl<>(entityList, pageable, 2);
    when(productItemUomInfoRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId,
        productCode, pageable)).thenReturn(entityPage);
    Map<String, ProductItem> skuCodeAndProductItemMap = new HashMap<>();
    ProductItem productItem = new ProductItem();
    productItem.setSkuCode("SKU1");
    productItem.setOmniChannelSku("OC1");
    skuCodeAndProductItemMap.put("SKU1", productItem);
    ProductItem productItem3 = new ProductItem();
    productItem3.setSkuCode("SKU2");
    productItem3.setOmniChannelSku("OC2");
    skuCodeAndProductItemMap.put("SKU2", productItem3);
    Page<DistributionInfoPerSkuResponse> resultPage =
        distributionInfoServiceBean.getDistributionInfo(storeId, productCode, pageable, product,
            skuCodeAndProductItemMap);
    assertEquals(2, resultPage.getContent().size());
    assertEquals(2, resultPage.getTotalElements());
    DistributionInfoPerSkuResponse resp1 = resultPage.getContent().get(0);
    assertEquals("SKU1", resp1.getSkuCode());
    assertEquals("P1", resp1.getDistributionInfoResponse().getProductName());
    assertEquals("C1", resp1.getDistributionInfoResponse().getCategoryName());
    assertEquals("OC1", resp1.getDistributionItemInfoResponse().getOmniChannelSku());
    assertEquals("LOCAL", resp1.getDistributionItemInfoResponse().getOrigin());
    assertFalse(resp1.getDistributionItemInfoResponse().isExpiry());
    assertEquals(1, resp1.getDimensionsAndUomResponse().size());
    DistributionInfoPerSkuResponse resp2 = resultPage.getContent().get(1);
    assertEquals("SKU2", resp2.getSkuCode());
    assertEquals("OC2", resp2.getDistributionItemInfoResponse().getOmniChannelSku());
    assertEquals("IMPORT", resp2.getDistributionItemInfoResponse().getOrigin());
    assertTrue(resp2.getDistributionItemInfoResponse().isExpiry());
    assertEquals(1, resp2.getDimensionsAndUomResponse().size());
  }

  @Test
  public void updateProductItemInProductTest() {
    distributionInfoServiceBean.updateProductItemInProduct(product, new ArrayList<>());
    assertNotNull(product);
  }
} 
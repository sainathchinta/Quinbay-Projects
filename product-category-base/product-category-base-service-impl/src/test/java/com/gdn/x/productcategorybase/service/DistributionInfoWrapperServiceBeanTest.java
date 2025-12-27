package com.gdn.x.productcategorybase.service;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.dto.request.DistributionInfoUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.DimensionAndUomDTO;
import com.gdn.x.productcategorybase.dto.request.DistributionItemInfoRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemUomInfoDTO;
import com.gdn.x.productcategorybase.dto.response.DistributionInfoPerSkuResponse;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductItem;
import com.gdn.x.productcategorybase.domain.event.model.ProductDomainEventModel;
import com.gdn.x.productcategorybase.service.impl.DistributionInfoWrapperServiceBean;
import com.gdn.x.productcategorybase.service.impl.ProductItemServiceBean;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class DistributionInfoWrapperServiceBeanTest {

  private static final String STORE_ID = "10001";
  private static final String PRODUCT_CODE = "PROD-001";
  private static final String SELLER_CODE = "SELLER-001";
  private static final String SKU_CODE_1 = "SKU-001";
  private static final String SKU_CODE_2 = "SKU-002";

  @Mock
  private DistributionInfoService distributionInfoService;

  @Mock
  private ProductService productService;

  @Mock
  private ProductItemServiceBean productItemServiceBean;

  @Mock
  private DomainEventPublisherService domainEventPublisherService;

  @Mock(lenient = true)
  private ObjectMapper objectMapper;

  @InjectMocks
  private DistributionInfoWrapperServiceBean distributionInfoWrapperServiceBean;

  private Product product;
  private DistributionInfoUpdateRequest validRequest;
  private DimensionAndUomDTO dimensionAndUOMDTO;

  @BeforeEach
  void setUp() throws JsonProcessingException {
    // Create test product
    product = new Product();
    product.setId("PROD-001");
    product.setProductCode(PRODUCT_CODE);
    product.setStoreId(STORE_ID);

    // Create test product items
    ProductItem productItem1 = new ProductItem();
    productItem1.setId("ITEM-001");
    productItem1.setSkuCode(SKU_CODE_1);
    productItem1.setProductId(product.getId());

    ProductItem productItem2 = new ProductItem();
    productItem2.setId("ITEM-002");
    productItem2.setSkuCode(SKU_CODE_2);
    productItem2.setProductId(product.getId());

    product.setProductItems(Arrays.asList(productItem1, productItem2));

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

    // Setup ObjectMapper mock
    when(objectMapper.writeValueAsString(any())).thenReturn("{\"test\":\"json\"}");
  }

  @AfterEach
  void tearDown() {
    Mockito.verifyNoMoreInteractions(productItemServiceBean);
  }

  @Test
  void testUpdateDistributionInfoAndPublishProduct_Success() throws Exception {
    // Given
    when(productService.getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(STORE_ID,
        PRODUCT_CODE)).thenReturn(product);
    when(productService.saveAndFlush(any(Product.class))).thenReturn(product);
    when(domainEventPublisherService.publishProductChangeCategory(eq(product), isNull(), eq(false),
        eq(false), eq(false), eq(false), any(HashSet.class))).thenReturn(
        new ProductDomainEventModel());

    // When
    distributionInfoWrapperServiceBean.updateDistributionInfoAndPublishProduct(STORE_ID,
        PRODUCT_CODE, validRequest);

    // Then
    verify(productService, times(1)).getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(
        STORE_ID, PRODUCT_CODE);
    verify(productService, times(1)).setCompleteProductDetailsCached(STORE_ID, product, true);
    verify(distributionInfoService, times(1)).updateDistributionInfo(STORE_ID, PRODUCT_CODE, validRequest,
        product);
    verify(productService, times(1)).saveAndFlush(any(Product.class));
    verify(domainEventPublisherService, times(1)).publishProductChangeCategory(eq(product),
        isNull(), eq(false), eq(false), eq(false), eq(false), any(HashSet.class));
  }

  @Test
  void testUpdateDistributionInfoAndPublishProductUpdatedProductItemsNotEmpty() throws Exception {
    // Given
    when(productService.getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(STORE_ID,
        PRODUCT_CODE)).thenReturn(product);
    when(productService.saveAndFlush(any(Product.class))).thenReturn(product);
    when(domainEventPublisherService.publishProductChangeCategory(eq(product), isNull(), eq(false),
        eq(false), eq(false), eq(false), any(HashSet.class))).thenReturn(
        new ProductDomainEventModel());

    when(distributionInfoService.updateDistributionInfo(STORE_ID, PRODUCT_CODE, validRequest,
        product)).thenReturn(Collections.singletonList(new ProductItem()));

    distributionInfoWrapperServiceBean.updateDistributionInfoAndPublishProduct(STORE_ID,
        PRODUCT_CODE, validRequest);

    // Then
    verify(productService, times(1)).getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(
        STORE_ID, PRODUCT_CODE);
    verify(productService, times(1)).setCompleteProductDetailsCached(STORE_ID, product, true);
    verify(distributionInfoService, times(1)).updateDistributionInfo(STORE_ID, PRODUCT_CODE, validRequest,
        product);
    verify(productService, times(1)).saveAndFlush(any(Product.class));
    verify(domainEventPublisherService, times(1)).publishProductChangeCategory(eq(product),
        isNull(), eq(false), eq(false), eq(false), eq(false), any(HashSet.class));
    verify(productItemServiceBean).saveProductItem(new ProductItem());
    Assertions.assertNotNull(validRequest);
  }

  @Test
  void testUpdateDistributionInfoAndPublishProduct_ProductServiceThrowsException()
      throws Exception {
    // Given
    when(productService.getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(STORE_ID,
        PRODUCT_CODE)).thenThrow(new RuntimeException("Product not found"));

    // When & Then
    Exception exception = assertThrows(RuntimeException.class,
        () -> distributionInfoWrapperServiceBean.updateDistributionInfoAndPublishProduct(STORE_ID,
            PRODUCT_CODE, validRequest));
    assertEquals("Product not found", exception.getMessage());

    verify(productService, times(1)).getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(
        STORE_ID, PRODUCT_CODE);
    verify(productService, never()).setCompleteProductDetailsCached(any(), any(), anyBoolean());
    verify(distributionInfoService, never()).updateDistributionInfo(any(), any(), any(), any());
    verify(domainEventPublisherService, never()).publishProductChangeCategory(any(), any(),
        anyBoolean(), anyBoolean(), anyBoolean(), anyBoolean(), any());
  }

  @Test
  void testUpdateDistributionInfoAndPublishProduct_SetCompleteProductDetailsThrowsException()
      throws Exception {
    // Given
    when(productService.getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(STORE_ID,
        PRODUCT_CODE)).thenReturn(product);
    doThrow(new RuntimeException("Cache error")).when(productService)
        .setCompleteProductDetailsCached(STORE_ID, product, true);

    // When & Then
    Exception exception = assertThrows(RuntimeException.class,
        () -> distributionInfoWrapperServiceBean.updateDistributionInfoAndPublishProduct(STORE_ID,
            PRODUCT_CODE, validRequest));
    assertEquals("Cache error", exception.getMessage());

    verify(productService, times(1)).getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(
        STORE_ID, PRODUCT_CODE);
    verify(productService, times(1)).setCompleteProductDetailsCached(STORE_ID, product, true);
    verify(distributionInfoService, never()).updateDistributionInfo(any(), any(), any(), any());
    verify(productService, never()).saveAndFlush(any(Product.class));
    verify(domainEventPublisherService, never()).publishProductChangeCategory(any(), any(),
        anyBoolean(), anyBoolean(), anyBoolean(), anyBoolean(), any());
  }

  @Test
  void testUpdateDistributionInfoAndPublishProduct_DistributionInfoServiceThrowsException()
      throws Exception {
    // Given
    when(productService.getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(STORE_ID,
        PRODUCT_CODE)).thenReturn(product);
    doNothing().when(productService).setCompleteProductDetailsCached(STORE_ID, product, true);
    doThrow(new RuntimeException("Distribution info update failed")).when(distributionInfoService)
        .updateDistributionInfo(STORE_ID, PRODUCT_CODE, validRequest, product);

    // When & Then
    Exception exception = assertThrows(RuntimeException.class,
        () -> distributionInfoWrapperServiceBean.updateDistributionInfoAndPublishProduct(STORE_ID,
            PRODUCT_CODE, validRequest));
    assertEquals("Distribution info update failed", exception.getMessage());

    verify(productService, times(1)).getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(
        STORE_ID, PRODUCT_CODE);
    verify(productService, times(1)).setCompleteProductDetailsCached(STORE_ID, product, true);
    verify(distributionInfoService, times(1)).updateDistributionInfo(STORE_ID, PRODUCT_CODE, validRequest,
        product);
    verify(productService, never()).saveAndFlush(any(Product.class));
    verify(domainEventPublisherService, never()).publishProductChangeCategory(any(), any(),
        anyBoolean(), anyBoolean(), anyBoolean(), anyBoolean(), any());
  }

  @Test
  void testUpdateDistributionInfoAndPublishProduct_DomainEventPublisherServiceThrowsException()
      throws Exception {
    // Given
    DistributionInfoUpdateRequest requestWithoutDistributionInfo =
        DistributionInfoUpdateRequest.builder().sellerCode(SELLER_CODE)
            .productItemUomInfoDTOS(validRequest.getProductItemUomInfoDTOS())
            .build(); // No distributionInfoRequest

    when(productService.getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(STORE_ID,
        PRODUCT_CODE)).thenReturn(product);
    doThrow(new RuntimeException("Event publishing failed")).when(domainEventPublisherService)
        .publishProductChangeCategory(eq(product), isNull(), eq(false), eq(false), eq(false),
            eq(false), any(HashSet.class));

    // When & Then
    Exception exception = assertThrows(RuntimeException.class,
        () -> distributionInfoWrapperServiceBean.updateDistributionInfoAndPublishProduct(STORE_ID,
            PRODUCT_CODE, requestWithoutDistributionInfo));
    assertEquals("Event publishing failed", exception.getMessage());

    verify(productService, times(1)).getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(
        STORE_ID, PRODUCT_CODE);
    verify(productService, times(1)).setCompleteProductDetailsCached(STORE_ID, product, true);
    verify(distributionInfoService, times(1)).updateDistributionInfo(STORE_ID, PRODUCT_CODE,
        requestWithoutDistributionInfo, product);
    verify(productService, never()).saveAndFlush(any(Product.class));
    verify(domainEventPublisherService, times(1)).publishProductChangeCategory(eq(product),
        isNull(), eq(false), eq(false), eq(false), eq(false), any(HashSet.class));
  }

  @Test
  void testUpdateDistributionInfoAndPublishProduct_WithNullProduct() throws Exception {

    // When & Then
    Exception exception = assertThrows(NullPointerException.class,
        () -> distributionInfoWrapperServiceBean.updateDistributionInfoAndPublishProduct(STORE_ID,
            PRODUCT_CODE, validRequest));
    assertEquals(
        "Cannot invoke \"com.gdn.x.productcategorybase.entity.Product.setDistributionInfo(String)"
            + "\" because \"product\" is null",
        exception.getMessage());

    verify(productService, times(1)).getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(
        STORE_ID, PRODUCT_CODE);
    verify(productService, times(1)).setCompleteProductDetailsCached(STORE_ID, null, true);
    verify(distributionInfoService, times(1)).updateDistributionInfo(STORE_ID, PRODUCT_CODE, validRequest,
        null);
    verify(productService, never()).saveAndFlush(any(Product.class));
    verify(domainEventPublisherService, never()).publishProductChangeCategory(any(), any(),
        anyBoolean(), anyBoolean(), anyBoolean(), anyBoolean(), any());
  }

  @Test
  void testUpdateDistributionInfoAndPublishProduct_WithEmptyProductItems() throws Exception {
    // Given
    product.setProductItems(Collections.emptyList());
    when(productService.getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(STORE_ID,
        PRODUCT_CODE)).thenReturn(product);

    when(productService.saveAndFlush(any(Product.class))).thenReturn(product);
    when(domainEventPublisherService.publishProductChangeCategory(eq(product), isNull(), eq(false),
        eq(false), eq(false), eq(false), any(HashSet.class))).thenReturn(
        new ProductDomainEventModel());

    // When
    distributionInfoWrapperServiceBean.updateDistributionInfoAndPublishProduct(STORE_ID,
        PRODUCT_CODE, validRequest);

    // Then
    verify(productService, times(1)).getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(
        STORE_ID, PRODUCT_CODE);
    verify(productService, times(1)).setCompleteProductDetailsCached(STORE_ID, product, true);
    verify(distributionInfoService, times(1)).updateDistributionInfo(STORE_ID, PRODUCT_CODE, validRequest,
        product);
    verify(productService, times(1)).saveAndFlush(any(Product.class));
    verify(domainEventPublisherService, times(1)).publishProductChangeCategory(eq(product),
        isNull(), eq(false), eq(false), eq(false), eq(false), any(HashSet.class));
  }

  @Test
  void testUpdateDistributionInfoAndPublishProduct_WithNullRequest() throws Exception {
    // Given
    when(productService.getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(STORE_ID,
        PRODUCT_CODE)).thenReturn(product);

    // When & Then
    Exception exception = assertThrows(NullPointerException.class,
        () -> distributionInfoWrapperServiceBean.updateDistributionInfoAndPublishProduct(STORE_ID,
            PRODUCT_CODE, null));
    assertEquals(
        "Cannot invoke \"com.gdn.x.productcategorybase.dto.request.DistributionInfoUpdateRequest"
            + ".getDistributionInfoRequest()\" because \"distributionInfoUpdateRequest\" is null",
        exception.getMessage());

    verify(productService, times(1)).getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(
        STORE_ID, PRODUCT_CODE);
    verify(productService, times(1)).setCompleteProductDetailsCached(STORE_ID, product, true);
    verify(distributionInfoService, times(1)).updateDistributionInfo(STORE_ID, PRODUCT_CODE, null, product);
    verify(productService, never()).saveAndFlush(any(Product.class));
    verify(domainEventPublisherService, never()).publishProductChangeCategory(any(), any(),
        anyBoolean(), anyBoolean(), anyBoolean(), anyBoolean(), any());
  }

  @Test
  void testGetDistributionInfo_AllValidNeedDistributionInfoTrue() {
    String storeId = "S1";
    String productCode = "P1";
    PageRequest pageable = PageRequest.of(0, 10);
    Product newProduct = new Product();
    when(productService.getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(storeId,
        productCode)).thenReturn(newProduct);
    when(distributionInfoService.getDistributionInfo(storeId, productCode, pageable,
        newProduct, new HashMap<>())).thenReturn(Page.empty());
    Page<DistributionInfoPerSkuResponse> result =
        distributionInfoWrapperServiceBean.getDistributionInfo(storeId, productCode, pageable);
    assertNotNull(result);
    verify(productService, times(1)).getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(
        storeId, productCode);
    verify(distributionInfoService, times(1)).getDistributionInfo(storeId, productCode, pageable,
        newProduct, new HashMap<>());
    verify(productItemServiceBean).getProductItemsByStoreIdAndProductIdCached(storeId, newProduct.getId());
  }

  @Test
  void testGetDistributionInfo_NeedDistributionInfoFalse() {
    String storeId = "S1";
    String productCode = "P1";
    PageRequest pageable = PageRequest.of(0, 10);
    when(productService.getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(storeId,
        productCode)).thenReturn(product);
    when(distributionInfoService.getDistributionInfo(storeId, productCode, pageable,
        product, new HashMap<>())).thenReturn(Page.empty());
    Page<DistributionInfoPerSkuResponse> result =
        distributionInfoWrapperServiceBean.getDistributionInfo(storeId, productCode, pageable);
    assertNotNull(result);
    verify(productService).getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(
        anyString(), anyString());
    verify(productItemServiceBean).getProductItemsByStoreIdAndProductIdCached(storeId, product.getId());
  }

  @Test
  void testGetDistributionInfo_ThrowsExceptionWhenStoreIdEmpty() {
    String storeId = "";
    String productCode = "P1";
    PageRequest pageable = PageRequest.of(0, 10);
    ApplicationRuntimeException ex = assertThrows(ApplicationRuntimeException.class,
        () -> distributionInfoWrapperServiceBean.getDistributionInfo(storeId, productCode, pageable));
    assertTrue(ex.getMessage().contains(ErrorMessage.STORE_ID_MUST_NOT_BE_EMPTY.getMessage()));
    verifyNoInteractions(productService);
    verifyNoInteractions(distributionInfoService);
  }

  @Test
  void testGetDistributionInfo_ThrowsExceptionWhenProductCodeEmpty() {
    String storeId = "S1";
    String productCode = "";
    PageRequest pageable = PageRequest.of(0, 10);
    ApplicationRuntimeException ex = assertThrows(ApplicationRuntimeException.class,
        () -> distributionInfoWrapperServiceBean.getDistributionInfo(storeId, productCode, pageable));
    assertTrue(ex.getMessage().contains(ErrorMessage.PRODUCT_CODE_MUST_NOT_BE_EMPTY.getMessage()));
    verifyNoInteractions(productService);
    verifyNoInteractions(distributionInfoService);
  }

  @Test
  void testGetDistributionInfo_ThrowsExceptionWhenProductNotFound() {
    String storeId = "S1";
    String productCode = "P1";
    PageRequest pageable = PageRequest.of(0, 10);
    when(productService.getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(storeId,
        productCode)).thenReturn(null);
    ApplicationRuntimeException ex = assertThrows(ApplicationRuntimeException.class,
        () -> distributionInfoWrapperServiceBean.getDistributionInfo(storeId, productCode, pageable));
    assertTrue(ex.getMessage().contains(ErrorMessage.PRODUCT_NOT_FOUND.getMessage()));
    verify(productService, times(1)).getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(
        storeId, productCode);
    verifyNoInteractions(distributionInfoService);
  }

} 
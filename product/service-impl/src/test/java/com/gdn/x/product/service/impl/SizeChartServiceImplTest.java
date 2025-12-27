package com.gdn.x.product.service.impl;

import java.util.Arrays;
import java.util.Collections;
import java.util.Set;

import com.gdn.x.product.rest.web.model.response.AuditTrailListResponse;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.product.constants.ErrorMessages;
import com.gdn.x.product.domain.event.config.ProductDomainEventName;
import com.gdn.x.product.domain.event.model.ProductAndItemEventModel;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.outbound.api.ProductCategoryBaseOutbound;
import com.gdn.x.product.rest.web.model.request.ProductSizeChartUpdateRequest;
import com.gdn.x.product.rest.web.model.response.SizeChartResponse;
import com.gdn.x.product.service.api.ProductService;
import com.gdn.x.product.service.api.SaveOperationService;
import com.gdn.x.product.service.config.KafkaPublisher;

public class SizeChartServiceImplTest {

  private static final String STORE_ID = "STORE_ID";
  private static final String SIZE_CHART_CODE = "SIZE_CHART_CODE";
  private static final String PRODUCT_SKU_1 = "PRODUCT_SKU_1";
  private static final String PRODUCT_SKU_2 = "PRODUCT_SKU_2";
  private static final String PRODUCT_SKU_3 = "PRODUCT_SKU_3";

  @InjectMocks
  private SizeChartServiceImpl sizeCharServiceImpl;

  @Mock
  private ProductService productService;

  @Mock
  private SaveOperationService saveOperationService;

  @Mock
  private ProductCategoryBaseOutbound productCategoryBaseOutbound;

  @Mock
  private KafkaPublisher kafkaProducer;

  @Captor
  private ArgumentCaptor<ProductAndItemEventModel> productAndItemEventModelArgumentCaptor;

  @Captor
  private ArgumentCaptor<AuditTrailListResponse> auditTrailListResponseArgumentCaptor;

  private SizeChartResponse sizeChartResponse;

  @BeforeEach
  public void init() {
    MockitoAnnotations.openMocks(this);
    sizeChartResponse = new SizeChartResponse();
    sizeChartResponse.setSizeChartCode(SIZE_CHART_CODE);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(productService, saveOperationService, productCategoryBaseOutbound, kafkaProducer);
  }

  @Test
  public void fetchSizeChartTest() {
    Mockito.when(productCategoryBaseOutbound.fetchSizeChartDetails(STORE_ID, SIZE_CHART_CODE)).thenReturn(sizeChartResponse);
    SizeChartResponse sizeChartResponse = sizeCharServiceImpl.fetchSizeChartDetails(STORE_ID, SIZE_CHART_CODE);
    Mockito.verify(productCategoryBaseOutbound).fetchSizeChartDetails(STORE_ID, SIZE_CHART_CODE);
    Assertions.assertEquals(SIZE_CHART_CODE, sizeChartResponse.getSizeChartCode());
  }

  @Test
  public void fetchSizeChartExceptionTest() {
    try {
      sizeCharServiceImpl.fetchSizeChartDetails(STORE_ID, null);
    } catch (ApplicationRuntimeException exception) {
      Assertions.assertTrue(exception.getErrorMessage().contains(ErrorMessages.SIZE_CHART_CODE_MUST_NOT_BE_NULL));
    }
  }

  @Test
  public void updateProductSizeChartCodeTest() throws Exception {
    ProductSizeChartUpdateRequest productSizeChartUpdateRequest =
        new ProductSizeChartUpdateRequest(Set.of(PRODUCT_SKU_1), Set.of(PRODUCT_SKU_2));
    Product product1 = new Product();
    product1.setProductSku(PRODUCT_SKU_1);
    Product product2 = new Product();
    product2.setProductSku(PRODUCT_SKU_2);
    product2.setSizeChartCode(SIZE_CHART_CODE);
    Mockito.when(productCategoryBaseOutbound.isSizeChartCodeValid(SIZE_CHART_CODE)).thenReturn(true);
    Mockito.when(productService.getProducts(STORE_ID, Set.of(PRODUCT_SKU_1, PRODUCT_SKU_2)))
        .thenReturn(Arrays.asList(product1, product2));
    Mockito.when(saveOperationService.saveProductWithoutUpdatingSolr(product1, Collections.EMPTY_LIST,
            StringUtils.EMPTY, Collections.EMPTY_MAP))
        .thenReturn(product1);
    Mockito.when(saveOperationService.saveProductWithoutUpdatingSolr(product2, Collections.EMPTY_LIST,
            StringUtils.EMPTY, Collections.EMPTY_MAP))
        .thenReturn(product1);
    sizeCharServiceImpl.updateProductSizeChartCode(STORE_ID, SIZE_CHART_CODE, productSizeChartUpdateRequest);
    Mockito.verify(productCategoryBaseOutbound).isSizeChartCodeValid(SIZE_CHART_CODE);
    Mockito.verify(productService).getProducts(STORE_ID, Set.of(PRODUCT_SKU_1, PRODUCT_SKU_2));
    Mockito.verify(saveOperationService).saveProductWithoutUpdatingSolr(product1, Collections.EMPTY_LIST,
        StringUtils.EMPTY, Collections.EMPTY_MAP);
    Mockito.verify(saveOperationService).saveProductWithoutUpdatingSolr(product2, Collections.EMPTY_LIST,
        StringUtils.EMPTY, Collections.EMPTY_MAP);
    Mockito.verify(kafkaProducer).send(Mockito.eq(ProductDomainEventName.UPDATE_TO_SOLR), Mockito.eq(PRODUCT_SKU_1),
        productAndItemEventModelArgumentCaptor.capture());
    Mockito.verify(kafkaProducer).send(Mockito.eq(ProductDomainEventName.UPDATE_TO_SOLR), Mockito.eq(PRODUCT_SKU_2),
        productAndItemEventModelArgumentCaptor.capture());
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(ProductDomainEventName.PRODUCT_SKU_UPDATE_HISTORY),
            auditTrailListResponseArgumentCaptor.capture());
    Assertions.assertEquals(SIZE_CHART_CODE, product1.getSizeChartCode());
    Assertions.assertEquals(StringUtils.EMPTY, product2.getSizeChartCode());
  }

  @Test
  public void updateProductSizeChartCodeOnlyAddSizeChartTest() throws Exception {
    ProductSizeChartUpdateRequest productSizeChartUpdateRequest =
        new ProductSizeChartUpdateRequest(Set.of(PRODUCT_SKU_1), Collections.EMPTY_SET);
    Product product1 = new Product();
    product1.setProductSku(PRODUCT_SKU_1);
    Mockito.when(productCategoryBaseOutbound.isSizeChartCodeValid(SIZE_CHART_CODE)).thenReturn(true);
    Mockito.when(productService.getProducts(STORE_ID, Set.of(PRODUCT_SKU_1))).thenReturn(Arrays.asList(product1));
    Mockito.when(saveOperationService.saveProductWithoutUpdatingSolr(product1, Collections.EMPTY_LIST,
            StringUtils.EMPTY, Collections.EMPTY_MAP))
        .thenReturn(product1);
    sizeCharServiceImpl.updateProductSizeChartCode(STORE_ID, SIZE_CHART_CODE, productSizeChartUpdateRequest);
    Mockito.verify(productCategoryBaseOutbound).isSizeChartCodeValid(SIZE_CHART_CODE);
    Mockito.verify(productService).getProducts(STORE_ID, Set.of(PRODUCT_SKU_1));
    Mockito.verify(saveOperationService).saveProductWithoutUpdatingSolr(product1, Collections.EMPTY_LIST,
        StringUtils.EMPTY, Collections.EMPTY_MAP);
    Mockito.verify(kafkaProducer).send(Mockito.eq(ProductDomainEventName.UPDATE_TO_SOLR), Mockito.eq(PRODUCT_SKU_1),
        productAndItemEventModelArgumentCaptor.capture());
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(ProductDomainEventName.PRODUCT_SKU_UPDATE_HISTORY),
            auditTrailListResponseArgumentCaptor.capture());
    Assertions.assertEquals(SIZE_CHART_CODE, product1.getSizeChartCode());
  }

  @Test
  public void updateProductSizeChartCodeOnlyRemoveSizeChartTest() throws Exception {
    ProductSizeChartUpdateRequest productSizeChartUpdateRequest =
        new ProductSizeChartUpdateRequest(Collections.EMPTY_SET, (Set.of(PRODUCT_SKU_1)));
    Product product1 = new Product();
    product1.setProductSku(PRODUCT_SKU_1);
    product1.setSizeChartCode(SIZE_CHART_CODE);
    Mockito.when(productCategoryBaseOutbound.isSizeChartCodeValid(SIZE_CHART_CODE)).thenReturn(true);
    Mockito.when(productService.getProducts(STORE_ID, Set.of(PRODUCT_SKU_1))).thenReturn(Arrays.asList(product1));
    Mockito.when(saveOperationService.saveProductWithoutUpdatingSolr(product1, Collections.EMPTY_LIST,
            StringUtils.EMPTY, Collections.EMPTY_MAP))
        .thenReturn(product1);
    sizeCharServiceImpl.updateProductSizeChartCode(STORE_ID, SIZE_CHART_CODE, productSizeChartUpdateRequest);
    Mockito.verify(productCategoryBaseOutbound).isSizeChartCodeValid(SIZE_CHART_CODE);
    Mockito.verify(productService).getProducts(STORE_ID, Set.of(PRODUCT_SKU_1));
    Mockito.verify(saveOperationService).saveProductWithoutUpdatingSolr(product1, Collections.EMPTY_LIST,
        StringUtils.EMPTY, Collections.EMPTY_MAP);
    Mockito.verify(kafkaProducer).send(Mockito.eq(ProductDomainEventName.UPDATE_TO_SOLR), Mockito.eq(PRODUCT_SKU_1),
        productAndItemEventModelArgumentCaptor.capture());
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(ProductDomainEventName.PRODUCT_SKU_UPDATE_HISTORY),
            auditTrailListResponseArgumentCaptor.capture());
    Assertions.assertEquals(StringUtils.EMPTY, product1.getSizeChartCode());
  }

  @Test
  public void updateProductSizeChartCodeInvalidTest() throws Exception {
    ProductSizeChartUpdateRequest productSizeChartUpdateRequest =
        new ProductSizeChartUpdateRequest(Set.of(PRODUCT_SKU_1), Set.of(PRODUCT_SKU_2));
    Mockito.when(productCategoryBaseOutbound.isSizeChartCodeValid(SIZE_CHART_CODE)).thenReturn(false);
    try {
      Assertions.assertThrows(
          ApplicationRuntimeException.class, () -> sizeCharServiceImpl.updateProductSizeChartCode(STORE_ID, SIZE_CHART_CODE, productSizeChartUpdateRequest));
    } finally {
      Mockito.verify(productCategoryBaseOutbound).isSizeChartCodeValid(SIZE_CHART_CODE);
    }
  }

  @Test
  public void updateProductSizeChartCodeProductCountNotMatchTest() throws Exception {
    ProductSizeChartUpdateRequest productSizeChartUpdateRequest =
        new ProductSizeChartUpdateRequest(Set.of(PRODUCT_SKU_1), Set.of(PRODUCT_SKU_2));
    Product product1 = new Product();
    product1.setProductSku(PRODUCT_SKU_1);
    Mockito.when(productCategoryBaseOutbound.isSizeChartCodeValid(SIZE_CHART_CODE)).thenReturn(true);
    Mockito.when(productService.getProducts(STORE_ID, Set.of(PRODUCT_SKU_1, PRODUCT_SKU_2)))
        .thenReturn(Arrays.asList(product1));
    try {
      Assertions.assertThrows(
          ApplicationRuntimeException.class, () -> sizeCharServiceImpl.updateProductSizeChartCode(STORE_ID, SIZE_CHART_CODE, productSizeChartUpdateRequest));
    } finally {
      Mockito.verify(productCategoryBaseOutbound).isSizeChartCodeValid(SIZE_CHART_CODE);
      Mockito.verify(productService).getProducts(STORE_ID, Set.of(PRODUCT_SKU_1, PRODUCT_SKU_2));
    }
  }

  @Test
  public void updateProductSizeChartCodeProductSizeChartCodeMismatchTest() throws Exception {
    ProductSizeChartUpdateRequest productSizeChartUpdateRequest =
        new ProductSizeChartUpdateRequest(Set.of(PRODUCT_SKU_1), Set.of(PRODUCT_SKU_2));
    Product product1 = new Product();
    product1.setProductSku(PRODUCT_SKU_1);
    Product product2 = new Product();
    product2.setProductSku(PRODUCT_SKU_2);
    Mockito.when(productCategoryBaseOutbound.isSizeChartCodeValid(SIZE_CHART_CODE)).thenReturn(true);
    Mockito.when(productService.getProducts(STORE_ID, Set.of(PRODUCT_SKU_1, PRODUCT_SKU_2)))
        .thenReturn(Arrays.asList(product1, product2));
    try {
      Assertions.assertThrows(
          ApplicationRuntimeException.class, () -> sizeCharServiceImpl.updateProductSizeChartCode(STORE_ID, SIZE_CHART_CODE, productSizeChartUpdateRequest));
    } finally {
      Mockito.verify(productCategoryBaseOutbound).isSizeChartCodeValid(SIZE_CHART_CODE);
      Mockito.verify(productService).getProducts(STORE_ID, Set.of(PRODUCT_SKU_1, PRODUCT_SKU_2));
    }
  }

  @Test
  public void updateProductSizeChartCodeProductNotFoundTest() throws Exception {
    ProductSizeChartUpdateRequest productSizeChartUpdateRequest =
        new ProductSizeChartUpdateRequest(Set.of(PRODUCT_SKU_1), Set.of(PRODUCT_SKU_2));
    Product product1 = new Product();
    product1.setProductSku(PRODUCT_SKU_1);
    Product product2 = new Product();
    product2.setProductSku(PRODUCT_SKU_3);
    Mockito.when(productCategoryBaseOutbound.isSizeChartCodeValid(SIZE_CHART_CODE)).thenReturn(true);
    Mockito.when(productService.getProducts(STORE_ID, Set.of(PRODUCT_SKU_1, PRODUCT_SKU_2)))
        .thenReturn(Arrays.asList(product1, product2));
    try {
      Assertions.assertThrows(
          ApplicationRuntimeException.class, () -> sizeCharServiceImpl.updateProductSizeChartCode(STORE_ID, SIZE_CHART_CODE, productSizeChartUpdateRequest));
    } finally {
      Mockito.verify(productCategoryBaseOutbound).isSizeChartCodeValid(SIZE_CHART_CODE);
      Mockito.verify(productService).getProducts(STORE_ID, Set.of(PRODUCT_SKU_1, PRODUCT_SKU_2));
    }
  }

  @Test
  public void updateProductSizeChartCodeAddProductNotFoundTest() throws Exception {
    ProductSizeChartUpdateRequest productSizeChartUpdateRequest =
        new ProductSizeChartUpdateRequest(Set.of(PRODUCT_SKU_1), Set.of(PRODUCT_SKU_2));
    Product product1 = new Product();
    product1.setProductSku(PRODUCT_SKU_3);
    Product product2 = new Product();
    product2.setProductSku(PRODUCT_SKU_2);
    product2.setSizeChartCode(SIZE_CHART_CODE);
    Mockito.when(productCategoryBaseOutbound.isSizeChartCodeValid(SIZE_CHART_CODE)).thenReturn(true);
    Mockito.when(productService.getProducts(STORE_ID, Set.of(PRODUCT_SKU_1, PRODUCT_SKU_2)))
        .thenReturn(Arrays.asList(product1, product2));
    try {
      Assertions.assertThrows(
          ApplicationRuntimeException.class, () -> sizeCharServiceImpl.updateProductSizeChartCode(STORE_ID, SIZE_CHART_CODE, productSizeChartUpdateRequest));
    } finally {
      Mockito.verify(productCategoryBaseOutbound).isSizeChartCodeValid(SIZE_CHART_CODE);
      Mockito.verify(productService).getProducts(STORE_ID, Set.of(PRODUCT_SKU_1, PRODUCT_SKU_2));
    }
  }

  @Test
  public void updateProductSizeChartCodeCommonSkuTest() throws Exception {
    ProductSizeChartUpdateRequest productSizeChartUpdateRequest =
        new ProductSizeChartUpdateRequest(Set.of(PRODUCT_SKU_1), Set.of(PRODUCT_SKU_1));
    Assertions.assertThrows(
        ApplicationRuntimeException.class, () -> sizeCharServiceImpl.updateProductSizeChartCode(STORE_ID, SIZE_CHART_CODE, productSizeChartUpdateRequest));
  }

  @Test
  public void updateProductSizeChartCodeBothEmptyTest() throws Exception {
    ProductSizeChartUpdateRequest productSizeChartUpdateRequest =
        new ProductSizeChartUpdateRequest(Collections.EMPTY_SET, Collections.EMPTY_SET);
    Assertions.assertThrows(
        ApplicationRuntimeException.class, () -> sizeCharServiceImpl.updateProductSizeChartCode(STORE_ID, SIZE_CHART_CODE, productSizeChartUpdateRequest));
  }

  @Test
  public void updateProductSizeChartCodeStoreIdEmptyTest() throws Exception {
    ProductSizeChartUpdateRequest productSizeChartUpdateRequest =
        new ProductSizeChartUpdateRequest(Collections.EMPTY_SET, Collections.EMPTY_SET);
    Assertions.assertThrows(
        ApplicationRuntimeException.class, () -> sizeCharServiceImpl.updateProductSizeChartCode(StringUtils.EMPTY, SIZE_CHART_CODE, productSizeChartUpdateRequest));
  }

  @Test
  public void updateProductSizeChartCodeSizeChartCodeEmptyTest() throws Exception {
    ProductSizeChartUpdateRequest productSizeChartUpdateRequest =
        new ProductSizeChartUpdateRequest(Collections.EMPTY_SET, Collections.EMPTY_SET);
    Assertions.assertThrows(
        ApplicationRuntimeException.class, () -> sizeCharServiceImpl.updateProductSizeChartCode(STORE_ID, StringUtils.EMPTY, productSizeChartUpdateRequest));
  }

  @Test
  public void evictSizeChartCacheTest(){
    sizeCharServiceImpl.evictSizeChartCache(STORE_ID, SIZE_CHART_CODE);
    Assertions.assertTrue(true);
  }
}

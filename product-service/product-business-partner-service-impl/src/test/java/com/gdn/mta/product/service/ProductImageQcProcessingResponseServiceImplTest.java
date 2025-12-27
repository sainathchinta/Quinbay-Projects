package com.gdn.mta.product.service;

import java.util.Arrays;
import java.util.List;

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

import com.gdn.mta.product.entity.ProductImageQcProcessingResponse;
import com.gdn.mta.product.repository.ProductImageQcProcessingResponseRepository;

public class ProductImageQcProcessingResponseServiceImplTest {

  private static final String STORE_ID = "10001";
  private static final String PRODUCT_CODE = "productCode";
  private static final String IMAGE_PREDICTION_RESPONSE_1 =
      "[{\"locationPath\":\"path1\",\"hashCode\":\"hashCode1\",\"predictions\":[{\"predictionType\":\"watermark\",\"displayName\":\"water mark present\",\"present\":false,\"confidence\":40},{\"predictionType\":\"nsfw\",\"displayName\":\"nsfw present\",\"present\":true,\"confidence\":80}]},{\"locationPath\":\"path2\",\"hashCode\":\"hashCode2\",\"predictions\":[{\"predictionType\":\"watermark\",\"displayName\":\"water mark present\",\"present\":false,\"confidence\":40},{\"predictionType\":\"nsfw\",\"displayName\":\"nsfw present\",\"present\":true,\"confidence\":80}]}]";

  private ProductImageQcProcessingResponse productImageQcProcessingResponse;

  @InjectMocks
  private ProductImageQcProcessingResponseServiceImpl productImageQcProcessingResponseService;

  @Mock
  private ProductImageQcProcessingResponseRepository productImageQcProcessingResponseRepository;

  @Captor
  private ArgumentCaptor<ProductImageQcProcessingResponse> productImageQcProcessingResponseArgumentCaptor;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    productImageQcProcessingResponse = new ProductImageQcProcessingResponse();
    productImageQcProcessingResponse.setProductCode(PRODUCT_CODE);
    productImageQcProcessingResponse.setForceReview(true);
    productImageQcProcessingResponse.setImageQcResponse(IMAGE_PREDICTION_RESPONSE_1);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(productImageQcProcessingResponseRepository);
  }

  @Test
  public void findByStoreIdAndProductCodeTest() {
    Mockito.when(productImageQcProcessingResponseRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(productImageQcProcessingResponse);
    ProductImageQcProcessingResponse response =
        productImageQcProcessingResponseService.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productImageQcProcessingResponseRepository).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Assertions.assertEquals(PRODUCT_CODE, response.getProductCode());
    Assertions.assertTrue(response.isForceReview());
  }

  @Test
  public void findByStoreIdAndProductCodeInTest() {
    Mockito.when(
        productImageQcProcessingResponseRepository.findByStoreIdAndProductCodeIn(STORE_ID, Arrays.asList(PRODUCT_CODE)))
        .thenReturn(Arrays.asList(productImageQcProcessingResponse));
    List<ProductImageQcProcessingResponse> response =
        productImageQcProcessingResponseService.findByStoreIdAndProductCodeIn(STORE_ID, Arrays.asList(PRODUCT_CODE));
    Mockito.verify(productImageQcProcessingResponseRepository)
        .findByStoreIdAndProductCodeIn(STORE_ID, Arrays.asList(PRODUCT_CODE));
    Assertions.assertEquals(PRODUCT_CODE, response.get(0).getProductCode());
    Assertions.assertTrue(response.get(0).isForceReview());
  }

  @Test
  public void saveTest() {
    productImageQcProcessingResponseService.save(productImageQcProcessingResponse);
    Mockito.verify(productImageQcProcessingResponseRepository)
        .saveAndFlush(
            productImageQcProcessingResponseArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_CODE, productImageQcProcessingResponseArgumentCaptor.getValue().getProductCode());
    Assertions.assertTrue(productImageQcProcessingResponseArgumentCaptor.getValue().isForceReview());
    Assertions.assertEquals(IMAGE_PREDICTION_RESPONSE_1,
        productImageQcProcessingResponseArgumentCaptor.getValue().getImageQcResponse());
  }

  @Test
  public void findByStoreIdAndProductCodeDbTest() {
    productImageQcProcessingResponseService.findByStoreIdAndProductCodeDb(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productImageQcProcessingResponseRepository).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
  }

  @Test
  public void deleteProductImageQcProcessingResponseByStoreIdAndProductCodeTest() {
    productImageQcProcessingResponseService.deleteProductImageQcProcessingResponseByStoreIdAndProductCode(STORE_ID,
        PRODUCT_CODE);
    Mockito.verify(productImageQcProcessingResponseRepository).deleteByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
  }
}
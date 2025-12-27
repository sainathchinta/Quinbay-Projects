package com.gdn.x.mta.distributiontask.service.impl;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.slf4j.MDC;

import com.gdn.x.mta.distributiontask.util.GdnMandatoryRequestParameterUtil;
import com.gdn.x.mta.distributiontask.dao.api.ProductImageQcFeedbackRepository;
import com.gdn.x.mta.distributiontask.model.ProductImageQcFeedback;
import com.gdn.x.mta.distributiontask.response.ProductImageQcFeedbackResponse;
import com.gdn.x.mta.distributiontask.rest.model.request.ProductImageQcFeedbackRequest;

@ExtendWith(MockitoExtension.class)
public class ProductImageQcFeedbackServiceImplTest {

  private static final String STORE_ID = "store-id";
  private static final String PRODUCT_CODE = "productCode";
  private static final String SYSTEM_FEEEDBACK =
      "{\"timestamp\":0,\"productCode\":\"MTA-0001\",\"images\":[{\"locationPath\":\"path1\",\"hashCode\":\"hashCode1\",\"predictions\":[{\"predictionType\":\"watermark\",\"displayName\":\"watermarkpresent\",\"present\":false,\"confidence\":40},{\"predictionType\":\"nsfw\",\"displayName\":\"nsfwpresent\",\"present\":true,\"confidence\":80}]},{\"locationPath\":\"path2\",\"hashCode\":\"hashCode2\",\"predictions\":[{\"predictionType\":\"watermark\",\"displayName\":\"watermarkpresent\",\"present\":false,\"confidence\":40},{\"predictionType\":\"nsfw\",\"displayName\":\"nsfwpresent\",\"present\":true,\"confidence\":80}]}],\"success\":true,\"errorMessage\":null}";
  private static final String NEW_SYSTEM_FEEEDBACK =
      "{\"timestamp\":0,\"productCode\":\"MTA-0001\",\"images\":[{\"locationPath\":\"path3\",\"hashCode\":\"hashCode1\",\"predictions\":[{\"predictionType\":\"watermark\",\"displayName\":\"watermarkpresent\",\"present\":false,\"confidence\":40},{\"predictionType\":\"nsfw\",\"displayName\":\"nsfwpresent\",\"present\":true,\"confidence\":80}]},{\"locationPath\":\"path2\",\"hashCode\":\"hashCode2\",\"predictions\":[{\"predictionType\":\"watermark\",\"displayName\":\"watermarkpresent\",\"present\":false,\"confidence\":40},{\"predictionType\":\"nsfw\",\"displayName\":\"nsfwpresent\",\"present\":true,\"confidence\":80}]}],\"success\":true,\"errorMessage\":null}";
  private static final String USER_FEEDBACK =
      "{\"imageFeedback\":[{\"locationPath\":\"path1\",\"userFeedback\":[\"Text\",\"Blur\"]}]}";
  private static final String USER_FEEDBACK_2 =
      "{\"userFeedback\":[{\"locationPath\":\"/filestore/mta/images/source/locationPath\",\"userPrediction\":[\"Text\"]}],\"otherModelFeedBack\":[\"Google restriction\"]}";
  private static final String USER_FEEDBACK_3 =
      "{\"userFeedback\":[{\"locationPath\":\"/filestore/mta/images/source/locationPath\",\"userPrediction\":[\"Text\"]}]}";
  private static final String USER_FEEDBACK_1 =
      "{\"userFeedback\":[{\"locationPath\":\"/filestore/mta/images/source/locationPath\",\"userPrediction\":[\"Text\"]}],\"otherModelFeedBack\":null}";
  private static final String USER_FEEDBACK_4 =
      "{\"userFeedback\":null,\"otherModelFeedBack\":[\"Google restriction\"]}";
  private static final String USER_FEEDBACK_5 = "{\"userFeedback\":null,\"otherModelFeedBack\":null}";

  private ProductImageQcFeedback productImageQcFeedback;
  private ProductImageQcFeedbackRequest productImageQcFeedbackRequest;

  @InjectMocks
  private ProductImageQcFeedbackServiceImpl productImageQcFeedbackService;

  @Mock
  private ProductImageQcFeedbackRepository productImageQcFeedbackRepository;

  @Captor
  private ArgumentCaptor<ProductImageQcFeedback> productImageQcFeedbackArgumentCaptor;

  @BeforeEach
  public void setUp() throws Exception {
    productImageQcFeedback = new ProductImageQcFeedback();
    productImageQcFeedback.setProductCode(PRODUCT_CODE);
    productImageQcFeedback.setSystemFeedback(SYSTEM_FEEEDBACK);
    productImageQcFeedback.setUserFeedback(USER_FEEDBACK);
    productImageQcFeedback.setFeedbackUpdated(true);
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, STORE_ID);

    productImageQcFeedbackRequest = new ProductImageQcFeedbackRequest();
    productImageQcFeedbackRequest.setProductCode(PRODUCT_CODE);
    productImageQcFeedbackRequest.setUserFeedback(USER_FEEDBACK);
    productImageQcFeedbackRequest.setSystemFeedback(SYSTEM_FEEEDBACK);
    productImageQcFeedbackRequest.setFeedbackUpdated(true);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(productImageQcFeedbackRepository);
  }

  @Test
   void upsertImageQcFeedbackInsertTest() throws Exception {
    productImageQcFeedback.setUserFeedback(USER_FEEDBACK_2);
    Mockito.when(productImageQcFeedbackRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(productImageQcFeedback);
    productImageQcFeedbackRequest.setUserFeedback(USER_FEEDBACK_1);
    productImageQcFeedbackService.upsertImageQcFeedback(productImageQcFeedbackRequest, true, true);
    Mockito.verify(productImageQcFeedbackRepository).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productImageQcFeedbackRepository).saveAndFlush(productImageQcFeedbackArgumentCaptor.capture());
    Assertions.assertEquals(STORE_ID, productImageQcFeedbackArgumentCaptor.getValue().getStoreId());
    Assertions.assertEquals(PRODUCT_CODE,
        productImageQcFeedbackArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(SYSTEM_FEEEDBACK,
        productImageQcFeedbackArgumentCaptor.getValue().getSystemFeedback());
    Assertions.assertTrue(productImageQcFeedbackArgumentCaptor.getValue().isFeedbackUpdated());
  }

  @Test
   void upsertImageQcFeedbackInsertUserFeedBackNullTest() throws Exception {
    productImageQcFeedback.setUserFeedback(null);
    Mockito.when(productImageQcFeedbackRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(productImageQcFeedback);
    productImageQcFeedbackRequest.setUserFeedback(USER_FEEDBACK_1);
    productImageQcFeedbackService.upsertImageQcFeedback(productImageQcFeedbackRequest, true, true);
    Mockito.verify(productImageQcFeedbackRepository).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productImageQcFeedbackRepository).saveAndFlush(productImageQcFeedbackArgumentCaptor.capture());
    Assertions.assertEquals(STORE_ID, productImageQcFeedbackArgumentCaptor.getValue().getStoreId());
    Assertions.assertEquals(PRODUCT_CODE,
        productImageQcFeedbackArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(SYSTEM_FEEEDBACK,
        productImageQcFeedbackArgumentCaptor.getValue().getSystemFeedback());
    Assertions.assertTrue(productImageQcFeedbackArgumentCaptor.getValue().isFeedbackUpdated());
  }

  @Test
   void upsertImageQcFeedbackUserFeedBackNullTest() throws Exception {
    productImageQcFeedback.setUserFeedback(USER_FEEDBACK_2);
    Mockito.when(productImageQcFeedbackRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(productImageQcFeedback);
    productImageQcFeedbackRequest.setUserFeedback(USER_FEEDBACK_4);
    productImageQcFeedbackService.upsertImageQcFeedback(productImageQcFeedbackRequest, true, true);
    Mockito.verify(productImageQcFeedbackRepository).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productImageQcFeedbackRepository).saveAndFlush(productImageQcFeedbackArgumentCaptor.capture());
    Assertions.assertEquals(STORE_ID, productImageQcFeedbackArgumentCaptor.getValue().getStoreId());
    Assertions.assertEquals(PRODUCT_CODE,
        productImageQcFeedbackArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(SYSTEM_FEEEDBACK,
        productImageQcFeedbackArgumentCaptor.getValue().getSystemFeedback());
    Assertions.assertTrue(productImageQcFeedbackArgumentCaptor.getValue().isFeedbackUpdated());
  }

  @Test
   void upsertImageQcFeedbackUserNullTest() throws Exception {
    try {
      productImageQcFeedback.setUserFeedback(USER_FEEDBACK_2);
      Mockito.when(productImageQcFeedbackRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
          .thenReturn(productImageQcFeedback);
      productImageQcFeedbackRequest.setUserFeedback(USER_FEEDBACK_5);
      Assertions.assertThrows(Exception.class,
        () -> productImageQcFeedbackService.upsertImageQcFeedback(productImageQcFeedbackRequest,
          true, true));
    } finally {
      Mockito.verify(productImageQcFeedbackRepository).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    }
  }

  @Test
   void upsertImageQcFeedbackUserBothValuesNonNullTest() throws Exception {
    try {
      productImageQcFeedback.setUserFeedback(USER_FEEDBACK_2);
      Mockito.when(productImageQcFeedbackRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
          .thenReturn(productImageQcFeedback);
      productImageQcFeedbackRequest.setUserFeedback(USER_FEEDBACK_2);
      Assertions.assertThrows(Exception.class,
        () -> productImageQcFeedbackService.upsertImageQcFeedback(productImageQcFeedbackRequest,
          true, true));
    } finally {
      Mockito.verify(productImageQcFeedbackRepository).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    }
  }

  @Test
   void upsertImageQcFeedbackUserFeedbackRequestOldTest() throws Exception {
    productImageQcFeedback.setUserFeedback(USER_FEEDBACK_3);
    Mockito.when(productImageQcFeedbackRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(productImageQcFeedback);
    productImageQcFeedbackRequest.setUserFeedback(USER_FEEDBACK_1);
    productImageQcFeedbackService.upsertImageQcFeedback(productImageQcFeedbackRequest, true, true);
    Mockito.verify(productImageQcFeedbackRepository).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productImageQcFeedbackRepository).saveAndFlush(productImageQcFeedbackArgumentCaptor.capture());
    Assertions.assertEquals(STORE_ID, productImageQcFeedbackArgumentCaptor.getValue().getStoreId());
    Assertions.assertEquals(PRODUCT_CODE,
        productImageQcFeedbackArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(SYSTEM_FEEEDBACK,
        productImageQcFeedbackArgumentCaptor.getValue().getSystemFeedback());
    Assertions.assertTrue(productImageQcFeedbackArgumentCaptor.getValue().isFeedbackUpdated());
  }

  @Test
   void upsertImageQcFeedbackUserFeedbackRequestOlduserFeedbackNullTest() throws Exception {
    productImageQcFeedback.setUserFeedback(USER_FEEDBACK_3);
    Mockito.when(productImageQcFeedbackRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(productImageQcFeedback);
    productImageQcFeedbackRequest.setUserFeedback(USER_FEEDBACK_4);
    productImageQcFeedbackService.upsertImageQcFeedback(productImageQcFeedbackRequest, true, true);
    Mockito.verify(productImageQcFeedbackRepository).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productImageQcFeedbackRepository).saveAndFlush(productImageQcFeedbackArgumentCaptor.capture());
    Assertions.assertEquals(STORE_ID, productImageQcFeedbackArgumentCaptor.getValue().getStoreId());
    Assertions.assertEquals(PRODUCT_CODE,
        productImageQcFeedbackArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(SYSTEM_FEEEDBACK,
        productImageQcFeedbackArgumentCaptor.getValue().getSystemFeedback());
    Assertions.assertTrue(productImageQcFeedbackArgumentCaptor.getValue().isFeedbackUpdated());
  }

  @Test
   void upsertImageQcFeedbackUpdateTest() throws Exception {
    productImageQcFeedback.setUserFeedback(USER_FEEDBACK_2);
    Mockito.when(productImageQcFeedbackRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(productImageQcFeedback);
    productImageQcFeedbackRequest.setSystemFeedback(NEW_SYSTEM_FEEEDBACK);
    productImageQcFeedbackRequest.setUserFeedback(USER_FEEDBACK_1);
    productImageQcFeedbackService.upsertImageQcFeedback(productImageQcFeedbackRequest, true, true);
    Mockito.verify(productImageQcFeedbackRepository).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productImageQcFeedbackRepository).saveAndFlush(productImageQcFeedbackArgumentCaptor.capture());
    Assertions.assertEquals(STORE_ID, productImageQcFeedbackArgumentCaptor.getValue().getStoreId());
    Assertions.assertEquals(PRODUCT_CODE,
        productImageQcFeedbackArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(NEW_SYSTEM_FEEEDBACK,
        productImageQcFeedbackArgumentCaptor.getValue().getSystemFeedback());
    Assertions.assertEquals(USER_FEEDBACK_2,
        productImageQcFeedbackArgumentCaptor.getValue().getUserFeedback());
    Assertions.assertTrue(productImageQcFeedbackArgumentCaptor.getValue().isFeedbackUpdated());
  }

  @Test
   void findProductQcFeedbackResponseByProductCodeTest() {
    Mockito.when(productImageQcFeedbackRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(productImageQcFeedback);
    ProductImageQcFeedbackResponse response =
        productImageQcFeedbackService.findProductQcFeedbackResponseByProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productImageQcFeedbackRepository).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Assertions.assertEquals(PRODUCT_CODE, response.getProductCode());
    Assertions.assertEquals(SYSTEM_FEEEDBACK, response.getSystemFeedback());
    Assertions.assertEquals(USER_FEEDBACK, response.getUserFeedback());
  }

  @Test
   void findProductQcFeedbackResponseByProductCode_NullTest() {
    Mockito.when(productImageQcFeedbackRepository.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(null);
    ProductImageQcFeedbackResponse response =
        productImageQcFeedbackService.findProductQcFeedbackResponseByProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productImageQcFeedbackRepository).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Assertions.assertNull(response);
  }
}

package com.gdn.partners.pbp.service.listener;

import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.verify;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.test.util.ReflectionTestUtils;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.micro.graphics.domain.event.model.ScaleEditedImagesResponse;
import com.gdn.micro.graphics.domain.event.model.ScaleImageResponse;
import com.gdn.mta.product.entity.ProductHistory;
import com.gdn.mta.product.service.ProductService;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.workflow.product.ProductWfService;

public class ScaleRevisedImageKafkaSubscriberBeanTest {

  private static final String HASH_CODE = "hashCode";
  private static final String PRODUCT_CODE = "productCode";
  private static final String IMAGE_LOCATION = "imageLocation";
  private static final String STORE_ID = "10001";
  private static final String CLIENT_ID = "clientId";
  private static final String USER_NAME = "userName";

  @InjectMocks
  private ScaleRevisedImageKafkaSubscriberBean scaleRevisedImageKafkaSubscriberBean;

  private ScaleEditedImagesResponse scaleEditedImagesResponse;
  private ObjectMapper mapper;

  @Mock
  private ProductService productService;

  @Mock
  private ProductWfService productWfService;

  @Mock
  private ObjectMapper objectMapper;

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
    ReflectionTestUtils
        .setField(this.scaleRevisedImageKafkaSubscriberBean, GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
            "channelId");
    ReflectionTestUtils
        .setField(this.scaleRevisedImageKafkaSubscriberBean, GdnMandatoryRequestParameterUtil.AUTHENTICATOR_KEY, "");
    ScaleImageResponse scaleImageResponse =
        ScaleImageResponse.builder().hashCode(HASH_CODE).clientId(CLIENT_ID).imagePathLocation(IMAGE_LOCATION)
            .errorMessage(null).isActive(true).success(true).build();
    ScaleImageResponse scaleImageResponse1 =
        ScaleImageResponse.builder().hashCode(HASH_CODE).clientId(CLIENT_ID).imagePathLocation(null).errorMessage(null)
            .isActive(false).success(false).build();
    List<ScaleImageResponse> scaleImageResponses = new ArrayList<>();
    scaleImageResponses.add(scaleImageResponse);
    scaleImageResponses.add(scaleImageResponse1);
    this.scaleEditedImagesResponse =
        ScaleEditedImagesResponse.builder().imageResponses(scaleImageResponses).productCode(PRODUCT_CODE)
            .storeId(STORE_ID).username(USER_NAME).build();
    mapper = new ObjectMapper();
    Mockito.doNothing().when(productService)
        .saveProductHistory(Mockito.eq(PRODUCT_CODE), Mockito.any(ProductHistory.class));
  }

  @AfterEach
  public void finalizeTest() throws Exception {
    Mockito.verifyNoMoreInteractions(this.productWfService, productService, objectMapper);
  }

  @Test
  public void listenWithNullObject() throws Exception {
    String message = mapper.writeValueAsString(scaleEditedImagesResponse);
    scaleEditedImagesResponse.setProductCode(null);
    scaleEditedImagesResponse.getImageResponses().get(1).setSuccess(true);
    scaleEditedImagesResponse.getImageResponses().add(null);
    doThrow(Exception.class).when(productWfService)
        .approveImageForRevisedProduct(STORE_ID, PRODUCT_CODE, scaleEditedImagesResponse.getImageResponses(), false,
            new ArrayList<>());
    Mockito.when(objectMapper.readValue(message, ScaleEditedImagesResponse.class))
        .thenReturn(scaleEditedImagesResponse);
    try {
      this.scaleRevisedImageKafkaSubscriberBean.onDomainEventConsumed(message);
    } finally {
      verify(objectMapper).readValue(message, ScaleEditedImagesResponse.class);
    }
  }

  @Test
  public void listenWith_SuccessFalse() throws Exception {
    String message = mapper.writeValueAsString(scaleEditedImagesResponse);
    Mockito.when(objectMapper.readValue(message, ScaleEditedImagesResponse.class))
        .thenReturn(scaleEditedImagesResponse);
    this.scaleRevisedImageKafkaSubscriberBean.onDomainEventConsumed(message);
    verify(objectMapper).readValue(message, ScaleEditedImagesResponse.class);
    Mockito.verify(this.productService).saveProductHistory(Mockito.eq(scaleEditedImagesResponse.getStoreId()),
        Mockito.eq(scaleEditedImagesResponse.getProductCode()), Mockito.eq(Constants.DEFAULT_USERNAME),
        Mockito.anyString(), Mockito.eq(null));
  }

  @Test
  public void listenWith_SuccessTrue() throws Exception {
    scaleEditedImagesResponse.getImageResponses().get(1).setSuccess(true);
    String message = mapper.writeValueAsString(scaleEditedImagesResponse);
    Mockito.when(objectMapper.readValue(message, ScaleEditedImagesResponse.class))
        .thenReturn(scaleEditedImagesResponse);
    doNothing().when(productWfService)
        .approveImageForRevisedProduct(STORE_ID, PRODUCT_CODE, scaleEditedImagesResponse.getImageResponses(), false,
            new ArrayList<>());
    this.scaleRevisedImageKafkaSubscriberBean.onDomainEventConsumed(message);
    verify(objectMapper).readValue(message, ScaleEditedImagesResponse.class);
    Mockito.verify(this.productWfService)
        .approveImageForRevisedProduct(STORE_ID, PRODUCT_CODE, scaleEditedImagesResponse.getImageResponses(), false,
            new ArrayList<>());
  }

  @Test
  public void listenWith_ProductCodeEmpty() throws Exception {
    scaleEditedImagesResponse.setProductCode(PRODUCT_CODE);
    String message = mapper.writeValueAsString(scaleEditedImagesResponse);
    scaleEditedImagesResponse.getImageResponses().get(1).setSuccess(true);
    scaleEditedImagesResponse.getImageResponses().add(null);
    doThrow(Exception.class).when(productWfService)
        .approveImageForRevisedProduct(STORE_ID, PRODUCT_CODE, scaleEditedImagesResponse.getImageResponses(), false,
            new ArrayList<>());
    Mockito.when(objectMapper.readValue(message, ScaleEditedImagesResponse.class))
        .thenReturn(scaleEditedImagesResponse);
    try {
      this.scaleRevisedImageKafkaSubscriberBean.onDomainEventConsumed(message);
    } finally {
      verify(objectMapper).readValue(message, ScaleEditedImagesResponse.class);
      verify(productService).setReviewPendingFlagToTrue(STORE_ID, PRODUCT_CODE);
    }
  }

  @Test
  public void listen_emptyResponseTest() throws Exception {
    String message = mapper.writeValueAsString(scaleEditedImagesResponse);
    scaleEditedImagesResponse.setImageResponses(Collections.EMPTY_LIST);
    Mockito.when(objectMapper.readValue(message, ScaleEditedImagesResponse.class)).thenReturn(scaleEditedImagesResponse);
    this.scaleRevisedImageKafkaSubscriberBean.onDomainEventConsumed(message);
    verify(objectMapper).readValue(message, ScaleEditedImagesResponse.class);
  }
}

package com.gdn.partners.pbp.service.listener;

import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;

import java.util.Collections;

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
import org.slf4j.MDC;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.response.BrandPredictionResponse;
import com.gda.mta.product.dto.response.BrandRestrictedModelsResponse;
import com.gda.mta.product.dto.response.ImageQcPredictionResponse;
import com.gda.mta.product.dto.response.RestrictionModelsResponse;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.domain.event.modal.ImageQcResponseDomainEvent;
import com.gdn.mta.product.service.ProductService;
import com.gdn.mta.product.service.ProductServiceWrapper;
import com.gdn.partners.pbp.commons.constants.Constants;

public class ImageQcKafkaSubscriberBeanTest {

  private static final String STORE_ID = "10001";
  private static final String PRODUCT_CODE = "productCode";
  private static final String BRAND = "brand";
  private ObjectMapper mapper;

  private ImageQcResponseDomainEvent imageQcResponseDomainEvent;

  @InjectMocks
  private ImageQcKafkaSubscriberBean imageQcKafkaSubscriberBean;

  @Mock
  private ProductServiceWrapper productServiceWrapper;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private ProductService productService;

  @Captor
  private ArgumentCaptor<ImageQcResponseDomainEvent> imageQcResponseDomainEventArgumentCaptor;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    imageQcResponseDomainEvent = new ImageQcResponseDomainEvent();
    imageQcResponseDomainEvent.setProductCode(PRODUCT_CODE);
    RestrictionModelsResponse restrictionModelsResponse = new RestrictionModelsResponse();
    restrictionModelsResponse.setPredictionType(PRODUCT_CODE);
    ImageQcPredictionResponse imageQcPredictionResponse = new ImageQcPredictionResponse();
    imageQcPredictionResponse.setConfidence(10);
    restrictionModelsResponse.setPredictions(Collections.singletonList(imageQcPredictionResponse));
    imageQcResponseDomainEvent.setRestrictionModels(Collections.singletonList(restrictionModelsResponse));
    BrandRestrictedModelsResponse brandRestrictedModelsResponse = new BrandRestrictedModelsResponse();
    BrandPredictionResponse brandPredictionResponse = new BrandPredictionResponse();
    brandPredictionResponse.setBrandName(BRAND);
    brandRestrictedModelsResponse.setPredictions(Collections.singletonList(brandPredictionResponse));
    imageQcResponseDomainEvent.setBrandModels(Collections.singletonList(brandRestrictedModelsResponse));
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, STORE_ID);
    mapper = new ObjectMapper();
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(productServiceWrapper, objectMapper);
    Mockito.verifyNoMoreInteractions(productService);
  }

  @Test
  public void onDomainEventConsumedTest() throws Exception {
    String message = mapper.writeValueAsString(imageQcResponseDomainEvent);
    Mockito.when(objectMapper.readValue(message, ImageQcResponseDomainEvent.class))
        .thenReturn(imageQcResponseDomainEvent);
    this.imageQcKafkaSubscriberBean.onDomainEventConsumed(message);
    Mockito.verify(productServiceWrapper)
        .processImageQcResponse(eq(STORE_ID), imageQcResponseDomainEventArgumentCaptor.capture());
    verify(objectMapper).readValue(message, ImageQcResponseDomainEvent.class);
    Assertions.assertEquals(PRODUCT_CODE, imageQcResponseDomainEventArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(BRAND,
        imageQcResponseDomainEventArgumentCaptor.getValue().getBrandModels().get(0).getPredictions().get(0)
            .getBrandName());
  }

  @Test
  public void onDomainEventConsumedImageQcResponseProductCodeNullTest() throws Exception {
    String message = mapper.writeValueAsString("{}");
    Mockito.when(objectMapper.readValue(message, ImageQcResponseDomainEvent.class))
        .thenReturn(new ImageQcResponseDomainEvent());
    this.imageQcKafkaSubscriberBean.onDomainEventConsumed(message);
    verify(objectMapper).readValue(message, ImageQcResponseDomainEvent.class);
  }

  @Test
  public void onDomainEventConsumedBacklogTest() throws Exception {
    imageQcResponseDomainEvent.setProductCode(Constants.BACKLOG + PRODUCT_CODE);
    String message = mapper.writeValueAsString(imageQcResponseDomainEvent);
    Mockito.when(objectMapper.readValue(message, ImageQcResponseDomainEvent.class))
        .thenReturn(imageQcResponseDomainEvent);
    this.imageQcKafkaSubscriberBean.onDomainEventConsumed(message);
    verify(objectMapper).readValue(message, ImageQcResponseDomainEvent.class);
    verify(productService).publishImageQcBacklogRequestEvent(imageQcResponseDomainEvent);
  }

  @Test
  public void onDomainEventConsumedExceptionTest() throws Exception {
    Mockito.doThrow(Exception.class).when(productServiceWrapper)
        .processImageQcResponse(STORE_ID, imageQcResponseDomainEvent);
    String message = mapper.writeValueAsString(imageQcResponseDomainEvent);
    Mockito.when(objectMapper.readValue(message, ImageQcResponseDomainEvent.class))
        .thenReturn(imageQcResponseDomainEvent);
    this.imageQcKafkaSubscriberBean.onDomainEventConsumed(message);
    Mockito.verify(productServiceWrapper)
        .processImageQcResponse(eq(STORE_ID), imageQcResponseDomainEventArgumentCaptor.capture());
    verify(objectMapper).readValue(message, ImageQcResponseDomainEvent.class);
    Assertions.assertEquals(PRODUCT_CODE, imageQcResponseDomainEventArgumentCaptor.getValue().getProductCode());
  }
}
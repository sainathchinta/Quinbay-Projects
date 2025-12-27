package com.gdn.partners.pbp.service.listener;

import static org.mockito.Mockito.verify;

import java.util.Arrays;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.micro.graphics.domain.event.model.BulkImageProcessResponse;
import com.gdn.micro.graphics.domain.event.model.ImageResponse;
import com.gdn.mta.product.service.ProductServiceWrapper;

public class RevisedImageResizeStatusKafkaSubscriberBeanTest {

  public static final String STORE_ID = "storeId";
  public static final String PRODUCT_CODE = "PRODUCT_CODE";
  public static final String USER_NAME = "USER_NAME";

  @Mock
  private ProductServiceWrapper productServiceWrapper;

  @Mock
  private ObjectMapper objectMapper;

  @InjectMocks
  private RevisedImageResizeStatusKafkaSubscriberBean revisedImageResizeStatusKafkaSubscriberBean;

  private BulkImageProcessResponse bulkImageProcessResponse;
  private ObjectMapper mapper;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    bulkImageProcessResponse = new BulkImageProcessResponse();
    bulkImageProcessResponse.setUsername(USER_NAME);
    bulkImageProcessResponse.setStoreId(STORE_ID);
    bulkImageProcessResponse.setGroupCode(PRODUCT_CODE);
    ImageResponse imageResponse = new ImageResponse();
    imageResponse.setSuccess(true);
    ImageResponse imageResponse1 = new ImageResponse();
    imageResponse1.setSuccess(true);
    ImageResponse imageResponse2 = new ImageResponse();
    imageResponse2.setSuccess(true);
    bulkImageProcessResponse.setImageResponses(Arrays.asList(imageResponse, imageResponse1, imageResponse2));
    mapper = new ObjectMapper();
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(productServiceWrapper, objectMapper);
  }

  @Test
  public void onDomainEventConsumedTest() throws Exception {
    String message = mapper.writeValueAsString(bulkImageProcessResponse);
    Mockito.when(objectMapper.readValue(message, BulkImageProcessResponse.class)).thenReturn(bulkImageProcessResponse);
    Mockito.doNothing().when(productServiceWrapper).updateImagePathsForEditedResizeImages(bulkImageProcessResponse, true);
    this.revisedImageResizeStatusKafkaSubscriberBean.onDomainEventConsumed(message);
    Mockito.verify(productServiceWrapper).updateImagePathsForEditedResizeImages(bulkImageProcessResponse, true);
    verify(objectMapper).readValue(message, BulkImageProcessResponse.class);
    Assertions.assertEquals(USER_NAME, GdnMandatoryRequestParameterUtil.getUsername());
  }

  @Test
  public void onDomainEventConsumedTestFail() throws Exception {
    String message = mapper.writeValueAsString(bulkImageProcessResponse);
    Mockito.when(objectMapper.readValue(message, BulkImageProcessResponse.class)).thenReturn(bulkImageProcessResponse);
    Mockito.doThrow(Exception.class).when(productServiceWrapper)
        .updateImagePathsForEditedResizeImages(bulkImageProcessResponse, true);
    this.revisedImageResizeStatusKafkaSubscriberBean.onDomainEventConsumed(message);
    Mockito.verify(productServiceWrapper).updateImagePathsForEditedResizeImages(bulkImageProcessResponse, true);
    verify(objectMapper).readValue(message, BulkImageProcessResponse.class);
  }

}

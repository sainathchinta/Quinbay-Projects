package com.gdn.partners.pbp.service.listener;

import static org.mockito.Mockito.verify;

import java.util.ArrayList;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.micro.graphics.web.model.BulkResizeImageRequest;
import com.gdn.mta.domain.event.modal.EditedImageResizeEvent;
import com.gdn.mta.product.service.ImageProcessorService;

public class ReviseImageResizeKafkaSubscriberBeanTest {
  public static final String STORE_ID = "storeId";
  public static final String PRODUCT_CODE = "productCode";
  private ObjectMapper mapper;

  @InjectMocks
  private ReviseImageResizeKafkaSubscriberBean reviseImageResizeKafkaSubscriberBean;

  @Mock
  private ImageProcessorService imageProcessorService;

  @Mock
  private ObjectMapper objectMapper;

  private EditedImageResizeEvent editedImageResizeEvent;

  private BulkResizeImageRequest bulkResizeImageRequest;

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
    editedImageResizeEvent = new EditedImageResizeEvent();
    editedImageResizeEvent.setStoreId(STORE_ID);
    editedImageResizeEvent.setProductCode(PRODUCT_CODE);
    editedImageResizeEvent.setImageRequests(new ArrayList<>());
    bulkResizeImageRequest = new BulkResizeImageRequest();
    bulkResizeImageRequest.setGroupCode(PRODUCT_CODE);
    bulkResizeImageRequest.setImageRequests(new ArrayList<>());
    mapper = new ObjectMapper();
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(imageProcessorService, objectMapper);
  }

  @Test
  public void onDomainEventConsumed() throws Exception {
    Mockito.doNothing().when(imageProcessorService).resizeRevisedImage(bulkResizeImageRequest);
    String message = mapper.writeValueAsString(editedImageResizeEvent);
    Mockito.when(objectMapper.readValue(message, EditedImageResizeEvent.class)).thenReturn(editedImageResizeEvent);
    this.reviseImageResizeKafkaSubscriberBean.onDomainEventConsumed(message);
    Mockito.verify(imageProcessorService).resizeRevisedImage(bulkResizeImageRequest);
    verify(objectMapper).readValue(message, EditedImageResizeEvent.class);
  }

  @Test
  public void onDomainEventConsumedExceptionTest() throws Exception {
    Mockito.doThrow(ApplicationRuntimeException.class).when(imageProcessorService)
        .resizeRevisedImage(bulkResizeImageRequest);
    String message = mapper.writeValueAsString(editedImageResizeEvent);
    Mockito.when(objectMapper.readValue(message, EditedImageResizeEvent.class)).thenReturn(editedImageResizeEvent);
    this.reviseImageResizeKafkaSubscriberBean.onDomainEventConsumed(message);
    Mockito.verify(imageProcessorService).resizeRevisedImage(bulkResizeImageRequest);
    verify(objectMapper).readValue(message, EditedImageResizeEvent.class);
  }
}

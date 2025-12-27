package com.gdn.micro.graphics.service.listener;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.micro.graphics.config.KafkaTopicProperties;
import com.gdn.micro.graphics.domain.event.config.DomainEventName;
import com.gdn.micro.graphics.model.ImageProcessingModel;
import com.gdn.micro.graphics.model.ResizeImageScalingModel;
import com.gdn.micro.graphics.service.AsyncGraphicsProcessService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import java.io.IOException;
import java.util.concurrent.ExecutionException;

public class ResizeImageScalingListenerTest {

  private final String ORDER = "xorder";
  ResizeImageScalingModel resizeImageScalingModel = new ResizeImageScalingModel();
  @InjectMocks
  private ResizeImageScalingListener resizeImageScalingListener;
  @Mock
  private ObjectMapper objectMapper;
  @Mock
  private KafkaTopicProperties kafkaTopicProperties;
  @Mock
  private AsyncGraphicsProcessService asyncGraphicsProcessService;
  private String message;

  @BeforeEach
  public void setUp() throws IOException {
    ObjectMapper mapper = new ObjectMapper();
    MockitoAnnotations.initMocks(this);
    resizeImageScalingModel.setClientId(ORDER);
    message = mapper.writeValueAsString(resizeImageScalingModel);
  }

  @Test
  void onDomainEventConsumedBulkImageScalingTest() throws Exception {
    Mockito.when(kafkaTopicProperties.getImageResizeScaling())
      .thenReturn(DomainEventName.PROCESS_BULK_IMAGE_RESIZE_PRIORITY_1_EVENT);
    Mockito.when(objectMapper.readValue(message, ResizeImageScalingModel.class))
      .thenReturn(resizeImageScalingModel);
    resizeImageScalingListener.onDomainEventConsumedImageScalingAndUpload(message);
    Mockito.verify(kafkaTopicProperties).getImageResizeScaling();
    Mockito.verify(asyncGraphicsProcessService).scaleListOfImages(resizeImageScalingModel);
  }

  @Test
  void onDomainEventConsumedBulkImageScalingExceptionTest() throws Exception {
    Mockito.when(kafkaTopicProperties.getImageResizeScaling())
      .thenReturn(DomainEventName.PROCESS_BULK_IMAGE_RESIZE_PRIORITY_1_EVENT);
    Mockito.when(objectMapper.readValue(message, ResizeImageScalingModel.class))
      .thenReturn(resizeImageScalingModel);
    Mockito.doThrow(ExecutionException.class).when(asyncGraphicsProcessService)
      .scaleListOfImages(resizeImageScalingModel);
    resizeImageScalingListener.onDomainEventConsumedImageScalingAndUpload(message);
    Mockito.verify(kafkaTopicProperties).getImageResizeScaling();
    Mockito.verify(asyncGraphicsProcessService).scaleListOfImages(resizeImageScalingModel);
  }
}

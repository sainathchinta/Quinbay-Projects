package com.gdn.micro.graphics.service.listener;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.ExecutionException;

import com.gdn.micro.graphics.service.GcsService;
import com.gdn.micro.graphics.service.config.GcsProperties;
import com.google.cloud.storage.Bucket;
import com.gdn.micro.graphics.model.GraphicDetailCommand;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.micro.graphics.config.KafkaTopicProperties;
import com.gdn.micro.graphics.domain.event.config.DomainEventName;
import com.gdn.micro.graphics.model.GraphicImageDetail;
import com.gdn.micro.graphics.model.ImageProcessingModel;
import com.gdn.micro.graphics.model.ImageScalingAndUploadModel;
import com.gdn.micro.graphics.service.AsyncGraphicsProcessService;
import com.gdn.micro.graphics.service.AsyncGraphicsProcessorServiceWrapper;
import com.gdn.micro.graphics.web.model.ScaleEditedImageRequest;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;

public class ProcessImageScalingListenerTest {
  private static final String CLIENT_ID = "clientId";
  private static final String REQUEST_ID = "requestId";

  @InjectMocks
  private ProcessImageScalingListener processImageScalingListener;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private AsyncGraphicsProcessService service;

  @Mock
  private AsyncGraphicsProcessorServiceWrapper asyncGraphicsProcessorServiceWrapper;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @Mock
  private GcsProperties gcsProperties;

  @Mock
  private GcsService gcsService;

  @Mock
  private Bucket rmaImageBucket;


  private String payload;
  private String payload2;
  private String payload3;
  private ImageScalingAndUploadModel imageScalingAndUploadModel;
  private ImageProcessingModel imageProcessingModel;
  private List<GraphicImageDetail> graphicImageDetails;
  private String groupCode;
  private String username;
  private String storeId;
  private String clientId;
  private boolean isResize;
  private boolean isEdited;
  private boolean isRevised;
  private ScaleEditedImageRequest request;
  private final String PATH = "/path";

  @BeforeEach
  public void setUp() throws IOException {
    MockitoAnnotations.initMocks(this);

    ObjectMapper mapper = new ObjectMapper();
    imageScalingAndUploadModel = new ImageScalingAndUploadModel();
    imageScalingAndUploadModel.setGraphicDetailCommands(new ArrayList<>());
    imageScalingAndUploadModel.setUploadToGcs(true);
    imageScalingAndUploadModel.setClientId(CLIENT_ID);
    imageScalingAndUploadModel.setRequestId(REQUEST_ID);
    imageProcessingModel =
        mapper.readValue(new File("src/test/resources/event/ScaleImagePayload.json"), ImageProcessingModel.class);
    payload = mapper.writeValueAsString(imageProcessingModel);
    payload2 = mapper.writeValueAsString(imageScalingAndUploadModel);
    GraphicDetailCommand graphicDetailCommand = new GraphicDetailCommand(null,PATH,PATH,
        null,PATH,true) ;
    imageScalingAndUploadModel.setGraphicDetailCommands(Collections.singletonList(graphicDetailCommand));
    payload3 = mapper.writeValueAsString(imageScalingAndUploadModel);
    graphicImageDetails = imageProcessingModel.getGraphicImageDetails();
    groupCode = imageProcessingModel.getGroupCode();
    username = imageProcessingModel.getUsername();
    storeId = imageProcessingModel.getStoreId();
    clientId = imageProcessingModel.getClientId();
    isResize = imageProcessingModel.isResize();
    isEdited = imageProcessingModel.isEdited();
    isRevised = imageProcessingModel.isRevised();
    request = imageProcessingModel.getRequest();
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(objectMapper, service, kafkaTopicProperties, gcsService, gcsProperties, rmaImageBucket);
  }

  @Test
  void onDomainEventConsumedBulkImageScalingTest() throws IOException {
    Mockito.when(kafkaTopicProperties.getImageScalingNoPriority())
        .thenReturn(DomainEventName.PROCESS_BULK_IMAGE_SCALING);
    Mockito.when(objectMapper.readValue(payload, ImageProcessingModel.class)).thenReturn(imageProcessingModel);
    Mockito.doNothing().when(service)
        .processImages(graphicImageDetails, groupCode, username, storeId, clientId, isResize, isEdited, isRevised,
            request, 0);
    processImageScalingListener.onDomainEventConsumedBulkImageScaling(payload);
    Mockito.verify(objectMapper).readValue(payload, ImageProcessingModel.class);
    Mockito.verify(service)
        .processImages(graphicImageDetails, groupCode, username, storeId, clientId, isResize, isEdited, isRevised,
            request, 0);
    Mockito.verify(kafkaTopicProperties).getImageScalingNoPriority();
  }

  @Test
  void onDomainEventConsumedBulkImageScalingErrorTest() throws IOException {
    Mockito.when(kafkaTopicProperties.getImageScalingNoPriority())
        .thenReturn(DomainEventName.PROCESS_BULK_IMAGE_SCALING);
    Mockito.when(objectMapper.readValue(payload, ImageProcessingModel.class)).thenReturn(imageProcessingModel);
    Mockito.doThrow(IllegalStateException.class).when(service)
        .processImages(graphicImageDetails, groupCode, username, storeId, clientId, isResize, isEdited, isRevised,
            request, 0);
    processImageScalingListener.onDomainEventConsumedBulkImageScaling(payload);
    Mockito.verify(objectMapper).readValue(payload, ImageProcessingModel.class);
    Mockito.verify(service)
        .processImages(graphicImageDetails, groupCode, username, storeId, clientId, isResize, isEdited, isRevised,
            request, 0);
    Mockito.verify(kafkaTopicProperties).getImageScalingNoPriority();
  }

  @Test
  void onDomainEventConsumedBulkImageScalingPriority1Test() throws IOException {
    imageProcessingModel.setPrioritySeller(1);
    Mockito.when(kafkaTopicProperties.getImageScalingPriority1())
        .thenReturn(DomainEventName.PROCESS_BULK_IMAGE_SCALING_PRIORITY_1);
    Mockito.when(objectMapper.readValue(payload, ImageProcessingModel.class)).thenReturn(imageProcessingModel);
    Mockito.doNothing().when(service)
        .processImages(graphicImageDetails, groupCode, username, storeId, clientId, isResize, isEdited, isRevised,
            request, 1);
    processImageScalingListener.onDomainEventConsumedBulkImageScalingPriority1(payload);
    Mockito.verify(objectMapper).readValue(payload, ImageProcessingModel.class);
    Mockito.verify(service)
        .processImages(graphicImageDetails, groupCode, username, storeId, clientId, isResize, isEdited, isRevised,
            request, 1);
    Mockito.verify(kafkaTopicProperties).getImageScalingPriority1();
  }

  @Test
  void onDomainEventConsumedBulkImageScalingPriority1ErrorTest() throws IOException {
    imageProcessingModel.setPrioritySeller(1);
    Mockito.when(kafkaTopicProperties.getImageScalingPriority1())
        .thenReturn(DomainEventName.PROCESS_BULK_IMAGE_SCALING_PRIORITY_1);
    Mockito.when(objectMapper.readValue(payload, ImageProcessingModel.class)).thenReturn(imageProcessingModel);
    Mockito.doThrow(IllegalStateException.class).when(service)
        .processImages(graphicImageDetails, groupCode, username, storeId, clientId, isResize, isEdited, isRevised,
            request, 1);
    processImageScalingListener.onDomainEventConsumedBulkImageScalingPriority1(payload);
    Mockito.verify(objectMapper).readValue(payload, ImageProcessingModel.class);
    Mockito.verify(service)
        .processImages(graphicImageDetails, groupCode, username, storeId, clientId, isResize, isEdited, isRevised,
            request, 1);
    Mockito.verify(kafkaTopicProperties).getImageScalingPriority1();
  }

  @Test
  void onDomainEventConsumedBulkImageScalingPriority2Test() throws IOException {
    imageProcessingModel.setPrioritySeller(2);
    Mockito.when(kafkaTopicProperties.getImageScalingPriority2())
        .thenReturn(DomainEventName.PROCESS_BULK_IMAGE_SCALING_PRIORITY_2);
    Mockito.when(objectMapper.readValue(payload, ImageProcessingModel.class)).thenReturn(imageProcessingModel);
    Mockito.doNothing().when(service)
        .processImages(graphicImageDetails, groupCode, username, storeId, clientId, isResize, isEdited, isRevised,
            request, 2);
    processImageScalingListener.onDomainEventConsumedBulkImageScalingPriority2(payload);
    Mockito.verify(objectMapper).readValue(payload, ImageProcessingModel.class);
    Mockito.verify(service)
        .processImages(graphicImageDetails, groupCode, username, storeId, clientId, isResize, isEdited, isRevised,
            request, 2);
    Mockito.verify(kafkaTopicProperties).getImageScalingPriority2();
  }

  @Test
  void onDomainEventConsumedBulkImageScalingPriority2ErrorTest() throws IOException {
    imageProcessingModel.setPrioritySeller(2);
    Mockito.when(kafkaTopicProperties.getImageScalingPriority2())
        .thenReturn(DomainEventName.PROCESS_BULK_IMAGE_SCALING_PRIORITY_2);
    Mockito.when(objectMapper.readValue(payload, ImageProcessingModel.class)).thenReturn(imageProcessingModel);
    Mockito.doThrow(IllegalStateException.class).when(service)
        .processImages(graphicImageDetails, groupCode, username, storeId, clientId, isResize, isEdited, isRevised,
            request, 2);
    processImageScalingListener.onDomainEventConsumedBulkImageScalingPriority2(payload);
    Mockito.verify(objectMapper).readValue(payload, ImageProcessingModel.class);
    Mockito.verify(service)
        .processImages(graphicImageDetails, groupCode, username, storeId, clientId, isResize, isEdited, isRevised,
            request, 2);
    Mockito.verify(kafkaTopicProperties).getImageScalingPriority2();
  }

  @Test
  void onDomainEventConsumedBulkImageResizeTest() throws IOException {
    Mockito.when(kafkaTopicProperties.getImageResize()).thenReturn(DomainEventName.PROCESS_BULK_IMAGE_RESIZE);
    Mockito.when(objectMapper.readValue(payload, ImageProcessingModel.class)).thenReturn(imageProcessingModel);
    Mockito.doNothing().when(service)
        .processImages(graphicImageDetails, groupCode, username, storeId, clientId, isResize, isEdited, isRevised,
            request, 0);
    processImageScalingListener.onDomainEventConsumedBulkImageResize(payload);
    Mockito.verify(objectMapper).readValue(payload, ImageProcessingModel.class);
    Mockito.verify(service)
        .processImages(graphicImageDetails, groupCode, username, storeId, clientId, isResize, isEdited, isRevised,
            request, 0);
    Mockito.verify(kafkaTopicProperties).getImageResize();
  }

  @Test
  void onDomainEventConsumedBulkImageResizeErrorTest() throws IOException {
    Mockito.when(kafkaTopicProperties.getImageResize()).thenReturn(DomainEventName.PROCESS_BULK_IMAGE_RESIZE);
    Mockito.when(objectMapper.readValue(payload, ImageProcessingModel.class)).thenReturn(imageProcessingModel);
    Mockito.doThrow(IllegalStateException.class).when(service)
        .processImages(graphicImageDetails, groupCode, username, storeId, clientId, isResize, isEdited, isRevised,
            request, 0);
    processImageScalingListener.onDomainEventConsumedBulkImageResize(payload);
    Mockito.verify(objectMapper).readValue(payload, ImageProcessingModel.class);
    Mockito.verify(service)
        .processImages(graphicImageDetails, groupCode, username, storeId, clientId, isResize, isEdited, isRevised,
            request, 0);
    Mockito.verify(kafkaTopicProperties).getImageResize();
  }

  @Test
  void onDomainEventConsumedBulkImageResizePriority1Test() throws IOException {
    Mockito.when(kafkaTopicProperties.getImageResizePriority1())
        .thenReturn(DomainEventName.PROCESS_BULK_IMAGE_RESIZE_PRIORITY_1_EVENT);
    Mockito.when(objectMapper.readValue(payload, ImageProcessingModel.class)).thenReturn(imageProcessingModel);
    Mockito.doNothing().when(service)
            .processImages(graphicImageDetails, groupCode, username, storeId, clientId, isResize, isEdited, isRevised,
                    request, 0);
    processImageScalingListener.onDomainEventConsumedBulkImageResizePriority1(payload);
    Mockito.verify(objectMapper).readValue(payload, ImageProcessingModel.class);
    Mockito.verify(service)
            .processImages(graphicImageDetails, groupCode, username, storeId, clientId, isResize, isEdited, isRevised,
                    request, 0);
    Mockito.verify(kafkaTopicProperties).getImageResizePriority1();
  }

  @Test
  void onDomainEventConsumedBulkImageResizePriority1ErrorTest() throws IOException {
    Mockito.when(kafkaTopicProperties.getImageResizePriority1())
        .thenReturn(DomainEventName.PROCESS_BULK_IMAGE_RESIZE_PRIORITY_1_EVENT);
    Mockito.when(objectMapper.readValue(payload, ImageProcessingModel.class)).thenReturn(imageProcessingModel);
    Mockito.doThrow(IllegalStateException.class).when(service)
            .processImages(graphicImageDetails, groupCode, username, storeId, clientId, isResize, isEdited, isRevised,
                    request, 0);
    processImageScalingListener.onDomainEventConsumedBulkImageResizePriority1(payload);
    Mockito.verify(objectMapper).readValue(payload, ImageProcessingModel.class);
    Mockito.verify(service)
            .processImages(graphicImageDetails, groupCode, username, storeId, clientId, isResize, isEdited, isRevised,
                    request, 0);
    Mockito.verify(kafkaTopicProperties).getImageResizePriority1();
  }

  @Test
  void onDomainEventConsumedBulkImageResizePriority2Test() throws IOException {
    Mockito.when(kafkaTopicProperties.getImageResizePriority2())
        .thenReturn(DomainEventName.PROCESS_BULK_IMAGE_RESIZE_PRIORITY_2_EVENT);
    Mockito.when(objectMapper.readValue(payload, ImageProcessingModel.class)).thenReturn(imageProcessingModel);
    Mockito.doNothing().when(service)
            .processImages(graphicImageDetails, groupCode, username, storeId, clientId, isResize, isEdited, isRevised,
                    request, 0);
    processImageScalingListener.onDomainEventConsumedBulkImageResizePriority2(payload);
    Mockito.verify(objectMapper).readValue(payload, ImageProcessingModel.class);
    Mockito.verify(service)
            .processImages(graphicImageDetails, groupCode, username, storeId, clientId, isResize, isEdited, isRevised,
                    request, 0);
    Mockito.verify(kafkaTopicProperties).getImageResizePriority2();
  }

  @Test
  void onDomainEventConsumedBulkImageResizePriority2ErrorTest() throws IOException {
    Mockito.when(kafkaTopicProperties.getImageResizePriority2())
        .thenReturn(DomainEventName.PROCESS_BULK_IMAGE_RESIZE_PRIORITY_2_EVENT);
    Mockito.when(objectMapper.readValue(payload, ImageProcessingModel.class)).thenReturn(imageProcessingModel);
    Mockito.doThrow(IllegalStateException.class).when(service)
            .processImages(graphicImageDetails, groupCode, username, storeId, clientId, isResize, isEdited, isRevised,
                    request, 0);
    processImageScalingListener.onDomainEventConsumedBulkImageResizePriority2(payload);
    Mockito.verify(objectMapper).readValue(payload, ImageProcessingModel.class);
    Mockito.verify(service)
            .processImages(graphicImageDetails, groupCode, username, storeId, clientId, isResize, isEdited, isRevised,
                    request, 0);
    Mockito.verify(kafkaTopicProperties).getImageResizePriority2();
  }

  @Test
  void onDomainEventConsumedBulkImageEditResizeTest() throws IOException {
    Mockito.when(kafkaTopicProperties.getImageEditResize()).thenReturn(DomainEventName.PROCESS_BULK_IMAGE_EDIT_RESIZE);
    Mockito.when(objectMapper.readValue(payload, ImageProcessingModel.class)).thenReturn(imageProcessingModel);
    Mockito.doNothing().when(service)
        .processImages(graphicImageDetails, groupCode, username, storeId, clientId, isResize, isEdited, isRevised,
            request, 0);
    processImageScalingListener.onDomainEventConsumedBulkImageEditResize(payload);
    Mockito.verify(objectMapper).readValue(payload, ImageProcessingModel.class);
    Mockito.verify(service)
        .processImages(graphicImageDetails, groupCode, username, storeId, clientId, isResize, isEdited, isRevised,
            request, 0);
    Mockito.verify(kafkaTopicProperties).getImageEditResize();
  }

  @Test
  void onDomainEventConsumedBulkImageEditResizeErrorTest() throws IOException {
    Mockito.when(kafkaTopicProperties.getImageEditResize()).thenReturn(DomainEventName.PROCESS_BULK_IMAGE_EDIT_RESIZE);
    Mockito.when(objectMapper.readValue(payload, ImageProcessingModel.class)).thenReturn(imageProcessingModel);
    Mockito.doThrow(IllegalStateException.class).when(service)
        .processImages(graphicImageDetails, groupCode, username, storeId, clientId, isResize, isEdited, isRevised,
            request, 0);
    processImageScalingListener.onDomainEventConsumedBulkImageEditResize(payload);
    Mockito.verify(objectMapper).readValue(payload, ImageProcessingModel.class);
    Mockito.verify(service)
        .processImages(graphicImageDetails, groupCode, username, storeId, clientId, isResize, isEdited, isRevised,
            request, 0);
    Mockito.verify(kafkaTopicProperties).getImageEditResize();
  }

  @Test
  void onDomainEventConsumedBulkImageRevisedResizeTest() throws IOException {
    Mockito.when(kafkaTopicProperties.getRevisedImageResize())
        .thenReturn(DomainEventName.PROCESS_BULK_IMAGE_REVISED_RESIZE);
    Mockito.when(objectMapper.readValue(payload, ImageProcessingModel.class)).thenReturn(imageProcessingModel);
    Mockito.doNothing().when(service)
        .processImages(graphicImageDetails, groupCode, username, storeId, clientId, isResize, isEdited, isRevised,
            request, 0);
    processImageScalingListener.onDomainEventConsumedBulkImageRevisedResize(payload);
    Mockito.verify(objectMapper).readValue(payload, ImageProcessingModel.class);
    Mockito.verify(service)
        .processImages(graphicImageDetails, groupCode, username, storeId, clientId, isResize, isEdited, isRevised,
            request, 0);
    Mockito.verify(kafkaTopicProperties).getRevisedImageResize();
  }

  @Test
  void onDomainEventConsumedBulkImageRevisedResizeErrorTest() throws IOException {
    Mockito.when(kafkaTopicProperties.getRevisedImageResize())
        .thenReturn(DomainEventName.PROCESS_BULK_IMAGE_REVISED_RESIZE);
    Mockito.when(objectMapper.readValue(payload, ImageProcessingModel.class)).thenReturn(imageProcessingModel);
    Mockito.doThrow(IllegalStateException.class).when(service)
        .processImages(graphicImageDetails, groupCode, username, storeId, clientId, isResize, isEdited, isRevised,
            request, 0);
    processImageScalingListener.onDomainEventConsumedBulkImageRevisedResize(payload);
    Mockito.verify(objectMapper).readValue(payload, ImageProcessingModel.class);
    Mockito.verify(service)
        .processImages(graphicImageDetails, groupCode, username, storeId, clientId, isResize, isEdited, isRevised,
            request, 0);
    Mockito.verify(kafkaTopicProperties).getRevisedImageResize();
  }

  @Test
  void onDomainEventConsumedBulkImageEditScalingTest() throws IOException {
    Mockito.when(kafkaTopicProperties.getEditedImageScaling())
        .thenReturn(DomainEventName.PROCESS_BULK_IMAGE_EDIT_SCALING);
    Mockito.when(objectMapper.readValue(payload, ImageProcessingModel.class)).thenReturn(imageProcessingModel);
    Mockito.doNothing().when(service)
        .processImages(graphicImageDetails, groupCode, username, storeId, clientId, isResize, isEdited, isRevised,
            request, 0);
    processImageScalingListener.onDomainEventConsumedBulkImageEditScaling(payload);
    Mockito.verify(objectMapper).readValue(payload, ImageProcessingModel.class);
    Mockito.verify(service)
        .processImages(graphicImageDetails, groupCode, username, storeId, clientId, isResize, isEdited, isRevised,
            request, 0);
    Mockito.verify(kafkaTopicProperties).getEditedImageScaling();
  }

  @Test
  void onDomainEventConsumedBulkImageEditScalingErrorTest() throws IOException {
    Mockito.when(kafkaTopicProperties.getEditedImageScaling())
        .thenReturn(DomainEventName.PROCESS_BULK_IMAGE_EDIT_SCALING);
    Mockito.when(objectMapper.readValue(payload, ImageProcessingModel.class)).thenReturn(imageProcessingModel);
    Mockito.doThrow(IllegalStateException.class).when(service)
        .processImages(graphicImageDetails, groupCode, username, storeId, clientId, isResize, isEdited, isRevised,
            request, 0);
    processImageScalingListener.onDomainEventConsumedBulkImageEditScaling(payload);
    Mockito.verify(objectMapper).readValue(payload, ImageProcessingModel.class);
    Mockito.verify(service)
        .processImages(graphicImageDetails, groupCode, username, storeId, clientId, isResize, isEdited, isRevised,
            request, 0);
    Mockito.verify(kafkaTopicProperties).getEditedImageScaling();
  }

  @Test
  void onDomainEventConsumedImageScalingAndUploadTest() throws JsonProcessingException {
    Mockito.when(kafkaTopicProperties.getImageScalingAndUpload())
        .thenReturn(DomainEventName.PROCESS_IMAGE_SCALING_AND_UPLOAD);
    Mockito.when(objectMapper.readValue(payload2, ImageScalingAndUploadModel.class))
        .thenReturn(imageScalingAndUploadModel);
    Mockito.doNothing().when(asyncGraphicsProcessorServiceWrapper).scaleImage(imageScalingAndUploadModel.getGraphicDetailCommands(),
        imageScalingAndUploadModel.isUploadToGcs(), imageScalingAndUploadModel.getClientId(), imageScalingAndUploadModel.getRequestId());
    Mockito.when(gcsProperties.getRmaTemporaryImageSourcePath()).thenReturn(PATH);
    Mockito.when(rmaImageBucket.getName()).thenReturn("bucket");
    Mockito.doNothing().when(gcsService).downloadFileTo(any(),any(),any());
    processImageScalingListener.onDomainEventConsumedImageScalingAndUpload(payload2);
    Mockito.verify(asyncGraphicsProcessorServiceWrapper).scaleImage(imageScalingAndUploadModel.getGraphicDetailCommands(),
        imageScalingAndUploadModel.isUploadToGcs(), imageScalingAndUploadModel.getClientId(), imageScalingAndUploadModel.getRequestId());
    Mockito.verify(objectMapper).readValue(payload2, ImageScalingAndUploadModel.class);
    Mockito.verify(kafkaTopicProperties).getImageScalingAndUpload();
    Mockito.verify(gcsProperties).getRmaTemporaryImageSourcePath();
    Mockito.verify(rmaImageBucket).getName();
    Mockito.verify(gcsService).downloadFileTo(anyString(),anyString(),anyString());
  }

  @Test
  void onDomainEventConsumedImageScalingAndUploadExceptionTest() throws JsonProcessingException {
    Mockito.when(kafkaTopicProperties.getImageScalingAndUpload())
        .thenReturn(DomainEventName.PROCESS_IMAGE_SCALING_AND_UPLOAD);
    Mockito.when(objectMapper.readValue(payload2, ImageScalingAndUploadModel.class))
        .thenReturn(imageScalingAndUploadModel);
    Mockito.when(gcsProperties.getRmaTemporaryImageSourcePath()).thenReturn(PATH);
    Mockito.when(rmaImageBucket.getName()).thenReturn("bucket");
    Mockito.doNothing().when(gcsService).downloadFileTo(any(),any(),any());
    Mockito.doThrow(ApplicationRuntimeException.class).when(asyncGraphicsProcessorServiceWrapper).scaleImage(imageScalingAndUploadModel.getGraphicDetailCommands(),
        imageScalingAndUploadModel.isUploadToGcs(), imageScalingAndUploadModel.getClientId(), imageScalingAndUploadModel.getRequestId());
    processImageScalingListener.onDomainEventConsumedImageScalingAndUpload(payload2);
    Mockito.verify(asyncGraphicsProcessorServiceWrapper).scaleImage(imageScalingAndUploadModel.getGraphicDetailCommands(),
        imageScalingAndUploadModel.isUploadToGcs(), imageScalingAndUploadModel.getClientId(), imageScalingAndUploadModel.getRequestId());
    Mockito.verify(objectMapper).readValue(payload2, ImageScalingAndUploadModel.class);
    Mockito.verify(kafkaTopicProperties).getImageScalingAndUpload();
    Mockito.verify(gcsProperties).getRmaTemporaryImageSourcePath();
    Mockito.verify(rmaImageBucket).getName();
    Mockito.verify(gcsService).downloadFileTo(anyString(),anyString(),anyString());
  }

  @Test
  void onDomainEventConsumedImageScalingAndUploadTest_uploadingToGcs() throws JsonProcessingException {
    Mockito.when(kafkaTopicProperties.getImageScalingAndUpload())
        .thenReturn(DomainEventName.PROCESS_IMAGE_SCALING_AND_UPLOAD);
    Mockito.when(gcsProperties.getRmaTemporaryImageSourcePath()).thenReturn(PATH);
    Mockito.when(objectMapper.readValue(payload3, ImageScalingAndUploadModel.class))
        .thenReturn(imageScalingAndUploadModel);
    Mockito.when(rmaImageBucket.getName()).thenReturn("bucket");
    Mockito.doNothing().when(gcsService).downloadFileTo(any(),any(),any());
    Mockito.doNothing().when(asyncGraphicsProcessorServiceWrapper).scaleImage(imageScalingAndUploadModel.getGraphicDetailCommands(),
        imageScalingAndUploadModel.isUploadToGcs(), imageScalingAndUploadModel.getClientId(), imageScalingAndUploadModel.getRequestId());
    processImageScalingListener.onDomainEventConsumedImageScalingAndUpload(payload3);
    Mockito.verify(asyncGraphicsProcessorServiceWrapper).scaleImage(imageScalingAndUploadModel.getGraphicDetailCommands(),
        imageScalingAndUploadModel.isUploadToGcs(), imageScalingAndUploadModel.getClientId(), imageScalingAndUploadModel.getRequestId());
    Mockito.verify(objectMapper).readValue(payload3, ImageScalingAndUploadModel.class);
    Mockito.verify(kafkaTopicProperties).getImageScalingAndUpload();
    Mockito.verify(gcsProperties).getRmaTemporaryImageSourcePath();
    Mockito.verify(rmaImageBucket).getName();
    Mockito.verify(gcsService).downloadFileTo(anyString(),anyString(),anyString());
  }
}

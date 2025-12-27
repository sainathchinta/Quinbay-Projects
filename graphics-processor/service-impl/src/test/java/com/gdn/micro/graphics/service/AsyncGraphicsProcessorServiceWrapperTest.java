package com.gdn.micro.graphics.service;


import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.concurrent.ExecutorService;

import com.gdn.micro.graphics.config.KafkaPublisher;
import com.gdn.micro.graphics.service.config.GcsProperties;
import org.apache.commons.io.FileUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.slf4j.MDC;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.micro.graphics.config.KafkaTopicProperties;
import com.gdn.micro.graphics.domain.event.config.DomainEventName;
import com.gdn.micro.graphics.domain.event.model.ImageResultDetail;
import com.gdn.micro.graphics.domain.event.model.ImageResultDetailList;
import com.gdn.micro.graphics.model.CustomGraphicsSettings;
import com.gdn.micro.graphics.model.GraphicDetailCommand;
import com.gdn.micro.graphics.model.ImageScalingAndUploadModel;
import com.gdn.micro.graphics.service.enums.TargetType;
import com.google.common.util.concurrent.MoreExecutors;

import static org.mockito.ArgumentMatchers.eq;

public class AsyncGraphicsProcessorServiceWrapperTest {

  @InjectMocks
  private AsyncGraphicsProcessorServiceWrapper asyncGraphicsProcessorServiceWrapper;

  @Mock
  private GraphicsProcessorService service;

  @Mock
  private FileStorageService fileStorageService;

  private byte[] imageContent;

  private ExecutorService executorService = MoreExecutors.newDirectExecutorService();

  @Mock
  private KafkaPublisher kafkaPublisher;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @Mock
  private GcsProperties gcsProperties;

  private static final String PATH = "path";
  private static final String SOURCE = "source";
  private static final String DESTINATION = "destination";
  private static final String PREFIX = "prefix";
  private static final String FILENAME = "fileName";
  private static final String TEST_FILE = "Test.txt";
  private static final String CLIENT_ID = "clientId";
  private static final String ORDER = "xorder";
  private static final String REQUEST_ID = "requestId";

  private ImageResultDetail imageResultDetail;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    asyncGraphicsProcessorServiceWrapper.setExecutorService(executorService);
    imageContent = new byte[]{-1, -40, -20, -10};

    imageResultDetail = new ImageResultDetail();
    imageResultDetail.setSuccess(false);

  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(service, kafkaPublisher, kafkaTopicProperties);
    FileUtils.deleteDirectory(new File(PATH));
  }

  @Test
  void convertTest() throws Exception {
    Mockito.when(kafkaTopicProperties.getImageResultDetailStatus())
        .thenReturn(DomainEventName.GRAPHIC_DETAIL_STATUS_EVENT_NAME);
    MDC.setContextMap(new HashMap<>());
    Mockito.when(
        service.convert(Mockito.any(), eq(SOURCE), eq(DESTINATION), eq(FILENAME), Mockito.any(),
            eq(PREFIX), eq(CLIENT_ID), eq(REQUEST_ID))).thenReturn(new ImageResultDetail());
    asyncGraphicsProcessorServiceWrapper
        .convert(TargetType.JPG, mockFile(PATH + TEST_FILE), SOURCE, DESTINATION, new CustomGraphicsSettings(), FILENAME,
            PREFIX, CLIENT_ID, REQUEST_ID);
    Mockito.verify(service)
        .convert(Mockito.any(), eq(SOURCE), eq(DESTINATION), eq(FILENAME), Mockito.any(),
            eq(PREFIX), eq(CLIENT_ID), eq(REQUEST_ID));
    Mockito.verify(kafkaPublisher)
        .send(eq(DomainEventName.GRAPHIC_DETAIL_STATUS_EVENT_NAME), Mockito.any(ImageResultDetailList.class));
    Mockito.verify(kafkaTopicProperties).getImageResultDetailStatus();
  }

  @Test
  void convertTestException() throws Exception {
    MDC.setContextMap(new HashMap<>());
    Mockito.when(kafkaTopicProperties.getImageResultDetailStatus())
        .thenReturn(DomainEventName.GRAPHIC_DETAIL_STATUS_EVENT_NAME);
    Mockito.when(
        service.convert(Mockito.any(), eq(SOURCE), eq(DESTINATION), eq(FILENAME), Mockito.any(),
            eq(PREFIX), eq(CLIENT_ID), eq(REQUEST_ID))).thenReturn(new ImageResultDetail());
    Mockito.doThrow(ApplicationRuntimeException.class).when(kafkaPublisher)
        .send(eq(DomainEventName.GRAPHIC_DETAIL_STATUS_EVENT_NAME), Mockito.any(ImageResultDetailList.class));
    try {
      asyncGraphicsProcessorServiceWrapper
          .convert(TargetType.JPG, mockFile(TEST_FILE), SOURCE, DESTINATION, new CustomGraphicsSettings(), FILENAME,
              PREFIX, CLIENT_ID, REQUEST_ID);
    } catch (Exception e) {
      Assertions.assertEquals(ApplicationRuntimeException.class, e.getClass());
    } finally {
      Mockito.verify(service)
          .convert(Mockito.any(), eq(SOURCE), eq(DESTINATION), eq(FILENAME), Mockito.any(),
              eq(PREFIX), eq(CLIENT_ID), eq(REQUEST_ID));
      Mockito.verify(kafkaPublisher)
          .send(eq(DomainEventName.GRAPHIC_DETAIL_STATUS_EVENT_NAME), Mockito.any(ImageResultDetailList.class));
      Mockito.verify(kafkaTopicProperties).getImageResultDetailStatus();
    }
  }

  @Test
  void scaleTest() throws Exception {
    MDC.setContextMap(new HashMap<>());
    Mockito.when(kafkaTopicProperties.getImageResultDetailStatus())
        .thenReturn(DomainEventName.GRAPHIC_DETAIL_STATUS_EVENT_NAME);
    Mockito.when(
        service.scale(eq(SOURCE), eq(DESTINATION), Mockito.any(), eq(PREFIX), eq(false), eq(null)))
        .thenReturn(new ImageResultDetail());
    asyncGraphicsProcessorServiceWrapper
        .scale(mockFile(TEST_FILE), SOURCE, DESTINATION, new CustomGraphicsSettings(), PREFIX);
    Mockito.verify(service)
        .scale(eq(SOURCE), eq(DESTINATION), Mockito.any(), eq(PREFIX), eq(false), eq(null));
    Mockito.verify(kafkaPublisher)
        .send(eq(DomainEventName.GRAPHIC_DETAIL_STATUS_EVENT_NAME), Mockito.any(ImageResultDetailList.class));
    Mockito.verify(kafkaTopicProperties).getImageResultDetailStatus();
  }

  @Test
  void scaleTestException() throws Exception {
    MDC.setContextMap(new HashMap<>());
    Mockito.when(kafkaTopicProperties.getImageResultDetailStatus())
        .thenReturn(DomainEventName.GRAPHIC_DETAIL_STATUS_EVENT_NAME);
    Mockito.when(
        service.scale(eq(SOURCE), eq(DESTINATION), Mockito.any(), eq(PREFIX), eq(false), eq(null)))
        .thenReturn(new ImageResultDetail());
    Mockito.doThrow(ApplicationRuntimeException.class).when(kafkaPublisher)
        .send(eq(DomainEventName.GRAPHIC_DETAIL_STATUS_EVENT_NAME), Mockito.any(ImageResultDetailList.class));
    try {
      asyncGraphicsProcessorServiceWrapper
          .scale(mockFile(PATH + TEST_FILE), SOURCE, DESTINATION, new CustomGraphicsSettings(), PREFIX);
    } catch (Exception e) {
      Assertions.assertEquals(ApplicationRuntimeException.class, e.getClass());
    } finally {
      Mockito.verify(service)
          .scale(eq(SOURCE), eq(DESTINATION), Mockito.any(), eq(PREFIX), eq(false), eq(null));
      Mockito.verify(kafkaPublisher)
          .send(eq(DomainEventName.GRAPHIC_DETAIL_STATUS_EVENT_NAME), Mockito.any(ImageResultDetailList.class));
      Mockito.verify(kafkaTopicProperties).getImageResultDetailStatus();
    }
  }

  @Test
  void scalesTest() throws Exception {
    MDC.setContextMap(new HashMap<>());
    Mockito.when(kafkaTopicProperties.getImageResultDetailStatus())
        .thenReturn(DomainEventName.GRAPHIC_DETAIL_STATUS_EVENT_NAME);
    asyncGraphicsProcessorServiceWrapper.scales(new ArrayList<>(), false, CLIENT_ID, REQUEST_ID);
    Mockito.verify(kafkaPublisher)
        .send(eq(DomainEventName.GRAPHIC_DETAIL_STATUS_EVENT_NAME), Mockito.any(ImageResultDetailList.class));
    Mockito.verify(kafkaTopicProperties).getImageResultDetailStatus();
  }

  @Test
  void scalesEventProcessingTest() throws Exception {
    MDC.setContextMap(new HashMap<>());
    Mockito.when(kafkaTopicProperties.getImageScalingAndUpload())
        .thenReturn(DomainEventName.PROCESS_IMAGE_SCALING_AND_UPLOAD);
    ReflectionTestUtils.setField(asyncGraphicsProcessorServiceWrapper, "eventBasedScalingEnabled", true);
    asyncGraphicsProcessorServiceWrapper.scales(new ArrayList<>(), false, CLIENT_ID, REQUEST_ID);
    Mockito.verify(kafkaPublisher)
        .send(eq(DomainEventName.PROCESS_IMAGE_SCALING_AND_UPLOAD), Mockito.any(ImageScalingAndUploadModel.class));
    Mockito.verify(kafkaTopicProperties).getImageScalingAndUpload();
  }

  @Test
  void scalesTestException() throws Exception {
    MDC.setContextMap(new HashMap<>());
    Mockito.when(kafkaTopicProperties.getImageResultDetailStatus())
        .thenReturn(DomainEventName.GRAPHIC_DETAIL_STATUS_EVENT_NAME);
    Mockito.doThrow(ApplicationRuntimeException.class).when(kafkaPublisher)
        .send(eq(DomainEventName.GRAPHIC_DETAIL_STATUS_EVENT_NAME), Mockito.any(ImageResultDetailList.class));
    try {
      asyncGraphicsProcessorServiceWrapper.scales(new ArrayList<>(), false, CLIENT_ID, REQUEST_ID);
    } catch (Exception e) {
      Assertions.assertEquals(ApplicationRuntimeException.class, e.getClass());
    } finally {
      Mockito.verify(kafkaPublisher)
          .send(eq(DomainEventName.GRAPHIC_DETAIL_STATUS_EVENT_NAME), Mockito.any(ImageResultDetailList.class));
      Mockito.verify(kafkaTopicProperties).getImageResultDetailStatus();
    }
  }

  @Test
  void scaleAndRetryTest() throws Exception {
    Mockito.when(kafkaTopicProperties.getImageResultDetailStatus())
        .thenReturn(DomainEventName.GRAPHIC_DETAIL_STATUS_EVENT_NAME);
    ReflectionTestUtils.setField(asyncGraphicsProcessorServiceWrapper, "totalNoOfTries", 3);
    MDC.setContextMap(new HashMap<>());
    GraphicDetailCommand graphicDetailCommand =
        new GraphicDetailCommand(mockFile(TEST_FILE), SOURCE, DESTINATION, new CustomGraphicsSettings(), PREFIX, false);
    Mockito.when(service
        .scale(eq(graphicDetailCommand.getSourcePath()), eq(graphicDetailCommand.getDestinationPath()),
            eq(graphicDetailCommand.getCustomGraphicsSettings()),
            eq(graphicDetailCommand.getPrefixPath()), Mockito.anyBoolean(), eq(null))).thenReturn(imageResultDetail);
    asyncGraphicsProcessorServiceWrapper.scales(Arrays.asList(graphicDetailCommand), false, CLIENT_ID, REQUEST_ID);
    Mockito.verify(service, Mockito.times(3))
        .scale(graphicDetailCommand.getSourcePath(), graphicDetailCommand.getDestinationPath(),
            graphicDetailCommand.getCustomGraphicsSettings(), graphicDetailCommand.getPrefixPath(), false, null);
    Mockito.verify(kafkaPublisher)
        .send(eq(DomainEventName.GRAPHIC_DETAIL_STATUS_EVENT_NAME), Mockito.any(ImageResultDetailList.class));
    Mockito.verify(kafkaTopicProperties).getImageResultDetailStatus();
  }

  @Test
  void scaleWithRetryExceptionTest() throws Exception {
    MDC.setContextMap(new HashMap<>());
    GraphicDetailCommand graphicDetailCommand =
        new GraphicDetailCommand(mockFile(TEST_FILE), SOURCE, DESTINATION, new CustomGraphicsSettings(), PREFIX, false);
    Mockito.when(service
        .scale(eq(graphicDetailCommand.getSourcePath()), eq(graphicDetailCommand.getDestinationPath()),
            eq(graphicDetailCommand.getCustomGraphicsSettings()),
            eq(graphicDetailCommand.getPrefixPath()), Mockito.anyBoolean(), eq(null))).thenReturn(imageResultDetail);
    try {
      asyncGraphicsProcessorServiceWrapper.scales(Arrays.asList(graphicDetailCommand), false, CLIENT_ID, REQUEST_ID);
    }
    catch (ApplicationRuntimeException ex){
      Assertions.assertNotNull(ex);
    }
    finally {
      Mockito.verify(service).scale(graphicDetailCommand.getSourcePath(), graphicDetailCommand.getDestinationPath(),
          graphicDetailCommand.getCustomGraphicsSettings(), graphicDetailCommand.getPrefixPath(), false, null);
    }
  }

  @Test
  void scaleImageTest() throws Exception {
    Mockito.when(kafkaTopicProperties.getImageResultDetailStatus())
      .thenReturn(DomainEventName.GRAPHIC_DETAIL_STATUS_EVENT_NAME);
    ReflectionTestUtils.setField(asyncGraphicsProcessorServiceWrapper, "totalNoOfTries", 3);
    MDC.setContextMap(new HashMap<>());
    GraphicDetailCommand graphicDetailCommand =
      new GraphicDetailCommand(mockFile(TEST_FILE), SOURCE, DESTINATION,
        new CustomGraphicsSettings(), PREFIX, false);
    Mockito.when(service.scale(eq(graphicDetailCommand.getSourcePath()),
        eq(graphicDetailCommand.getDestinationPath()),
        eq(graphicDetailCommand.getCustomGraphicsSettings()),
        eq(graphicDetailCommand.getPrefixPath()), Mockito.anyBoolean(), eq(null)))
      .thenReturn(imageResultDetail);
    asyncGraphicsProcessorServiceWrapper.scaleImage(Collections.singletonList(graphicDetailCommand),
      true, ORDER, new ArrayList<>());
    Mockito.verify(service, Mockito.times(3))
      .scale(graphicDetailCommand.getSourcePath(), graphicDetailCommand.getDestinationPath(),
        graphicDetailCommand.getCustomGraphicsSettings(), graphicDetailCommand.getPrefixPath(),
        false, null);
  }

  @Test
  void scaleImageExceptionTest() throws Exception {
    ReflectionTestUtils.setField(asyncGraphicsProcessorServiceWrapper, "totalNoOfTries", 3);
    MDC.setContextMap(new HashMap<>());
    GraphicDetailCommand graphicDetailCommand =
      new GraphicDetailCommand(mockFile(TEST_FILE), SOURCE, DESTINATION,
        new CustomGraphicsSettings(), PREFIX, false);
    Mockito.doThrow(ApplicationRuntimeException.class).when(service)
      .scale(eq(graphicDetailCommand.getSourcePath()),
        eq(graphicDetailCommand.getDestinationPath()),
        eq(graphicDetailCommand.getCustomGraphicsSettings()),
        eq(graphicDetailCommand.getPrefixPath()), Mockito.anyBoolean(), eq(null));
    asyncGraphicsProcessorServiceWrapper.scaleImage(Collections.singletonList(graphicDetailCommand),
      true, ORDER, new ArrayList<>());
    Mockito.verify(service)
      .scale(graphicDetailCommand.getSourcePath(), graphicDetailCommand.getDestinationPath(),
        graphicDetailCommand.getCustomGraphicsSettings(), graphicDetailCommand.getPrefixPath(),
        false, null);
  }

  @Test
  void scaleImageUploadExceptionTest() throws Exception {
    Mockito.when(kafkaTopicProperties.getImageResultDetailStatus())
      .thenReturn(DomainEventName.GRAPHIC_DETAIL_STATUS_EVENT_NAME);
    ReflectionTestUtils.setField(asyncGraphicsProcessorServiceWrapper, "totalNoOfTries", 3);
    MDC.setContextMap(new HashMap<>());
    GraphicDetailCommand graphicDetailCommand =
      new GraphicDetailCommand(mockFile(TEST_FILE), SOURCE, DESTINATION,
        new CustomGraphicsSettings(), PREFIX, false);
    Mockito.when(service.scale(eq(graphicDetailCommand.getSourcePath()),
        eq(graphicDetailCommand.getDestinationPath()),
        eq(graphicDetailCommand.getCustomGraphicsSettings()),
        eq(graphicDetailCommand.getPrefixPath()), Mockito.anyBoolean(), eq(null)))
      .thenReturn(imageResultDetail);
    Mockito.doThrow(Exception.class).when(fileStorageService).uploadToAndDeleteFromTempLocationGcs(eq(true),
      Mockito.anyString(),Mockito.anyList(),Mockito.anyList());
    asyncGraphicsProcessorServiceWrapper.scaleImage(Collections.singletonList(graphicDetailCommand),
      true, ORDER, new ArrayList<>());
    Mockito.verify(service, Mockito.times(3))
      .scale(graphicDetailCommand.getSourcePath(), graphicDetailCommand.getDestinationPath(),
        graphicDetailCommand.getCustomGraphicsSettings(), graphicDetailCommand.getPrefixPath(),
        false, null);
  }


  private File mockFile(String filePath) throws IOException {
    File file = new File(filePath);
    FileUtils.writeByteArrayToFile(file, imageContent);
    return file;
  }

}

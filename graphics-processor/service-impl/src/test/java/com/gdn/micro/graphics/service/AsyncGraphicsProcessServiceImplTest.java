package com.gdn.micro.graphics.service;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.eq;

import java.io.File;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.micro.graphics.config.KafkaPublisher;
import com.gdn.micro.graphics.domain.event.model.ImagePathResult;
import com.gdn.micro.graphics.domain.event.model.ScaleImagesResponse;
import com.gdn.micro.graphics.model.ImagePaths;
import com.gdn.micro.graphics.model.ResizeImageScalingModel;
import com.gdn.micro.graphics.service.config.GcsProperties;
import com.gdn.micro.graphics.web.model.XgpImageScaleRequest;
import com.google.cloud.storage.Bucket;

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
import org.mockito.Spy;
import org.springframework.test.util.ReflectionTestUtils;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.micro.graphics.config.KafkaTopicProperties;
import com.gdn.micro.graphics.domain.event.config.DomainEventName;
import com.gdn.micro.graphics.domain.event.model.BulkImageProcessResponse;
import com.gdn.micro.graphics.domain.event.model.ImageResponse;
import com.gdn.micro.graphics.domain.event.model.ImageResultDetail;
import com.gdn.micro.graphics.domain.event.model.ScaleEditedImagesResponse;
import com.gdn.micro.graphics.model.CustomGraphicsSettings;
import com.gdn.micro.graphics.model.GraphicDimension;
import com.gdn.micro.graphics.model.GraphicImageDetail;
import com.gdn.micro.graphics.model.ImageProcessingModel;
import com.gdn.micro.graphics.web.model.ScaleEditedImageRequest;
import com.gdn.micro.graphics.web.model.ScaleImageRequest;
import com.google.common.util.concurrent.MoreExecutors;
import com.gdn.micro.graphics.web.model.FullImageUploadRequest;
import com.gdn.micro.graphics.web.model.MediumImageUploadRequest;
import com.gdn.micro.graphics.web.model.ThumbNailImageUploadRequest;

import com.gdn.micro.graphics.service.config.ImageConfigurationProperties;
import com.gdn.micro.graphics.model.IdentifyImageResult;
import com.gdn.micro.graphics.service.enums.TargetType;

/**
 * Created by Vishal on 21/06/18.
 */
public class AsyncGraphicsProcessServiceImplTest {

  private static final String GROUP_CODE = "group_code";
  private static final String STORE_ID = "storeId";
  private static final String USERNAME = "username";
  private static final String HASH_CODE = "hashCode";
  private static final String SOURCE_PATH = "sourcePath/";
  private static final String IMAGE_NAME = "imageName";
  private static final String DESTINATION_PATH = "destinationPath";
  private static final String PREFIX_PATH = "prefixPath";
  private static final String CLIENT_ID = "clientId";
  private static final String EVENT = "event";
  private static final ObjectMapper MAPPER = new ObjectMapper();
  private static final String TOPIC = "topic";
  private byte[] testImageBytes;
  byte[] jpegHeader = new byte[]{ (byte)0xFF, (byte)0xD8, (byte)0xFF };
  byte[] pngHeader = new byte[] {(byte) 0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A};
  byte[] webpHeader = "RIFFxxxxWEBP".getBytes(StandardCharsets.US_ASCII);


  @InjectMocks
  private AsyncGraphicsProcessServiceImpl wrapper;

  @Mock
  private ExecutorService executorService;

  @Mock
  private KafkaPublisher kafkaPublisher;

  @Mock
  private FileStorageService fileStorageService;

  @Spy
  private GraphicsProcessorServiceImpl graphicsProcessorService = Mockito.mock(GraphicsProcessorServiceImpl.class);

  @Captor
  private ArgumentCaptor<BulkImageProcessResponse> bulkImageProcessResponseArgumentCaptor;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @Mock
  private GcsProperties gcsProperties;

  @Mock
  private GcsService gcsService;

  @Mock
  private Bucket orderImageBucket;

  @Mock
  private AsyncGraphicsProcessorServiceWrapper asyncGraphicsProcessorServiceWrapper;

  @Mock
  private ImageConfigurationProperties imageConfig;

  private GraphicImageDetail graphicImageDetail;
  private ResizeImageScalingModel resizeImageScalingModel;
  private ImagePaths imagePaths;


  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    ReflectionTestUtils.setField(wrapper, "sourcePath", "/filestore/mta/images/source");
    ReflectionTestUtils.setField(wrapper, "parallelImageDownloadEnabled", false);
    ReflectionTestUtils.setField(wrapper, "eventBasedProcessingEnabled", false);
    ReflectionTestUtils.setField(wrapper, "enableParallelImageResize", true);
    Mockito.when(imageConfig.getFullWidth()).thenReturn(1920);
    Mockito.when(imageConfig.getFullHeight()).thenReturn(1080);
    Mockito.when(imageConfig.getFullQuality()).thenReturn(85);
    Mockito.when(imageConfig.getFullDpi()).thenReturn(72);
    
    Mockito.when(imageConfig.getMediumWidth()).thenReturn(800);
    Mockito.when(imageConfig.getMediumHeight()).thenReturn(600);
    Mockito.when(imageConfig.getMediumQuality()).thenReturn(80);
    Mockito.when(imageConfig.getMediumDpi()).thenReturn(72);
    
    Mockito.when(imageConfig.getThumbnailWidth()).thenReturn(200);
    Mockito.when(imageConfig.getThumbnailHeight()).thenReturn(200);
    Mockito.when(imageConfig.getThumbnailQuality()).thenReturn(75);
    Mockito.when(imageConfig.getThumbnailDpi()).thenReturn(72);
    
    Mockito.when(imageConfig.getWebpDpi()).thenReturn(72);
    Mockito.when(imageConfig.getWebpQuality()).thenReturn(75);

    graphicImageDetail = new GraphicImageDetail();
    graphicImageDetail.setSourcePath(SOURCE_PATH);
    //xgpImageScaleRequest = new XgpImageScaleRequest();

    testImageBytes = new byte[] {(byte) 0xFF, (byte) 0xD8, (byte) 0xFF, (byte) 0xE0};
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(executorService, kafkaPublisher, kafkaTopicProperties, imageConfig);
  }

  @Test
  void scaleBulkImages() throws Exception {
    Mockito.when(kafkaTopicProperties.getImageProcessStatusNoPriority())
        .thenReturn(DomainEventName.GRAPHIC_IMAGE_DETAIL_STATUS_EVENT_NAME);
    ReflectionTestUtils.setField(wrapper, "executorService", MoreExecutors.newDirectExecutorService());
    List<GraphicImageDetail> graphicImageDetails = new ArrayList<>();
    wrapper.scaleBulkImages(graphicImageDetails, GROUP_CODE, CLIENT_ID, false, 0);
    Mockito.verify(kafkaPublisher).send(Mockito.anyString(), eq(GROUP_CODE), Mockito.any(BulkImageProcessResponse.class));
    Mockito.verify(kafkaTopicProperties).getImageProcessStatusNoPriority();
  }

  @Test
  void scaleBulkImagesParallelDownloads() throws Exception {
    ReflectionTestUtils.setField(wrapper, "parallelImageDownloadEnabled", true);
    ReflectionTestUtils.setField(wrapper, "executorService", MoreExecutors.newDirectExecutorService());
    List<GraphicImageDetail> graphicImageDetails = Arrays.asList(graphicImageDetail);
    wrapper.scaleBulkImages(graphicImageDetails, GROUP_CODE, CLIENT_ID, false, 0);
  }

  @Test
  void scaleBulkImagesForRevisedProduct() throws Exception {
    Mockito.when(kafkaTopicProperties.getRevisedImageScaleStatus())
        .thenReturn(DomainEventName.GRAPHIC_REVISED_IMAGE_SCALE_STATUS_EVENT);
    ReflectionTestUtils.setField(wrapper, "executorService", MoreExecutors.newDirectExecutorService());
    List<GraphicImageDetail> graphicImageDetails = new ArrayList<>();
    wrapper.scaleBulkImages(graphicImageDetails, GROUP_CODE, CLIENT_ID, true, 0);
    Mockito.verify(kafkaPublisher).send(eq(DomainEventName.GRAPHIC_REVISED_IMAGE_SCALE_STATUS_EVENT), eq(GROUP_CODE), Mockito.any(ScaleEditedImagesResponse.class));
    Mockito.verify(kafkaTopicProperties).getRevisedImageScaleStatus();
  }

  @Test
  void scaleBulkImagesEventBased() throws Exception {
    Mockito.when(kafkaTopicProperties.getImageScalingNoPriority())
        .thenReturn(DomainEventName.PROCESS_BULK_IMAGE_SCALING);
    ReflectionTestUtils.setField(wrapper, "eventBasedProcessingEnabled", true);
    List<GraphicImageDetail> graphicImageDetails = new ArrayList<>();
    wrapper.scaleBulkImages(graphicImageDetails, GROUP_CODE, CLIENT_ID, false, 0);
    Mockito.verify(kafkaPublisher)
        .send(eq(DomainEventName.PROCESS_BULK_IMAGE_SCALING), eq(GROUP_CODE), Mockito.any(ImageProcessingModel.class));
    Mockito.verify(kafkaTopicProperties).getImageScalingNoPriority();
  }

  @Test
  void scaleBulkImagesEventBasedPriority1() throws Exception {
    Mockito.when(kafkaTopicProperties.getImageScalingPriority1())
        .thenReturn(DomainEventName.PROCESS_BULK_IMAGE_SCALING_PRIORITY_1);
    ReflectionTestUtils.setField(wrapper, "eventBasedProcessingEnabled", true);
    List<GraphicImageDetail> graphicImageDetails = new ArrayList<>();
    wrapper.scaleBulkImages(graphicImageDetails, GROUP_CODE, CLIENT_ID, false, 1);
    Mockito.verify(kafkaPublisher)
        .send(eq(DomainEventName.PROCESS_BULK_IMAGE_SCALING_PRIORITY_1), eq(GROUP_CODE), Mockito.any(ImageProcessingModel.class));
    Mockito.verify(kafkaTopicProperties).getImageScalingNoPriority();
    Mockito.verify(kafkaTopicProperties).getImageScalingPriority1();
  }

  @Test
  void scaleBulkImagesEventBasedPriority2() throws Exception {
    Mockito.when(kafkaTopicProperties.getImageScalingPriority2())
        .thenReturn(DomainEventName.PROCESS_BULK_IMAGE_SCALING_PRIORITY_2);
    ReflectionTestUtils.setField(wrapper, "eventBasedProcessingEnabled", true);
    List<GraphicImageDetail> graphicImageDetails = new ArrayList<>();
    wrapper.scaleBulkImages(graphicImageDetails, GROUP_CODE, CLIENT_ID, false, 2);
    Mockito.verify(kafkaPublisher)
        .send(eq(DomainEventName.PROCESS_BULK_IMAGE_SCALING_PRIORITY_2), eq(GROUP_CODE), Mockito.any(ImageProcessingModel.class));
    Mockito.verify(kafkaTopicProperties).getImageScalingNoPriority();
    Mockito.verify(kafkaTopicProperties).getImageScalingPriority2();
  }

  @Test
  void resizeBulkImages() throws Exception {
    Mockito.when(kafkaTopicProperties.getImageResizeStatusNoPriority())
        .thenReturn(DomainEventName.GRAPHIC_RESIZE_IMAGE_STATUS_EVENT);
    List<GraphicImageDetail> graphicImageDetails = new ArrayList<>();
    ReflectionTestUtils.setField(wrapper , "executorService", MoreExecutors.newDirectExecutorService());
    wrapper.resizeBulkImages(graphicImageDetails, GROUP_CODE, CLIENT_ID, 0);
    Mockito.verify(kafkaPublisher).send(Mockito.anyString(), eq(GROUP_CODE), Mockito.any(BulkImageProcessResponse.class));
    Mockito.verify(kafkaTopicProperties).getImageResizeStatusNoPriority();
  }


  @Test
  void resizeBulkImagesEventBased() throws Exception {
    Mockito.when(kafkaTopicProperties.getImageResize()).thenReturn(DomainEventName.PROCESS_BULK_IMAGE_RESIZE);
    List<GraphicImageDetail> graphicImageDetails = new ArrayList<>();
    ReflectionTestUtils.setField(wrapper, "eventBasedProcessingEnabled", true);
    wrapper.resizeBulkImages(graphicImageDetails, GROUP_CODE, CLIENT_ID, 0);
    Mockito.verify(kafkaPublisher)
        .send(eq(DomainEventName.PROCESS_BULK_IMAGE_RESIZE), eq(GROUP_CODE), Mockito.any(ImageProcessingModel.class));
    Mockito.verify(kafkaTopicProperties).getImageResize();
  }

  @Test
  void resizeBulkImagesEventBasedPriority1() throws Exception {
    Mockito.when(kafkaTopicProperties.getImageResizePriority1())
        .thenReturn(DomainEventName.PROCESS_BULK_IMAGE_RESIZE_PRIORITY_1_EVENT);
    List<GraphicImageDetail> graphicImageDetails = new ArrayList<>();
    ReflectionTestUtils.setField(wrapper, "eventBasedProcessingEnabled", true);
    wrapper.resizeBulkImages(graphicImageDetails, GROUP_CODE, CLIENT_ID, 1);
    Mockito.verify(kafkaPublisher)
            .send(eq(DomainEventName.PROCESS_BULK_IMAGE_RESIZE_PRIORITY_1_EVENT), eq(GROUP_CODE), Mockito.any(ImageProcessingModel.class));
    Mockito.verify(kafkaTopicProperties).getImageResize();
    Mockito.verify(kafkaTopicProperties).getImageResizePriority1();
  }

  @Test
  void resizeBulkImagesEventBasedPriority2() throws Exception {
    Mockito.when(kafkaTopicProperties.getImageResizePriority2())
        .thenReturn(DomainEventName.PROCESS_BULK_IMAGE_RESIZE_PRIORITY_2_EVENT);
    List<GraphicImageDetail> graphicImageDetails = new ArrayList<>();
    ReflectionTestUtils.setField(wrapper, "eventBasedProcessingEnabled", true);
    wrapper.resizeBulkImages(graphicImageDetails, GROUP_CODE, CLIENT_ID, 2);
    Mockito.verify(kafkaPublisher)
            .send(eq(DomainEventName.PROCESS_BULK_IMAGE_RESIZE_PRIORITY_2_EVENT), eq(GROUP_CODE), Mockito.any(ImageProcessingModel.class));
    Mockito.verify(kafkaTopicProperties).getImageResize();
    Mockito.verify(kafkaTopicProperties).getImageResizePriority2();
  }

  @Test
  void resizeEditedImages() throws Exception {
    Mockito.when(kafkaTopicProperties.getEditedImageResizeStatus())
        .thenReturn(DomainEventName.GRAPHIC_EDITED_RESIZE_IMAGE_STATUS_EVENT);
    List<GraphicImageDetail> graphicImageDetails = new ArrayList<>();
    ReflectionTestUtils.setField(wrapper , "executorService", MoreExecutors.newDirectExecutorService());
    wrapper.resizeEditedImages(graphicImageDetails,  GROUP_CODE, CLIENT_ID);
    Mockito.verify(kafkaPublisher).send(Mockito.anyString(), eq(GROUP_CODE), Mockito.any(BulkImageProcessResponse.class));
    Mockito.verify(kafkaTopicProperties).getEditedImageResizeStatus();
  }

  @Test
  void resizeEditedImagesEventBased() throws Exception {
    Mockito.when(kafkaTopicProperties.getImageEditResize())
        .thenReturn(DomainEventName.PROCESS_BULK_IMAGE_EDIT_RESIZE);
    List<GraphicImageDetail> graphicImageDetails = new ArrayList<>();
    ReflectionTestUtils.setField(wrapper, "eventBasedProcessingEnabled", true);
    wrapper.resizeEditedImages(graphicImageDetails,  GROUP_CODE, CLIENT_ID);
    Mockito.verify(kafkaPublisher)
        .send(eq(DomainEventName.PROCESS_BULK_IMAGE_EDIT_RESIZE), eq(GROUP_CODE), Mockito.any(ImageProcessingModel.class));
    Mockito.verify(kafkaTopicProperties).getImageEditResize();
  }

  @Test
  void resizeRevisedImages() throws Exception {
    Mockito.when(kafkaTopicProperties.getRevisedImageResizeStatus())
        .thenReturn(DomainEventName.GRAPHIC_EDITED_REVISED_IMAGE_STATUS_EVENT);
    List<GraphicImageDetail> graphicImageDetails = new ArrayList<>();
    ReflectionTestUtils.setField(wrapper , "executorService", MoreExecutors.newDirectExecutorService());
    wrapper.resizeRevisedImages(graphicImageDetails, GROUP_CODE, CLIENT_ID);
    Mockito.verify(kafkaPublisher).send(Mockito.anyString(), eq(GROUP_CODE), Mockito.any(BulkImageProcessResponse.class));
    Mockito.verify(kafkaTopicProperties).getRevisedImageResizeStatus();
  }

  @Test
  void resizeRevisedImagesEventBased() throws Exception {
    Mockito.when(kafkaTopicProperties.getRevisedImageResize())
        .thenReturn(DomainEventName.PROCESS_BULK_IMAGE_REVISED_RESIZE);
    List<GraphicImageDetail> graphicImageDetails = new ArrayList<>();
    ReflectionTestUtils.setField(wrapper, "eventBasedProcessingEnabled", true);
    wrapper.resizeRevisedImages(graphicImageDetails, GROUP_CODE, CLIENT_ID);
    Mockito.verify(kafkaPublisher)
        .send(eq(DomainEventName.PROCESS_BULK_IMAGE_REVISED_RESIZE), eq(GROUP_CODE), Mockito.any(ImageProcessingModel.class));
    Mockito.verify(kafkaTopicProperties).getRevisedImageResize();

  }

  @Test
  void scaleEditedImages() throws Exception {
    Mockito.when(kafkaTopicProperties.getEditedImageScaleStatus())
        .thenReturn(DomainEventName.GRAPHIC_EDITED_IMAGE_SCALE_STATUS_EVENT);
    ScaleEditedImageRequest scaleEditedImageRequest = ScaleEditedImageRequest.builder()
        .imageRequests(Collections.singletonList(new ScaleImageRequest("", "", "", false, true))).build();
    List<GraphicImageDetail> graphicImageDetails = new ArrayList<>();
    ReflectionTestUtils.setField(wrapper , "executorService", MoreExecutors.newDirectExecutorService());
    wrapper.scaleEditedImages(new ArrayList<>(), GROUP_CODE, CLIENT_ID, scaleEditedImageRequest, CLIENT_ID);
    Mockito.verify(kafkaPublisher).send(Mockito.anyString(), eq(GROUP_CODE), Mockito.any(ScaleEditedImagesResponse.class));
    Mockito.verify(kafkaTopicProperties).getEditedImageScaleStatus();
  }

  @Test
  void scaleEditedImagesEventBased() throws Exception {
    Mockito.when(kafkaTopicProperties.getEditedImageScaling())
        .thenReturn(DomainEventName.PROCESS_BULK_IMAGE_EDIT_SCALING);
    ScaleEditedImageRequest scaleEditedImageRequest = ScaleEditedImageRequest.builder()
        .imageRequests(Collections.singletonList(new ScaleImageRequest("", "", "", false, true))).build();
    List<GraphicImageDetail> graphicImageDetails = new ArrayList<>();
    ReflectionTestUtils.setField(wrapper, "eventBasedProcessingEnabled", true);
    wrapper.scaleEditedImages(new ArrayList<>(), GROUP_CODE, CLIENT_ID, scaleEditedImageRequest, CLIENT_ID);
    Mockito.verify(kafkaPublisher)
        .send(eq(DomainEventName.PROCESS_BULK_IMAGE_EDIT_SCALING), eq(GROUP_CODE), Mockito.any(ImageProcessingModel.class));
    Mockito.verify(kafkaTopicProperties).getEditedImageScaling();
  }

  @Test
  void scaleEditedImages_success() throws Exception {
    List<CustomGraphicsSettings> customGraphicsSettings = new ArrayList<>();
    customGraphicsSettings.add(new CustomGraphicsSettings(80, 80, new GraphicDimension(100, 100)));
    String settings = MAPPER.writeValueAsString(customGraphicsSettings);
    ScaleEditedImageRequest scaleEditedImageRequest = new ScaleEditedImageRequest();
    scaleEditedImageRequest.setProductCode(GROUP_CODE);
    scaleEditedImageRequest.setCustomGraphicsSettings(settings);
    ScaleImageRequest imageRequest =
        ScaleImageRequest.builder().imagePathLocation(SOURCE_PATH).imageName(IMAGE_NAME).hashCode(HASH_CODE)
            .isActive(false).build();
    ScaleImageRequest imageRequest1 =
        ScaleImageRequest.builder().imagePathLocation(SOURCE_PATH).imageName(IMAGE_NAME).hashCode(HASH_CODE)
            .isActive(true).build();
    scaleEditedImageRequest.setImageRequests(Arrays.asList(imageRequest, imageRequest1));
    wrapper.scaleEditedImages(customGraphicsSettings, GROUP_CODE, CLIENT_ID, scaleEditedImageRequest, CLIENT_ID);
    Mockito.verify(executorService).execute(Mockito.any(Runnable.class));
  }

  @Test
  void scaleWebpEditedImages_success() throws Exception {
    ReflectionTestUtils.setField(wrapper, "webpConversionEnabled", true);
    List<CustomGraphicsSettings> customGraphicsSettings = new ArrayList<>();
    customGraphicsSettings.add(new CustomGraphicsSettings(80, 80, new GraphicDimension(100, 100)));
    String settings = MAPPER.writeValueAsString(customGraphicsSettings);
    ScaleEditedImageRequest scaleEditedImageRequest = new ScaleEditedImageRequest();
    scaleEditedImageRequest.setProductCode(GROUP_CODE);
    scaleEditedImageRequest.setCustomGraphicsSettings(settings);
    ScaleImageRequest imageRequest =
        ScaleImageRequest.builder().imagePathLocation(SOURCE_PATH).imageName(IMAGE_NAME).hashCode(HASH_CODE)
            .isActive(false).build();
    ScaleImageRequest imageRequest1 =
        ScaleImageRequest.builder().imagePathLocation(SOURCE_PATH).imageName(IMAGE_NAME).hashCode(HASH_CODE)
            .isActive(true).build();
    scaleEditedImageRequest.setImageRequests(Arrays.asList(imageRequest, imageRequest1));
    wrapper.scaleEditedImages(customGraphicsSettings, GROUP_CODE, CLIENT_ID, scaleEditedImageRequest, CLIENT_ID);
    Mockito.verify(executorService).execute(Mockito.any(Runnable.class));
  }

  @Test
  void processImagesTest_sucessSeqUploadTest() throws Exception {
    Mockito.when(kafkaTopicProperties.getImageProcessStatusNoPriority())
        .thenReturn(DomainEventName.GRAPHIC_IMAGE_DETAIL_STATUS_EVENT_NAME);
    List<GraphicImageDetail> graphicImageDetails = new ArrayList<>();
    CustomGraphicsSettings customGraphicsSettings = new CustomGraphicsSettings();
    GraphicImageDetail imageDetail =
        new GraphicImageDetail(HASH_CODE, SOURCE_PATH, DESTINATION_PATH, customGraphicsSettings,
            PREFIX_PATH, GROUP_CODE);
    imageDetail.setCommonImage(true);
    graphicImageDetails.add(imageDetail);
    List<Future<ImageResponse>> futureResults = new ArrayList<>();
    Future<ImageResponse> imageResponseFuture = getImageResponseFutureUpload();
    futureResults.add(imageResponseFuture);
    Mockito.when(executorService.invokeAll(Mockito.any(List.class)))
        .thenReturn(futureResults);
    wrapper.processImages(graphicImageDetails, GROUP_CODE, USERNAME, STORE_ID, CLIENT_ID, Boolean.FALSE, Boolean.FALSE, false, null, 0);
    Mockito.verify(executorService).invokeAll(Mockito.any(List.class));
    Mockito.verify(kafkaPublisher).send(Mockito.anyString(), eq(GROUP_CODE), Mockito.any(BulkImageProcessResponse.class));
    Mockito.verify(fileStorageService).saveImages(null, imageResponseFuture.get(), false, null, null);
    Mockito.verify(kafkaTopicProperties).getImageProcessStatusNoPriority();
  }

  @Test
  void processImagesTest_sucessSeqRezizeTest() throws Exception {
    Mockito.when(kafkaTopicProperties.getImageProcessStatusNoPriority())
        .thenReturn(DomainEventName.GRAPHIC_IMAGE_DETAIL_STATUS_EVENT_NAME);
    ReflectionTestUtils.setField(wrapper, "enableParallelImageResize", false);
    List<GraphicImageDetail> graphicImageDetails = new ArrayList<>();
    CustomGraphicsSettings customGraphicsSettings = new CustomGraphicsSettings();
    GraphicImageDetail imageDetail =
        new GraphicImageDetail(HASH_CODE, SOURCE_PATH, DESTINATION_PATH, customGraphicsSettings, PREFIX_PATH,
            GROUP_CODE);
    imageDetail.setCommonImage(true);
    graphicImageDetails.add(imageDetail);
    List<Future<ImageResponse>> futureResults = new ArrayList<>();
    Future<ImageResponse> imageResponseFuture = getImageResponseFutureUpload();
    futureResults.add(imageResponseFuture);
    Mockito.when(graphicsProcessorService.scale(Mockito.anyString(), Mockito.anyString(),
        Mockito.any(CustomGraphicsSettings.class), Mockito.anyString(), Mockito.anyBoolean(), Mockito.anyBoolean())).thenReturn(new ImageResultDetail());
    wrapper.processImages(graphicImageDetails, GROUP_CODE, USERNAME, STORE_ID, CLIENT_ID, Boolean.FALSE, Boolean.FALSE,
        false, null, 0);
    Mockito.verify(kafkaPublisher)
        .send(Mockito.anyString(), eq(GROUP_CODE), Mockito.any(BulkImageProcessResponse.class));
    Mockito.verify(kafkaTopicProperties).getImageProcessStatusNoPriority();
  }

  @Test
  void processImagesTest_sucess() throws Exception {
    Mockito.when(kafkaTopicProperties.getImageProcessStatusNoPriority())
        .thenReturn(DomainEventName.GRAPHIC_IMAGE_DETAIL_STATUS_EVENT_NAME);
    List<GraphicImageDetail> graphicImageDetails = new ArrayList<>();
    CustomGraphicsSettings customGraphicsSettings = new CustomGraphicsSettings();
    GraphicImageDetail imageDetail =
        new GraphicImageDetail(HASH_CODE, SOURCE_PATH, DESTINATION_PATH, customGraphicsSettings,
            PREFIX_PATH, GROUP_CODE);
    imageDetail.setCommonImage(true);
    graphicImageDetails.add(imageDetail);
    List<Future<ImageResponse>> futureResults = new ArrayList<>();
    Future<ImageResponse> imageResponseFuture = getImageResponseFuture();
    futureResults.add(imageResponseFuture);
    Mockito.when(executorService.invokeAll(Mockito.any(List.class)))
        .thenReturn(futureResults);
    wrapper.processImages(graphicImageDetails, GROUP_CODE, USERNAME, STORE_ID, CLIENT_ID, Boolean.FALSE, Boolean.FALSE, false, null, 0);
    Mockito.verify(executorService).invokeAll(Mockito.any(List.class));
    Mockito.verify(kafkaPublisher).send(Mockito.anyString(), eq(GROUP_CODE), Mockito.any(BulkImageProcessResponse.class));
    Mockito.verify(kafkaTopicProperties).getImageProcessStatusNoPriority();
  }

  @Test
  void processImagesTest_sucess_Priority1() throws Exception {
    Mockito.when(kafkaTopicProperties.getImageProcessStatusPriority1())
        .thenReturn(DomainEventName.GRAPHIC_IMAGE_DETAIL_STATUS_EVENT_NAME_PRIORITY_1);
    List<GraphicImageDetail> graphicImageDetails = new ArrayList<>();
    CustomGraphicsSettings customGraphicsSettings = new CustomGraphicsSettings();
    GraphicImageDetail imageDetail =
            new GraphicImageDetail(HASH_CODE, SOURCE_PATH, DESTINATION_PATH, customGraphicsSettings,
                    PREFIX_PATH, GROUP_CODE);
    imageDetail.setCommonImage(true);
    graphicImageDetails.add(imageDetail);
    List<Future<ImageResponse>> futureResults = new ArrayList<>();
    Future<ImageResponse> imageResponseFuture = getImageResponseFuture();
    futureResults.add(imageResponseFuture);
    Mockito.when(executorService.invokeAll(Mockito.any(List.class)))
            .thenReturn(futureResults);
    wrapper.processImages(graphicImageDetails, GROUP_CODE, USERNAME, STORE_ID, CLIENT_ID, Boolean.FALSE, Boolean.FALSE, false, null, 1);
    Mockito.verify(executorService).invokeAll(Mockito.any(List.class));
    Mockito.verify(kafkaPublisher).send(Mockito.anyString(), eq(GROUP_CODE), Mockito.any(BulkImageProcessResponse.class));
    Mockito.verify(kafkaTopicProperties).getImageProcessStatusNoPriority();
    Mockito.verify(kafkaTopicProperties).getImageProcessStatusPriority1();
  }

  @Test
  void processImagesTest_sucess_Priority2() throws Exception {
    Mockito.when(kafkaTopicProperties.getImageProcessStatusPriority1())
        .thenReturn(DomainEventName.GRAPHIC_IMAGE_DETAIL_STATUS_EVENT_NAME_PRIORITY_2);
    List<GraphicImageDetail> graphicImageDetails = new ArrayList<>();
    CustomGraphicsSettings customGraphicsSettings = new CustomGraphicsSettings();
    GraphicImageDetail imageDetail =
            new GraphicImageDetail(HASH_CODE, SOURCE_PATH, DESTINATION_PATH, customGraphicsSettings,
                    PREFIX_PATH, GROUP_CODE);
    imageDetail.setCommonImage(true);
    graphicImageDetails.add(imageDetail);
    List<Future<ImageResponse>> futureResults = new ArrayList<>();
    Future<ImageResponse> imageResponseFuture = getImageResponseFuture();
    futureResults.add(imageResponseFuture);
    Mockito.when(executorService.invokeAll(Mockito.any(List.class)))
            .thenReturn(futureResults);
    Mockito.when(kafkaTopicProperties.getImageProcessStatusPriority2()).thenReturn(TOPIC);
    wrapper.processImages(graphicImageDetails, GROUP_CODE, USERNAME, STORE_ID, CLIENT_ID, Boolean.FALSE, Boolean.FALSE, false, null, 2);
    Mockito.verify(executorService).invokeAll(Mockito.any(List.class));
    Mockito.verify(kafkaPublisher).send(Mockito.eq(TOPIC), eq(GROUP_CODE), Mockito.any(BulkImageProcessResponse.class));
    Mockito.verify(kafkaTopicProperties).getImageProcessStatusNoPriority();
    Mockito.verify(kafkaTopicProperties).getImageProcessStatusPriority2();
  }

  @Test
  void processImagesTestPriority1_sucess() throws Exception {
    List<GraphicImageDetail> graphicImageDetails = new ArrayList<>();
    CustomGraphicsSettings customGraphicsSettings = new CustomGraphicsSettings();
    GraphicImageDetail imageDetail =
            new GraphicImageDetail(HASH_CODE, SOURCE_PATH, DESTINATION_PATH, customGraphicsSettings,
                    PREFIX_PATH, GROUP_CODE);
    imageDetail.setCommonImage(true);
    graphicImageDetails.add(imageDetail);
    List<Future<ImageResponse>> futureResults = new ArrayList<>();
    Future<ImageResponse> imageResponseFuture = getImageResponseFuture();
    futureResults.add(imageResponseFuture);
    Mockito.when(executorService.invokeAll(Mockito.any(List.class)))
            .thenReturn(futureResults);
    Mockito.when(kafkaTopicProperties.getImageResizeStatusPriority1()).thenReturn(TOPIC);
    wrapper.processImages(graphicImageDetails, GROUP_CODE, USERNAME, STORE_ID, CLIENT_ID, Boolean.TRUE, Boolean.FALSE, false, null, 1);
    Mockito.verify(executorService).invokeAll(Mockito.any(List.class));
    Mockito.verify(kafkaPublisher).send(Mockito.eq(TOPIC), eq(GROUP_CODE), Mockito.any(BulkImageProcessResponse.class));
    Mockito.verify(kafkaTopicProperties).getImageResizeStatusNoPriority();
    Mockito.verify(kafkaTopicProperties).getImageResizeStatusPriority1();
  }

  @Test
  void processImagesTestPriority2_sucess() throws Exception {
    List<GraphicImageDetail> graphicImageDetails = new ArrayList<>();
    CustomGraphicsSettings customGraphicsSettings = new CustomGraphicsSettings();
    GraphicImageDetail imageDetail =
            new GraphicImageDetail(HASH_CODE, SOURCE_PATH, DESTINATION_PATH, customGraphicsSettings,
                    PREFIX_PATH, GROUP_CODE);
    imageDetail.setCommonImage(true);
    graphicImageDetails.add(imageDetail);
    List<Future<ImageResponse>> futureResults = new ArrayList<>();
    Future<ImageResponse> imageResponseFuture = getImageResponseFuture();
    futureResults.add(imageResponseFuture);
    Mockito.when(executorService.invokeAll(Mockito.any(List.class)))
            .thenReturn(futureResults);
    Mockito.when(kafkaTopicProperties.getImageResizeStatusPriority2()).thenReturn(TOPIC);
    wrapper.processImages(graphicImageDetails, GROUP_CODE, USERNAME, STORE_ID, CLIENT_ID, Boolean.TRUE, Boolean.FALSE, false, null, 2);
    Mockito.verify(executorService).invokeAll(Mockito.any(List.class));
    Mockito.verify(kafkaPublisher).send(Mockito.eq(TOPIC), eq(GROUP_CODE), Mockito.any(BulkImageProcessResponse.class));
    Mockito.verify(kafkaTopicProperties).getImageResizeStatusNoPriority();
    Mockito.verify(kafkaTopicProperties).getImageResizeStatusPriority2();
  }

  @Test
  void resizeImagesTest() throws Exception {
    List<GraphicImageDetail> graphicImageDetails = new ArrayList<>();
    CustomGraphicsSettings customGraphicsSettings = new CustomGraphicsSettings();
    GraphicImageDetail imageDetail =
        new GraphicImageDetail(HASH_CODE, SOURCE_PATH, DESTINATION_PATH, customGraphicsSettings,
            PREFIX_PATH, GROUP_CODE);
    graphicImageDetails.add(imageDetail);
    List<Future<ImageResponse>> futureResults = new ArrayList<>();
    Future<ImageResponse> imageResponseFuture = getImageResponseFuture();
    futureResults.add(imageResponseFuture);
    Mockito.when(executorService.invokeAll(Mockito.any(List.class)))
        .thenReturn(futureResults);
    Mockito.when(kafkaTopicProperties.getImageResizeStatusNoPriority()).thenReturn(TOPIC);
    wrapper.processImages(graphicImageDetails, GROUP_CODE, USERNAME, STORE_ID, CLIENT_ID, Boolean.TRUE, Boolean.FALSE, false, null, 0);
    Mockito.verify(executorService).invokeAll(Mockito.any(List.class));
    Mockito.verify(kafkaPublisher).send(Mockito.eq(TOPIC), eq(GROUP_CODE), bulkImageProcessResponseArgumentCaptor.capture());
    assertEquals(GROUP_CODE, bulkImageProcessResponseArgumentCaptor.getValue().getGroupCode());
    Assertions.assertNotNull(bulkImageProcessResponseArgumentCaptor.getValue().getImageResponses());
    Mockito.verify(kafkaTopicProperties).getImageResizeStatusNoPriority();
  }

  @Test
  void resizeEditedImagesTest() throws Exception {
    List<GraphicImageDetail> graphicImageDetails = new ArrayList<>();
    CustomGraphicsSettings customGraphicsSettings = new CustomGraphicsSettings();
    GraphicImageDetail imageDetail =
        new GraphicImageDetail(HASH_CODE, SOURCE_PATH, DESTINATION_PATH, customGraphicsSettings,
            PREFIX_PATH, GROUP_CODE);
    graphicImageDetails.add(imageDetail);
    List<Future<ImageResponse>> futureResults = new ArrayList<>();
    Future<ImageResponse> imageResponseFuture = getImageResponseFuture();
    futureResults.add(imageResponseFuture);
    Mockito.when(executorService.invokeAll(Mockito.any(List.class)))
        .thenReturn(futureResults);
    Mockito.when(kafkaTopicProperties.getEditedImageResizeStatus()).thenReturn(TOPIC);
    wrapper.processImages(graphicImageDetails, GROUP_CODE, USERNAME, STORE_ID, CLIENT_ID, Boolean.TRUE, Boolean.TRUE, false, null, 0);
    Mockito.verify(executorService).invokeAll(Mockito.any(List.class));
    Mockito.verify(kafkaPublisher).send(Mockito.eq(TOPIC), eq(GROUP_CODE), bulkImageProcessResponseArgumentCaptor.capture());
    assertEquals(GROUP_CODE, bulkImageProcessResponseArgumentCaptor.getValue().getGroupCode());
    Assertions.assertNotNull(bulkImageProcessResponseArgumentCaptor.getValue().getImageResponses());
    Mockito.verify(kafkaTopicProperties).getEditedImageResizeStatus();
  }

  @Test
  void resizeRevisedImagesTest() throws Exception {
    List<GraphicImageDetail> graphicImageDetails = new ArrayList<>();
    CustomGraphicsSettings customGraphicsSettings = new CustomGraphicsSettings();
    GraphicImageDetail imageDetail =
        new GraphicImageDetail(HASH_CODE, SOURCE_PATH, DESTINATION_PATH, customGraphicsSettings,
            PREFIX_PATH, GROUP_CODE);
    graphicImageDetails.add(imageDetail);
    List<Future<ImageResponse>> futureResults = new ArrayList<>();
    Future<ImageResponse> imageResponseFuture = getImageResponseFuture();
    futureResults.add(imageResponseFuture);
    Mockito.when(executorService.invokeAll(Mockito.any(List.class)))
        .thenReturn(futureResults);
    Mockito.when(kafkaTopicProperties.getRevisedImageResizeStatus()).thenReturn(TOPIC);
    wrapper.processImages(graphicImageDetails, GROUP_CODE, USERNAME, STORE_ID, CLIENT_ID, Boolean.TRUE, false, true, null, 0);
    Mockito.verify(executorService).invokeAll(Mockito.any(List.class));
    Mockito.verify(kafkaPublisher)
        .send(Mockito.eq(TOPIC), eq(GROUP_CODE), bulkImageProcessResponseArgumentCaptor.capture());
    assertEquals(GROUP_CODE, bulkImageProcessResponseArgumentCaptor.getValue().getGroupCode());
    Assertions.assertNotNull(bulkImageProcessResponseArgumentCaptor.getValue().getImageResponses());
    Mockito.verify(kafkaTopicProperties).getRevisedImageResizeStatus();
  }

  @Test
  void scaleEditedImagesTest() throws Exception {
    ScaleEditedImageRequest scaleEditedImageRequest = ScaleEditedImageRequest.builder()
        .imageRequests(Collections.singletonList(new ScaleImageRequest("", "", "", true, true))).build();
    List<GraphicImageDetail> graphicImageDetails = new ArrayList<>();
    CustomGraphicsSettings customGraphicsSettings = new CustomGraphicsSettings();
    GraphicImageDetail imageDetail =
        new GraphicImageDetail(HASH_CODE, SOURCE_PATH, DESTINATION_PATH, customGraphicsSettings, PREFIX_PATH,
            GROUP_CODE);
    graphicImageDetails.add(imageDetail);
    List<Future<ImageResponse>> futureResults = new ArrayList<>();
    Future<ImageResponse> imageResponseFuture = getImageResponseFuture();
    futureResults.add(imageResponseFuture);
    Mockito.when(executorService.invokeAll(Mockito.any(List.class))).thenReturn(futureResults);
    Mockito.when(kafkaTopicProperties.getEditedImageScaleStatus()).thenReturn(TOPIC);
    wrapper.processImages(graphicImageDetails, GROUP_CODE, USERNAME, STORE_ID, CLIENT_ID, Boolean.FALSE, Boolean.TRUE, false,
        scaleEditedImageRequest, 0);
    Mockito.verify(executorService).invokeAll(Mockito.any(List.class));
    Mockito.verify(kafkaPublisher)
        .send(Mockito.eq(TOPIC), eq(GROUP_CODE), Mockito.any(ScaleEditedImagesResponse.class));
    Mockito.verify(kafkaTopicProperties).getEditedImageScaleStatus();
  }

  @Test
  void processImagesTest_fail() throws Exception {
    List<GraphicImageDetail> graphicImageDetails = new ArrayList<>();
    CustomGraphicsSettings customGraphicsSettings = new CustomGraphicsSettings();
    GraphicImageDetail imageDetail =
        new GraphicImageDetail(HASH_CODE, SOURCE_PATH, DESTINATION_PATH, customGraphicsSettings,
            PREFIX_PATH, GROUP_CODE);
    graphicImageDetails.add(imageDetail);
    List<Future<ImageResponse>> futureResults = new ArrayList<>();
    Future<ImageResponse> imageResponseFuture = getImageResponseFuture();
    futureResults.add(imageResponseFuture);
    Mockito.when(executorService.invokeAll(Mockito.any(List.class)))
        .thenReturn(futureResults);
    Mockito.doThrow(new RuntimeException()).when(kafkaPublisher)
        .send(Mockito.anyString(), eq(GROUP_CODE), Mockito.any(BulkImageProcessResponse.class));
    Mockito.when(kafkaTopicProperties.getImageProcessStatusNoPriority()).thenReturn(TOPIC);
    wrapper.processImages(graphicImageDetails, GROUP_CODE, USERNAME, STORE_ID, CLIENT_ID, Boolean.FALSE, Boolean.FALSE, false, null, 0);
    Mockito.verify(kafkaPublisher)
        .send(Mockito.eq(TOPIC), eq(GROUP_CODE), Mockito.any(BulkImageProcessResponse.class));
    Mockito.verify(executorService).invokeAll(Mockito.any(List.class));
    Mockito.verify(kafkaTopicProperties).getImageProcessStatusNoPriority();
  }

  @Test
  void processImagesTest_resizefail() throws Exception {
    List<GraphicImageDetail> graphicImageDetails = new ArrayList<>();
    CustomGraphicsSettings customGraphicsSettings = new CustomGraphicsSettings();
    GraphicImageDetail imageDetail =
        new GraphicImageDetail(HASH_CODE, SOURCE_PATH, DESTINATION_PATH, customGraphicsSettings,
            PREFIX_PATH, GROUP_CODE);
    graphicImageDetails.add(imageDetail);
    List<Future<ImageResponse>> futureResults = new ArrayList<>();
    Future<ImageResponse> imageResponseFuture = getImageResponseFuture();
    futureResults.add(imageResponseFuture);
    Mockito.when(executorService.invokeAll(Mockito.any(List.class)))
        .thenReturn(futureResults);
    Mockito.doThrow(new RuntimeException()).when(kafkaPublisher)
        .send(Mockito.anyString(), eq(GROUP_CODE), Mockito.any(BulkImageProcessResponse.class));
    Mockito.when(kafkaTopicProperties.getImageResizeStatusNoPriority()).thenReturn(TOPIC);
    wrapper.processImages(graphicImageDetails, GROUP_CODE, USERNAME, STORE_ID, CLIENT_ID, Boolean.TRUE, Boolean.FALSE, false,null, 0);
    Mockito.verify(executorService).invokeAll(Mockito.any(List.class));
    Mockito.verify(kafkaPublisher)
        .send(Mockito.eq(TOPIC), eq(GROUP_CODE), Mockito.any(BulkImageProcessResponse.class));
    Mockito.verify(kafkaTopicProperties).getImageResizeStatusNoPriority();
  }

  @Test
  void processImagesTest_resizeEditedfail() throws Exception {
    List<GraphicImageDetail> graphicImageDetails = new ArrayList<>();
    CustomGraphicsSettings customGraphicsSettings = new CustomGraphicsSettings();
    GraphicImageDetail imageDetail =
        new GraphicImageDetail(HASH_CODE, SOURCE_PATH, DESTINATION_PATH, customGraphicsSettings,
            PREFIX_PATH, GROUP_CODE);
    graphicImageDetails.add(imageDetail);
    List<Future<ImageResponse>> futureResults = new ArrayList<>();
    Future<ImageResponse> imageResponseFuture = getImageResponseFuture();
    futureResults.add(imageResponseFuture);
    Mockito.when(executorService.invokeAll(Mockito.any(List.class)))
        .thenReturn(futureResults);
    Mockito.doThrow(new RuntimeException()).when(kafkaPublisher)
        .send(Mockito.anyString(), eq(GROUP_CODE), Mockito.any(BulkImageProcessResponse.class));
    Mockito.when(kafkaTopicProperties.getEditedImageResizeStatus()).thenReturn(TOPIC);
    wrapper.processImages(graphicImageDetails, GROUP_CODE, USERNAME, STORE_ID, CLIENT_ID, Boolean.TRUE, Boolean.TRUE, false,null, 0);
    Mockito.verify(executorService).invokeAll(Mockito.any(List.class));
    Mockito.verify(kafkaPublisher)
        .send(Mockito.eq(TOPIC), eq(GROUP_CODE), Mockito.any(BulkImageProcessResponse.class));
    Mockito.verify(kafkaTopicProperties).getEditedImageResizeStatus();
  }

  @Test
  void processImagesTest_scaleEditedfail() throws Exception {
    List<GraphicImageDetail> graphicImageDetails = new ArrayList<>();
    CustomGraphicsSettings customGraphicsSettings = new CustomGraphicsSettings();
    GraphicImageDetail imageDetail =
        new GraphicImageDetail(HASH_CODE, SOURCE_PATH, DESTINATION_PATH, customGraphicsSettings,
            PREFIX_PATH, GROUP_CODE);
    graphicImageDetails.add(imageDetail);
    List<Future<ImageResponse>> futureResults = new ArrayList<>();
    Future<ImageResponse> imageResponseFuture = getImageResponseFuture();
    futureResults.add(imageResponseFuture);
    Mockito.when(executorService.invokeAll(Mockito.any(List.class)))
        .thenReturn(futureResults);
    Mockito.doThrow(new RuntimeException()).when(kafkaPublisher)
        .send(Mockito.anyString(), eq(GROUP_CODE), Mockito.any(BulkImageProcessResponse.class));
    wrapper.processImages(graphicImageDetails, GROUP_CODE, USERNAME, STORE_ID, CLIENT_ID, Boolean.FALSE, Boolean.TRUE, false,null, 0);
    Mockito.verify(executorService).invokeAll(Mockito.any(List.class));
  }

  @Test
  void scaleListOfImagesTest() {
    ReflectionTestUtils.setField(wrapper, "imageScaleLimit", 1);
    ResizeImageScalingModel resizeImageScalingModel = new ResizeImageScalingModel();
    resizeImageScalingModel.setUniqueIdentifier(GROUP_CODE);
    ImagePaths imagePaths = new ImagePaths();
    imagePaths.setFinalPath("final");
    imagePaths.setSourcePath("source");
    imagePaths.setSettings(
      "{\"dpi\":72,\"quality\":75.0,\"dimession\":{\"width\":800,\"height\":800},\"height\":800,\"width\":800}");
    resizeImageScalingModel.setImagePathsList(Collections.singletonList(imagePaths));
    Mockito.when(gcsProperties.getOrderTemporaryImageSourcePath()).thenReturn("/tmp/order");
    Mockito.when(kafkaTopicProperties.getImageResizeScalingResult()).thenReturn(TOPIC);
    wrapper.scaleListOfImages(resizeImageScalingModel);
    Mockito.verify(gcsProperties).getOrderTemporaryImageSourcePath();
    Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getImageResizeScalingResult();
    Mockito.verify(kafkaPublisher)
      .send(Mockito.eq(TOPIC), eq(GROUP_CODE), Mockito.any(ScaleImagesResponse.class));
  }

  @Test
  void scaleListOfImagesPartialSuccessTest() {
    ReflectionTestUtils.setField(wrapper, "imageScaleLimit", 1);
    ResizeImageScalingModel resizeImageScalingModel = new ResizeImageScalingModel();
    resizeImageScalingModel.setUniqueIdentifier(GROUP_CODE);
    ImagePaths imagePaths = new ImagePaths();
    imagePaths.setFinalPath("final");
    imagePaths.setSourcePath("source");
    imagePaths.setSettings(
      "{\"dpi\":72,\"quality\":75.0,\"dimession\":{\"width\":800,\"height\":800},\"height\":800,\"width\":800}");
    resizeImageScalingModel.setImagePathsList(Collections.singletonList(imagePaths));
    Mockito.doThrow(ApplicationRuntimeException.class).when(gcsService)
      .downloadFileTo(Mockito.anyString(), Mockito.anyString(), Mockito.anyString());
    Mockito.when(gcsProperties.getOrderTemporaryImageSourcePath()).thenReturn("/tmp/order");
    Mockito.when(kafkaTopicProperties.getImageResizeScalingResult()).thenReturn(TOPIC);
    wrapper.scaleListOfImages(resizeImageScalingModel);
    Mockito.verify(gcsProperties).getOrderTemporaryImageSourcePath();
    Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getImageResizeScalingResult();
    Mockito.verify(kafkaPublisher)
      .send(Mockito.eq(TOPIC), eq(GROUP_CODE), Mockito.any(ScaleImagesResponse.class));
  }

  @Test
  void scaleListOfImagesExceptionTest() {
    ReflectionTestUtils.setField(wrapper, "imageScaleLimit", 1);
    resizeImageScalingModel = new ResizeImageScalingModel();
    resizeImageScalingModel.setUniqueIdentifier(GROUP_CODE);
    imagePaths = new ImagePaths();
    imagePaths.setFinalPath("final");
    imagePaths.setSourcePath("source");
    imagePaths.setSettings(
      "{\"dpi\":72,\"quality\":75.0,\"dimession\":{\"width\":800,\"height\":800},\"height\":800,\"width\":800}");
    resizeImageScalingModel.setImagePathsList(Collections.singletonList(imagePaths));
    Mockito.when(kafkaTopicProperties.getImageResizeScalingResult()).thenReturn(EVENT);
    Mockito.doThrow(ApplicationRuntimeException.class).when(gcsProperties)
      .getOrderTemporaryImageSourcePath();
    wrapper.scaleListOfImages(resizeImageScalingModel);
    Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getImageResizeScalingResult();
    Mockito.verify(gcsProperties).getOrderTemporaryImageSourcePath();
    Mockito.verify(kafkaPublisher)
      .send(Mockito.anyString(), eq(GROUP_CODE), Mockito.any(ScaleImagesResponse.class));
  }

  @Test
  void validateImageListSizeWithFailureTest() {
    resizeImageScalingModel = new ResizeImageScalingModel();
    imagePaths = new ImagePaths();
    imagePaths.setSourcePath("source");
    resizeImageScalingModel.setImagePathsList(Arrays.asList(imagePaths, imagePaths));
    ReflectionTestUtils.setField(wrapper, "imageScaleLimit", 1);
    List<ImagePathResult> failedImageList = new ArrayList<>();
    wrapper.validateImageListSize(resizeImageScalingModel, failedImageList);
    assertEquals(1, failedImageList.size());
    assertEquals(1, resizeImageScalingModel.getImagePathsList().size());
  }

  @Test
  void validateImageListSizeTest() {
      resizeImageScalingModel = new ResizeImageScalingModel();
      imagePaths = new ImagePaths();
      imagePaths.setSourcePath("source");
      resizeImageScalingModel.setImagePathsList(Arrays.asList(imagePaths, imagePaths));
      ReflectionTestUtils.setField(wrapper, "imageScaleLimit", 2);
      List<ImagePathResult> failedImageList = new ArrayList<>();
      wrapper.validateImageListSize(resizeImageScalingModel, failedImageList);
      assertEquals(0, failedImageList.size());
      assertEquals(2, resizeImageScalingModel.getImagePathsList().size());
  }

  @Test
  void scaleActiveProductNewImages_nullRequest_shouldLogErrorAndReturn() throws Exception {
    ReflectionTestUtils.setField(wrapper, "tempDirForActiveProductImageConversion", "/tmp");
    XgpImageScaleRequest nullRequest = null;

    ApplicationException exception = Assertions.assertThrows(ApplicationException.class, () -> {
      wrapper.scaleActiveProductNewImages(STORE_ID, CLIENT_ID, nullRequest, StringUtils.EMPTY);
    });

    Assertions.assertNotNull(exception);
  }

  @Test
  void scaleActiveProductNewImages_nullImageBytes_shouldLogErrorAndReturn() throws Exception {
    ReflectionTestUtils.setField(wrapper, "tempDirForActiveProductImageConversion", "/tmp");
    XgpImageScaleRequest request = new XgpImageScaleRequest();
    request.setImageBytes(null);

    ApplicationException exception = Assertions.assertThrows(ApplicationException.class, () -> {
      wrapper.scaleActiveProductNewImages(STORE_ID, CLIENT_ID, request, StringUtils.EMPTY);
    });

    Assertions.assertNotNull(exception);
  }

  @Test
  void scaleActiveProductNewImages_emptyImageBytes_shouldLogErrorAndReturn() throws Exception {
    ReflectionTestUtils.setField(wrapper, "tempDirForActiveProductImageConversion", "/tmp");
    XgpImageScaleRequest request = new XgpImageScaleRequest();
    request.setImageBytes(null);

    ApplicationException exception = Assertions.assertThrows(ApplicationException.class, () -> {
      wrapper.scaleActiveProductNewImages(STORE_ID, CLIENT_ID, request, StringUtils.EMPTY);
    });

    Assertions.assertNotNull(exception);
  }

  @Test
  void scaleActiveProductNewImages_validRequest_allImageTypes_success() throws Exception {
    ReflectionTestUtils.setField(wrapper, "tempDirForActiveProductImageConversion", "/tmp");
    ReflectionTestUtils.setField(wrapper, "webpConversionEnabled", false);
    byte[] imageBytes = "test image bytes".getBytes();
    XgpImageScaleRequest request = new XgpImageScaleRequest();
    request.setImageBytes(jpegHeader);

    FullImageUploadRequest fullRequest = new FullImageUploadRequest();
    fullRequest.setImagePath("/full/image/path");
    request.setFullImageUploadRequest(fullRequest);

    MediumImageUploadRequest mediumRequest = new MediumImageUploadRequest();
    mediumRequest.setImagePath("/medium/image/path");
    request.setMediumImageUploadRequest(mediumRequest);

    ThumbNailImageUploadRequest thumbnailRequest = new ThumbNailImageUploadRequest();
    thumbnailRequest.setImagePath("/thumbnail/image/path");
    request.setThumbNailImageUploadRequest(thumbnailRequest);

    ImageResultDetail fullResponse = new ImageResultDetail();
    fullResponse.setSuccess(true);
    fullResponse.setTempFileLocation("/temp/full_processed.jpg");
    fullResponse.setImagePathLocation("/temp/full_processed.jpg");

    ImageResultDetail mediumResponse = new ImageResultDetail();
    mediumResponse.setSuccess(true);
    mediumResponse.setTempFileLocation("/temp/medium_processed.jpg");
    mediumResponse.setImagePathLocation("/temp/medium_processed.jpg");

    ImageResultDetail thumbnailResponse = new ImageResultDetail();
    thumbnailResponse.setSuccess(true);
    thumbnailResponse.setTempFileLocation("/temp/thumbnail_processed.jpg");
    thumbnailResponse.setImagePathLocation("/temp/thumbnail_processed.jpg");

    Mockito.when(graphicsProcessorService.scale(Mockito.anyString(), Mockito.anyString(),
        Mockito.any(CustomGraphicsSettings.class), Mockito.anyString(), Mockito.anyBoolean(),
        Mockito.anyBoolean())).thenReturn(fullResponse).thenReturn(mediumResponse)
      .thenReturn(thumbnailResponse);

    ReflectionTestUtils.setField(wrapper, "executorService", MoreExecutors.newDirectExecutorService());

    wrapper.scaleActiveProductNewImages(STORE_ID, CLIENT_ID, request, StringUtils.EMPTY);
    Mockito.verify(fileStorageService, Mockito.times(3))
      .uploadToGcs(Mockito.any(ImageResultDetail.class), Mockito.eq(CLIENT_ID));
    Mockito.verify(imageConfig).getFullWidth();
    Mockito.verify(imageConfig).getFullDpi();
    Mockito.verify(imageConfig).getFullQuality();
    Mockito.verify(imageConfig).getFullHeight();
    Mockito.verify(imageConfig).getMediumDpi();
    Mockito.verify(imageConfig).getMediumHeight();
    Mockito.verify(imageConfig).getMediumQuality();
    Mockito.verify(imageConfig).getMediumWidth();
    Mockito.verify(imageConfig).getThumbnailHeight();
    Mockito.verify(imageConfig).getThumbnailDpi();
    Mockito.verify(imageConfig).getThumbnailQuality();
    Mockito.verify(imageConfig).getThumbnailWidth();
  }

  @Test
  void scaleActiveProductNewImages_validRequest_NoFullImageTypes_success() throws Exception {
    ReflectionTestUtils.setField(wrapper, "tempDirForActiveProductImageConversion", "/tmp");
    ReflectionTestUtils.setField(wrapper, "webpConversionEnabled", false);
    byte[] imageBytes = "test image bytes".getBytes();
    XgpImageScaleRequest request = new XgpImageScaleRequest();
    request.setImageBytes(jpegHeader);

    MediumImageUploadRequest mediumRequest = new MediumImageUploadRequest();
    mediumRequest.setImagePath("/medium/image/path");
    request.setMediumImageUploadRequest(mediumRequest);

    ThumbNailImageUploadRequest thumbnailRequest = new ThumbNailImageUploadRequest();
    thumbnailRequest.setImagePath("/thumbnail/image/path");
    request.setThumbNailImageUploadRequest(thumbnailRequest);

    ImageResultDetail mediumResponse = new ImageResultDetail();
    mediumResponse.setSuccess(true);
    mediumResponse.setTempFileLocation("/temp/medium_processed.jpg");
    mediumResponse.setImagePathLocation("/temp/medium_processed.jpg");

    ImageResultDetail thumbnailResponse = new ImageResultDetail();
    thumbnailResponse.setSuccess(true);
    thumbnailResponse.setTempFileLocation("/temp/thumbnail_processed.jpg");
    thumbnailResponse.setImagePathLocation("/temp/thumbnail_processed.jpg");

    Mockito.when(graphicsProcessorService.scale(Mockito.anyString(), Mockito.anyString(),
        Mockito.any(CustomGraphicsSettings.class), Mockito.anyString(), Mockito.anyBoolean(),
        Mockito.anyBoolean())).thenReturn(mediumResponse).thenReturn(thumbnailResponse);

    ReflectionTestUtils.setField(wrapper, "executorService", MoreExecutors.newDirectExecutorService());

    wrapper.scaleActiveProductNewImages(STORE_ID, CLIENT_ID, request, StringUtils.EMPTY);
    Mockito.verify(fileStorageService, Mockito.times(2))
      .uploadToGcs(Mockito.any(ImageResultDetail.class), Mockito.eq(CLIENT_ID));
    Mockito.verify(imageConfig).getMediumDpi();
    Mockito.verify(imageConfig).getMediumHeight();
    Mockito.verify(imageConfig).getMediumQuality();
    Mockito.verify(imageConfig).getMediumWidth();
    Mockito.verify(imageConfig).getThumbnailHeight();
    Mockito.verify(imageConfig).getThumbnailDpi();
    Mockito.verify(imageConfig).getThumbnailQuality();
    Mockito.verify(imageConfig).getThumbnailWidth();
  }

  @Test
  void scaleActiveProductNewImages_validRequest_partialImageTypes_success() throws Exception {
    ReflectionTestUtils.setField(wrapper, "tempDirForActiveProductImageConversion", "/tmp");
    ReflectionTestUtils.setField(wrapper, "webpConversionEnabled", false);
    byte[] imageBytes = "test image bytes".getBytes();
    XgpImageScaleRequest request = new XgpImageScaleRequest();
    request.setImageBytes(jpegHeader);

    FullImageUploadRequest fullRequest = new FullImageUploadRequest();
    fullRequest.setImagePath("/full/image/path");
    request.setFullImageUploadRequest(fullRequest);

    ThumbNailImageUploadRequest thumbnailRequest = new ThumbNailImageUploadRequest();
    thumbnailRequest.setImagePath("/thumbnail/image/path");
    request.setThumbNailImageUploadRequest(thumbnailRequest);

    ImageResultDetail fullResponse = new ImageResultDetail();
    fullResponse.setSuccess(true);
    fullResponse.setTempFileLocation("/temp/full_processed.jpg");
    fullResponse.setImagePathLocation("/temp/full_processed.jpg");

    ImageResultDetail thumbnailResponse = new ImageResultDetail();
    thumbnailResponse.setSuccess(true);
    thumbnailResponse.setTempFileLocation("/temp/thumbnail_processed.jpg");
    thumbnailResponse.setImagePathLocation("/temp/thumbnail_processed.jpg");

    Mockito.when(graphicsProcessorService.scale(Mockito.anyString(), Mockito.anyString(),
      Mockito.any(CustomGraphicsSettings.class), Mockito.anyString(), Mockito.anyBoolean(),
      Mockito.anyBoolean())).thenReturn(fullResponse).thenReturn(thumbnailResponse);

    ReflectionTestUtils.setField(wrapper, "executorService", MoreExecutors.newDirectExecutorService());

    wrapper.scaleActiveProductNewImages(STORE_ID, CLIENT_ID, request, StringUtils.EMPTY);
    Mockito.verify(fileStorageService, Mockito.times(2))
      .uploadToGcs(Mockito.any(ImageResultDetail.class), Mockito.eq(CLIENT_ID));
    Mockito.verify(imageConfig).getFullWidth();
    Mockito.verify(imageConfig).getFullDpi();
    Mockito.verify(imageConfig).getFullQuality();
    Mockito.verify(imageConfig).getFullHeight();
    Mockito.verify(imageConfig).getThumbnailHeight();
    Mockito.verify(imageConfig).getThumbnailDpi();
    Mockito.verify(imageConfig).getThumbnailQuality();
    Mockito.verify(imageConfig).getThumbnailWidth();
  }

  @Test
  void scaleActiveProductNewImages_webpConversionEnabled_success() throws Exception {
    ReflectionTestUtils.setField(wrapper, "tempDirForActiveProductImageConversion", "/tmp");
    ReflectionTestUtils.setField(wrapper, "webpConversionEnabled", true);
    byte[] imageBytes = "test image bytes".getBytes();
    XgpImageScaleRequest request = new XgpImageScaleRequest();
    request.setImageBytes(jpegHeader);

    FullImageUploadRequest fullRequest = new FullImageUploadRequest();
    fullRequest.setImagePath("/full/image/path");
    request.setFullImageUploadRequest(fullRequest);

    ImageResultDetail fullResponse = new ImageResultDetail();
    fullResponse.setSuccess(true);
    fullResponse.setTempFileLocation("/temp/full_processed.jpg");
    fullResponse.setImagePathLocation("/temp/full_processed.jpg");

    Mockito.when(graphicsProcessorService.scale(Mockito.anyString(), Mockito.anyString(),
      Mockito.any(CustomGraphicsSettings.class), Mockito.anyString(), Mockito.anyBoolean(),
      Mockito.anyBoolean())).thenReturn(fullResponse);

    ImageResultDetail webpConversionResult = new ImageResultDetail();
    webpConversionResult.setSuccess(true);
    webpConversionResult.setImagePathLocation("/tmp/clientId/webp_converted.webp");
    Mockito.when(graphicsProcessorService.convert(Mockito.eq(TargetType.WEBP), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.any(CustomGraphicsSettings.class),
        Mockito.anyString(), Mockito.anyString(), Mockito.anyString()))
      .thenReturn(webpConversionResult);

    IdentifyImageResult imageInfo = new IdentifyImageResult();
    String resultString = "JPEG,80,1920,1080";
    imageInfo.consumeOutput(new java.io.ByteArrayInputStream(resultString.getBytes()));
    Mockito.when(graphicsProcessorService.getGraphicsProperty(Mockito.anyString()))
      .thenReturn(imageInfo);

    ReflectionTestUtils.setField(wrapper, "executorService", MoreExecutors.newDirectExecutorService());

    wrapper.scaleActiveProductNewImages(STORE_ID, CLIENT_ID, request, StringUtils.EMPTY);
    Mockito.verify(fileStorageService, Mockito.times(1))
      .uploadToGcs(Mockito.any(ImageResultDetail.class), Mockito.eq(CLIENT_ID));
    Mockito.verify(imageConfig).getFullWidth();
    Mockito.verify(imageConfig).getFullDpi();
    Mockito.verify(imageConfig).getFullQuality();
    Mockito.verify(imageConfig).getFullHeight();
    Mockito.verify(graphicsProcessorService)
      .convert(Mockito.eq(TargetType.WEBP), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.any(CustomGraphicsSettings.class), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString());
    Mockito.verify(imageConfig).getWebpDpi();
    Mockito.verify(imageConfig).getWebpQuality();
  }

  @Test
  void scaleActiveProductNewImages_webpConversionEnabled_alreadyWebP_shouldSkipConversion()
    throws Exception {
    ReflectionTestUtils.setField(wrapper, "tempDirForActiveProductImageConversion", "/tmp");
    ReflectionTestUtils.setField(wrapper, "webpConversionEnabled", true);
    byte[] imageBytes = "test image bytes".getBytes();
    XgpImageScaleRequest request = new XgpImageScaleRequest();
    request.setImageBytes(jpegHeader);

    FullImageUploadRequest fullRequest = new FullImageUploadRequest();
    fullRequest.setImagePath("/full/image/path");
    request.setFullImageUploadRequest(fullRequest);

    ImageResultDetail fullResponse = new ImageResultDetail();
    fullResponse.setSuccess(true);
    fullResponse.setTempFileLocation("/temp/full_processed.jpg");
    fullResponse.setImagePathLocation("/temp/full_processed.jpg");

    Mockito.when(graphicsProcessorService.scale(Mockito.anyString(), Mockito.anyString(),
      Mockito.any(CustomGraphicsSettings.class), Mockito.anyString(), Mockito.anyBoolean(),
      Mockito.anyBoolean())).thenReturn(fullResponse);

    IdentifyImageResult imageInfo = new IdentifyImageResult();
    String resultString = "WEBP,75,1920,1080";
    imageInfo.consumeOutput(new java.io.ByteArrayInputStream(resultString.getBytes()));
    Mockito.when(graphicsProcessorService.getGraphicsProperty(Mockito.anyString()))
      .thenReturn(imageInfo);

    ReflectionTestUtils.setField(wrapper, "executorService", MoreExecutors.newDirectExecutorService());

    wrapper.scaleActiveProductNewImages(STORE_ID, CLIENT_ID, request, StringUtils.EMPTY);
    Mockito.verify(fileStorageService, Mockito.times(1))
      .uploadToGcs(Mockito.any(ImageResultDetail.class), Mockito.eq(CLIENT_ID));
    Mockito.verify(imageConfig).getFullWidth();
    Mockito.verify(imageConfig).getFullDpi();
    Mockito.verify(imageConfig).getFullQuality();
    Mockito.verify(imageConfig).getFullHeight();
    Mockito.verify(graphicsProcessorService, Mockito.never())
      .convert(Mockito.eq(TargetType.WEBP), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.any(CustomGraphicsSettings.class), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString());
  }

  @Test
  void scaleActiveProductNewImages_webpConversionEnabled_conversionFailure_shouldThrowException()
    throws Exception {
    ReflectionTestUtils.setField(wrapper, "tempDirForActiveProductImageConversion", "/tmp");
    ReflectionTestUtils.setField(wrapper, "webpConversionEnabled", true);
    byte[] imageBytes = "test image bytes".getBytes();
    XgpImageScaleRequest request = new XgpImageScaleRequest();
    request.setImageBytes(jpegHeader);

    FullImageUploadRequest fullRequest = new FullImageUploadRequest();
    fullRequest.setImagePath("/full/image/path");
    request.setFullImageUploadRequest(fullRequest);

    IdentifyImageResult imageInfo = new IdentifyImageResult();
    String resultString = "JPEG,80,1920,1080";
    imageInfo.consumeOutput(new java.io.ByteArrayInputStream(resultString.getBytes()));
    Mockito.when(graphicsProcessorService.getGraphicsProperty(Mockito.anyString()))
      .thenReturn(imageInfo);

    ImageResultDetail webpConversionResult = new ImageResultDetail();
    webpConversionResult.setSuccess(false);
    Mockito.when(graphicsProcessorService.convert(Mockito.eq(TargetType.WEBP), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.any(CustomGraphicsSettings.class),
        Mockito.anyString(), Mockito.anyString(), Mockito.anyString()))
      .thenReturn(webpConversionResult);

    ApplicationException exception = Assertions.assertThrows(ApplicationException.class, () -> {
      wrapper.scaleActiveProductNewImages(STORE_ID, CLIENT_ID, request, StringUtils.EMPTY);
    });

    Assertions.assertNotNull(exception);
    Mockito.verify(imageConfig).getWebpDpi();
    Mockito.verify(imageConfig).getWebpQuality();
  }

  @Test
  void scaleActiveProductNewImages_webpConversionEnabled_conversionException_shouldReturnOriginalFile()
    throws Exception {
    ReflectionTestUtils.setField(wrapper, "tempDirForActiveProductImageConversion", "/tmp");
    ReflectionTestUtils.setField(wrapper, "webpConversionEnabled", true);
    byte[] imageBytes = "test image bytes".getBytes();
    XgpImageScaleRequest request = new XgpImageScaleRequest();
    request.setImageBytes(jpegHeader);

    FullImageUploadRequest fullRequest = new FullImageUploadRequest();
    fullRequest.setImagePath("/full/image/path");
    request.setFullImageUploadRequest(fullRequest);

    ImageResultDetail fullResponse = new ImageResultDetail();
    fullResponse.setSuccess(true);
    fullResponse.setTempFileLocation("/temp/full_processed.jpg");

    Mockito.when(graphicsProcessorService.scale(Mockito.anyString(), Mockito.anyString(),
      Mockito.any(CustomGraphicsSettings.class), Mockito.anyString(), Mockito.anyBoolean(),
      Mockito.anyBoolean())).thenReturn(fullResponse);

    IdentifyImageResult imageInfo = new IdentifyImageResult();
    String resultString = "JPEG,80,1920,1080";
    imageInfo.consumeOutput(new java.io.ByteArrayInputStream(resultString.getBytes()));
    Mockito.when(graphicsProcessorService.getGraphicsProperty(Mockito.anyString()))
      .thenReturn(imageInfo);

    Mockito.when(graphicsProcessorService.convert(Mockito.eq(TargetType.WEBP), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.any(CustomGraphicsSettings.class),
        Mockito.anyString(), Mockito.anyString(), Mockito.anyString()))
      .thenThrow(new RuntimeException("WebP conversion failed"));
    try {
      wrapper.scaleActiveProductNewImages(STORE_ID, CLIENT_ID, request, StringUtils.EMPTY);
    } catch (Exception ignored) {
    } finally {
      Mockito.verify(imageConfig).getWebpDpi();
      Mockito.verify(imageConfig).getWebpQuality();
    }
  }

  @Test
  void scaleActiveProductNewImages_webpConversionEnabled_notImage_shouldReturnOriginalFile()
    throws Exception {
    ReflectionTestUtils.setField(wrapper, "tempDirForActiveProductImageConversion", "/tmp");
    ReflectionTestUtils.setField(wrapper, "webpConversionEnabled", true);
    byte[] imageBytes = "test image bytes".getBytes();
    XgpImageScaleRequest request = new XgpImageScaleRequest();
    request.setImageBytes(jpegHeader);

    FullImageUploadRequest fullRequest = new FullImageUploadRequest();
    fullRequest.setImagePath("/full/image/path");
    request.setFullImageUploadRequest(fullRequest);

    ImageResultDetail fullResponse = new ImageResultDetail();
    fullResponse.setSuccess(true);
    fullResponse.setTempFileLocation("/temp/full_processed.jpg");

    Mockito.when(graphicsProcessorService.scale(Mockito.anyString(), Mockito.anyString(),
      Mockito.any(CustomGraphicsSettings.class), Mockito.anyString(), Mockito.anyBoolean(),
      Mockito.anyBoolean())).thenReturn(fullResponse);

    IdentifyImageResult imageInfo = new IdentifyImageResult();
    String resultString = "INVALID,0,0,0";
    imageInfo.consumeOutput(new java.io.ByteArrayInputStream(resultString.getBytes()));
    Mockito.when(graphicsProcessorService.getGraphicsProperty(Mockito.anyString()))
      .thenReturn(imageInfo);

    try {
      wrapper.scaleActiveProductNewImages(STORE_ID, CLIENT_ID, request, StringUtils.EMPTY);
    } catch (Exception e) {
    } finally {
      Mockito.verify(imageConfig).getWebpDpi();
      Mockito.verify(imageConfig).getWebpQuality();
    }
  }

  @Test
  void scaleActiveProductNewImages_webpConversionDisabled_shouldNotConvert() throws Exception {
    ReflectionTestUtils.setField(wrapper, "tempDirForActiveProductImageConversion", "/tmp");
    ReflectionTestUtils.setField(wrapper, "webpConversionEnabled", false);
    byte[] imageBytes = "test image bytes".getBytes();
    XgpImageScaleRequest request = new XgpImageScaleRequest();
    request.setImageBytes(jpegHeader);

    FullImageUploadRequest fullRequest = new FullImageUploadRequest();
    fullRequest.setImagePath("/full/image/path");
    request.setFullImageUploadRequest(fullRequest);

    ImageResultDetail fullResponse = new ImageResultDetail();
    fullResponse.setSuccess(true);
    fullResponse.setTempFileLocation("/temp/full_processed.jpg");
    fullResponse.setImagePathLocation("/temp/full_processed.jpg");

    Mockito.when(graphicsProcessorService.scale(Mockito.anyString(), Mockito.anyString(),
      Mockito.any(CustomGraphicsSettings.class), Mockito.anyString(), Mockito.anyBoolean(),
      Mockito.anyBoolean())).thenReturn(fullResponse);

    ReflectionTestUtils.setField(wrapper, "executorService", MoreExecutors.newDirectExecutorService());

    wrapper.scaleActiveProductNewImages(STORE_ID, CLIENT_ID, request, StringUtils.EMPTY);
    Mockito.verify(fileStorageService, Mockito.times(1))
      .uploadToGcs(Mockito.any(ImageResultDetail.class), Mockito.eq(CLIENT_ID));
    Mockito.verify(imageConfig).getFullWidth();
    Mockito.verify(imageConfig).getFullDpi();
    Mockito.verify(imageConfig).getFullQuality();
    Mockito.verify(imageConfig).getFullHeight();
    Mockito.verify(graphicsProcessorService, Mockito.never())
      .convert(Mockito.eq(TargetType.WEBP), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.any(CustomGraphicsSettings.class), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString());
  }

  /*@Test
  void processImageType_scaleThrowsException_shouldThrowApplicationRuntimeException() throws Exception {
    ReflectionTestUtils.setField(wrapper, "tempDirForActiveProductImageConversion", "/tmp");
    File sourceImageFile = new File("/source/image.jpg");
    String gcsDestinationPath = "/gcs/destination/full.jpg";
    String imageType = "FULL";

    Mockito.when(graphicsProcessorService.scale(Mockito.anyString(), Mockito.anyString(),
      Mockito.any(CustomGraphicsSettings.class), Mockito.anyString(), Mockito.anyBoolean(),
      Mockito.anyBoolean())).thenThrow(new RuntimeException("Scale failed"));

    ApplicationRuntimeException exception = Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      wrapper.processImageType(sourceImageFile, gcsDestinationPath, imageType);
    });

    Assertions.assertNotNull(exception);
    Mockito.verify(imageConfig).getFullWidth();
    Mockito.verify(imageConfig).getFullDpi();
    Mockito.verify(imageConfig).getFullQuality();
    Mockito.verify(imageConfig).getFullHeight();
  }*/

  @Test
  void processImageType_scaleReturnsUnsuccessful_shouldReturnNull() throws Exception {
    ReflectionTestUtils.setField(wrapper, "tempDirForActiveProductImageConversion", "/tmp");
    File sourceImageFile = new File("/source/image.jpg");
    String gcsDestinationPath = "/gcs/destination/full.jpg";
    String imageType = "FULL";

    ImageResultDetail scaleResult = new ImageResultDetail();
    scaleResult.setSuccess(false);
    Mockito.when(graphicsProcessorService.scale(Mockito.anyString(), Mockito.anyString(),
      Mockito.any(CustomGraphicsSettings.class), Mockito.anyString(), Mockito.anyBoolean(),
      Mockito.anyBoolean())).thenReturn(scaleResult);

    ImageResultDetail result = wrapper.processImageType(sourceImageFile, gcsDestinationPath, imageType);
    Assertions.assertNull(result);
    Mockito.verify(imageConfig).getFullWidth();
    Mockito.verify(imageConfig).getFullDpi();
    Mockito.verify(imageConfig).getFullQuality();
    Mockito.verify(imageConfig).getFullHeight();
  }

  @Test
  void allFail() throws Exception {
    ReflectionTestUtils.setField(wrapper, "tempDirForActiveProductImageConversion", "/tmp");
    ReflectionTestUtils.setField(wrapper, "webpConversionEnabled", false);
    byte[] imageBytes = "test image bytes".getBytes();
    XgpImageScaleRequest request = new XgpImageScaleRequest();
    request.setImageBytes(jpegHeader);

    FullImageUploadRequest fullRequest = new FullImageUploadRequest();
    fullRequest.setImagePath("/full/image/path");
    request.setFullImageUploadRequest(fullRequest);

    MediumImageUploadRequest mediumRequest = new MediumImageUploadRequest();
    mediumRequest.setImagePath("/medium/image/path");
    request.setMediumImageUploadRequest(mediumRequest);

    ThumbNailImageUploadRequest thumbnailRequest = new ThumbNailImageUploadRequest();
    thumbnailRequest.setImagePath("/thumbnail/image/path");
    request.setThumbNailImageUploadRequest(thumbnailRequest);

    ImageResultDetail fullResponse = new ImageResultDetail();
    fullResponse.setSuccess(false);
    fullResponse.setTempFileLocation("/temp/full_processed.jpg");
    fullResponse.setImagePathLocation("/temp/full_processed.jpg");

    ImageResultDetail mediumResponse = new ImageResultDetail();
    mediumResponse.setSuccess(false);
    mediumResponse.setTempFileLocation("/temp/medium_processed.jpg");
    mediumResponse.setImagePathLocation("/temp/medium_processed.jpg");

    ImageResultDetail thumbnailResponse = new ImageResultDetail();
    thumbnailResponse.setSuccess(false);
    thumbnailResponse.setTempFileLocation("/temp/thumbnail_processed.jpg");
    thumbnailResponse.setImagePathLocation("/temp/thumbnail_processed.jpg");

    Mockito.when(graphicsProcessorService.scale(Mockito.anyString(), Mockito.anyString(),
        Mockito.any(CustomGraphicsSettings.class), Mockito.anyString(), Mockito.anyBoolean(),
        Mockito.anyBoolean())).thenReturn(fullResponse).thenReturn(mediumResponse)
      .thenReturn(thumbnailResponse);

    ReflectionTestUtils.setField(wrapper, "executorService", MoreExecutors.newDirectExecutorService());

    wrapper.scaleActiveProductNewImages(STORE_ID, CLIENT_ID, request, StringUtils.EMPTY);
    Mockito.verify(fileStorageService, Mockito.never())
      .uploadToGcs(Mockito.any(ImageResultDetail.class), Mockito.eq(CLIENT_ID));
    Mockito.verify(imageConfig).getFullWidth();
    Mockito.verify(imageConfig).getFullDpi();
    Mockito.verify(imageConfig).getFullQuality();
    Mockito.verify(imageConfig).getFullHeight();
    Mockito.verify(imageConfig).getMediumDpi();
    Mockito.verify(imageConfig).getMediumHeight();
    Mockito.verify(imageConfig).getMediumQuality();
    Mockito.verify(imageConfig).getMediumWidth();
    Mockito.verify(imageConfig).getThumbnailHeight();
    Mockito.verify(imageConfig).getThumbnailDpi();
    Mockito.verify(imageConfig).getThumbnailQuality();
    Mockito.verify(imageConfig).getThumbnailWidth();
  }

  @Test
  void shouldLogError_whenExceptionThrownDuringCleanup() {
    File mockFile = Mockito.mock(File.class);

    Mockito.when(mockFile.exists()).thenThrow(new RuntimeException("Simulated failure"));
    Mockito.when(mockFile.getAbsolutePath()).thenReturn("/tmp/fake.txt");

    wrapper.cleanupTempFile(mockFile);

  }

  @Test
  public void testJpegMimeType() {
    String result = wrapper.getImageExtensionFromBytes(jpegHeader);
    assertEquals(".jpg", result);
  }

  @Test
  void testDetectMimeTypeException() {
    String result = wrapper.getImageExtensionFromBytes(null);
    assertEquals(".jpg", result);
  }

  @Test
  public void testRandomMimeType() {
    String result = wrapper.getImageExtensionFromBytes(testImageBytes);
    assertEquals(".jpg", result);
  }

  @Test
  public void testPngMimeType() {
    String result = wrapper.getImageExtensionFromBytes(pngHeader);
    assertEquals(".png", result);
  }

  @Test
  public void testWebpMimeType() {
    String result = wrapper.getImageExtensionFromBytes(webpHeader);
    assertEquals(".webp", result);
  }

  @Test
  void test() throws Exception {
    ReflectionTestUtils.setField(wrapper, "tempDirForActiveProductImageConversion", "/tmp");
    ReflectionTestUtils.setField(wrapper, "webpConversionEnabled", true);
    byte[] imageBytes = "test image bytes".getBytes();
    XgpImageScaleRequest request = new XgpImageScaleRequest();
    request.setImageBytes(jpegHeader);
    request.setActive(true);

    FullImageUploadRequest fullRequest = new FullImageUploadRequest();
    fullRequest.setImagePath("/full/image/path");
    request.setFullImageUploadRequest(fullRequest);

    ImageResultDetail fullResponse = new ImageResultDetail();
    fullResponse.setSuccess(true);
    fullResponse.setTempFileLocation("/temp/full_processed.jpg");
    fullResponse.setImagePathLocation("/temp/full_processed.jpg");

    Mockito.when(graphicsProcessorService.scale(Mockito.anyString(), Mockito.anyString(),
      Mockito.any(CustomGraphicsSettings.class), Mockito.anyString(), Mockito.anyBoolean(),
      Mockito.anyBoolean())).thenReturn(fullResponse);

    ImageResultDetail webpConversionResult = new ImageResultDetail();
    webpConversionResult.setSuccess(true);
    webpConversionResult.setImagePathLocation("/tmp/clientId/webp_converted.webp");
    Mockito.when(graphicsProcessorService.convert(Mockito.eq(TargetType.WEBP), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.any(CustomGraphicsSettings.class),
        Mockito.anyString(), Mockito.anyString(), Mockito.anyString()))
      .thenReturn(webpConversionResult);

    IdentifyImageResult imageInfo = new IdentifyImageResult();
    String resultString = "JPEG,80,1920,1080";
    imageInfo.consumeOutput(new java.io.ByteArrayInputStream(resultString.getBytes()));
    Mockito.when(graphicsProcessorService.getGraphicsProperty(Mockito.anyString()))
      .thenReturn(imageInfo);

    ReflectionTestUtils.setField(wrapper, "executorService", MoreExecutors.newDirectExecutorService());

    wrapper.scaleActiveProductNewImages(STORE_ID, CLIENT_ID, request, StringUtils.EMPTY);
    Mockito.verify(fileStorageService, Mockito.times(1))
      .uploadToGcs(Mockito.any(ImageResultDetail.class), Mockito.eq(CLIENT_ID));

    Mockito.verify(graphicsProcessorService)
      .convert(Mockito.eq(TargetType.WEBP), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.any(CustomGraphicsSettings.class), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString());
    Mockito.verify(imageConfig).getWebpDpi();
    Mockito.verify(imageConfig).getWebpQuality();
  }

  private Future<ImageResponse> getImageResponseFuture() {
    return new Future<ImageResponse>() {
      @Override
      public boolean cancel(boolean mayInterruptIfRunning) {
        return false;
      }

      @Override
      public boolean isCancelled() {
        return false;
      }

      @Override
      public boolean isDone() {
        return false;
      }

      @Override
      public ImageResponse get() throws InterruptedException, ExecutionException {
        return new ImageResponse();
      }

      @Override
      public ImageResponse get(long timeout, TimeUnit unit)
          throws InterruptedException, ExecutionException, TimeoutException {
        return null;
      }
    };
  }

  private Future<ImageResponse> getImageResponseFutureUpload() {
    return new Future<ImageResponse>() {
      @Override
      public boolean cancel(boolean mayInterruptIfRunning) {
        return false;
      }

      @Override
      public boolean isCancelled() {
        return false;
      }

      @Override
      public boolean isDone() {
        return false;
      }

      @Override
      public ImageResponse get() throws InterruptedException, ExecutionException {
        ImageResponse imageResponse = new ImageResponse();
        imageResponse.setUploadRequired(true);
        return imageResponse;
      }

      @Override
      public ImageResponse get(long timeout, TimeUnit unit)
          throws InterruptedException, ExecutionException, TimeoutException {
        return null;
      }
    };
  }

  @Test
  void scaleActiveProductNewImages_newFlowEnabled_shouldUseScaleToWebP() throws Exception {
    ReflectionTestUtils.setField(wrapper, "tempDirForActiveProductImageConversion", "/tmp");
    ReflectionTestUtils.setField(wrapper, "webpConversionEnabled", true);
    ReflectionTestUtils.setField(wrapper, "newWebpFlowEnabled", true);
    XgpImageScaleRequest request = new XgpImageScaleRequest();
    request.setImageBytes(jpegHeader);

    FullImageUploadRequest fullRequest = new FullImageUploadRequest();
    fullRequest.setImagePath("/full/image/path");
    request.setFullImageUploadRequest(fullRequest);

    MediumImageUploadRequest mediumRequest = new MediumImageUploadRequest();
    mediumRequest.setImagePath("/medium/image/path");
    request.setMediumImageUploadRequest(mediumRequest);

    ThumbNailImageUploadRequest thumbnailRequest = new ThumbNailImageUploadRequest();
    thumbnailRequest.setImagePath("/thumbnail/image/path");
    request.setThumbNailImageUploadRequest(thumbnailRequest);

    ImageResultDetail fullResponse = new ImageResultDetail();
    fullResponse.setSuccess(true);
    fullResponse.setTempFileLocation("/temp/full_processed.webp");
    fullResponse.setImagePathLocation("/temp/full_processed.webp");

    ImageResultDetail mediumResponse = new ImageResultDetail();
    mediumResponse.setSuccess(true);
    mediumResponse.setTempFileLocation("/temp/medium_processed.webp");
    mediumResponse.setImagePathLocation("/temp/medium_processed.webp");

    ImageResultDetail thumbnailResponse = new ImageResultDetail();
    thumbnailResponse.setSuccess(true);
    thumbnailResponse.setTempFileLocation("/temp/thumbnail_processed.webp");
    thumbnailResponse.setImagePathLocation("/temp/thumbnail_processed.webp");

    Mockito.when(graphicsProcessorService.scaleToWebP(Mockito.anyString(), Mockito.anyString(),
        Mockito.any(CustomGraphicsSettings.class), Mockito.anyString()))
      .thenReturn(fullResponse).thenReturn(mediumResponse).thenReturn(thumbnailResponse);

    IdentifyImageResult imageInfo = new IdentifyImageResult();
    String resultString = "JPEG,80,1920,1080";
    imageInfo.consumeOutput(new java.io.ByteArrayInputStream(resultString.getBytes()));
    Mockito.when(graphicsProcessorService.getGraphicsProperty(Mockito.anyString()))
      .thenReturn(imageInfo);

    ReflectionTestUtils.setField(wrapper, "executorService", MoreExecutors.newDirectExecutorService());

    wrapper.scaleActiveProductNewImages(STORE_ID, CLIENT_ID, request, StringUtils.EMPTY);

    Mockito.verify(graphicsProcessorService, Mockito.times(3))
      .scaleToWebP(Mockito.anyString(), Mockito.anyString(),
        Mockito.any(CustomGraphicsSettings.class), Mockito.anyString());
    Mockito.verify(graphicsProcessorService, Mockito.never())
      .convert(Mockito.eq(TargetType.WEBP), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.any(CustomGraphicsSettings.class), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString());
    Mockito.verify(fileStorageService, Mockito.times(3))
      .uploadToGcs(Mockito.any(ImageResultDetail.class), Mockito.eq(CLIENT_ID));
    Mockito.verify(imageConfig).getFullWidth();
    Mockito.verify(imageConfig).getFullDpi();
    Mockito.verify(imageConfig).getFullQuality();
    Mockito.verify(imageConfig).getFullHeight();
    Mockito.verify(imageConfig).getMediumWidth();
    Mockito.verify(imageConfig).getMediumDpi();
    Mockito.verify(imageConfig).getMediumQuality();
    Mockito.verify(imageConfig).getMediumHeight();
    Mockito.verify(imageConfig).getThumbnailWidth();
    Mockito.verify(imageConfig).getThumbnailDpi();
    Mockito.verify(imageConfig).getThumbnailQuality();
    Mockito.verify(imageConfig).getThumbnailHeight();
    Assertions.assertEquals(3, Mockito.mockingDetails(graphicsProcessorService).getInvocations().stream()
        .filter(invocation -> invocation.getMethod().getName().equals("scaleToWebP")).count());
  }

  @Test
  void scaleActiveProductNewImages_newFlowEnabled_shouldCleanupImmediately() throws Exception {
    ReflectionTestUtils.setField(wrapper, "tempDirForActiveProductImageConversion", "/tmp");
    ReflectionTestUtils.setField(wrapper, "webpConversionEnabled", true);
    ReflectionTestUtils.setField(wrapper, "newWebpFlowEnabled", true);
    XgpImageScaleRequest request = new XgpImageScaleRequest();
    request.setImageBytes(jpegHeader);

    FullImageUploadRequest fullRequest = new FullImageUploadRequest();
    fullRequest.setImagePath("/full/image/path");
    request.setFullImageUploadRequest(fullRequest);

    ImageResultDetail fullResponse = new ImageResultDetail();
    fullResponse.setSuccess(true);
    fullResponse.setTempFileLocation("/temp/full_processed.webp");
    fullResponse.setImagePathLocation("/temp/full_processed.webp");

    Mockito.when(graphicsProcessorService.scaleToWebP(Mockito.anyString(), Mockito.anyString(),
        Mockito.any(CustomGraphicsSettings.class), Mockito.anyString()))
      .thenReturn(fullResponse);

    IdentifyImageResult imageInfo = new IdentifyImageResult();
    String resultString = "JPEG,80,1920,1080";
    imageInfo.consumeOutput(new java.io.ByteArrayInputStream(resultString.getBytes()));
    Mockito.when(graphicsProcessorService.getGraphicsProperty(Mockito.anyString()))
      .thenReturn(imageInfo);

    ReflectionTestUtils.setField(wrapper, "executorService", MoreExecutors.newDirectExecutorService());

    wrapper.scaleActiveProductNewImages(STORE_ID, CLIENT_ID, request, StringUtils.EMPTY);

    ArgumentCaptor<ImageResultDetail> uploadCaptor = ArgumentCaptor.forClass(ImageResultDetail.class);
    Mockito.verify(fileStorageService).uploadToGcs(uploadCaptor.capture(), Mockito.eq(CLIENT_ID));
    Assertions.assertNull(uploadCaptor.getValue().getImagePathLocation());
    Mockito.verify(imageConfig).getFullWidth();
    Mockito.verify(imageConfig).getFullDpi();
    Mockito.verify(imageConfig).getFullQuality();
    Mockito.verify(imageConfig).getFullHeight();
  }

  @Test
  void scaleActiveProductNewImages_newFlowEnabled_alreadyWebP_shouldNotConvert() throws Exception {
    ReflectionTestUtils.setField(wrapper, "tempDirForActiveProductImageConversion", "/tmp");
    ReflectionTestUtils.setField(wrapper, "webpConversionEnabled", true);
    ReflectionTestUtils.setField(wrapper, "newWebpFlowEnabled", true);
    XgpImageScaleRequest request = new XgpImageScaleRequest();
    request.setImageBytes(webpHeader);

    FullImageUploadRequest fullRequest = new FullImageUploadRequest();
    fullRequest.setImagePath("/full/image/path");
    request.setFullImageUploadRequest(fullRequest);

    ImageResultDetail fullResponse = new ImageResultDetail();
    fullResponse.setSuccess(true);
    fullResponse.setTempFileLocation("/temp/full_processed.webp");
    fullResponse.setImagePathLocation("/temp/full_processed.webp");

    Mockito.when(graphicsProcessorService.scaleToWebP(Mockito.anyString(), Mockito.anyString(),
        Mockito.any(CustomGraphicsSettings.class), Mockito.anyString()))
      .thenReturn(fullResponse);

    IdentifyImageResult imageInfo = new IdentifyImageResult();
    String resultString = "WEBP,75,1920,1080";
    imageInfo.consumeOutput(new java.io.ByteArrayInputStream(resultString.getBytes()));
    Mockito.when(graphicsProcessorService.getGraphicsProperty(Mockito.anyString()))
      .thenReturn(imageInfo);

    ReflectionTestUtils.setField(wrapper, "executorService", MoreExecutors.newDirectExecutorService());

    wrapper.scaleActiveProductNewImages(STORE_ID, CLIENT_ID, request, StringUtils.EMPTY);

    Mockito.verify(graphicsProcessorService, Mockito.times(1))
      .scaleToWebP(Mockito.anyString(), Mockito.anyString(),
        Mockito.any(CustomGraphicsSettings.class), Mockito.anyString());
    Mockito.verify(graphicsProcessorService, Mockito.never())
      .convert(Mockito.eq(TargetType.WEBP), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.any(CustomGraphicsSettings.class), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString());
    Mockito.verify(imageConfig).getFullWidth();
    Mockito.verify(imageConfig).getFullDpi();
    Mockito.verify(imageConfig).getFullQuality();
    Mockito.verify(imageConfig).getFullHeight();
    Assertions.assertTrue(fullResponse.isSuccess());
  }

  @Test
  void scaleActiveProductNewImages_newFlowEnabled_isActive_shouldReturnActiveImage() throws Exception {
    ReflectionTestUtils.setField(wrapper, "tempDirForActiveProductImageConversion", "/tmp");
    ReflectionTestUtils.setField(wrapper, "webpConversionEnabled", true);
    ReflectionTestUtils.setField(wrapper, "newWebpFlowEnabled", true);
    XgpImageScaleRequest request = new XgpImageScaleRequest();
    request.setImageBytes(jpegHeader);
    request.setActive(true);

    FullImageUploadRequest fullRequest = new FullImageUploadRequest();
    fullRequest.setImagePath("/full/image/path");
    request.setFullImageUploadRequest(fullRequest);

    ReflectionTestUtils.setField(wrapper, "executorService", MoreExecutors.newDirectExecutorService());

    wrapper.scaleActiveProductNewImages(STORE_ID, CLIENT_ID, request, StringUtils.EMPTY);

    Mockito.verify(graphicsProcessorService, Mockito.never())
      .scaleToWebP(Mockito.anyString(), Mockito.anyString(),
        Mockito.any(CustomGraphicsSettings.class), Mockito.anyString());
    Assertions.assertTrue(request.isActive());
  }

  @Test
  void scaleActiveProductNewImages_newFlowEnabled_processImageTypeThrowsException_shouldHandleGracefully() throws Exception {
    ReflectionTestUtils.setField(wrapper, "tempDirForActiveProductImageConversion", "/tmp");
    ReflectionTestUtils.setField(wrapper, "webpConversionEnabled", true);
    ReflectionTestUtils.setField(wrapper, "newWebpFlowEnabled", true);
    XgpImageScaleRequest request = new XgpImageScaleRequest();
    request.setImageBytes(jpegHeader);

    FullImageUploadRequest fullRequest = new FullImageUploadRequest();
    fullRequest.setImagePath("/full/image/path");
    request.setFullImageUploadRequest(fullRequest);

    Mockito.when(graphicsProcessorService.scaleToWebP(Mockito.anyString(), Mockito.anyString(),
        Mockito.any(CustomGraphicsSettings.class), Mockito.anyString()))
      .thenThrow(new RuntimeException("Processing failed"));

    IdentifyImageResult imageInfo = new IdentifyImageResult();
    String resultString = "JPEG,80,1920,1080";
    imageInfo.consumeOutput(new java.io.ByteArrayInputStream(resultString.getBytes()));
    Mockito.when(graphicsProcessorService.getGraphicsProperty(Mockito.anyString()))
      .thenReturn(imageInfo);

    ReflectionTestUtils.setField(wrapper, "executorService", MoreExecutors.newDirectExecutorService());

    wrapper.scaleActiveProductNewImages(STORE_ID, CLIENT_ID, request, StringUtils.EMPTY);

    Mockito.verify(graphicsProcessorService, Mockito.times(1))
      .scaleToWebP(Mockito.anyString(), Mockito.anyString(),
        Mockito.any(CustomGraphicsSettings.class), Mockito.anyString());
    Mockito.verify(fileStorageService, Mockito.never())
      .uploadToGcs(Mockito.any(ImageResultDetail.class), Mockito.eq(CLIENT_ID));
    Mockito.verify(imageConfig).getFullWidth();
    Mockito.verify(imageConfig).getFullDpi();
    Mockito.verify(imageConfig).getFullQuality();
    Mockito.verify(imageConfig).getFullHeight();
    Assertions.assertNotNull(request);
  }

  @Test
  void scaleActiveProductNewImages_newFlowEnabled_processImageTypeReturnsNull_shouldNotUpload() throws Exception {
    ReflectionTestUtils.setField(wrapper, "tempDirForActiveProductImageConversion", "/tmp");
    ReflectionTestUtils.setField(wrapper, "webpConversionEnabled", true);
    ReflectionTestUtils.setField(wrapper, "newWebpFlowEnabled", true);
    XgpImageScaleRequest request = new XgpImageScaleRequest();
    request.setImageBytes(jpegHeader);

    FullImageUploadRequest fullRequest = new FullImageUploadRequest();
    fullRequest.setImagePath("/full/image/path");
    request.setFullImageUploadRequest(fullRequest);

    Mockito.when(graphicsProcessorService.scaleToWebP(Mockito.anyString(), Mockito.anyString(),
        Mockito.any(CustomGraphicsSettings.class), Mockito.anyString()))
      .thenReturn(null);

    IdentifyImageResult imageInfo = new IdentifyImageResult();
    String resultString = "JPEG,80,1920,1080";
    imageInfo.consumeOutput(new java.io.ByteArrayInputStream(resultString.getBytes()));
    Mockito.when(graphicsProcessorService.getGraphicsProperty(Mockito.anyString()))
      .thenReturn(imageInfo);

    ReflectionTestUtils.setField(wrapper, "executorService", MoreExecutors.newDirectExecutorService());

    wrapper.scaleActiveProductNewImages(STORE_ID, CLIENT_ID, request, StringUtils.EMPTY);

    Mockito.verify(graphicsProcessorService, Mockito.times(1))
      .scaleToWebP(Mockito.anyString(), Mockito.anyString(),
        Mockito.any(CustomGraphicsSettings.class), Mockito.anyString());
    Mockito.verify(fileStorageService, Mockito.never())
      .uploadToGcs(Mockito.any(ImageResultDetail.class), Mockito.eq(CLIENT_ID));
    Mockito.verify(imageConfig).getFullWidth();
    Mockito.verify(imageConfig).getFullDpi();
    Mockito.verify(imageConfig).getFullQuality();
    Mockito.verify(imageConfig).getFullHeight();
    Assertions.assertNotNull(request);
  }

  @Test
  void scaleActiveProductNewImages_newFlowEnabled_processImageTypeReturnsUnsuccessful_shouldNotUpload() throws Exception {
    ReflectionTestUtils.setField(wrapper, "tempDirForActiveProductImageConversion", "/tmp");
    ReflectionTestUtils.setField(wrapper, "webpConversionEnabled", true);
    ReflectionTestUtils.setField(wrapper, "newWebpFlowEnabled", true);
    XgpImageScaleRequest request = new XgpImageScaleRequest();
    request.setImageBytes(jpegHeader);

    FullImageUploadRequest fullRequest = new FullImageUploadRequest();
    fullRequest.setImagePath("/full/image/path");
    request.setFullImageUploadRequest(fullRequest);

    ImageResultDetail unsuccessfulResponse = new ImageResultDetail();
    unsuccessfulResponse.setSuccess(false);
    unsuccessfulResponse.setErrorMessage("Processing failed");

    Mockito.when(graphicsProcessorService.scaleToWebP(Mockito.anyString(), Mockito.anyString(),
        Mockito.any(CustomGraphicsSettings.class), Mockito.anyString()))
      .thenReturn(unsuccessfulResponse);

    IdentifyImageResult imageInfo = new IdentifyImageResult();
    String resultString = "JPEG,80,1920,1080";
    imageInfo.consumeOutput(new java.io.ByteArrayInputStream(resultString.getBytes()));
    Mockito.when(graphicsProcessorService.getGraphicsProperty(Mockito.anyString()))
      .thenReturn(imageInfo);

    ReflectionTestUtils.setField(wrapper, "executorService", MoreExecutors.newDirectExecutorService());

    wrapper.scaleActiveProductNewImages(STORE_ID, CLIENT_ID, request, StringUtils.EMPTY);

    Mockito.verify(graphicsProcessorService, Mockito.times(1))
      .scaleToWebP(Mockito.anyString(), Mockito.anyString(),
        Mockito.any(CustomGraphicsSettings.class), Mockito.anyString());
    Mockito.verify(fileStorageService, Mockito.never())
      .uploadToGcs(Mockito.any(ImageResultDetail.class), Mockito.eq(CLIENT_ID));
    Mockito.verify(imageConfig).getFullWidth();
    Mockito.verify(imageConfig).getFullDpi();
    Mockito.verify(imageConfig).getFullQuality();
    Mockito.verify(imageConfig).getFullHeight();
    Assertions.assertFalse(unsuccessfulResponse.isSuccess());
  }

  @Test
  void scaleActiveProductNewImages_newFlowEnabled_uploadThrowsException_shouldHandleGracefully() throws Exception {
    ReflectionTestUtils.setField(wrapper, "tempDirForActiveProductImageConversion", "/tmp");
    ReflectionTestUtils.setField(wrapper, "webpConversionEnabled", true);
    ReflectionTestUtils.setField(wrapper, "newWebpFlowEnabled", true);
    XgpImageScaleRequest request = new XgpImageScaleRequest();
    request.setImageBytes(jpegHeader);

    FullImageUploadRequest fullRequest = new FullImageUploadRequest();
    fullRequest.setImagePath("/full/image/path");
    request.setFullImageUploadRequest(fullRequest);

    ImageResultDetail fullResponse = new ImageResultDetail();
    fullResponse.setSuccess(true);
    fullResponse.setTempFileLocation("/temp/full_processed.webp");
    fullResponse.setImagePathLocation("/temp/full_processed.webp");

    Mockito.when(graphicsProcessorService.scaleToWebP(Mockito.anyString(), Mockito.anyString(),
        Mockito.any(CustomGraphicsSettings.class), Mockito.anyString()))
      .thenReturn(fullResponse);

    Mockito.doThrow(new RuntimeException("Upload failed"))
      .when(fileStorageService).uploadToGcs(Mockito.any(ImageResultDetail.class), Mockito.eq(CLIENT_ID));

    IdentifyImageResult imageInfo = new IdentifyImageResult();
    String resultString = "JPEG,80,1920,1080";
    imageInfo.consumeOutput(new java.io.ByteArrayInputStream(resultString.getBytes()));
    Mockito.when(graphicsProcessorService.getGraphicsProperty(Mockito.anyString()))
      .thenReturn(imageInfo);

    ReflectionTestUtils.setField(wrapper, "executorService", MoreExecutors.newDirectExecutorService());

    wrapper.scaleActiveProductNewImages(STORE_ID, CLIENT_ID, request, StringUtils.EMPTY);

    Mockito.verify(fileStorageService, Mockito.times(1))
      .uploadToGcs(Mockito.any(ImageResultDetail.class), Mockito.eq(CLIENT_ID));
    Mockito.verify(imageConfig).getFullWidth();
    Mockito.verify(imageConfig).getFullDpi();
    Mockito.verify(imageConfig).getFullQuality();
    Mockito.verify(imageConfig).getFullHeight();
    Assertions.assertTrue(fullResponse.isSuccess());
  }

  @Test
  void scaleActiveProductNewImages_newFlowEnabled_resultNotNullButNotSuccess_shouldNotUpload() throws Exception {
    ReflectionTestUtils.setField(wrapper, "tempDirForActiveProductImageConversion", "/tmp");
    ReflectionTestUtils.setField(wrapper, "webpConversionEnabled", true);
    ReflectionTestUtils.setField(wrapper, "newWebpFlowEnabled", true);
    XgpImageScaleRequest request = new XgpImageScaleRequest();
    request.setImageBytes(jpegHeader);

    FullImageUploadRequest fullRequest = new FullImageUploadRequest();
    fullRequest.setImagePath("/full/image/path");
    request.setFullImageUploadRequest(fullRequest);

    ImageResultDetail unsuccessfulResult = new ImageResultDetail();
    unsuccessfulResult.setSuccess(false);
    unsuccessfulResult.setErrorMessage("Processing failed");

    Mockito.when(graphicsProcessorService.scaleToWebP(Mockito.anyString(), Mockito.anyString(),
        Mockito.any(CustomGraphicsSettings.class), Mockito.anyString()))
      .thenReturn(unsuccessfulResult);

    IdentifyImageResult imageInfo = new IdentifyImageResult();
    String resultString = "JPEG,80,1920,1080";
    imageInfo.consumeOutput(new java.io.ByteArrayInputStream(resultString.getBytes()));
    Mockito.when(graphicsProcessorService.getGraphicsProperty(Mockito.anyString()))
      .thenReturn(imageInfo);

    ReflectionTestUtils.setField(wrapper, "executorService", MoreExecutors.newDirectExecutorService());

    wrapper.scaleActiveProductNewImages(STORE_ID, CLIENT_ID, request, StringUtils.EMPTY);

    Mockito.verify(fileStorageService, Mockito.never())
      .uploadToGcs(Mockito.any(ImageResultDetail.class), Mockito.eq(CLIENT_ID));
    Mockito.verify(imageConfig).getFullWidth();
    Mockito.verify(imageConfig).getFullDpi();
    Mockito.verify(imageConfig).getFullQuality();
    Mockito.verify(imageConfig).getFullHeight();
    Assertions.assertFalse(unsuccessfulResult.isSuccess());
  }

  @Test
  void scaleActiveProductNewImages_newFlowEnabled_determineOutputExtensionWebpDisabled_shouldReadFile() throws Exception {
    ReflectionTestUtils.setField(wrapper, "tempDirForActiveProductImageConversion", "/tmp");
    ReflectionTestUtils.setField(wrapper, "webpConversionEnabled", false);
    ReflectionTestUtils.setField(wrapper, "newWebpFlowEnabled", true);
    XgpImageScaleRequest request = new XgpImageScaleRequest();
    request.setImageBytes(jpegHeader);

    FullImageUploadRequest fullRequest = new FullImageUploadRequest();
    fullRequest.setImagePath("/full/image/path");
    request.setFullImageUploadRequest(fullRequest);

    ImageResultDetail fullResponse = new ImageResultDetail();
    fullResponse.setSuccess(true);
    fullResponse.setTempFileLocation("/temp/full_processed.jpg");
    fullResponse.setImagePathLocation("/temp/full_processed.jpg");

    Mockito.when(graphicsProcessorService.scaleToWebP(Mockito.anyString(), Mockito.anyString(),
        Mockito.any(CustomGraphicsSettings.class), Mockito.anyString()))
      .thenReturn(fullResponse);

    IdentifyImageResult imageInfo = new IdentifyImageResult();
    String resultString = "JPEG,80,1920,1080";
    imageInfo.consumeOutput(new java.io.ByteArrayInputStream(resultString.getBytes()));
    Mockito.when(graphicsProcessorService.getGraphicsProperty(Mockito.anyString()))
      .thenReturn(imageInfo);

    ReflectionTestUtils.setField(wrapper, "executorService", MoreExecutors.newDirectExecutorService());

    wrapper.scaleActiveProductNewImages(STORE_ID, CLIENT_ID, request, StringUtils.EMPTY);
    Mockito.verify(imageConfig).getFullWidth();
    Mockito.verify(imageConfig).getFullDpi();
    Mockito.verify(imageConfig).getFullQuality();
    Mockito.verify(imageConfig).getFullHeight();
  }

  @Test
  void scaleActiveProductNewImages_newFlowEnabled_determineOutputExtensionThrowsException_shouldDefaultToJpg() throws Exception {
    ReflectionTestUtils.setField(wrapper, "tempDirForActiveProductImageConversion", "/tmp");
    ReflectionTestUtils.setField(wrapper, "webpConversionEnabled", false);
    ReflectionTestUtils.setField(wrapper, "newWebpFlowEnabled", true);
    XgpImageScaleRequest request = new XgpImageScaleRequest();
    request.setImageBytes(jpegHeader);

    FullImageUploadRequest fullRequest = new FullImageUploadRequest();
    fullRequest.setImagePath("/full/image/path");
    request.setFullImageUploadRequest(fullRequest);

    ImageResultDetail fullResponse = new ImageResultDetail();
    fullResponse.setSuccess(true);
    fullResponse.setTempFileLocation("/temp/full_processed.jpg");
    fullResponse.setImagePathLocation("/temp/full_processed.jpg");

    Mockito.when(graphicsProcessorService.scaleToWebP(Mockito.anyString(), Mockito.anyString(),
        Mockito.any(CustomGraphicsSettings.class), Mockito.anyString()))
      .thenReturn(fullResponse);

    IdentifyImageResult imageInfo = new IdentifyImageResult();
    String resultString = "JPEG,80,1920,1080";
    imageInfo.consumeOutput(new java.io.ByteArrayInputStream(resultString.getBytes()));
    Mockito.when(graphicsProcessorService.getGraphicsProperty(Mockito.anyString()))
      .thenReturn(imageInfo);

    ReflectionTestUtils.setField(wrapper, "executorService", MoreExecutors.newDirectExecutorService());

    wrapper.scaleActiveProductNewImages(STORE_ID, CLIENT_ID, request, StringUtils.EMPTY);

    Mockito.verify(imageConfig).getFullWidth();
    Mockito.verify(imageConfig).getFullDpi();
    Mockito.verify(imageConfig).getFullQuality();
    Mockito.verify(imageConfig).getFullHeight();
  }

  @Test
  void scaleActiveProductNewImages_newFlowEnabled_onlyMediumImage_shouldProcessOnlyMedium() throws Exception {
    ReflectionTestUtils.setField(wrapper, "tempDirForActiveProductImageConversion", "/tmp");
    ReflectionTestUtils.setField(wrapper, "webpConversionEnabled", true);
    ReflectionTestUtils.setField(wrapper, "newWebpFlowEnabled", true);
    XgpImageScaleRequest request = new XgpImageScaleRequest();
    request.setImageBytes(jpegHeader);

    MediumImageUploadRequest mediumRequest = new MediumImageUploadRequest();
    mediumRequest.setImagePath("/medium/image/path");
    request.setMediumImageUploadRequest(mediumRequest);

    ImageResultDetail mediumResponse = new ImageResultDetail();
    mediumResponse.setSuccess(true);
    mediumResponse.setTempFileLocation("/temp/medium_processed.webp");
    mediumResponse.setImagePathLocation("/temp/medium_processed.webp");

    Mockito.when(graphicsProcessorService.scaleToWebP(Mockito.anyString(), Mockito.anyString(),
        Mockito.any(CustomGraphicsSettings.class), Mockito.anyString()))
      .thenReturn(mediumResponse);

    IdentifyImageResult imageInfo = new IdentifyImageResult();
    String resultString = "JPEG,80,1920,1080";
    imageInfo.consumeOutput(new java.io.ByteArrayInputStream(resultString.getBytes()));
    Mockito.when(graphicsProcessorService.getGraphicsProperty(Mockito.anyString()))
      .thenReturn(imageInfo);

    ReflectionTestUtils.setField(wrapper, "executorService", MoreExecutors.newDirectExecutorService());

    wrapper.scaleActiveProductNewImages(STORE_ID, CLIENT_ID, request, StringUtils.EMPTY);

    Mockito.verify(graphicsProcessorService, Mockito.times(1))
      .scaleToWebP(Mockito.anyString(), Mockito.anyString(),
        Mockito.any(CustomGraphicsSettings.class), Mockito.anyString());
    Mockito.verify(fileStorageService, Mockito.times(1))
      .uploadToGcs(Mockito.any(ImageResultDetail.class), Mockito.eq(CLIENT_ID));
    Mockito.verify(imageConfig).getMediumWidth();
    Mockito.verify(imageConfig).getMediumDpi();
    Mockito.verify(imageConfig).getMediumQuality();
    Mockito.verify(imageConfig).getMediumHeight();
    Assertions.assertEquals(1, Mockito.mockingDetails(graphicsProcessorService).getInvocations().stream()
        .filter(inv -> inv.getMethod().getName().equals("scaleToWebP")).count());
  }

  @Test
  void scaleActiveProductNewImages_newFlowEnabled_onlyThumbnailImage_shouldProcessOnlyThumbnail() throws Exception {
    ReflectionTestUtils.setField(wrapper, "tempDirForActiveProductImageConversion", "/tmp");
    ReflectionTestUtils.setField(wrapper, "webpConversionEnabled", true);
    ReflectionTestUtils.setField(wrapper, "newWebpFlowEnabled", true);
    XgpImageScaleRequest request = new XgpImageScaleRequest();
    request.setImageBytes(jpegHeader);

    ThumbNailImageUploadRequest thumbnailRequest = new ThumbNailImageUploadRequest();
    thumbnailRequest.setImagePath("/thumbnail/image/path");
    request.setThumbNailImageUploadRequest(thumbnailRequest);

    ImageResultDetail thumbnailResponse = new ImageResultDetail();
    thumbnailResponse.setSuccess(true);
    thumbnailResponse.setTempFileLocation("/temp/thumbnail_processed.webp");
    thumbnailResponse.setImagePathLocation("/temp/thumbnail_processed.webp");

    Mockito.when(graphicsProcessorService.scaleToWebP(Mockito.anyString(), Mockito.anyString(),
        Mockito.any(CustomGraphicsSettings.class), Mockito.anyString()))
      .thenReturn(thumbnailResponse);

    IdentifyImageResult imageInfo = new IdentifyImageResult();
    String resultString = "JPEG,80,1920,1080";
    imageInfo.consumeOutput(new java.io.ByteArrayInputStream(resultString.getBytes()));
    Mockito.when(graphicsProcessorService.getGraphicsProperty(Mockito.anyString()))
      .thenReturn(imageInfo);

    ReflectionTestUtils.setField(wrapper, "executorService", MoreExecutors.newDirectExecutorService());

    wrapper.scaleActiveProductNewImages(STORE_ID, CLIENT_ID, request, StringUtils.EMPTY);
    Mockito.verify(graphicsProcessorService, Mockito.times(1))
      .scaleToWebP(Mockito.anyString(), Mockito.anyString(),
        Mockito.any(CustomGraphicsSettings.class), Mockito.anyString());

    Mockito.verify(fileStorageService, Mockito.times(1))
      .uploadToGcs(Mockito.any(ImageResultDetail.class), Mockito.eq(CLIENT_ID));

    Mockito.verify(imageConfig).getThumbnailWidth();
    Mockito.verify(imageConfig).getThumbnailDpi();
    Mockito.verify(imageConfig).getThumbnailQuality();
    Mockito.verify(imageConfig).getThumbnailHeight();
  }

  @Test
  void scaleActiveProductNewImages_newFlowEnabled_noImageRequests_shouldCompleteWithoutProcessing() throws Exception {
    ReflectionTestUtils.setField(wrapper, "tempDirForActiveProductImageConversion", "/tmp");
    ReflectionTestUtils.setField(wrapper, "webpConversionEnabled", true);
    ReflectionTestUtils.setField(wrapper, "newWebpFlowEnabled", true);
    XgpImageScaleRequest request = new XgpImageScaleRequest();
    request.setImageBytes(jpegHeader);

    IdentifyImageResult imageInfo = new IdentifyImageResult();
    String resultString = "JPEG,80,1920,1080";
    imageInfo.consumeOutput(new java.io.ByteArrayInputStream(resultString.getBytes()));
    Mockito.when(graphicsProcessorService.getGraphicsProperty(Mockito.anyString()))
      .thenReturn(imageInfo);

    ReflectionTestUtils.setField(wrapper, "executorService", MoreExecutors.newDirectExecutorService());

    wrapper.scaleActiveProductNewImages(STORE_ID, CLIENT_ID, request, StringUtils.EMPTY);
    Mockito.verify(graphicsProcessorService, Mockito.never())
      .scaleToWebP(Mockito.anyString(), Mockito.anyString(),
        Mockito.any(CustomGraphicsSettings.class), Mockito.anyString());

    Mockito.verify(fileStorageService, Mockito.never())
      .uploadToGcs(Mockito.any(ImageResultDetail.class), Mockito.eq(CLIENT_ID));
  }

  @Test
  void scaleActiveProductNewImages_newFlowEnabled_mediumImageProcessingThrowsException_shouldHandleGracefully() throws Exception {
    ReflectionTestUtils.setField(wrapper, "tempDirForActiveProductImageConversion", "/tmp");
    ReflectionTestUtils.setField(wrapper, "webpConversionEnabled", true);
    ReflectionTestUtils.setField(wrapper, "newWebpFlowEnabled", true);
    XgpImageScaleRequest request = new XgpImageScaleRequest();
    request.setImageBytes(jpegHeader);

    MediumImageUploadRequest mediumRequest = new MediumImageUploadRequest();
    mediumRequest.setImagePath("/medium/image/path");
    request.setMediumImageUploadRequest(mediumRequest);

    Mockito.when(graphicsProcessorService.scaleToWebP(Mockito.anyString(), Mockito.anyString(),
        Mockito.any(CustomGraphicsSettings.class), Mockito.anyString()))
      .thenThrow(new RuntimeException("Processing failed"));

    IdentifyImageResult imageInfo = new IdentifyImageResult();
    String resultString = "JPEG,80,1920,1080";
    imageInfo.consumeOutput(new java.io.ByteArrayInputStream(resultString.getBytes()));
    Mockito.when(graphicsProcessorService.getGraphicsProperty(Mockito.anyString()))
      .thenReturn(imageInfo);

    ReflectionTestUtils.setField(wrapper, "executorService", MoreExecutors.newDirectExecutorService());

    wrapper.scaleActiveProductNewImages(STORE_ID, CLIENT_ID, request, StringUtils.EMPTY);

    Mockito.verify(graphicsProcessorService, Mockito.times(1))
      .scaleToWebP(Mockito.anyString(), Mockito.anyString(),
        Mockito.any(CustomGraphicsSettings.class), Mockito.anyString());
    Mockito.verify(fileStorageService, Mockito.never())
      .uploadToGcs(Mockito.any(ImageResultDetail.class), Mockito.eq(CLIENT_ID));
    Mockito.verify(imageConfig).getMediumWidth();
    Mockito.verify(imageConfig).getMediumDpi();
    Mockito.verify(imageConfig).getMediumQuality();
    Mockito.verify(imageConfig).getMediumHeight();
    Assertions.assertNotNull(request);
  }

  @Test
  void scaleActiveProductNewImages_newFlowEnabled_thumbnailImageProcessingThrowsException_shouldHandleGracefully() throws Exception {
    ReflectionTestUtils.setField(wrapper, "tempDirForActiveProductImageConversion", "/tmp");
    ReflectionTestUtils.setField(wrapper, "webpConversionEnabled", true);
    ReflectionTestUtils.setField(wrapper, "newWebpFlowEnabled", true);
    XgpImageScaleRequest request = new XgpImageScaleRequest();
    request.setImageBytes(jpegHeader);

    ThumbNailImageUploadRequest thumbnailRequest = new ThumbNailImageUploadRequest();
    thumbnailRequest.setImagePath("/thumbnail/image/path");
    request.setThumbNailImageUploadRequest(thumbnailRequest);

    Mockito.when(graphicsProcessorService.scaleToWebP(Mockito.anyString(), Mockito.anyString(),
        Mockito.any(CustomGraphicsSettings.class), Mockito.anyString()))
      .thenThrow(new RuntimeException("Processing failed"));

    IdentifyImageResult imageInfo = new IdentifyImageResult();
    String resultString = "JPEG,80,1920,1080";
    imageInfo.consumeOutput(new java.io.ByteArrayInputStream(resultString.getBytes()));
    Mockito.when(graphicsProcessorService.getGraphicsProperty(Mockito.anyString()))
      .thenReturn(imageInfo);

    ReflectionTestUtils.setField(wrapper, "executorService", MoreExecutors.newDirectExecutorService());

    wrapper.scaleActiveProductNewImages(STORE_ID, CLIENT_ID, request, StringUtils.EMPTY);

    Mockito.verify(graphicsProcessorService, Mockito.times(1))
      .scaleToWebP(Mockito.anyString(), Mockito.anyString(),
        Mockito.any(CustomGraphicsSettings.class), Mockito.anyString());
    Mockito.verify(fileStorageService, Mockito.never())
      .uploadToGcs(Mockito.any(ImageResultDetail.class), Mockito.eq(CLIENT_ID));
    Mockito.verify(imageConfig).getThumbnailWidth();
    Mockito.verify(imageConfig).getThumbnailDpi();
    Mockito.verify(imageConfig).getThumbnailQuality();
    Mockito.verify(imageConfig).getThumbnailHeight();
    Assertions.assertNotNull(request);
  }

  @Test
  void scaleActiveProductNewImages_newFlowEnabled_futureGetThrowsException_shouldHandleGracefully() throws Exception {
    ReflectionTestUtils.setField(wrapper, "tempDirForActiveProductImageConversion", "/tmp");
    ReflectionTestUtils.setField(wrapper, "webpConversionEnabled", true);
    ReflectionTestUtils.setField(wrapper, "newWebpFlowEnabled", true);
    XgpImageScaleRequest request = new XgpImageScaleRequest();
    request.setImageBytes(jpegHeader);

    FullImageUploadRequest fullRequest = new FullImageUploadRequest();
    fullRequest.setImagePath("/full/image/path");
    request.setFullImageUploadRequest(fullRequest);

    IdentifyImageResult imageInfo = new IdentifyImageResult();
    String resultString = "JPEG,80,1920,1080";
    imageInfo.consumeOutput(new java.io.ByteArrayInputStream(resultString.getBytes()));
    Mockito.when(graphicsProcessorService.getGraphicsProperty(Mockito.anyString()))
      .thenReturn(imageInfo);

    ExecutorService mockExecutor = Mockito.mock(ExecutorService.class);
    Mockito.doAnswer(invocation -> {
      Runnable command = invocation.getArgument(0);
      command.run();
      return null;
    }).when(mockExecutor).execute(Mockito.any(Runnable.class));
    ReflectionTestUtils.setField(wrapper, "executorService", mockExecutor);

    ImageResultDetail fullResponse = new ImageResultDetail();
    fullResponse.setSuccess(true);
    fullResponse.setTempFileLocation("/temp/full_processed.webp");
    fullResponse.setImagePathLocation("/temp/full_processed.webp");

    Mockito.when(graphicsProcessorService.scaleToWebP(Mockito.anyString(), Mockito.anyString(),
        Mockito.any(CustomGraphicsSettings.class), Mockito.anyString()))
      .thenReturn(fullResponse);

    CompletableFuture<ImageResultDetail> future = new CompletableFuture<>();
    future.completeExceptionally(new ExecutionException("Future failed", new RuntimeException("Future failed")));
    ReflectionTestUtils.setField(wrapper, "executorService", MoreExecutors.newDirectExecutorService());

    wrapper.scaleActiveProductNewImages(STORE_ID, CLIENT_ID, request, StringUtils.EMPTY);

    Mockito.verify(imageConfig).getFullWidth();
    Mockito.verify(imageConfig).getFullDpi();
    Mockito.verify(imageConfig).getFullQuality();
    Mockito.verify(imageConfig).getFullHeight();
    Assertions.assertNotNull(request);
  }

  @Test
  void scaleActiveProductNewImages_oldFlow_isActive_shouldReturnActiveImage() throws Exception {
    ReflectionTestUtils.setField(wrapper, "tempDirForActiveProductImageConversion", "/tmp");
    ReflectionTestUtils.setField(wrapper, "webpConversionEnabled", true);
    ReflectionTestUtils.setField(wrapper, "newWebpFlowEnabled", false);
    XgpImageScaleRequest request = new XgpImageScaleRequest();
    request.setImageBytes(jpegHeader);
    request.setActive(true);

    FullImageUploadRequest fullRequest = new FullImageUploadRequest();
    fullRequest.setImagePath("/full/image/path");
    request.setFullImageUploadRequest(fullRequest);

    ImageResultDetail webpConversionResult = new ImageResultDetail();
    webpConversionResult.setSuccess(true);
    webpConversionResult.setImagePathLocation("/tmp/clientId/webp_converted.webp");

    Mockito.when(graphicsProcessorService.convert(Mockito.eq(TargetType.WEBP), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.any(CustomGraphicsSettings.class),
        Mockito.anyString(), Mockito.anyString(), Mockito.anyString()))
      .thenReturn(webpConversionResult);

    IdentifyImageResult imageInfo = new IdentifyImageResult();
    String resultString = "JPEG,80,1920,1080";
    imageInfo.consumeOutput(new java.io.ByteArrayInputStream(resultString.getBytes()));
    Mockito.when(graphicsProcessorService.getGraphicsProperty(Mockito.anyString()))
      .thenReturn(imageInfo);

    ReflectionTestUtils.setField(wrapper, "executorService", MoreExecutors.newDirectExecutorService());

    wrapper.scaleActiveProductNewImages(STORE_ID, CLIENT_ID, request, StringUtils.EMPTY);
    Mockito.verify(graphicsProcessorService, Mockito.times(1))
      .convert(Mockito.eq(TargetType.WEBP), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.any(CustomGraphicsSettings.class), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString());
    Mockito.verify(graphicsProcessorService, Mockito.never())
      .scale(Mockito.anyString(), Mockito.anyString(),
        Mockito.any(CustomGraphicsSettings.class), Mockito.anyString(), Mockito.anyBoolean(),
        Mockito.anyBoolean());
    Mockito.verify(imageConfig).getWebpDpi();
    Mockito.verify(imageConfig).getWebpQuality();
  }

  @Test
  void scaleActiveProductNewImages_oldFlow_webpConversionDisabled_shouldNotConvert() throws Exception {
    ReflectionTestUtils.setField(wrapper, "tempDirForActiveProductImageConversion", "/tmp");
    ReflectionTestUtils.setField(wrapper, "webpConversionEnabled", false);
    ReflectionTestUtils.setField(wrapper, "newWebpFlowEnabled", false);
    XgpImageScaleRequest request = new XgpImageScaleRequest();
    request.setImageBytes(jpegHeader);

    FullImageUploadRequest fullRequest = new FullImageUploadRequest();
    fullRequest.setImagePath("/full/image/path");
    request.setFullImageUploadRequest(fullRequest);

    ImageResultDetail fullResponse = new ImageResultDetail();
    fullResponse.setSuccess(true);
    fullResponse.setTempFileLocation("/temp/full_processed.jpg");
    fullResponse.setImagePathLocation("/temp/full_processed.jpg");

    Mockito.when(graphicsProcessorService.scale(Mockito.anyString(), Mockito.anyString(),
        Mockito.any(CustomGraphicsSettings.class), Mockito.anyString(), Mockito.anyBoolean(),
        Mockito.anyBoolean())).thenReturn(fullResponse);

    IdentifyImageResult imageInfo = new IdentifyImageResult();
    String resultString = "JPEG,80,1920,1080";
    imageInfo.consumeOutput(new java.io.ByteArrayInputStream(resultString.getBytes()));
    Mockito.when(graphicsProcessorService.getGraphicsProperty(Mockito.anyString()))
      .thenReturn(imageInfo);

    ReflectionTestUtils.setField(wrapper, "executorService", MoreExecutors.newDirectExecutorService());

    wrapper.scaleActiveProductNewImages(STORE_ID, CLIENT_ID, request, StringUtils.EMPTY);
    Mockito.verify(graphicsProcessorService, Mockito.never())
      .convert(Mockito.eq(TargetType.WEBP), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.any(CustomGraphicsSettings.class), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString());
    Mockito.verify(graphicsProcessorService, Mockito.times(1))
      .scale(Mockito.anyString(), Mockito.anyString(),
        Mockito.any(CustomGraphicsSettings.class), Mockito.anyString(), Mockito.anyBoolean(),
        Mockito.anyBoolean());

    Mockito.verify(imageConfig).getFullWidth();
    Mockito.verify(imageConfig).getFullDpi();
    Mockito.verify(imageConfig).getFullQuality();
    Mockito.verify(imageConfig).getFullHeight();
  }

  @Test
  void scaleActiveProductNewImages_oldFlow_uploadThrowsException_shouldHandleGracefully() throws Exception {
    ReflectionTestUtils.setField(wrapper, "tempDirForActiveProductImageConversion", "/tmp");
    ReflectionTestUtils.setField(wrapper, "webpConversionEnabled", true);
    ReflectionTestUtils.setField(wrapper, "newWebpFlowEnabled", false);
    XgpImageScaleRequest request = new XgpImageScaleRequest();
    request.setImageBytes(jpegHeader);

    FullImageUploadRequest fullRequest = new FullImageUploadRequest();
    fullRequest.setImagePath("/full/image/path");
    request.setFullImageUploadRequest(fullRequest);

    ImageResultDetail webpConversionResult = new ImageResultDetail();
    webpConversionResult.setSuccess(true);
    webpConversionResult.setImagePathLocation("/tmp/clientId/webp_converted.webp");

    ImageResultDetail fullResponse = new ImageResultDetail();
    fullResponse.setSuccess(true);
    fullResponse.setTempFileLocation("/temp/full_processed.webp");
    fullResponse.setImagePathLocation("/temp/full_processed.webp");

    Mockito.when(graphicsProcessorService.convert(Mockito.eq(TargetType.WEBP), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.any(CustomGraphicsSettings.class),
        Mockito.anyString(), Mockito.anyString(), Mockito.anyString()))
      .thenReturn(webpConversionResult);

    Mockito.when(graphicsProcessorService.scale(Mockito.anyString(), Mockito.anyString(),
        Mockito.any(CustomGraphicsSettings.class), Mockito.anyString(), Mockito.anyBoolean(),
        Mockito.anyBoolean())).thenReturn(fullResponse);

    Mockito.doThrow(new RuntimeException("Upload failed"))
      .when(fileStorageService).uploadToGcs(Mockito.any(ImageResultDetail.class), Mockito.eq(CLIENT_ID));

    IdentifyImageResult imageInfo = new IdentifyImageResult();
    String resultString = "JPEG,80,1920,1080";
    imageInfo.consumeOutput(new java.io.ByteArrayInputStream(resultString.getBytes()));
    Mockito.when(graphicsProcessorService.getGraphicsProperty(Mockito.anyString()))
      .thenReturn(imageInfo);

    ReflectionTestUtils.setField(wrapper, "executorService", MoreExecutors.newDirectExecutorService());

    wrapper.scaleActiveProductNewImages(STORE_ID, CLIENT_ID, request, StringUtils.EMPTY);

    Mockito.verify(fileStorageService, Mockito.times(1))
      .uploadToGcs(Mockito.any(ImageResultDetail.class), Mockito.eq(CLIENT_ID));

    Mockito.verify(imageConfig).getWebpDpi();
    Mockito.verify(imageConfig).getWebpQuality();
    Mockito.verify(imageConfig).getFullWidth();
    Mockito.verify(imageConfig).getFullDpi();
    Mockito.verify(imageConfig).getFullQuality();
    Mockito.verify(imageConfig).getFullHeight();
  }

  @Test
  void scaleActiveProductNewImages_oldFlow_resultIsNull_shouldNotAddToResponses() throws Exception {
    ReflectionTestUtils.setField(wrapper, "tempDirForActiveProductImageConversion", "/tmp");
    ReflectionTestUtils.setField(wrapper, "webpConversionEnabled", true);
    ReflectionTestUtils.setField(wrapper, "newWebpFlowEnabled", false);
    XgpImageScaleRequest request = new XgpImageScaleRequest();
    request.setImageBytes(jpegHeader);

    FullImageUploadRequest fullRequest = new FullImageUploadRequest();
    fullRequest.setImagePath("/full/image/path");
    request.setFullImageUploadRequest(fullRequest);

    ImageResultDetail webpConversionResult = new ImageResultDetail();
    webpConversionResult.setSuccess(true);
    webpConversionResult.setImagePathLocation("/tmp/clientId/webp_converted.webp");

    Mockito.when(graphicsProcessorService.convert(Mockito.eq(TargetType.WEBP), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.any(CustomGraphicsSettings.class),
        Mockito.anyString(), Mockito.anyString(), Mockito.anyString()))
      .thenReturn(webpConversionResult);

    Mockito.when(graphicsProcessorService.scale(Mockito.anyString(), Mockito.anyString(),
        Mockito.any(CustomGraphicsSettings.class), Mockito.anyString(), Mockito.anyBoolean(),
        Mockito.anyBoolean())).thenReturn(null);

    IdentifyImageResult imageInfo = new IdentifyImageResult();
    String resultString = "JPEG,80,1920,1080";
    imageInfo.consumeOutput(new java.io.ByteArrayInputStream(resultString.getBytes()));
    Mockito.when(graphicsProcessorService.getGraphicsProperty(Mockito.anyString()))
      .thenReturn(imageInfo);

    ReflectionTestUtils.setField(wrapper, "executorService", MoreExecutors.newDirectExecutorService());

    wrapper.scaleActiveProductNewImages(STORE_ID, CLIENT_ID, request, StringUtils.EMPTY);
    Mockito.verify(fileStorageService, Mockito.never())
      .uploadToGcs(Mockito.any(ImageResultDetail.class), Mockito.eq(CLIENT_ID));

    Mockito.verify(imageConfig).getWebpDpi();
    Mockito.verify(imageConfig).getWebpQuality();
    Mockito.verify(imageConfig).getFullWidth();
    Mockito.verify(imageConfig).getFullDpi();
    Mockito.verify(imageConfig).getFullQuality();
    Mockito.verify(imageConfig).getFullHeight();
  }

  @Test
  void scaleActiveProductNewImages_oldFlow_resultNotSuccess_shouldNotUpload() throws Exception {
    ReflectionTestUtils.setField(wrapper, "tempDirForActiveProductImageConversion", "/tmp");
    ReflectionTestUtils.setField(wrapper, "webpConversionEnabled", true);
    ReflectionTestUtils.setField(wrapper, "newWebpFlowEnabled", false);
    XgpImageScaleRequest request = new XgpImageScaleRequest();
    request.setImageBytes(jpegHeader);

    FullImageUploadRequest fullRequest = new FullImageUploadRequest();
    fullRequest.setImagePath("/full/image/path");
    request.setFullImageUploadRequest(fullRequest);

    ImageResultDetail webpConversionResult = new ImageResultDetail();
    webpConversionResult.setSuccess(true);
    webpConversionResult.setImagePathLocation("/tmp/clientId/webp_converted.webp");

    ImageResultDetail unsuccessfulResult = new ImageResultDetail();
    unsuccessfulResult.setSuccess(false);
    unsuccessfulResult.setErrorMessage("Processing failed");

    Mockito.when(graphicsProcessorService.convert(Mockito.eq(TargetType.WEBP), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.any(CustomGraphicsSettings.class),
        Mockito.anyString(), Mockito.anyString(), Mockito.anyString()))
      .thenReturn(webpConversionResult);

    Mockito.when(graphicsProcessorService.scale(Mockito.anyString(), Mockito.anyString(),
        Mockito.any(CustomGraphicsSettings.class), Mockito.anyString(), Mockito.anyBoolean(),
        Mockito.anyBoolean())).thenReturn(unsuccessfulResult);

    IdentifyImageResult imageInfo = new IdentifyImageResult();
    String resultString = "JPEG,80,1920,1080";
    imageInfo.consumeOutput(new java.io.ByteArrayInputStream(resultString.getBytes()));
    Mockito.when(graphicsProcessorService.getGraphicsProperty(Mockito.anyString()))
      .thenReturn(imageInfo);

    ReflectionTestUtils.setField(wrapper, "executorService", MoreExecutors.newDirectExecutorService());

    wrapper.scaleActiveProductNewImages(STORE_ID, CLIENT_ID, request, StringUtils.EMPTY);
    Mockito.verify(fileStorageService, Mockito.never())
      .uploadToGcs(Mockito.any(ImageResultDetail.class), Mockito.eq(CLIENT_ID));

    Mockito.verify(imageConfig).getWebpDpi();
    Mockito.verify(imageConfig).getWebpQuality();
    Mockito.verify(imageConfig).getFullWidth();
    Mockito.verify(imageConfig).getFullDpi();
    Mockito.verify(imageConfig).getFullQuality();
    Mockito.verify(imageConfig).getFullHeight();
  }

  @Test
  void scaleActiveProductNewImages_oldFlow_processImageTypeOldFlowThrowsException_shouldHandleGracefully() throws Exception {
    ReflectionTestUtils.setField(wrapper, "tempDirForActiveProductImageConversion", "/tmp");
    ReflectionTestUtils.setField(wrapper, "webpConversionEnabled", true);
    ReflectionTestUtils.setField(wrapper, "newWebpFlowEnabled", false);
    XgpImageScaleRequest request = new XgpImageScaleRequest();
    request.setImageBytes(jpegHeader);

    FullImageUploadRequest fullRequest = new FullImageUploadRequest();
    fullRequest.setImagePath("/full/image/path");
    request.setFullImageUploadRequest(fullRequest);

    ImageResultDetail webpConversionResult = new ImageResultDetail();
    webpConversionResult.setSuccess(true);
    webpConversionResult.setImagePathLocation("/tmp/clientId/webp_converted.webp");

    Mockito.when(graphicsProcessorService.convert(Mockito.eq(TargetType.WEBP), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.any(CustomGraphicsSettings.class),
        Mockito.anyString(), Mockito.anyString(), Mockito.anyString()))
      .thenReturn(webpConversionResult);

    Mockito.when(graphicsProcessorService.scale(Mockito.anyString(), Mockito.anyString(),
        Mockito.any(CustomGraphicsSettings.class), Mockito.anyString(), Mockito.anyBoolean(),
        Mockito.anyBoolean())).thenThrow(new RuntimeException("Processing failed"));

    IdentifyImageResult imageInfo = new IdentifyImageResult();
    String resultString = "JPEG,80,1920,1080";
    imageInfo.consumeOutput(new java.io.ByteArrayInputStream(resultString.getBytes()));
    Mockito.when(graphicsProcessorService.getGraphicsProperty(Mockito.anyString()))
      .thenReturn(imageInfo);

    ReflectionTestUtils.setField(wrapper, "executorService", MoreExecutors.newDirectExecutorService());

    Assertions.assertThrows(Exception.class, () -> {
      wrapper.scaleActiveProductNewImages(STORE_ID, CLIENT_ID, request, StringUtils.EMPTY);
    });

    Mockito.verify(imageConfig).getWebpDpi();
    Mockito.verify(imageConfig).getWebpQuality();
    Mockito.verify(imageConfig).getFullWidth();
    Mockito.verify(imageConfig).getFullDpi();
    Mockito.verify(imageConfig).getFullQuality();
    Mockito.verify(imageConfig).getFullHeight();
  }

}

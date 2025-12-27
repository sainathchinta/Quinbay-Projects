package com.gdn.micro.graphics.service;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;
import java.util.stream.Collectors;

import com.gdn.micro.graphics.web.model.FullImageUploadRequest;
import org.apache.tika.Tika;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.micro.graphics.AvailableSize;
import com.gdn.micro.graphics.config.KafkaPublisher;
import com.gdn.micro.graphics.domain.event.model.ImagePathResult;
import com.gdn.micro.graphics.domain.event.model.ScaleImagesResponse;
import com.gdn.micro.graphics.model.GraphicDetailCommand;
import com.gdn.micro.graphics.model.ImagePaths;
import com.gdn.micro.graphics.model.ResizeImageScalingModel;
import com.gdn.micro.graphics.service.config.GcsProperties;
import com.gdn.micro.graphics.util.Constants;
import com.gdn.micro.graphics.web.model.XgpImageScaleRequest;
import com.google.cloud.storage.Bucket;
import com.google.common.collect.Lists;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.gdn.common.util.GdnMandatoryRequestParameterUtil;
import com.gdn.micro.graphics.config.KafkaTopicProperties;
import com.gdn.micro.graphics.domain.event.model.BulkImageProcessResponse;
import com.gdn.micro.graphics.domain.event.model.ImageDownloadResponse;
import com.gdn.micro.graphics.domain.event.model.ImageResponse;
import com.gdn.micro.graphics.domain.event.model.ScaleEditedImagesResponse;
import com.gdn.micro.graphics.domain.event.model.ScaleImageResponse;
import com.gdn.micro.graphics.model.CustomGraphicsSettings;
import com.gdn.micro.graphics.model.GraphicImageDetail;
import com.gdn.micro.graphics.model.ImageProcessingModel;
import com.gdn.micro.graphics.model.PrioritySeller;
import com.gdn.micro.graphics.service.executor.AsyncImageProcessor;
import com.gdn.micro.graphics.service.executor.ImageDownloadProcess;
import com.gdn.micro.graphics.utils.GraphicsProcessorHelper;
import com.gdn.micro.graphics.web.model.ScaleEditedImageRequest;
import com.gdn.micro.graphics.web.model.ScaleImageRequest;
import com.gdn.micro.graphics.domain.event.model.ImageResultDetail;
import com.gdn.micro.graphics.service.config.ImageConfigurationProperties;
import com.gdn.micro.graphics.model.GraphicDimension;
import com.gdn.micro.graphics.model.IdentifyImageResult;
import com.gdn.micro.graphics.service.enums.TargetType;

/**
 * Created by Vishal on 21/06/18.
 */

@Service
@Slf4j
public class AsyncGraphicsProcessServiceImpl implements AsyncGraphicsProcessService {

  private static final Logger LOG = LoggerFactory.getLogger(AsyncGraphicsProcessServiceImpl.class);
  private static final String JPG_EXTENSION = ".jpg";
  private static final String WEBP_EXTENSION = ".webp";
  private static final String UNDERSCORE = "_";
  private static final String PERMITTED_CHARS = "[^a-zA-Z0-9-]";
  private static final String DELIMITER_DOT = ".";
  private static final Tika TIKA = new Tika();

  @Autowired
  private GraphicsProcessorService service;

  @Autowired
  private ExecutorService executorService;

  @Autowired
  private KafkaPublisher kafkaPublisher;

  @Autowired
  private FileStorageService fileStorageService;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Autowired
  private GcsService gcsService;

  @Autowired
  private GcsProperties gcsProperties;

  @Autowired
  @Qualifier("orderImageBucket")
  private Bucket orderImageBucket;

  @Autowired
  private AsyncGraphicsProcessorServiceWrapper asyncGraphicsProcessorServiceWrapper;

  @Value("${resize}")
  private String sourcePath;

  @Value("${mta.source.directory}")
  private String sourcePrefixPath;

  @Value("${parallel.image.download.enabled}")
  private boolean parallelImageDownloadEnabled;

  @Value("${event.based.processing.enabled}")
  private boolean eventBasedProcessingEnabled;

  @Value("${parallel.image.resize.enabled}")
  private boolean enableParallelImageResize;

  @Value("${image.scale.limit}")
  private int imageScaleLimit;

  @Value("${webp.conversion.enabled}")
  private boolean webpConversionEnabled;

  @Value("${new.webp.flow.enabled}")
  private boolean newWebpFlowEnabled;

  @Value("${temp.dir.for.active.product.image.conversion}")
  private String tempDirForActiveProductImageConversion;

  @Autowired
  private ImageConfigurationProperties imageConfig;

  private static final ObjectMapper OBJECT_MAPPER = new ObjectMapper();

  @Override
  public void scaleBulkImages(final List<GraphicImageDetail> graphicImageDetails, final String groupCode,
      final String clientId, final boolean isRevised, int prioritySeller) throws Exception {
    final String username = GdnMandatoryRequestParameterUtil.getUsername();
    final String storeId = GdnMandatoryRequestParameterUtil.getStoreId();
    if (eventBasedProcessingEnabled) {
      String scaleBulkEvent = kafkaTopicProperties.getImageScalingNoPriority();
      if (PrioritySeller.PRIORITY_1.getPrioritySeller() == prioritySeller) {
        scaleBulkEvent = kafkaTopicProperties.getImageScalingPriority1();
      } else if (PrioritySeller.PRIORITY_2.getPrioritySeller() == prioritySeller) {
        scaleBulkEvent = kafkaTopicProperties.getImageScalingPriority2();
      }
      kafkaPublisher.send(scaleBulkEvent, groupCode,
          new ImageProcessingModel(graphicImageDetails, groupCode, username, storeId, clientId, false, false, isRevised,
              null, prioritySeller));
    } else {
      executorService.execute(
          () -> processImages(graphicImageDetails, groupCode, username, storeId, clientId, false, false, isRevised,
              null, prioritySeller));
    }
  }

  @Override
  public void resizeBulkImages(List<GraphicImageDetail> graphicImageDetails,
      String groupCode, String clientId, int prioritySeller) throws Exception {
    final String username = GdnMandatoryRequestParameterUtil.getUsername();
    final String storeId = GdnMandatoryRequestParameterUtil.getStoreId();
    if (eventBasedProcessingEnabled) {
      String resizeBulkEvent = kafkaTopicProperties.getImageResize();
      if (PrioritySeller.PRIORITY_1.getPrioritySeller() == prioritySeller) {
        resizeBulkEvent = kafkaTopicProperties.getImageResizePriority1();
      } else if (PrioritySeller.PRIORITY_2.getPrioritySeller() == prioritySeller) {
        resizeBulkEvent = kafkaTopicProperties.getImageResizePriority2();
      }
      kafkaPublisher.send(resizeBulkEvent, groupCode,
          new ImageProcessingModel(graphicImageDetails, groupCode, username, storeId, clientId, true, false, false,
              null, prioritySeller));
    } else {
      executorService.execute(
              () -> processImages(graphicImageDetails, groupCode, username, storeId, clientId, true, false, false, null, prioritySeller));
    }
  }

  @Override
  public void resizeEditedImages(List<GraphicImageDetail> graphicImageDetails,
      String groupCode, String clientId) throws Exception {
    final String username = GdnMandatoryRequestParameterUtil.getUsername();
    final String storeId = GdnMandatoryRequestParameterUtil.getStoreId();
    if (eventBasedProcessingEnabled) {
      kafkaPublisher.send(kafkaTopicProperties.getImageEditResize(), groupCode,
          new ImageProcessingModel(graphicImageDetails, groupCode, username, storeId, clientId, true, true, false, null,
              0));
    } else {
      executorService.execute(
          () -> processImages(graphicImageDetails, groupCode, username, storeId, clientId, true, true, false, null, 0));
    }
  }

  @Override
  public void resizeRevisedImages(List<GraphicImageDetail> graphicImageDetails, String groupCode, String clientId)
      throws Exception {
    final String username = GdnMandatoryRequestParameterUtil.getUsername();
    final String storeId = GdnMandatoryRequestParameterUtil.getStoreId();
    if (eventBasedProcessingEnabled) {
      kafkaPublisher.send(kafkaTopicProperties.getRevisedImageResize(), groupCode,
          new ImageProcessingModel(graphicImageDetails, groupCode, username, storeId, clientId, true, false, true, null, 0));
    } else {
      executorService.execute(
          () -> processImages(graphicImageDetails, groupCode, username, storeId, clientId, true, false, true, null, 0));
    }
  }

  @Override
  public void scaleEditedImages(List<CustomGraphicsSettings> customGraphicsSettings, String groupCode, String clientId,
      ScaleEditedImageRequest request, String prefix) throws Exception {
    final String username = GdnMandatoryRequestParameterUtil.getUsername();
    final String storeId = GdnMandatoryRequestParameterUtil.getStoreId();
    List<GraphicImageDetail> graphicImageDetails = new ArrayList<>();
    for (ScaleImageRequest imageRequest : request.getImageRequests()) {
      if (!imageRequest.isActive()) {
        for (CustomGraphicsSettings graphicSettings : customGraphicsSettings) {
          String imageNameWithoutExtension = imageRequest.getImageName().split("\\.(?=[^.]*$)")[0];
          StringBuilder newImageName =
              new StringBuilder(imageNameWithoutExtension.replaceAll(PERMITTED_CHARS, UNDERSCORE));
          if (webpConversionEnabled) {
            newImageName.append(WEBP_EXTENSION);
          } else {
            newImageName.append(JPG_EXTENSION);
          }
          String generatedImageLocation = GraphicsProcessorHelper
              .getFileSystemPath(request.getProductCode(), newImageName.toString(),
                  GraphicsProcessorHelper.getSize(graphicSettings), prefix);
          String sourceLocationPath =
              sourcePath.concat(GraphicsProcessorHelper.PATH_SEPARATOR).concat(imageRequest.getImagePathLocation());
          GraphicImageDetail graphicImageDetail =
              new GraphicImageDetail(imageRequest.getHashCode(), sourceLocationPath, generatedImageLocation,
                  graphicSettings, prefix, request.getProductCode());
          graphicImageDetail.setCommonImage(imageRequest.isCommonImage());
          graphicImageDetails.add(graphicImageDetail);
        }
      }
    }
    if (eventBasedProcessingEnabled) {
      kafkaPublisher.send(kafkaTopicProperties.getEditedImageScaling(), groupCode,
          new ImageProcessingModel(graphicImageDetails, groupCode, username, storeId, clientId, false, true, false,
              request, 0));
    } else {
      executorService.execute(
          () -> processImages(graphicImageDetails, groupCode, username, storeId, clientId, false, true, false,
              request, 0));
    }
  }

  @Override
  public void processImages(final List<GraphicImageDetail> graphicImageDetails, final String groupCode,
                            final String username, final String storeId, final String clientId, boolean isResize, boolean isEdited, boolean isRevised, ScaleEditedImageRequest request, int prioritySeller) {
    long startTime = System.currentTimeMillis();
    BulkImageProcessResponse bulkImageProcessResponse = new BulkImageProcessResponse();
    bulkImageProcessResponse.setGroupCode(groupCode);
    bulkImageProcessResponse.setStoreId(storeId);
    bulkImageProcessResponse.setUsername(username);
    try {
      List<AsyncImageProcessor> asyncImageProcessors = new ArrayList<>();
      List<ImageResponse> imageResponses = new ArrayList<>();

      Collection<String> tempDownloadedImages = Collections.EMPTY_LIST;
      if (parallelImageDownloadEnabled) {
        tempDownloadedImages = downloadImagesFromGcsAndSaveToFileStoreParallel(graphicImageDetails, isResize, isEdited);
      } else {
        tempDownloadedImages = fileStorageService.downloadImagesAndSaveToTempLocation(graphicImageDetails, isResize, isEdited);
      }

      for (GraphicImageDetail graphicImageDetail : graphicImageDetails) {
        AsyncImageProcessor asyncImageProcessor =
            new AsyncImageProcessor(graphicImageDetail.getHashCode(), graphicImageDetail.getSourcePath(),
                graphicImageDetail.getDestinationPath(), graphicImageDetail.getCustomGraphicsSettings(),
                graphicImageDetail.getPrefixPath(), clientId, isResize, service, isEdited);
        asyncImageProcessor.setCommonImage(graphicImageDetail.isCommonImage());
        asyncImageProcessors.add(asyncImageProcessor);
      }


      if (enableParallelImageResize) {
        List<Future<ImageResponse>> futureResults = executorService.invokeAll(asyncImageProcessors);
        for (Future<ImageResponse> imageResponse : futureResults) {
          imageResponses.add(imageResponse.get());
        }
      }
      else {
        for (AsyncImageProcessor asyncImageProcessor: asyncImageProcessors) {
          imageResponses.add(asyncImageProcessor.call());
        }
      }


      LOG.info("image processing is completed. Will start with upload");
      for (ImageResponse imageResponse : imageResponses) {
        if (imageResponse.isUploadRequired()) {
          fileStorageService.saveImages(imageResponse.getDestinationPath(), imageResponse, imageResponse.isResize(),
              imageResponse.getProductCodeFileName(), imageResponse.getEdited());
        }
      }

      bulkImageProcessResponse.setImageResponses(imageResponses);
      if (!isResize && !isEdited & !isRevised) {
        String imageDetailStatusEvent = kafkaTopicProperties.getImageProcessStatusNoPriority();
        if (PrioritySeller.PRIORITY_1.getPrioritySeller() == prioritySeller) {
          imageDetailStatusEvent = kafkaTopicProperties.getImageProcessStatusPriority1();
        } else if (PrioritySeller.PRIORITY_2.getPrioritySeller() == prioritySeller) {
          imageDetailStatusEvent = kafkaTopicProperties.getImageProcessStatusPriority2();
        }
        kafkaPublisher.send(imageDetailStatusEvent, groupCode, bulkImageProcessResponse);
      } else if (!isResize && Objects.nonNull(request)) {
        ScaleEditedImagesResponse scaleEditedImagesResponse =
            ScaleEditedImagesResponse.builder().productCode(request.getProductCode()).storeId(storeId)
                .username(username).imageResponses(new ArrayList<>()).build();
        scaleEditedImagesResponse
            .setImageResponses(this.toScaleImageResponseList(imageResponses, request.getImageRequests(), clientId));
        LOG.info("scaleEditedImagesResponse getting published : {}",scaleEditedImagesResponse);
        kafkaPublisher.send(kafkaTopicProperties.getEditedImageScaleStatus(), groupCode, scaleEditedImagesResponse);
      } else if (!isResize && isRevised) {
        ScaleEditedImagesResponse scaleEditedImagesResponse =
            ScaleEditedImagesResponse.builder().productCode(groupCode).storeId(storeId).username(username)
                .imageResponses(new ArrayList<>()).build();
        scaleEditedImagesResponse.setImageResponses(
            this.toScaleImageResponseList(bulkImageProcessResponse.getImageResponses(), new ArrayList<>(), null));
        LOG.info("scaleEditedImagesResponse getting published for revised product : {}", scaleEditedImagesResponse);
        kafkaPublisher.send(kafkaTopicProperties.getRevisedImageScaleStatus(), groupCode, scaleEditedImagesResponse);
      } else if (isResize && isEdited) {
        kafkaPublisher.send(kafkaTopicProperties.getEditedImageResizeStatus(), groupCode, bulkImageProcessResponse);
      } else if (isResize && isRevised) {
        kafkaPublisher.send(kafkaTopicProperties.getRevisedImageResizeStatus(), groupCode, bulkImageProcessResponse);
      } else if (isResize) {
        String resizeStatusEvent = kafkaTopicProperties.getImageResizeStatusNoPriority();
        if (PrioritySeller.PRIORITY_1.getPrioritySeller() == prioritySeller) {
          resizeStatusEvent = kafkaTopicProperties.getImageResizeStatusPriority1();
        } else if (PrioritySeller.PRIORITY_2.getPrioritySeller() == prioritySeller) {
          resizeStatusEvent = kafkaTopicProperties.getImageResizeStatusPriority2();
        }
        kafkaPublisher.send(resizeStatusEvent, groupCode, bulkImageProcessResponse);
      }

      fileStorageService.deleteTempCreatedImages(tempDownloadedImages);
      fileStorageService.deleteDestinationPath(
          graphicImageDetails.stream().map(GraphicImageDetail::getDestinationPath).collect(Collectors.toSet()),
          isResize);
    } catch (Exception e) {
      LOG.error("can not get result for async process, groupCode : {} ", groupCode, e);
    }

    long endTime = System.currentTimeMillis();
    LOG.info("End to end resize time graphicImageDetail : {} , username : {}, timeTaken : {} ", graphicImageDetails, username, endTime - startTime);
  }

  @Override
  public void scaleListOfImages(ResizeImageScalingModel resizeImageScalingModel) {
    List<ImagePathResult> imagePathResult = new ArrayList<>();
    validateImageListSize(resizeImageScalingModel, imagePathResult);
    List<GraphicDetailCommand> graphicDetailCommandList =
      downloadFileFromGcsToLocal(resizeImageScalingModel, imagePathResult);
    asyncGraphicsProcessorServiceWrapper.scaleImage(graphicDetailCommandList, true,
      resizeImageScalingModel.getClientId(), imagePathResult);
    ScaleImagesResponse scaleImagesResponse =
      populateImageScalingResult(resizeImageScalingModel, imagePathResult);
    log.info("Publishing event = {} , topic = {} , for identifier = {} ", scaleImagesResponse,
      kafkaTopicProperties.getImageResizeScalingResult(),
      resizeImageScalingModel.getUniqueIdentifier());
    kafkaPublisher.send(kafkaTopicProperties.getImageResizeScalingResult(),
      resizeImageScalingModel.getUniqueIdentifier(), scaleImagesResponse);
  }

  @Override
  public void scaleActiveProductNewImages(String storeId, String clientId,
    XgpImageScaleRequest xgpImageScaleRequest, String requestId) throws Exception {
    
    long totalStartTime = System.currentTimeMillis();
    long stepStartTime;
    long stepEndTime;

    File tempImageFile = null;
    List<ImageResultDetail> allImageResponses = new ArrayList<>();

    try {
      stepStartTime = System.currentTimeMillis();
      if (Objects.isNull(xgpImageScaleRequest) || ArrayUtils.isEmpty(xgpImageScaleRequest.getImageBytes())) {
        log.error("Invalid request data: xgpImageScaleRequest or imageBytes is null");
        throw new ApplicationRuntimeException();
      }

      log.info("Starting scaleActiveProductNewImages for path {}, requestId {} ",
        Optional.of(xgpImageScaleRequest).map(XgpImageScaleRequest::getFullImageUploadRequest)
          .map(FullImageUploadRequest::getImagePath).orElse("null"), requestId);

      tempImageFile = createTempFileFromBytes(xgpImageScaleRequest.getImageBytes(), clientId);
      stepEndTime = System.currentTimeMillis();
      log.info("Step 1 - Temp file creation completed in {} ms for requestId {} ",
        stepEndTime - stepStartTime, requestId);

      stepStartTime = System.currentTimeMillis();
      if (newWebpFlowEnabled) {
        log.info("Using NEW optimized WebP flow for requestId {}", requestId);
        processWithNewFlow(xgpImageScaleRequest, tempImageFile, clientId, allImageResponses);
      } else {
        log.info("Using OLD WebP flow for requestId {}", requestId);
        processWithOldFlow(xgpImageScaleRequest, tempImageFile, clientId, requestId, allImageResponses);
      }
      stepEndTime = System.currentTimeMillis();
      log.info("Image processing completed in {} ms for requestId {} ",
        stepEndTime - stepStartTime, requestId);

      log.info("Successfully processed {} images for requestId {} ", allImageResponses.size(),
        requestId);

    } catch (Exception e) {
      log.error("Error processing scaleActiveProductNewImages for storeId: {}, clientId: {}",
        storeId, clientId, e);
      throw new ApplicationException(ErrorCategory.UNSPECIFIED,
        "failed to process image processing for request : {}" + xgpImageScaleRequest);

    } finally {
      // Step 3: Final cleanup - only cleanup files that weren't already cleaned up incrementally
      stepStartTime = System.currentTimeMillis();
      cleanupTempFile(tempImageFile);
      for (ImageResultDetail imageResponse : allImageResponses) {
        // Only cleanup if file path is still set (wasn't cleaned up incrementally)
        if (Objects.nonNull(imageResponse.getImagePathLocation())) {
          cleanupTempFile(new File(imageResponse.getImagePathLocation()));
        }
      }
      stepEndTime = System.currentTimeMillis();
      log.info("Step 3 - Final cleanup completed in {} ms", stepEndTime - stepStartTime);
    }

    long totalEndTime = System.currentTimeMillis();
    long totalProcessingTime = totalEndTime - totalStartTime;

    log.info("scaleActiveProductNewImages completed. Total time: {} ms, Images processed: {}, "
      + "requetId {} ", totalProcessingTime, allImageResponses.size(), requestId);
  }

  /**
   * NEW FLOW: Optimized processing with single pass scale+convert and immediate cleanup
   */
  private void processWithNewFlow(XgpImageScaleRequest xgpImageScaleRequest, File tempImageFile,
    String clientId, List<ImageResultDetail> allImageResponses) throws Exception {

    if (xgpImageScaleRequest.isActive()) {
      ImageResultDetail activeImageResponse =
        getImageResultDetail(xgpImageScaleRequest, tempImageFile);
      allImageResponses.add(activeImageResponse);
    } else {
      // Process all image types in parallel with immediate upload + cleanup
      List<CompletableFuture<ImageResultDetail>> processingFutures = new ArrayList<>();
      final File finalTempImageFile = tempImageFile;

      if (Objects.nonNull(xgpImageScaleRequest.getFullImageUploadRequest())) {
        processingFutures.add(CompletableFuture.supplyAsync(() -> {
          try {
            ImageResultDetail result = processImageType(finalTempImageFile,
              xgpImageScaleRequest.getFullImageUploadRequest().getImagePath(),
              AvailableSize.FULL.getName());
            fileStorageService.uploadToGcs(result, clientId);
            log.debug("Successfully uploaded FULL image: {}", result.getDestinationPath());
            File tempFile = new File(result.getImagePathLocation());
            cleanupTempFile(tempFile);
            result.setImagePathLocation(null);
            return result;
          } catch (Exception e) {
            log.error("Error processing FULL image", e);
            return null;
          }
        }, executorService));
      }

      if (Objects.nonNull(xgpImageScaleRequest.getMediumImageUploadRequest())) {
        processingFutures.add(CompletableFuture.supplyAsync(() -> {
          try {
            ImageResultDetail result = processImageType(finalTempImageFile,
              xgpImageScaleRequest.getMediumImageUploadRequest().getImagePath(),
              AvailableSize.MEDIUM.getName());
            fileStorageService.uploadToGcs(result, clientId);
            log.debug("Successfully uploaded MEDIUM image: {}", result.getDestinationPath());
            File tempFile = new File(result.getImagePathLocation());
            cleanupTempFile(tempFile);
            result.setImagePathLocation(null);
            return result;
          } catch (Exception e) {
            log.error("Error processing MEDIUM image", e);
            return null;
          }
        }, executorService));
      }

      if (Objects.nonNull(xgpImageScaleRequest.getThumbNailImageUploadRequest())) {
        processingFutures.add(CompletableFuture.supplyAsync(() -> {
          try {
            ImageResultDetail result = processImageType(finalTempImageFile,
              xgpImageScaleRequest.getThumbNailImageUploadRequest().getImagePath(),
              AvailableSize.THUMBNAIL.getName());
            fileStorageService.uploadToGcs(result, clientId);
            log.debug("Successfully uploaded THUMBNAIL image: {}", result.getDestinationPath());
            File tempFile = new File(result.getImagePathLocation());
            cleanupTempFile(tempFile);
            result.setImagePathLocation(null);
            return result;
          } catch (Exception e) {
            log.error("Error processing THUMBNAIL image", e);
            return null;
          }
        }, executorService));
      }

      CompletableFuture<Void> allProcessing =
        CompletableFuture.allOf(processingFutures.toArray(new CompletableFuture[0]));

      allProcessing.thenRun(() -> {
        for (CompletableFuture<ImageResultDetail> future : processingFutures) {
          try {
            ImageResultDetail result = future.get();
            if (Objects.nonNull(result)) {
              allImageResponses.add(result);
            }
          } catch (Exception e) {
            log.error("Error getting processing result", e);
          }
        }
      }).get();
    }
  }

  private void processWithOldFlow(XgpImageScaleRequest xgpImageScaleRequest, File tempImageFile,
    String clientId, String requestId, List<ImageResultDetail> allImageResponses) throws Exception {

    long stepStartTime;
    long stepEndTime;

    // Step 2: WebP conversion (if needed)
    stepStartTime = System.currentTimeMillis();
    if (webpConversionEnabled) {
      tempImageFile = convertToWebPIfNeeded(tempImageFile, clientId, requestId);
    }
    stepEndTime = System.currentTimeMillis();
    log.info("Step 2 - WebP conversion completed in {} ms for requestId {} ",
      stepEndTime - stepStartTime, requestId);

    // Step 3: Parallel image processing
    stepStartTime = System.currentTimeMillis();
    if (xgpImageScaleRequest.isActive()) {
      ImageResultDetail activeImageResponse =
        getImageResultDetail(xgpImageScaleRequest, tempImageFile);
      allImageResponses.add(activeImageResponse);
    } else {
      List<CompletableFuture<ImageResultDetail>> processingFutures = new ArrayList<>();
      final File finalTempImageFile = tempImageFile;

      if (Objects.nonNull(xgpImageScaleRequest.getFullImageUploadRequest())) {
        processingFutures.add(CompletableFuture.supplyAsync(
          () -> processImageTypeOldFlow(finalTempImageFile,
            xgpImageScaleRequest.getFullImageUploadRequest().getImagePath(),
            AvailableSize.FULL.getName()), executorService));
      }

      if (Objects.nonNull(xgpImageScaleRequest.getMediumImageUploadRequest())) {
        processingFutures.add(CompletableFuture.supplyAsync(
          () -> processImageTypeOldFlow(finalTempImageFile,
            xgpImageScaleRequest.getMediumImageUploadRequest().getImagePath(),
            AvailableSize.MEDIUM.getName()), executorService));
      }

      if (Objects.nonNull(xgpImageScaleRequest.getThumbNailImageUploadRequest())) {
        processingFutures.add(CompletableFuture.supplyAsync(
          () -> processImageTypeOldFlow(finalTempImageFile,
            xgpImageScaleRequest.getThumbNailImageUploadRequest().getImagePath(),
            AvailableSize.THUMBNAIL.getName()), executorService));
      }

      CompletableFuture<Void> allProcessing =
        CompletableFuture.allOf(processingFutures.toArray(new CompletableFuture[0]));

      allProcessing.thenRun(() -> {
        for (CompletableFuture<ImageResultDetail> future : processingFutures) {
          try {
            ImageResultDetail result = future.get();
            if (Objects.nonNull(result)) {
              allImageResponses.add(result);
            }
          } catch (Exception e) {
            log.error("Error getting processing result", e);
          }
        }
      }).get();
    }
    stepEndTime = System.currentTimeMillis();
    log.info("Step 3 - Parallel image processing completed in {} ms for requestId {} ",
      stepEndTime - stepStartTime, requestId);

    // Step 4: Parallel GCS uploads
    stepStartTime = System.currentTimeMillis();
    List<CompletableFuture<ImageResultDetail>> uploadFutures = new ArrayList<>();
    for (ImageResultDetail imageResponse : allImageResponses) {
      if (imageResponse.isSuccess()) {
        CompletableFuture<Void> uploadFuture = CompletableFuture.runAsync(() -> {
          try {
            fileStorageService.uploadToGcs(imageResponse, clientId);
            log.debug("Successfully uploaded image: {}", imageResponse.getDestinationPath());
          } catch (Exception e) {
            log.error("Error uploading image to GCS: {}", imageResponse.getDestinationPath(), e);
          }
        }, executorService);
        uploadFutures.add(uploadFuture.thenApply(v -> imageResponse));
      }
    }

    CompletableFuture<Void> allUploads =
      CompletableFuture.allOf(uploadFutures.toArray(new CompletableFuture[0]));
    allUploads.get();

    stepEndTime = System.currentTimeMillis();
    log.info("Step 4 - Parallel GCS uploads completed in {} ms for requestId {} ",
      stepEndTime - stepStartTime, requestId);
  }

  private static ImageResultDetail getImageResultDetail(XgpImageScaleRequest xgpImageScaleRequest,
    File tempImageFile) {
    ImageResultDetail activeImageResponse = new ImageResultDetail();
    activeImageResponse.setSuccess(true);
    activeImageResponse.setImagePathLocation(tempImageFile.getAbsolutePath());
    activeImageResponse.setTempFileLocation(tempImageFile.getAbsolutePath());
    activeImageResponse.setDestinationPath(
      xgpImageScaleRequest.getFullImageUploadRequest().getImagePath());
    activeImageResponse.setResize(false);
    activeImageResponse.setUploadRequired(true);
    activeImageResponse.setEdited(false);
    activeImageResponse.setActive(true);
    return activeImageResponse;
  }

  private Collection<String> downloadImagesFromGcsAndSaveToFileStoreParallel(List<GraphicImageDetail> graphicImageDetails, boolean isResize, boolean isEdited)
      throws InterruptedException, ExecutionException {
    Set<String> distinctImageSourcePath =
        graphicImageDetails.stream().map(GraphicImageDetail::getSourcePath).collect(Collectors.toSet());
    List<ImageDownloadProcess> imageDownloadProcessList = new ArrayList<>();
    Map<String, String> originalAndModifiedSourceImageMap = new HashMap<>();
    for (String sourceImagePath : distinctImageSourcePath) {
      imageDownloadProcessList.add(
          new ImageDownloadProcess(sourceImagePath, sourcePrefixPath, isResize, isEdited, fileStorageService));
    }
    List<Future<ImageDownloadResponse>> ImageDownloadFutureResults =
        executorService.invokeAll(imageDownloadProcessList);
    for (Future<ImageDownloadResponse> imageDownloadResponseFuture : ImageDownloadFutureResults) {
      ImageDownloadResponse imageDownloadResponse = imageDownloadResponseFuture.get();
      originalAndModifiedSourceImageMap.put(imageDownloadResponse.getImageSourcePath(),
          imageDownloadResponse.getImageModifiedSourcePath());
    } for (GraphicImageDetail graphicImageDetail : graphicImageDetails) {
      graphicImageDetail.setSourcePath(originalAndModifiedSourceImageMap.get(graphicImageDetail.getSourcePath()));
    }
    return originalAndModifiedSourceImageMap.values();
  }

  private List<ScaleImageResponse> toScaleImageResponseList(List<ImageResponse> processedImageResponses,
      List<ScaleImageRequest> request, String clientId) {
    List<ScaleImageResponse> scaleImageResponses = new ArrayList<>();
    for (ImageResponse imageResponse : processedImageResponses) {
      ScaleImageResponse scaleImageResponse = new ScaleImageResponse();
      BeanUtils.copyProperties(imageResponse, scaleImageResponse);
      scaleImageResponse.setActive(false);
      scaleImageResponses.add(scaleImageResponse);
      scaleImageResponse.setCommonImage(imageResponse.isCommonImage());
    }
    for (ScaleImageRequest imageRequest : request) {
      ScaleImageResponse scaleImageResponse = new ScaleImageResponse();
      if (imageRequest.isActive()) {
        BeanUtils.copyProperties(imageRequest, scaleImageResponse);
        scaleImageResponse.setSuccess(true);
        scaleImageResponse.setClientId(clientId);
        scaleImageResponses.add(scaleImageResponse);
        scaleImageResponse.setCommonImage(imageRequest.isCommonImage());
      }
    }
    return scaleImageResponses;
  }

  private List<GraphicDetailCommand> downloadFileFromGcsToLocal(
    ResizeImageScalingModel resizeImageScalingModel, List<ImagePathResult> imagePathResult) {
    List<GraphicDetailCommand> graphicDetailCommandList = new ArrayList<>();
    for (ImagePaths filePath : resizeImageScalingModel.getImagePathsList()) {
      try {
        CustomGraphicsSettings customGraphicsSettings =
          GraphicsProcessorHelper.initializeSettingFromString(filePath.getSettings(),
            OBJECT_MAPPER);
        File temporaryFile = new File(
          gcsProperties.getOrderTemporaryImageSourcePath() + File.separator + UUID.randomUUID()
            + Constants.DOT + FilenameUtils.getExtension(filePath.getSourcePath()));
        temporaryFile.getParentFile().mkdirs();
        gcsService.downloadFileTo(orderImageBucket.getName(), filePath.getSourcePath(),
          temporaryFile.getAbsolutePath());
        GraphicDetailCommand graphicDetailCommand =
          new GraphicDetailCommand(null, temporaryFile.getAbsolutePath(), filePath.getFinalPath(),
            customGraphicsSettings, StringUtils.EMPTY, true);
        graphicDetailCommand.setSourcePath(temporaryFile.getAbsolutePath());
        graphicDetailCommand.setSourceGcsPath(filePath.getSourcePath());
        graphicDetailCommandList.add(graphicDetailCommand);
      } catch (Exception ex) {
        LOG.error("Exception in downloading image {} ", filePath, ex);
        imagePathResult.add(
          ImagePathResult.builder().success(false).finalPath(filePath.getSourcePath()).build());
      }
    }
    return graphicDetailCommandList;
  }

  public ScaleImagesResponse populateImageScalingResult(
    ResizeImageScalingModel resizeImageScalingModel, List<ImagePathResult> imagePathResult) {
    ScaleImagesResponse scaleImagesResponse = new ScaleImagesResponse();
    scaleImagesResponse.setClientId(resizeImageScalingModel.getClientId());
    scaleImagesResponse.setUniqueIdentifier(resizeImageScalingModel.getUniqueIdentifier());
    scaleImagesResponse.setStoreId(resizeImageScalingModel.getStoreId());
    scaleImagesResponse.setRequestId(resizeImageScalingModel.getRequestId());
    scaleImagesResponse.setImagePathResult(imagePathResult);
    return scaleImagesResponse;
  }

  public void validateImageListSize(ResizeImageScalingModel resizeImageScalingModel,
    List<ImagePathResult> imagePathResult) {
    List<List<ImagePaths>> batchedList =
      Lists.partition(resizeImageScalingModel.getImagePathsList(), imageScaleLimit);
    if (batchedList.size() > 1) {
      resizeImageScalingModel.setImagePathsList(batchedList.get(0));
      batchedList.stream().skip(1).flatMap(List::stream)
        .map(imagePath -> new ImagePathResult(imagePath.getSourcePath(), false))
        .forEach(imagePathResult::add);
    }
  }



  private File createTempFileFromBytes(byte[] imageBytes, String clientId) throws IOException {
    // Detect actual image format from bytes to avoid format mismatch
    String detectedExtension = getImageExtensionFromBytes(imageBytes);
    String tempFileName = UUID.randomUUID() + Constants.TEMP_IMAGE + detectedExtension;
    Path tempFileDir = Path.of(tempDirForActiveProductImageConversion, clientId);

    Files.createDirectories(tempFileDir);

    Path tempFilePath = tempFileDir.resolve(tempFileName);

    Files.write(tempFilePath, imageBytes, StandardOpenOption.CREATE_NEW);
    
    log.info("Created temp file with correct extension: {} (detected format: {})", 
             tempFilePath.getFileName(), detectedExtension);

    return tempFilePath.toFile();
  }

  public String getImageExtensionFromBytes(byte[] imageBytes) {
    try {
      String mimeType = TIKA.detect(imageBytes);
      log.debug("Detected mime type from bytes: {}", mimeType);
      return getImageExtensionByMimeType(mimeType);
    } catch (Exception e) {
      log.warn("Could not detect image type from bytes, defaulting to .jpg", e);
      return ".jpg";
    }
  }


  /**
   * NEW FLOW: Uses scaleToWebP for optimized single-pass processing
   */
  public ImageResultDetail processImageType(File sourceImageFile, String gcsDestinationPath,
    String imageType) {
    File tempProcessedFile;
    try {
      log.info("Processing {} image. Source: {}, GCS Destination: {}", imageType,
        sourceImageFile.getAbsolutePath(), gcsDestinationPath);

      CustomGraphicsSettings graphicsSettings = createGraphicsSettingsForImageType(imageType);

      // Use proper extension based on WebP conversion setting and source format
      String outputExtension = determineOutputExtension(sourceImageFile);
      String tempProcessedFileName = UUID.randomUUID() + "_processed_" + outputExtension;
      Path tempDirPath = Path.of(tempDirForActiveProductImageConversion);
      Files.createDirectories(tempDirPath);

      Path tempProcessedFilePath = tempDirPath.resolve(tempProcessedFileName);
      tempProcessedFile = tempProcessedFilePath.toFile();

      log.info("Processing with output extension: {} (WebP enabled: {})", outputExtension, webpConversionEnabled);

      ImageResultDetail result =
        service.scaleToWebP(sourceImageFile.getAbsolutePath(), tempProcessedFile.getAbsolutePath(),
          graphicsSettings, StringUtils.EMPTY);

      if (Objects.nonNull(result) && result.isSuccess()) {
        ImageResultDetail imageResponse =
          getImageResultDetail(gcsDestinationPath, tempProcessedFile);
        log.info("Successfully processed {} image: {} -> {}", imageType,
          tempProcessedFile.getAbsolutePath(), gcsDestinationPath);
        return imageResponse;
      }
    } catch (Exception e) {
      log.error("Error processing {} image: {}", imageType, gcsDestinationPath, e);
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED);
    }
    return null;
  }

  /**
   * OLD FLOW: Uses scale() method (source is already WebP from previous conversion step)
   */
  private ImageResultDetail processImageTypeOldFlow(File sourceImageFile, String gcsDestinationPath,
    String imageType) {
    File tempProcessedFile;
    try {
      log.info("Processing {} image (OLD FLOW). Source: {}, GCS Destination: {}", imageType,
        sourceImageFile.getAbsolutePath(), gcsDestinationPath);

      CustomGraphicsSettings graphicsSettings = createGraphicsSettingsForImageType(imageType);

      // Use proper extension based on WebP conversion setting and source format
      String outputExtension = determineOutputExtension(sourceImageFile);
      String tempProcessedFileName = UUID.randomUUID() + "_processed_" + outputExtension;
      Path tempDirPath = Path.of(tempDirForActiveProductImageConversion);
      Files.createDirectories(tempDirPath);

      Path tempProcessedFilePath = tempDirPath.resolve(tempProcessedFileName);
      tempProcessedFile = tempProcessedFilePath.toFile();

      log.info("Processing with output extension: {} (WebP enabled: {})", outputExtension, webpConversionEnabled);

      // Use old scale() method (source is already WebP from convertToWebPIfNeeded step)
      ImageResultDetail result =
        service.scale(sourceImageFile.getAbsolutePath(), tempProcessedFile.getAbsolutePath(),
          graphicsSettings, StringUtils.EMPTY, true, true);

      if (Objects.nonNull(result) && result.isSuccess()) {
        ImageResultDetail imageResponse =
          getImageResultDetail(gcsDestinationPath, tempProcessedFile);
        log.info("Successfully processed {} image: {} -> {}", imageType,
          tempProcessedFile.getAbsolutePath(), gcsDestinationPath);
        return imageResponse;
      }
    } catch (Exception e) {
      log.error("Error processing {} image: {}", imageType, gcsDestinationPath, e);
      throw new ApplicationRuntimeException();
    }
    return null;
  }

  private String determineOutputExtension(File sourceImageFile) {
    try {
      // If WebP conversion is enabled, output should be WebP
      if (webpConversionEnabled) {
        return WEBP_EXTENSION;
      }
      byte[] sourceBytes = Files.readAllBytes(sourceImageFile.toPath());
      return getImageExtensionFromBytes(sourceBytes);
    } catch (Exception e) {
      LOG.warn("Could not determine output extension, defaulting to .jpg", e);
      return ".jpg";
    }
  }

  public String getImageExtensionByMimeType(String mimeType) {
    return switch (mimeType) {
      case "image/png" -> ".png"; // Preserve PNG format to maintain transparency
      case "image/webp" -> WEBP_EXTENSION;
      default -> ".jpg"; // Default to JPEG for best compression
    };
  }

  private static ImageResultDetail getImageResultDetail(String gcsDestinationPath,
    File tempProcessedFile) {
    ImageResultDetail imageResponse = new ImageResultDetail();
    imageResponse.setSuccess(true);
    imageResponse.setDestinationPath(gcsDestinationPath);
    imageResponse.setResize(true);
    imageResponse.setUploadRequired(true);
    imageResponse.setEdited(true);
    imageResponse.setTempFileLocation(tempProcessedFile.getAbsolutePath());
    imageResponse.setImagePathLocation(tempProcessedFile.getAbsolutePath());
    return imageResponse;
  }

  private CustomGraphicsSettings createGraphicsSettingsForImageType(String imageType) {
    AvailableSize availableSize = AvailableSize.valueOf(imageType.toUpperCase());
    int width = 0, height = 0, quality = 0, dpi = 0;
    switch (availableSize) {
      case FULL:
        width = imageConfig.getFullWidth();
        height = imageConfig.getFullHeight();
        quality = imageConfig.getFullQuality();
        dpi = imageConfig.getFullDpi();
        break;
      case MEDIUM:
        width = imageConfig.getMediumWidth();
        height = imageConfig.getMediumHeight();
        quality = imageConfig.getMediumQuality();
        dpi = imageConfig.getMediumDpi();
        break;
      case THUMBNAIL:
        width = imageConfig.getThumbnailWidth();
        height = imageConfig.getThumbnailHeight();
        quality = imageConfig.getThumbnailQuality();
        dpi = imageConfig.getThumbnailDpi();
        break;
    }

    // Create dimension and settings together
    GraphicDimension dimension = new GraphicDimension(width, height);

    return new CustomGraphicsSettings(dpi, quality, dimension);
  }

  private File convertToWebPIfNeeded(File sourceFile, String clientId, String requestId) {
    File webpFile;
    ImageResultDetail result = new ImageResultDetail();
    try {
      IdentifyImageResult imageInfo =
        ((GraphicsProcessorServiceImpl) service).getGraphicsProperty(sourceFile.getAbsolutePath());

      if (imageInfo.isImage() && TargetType.WEBP.name()
        .equalsIgnoreCase(imageInfo.getImageType())) {
        log.info("Image is already WebP format, skipping conversion: {}",
          sourceFile.getAbsolutePath());
        return sourceFile;
      }

      log.info("Converting image to WebP format: {}", sourceFile.getAbsolutePath());

      CustomGraphicsSettings webpSettings = new CustomGraphicsSettings();
      webpSettings.setDpi(imageConfig.getWebpDpi());
      webpSettings.setQuality(imageConfig.getWebpQuality());
      webpSettings.setDimension(new GraphicDimension(imageInfo.getWidth(), imageInfo.getHeight()));

      String webpFileName = UUID.randomUUID() + Constants.TEMP_IMAGE;
      Path outputDir = Path.of(tempDirForActiveProductImageConversion, clientId);
      Files.createDirectories(outputDir);

      Path webpFilePath = outputDir.resolve(webpFileName);
      webpFile = webpFilePath.toFile();

      result =
        service.convert(TargetType.WEBP, sourceFile.getAbsolutePath(), outputDir.toString(),
          webpFileName, webpSettings, StringUtils.EMPTY, clientId, UUID.randomUUID().toString());

      if (Objects.nonNull(result) && result.isSuccess()) {
        log.info("Successfully converted image to WebP: {} -> {} , requestId {} ",
          sourceFile.getAbsolutePath(), webpFile.getAbsolutePath(), requestId);
        cleanupTempFile(sourceFile);
        return new File(result.getImagePathLocation());
      } else {
        log.error("Error converting image to WebP: {}", sourceFile.getAbsolutePath());
        throw new ApplicationRuntimeException();
      }
    } catch (Exception e) {
      log.error("Error converting image to WebP: {}", sourceFile.getAbsolutePath(), e);
      cleanupTempFile(sourceFile);
      throw new ApplicationRuntimeException();
    }
  }

  public void cleanupTempFile(File tempFile) {
    try {
      if (Objects.nonNull(tempFile) && tempFile.exists()) {
        boolean deleted = tempFile.delete();
        LOG.info("Temporary file cleanup: {} - {}", tempFile.getAbsolutePath(), deleted);
      }
    } catch (Exception e) {
      LOG.error("Error cleaning up temporary file: {}", tempFile.getAbsolutePath(), e);
    }
  }

}

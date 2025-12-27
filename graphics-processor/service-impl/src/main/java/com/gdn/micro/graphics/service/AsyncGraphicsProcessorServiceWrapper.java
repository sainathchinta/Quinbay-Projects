package com.gdn.micro.graphics.service;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;

import com.gdn.micro.graphics.config.KafkaPublisher;
import com.gdn.micro.graphics.domain.event.model.ImagePathResult;
import com.gdn.micro.graphics.service.config.GcsProperties;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.task.TaskExecutor;
import org.springframework.stereotype.Component;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.micro.graphics.config.KafkaTopicProperties;
import com.gdn.micro.graphics.domain.event.model.ImageResultDetail;
import com.gdn.micro.graphics.domain.event.model.ImageResultDetailList;
import com.gdn.micro.graphics.model.CustomGraphicsSettings;
import com.gdn.micro.graphics.model.GraphicDetailCommand;
import com.gdn.micro.graphics.model.ImageScalingAndUploadModel;
import com.gdn.micro.graphics.service.enums.TargetType;

@Component
public class AsyncGraphicsProcessorServiceWrapper {

  @Value("${scale.api.max.retries}")
  private Integer totalNoOfTries;

  @Value("${event.based.scaling.enabled}")
  private boolean eventBasedScalingEnabled;

  @Autowired
  private GraphicsProcessorService service;

  @Autowired
  @Qualifier("taskExecutor")
  private TaskExecutor executor;

  private ExecutorService executorService;

  @Autowired
  private KafkaPublisher kafkaPublisher;

  @Autowired
  private FileStorageService fileStorageService;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Autowired
  private GcsProperties gcsProperties;

  @Autowired
  public void setExecutorService(ExecutorService executorService) {
    this.executorService = executorService;
  }

  private static final Logger LOG =
      LoggerFactory.getLogger(AsyncGraphicsProcessorServiceWrapper.class);

  public static Callable<ImageResultDetail> wrap(
      final Map<String, String> context, final GraphicsProcessorService service,
      final GraphicDetailCommand graphicDetailCommand) {
    return new Callable<ImageResultDetail>() {
      @Override
      public ImageResultDetail call() throws Exception {
        Map<String, String> previous = MDC.getCopyOfContextMap();
        if (context == null) {
          MDC.clear();
        } else {
          MDC.setContextMap(context);
        }
        try {
          return service.scale(graphicDetailCommand.getSourcePath(),
              graphicDetailCommand.getDestinationPath(),
              graphicDetailCommand.getCustomGraphicsSettings(),
              graphicDetailCommand.getPrefixPath(), false, null);
        }
        catch (Exception e) {
          LOG.error("can not execute scaling processor for image : {}",graphicDetailCommand.getSourcePath(), e);
          throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED);
        }
        finally {
          if (previous == null) {
            MDC.clear();
          } else {
            MDC.setContextMap(previous);
          }
          try {
            graphicDetailCommand.getTemporaryFile().delete();
          }catch(Exception ex){
            LOG.error(ex.getMessage());

          }
        }
      }
    };
  }

  public static Runnable wrap(final Runnable runnable, final Map<String, String> context) {
    return new Runnable() {
      @Override
      public void run() {
        Map<String, String> previous = MDC.getCopyOfContextMap();
        if (context == null) {
          MDC.clear();
        } else {
          MDC.setContextMap(context);
        }
        try {
          runnable.run();
        } finally {
          if (previous == null) {
            MDC.clear();
          } else {
            MDC.setContextMap(previous);
          }
        }
      }
    };
  }

  public void convert(
      TargetType targetType, final File temporaryFile, final String sourcePath, final String destinationFolder,
      final CustomGraphicsSettings customGraphicsSettings, final String fileNameNoExt,
      final String prefixPath, String clientId, String requestId) throws Exception {
    executorService.execute(wrap(new Runnable() {
      @Override
      public void run() {
        try {
          kafkaPublisher.send(kafkaTopicProperties.getImageResultDetailStatus(), new ImageResultDetailList(Arrays.asList(
              service.convert(targetType, sourcePath, destinationFolder, fileNameNoExt, customGraphicsSettings,
                  prefixPath, clientId, requestId))));
        } catch (Exception e) {
          LOG.error("can not execute scaling processor", e);
          throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED);
        } finally {
          temporaryFile.delete();
        }
      }
    }, MDC.getCopyOfContextMap()));
  }

  public void scale(final File temporaryFile, final String sourcePath, final String destinationPath,
      final CustomGraphicsSettings customGraphicsSettings, final String prefixPath)
          throws Exception {
    executorService.execute(wrap(new Runnable() {
      @Override
      public void run() {
        try {
          kafkaPublisher.send(kafkaTopicProperties.getImageResultDetailStatus(), new ImageResultDetailList(
              Arrays.asList(service.scale(sourcePath, destinationPath, customGraphicsSettings, prefixPath, false, null))));
        } catch (Exception e) {
          LOG.error("can not execute scaling processor", e);
          throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED);
        } finally {
          temporaryFile.delete();
        }
      }
    }, MDC.getCopyOfContextMap()));
  }

  public void scales(List<GraphicDetailCommand> graphicDetailCommands, boolean uploadToGcs,
      String clientId, String requestId) {
    if (eventBasedScalingEnabled) {
      kafkaPublisher.send(kafkaTopicProperties.getImageScalingAndUpload(),
          new ImageScalingAndUploadModel(graphicDetailCommands, clientId, uploadToGcs, requestId));
    } else {
      executorService.execute(
          wrap(() -> scaleImage(graphicDetailCommands, uploadToGcs, clientId, requestId), MDC.getCopyOfContextMap()));
    }
  }

  public void scaleImage(List<GraphicDetailCommand> graphicDetailCommands, boolean uploadToGcs,
      String clientId, String requestId) {
    List<Future<ImageResultDetail>> futureResults = new ArrayList<>();
    for (final GraphicDetailCommand graphicDetailCommand : graphicDetailCommands) {
      futureResults.add(scaleWithRetry(graphicDetailCommand));
    }
    LOG.trace("context : {}", MDC.getCopyOfContextMap());
    try {
      List<ImageResultDetail> resultList = new ArrayList<>();
      for (Future<ImageResultDetail> imageResultDetailFuture : futureResults) {
        ImageResultDetail imageResultDetail = imageResultDetailFuture.get();
        imageResultDetail.setUploadedToGCs(uploadToGcs);
        imageResultDetail.setClientId(clientId);
        imageResultDetail.setRequestId(requestId);
        resultList.add(imageResultDetail);
      }
      fileStorageService.uploadToAndDeleteFromTempLocationGcs(uploadToGcs, clientId, graphicDetailCommands, resultList);
      kafkaPublisher.send(kafkaTopicProperties.getImageResultDetailStatus(), new ImageResultDetailList(resultList));
    } catch (Exception e) {
      LOG.error("can not get result for async process", e);
    }
  }

  private Future<ImageResultDetail> scaleWithRetry(GraphicDetailCommand graphicDetailCommand) {
    Future<ImageResultDetail> imageResultDetailFuture;
    Integer retryCounter = 0;
    try {
      do {
        imageResultDetailFuture = executorService.submit(wrap(MDC.getCopyOfContextMap(), service, graphicDetailCommand));
        retryCounter++;
      } while (retryCounter < totalNoOfTries && !imageResultDetailFuture.get().isSuccess());
    } catch (Exception e) {
      LOG.error("Error while image scaling ", e);
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, "Failed scaling with retry");
    }
    return imageResultDetailFuture;
  }

  public void scaleImage(List<GraphicDetailCommand> graphicDetailCommands, boolean uploadToGcs,
    String clientId, List<ImagePathResult> imagePathResult) {
    List<Future<ImageResultDetail>> futureResults = new ArrayList<>();
    for (final GraphicDetailCommand graphicDetailCommand : graphicDetailCommands) {
      futureResults.add(scaleWithRetryForOrder(graphicDetailCommand, imagePathResult));
    }
    LOG.trace("context : {}", MDC.getCopyOfContextMap());
    List<ImageResultDetail> resultList = new ArrayList<>();
    for (Future<ImageResultDetail> imageResultDetailFuture : futureResults) {
      try {
        ImageResultDetail imageResultDetail = imageResultDetailFuture.get();
        imageResultDetail.setUploadedToGCs(uploadToGcs);
        resultList.add(imageResultDetail);
        imagePathResult.add(
          ImagePathResult.builder().finalPath(imageResultDetail.getImagePathLocation()).success(true)
            .build());
      } catch (Exception e) {
        LOG.error("Failed to get result for a future: ", e);
      }
    }
    try {
      fileStorageService.uploadToAndDeleteFromTempLocationGcs(uploadToGcs, clientId,
        graphicDetailCommands, resultList);
    } catch (Exception e) {
      LOG.error("Failed to upload and delete from temp location: ", e);
    }
  }
  private Future<ImageResultDetail> scaleWithRetryForOrder(
    GraphicDetailCommand graphicDetailCommand, List<ImagePathResult> imagePathResult) {
    Future<ImageResultDetail> imageResultDetailFuture = null;
    Integer retryCounter = 0;
    try {
      do {
        imageResultDetailFuture =
          executorService.submit(wrap(MDC.getCopyOfContextMap(), service, graphicDetailCommand));
        retryCounter++;
      } while (retryCounter < totalNoOfTries && !imageResultDetailFuture.get().isSuccess());
    } catch (Exception e) {
      LOG.error("Error while image scaling ", e);
      imagePathResult.add(
        ImagePathResult.builder().success(false).finalPath(graphicDetailCommand.getSourceGcsPath())
          .build());
    }
    return imageResultDetailFuture;
  }

}

package com.gdn.micro.graphics.service;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Objects;
import java.util.UUID;

import com.gdn.micro.graphics.model.GraphicDetailCommand;
import com.gdn.micro.graphics.service.config.GcsProperties;
import com.gdn.micro.graphics.util.Constants;
import com.gdn.micro.graphics.web.helper.ConvertImageResponse;
import com.google.cloud.storage.Blob;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.gm4java.engine.GMService;
import org.gm4java.engine.support.GMConnectionPoolConfig;
import org.gm4java.engine.support.PooledGMService;
import org.gm4java.im4java.GMBatchCommand;
import org.gm4java.im4java.GMOperation;
import org.im4java.core.IM4JavaException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.util.GdnMandatoryRequestParameterUtil;
import com.gdn.micro.graphics.domain.event.model.ImageResultDetail;
import com.gdn.micro.graphics.model.ChmodTarget;
import com.gdn.micro.graphics.model.CustomGraphicsSettings;
import com.gdn.micro.graphics.model.GraphicDimension;
import com.gdn.micro.graphics.model.IdentifyImageResult;
import com.gdn.micro.graphics.service.enums.TargetType;
import com.gdn.micro.graphics.util.AvailableCommand;
import com.gdn.micro.graphics.utils.GraphicsProcessorHelper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.multipart.MultipartFile;

/**
 * Created by Yudhi K. Surtan on 11/28/2015.
 */
@Slf4j
@Service
public class GraphicsProcessorServiceImpl implements GraphicsProcessorService {

  private static final int CHMOD_644 = 644;
  private static final String PNG = "PNG";
  private static final String WEBP = "WEBP";
  private static final String SLASH = "/";
  private static final String RESIZE = "resize";
  private static final String SCALE = "scale";
  private static final String SEOUL = "seoul";
  private static final String PERMITTED_CHARS = "[^a-zA-Z0-9.-]";

  private static final Logger LOG = LoggerFactory.getLogger(GraphicsProcessorServiceImpl.class);

  private final GMService service;

  @Value("${max.image.size.allowed}")
  private int maxImageSize;

  @Value("${event.based.scaling.enabled}")
  private boolean eventBasedScalingEnabled;

  @Autowired
  private FileStorageService fileStorageService;

  @Autowired
  private ImagePathConfiguration imagePathConfiguration;

  @Autowired
  private GcsProperties gcsProperties;

  public GraphicsProcessorServiceImpl(GMConnectionPoolConfig config) {
    this.service = new PooledGMService(config);
  }

  @Override
  // @PublishDomainEvent(domainEventName = DomainEventName.GRAPHIC_DETAIL_STATUS_EVENT_NAME,
  // publishEventClass = ImageResultDetail.class)
  public ImageResultDetail convert(
      TargetType targetType, String sourcePath, String destinationFolder, String fileNameNoExt,
      CustomGraphicsSettings customGraphicsSettings, String prefixPath, String clientId,
      String requestId) throws Exception {
    StringBuilder destinationPathBuilder = new StringBuilder().append(destinationFolder)
        .append(File.separator).append(fileNameNoExt).append(".").append(targetType);
    LOG.trace("[{}] source path : {}, destinationPath : {}, graphic setting : {}, prefix path : ",
        new Object[] {targetType, sourcePath, destinationPathBuilder.toString(),
            customGraphicsSettings, prefixPath});
    IdentifyImageResult result = getGraphicsProperty(sourcePath);
    ImageResultDetail resultDetail = new ImageResultDetail();
    try {
      if (result.isImage()) {
        GMBatchCommand command = new GMBatchCommand(service, AvailableCommand.CONVERT.getName());
        GMOperation operation = new GMOperation();
        operation.addImage(new File(sourcePath));
        if (result.isContainResolution()) {
          operation.resample(customGraphicsSettings.getDpi());
        }
        double quality = customGraphicsSettings.getQuality();
        if (result.getQuality() < customGraphicsSettings.getQuality()) {
          quality = result.getQuality();
        }
        operation.quality(getTransformedQuality(targetType, quality));

        operation.stripProfiles();
        operation.interlace("Line");
        Collection<GMOperation.GeometryAnnotation> annotations = new ArrayList<>();
        annotations.add(GMOperation.GeometryAnnotation.FillUsingAspectRatio);
        operation.scale(customGraphicsSettings.getWidth(), customGraphicsSettings.getHeight(),
            annotations);
        GraphicsProcessorHelper.createDestinationDirectory(destinationPathBuilder.toString());
        operation.addImage(destinationPathBuilder.toString());
        command.run(operation);
        resultDetail =
            getSuccessResult(destinationPathBuilder.toString(), prefixPath, clientId, requestId);
      } else {
        // just skip this file and delete the temp file
        // new File(sourcePath).delete();
        resultDetail.setErrorMessage(
            String.format("[%s] Not suitable image format (only use JPEG/JPG or PNG)", targetType));
      }
    } catch (IOException | InterruptedException | IM4JavaException e) {
      LOG.error("something bad happened when scaling image", e);
      resultDetail.setErrorMessage(e.getMessage());
    }
    return resultDetail;
  }

  @Override
  public IdentifyImageResult getGraphicsProperty(InputStream input, String clientId) throws Exception {
    File temporaryFile = null;
    try {
      temporaryFile = fileStorageService.createTemporaryFile(input, "tmp2", clientId);
      return getGraphicsProperty(temporaryFile.getAbsolutePath());
    } catch (Exception e) {
      LOG.error("something bad happened", e);
      throw new ApplicationException(ErrorCategory.UNSPECIFIED);
    } finally {
      if (temporaryFile != null) {
        temporaryFile.delete();
      }
    }
  }

  protected IdentifyImageResult getGraphicsProperty(String path) throws Exception {
    GMBatchCommand command = new GMBatchCommand(service, AvailableCommand.IDENTIFY.getName());
    IdentifyImageResult identifyImageResult = new IdentifyImageResult();
    command.setOutputConsumer(identifyImageResult);
    EnhancedGMOperation operation = new EnhancedGMOperation();
    operation.format("%m,%Q,%w,%h");
    operation.addImage(new File(path));
    command.run(operation);
    return identifyImageResult;
  }

  private String getNewFileExt(String fileName, String newExt) {
    int dotIndex = fileName.lastIndexOf(".");
    String newFileName =
        (dotIndex > -1 ? fileName.substring(0, dotIndex + 1) : fileName + ".") + newExt;
    return newFileName;
  }

  private ImageResultDetail getSuccessResult(String destinationPath, String prefixPath) {
    ImageResultDetail resultDetail = new ImageResultDetail();
    resultDetail.setSuccess(true);
    resultDetail.setClientId(GdnMandatoryRequestParameterUtil.getClientId());
    resultDetail.setRequestId(GdnMandatoryRequestParameterUtil.getRequestId());
    resultDetail.setImagePathLocation(
        destinationPath.substring(prefixPath.length(), destinationPath.length()));
    return resultDetail;
  }

  private ImageResultDetail getSuccessResult(String destinationPath, String prefixPath, String clientId, String requestId) {
    ImageResultDetail resultDetail = new ImageResultDetail();
    resultDetail.setSuccess(true);
    resultDetail.setClientId(clientId);
    resultDetail.setRequestId(requestId);
    resultDetail.setImagePathLocation(
        destinationPath.substring(prefixPath.length(), destinationPath.length()));
    return resultDetail;
  }

  // currently.. we need to hard code it..
  private double getTransformedQuality(TargetType targetType,
      double origQuality) {
    double quality = origQuality;
    switch (targetType) {
      case BPG:
        quality = 32 + ((int) (GraphicsProcessorHelper.DEFAULT_QUALITY + 5 - origQuality) / 2);
        quality = quality > 51 ? 51 : quality < 0 ? 0 : quality;

        LOG.error("quality: " + quality);
        // System.out.println("quality: " + quality);
        break;
      case WEBP:
        // For WebP, use the original quality as WebP supports quality 0-100
        quality = origQuality;
        break;
    }
    return quality;
  }

  @Override
  public boolean isGraphics(String path) throws Exception {
    return getGraphicsProperty(path).isImage();
  }

  private ImagePathConfiguration getImagePathConfiguration() {
    return imagePathConfiguration;
  }

  @Override
  // @PublishDomainEvent(domainEventName = DomainEventName. GRAPHIC_DETAIL_STATUS_EVENT_NAME,
  // publishEventClass = ImageResultDetail.class)
  public ImageResultDetail scale(String sourcePath, String destinationPath,
      CustomGraphicsSettings customGraphicsSettings, String prefixPath, boolean resize, Boolean isEdited) throws Exception {
    GraphicsProcessorServiceImpl.LOG.info(
        "source path : {}, destinationPath : {}, graphic setting : {}, prefix path : ",
        new Object[] {sourcePath, destinationPath, customGraphicsSettings, prefixPath});
    GraphicsProcessorServiceImpl.LOG.info("delegation value => client id : {},  request id : {}",
        new Object[] {GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId()});

    IdentifyImageResult result = this.getGraphicsProperty(sourcePath);
    ImageResultDetail resultDetail = new ImageResultDetail();
    try {
      if (result.isImage()) {
        GMBatchCommand command =
            new GMBatchCommand(this.service, AvailableCommand.CONVERT.getName());
        command.setOutputConsumer(
            new ChmodTarget(destinationPath, GraphicsProcessorServiceImpl.CHMOD_644));
        GMOperation operation = new GMOperation();
        operation.addImage(new File(sourcePath));
        File file;
        if (resize) {
          do {
            LOG.debug("Resize image with quality :{}, filename :{} ", customGraphicsSettings.getQuality(), sourcePath);
            scaleImage(destinationPath, customGraphicsSettings, result, command, operation);
            file = new File(destinationPath);
            if (customGraphicsSettings.getQuality() > 10) {
              customGraphicsSettings.setQuality(((int) Math.round(customGraphicsSettings.getQuality())) - 10);
            } else if (customGraphicsSettings.getQuality() <= 1) {
              break;
            } else {
              customGraphicsSettings.setQuality(((int) Math.round(customGraphicsSettings.getQuality())) - 1);
            }
          } while (file.length() > maxImageSize);
        } else {
          LOG.debug("Scale image with quality :{}, filename :{} ", customGraphicsSettings.getQuality(), sourcePath);
          scaleImage(destinationPath, customGraphicsSettings, result, command, operation);
        }
        resultDetail = getSuccessResult(destinationPath, prefixPath);

        if (destinationPath.contains(getImagePathConfiguration().getLocationPrefix(RESIZE)) || destinationPath.contains(
            getImagePathConfiguration().getLocationPrefix(SCALE)) || destinationPath.contains(
            getImagePathConfiguration().getLocationPrefix(SEOUL))) {
          String productCodeFileName = getProductCodeFileName(destinationPath);
          resultDetail.setDestinationPath(destinationPath);
          resultDetail.setProductCodeFileName(productCodeFileName);
          resultDetail.setResize(resize);
          resultDetail.setEdited(isEdited);
          resultDetail.setUploadRequired(true);
        }
      } else {
        // just skip this file and delete the temp file
        // new File(sourcePath).delete();
        LOG.warn("Not suitable image format (only use JPEG/JPG or PNG) : {}",sourcePath);
        resultDetail.setErrorMessage("Not suitable image format (only use JPEG/JPG or PNG)");
      }
    } catch (IOException | InterruptedException | IM4JavaException e) {
      LOG.error("something bad happened when scaling image {}",sourcePath, e);
      resultDetail.setErrorMessage(e.getMessage());
    }
    return resultDetail;
  }

  public static String getProductCodeFileName(String sourcePath) {
    String temp = sourcePath.replaceAll("resize","").replaceAll("//", "/");
    String pathWithoutName = temp.substring(0, temp.lastIndexOf(SLASH));
    String productCode = pathWithoutName.substring(pathWithoutName.lastIndexOf(SLASH) + 1);
    String fileName = temp.substring(temp.lastIndexOf(SLASH));
    return productCode + fileName;
  }

  private void scaleImage(String destinationPath, CustomGraphicsSettings customGraphicsSettings,
      IdentifyImageResult result, GMBatchCommand command, GMOperation operation) throws Exception {
    if (result.isContainResolution()) {
      operation.resample(customGraphicsSettings.getDpi());
    }
    if (result.getQuality() > customGraphicsSettings.getQuality()) {
      operation.quality(customGraphicsSettings.getQuality());
    } else {
      operation.quality(result.getQuality());
    }
    operation.stripProfiles();
    operation.addRawArg("-auto-orient");
    operation.interlace("Line");
    if (PNG.equalsIgnoreCase(result.getImageType())) {
      operation.flatten();
    }
    Collection<GMOperation.GeometryAnnotation> annotations = new ArrayList<>();
    annotations.add(GMOperation.GeometryAnnotation.FillUsingAspectRatio);
    operation.scale(customGraphicsSettings.getWidth(), customGraphicsSettings.getHeight(),
        annotations);
    GraphicsProcessorHelper.createDestinationDirectory(destinationPath);
    operation.addImage(destinationPath);
    command.run(operation);
  }

  @Override
  public ImageResultDetail scaleFull(String sourcePath, String destinationPath, String prefixPath)
      throws Exception {
    CustomGraphicsSettings settings = new CustomGraphicsSettings(
        GraphicsProcessorHelper.DEFAULT_DPI, GraphicsProcessorHelper.DEFAULT_QUALITY,
        new GraphicDimension(GraphicsProcessorHelper.DEFAULT_FULL_SIZE,
            GraphicsProcessorHelper.DEFAULT_FULL_SIZE));
    return scale(sourcePath, destinationPath, settings, prefixPath, false, null);
  }

  @Override
  public ImageResultDetail scaleMedium(String sourcePath, String destinationPath, String prefixPath)
      throws Exception {
    CustomGraphicsSettings settings = new CustomGraphicsSettings(
        GraphicsProcessorHelper.DEFAULT_DPI, GraphicsProcessorHelper.DEFAULT_QUALITY,
        new GraphicDimension(GraphicsProcessorHelper.DEFAULT_MEDIUM_SIZE,
            GraphicsProcessorHelper.DEFAULT_MEDIUM_SIZE));
    return scale(sourcePath, destinationPath, settings, prefixPath, false, null);
  }

  @Override
  public ImageResultDetail scaleThumbnail(String sourcePath, String destinationPath,
      String prefixPath) throws Exception {
    CustomGraphicsSettings settings = new CustomGraphicsSettings(
        GraphicsProcessorHelper.DEFAULT_DPI, GraphicsProcessorHelper.DEFAULT_QUALITY,
        new GraphicDimension(GraphicsProcessorHelper.DEFAULT_THUMBNAIL_SIZE,
            GraphicsProcessorHelper.DEFAULT_THUMBNAIL_SIZE));
    return scale(sourcePath, destinationPath, settings, prefixPath, false, null);
  }

  @Override
  public ImageResultDetail scaleToWebP(String sourcePath, String destinationPath,
      CustomGraphicsSettings customGraphicsSettings, String prefixPath) throws Exception {
    LOG.info("scaleToWebP: source path : {}, destinationPath : {}, graphic setting : {}",
        sourcePath, destinationPath, customGraphicsSettings);

    IdentifyImageResult result = this.getGraphicsProperty(sourcePath);
    ImageResultDetail resultDetail = new ImageResultDetail();
    try {
      if (result.isImage()) {
        GMBatchCommand command =
            new GMBatchCommand(this.service, AvailableCommand.CONVERT.getName());
        command.setOutputConsumer(
            new ChmodTarget(destinationPath, GraphicsProcessorServiceImpl.CHMOD_644));
        GMOperation operation = new GMOperation();
        operation.addImage(new File(sourcePath));

        // Apply graphics settings
        if (result.isContainResolution()) {
          operation.resample(customGraphicsSettings.getDpi());
        }
        if (result.getQuality() > customGraphicsSettings.getQuality()) {
          operation.quality(customGraphicsSettings.getQuality());
        } else {
          operation.quality(result.getQuality());
        }

        operation.stripProfiles(); // Remove EXIF/metadata profiles to reduce file size
        operation.addRawArg("-auto-orient"); // Auto-rotate image based on EXIF orientation tag
        operation.interlace("Line"); // Enable progressive encoding for faster perceived loading

        // Scale operation
        Collection<GMOperation.GeometryAnnotation> annotations = new ArrayList<>();
        annotations.add(GMOperation.GeometryAnnotation.FillUsingAspectRatio);
        operation.scale(customGraphicsSettings.getWidth(), customGraphicsSettings.getHeight(),
            annotations);

        GraphicsProcessorHelper.createDestinationDirectory(destinationPath);
        operation.addImage(destinationPath);
        command.run(operation);

        resultDetail = getSuccessResult(destinationPath, StringUtils.EMPTY);
        resultDetail.setResize(true);
        resultDetail.setUploadRequired(true);
      } else {
        LOG.warn("Not suitable image format for scaleToWebP: {}", sourcePath);
        resultDetail.setErrorMessage("Not suitable image format");
      }
    } catch (IOException | InterruptedException | IM4JavaException e) {
      LOG.error("Error in scaleToWebP for image {}", sourcePath, e);
      resultDetail.setErrorMessage(e.getMessage());
    }
    return resultDetail;
  }

  @Override
  // @PublishDomainEvent(domainEventName = DomainEventName.GRAPHIC_DETAIL_STATUS_EVENT_NAME,
  // publishEventClass = ImageResultDetail.class)
  public ImageResultDetail store(InputStream input, String destinationPath, String prefixPath, String clientId, boolean uploadToGcs)
      throws Exception {
    File temporaryFile = null;
    try {
      temporaryFile = fileStorageService.createTemporaryFile(input, "tmp2", clientId);
      return store(temporaryFile.getAbsolutePath(), destinationPath, prefixPath, clientId, uploadToGcs);
    } catch (Exception e) {
      LOG.error("something bad happened", e);
      throw new ApplicationException(ErrorCategory.UNSPECIFIED);
    } finally {
      if (temporaryFile != null) {
        temporaryFile.delete();
      }
    }
  }

  protected ImageResultDetail store(String sourcePath, String destinationPath, String prefixPath, String clientId, boolean uploadToGcs)
      throws Exception {
    IdentifyImageResult result = getGraphicsProperty(sourcePath);
    ImageResultDetail resultDetail = new ImageResultDetail();
    try {
      if (result.isImage()) {
        GraphicsProcessorHelper.createDestinationDirectory(destinationPath);
        stripMetadataAndAddInterlance(sourcePath, destinationPath, prefixPath);
        resultDetail = getSuccessResult(destinationPath, prefixPath);
        if (uploadToGcs) {
          resultDetail.setUploadedToGCs(true);
          fileStorageService.uploadToGcs(resultDetail, clientId);
          fileStorageService.deleteFromTemporaryLocation(resultDetail.getImagePathLocation(), clientId);
        }
      }
    } catch (IOException | InterruptedException | IM4JavaException e) {
      LOG.error("something bad happened when scaling image", e);
      resultDetail.setErrorMessage(e.getMessage());
    }
    return resultDetail;
  }


  @Override
  public ImageResultDetail stripMetadataAndAddInterlance(String sourcePath, String destinationPath,
      CustomGraphicsSettings customGraphicsSettings, String prefixPath) throws Exception {
    ImageResultDetail resultDetail = new ImageResultDetail();
    IdentifyImageResult result = getGraphicsProperty(sourcePath);
    if (result.isImage()) {
      GMBatchCommand command = new GMBatchCommand(service, AvailableCommand.CONVERT.getName());
      GMOperation operation = new GMOperation();
      operation.addImage(new File(sourcePath));
      if (result.isContainResolution()) {
        operation.resample(customGraphicsSettings.getDpi());
      }
      if (result.getQuality() > customGraphicsSettings.getQuality()) {
        operation.quality(customGraphicsSettings.getQuality());
      } else {
        operation.quality(result.getQuality());
      }
      operation.stripProfiles();
      operation.interlace("Line");
      operation.addImage(new File(destinationPath));
      command.run(operation);
      resultDetail = getSuccessResult(destinationPath, prefixPath);
    }
    return resultDetail;
  }

  @Override
  public ImageResultDetail stripMetadataAndAddInterlance(String sourcePath, String destinationPath,
      String prefixPath) throws Exception {
    return stripMetadataAndAddInterlance(sourcePath, destinationPath, new CustomGraphicsSettings(),
        prefixPath);
  }

  @Override
  public void uploadFileAndGenerateCommandRequestForScaling(List<GraphicDetailCommand> commands,
      List<ConvertImageResponse> responses, List<CustomGraphicsSettings> customGraphicsSettings,
      String imageName, MultipartFile file, String clientId, String groupCode, boolean uploadToGcs)
      throws Exception {
    for (CustomGraphicsSettings graphicSettings : customGraphicsSettings) {
      // we better provide extension on the temp file for better file type recognition,
      // especially on delegate
      File temporaryFile = null;
      Blob blob = null;
      if (eventBasedScalingEnabled) {
        blob = fileStorageService.createTemporaryFileInGCSRmaScaling(file.getBytes(),
            gcsProperties.getRmaTemporaryImageSourcePath() + Constants.SLASH + imageName
                + Constants.UNDERSCORE + UUID.randomUUID() + Constants.DOT
                + FilenameUtils.getExtension(file.getName()));
      } else {
        temporaryFile = fileStorageService.createTemporaryFile(file.getInputStream(),
            FilenameUtils.getExtension(file.getOriginalFilename()), clientId);
      }
      String prefix = fileStorageService.getImageLocationPathPrefix(clientId);
      StringBuilder newImageName =
          new StringBuilder(imageName.replaceAll(PERMITTED_CHARS, Constants.UNDERSCORE)).append(
              Constants.JPG_EXTENSION);
      String generatedImageLocation =
          GraphicsProcessorHelper.getFileSystemPath(groupCode, newImageName.toString(),
              GraphicsProcessorHelper.getSize(graphicSettings), prefix);
      responses.add(new ConvertImageResponse(generatedImageLocation.substring(prefix.length()),
          newImageName.toString(), graphicSettings.getWidth(), graphicSettings.getHeight(),
          uploadToGcs));
      if (eventBasedScalingEnabled) {
        if (Objects.nonNull(blob)) {
          commands.add(
              new GraphicDetailCommand(temporaryFile, blob.getName(), generatedImageLocation,
                  graphicSettings, prefix, uploadToGcs));
        } else {
          log.error("Failed to upload temporary file to GCS for RMA scaling");
        }
      } else {
        commands.add(new GraphicDetailCommand(temporaryFile, temporaryFile.getAbsolutePath(),
            generatedImageLocation, graphicSettings, prefix, uploadToGcs));
      }
    }
  }
}

package com.gdn.micro.graphics.service;

import java.io.InputStream;
import java.util.List;

import com.gdn.micro.graphics.domain.event.model.ImageResultDetail;
import com.gdn.micro.graphics.model.CustomGraphicsSettings;
import com.gdn.micro.graphics.model.GraphicDetailCommand;
import com.gdn.micro.graphics.model.IdentifyImageResult;
import com.gdn.micro.graphics.service.enums.TargetType;
import com.gdn.micro.graphics.web.helper.ConvertImageResponse;
import org.springframework.web.multipart.MultipartFile;

/**
 * Created by Yudhi K. Surtan on 11/28/2015.
 */
public interface GraphicsProcessorService {

  ImageResultDetail convert(
      TargetType targetType, String sourcePath, String destinationFolder, String fileNameNoExt,
      CustomGraphicsSettings customGraphicsSettings, String prefixPath, String clientId,
      String requestId) throws Exception;

  IdentifyImageResult getGraphicsProperty(InputStream input, String clientId) throws Exception;

  boolean isGraphics(String path) throws Exception;

  ImageResultDetail scale(String sourcePath, String destinationPath, CustomGraphicsSettings customGraphicsSettings,
      String prefixPath, boolean resize, Boolean isEdited) throws Exception;

  ImageResultDetail scaleFull(String sourcePath, String destinationPath, String prefixPath)
      throws Exception;

  ImageResultDetail scaleMedium(String sourcePath, String destinationPath, String prefixPath)
      throws Exception;

  ImageResultDetail scaleThumbnail(String sourcePath, String destinationPath, String prefixPath)
      throws Exception;

  /**
   * Optimized scale method that converts to WebP during scaling in a single operation.
   * No intermediate conversion step - GraphicsMagick handles format conversion automatically.
   *
   * @param sourcePath             Source image path (any format)
   * @param destinationPath        Destination path (must end with .webp extension)
   * @param customGraphicsSettings Graphics settings for scaling
   * @return ImageResultDetail with processing result
   * @throws Exception if processing fails
   */
  ImageResultDetail scaleToWebP(String sourcePath, String destinationPath,
      CustomGraphicsSettings customGraphicsSettings, String prefixPath) throws Exception;

  ImageResultDetail store(InputStream input, String destinationPath, String prefixPath, String clientId, boolean uploadToGcs)
      throws Exception;

  ImageResultDetail stripMetadataAndAddInterlance(String sourcePath, String destinationPath,
      CustomGraphicsSettings customGraphicsSettings, String prefixPath) throws Exception;

  ImageResultDetail stripMetadataAndAddInterlance(String sourcePath, String destinationPath,
      String prefixPath) throws Exception;

  /**
   * upload the file to gcs or disk based on switch and generate request for rma-image scaling
   * @param commands
   * @param responses
   * @param customGraphicsSettings
   * @param imageName
   * @param file
   * @param clientId
   * @param groupCode
   * @throws Exception
   */
  void uploadFileAndGenerateCommandRequestForScaling(List<GraphicDetailCommand> commands,
      List<ConvertImageResponse> responses, List<CustomGraphicsSettings> customGraphicsSettings,
      String imageName, MultipartFile file, String clientId, String groupCode, boolean uploadToGcs) throws Exception;
}

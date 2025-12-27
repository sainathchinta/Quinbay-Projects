package com.gdn.micro.graphics.controller;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import com.gdn.micro.graphics.web.model.XgpImageScaleRequest;
import jakarta.servlet.http.HttpServletResponse;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.base.mapper.impl.OrikaMapper;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.micro.graphics.domain.event.model.BulkImageProcessResponse;
import com.gdn.micro.graphics.domain.event.model.ImageResultDetail;
import com.gdn.micro.graphics.model.CustomGraphicsSettings;
import com.gdn.micro.graphics.model.GraphicDetailCommand;
import com.gdn.micro.graphics.model.GraphicImageDetail;
import com.gdn.micro.graphics.model.IdentifyImageResult;
import com.gdn.micro.graphics.service.AsyncGraphicsProcessService;
import com.gdn.micro.graphics.service.AsyncGraphicsProcessorServiceWrapper;
import com.gdn.micro.graphics.service.FileStorageService;
import com.gdn.micro.graphics.service.GraphicsProcessorService;
import com.gdn.micro.graphics.service.ImagePathConfiguration;
import com.gdn.micro.graphics.service.enums.TargetType;
import com.gdn.micro.graphics.utils.ConverterUtil;
import com.gdn.micro.graphics.utils.GraphicsProcessorHelper;
import com.gdn.micro.graphics.web.helper.ApiPath;
import com.gdn.micro.graphics.web.helper.ConvertImageResponse;
import com.gdn.micro.graphics.web.helper.ImageResultByTypeResponse;
import com.gdn.micro.graphics.web.helper.ValidationKeyEncryptor;
import com.gdn.micro.graphics.web.model.BaseResponse;
import com.gdn.micro.graphics.web.model.BulkImagesProcessRequest;
import com.gdn.micro.graphics.web.model.BulkResizeImageRequest;
import com.gdn.micro.graphics.web.model.IdentifyImageResponse;
import com.gdn.micro.graphics.web.model.ImageRequest;
import com.gdn.micro.graphics.web.model.RemoveImageRequest;
import com.gdn.micro.graphics.web.model.ScaleEditedImageRequest;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;


/**
 * Created by Yudhi K. Surtan on 11/29/2015.
 */
@RestController
@Tag(name = "GraphicsOperationController", description = "Graphics Operation Controller API")
public class GraphicsOperationController {

  private static final String CONTENT_DISPOSITION_HEADER_KEY = "Content-Disposition";
  private static final String APPLICATION_OCTET_STREAM_MEDIA_TYPE = "application/octet-stream";
  private static final String APPLICATION_JSON_MEDIA_TYPE = "application/json";
  private static final String JPG_EXTENSION = ".jpg";
  private static final String WEBP_EXTENSION = ".webp";
  private static final String UNDERSCORE = "_";
  private static final String PERMITTED_CHARS_EXCLUDING_DOT = "[^a-zA-Z0-9-]";
  private static final Logger LOG = LoggerFactory.getLogger(GraphicsOperationController.class);
  private static final ObjectMapper OBJECT_MAPPER = new ObjectMapper();
  private static final String IMAGE_NOT_FOUND = "image not found";
  public static final String REGEX_FOR_SPLIT_ON_LAST_DOT = "\\.(?=[^.]*$)";

  @Autowired
  private GraphicsProcessorService service;

  @Autowired
  private AsyncGraphicsProcessorServiceWrapper asyncService;

  @Autowired
  private ImagePathConfiguration imagePathConfiguration;

  @Autowired
  private AsyncGraphicsProcessService asyncGraphicsProcessService;

  @Autowired
  private FileStorageService fileStorageService;

  @Autowired
  private OrikaMapper orikaMapper;

  @Value("${resize}")
  private String sourcePath;

  @Value("${webp.conversion.enabled}")
  private boolean webpConversionEnabled;

  // multiple target type conversion
  @RequestMapping(value = ApiPath.CONVERT_OPERATION_PATH,
      consumes = {MediaType.MULTIPART_FORM_DATA_VALUE},
      produces = {MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE},
      method = {RequestMethod.POST})
  @Operation
  public GdnRestSingleResponse<ImageResultByTypeResponse> convert(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam("imageName") String imageName,
      @RequestParam("settings") String settings, @RequestParam("content") MultipartFile file,
      @RequestParam("targetTypeList") String targetTypeList) throws Exception {
    isContentPresent(file);
    ImageResultByTypeResponse resultByTypeResponse = new ImageResultByTypeResponse();

    try {
      List<CustomGraphicsSettings> customGraphicsSettings =
          GraphicsProcessorHelper.initializeSettingsFromString(settings, OBJECT_MAPPER);
      for (CustomGraphicsSettings graphicSettings : customGraphicsSettings) {

        File temporaryFile = GraphicsProcessorHelper.generateTemporaryFileFromStream(
            file.getInputStream(), FilenameUtils.getExtension(file.getOriginalFilename()));
        String prefix = getImagePathConfiguration().getLocationPrefix(clientId);

        // add image name with uuid prefix
        String newImageName = UUID.randomUUID().toString() + UNDERSCORE + imageName;
        String newImageNameNoExt = FilenameUtils.removeExtension(newImageName);
        String generatedImageLocation =
            GraphicsProcessorHelper.getFileSystemPath(FilenameUtils.removeExtension(imageName),
                GraphicsProcessorHelper.getSize(graphicSettings), prefix);

        // split targetTypeList string by ","
        List<TargetType> listTarget = new ArrayList<>();
        listTarget.add(TargetType.JPG);
        for (String targetType : targetTypeList.toUpperCase().split(",")) {
          if (targetType.isEmpty())
            continue;
          targetType = targetType.trim();
          if (TargetType.JPG.toString().equalsIgnoreCase(targetType))
            continue;
          listTarget.add(TargetType.valueOf(targetType));
        }

        for (TargetType targetType : listTarget) {
          getAsyncService().convert(targetType, temporaryFile, temporaryFile.getAbsolutePath(),
              FilenameUtils.getFullPathNoEndSeparator(generatedImageLocation), graphicSettings,
              newImageNameNoExt, prefix, clientId, requestId);
        }


        for (TargetType type : listTarget)
          resultByTypeResponse.addImage(type.toString(),
              new ConvertImageResponse(
                  generatedImageLocation.substring(prefix.length(), generatedImageLocation.length())
                  + newImageNameNoExt + "." + type,
                  newImageNameNoExt + "." + type, graphicSettings.getWidth(),
                  graphicSettings.getHeight()));
      }
    } catch (Exception e) {
      LOG.error(e.getMessage(), e);
      throw new ApplicationException(ErrorCategory.UNSPECIFIED,
          "something not good when calling processor");
    }
    return new GdnRestSingleResponse<>(resultByTypeResponse, requestId);
  }

  public AsyncGraphicsProcessorServiceWrapper getAsyncService() {
    return asyncService;
  }

  public ImagePathConfiguration getImagePathConfiguration() {
    return imagePathConfiguration;
  }


  @SuppressWarnings("unused")
  private String getNewFileExt(String fileName, String newExt) {
    return FilenameUtils.removeExtension(fileName) + "." + newExt;
  }

  public GraphicsProcessorService getService() {
    return service;
  }

  @RequestMapping(value = ApiPath.IDENTIFY_OPERATION_PATH,
      consumes = {MediaType.MULTIPART_FORM_DATA_VALUE},
      produces = {MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE},
      method = {RequestMethod.POST})
  @Operation
  public GdnRestSingleResponse<IdentifyImageResponse> identify(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam("content") MultipartFile file)
          throws Exception {
    IdentifyImageResult result = getService().getGraphicsProperty(file.getInputStream(), clientId);
    IdentifyImageResponse response = orikaMapper.deepCopy(result, IdentifyImageResponse.class);
    return new GdnRestSingleResponse<>(response, requestId);
  }

  private void isContentPresent(MultipartFile file) throws ApplicationException {
    if (file.isEmpty()) {
      LOG.error("empty file is not allowed");
      throw new ApplicationException(ErrorCategory.VALIDATION, "empty content is not allowed");
    }
  }

  private String parseValue(RemoveImageRequest request, String clientId) throws Exception {
    LOG.trace("request : {}, clientID : {}", new Object[] {request, clientId});
    ValidationKeyEncryptor encryptor = new ValidationKeyEncryptor(request.getRandomSeed());
    String decryptedText = encryptor.decrypt(request.getValue());
    LOG.trace("decrypted : {}", new Object[] {decryptedText});
    String[] values = decryptedText.split(",");
    if (values[0].equals(clientId)) {
      return values[1];
    }
    throw new ApplicationException(ErrorCategory.VALIDATION, "who are you?");
  }

  @RequestMapping(value = ApiPath.REMOVE_PATH, consumes = {
      MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE}, method = {
      RequestMethod.POST})
  @Operation
  public GdnBaseRestResponse remove(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestBody RemoveImageRequest request)
          throws Exception {
    String imagePath = parseValue(request, clientId);
    byte[] content = fileStorageService.gcsRemoveForOxford(imagePath, clientId);
    if (ArrayUtils.isEmpty(content)) {
      throw new ApplicationException(ErrorCategory.DATA_NOT_FOUND);
    }
    return new GdnBaseRestResponse(requestId);
  }

  @RequestMapping(value = ApiPath.SCALE_OPERATION_PATH, consumes = {
      MediaType.MULTIPART_FORM_DATA_VALUE}, produces = {
      MediaType.APPLICATION_JSON_VALUE}, method = {RequestMethod.POST})
  @Operation
  public GdnRestListResponse<ConvertImageResponse> scale(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam(required = false) String groupCode, @RequestParam("imageName") String imageName,
      @RequestParam("settings") String settings, @RequestParam("content") MultipartFile file) throws Exception {
    isContentPresent(file);
    try {
      List<CustomGraphicsSettings> customGraphicsSettings =
          GraphicsProcessorHelper.initializeSettingsFromString(settings, OBJECT_MAPPER);
      List<ConvertImageResponse> responses = new ArrayList<>(customGraphicsSettings.size());
      List<GraphicDetailCommand> commands = new ArrayList<>(customGraphicsSettings.size());
      boolean uploadToGcs = fileStorageService.uploadToGcsRMA(clientId);
      service.uploadFileAndGenerateCommandRequestForScaling(commands, responses,
          customGraphicsSettings, imageName, file, clientId, groupCode, uploadToGcs);
      getAsyncService().scales(commands, uploadToGcs, clientId, requestId);
      return new GdnRestListResponse<>(responses,
          new PageMetaData(responses.size(), 0, responses.size()), requestId);
    } catch (Exception e) {
      LOG.error(e.getMessage(), e);
      throw new ApplicationException(ErrorCategory.UNSPECIFIED,
          "something not good when calling processor");
    }
  }
  @RequestMapping(value = ApiPath.SCALE_BULK_IMAGES_OPERATION_PATH, consumes = MediaType
      .APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE, method =
      RequestMethod.POST)
  @Operation(summary = "process bulk images", description = "MTA - process bulk images")
  public GdnBaseRestResponse scaleBulkImages(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username,
      @RequestBody BulkImagesProcessRequest request) throws ApplicationException{
    try {
      if (CollectionUtils.isNotEmpty(request.getImageRequests())) {
        List<CustomGraphicsSettings> customGraphicsSettings = GraphicsProcessorHelper
            .initializeSettingsFromString(request.getCustomGraphicsSettings(), OBJECT_MAPPER);
        BulkImageProcessResponse bulkImageProcessResponse = new BulkImageProcessResponse();
        bulkImageProcessResponse.setGroupCode(request.getGroupCode());
        List<GraphicImageDetail> graphicImageDetails = new ArrayList<>();
        String prefix = getImagePathConfiguration().getLocationPrefix(clientId);
        for (ImageRequest imageRequest : request.getImageRequests()) {
          for (CustomGraphicsSettings graphicSettings : customGraphicsSettings) {
            String imageNameWithoutExtension = imageRequest.getImageName().split(REGEX_FOR_SPLIT_ON_LAST_DOT)[0];
            StringBuilder newImageName =
                new StringBuilder(imageNameWithoutExtension.replaceAll(PERMITTED_CHARS_EXCLUDING_DOT, UNDERSCORE));
            ConverterUtil.getImageNameWithExtension(newImageName, webpConversionEnabled);
            String generatedImageLocation = GraphicsProcessorHelper
                .getFileSystemPath(request.getGroupCode(), newImageName.toString(),
                    GraphicsProcessorHelper.getSize(graphicSettings), prefix);
            GraphicImageDetail graphicImageDetail = new GraphicImageDetail(imageRequest.getHashCode(),
                imageRequest.getAbsoluteImagePath(), generatedImageLocation, graphicSettings,
                prefix, request.getGroupCode());
            graphicImageDetail.setCommonImage(imageRequest.isCommonImage());
            graphicImageDetails.add(graphicImageDetail);
          }
        }
        asyncGraphicsProcessService
            .scaleBulkImages(graphicImageDetails, request.getGroupCode(), clientId, request.isRevised(),
                request.getPrioritySeller());
      }
    } catch (Exception e) {
      LOG.error("failed to process image processing for productCode : {} , error - ", request.getGroupCode(),
          e);
      throw new ApplicationException(ErrorCategory.UNSPECIFIED,
          "failed to process image processing for productCode : {} " + request.getGroupCode());
    }
    return new GdnBaseRestResponse(requestId);
  }


  @RequestMapping(value = ApiPath.RESIZE_BULK_IMAGES_OPERATION_PATH, consumes = MediaType
      .APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE, method =
                      RequestMethod.POST)
  @Operation(summary = "process bulk images", description = "MTA - process bulk images")
  public GdnBaseRestResponse resizeBulkImages(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username,
      @RequestBody BulkResizeImageRequest request) throws Exception {
    try {
      if (CollectionUtils.isNotEmpty(request.getImageRequests())) {
        BulkImageProcessResponse bulkImageProcessResponse = new BulkImageProcessResponse();
        bulkImageProcessResponse.setGroupCode(request.getGroupCode());
        List<GraphicImageDetail> graphicImageDetails = new ArrayList<>();
        String prefix = getImagePathConfiguration().getLocationPrefix(clientId);
        for (ImageRequest imageRequest : request.getImageRequests()) {
          CustomGraphicsSettings customGraphicsSettings = ConverterUtil.getCustomGraphicsSettings(request);
          String imageNameWithoutExtension = imageRequest.getImageName().split(REGEX_FOR_SPLIT_ON_LAST_DOT)[0];
          StringBuilder newImageName =
              new StringBuilder(imageNameWithoutExtension.replaceAll(PERMITTED_CHARS_EXCLUDING_DOT, UNDERSCORE));
          ConverterUtil.getImageNameWithExtension(newImageName, webpConversionEnabled);
          String generatedImageLocation =
              GraphicsProcessorHelper.getResizedImagePath(request.getGroupCode(), newImageName.toString(), prefix);
          GraphicImageDetail graphicImageDetail =
              new GraphicImageDetail(imageRequest.getHashCode(), imageRequest.getAbsoluteImagePath(),
                  generatedImageLocation, customGraphicsSettings, prefix, request.getGroupCode());
          graphicImageDetail.setCommonImage(imageRequest.isCommonImage());
          graphicImageDetails.add(graphicImageDetail);
        }
        asyncGraphicsProcessService
            .resizeBulkImages(graphicImageDetails, request.getGroupCode(), clientId, request.getPrioritySeller());
      }
    } catch (Exception e) {
      LOG.error("failed to process image resizing for productCode : {}", request.getGroupCode(),
          e);
      throw new ApplicationException(ErrorCategory.UNSPECIFIED,
          "failed to process image resizing for productCode : {}" + request.getGroupCode());
    }
    return new GdnBaseRestResponse(requestId);
  }


  public void setAsyncService(AsyncGraphicsProcessorServiceWrapper asyncService) {
    this.asyncService = asyncService;
  }

  public void setService(GraphicsProcessorService service) {
    this.service = service;
  }

  @RequestMapping(value = ApiPath.DISPLAY_PATH,
      produces = {MediaType.IMAGE_JPEG_VALUE, MediaType.IMAGE_GIF_VALUE, MediaType.IMAGE_PNG_VALUE},
      method = {RequestMethod.GET})
  @Operation
  public void showImage(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam String imagePath,
      HttpServletResponse response) throws Exception {
    byte[] content = fileStorageService.gcsToFileForXRMA(imagePath, clientId);
    if (ArrayUtils.isNotEmpty(content)) {
      response.setContentType(APPLICATION_OCTET_STREAM_MEDIA_TYPE);
      response.setHeader(CONTENT_DISPOSITION_HEADER_KEY,
          new StringBuilder("filename=\"").append(imagePath).append("\"").toString());
      response.getOutputStream().write(content);
    } else {
      LOG.warn("{} image file not found", imagePath);
      BaseResponse baseResponse =
          new BaseResponse(IMAGE_NOT_FOUND, ErrorCategory.DATA_NOT_FOUND.toString(), Boolean.FALSE, requestId);
      response.setContentType(APPLICATION_JSON_MEDIA_TYPE);
      response.setStatus(HttpStatus.NOT_FOUND.value());
      ByteArrayInputStream inputStream =
          new ByteArrayInputStream(baseResponse.toString().getBytes(StandardCharsets.UTF_8.name()));
      IOUtils.copyLarge(inputStream, response.getOutputStream());
    }
  }

  @RequestMapping(value = ApiPath.STORE_OPERATION_PATH,
      consumes = {MediaType.MULTIPART_FORM_DATA_VALUE},
      produces = {MediaType.APPLICATION_JSON_VALUE},
      method = {RequestMethod.POST})
  @ResponseBody
  @Operation
  public GdnRestSingleResponse<ConvertImageResponse> store(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam("imageName") String imageName,
      @RequestParam("content") MultipartFile file) throws Exception {
    isContentPresent(file);
    String prefix = fileStorageService.getImageLocationPathPrefix(clientId);
    boolean uploadedToGcs = fileStorageService.uploadToGcsOxford(clientId);
    IdentifyImageResult result = getService().getGraphicsProperty(file.getInputStream(), clientId);
    String generatedImageLocation = GraphicsProcessorHelper.getFileSystemPath(imageName,
        GraphicsProcessorHelper.getSize(result.getWidth()), prefix);
    ConvertImageResponse response = new ConvertImageResponse();
    try {
      ImageResultDetail storeResult =
          getService().store(file.getInputStream(), generatedImageLocation, prefix, clientId, uploadedToGcs);
      if (storeResult.isSuccess()) {
        response = new ConvertImageResponse(
            generatedImageLocation.substring(prefix.length(), generatedImageLocation.length()),
            imageName, result.getWidth(), result.getHeight());
        response.setUploadedToGcs(uploadedToGcs);
      }
      return new GdnRestSingleResponse<>(response, requestId);
    } catch (Exception e) {
      LOG.error("something bad happened", e);
      throw new ApplicationException(ErrorCategory.DATA_ACCESS);
    }
  }

  @RequestMapping(value = ApiPath.RESIZE_EDITED_IMAGES_OPERATION_PATH, consumes = MediaType
      .APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE, method =
      RequestMethod.POST)
  @Operation(summary = "process edited images", description = "MTA - process edited images")
  @ResponseBody
  public GdnBaseRestResponse resizeEditedImages(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username,
      @RequestBody BulkResizeImageRequest request) throws Exception {
    try {
      if (CollectionUtils.isNotEmpty(request.getImageRequests())) {
        List<GraphicImageDetail> graphicImageDetails = new ArrayList<>();
        String prefix = getImagePathConfiguration().getLocationPrefix(clientId);
        for (ImageRequest imageRequest : request.getImageRequests()) {
          CustomGraphicsSettings customGraphicsSettings = ConverterUtil.getCustomGraphicsSettings(request);
          String imageNameWithoutExtension = imageRequest.getImageName().split(REGEX_FOR_SPLIT_ON_LAST_DOT)[0];
          StringBuilder newImageName =
            new StringBuilder(imageNameWithoutExtension.replaceAll(PERMITTED_CHARS_EXCLUDING_DOT, UNDERSCORE));
          ConverterUtil.getImageNameWithExtension(newImageName, webpConversionEnabled);
          String generatedImageLocation =
              GraphicsProcessorHelper.getResizedImagePath(request.getGroupCode(), newImageName.toString(), prefix);
          GraphicImageDetail graphicImageDetail =
              new GraphicImageDetail(imageRequest.getHashCode(), imageRequest.getAbsoluteImagePath(),
                  generatedImageLocation, customGraphicsSettings, prefix, request.getGroupCode());
          graphicImageDetail.setCommonImage(imageRequest.isCommonImage());
          graphicImageDetails.add(graphicImageDetail);
        }
        asyncGraphicsProcessService
            .resizeEditedImages(graphicImageDetails, request.getGroupCode(), clientId);
      }
    } catch (Exception e) {
      LOG.error("failed to process image resizing for productCode : {} ", request.getGroupCode(),
          e);
      throw new ApplicationException(ErrorCategory.UNSPECIFIED,
          "failed to process image resizing for productCode : {}" + request.getGroupCode());
    }
    return new GdnBaseRestResponse(requestId);
  }

  @RequestMapping(value = ApiPath.RESIZE_REVISED_IMAGES_OPERATION_PATH, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE, method = RequestMethod.POST)
  @Operation(summary = "process revised images", description = "MTA - process revised images")
  @ResponseBody
  public GdnBaseRestResponse resizeRevisedImages(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestBody BulkResizeImageRequest request) throws Exception {
    try {
      if (CollectionUtils.isNotEmpty(request.getImageRequests())) {
        String prefix = getImagePathConfiguration().getLocationPrefix(clientId);
        asyncGraphicsProcessService
            .resizeRevisedImages(ConverterUtil.getGraphicImageDetails(request, prefix, webpConversionEnabled), request.getGroupCode(),
                clientId);
      }
    } catch (Exception e) {
      LOG.error("failed to process image resizing for productCode : {} ", request.getGroupCode(), e);
      throw new ApplicationException(ErrorCategory.UNSPECIFIED,
          "failed to process image resizing for productCode : {}" + request.getGroupCode());
    }
    return new GdnBaseRestResponse(requestId);
  }

  @RequestMapping(value = ApiPath.SCALE_EDITED_IMAGES_OPERATION_PATH, consumes = MediaType.APPLICATION_JSON_VALUE,
      produces = MediaType.APPLICATION_JSON_VALUE, method = RequestMethod.POST)
  @Operation(summary = "scale edited images", description = "MTA - scale edited images")
  @ResponseBody
  public GdnBaseRestResponse scaleEditedImages(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestBody ScaleEditedImageRequest request) throws ApplicationException {
    LOG.info("Request sent to scale api : {}", request);
    try {
      if (CollectionUtils.isNotEmpty(request.getImageRequests())) {
        List<CustomGraphicsSettings> customGraphicsSettings =
            GraphicsProcessorHelper.initializeSettingsFromString(request.getCustomGraphicsSettings(), OBJECT_MAPPER);
        String prefix = getImagePathConfiguration().getLocationPrefix(clientId);
        asyncGraphicsProcessService
            .scaleEditedImages(customGraphicsSettings, request.getProductCode(), clientId, request, prefix);
      }
    } catch (Exception e) {
      LOG.error("failed to process image processing for productCode : {} ", request.getProductCode(), e);
      throw new ApplicationException(ErrorCategory.UNSPECIFIED,
          "failed to process image processing for productCode : {}" + request.getProductCode());
    }
    return new GdnBaseRestResponse(requestId);
  }

  @RequestMapping(value = ApiPath.SCALE_ACTIVE_PRODUCT_NEW_IMAGES, consumes = MediaType.APPLICATION_JSON_VALUE,
    produces = MediaType.APPLICATION_JSON_VALUE, method = RequestMethod.POST)
  public GdnBaseRestResponse scaleActiveProductNewImages(@RequestParam String storeId,
    @RequestParam String clientId, @RequestParam String requestId,
    @RequestBody XgpImageScaleRequest request) throws Exception {
    try {
      asyncGraphicsProcessService.scaleActiveProductNewImages(storeId, clientId, request,
        requestId);
    } catch (Exception e) {
      LOG.error("failed to process image processing for request : {} ", request, e);
      throw new ApplicationException(ErrorCategory.UNSPECIFIED,
        "failed to process image processing for request : {}" + request);
    }
    return new GdnBaseRestResponse(requestId);
  }
}

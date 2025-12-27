package com.gdn.partners.pcu.external.service.impl;

import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.net.URLConnection;
import java.util.Arrays;
import java.util.Base64;
import java.util.Iterator;
import java.util.List;

import javax.imageio.ImageIO;
import javax.imageio.ImageReader;
import javax.imageio.stream.ImageInputStream;

import com.gdn.partners.core.security.Credential;
import com.gdn.partners.pbp.commons.util.CommonUtils;
import com.gdn.partners.pcu.external.client.helper.MandatoryParameterHelper;
import com.gdn.partners.pcu.external.service.XgpService;
import com.gdn.partners.pcu.external.service.impl.helper.RequestHelper;
import com.gdn.partners.pcu.external.service.model.request.UploadAttributeImageRequest;
import com.gdn.partners.pcu.external.service.model.request.XgpImageScaleRequest;
import com.gdn.partners.pcu.external.web.model.response.ApiErrorCode;
import com.gdn.x.product.exception.ApiIncorrectInputDataException;
import org.apache.commons.lang3.ArrayUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.partners.pcu.external.model.Constants;
import com.gdn.partners.pcu.external.model.ErrorMessages;
import com.gdn.partners.pcu.external.properties.ImageProperties;
import com.gdn.partners.pcu.external.service.FileStorageService;
import com.gdn.partners.pcu.external.service.ImageService;
import com.gdn.partners.pcu.external.service.impl.exception.ImageNotFoundException;
import com.gdn.partners.pcu.external.service.impl.helper.ImageValidator;
import com.gdn.partners.pcu.external.service.model.request.UploadImageRequest;

import lombok.extern.slf4j.Slf4j;
import org.springframework.web.multipart.MultipartFile;


@Slf4j
@Service
public class ImageServiceImpl implements ImageService {

  private static final String ESCAPE_SPACE = "\"";
  private static final String SLASH = "/";
  private static final String RESIZE_LOG = "Time taken upto resize: {}";
  private static final String WRITE_LOG_MSG = "Time taken upto write: {}";
  private static final String DOT = ".";
  private static final String JPEG = "jpeg";
  private static final String JPG = "jpg";
  private static final String PNG = "png";
  private static final String EXCEPTION_ERR_MSG = "Exception while writing content into file image {} for productCode : {} ";
  private static final String EXCEPTION_ATTR_ERR_MSG =
      "Exception while writing content into file image {}";
  @Autowired
  private ImageProperties imageProperties;

  @Autowired
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Autowired
  private FileStorageService fileStorageService;

  @Autowired
  private XgpService xgpService;

  @Value("${http.connection.timeout}")
  private int httpConnectionTimeout;

  @Value("${http.connection.readTimeout}")
  private int httpConnectionReadTimeout;

  @Value("${product.image.max.size.byte}")
  private int imageMaxSize;

  @Value("${decrease.image.resolution}")
  private boolean decreaseImageResolution;

  @Value("${image.upload.size.threshold.mb}")
  private long imageUploadSizeThresholdInMB;

  @Value("#{'${attribute.image.allowed.formats}'.split(',')}")
  private List<String> attributeImageAllowedFormats;

  @Value("${validate.product.accessibility}")
  private boolean validateProductAccessibility;

  @Value("${product.accessibility.list}")
  private String productAccessibilityList;

  @Value("${image.formats.supported}")
  private List<String> imageFormatsSupported;

  @Value("${allowed.image.mime.types}")
  private List<String> allowedImageMimeTypes;

  @Value("${call.xgp.for.image.scaling}")
  private boolean callXgpForImageScaling;

  @Override
  public boolean uploadImage(UploadImageRequest request) {
    log.debug("upload image service call, fileName : {}", request.getImageFileName());
    RequestHelper.validateAccessibilityForProductTab(validateProductAccessibility,
        Arrays.asList(Credential.getAccessibilities()), Boolean.parseBoolean(mandatoryParameterHelper.isExternal()),
        productAccessibilityList, mandatoryParameterHelper.getClientType());
    boolean success = false;
    long methodEntryTimeStamp = System.currentTimeMillis();
    if (request.isActive()) {
      if (imageFormatsSupported.contains(Constants.WEBP_FORMAT) || callXgpForImageScaling) {
        try {
          XgpImageScaleRequest xgpImageScaleRequest =
              fileStorageService.generateXgpImageScaleRequest(request);
          xgpService.scaleActiveProductNewImages(xgpImageScaleRequest);
          success = true;
        } catch (Exception e) {
          log.error(EXCEPTION_ERR_MSG, request.getImageFileName(), request.getProductCode(), e);
          throw new ApiIncorrectInputDataException(
              String.format(ApiErrorCode.ERROR_ON_IMAGE_UPLOAD.getDesc(),
                  request.getImageFileName()), ApiErrorCode.ERROR_ON_IMAGE_UPLOAD.getCode());
        }
      } else {
        try {
          fileStorageService.uploadActiveImages(request);
          success = true;
          long imageWrittenTimeStamp = System.currentTimeMillis();
          log.info(WRITE_LOG_MSG, imageWrittenTimeStamp - methodEntryTimeStamp);
        } catch (Exception e) {
          log.error(EXCEPTION_ERR_MSG, request.getImageFileName(), request.getProductCode(), e);
          throw new ApiIncorrectInputDataException(
              String.format(ApiErrorCode.ERROR_ON_IMAGE_UPLOAD.getDesc(),
                  request.getImageFileName()), ApiErrorCode.ERROR_ON_IMAGE_UPLOAD.getCode());
        }
      }
    } else {
      try {
        String sourceImagepath = fileStorageService.uploadImage(request);
        long imageWrittenTimeStamp = System.currentTimeMillis();
        log.info(WRITE_LOG_MSG, imageWrittenTimeStamp - methodEntryTimeStamp);
        log.debug("Path where image is uploaded , filePath :{}", sourceImagepath);
        success = true;
      } catch (Exception e) {
        log.error(EXCEPTION_ERR_MSG, request.getImageFileName(), request.getProductCode(), e);
        success = false;
        throw new ApiIncorrectInputDataException(
          String.format(ApiErrorCode.ERROR_ON_IMAGE_UPLOAD.getDesc(), request.getImageFileName()),
          ApiErrorCode.ERROR_ON_IMAGE_UPLOAD.getCode());
      }
    }
    return success;
  }

  @Override
  public String uploadAttributeImage(MultipartFile image, String imageFilename) {
    log.info("upload attribute image service call, fileName : {}", imageFilename);
    long methodEntryTimeStamp = System.currentTimeMillis();
    try {
      ImageValidator.validateImageFile(image);
      ImageValidator.validateImageExtensionAndSize(image.getOriginalFilename().toLowerCase(),
          image.getBytes(), image.getSize(), imageUploadSizeThresholdInMB,
          attributeImageAllowedFormats);
      UploadAttributeImageRequest request =
          RequestHelper.toUploadAttributeImageRequest(imageFilename, image.getBytes(),
              image.getOriginalFilename().toLowerCase());
      String finalPath = fileStorageService.uploadAttributeImages(request);
      long imageWrittenTimeStamp = System.currentTimeMillis();
      log.info(WRITE_LOG_MSG, imageWrittenTimeStamp - methodEntryTimeStamp);
      return finalPath;
    } catch (Exception e) {
      log.error(EXCEPTION_ATTR_ERR_MSG, imageFilename, e);
      throw new ApiIncorrectInputDataException(
          String.format(ApiErrorCode.ERROR_ON_IMAGE_UPLOAD.getDesc(), imageFilename),
          ApiErrorCode.ERROR_ON_IMAGE_UPLOAD.getCode());
    }
  }

  @Override
  public byte[] getImageDetail(String fileName, boolean active) throws Exception {
    byte[] imageContent;
    if (active) {
      imageContent = fileStorageService.getImageContent(fileName);
    } else {
      String pathPrefix = fileStorageService.getImagePathPrefix();
      if (fileName.contains(pathPrefix)) {
        imageContent = fileStorageService.downloadFile(fileName);
      }
      else {
        String[] splitImageFilenameByDash = fileName.split(SLASH);
        if (splitImageFilenameByDash.length > 3) {
          String file = File.separator + splitImageFilenameByDash[splitImageFilenameByDash.length - 2] + File.separator
              + splitImageFilenameByDash[splitImageFilenameByDash.length - 1];
          imageContent = getByteArrayFromImage(imageProperties.getBasePath(), file);
        } else {
          String file = File.separator + splitImageFilenameByDash[splitImageFilenameByDash.length - 1];
          imageContent = getByteArrayFromImage(imageProperties.getBasePath(), file);
        }
      }
    }
    return imageContent;
  }

  /**
   * Get byte array of image by file name and path
   *
   * @param path
   * @param fileName
   * @return
   * @throws IOException
   */
  private byte[] getByteArrayFromImage(String path, String fileName) throws IOException {
    File file = new File(path + fileName);
    byte[] imageContent;
    if (file.exists()) {
      try (InputStream bufferedInputStream = new BufferedInputStream(new FileInputStream(file))) {
        imageContent = new byte[(int) file.length()];
        bufferedInputStream.read(imageContent);
      }
    } else {
      throw new ImageNotFoundException(ErrorMessages.IMAGE_NOT_FOUND + file.getPath());
    }
    return imageContent;
  }

  @Override
  public InputStream getImageInputStream(String imageData) throws Exception {
    InputStream inputStream = null;
    if (imageData.startsWith(Constants.HTTP_PREFIX)) {
      throw new ApplicationException(ErrorCategory.INVALID_FORMAT, ErrorMessages.INVALID_URL);
    } else if (imageData.startsWith(Constants.HTTPS_PREFIX)) {
      inputStream = downloadImageInputStreamFromUrl(imageData);
    } else {
      inputStream = getImageInputStreamFromBase64(imageData);
    }
    return inputStream;
  }

  private InputStream downloadImageInputStreamFromUrl(String urlPath) throws Exception {
    try {
      URL url = new URL(urlPath);
      URLConnection connection = url.openConnection();
      connection.setConnectTimeout(httpConnectionTimeout);
      connection.setReadTimeout(httpConnectionReadTimeout);
      connection.setRequestProperty(Constants.ACCEPT_HEADER, Constants.IMAGE_HEADER);
      if (!allowedImageMimeTypes.contains(connection.getContentType())) {
        log.error("Not able to download image : {} invalid type ", urlPath);
        return null;
      }
      if (urlPath.equals(String.valueOf(connection.getURL()))) {
        return connection.getInputStream();
      }
      return null;
    } catch (Exception e) {
      log.error("Not able to download image : {} ", urlPath, e);
      throw new ApplicationException(ErrorCategory.VALIDATION, ErrorMessages.INVALID_REQUEST);
    }
  }

  private InputStream getImageInputStreamFromBase64(String base64Image) {
    return new ByteArrayInputStream(Base64.getDecoder().decode(base64Image));
  }

  @Override
  public boolean imageExistsAndValid(String path) throws Exception {
    byte[] bytesArray = fileStorageService.getImageFromImagePath(path);
    if (ArrayUtils.isEmpty(bytesArray)) {
      return false;
    }
    int width = 0, height = 0;
    try (ImageInputStream imageInputStream = ImageIO.createImageInputStream(new ByteArrayInputStream(bytesArray))) {
      Iterator<ImageReader> imageReaders = ImageIO.getImageReaders(imageInputStream);
      ImageReader reader = imageReaders.next();
      reader.setInput(imageInputStream, true);
      width = reader.getWidth(0);
      height = reader.getHeight(0);
      ImageValidator.validateImages(bytesArray, imageMaxSize, width, height, decreaseImageResolution);
      return true;
    } catch (Exception e) {
      log.error("Error while validating image path : {}", path, e);
      return false;
    }
  }
}

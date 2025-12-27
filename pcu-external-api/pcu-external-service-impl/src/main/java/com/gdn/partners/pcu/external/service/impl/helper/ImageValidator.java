package com.gdn.partners.pcu.external.service.impl.helper;


import java.util.List;
import java.util.stream.Stream;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.partners.pcu.external.model.Constants;
import com.gdn.partners.pcu.external.model.ErrorMessages;
import com.gdn.partners.pcu.external.service.impl.exception.ImageValidationException;
import com.gdn.partners.pcu.external.service.impl.exception.ValidationException;
import com.gdn.partners.pcu.external.web.model.response.ApiErrorCode;
import com.gdn.x.product.exception.ApiIncorrectInputDataException;

import org.apache.commons.lang3.StringUtils;
import org.springframework.web.multipart.MultipartFile;

public class ImageValidator {

  private static final String DOT = ".";
  private static final String JPEG = ".jpeg";
  private static final String JPG = ".jpg";
  private static final String PNG = ".png";

  private static final String IMAGE_VALIDATION_ERR_MESSAGE = "Image content is incorrect";

  private static final String IMAGE_SIZE_VALIDATION_ERR_MESSAGE = "Image size is greater than 4Mb";

  private static final String IMAGE_LESS_RESOLUTION_VALIDATION_ERR_MESSAGE =
      "Image resolution must be minimum 600*600 pixels dimensions";

  private static final int MINIMUM_IMAGE_RESOLUTION_VALUE_600_600 = 360000;
  private static final int MINIMUM_IMAGE_RESOLUTION_VALUE_300_300 = 90000;
  private static final long IMAGE_UPLOAD_SIZE_THRESHOLD = 1024 * 1024;
  private static final String UNDEFINED = "undefined/";


  public static void validateImages(byte[] bytes, int maxSize, int width, int height,
    boolean decreaseImageResolution) {
    int MINIMUM_IMAGE_RESOLUTION_VALUE = decreaseImageResolution ?
      MINIMUM_IMAGE_RESOLUTION_VALUE_300_300 :
      MINIMUM_IMAGE_RESOLUTION_VALUE_600_600;
    if (width * height < MINIMUM_IMAGE_RESOLUTION_VALUE) {
      throw new ApplicationRuntimeException(ErrorCategory.INVALID_STATE, IMAGE_LESS_RESOLUTION_VALIDATION_ERR_MESSAGE);
    } else if (bytes.length > maxSize) {
      throw new ApplicationRuntimeException(ErrorCategory.INVALID_STATE, IMAGE_SIZE_VALIDATION_ERR_MESSAGE);
    }
    Stream.of(FileValidator.JPEG, FileValidator.PNG, FileValidator.WEBP)
        .filter(imageType -> FileValidator.checkFileByType(bytes, imageType)).findFirst()
        .orElseThrow(() -> new ApplicationRuntimeException(ErrorCategory.INVALID_STATE, IMAGE_VALIDATION_ERR_MESSAGE));
  }

  public static void validateImageType(String fileName, List<String> imageFormatSupported) {
    if (imageFormatSupported.stream().noneMatch(
        imageFormat -> fileName.substring(fileName.lastIndexOf(DOT) + 1).equals(imageFormat))) {
      throw new ApiIncorrectInputDataException(
        String.format(ApiErrorCode.INVALID_IMAGE_TYPE.getDesc(), imageFormatSupported),
        ApiErrorCode.INVALID_IMAGE_TYPE.getCode());
    }
  }

  public static void validateImages(byte[] bytes) {
    Stream.of(FileValidator.JPEG, FileValidator.PNG, FileValidator.WEBP)
      .filter(imageType -> FileValidator.checkFileByType(bytes, imageType)).findFirst().orElseThrow(
        () -> new ApiIncorrectInputDataException(ApiErrorCode.INVALID_IMAGE_TYPE.getDesc(),
          ApiErrorCode.INVALID_IMAGE_TYPE.getCode()));
  }

  public static void validateImageExtensionAndSize(String fileName, byte[] bytes, long imageSize, long sizeThresholdInMB,
      List<String> imageFormatSupported){
    validateImageType(fileName, imageFormatSupported);
    validateImages(bytes);
    validateImageSize(imageSize, sizeThresholdInMB);
  }

  private static void validateImageSize(long imageSize, long sizeThresholdInMB) {
    if(Long.compare(imageSize, sizeThresholdInMB * IMAGE_UPLOAD_SIZE_THRESHOLD) == 1){
      throw new ImageValidationException(
        String.format(ApiErrorCode.INVALID_IMAGE_SIZE.getDesc(), sizeThresholdInMB));
    }
  }

  public static void validateImageFile(MultipartFile image) {
    if (image.isEmpty()) {
      throw new ValidationException(ErrorMessages.FILE_EMPTY_ERROR_MESSAGE);
    }
  }

  public static String replaceUndefinedInImageFileWithProductCode(boolean replaceUndefinedWithProductCode,
      String imageFilename, String productCode) {
    return (replaceUndefinedWithProductCode && StringUtils.isNotBlank(imageFilename) && imageFilename.toLowerCase()
        .contains(UNDEFINED.toLowerCase())) ?
        StringUtils.replace(imageFilename, UNDEFINED, productCode + Constants.ROOT) :
        imageFilename;
  }
}

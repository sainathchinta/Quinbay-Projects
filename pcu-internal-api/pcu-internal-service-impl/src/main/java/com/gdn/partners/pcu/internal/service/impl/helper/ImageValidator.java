package com.gdn.partners.pcu.internal.service.impl.helper;

import com.blibli.oss.common.error.ValidationException;
import com.gdn.merchant.service.utils.FileValidator;
import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.partners.pcu.internal.model.ErrorMessages;
import com.gdn.partners.pcu.internal.service.impl.exception.ImageValidationException;
import com.gdn.x.product.exception.ApiIncorrectInputDataException;
import org.apache.commons.lang3.StringUtils;
import org.springframework.web.multipart.MultipartFile;

import java.util.Collections;
import java.util.Map;
import java.util.stream.Stream;

import static com.gdn.partners.pcu.internal.model.ErrorMessages.INVALID_IMAGE_SIZE;
import static com.gdn.partners.pcu.internal.model.ErrorMessages.INVALID_IMAGE_TYPE;
import static com.gdn.partners.pcu.internal.model.ErrorMessages.INVALID_IMAGE_TYPE_CODE;

public class ImageValidator {
  private static final String DOT = ".";
  private static final String JPEG = ".jpeg";
  private static final String JPG = ".jpg";
  private static final String PNG = ".png";
  private static final long IMAGE_UPLOAD_SIZE_THRESHOLD = 1024 * 1024;
  private static final String UNDEFINED = "undefined/";

  public static void verifyImageFormat(String fileName) {
    if (!fileName.substring(fileName.lastIndexOf(DOT)).equalsIgnoreCase(JPG) && !fileName.substring(
        fileName.lastIndexOf(DOT)).equalsIgnoreCase(PNG) && !fileName.substring(fileName.lastIndexOf(DOT))
        .equalsIgnoreCase(JPEG)) {
      throw new ApiIncorrectInputDataException(
          String.format(INVALID_IMAGE_TYPE), INVALID_IMAGE_TYPE_CODE);
    }
  }

  public static void validateImages(byte[] bytes) {
    Stream.of(FileValidator.JPEG, FileValidator.PNG)
        .filter(imageType -> FileValidator.checkFileByType(bytes, imageType)).findFirst().orElseThrow(
            () -> new ApiIncorrectInputDataException(INVALID_IMAGE_TYPE,
                INVALID_IMAGE_TYPE_CODE));
  }

  public static void validateImageExtensionAndSize(String fileName, byte[] bytes, long imageSize, long sizeThresholdInMB){
    verifyImageFormat(fileName);
    validateImages(bytes);
    validateImageSize(imageSize, sizeThresholdInMB);
  }

  public static void validateImageExtensionAndSize(String fileName, byte[] bytes, long imageSize,
      long sizeThresholdInMB, Map<String, String> allowedExtensionsToMagicNumbersMap) {
    verifyImageFormat(fileName, allowedExtensionsToMagicNumbersMap);
    validateImages(bytes, allowedExtensionsToMagicNumbersMap);
    validateImageSize(imageSize, sizeThresholdInMB);
  }

  public static void verifyImageFormat(String fileName,
      Map<String, String> allowedExtensionsToMagicNumbersMap) {
    String extensions = fileName.substring(fileName.lastIndexOf(DOT)).toLowerCase();
    if (!allowedExtensionsToMagicNumbersMap.containsKey(extensions)) {
      throw new ApiIncorrectInputDataException(String.format(INVALID_IMAGE_TYPE),
          INVALID_IMAGE_TYPE_CODE);
    }
  }

  public static void validateImages(byte[] bytes, Map<String, String> allowedExtensionsToMagicNumbersMap) {
    allowedExtensionsToMagicNumbersMap.values().stream()
        .filter(imageType -> FileValidator.checkFileByType(bytes, imageType)).findFirst()
        .orElseThrow(
            () -> new ApiIncorrectInputDataException(INVALID_IMAGE_TYPE, INVALID_IMAGE_TYPE_CODE));
  }

  private static void validateImageSize(long imageSize, long sizeThresholdInMB) {
    if(Long.compare(imageSize, sizeThresholdInMB * IMAGE_UPLOAD_SIZE_THRESHOLD) == 1){
      throw new ImageValidationException(
          String.format(INVALID_IMAGE_SIZE, sizeThresholdInMB));
    }
  }

  public static void validateImageFile(MultipartFile image) {
    if (image.isEmpty()) {
      throw new ValidationException(Collections.singleton(ErrorMessages.FILE_EMPTY_ERROR_MESSAGE));
    }
  }

  public static String replaceUndefinedInImageFileWithProductCode(
      boolean replaceUndefinedWithProductCode, String imageFilename, String productCode) {
    return (replaceUndefinedWithProductCode && StringUtils.isNotBlank(imageFilename)
        && imageFilename.toLowerCase().contains(UNDEFINED.toLowerCase())) ?
        StringUtils.replace(imageFilename, UNDEFINED, productCode + Constants.ROOT) :
        imageFilename;
  }
}

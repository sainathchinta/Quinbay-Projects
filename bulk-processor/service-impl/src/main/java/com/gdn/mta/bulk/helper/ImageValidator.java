package com.gdn.mta.bulk.helper;

import java.util.stream.Stream;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;


public class ImageValidator {

  private static final String IMAGE_VALIDATION_ERR_MESSAGE = "Image content is incorrect";

  private static final String IMAGE_SIZE_VALIDATION_ERR_MESSAGE = "Image size is greater than 4Mb";

  private static final String IMAGE_LESS_RESOLUTION_VALIDATION_ERR_MESSAGE =
      "Image resolution must be minimum 600*600 pixels dimensions";
  private static final int MINIMUM_IMAGE_RESOLUTION_VALUE_600_600 = 360000;
  private static final int MINIMUM_IMAGE_RESOLUTION_VALUE_300_300 = 90000;

  private ImageValidator() {
  }

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
}

package com.gdn.mta.bulk.helper;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.gdn.common.exception.ApplicationRuntimeException;

public class ImageValidatorTest {

  private byte[] bytes;
  int OneMegaBytes = 1024 * 1024;
  int FourMegaBytes = 4 * 1024 * 1024;
  private static final int width = 600;
  private static final int height = 600;
  private static final int width1 = 300;
  private static final int height1 = 300;

  private static final String IMAGE_LESS_RESOLUTION_VALIDATION_ERR_MESSAGE =
      "Image resolution must be minimum 600*600 pixels dimensions";

  @BeforeEach
  public void setUp() {
    bytes = new byte[]{-1, -40, -20, -10};
  }

  @Test
  public void validateImageTest() {
    ImageValidator.validateImages(bytes, OneMegaBytes, width, height, false);
  }

  @Test
  public void validateImage_expectException() {
    bytes = new byte[10];
    Assertions.assertThrows(RuntimeException.class,
        () -> ImageValidator.validateImages(bytes, OneMegaBytes, width, height, false));
  }

  @Test
  public void validateImage_SizeExceptionTest() {
    bytes = new byte[(int) (FourMegaBytes + FourMegaBytes)];
    Assertions.assertThrows(RuntimeException.class,
        () -> ImageValidator.validateImages(bytes, FourMegaBytes, width, height, false));
  }

  @Test
  public void validateImageWithLessResolution_ExceptionTest() {
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> ImageValidator.validateImages(bytes, FourMegaBytes, width, 500, false));
    } catch (ApplicationRuntimeException e) {
      Assertions.assertTrue(e.getErrorMessage().contains(IMAGE_LESS_RESOLUTION_VALIDATION_ERR_MESSAGE));
      throw e;
    }
  }

  @Test
  public void validateImageWithLessResolutionTest() {
    ImageValidator.validateImages(bytes, OneMegaBytes, width1, height1, true);
  }

  @Test
  public void validateImageWithLessThan300Resolution_ExceptionTest() {
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> ImageValidator.validateImages(bytes, FourMegaBytes, 250, 250, true));
    } catch (ApplicationRuntimeException e) {
      Assertions.assertTrue(e.getErrorMessage().contains(IMAGE_LESS_RESOLUTION_VALIDATION_ERR_MESSAGE));
      throw e;
    }
  }

}

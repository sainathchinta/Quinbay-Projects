package com.gdn.partners.pcu.external.service.impl.helper;

import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.partners.pcu.external.service.impl.exception.ImageValidationException;
import com.gdn.partners.pcu.external.service.impl.exception.ValidationException;
import com.gdn.x.product.exception.ApiIncorrectInputDataException;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;

public class ImageValidatorTest {

  private static final String IMAGE_FILE_NAME = "example.jpg";
  private static final String PNG_IMAGE_FILE_NAME = "example.png";
  private static final String JPEG_IMAGE_FILE_NAME = "example.jpeg";
  private static final String IMAGE_FILE_NAME_1 = "example.jpg1";
  private static final String PRODUCT_CODE = "product_code";
  private static final String JPEG = "jpeg";
  private static final String JPG = "jpg";
  private static final String PNG = "png";
  private static final String UNDEFINED_IMAGE_FILE_NAME = "undefined/example.jpg";
  private byte[] bytes;
  int OneMegaBytes = 1024 * 1024;
  int FourMegaBytes = 4 * 1024 * 1024;
  int EightMegaBytes = 8 * 1024 * 1024;
  private static final int width = 600;
  private static final int height = 600;
  private static final int width1 = 300;
  private static final int height1 = 300;
  private static final String IMAGE_LESS_RESOLUTION_VALIDATION_ERR_MESSAGE =
      "Image resolution must be minimum 600*600 pixels dimensions";
  private MultipartFile multipartFile;

  @BeforeEach
  public void setUp() {
    bytes = new byte[]{-1, -40, -20, -10};
  }

  @Test
  public void validateImageType() {
    ImageValidator.validateImageType( IMAGE_FILE_NAME, List.of(JPG, PNG, JPEG));
    ImageValidator.validateImageType(PNG_IMAGE_FILE_NAME, List.of(JPG, PNG, JPEG));
    ImageValidator.validateImageType(JPEG_IMAGE_FILE_NAME, List.of(JPG, PNG, JPEG));
  }

  @Test
  public void validateImageTypeExceptionTest() {
    Assertions.assertThrows(ApiIncorrectInputDataException.class,
        () -> ImageValidator.validateImageType(IMAGE_FILE_NAME_1, List.of(JPG, PNG, JPEG)));
  }

  @Test
  public void validateImage() throws ApplicationException {
    ImageValidator.validateImages(bytes);
  }

  @Test
  public void validateImageWithException() throws ApplicationException {
    Assertions.assertThrows(ApiIncorrectInputDataException.class,
        () -> ImageValidator.validateImages(new byte[10]));
  }

  @Test
  public void validateImageTest() {
    ImageValidator.validateImages(bytes, OneMegaBytes, width, height, false);
  }

  @Test
  public void validateImage_expectException() {
    bytes = new byte[10];
    Assertions.assertThrows(ApplicationRuntimeException.class,
        () -> ImageValidator.validateImages(bytes, OneMegaBytes, width, height, false));
  }

  @Test
  public void validateImage1_expectException() {
    bytes = new byte[10];
    Assertions.assertThrows(ApplicationRuntimeException.class,
        () -> ImageValidator.validateImages(bytes, OneMegaBytes, 100, 100, false));
  }

  @Test
  public void validateImage_SizeExceptionTest() {
    bytes = new byte[(int) (FourMegaBytes + FourMegaBytes)];
    Assertions.assertThrows(ApplicationRuntimeException.class,
        () -> ImageValidator.validateImages(bytes, FourMegaBytes, width, height, false));
  }

  @Test
  public void validateImageWithLessResolution_ExceptionTest() {
      Assertions.assertThrows(ApplicationRuntimeException.class,
          () -> ImageValidator.validateImages(bytes, FourMegaBytes, width, 500, false));
  }

  @Test
  public void validateImageWithLessResolutionTest() {
    ImageValidator.validateImages(bytes, OneMegaBytes, width1, height1, true);
  }

  @Test
  public void validateImage_SizeExceptionTrueResolutionLessTest() {
    bytes = new byte[(int) (FourMegaBytes + FourMegaBytes)];
    Assertions.assertThrows(ApplicationRuntimeException.class,
        () -> ImageValidator.validateImages(bytes, FourMegaBytes, width, height, true));
  }
  @Test
  public void validateImageWithLessThan300Resolution_ExceptionTest() {
      Assertions.assertThrows(ApplicationRuntimeException.class,
          () -> ImageValidator.validateImages(bytes, FourMegaBytes, 250, 250, false));
  }

  @Test
  public void validateImageExtensionAndSizeTest() {
    ImageValidator.validateImageExtensionAndSize(IMAGE_FILE_NAME, bytes, OneMegaBytes, FourMegaBytes,
        List.of(JPG, PNG, JPEG));
  }

  @Test
  public void validateImageExtensionAndSizeWithSizeExceededTest() {
    Assertions.assertThrows(ImageValidationException.class,
        () -> ImageValidator.validateImageExtensionAndSize(IMAGE_FILE_NAME, bytes, EightMegaBytes,
            6, List.of(JPG, PNG, JPEG)));
  }

  @Test
  public void validateImageFileTest() {
    multipartFile = new MockMultipartFile("image", null, "image/jpeg", (byte[]) null);
    Assertions.assertThrows(ValidationException.class,
        () -> ImageValidator.validateImageFile(multipartFile));
  }

  @Test
  public void replaceUndefinedInImageFileWithProductCodeTest() {
    ImageValidator.replaceUndefinedInImageFileWithProductCode(false, IMAGE_FILE_NAME, PRODUCT_CODE);
    ImageValidator.replaceUndefinedInImageFileWithProductCode(true, null, PRODUCT_CODE);
    Assertions.assertEquals(IMAGE_FILE_NAME,
        ImageValidator.replaceUndefinedInImageFileWithProductCode(true, IMAGE_FILE_NAME, PRODUCT_CODE));
    ImageValidator.replaceUndefinedInImageFileWithProductCode(true, UNDEFINED_IMAGE_FILE_NAME, PRODUCT_CODE);
  }
}
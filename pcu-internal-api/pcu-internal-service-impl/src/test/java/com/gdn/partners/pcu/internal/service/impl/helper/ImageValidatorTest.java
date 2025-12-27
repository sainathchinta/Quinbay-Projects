package com.gdn.partners.pcu.internal.service.impl.helper;

import com.gdn.partners.pcu.internal.service.impl.exception.ImageValidationException;
import com.gdn.x.product.exception.ApiIncorrectInputDataException;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.web.multipart.MultipartFile;

import java.awt.*;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Map;

import static com.gdn.partners.pcu.internal.model.ErrorMessages.INVALID_IMAGE_TYPE;
import static com.gdn.partners.pcu.internal.model.ErrorMessages.INVALID_IMAGE_TYPE_CODE;

class ImageValidatorTest {

  private static final String IMAGE_FILE_NAME = "example.jpg";
  private static final String PNG_IMAGE_FILE_NAME = "example.png";
  private static final String JPEG_IMAGE_FILE_NAME = "example.jpeg";
  private static final String PDF_IMAGE_FILE_NAME = "example.pdf";
  private static final String IMAGE_FILE_NAME_1 = "example.jpg1";
  private byte[] bytes;
  int OneMegaBytes = 1024 * 1024;
  int FourMegaBytes = 4 * 1024 * 1024;
  int EightMegaBytes = 8 * 1024 * 1024;
  private MultipartFile multipartFile;
  Map<String, String> extensionToMagicNumberMap =
      Map.of(".jpeg", "ffd8", ".jpg", "ffd8", ".png", "8950", ".webp", "52494646");
  private byte[] webpImageBytes;
  private byte[] pdfBytes;
  private byte[] jpgImageBytes;
  private static final String PRODUCT_CODE = "MTA-12345";

  @BeforeEach
  public void setUp() {
    bytes = new byte[] {-1, -40, -20, -10};
  }

  @Test
  void validateImageType() {
    ImageValidator.verifyImageFormat(IMAGE_FILE_NAME);
    ImageValidator.verifyImageFormat(PNG_IMAGE_FILE_NAME);
    ImageValidator.verifyImageFormat(JPEG_IMAGE_FILE_NAME);
  }

  @Test
  void validateImageTypeExceptionTest() {
    try {
      ImageValidator.verifyImageFormat(IMAGE_FILE_NAME_1);
    } catch (ApiIncorrectInputDataException ex) {
      Assertions.assertNotNull(ex);
    }
  }

  @Test
  void validateImageExtensionAndSizeTest() {
    ImageValidator.validateImageExtensionAndSize(IMAGE_FILE_NAME, bytes, OneMegaBytes,
        FourMegaBytes);
  }

  @Test
  void validateImageExtensionAndSizeWithSizeExceededTest() {
    try {
      ImageValidator.validateImageExtensionAndSize(IMAGE_FILE_NAME, bytes, EightMegaBytes, 6);
    } catch (ImageValidationException ex) {
      Assertions.assertNotNull(ex);
    }
  }

  @Test
  void validateImageFileTest() {
    multipartFile = new MockMultipartFile("image", null, "image/jpeg", (byte[]) null);
    try {
      ImageValidator.validateImageFile(multipartFile);
    } catch (Exception ex) {
      Assertions.assertNotNull(ex);
    }
  }

  @Test
  void verifyImageFormat_jpeg() {
    ImageValidator.verifyImageFormat(JPEG_IMAGE_FILE_NAME, extensionToMagicNumberMap);
  }

  @Test
  void verifyImageFormat_pdf() {
    try {
      ImageValidator.verifyImageFormat(PDF_IMAGE_FILE_NAME, extensionToMagicNumberMap);
    } catch (ApiIncorrectInputDataException ex) {
      Assertions.assertNotNull(ex);
      Assertions.assertEquals(String.format(INVALID_IMAGE_TYPE), ex.getErrorMessage());
    }
  }

  @Test
  void validateImages_webp() throws IOException {
    webpImageBytes = Files.readAllBytes(Paths.get("src/test/resources/imageValidator/webp_test.webp"));
    ImageValidator.validateImages(webpImageBytes, extensionToMagicNumberMap);
  }

  @Test
  void validateImages_pdf() throws IOException {
    pdfBytes =
        Files.readAllBytes(Paths.get("src/test/resources/imageValidator/pdf_test.pdf"));
    try {
      ImageValidator.validateImages(pdfBytes, extensionToMagicNumberMap);
    } catch (ApiIncorrectInputDataException ex) {
      Assertions.assertNotNull(ex);
      Assertions.assertEquals(String.format(INVALID_IMAGE_TYPE), ex.getErrorMessage());
    }
  }

  @Test
  void validateImageExtensionAndSize() throws IOException {
    jpgImageBytes = Files.readAllBytes(Paths.get("src/test/resources/imageValidator/jpg_test.jpg"));
    ImageValidator.validateImageExtensionAndSize("jpg_test.jpg", jpgImageBytes, OneMegaBytes,
        EightMegaBytes, extensionToMagicNumberMap);
  }

  @Test
  void replaceUndefinedInImageFileWithProductCode_flagFalse() {
    String actual =
        ImageValidator.replaceUndefinedInImageFileWithProductCode(false, IMAGE_FILE_NAME,
            PRODUCT_CODE);
    Assertions.assertEquals(IMAGE_FILE_NAME, actual);
  }

  @Test
  void replaceUndefinedInImageFileWithProductCode_flagTrueBlankName() {
    String actual =
        ImageValidator.replaceUndefinedInImageFileWithProductCode(true, "",
            PRODUCT_CODE);
    Assertions.assertEquals("", actual);
  }

  @Test
  void replaceUndefinedInImageFileWithProductCode_flagTrueUndefinedName() {
    String actual = ImageValidator.replaceUndefinedInImageFileWithProductCode(true,
        "undefined/" + IMAGE_FILE_NAME, PRODUCT_CODE);
    Assertions.assertEquals(PRODUCT_CODE + "/" + IMAGE_FILE_NAME, actual);
  }

  @Test
  void replaceUndefinedInImageFileWithProductCode_flagTrue() {
    String actual = ImageValidator.replaceUndefinedInImageFileWithProductCode(true,
        IMAGE_FILE_NAME, PRODUCT_CODE);
    Assertions.assertEquals(IMAGE_FILE_NAME, actual);
  }
}

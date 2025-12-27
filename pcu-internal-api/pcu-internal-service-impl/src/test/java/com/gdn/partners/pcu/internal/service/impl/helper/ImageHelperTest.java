package com.gdn.partners.pcu.internal.service.impl.helper;


import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import javax.imageio.ImageIO;

import org.apache.commons.io.FileUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.mock.web.MockMultipartFile;

import com.gdn.partners.pcu.internal.service.impl.exception.ImageValidationException;
import com.gdn.x.productcategorybase.dto.brand.BrandApproveRequest;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class ImageHelperTest {

  private static final String SLASH_SEPARATOR = "/";
  private static final String FULL_IMAGE_TYPE = "full-MTA.jpg";
  private static final String IMAGE_PATH = "image";
  private BrandApproveRequest brandApproveRequest = new BrandApproveRequest();
  private static final String BRAND_CODE = "BRD-00001";
  private static final String SOURCE_PATH = "SOURCE_PATH";
  private static final String FINAL_PATH = "FINAL_PATH";
  private static final String DEFAULT_BRAND_REQUEST_CODE = "BR-0001-0001";

  private String[] splitImageFilenameByDash;
  private byte[] bytes;

  @BeforeEach
  public void setUp() throws Exception {
    splitImageFilenameByDash = new String[1];
    splitImageFilenameByDash[0] = FULL_IMAGE_TYPE;
    bytes = new byte[]{-1, -40, -20, -10};
    brandApproveRequest.setBrandRequestCode(DEFAULT_BRAND_REQUEST_CODE);
    brandApproveRequest.setBrandLogoPath(FULL_IMAGE_TYPE);
    brandApproveRequest.setProfileBannerPath(FULL_IMAGE_TYPE);
  }

  @Test
  public void checkImageSizeTest() throws IOException {
    mockFile(IMAGE_PATH + SLASH_SEPARATOR + FULL_IMAGE_TYPE);
    int response = ImageHelper.checkImageSize(splitImageFilenameByDash, new StringBuilder(IMAGE_PATH));
    Assertions.assertEquals(3827, response);
  }

  @Test
  public void checkImageSizeTest2() throws IOException {
    mockFile(IMAGE_PATH + SLASH_SEPARATOR + IMAGE_PATH + SLASH_SEPARATOR + FULL_IMAGE_TYPE);
    int response = ImageHelper
        .checkImageSize(splitImageFilenameByDash, new StringBuilder(IMAGE_PATH + SLASH_SEPARATOR + IMAGE_PATH));
    Assertions.assertEquals(3827, response);
  }

  @Test
  public void checkImageSizeTest3() throws IOException {
    mockFile(
        IMAGE_PATH + SLASH_SEPARATOR + IMAGE_PATH + SLASH_SEPARATOR + IMAGE_PATH + SLASH_SEPARATOR + FULL_IMAGE_TYPE);
    int response = ImageHelper.checkImageSize(splitImageFilenameByDash,
        new StringBuilder(IMAGE_PATH + SLASH_SEPARATOR + IMAGE_PATH + SLASH_SEPARATOR + IMAGE_PATH));
    Assertions.assertEquals(3827, response);
  }

  @Test
  public void approveBrandLogoTest() throws Exception {
    mockFile(SOURCE_PATH + SLASH_SEPARATOR + DEFAULT_BRAND_REQUEST_CODE + SLASH_SEPARATOR + FULL_IMAGE_TYPE);
    ImageHelper.approveBrandLogo(brandApproveRequest, BRAND_CODE, SOURCE_PATH, FINAL_PATH);
    File finalPathFile =
        new File((FINAL_PATH) + SLASH_SEPARATOR + BRAND_CODE + SLASH_SEPARATOR + FULL_IMAGE_TYPE);
    FileReader fileReader = new FileReader(finalPathFile);
    Assertions.assertNotNull(fileReader);
  }

  @Test
  public void approveProfileBannerTest() throws Exception {
    mockFile(SOURCE_PATH + SLASH_SEPARATOR + DEFAULT_BRAND_REQUEST_CODE + SLASH_SEPARATOR + FULL_IMAGE_TYPE);
    ImageHelper.approveProfileBanner(brandApproveRequest, BRAND_CODE, SOURCE_PATH, FINAL_PATH);
    File finalPathFile =
        new File((FINAL_PATH) + SLASH_SEPARATOR + BRAND_CODE + SLASH_SEPARATOR + FULL_IMAGE_TYPE);
    FileReader fileReader = new FileReader(finalPathFile);
    Assertions.assertNotNull(fileReader);
  }

  private void mockFile(String filePath) throws IOException {
    File file = new File(filePath);
    file.mkdirs();
    int width = 640;
    int height = 320;
    BufferedImage img = new BufferedImage(width, height, BufferedImage.TYPE_3BYTE_BGR);
    ImageIO.write(img, "jpg", file);
  }

  private void deleteFolder(String folderPath) throws IOException {
    FileUtils.deleteDirectory(new File(folderPath));
  }

  @Test
  public void checkImageSizeIOExceptionTest() throws IOException {
    try {
      ImageHelper.checkImageSize(splitImageFilenameByDash, new StringBuilder(IMAGE_PATH));
    } catch (ImageValidationException ex){
      Assertions.assertNotNull(ex);
    }
  }

  @AfterEach
  public void tearDown() throws Exception {
    deleteFolder(IMAGE_PATH + SLASH_SEPARATOR);
    deleteFolder(SOURCE_PATH);
    deleteFolder(FINAL_PATH);
  }

  @Test
  public void validateImage() {
    ImageHelper.validateImages(bytes);
  }

  @Test
  public void validateImageWithException() {
    try {
      ImageHelper.validateImages(new byte[10]);
    } catch (ImageValidationException ex){
      Assertions.assertNotNull(ex);
    }
  }

  @Test
  public void mountBrandFileTest() throws Exception {
    ImageHelper.mountBrandFile(BRAND_CODE, new MockMultipartFile(FINAL_PATH, new byte[1]), SOURCE_PATH, FINAL_PATH);
  }

  @Test
  public void validate_imagesTest() throws Exception {
    ImageHelper.validate_images(new MockMultipartFile(IMAGE_PATH,bytes), new MockMultipartFile(IMAGE_PATH,bytes));
  }

  @Test
  public void deleteImagesTest() throws Exception {
    mockFile(IMAGE_PATH + SLASH_SEPARATOR + FULL_IMAGE_TYPE);
    ImageHelper.deleteImages(FULL_IMAGE_TYPE, IMAGE_PATH);
  }

  @Test
  public void deleteImagesWithEmptyImageTest() throws Exception {
    ImageHelper.deleteImages(SOURCE_PATH, FULL_IMAGE_TYPE);
  }

}
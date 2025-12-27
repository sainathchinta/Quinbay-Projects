package com.gdn.mta.bulk.util;

import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.bulk.dto.ValidateImageDTO;
import com.gdn.mta.bulk.models.BulkUploadErrorCounter;
import com.gdn.mta.bulk.models.ProductBasicDetail;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.productcategorybase.dto.Image;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.net.HttpURLConnection;
import java.net.URLConnection;
import java.nio.file.Files;
import java.util.AbstractMap;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.imageio.ImageIO;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.when;

public class ImageUtilTest {

  private static final String IMG_PATH = "CreateProductV2";
  private static final String IMAGE_1 = "img-1.jpg";
  private static final String IMAGE_WRONG_TYPE = "image-wrong-type.txt";
  private static final String JPG = "jpg";

  @InjectMocks
  private ImageUtil imageUtil;

  private String imagePath;
  private ClassLoader classLoader;
  private static final Map<String,String> allowedImagesTypes = new HashMap<>();

  @BeforeEach
  public void setUp() {
    classLoader = getClass().getClassLoader();
    MockitoAnnotations.initMocks(this);
    allowedImagesTypes.put("jpg", "ffd8");
    allowedImagesTypes.put("gif", "4749");
    allowedImagesTypes.put("png", "8950");
    allowedImagesTypes.put("webp", "5249");
  }

  @Test
  public void validateAndGetImageTypeJpgFileSuccess() throws Exception {
    imagePath = classLoader.getResource(IMG_PATH).getPath();
    File file = new File(imagePath + File.separator + IMAGE_1);
    String imageType = imageUtil.validateAndGetImageType(Files.readAllBytes(file.toPath()), allowedImagesTypes);
    Assertions.assertEquals(JPG, imageType);
  }

  @Test
  public void validateAndGetImageType_NullByte_IllegalArgumentException() throws Exception {
    Assertions.assertThrows(RuntimeException.class,
        () -> imageUtil.validateAndGetImageType(null, allowedImagesTypes));
  }

  @Test
  public void validateAndGetImageType_WrongFileType_ApiValidationException() throws Exception {
    imagePath = classLoader.getResource(IMG_PATH).getPath();
    File file = new File(imagePath + File.separator + IMAGE_WRONG_TYPE);
    Assertions.assertThrows(ApplicationException.class,
        () -> imageUtil.validateAndGetImageType(Files.readAllBytes(file.toPath()),
            allowedImagesTypes));
  }

  @Test
  public void generateHashcodeByLocationPath() {
    String imageHashCode = imageUtil.generateHashcodeByLocationPath(IMG_PATH);
    Assertions.assertNotNull(imageHashCode);
  }

  @Test
  public void getImageTest() {
    Image image = imageUtil.getImage(IMG_PATH, Map.of(IMG_PATH, IMG_PATH), IMG_PATH, true, 0, true);
    Assertions.assertNotNull(image);
    Assertions.assertEquals(IMG_PATH, image.getLocationPath());
    assertTrue(image.getOriginalImage());
    assertTrue(image.isMainImages());
    assertTrue(image.isCommonImage());
  }

  @Test
  public void addCommonImagesToProductItemRequestTest() {
    Image commonImage = new Image();
    Set<Image> commonImageSet = new HashSet<>();
    Set<Image> itemImageSet = new HashSet<>();
    commonImage.setLocationPath(IMG_PATH);
    commonImageSet.add(commonImage);
    ImageUtil.addCommonImagesToProductItemRequest(commonImageSet, itemImageSet,0, true);
    Image itemImage = itemImageSet.iterator().next();
    Assertions.assertEquals(IMG_PATH, itemImage.getLocationPath());
    Assertions.assertFalse(itemImage.isMainImages());
    ImageUtil.addCommonImagesToProductItemRequest(commonImageSet, itemImageSet,0, false);
    Image itemImage1 = itemImageSet.iterator().next();
    Assertions.assertEquals(IMG_PATH, itemImage1.getLocationPath());
    Assertions.assertFalse(itemImage1.isMainImages());
  }

  @Test
  void testResizeImage() throws IOException {
    BufferedImage original = new BufferedImage(10, 10, BufferedImage.TYPE_INT_RGB);
    ByteArrayOutputStream baos = new ByteArrayOutputStream();
    ImageIO.write(original, "jpg", baos);
    byte[] data = baos.toByteArray();
    BufferedImage resized = ImageUtil.resizeImage(5, 5, data);
    Assertions.assertNotNull(resized);
    Assertions.assertEquals(5, resized.getWidth());
    Assertions.assertEquals(5, resized.getHeight());
  }

  @Test
  void testClearDoubleSlashFromPath_WithDoubleSlash() {
    String result = ImageUtil.clearDoubleSlashFromPath("path//to//image");
    Assertions.assertEquals("path/to/image", result);
  }

  @Test
  void testClearDoubleSlashFromPath_NoDoubleSlash() {
    String result = ImageUtil.clearDoubleSlashFromPath("path/to/image");
    Assertions.assertEquals("path/to/image", result);
  }

  @Test
  void testGenerateImageName() {
    ProductBasicDetail product = new ProductBasicDetail();
    product.setBrand("Brand&123");
    product.setProductName("Prod@Name");
    String name = ImageUtil.generateImageName(1, "image/jpeg", product);
    assertTrue(name.matches("brand-123_prod-name_full02_.*\\.jpg"));
  }

  @Test
  void testSanitizeName_NullInput() {
    String result = ImageUtil.sanitizeName(null);
    Assertions.assertEquals("", result);
  }

  @Test
  void testSanitizeName_WithSpecialCharacters() {
    String result = ImageUtil.sanitizeName("Pro@duct Name#1");
    Assertions.assertEquals("Pro-duct_Name-1", result);
  }

  @Test
  void testCreateHash() {
    String hash = ImageUtil.createHash(12345);
    Assertions.assertNotNull(hash);
    Assertions.assertFalse(hash.isEmpty());
  }

  @Test
  void testMimeToExt_Jpeg() {
    Assertions.assertEquals("jpg", ImageUtil.mimeToExt("image/jpeg"));
  }

  @Test
  void testMimeToExt_Png() {
    Assertions.assertEquals("png", ImageUtil.mimeToExt("image/png"));
  }

  @Test
  void testMimeToExt_Gif() {
    Assertions.assertEquals("gif", ImageUtil.mimeToExt("image/gif"));
  }

  @Test
  void testMimeToExt_Webp() {
    Assertions.assertEquals("webp", ImageUtil.mimeToExt("image/webp"));
  }

  @Test
  void testMimeToExt_Unknown() {
    Assertions.assertEquals("img", ImageUtil.mimeToExt("unknown/type"));
  }

  @Test
  public void testValidateImageConnection_HttpURLConnectionBlock() throws Exception {
    Map.Entry<String, String> image = new AbstractMap.SimpleEntry<>("key", "value");
    BulkUploadErrorCounter counter = new BulkUploadErrorCounter();
    StringBuilder errorMessage = new StringBuilder();
    HttpURLConnection connection = Mockito.mock(HttpURLConnection.class);
    when(connection.getResponseCode()).thenReturn(HttpURLConnection.HTTP_OK);
    when(connection.getContentType()).thenReturn("image/png");
    ValidateImageDTO dto = new ValidateImageDTO();
    dto.setImage(image);
    dto.setConnection(connection);
    dto.setBulkProcessCode("code123");
    dto.setUrlImagesWithInvalidExtension(new HashSet<>());
    dto.setImages(new ArrayList<>());
    dto.setBulkUploadErrorCounter(counter);
    dto.setValidationErrorMessage(errorMessage);
    dto.setInternationalMerchant(true);
    dto.setUseHttpConnectionForImageDownload(true);
    dto.setAllowedImageTypes("image/png");
    assertTrue(ImageUtil.validateImageConnection(dto));
  }

  @Test
  public void testValidateImageConnection_HttpURLConnection_OK_Allowed() throws Exception {
    HttpURLConnection connection = Mockito.mock(HttpURLConnection.class);
    when(connection.getResponseCode()).thenReturn(HttpURLConnection.HTTP_OK);
    when(connection.getContentType()).thenReturn("image/png");
    ValidateImageDTO dto = new ValidateImageDTO();
    dto.setImage(new AbstractMap.SimpleEntry<>("k","v"));
    dto.setConnection(connection);
    dto.setBulkProcessCode("code");
    dto.setUrlImagesWithInvalidExtension(new HashSet<>());
    dto.setImages(new ArrayList<>());
    dto.setBulkUploadErrorCounter(new BulkUploadErrorCounter());
    dto.setValidationErrorMessage(new StringBuilder());
    dto.setInternationalMerchant(true);
    dto.setUseHttpConnectionForImageDownload(true);
    dto.setAllowedImageTypes("image/png");
    assertTrue(ImageUtil.validateImageConnection(dto));
  }

  @Test
  public void testValidateImageConnection_HttpURLConnection_OK_Disallowed() throws Exception {
    HttpURLConnection connection = Mockito.mock(HttpURLConnection.class);
    when(connection.getResponseCode()).thenReturn(HttpURLConnection.HTTP_OK);
    when(connection.getContentType()).thenReturn("image/jpeg");
    ValidateImageDTO dto = new ValidateImageDTO();
    dto.setImage(new AbstractMap.SimpleEntry<>("k","v"));
    dto.setConnection(connection);
    dto.setBulkProcessCode("code");
    dto.setUrlImagesWithInvalidExtension(new HashSet<>());
    dto.setImages(new ArrayList<>());
    dto.setBulkUploadErrorCounter(new BulkUploadErrorCounter());
    dto.setValidationErrorMessage(new StringBuilder());
    dto.setInternationalMerchant(true);
    dto.setUseHttpConnectionForImageDownload(true);
    dto.setAllowedImageTypes("image/png");
    assertFalse(ImageUtil.validateImageConnection(dto));
  }

  @Test
  public void testValidateImageConnection_HttpURLConnection_NonOK() throws Exception {
    HttpURLConnection connection = Mockito.mock(HttpURLConnection.class);
    when(connection.getResponseCode()).thenReturn(HttpURLConnection.HTTP_NOT_FOUND);
    when(connection.getContentType()).thenReturn("image/png");
    ValidateImageDTO dto = new ValidateImageDTO();
    dto.setImage(new AbstractMap.SimpleEntry<>("k","v"));
    dto.setConnection(connection);
    dto.setBulkProcessCode("code");
    dto.setUrlImagesWithInvalidExtension(new HashSet<>());
    dto.setImages(new ArrayList<>());
    dto.setBulkUploadErrorCounter(new BulkUploadErrorCounter());
    dto.setValidationErrorMessage(new StringBuilder());
    dto.setInternationalMerchant(true);
    dto.setUseHttpConnectionForImageDownload(true);
    dto.setAllowedImageTypes("image/png");
    assertFalse(ImageUtil.validateImageConnection(dto));
  }

  @Test
  public void testValidateImageConnection_ElseBlock_Allowed() throws Exception {
    URLConnection connection = Mockito.mock(URLConnection.class);
    when(connection.getContentType()).thenReturn("image/png");
    ValidateImageDTO dto = new ValidateImageDTO();
    dto.setImage(new AbstractMap.SimpleEntry<>("k","v"));
    dto.setConnection(connection);
    dto.setBulkProcessCode("code");
    dto.setUrlImagesWithInvalidExtension(new HashSet<>());
    dto.setImages(new ArrayList<>());
    dto.setBulkUploadErrorCounter(new BulkUploadErrorCounter());
    dto.setValidationErrorMessage(new StringBuilder());
    dto.setInternationalMerchant(true);
    dto.setUseHttpConnectionForImageDownload(false);
    dto.setAllowedImageTypes("image/png");
    assertTrue(ImageUtil.validateImageConnection(dto));
  }

  @Test
  public void testValidateImageConnection_ElseBlock_Disallowed() throws Exception {
    URLConnection connection = Mockito.mock(URLConnection.class);
    when(connection.getContentType()).thenReturn("image/jpeg");
    ValidateImageDTO dto = new ValidateImageDTO();
    dto.setImage(new AbstractMap.SimpleEntry<>("k","v"));
    dto.setConnection(connection);
    dto.setBulkProcessCode("code");
    dto.setUrlImagesWithInvalidExtension(new HashSet<>());
    dto.setImages(new ArrayList<>());
    dto.setBulkUploadErrorCounter(new BulkUploadErrorCounter());
    dto.setValidationErrorMessage(new StringBuilder());
    dto.setInternationalMerchant(true);
    dto.setUseHttpConnectionForImageDownload(true);
    dto.setAllowedImageTypes("image/png");
    assertFalse(ImageUtil.validateImageConnection(dto));
  }

  @Test
  public void testHandleInvalidContentType_ErrorCountExceeded() {
    BulkUploadErrorCounter counter = new BulkUploadErrorCounter();
    counter.setImage(Constant.ERROR_COUNT + 1);
    StringBuilder validationErrorMessage = new StringBuilder();
    Set<String> invalidUrls = new HashSet<>();
    List<String> images = new ArrayList<>(Arrays.asList("val"));
    Map.Entry<String, String> image = new AbstractMap.SimpleEntry<>("key", "val");
    ImageUtil.handleInvalidContentType(image, "code", "image/jpeg", invalidUrls, images, counter,
      validationErrorMessage, true);
    assertTrue(invalidUrls.contains("key"));
    assertFalse(images.contains("val"));
    assertEquals("", validationErrorMessage.toString());
  }

  @Test
  public void testHandleNonOkResponse_ErrorCountExceeded() {
    BulkUploadErrorCounter counter = new BulkUploadErrorCounter();
    counter.setImage(Constant.ERROR_COUNT + 1);
    StringBuilder validationErrorMessage = new StringBuilder();
    Map.Entry<String, String> image = new AbstractMap.SimpleEntry<>("key", "val");
    ImageUtil.handleNonOkResponse(image, "code", 404, "image/jpeg", counter, validationErrorMessage,
      true);
    assertEquals("", validationErrorMessage.toString());
  }
}

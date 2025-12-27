package com.gdn.mta.bulk.util;

import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.URLConnection;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.Set;

import javax.imageio.ImageIO;

import com.gdn.mta.bulk.BulkProcessValidationErrorMessages;
import com.gdn.mta.bulk.dto.ValidateImageDTO;
import com.gdn.mta.bulk.models.BulkUploadErrorCounter;
import com.gdn.partners.bulk.util.Constant;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.bulk.models.ProductBasicDetail;
import com.gdn.mta.bulk.service.util.BeanUtils;
import com.gdn.x.productcategorybase.dto.Image;
import lombok.extern.slf4j.Slf4j;

import static com.gdn.mta.bulk.helper.ProductLevel3ProcessorValidationMessageHelper.errorMessage;
import static com.gdn.mta.bulk.helper.ProductLevel3ProcessorValidationMessageHelper.errorMessageBasedOnMerchant;

@Component
@Slf4j
public class ImageUtil {

  private final static String[] hexSymbols =
      {"0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "a", "b", "c", "d", "e", "f"};

  public final static int BITS_PER_HEX_DIGIT = 4;

  public final static String XLSX = "504b03041400";
  public final static String XLS = "d0cf";
  public final static String JPEG = "ffd8";
  public final static String PNG = "8950";
  public final static String ZIP = "504b0304";
  public final static String PDF = "2550";
  public final static String GIF = "4749";
  public static final String WEBP = "5249";
  public static final String ALPHABET = "abcdefghijklmnopqrstuvwxyz1234567890";
  private static final String CHARACTERS = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
  private static final SecureRandom SECURE_RANDOM = new SecureRandom();

  public String validateAndGetImageType(byte[] bytes, Map<String, String> acceptedFileTypes) throws Exception {
    return validateImageType(bytes, acceptedFileTypes);
  }

  private String validateImageType(byte[] bytes, Map<String, String> acceptedFileTypes) throws ApplicationException {
    for (Entry<String, String> entry : acceptedFileTypes.entrySet()) {
      try {
        byte[] bytesCut = Arrays.copyOfRange(bytes, 0, entry.getValue().length() / 2);
        String hexHeader = toHexFromBytes(bytesCut);
        if (hexHeader.indexOf(entry.getValue()) != -1) {
          return entry.getKey();
        }
      } catch (Exception e) {
        throw new IllegalArgumentException(e.getMessage(), e);
      }
    }
    throw new ApplicationException(ErrorCategory.INVALID_STATE,
        "Image type doesn't supported by the system");
  }

  private String toHexFromByte(final byte b) {
    byte leftSymbol = (byte) ((b >>> BITS_PER_HEX_DIGIT) & 0x0f);
    byte rightSymbol = (byte) (b & 0x0f);
    return (hexSymbols[leftSymbol] + hexSymbols[rightSymbol]);
  }

  private String toHexFromBytes(final byte[] bytes) {
    StringBuilder hexBuffer = new StringBuilder(bytes.length * 2);
    for (int i = 0; i < bytes.length; i++) {
      hexBuffer.append(toHexFromByte(bytes[i]));
    }
    return (hexBuffer.toString());
  }

  public static String generateHashcodeByLocationPath(String imageLocationPath) {
    try {
      MessageDigest messageDigest = MessageDigest.getInstance("MD5");
      messageDigest.update(imageLocationPath.getBytes());
      byte[] digestBytes = messageDigest.digest();
      StringBuilder hashcode = new StringBuilder();
      for (int i = 0; i < digestBytes.length; i++) {
        hashcode.append(Integer.toString((digestBytes[i] & 0xff) + 0x100, 16).substring(1));
      }
      return hashcode.toString();
    } catch (NoSuchAlgorithmException e) {
      log.error("Invalid Algorithm used for hashing", e);
      return null;
    }
  }

  public static Image getImage(String storeId, Map<String, String> mappingImageFilenames, String imageFilename,
      boolean mainImage, int sequence, boolean commonImage) {
    Image image = new Image();
    image.setLocationPath(mappingImageFilenames.get(imageFilename));
    image.setMainImages(mainImage);
    image.setSequence(sequence);
    image.setHashCode(ImageUtil.generateHashcodeByLocationPath(mappingImageFilenames.get(imageFilename)));
    image.setOriginalImage(Boolean.TRUE);
    image.setStoreId(storeId);
    image.setCommonImage(commonImage);
    return image;
  }

  public static void addCommonImagesToProductItemRequest(Set<Image> commonImageSet, Set<Image> itemImageSet,
      int sequence, boolean overrideMainImage) {
    for (Image commonImage : commonImageSet) {
      Image itemCommonImage = new Image();
      BeanUtils.copyProperties(commonImage, itemCommonImage);
      if(overrideMainImage) {
        itemCommonImage.setMainImages(false);
      }
      itemCommonImage.setSequence(sequence);
      itemImageSet.add(itemCommonImage);
      sequence++;
    }
  }


  public static BufferedImage resizeImage(int width, int height, byte[] imageData) throws IOException {
    InputStream inputStream = new ByteArrayInputStream(imageData);
    BufferedImage inputImage = ImageIO.read(inputStream);
    BufferedImage outputImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);
    Graphics2D mediumGraphic = outputImage.createGraphics();
    mediumGraphic.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BICUBIC);
    mediumGraphic.clearRect(0, 0, width, height);
    mediumGraphic.drawImage(inputImage, 0, 0, width, height, null);
    mediumGraphic.dispose();
    return outputImage;
  }

  public static String clearDoubleSlashFromPath(String path) {
    return path.replaceAll("//", "/");
  }

  public static String generateImageName(int index, String mimeType, ProductBasicDetail product) {
    String ext = mimeToExt(mimeType);
    String brand = sanitizeName(product.getBrand());
    String productName = sanitizeName(product.getProductName());
    String hashKey = generateRandomString(6);
    String sequence = (index < 9 ? "0" + (index + 1) : Integer.toString(index + 1));
    String fileName = String.format("%s_%s_full%s_%s.%s", brand, productName, sequence, hashKey, ext);
    return fileName.toLowerCase();
  }

  public static String generateRandomString(int length) {
    StringBuilder sb = new StringBuilder(length);
    for (int i = 0; i < length; i++) {
      int index = SECURE_RANDOM.nextInt(CHARACTERS.length());
      sb.append(CHARACTERS.charAt(index));
    }
    return sb.toString();
  }

  public static String sanitizeName(String input) {
    if (input == null)
      return "";
    return input.replaceAll("[^a-zA-Z0-9 ]", "-").replaceAll("\\s", "_");
  }

  public static String createHash(int param) {
    StringBuilder hash = new StringBuilder();
    int len = ALPHABET.length();
    do {
      hash.insert(0, ALPHABET.charAt(param % len));
      param /= len;
    } while (param > 0);
    return hash.toString();
  }

  public static String mimeToExt(String mimeType) {
    switch (mimeType) {
      case "image/jpeg":
        return "jpg";
      case "image/png":
        return "png";
      case "image/gif":
        return "gif";
      case "image/webp":
        return "webp";
      default:
        return "img";
    }
  }

  public static boolean validateImageConnection(ValidateImageDTO validateImageDTO)
    throws IOException {
    Map.Entry<String, String> image = validateImageDTO.getImage();
    URLConnection connection = validateImageDTO.getConnection();
    String bulkProcessCode = validateImageDTO.getBulkProcessCode();
    Set<String> urlImagesWithInvalidExtension = validateImageDTO.getUrlImagesWithInvalidExtension();
    List<String> images = validateImageDTO.getImages();
    BulkUploadErrorCounter bulkUploadErrorCounter = validateImageDTO.getBulkUploadErrorCounter();
    StringBuilder validationErrorMessage = validateImageDTO.getValidationErrorMessage();
    boolean isInternationalMerchant = validateImageDTO.isInternationalMerchant();
    boolean useHttpConnectionForImageDownload =
      validateImageDTO.isUseHttpConnectionForImageDownload();
    String allowedImageTypes = validateImageDTO.getAllowedImageTypes();
    if (useHttpConnectionForImageDownload && connection instanceof HttpURLConnection) {
      HttpURLConnection httpURLConnection = (HttpURLConnection) connection;
      int responseCode = httpURLConnection.getResponseCode();
      String contentType = getContentType(connection);
      if (responseCode == HttpURLConnection.HTTP_OK) {
        if (!isAllowedContentType(contentType, allowedImageTypes)) {
          handleInvalidContentType(image, bulkProcessCode, contentType,
            urlImagesWithInvalidExtension, images, bulkUploadErrorCounter, validationErrorMessage,
            isInternationalMerchant);
          return false;
        }
      } else {
        handleNonOkResponse(image, bulkProcessCode, responseCode, contentType,
          bulkUploadErrorCounter, validationErrorMessage, isInternationalMerchant);
        return false;
      }
    } else {
      String contentType = getContentType(connection);
      if (!isAllowedContentType(contentType, allowedImageTypes)) {
        handleInvalidContentType(image, bulkProcessCode, contentType, urlImagesWithInvalidExtension,
          images, bulkUploadErrorCounter, validationErrorMessage, isInternationalMerchant);
        return false;
      }
    }
    return true;
  }

  private static String getContentType(URLConnection connection) {
    return Optional.ofNullable(connection.getContentType()).orElse(StringUtils.EMPTY);
  }

  private static boolean isAllowedContentType(String contentType, String allowedImageTypes) {
    return BulkCreationCommonUtil.getImageAllowedTypeList(allowedImageTypes).contains(contentType);
  }

  public static void handleInvalidContentType(Map.Entry<String, String> image,
    String bulkProcessCode, String contentType, Set<String> urlImagesWithInvalidExtension,
    List<String> images, BulkUploadErrorCounter bulkUploadErrorCounter,
    StringBuilder validationErrorMessage, boolean isInternationalMerchant) {
    urlImagesWithInvalidExtension.add(image.getKey());
    images.remove(image.getValue());
    bulkUploadErrorCounter.incrementImage();
    if (bulkUploadErrorCounter.getImage() <= Constant.ERROR_COUNT) {
      validationErrorMessage.append(errorMessage(image.getKey() + " - ",
          errorMessageBasedOnMerchant(isInternationalMerchant,
            BulkProcessValidationErrorMessages.IMAGE_URL_TYPE_INVALID_EN,
            BulkProcessValidationErrorMessages.IMAGE_URL_TYPE_INVALID), StringUtils.EMPTY))
        .append(Constant.DOT);
    }
    log.error(
      "Invalid image content type for image : {} , bulkProcessCode : {} , contentType : {} ",
      image.getKey(), bulkProcessCode, contentType);
  }

  public static void handleNonOkResponse(Map.Entry<String, String> image, String bulkProcessCode,
    int responseCode, String contentType, BulkUploadErrorCounter bulkUploadErrorCounter,
    StringBuilder validationErrorMessage, boolean isInternationalMerchant) {
    bulkUploadErrorCounter.incrementImage();
    if (bulkUploadErrorCounter.getImage() <= Constant.ERROR_COUNT) {
      validationErrorMessage.append(errorMessage(image.getKey() + " - ",
          errorMessageBasedOnMerchant(isInternationalMerchant,
            BulkProcessValidationErrorMessages.IMAGE_FILE_NOT_FOUND_EN,
            BulkProcessValidationErrorMessages.IMAGE_FILE_NOT_FOUND), StringUtils.EMPTY))
        .append(Constant.DOT);
    }
    log.error(
      "Unable to download image : {} , responseCode : {} , bulkProcessCode : {} , contentType : {}",
      image.getKey(), responseCode, bulkProcessCode, contentType);
  }
}

package com.gdn.micro.graphics.utils;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.math.BigInteger;
import java.util.List;
import java.util.UUID;

import org.slf4j.LoggerFactory;
import org.springframework.util.FileCopyUtils;
import org.springframework.util.StringUtils;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.micro.graphics.AvailableSize;
import com.gdn.micro.graphics.model.CustomGraphicsSettings;

public class GraphicsProcessorHelper {

  private static final org.slf4j.Logger LOG =
      LoggerFactory.getLogger(GraphicsProcessorHelper.class);

  public static final int DEFAULT_DPI = 72;
  public static final int DEFAULT_QUALITY = 75;
  public static final int DEFAULT_THUMBNAIL_SIZE = 110;
  public static final int DEFAULT_MEDIUM_SIZE = 380;

  public static final int DEFAULT_FULL_SIZE = 800;

  private static final BigInteger FOLDER_SIZE = BigInteger.valueOf(200_000);
  public static final String PATH_SEPARATOR = System.getProperty("file.separator", "/");
  public static final String RESIZE = "resize";

  public static void createDestinationDirectory(String destinationPath) throws Exception {
    String destinationDirectory = destinationPath.substring(0,
        destinationPath.lastIndexOf(GraphicsProcessorHelper.PATH_SEPARATOR));
    File file = new File(destinationDirectory);
    if (!file.exists()) {
      file.mkdirs();
    }
  }

  public static File generateTemporaryFileFromStream(InputStream inputStream, String fileExt)
      throws Exception {
    try {
      File temporaryFile = File.createTempFile(UUID.randomUUID().toString(), ".tmp." + fileExt);
      BufferedOutputStream stream = new BufferedOutputStream(new FileOutputStream(temporaryFile));
      FileCopyUtils.copy(inputStream, stream);
      stream.close();
      return temporaryFile;
    } catch (Exception e) {
      throw new ApplicationException(ErrorCategory.UNSPECIFIED);
    }
  }

  public static String getFileSystemPath(String stringToModulus, AvailableSize size,
      String prefixSystemImagePath) throws Exception {
    StringBuilder pathBuilder = new StringBuilder();
    pathBuilder.append(prefixSystemImagePath).append(PATH_SEPARATOR);
    pathBuilder.append(size.getName()).append(PATH_SEPARATOR);
    pathBuilder.append(getModulusFromString(stringToModulus)).append(PATH_SEPARATOR);
    pathBuilder.append(stringToModulus);
    return pathBuilder.toString();
  }
  
  public static String getFileSystemPath(String groupCode, String imageFilename, AvailableSize size, String prefix)
      throws Exception {
    if (StringUtils.isEmpty(groupCode)) {
      return GraphicsProcessorHelper.getFileSystemPath(imageFilename, size, prefix);
    } else {
      StringBuilder pathBuilder = new StringBuilder();
      pathBuilder.append(prefix).append(PATH_SEPARATOR);
      pathBuilder.append(size.getName()).append(PATH_SEPARATOR);
      pathBuilder.append(getModulusFromString(groupCode)).append(PATH_SEPARATOR);
      pathBuilder.append(groupCode).append(PATH_SEPARATOR);
      pathBuilder.append(imageFilename);
      return pathBuilder.toString();
    }
  }

  public static String getResizedImagePath(String groupCode, String imageFilename, String prefix) throws Exception {
    StringBuilder pathBuilder = new StringBuilder();
    pathBuilder.append(prefix).append(PATH_SEPARATOR);
    pathBuilder.append(groupCode).append(PATH_SEPARATOR);
    pathBuilder.append(RESIZE).append(PATH_SEPARATOR);
    pathBuilder.append(imageFilename);
    return pathBuilder.toString();
  }


  private static String getModulusFromString(String inputString) {
    BigInteger value = BigInteger.ONE;
    for (int i = 0; i < inputString.length(); i++) {
      value = value.add(new BigInteger("" + (Character.getNumericValue(inputString.charAt(i)) > 0
          ? Character.getNumericValue(inputString.charAt(i)) : 1)));
    }
    return String.valueOf(value.mod(FOLDER_SIZE));
  }

  public static AvailableSize getSize(CustomGraphicsSettings graphicSettings) {
    return getSize(graphicSettings.getWidth());
  }

  public static AvailableSize getSize(Integer width) {
    if (width > GraphicsProcessorHelper.DEFAULT_MEDIUM_SIZE) {
      return AvailableSize.FULL;
    } else if (width > GraphicsProcessorHelper.DEFAULT_THUMBNAIL_SIZE
        && width <= GraphicsProcessorHelper.DEFAULT_MEDIUM_SIZE) {
      return AvailableSize.MEDIUM;
    } else {
      return AvailableSize.THUMBNAIL;
    }
  }

  public static List<CustomGraphicsSettings> initializeSettingsFromString(String settings,
      ObjectMapper objectMapper) throws Exception {
    try {
      List<CustomGraphicsSettings> customGraphicsSettings =
          objectMapper.readValue(settings, new TypeReference<List<CustomGraphicsSettings>>() {});
      return customGraphicsSettings;
    } catch (Exception e) {
      throw new ApplicationException(ErrorCategory.VALIDATION,
          "settings parameter value is not valid", e);
    }
  }

  public static CustomGraphicsSettings initializeSettingFromString(String settings, ObjectMapper objectMapper)
      throws Exception {
    try {
      CustomGraphicsSettings customGraphicsSettings =
          objectMapper.readValue(settings, new TypeReference<CustomGraphicsSettings>() {
          });
      return customGraphicsSettings;
    } catch (Exception e) {
      throw new ApplicationException(ErrorCategory.VALIDATION, "settings parameter value is not valid");
    }
  }
}

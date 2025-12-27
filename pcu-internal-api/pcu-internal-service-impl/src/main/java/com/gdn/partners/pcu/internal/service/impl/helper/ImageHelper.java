package com.gdn.partners.pcu.internal.service.impl.helper;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.util.Objects;
import java.util.stream.Stream;

import org.springframework.web.multipart.MultipartFile;

import com.gdn.merchant.service.utils.FileValidator;
import com.gdn.partners.pcu.internal.model.ErrorMessages;
import com.gdn.partners.pcu.internal.service.impl.exception.ImageValidationException;
import com.gdn.x.productcategorybase.dto.brand.BrandApproveRequest;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public class ImageHelper {

  private static final String SLASH_SEPARATOR = "/";

  public static int checkImageSize(String[] splitImageFilenameByDash, StringBuilder sourceDirectory)
      throws IOException {
    File imageFile;
    byte[] imageContent;
    if (splitImageFilenameByDash.length > 2) {
      imageFile = new File(
          sourceDirectory.append(SLASH_SEPARATOR).append(splitImageFilenameByDash[splitImageFilenameByDash.length - 3])
              .append(SLASH_SEPARATOR).append(splitImageFilenameByDash[splitImageFilenameByDash.length - 2])
              .append(SLASH_SEPARATOR).append(splitImageFilenameByDash[splitImageFilenameByDash.length - 1])
              .toString());
    } else if (splitImageFilenameByDash.length > 1) {
      imageFile = new File(
          sourceDirectory.append(SLASH_SEPARATOR).append(splitImageFilenameByDash[splitImageFilenameByDash.length - 2])
              .append(SLASH_SEPARATOR).append(splitImageFilenameByDash[splitImageFilenameByDash.length - 1])
              .toString());
    } else {
      imageFile = new File(
          sourceDirectory.append(SLASH_SEPARATOR).append(splitImageFilenameByDash[splitImageFilenameByDash.length - 1])
              .toString());
    }
    if (imageFile.exists()) {
      try (InputStream bufferedInputStream = new BufferedInputStream(new FileInputStream(imageFile))) {
        imageContent = new byte[(int) imageFile.length()];
        bufferedInputStream.read(imageContent);
      }
    } else {
      throw new ImageValidationException(ErrorMessages.IMAGE_NOT_FOUND);
    }
    return imageContent.length;
  }

  public static void validateImages(byte[] bytes) {
    Stream.of(FileValidator.JPEG, FileValidator.PNG, FileValidator.GIF)
        .filter(imageType -> FileValidator.checkFileByType(bytes, imageType)).findFirst()
        .orElseThrow(() -> new ImageValidationException(ErrorMessages.IMAGE_VALIDATION_ERR_MESSAGE));
  }

  public static void approveProfileBanner(BrandApproveRequest brandWip, String brandCode,
      String profileBannerSourcePath, String profileBannerFinalPath) throws Exception {
    File file = getFile(brandWip, profileBannerSourcePath, brandWip.getProfileBannerPath());
    if (file.exists()) {
      checkIfDirectoryExists(brandCode, profileBannerFinalPath);
      moveImageToFinalPath(brandWip, brandCode, profileBannerSourcePath, profileBannerFinalPath,
          brandWip.getProfileBannerPath());
    }
  }

  public static void approveBrandLogo(BrandApproveRequest brandWip, String brandCode, String brandLogoSourcePath,
      String brandLogoFinalPath) throws Exception {
    File file = getFile(brandWip, brandLogoSourcePath, brandWip.getBrandLogoPath());
    if (file.exists()) {
      checkIfDirectoryExists(brandCode, brandLogoFinalPath);
      moveImageToFinalPath(brandWip, brandCode, brandLogoSourcePath, brandLogoFinalPath, brandWip.getBrandLogoPath());
    }
  }

  private static void moveImageToFinalPath(BrandApproveRequest brandWip, String brandCode,
      String sourcePath, String finalPath, String fileName) throws IOException {
    Files.move(Paths.get(
        new StringBuilder(sourcePath).append(SLASH_SEPARATOR).append(brandWip.getBrandRequestCode())
            .append(SLASH_SEPARATOR).append(fileName).toString()), Paths.get(
        new StringBuilder(finalPath).append(SLASH_SEPARATOR).append(brandCode).append(SLASH_SEPARATOR)
            .append(fileName).toString()), StandardCopyOption.REPLACE_EXISTING);
  }

  private static void checkIfDirectoryExists(String brandCode, String profileBannerFinalPath) {
    File directory =
        new File(new StringBuilder(profileBannerFinalPath).append(SLASH_SEPARATOR).append(brandCode).toString());
    if (!directory.exists()) {
      directory.mkdirs();
    }
  }

  public static File getFile(BrandApproveRequest brandWip, String sourcePath, String fileName) {
    return new File((new StringBuilder().append(sourcePath).append(SLASH_SEPARATOR).append(brandWip.getBrandRequestCode())
        .append(SLASH_SEPARATOR).append(fileName)).toString());
  }

  public static void createFile(MultipartFile newFile, String locationPath) throws Exception {
    File file = new File(locationPath);
    File directory = file.getParentFile();
    if (!directory.exists()) {
      directory.mkdirs();
    }
    byte[] bytes = newFile.getBytes();
    try (BufferedOutputStream bufferedOutputStream = new BufferedOutputStream(new FileOutputStream(file))) {
      bufferedOutputStream.write(bytes);
    }
  }

  public static void validate_images(MultipartFile image1, MultipartFile image2) throws Exception {
    if (Objects.nonNull(image1) && !image1.isEmpty()) {
      ImageHelper.validateImages(image1.getBytes());
    }
    if (Objects.nonNull(image2) && !image2.isEmpty()) {
      ImageHelper.validateImages(image2.getBytes());
    }
  }

  public static void mountBrandFile(String code, MultipartFile file, String filePath, String basePath)
      throws Exception {
    if (Objects.nonNull(file) && !file.isEmpty()) {
      String locationPath =
          new StringBuilder(basePath).append(SLASH_SEPARATOR).append(code).append(SLASH_SEPARATOR).append(filePath)
              .toString();
      ImageHelper.createFile(file, locationPath);
    }
  }

  public static void deleteImages(String imageLocationPath, String basePath) {
    if (!imageLocationPath.startsWith(File.separator)) {
      imageLocationPath = File.separator + imageLocationPath;
    }
    log.info("Deleting image from the location : {}", basePath + imageLocationPath);
    File image = new File(basePath + imageLocationPath);
    if (image.exists()) {
      image.delete();
    } else {
      log.error("Image not found");
    }
  }
}

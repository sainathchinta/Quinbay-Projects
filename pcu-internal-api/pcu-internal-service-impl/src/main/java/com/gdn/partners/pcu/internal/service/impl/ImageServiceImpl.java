package com.gdn.partners.pcu.internal.service.impl;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;

import com.gdn.partners.pcu.internal.model.ErrorMessages;
import com.gdn.partners.pcu.internal.service.FileStorageService;
import com.gdn.partners.pcu.internal.service.impl.helper.ImageValidator;
import com.gdn.partners.pcu.internal.service.impl.helper.RequestHelper;
import com.gdn.partners.pcu.internal.service.model.UploadAttributeImageRequest;
import com.gdn.partners.pcu.internal.web.model.request.UploadImageRequest;
import com.gdn.x.product.exception.ApiIncorrectInputDataException;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.gdn.mta.product.util.GdnRestSimpleResponse;
import com.gdn.partners.pcu.internal.client.feign.XGPFeign;
import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.partners.pcu.internal.properties.ImageProperties;
import com.gdn.partners.pcu.internal.properties.SystemParameterProperties;
import com.gdn.partners.pcu.internal.service.ImageService;
import com.gdn.partners.pcu.internal.service.impl.helper.ResponseHelper;

import lombok.extern.slf4j.Slf4j;
import org.springframework.web.multipart.MultipartFile;

import static com.gdn.partners.pcu.internal.model.ErrorMessages.ERROR_ON_IMAGE_UPLOAD;
import static com.gdn.partners.pcu.internal.model.ErrorMessages.ERROR_ON_IMAGE_UPLOAD_CODE;

@Service
@Slf4j
public class ImageServiceImpl implements ImageService {
  private static final String SLASH_SEPARATOR = "/";
  private static final String FULL_IMAGE_TYPE = "full";
  private static final String ESCAPE_SPACE = "\"";
  private static final String WRITE_LOG_MSG = "Time taken upto write: {}";
  private static final String EXCEPTION_ATTR_ERR_MSG =
      "Exception while writing content into file image {}";

  @Autowired
  private ImageProperties imageProperties;

  @Autowired
  private SystemParameterProperties systemParameterProperties;

  @Autowired
  private FileStorageService fileStorageService;

  @Value("${image.upload.size.threshold.mb}")
  private long imageUploadSizeThresholdInMB;

  @Override
  public boolean checkImageSize(String imageFileName, boolean active) throws IOException {
    int imageSize;
    String[] splitImageFilenameByDash = imageFileName.split(SLASH_SEPARATOR);
    if (active) {
      imageSize = fileStorageService.getImageSize(imageFileName);
    } else {
      imageSize = fileStorageService.checkImageSize(imageFileName, splitImageFilenameByDash,
        new StringBuilder(imageProperties.getSourceDirectory()));
    }
    return imageSize <= imageProperties.getMaxReviewSize();
  }

  @Override
  public void uploadDocument(String documentFileName, byte[] request, String sellerCode, String brandCode)
      throws Exception {
    String directory =
        this.systemParameterProperties.getDirectoryBrandAuthDocPath() + SLASH_SEPARATOR + brandCode + Constants.HYPHEN
            + sellerCode;
    StringBuilder path =
        new StringBuilder(directory + File.separator + documentFileName.replace(ESCAPE_SPACE, StringUtils.EMPTY));
    File file = new File(path.toString());
    if (!file.getParentFile().exists()) {
      file.getParentFile().mkdir();
    }
    try (BufferedOutputStream bufferedOutputStream = new BufferedOutputStream(new FileOutputStream(file))) {
      bufferedOutputStream.write(request);
    }
  }

  @Override
  public String uploadAttributeImage(MultipartFile image, String imageFilename) throws Exception {
    log.info("upload attribute image service call, fileName : {}", imageFilename);
    long methodEntryTimeStamp = System.currentTimeMillis();
    try {
      ImageValidator.validateImageFile(image);
      ImageValidator.validateImageExtensionAndSize(image.getOriginalFilename().toLowerCase(),
          image.getBytes(), image.getSize(), imageUploadSizeThresholdInMB);
      UploadAttributeImageRequest request =
          RequestHelper.toUploadAttributeImageRequest(imageFilename, image.getBytes(),
              image.getOriginalFilename().toLowerCase());
      String finalPath = fileStorageService.uploadAttributeImages(request);
      long imageWrittenTimeStamp = System.currentTimeMillis();
      log.info(WRITE_LOG_MSG, imageWrittenTimeStamp - methodEntryTimeStamp);
      return finalPath;
    } catch (Exception e) {
      log.error(EXCEPTION_ATTR_ERR_MSG, imageFilename, e);
      throw new ApiIncorrectInputDataException(
          String.format(ERROR_ON_IMAGE_UPLOAD, imageFilename),
          ERROR_ON_IMAGE_UPLOAD_CODE);
    }
  }

  @Override
  public boolean uploadImage(UploadImageRequest request) {
    log.debug("upload image service call, fileName : {}", request.getImageFileName());
    boolean success;
    long methodEntryTimeStamp = System.currentTimeMillis();
    if (request.isActive()) {
      try {
        fileStorageService.uploadActiveImages(request);
        success = true;
        long imageWrittenTimeStamp = System.currentTimeMillis();
        log.info(WRITE_LOG_MSG, imageWrittenTimeStamp - methodEntryTimeStamp);
      } catch (Exception e) {
        log.error(ErrorMessages.EXCEPTION_ERR_MSG, request.getImageFileName(),
            request.getProductCode(), e);
        throw new ApiIncorrectInputDataException(
            String.format(ErrorMessages.ERROR_ON_IMAGE_UPLOAD, request.getImageFileName()),
            ErrorMessages.ERROR_ON_IMAGE_UPLOAD_CODE);
      }
    } else {
      try {
        String sourceImagepath = fileStorageService.uploadImage(request);
        long imageWrittenTimeStamp = System.currentTimeMillis();
        log.info(WRITE_LOG_MSG, imageWrittenTimeStamp - methodEntryTimeStamp);
        log.debug("Path where image is uploaded , filePath :{}", sourceImagepath);
        success = true;
      } catch (Exception e) {
        log.error(ErrorMessages.EXCEPTION_ERR_MSG, request.getImageFileName(),
            request.getProductCode(), e);
        throw new ApiIncorrectInputDataException(
            String.format(ErrorMessages.ERROR_ON_IMAGE_UPLOAD, request.getImageFileName()),
            ErrorMessages.ERROR_ON_IMAGE_UPLOAD_CODE);
      }
    }
    return success;
  }
}


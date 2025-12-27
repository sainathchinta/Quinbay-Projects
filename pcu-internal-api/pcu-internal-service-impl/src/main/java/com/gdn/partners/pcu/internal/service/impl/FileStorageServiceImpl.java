package com.gdn.partners.pcu.internal.service.impl;

import static com.gdn.partners.pbp.commons.constants.Constants.DELIMITER_SLASH;
import static com.gdn.partners.pcu.internal.model.ErrorMessages.ERROR_ON_IMAGE_UPLOAD;
import static com.gdn.partners.pcu.internal.model.ErrorMessages.ERROR_ON_IMAGE_UPLOAD_CODE;
import static com.gdn.partners.pcu.internal.service.model.BulkInternalProcessType.getBulkInternalProcessType;

import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.image.BufferedImage;
import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.util.GdnRestSimpleResponse;
import com.gdn.partners.pcu.internal.client.feign.XGPFeign;
import com.gdn.partners.pcu.internal.model.ErrorMessages;
import com.gdn.partners.pcu.internal.properties.ImageProperties;
import com.gdn.partners.pcu.internal.service.impl.exception.ImageValidationException;

import com.gdn.partners.pcu.internal.service.model.UploadAttributeImageRequest;
import com.gdn.partners.pcu.internal.web.model.request.UploadImageRequest;
import com.gdn.x.product.exception.ApiIncorrectInputDataException;
import org.apache.commons.imaging.formats.jpeg.exif.ExifRewriter;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.partners.pcu.internal.properties.GCSProperties;
import com.gdn.partners.pcu.internal.properties.SystemParameterProperties;
import com.gdn.partners.pcu.internal.service.FileStorageService;
import com.gdn.partners.pcu.internal.service.GCSService;
import com.gdn.partners.pcu.internal.service.impl.helper.ImageHelper;
import com.gdn.partners.pcu.internal.service.impl.helper.RequestHelper;
import com.gdn.partners.pcu.internal.service.impl.helper.ResponseHelper;
import com.gdn.x.productcategorybase.dto.brand.BrandApproveRequest;
import com.gdn.x.productcategorybase.dto.brand.UpdateBrandRequest;
import com.google.cloud.storage.Bucket;

import lombok.extern.slf4j.Slf4j;

import javax.imageio.ImageIO;

@Service
@Slf4j
public class FileStorageServiceImpl implements FileStorageService {

  @Autowired
  private GCSService gcsService;
  @Autowired
  private GCSProperties gcsProperties;
  @Autowired
  private SystemParameterProperties systemParameterProperties;

  @Autowired
  private XGPFeign xgpFeign;

  @Autowired
  @Qualifier("bulkBucket")
  private Bucket bulkBucket;

  @Autowired
  @Qualifier("sourceImageBucket")
  private Bucket sourceImageBucket;

  @Autowired
  @Qualifier("finalImageBucket")
  private Bucket finalImageBucket;

  @Autowired
  private ImageProperties imageProperties;

  @Autowired
  @Qualifier("attributeBucket")
  private Bucket attributeBucket;

  @Value("#{${internal.templates.gcs.path}}")
  private Map<String, String> internalTemplatesGcsPath;

  @Autowired
  @Qualifier("brandBucket")
  private Bucket brandBucket;

  private static final String SLASH_SEPARATOR = "/";
  private static final String ESCAPE_SPACE = "\"";
  private static final String DOUBLE_SLASH = "//";
  private static final String JPEG = "jpeg";
  private static final String JPG = "jpg";

  private static final String FULL_IMAGE_TYPE = "full";

  public String uploadFilePath(MultipartFile multipartFile, String requestId, String bulkInternalProcessType)
      throws Exception {
    String baseDirPath = null;
    if (gcsProperties.isEnabled()) {
      byte[] byteArr = multipartFile.getBytes();
      baseDirPath = gcsService.uploadCreatedFile(bulkBucket,
          getBaseDirectoryPath(bulkInternalProcessType) + (requestId) + (DELIMITER_SLASH)
              + (multipartFile.getOriginalFilename()), byteArr)
          .replace(multipartFile.getOriginalFilename(), StringUtils.EMPTY);
    } else {
      baseDirPath = getBaseDirectoryPath(bulkInternalProcessType) + (DELIMITER_SLASH) + (requestId) + (DELIMITER_SLASH);
      RequestHelper.createDirIfNotExists(baseDirPath);
      multipartFile.transferTo(new File(baseDirPath + multipartFile.getOriginalFilename()));
    }
    return baseDirPath;
  }

  private String getBaseDirectoryPath(String bulkProcessType) {
    switch (getBulkInternalProcessType(bulkProcessType)) {
      case BULK_INTERNAL_PROCESS_TYPE: {
        return gcsProperties.isEnabled() ?
            systemParameterProperties.getGcsInternalUploadPath() :
            systemParameterProperties.getBulkUploadFilePath();
      }
      case BULK_INTERNAL_SUSPENSION_TYPE: {
        return gcsProperties.isEnabled() ?
            systemParameterProperties.getGcsSuspensionUploadPath() :
            systemParameterProperties.getBulkProductSuspensionFilePath();
      }
      case INTERNAL_STORE_COPY: {
        return gcsProperties.isEnabled() ?
            systemParameterProperties.getGcsStoreCopyUploadPath() :
            systemParameterProperties.getStoreCopyUploadFilePath();
      }
      case INTERNAL_SALES_CATEGORY_UPDATE: {
        return gcsProperties.isEnabled() ?
            systemParameterProperties.getGcsSalesCategoryUploadPath() :
            systemParameterProperties.getSalesCategoryUploadFilePath();
      }
      case INTERNAL_RECAT: {
        return gcsProperties.isEnabled() ?
            systemParameterProperties.getGcsRecatUploadPath() :
            systemParameterProperties.getRecatUploadFilePath();
      }
      case INTERNAL_BULK_CONFIGURATION: {
        return gcsProperties.isEnabled() ?
            systemParameterProperties.getGcsBulkConfigUploadPath() :
            systemParameterProperties.getBulkConfigUploadFilePath();
      }
      case INTERNAL_VENDOR_BULK: {
        return gcsProperties.isEnabled() ?
            systemParameterProperties.getGcsVendorUploadPath() :
            systemParameterProperties.getVendorBulkAssignFilePath();
      }
      case RESTRICTED_KEYWORD_UPSERT: {
        return systemParameterProperties.getGcsRestrictedKeywordUpsertUploadPath();
      }
      case RESTRICTED_KEYWORD_DELETE: {
        return systemParameterProperties.getGcsRestrictedKeywordDeleteUploadPath();
      }
      case BRAND_AUTH_ADD: {
        return systemParameterProperties.getBulkBrandAuthAddUploadPath();
      }
      case BRAND_AUTH_DELETE: {
        return systemParameterProperties.getBulkBrandAuthDeleteUploadPath();
      }
      case BULK_APPROVAL: {
        return systemParameterProperties.getBulkReviewApprovalUploadPath();
      }
      case BULK_REJECTION: {
        return systemParameterProperties.getBulkReviewRejectionUploadPath();
      }
      case MASTER_SKU_BULK_ASSIGNEE: {
        return systemParameterProperties.getMasterSkuBulkAssigneeUploadPath();
      }
      case MASTER_SKU_BULK_REVIEW: {
        return systemParameterProperties.getMasterSkuBulkReviewUploadPath();
      }
      case AUTO_APPROVED_PRODUCTS_BULK_ASSIGN: {
        return systemParameterProperties.getAutoApprovedBulkAssignUploadPath();
      }
      case BULK_PRICE_UPDATE: {
        return systemParameterProperties.getBulkPriceUpdateUploadPath();
      }
      case IPR_PORTAL_BULK_ADD_REVIEW: {
        return systemParameterProperties.getIprBulkAddReviewUploadPath();
      }
      default:
        return StringUtils.EMPTY;
    }
  }

  @Override
  public Map<String, String> getInternalDownloadTemplateFilePaths() {
    return internalTemplatesGcsPath;
  }

  @Override
  public int checkImageSize(String imageFileName, String[] splitImageFilenameByDash,
    StringBuilder sourceDirectory) throws IOException {
    File imageFile;
    byte[] imageContent;
    if (imageFileName.contains(gcsProperties.getPathPrefix())) {
      String fileDownloadPath =
        gcsProperties.getSourceImageDirectory().concat(SLASH_SEPARATOR).concat(imageFileName)
          .replaceAll("//", "/");
      imageContent = gcsService.downloadFile(gcsProperties.getSourceImageBucketName(), fileDownloadPath);
      return imageContent.length;
    } else {
      if (splitImageFilenameByDash.length > 2) {
        imageFile = new File(sourceDirectory.append(SLASH_SEPARATOR)
          .append(splitImageFilenameByDash[splitImageFilenameByDash.length - 3])
          .append(SLASH_SEPARATOR)
          .append(splitImageFilenameByDash[splitImageFilenameByDash.length - 2])
          .append(SLASH_SEPARATOR)
          .append(splitImageFilenameByDash[splitImageFilenameByDash.length - 1]).toString());
      } else if (splitImageFilenameByDash.length > 1) {
        imageFile = new File(sourceDirectory.append(SLASH_SEPARATOR)
          .append(splitImageFilenameByDash[splitImageFilenameByDash.length - 2])
          .append(SLASH_SEPARATOR)
          .append(splitImageFilenameByDash[splitImageFilenameByDash.length - 1]).toString());
      } else {
        imageFile = new File(sourceDirectory.append(SLASH_SEPARATOR)
          .append(splitImageFilenameByDash[splitImageFilenameByDash.length - 1]).toString());
      }
      if (imageFile.exists()) {
        try (InputStream bufferedInputStream = new BufferedInputStream(
          new FileInputStream(imageFile))) {
          imageContent = new byte[(int) imageFile.length()];
          bufferedInputStream.read(imageContent);
        }
      } else {
        throw new ImageValidationException(ErrorMessages.IMAGE_NOT_FOUND);
      }
    }
    return imageContent.length;
  }

  @Override
  public int getImageSize(String imageFileName) throws IOException {
    int imageSize;
    imageSize = checkImageSizeForFinalImage(imageFileName);
    return imageSize;
  }

  @Override
  public void uploadBrandAuthDoc(String fileName, String brandCode, String sellerCode,
    byte[] multipartFile) throws Exception {
    String baseDirPath = systemParameterProperties.getGcsBulkBasePath().replace("/","");
    baseDirPath = baseDirPath.concat(SLASH_SEPARATOR).concat(brandCode).concat(SLASH_SEPARATOR)
      .concat(sellerCode).concat(SLASH_SEPARATOR).concat(fileName);
    gcsService.uploadCreatedFile(bulkBucket, baseDirPath, multipartFile);
  }
  @Override
  public byte[] checkIfFileExisting(String fileName, String brandCode, String sellerCode) {
    String baseDirPath = systemParameterProperties.getGcsBulkBasePath().replace("/", "");
    baseDirPath = baseDirPath.concat(SLASH_SEPARATOR).concat(brandCode).concat(SLASH_SEPARATOR)
      .concat(sellerCode).concat(SLASH_SEPARATOR).concat(fileName);
    return gcsService.downloadFile(gcsProperties.getBucketName(), baseDirPath);
  }

  private int checkImageSizeForFinalImage(String imageFileName) throws IOException {
    byte[] imageContent = new byte[0];
    int imageSize;
    String fileDownloadPath = gcsProperties.getFinalImageDirectory().concat(SLASH_SEPARATOR)
        .concat(gcsProperties.getFinalFullImageDirectory()).concat(SLASH_SEPARATOR).concat(imageFileName)
        .replaceAll("//", "/");
    try {
      imageContent = gcsService.downloadFile(gcsProperties.getFinalImageBucketName(), fileDownloadPath);
      return imageContent.length;
    } catch (Exception e) {
      log.info("File not found in GCS now checking in filestore. imageFileName : {} ", imageFileName, e);
      GdnRestSimpleResponse<String> response =
          xgpFeign.checkImageSizeByImageFilename(SLASH_SEPARATOR + FULL_IMAGE_TYPE + imageFileName);
      ResponseHelper.validateResponse(response);
      imageSize = response.getValue().getBytes().length;
      return imageSize;
    }
  }

  @Override
  public String uploadBrandFileToGcs(MultipartFile multipartFile, String locationPath) throws Exception {
    if (StringUtils.isNotEmpty(locationPath) && (Objects.nonNull(multipartFile) && !multipartFile.isEmpty())) {
      gcsService.uploadCreatedFile(brandBucket, locationPath, multipartFile.getBytes());
    }
    return locationPath;
  }

  @Override
  public void createBrandLogoFile(BrandApproveRequest brandApproveRequest, MultipartFile brandLogo,
      String brandRequestCode) throws Exception {
    if (gcsProperties.isBrandGcsEnabled()) {
      String path = generateBrandFileSourcePath(brandApproveRequest, brandRequestCode, true);
      uploadBrandFileToGcs(brandLogo, path);
    } else {
      ImageHelper.mountBrandFile(brandApproveRequest.getBrandRequestCode(), brandLogo,
          brandApproveRequest.getBrandLogoPath(), systemParameterProperties.getDirectoryBrandLogoSource());
    }
  }

  @Override
  public void createBrandProfileBannerFile(BrandApproveRequest brandApproveRequest, MultipartFile profileBanner,
      String brandRequestCode) throws Exception {
    if (gcsProperties.isBrandGcsEnabled()) {
      String path = generateBrandFileSourcePath(brandApproveRequest, brandRequestCode, false);
      uploadBrandFileToGcs(profileBanner, path);
    } else {
      ImageHelper.mountBrandFile(brandApproveRequest.getBrandRequestCode(), profileBanner,
          brandApproveRequest.getProfileBannerPath(), systemParameterProperties.getDirectoryProfileBannerSource());
    }
  }

  @Override
  public void updateBrandFiles(UpdateBrandRequest updateBrandRequest, MultipartFile brandLogo,
      MultipartFile profileBanner, String brandRequestCode) throws Exception {
    if (gcsProperties.isBrandGcsEnabled()) {
      String brandLogoPath = generateBrandFileFinalPathForUpdate(updateBrandRequest, brandRequestCode, true);
      String profileBannerPath = generateBrandFileFinalPathForUpdate(updateBrandRequest, brandRequestCode, false);
      uploadBrandFileToGcs(brandLogo, brandLogoPath);
      uploadBrandFileToGcs(profileBanner, profileBannerPath);
    } else {
      ImageHelper.mountBrandFile(updateBrandRequest.getBrandCode(), brandLogo, updateBrandRequest.getBrandLogoPath(),
          systemParameterProperties.getDirectoryBrandLogoFinal());
      ImageHelper.mountBrandFile(updateBrandRequest.getBrandCode(), profileBanner,
          updateBrandRequest.getProfileBannerPath(), systemParameterProperties.getDirectoryProfileBannerFinal());
    }
  }

  @Override
  public void deleteUpdatedBrandLogo(String locationPath, String brandRequestCode) throws Exception {
    if (gcsProperties.isBrandGcsEnabled()) {
      String logoPath = new StringBuilder().append(gcsProperties.getBrandFinalDirectory()).append(Constants.SLASH)
          .append(gcsProperties.getBrandLogoDirectory()).append(Constants.SLASH).append(brandRequestCode)
          .append(Constants.SLASH).append(locationPath.replaceAll(DOUBLE_SLASH, SLASH_SEPARATOR)).toString();
      gcsService.deleteFile(gcsProperties.getBrandBucketName(), logoPath);
    } else {
      ImageHelper.deleteImages(locationPath, systemParameterProperties.getDirectoryBrandLogoFinal());
    }
  }

  @Override
  public void approveBrandLogo(BrandApproveRequest brandApproveRequest, String brandRequestCode,
      MultipartFile brandLogo, String brandCode) throws Exception {
    if(gcsProperties.isBrandGcsEnabled()){
      String path = generateBrandFileSourcePath(brandApproveRequest, brandRequestCode, true);
      byte[] brandLogoFile = getBrandImageFromGcsOrFileStore(brandApproveRequest, brandRequestCode, true);
      if (Objects.nonNull(brandLogoFile)) {
        String finalPath = generateBrandFileFinalPath(brandApproveRequest, brandCode, true);
        gcsService.uploadCreatedFile(brandBucket, finalPath, brandLogoFile);
        gcsService.deleteFile(gcsProperties.getBrandBucketName(), path);
      }
    } else {
      ImageHelper.approveBrandLogo(brandApproveRequest, brandCode,
          systemParameterProperties.getDirectoryBrandLogoSource(),
          systemParameterProperties.getDirectoryBrandLogoFinal());
    }
  }


  @Override
  public void approveProfileBanner(BrandApproveRequest brandApproveRequest, String brandRequestCode,
      MultipartFile profileBanner, String brandCode) throws Exception {
    if (gcsProperties.isBrandGcsEnabled()) {
      String path = generateBrandFileSourcePath(brandApproveRequest, brandRequestCode, false);
      byte[] brandProfileBannerFile = getBrandImageFromGcsOrFileStore(brandApproveRequest, brandRequestCode, false);
      if (Objects.nonNull(brandProfileBannerFile)) {
        String finalPath = generateBrandFileFinalPath(brandApproveRequest, brandCode, false);
        gcsService.uploadCreatedFile(brandBucket, finalPath, brandProfileBannerFile);
        gcsService.deleteFile(gcsProperties.getBrandBucketName(), path);
      }
    } else {
      ImageHelper.approveProfileBanner(brandApproveRequest, brandCode,
          systemParameterProperties.getDirectoryProfileBannerSource(),
          systemParameterProperties.getDirectoryProfileBannerFinal());
    }
  }

  private byte[] getBrandImageFromGcsOrFileStore(BrandApproveRequest brandApproveRequest, String brandRequestCode,
      boolean isBrandLogo) throws IOException {
    String path = generateBrandFileSourcePath(brandApproveRequest, brandRequestCode, isBrandLogo);
    try {
      byte[] brandImage = gcsService.downloadFile(gcsProperties.getBrandBucketName(), path);
      return brandImage;
    } catch (ApplicationRuntimeException e) {
      log.error("Error while downloading brand image. Path : {} ", path, e);
      File file = new File(generateBrandFilestoreSourcePath(brandApproveRequest, brandRequestCode, isBrandLogo));
      if (file.exists()) {
        byte[] brandImage = FileUtils.readFileToByteArray(file);
        return brandImage;
      }
      return null;
    }
  }

  private String generateBrandFileSourcePath(BrandApproveRequest brandApproveRequest, String brandRequestCode,
      boolean isBrandLogo) {
    if (isBrandLogo) {
      return new StringBuilder().append(gcsProperties.getBrandSourceDirectory()).append(Constants.SLASH)
          .append(gcsProperties.getBrandLogoDirectory()).append(Constants.SLASH).append(brandRequestCode)
          .append(Constants.SLASH)
          .append(brandApproveRequest.getBrandLogoPath().replaceAll(DOUBLE_SLASH, SLASH_SEPARATOR)).toString();
    } else {
      return new StringBuilder().append(gcsProperties.getBrandSourceDirectory()).append(Constants.SLASH)
          .append(gcsProperties.getBrandProfileBannerDirectory()).append(Constants.SLASH).append(brandRequestCode)
          .append(Constants.SLASH)
          .append(brandApproveRequest.getProfileBannerPath().replaceAll(DOUBLE_SLASH, SLASH_SEPARATOR)).toString();
    }
  }

  private String generateBrandFilestoreSourcePath(BrandApproveRequest brandApproveRequest, String brandRequestCode,
      boolean isBrandLogo) {
    if (isBrandLogo) {
      return new StringBuilder().append(systemParameterProperties.getDirectoryBrandLogoSource()).append(SLASH_SEPARATOR)
          .append(brandRequestCode).append(SLASH_SEPARATOR).append(brandApproveRequest.getBrandLogoPath()).toString()
          .replaceAll(DOUBLE_SLASH, SLASH_SEPARATOR);
    } else {
      return new StringBuilder().append(systemParameterProperties.getDirectoryProfileBannerSource())
          .append(SLASH_SEPARATOR).append(brandRequestCode).append(SLASH_SEPARATOR)
          .append(brandApproveRequest.getProfileBannerPath()).toString().replaceAll(DOUBLE_SLASH, SLASH_SEPARATOR);
    }
  }

  private String generateBrandFileFinalPath(BrandApproveRequest brandApproveRequest, String brandRequestCode,
      boolean isBrandLogo) {
    if (isBrandLogo) {
      return new StringBuilder().append(gcsProperties.getBrandFinalDirectory()).append(Constants.SLASH)
          .append(gcsProperties.getBrandLogoDirectory()).append(Constants.SLASH).append(brandRequestCode)
          .append(Constants.SLASH)
          .append(brandApproveRequest.getBrandLogoPath().replaceAll(DOUBLE_SLASH, SLASH_SEPARATOR)).toString();
    } else {
      return new StringBuilder().append(gcsProperties.getBrandFinalDirectory()).append(Constants.SLASH)
          .append(gcsProperties.getBrandProfileBannerDirectory()).append(Constants.SLASH).append(brandRequestCode)
          .append(Constants.SLASH)
          .append(brandApproveRequest.getProfileBannerPath().replaceAll(DOUBLE_SLASH, SLASH_SEPARATOR)).toString();
    }
  }

  private String generateBrandFileFinalPathForUpdate(UpdateBrandRequest updateBrandRequest, String brandRequestCode,
      boolean isBrandLogo) {
    if (isBrandLogo) {
      return new StringBuilder().append(gcsProperties.getBrandFinalDirectory()).append(Constants.SLASH)
          .append(gcsProperties.getBrandLogoDirectory()).append(Constants.SLASH).append(brandRequestCode)
          .append(Constants.SLASH)
          .append(updateBrandRequest.getBrandLogoPath().replaceAll(DOUBLE_SLASH, SLASH_SEPARATOR)).toString();
    } else {
      return new StringBuilder().append(gcsProperties.getBrandFinalDirectory()).append(Constants.SLASH)
          .append(gcsProperties.getBrandLogoDirectory()).append(Constants.SLASH).append(brandRequestCode)
          .append(Constants.SLASH)
          .append(updateBrandRequest.getBrandLogoPath().replaceAll(DOUBLE_SLASH, SLASH_SEPARATOR)).toString();
    }
  }

  @Override
  public byte[] getBrandImage(String brandCode, String brandLogoPath, boolean isBandApproved, boolean isBrandLogo) {
    StringBuilder brandImagePath = new StringBuilder();
    if (isBandApproved) {
      if (isBrandLogo) {
        brandImagePath.append(gcsProperties.getBrandFinalDirectory()).append(Constants.SLASH)
            .append(gcsProperties.getBrandLogoDirectory()).append(Constants.SLASH).append(brandCode)
            .append(Constants.SLASH).append(brandLogoPath.replaceAll(DOUBLE_SLASH, SLASH_SEPARATOR));
      } else {
        brandImagePath.append(gcsProperties.getBrandFinalDirectory()).append(Constants.SLASH)
            .append(gcsProperties.getBrandProfileBannerDirectory()).append(Constants.SLASH).append(brandCode)
            .append(Constants.SLASH).append(brandLogoPath.replaceAll(DOUBLE_SLASH, SLASH_SEPARATOR));
      }
    } else {
      if (isBrandLogo) {
        brandImagePath.append(gcsProperties.getBrandSourceDirectory()).append(Constants.SLASH)
            .append(gcsProperties.getBrandLogoDirectory()).append(Constants.SLASH).append(brandCode)
            .append(Constants.SLASH).append(brandLogoPath.replaceAll(DOUBLE_SLASH, SLASH_SEPARATOR));
      } else {
        brandImagePath.append(gcsProperties.getBrandSourceDirectory()).append(Constants.SLASH)
            .append(gcsProperties.getBrandProfileBannerDirectory()).append(Constants.SLASH).append(brandCode)
            .append(Constants.SLASH).append(brandLogoPath.replaceAll(DOUBLE_SLASH, SLASH_SEPARATOR));
      }
    }
    return gcsService.downloadFile(gcsProperties.getBrandBucketName(),
        brandImagePath.toString().replaceAll(DOUBLE_SLASH, SLASH_SEPARATOR));
  }

  @Override
  public String uploadAttributeImages(UploadAttributeImageRequest request) {
    String randomId = UUID.randomUUID().toString();
    String path = clearDoubleSlashFromPath(
        gcsProperties.getAttributeImageDirectory() + File.separator
            + gcsProperties.getAttributePathPrefix() + File.separator + randomId + File.separator
            + request.getImageFileName().replace(ESCAPE_SPACE, StringUtils.EMPTY));
    try (ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream()) {
      byte[] bytes =
          uploadImage(request.getOriginalFileType(), request.getBytes(), byteArrayOutputStream);
      gcsService.uploadCreatedFile(attributeBucket, path, bytes);
      return path;
    } catch (Exception e) {
      log.error("Exception while writing content into file image {} ", request.getImageFileName(),
          e);
      throw new ApiIncorrectInputDataException(
          String.format(ERROR_ON_IMAGE_UPLOAD, request.getImageFileName()),
          ERROR_ON_IMAGE_UPLOAD_CODE);
    }
  }

  private String clearDoubleSlashFromPath(String path) {
    return path.replaceAll(DOUBLE_SLASH, SLASH_SEPARATOR);
  }

  private byte[] uploadImage(String originalFileType, byte[] requestBytes,
      ByteArrayOutputStream byteArrayOutputStream) throws Exception {
    if (JPEG.equalsIgnoreCase(originalFileType) || JPG.equalsIgnoreCase(
        originalFileType)) {
      new ExifRewriter().removeExifMetadata(requestBytes, byteArrayOutputStream);
    } else {
      byteArrayOutputStream.write(requestBytes);
    }
    return byteArrayOutputStream.toByteArray();
  }

  @Override
  public void uploadActiveImages(UploadImageRequest request) {
    try {
      uploadFullActiveImage(request);
      uploadResizeActiveImageFile(Constants.MEDIUM_IMAGE,
          request.getImageFileName().replace(ESCAPE_SPACE, StringUtils.EMPTY),
          Constants.UPLOAD_FINAL_IMAGE_MEDIUM_WIDTH, Constants.UPLOAD_FINAL_IMAGE_MEDIUM_HEIGHT,
          request.getBytes());
      uploadResizeActiveImageFile(Constants.THUMBNAIL_IMAGE,
          request.getImageFileName().replace(ESCAPE_SPACE, StringUtils.EMPTY),
          Constants.UPLOAD_FINAL_IMAGE_THUMBNAIL_WIDTH,
          Constants.UPLOAD_FINAL_IMAGE_THUMBNAIL_HEIGHT, request.getBytes());
    } catch (Exception e) {
      log.error("Exception while writing content into file image {} for productCode : {} ",
          request.getImageFileName(), request.getProductCode(), e);
      throw new ApiIncorrectInputDataException(
          String.format(ErrorMessages.ERROR_ON_IMAGE_UPLOAD, request.getImageFileName()),
          ErrorMessages.ERROR_ON_IMAGE_UPLOAD_CODE);
    }
  }

  private void uploadFullActiveImage(UploadImageRequest request) throws Exception {
    if (gcsProperties.isFinalImageEnabled()) {
      String path = clearDoubleSlashFromPath(gcsProperties.getFinalImageDirectory() + File.separator
          + gcsProperties.getFinalFullImageDirectory() + File.separator
          + gcsProperties.getPathPrefix() + File.separator + request.getImageFileName()
          .replace(ESCAPE_SPACE, StringUtils.EMPTY));
      try (ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream()) {
        byte[] bytes =
            uploadImage(request.getOriginalFileType(), request.getBytes(), byteArrayOutputStream);
        gcsService.uploadCreatedFile(finalImageBucket, path, bytes);
      }
    } else {
      String path = clearDoubleSlashFromPath(
          imageProperties.getFullPath() + File.separator + request.getImageFileName()
              .replace(ESCAPE_SPACE, StringUtils.EMPTY));
      File newFile = new File(path);
      newFile.getParentFile().mkdirs();
      try (BufferedOutputStream bufferedOutputStream = new BufferedOutputStream(
          new FileOutputStream(newFile))) {
        if (JPEG.equalsIgnoreCase(request.getOriginalFileType()) || JPG.equalsIgnoreCase(
            request.getOriginalFileType())) {
          new ExifRewriter().removeExifMetadata(request.getBytes(), bufferedOutputStream);
        } else {
          bufferedOutputStream.write(request.getBytes());
        }
      }
    }
  }

  private void uploadResizeActiveImageFile(String param, String imageName, int width, int height,
      byte[] imageData) throws Exception {
    Integer extensionIndex = imageName.lastIndexOf(Constants.DOT);
    BufferedImage resizedImage = resizeImage(imageName, width, height, imageData);
    if (gcsProperties.isFinalImageEnabled()) {
      String path = getActiveResizedImagePathForGcsEnabled(param, imageName);
      try (ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream()) {
        ImageIO.write(resizedImage, imageName.substring(extensionIndex + 1), byteArrayOutputStream);
        byte[] bytes = byteArrayOutputStream.toByteArray();
        gcsService.uploadCreatedFile(finalImageBucket, path, bytes);
      }
    } else {
      String path = getActiveResizedmagePathForGcsDisabled(param, imageName);
      File file = new File(path);
      file.getParentFile().mkdir();
      ImageIO.write(resizedImage, imageName.substring(extensionIndex + 1), file);
    }
  }

  private BufferedImage resizeImage(String imageName, int width, int height, byte[] imageData)
      throws IOException {
    InputStream inputStream = new ByteArrayInputStream(imageData);
    BufferedImage inputImage = ImageIO.read(inputStream);
    BufferedImage outputImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);
    Graphics2D mediumGraphic = outputImage.createGraphics();
    mediumGraphic.setRenderingHint(RenderingHints.KEY_INTERPOLATION,
        RenderingHints.VALUE_INTERPOLATION_BICUBIC);
    mediumGraphic.clearRect(0, 0, width, height);
    mediumGraphic.drawImage(inputImage, 0, 0, width, height, null);
    mediumGraphic.dispose();
    return outputImage;
  }

  private String getActiveResizedImagePathForGcsEnabled(String param, String imageName) {
    if (Constants.MEDIUM_IMAGE.equals(param)) {
      return clearDoubleSlashFromPath(gcsProperties.getFinalImageDirectory() + File.separator
          + gcsProperties.getFinalMediumImageDirectory() + File.separator
          + gcsProperties.getPathPrefix() + File.separator + imageName);
    } else {
      return clearDoubleSlashFromPath(gcsProperties.getFinalImageDirectory() + File.separator
          + gcsProperties.getFinalThumbnailImageDirectory() + File.separator
          + gcsProperties.getPathPrefix() + File.separator + imageName);
    }
  }

  private String getActiveResizedmagePathForGcsDisabled(String param, String imageName) {
    if (Constants.MEDIUM_IMAGE.equals(param)) {
      return clearDoubleSlashFromPath(imageProperties.getMediumPath() + File.separator + imageName);
    } else {
      return clearDoubleSlashFromPath(
          imageProperties.getThumbnailPath() + File.separator + imageName);
    }
  }

  @Override
  public String uploadImage(UploadImageRequest uploadImageRequest) throws Exception {
    String path = StringUtils.EMPTY;
    if (gcsProperties.isSourceImageEnabled()) {
      path = gcsProperties.getSourceImageDirectory().concat(Constants.ROOT)
          .concat(gcsProperties.getPathPrefix()).concat(Constants.ROOT)
          .concat(uploadImageRequest.getImageFileName()).replace(ESCAPE_SPACE, StringUtils.EMPTY);
      gcsService.uploadCreatedFile(sourceImageBucket, path, uploadImageRequest.getBytes());
    } else {
      path = imageProperties.getBasePath().concat(Constants.ROOT)
          .concat(uploadImageRequest.getImageFileName()).concat(Constants.ROOT)
          .replace(ESCAPE_SPACE, StringUtils.EMPTY);
      File file = new File(path);
      file.getParentFile().mkdir();
      try (BufferedOutputStream bufferedOutputStream = new BufferedOutputStream(
          new FileOutputStream(file))) {
        if (JPEG.equalsIgnoreCase(uploadImageRequest.getOriginalFileType()) || JPG.equalsIgnoreCase(
            uploadImageRequest.getOriginalFileType())) {
          new ExifRewriter().removeExifMetadata(uploadImageRequest.getBytes(),
              bufferedOutputStream);
        } else {
          bufferedOutputStream.write(uploadImageRequest.getBytes());
        }
      }
    }
    return path;
  }
}


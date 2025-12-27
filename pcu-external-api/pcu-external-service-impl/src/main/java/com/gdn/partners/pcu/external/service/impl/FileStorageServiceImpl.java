package com.gdn.partners.pcu.external.service.impl;

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
import java.time.Duration;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;

import javax.imageio.ImageIO;

import com.gda.mta.product.dto.ProductLevel3SummaryDetailsImageRequest;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.partners.pcu.external.service.model.request.FullImageUploadRequest;
import com.gdn.partners.pcu.external.service.model.request.MediumImageUploadRequest;
import com.gdn.partners.pcu.external.service.model.request.ThumbNailImageUploadRequest;
import com.gdn.partners.core.security.Credential;
import com.gdn.partners.core.security.exception.UnauthorizedException;
import com.gdn.partners.pcu.external.model.Accessibilty;
import com.gdn.partners.pcu.external.service.model.request.UploadAttributeImageRequest;
import com.gdn.partners.pcu.external.service.model.request.XgpImageScaleRequest;
import com.gdn.partners.pcu.external.streaming.model.bulk.BulkProcessEntity;
import com.gdn.partners.pcu.external.web.model.request.ProcessFileType;
import com.gdn.partners.pcu.external.web.model.request.SignedUrlRequest;
import com.gdn.partners.pcu.external.web.model.response.ApiErrorCode;
import com.gdn.partners.pcu.external.web.model.response.FilePath;
import com.gdn.partners.pcu.external.web.model.response.SignedUrlResponse;
import com.gdn.x.product.exception.ApiIncorrectInputDataException;
import org.apache.commons.imaging.formats.jpeg.exif.ExifRewriter;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.stereotype.Component;
import org.springframework.web.multipart.MultipartFile;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.partners.pcu.external.model.Constants;
import com.gdn.partners.pcu.external.model.ErrorMessages;
import com.gdn.partners.pcu.external.properties.GCSProperties;
import com.gdn.partners.pcu.external.properties.ImageProperties;
import com.gdn.partners.pcu.external.properties.SystemParameterProperties;
import com.gdn.partners.pcu.external.service.FileStorageService;
import com.gdn.partners.pcu.external.service.GCSService;
import com.gdn.partners.pcu.external.service.impl.helper.ExcelTemplateUtil;
import com.gdn.partners.pcu.external.service.impl.helper.RequestHelper;
import com.gdn.partners.pcu.external.service.model.request.UploadImageRequest;
import com.gdn.x.productcategorybase.dto.brand.CreateBrandWipRequest;
import com.google.cloud.storage.Blob;
import com.google.cloud.storage.Bucket;
import com.google.common.io.Files;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@Component
public class FileStorageServiceImpl implements FileStorageService {

  private static final String DOT = ".";
  private static final String XLSX_EXTENSION = ".xlsx";
  private static final String XLSM_EXTENSION = ".xlsm";
  private static final String ZIP = ".zip";
  private static final String PDF_EXTENSION = ".pdf";

  private static final String ESCAPE_SPACE = "\"";
  private static final String JPEG = "jpeg";
  private static final String JPG = "jpg";
  private static final String WEBP = "webp";
  private static final String WEBP_EXTENSION = ".webp";
  private static final String INTERNAL = "INTERNAL";
  private static final String SLASH_SEPARATOR = "/";
  private static final String DOUBLE_SLASH = "//";

  @Autowired
  private GCSProperties gcsProperties;

  @Autowired
  private GCSService gcsService;

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
  @Qualifier("brandBucket")
  private Bucket brandBucket;

  @Autowired
  @Qualifier("attributeBucket")
  private Bucket attributeBucket;

  @Autowired
  @Qualifier("iprBucket")
  private Bucket iprBucket;

  @Value("${file.upload.allowed.type}")
  private String fileUploadAllowedType;

  @Autowired
  private ImageProperties imageProperties;

  @Autowired
  private SystemParameterProperties systemParameterProperties;

  @Value("${excel.template.upload.size.threshold.mb}")
  private long excelTemplateUploadSizeThresholdInMB;

  @Value("${bulk.master.data.upload.location}")
  private String bulkMasterDataUploadLocation;

  @Value("${external.creation.data.upload.location}")
  private String externalCreationDataUploadLocation;

  @Value("${signed.url.duration}")
  private long signedUrlDuration;

  @Value("${signed.url.upload.file.types}")
  private String signedUrlUploadFileTypes;

  @Value("${image.formats.supported}")
  private List<String> imageFormatsSupported;

  @Override
  public List<MultipartFile> downloadMultiPartFile(String username, List<String> filenames)
    throws Exception {
    List<MultipartFile> multipartFileList;
    if (gcsProperties.isEnabled()) {
      multipartFileList = getListOfMultipartFile(username, filenames);
    } else {
      multipartFileList = ExcelTemplateUtil.getMultipartFiles(username, filenames);
    }
    return multipartFileList;
  }

  @Override
  public String uploadFileToGcs(MultipartFile multipartFile, String username) throws Exception {
    RequestHelper.validateSizeForExcelUploads(multipartFile, excelTemplateUploadSizeThresholdInMB);
    String excelFileName = generateExcelFileName(multipartFile);
    if (StringUtils.isNotEmpty(excelFileName)) {
      String filePath = generateFileName(multipartFile.getOriginalFilename(), username);
      gcsService.uploadCreatedFile(bulkBucket, filePath, multipartFile.getBytes());
    } else {
      log.error("Uploading template of invalid format: {}", multipartFile.getOriginalFilename());
      throw new ApiIncorrectInputDataException(ApiErrorCode.INVALID_EXCEL_FILE.getDesc(),
        ApiErrorCode.INVALID_EXCEL_FILE.getCode());
    }
    return excelFileName;
  }

  @Override
  public String uploadEvidenceFileToGcs(MultipartFile multipartFile, String productSku,
      String businessPartnerCode)
      throws Exception {
    String filePath = StringUtils.EMPTY;
    if (StringUtils.isNotBlank(multipartFile.getOriginalFilename())) {
      String fileExtension = multipartFile.getOriginalFilename()
          .substring(multipartFile.getOriginalFilename().lastIndexOf(DOT));
      if (fileUploadAllowedType.contains(fileExtension)) {
        filePath = String.valueOf(generateIprFileName(productSku, businessPartnerCode,
            multipartFile.getOriginalFilename(), fileExtension));
        gcsService.uploadCreatedFile(iprBucket, filePath, multipartFile.getBytes());
      } else {
        log.error("Uploading template of invalid format: {}", multipartFile.getOriginalFilename());
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
            ErrorMessages.PDF_FILE_TYPE_INVALID_MESSAGE);
      }
    }
    return filePath;
  }

  @Override
  public FilePath processFileUpload(MultipartFile multipartFile, String productSku,
      String processFileType, String businessPartnerCode) throws Exception {
    if (processFileType.equals(ProcessFileType.EVIDENCE_FILE.getValue())) {
      return FilePath.builder().filePath(this.uploadEvidenceFileToGcs(multipartFile, productSku,
              businessPartnerCode))
          .build();
    }
    return new FilePath();
  }

  @Override
  public SignedUrlResponse generateSignedUrl(SignedUrlRequest signedUrlRequest) {
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(signedUrlRequest.getFileName()),
      ErrorMessages.FILE_NAME_CANNOT_BE_EMPTY);
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(signedUrlRequest.getProcessType()),
      ErrorMessages.PROCESS_TYPE_CANNOT_BE_EMPTY);
    try {
      if (BulkProcessEntity.PRODUCT_BASIC_INFO.name().equals(signedUrlRequest.getProcessType())
          || BulkProcessEntity.EXTERNAL_UPLOAD.name().equals(signedUrlRequest.getProcessType())) {
          return buildSignedUrlResponse(signedUrlRequest.getProcessType(), signedUrlRequest.getFileName());
      } else if (signedUrlUploadFileTypes.contains(signedUrlRequest.getProcessType())) {
        List<String> accessibility = Arrays.asList(Credential.getAccessibilities());
        if (!accessibility.contains(Accessibilty.STORE_PRODUCT_PRODUCT_CREATION_BULK_CREATION)) {
          throw new UnauthorizedException(ErrorMessages.UNAUTHORIZED_ERR_MESSAGE);
        }
        String bulkProcessCode = UUID.randomUUID().toString();
        Duration uploadDuration = Duration.ofMinutes(signedUrlDuration);
        String filePath =
          generateFileName(signedUrlRequest.getFileName(), signedUrlRequest.getUsername());
        String signedUrl = gcsService.generateSignedUrl(bulkBucket, filePath, uploadDuration);
        long expiresAtMillis = System.currentTimeMillis() + uploadDuration.toMillis();
        return SignedUrlResponse.builder().signedUrl(signedUrl).expiresAt(expiresAtMillis)
          .uploadedPath(filePath).bulkProcessCode(bulkProcessCode).build();
      }
      else {
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          ErrorMessages.INVALID_PROCESS_TYPE);
      }
    } catch (Exception e) {
      log.error("Error generating GCS signed URL for request {} , message {} ", signedUrlRequest,
        e.getMessage(), e);
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED,
        ErrorMessages.FAILED_TO_GENERATE_SIGNED_URL, e);
    }
  }

  @Override
  public String uploadSubjectToVatFile(MultipartFile multipartFile, String bulkProcessCode)
    throws Exception {
    String fileName;
    if (gcsProperties.isEnabled()) {
      if (ExcelTemplateUtil.isFileXlsxType(multipartFile)) {
        fileName =
          gcsProperties.getVatUploadPath() + bulkProcessCode + Constants.ROOT + bulkProcessCode
            + XLSX_EXTENSION;
        log.info("Uploading the VAT file at GCS. File name : {}",
          multipartFile.getOriginalFilename());
        gcsService.uploadCreatedFile(bulkBucket, fileName, multipartFile.getBytes());
      } else {
        log.error("Uploading template of invalid format: {}", multipartFile.getOriginalFilename());
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          ErrorMessages.EXCEL_FILE_TYPE_INVALID_VAT_MESSAGE);
      }
    } else {
      fileName = ExcelTemplateUtil.updateSubjectToVatTemplate(multipartFile,
        systemParameterProperties.getDirectoryVatFilepath(), bulkProcessCode);
    }
    return fileName;
  }

  private List<MultipartFile> getListOfMultipartFile(String username, List<String> fileNames)
    throws Exception {
    List<MultipartFile> multiPartFileList = new ArrayList<>();
    for (String fileName : fileNames) {
      String originalFileName = generateFileName(fileName, username);
      Blob blob = gcsService.downloadFile(gcsProperties.getBucketName(), originalFileName);
      if (Objects.nonNull(blob)) {
        multiPartFileList.add(new MockMultipartFile(fileName, originalFileName, null,
          new ByteArrayInputStream(blob.getContent())));
      }
    }
    return multiPartFileList;
  }

  private static String generateExcelFileName(MultipartFile multipart) {
    String excelFileName = StringUtils.EMPTY;
    if (multipart.getOriginalFilename().substring(multipart.getOriginalFilename().lastIndexOf(DOT))
        .equals(XLSX_EXTENSION) || multipart.getOriginalFilename()
        .substring(multipart.getOriginalFilename().lastIndexOf(DOT)).equals(XLSM_EXTENSION)
        || multipart.getOriginalFilename().substring(multipart.getOriginalFilename().lastIndexOf(DOT)).equals(ZIP)) {
      excelFileName = multipart.getOriginalFilename();
    }
    return excelFileName;
  }

  public String generateFileName(String fileName, String username) {
    return gcsProperties.getDataUploadPath() + Constants.ROOT + username + Constants.ROOT
      + fileName;
  }

  public String generateFileUploadPath(String prefixFilePath, String bulkProcessCode,
    String fileName) {
    return String.join(Constants.ROOT, prefixFilePath, bulkProcessCode, fileName);
  }

  private SignedUrlResponse buildSignedUrlResponse(String processType, String fileName) {
    String bulkProcessCode = UUID.randomUUID().toString();
    Duration uploadDuration = Duration.ofMinutes(signedUrlDuration);
    String uploadLocation = StringUtils.EMPTY;
    String uploadPath = StringUtils.EMPTY;
    if (BulkProcessEntity.EXTERNAL_UPLOAD.name().equals(processType)) {
      uploadLocation = externalCreationDataUploadLocation;
      uploadPath = generateFileUploadPath(uploadLocation, bulkProcessCode, bulkProcessCode + ZIP);
    } else {
      uploadLocation = bulkMasterDataUploadLocation;
      uploadPath = generateFileUploadPath(uploadLocation, bulkProcessCode, fileName);
    }

    String signedUrl = gcsService.generateSignedUrl(bulkBucket, uploadPath, uploadDuration);
    long expiresAtMillis = System.currentTimeMillis() + uploadDuration.toMillis();

    return SignedUrlResponse.builder().signedUrl(signedUrl).expiresAt(expiresAtMillis)
        .uploadedPath(uploadPath).bulkProcessCode(bulkProcessCode).build();
  }

  public StringBuilder generateIprFileName(String productSku, String businessPartCode, String fileName,
      String fileExtension) {
    return new StringBuilder(gcsProperties.getIprDataUploadPath() + Constants.ROOT + businessPartCode + Constants.ROOT
        + productSku + Constants.ROOT + fileName.substring(Constants.ZERO, fileName.lastIndexOf(DOT)).trim()
        .replaceAll(Constants.SPACE_REGEX, StringUtils.EMPTY) + Constants.HYPHEN + UUID.randomUUID()
        .toString() + fileExtension);
  }

  @Override
  public Map<String, String> getExternalDownloadTemplateFilePaths() {
    String basePath = StringUtils.EMPTY;
    if (gcsProperties.isEnabled()) {
      basePath = systemParameterProperties.getGcsBulkBasePath();
    } else {
      basePath = systemParameterProperties.getFileStoreBulkBasePath();
    }
    Map<String, String> externalDownloadTemplateFilePaths = new HashMap<>();
    externalDownloadTemplateFilePaths.put(Constants.PRODUCT_ARCHIEVE_TEMPLATE,
        basePath.concat(systemParameterProperties.getArchieveTemplate()));
    externalDownloadTemplateFilePaths.put(Constants.INSTORE,
        basePath.concat(systemParameterProperties.getInstoreTemplate()));
    externalDownloadTemplateFilePaths.put(Constants.L5_DELETE,
        basePath.concat(systemParameterProperties.getL5DeleteTemplate()));
    externalDownloadTemplateFilePaths.put(Constants.VAT, basePath.concat(systemParameterProperties.getVatTemplate()));
    externalDownloadTemplateFilePaths.put(Constants.LOGISTICS,
        basePath.concat(systemParameterProperties.getLogisticsTemplate()));
    return externalDownloadTemplateFilePaths;
  }


  @Override
  public String uploadImage(UploadImageRequest uploadImageRequest) throws Exception {
    String path = StringUtils.EMPTY;
    if (gcsProperties.isSourceImageEnabled()) {
      path = gcsProperties.getSourceImageDirectory().concat(Constants.ROOT).concat(gcsProperties.getPathPrefix())
          .concat(Constants.ROOT).concat(uploadImageRequest.getImageFileName())
          .replace(ESCAPE_SPACE, StringUtils.EMPTY);
      gcsService.uploadCreatedFile(sourceImageBucket, path, uploadImageRequest.getBytes());
    } else {
      path = imageProperties.getBasePath().concat(Constants.ROOT).concat(uploadImageRequest.getImageFileName())
          .concat(Constants.ROOT).replace(ESCAPE_SPACE, StringUtils.EMPTY);
      File file = new File(path);
      file.getParentFile().mkdir();
      try (BufferedOutputStream bufferedOutputStream = new BufferedOutputStream(new FileOutputStream(file))) {
        if (JPEG.equalsIgnoreCase(uploadImageRequest.getOriginalFileType()) || JPG.equalsIgnoreCase(
            uploadImageRequest.getOriginalFileType())) {
          new ExifRewriter().removeExifMetadata(uploadImageRequest.getBytes(), bufferedOutputStream);
        } else {
          bufferedOutputStream.write(uploadImageRequest.getBytes());
        }
      }
    }
    return path;
  }

  @Override
  public String getImagePathPrefix() {
    return gcsProperties.getPathPrefix();
  }

  @Override
  public byte[] downloadFile(String fileName) throws Exception {
    String fileDownloadPath =
        gcsProperties.getSourceImageDirectory().concat(Constants.ROOT).concat(fileName).replaceAll("//", "/");
    return gcsService.downloadFile(gcsProperties.getSourceImageBucketName(), fileDownloadPath).getContent();
  }

  @Override
  public byte[] getImageContent(String fileName) throws Exception {
    byte[] imageContent;
    imageContent = getImageFromImagePath(
        (gcsProperties.getFinalImageDirectory() + File.separator + gcsProperties.getFinalSeoulImageDirectory()
            + File.separator + gcsProperties.getPathPrefix()).replaceAll("//", "/"));
    if (ArrayUtils.isEmpty(imageContent)) {
      imageContent = getByteArrayFromImage(imageProperties.getSeoul(), fileName);
    }
    return imageContent;
  }

  @Override
  public byte[] getImageFromImagePath(String imagePath) throws Exception {
    if (!gcsProperties.isFileStoreToGcsMigrationCompleted()) {
      Blob blob = gcsService.downloadFile(gcsProperties.getFinalImageBucketName(),
          (gcsProperties.getFinalImageDirectory() + File.separator + gcsProperties.getFinalFullImageDirectory()
              + File.separator + imagePath).replaceAll("//", "/"));
      if (Objects.nonNull(blob)) {
        return blob.getContent();
      } else {
        log.info("File not found in Gcs imagePath: {} ", imagePath);
        return getFileData(imagePath);
      }
    } else {
      Blob blob = gcsService.downloadFile(gcsProperties.getFinalImageBucketName(),
          (gcsProperties.getFinalImageDirectory() + File.separator + gcsProperties.getFinalFullImageDirectory()
              + File.separator + imagePath).replaceAll("//", "/"));
      return blob.getContent();
    }
  }

  private byte[] getFileData(String imagePath) throws IOException {
    String filePath = (imageProperties.getFullPath() + File.separator + imagePath).replaceAll("//", "/");
    log.info("Filpath : {} ", filePath);
    File file = new File((imageProperties.getFullPath() + File.separator + imagePath).replaceAll("//", "/"));
    if (file.exists()) {
      return Files.toByteArray(file);
    } else {
      return null;
    }
  }

  private byte[] getByteArrayFromImage(String path, String fileName) throws IOException {
    File file = new File(path + fileName);
    byte[] imageContent;
    if (file.exists()) {
      try (InputStream bufferedInputStream = new BufferedInputStream(new FileInputStream(file))) {
        imageContent = new byte[(int) file.length()];
        bufferedInputStream.read(imageContent);
      }
    } else {
      return null;
    }
    return imageContent;
  }

  @Override
  public void uploadActiveImages(UploadImageRequest request) {
    try {
      uploadFullActiveImage(request);
      uploadResizeActiveImageFile(Constants.MEDIUM_IMAGE,
          request.getImageFileName().replace(ESCAPE_SPACE, StringUtils.EMPTY),
          Constants.UPLOAD_FINAL_IMAGE_MEDIUM_WIDTH, Constants.UPLOAD_FINAL_IMAGE_MEDIUM_HEIGHT, request.getBytes());
      uploadResizeActiveImageFile(Constants.THUMBNAIL_IMAGE,
          request.getImageFileName().replace(ESCAPE_SPACE, StringUtils.EMPTY),
          Constants.UPLOAD_FINAL_IMAGE_THUMBNAIL_WIDTH, Constants.UPLOAD_FINAL_IMAGE_THUMBNAIL_HEIGHT,
          request.getBytes());
    } catch (Exception e) {
      log.error("Exception while writing content into file image {} for productCode : {} ", request.getImageFileName(),
          request.getProductCode(), e);
      throw new ApiIncorrectInputDataException(
        String.format(ApiErrorCode.ERROR_ON_IMAGE_UPLOAD.getDesc(), request.getImageFileName()),
        ApiErrorCode.ERROR_ON_IMAGE_UPLOAD.getCode());
    }
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
          String.format(ApiErrorCode.ERROR_ON_IMAGE_UPLOAD.getDesc(), request.getImageFileName()),
          ApiErrorCode.ERROR_ON_IMAGE_UPLOAD.getCode());
    }
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
  public String generatePath(String productCode, String filename) {
    if (gcsProperties.isFinalImageEnabled()) {
      return (gcsProperties.getPathPrefix() + Constants.ROOT + productCode + Constants.ROOT + filename
          + Constants.JPEG).replaceAll("//", "/");
    } else {
      return (productCode + Constants.ROOT + filename + Constants.JPEG).replaceAll("//","/");
    }
  }

  @Override
  public XgpImageScaleRequest generateXgpImageScaleRequest(UploadImageRequest uploadImageRequest) {
    int extensionIndex = uploadImageRequest.getImageFileName().lastIndexOf(DOT);
    if (imageFormatsSupported.contains(Constants.WEBP_FORMAT)) {
      String webpFileName = uploadImageRequest.getImageFileName().substring(0, extensionIndex) + Constants.WEBP_EXTENSION;
      uploadImageRequest.setImageFileName(webpFileName);
    }
    XgpImageScaleRequest xgpImageScaleRequest = new XgpImageScaleRequest();
    FullImageUploadRequest fullImageUploadRequest = new FullImageUploadRequest();
    fullImageUploadRequest.setImagePath(
        clearDoubleSlashFromPath(generateFinalImagePath(uploadImageRequest)));
    MediumImageUploadRequest mediumImageUploadRequest = new MediumImageUploadRequest();
    mediumImageUploadRequest.setImagePath(clearDoubleSlashFromPath(
        getActiveResizedImagePathForGcsEnabled(Constants.MEDIUM_IMAGE,
            uploadImageRequest.getImageFileName())));
    ThumbNailImageUploadRequest thumbNailImageUploadRequest = new ThumbNailImageUploadRequest();
    thumbNailImageUploadRequest.setImagePath(clearDoubleSlashFromPath(
        getActiveResizedImagePathForGcsEnabled(Constants.THUMBNAIL_IMAGE,
            uploadImageRequest.getImageFileName())));
    xgpImageScaleRequest.setMediumImageUploadRequest(mediumImageUploadRequest);
    xgpImageScaleRequest.setFullImageUploadRequest(fullImageUploadRequest);
    xgpImageScaleRequest.setThumbNailImageUploadRequest(thumbNailImageUploadRequest);
    xgpImageScaleRequest.setImageBytes(uploadImageRequest.getBytes());
    return xgpImageScaleRequest;
  }

  private void uploadFullActiveImage(UploadImageRequest request) throws Exception {
    if (gcsProperties.isFinalImageEnabled()) {
      String path = clearDoubleSlashFromPath(generateFinalImagePath(request));
      try (ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream()) {
        byte[] bytes = uploadImage(request.getOriginalFileType(), request.getBytes(), byteArrayOutputStream);
        gcsService.uploadCreatedFile(finalImageBucket, path, bytes);
      }
    } else {
      String path = clearDoubleSlashFromPath(imageProperties.getFullPath() + File.separator + request.getImageFileName()
          .replace(ESCAPE_SPACE, StringUtils.EMPTY));
      File newFile = new File(path);
      newFile.getParentFile().mkdirs();
      try (BufferedOutputStream bufferedOutputStream = new BufferedOutputStream(new FileOutputStream(newFile))) {
        if (JPEG.equalsIgnoreCase(request.getOriginalFileType()) || JPG.equalsIgnoreCase(
            request.getOriginalFileType())) {
          new ExifRewriter().removeExifMetadata(request.getBytes(), bufferedOutputStream);
        } else {
          bufferedOutputStream.write(request.getBytes());
        }
      }
    }
  }

  private String generateFinalImagePath(UploadImageRequest request) {
    return gcsProperties.getFinalImageDirectory() + File.separator
        + gcsProperties.getFinalFullImageDirectory() + File.separator
        + gcsProperties.getPathPrefix() + File.separator + request.getImageFileName()
        .replace(ESCAPE_SPACE, StringUtils.EMPTY);
  }

  private void uploadResizeActiveImageFile(String param, String imageName, int width, int height, byte[] imageData)
      throws Exception {
    Integer extensionIndex = imageName.lastIndexOf(DOT);
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

  private String getActiveResizedmagePathForGcsDisabled(String param, String imageName) {
    if (Constants.MEDIUM_IMAGE.equals(param)) {
      return clearDoubleSlashFromPath(imageProperties.getMediumPath() + File.separator + imageName);
    } else {
      return clearDoubleSlashFromPath(imageProperties.getThumbnailPath() + File.separator + imageName);
    }
  }

  private String getActiveResizedImagePathForGcsEnabled(String param, String imageName) {
    if (Constants.MEDIUM_IMAGE.equals(param)) {
      return clearDoubleSlashFromPath(
          gcsProperties.getFinalImageDirectory() + File.separator + gcsProperties.getFinalMediumImageDirectory()
              + File.separator + gcsProperties.getPathPrefix() + File.separator + imageName);
    } else {
      return clearDoubleSlashFromPath(
          gcsProperties.getFinalImageDirectory() + File.separator + gcsProperties.getFinalThumbnailImageDirectory()
              + File.separator + gcsProperties.getPathPrefix() + File.separator + imageName);
    }
  }

  private String clearDoubleSlashFromPath(String path) {
    return path.replaceAll("//", "/");
  }

  private BufferedImage resizeImage(String imageName, int width, int height, byte[] imageData) throws IOException {
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

  @Override
  public String uploadBrandFileToGcs(MultipartFile multipartFile, String locationPath) throws Exception {
    if(StringUtils.isNotEmpty(locationPath)) {
      gcsService.uploadCreatedFile(brandBucket, locationPath, multipartFile.getBytes());
    } else {
      log.error("Empty Location Path For Brand: {}", locationPath);
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ErrorMessages.EMPTY_LOCATION_PATH);
    }
    return locationPath;
  }

  @Override
  public void createBrandLogoFile(CreateBrandWipRequest createBrandWipRequest, MultipartFile brandLogo,
      String brandRequestCode) throws Exception {
    if (gcsProperties.isBrandGcsEnabled()) {
      String path = generateBrandFileSourcePath(createBrandWipRequest, brandRequestCode, true);
      uploadBrandFileToGcs(brandLogo, path);
      if (!INTERNAL.equalsIgnoreCase(createBrandWipRequest.getBusinessPartnerCode())) {
        String finalPath = generateBrandFileFinalPath(createBrandWipRequest, brandRequestCode, true);
        uploadBrandFileToGcs(brandLogo, finalPath);
      }
    } else {
      String locationPath = generateBrandFileSourcePath(createBrandWipRequest, brandRequestCode, true);
      RequestHelper.createFile(brandLogo, locationPath);
      if (!INTERNAL.equalsIgnoreCase(createBrandWipRequest.getBusinessPartnerCode())) {
        String finalBrandLogoPath = generateBrandFileFinalPath(createBrandWipRequest, brandRequestCode, true);
        RequestHelper.createFile(brandLogo, finalBrandLogoPath);
      }
    }
  }

  @Override
  public void createBrandProfileBannerFile(CreateBrandWipRequest createBrandWipRequest, MultipartFile profileBanner,
      String brandRequestCode) throws Exception {
    if (gcsProperties.isBrandGcsEnabled()) {
      String profileBannerPath = generateBrandFileSourcePath(createBrandWipRequest, brandRequestCode, false);
      uploadBrandFileToGcs(profileBanner, profileBannerPath);
      if (!INTERNAL.equalsIgnoreCase(createBrandWipRequest.getBusinessPartnerCode())) {
        String profileBannerFinalPath = generateBrandFileFinalPath(createBrandWipRequest, brandRequestCode, false);
        uploadBrandFileToGcs(profileBanner, profileBannerFinalPath);
      }
    } else {
      String locationPath = generateBrandFileSourcePath(createBrandWipRequest, brandRequestCode, false);
      RequestHelper.createFile(profileBanner, locationPath);
      if (!INTERNAL.equalsIgnoreCase(createBrandWipRequest.getBusinessPartnerCode())) {
        String finalBrandProfileBannerPath = generateBrandFileFinalPath(createBrandWipRequest, brandRequestCode, false);
        RequestHelper.createFile(profileBanner, finalBrandProfileBannerPath);
      }
    }
  }

  private String generateBrandFileSourcePath(CreateBrandWipRequest createBrandWipRequest, String brandRequestCode,
      boolean isBrandLogo) {
    if (gcsProperties.isBrandGcsEnabled()) {
      if (isBrandLogo) {
        return new StringBuilder().append(gcsProperties.getBrandSourceDirectory()).append(Constants.ROOT)
            .append(gcsProperties.getBrandLogoDirectory()).append(Constants.ROOT).append(brandRequestCode)
            .append(Constants.ROOT)
            .append(createBrandWipRequest.getBrandLogoPath().replaceAll(DOUBLE_SLASH, SLASH_SEPARATOR)).toString();
      } else {
        return new StringBuilder().append(gcsProperties.getBrandSourceDirectory()).append(Constants.ROOT)
            .append(gcsProperties.getBrandProfileBannerDirectory()).append(Constants.ROOT).append(brandRequestCode)
            .append(Constants.ROOT)
            .append(createBrandWipRequest.getProfileBannerPath().replaceAll(DOUBLE_SLASH, SLASH_SEPARATOR)).toString();
      }
    } else {
      if (isBrandLogo) {
        return new StringBuilder().append(systemParameterProperties.getDirectoryBrandlogoSource())
            .append(Constants.ROOT).append(brandRequestCode).append(Constants.ROOT)
            .append(createBrandWipRequest.getBrandLogoPath()).toString();
      } else {
        return new StringBuilder().append(systemParameterProperties.getDirectoryProfilebannerSource())
            .append(Constants.ROOT).append(brandRequestCode).append(Constants.ROOT)
            .append(createBrandWipRequest.getProfileBannerPath()).toString();
      }
    }
  }

  private String generateBrandFileFinalPath(CreateBrandWipRequest createBrandWipRequest, String brandRequestCode,
      boolean isBrandLogo) {
    if (gcsProperties.isBrandGcsEnabled()) {
      if (isBrandLogo) {
        return new StringBuilder().append(gcsProperties.getBrandFinalDirectory()).append(Constants.ROOT)
            .append(gcsProperties.getBrandLogoDirectory()).append(Constants.ROOT).append(brandRequestCode)
            .append(Constants.ROOT)
            .append(createBrandWipRequest.getBrandLogoPath().replaceAll(DOUBLE_SLASH, SLASH_SEPARATOR)).toString();
      } else {
        return new StringBuilder().append(gcsProperties.getBrandFinalDirectory()).append(Constants.ROOT)
            .append(gcsProperties.getBrandProfileBannerDirectory()).append(Constants.ROOT).append(brandRequestCode)
            .append(Constants.ROOT)
            .append(createBrandWipRequest.getProfileBannerPath().replaceAll(DOUBLE_SLASH, SLASH_SEPARATOR)).toString();
      }
    } else {
      if (isBrandLogo) {
        return new StringBuilder().append(systemParameterProperties.getDirectoryBrandLogoFinal())
            .append(Constants.ROOT).append(brandRequestCode).append(Constants.ROOT)
            .append(createBrandWipRequest.getBrandLogoPath()).toString();
      } else {
        return new StringBuilder().append(systemParameterProperties.getDirectoryProfileBannerFinal())
            .append(Constants.ROOT).append(brandRequestCode).append(Constants.ROOT)
            .append(createBrandWipRequest.getProfileBannerPath()).toString();
      }
    }
  }
}


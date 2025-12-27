package com.gdn.mta.bulk.service;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.gdn.mta.bulk.BulkInternalProcessType;
import com.gdn.mta.bulk.dto.BulkBasicInfoRequest;
import com.gdn.mta.bulk.dto.BulkInternalProcessDTO;
import com.gdn.mta.bulk.dto.BulkInternalUploadRequestDTO;
import com.gdn.mta.bulk.dto.BulkUpdateProcessDTO;
import com.gdn.mta.bulk.dto.QrCodeRowInfo;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.BulkUpdateQueue;
import com.gdn.mta.bulk.models.BulkProcessEntity;
import com.gdn.mta.bulk.models.BulkUploadErrorCounter;
import com.gdn.mta.bulk.models.ImageDownloadResult;
import com.gdn.mta.bulk.models.ProductBasicDetail;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;

import com.google.api.gax.paging.Page;
import com.google.cloud.storage.Blob;

import org.apache.commons.lang3.tuple.Pair;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.openxml4j.exceptions.InvalidFormatException;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.apache.xmlbeans.XmlException;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;
import java.io.InputStream;
import java.text.ParseException;
import java.util.List;
import java.util.Map;
import java.util.Set;

public interface FileStorageService {

  /**
   * @param bulkUpdateProcessDTO
   * @param bulkProcessCode
   * @param fileName
   */
  void createBulkFile(BulkUpdateProcessDTO bulkUpdateProcessDTO, String bulkProcessCode,
    String fileName) throws Exception;

  /**
   *
   * @param bulkProcess
   * @param excelFileType
   * @return
   */
  byte[] downloadFile(BulkProcess bulkProcess, String excelFileType) throws IOException;
  /**
   * Check if Mass Template File Exists
   * @param
   * @param unifiedFileName
   * @return
   */
  boolean isMassTemplateFileExist(String unifiedFileName);

  XSSFWorkbook getUnifiedBaseTemplate(String unifiedBaseTemplateFileName, String templatePath, boolean isEnglishTemplate)
    throws Exception;

  /**
   * getting base path
   * @param bulkProcessType
   * @return
   */
  String getBasePath(String bulkProcessType);

  /**
   * get filePrefix according to gcs switch
   *
   * @param bulkProcessType
   * @return
   */
   String getFilePrefix(String bulkProcessType);

  /**
   *
   * @param bulkUpdateQueue
   * @param bulkProcess
   * @return
   */
   Sheet getFileData(BulkUpdateQueue bulkUpdateQueue, BulkProcess bulkProcess) throws IOException;

  /**
   * Get File by filename and bulk process
   *
   * @param filename must not be empty
   * @param bulkProcess must not be null
   * @return
   * @throws IOException
   */
  Sheet getFileDataByFileName(String filename, BulkProcess bulkProcess) throws IOException;

  /**
   *
   * @param filePath
   * @param bulkProcessType
   * @param bulkProcessCode
   * @param fileName
   * @return
   */
  String getDownloadLink(String filePath, String bulkProcessType, String bulkProcessCode,
    String fileName);

  /**
   *
   * @param bulkProcessCode
   * @return
   */
  String getFailedProductDownloadLink(String bulkProcessCode);
  /**
   * Get Sheet Data based on Bulk Internal process Type
   *
   * @param bulkInternalUploadRequestDTO
   * @throws
   * @return
   */
  Sheet getFileDataWithInternalUploadRequest(BulkInternalUploadRequestDTO bulkInternalUploadRequestDTO) throws IOException;

  /**
   * Check if File Exists using BulkInternalUploadRequest
   *
   * @param bulkInternalUploadRequestDTO
   * @throws
   * @return
   */
  Boolean isFileExists(BulkInternalUploadRequestDTO bulkInternalUploadRequestDTO);

  /**
   * generate bulk file for download
   *
   * @param request
   * @param bytes
   * @return
   */
  String generateFile(BulkDownloadRequest request, byte[] bytes) throws Exception;

  /**
   *
   * @param filePath
   * @return
   */
   byte[] downloadGenericTemplateFile(String filePath)
     throws IOException;

  /**
   *
   * @param businessPartnerCode
   * @param instore
   * @param bulkInternalProcessType
   * @param byteGenericFileData
   * @return
   * @throws Exception
   */
  String createGenericBulkFile(String businessPartnerCode, boolean instore, BulkInternalProcessType bulkInternalProcessType,
    byte[] byteGenericFileData) throws Exception;


  /**
   * @param request
   * @param bulkInternalProcessDTO
   * @param processType
   * @return
   * @throws Exception
   */
  String createBulkInternalFile(BulkDownloadRequest request, BulkInternalProcessDTO bulkInternalProcessDTO, String processType)
      throws Exception;

  /**
   * decompress image zip file and upload to temp location
   * @param bulkProcessCode
   * @param zipImageFile
   */
  void decompressFile(String bulkProcessCode, byte[] zipImageFile) throws Exception;

  /**
   * download images from 3rd party location and save it in temp location
   *
   * @param bulkProcessCode
   * @param imagesToDownload
   * @param httpConnectionTimeout
   * @param httpConnectionReadTimeout
   * @param urlImagesWithInvalidExtension
   * @param images
   * @param bulkUploadErrorCounter
   * @param validationErrorMessage
   * @param isInternationalMerchant
   * @return
   */
  ImageDownloadResult downloadImages(String bulkProcessCode, Map<String, String> imagesToDownload,
    int httpConnectionTimeout, int httpConnectionReadTimeout,
    Set<String> urlImagesWithInvalidExtension, List<String> images,
    BulkUploadErrorCounter bulkUploadErrorCounter, StringBuilder validationErrorMessage,
    boolean isInternationalMerchant);


  /**
   * Downloads images from the provided URLs, validates them for type, size, and resolution,
   * and uploads them to gcs. The method ensures
   * each image is processed only once — performing all validations before uploading.
   *
   * @param bulkProcessCode        the unique code for the current bulk process, used in GCS pathing.
   * @param imagesToDownload       a map of image URLs to image identifiers (names without extension).
   * @param images                 a list to which successfully processed image filenames will be added.
   * @param validationErrorMessage a string builder for collecting all error messages during validation.
   * @param productBasicDetails
   * @return an {@link ImageDownloadResult} object containing the final status,
   * image type, and image file name.
   */
  ImageDownloadResult downloadImageFileValidateAndUploadToGCS(String bulkProcessCode,
      Map<String, String> imagesToDownload, List<String> images, StringBuilder validationErrorMessage,
      List<ProductBasicDetail> productBasicDetails);

  /**
   * Doenlaod and validate images in temp location
   * @param bulkProcess
   * @param bulkUploadErrorCounter
   * @param imageAndImageUrlReverseMap
   * @param validationErrorMessage
   * @param images
   * @param columnRowInformation
   * @param imageMaxSize
   * @param isInternationalMerchant
   * @throws IOException
   */
  boolean downloadAndValidateProductCreationImages(BulkProcess bulkProcess,
      BulkUploadErrorCounter bulkUploadErrorCounter, Map<String, String> imageAndImageUrlReverseMap,
      StringBuilder validationErrorMessage, List<String> images, String columnRowInformation, int imageMaxSize,
      boolean isInternationalMerchant) throws IOException;

  /**
   * upload image from temp location to source folder
   * @param mappingImageEntry
   * @param bulkProcess
   * @param mtaSourcePath
   * @throws IOException
   */
  void uploadImageFilesToSourceLocation(Map.Entry<String, String> mappingImageEntry,
      BulkProcess bulkProcess, String mtaSourcePath) throws Exception;


  /**
   * get email prefix based on gcs switch
   *
   * @return
   */
  String getEmailPrefix();


  /**
   * get notification detail
   *
   * @param request
   * @return
   */
  String getNotificationDetailPath(BulkDownloadRequest request) throws Exception;

  /**
   * get notification type
   * @param bulkProcessEntity
   * @return
   */
  String getNotificationType(BulkProcessEntity bulkProcessEntity);

  /**
   * get download link for need revision deletion
   *
   * @param basePath
   * @param deleteProcessCode
   * @return
   */
  String getDownloadLinkForNeedRevisionDeletion(String basePath, String deleteProcessCode);

  /**
   *
   * @param filePth
   * @param bulkProcessType
   * @param bulkProcessCode
   * @param fileName
   * @return
   */
   String campaignErrorFilePath(String filePth, String bulkProcessType, String bulkProcessCode,
    String fileName);

  /**
   * Write generic Template for Category template regeneration
   * @param workbook
   * @param templatePath
   * @throws
   * @return
   */
  void writeGenericTemplate(String templatePath, XSSFWorkbook workbook) throws Exception;

  /**
   * Regenerate Master Brand Value Sheet with Filetype
   * regeneration
   * @param genericTemplateFileType
   * @throws
   * @return
   */
  Map<String,byte[]> regenerateMasterBrandValuesSheet(String genericTemplateFileType)
      throws XmlException, IOException, InvalidFormatException;

  /**
   * upload generic upload template to gcs
   * @param genericTemplate
   * @param genericEnglishTemplate
   * @param genericTemplateFileType
   * @throws Exception
   */
  void uploadGenericTemplateToGcs(byte[] genericTemplate, byte[] genericEnglishTemplate, String genericTemplateFileType)
      throws Exception;

  /**
   * Check if Category Template Exists else Will create a new Category Template
   * regeneration
   * @param categoryUploadTemplateFile
   * @param categoryBaseTemplateFileEnglish
   * @param categoryBaseTemplateFile
   * @param categoryUploadTemplateFileEnglish
   * @throws
   * @return
   */
  Map<String,byte[]> isCategoryTemplateFileExist(String categoryUploadTemplateFile,
    String categoryUploadTemplateFileEnglish, String categoryBaseTemplateFile,
    String categoryBaseTemplateFileEnglish) throws Exception;

  /**
   * get Category Template Location
   * @throws Exception
   */
  String getCategoryTemplateFilePath();

  /**
   * upload regenerated Templates For generic Bulk Upload And Category Template
   * @param brandValuesSheet
   * @param brandValuesSheetEnglish
   * @param genericTemplateFileType
   * @param isCategoryTemplate
   * @throws Exception
   */
  void uploadRegeneratedTemplates(byte[] brandValuesSheet, byte[] brandValuesSheetEnglish,
    String genericTemplateFileType, boolean isCategoryTemplate) throws Exception;

  /**
   * Check if file exists
   *
   * @param filePath
   * @return
   */
  Boolean checkIfFileExistsByFilePath(String filePath);

  /**
   * copy image from mita-api temp file to gcs
   * @param requestId
   * @param uniqueImageList
   * @param productCode
   * @throws Exception
   */
  void moveTmpImageToProductImage(String requestId, Set<String> uniqueImageList, String productCode)
      throws Exception;

  /**
   *
   * @param workbook
   * @param requestId
   */
  void uploadUnmappedSku(Workbook workbook, String requestId) throws Exception;

  /**
   * generate restricted keyword error file
   *
   * @param errorFilePath
   * @param excelRowNumberAndErrorMessageMapping
   * @return
   */
  void downloadFileAndGenerateErrorFile(String filePath, String errorFilePath,
      List<Pair<Integer, String>> excelRowNumberAndErrorMessageMapping, String processType) throws Exception;

  /**
   * get Error FileLink For Bulk process Listing
   * @param bulkProcess supports process : update/upsert/create
   * @return String Error File
   */
  String getErrorFileLinkForListing(BulkProcess bulkProcess)
    throws JsonProcessingException, ParseException;

  /**
   * Fetch QR code template from GCS
   *
   * @param rowInfo
   * @return vtl string contents of template file
   */
  String fetchQRCodeTemplate(QrCodeRowInfo rowInfo);

  /**
   * Uploads file to GCS bucket at the given {@code filePath}
   *
   * @param filePath must not be a directory
   * @param contentType must not be blank, must match the type of the contents in the stream
   * @param fileContentsStream must not be null
   * @return true if file was uploaded successfully
   * @throws Exception
   */
  boolean uploadFileToGcs(String filePath, String contentType, InputStream fileContentsStream)
      throws Exception;

  /**
   * Downloads file from GCS from the filePath
   * @param filePath
   * @return
   * @throws Exception
   */
  byte[] downloadFileFromGcs(String filePath)  throws Exception;

  /**
   * @param directory must not be blank
   * @return Internally paginated list of files at a given directory.
   */
 Page<Blob> listFilesAtGcsDirectory(String directory);

  /**
   * Uploads a new file to GCS directory with the given {@code newFileName}.
   * If file upload is successful, all other files in the GCS directory
   * will be deleted.
   *
   * @param directory must exist, and must not contain nested directories.
   * @param newFileName must not be blank.
   * @param newFileContents must not be blank.
   * @throws Exception
   */
  String replaceFilesAtGcsDirectory(String directory, String newFileName,
      String contentType, InputStream newFileContents) throws Exception;

  /**
   * download base template for price recommendation
   *
   * @return
   */
  byte[] downloadBaseTemplateForPriceRecommendation();

  /**
   * Upload file to pricing bucket
   *
   * @param filePath
   * @param bytes
   * @throws Exception
   */
  void uploadToPricingBucket(String filePath, byte[] bytes) throws Exception;

  /**
   * upload file to bulk bucket
   *
   * @param filePath
   * @param bytes
   * @throws Exception
   */
  void uploadFileToBulkBucket(String filePath, byte[] bytes) throws Exception;

  /**
   * download base template for bulk master data update
   * @param fileName String
   * @return byte[]
   */
  byte[] downloadBaseTemplateForBulkBasicInfoUpdate(String fileName);

  /**
   * Get Sheet from file name and BulkProcess
   *
   * @param bulkBasicInfoRequest
   * @param bulkProcess
   * @return
   */
  Sheet getFileDataForBasicInfo(BulkBasicInfoRequest bulkBasicInfoRequest, BulkProcess bulkProcess) throws IOException;

  /**
   * Get list of multipart files
   *
   * @param username
   * @param fileNames
   * @return
   */
  List<MultipartFile> getListOfMultipartFile(String username, List<String> fileNames)
      throws Exception;
}

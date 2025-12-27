package com.gdn.mta.bulk.util;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;
import java.util.zip.ZipOutputStream;

import com.gdn.mta.bulk.dto.BulkProcessPath;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.StringUtils;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.BulkInternalProcessType;
import com.gdn.mta.bulk.dto.BulkInternalProcessUploadRequest;
import com.gdn.mta.bulk.models.BulkProcessEntity;
import com.gdn.mta.bulk.models.FileType;
import com.gdn.partners.bulk.util.Constant;
import com.google.common.collect.ImmutableMap;

public class ProcessorUtils {

  private static final Logger LOGGER = LoggerFactory.getLogger(ProcessorUtils.class);

  public static final String X_BULK_DIRECTORY = "/x-bulk/";
  public static final String X_BULK= "x-bulk";
  public static final String FILE_PREFIX = "filePrefix";
  public static final String DATA_BASE_DIR = (!StringUtils.isEmpty(System.getenv().get("DATA_DIR")) ? System.getenv().get("DATA_DIR") : "target") + X_BULK_DIRECTORY;
  public static final String BULK_UPDATE_DIR = DATA_BASE_DIR + "productUpdate/";
  public static final String WORK_ORDER_UPLOAD_DIR = DATA_BASE_DIR + "workOrderUpload/";
  public static final String DOWNLOAD_FAILED_MPP_PRODUCT_DIR = DATA_BASE_DIR + "downloadMpp/";
  public static final String BULK_VAT_UPDATE_DIR = DATA_BASE_DIR + "subjectToVat/";
  public static final String BULK_UPDATE_ARCHIVE_DIR = DATA_BASE_DIR + "BulkArchive/";
  public static final String BULK_SUSPENSION_DIR = DATA_BASE_DIR + "productSuspension/";
  public static final String BULK_PRICE_REBATE_DIR = DATA_BASE_DIR + "rebate/";
  public static final String BULK_PRICE_PRODUCT_TYPE_TAGGING_DIR =
    DATA_BASE_DIR + BulkProcessPath.BULK_PRICE_PRODUCT_TYPE_TAGGING.getValue();
  public static final String BULK_CONFIGURATION_DIR = DATA_BASE_DIR + "configurationUpload/";
  public static final String BULK_VENDOR_ASSIGN = DATA_BASE_DIR + "vendorAssign/";
  public static final String BULK_CAMPAIGN_PRODUCT_CREATE_DIR = DATA_BASE_DIR + "createCampaignProduct/";
  public static final String BULK_CAMPAIGN_ERROR_PRODUCT_CREATE_DIR =
    DATA_BASE_DIR + "campaignErrorProduct/";
  public static final String BULK_CAMPAIGN_PRODUCT_DOWNLOAD_DIR = DATA_BASE_DIR + "downloadCampaignProduct/";
  public static final String BULK_INSTANT_PICKUP_PRODUCT_DOWNLOAD_DIR = DATA_BASE_DIR + "downloadInstantPickupProduct/";
  public static final String BULK_INSTANT_PICKUP_PRODUCT_UPSERT_DIR = DATA_BASE_DIR + "upsertInstantPickupProduct/";
  public static final String BULK_INSTANT_PICKUP_PRODUCT_DELETE_DIR = DATA_BASE_DIR + "deleteInstantPickupProduct/";
  public static final String BULK_DOWNLOAD_DIR = DATA_BASE_DIR + "downloadProducts/";
  public static final String BULK_DOWNLOAD_EAN_DIR = DATA_BASE_DIR + "downloadProducts/ean/";
  public static final String BULK_DOWNLOAD_BASIC_INFO_DIR = DATA_BASE_DIR + "downloadProductsBasicInfo/";
  public static final String BULK_APPROVAL = DATA_BASE_DIR + "bulkApproval/";
  public static final String BULK_REJECTION= DATA_BASE_DIR + "bulkRejection/";
  public static final String BULK_DOWNLOAD_DIR_FOR_MASTER_PRODUCT = DATA_BASE_DIR + "downloadMasterProducts/";
  public static final String BULK_FAILED_PRODUCT_DIR = DATA_BASE_DIR + "downloadFailedProducts/";
  public static final String BULK_ORDER_DOWNLOAD_DIR = DATA_BASE_DIR + "downloadOrders/";
  public static final String BULK_PRODUCT_VENDOR_DIR = DATA_BASE_DIR + "downloadProductVendors/";
  public static final String BULK_PRODUCT_REVIEW_DOWNLOAD = DATA_BASE_DIR + "downloadProductReview/";
  public static final String BULK_RECAT_DIR = DATA_BASE_DIR + "recat/";
  public static final String BULK_STORE_COPY_DIR = DATA_BASE_DIR + "storeCopy/";
  public static final String BULK_STORE_COPY_DIR_UPLOADS = DATA_BASE_DIR + "storeCopyUploads/";
  public static final String DELETE_BRAND_AUTHORISATION_DIR_UPLOADS = DATA_BASE_DIR + "deleteBrandAuthUploads/";
  public static final String BULK_STORE_COPY_UPLOAD_TEMPLATE_DIR = DATA_BASE_DIR + "storeCopyUploadTemplates/";
  public static final String BULK_SALES_CATEGORY_UPDATE_DIR = DATA_BASE_DIR + "salesCategoryUpdate/";
  public static final String BULK_BRAND_AUTHORIZATION_TEMPLATE_DIR = DATA_BASE_DIR + "brandAuthorizationUploadTemplates/";
  public static final String DELETE_UPDATE_PICKUP_POITS_TEMPLATE_DIR = DATA_BASE_DIR + "deleteUpdatePickupPointsTemplates/";
  public static final String MASTER_SKU_ITEMS_DOWNLOAD_TEMPLATE_DIR =
      DATA_BASE_DIR + "masterSkuAllItemsDownloadTemplates/";
  public static final String AUTO_APPROVED_PRODUCTS_DOWNLOAD_TEMPLATE_DIR =
      DATA_BASE_DIR + "autoApprovedProductsDownloadTemplates/";
  public static final String BULK_PRICE_RECOMMENDATION_DOWNLOAD_TEMPLATE_DIR =
      DATA_BASE_DIR + "bulkPriceRecommendationDownloadTemplates/";
  public static final String BULK_DOWNLOAD_TAGGED_PRODUCTS_TEMPLATE_DIR =
      DATA_BASE_DIR + "taggedProducts/";
  public static final String MASTER_SKU_IN_REVIEW_DOWNLOAD_TEMPLATE_DIR =
    DATA_BASE_DIR + "masterSkuInReviewDownloadTemplates/";

  public static final String IPR_PRODUCT_DOWNLOAD_TEMPLATE_DIR =
      DATA_BASE_DIR + "iprProductsDownloadTemplates/";

  public static final String BULK_PRICE_UPDATE = DATA_BASE_DIR + "bulkPriceUpdate/";

  public static final String DATA_RAW_DIR = "raw";
  public static final String FILETYPE_EXCEL = ".xls";
  public static final String FILETYPE_XLSX_EXCEL = ".xlsx";
  public static final String FILETYPE_XLSM_EXCEL = ".xlsm";
  public static final String FILETYPE_CSV = ".csv";
  public static final String FILETYPE_PDF = ".pdf";

  public static final DecimalFormat REMOVE_DENOMINATOR_DECIMAL_FORMAT = new DecimalFormat("#");


  private ProcessorUtils() {
  }

  public static final ImmutableMap<BulkProcessEntity, String> ENTITY_DIR_MAP =
      new ImmutableMap.Builder<BulkProcessEntity, String>()
          .put(BulkProcessEntity.ORDER, BULK_ORDER_DOWNLOAD_DIR)
          .put(BulkProcessEntity.PRODUCT, BULK_DOWNLOAD_DIR)
          .put(BulkProcessEntity.PRODUCT_EAN, BULK_DOWNLOAD_EAN_DIR)
          .put(BulkProcessEntity.FAILED_PRODUCT, BULK_FAILED_PRODUCT_DIR)
          .put(BulkProcessEntity.PRODUCT_VENDOR, BULK_PRODUCT_VENDOR_DIR)
          .put(BulkProcessEntity.CAMPAIGN_PRODUCT, BULK_CAMPAIGN_PRODUCT_DOWNLOAD_DIR)
          .put(BulkProcessEntity.MASTER_PRODUCT, BULK_DOWNLOAD_DIR_FOR_MASTER_PRODUCT)
          .put(BulkProcessEntity.SELECTED_MASTER_PRODUCTS, BULK_DOWNLOAD_DIR_FOR_MASTER_PRODUCT)
          .put(BulkProcessEntity.REVIEW_PRODUCTS, BULK_PRODUCT_REVIEW_DOWNLOAD)
          .put(BulkProcessEntity.VENDOR_FILTERED_PRODUCT, BULK_PRODUCT_REVIEW_DOWNLOAD)
          .put(BulkProcessEntity.CONFIGURATION_MERCHANT_SUMMARY, BULK_CONFIGURATION_DIR)
          .put(BulkProcessEntity.CONFIGURATION_CATEGORY_SUMMARY, BULK_CONFIGURATION_DIR)
          .put(BulkProcessEntity.INSTANT_PICKUP_PRODUCT, BULK_INSTANT_PICKUP_PRODUCT_DOWNLOAD_DIR)
          .put(BulkProcessEntity.RECAT_FAILED_PRODUCTS, BULK_RECAT_DIR)
          .put(BulkProcessEntity.STORE_COPY_FAILED_PRODUCTS, BULK_STORE_COPY_DIR)
          .put(BulkProcessEntity.UPDATE_SALES_CATEGORY_FAILED_PRODUCTS, BULK_SALES_CATEGORY_UPDATE_DIR)
          .put(BulkProcessEntity.STORE_COPY_PRODUCTS, BULK_STORE_COPY_DIR)
          .put(BulkProcessEntity.STORE_COPY_UPLOAD_TEMPLATE, BULK_STORE_COPY_UPLOAD_TEMPLATE_DIR)
          .put(BulkProcessEntity.BRAND_AUTHORIZATION_DOWNLOAD, BULK_BRAND_AUTHORIZATION_TEMPLATE_DIR)
          .put(BulkProcessEntity.DELETE_UPDATE_PICKUP_POINTS,DELETE_UPDATE_PICKUP_POITS_TEMPLATE_DIR)
          .put(BulkProcessEntity.MASTER_SKU_SELECTED_ITEMS_DOWNLOAD, MASTER_SKU_ITEMS_DOWNLOAD_TEMPLATE_DIR)
          .put(BulkProcessEntity.MASTER_SKU_ALL_ITEMS_DOWNLOAD, MASTER_SKU_ITEMS_DOWNLOAD_TEMPLATE_DIR)
          .put(BulkProcessEntity.MASTER_SKU_IN_REVIEW_DOWNLOAD, MASTER_SKU_IN_REVIEW_DOWNLOAD_TEMPLATE_DIR)
          .put(BulkProcessEntity.AUTO_APPROVED_PRODUCTS_ALL_DOWNLOAD, AUTO_APPROVED_PRODUCTS_DOWNLOAD_TEMPLATE_DIR)
          .put(BulkProcessEntity.AUTO_APPROVED_PRODUCTS_SELECTED_DOWNLOAD, AUTO_APPROVED_PRODUCTS_DOWNLOAD_TEMPLATE_DIR)
          .put(BulkProcessEntity.BULK_PRICE_RECOMMENDATION, BULK_PRICE_RECOMMENDATION_DOWNLOAD_TEMPLATE_DIR)
          .put(BulkProcessEntity.BULK_DOWNLOAD_TAGGED_PRODUCTS,BULK_DOWNLOAD_TAGGED_PRODUCTS_TEMPLATE_DIR)
          .put(BulkProcessEntity.IPR_PRODUCTS_DOWNLOAD_ALL, IPR_PRODUCT_DOWNLOAD_TEMPLATE_DIR)
          .put(BulkProcessEntity.PRODUCT_BASIC_INFO, BULK_DOWNLOAD_BASIC_INFO_DIR)
          .build();

  public static void createDirectories(String path)  {
    File directories = new File(path);
    if (!directories.exists()) {
      directories.mkdirs();
    }
  }

  public static void createDirectory(String path)  {
    File directory = new File(path);
    if (!directory.exists()) {
      directory.mkdir();
    }
  }

  public static void createFile(String path, byte[] data) throws IOException {
    File file = new File(path);
    createDirectory(file.getParent());
    file.createNewFile();
    FileOutputStream fileOutputStream = new FileOutputStream(file, false);
    fileOutputStream.write(data);
    fileOutputStream.close();
  }

  public static void createXLSXFile(String path, Workbook workbook) throws IOException {
    try (FileOutputStream fileOutputStream = new FileOutputStream(new File(path))) {
      workbook.write(fileOutputStream);
    } catch (Exception e) {
      LOGGER.error("Error while creating XLSX file. filePath: {}", path);
      throw e;
    }
  }

  public static void compressFiles(String zipFilePath, String zipFileName,
      Map<String, InputStream> filesToCompress) throws Exception {

    try (OutputStream compressedByteStream = new FileOutputStream(
        zipFilePath + File.separator + zipFileName);
        ZipOutputStream zipOutputStream = new ZipOutputStream(compressedByteStream)) {
      for (Map.Entry<String, InputStream> entry : filesToCompress.entrySet()) {
        String filename = entry.getKey();
        InputStream inputStream = entry.getValue();
        ZipEntry zipEntry = new ZipEntry(zipFileName + File.separator + filename);
        zipOutputStream.putNextEntry(zipEntry);

        byte[] bytes = new byte[4096];
        int length;
        while ((length = inputStream.read(bytes)) >= 0) {
          zipOutputStream.write(bytes, 0, length);
        }

        inputStream.close();
      }

      zipOutputStream.finish();
    }
  }

  public static void decompressFile(String path, byte[] data) throws IOException {
    createDirectory(path);

    ZipInputStream zipInputStream = new ZipInputStream(new ByteArrayInputStream(data));
    ZipEntry zipEntry = null;
    while ((zipEntry = zipInputStream.getNextEntry()) != null) {
      LOGGER.debug("processing zip file {}", zipEntry.getName());
      String[] files = zipEntry.getName().split("/");
      File file = new File(path + File.separator + files[files.length - 1]);
      createDirectories(file.getParent());

      FileOutputStream fileOutputStream = new FileOutputStream(file);
      byte[] buffer = new byte[1024];
      int read;
      while ((read = zipInputStream.read(buffer)) > 0) {
        fileOutputStream.write(buffer, 0, read);
      }
      fileOutputStream.close();
    }
    zipInputStream.closeEntry();
    zipInputStream.close();
  }

  private static void deleteFile(File directory)  {
    if (directory.isDirectory()) {
      if (directory.list().length == 0) {
        directory.delete();
      } else {
        for (String path : directory.list()) {
          File childDirectory = new File(directory, path);
          deleteFile(childDirectory);
        }
        if (directory.list().length == 0) {
          directory.delete();
        }
      }
    } else {
      directory.delete();
    }
  }

  public static void deleteFile(String path)  {
    File directory = new File(path);
    deleteFile(directory);
  }

  private static void searchFile(File directory, List<File> files, String filename)  {
    if (directory.isDirectory()) {
      for (File file : directory.listFiles()) {
        if (file.isDirectory()) {
          searchFile(file, files, filename);
        } else if (file.getName().equals(filename)) {
            files.add(file);
        }
      }
    }
  }

  private static void findFiles(File directory, List<File> files) {
    if (directory.isDirectory()) {
      for (File file : directory.listFiles()) {
        if (file.isDirectory()) {
          findFiles(file, files);
        } else {
            files.add(file);
        }
      }
    }
  }

  public static String getFileFormat(String fileName) {
    if (fileName.endsWith(FILETYPE_XLSX_EXCEL)) {
      return FILETYPE_XLSX_EXCEL;
    } else if (fileName.endsWith(FILETYPE_XLSM_EXCEL)) {
      return FILETYPE_XLSM_EXCEL;
    } else if (fileName.endsWith(FILETYPE_CSV)) {
      return FILETYPE_CSV;
    } else {
      return FILETYPE_EXCEL;
    }
  }

  public static FileType getFileType(String fileName) {
    if (fileName.endsWith(FILETYPE_XLSX_EXCEL)) {
      return FileType.XLSX;
    } else if (fileName.endsWith(FILETYPE_XLSM_EXCEL)) {
      return FileType.XLSM;
    } else if (fileName.endsWith(FILETYPE_CSV)) {
      return FileType.CSV;
    } else {
      return FileType.XLS;
    }
  }

  public static List<File> searchFile(String path, String filename)  {
    List<File> files = new ArrayList<File>();
    File directory = new File(path);
    searchFile(directory, files, filename);
    return files;
  }

  public static List<File> findAllFiles(String path)  {
    List<File> files = new ArrayList<File>();
    File directory = new File(path);
    findFiles(directory, files);
    return files;
  }

  public static void deleteDirectory(Path dirPath) {
    try {
      Files.walk(dirPath).sorted(Comparator.reverseOrder()).map(Path::toFile)
          .forEach(File::delete);
    } catch (IOException e) {
      LOGGER.error("Error delete dir : {}", dirPath, e);
    }
  }

  public static Date getEarlierDateBySeconds(int seconds) {
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.SECOND, -seconds);
    return calendar.getTime();
  }

  public static String formatNumberByRemovingDenominator(Double number) {
    if (number != null) {
      return REMOVE_DENOMINATOR_DECIMAL_FORMAT.format(number);
    }
    return null;
  }

  public static Sheet getExcelSheetData(String filePath) {
    try (InputStream fileInputStream = new FileInputStream(new File(filePath))) {
      return POIUtil.getSheetForInputStream(fileInputStream, filePath.endsWith(ProcessorUtils.FILETYPE_XLSX_EXCEL), 0);
    } catch (IOException e) {
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND, e.getMessage(), e);
    }
  }

  public static Sheet getExcelSheetDataForInternalProcess(String processType, String requestCode, String fileName) {
    Sheet sheet = null;
    if (BulkInternalProcessType.STORE_COPY.name().equals(processType)) {
      sheet = getExcelSheetData(ProcessorUtils.BULK_STORE_COPY_DIR_UPLOADS + requestCode + Constant.SLASH + fileName);
    } else if (BulkInternalProcessType.SALES_CATEGORY_UPDATE.name().equals(processType)) {
      sheet = getExcelSheetData(ProcessorUtils.BULK_SALES_CATEGORY_UPDATE_DIR + requestCode + Constant.SLASH + fileName);
    } else if (BulkInternalProcessType.DELETE_BRAND_AUTHORISATION.name().equals(processType)) {
      sheet = getExcelSheetData(ProcessorUtils.DELETE_BRAND_AUTHORISATION_DIR_UPLOADS + requestCode + Constant.SLASH + fileName);
    } else if (BulkInternalProcessType.INTERNAL_BULK_UPLOAD.name().equals(processType)) {
      sheet = getExcelSheetData(fileName);
    } else if(BulkInternalProcessType.VENDOR_BULK_ASSIGNMENT.name().equals(processType)) {
      sheet = getExcelSheetData(fileName);
    }
    return sheet;
  }

  public static void deleteInternalProcessDirectoryByProcessTypeAndRequestCode(String processType, String requestCode,
      String fileName) {
    if (BulkInternalProcessType.STORE_COPY.name().equals(processType)) {
      deleteFile(ProcessorUtils.BULK_STORE_COPY_DIR + requestCode);
    } else if (BulkInternalProcessType.SALES_CATEGORY_UPDATE.name().equals(processType)) {
      deleteFile(ProcessorUtils.BULK_SALES_CATEGORY_UPDATE_DIR + requestCode);
    } else if (BulkInternalProcessType.INTERNAL_BULK_UPLOAD.name().equals(processType)) {
      deleteFile(fileName);
    } else if (BulkInternalProcessType.VENDOR_BULK_ASSIGNMENT.name().equals(processType)) {
      deleteFile(fileName);
    }
  }

  public static void validateExcelFile(BulkInternalProcessUploadRequest bulkInternalProcessUploadRequest) {
    if (BulkInternalProcessType.STORE_COPY.name().equalsIgnoreCase(bulkInternalProcessUploadRequest.getProcessType())) {
      File file = new File(
          ProcessorUtils.BULK_STORE_COPY_DIR_UPLOADS + bulkInternalProcessUploadRequest.getInternalProcessRequestCode()
              + Constant.SLASH + bulkInternalProcessUploadRequest.getFileName());
      checkIfFileExists(file);
    } else if (BulkInternalProcessType.DELETE_BRAND_AUTHORISATION.name()
        .equals(bulkInternalProcessUploadRequest.getProcessType())) {
      File file = new File(ProcessorUtils.DELETE_BRAND_AUTHORISATION_DIR_UPLOADS
          + bulkInternalProcessUploadRequest.getInternalProcessRequestCode() + Constant.SLASH
          + bulkInternalProcessUploadRequest.getFileName());
    } else if (BulkInternalProcessType.SALES_CATEGORY_UPDATE.name()
        .equalsIgnoreCase(bulkInternalProcessUploadRequest.getProcessType())) {
      File file = new File(ProcessorUtils.BULK_SALES_CATEGORY_UPDATE_DIR
          + bulkInternalProcessUploadRequest.getInternalProcessRequestCode() + Constant.SLASH
          + bulkInternalProcessUploadRequest.getFileName());
      checkIfFileExists(file);
    }
  }

  private static void checkIfFileExists(File file) {
    if (!file.exists()) {
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND);
    }
  }
}

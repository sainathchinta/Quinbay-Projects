package com.gdn.mta.bulk.service;

import static com.gdn.mta.bulk.dto.BulkProcessType.DOWNLOAD_FAILED_UPSERT_MPP_PRODUCT;
import static com.gdn.mta.bulk.dto.BulkProcessType.QR_GENERATION;
import static com.gdn.mta.bulk.dto.BulkProcessType.getBulkProcessType;
import static com.gdn.mta.bulk.helper.ProductLevel3ProcessorValidationMessageHelper.errorMessage;
import static com.gdn.mta.bulk.helper.ProductLevel3ProcessorValidationMessageHelper.errorMessageBasedOnMerchant;

import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.net.URLConnection;
import java.nio.charset.Charset;
import java.text.ParseException;
import java.time.LocalDate;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import javax.imageio.ImageIO;
import javax.imageio.ImageReader;
import javax.imageio.stream.ImageInputStream;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.mta.bulk.dto.ValidateImageDTO;
import com.gdn.mta.bulk.feignConfig.XgpFeign;
import com.gdn.mta.bulk.request.FullImageUploadRequest;
import com.gdn.mta.bulk.request.MediumImageUploadRequest;
import com.gdn.mta.bulk.request.ThumbNailImageUploadRequest;
import com.gdn.mta.bulk.request.XgpImageScaleRequest;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.BulkInternalProcessType;
import com.gdn.mta.bulk.BulkProcessValidationErrorMessages;
import com.gdn.mta.bulk.config.QRCodeProperties;
import com.gdn.mta.bulk.config.SystemParameter;
import com.gdn.mta.bulk.dto.BulkBasicInfoRequest;
import com.gdn.mta.bulk.dto.BulkInternalProcessDTO;
import com.gdn.mta.bulk.dto.BulkInternalUploadRequestDTO;
import com.gdn.mta.bulk.dto.BulkProcessPath;
import com.gdn.mta.bulk.dto.BulkProcessType;
import com.gdn.mta.bulk.dto.BulkUpdateProcessDTO;
import com.gdn.mta.bulk.dto.GenericTemplateFileType;
import com.gdn.mta.bulk.dto.QrCodeRowInfo;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.BulkUpdateQueue;
import com.gdn.mta.bulk.factory.BulkProcessHelperFactory;
import com.gdn.mta.bulk.helper.BulkProcessHelper;
import com.gdn.mta.bulk.helper.CategoryExcelTemplateUtil;
import com.gdn.mta.bulk.helper.ExcelTemplateUtil;
import com.gdn.mta.bulk.helper.ImageValidator;
import com.gdn.mta.bulk.models.AuditTrailInfo;
import com.gdn.mta.bulk.models.BulkErrorCategory;
import com.gdn.mta.bulk.models.BulkProcessEntity;
import com.gdn.mta.bulk.models.BulkUploadErrorCounter;
import com.gdn.mta.bulk.models.EmailConstants;
import com.gdn.mta.bulk.models.ImageDownloadResult;
import com.gdn.mta.bulk.models.ProductBasicDetail;
import com.gdn.mta.bulk.models.UploadImageRequest;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.util.BulkCreationCommonUtil;
import com.gdn.mta.bulk.util.BulkUpdateServiceUtil;
import com.gdn.mta.bulk.util.CommonUtils;
import com.gdn.mta.bulk.util.ImageUtil;
import com.gdn.mta.bulk.util.POIUtil;
import com.gdn.mta.bulk.util.ProcessorUtils;
import com.gdn.mta.notification.enumeration.NotificationType;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.partners.bulk.util.NotificationTypeConstant;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.google.api.gax.paging.Page;
import com.google.cloud.storage.Blob;
import com.google.cloud.storage.BlobInfo;
import com.google.cloud.storage.Bucket;
import com.google.common.collect.ImmutableSet;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.multipart.MultipartFile;

@Service("FileStorageService")
@Slf4j
public class FileStorageServiceBean implements FileStorageService {

  private static final String FILE_BYTE_DATA = "fileByteData";
  private static final String FILE_BYTE_DATA_EN = "fileByteDataEnglish";
  private static int DEFAULT_BUFFER_SIZE = 4096;
  public static final String BULK_PRICE_RECOMMENDATION_BASE_TEMPLATE = "BulkPriceRecommendationBaseTemplate.xlsx";
  private static final String ESCAPE_SPACE = "\"";

  @Autowired
  private GCSService gcsService;

  @Autowired
  private BulkProcessHelperFactory bulkProcessHelperFactory;

  @Autowired
  private SystemParameter systemParameter;

  @Autowired
  @Qualifier("sourceImageBucket")
  private Bucket sourceImageBucket;

  @Autowired
  @Qualifier("bulkBucket")
  private Bucket bulkBucket;

  @Autowired
  @Qualifier("pricingBulkBucket")
  private Bucket pricingBulkBucket;

  @Value("${gcs.enabled}")
  private boolean gcsEnabled;

  @Value("${gcs.basePath}")
  private String gcsBasePath;

  @Value("${gcs.update.path}")
  private String gcsUpdatePath;

  @Value("${gcs.update.ean.path}")
  private String gcsUpdateEanPath;

  @Value("${gcs.upload.path}")
  private String gcsUploadPath;

  @Value("${gcs.campaign.upload.path}")
  private String gcsCampaignUploadPath;

  @Value("${gcs.upsert.upload.path}")
  private String gcsUpsertUploadPath;

  @Value("${gcs.delete.upsert.upload.path}")
  private String gcsDeleteUpsertUploadPath;

  @Value("${gcs.subject.to.vat.path}")
  private String gcsSubjectToVatPath;

  @Value("${gcs.subjectToVat.error.upload.path}")
  private String gcsSubjectToVatErrorPath;

  @Value("${gcs.archival.upload.path}")
  private String gcsArchivalUploadPath;

  @Value("${gcs.master.sku.items.download.path}")
  private String gcsMasterSkuItemsDownloadPath;

  @Value("${gcs.master.sku.anchors.download.path}")
  private String gcsMasterSkuAnchorsDownloadPath;

  @Value("${gcs.auto.approved.products.download.path}")
  private String gcsAutoApprovedProductsDownloadPath;
  @Value("${gcs.ipr.products.download.path}")
  private String gcsIprProductsDownloadPath;

  @Value("${gcs.instore.upload.path}")
  private String gcsInstoreUploadPath;

  @Value("${gcs.failed.mpp.upload.path}")
  private String gcsFailedMppUploadPath;

  @Value("${gcs.download.failed.products.path}")
  private String gcsDownloadFailedProductsPath;

  @Value("${static.baseUrl}")
  private String staticBaseUrl;

  @Value("${blibli.mass.template.location}")
  private String unifiedTemplateDirectory;

  @Value("${gcs.campaign.error.upload.path}")
  private String gcsCampaignErrorUploadPath;

  @Value("${gcs.suspension.error.file.path}")
  private String gcsSuspensionErrorFilePath;

  @Value("${gcs.master.product.download.path}")
  private String gcsMasterProductDownloadPath;

  @Value("${gcs.uncategorised.sku.download.path}")
  private String gcsUncategorisedSkuPath;

  @Value("${gcs.store.copy.download.path}")
  private String gcsStoreCopyDownloadPath;

  @Value("${gcs.suspension.product.download.path}")
  private String gcsSuspensionProductDownloadPath;

  @Value("${gcs.configuration.error.upload.path}")
  private String gcsConfigurationErrorUploadPath;

  @Value("${gcs.bucket.name}")
  private String gcsBulkBucketName;

  @Value("${gcs.pricing.bucket.name}")
  private String gcsPricingBucketName;

  @Value("${max.io.buffer.size:16384}")
  private int maxBufferSize;

  @Value("${decrease.image.resolution}")
  private boolean decreaseImageResolution;

  @Value("${product.image.max.size.byte}")
  private int imageMaxSize;

 //2023-07-16T16:01:05.091
  @Value("${update.format.change.date}")
  private String comparisonDate;

  @Value("${gcs.archive.error.upload.path}")
  private String gcsArchiveErrorUploadPath;

  @Value("${gcs.delete.pp.error.upload.path}")
  private String gcsDeletePpErrorUploadPath;

  @Value("${gcs.work.order.error.upload.path}")
  private String gcsWorkOrderErrorUploadPath;

  @Value("${mpp.delete.file.exist.date}")
  private String mppDeleteFileExistDate;

  @Value("${gcs.sku.level.rebate.upload.path}")
  private String gcsSkuLevelRebateUploadPath;

  @Value("${gcs.new.price.update.path}")
  private String gcsNewPriceUpdatePath;

  @Value("${gcs.basic.info.update.path}")
  private String gcsBasicInfoUpdatePath;

  @Value("${gcs.external.creation.upload.path}")
  private String gcsExternalCreationUploadPath;

  @Value("${http.connection.timeout}")
  private int httpConnectionTimeout;

  @Value("${http.connection.readTimeout}")
  private int httpConnectionReadTimeout;

  @Autowired
  @Qualifier("finalImageBucket")
  private Bucket finalImageBucket;

  @Value("${gcs.final.image.bucket.directory}")
  private String finalImageDirectory;

  @Value("${gcs.final.full.image.directory}")
  private String finalFullImageDirectory;

  @Value("${gcs.image.path.prefix}")
  private String pathPrefix;

  @Value("${gcs.final.medium.image.directory}")
  private String finalMediumImageDirectory;

  @Value("${gcs.final.thumbnail.image.directory}")
  private String finalThumbnailImageDirectory;

  public static final String DOWNLOAD_LINK_HREF = "<a href=\"";
  public static final String DOWNLOAD_LINK_DOWNLOAD = "\">Download</a>";
  public static final String XLS_EXTENSION = ".xls";
  public static final String FAILED_PRODUCT_EXTENSION = "/failed-products/";
  public static final String START_DATE_FORMAT = "yyyy-MM-dd'T'HH:mm:ss.SSS";
  private static final int CAMPAIGN_BULK_ERROR_PROCESS_CODE_TRIM_LENGTH = 8;
  private static final int SUBJECT_TO_VAT_ERROR_PROCESS_CODE_TRIM_LENGTH = 8;


  @Value("${gcs.blibli.mass.template.location}")
  private String gcsMassTemplateLocation;

  @Value("${gcs.blibli.mass.budling.template.path}")
  private String gcsMassBundlingTemplatePath;

  @Value("${blibli.mass.template.location}")
  private String blibliMassTemplateLocation;

  @Value("${sysparam.directory.unified.file.en}")
  private String unifiedUploadTemplateFileEnglish;

  @Value("${sysparam.directory.unified.file}")
  private String unifiedUploadTemplateFile;

  @Value("${sysparam.directory.template}")
  private String categoryTemplateLocation;

  @Value("${save.image.raw.folder.to.gcs}")
  private boolean saveImageRawFolderToGcs;

  @Value("${source.image.directory}")
  private String sourceImageDirectory;

  @Value("${storeCopy.template.path}")
  private String storeCopyUploadTemplatePath;

  @Value("${sysparam.directory.category.file}")
  private String categoryBaseTemplatePath;

  @Value("${sysparam.directory.category.file.en}")
  private String categoryBaseTemplatePathEn;

  @Value(value = "${allowed.image.types}")
  private String allowedImageTypes;

  @Value("${error.file.name.size}")
  private int errorFileNameSize;

  @Value("${bulk.excel.versioning.switch.en}")
  private boolean bulkExcelVersioningEn;

  @Value("${bulk.generic.excel.version}")
  private String bulkGenericExcelVersion;

  @Value("${bulk.generic.instore.excel.version}")
  private String bulkGenericInstoreExcelVersion;

  @Value("${bulk.image.download.use.http.connection}")
  private boolean useHttpConnectionForImageDownload;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private QRCodeProperties qrCodeProperties;

  @Autowired
  private XgpFeign xgpFeign;

  private static final String QR_TEMPLATE_PATH = "/qrTemplates/";
  private static final Set<BulkInternalProcessType> PRICE_ANALYTICS_BULK_PROCESS_TYPES =
      ImmutableSet.of(BulkInternalProcessType.BULK_SKU_LEVEL_REBATE, BulkInternalProcessType.BULK_PRICE_UPDATE_NEW);




  @Override
  public void createBulkFile(BulkUpdateProcessDTO bulkUpdateProcessDTO, String bulkProcessCode, String fileName)
      throws Exception {
    gcsService.uploadCreatedFile(bulkBucket,
        getBasePath(bulkUpdateProcessDTO.getBulkProcessType()) + bulkProcessCode + File.separator + bulkProcessCode
            + ProcessorUtils.getFileFormat(fileName), bulkUpdateProcessDTO.getFileContent());
  }

  @Override
  public String createBulkInternalFile(BulkDownloadRequest request, BulkInternalProcessDTO bulkInternalProcessDTO,
      String processType) throws Exception {
    if (gcsEnabled) {
      BulkProcessHelper bulkProcessHelper = bulkProcessHelperFactory.getHelper(request);
      String directoryPath = bulkProcessHelper.getDirectory(request);
      Workbook finalWorkbook = bulkInternalProcessDTO.getFinalWorkbook();
      try (ByteArrayOutputStream out = new ByteArrayOutputStream()) {
        finalWorkbook.write(out);
        byte[] destinationByteFile = out.toByteArray();
        String filePath = gcsBasePath + File.separator + StringUtils.remove(directoryPath, ProcessorUtils.DATA_BASE_DIR)
            + File.separator + request.getFilename();
        gcsService.uploadCreatedFile(bulkBucket, filePath, destinationByteFile);
        return filePath;
      }
    } else {
      ProcessorUtils.createDirectories(bulkInternalProcessDTO.getDirectoryPath());
      ProcessorUtils.createXLSXFile(bulkInternalProcessDTO.getFilepath(), bulkInternalProcessDTO.getFinalWorkbook());
      return bulkInternalProcessDTO.getFilepath();
    }
  }

  @Override
  public byte[] downloadFile(BulkProcess bulkProcess, String excelFileType) throws IOException {
    String filePath = getBasePath(bulkProcess.getBulkProcessType()) + bulkProcess.getBulkProcessCode() + File.separator
        + bulkProcess.getBulkProcessCode() + excelFileType;
    return gcsService.downloadFile(gcsBulkBucketName, filePath);
  }

  @Override
  public byte[] downloadBaseTemplateForPriceRecommendation() {
    String filePath = gcsMassTemplateLocation + Constant.SLASH + BULK_PRICE_RECOMMENDATION_BASE_TEMPLATE;
    return gcsService.downloadFile(gcsBulkBucketName, filePath);
  }

  @Override
  public boolean isMassTemplateFileExist(String unifiedFileName) {
    StringBuilder massTemplateFilePath =
        new StringBuilder(gcsMassTemplateLocation).append(Constant.SLASH).append(unifiedFileName);
    return gcsService.isFileExists(gcsBulkBucketName, massTemplateFilePath.toString());
  }

  @Override
  public XSSFWorkbook getUnifiedBaseTemplate(String unifiedBaseTemplateFileName,
    String unifiedUploadTemplateFileName, boolean isEnglishTemplate) throws Exception {
    try {
      byte[] bytes = gcsService.downloadFile(gcsBulkBucketName,
        gcsMassTemplateLocation + Constant.SLASH + unifiedUploadTemplateFileName);
      try (InputStream inputStream = new ByteArrayInputStream(bytes)) {
        XSSFWorkbook workBookForInputStream = new XSSFWorkbook(inputStream);
        ExcelTemplateUtil.regenerateValueHeaderSheet(workBookForInputStream, isEnglishTemplate);
        return workBookForInputStream;
      }
    } catch (ApplicationRuntimeException ex) {
      byte[] baseTemplateBytes = gcsService.downloadFile(gcsBulkBucketName,
        gcsMassTemplateLocation + Constant.SLASH + unifiedBaseTemplateFileName);
      try (InputStream inputStream = new ByteArrayInputStream(baseTemplateBytes)) {
        return new XSSFWorkbook(inputStream);
      }
    }
  }

  @Override
  public void writeGenericTemplate(String templatePath, XSSFWorkbook workbook) throws Exception {
    try (ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream()) {
      workbook.write(byteArrayOutputStream);
      gcsService.uploadCreatedFile(bulkBucket, gcsMassTemplateLocation + Constant.SLASH + templatePath,
          byteArrayOutputStream.toByteArray());
    }
  }

  @Override
  public Sheet getFileData(BulkUpdateQueue bulkUpdateQueue, BulkProcess bulkProcess) throws IOException {
    GdnPreconditions.checkArgument(bulkProcess != null, BulkUpdateServiceUtil.BULK_PROCESS_NULL_ERROR);
    final String fileName = bulkUpdateQueue.getFileName();
    return getDataSheet(fileName, bulkProcess);
  }

  @Override
  public Sheet getFileDataByFileName(String fileName, BulkProcess bulkProcess) throws IOException {
    return getDataSheet(fileName, bulkProcess);
  }

  private Sheet getDataSheet(String fileName, BulkProcess bulkProcess) throws IOException {
    GdnPreconditions.checkArgument(Objects.nonNull(bulkProcess), BulkUpdateServiceUtil.BULK_PROCESS_NULL_ERROR);
    if (gcsEnabled) {
      try (InputStream fileInputStream = new ByteArrayInputStream(gcsService.downloadFile(gcsBulkBucketName,
        getBasePath(bulkProcess.getBulkProcessType()) + bulkProcess.getBulkProcessCode() + File.separator + bulkProcess.getBulkProcessCode() + ProcessorUtils.getFileFormat(
          fileName)))) {
        return POIUtil.getSheetForInputStream(fileInputStream, fileName.endsWith(ProcessorUtils.FILETYPE_XLSX_EXCEL), 0);
      }
    } else {
      try (InputStream fileInputStream = BulkUpdateServiceUtil.getFileInputStream(bulkProcess.getBulkProcessCode(),
        fileName, getBasePath(bulkProcess.getBulkProcessType()))) {
        return POIUtil.getSheetForInputStream(fileInputStream, fileName.endsWith(ProcessorUtils.FILETYPE_XLSX_EXCEL),
          0);
      }
    }
  }

  @Override
  public String getDownloadLink(String filePath, String bulkProcessType, String bulkProcessCode, String fileName) {
    if (gcsEnabled) {
      return getDownloadLinkHtml(
          staticBaseUrl + Constant.SLASH + getBasePath(bulkProcessType) + bulkProcessCode + File.separator + bulkProcessCode + ProcessorUtils.getFileFormat(fileName));
    } else {
      return getDownloadLinkHtml(
          staticBaseUrl + getBasePath(bulkProcessType) + filePath.replaceFirst(ProcessorUtils.DATA_BASE_DIR, StringUtils.EMPTY));
    }
  }

  @Override
  public String getDownloadLinkForNeedRevisionDeletion(String basePath, String deleteProcessCode) {
    return basePath + Constant.SLASH + deleteProcessCode + Constant.SLASH + deleteProcessCode
        + ProcessorUtils.FILETYPE_XLSX_EXCEL;
  }

  @Override
  public String campaignErrorFilePath(String filePath, String bulkProcessType,
    String bulkProcessCode, String fileName) {
    if (gcsEnabled) {
      return getDownloadLinkHtml(
        staticBaseUrl + Constant.SLASH + getBasePath(bulkProcessType) + bulkProcessCode
          + File.separator + bulkProcessCode + ProcessorUtils.getFileFormat(fileName));
    } else {
      return getDownloadLinkHtml(staticBaseUrl + getBasePath(bulkProcessType) + filePath
        .replaceFirst(ProcessorUtils.DATA_BASE_DIR, StringUtils.EMPTY));
    }
  }

  @Override
  public String getFailedProductDownloadLink(String bulkProcessCode) {
    if (gcsEnabled) {
      return staticBaseUrl + Constant.SLASH + gcsDownloadFailedProductsPath + bulkProcessCode + Constant.SLASH + bulkProcessCode + XLS_EXTENSION;
    } else {
      return staticBaseUrl + FAILED_PRODUCT_EXTENSION + bulkProcessCode + Constant.SLASH + bulkProcessCode + XLS_EXTENSION;
    }
  }

  public String getDownloadLinkHtml(String url) {
    return DOWNLOAD_LINK_HREF + url + DOWNLOAD_LINK_DOWNLOAD;
  }

  @Override
  public Sheet getFileDataWithInternalUploadRequest(BulkInternalUploadRequestDTO bulkInternalUploadRequestDTO) throws IOException {
    final String filePath = getFilePath(bulkInternalUploadRequestDTO);
    if (gcsEnabled) {
      String bucket =
          PRICE_ANALYTICS_BULK_PROCESS_TYPES.contains(bulkInternalUploadRequestDTO.getBulkInternalProcessType()) ?
              gcsPricingBucketName : gcsBulkBucketName;
      try (InputStream fileInputStream = new ByteArrayInputStream(gcsService.downloadFile(bucket, filePath))) {
        return POIUtil.getSheetForInputStream(fileInputStream, filePath.endsWith(ProcessorUtils.FILETYPE_XLSX_EXCEL), 0);
      }
    } else {
      try (InputStream fileInputStream = new FileInputStream(new File(filePath))) {
        return POIUtil.getSheetForInputStream(fileInputStream, filePath.endsWith(ProcessorUtils.FILETYPE_XLSX_EXCEL), 0);
      }
    }
  }

  @Override
  public Boolean isFileExists(BulkInternalUploadRequestDTO bulkInternalUploadRequestDTO) {
    String filePath = getFilePath(bulkInternalUploadRequestDTO);
    return fileExists(PRICE_ANALYTICS_BULK_PROCESS_TYPES.contains(bulkInternalUploadRequestDTO.getBulkInternalProcessType()) ?
        gcsPricingBucketName : gcsBulkBucketName, filePath);
  }

  private Boolean fileExists(String bucketName, String filePath) {
    if (gcsEnabled) {
      return gcsService.isFileExists(bucketName, filePath);
    }
    return new File(filePath).exists();
  }

  @Override
  public Boolean checkIfFileExistsByFilePath(String filePath) {
    return fileExists(gcsBulkBucketName, filePath);
  }

  @Override
  public byte[] downloadGenericTemplateFile(String unifiedTemplateFile) throws IOException {
    if (gcsEnabled) {
      return gcsService.downloadFile(gcsBulkBucketName,gcsMassTemplateLocation + Constant.SLASH + unifiedTemplateFile);
    } else {
      File file = new File(unifiedTemplateDirectory + Constant.SLASH + unifiedTemplateFile);
      return FileUtils.readFileToByteArray(file);
    }
  }

  @Override
  public String createGenericBulkFile(String businessPartnerCode, boolean instore, BulkInternalProcessType bulkInternalProcessType,
      byte[] byteGenericFileData) throws Exception {
    BulkInternalUploadRequestDTO requestDTO =
        BulkInternalUploadRequestDTO.builder().bulkInternalProcessType(bulkInternalProcessType)
            .businessPartnerCode(businessPartnerCode).instore(instore).build();
    if (gcsEnabled) {
      gcsService.uploadCreatedFile(bulkBucket, getFilePath(requestDTO), byteGenericFileData);
    } else {
      ProcessorUtils.createDirectories(unifiedTemplateDirectory + Constant.SLASH + businessPartnerCode);
      ProcessorUtils.createFile(getFilePath(requestDTO), byteGenericFileData);
    }
    return getFilePath(requestDTO);
  }

  //returns Final File Path according to bulk process Type
  private String getFilePath(BulkInternalUploadRequestDTO bulkInternalUploadRequestDTO) {
    StringBuilder filePath = new StringBuilder();
    switch (bulkInternalUploadRequestDTO.getBulkInternalProcessType()) {
      case SUSPEND:
      case INTERNAL_BULK_UPLOAD:
      case VENDOR_BULK_ASSIGNMENT:
      case RESTRICTED_KEYWORD_UPSERT:
      case RESTRICTED_KEYWORD_DELETE:
      case BRAND_AUTH_ADD:
      case BRAND_AUTH_DELETE:
      case BULK_APPROVAL:
      case BULK_REJECTION:
      case MASTER_SKU_BULK_ASSIGNEE:
      case MASTER_SKU_BULK_REVIEW:
      case AUTO_APPROVED_PRODUCTS_BULK_ASSIGN:
      case IPR_PORTAL_BULK_ADD_REVIEW:
      case CONFIGURATION: {
        return bulkInternalUploadRequestDTO.getRelativePath();
      }
      case RECATEGORISATION: {
        return filePath.append(gcsEnabled ?
            gcsUploadPath + StringUtils.remove(ProcessorUtils.BULK_RECAT_DIR, ProcessorUtils.DATA_BASE_DIR) :
            ProcessorUtils.BULK_RECAT_DIR).append(bulkInternalUploadRequestDTO.getInternalProcessRequestCode()).append(Constant.SLASH).append(bulkInternalUploadRequestDTO.getFileName()).toString();
      }
      case STORE_COPY: {
        return filePath.append(gcsEnabled ?
                gcsUploadPath + StringUtils.remove(ProcessorUtils.BULK_STORE_COPY_DIR_UPLOADS, ProcessorUtils.DATA_BASE_DIR) :
                ProcessorUtils.BULK_STORE_COPY_DIR_UPLOADS).append(bulkInternalUploadRequestDTO.getInternalProcessRequestCode()).append(Constant.SLASH)
            .append(bulkInternalUploadRequestDTO.getFileName()).toString();
      }
      case SALES_CATEGORY_UPDATE: {
        return filePath.append(gcsEnabled ?
                gcsUploadPath + StringUtils.remove(ProcessorUtils.BULK_SALES_CATEGORY_UPDATE_DIR, ProcessorUtils.DATA_BASE_DIR) :
                ProcessorUtils.BULK_SALES_CATEGORY_UPDATE_DIR).append(bulkInternalUploadRequestDTO.getInternalProcessRequestCode()).append(Constant.SLASH)
            .append(bulkInternalUploadRequestDTO.getFileName()).toString();
      }
      case GENERIC_FILE_GENERATION: {
        filePath.append(gcsEnabled ? gcsMassTemplateLocation : unifiedTemplateDirectory).append(Constant.SLASH)
            .append(bulkInternalUploadRequestDTO.getBusinessPartnerCode()).append(Constant.SLASH)
            .append(Constant.NEW_GENERAL_TEMPLATE);
        if (bulkExcelVersioningEn) {
          String version = bulkGenericExcelVersion;
          if (bulkInternalUploadRequestDTO.isInstore()) {
            version = bulkGenericInstoreExcelVersion;
          }
          filePath.append(Constant.UNDERSCORE).append(version);
        }
        filePath.append(Constant.NEW_GENERAL_TEMPLATE_EXTENSION);
        return filePath.toString();
      }
      case BULK_PRICE_UPDATE: {
        return filePath.append(gcsUploadPath)
            .append(StringUtils.remove(ProcessorUtils.BULK_PRICE_UPDATE, ProcessorUtils.DATA_BASE_DIR))
            .append(bulkInternalUploadRequestDTO.getInternalProcessRequestCode()).append(Constant.SLASH)
            .append(bulkInternalUploadRequestDTO.getFileName()).toString();
      }
      case BULK_PRICE_REBATE: {
       return filePath.append(gcsUploadPath)
            .append(StringUtils.remove(ProcessorUtils.BULK_PRICE_REBATE_DIR, ProcessorUtils.DATA_BASE_DIR))
            .append(bulkInternalUploadRequestDTO.getInternalProcessRequestCode()).append(Constant.SLASH)
            .append(bulkInternalUploadRequestDTO.getInternalProcessRequestCode()).append(".").append(Constant.FILE_TYPE_XLSX).toString();
      }
      case BULK_PRICE_PRODUCT_TYPE_TAGGING: {
        return filePath.append(gcsUploadPath).append(StringUtils.remove(ProcessorUtils.BULK_PRICE_PRODUCT_TYPE_TAGGING_DIR, ProcessorUtils.DATA_BASE_DIR))
          .append(bulkInternalUploadRequestDTO.getInternalProcessRequestCode()).append(Constant.SLASH)
          .append(bulkInternalUploadRequestDTO.getInternalProcessRequestCode()).append(".").append(Constant.FILE_TYPE_XLSX).toString();
      }
      case BULK_SKU_LEVEL_REBATE: {
        return filePath.append(gcsSkuLevelRebateUploadPath)
            .append(bulkInternalUploadRequestDTO.getCreatedBy()).append(Constant.SLASH)
            .append(bulkInternalUploadRequestDTO.getInternalProcessRequestCode()).append(".")
            .append(Constant.FILE_TYPE_XLSX).toString();
      }
      case BULK_PRICE_UPDATE_NEW: {
        return filePath.append(gcsNewPriceUpdatePath)
            .append(bulkInternalUploadRequestDTO.getCreatedBy()).append(Constant.SLASH)
            .append(bulkInternalUploadRequestDTO.getInternalProcessRequestCode()).append(".")
            .append(Constant.FILE_TYPE_XLSX).toString();
      }
      default:
        return StringUtils.EMPTY;
    }
  }

  @Override
  public String getBasePath(String bulkProcessType) {
    switch (getBulkProcessType(bulkProcessType)) {
      case CAMPAIGN: {
        return gcsEnabled ? gcsCampaignUploadPath : ProcessorUtils.BULK_CAMPAIGN_PRODUCT_CREATE_DIR;
      }
      case PRODUCT_CREATION_UPLOAD:
      case PRODUCT_CREATION_UPLOAD_PRIORITY:
      case PRODUCT_CREATION_UPLOAD_PRIORITY_1:
      case PRODUCT_CREATION_UPLOAD_PRIORITY_2:
      case ASSEMBLY_REQUEST:
      case DISASSEMBLY_REQUEST:
      case TRANSFER_REQUEST:
      case CONVERTED_PRODUCT_CREATION_UPLOAD: {
        return gcsEnabled ? gcsUploadPath : ProcessorUtils.DATA_BASE_DIR;
      }
      case INSTANT_PICKUP_PRODUCT_UPSERT: {
        return gcsEnabled ? gcsUpsertUploadPath : ProcessorUtils.BULK_INSTANT_PICKUP_PRODUCT_UPSERT_DIR;
      }
      case INSTANT_PICKUP_PRODUCT_DELETE: {
        return gcsEnabled ? gcsDeleteUpsertUploadPath : ProcessorUtils.BULK_INSTANT_PICKUP_PRODUCT_DELETE_DIR;
      }
      case SUBJECT_TO_VAT: {
        return gcsEnabled ? gcsSubjectToVatPath : ProcessorUtils.BULK_VAT_UPDATE_DIR;
      }
      case SUBJECT_TO_VAT_ERROR: {
          return gcsSubjectToVatErrorPath;
      }
      case ARCHIVE: {
        return gcsEnabled ? gcsArchivalUploadPath : ProcessorUtils.BULK_UPDATE_DIR;
      }
      case PRODUCT_LEVEL_3:
      case PRODUCT_LEVEL_3_UPDATE_PRIORITY_1:
      case PRODUCT_LEVEL_3_UPDATE_PRIORITY_2: {
        return gcsEnabled ? gcsUpdatePath : ProcessorUtils.BULK_UPDATE_DIR;
      }
      case EAN_PRODUCT_LEVEL_4 : {
        return gcsUpdateEanPath;
      }
      case PRODUCT_BASIC_INFO:
      case PRODUCT_BASIC_INFO_PRIORITY_1:
      case PRODUCT_BASIC_INFO_PRIORITY_2: {
        return gcsBasicInfoUpdatePath;
      }
      case EXTERNAL_CREATION_UPLOAD: {
        return gcsExternalCreationUploadPath;
      }
      case IN_STORE: {
        return gcsEnabled ? gcsInstoreUploadPath : ProcessorUtils.BULK_UPDATE_DIR;
      }
      case DOWNLOAD_FAILED_UPSERT_MPP_PRODUCT: {
        return gcsEnabled ? gcsFailedMppUploadPath : ProcessorUtils.DOWNLOAD_FAILED_MPP_PRODUCT_DIR;
      }
      case PRODUCT_CREATION_FAILED_DIR: {
        return gcsEnabled ? gcsDownloadFailedProductsPath : ProcessorUtils.BULK_FAILED_PRODUCT_DIR;
      }
      case PRODUCT_SUSPENSION: {
        return gcsEnabled ?
            gcsUploadPath + BulkProcessPath.SUSPENSION_PATH.getValue() :
            ProcessorUtils.BULK_SUSPENSION_DIR;
      }
      case BULK_PRICE_REBATE: {
        return gcsUploadPath + BulkProcessPath.BULK_REBATE.getValue();
      }
      case BULK_PRICE_PRODUCT_TYPE_TAGGING:{
        return gcsUploadPath + BulkProcessPath.BULK_PRICE_PRODUCT_TYPE_TAGGING.getValue();
      }
      case INTERNAL_UPLOAD: {
        return gcsEnabled ? gcsUploadPath + BulkProcessPath.INTERNAL_UPLOAD.getValue() : ProcessorUtils.BULK_UPDATE_DIR;
      }
      case CONFIGURATION: {
        return gcsEnabled ?
            gcsUploadPath + BulkProcessPath.CONFIGURATION.getValue() :
            ProcessorUtils.BULK_CONFIGURATION_DIR;
      }
      case CAMPAIGN_ERROR: {
        return gcsEnabled ?
          gcsCampaignErrorUploadPath :
          ProcessorUtils.BULK_CAMPAIGN_ERROR_PRODUCT_CREATE_DIR;
      }
      case SUSPENSION_ERROR: {
        return gcsEnabled ?
            gcsSuspensionErrorFilePath :
            ProcessorUtils.BULK_SUSPENSION_DIR;
      }
      case CONFIGURATION_ERROR: {
        return gcsEnabled ?
            gcsConfigurationErrorUploadPath :
            EmailConstants.BULK_CONFIGURATION_ERROR_FILE_PREFIX;
      }
      case RECAT_ERROR: {
        return gcsEnabled ?
            gcsBasePath + Constant.SLASH + BulkProcessPath.RECAT.getValue() :
            ProcessorUtils.X_BULK + BulkProcessPath.CONFIGURATION.getValue();
      }
      case STORE_COPY_TEMPLATE:
        return gcsEnabled ? gcsMassTemplateLocation : storeCopyUploadTemplatePath;
      case QR_GENERATION:
        return gcsBasePath + File.separator + QR_GENERATION.getValue() + File.separator;
      case ARCHIVE_ERROR:
        return gcsArchiveErrorUploadPath;
      case PICKUP_POINT_DELETE_ERROR:
        return gcsDeletePpErrorUploadPath;
      case WORK_ORDER_ERROR:
        return gcsWorkOrderErrorUploadPath;
      default:
        return StringUtils.EMPTY;
    }
  }

  @Override
  public String getFilePrefix(String bulkProcessType) {
    switch (getBulkProcessType(bulkProcessType)) {
      case MASTER_PRODUCT:
        return gcsEnabled ?
          gcsMasterProductDownloadPath :
          EmailConstants.MASTER_PRODUCT_DOWNLOAD_FILE_PREFIX;
      case UNCATEGORISED_SKU:
        return gcsEnabled ? gcsUncategorisedSkuPath :
            EmailConstants.UNCATEGORISED_SKU_DOWNLOAD_FILE_PREFIX;
      case STORE_COPY:
        return gcsEnabled ? gcsStoreCopyDownloadPath : EmailConstants.STORE_COPY_FILE_PREFIX;
      case PRODUCT_SUSPENSION:
        return gcsEnabled ?
          gcsSuspensionProductDownloadPath :
          EmailConstants.BULK_PRODUCT_SUSPENSION_FILE_PREFIX;
      case CONFIGURATION:
        return gcsEnabled ?
            gcsConfigurationErrorUploadPath :
            EmailConstants.BULK_CONFIGURATION_ERROR_FILE_PREFIX;
      case MASTER_SKU_REVIEW_ITEMS_DOWNLOAD:
        return gcsMasterSkuItemsDownloadPath;
      case MASTER_SKU_IN_REVIEW_DOWNLOAD:
        return gcsMasterSkuAnchorsDownloadPath;
      case AUTO_APPROVED_PRODUCTS_DOWNLOAD:
        return gcsAutoApprovedProductsDownloadPath;
      case IPR_PRODUCTS_DOWNLOAD_ALL:
          return gcsIprProductsDownloadPath;
      default:
        return StringUtils.EMPTY;
    }
  }

  @Override
  public void decompressFile(String bulkProcessCode, byte[] zipImageFile) throws Exception {
      unzipImageFileAndUploadToTempGcsLocation(bulkProcessCode, zipImageFile);
  }

  private void unzipImageFileAndUploadToTempGcsLocation(String bulkProcessCode, byte[] zipImageFile) throws Exception {
    try (ZipInputStream zipInputStream = new ZipInputStream(new ByteArrayInputStream(zipImageFile))) {
      ZipEntry zipEntry = null;
      while ((zipEntry = zipInputStream.getNextEntry()) != null) {
        String[] files = zipEntry.getName().split("/");
        log.debug("processing zip file {}", zipEntry.getName());
        try (ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream()) {
          byte[] buffer = new byte[DEFAULT_BUFFER_SIZE];
          int read;
          while ((read = zipInputStream.read(buffer)) != -1) {
            byteArrayOutputStream.write(buffer, 0, read);
            DEFAULT_BUFFER_SIZE = Math.min(buffer.length, maxBufferSize);
            buffer = new byte[DEFAULT_BUFFER_SIZE];
          }
          String path = gcsBasePath + Constant.SLASH + bulkProcessCode + Constant.SLASH + ProcessorUtils.DATA_RAW_DIR
              + Constant.SLASH + files[files.length - 1];
          gcsService.uploadCreatedFile(bulkBucket, path, byteArrayOutputStream.toByteArray());
        }
      }
      zipInputStream.closeEntry();
      zipInputStream.close();
    }
  }

  @Override
  public ImageDownloadResult downloadImages(String bulkProcessCode, Map<String, String> imagesToDownload,
    int httpConnectionTimeout,
      int httpConnectionReadTimeout, Set<String> urlImagesWithInvalidExtension, List<String> images,
      BulkUploadErrorCounter bulkUploadErrorCounter, StringBuilder validationErrorMessage,
      boolean isInternationalMerchant) {
      return downloadImageFileAndUploadToTempGcsLocation(bulkProcessCode, imagesToDownload, httpConnectionTimeout,
          httpConnectionReadTimeout, urlImagesWithInvalidExtension, images, bulkUploadErrorCounter,
          validationErrorMessage, isInternationalMerchant);
  }
  private ImageDownloadResult downloadImageFileAndUploadToTempGcsLocation(String bulkProcessCode,
      Map<String, String> imagesToDownload, int httpConnectionTimeout, int httpConnectionReadTimeout,
      Set<String> urlImagesWithInvalidExtension, List<String> images, BulkUploadErrorCounter bulkUploadErrorCounter,
      StringBuilder validationErrorMessage, boolean isInternationalMerchant) {
    boolean result = true;
    String imageType = StringUtils.EMPTY;
    String imageName = StringUtils.EMPTY;
    String path;
    for (Map.Entry<String, String> image : imagesToDownload.entrySet()) {
      imageName = Constant.IMAGE.concat(image.getValue()).concat(Constant.DOT);
      log.info("Downloading image from : {} for bulkProcessCode : {} ", image.getKey(), bulkProcessCode);
      URL urlConnection = null;
      URLConnection connection = null;
      try {
        urlConnection = new URL(image.getKey());
        connection = urlConnection.openConnection();
        connection.setConnectTimeout(httpConnectionTimeout);
        connection.setReadTimeout(httpConnectionReadTimeout);
        connection.setRequestProperty(Constant.ACCEPT_HEADER, Constant.IMAGE_HEADER);
        ValidateImageDTO validateImageDTO =
          CommonUtils.getValidateImageDTO(bulkProcessCode, urlImagesWithInvalidExtension, images,
            bulkUploadErrorCounter, validationErrorMessage, isInternationalMerchant, image,
            connection);
        validateImageDTO.setUseHttpConnectionForImageDownload(useHttpConnectionForImageDownload);
        validateImageDTO.setAllowedImageTypes(allowedImageTypes);
        boolean isValid = ImageUtil.validateImageConnection(validateImageDTO);
        if (!isValid) {
          result = false;
          continue;
        }
        if (result) {
          try (InputStream in = connection.getInputStream()) {
            imageType = BulkCreationCommonUtil.getImageType(connection.getContentType());
            path = gcsBasePath + Constant.SLASH + bulkProcessCode + Constant.SLASH
              + ProcessorUtils.DATA_RAW_DIR + Constant.SLASH + imageName + imageType;
            gcsService.uploadCreatedFile(bulkBucket, path, IOUtils.toByteArray(in));
          } catch (Exception ex) {
            log.error("Image upload failed : {} for bulkProcessCode : {} ", image.getKey(),
              bulkProcessCode, ex);
            validationErrorMessage.append(errorMessage(image.getKey() + " - ",
              errorMessageBasedOnMerchant(isInternationalMerchant,
                BulkProcessValidationErrorMessages.IMAGE_FILE_NOT_FOUND_EN,
                BulkProcessValidationErrorMessages.IMAGE_FILE_NOT_FOUND), StringUtils.EMPTY))
              .append(Constant.DOT);
            result = false;
          }
        }
        if (result) {
          log.info("Image download complete for : {} for bulkProcessCode : {} ", image.getKey(), bulkProcessCode);
        }
      } catch (Exception e) {
        log.error("Url connection for key {} is {} AND open connection is {} ", image.getKey(),
          urlConnection, connection);
        log.error("Not able to download image : {} for bulkProcessCode : {} ", image.getKey(),
          bulkProcessCode, e);
        validationErrorMessage.append(errorMessage(image.getKey() + " - ",
          errorMessageBasedOnMerchant(isInternationalMerchant,
            BulkProcessValidationErrorMessages.IMAGE_FILE_NOT_FOUND_EN,
            BulkProcessValidationErrorMessages.IMAGE_FILE_NOT_FOUND), StringUtils.EMPTY))
          .append(Constant.DOT);
        result = false;
      }
    }
    return ImageDownloadResult.builder().downloadSuccess(result).imageType(imageType)
      .imageFileName(imageName).build();
  }

  @Override
  public ImageDownloadResult downloadImageFileValidateAndUploadToGCS(String bulkProcessCode,
      Map<String, String> imagesToDownload, List<String> images, StringBuilder validationErrorMessage,
      List<ProductBasicDetail> productBasicDetails) {
    boolean result = true;
    String uploadedImageType = StringUtils.EMPTY;
    String uploadedImageName = StringUtils.EMPTY;
    String locationPath = "";
    for (Map.Entry<String, String> entry : imagesToDownload.entrySet()) {
      String imageUrl = entry.getKey();
      String imageKey = entry.getValue();
      uploadedImageName = Constant.IMAGE + imageKey + Constant.DOT;
      log.info("Downloading image from: {} for bulkProcessCode: {}", imageUrl, bulkProcessCode);
      try {
        // Open and configure connection
        URLConnection connection =
            CommonUtils.getUrlConnection(imageUrl, httpConnectionTimeout, httpConnectionReadTimeout);
        String contentType = Optional.of(connection).map(URLConnection::getContentType).orElse(StringUtils.EMPTY);
        // Validate content type
        if (!BulkCreationCommonUtil.getImageAllowedTypeList(allowedImageTypes).contains(contentType)) {
          CommonUtils.handleInvalidType(imageUrl, imageKey, connection.getContentType(), images, validationErrorMessage,
              bulkProcessCode);
          result = false;
          continue;
        }
        // Read file bytes
        byte[] fileBytes;
        try (InputStream in = connection.getInputStream()) {
          fileBytes = IOUtils.toByteArray(in);
        }
        if (Objects.isNull(fileBytes)) {
          throw new IOException("Downloaded file is null");
        }
        // Determine image type and dimensions
        uploadedImageType = BulkCreationCommonUtil.getImageType(contentType);
        int width = 0, height = 0;
        try (ByteArrayInputStream byteArrayInputStream = new ByteArrayInputStream(fileBytes);
            ImageInputStream imageInputStream = ImageIO.createImageInputStream(byteArrayInputStream)) {
          Iterator<ImageReader> readers = ImageIO.getImageReaders(imageInputStream);
          if (readers.hasNext()) {
            ImageReader reader = readers.next();
            reader.setInput(imageInputStream, true);
            width = reader.getWidth(0);
            height = reader.getHeight(0);
          }
        }
        // Validate resolution and size
        try {
          ImageValidator.validateImages(fileBytes, imageMaxSize, width, height,
              decreaseImageResolution);
          if (allowedImageTypes.contains(Constant.WEBP_IMAGE_MIME_TYPE)) {
            locationPath = ImageUtil.generateImageName(0, Constant.WEBP_IMAGE_MIME_TYPE,
                productBasicDetails.get(0));
            for (ProductBasicDetail productBasicDetail : productBasicDetails) {
              String finalImageName =
                  productBasicDetail.getProductCode() + Constant.SLASH + locationPath;
              XgpImageScaleRequest xgpImageScaleRequest =
                  generateXgpImageScaleRequest(fileBytes, finalImageName);
              GdnBaseRestResponse response =
                  xgpFeign.scaleActiveProductNewImages(Constant.STORE_ID, Constant.CLIENT_ID,
                      bulkProcessCode, xgpImageScaleRequest);
              if (!response.isSuccess()) {
                throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED,
                    String.format(BulkProcessValidationErrorMessages.ERROR_WHILE_SCALING,
                        imageUrl));
              }
            }
          } else {
            locationPath = uploadProductImagesToGCS(productBasicDetails, contentType, fileBytes);
          }
          log.info("Image download and validation complete for: {} for bulkProcessCode: {}", imageUrl, bulkProcessCode);
        } catch (ApplicationRuntimeException e) {
          result = false;
          CommonUtils.handleValidationException(e.getErrorMessage(), imageUrl, validationErrorMessage);
          log.error("Image validation failed for {}: {}", imageUrl, e.getMessage(), e);
        }

      } catch (Exception e) {
        result = false;
        validationErrorMessage.append(
                errorMessage(imageUrl + " - ", BulkProcessValidationErrorMessages.IMAGE_FILE_NOT_FOUND, StringUtils.EMPTY))
            .append(Constant.DOT);
        log.error("Failed to download or process image: {} for bulkProcessCode: {}", imageUrl, bulkProcessCode, e);
      }
    }
    return ImageDownloadResult.builder().downloadSuccess(result).imageType(uploadedImageType)
        .imageFileName(locationPath).build();
  }

  private XgpImageScaleRequest generateXgpImageScaleRequest(byte[] fileBytes, String imageName) {
    UploadImageRequest uploadImageRequest = new UploadImageRequest();
    uploadImageRequest.setImageFileName(imageName);
    XgpImageScaleRequest xgpImageScaleRequest = new XgpImageScaleRequest();
    FullImageUploadRequest fullImageUploadRequest = new FullImageUploadRequest();
    fullImageUploadRequest.setImagePath(generateFinalImagePath(uploadImageRequest));
    MediumImageUploadRequest mediumImageUploadRequest = new MediumImageUploadRequest();
    mediumImageUploadRequest.setImagePath(
        getActiveResizedImagePathForGcsEnabled(Constant.MEDIUM_IMAGE,
            uploadImageRequest.getImageFileName()));
    ThumbNailImageUploadRequest thumbNailImageUploadRequest = new ThumbNailImageUploadRequest();
    thumbNailImageUploadRequest.setImagePath(
        getActiveResizedImagePathForGcsEnabled(Constant.THUMBNAIL_IMAGE,
            uploadImageRequest.getImageFileName()));
    xgpImageScaleRequest.setFullImageUploadRequest(fullImageUploadRequest);
    xgpImageScaleRequest.setMediumImageUploadRequest(mediumImageUploadRequest);
    xgpImageScaleRequest.setThumbNailImageUploadRequest(thumbNailImageUploadRequest);
    xgpImageScaleRequest.setImageBytes(fileBytes);
    return xgpImageScaleRequest;
  }

  public String uploadProductImagesToGCS(List<ProductBasicDetail> productBasicDetails, String contentType,
      byte[] fileBytes) throws Exception {
    String imageName = ImageUtil.generateImageName(0, contentType, productBasicDetails.get(0));
    for (ProductBasicDetail productBasicDetail : productBasicDetails) {
      //upload to GCS
      String finalImageName = productBasicDetail.getProductCode() + Constant.SLASH + imageName;
      UploadImageRequest uploadImageRequest = new UploadImageRequest();
      uploadImageRequest.setImageFileName(finalImageName);
      uploadImageRequest.setBytes(fileBytes);
      uploadImageRequest.setActive(true);
      uploadFullActiveImage(uploadImageRequest, contentType);
      uploadResizeActiveImageFile(Constant.MEDIUM_IMAGE,
          uploadImageRequest.getImageFileName().replace(ESCAPE_SPACE, StringUtils.EMPTY),
          Constant.UPLOAD_FINAL_IMAGE_MEDIUM_WIDTH, Constant.UPLOAD_FINAL_IMAGE_MEDIUM_HEIGHT,
          uploadImageRequest.getBytes(), contentType);
      uploadResizeActiveImageFile(Constant.THUMBNAIL_IMAGE,
          uploadImageRequest.getImageFileName().replace(ESCAPE_SPACE, StringUtils.EMPTY),
          Constant.UPLOAD_FINAL_IMAGE_THUMBNAIL_WIDTH, Constant.UPLOAD_FINAL_IMAGE_THUMBNAIL_HEIGHT,
          uploadImageRequest.getBytes(), contentType);
    }
    return imageName;
  }

  public void uploadFullActiveImage(UploadImageRequest request, String contentType) throws Exception {
    String path = generateFinalImagePath(request);
    try (ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream()) {
      byteArrayOutputStream.write(request.getBytes());
      byte[] bytes = byteArrayOutputStream.toByteArray();
      log.info("Uploading file to GCS at path: {} ", path);
      gcsService.uploadCreatedFileWithMimeType(finalImageBucket, path, bytes, contentType);
    }
  }

  private String generateFinalImagePath(UploadImageRequest request) {
    return ImageUtil.clearDoubleSlashFromPath(
        finalImageDirectory + File.separator + finalFullImageDirectory + File.separator + pathPrefix
            + File.separator + request.getImageFileName().replace(ESCAPE_SPACE, StringUtils.EMPTY));
  }

  public void uploadResizeActiveImageFile(String param, String imageName, int width, int height, byte[] imageData,
      String contentType) throws Exception {
    int extensionIndex = imageName.lastIndexOf(Constant.DOT);
    BufferedImage resizedImage = ImageUtil.resizeImage(width, height, imageData);
    String path = getActiveResizedImagePathForGcsEnabled(param, imageName);
    try (ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream()) {
      ImageIO.write(resizedImage, imageName.substring(extensionIndex + 1), byteArrayOutputStream);
      byte[] bytes = byteArrayOutputStream.toByteArray();
      log.info("Uploading file to GCS at path: {}", path);
      gcsService.uploadCreatedFileWithMimeType(finalImageBucket, path, bytes, contentType);
    }
  }

  public String getActiveResizedImagePathForGcsEnabled(String param, String imageName) {
    if (Constant.MEDIUM_IMAGE.equals(param)) {
      return ImageUtil.clearDoubleSlashFromPath(
          finalImageDirectory + File.separator + finalMediumImageDirectory + File.separator + pathPrefix
              + File.separator + imageName);
    } else {
      return ImageUtil.clearDoubleSlashFromPath(
          finalImageDirectory + File.separator + finalThumbnailImageDirectory + File.separator + pathPrefix
              + File.separator + imageName);
    }
  }

  @Override
  public boolean downloadAndValidateProductCreationImages(BulkProcess bulkProcess,
      BulkUploadErrorCounter bulkUploadErrorCounter, Map<String, String> imageAndImageUrlReverseMap,
      StringBuilder validationErrorMessage, List<String> images, String columnRowInformation, int imageMaxSize,
      boolean isInternationalMerchant) throws IOException {
    if (gcsEnabled) {
      return downloadAndValidateProductCreationImagesinGcs(bulkProcess, bulkUploadErrorCounter, imageAndImageUrlReverseMap,
          validationErrorMessage, images, columnRowInformation, imageMaxSize, isInternationalMerchant);
    } else {
      return ExcelTemplateUtil.downloadAndValidateProductCreationImages(bulkProcess, bulkUploadErrorCounter,
          imageAndImageUrlReverseMap, validationErrorMessage, images, columnRowInformation, imageMaxSize,
          isInternationalMerchant, decreaseImageResolution);
    }
  }

  private boolean downloadAndValidateProductCreationImagesinGcs(BulkProcess bulkProcess,
      BulkUploadErrorCounter bulkUploadErrorCounter, Map<String, String> imageAndImageUrlReverseMap,
      StringBuilder validationErrorMessage, List<String> images, String columnRowInformation, int imageMaxSize,
      boolean isInternationalMerchant) throws IOException {
    boolean result = true;
    byte[] file = null;
    for (String image : images) {
      try {
        file = gcsService.downloadFile(gcsBulkBucketName,
          gcsBasePath + Constant.SLASH + bulkProcess.getBulkProcessCode() + Constant.SLASH + ProcessorUtils.DATA_RAW_DIR
            + Constant.SLASH + image);
      }
      catch (ApplicationRuntimeException e){
        result = false;
        bulkUploadErrorCounter.incrementImage();
        String imageFileNameInExcel = image;
        if (imageAndImageUrlReverseMap.containsKey(image)) {
          imageFileNameInExcel = imageAndImageUrlReverseMap.get(image);
        }
        BulkCreationCommonUtil.addValidationErrorMessage(bulkUploadErrorCounter.getImage(), validationErrorMessage,
            errorMessage(imageFileNameInExcel + " - ", errorMessageBasedOnMerchant(isInternationalMerchant,
                BulkProcessValidationErrorMessages.IMAGE_FILE_NOT_FOUND_EN,
                BulkProcessValidationErrorMessages.IMAGE_FILE_NOT_FOUND), StringUtils.EMPTY));
        log.error("Bulk process: {}, Row {} : {} - {}.", bulkProcess.getBulkProcessCode(),
            (Integer.parseInt(columnRowInformation) + 1), BulkErrorCategory.FILE_NOT_FOUND.getDescription(),
            BulkProcessValidationErrorMessages.IMAGE_FILE_NOT_FOUND);
      }
          if(Objects.nonNull(file)) {
            int width = 0, height = 0;
            try (ByteArrayInputStream byteArrayInputStream = new ByteArrayInputStream(file)) {
              ImageInputStream imageInputStream = ImageIO.createImageInputStream(byteArrayInputStream);
              Iterator<ImageReader> imageReaders = ImageIO.getImageReaders(imageInputStream);
              if (imageReaders.hasNext()) {
                ImageReader reader = imageReaders.next();
                reader.setInput(imageInputStream, true);
                width = reader.getWidth(0);
                height = reader.getHeight(0);
              }
              try {
                ImageValidator.validateImages(file, imageMaxSize, width, height, decreaseImageResolution);
              } catch (ApplicationRuntimeException e) {
                result = false;
                if (bulkUploadErrorCounter.getImage() <= Constant.ERROR_COUNT) {
                  bulkUploadErrorCounter.incrementImage();
                  String imageFileNameInExcel = image;
                  if (imageAndImageUrlReverseMap.containsKey(image)) {
                    imageFileNameInExcel = imageAndImageUrlReverseMap.get(image);
                  }
                  if (e.getErrorMessage().contains(BulkProcessValidationErrorMessages.IMAGE_LESS_RESOLUTION_VALIDATION_ERR_MESSAGE_EN)) {
                    String commonErrorMessage = isInternationalMerchant ?
                      BulkProcessValidationErrorMessages.IMAGE_LESS_RESOLUTION_VALIDATION_ERR_MESSAGE_EN :
                      BulkProcessValidationErrorMessages.IMAGE_LESS_RESOLUTION_VALIDATION_ERR_MESSAGE;
                    validationErrorMessage.append(errorMessage(imageFileNameInExcel + " - ", commonErrorMessage, ""))
                      .append(Constant.PERIOD);
                }
                  else if (e.getErrorMessage()
                    .contains(BulkProcessValidationErrorMessages.IMAGE_MAX_SIZE_VALIDATION_ERR_MESSAGE_EN)) {
                  validationErrorMessage.append(errorMessage(imageFileNameInExcel + " - ",
                      errorMessageBasedOnMerchant(isInternationalMerchant,
                          BulkProcessValidationErrorMessages.IMAGE_MAX_SIZE_VALIDATION_ERR_MESSAGE_EN,
                          BulkProcessValidationErrorMessages.IMAGE_MAX_SIZE_VALIDATION_ERR_MESSAGE_EN),
                      StringUtils.EMPTY)).append(Constant.PERIOD);
                  }
                }
                log.error(
                  "Bulk process : {}, Row {} : {} - {}. Invalid image type for given file : {}",
                  bulkProcess.getBulkProcessCode(), (Integer.parseInt(columnRowInformation) + 1),
                  BulkErrorCategory.FILE_TYPE_INVALID.getDescription(),
                  BulkProcessValidationErrorMessages.IMAGE_SIZE_VALIDATION_ERR_MESSAGE_EN, e);
              }
            }
          }
        }
    return result;
  }

  @Override
  public void uploadImageFilesToSourceLocation(Map.Entry<String, String> mappingImageEntry, BulkProcess bulkProcess,
      String mtaSourcePath) throws Exception {
    if (gcsEnabled) {
      uploadImageFilesToSourceLocationFromGcs(mappingImageEntry, bulkProcess, mtaSourcePath);
    } else {
      ExcelTemplateUtil.uploadImageFiles(mappingImageEntry, bulkProcess, mtaSourcePath);
    }
  }

  private void uploadImageFilesToSourceLocationFromGcs(Map.Entry<String, String> mappingImageEntry,
      BulkProcess bulkProcess, String mtaSourcePath) throws Exception {
    byte[] file = gcsService.downloadFile(gcsBulkBucketName,
        gcsBasePath + Constant.SLASH + bulkProcess.getBulkProcessCode() + Constant.SLASH
            + ProcessorUtils.DATA_RAW_DIR + Constant.SLASH + mappingImageEntry.getKey());
    if (saveImageRawFolderToGcs) {
      String[] split = mappingImageEntry.getValue().split(Constant.SLASH);
      String productCode = split[0];
      String fileName = split[1];
      String filePath = sourceImageDirectory + Constant.SLASH + productCode + Constant.SLASH + fileName;
      gcsService.uploadCreatedFile(sourceImageBucket, filePath, file);
      log.info("File downloaded from raw folder uploaded to Gcp. filePath : {} ", filePath);
    } else {
      ProcessorUtils.createFile(mtaSourcePath + File.separator + mappingImageEntry.getValue(), file);
    }
  }

  @Override
  public String getEmailPrefix() {
    return gcsEnabled ? gcsBasePath : ProcessorUtils.X_BULK;
  }

  @Override
  public String generateFile(BulkDownloadRequest request, byte[] bytes) throws Exception {
    if (gcsEnabled) {
      String path = getGcsFileDownloadPath(request);
      gcsService.uploadCreatedFile(bulkBucket, path, bytes);
      return path;
    } else {
      BulkProcessHelper bulkProcessHelper = bulkProcessHelperFactory.getHelper(request);
      String directoryPath = bulkProcessHelper.getDirectory(request);
      String filepath = bulkProcessHelper.getFilePath(directoryPath, request.getFilename());
      ProcessorUtils.createDirectories(directoryPath);
      bulkProcessHelper.generateFile(filepath, bytes);
      return filepath;
    }
  }

  @Override
  public String getNotificationDetailPath(BulkDownloadRequest request) throws Exception {
    return gcsEnabled ? getGcsFileDownloadPath(request) : request.getRequestId();
  }

  private String getGcsFileDownloadPath(BulkDownloadRequest request) throws Exception {
    String generatedFileName =
      generateFileName(request.getBulkProcessEntity().name(), request.getMerchantId(),
        request.getFilename());
    BulkProcessHelper bulkProcessHelper = bulkProcessHelperFactory.getHelper(request);
    return gcsBasePath + Constant.SLASH + StringUtils
      .remove(bulkProcessHelper.getDirectory(request), ProcessorUtils.DATA_BASE_DIR)
      + Constant.SLASH + generatedFileName;
  }


  @Override
  public Map<String, byte[]> regenerateMasterBrandValuesSheet(String genericTemplateFileType) throws IOException {
    byte[] fileByteData;
    byte[] fileByteDataEn;
    Map<String, byte[]> fileByteDataMap = new HashMap<>();
    String fileName = Arrays.asList(unifiedUploadTemplateFile.split(Constants.COMMA))
        .get(Integer.parseInt(GenericTemplateFileType.valueOf(genericTemplateFileType).getValue()) - 1);
    String fileNameEnglish = Arrays.asList(unifiedUploadTemplateFileEnglish.split(Constants.COMMA))
        .get(Integer.parseInt(GenericTemplateFileType.valueOf(genericTemplateFileType).getValue()) - 1);
    fileByteData = gcsService.downloadFile(gcsBulkBucketName, gcsMassTemplateLocation + Constant.SLASH + fileName);
    fileByteDataEn =
        gcsService.downloadFile(gcsBulkBucketName, gcsMassTemplateLocation + Constant.SLASH + fileNameEnglish);
    log.info("regenerateMasterBrandValuesSheet, fileByteData : {} and  fileByteDataEn : {} ", fileByteData.length,
        fileByteDataEn.length);
    fileByteDataMap.put(FILE_BYTE_DATA, fileByteData);
    fileByteDataMap.put(FILE_BYTE_DATA_EN, fileByteDataEn);
    return fileByteDataMap;
  }

  @Override
  public void uploadGenericTemplateToGcs(byte[] genericTemplate, byte[] genericEnglishTemplate, String genericTemplateFileType)
      throws Exception {
    String filePath = gcsMassTemplateLocation + Constant.SLASH + Arrays.asList(unifiedUploadTemplateFile.split(Constants.COMMA))
        .get(Integer.parseInt(GenericTemplateFileType.valueOf(genericTemplateFileType).getValue()) - 1);
    String filePathEnglish = gcsMassTemplateLocation + Constant.SLASH + Arrays.asList(unifiedUploadTemplateFileEnglish.split(Constants.COMMA))
        .get(Integer.parseInt(GenericTemplateFileType.valueOf(genericTemplateFileType).getValue()) - 1);
    gcsService.uploadCreatedFile(bulkBucket, filePath, genericTemplate);
    gcsService.uploadCreatedFile(bulkBucket, filePathEnglish, genericEnglishTemplate);
  }

  @Override
  public String getCategoryTemplateFilePath() {
    if (gcsEnabled) {
      return gcsMassTemplateLocation;
    }
    return categoryTemplateLocation;
  }

  @Override
  public void uploadRegeneratedTemplates(byte[] brandValuesSheet, byte[] brandValuesSheetEnglish,
      String genericTemplateFileType, boolean isCategoryTemplate) throws Exception {
    if (isCategoryTemplate) {
      gcsService.uploadCreatedFile(bulkBucket, gcsMassTemplateLocation + Constant.SLASH + categoryBaseTemplatePath,
          brandValuesSheet);
      gcsService.uploadCreatedFile(bulkBucket, gcsMassTemplateLocation + Constant.SLASH + categoryBaseTemplatePathEn,
          brandValuesSheetEnglish);
    } else {
      String fileName = Arrays.asList(unifiedUploadTemplateFile.split(Constants.COMMA))
          .get(Integer.parseInt(GenericTemplateFileType.valueOf(genericTemplateFileType).getValue()) - 1);
      String fileNameEnglish = Arrays.asList(unifiedUploadTemplateFileEnglish.split(Constants.COMMA))
          .get(Integer.parseInt(GenericTemplateFileType.valueOf(genericTemplateFileType).getValue()) - 1);
      String filePath = gcsMassTemplateLocation + Constant.SLASH + fileName;
      String filePathEn = gcsMassTemplateLocation + Constant.SLASH + fileNameEnglish;
      gcsService.uploadCreatedFile(bulkBucket, filePath, brandValuesSheet);
      gcsService.uploadCreatedFile(bulkBucket, filePathEn, brandValuesSheetEnglish);
    }
  }

  @Override
  public Map<String,byte[]> isCategoryTemplateFileExist(String categoryUploadTemplateFile,
    String categoryUploadTemplateFileEnglish, String categoryBaseTemplateFile,
    String categoryBaseTemplateFileEnglish) throws Exception {
    if (gcsEnabled) {
      String sourcePath = gcsMassTemplateLocation +Constant.SLASH+ categoryBaseTemplateFileEnglish;
      String destinationPath =
        gcsMassTemplateLocation +Constant.SLASH+ categoryUploadTemplateFileEnglish;
      return createDestinationFileIfNotExists(categoryUploadTemplateFile, categoryBaseTemplateFile,
        sourcePath, destinationPath);
    }
    return CategoryExcelTemplateUtil.isUploadTemplateExist(categoryTemplateLocation,
      categoryUploadTemplateFile, categoryUploadTemplateFileEnglish, categoryBaseTemplateFile,
      categoryBaseTemplateFileEnglish);
  }

  private Map<String,byte[]> createDestinationFileIfNotExists(String categoryUploadTemplateFile,
    String categoryBaseTemplateFile, String sourcePath, String destinationPath) throws Exception {
    Map<String, byte[]> map = new HashMap<>();
    Blob blob = null;
    if (gcsService.isFileExists(gcsBulkBucketName, destinationPath)) {
      map.put(Constant.CATEGORY_UPLOAD_TEMPLATE_EN, gcsService.downloadFile(gcsBulkBucketName, destinationPath));
    } else {
      blob = gcsService.copyBlobInChunks(gcsBulkBucketName, sourcePath, destinationPath);
      if (Objects.isNull(blob)) {
        throw new FileNotFoundException();
      }
      map.put(Constant.CATEGORY_UPLOAD_TEMPLATE_EN, blob.getContent());
    }

    sourcePath = gcsMassTemplateLocation + Constant.SLASH + categoryBaseTemplateFile;
    destinationPath = gcsMassTemplateLocation + Constant.SLASH + categoryUploadTemplateFile;
    if (gcsService.isFileExists(gcsBulkBucketName, destinationPath)) {
      map.put(Constant.CATEGORY_UPLOAD_TEMPLATE, gcsService.downloadFile(gcsBulkBucketName, destinationPath));
    } else {
      blob = gcsService.copyBlobInChunks(gcsBulkBucketName, sourcePath, destinationPath);
      map.put(Constant.CATEGORY_UPLOAD_TEMPLATE, blob.getContent());
    }
    return map;
  }
  @Override
  public String getNotificationType(BulkProcessEntity bulkProcessEntity) {
    if (BulkProcessEntity.PRODUCT_VENDOR.equals(bulkProcessEntity)) {
      return NotificationTypeConstant.NOTIFICATION_TYPE_MAP.get(NotificationTypeConstant.VENDOR_BULK_DOWNLOADED)
          .get(gcsEnabled);
    } else {
      return NotificationTypeConstant.NOTIFICATION_TYPE_MAP.get(NotificationType.BULK_DOWNLOADED.getValue())
          .get(gcsEnabled);
    }
  }

  private String generateFileName(String downloadProcess, String businessPartnerCode,
    String defaultFileName) {
    if (downloadProcess.equalsIgnoreCase(BulkProcessEntity.PRODUCT.toString())) {
      return Constant.FILE_BULK_UPDATE_PRODUCT_TEMPLATE + ProcessorUtils.FILETYPE_XLSX_EXCEL;
    } else if (downloadProcess.equalsIgnoreCase(BulkProcessEntity.PRODUCT_EAN.toString())) {
        return Constant.FILE_BULK_UPDATE_PRODUCT_EAN_TEMPLATE + ProcessorUtils.FILETYPE_XLSX_EXCEL;
    } else if (downloadProcess.equalsIgnoreCase(BulkProcessEntity.ORDER.toString())) {
      return Constant.FILE_BULK_ORDER_DOWNLOAD_TEMPLATE + ProcessorUtils.FILETYPE_CSV;
    } else if (downloadProcess.equalsIgnoreCase(BulkProcessEntity.CAMPAIGN_PRODUCT.toString())) {
      return Constant.FILE_CAMPAIGN_PRODUCT_LIST + businessPartnerCode + ProcessorUtils.FILETYPE_XLSX_EXCEL;
    } else if (downloadProcess.equalsIgnoreCase(BulkProcessEntity.BULK_PRICE_RECOMMENDATION.toString())) {
      return Constant.BULK_PRICE_INFORMATION + ProcessorUtils.FILETYPE_XLSX_EXCEL;
    } else if (downloadProcess.equalsIgnoreCase(BulkProcessEntity.PRODUCT_BASIC_INFO.toString())) {
      return Constant.FILE_BULK_UPDATE_PRODUCT_BASIC_INFO_TEMPLATE + ProcessorUtils.FILETYPE_XLSX_EXCEL;
    } else {
      return defaultFileName;
    }
  }

  @Override
  public void moveTmpImageToProductImage(String requestId, Set<String> uniqueImageList, String productCode)
      throws Exception {
    if (saveImageRawFolderToGcs) {
      moveTmpImageToGcs(requestId, uniqueImageList, productCode);
    } else {
      moveTmpImageToFileStore(requestId, uniqueImageList, productCode);
    }
  }

  @Override
  public void uploadUnmappedSku(Workbook workbook, String requestId) throws Exception {
    if (gcsEnabled) {
      try (ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream()) {
        workbook.write(byteArrayOutputStream);
        String filePath = gcsUncategorisedSkuPath + File.separator + requestId + File.separator + requestId
            + ProcessorUtils.FILETYPE_XLSX_EXCEL;
        gcsService.uploadCreatedFile(bulkBucket, filePath, byteArrayOutputStream.toByteArray());
      }
    } else {
      String directory = ProcessorUtils.ENTITY_DIR_MAP.get(BulkProcessEntity.PRODUCT);
      ProcessorUtils.createDirectories(directory + requestId);
      ProcessorUtils.createXLSXFile(
          directory + requestId + File.separator + requestId + ProcessorUtils.FILETYPE_XLSX_EXCEL, workbook);
    }
  }

  @Override
  public void downloadFileAndGenerateErrorFile(String filePath, String errorFilePath,
      List<Pair<Integer, String>> excelRowNumberAndErrorMessageMapping, String processType) throws Exception {
    String bucketName = gcsBulkBucketName;
    Bucket bucket = bulkBucket;
    if (BulkInternalProcessType.BULK_SKU_LEVEL_REBATE.name().equals(processType)) {
      bucketName = gcsPricingBucketName;
      bucket = pricingBulkBucket;
    }
    try (InputStream fileInputStream = new ByteArrayInputStream(gcsService.downloadFile(bucketName, filePath))) {
      Sheet sheet =
          POIUtil.getSheetForInputStream(fileInputStream, filePath.endsWith(ProcessorUtils.FILETYPE_XLSX_EXCEL), 0);
      byte[] errorFile = ExcelTemplateUtil.generateRestrictedKeywordErrorSheet(sheet, excelRowNumberAndErrorMessageMapping, processType);
      gcsService.uploadCreatedFile(bucket, errorFilePath, errorFile);
    }
  }

  private void moveTmpImageToGcs(String requestId, Set<String> uniqueImageList, String productCode) throws Exception {
    for (String uniqueImagePath : uniqueImageList) {
      File sourceImage =
          new File(systemParameter.getMtaApiTmpImage() + File.separator + requestId + File.separator + uniqueImagePath);
      String filePath =
          (sourceImageDirectory + Constant.SLASH + productCode + Constant.SLASH + uniqueImagePath).replaceAll("//",
              "/");
      gcsService.uploadCreatedFile(sourceImageBucket, filePath, FileUtils.readFileToByteArray(sourceImage));
    }
  }

  private void moveTmpImageToFileStore(String requestId, Set<String> uniqueImageList, String productCode)
      throws IOException {
    for (String uniqueImagePath : uniqueImageList) {
      File newImage = new File((systemParameter.getMtaImageSource() + File.separator + productCode + File.separator
          + uniqueImagePath).replaceAll("//", "/"));
      newImage.getParentFile().mkdir();
      File sourceImage = new File((systemParameter.getMtaApiTmpImage() + File.separator + requestId + File.separator
          + uniqueImagePath).replaceAll("//", "/"));
      com.google.common.io.Files.copy(sourceImage, newImage);
    }
  }

  @Override
  public String getErrorFileLinkForListing(BulkProcess bulkProcess)
    throws JsonProcessingException, ParseException {
    if (Optional.ofNullable(bulkProcess.getTotalCount()).orElse(0).equals(0)) {
      return StringUtils.EMPTY;
    }
    StringBuilder errorFileLink = new StringBuilder();
    switch (getBulkProcessType(bulkProcess.getBulkProcessType())) {
      case PRODUCT_CREATION_UPLOAD:
      case PRODUCT_CREATION_UPLOAD_PRIORITY_1:
      case PRODUCT_CREATION_UPLOAD_PRIORITY_2:
      case CONVERTED_PRODUCT_CREATION_UPLOAD: {
        String bulkProcessCode;
        if (bulkProcess.getBulkProcessCode().length() > 10) {
          bulkProcessCode = bulkProcess.getBulkProcessCode().substring(0, 10);
        } else {
          bulkProcessCode = bulkProcess.getBulkProcessCode();
        }
        errorFileLink = new StringBuilder(
          staticBaseUrl + Constant.SLASH + gcsDownloadFailedProductsPath + bulkProcessCode
            + Constant.SLASH + bulkProcessCode + XLS_EXTENSION);
        break;
      }
      case EXTERNAL_CREATION_UPLOAD: {
        errorFileLink = new StringBuilder(
          staticBaseUrl + Constant.SLASH + CommonUtils.generateConvertedTemplateUploadPath(
            gcsDownloadFailedProductsPath, bulkProcess));
        break;
      }
      case PRODUCT_LEVEL_3:
      case PRODUCT_LEVEL_3_UPDATE_PRIORITY_1:
      case PRODUCT_LEVEL_3_UPDATE_PRIORITY_2: {
        //Case handling for Change in File name from requestId to Bulk process code
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern(START_DATE_FORMAT);
        LocalDate startDate =
          bulkProcess.getStartDate().toInstant().atZone(ZoneId.systemDefault()).toLocalDate();
        LocalDate comparison = LocalDate.parse(comparisonDate, formatter);
        String fileNamePrefix = startDate.isAfter(comparison) ?
          bulkProcess.getBulkProcessCode() :
          bulkProcess.getRequestId();

        errorFileLink = new StringBuilder(staticBaseUrl + Constant.SLASH + getBasePath(
          BulkProcessType.PRODUCT_CREATION_FAILED_DIR.getValue()) + fileNamePrefix + File.separator
          + fileNamePrefix + ProcessorUtils.FILETYPE_XLSX_EXCEL);
        break;
      }
      case EAN_PRODUCT_LEVEL_4: {
        errorFileLink = new StringBuilder(staticBaseUrl + Constant.SLASH + getBasePath(
            BulkProcessType.EAN_PRODUCT_LEVEL_4.getValue()) + bulkProcess.getBulkProcessCode() + File.separator
            + bulkProcess.getBulkProcessCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL);
        break;
      }
      case PRODUCT_BASIC_INFO:
      case PRODUCT_BASIC_INFO_PRIORITY_1:
      case PRODUCT_BASIC_INFO_PRIORITY_2: {
        String bulkProcessCode = StringUtils.left(bulkProcess.getBulkProcessCode(), errorFileNameSize);
        errorFileLink = new StringBuilder(staticBaseUrl + Constant.SLASH + getBasePath(
            BulkProcessType.PRODUCT_CREATION_FAILED_DIR.getValue()) + bulkProcessCode
            + File.separator + bulkProcessCode + ProcessorUtils.FILETYPE_XLSX_EXCEL);
        break;
      }
      case INSTANT_PICKUP_PRODUCT_UPSERT: {
        AuditTrailInfo auditTrailInfo =
          objectMapper.readValue(bulkProcess.getNotes(), AuditTrailInfo.class);
        errorFileLink.append(staticBaseUrl).append(Constant.SLASH)
          .append(getBasePath(DOWNLOAD_FAILED_UPSERT_MPP_PRODUCT.getValue()))
          .append(bulkProcess.getBulkProcessCode()).append(File.separator)
          .append(bulkProcess.getBulkProcessCode())
          .append(ProcessorUtils.getFileFormat(auditTrailInfo.getFileName()));
        break;
      }
      case CAMPAIGN: {
        String bulkProcessCode = StringUtils.left(bulkProcess.getBulkProcessCode(),
          CAMPAIGN_BULK_ERROR_PROCESS_CODE_TRIM_LENGTH);
        errorFileLink = new StringBuilder().append(getDownloadLinkHtml(
          staticBaseUrl + Constant.SLASH + gcsCampaignErrorUploadPath + bulkProcessCode
            + File.separator + bulkProcessCode + ProcessorUtils.FILETYPE_XLSX_EXCEL));
        break;
      }
      case INSTANT_PICKUP_PRODUCT_DELETE: {
        String bulkProcessCode =
          StringUtils.left(bulkProcess.getBulkProcessCode(), errorFileNameSize);
        if (isFileExisting(bulkProcess)) {
          errorFileLink = new StringBuilder(staticBaseUrl + Constant.SLASH + getBasePath(
            BulkProcessType.PICKUP_POINT_DELETE_ERROR.getValue()) + bulkProcessCode + File.separator
            + bulkProcessCode + ProcessorUtils.FILETYPE_XLSX_EXCEL);
        } else {
          return StringUtils.EMPTY;
        }
        break;
      }
        case SUBJECT_TO_VAT: {
            String bulkProcessCode = StringUtils.left(bulkProcess.getBulkProcessCode(),
                    SUBJECT_TO_VAT_ERROR_PROCESS_CODE_TRIM_LENGTH);
            String path = gcsSubjectToVatErrorPath + bulkProcessCode
                    + File.separator + bulkProcessCode + ProcessorUtils.FILETYPE_XLSX_EXCEL;
            errorFileLink = new StringBuilder().append(staticBaseUrl + Constant.SLASH + path);
            if (!gcsService.isFileExists(gcsBulkBucketName, path)) {
                errorFileLink = new StringBuilder(StringUtils.EMPTY);
            }
            break;
        }
      default:
        // Error File generation is not being done for VAT, InStore and Logistics
        return StringUtils.EMPTY;
    }
    return errorFileLink.toString();
  }

  public boolean isFileExisting(BulkProcess bulkProcess) {
    DateTimeFormatter formatter = DateTimeFormatter.ofPattern(START_DATE_FORMAT);
    LocalDate startDate =
      bulkProcess.getStartDate().toInstant().atZone(ZoneId.systemDefault()).toLocalDate();
    LocalDate comparison = LocalDate.parse(mppDeleteFileExistDate, formatter);
    return startDate.isAfter(comparison);
  }

  @Override
  public String fetchQRCodeTemplate(QrCodeRowInfo qrCodeRowInfo) {
    String key = StringUtils.joinWith(Constant.HYPHEN, qrCodeRowInfo.getQrGenerationType(),
        qrCodeRowInfo.getTemplateSize(), BooleanUtils.toString(
            BooleanUtils.toBooleanDefaultIfNull(qrCodeRowInfo.getIsDarkTheme(), false),
            Constant.THEME_DARK, Constant.THEME_LIGHT), qrCodeRowInfo.getQrPerPage().toString());
    String templateFilePath = qrCodeProperties.getQrConfigToTemplateName().get(key);
    if (StringUtils.isBlank(templateFilePath)) {
      throw new RuntimeException(String.format("No QR template VTL mapped for %s", key));
    }
    byte[] templateBytes = gcsService.downloadFile(gcsBulkBucketName,
        gcsMassTemplateLocation + QR_TEMPLATE_PATH + templateFilePath);
    return new String(templateBytes, Charset.defaultCharset());
  }

  @Override
  public boolean uploadFileToGcs(String filePath, String contentType, InputStream fileContents) throws Exception {
    Blob blob = gcsService.uploadCreatedFileStream(bulkBucket, filePath, contentType, fileContents);
    return Objects.nonNull(blob);
  }

  @Override
  public byte[] downloadFileFromGcs(String filePath) throws Exception {
    return gcsService.downloadFile(gcsBulkBucketName, filePath);
  }

  @Override
  public Page<Blob> listFilesAtGcsDirectory(String directory) {
    return gcsService.listFilesAtDirectory(gcsBulkBucketName, directory);
  }

  @Override
  public String replaceFilesAtGcsDirectory(String directory, String newFileName,
      String contentType, InputStream newFileContents) throws Exception {
    List<Blob> filesToDelete = new ArrayList<>();
    Page<Blob> files = gcsService.listFilesAtDirectory(gcsBulkBucketName, directory);
    files.iterateAll().forEach((Blob file) -> {
      if (!file.isDirectory()) {
        filesToDelete.add(file);
      }
    });

    String url = StringUtils.EMPTY;
    Blob uploadedFile = gcsService.uploadCreatedFileStream(bulkBucket,
        directory + File.separator + newFileName, contentType, newFileContents);
    if (Objects.nonNull(uploadedFile)) {
      url = uploadedFile.getMediaLink();
      gcsService.deleteFilesByBlobIds(filesToDelete.stream().map(BlobInfo::getBlobId)
          .collect(Collectors.toList()));
    }

    return url;
  }

  @Override
  public void uploadToPricingBucket(String filePath, byte[] bytes) throws Exception {
    gcsService.uploadCreatedFile(pricingBulkBucket, filePath, bytes);
  }

  @Override
  public void uploadFileToBulkBucket(String filePath, byte[] bytes) throws Exception {
    gcsService.uploadCreatedFile(bulkBucket, filePath, bytes);
  }

  @Override
  public byte[] downloadBaseTemplateForBulkBasicInfoUpdate(String fileName) {
    String filePath = gcsMassTemplateLocation + Constant.SLASH + fileName;
    return gcsService.downloadFile(gcsBulkBucketName, filePath);
  }

  @Override
  public Sheet getFileDataForBasicInfo(BulkBasicInfoRequest bulkBasicInfoRequest, BulkProcess bulkProcess)
      throws IOException {
    GdnPreconditions.checkArgument(bulkProcess != null, BulkUpdateServiceUtil.BULK_PROCESS_NULL_ERROR);
    final String fileName = bulkBasicInfoRequest.getFileName();
    return getDataSheetForBasicInfo(fileName, bulkProcess);
  }

  public Sheet getDataSheetForBasicInfo(String fileName, BulkProcess bulkProcess) throws IOException {
    GdnPreconditions.checkArgument(Objects.nonNull(bulkProcess), BulkUpdateServiceUtil.BULK_PROCESS_NULL_ERROR);
    try (InputStream fileInputStream = new ByteArrayInputStream(gcsService.downloadFile(gcsBulkBucketName,
        getBasePath(bulkProcess.getBulkProcessType()) + bulkProcess.getBulkProcessCode() + File.separator
            + fileName))) {
      return POIUtil.getSheetForInputStream(fileInputStream, fileName.endsWith(ProcessorUtils.FILETYPE_XLSX_EXCEL), 0);
    }
  }

  @Override
  public List<MultipartFile> getListOfMultipartFile(String username, List<String> fileNames)
      throws Exception {
    List<MultipartFile> multiPartFileList = new ArrayList<>();
    for (String fileName : fileNames) {
      String originalFileName = generateFileName(fileName, username);
      byte[] file = gcsService.downloadFile(gcsBulkBucketName, originalFileName);
      if (Objects.nonNull(file)) {
        multiPartFileList.add(new MockMultipartFile(fileName, originalFileName, null,
            new ByteArrayInputStream(file)));
      }
    }
    return multiPartFileList;
  }

  public String generateFileName(String fileName, String username) {
    return gcsUploadPath + username + Constants.DELIMITER_SLASH
        + fileName;
  }

  public InputStream openGcsInputStream(BulkUpdateQueue bulkUpdateQueue, BulkProcess bulkProcess) {
    GdnPreconditions.checkArgument(Objects.nonNull(bulkProcess),
      BulkUpdateServiceUtil.BULK_PROCESS_NULL_ERROR);
    String fileName = bulkUpdateQueue.getFileName();
    String filePath =
      getBasePath(bulkProcess.getBulkProcessType()) + bulkProcess.getBulkProcessCode()
        + File.separator + bulkProcess.getBulkProcessCode() + ProcessorUtils.getFileFormat(
        fileName);
    return gcsService.openFileStream(gcsBulkBucketName, filePath);
  }
}

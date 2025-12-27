package com.gdn.mta.bulk.service.download;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.List;

import com.gdn.mta.bulk.MerchantStatusType;
import com.gdn.mta.bulk.dto.BulkUpdateProcessDTO;
import com.gdn.mta.bulk.dto.UnifiedBulkDownloadDTO;
import com.gdn.mta.bulk.entity.BulkProcessData;
import com.gdn.mta.bulk.helper.ExcelTemplateUtil;
import com.gdn.mta.bulk.service.BulkDownloadService;
import com.gdn.mta.bulk.service.BulkProcessDataService;
import com.gdn.mta.bulk.service.FileStorageService;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.util.BulkCreationCommonUtil;
import com.gdn.mta.bulk.util.POIUtil;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.poi.ss.usermodel.Workbook;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.bulk.BulkDownloadEntityStatus;
import com.gdn.mta.bulk.config.SystemParameter;
import com.gdn.mta.bulk.dto.BulkProcessType;
import com.gdn.mta.bulk.entity.BulkDownloadEntity;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.helper.BulkProductProcessHelper;
import com.gdn.mta.bulk.models.BulkProcessEntity;
import com.gdn.mta.bulk.repository.BulkDownloadAuditRepository;
import com.gdn.mta.bulk.service.NotificationService;
import com.gdn.mta.bulk.service.PCBOutboundService;
import com.gdn.mta.bulk.util.BulkParameters;
import com.gdn.mta.bulk.util.BulkUpdateServiceUtil;
import com.gdn.mta.bulk.util.CommonUtils;
import com.gdn.mta.bulk.util.ExcelUtils;
import com.gdn.mta.bulk.util.GenericBulkParameters;
import com.gdn.mta.notification.enumeration.NotificationType;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;

import static com.gdn.mta.bulk.util.ProcessorUtils.FILETYPE_EXCEL;

@Service
public class BulkFailedProductFileServiceBean implements BulkFailedProductFileService {

  private static final Logger LOG = LoggerFactory.getLogger(BulkFailedProductFileServiceBean.class);

  @Autowired
  private BulkProductProcessHelper helper;

  @Autowired
  private SystemParameter systemParameter;

  @Autowired
  private NotificationService notificationService;

  @Autowired
  private BulkDownloadAuditRepository bulkDownloadAuditRepository;

  @Autowired
  private BulkDownloadService bulkDownloadService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private PCBOutboundService pcbOutboundService;

  @Autowired
  private FileStorageService fileStorageService;

  @Autowired
  private BulkProcessDataService bulkProcessDataService;

  @Value("${mta.contextPath}")
  private String mtaContextPath;

  @Value("${static.baseUrl}")
  private String staticBaseUrl;

  @Value("${product.bundling.enabled}")
  private boolean productBundlingEnabled;

  @Value("${product.bundling.eligible.merchants}")
  private String productBundlingEligibleMerchantTypes;

  @Value("${bulk.update.template.column.width}")
  private int bulkUpdateTemplateColumnWidth;

  @Value("${product.suitability.feature.enabled}")
  private boolean productSuitabilityFeatureEnabled;

  @Value("${gcs.download.failed.products.path}")
  private String gcsDownloadFailedProductsPath;

  @Value("${generic.sheet.password}")
  private String genericSheetPassword;

  @Override
  public String createFile(List<List<Object>> rawExcelHeaders, BulkProcess bulkProcess,
    List<List<Object>> invalidRows, String sheetName, boolean mergeEmptyValueCells)
    throws ApplicationException, IOException {
    List<List<String>> excelHeaders = getExcelHeader(rawExcelHeaders);
    List<List<String>> excelData = getExcelData(invalidRows, excelHeaders.get(0).size());
    Workbook dataSheet =
        helper.generateDataSheetWithHSSFWorkBook(excelHeaders, excelData, sheetName, mergeEmptyValueCells);
    String fileName;
    if(bulkProcess.getBulkProcessCode().length() > 10) {
      fileName = bulkProcess.getBulkProcessCode().substring(0, 10);
    } else {
      fileName = bulkProcess.getBulkProcessCode();
    }
    ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
    try {
      saveToDownload(bulkProcess);

      outputStream = new ByteArrayOutputStream();
      dataSheet.write(outputStream);
      byte[] byteDataSheet = outputStream.toByteArray();

      BulkUpdateProcessDTO processDTO = BulkUpdateProcessDTO.builder().fileContent(byteDataSheet)
        .bulkProcessType(BulkProcessType.PRODUCT_CREATION_FAILED_DIR.getValue()).build();
      fileStorageService.createBulkFile(processDTO, fileName, FILETYPE_EXCEL);

    } catch (Exception e) {
      LOG.error("Error creating failed products file bulk process: {} - {}", bulkProcess.getBulkProcessCode(),
          e.getMessage(), e);
      throw new ApplicationException(ErrorCategory.UNSPECIFIED, e.getMessage(), e);
    } finally {
      outputStream.close();
    }
    return fileStorageService.getFailedProductDownloadLink(fileName);
  }

  @Override
  public int deleteFileWithModifiedDateLastWeek() throws IOException {
    int result = 0;
    Path deletedProducts = Paths.get(helper.getFailedProductDirectory(null));
    try (DirectoryStream<Path> stream = getLastWeekDirectoryStream(deletedProducts)) {
      for (Path path : stream) {
        if (deleteDirectory(path)) {
          result++;
        }
      }
    }
    return result;
  }

  private DirectoryStream<Path> getLastWeekDirectoryStream(Path deletedProductDir)
      throws IOException {
    return Files.newDirectoryStream(deletedProductDir, new DirectoryStream.Filter<Path>() {
      @Override
      public boolean accept(Path entry) throws IOException {
        return Files.getLastModifiedTime(entry, LinkOption.NOFOLLOW_LINKS).toMillis() <= getDateLastWeek()
            .getTime();
      }
    });
  }

  private boolean deleteDirectory(Path path) {
    try {
      FileUtils.deleteDirectory(path.toFile());
      return true;
    } catch (Exception e) {
      ApplicationException ae =
          new ApplicationException(ErrorCategory.DATA_ACCESS, e.getMessage(), e);
      LOG.error(ae.getMessage());
      return false;
    }
  }

  private Date getDateLastWeek() {
    Date date = new Date();
    Calendar calendar = Calendar.getInstance();
    calendar.setTime(date);
    calendar.add(Calendar.DATE, -7);
    calendar.set(Calendar.HOUR, 23);
    calendar.set(Calendar.MINUTE, 59);
    calendar.set(Calendar.SECOND, 59);
    return calendar.getTime();
  }

  @Override
  public String getDownloadLinkHtml(String url) {
    return new StringBuilder().append("<a href=\"").append(url)
        .append("\">Download</a>").toString();
  }

  @Override
  public void createFileAndSendNotification(BulkProcess bulkProcess, List<List<Object>> userInputRows,
      MerchantStatusType merchantStatusType, String merchantType, boolean instoreSeller) {
    try {
      String downloadLink = StringUtils.EMPTY;
      if (CollectionUtils.isNotEmpty(userInputRows)) {
        LOG.info("Start file creation for bulkProcess : {} {} ", bulkProcess.getBulkProcessCode(),
          userInputRows.size());
        String failedProductLink;
        if (Constant.GENERIC.equals(bulkProcess.getNotes())) {
          failedProductLink = this.createFile(
              ExcelUtils.getExcelHeaders(bulkProcess.getInternationalMerchant(), merchantStatusType, merchantType,
                  productBundlingEligibleMerchantTypes, productBundlingEnabled, instoreSeller),
            bulkProcess,
              userInputRows, GenericBulkParameters.USER_INPUT_DATA_SHEET, false);
        } else {
          CategoryDetailResponse category = pcbOutboundService.getCategoryDetailResponse(bulkProcess.getNotes());
          CommonUtils.filterHideFromSellerAttributes(productSuitabilityFeatureEnabled, category);
          failedProductLink = this.createFile(
              ExcelUtils.getCnExcelHeaders(bulkProcess.getInternationalMerchant(), category, merchantStatusType,
                  merchantType, productBundlingEnabled, productBundlingEligibleMerchantTypes, instoreSeller),
            bulkProcess, userInputRows, BulkParameters.DATA_SHEET, true);
        }
        LOG.info("failedProductLink : {} ", failedProductLink);
        downloadLink = this.getDownloadLinkHtml(failedProductLink);
        LOG.info("Failed product link for Generic bulk process : {}, {} ", bulkProcess.getBulkProcessCode(), downloadLink);
      }
      notificationService
        .sendBulkUploadedNotification(bulkProcess, NotificationType.BULK_UPLOADED.getValue(),
          downloadLink);
      BulkUpdateServiceUtil.removeDirectory(BulkProcessType.PRODUCT_CREATION_UPLOAD.getValue(),
        bulkProcess.getBulkProcessCode());
    } catch (Exception ex) {
      LOG.error("Error creating file and sending notification for bulkProcessCode: {} ",
        bulkProcess.getBulkProcessCode(), ex);
    }
  }


  @Override
  public void createErrorFileForConvertedUploadProcess(int internalActivationPeriod,
    BulkProcess bulkProcess, List<BulkProcessData> failedData, String merchantType) {
    try {
      BulkCreationCommonUtil.setStateAndFetchInvalidRows(bulkProcess, failedData);
      BulkCreationCommonUtil.setDescriptionForGeneric(bulkProcess, internalActivationPeriod,
        failedData.size());
      if (CollectionUtils.isEmpty(failedData)) {
        notificationService.sendBulkUploadedNotification(bulkProcess,
          NotificationType.BULK_UPLOADED.getValue(), StringUtils.EMPTY);
        return;
      }

      UnifiedBulkDownloadDTO unifiedBulkDownloadDTO =
        bulkDownloadService.downloadProductUnifiedTemplate(bulkProcess.getStoreId(),
          Constant.REQUEST_ID,
          bulkProcess.getBusinessPartnerCode(), Collections.emptySet());


      LOG.info("Creating error file for bulkProcess: {} with {} failed records", 
          bulkProcess.getBulkProcessCode(), failedData.size());

      byte[] modifiedFileBytes =
        POIUtil.processExcelFileWithErrors(unifiedBulkDownloadDTO.getDestinationFileByteFile(),
          failedData, genericSheetPassword, objectMapper,
          ExcelTemplateUtil.isEligibleForBundleCreation(merchantType,
            productBundlingEligibleMerchantTypes, productBundlingEnabled));

      String filePath =
        CommonUtils.generateConvertedTemplateUploadPath(gcsDownloadFailedProductsPath, bulkProcess);

      fileStorageService.uploadFileToBulkBucket(filePath, modifiedFileBytes);

      notificationService.sendBulkUploadedNotification(bulkProcess,
        NotificationType.BULK_UPLOADED.getValue(),
        getDownloadLinkHtml(staticBaseUrl.concat(Constant.SLASH).concat(filePath)));

      LOG.info("Converted file created successfully for bulkProcess: {}",
        bulkProcess.getBulkProcessCode());

      LOG.info("Save {} blp : {}", bulkProcess.getStatus(), bulkProcess.getBulkProcessCode());

    } catch (Exception ex) {
      LOG.error("Error creating error file for converted upload process, bulkProcessCode: {}", 
          bulkProcess.getBulkProcessCode(), ex);
    }
  }

  private List<List<String>> getExcelData(List<List<Object>> invalidRows, int columnLength) {
    List<List<String>> excelData = new ArrayList<>();
    for(List<Object> invalidRow : invalidRows){
      List<String> excelDataRow = new ArrayList<>();
      for (int i = 0; i < columnLength; i++) {
        Object obj = invalidRow.get(i);
        excelDataRow.add(String.valueOf(obj));
      }
      excelData.add(excelDataRow);
    }
    
    return excelData;
  }

  private List<List<String>> getExcelHeader(List<List<Object>> rawExcelHeaders)
      throws ApplicationException {
    if (rawExcelHeaders == null || rawExcelHeaders.isEmpty()) {
      throw new ApplicationException(ErrorCategory.VALIDATION, "Excel header cannot be null");
    }
    List<List<String>> excelHeaders = new ArrayList<>();
    for (List<Object> headerRow: rawExcelHeaders) {
      List<String> excelHeader = new ArrayList<>();
      for (Object obj : headerRow) {
        if (!BulkParameters.ERROR_HEADER.equals(String.valueOf(obj))) {
          excelHeader.add(String.valueOf(obj));
        }
      }
      excelHeader.add(BulkParameters.ERROR_HEADER);
      excelHeaders.add(excelHeader);
    }
    return excelHeaders;
  }

  private String getMtaDownloadPath(String fileName) {
    return new StringBuilder().append(staticBaseUrl)
        .append("/failed-products/").append(fileName).append("/").append(fileName).append(".xls").toString();
  }

  private void saveToDownload(BulkProcess bulkProcess) {
    BulkDownloadEntity bulkDownloadEntity = new BulkDownloadEntity();
    bulkDownloadEntity.setBusinessPartnerCode(bulkProcess.getBusinessPartnerCode());
    bulkDownloadEntity.setCreatedBy(bulkProcess.getCreatedBy());
    bulkDownloadEntity.setCreatedDate(bulkProcess.getCreatedDate());
    bulkDownloadEntity.setDescription(bulkProcess.getDescription());
    bulkDownloadEntity.setEndDate(bulkProcess.getEndDate());
    bulkDownloadEntity.setEntityType(BulkProcessEntity.FAILED_PRODUCT.toString());
    bulkDownloadEntity.setFileName(bulkProcess.getBulkProcessCode() + ".xls");
    bulkDownloadEntity.setMarkForDelete(false);
    bulkDownloadEntity.setRequestId(bulkProcess.getBulkProcessCode());
    bulkDownloadEntity.setStatus(BulkDownloadEntityStatus.STATUS_SUCCESS.getStatusValue());
    bulkDownloadAuditRepository.save(bulkDownloadEntity);
  }

}

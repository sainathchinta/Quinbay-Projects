package com.gdn.mta.bulk.service.download;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.gdn.mta.bulk.service.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.BulkDownloadEntityStatus;
import com.gdn.mta.bulk.BulkDownloadException;
import com.gdn.mta.bulk.SystemParameterConfigNames;
import com.gdn.mta.bulk.dto.BulkInternalProcessDTO;
import com.gdn.mta.bulk.dto.BulkPendingRequestsResponse;
import com.gdn.mta.bulk.dto.BulkProcessType;
import com.gdn.mta.bulk.dto.TaggedProductFilterRequest;
import com.gdn.mta.bulk.dto.product.TaggedProductFilterDTO;
import com.gdn.mta.bulk.entity.BulkDownloadEntity;
import com.gdn.mta.bulk.factory.BulkProcessDataFactory;
import com.gdn.mta.bulk.factory.BulkProcessHelperFactory;
import com.gdn.mta.bulk.helper.BulkProcessHelper;
import com.gdn.mta.bulk.models.BulkProcessEntity;
import com.gdn.mta.bulk.models.FileType;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.RecatFailedProductsDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkDataResponse;
import com.gdn.mta.bulk.repository.BusinessPartnerRepository;
import com.gdn.mta.bulk.service.BulkProcessService;
import com.gdn.mta.bulk.service.FileStorageService;
import com.gdn.mta.bulk.service.MailDeliveryService;
import com.gdn.mta.bulk.service.NotificationService;
import com.gdn.mta.bulk.service.SystemParameterConfigService;
import com.gdn.partners.bulk.util.Constant;

/**
 * Created by keshashah on 24/10/16.
 */
@Service
public class BulkProcessDownloadServiceBean implements BulkProcessDownloadService {

  private static final Logger LOGGER =
      LoggerFactory.getLogger(BulkProcessDownloadServiceBean.class);
  public static final String DELIMITER = "/";
  public static final String FILESTORE = "filestore";

  @Autowired
  private BulkProcessDataFactory bulkProcessDataFactory;

  @Autowired
  private BulkProcessHelperFactory bulkProcessHelperFactory;

  @Autowired
  private BulkDownloadAuditService bulkDownloadAuditService;

  @Autowired
  private MailDeliveryService mailDeliveryService;

  @Autowired
  private BulkProcessFileGeneration bulkProcessFileGeneration;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private SystemParameterConfigService systemParameterConfigService;

  @Autowired
  private NotificationService notificationService;

  @Autowired
  private FileStorageService fileStorageService;

  @Autowired
  private BusinessPartnerRepository businessPartnerRepository;

  @Autowired
  private BulkProcessService bulkProcessService;

  @Value("${process.download.new.flow}")
  private boolean processDownloadNewFlow;

  @Override
  public void downloadAll(BulkDownloadRequest request) throws Exception {
    BulkPendingRequestsResponse bulkPendingRequestsResponse =
        bulkProcessService.checkForPendingBulkProcess(Constant.STORE_ID, request.getUsername(),
            Constant.BULK_DOWNLOAD_TYPE, request.getMerchantId(), null, request.getBulkProcessEntity().name());
    if (bulkPendingRequestsResponse.getPendingRequestsCount() < Constant.LONG_ONE) {
      bulkDownloadAuditService.createAuditLog(request, BulkDownloadEntityStatus.STATUS_PENDING.getStatusValue());
    } else {
      LOGGER.info("Download request already in pending state. request : {} ", request);
    }
  }

  @Override
  @Async
  public void processDownload(String storeId) throws Exception {
    BulkDownloadRequest request = null;
    String finalStatus = StringUtils.EMPTY;
    String errorMessage = StringUtils.EMPTY;
    int recordsDownloaded = 0;
    int batchSize = Integer.parseInt(systemParameterConfigService
        .findValueByStoreIdAndVariable(storeId, SystemParameterConfigNames.DOWNLOAD_PROCESS_BATCH_SIZE).getValue());
    List<BulkDownloadEntity> bulkDownloadEntities = bulkDownloadAuditService
        .getPendingAuditLogs(BulkDownloadEntityStatus.STATUS_PENDING.getStatusValue(), batchSize);
    if (processDownloadNewFlow) {
      bulkDownloadEntities.stream().forEach(bulkDownloadEntity -> bulkDownloadEntity.setStatus(
          BulkDownloadEntityStatus.STATUS_IN_PROGRESS.getStatusValue()));
      bulkDownloadEntities = bulkDownloadAuditService.saveBulkDownloadEntityList(bulkDownloadEntities);
    }
    for (BulkDownloadEntity entity : bulkDownloadEntities) {
      try {
        if (!processDownloadNewFlow) {
          entity.setStatus(BulkDownloadEntityStatus.STATUS_IN_PROGRESS.getStatusValue());
          entity = bulkDownloadAuditService.saveBulkDownloadEntity(entity);
        }
        request =
          objectMapper.readValue(entity.getRequestBody(), BulkDownloadRequest.class);
        recordsDownloaded = 0;
        finalStatus = BulkDownloadEntityStatus.STATUS_SUCCESS.getStatusValue();
        errorMessage = StringUtils.EMPTY;
        try {
          BulkProcessDataService dataService = bulkProcessDataFactory.getRepository(request);
          BulkDataResponse response = dataService.getData(request);
          BulkProcessHelper helper = bulkProcessHelperFactory.getHelper(request);
          response.setBusinessPartnerCode(entity.getBusinessPartnerCode());
          bulkProcessFileGeneration.generateFileFromResponse(request, response, helper);
          recordsDownloaded = helper.getRecordsUpdated(response);
          Map<String, Object> emailParams = helper.getEmailParams(request, request.getLanguage());
          this.sendEmailAndNotification(request, emailParams, true, response.isPartialDownload());
        } catch (BulkDownloadException bulkException) {
          LOGGER.error("Bulk Download: Exception occurred while processing request {}, error: {}",
            request, bulkException.getErrorMessage(), bulkException);
          finalStatus = BulkDownloadEntityStatus.STATUS_FAILED.getStatusValue();
          errorMessage = bulkException.getErrorCode();
        } catch (Exception e) {
          LOGGER.error("Bulk Download: Exception occurred while processing request {}", request, e);
          finalStatus = BulkDownloadEntityStatus.STATUS_FAILED.getStatusValue();
          errorMessage = e.getMessage();
        }
        if (finalStatus.equalsIgnoreCase(BulkDownloadEntityStatus.STATUS_FAILED.getStatusValue())) {
          this.sendEmailAndNotification(request, null, false, false);
        }
      }
      catch (Exception e){
        LOGGER.error("Bulk Download: Exception occurred while processing request {},  error: {}", request, e);
        finalStatus = BulkDownloadEntityStatus.STATUS_FAILED.getStatusValue();
        errorMessage = e.getMessage();
      }
      finally {
        bulkDownloadAuditService.updateAuditLog(request.getRequestId(), finalStatus, recordsDownloaded, errorMessage);
      }

    }
  }

  @Override
  public void processDownloadTaggedProducts(TaggedProductFilterRequest taggedProductFilterRequest) throws Exception {
    String requestId = UUID.randomUUID().toString();
    String fileName = new StringBuilder().append(requestId).append(".").append(FileType.XLSX.name()).toString();
    TaggedProductFilterDTO taggedProductFilterDTO = new TaggedProductFilterDTO();
    BeanUtils.copyProperties(taggedProductFilterRequest, taggedProductFilterDTO);
    taggedProductFilterDTO.setRequestId(requestId);
    taggedProductFilterDTO.setFilename(fileName);
    taggedProductFilterDTO.setFileType(FileType.XLSX);
    taggedProductFilterDTO.setDirectDownload(false);
    taggedProductFilterDTO.setUsername(taggedProductFilterDTO.getEmailAddress());
    taggedProductFilterDTO.setEmailTo(taggedProductFilterDTO.getEmailAddress());
    taggedProductFilterDTO.setBulkProcessEntity(BulkProcessEntity.BULK_DOWNLOAD_TAGGED_PRODUCTS);
    bulkDownloadAuditService.createAuditLog(taggedProductFilterDTO,
        BulkDownloadEntityStatus.STATUS_PENDING.getStatusValue());
  }

  private void sendEmailAndNotification(BulkDownloadRequest request,
      Map<String, Object> emailParams, boolean success, boolean partialDownload) throws Exception {
    List<BulkProcessEntity> emailBulkProcessEntities =
        Arrays.asList(BulkProcessEntity.MASTER_PRODUCT, BulkProcessEntity.SELECTED_MASTER_PRODUCTS,
            BulkProcessEntity.STORE_COPY_PRODUCTS, BulkProcessEntity.MASTER_SKU_ALL_ITEMS_DOWNLOAD,
            BulkProcessEntity.MASTER_SKU_SELECTED_ITEMS_DOWNLOAD, BulkProcessEntity.AUTO_APPROVED_PRODUCTS_ALL_DOWNLOAD,
            BulkProcessEntity.AUTO_APPROVED_PRODUCTS_SELECTED_DOWNLOAD,
            BulkProcessEntity.BULK_DOWNLOAD_TAGGED_PRODUCTS, BulkProcessEntity.BULK_PRICE_RECOMMENDATION,
            BulkProcessEntity.IPR_PRODUCTS_DOWNLOAD_ALL);
    if (success) {
      if (emailBulkProcessEntities.contains(request.getBulkProcessEntity()) || !request.isDirectDownload()) {
        mailDeliveryService.sendEmail(request, emailParams);
      }
      if (!emailBulkProcessEntities.contains(request.getBulkProcessEntity()) ) {
        notificationService.sendDownloadNotification(request, true, partialDownload);
      }
    } else {
      if (!request.isDirectDownload()) {
        mailDeliveryService.sendBulkDownloadErrorMail(request);
      }
      if (!emailBulkProcessEntities.contains(request.getBulkProcessEntity())) {
        notificationService.sendDownloadNotification(request, false, false);
      }
    }
  }

  @Override
  public void downloadExcelFile(BulkDownloadRequest request, String value) throws Exception {
    BulkProcessHelper helper = bulkProcessHelperFactory.getHelper(request);
    RecatFailedProductsDownloadRequest recatFailedProductsDownloadRequest =
        (RecatFailedProductsDownloadRequest) request;
    String filePath = helper.getFilePath(fileStorageService.getBasePath(BulkProcessType.RECAT_ERROR.getValue())
        .concat(recatFailedProductsDownloadRequest.getRecatRequestCode()), request.getFilename());
    if (!fileStorageService.checkIfFileExistsByFilePath(filePath)) {
      BulkProcessDataService dataService = bulkProcessDataFactory.getRepository(request);
      BulkDataResponse response = dataService.getData(request);
      LOGGER.info("creating excel sheet for recat failed product, requestId : {}", request.getRequestId());
      BulkInternalProcessDTO bulkInternalProcessDTO =
          bulkProcessFileGeneration.generateFileResponse(request, response, helper);
      fileStorageService.createBulkInternalFile(request, bulkInternalProcessDTO, value);
    }
  }

  @Override
  public String internalProcessFailedDownloadExcelFile(BulkDownloadRequest request, String processType) throws Exception {
    BulkProcessHelper helper = bulkProcessHelperFactory.getHelper(request);
    BulkProcessDataService dataService = bulkProcessDataFactory.getRepository(request);
    BulkDataResponse response = dataService.getData(request);
    LOGGER.info("creating excel sheet for internal process failed product, requestId : {}", request.getRequestId());
    BulkInternalProcessDTO bulkInternalProcessDTO =
        bulkProcessFileGeneration.generateFileResponse(request, response, helper);
    String filePath = fileStorageService.createBulkInternalFile(request, bulkInternalProcessDTO, processType);
    filePath = String.join(DELIMITER, ArrayUtils.removeElement(filePath.split(DELIMITER), FILESTORE));
    return filePath;
  }

  @Override
  public String downloadAndOverwriteExcelFile(BulkDownloadRequest request) throws Exception {
    BulkProcessHelper helper = bulkProcessHelperFactory.getHelper(request);
    BulkProcessDataService dataService = bulkProcessDataFactory.getRepository(request);
    BulkDataResponse response = dataService.getData(request);
    LOGGER.info("create/overwrite excel sheet , requestId : {}", request.getRequestId());
    String filePath = bulkProcessFileGeneration.generateFileFromResponse(request, response, helper);
    filePath = String.join(DELIMITER, ArrayUtils.removeElement(filePath.split(DELIMITER), FILESTORE));
    return filePath;
  }


}

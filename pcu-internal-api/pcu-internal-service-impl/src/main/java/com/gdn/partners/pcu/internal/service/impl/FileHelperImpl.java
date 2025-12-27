package com.gdn.partners.pcu.internal.service.impl;

import java.io.File;
import java.io.IOException;

import com.blibli.oss.kafka.producer.KafkaProducer;
import com.gdn.partners.pcu.internal.model.ErrorMessages;
import com.gdn.partners.pcu.internal.properties.KafkaTopicProperties;
import com.gdn.partners.pcu.internal.service.impl.config.KafkaPublisher;
import com.gdn.partners.pcu.internal.service.impl.event.model.BulkReviewUploadModel;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.partners.pcu.internal.properties.SystemParameterProperties;
import com.gdn.partners.pcu.internal.service.FileHelper;
import com.gdn.partners.pcu.internal.service.FileStorageService;
import com.gdn.partners.pcu.internal.service.impl.helper.RequestHelper;
import com.gdn.partners.pcu.internal.service.model.BulkInternalProcessType;

@Service
@Slf4j
public class FileHelperImpl implements FileHelper {

  private static final String STORE_COPY_PROCESS_TYPE = "STORE_COPY";
  private static final String SALES_CATEGORY_UPDATE_PROCESS_TYPE = "SALES_CATEGORY_UPDATE";
  private static final String DELETE_BRAND_AUTHORISATION_PROCESS_TYPE = "DELETE_BRAND_AUTHORISATION";
  private static final String BULK_PRICE_UPDATE = "BULK_PRICE_UPDATE";

  @Autowired
  private SystemParameterProperties systemParameterProperties;

  @Autowired
  private FileStorageService fileStorageService;

  @Autowired
  private KafkaPublisher kafkaProducer;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Override
  public void uploadFileBasedOnProcessType(MultipartFile multipartFile, String processType, String bipRequestCode)
      throws Exception {
    switch (processType) {
      case STORE_COPY_PROCESS_TYPE : {
        uploadBIPExcel2(bipRequestCode, multipartFile, BulkInternalProcessType.INTERNAL_STORE_COPY.getValue());
        break;
      }
      case SALES_CATEGORY_UPDATE_PROCESS_TYPE: {
        uploadBIPExcel2(bipRequestCode, multipartFile, BulkInternalProcessType.INTERNAL_SALES_CATEGORY_UPDATE.getValue());
        break;
      }
      case BULK_PRICE_UPDATE: {
        uploadBIPExcel2(bipRequestCode, multipartFile, BulkInternalProcessType.BULK_PRICE_UPDATE.getValue());
        break;
      }
      case DELETE_BRAND_AUTHORISATION_PROCESS_TYPE : {
        uploadBIPExcel(systemParameterProperties.getDeleteBrandAuthorisation(), bipRequestCode, multipartFile,
            multipartFile.getOriginalFilename());
        break;
      }
    }
  }

  private void uploadBIPExcel(String prefix, String bipRequestCode, MultipartFile multipartFile, String fileName)
      throws IOException {
    String uploadPath =
        new StringBuilder().append(prefix)
            .append(Constants.SLASH).append(bipRequestCode).append(Constants.SLASH).toString();
    RequestHelper.createDirIfNotExists(uploadPath);
    multipartFile.transferTo(new File(uploadPath + fileName));
  }

  private void uploadBIPExcel2(String bipRequestCode, MultipartFile multipartFile, String bulkInternalProcessType) throws Exception {
    fileStorageService.uploadFilePath(multipartFile, bipRequestCode, bulkInternalProcessType);
  }

  @Override
  public void uploadBulkFile(MultipartFile file, String processType, String requestId,
      String storeId, String username) throws Exception {
    RequestHelper.checkParameter(
        StringUtils.isNotEmpty(BulkInternalProcessType.getValueOrEmpty(processType)),
        ErrorMessages.INVALID_PROCESS_TYPE);
    String baseDirPath = fileStorageService.uploadFilePath(file, requestId, processType);
    BulkReviewUploadModel bulkReviewUploadModel =
        RequestHelper.toBulkReviewUploadModel(storeId, baseDirPath + file.getOriginalFilename(),
            processType, requestId, username, null);
    log.info("Publishing event {} for bulkProcessCode = {} bulkReviewUploadModel = {} ",
        kafkaTopicProperties.getBulkReviewUploadEvent(), bulkReviewUploadModel.getBulkProcessCode(),
        bulkReviewUploadModel);
    kafkaProducer.send(kafkaTopicProperties.getBulkReviewUploadEvent(),
        bulkReviewUploadModel.getBulkProcessCode(), bulkReviewUploadModel);
  }
}

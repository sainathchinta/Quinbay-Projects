package com.gdn.partners.pcu.external.service.impl;


import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.mta.bulk.AllowedQRGenerationType;
import com.gdn.mta.bulk.dto.product.DownloadQRCodeRequest;
import com.gdn.partners.pcu.external.client.feign.XBulkFeign;
import com.gdn.partners.pcu.external.model.ErrorMessages;
import com.gdn.partners.pcu.external.service.QRV2Service;
import com.gdn.partners.pcu.external.service.impl.config.KafkaPublisher;
import com.gdn.partners.pcu.external.service.impl.config.KafkaTopicProperties;
import com.gdn.partners.pcu.external.service.impl.helper.BeanUtils;
import com.gdn.partners.pcu.external.service.impl.helper.ExcelTemplateUtil;
import com.gdn.partners.pcu.external.service.impl.helper.RequestHelper;
import com.gdn.partners.pcu.external.service.impl.helper.ResponseHelper;
import com.gdn.partners.pcu.external.web.model.request.ManualQRCodeRequest;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

@Slf4j
@Service
public class QRV2ServiceImpl implements QRV2Service {

  @Autowired
  private XBulkFeign xBulkFeign;

  @Autowired
  private KafkaPublisher kafkaProducer;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Override
  public boolean qrGenerationAccessible(String merchantCode) {
    return xBulkFeign.checkQrGenerationAccessible(merchantCode).isSuccess();
  }

  @Override
  public void downloadQRCodes(String storeId, String requestId, String businessPartnerCode,
      String merchantName, ManualQRCodeRequest request) throws Exception {
    if (!xBulkFeign.checkQrGenerationAccessible(businessPartnerCode).isSuccess()) {
      throw new ApplicationRuntimeException(ErrorCategory.AUTHORIZATION,
          ErrorMessages.UNAUTHORIZED_ERR_MESSAGE);
    }
    RequestHelper.validateManualQRCodeRequest(request);
    if (AllowedQRGenerationType.STORE.equals(request.getQrGenerationType())
        || AllowedQRGenerationType.ALL_PRODUCTS.equals(request.getQrGenerationType())) {
      this.kafkaProducer.send(kafkaTopicProperties.getGenerateQrCodeStore(),
          RequestHelper.populateDownloadQrCodeRequest(storeId, requestId, businessPartnerCode,
              merchantName, request));
    }
    else{
      this.kafkaProducer.send(kafkaTopicProperties.getGenerateQrCodeProduct(),
          RequestHelper.populateDownloadQrCodeRequest(storeId, requestId, businessPartnerCode,
              merchantName, request));
    }

  }

  @Override
  public void uploadExcelAndPublishRequest(String storeId, String requestId,
    String businessPartnerCode, String businessPartnerName, ManualQRCodeRequest manualQRCodeRequest,
    MultipartFile multipartFile) throws Exception {
    if (!ExcelTemplateUtil.isFileExcelType(multipartFile)) {
      throw new ApplicationRuntimeException(ErrorCategory.INVALID_FORMAT,
          ErrorMessages.EXCEL_FILE_TYPE_INVALID_VAT_MESSAGE);
    }
    if (!xBulkFeign.checkQrGenerationAccessible(businessPartnerCode).isSuccess()) {
      throw new ApplicationRuntimeException(ErrorCategory.AUTHORIZATION,
        ErrorMessages.UNAUTHORIZED_ERR_MESSAGE);
    }
    RequestHelper.validateExcelQRCodeRequest(manualQRCodeRequest);
    DownloadQRCodeRequest downloadQRCodeRequest = new DownloadQRCodeRequest();
    BeanUtils.copyProperties(manualQRCodeRequest, downloadQRCodeRequest);
    GdnBaseRestResponse response = this.xBulkFeign.uploadQrCodeExcel(storeId, requestId,
        RequestHelper.populateQrExcelUploadRequest(storeId, requestId, businessPartnerCode,
            businessPartnerName, manualQRCodeRequest, multipartFile));
    ResponseHelper.validateResponse(response);
  }
}

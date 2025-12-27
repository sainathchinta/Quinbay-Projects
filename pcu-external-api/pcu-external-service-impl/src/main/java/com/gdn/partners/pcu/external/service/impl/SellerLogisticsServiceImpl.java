package com.gdn.partners.pcu.external.service.impl;

import java.io.IOException;
import java.util.List;

import org.apache.commons.codec.binary.Base64;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.partners.pcu.external.client.feign.SellerLogisticsFeign;
import com.gdn.partners.pcu.external.client.helper.Response;
import com.gdn.partners.pcu.external.model.ErrorMessages;
import com.gdn.partners.pcu.external.service.SellerLogisticsService;
import com.gdn.partners.pcu.external.service.impl.helper.ResponseHelper;
import com.gdn.partners.pcu.external.web.model.response.ExcelSkuUpdateStatusResponse;
import com.gdn.partners.pcu.external.web.model.response.LogisticsDownloadTemplateResponse;
import com.gdn.partners.pcu.external.web.model.response.LogisticsExcelSkuUploadResponse;
import com.gdn.partners.pcu.external.web.model.response.SellerLogisticsProductResponse;
import com.gdn.seller.logistics.web.model.request.UploadExcelRequest;
import com.gdn.seller.logistics.web.model.response.DownloadSkuTemplateResponse;
import com.gdn.seller.logistics.web.model.response.GetSellerLogisticProductResponse;
import com.gdn.seller.logistics.web.model.response.UploadExcelSkuUpdateResponse;
import com.gdn.seller.logistics.web.model.response.UploadExcelSkuUpdateStatusResponse;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class SellerLogisticsServiceImpl implements SellerLogisticsService {

  @Autowired
  private SellerLogisticsFeign sellerLogisticsFeign;

  public List<SellerLogisticsProductResponse> getSellerLogisticsProduct(String merchantCode,
      String merchantDeliveryType) {
    log.info("Getting seller logistics product.MerchantCode: {} MerchantDeliveryType: {}", merchantCode,
        merchantDeliveryType);
    Response<List<GetSellerLogisticProductResponse>> getSkuLogisticsProductResponseList =
        sellerLogisticsFeign.getSellerLogisticProducts(merchantCode, merchantDeliveryType);
    ResponseHelper.validateResponse(getSkuLogisticsProductResponseList);
    return ResponseHelper.toListSellerLogisticsProductResponse(getSkuLogisticsProductResponseList.getData());
  }

  public LogisticsDownloadTemplateResponse getTemplateData(String merchantCode,
      String merchantDeliveryType) {
    log.info(
        "Downloading template from seller logistics service. MerchantCode: {} MerchantDeliveryType: {}",
        merchantCode, merchantDeliveryType);
    Response<DownloadSkuTemplateResponse> downloadSkuTemplateResponseResponse =
        sellerLogisticsFeign.getTemplateData(merchantCode, merchantDeliveryType);
    ResponseHelper.validateResponse(downloadSkuTemplateResponseResponse);
    return ResponseHelper
        .toLogisticDownloadTemplateResponse(downloadSkuTemplateResponseResponse.getData());
  }

  public LogisticsExcelSkuUploadResponse excelUpload(MultipartFile request, String merchantCode,
      String merchantDeliveryType) {
    UploadExcelRequest uploadExcelRequest = new UploadExcelRequest();
    try {
      uploadExcelRequest.setEncodedContent(Base64.encodeBase64String(request.getBytes()));
      uploadExcelRequest.setFileName(request.getOriginalFilename());
      log.info(
          "Uploading the file at Seller logistics service. MerchantCode: {} MerchantDeliveryType: {}",
          merchantCode, merchantDeliveryType);
      Response<UploadExcelSkuUpdateResponse> response =
          sellerLogisticsFeign.excelUpload(uploadExcelRequest, merchantCode, merchantDeliveryType);
      ResponseHelper.validateResponse(response);
      return ResponseHelper.toLogisticsExcelSkuUploadResponse(response.getData());
    } catch (IOException e) {
      log.error(
          "Error while uploading sku logistic excel file. MerchantCode: {} MerchantDeliveryType: {} Exception: {}",
          merchantCode, merchantDeliveryType, e.getMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED,
          ErrorMessages.ERROR_UPLOADING_FILE);
    }
  }

  @Override
  public ExcelSkuUpdateStatusResponse excelUploadStatus(String merchantCode) {
    log.info("UploadExcel status for MerchantCode: {}", merchantCode);
    Response<UploadExcelSkuUpdateStatusResponse> response =
        sellerLogisticsFeign.excelUploadStatus(merchantCode);
    ResponseHelper.validateResponse(response);
    return ResponseHelper.toLogisticsExcelSkuUploadStatusResponse(response.getData());
  }
}

package com.gdn.partners.pcu.external.service;

import java.util.List;

import org.springframework.web.multipart.MultipartFile;

import com.gdn.partners.pcu.external.web.model.response.ExcelSkuUpdateStatusResponse;
import com.gdn.partners.pcu.external.web.model.response.LogisticsDownloadTemplateResponse;
import com.gdn.partners.pcu.external.web.model.response.LogisticsExcelSkuUploadResponse;
import com.gdn.partners.pcu.external.web.model.response.SellerLogisticsProductResponse;

public interface SellerLogisticsService {

  /**
   * @param merchantCode
   * @param merchantDeliveryType
   * @return
   */
  List<SellerLogisticsProductResponse> getSellerLogisticsProduct(String merchantCode,
      String merchantDeliveryType);

  /**
   * @param merchantCode
   * @param merchantDeliveryType
   * @return
   */
  LogisticsDownloadTemplateResponse getTemplateData(String merchantCode,
      String merchantDeliveryType);

  /**
   * @param request
   * @param merchantCode
   * @param merchantDeliveryType
   * @return
   */
  LogisticsExcelSkuUploadResponse excelUpload(MultipartFile request, String merchantCode,
      String merchantDeliveryType);

  /**
   * @param merchantCode
   * @return
   */
  ExcelSkuUpdateStatusResponse excelUploadStatus(String merchantCode);
}

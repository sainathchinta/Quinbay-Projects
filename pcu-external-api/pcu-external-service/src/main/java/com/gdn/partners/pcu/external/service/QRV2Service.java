package com.gdn.partners.pcu.external.service;

import com.gdn.partners.pcu.external.web.model.request.ManualQRCodeRequest;
import org.springframework.web.multipart.MultipartFile;

public interface QRV2Service {

  /**
   * check is user can upload more QR request
   * @return boolean
   */
  boolean qrGenerationAccessible(String merchantCode);

  /**
   * send kafka to x-bulk to download QR
   *
   * @param storeId             must not be null
   * @param requestId           must not be null
   * @param businessPartnerCode must not be null
   * @param merchantName        must not be null
   * @param request             must not be null
   * @throws Exception
   */
  void downloadQRCodes(String storeId, String requestId, String businessPartnerCode,String merchantName,
      ManualQRCodeRequest request) throws Exception;

  /**
   *
   *  @param storeId must not be null
   * @param requestId must not be null
   * @param businessPartnerCode must not be null
   * @param businessPartnerName
   * @param manualQRCodeRequest must not be null
   * @param multipartFile must not be null
   */
  void uploadExcelAndPublishRequest(String storeId, String requestId, String businessPartnerCode,
    String businessPartnerName, ManualQRCodeRequest manualQRCodeRequest, MultipartFile multipartFile)
    throws Exception;
}

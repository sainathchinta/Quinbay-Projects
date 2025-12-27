package com.gdn.partners.pcu.external.web.controller;

import com.gdn.partners.core.web.dto.BaseResponse;
import com.gdn.partners.core.web.dto.SingleBaseResponse;
import com.gdn.partners.pcu.external.client.helper.MandatoryParameterHelper;
import com.gdn.partners.pcu.external.model.QRV2ApiPath;
import com.gdn.partners.pcu.external.service.QRV2Service;
import com.gdn.partners.pcu.external.web.model.request.ManualQRCodeRequest;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RequestPart;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

@Slf4j
@Tag(name = "QR Generate V2 API")
@RestController
@RequestMapping(value = QRV2ApiPath.BASE_PATH)
@Validated
public class QRV2Controller {

  @Autowired
  private MandatoryParameterHelper mandatoryParameterHelper;
  @Autowired
  private QRV2Service qrv2Service;

  @Operation(summary = "Check QR generation request allowed")
  @GetMapping(value = QRV2ApiPath.ACCESSIBLE, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<Boolean> checkQrGenerationAccessible() {
    String requestId = mandatoryParameterHelper.getRequestId();
    String merchantCode = mandatoryParameterHelper.getBusinessPartnerCode();
    log.info("Method : Check QR generation request allowed for merchant code: {}", merchantCode);
    return new SingleBaseResponse<>(null, null, true, requestId,
        qrv2Service.qrGenerationAccessible(merchantCode));
  }

  @Operation(summary = "API to download QR codes")
  @PostMapping(value = QRV2ApiPath.DOWNLOAD_QR_CODES, produces = MediaType.APPLICATION_JSON_VALUE
      , consumes = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse downloadQRCode(@RequestBody ManualQRCodeRequest request) throws Exception {
    log.info("invoking download QR codes  at controller for username :{} , with request : {}",
        mandatoryParameterHelper.getUsername(), request);
    String requestId = mandatoryParameterHelper.getRequestId();
    String storeId = mandatoryParameterHelper.getStoreId();
    this.qrv2Service.downloadQRCodes(storeId, requestId,
        mandatoryParameterHelper.getBusinessPartnerCode(),mandatoryParameterHelper.getBusinessPartnerName(), request);
    return new BaseResponse(null, null, true, requestId);
  }

  @Operation(summary = "API to upload excel for QR generation")
  @PostMapping(value = QRV2ApiPath.UPLOAD_FOR_QR, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse uploadExcelToGenerateQr(@RequestPart ManualQRCodeRequest manualQRCodeRequest,
    @RequestParam MultipartFile multipartFile) throws Exception {
    log.info("invoking download QR codes  at controller for username :{} , with request : {}",
      mandatoryParameterHelper.getUsername(), manualQRCodeRequest);
    String requestId = mandatoryParameterHelper.getRequestId();
    String storeId = mandatoryParameterHelper.getStoreId();
    this.qrv2Service.uploadExcelAndPublishRequest(storeId, requestId,
      mandatoryParameterHelper.getBusinessPartnerCode(),
      mandatoryParameterHelper.getBusinessPartnerName(), manualQRCodeRequest, multipartFile);
    return new BaseResponse(null, null, true, requestId);
  }
}

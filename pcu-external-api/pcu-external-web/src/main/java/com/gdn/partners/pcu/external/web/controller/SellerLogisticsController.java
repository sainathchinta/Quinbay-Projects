package com.gdn.partners.pcu.external.web.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RequestPart;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import com.gdn.partners.core.web.dto.ListBaseResponse;
import com.gdn.partners.core.web.dto.SingleBaseResponse;
import com.gdn.partners.pcu.external.client.helper.MandatoryParameterHelper;
import com.gdn.partners.pcu.external.model.Constants;
import com.gdn.partners.pcu.external.model.SellerLogisticsApiPath;
import com.gdn.partners.pcu.external.service.SellerLogisticsService;
import com.gdn.partners.pcu.external.web.model.response.ExcelSkuUpdateStatusResponse;
import com.gdn.partners.pcu.external.web.model.response.LogisticsDownloadTemplateResponse;
import com.gdn.partners.pcu.external.web.model.response.LogisticsExcelSkuUploadResponse;
import com.gdn.partners.pcu.external.web.model.response.SellerLogisticsProductResponse;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Tag(name ="Seller Logistics API")
@RestController
@RequestMapping(value = SellerLogisticsApiPath.BASE_PATH)
public class SellerLogisticsController {

  @Autowired
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Autowired
  private SellerLogisticsService sellerLogisticsService;

  @Operation(summary ="Get Seller Logistics Product")
  @GetMapping(value = SellerLogisticsApiPath.GET_SELER_LOGISTICS_PRODUCT,
      produces = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<SellerLogisticsProductResponse> getSellerLogisticsProduct(
      @RequestParam(required = true) String merchantDeliveryType) {
    String requestId = mandatoryParameterHelper.getRequestId();
    String merchantCode = mandatoryParameterHelper.getBusinessPartnerCode();
    List<SellerLogisticsProductResponse> sellerLogisticsProductResponse =
        sellerLogisticsService.getSellerLogisticsProduct(merchantCode, merchantDeliveryType);
    return new ListBaseResponse<>(null, null, true, requestId, sellerLogisticsProductResponse,
        null);
  }

  @Operation(summary ="Get Template Data")
  @GetMapping(value = SellerLogisticsApiPath.GET_TEMPLATE_DATA,
      produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<LogisticsDownloadTemplateResponse> getTemplateData(
      @RequestParam(required = true) String merchantDeliveryType) {
    String requestId = mandatoryParameterHelper.getRequestId();
    String merchantCode = mandatoryParameterHelper.getBusinessPartnerCode();
    LogisticsDownloadTemplateResponse logisticsDownloadTemplateResponse =
        sellerLogisticsService.getTemplateData(merchantCode, merchantDeliveryType);
    return new SingleBaseResponse<>(null, null, true, requestId, logisticsDownloadTemplateResponse);
  }


  @Operation(summary ="Excel Upload")
  @PostMapping(value = SellerLogisticsApiPath.UPLOAD_LOGISTICS_TEMPLATE_UPDATE,
      produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<LogisticsExcelSkuUploadResponse> excelUpload(
      @RequestParam(required = true) String merchantDeliveryType,
      @RequestPart MultipartFile request) throws Exception {
    String requestId = mandatoryParameterHelper.getRequestId();
    String merchantCode = mandatoryParameterHelper.getBusinessPartnerCode();
    log.info("Uploading the file at Seller logistics controller. Request : {}", request);
    LogisticsExcelSkuUploadResponse logisticsExcelSkuUploadResponse =
        sellerLogisticsService.excelUpload(request, merchantCode, merchantDeliveryType);
    return new SingleBaseResponse<>(null, null, true, requestId, logisticsExcelSkuUploadResponse);
  }

  @Operation(summary ="Excel Upload Status")
  @GetMapping(value = SellerLogisticsApiPath.GET_UPLOAD_STATUS,
      produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<ExcelSkuUpdateStatusResponse> excelUploadStatus() throws Exception {
    String requestId = mandatoryParameterHelper.getRequestId();
    String merchantCode = mandatoryParameterHelper.getBusinessPartnerCode();
    log.info("Checking upload status for merchant : {}", merchantCode);
    ExcelSkuUpdateStatusResponse response = sellerLogisticsService.excelUploadStatus(merchantCode);
    return new SingleBaseResponse<>(null, null, true, requestId, response);
  }
}

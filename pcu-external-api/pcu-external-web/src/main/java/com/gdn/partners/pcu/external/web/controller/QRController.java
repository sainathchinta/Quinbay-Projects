package com.gdn.partners.pcu.external.web.controller;


import java.io.IOException;
import java.util.Collections;

import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.mta.product.util.GdnRestSimpleResponse;
import com.gdn.partners.pcu.external.client.helper.MandatoryParameterHelper;
import com.gdn.partners.pcu.external.model.Constants;
import com.gdn.partners.pcu.external.model.ErrorMessages;
import com.gdn.partners.pcu.external.model.QRApiPath;
import com.gdn.partners.pcu.external.service.QRService;
import com.gdn.partners.pcu.external.web.model.request.QRDownloadWebRequest;
import com.gdn.partners.pcu.external.web.model.request.QRGenerateRequest;
import com.google.zxing.WriterException;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Tag(name ="QR Generate API")
@RestController
@RequestMapping(value = QRApiPath.BASE_PATH)
@Validated
@Deprecated
public class QRController {

  @Autowired
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Autowired
  private QRService qrService;

  @Operation(summary ="QR Creation API")
  @PutMapping(value = QRApiPath.GENERATE, produces = MediaType.APPLICATION_JSON_VALUE)
  public GdnRestSimpleResponse<String> generateQR(@RequestBody QRGenerateRequest request)
      throws IOException, WriterException {
    log.debug("Generating the QR Image/Template for details : {}", request);
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(request.getMerchantCode()), ErrorMessages.ERR_MER_CODE_NULL);
    String path = qrService.getPathOfImage(request);
    return new GdnRestSimpleResponse<>(null, null, true, mandatoryParameterHelper.getRequestId(),
        path);
  }

  @Operation(summary ="Merchant Template Download API")
  @PutMapping(value = QRApiPath.MERCHANT_TEMPLATE_DOWNLOAD, produces = MediaType.APPLICATION_JSON_VALUE)
  public GdnRestSimpleResponse<String> merchantTemplateDownload(@RequestBody QRGenerateRequest request)
      throws IOException, WriterException {
    log.debug("Generating the merchant template in PDF format wit request : {}", request);
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(request.getMerchantCode()), ErrorMessages.ERR_MER_CODE_NULL);
    String path = qrService.merchantTemplateDownload(request);
    return new GdnRestSimpleResponse<>(null, null, true, mandatoryParameterHelper.getRequestId(),
        path);
  }

  @RequestMapping(value = QRApiPath.DELETE, method = RequestMethod.DELETE, produces = {MediaType.APPLICATION_JSON_VALUE,
      MediaType.APPLICATION_XML_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary ="QR folders clean up API")
  @ResponseBody
  public GdnRestSimpleResponse<Boolean> deleteQRCodes (@RequestParam String days) throws Exception {
    int dayValue = Integer.parseInt(days);
    GdnPreconditions.checkArgument(dayValue > 0, ErrorMessages.INVALID_DAYS_ERROR_MESSAGE);
    log.info("Delete the QR codes generated before {} days ", days);
    qrService.deleteQRCodes(dayValue);
    return new GdnRestSimpleResponse<>(null, null, true, mandatoryParameterHelper.getRequestId(),
        true);
  }

  @Operation(summary ="Products QR Download API")
  @PutMapping(value = QRApiPath.DOWNLOAD, produces = MediaType.APPLICATION_JSON_VALUE)
  public GdnRestSimpleResponse<Boolean> productsQRDownload(@RequestBody QRDownloadWebRequest request) throws Exception {
    GdnPreconditions.checkArgument("pdf".equals(request.getType()) || "png".equals(request.getType()), ErrorMessages.ERR_INVALID_TYPE);
    GdnPreconditions.checkArgument(!request.getProducts().isEmpty(), ErrorMessages.ERR_EMPTY_PRODUCT_LIST);
    log.info("Generating the QR codes for the products : {}", request);
    qrService.downloadQRCodesForProducts(request);
    return new GdnRestSimpleResponse<>(null, null, true, mandatoryParameterHelper.getRequestId(),
        true);
  }
}

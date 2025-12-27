package com.gdn.partners.pcu.external.web.controller;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;

import com.gdn.partners.pcu.external.client.helper.MandatoryParameterHelper;
import com.gdn.partners.pcu.external.model.ErrorMessages;
import com.gdn.partners.pcu.external.model.ProductAssemblyApiPath;
import com.gdn.partners.pcu.external.service.ProductAssemblyService;
import com.gdn.partners.pcu.external.web.model.request.AssemblyDisAssemblyListingRequest;
import com.gdn.partners.pcu.external.web.model.request.SimpleListAssemblyDisassemblyRequest;
import com.gdn.partners.pcu.external.web.model.response.HistoryWebResponse;
import com.gdn.partners.pcu.external.web.model.response.MasterWarehouseListWebResponse;
import com.gdn.partners.pcu.external.web.model.response.RequestFormResponse;
import com.gdn.partners.pcu.external.web.model.response.RequestFormWebResponse;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Tag(name = "Product Assembly API")
@RestController
@RequestMapping(value = ProductAssemblyApiPath.BASE_PATH)
@Validated
public class ProductAssemblyController {

  @Autowired
  private ProductAssemblyService productAssemblyService;

  @Autowired
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Operation(summary = "Product assembly Detail API")
  @GetMapping(value = ProductAssemblyApiPath.GET_WAREHOUSE_CODE_AND_NAME_LIST, produces = MediaType.APPLICATION_JSON_VALUE)
  public GdnRestListResponse<MasterWarehouseListWebResponse> getWarehouseCodeAndFulfillmentCenter() {
    log.info("Fetching warehouse code and name list");
    String requestId = mandatoryParameterHelper.getRequestId();
    GdnRestListResponse<MasterWarehouseListWebResponse> response =
        productAssemblyService.getMasterWarehouseListResponse();
    return new GdnRestListResponse<>(response.getContent(), response.getPageMetaData(), requestId);
  }

  @Operation(summary = "API to fetch Assembly/DisAssembly/Transfer Request forms for listing", description = "API to fetch Assembly/DisAssembly/Transfer Request forms for listing")
  @RequestMapping(value = ProductAssemblyApiPath.GET_REQUEST_FORMS_LISTING, method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<RequestFormWebResponse> getAssemblyDisAssemblyAndTransferRequestListingResponse(@RequestParam(defaultValue = "0") int page,
      @RequestParam(defaultValue = "50") int size,
      @RequestBody AssemblyDisAssemblyListingRequest assemblyDisAssemblyListingRequest) {
    String requestId = mandatoryParameterHelper.getRequestId();
    log.info("Fetching Assembly/DisAssembly/Transfer Request forms with request {} and requestId {} ",
        assemblyDisAssemblyListingRequest, requestId);
    try {
      GdnPreconditions.checkArgument(StringUtils.isNotBlank(assemblyDisAssemblyListingRequest.getMerchantCode()),
          ErrorMessages.ERR_MER_CODE_NULL);
      GdnRestListResponse<RequestFormWebResponse> response = productAssemblyService
          .getRequestFormsListingResponse(page, size, requestId, assemblyDisAssemblyListingRequest);
      return new GdnRestListResponse<>(response.getContent(), response.getPageMetaData(), requestId);
    } catch (Exception e) {
      log.error("Error while getting listing response, requestId {} and error -{} ", requestId, e);
      return new GdnRestListResponse<>(e.getMessage(), null, false, requestId);
    }
  }

  @Operation(summary = "API to get history of a request form", description = "API to get history of a request form")
  @RequestMapping(value = ProductAssemblyApiPath.GET_HISTORY, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<HistoryWebResponse> getRequestFormHistory(@RequestParam(defaultValue = "0") int page,
      @RequestParam(defaultValue = "10") int size,
      @RequestParam(required = false, value = "sortOrder", defaultValue = "DESC") String sortOrder,
      @RequestParam("requestFormNumber") String requestFormNumber) {
    GdnRestListResponse<HistoryWebResponse> response = productAssemblyService
        .getRequestHistory(requestFormNumber, page, size, sortOrder, mandatoryParameterHelper.getRequestId(), mandatoryParameterHelper.getBusinessPartnerCode());
    return new GdnRestListResponse<>(response.getContent(), response.getPageMetaData(),
        mandatoryParameterHelper.getRequestId());
  }

  @Operation(summary = "API to create Assembly/DisAssembly/Transfer Requests", description = "API to create Assembly/DisAssembly/Transfer Requests")
  @RequestMapping(value = ProductAssemblyApiPath.CREATE_REQUEST, method = RequestMethod.POST,
      consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnBaseRestResponse createAssemblyDisAssemblyAndTransferRequests(@PathVariable("type") String type,
      @RequestBody SimpleListAssemblyDisassemblyRequest simpleListAssemblyDisassemblyRequest) {
    String requestId = mandatoryParameterHelper.getRequestId();
    log.info("Create Assembly/DisAssembly/Transfer Requests with request {} and requestId {} ",
        simpleListAssemblyDisassemblyRequest, requestId);
    String businessPartnerCode = mandatoryParameterHelper.getBusinessPartnerCode();
    productAssemblyService.createAssemblyDisAssemblyAndTransferRequests(businessPartnerCode, type,
        simpleListAssemblyDisassemblyRequest);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }


  @Operation(summary = "API to Cancel/Retry Request Form", description = "API to Cancel/Retry Request Form")
  @RequestMapping(value = ProductAssemblyApiPath.RETRY_OR_CANCEL, method = RequestMethod.POST)
  GdnBaseRestResponse cancelOrRetryRequest(@RequestParam("requestFormNumber") String requestFormNumber,
      @RequestParam("type") String type) {
    String requestId = mandatoryParameterHelper.getRequestId();
    log.info("{} request for request form number {}  and requestId {} ", type, requestFormNumber, requestId);
    return productAssemblyService.cancelOrRetry(requestFormNumber, type, mandatoryParameterHelper.getBusinessPartnerCode(), requestId);
  }
}

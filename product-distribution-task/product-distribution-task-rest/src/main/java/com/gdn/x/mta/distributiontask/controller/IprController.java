package com.gdn.x.mta.distributiontask.controller;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.x.mta.distributiontask.model.Constants;
import com.gdn.x.mta.distributiontask.model.ErrorCategory;
import com.gdn.x.mta.distributiontask.model.IprApiPath;
import com.gdn.x.mta.distributiontask.model.dto.BrandReport;
import com.gdn.x.mta.distributiontask.model.dto.IPRUpdateAssigneeRequest;
import com.gdn.x.mta.distributiontask.model.dto.IPRProductListRequest;
import com.gdn.x.mta.distributiontask.rest.model.response.IPRHistoryResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.IprProductDetailsResponse;
import com.gdn.x.mta.distributiontask.model.dto.IprActionRequest;
import com.gdn.x.mta.distributiontask.model.dto.SubmitEvidenceRequest;
import com.gdn.x.mta.distributiontask.rest.model.response.IprProductListResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.IprSuspensionInProgressResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.MapResponse;
import com.gdn.x.mta.distributiontask.service.api.IprService;
import com.gdn.x.mta.distributiontask.service.api.IprWrapperService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.util.Map;

@Slf4j
@RestController
@RequestMapping(value = IprApiPath.BASE_PATH)
@Tag(name = "IprController", description = "IPR Portal API")
@RequiredArgsConstructor
public class IprController {

  private final IprService iprService;

  private final IprWrapperService iprWrapperService;

  @GetMapping(value = {IprApiPath.SUSPENSION_IN_PROGRESS}, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Fetch suspension in progress products")
  public GdnRestListResponse<IprSuspensionInProgressResponse> findSuspensionInProgressProduct(
    @RequestParam String storeId, @RequestParam String requestId, @RequestParam String channelId,
    @RequestParam String clientId, @RequestParam(required = false) String username,
    @RequestParam String businessPartnerCode, @RequestParam(defaultValue = "0") Integer page,
    @RequestParam(defaultValue = "20") Integer size,
    @RequestParam(defaultValue = "ASC") String sortOrder) {
    try {
      MandatoryRequestParam mandatoryRequestParam =
          MandatoryRequestParam.generateMandatoryRequestParam(storeId, channelId, clientId,
              requestId, username, null);
      log.info("Api to fetch suspension in progress products mandatoryRequestParam: {}, "
          + "businessPartnerCode: {}", mandatoryRequestParam, businessPartnerCode);
      Page<IprSuspensionInProgressResponse> iprSuspensionInProgressResponse =
          iprService.findSuspensionInProgressProducts(storeId, businessPartnerCode, page, size,
            sortOrder);
      return new GdnRestListResponse<>(iprSuspensionInProgressResponse.getContent(),
          new PageMetaData(size, page, iprSuspensionInProgressResponse.getTotalElements()),
          requestId);
    } catch (Exception e) {
      log.error("Exception caught while finding suspension in progress products ", e);
      return new GdnRestListResponse<>("Failed to load suspension list products", null, false, null,
          null, requestId);
    }
  }

  @PostMapping(value = {IprApiPath.FILTER}, produces = {
    MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Fetch list of ipr products")
  public GdnRestListResponse<IprProductListResponse> getIprProductsList(
    @RequestParam String storeId, @RequestParam String requestId, @RequestParam String channelId,
    @RequestParam String clientId, @RequestParam(required = false) String username,
    @RequestBody IPRProductListRequest request, @RequestParam(defaultValue = "0") int page,
    @RequestParam(defaultValue = "25") int size) {
    try {
      log.info("Api to fetch ipr products list by filter request = {} ", request);
      Page<IprProductListResponse> iprProductListResponsePage =
        iprService.getIprProductListResponse(storeId, request, PageRequest.of(page, size));
      return new GdnRestListResponse<>(iprProductListResponsePage.getContent(),
        new PageMetaData(size, page, iprProductListResponsePage.getTotalElements()), requestId);
    } catch (Exception e) {
      log.error("Exception caught while fetching products for ipr portal error -  ", e);
      return new GdnRestListResponse<>(e.getMessage(), null, false, null, null, requestId);
    }
  }

  @PostMapping(value = IprApiPath.SUBMIT_EVIDENCE, produces = {
    MediaType.APPLICATION_JSON_VALUE}, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Submit evidence for IPR")
  public GdnBaseRestResponse submitEvidence(@RequestParam String requestId,
      @RequestBody SubmitEvidenceRequest request) throws JsonProcessingException {
    log.info("Api to submit evidence for a IPR product = {} ", request);
    iprWrapperService.submitEvidenceForProduct(request);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @PostMapping(value = {IprApiPath.ADD_PRODUCT_IPR}, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Create ipr products")
  public GdnBaseRestResponse createIprProduct(@RequestParam String requestId,
      @RequestParam String storeId, @PathVariable String productSku,
      @RequestParam(required = false) String source,
      @RequestParam(required = false) String assignee,
      @RequestBody(required = false) BrandReport brandReport) throws Exception {
    log.info("Create ipr product for bulk request for product sku: {} ", productSku);
    String errorMessage =
        iprWrapperService.addProductToIPR(productSku, storeId, source, assignee, brandReport);
    return new GdnBaseRestResponse(errorMessage, null, true, requestId);
  }

  @GetMapping(value = {IprApiPath.DETAILS}, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Get details of ipr products")
  public GdnRestSingleResponse<IprProductDetailsResponse> getProductDetailsForAllProductTypes(
      @RequestParam String requestId, @PathVariable("productSku") String productSku) {
    log.info("Api to fetch IPR Product details with productSku: {}", productSku);
    return new GdnRestSingleResponse<>(null, null, true,
        iprService.fetchIprProductDetails(productSku), requestId);
  }

  @PostMapping(value = {IprApiPath.UPDATE_ASSIGNEE}, produces = {
      MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  public GdnBaseRestResponse updateAssignee(@RequestParam String storeId,
      @RequestParam String requestId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam(required = false) String username,
      @RequestBody IPRUpdateAssigneeRequest iprAssigneeUpdateRequest) throws Exception {
    log.info("Update assignee for ipr product with request:{}", iprAssigneeUpdateRequest);
    iprWrapperService.updateAssignee(iprAssigneeUpdateRequest);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @PostMapping(value = IprApiPath.PERFORM_IPR_ACTION, produces = {
      MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Perform IPR actions")
  public GdnBaseRestResponse performIprAction(@RequestParam String requestId,
      @RequestParam String storeId, @RequestBody IprActionRequest request) throws Exception {
    log.info("Performing action on ipr products for request: {}", request);
    ErrorCategory iprActionResult = iprWrapperService.performIprActionForProduct(request, storeId);
    return new GdnBaseRestResponse(iprActionResult.getMessage(), iprActionResult.getCode(), true,
        requestId);
  }

  @GetMapping(value = IprApiPath.PRIMARY_FILTER_COUNTS, produces = {
    MediaType.APPLICATION_JSON_VALUE})
  public GdnRestSingleResponse<MapResponse> getPrimaryFilterCounts(@RequestParam String storeId,
    @RequestParam String requestId) throws Exception {
    log.info("Fetching primary filter counts for ipr portal");
    Map<String, Object> response = iprService.getPrimaryFilterCounts(storeId);
    return new GdnRestSingleResponse<>(new MapResponse(response), requestId);
  }

  @PostMapping(value = IprApiPath.SUSPEND_EVIDENCE_REQUESTED_PRODUCT, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Suspend Evidence Requested product", description = "Suspend Evidence Requested product")
  public GdnBaseRestResponse suspendEvidenceRequestedProduct(@RequestParam String requestId,
    @RequestParam String storeId, @RequestParam int daysThreshold,
    @RequestParam(defaultValue = "0") Integer page,
    @RequestParam(defaultValue = "20") Integer size) {
    Pageable pageable =
      PageRequest.of(page, size, Sort.by(Sort.Direction.DESC, Constants.UPDATED_DATE));
    iprService.fetchAndSuspendEvidenceRequestedProduct(storeId, daysThreshold, pageable);
    return new GdnBaseRestResponse(StringUtils.EMPTY, StringUtils.EMPTY, true, requestId);
  }

  @GetMapping(value = IprApiPath.HISTORY, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Get history of product in IPR portal", description = "Fetch history of "
      + "product in IPR portal")
  public GdnRestListResponse<IPRHistoryResponse> fetchIprHistoryByProductSku(
      @RequestParam String requestId, @RequestParam String storeId,@RequestParam String productSku,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "20") Integer size) {
    Pageable pageable = PageRequest.of(page, size);
    log.info("Fetch history for IPR product with productSku:{}", productSku);
    Page<IPRHistoryResponse> response =
        iprService.fetchIprHistoryByProductSku(storeId, productSku, pageable);
    return new GdnRestListResponse<>(response.getContent(),
        new PageMetaData(size, page, response.getTotalElements()), requestId);
  }
}

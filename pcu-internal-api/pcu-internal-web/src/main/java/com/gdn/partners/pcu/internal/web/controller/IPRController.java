package com.gdn.partners.pcu.internal.web.controller;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.partners.core.web.dto.BaseResponse;
import com.gdn.partners.core.web.dto.ListBaseResponse;
import com.gdn.partners.core.web.dto.Metadata;
import com.gdn.partners.core.web.dto.SingleBaseResponse;
import com.gdn.partners.pcu.internal.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.internal.client.model.request.IPRProductListRequest;
import com.gdn.partners.pcu.internal.client.model.request.IprActionRequest;
import com.gdn.partners.pcu.internal.client.model.response.IPRProductDetailResponse;
import com.gdn.partners.pcu.internal.client.model.request.IPRUpdateAssigneeRequest;
import com.gdn.partners.pcu.internal.client.model.response.IPRProductHistoryResponse;
import com.gdn.partners.pcu.internal.client.model.response.IPRProductListResponse;
import com.gdn.partners.pcu.internal.client.model.response.IprSuspensionInProgressResponse;
import com.gdn.partners.pcu.internal.client.model.response.IprSuspensionInProgressWebResponse;
import com.gdn.partners.pcu.internal.model.IPRApiPath;
import com.gdn.partners.pcu.internal.service.IPRService;
import com.gdn.partners.pcu.internal.web.model.request.IPRProductsDownloadWebRequest;
import com.gdn.x.mta.distributiontask.rest.model.response.MapResponse;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.http.MediaType;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

@Slf4j
@Tag(name = "IPR API")
@RestController
@RequestMapping(value = IPRApiPath.BASE_PATH)
@Validated
public class IPRController {

  @Autowired
  private ClientParameterHelper clientParameterHelper;

  @Autowired
  private IPRService iprService;

  @Operation(summary = "Get list of products for IPR review")
  @PostMapping(value = IPRApiPath.IPR_PRODUCT_LIST, produces = MediaType.APPLICATION_JSON_VALUE,
               consumes = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<IPRProductListResponse> getIPRProductList(
      @RequestParam(defaultValue = "0") int page, @RequestParam(defaultValue = "25") int size,
      @RequestBody IPRProductListRequest iprProductListRequest) {
    String requestId = clientParameterHelper.getRequestId();
    log.info("Fetching product list for IPR for request {} ", iprProductListRequest);
    Page<IPRProductListResponse> response =
        iprService.getIPRProductList(page, size, iprProductListRequest);
    return new ListBaseResponse<>(null, null, true, requestId, response.getContent(),
        new Metadata(page, size, response.getTotalElements()));
  }

  @Operation(summary = "Get ipr reviewer list")
  @GetMapping(value = IPRApiPath.IPR_REVIEWERS, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<List<String>> getProductReviewers() throws Exception {
    log.info("Invoking ipr reviewer list");
    List<String> response = iprService.getIPRReviewers();
    return new SingleBaseResponse<>(null, null, true, clientParameterHelper.getRequestId(),
      response);
  }

  @Operation(summary = "Get IPR product details")
  @GetMapping(value = IPRApiPath.IPR_PRODUCT_DETAIL, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<IPRProductDetailResponse> getIPRProductDetails(
      @PathVariable("productSku") String productSku) throws Exception {
    log.info("Fetching product details for IPR for productSku {}", productSku);
    IPRProductDetailResponse response = iprService.getIPRProductDetailByProductSku(productSku);
    return new SingleBaseResponse<>(null, null, true, clientParameterHelper.getRequestId(),
        response);
  }

  @Operation(summary = "Update assignee for IPR product")
  @PostMapping(value = IPRApiPath.UPDATE_ASSIGNEE, produces = MediaType.APPLICATION_JSON_VALUE,
               consumes = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse updateAssignee(@RequestBody IPRUpdateAssigneeRequest iprUpdateAssigneeRequest)
      throws Exception {
    log.info("Update assignee for IPR product with request: {}", iprUpdateAssigneeRequest);
    iprService.updateAssignee(iprUpdateAssigneeRequest);
    return new BaseResponse(null, null, true, clientParameterHelper.getRequestId());
  }

  @Operation(summary = "Get Primary filter counts for ipr portal")
  @GetMapping(value = IPRApiPath.PRIMARY_FILTER, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<MapResponse> getPrimaryFilterCounts() {
    log.info("Invoking product filter counts for ipr-portal");
    MapResponse response = iprService.getPrimaryFilterCounts();
    return new SingleBaseResponse<>(null, null, true, clientParameterHelper.getRequestId(),
      response);
  }

  @Operation(summary = "Get suspension in progress ipr products")
  @GetMapping(value = IPRApiPath.SUSPENSION_IN_PROGRESS, produces = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<IprSuspensionInProgressWebResponse> getSuspensionInProgressIprProducts(
    @RequestParam String businessPartnerCode, @RequestParam(defaultValue = "0") int page,
    @RequestParam(defaultValue = "20") int size,
    @RequestParam(defaultValue = "ASC") String sortOrder) throws Exception {
    String requestId = clientParameterHelper.getRequestId();
    log.info("Fetching suspension in progress products for IPR for seller {}", businessPartnerCode);
    Page<IprSuspensionInProgressWebResponse> response =
      iprService.getSuspensionInProcessProducts(page, size, businessPartnerCode, sortOrder);
    return new ListBaseResponse<>(null, null, true, requestId, response.getContent(),
      new Metadata(page, size, response.getTotalElements()));
  }

  @PostMapping(value = IPRApiPath.PERFORM_IPR_ACTION, produces = {
      MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Perform IPR actions")
  public BaseResponse performIprAction(@RequestBody IprActionRequest request) throws Exception {
    log.info("Performing action on ipr products for request: {}", request);
    request.setUpdatedBy(clientParameterHelper.getUsername());
    GdnBaseRestResponse response = iprService.performIprAction(request);
    return new BaseResponse(response.getErrorMessage(), response.getErrorCode(),
        response.isSuccess(), clientParameterHelper.getRequestId());
  }

  @Operation(summary = "Fetch history for ipr products")
  @GetMapping(value = IPRApiPath.HISTORY, produces = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<IPRProductHistoryResponse> fetchIprHistoryByProductSku(
      @RequestParam String productSku, @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "20") Integer size) {
    String requestId = clientParameterHelper.getRequestId();
    Page<IPRProductHistoryResponse> responses =
        iprService.fetchIprProductHistory(page, size, productSku);
    return new ListBaseResponse<>(null, null, true, requestId, responses.getContent(),
        new Metadata(page, size, responses.getTotalElements()));
  }

  @Operation(summary = "Mass download IPR Products")
  @PostMapping(value = IPRApiPath.IPR_PRODUCTS_DOWNLOAD, produces =
      MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse downloadIPRProducts(@RequestBody IPRProductsDownloadWebRequest request) {
    log.info("Invoking products download for IPR having request = {} ", request);
    iprService.downloadIPRProducts(clientParameterHelper.getUsername(), request);
    return new BaseResponse(null, null, true, clientParameterHelper.getRequestId());
  }
}

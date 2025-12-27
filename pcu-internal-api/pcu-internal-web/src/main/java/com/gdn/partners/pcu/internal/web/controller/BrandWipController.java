package com.gdn.partners.pcu.internal.web.controller;

import jakarta.validation.Valid;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RequestPart;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.core.web.dto.BaseResponse;
import com.gdn.partners.core.web.dto.ListBaseResponse;
import com.gdn.partners.core.web.dto.Metadata;
import com.gdn.partners.core.web.dto.SingleBaseResponse;
import com.gdn.partners.pcu.internal.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.internal.validaton.annotation.ApproveBrandRequestValid;
import com.gdn.partners.pcu.internal.web.model.response.BrandApprovalResponse;
import com.gdn.partners.pcu.internal.web.model.response.BrandRejectionWebResponse;
import com.gdn.partners.pcu.internal.model.BrandWipApiPath;
import com.gdn.partners.pcu.internal.service.BrandWipService;
import com.gdn.partners.pcu.internal.service.impl.helper.RequestHelper;
import com.gdn.partners.pcu.internal.web.model.request.ApproveBrandWipWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.BrandRejectWebRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandWipHistoryResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandWipHistorySummaryRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandWipResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandWipSummaryRequest;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Tag(name = "BrandWip API")
@RestController
@RequestMapping(value = BrandWipApiPath.BASE_PATH)
@Validated
public class BrandWipController {

  @Autowired
  private BrandWipService brandWipService;

  @Autowired
  private ClientParameterHelper clientParameterHelper;

  @Operation(summary = "Returns detail of brand wip")
  @GetMapping(value = BrandWipApiPath.DETAIL, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<BrandWipResponse> getBrandWipDetail(@PathVariable String brandRequestCode) {
    log.info("Fetching brand wip details for brand request code : {} ", brandRequestCode);
    GdnRestSingleResponse<BrandWipResponse> response = brandWipService.getBrandWipDetail(brandRequestCode);
    return new SingleBaseResponse<>(response.getErrorMessage(), response.getErrorCode(), response.isSuccess(),
        response.getRequestId(), response.getValue());
  }

  @Operation(summary = "Fetches brand wip history")
  @PostMapping(value = BrandWipApiPath.HISTORY_SUMMARY, produces = MediaType.APPLICATION_JSON_VALUE,
      consumes = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<BrandWipHistoryResponse> getBrandWipHistory(
      @RequestBody BrandWipHistorySummaryRequest brandWipHistorySummaryRequest,
      @RequestParam(defaultValue = "0") int page, @RequestParam(defaultValue = "10") int size) {
    log.info("Fetching brand wip history details for brand {} :", brandWipHistorySummaryRequest);
    GdnRestListResponse<BrandWipHistoryResponse> response =
        brandWipService.getBrandWipHistory(brandWipHistorySummaryRequest, page, size);
    return new ListBaseResponse<>(response.getErrorMessage(), response.getErrorCode(), response.isSuccess(), response.getRequestId(),
        response.getContent(), new Metadata(page, size, response.getPageMetaData().getTotalRecords()));
  }

  @Operation(summary = ("Returns brand wips in progress, rejected or approved"))
  @PostMapping(value = BrandWipApiPath.FILTER_SUMMARY, produces = MediaType.APPLICATION_JSON_VALUE,
      consumes = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<BrandWipResponse> getBrandWipList(
      @RequestBody BrandWipSummaryRequest brandWipSummaryRequest, @RequestParam(defaultValue = "0") int page,
      @RequestParam(defaultValue = "10") int size) {
    log.info("Fetching brand wip details for brands of name and state : {}", brandWipSummaryRequest.getBrandName(),
        brandWipSummaryRequest.getState());
    GdnRestListResponse<BrandWipResponse> response = brandWipService.getBrandWipList(brandWipSummaryRequest, page, size);
    return new ListBaseResponse<>(response.getErrorMessage(), response.getErrorCode(), response.isSuccess(),
        response.getRequestId(), response.getContent(),
        new Metadata(page, size, response.getPageMetaData().getTotalRecords()));
  }

  @Operation(summary = "Approve a brandWip")
  @PostMapping(value = BrandWipApiPath.APPROVE, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<BrandApprovalResponse> approveBrand(
      @RequestPart("request") @Valid @ApproveBrandRequestValid(message = "approve Brand validation failed")
          ApproveBrandWipWebRequest request, @PathVariable("brandRequestCode") String brandRequestCode,
      @RequestParam(required = false) MultipartFile brandLogo,
      @RequestParam(required = false) MultipartFile profileBanner) throws Exception {
    log.info("Approve brand : {}", brandRequestCode);
    BrandApprovalResponse response = this.brandWipService.approveBrand(request, brandLogo, profileBanner);
    return new SingleBaseResponse<>(null, null, true, clientParameterHelper.getRequestId(), response);
  }

  @Operation(summary = "Get rejection reason of brand")
  @GetMapping(value = BrandWipApiPath.REJECTION_REASON, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<BrandRejectionWebResponse> getBrandRejectionReason(
      @PathVariable String brandRequestCode) {
    log.info("Fetching rejection reason of brand by brand request code : {} ", brandRequestCode);
    String requestId = clientParameterHelper.getRequestId();
    BrandRejectionWebResponse response = brandWipService.getBrandRejectionReasonByBrandRequestCode(brandRequestCode);
    return new SingleBaseResponse<>(null, null, true, requestId, response);
  }

  @Operation(summary = "Reject a brandWip")
  @PostMapping(value = BrandWipApiPath.REJECT, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<String> rejectBrand(@PathVariable("brandRequestCode") String brandRequestCode,
      @RequestBody BrandRejectWebRequest request) throws Exception {
    log.info("Approve brand : {}", brandRequestCode);
    String response = this.brandWipService.rejectBrand(RequestHelper.toBrandRejectRequest(request));
    return new SingleBaseResponse(null, null, true, clientParameterHelper.getRequestId(), response);
  }

  @Operation(summary = "Update brand wip")
  @PostMapping(value = BrandWipApiPath.UPDATE, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse updateBrand(@RequestPart("request") ApproveBrandWipWebRequest request,
      @RequestParam(required = false) MultipartFile brandLogo,
      @RequestParam(required = false) MultipartFile profileBanner) throws Exception {
    log.info("Updating brand : {}", request.getBrandName());
    GdnBaseRestResponse response = this.brandWipService.update(request, brandLogo, profileBanner);
    return new BaseResponse(null, null, true, clientParameterHelper.getRequestId());
  }
}

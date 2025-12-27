package com.gdn.partners.pcu.internal.web.controller;

import java.io.IOException;
import java.util.List;


import com.gdn.partners.pcu.internal.client.model.request.BrandAuthDeleteRequest;
import jakarta.validation.Valid;
import jakarta.validation.constraints.NotEmpty;
import org.hibernate.validator.constraints.NotBlank;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.http.MediaType;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.core.web.dto.BaseResponse;
import com.gdn.partners.core.web.dto.ListBaseResponse;
import com.gdn.partners.core.web.dto.Metadata;
import com.gdn.partners.core.web.dto.SingleBaseResponse;
import com.gdn.partners.pcu.internal.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.internal.model.BrandAuthoriseApiPath;
import com.gdn.partners.pcu.internal.model.ErrorMessages;
import com.gdn.partners.pcu.internal.service.BrandAuthorisationService;
import com.gdn.partners.pcu.internal.service.impl.helper.RequestHelper;
import com.gdn.partners.pcu.internal.service.impl.util.ConverterUtil;
import com.gdn.partners.pcu.internal.web.model.request.BrandAuthCreateWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.BrandAuthHistoryWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.BrandAuthWebRequest;
import com.gdn.partners.pcu.internal.web.model.response.BrandAuthCreateWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.BrandAuthFilterWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.BrandAuthorisationDetailWebResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthCreateRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthHistoryRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthHistoryResponse;
import com.gdn.x.productcategorybase.dto.request.BrandAuthUpdateRequest;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Tag(name = "BrandAuthorisation API")
@RestController
@RequestMapping(value = BrandAuthoriseApiPath.BASE_PATH)
@Validated
public class BrandAuthorisationController {

  @Autowired
  BrandAuthorisationService brandAuthorisationService;
  @Autowired
  private ClientParameterHelper clientParameterHelper;

  @Value("${brand.auth.end.date.years.add:5}")
  private int numberOfYears;

  private static final String BRAND_AUTH_ADD = "BRAND_AUTH_ADD";
  private static final String BRAND_AUTH_DELETE = "BRAND_AUTH_DELETE";

  @Operation(summary = "Get Detail on Authorised Brand by Brand Code and Seller Code", description = "Brand Code and Seller Code are mandatory")
  @GetMapping(value = BrandAuthoriseApiPath.GET_DETAIL_BY_BRAND_CODE, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<BrandAuthorisationDetailWebResponse> getBrandAuthDetailByBrandCode(
    @PathVariable("brandCode") @Valid @NotBlank(message = ErrorMessages.EMPTY_BRAND_CODE_ERROR) String brandCode,
    @RequestParam("sellerCode") @Valid @NotBlank(message = ErrorMessages.EMPTY_SELLER_CODE_ERROR) String sellerCode) {
    log.info("Invoking getBrandAuthDetailByBrandCode for brand : {} with seller Code : {}",
      brandCode, sellerCode);
    String requestId = this.clientParameterHelper.getRequestId();
    String storeId = this.clientParameterHelper.getStoreId();
    try {
      BrandAuthorisationDetailWebResponse response =
        brandAuthorisationService.getBrandAuthDetailBySellerCodeAndBrandCode(storeId, sellerCode,
          brandCode);
      return new SingleBaseResponse<>(null, null, true, requestId, response);
    } catch (Exception e) {
      log.error("Error was Encountered while fetching Details for Seller Code : {} and error : {} ",
        sellerCode, e);
      return new SingleBaseResponse<>(ErrorCategory.UNSPECIFIED.getMessage(),
        ErrorCategory.UNSPECIFIED.getCode(), false, requestId, null);
    }
  }

  @Operation(summary = "Api to Delete Brand Auth Mapping", description = "deletes Auth mapping for a seller with brandCode")
  @PostMapping(value = BrandAuthoriseApiPath.DELETE, produces = MediaType.APPLICATION_JSON_VALUE)
  public GdnBaseRestResponse delete(@RequestBody
  @NotEmpty(message = "BrandAuth Delete Request List cannot be empty") List<@Valid BrandAuthDeleteRequest> brandAuthDeleteRequestList) {
    String requestId = this.clientParameterHelper.getRequestId();
    String storeId = this.clientParameterHelper.getStoreId();
    String username = this.clientParameterHelper.getUsername();
    log.info("Invoking new Brand Auth Delete mapping request for brandAuthDeleteRequestList : {}",
        brandAuthDeleteRequestList.toString());
    try {
      brandAuthorisationService.delete(storeId, username, brandAuthDeleteRequestList);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      log.error("Error for Brand Auth Delete mapping request for brandAuthDeleteRequestList ",
          brandAuthDeleteRequestList, e);
      return new GdnBaseRestResponse(ErrorMessages.ERR_DELETE_REQUEST_FOR_BRAND_AUTH, "400", false, requestId);
    }
  }

  @Operation(summary = "Api to get Brand Auths", description = "API to get Auths")
  @PostMapping(value = BrandAuthoriseApiPath.LISTING, produces = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<BrandAuthFilterWebResponse> getAuthorisations(
      @RequestBody BrandAuthWebRequest brandAuthFilterRequest, @RequestParam(defaultValue = "0") int page,
      @RequestParam(defaultValue = "10") int size) {
    String requestId = this.clientParameterHelper.getRequestId();
    log.info("Invoking get brand auth API for request : {}", brandAuthFilterRequest);
    Page<BrandAuthFilterWebResponse> authorisations =
        brandAuthorisationService.getAuthorisations(brandAuthFilterRequest, page, size);
    return new ListBaseResponse(null, null, true, requestId, authorisations.getContent(),
        new Metadata(page, size, authorisations.getTotalElements()));
  }

  @RequestMapping(value = BrandAuthoriseApiPath.CREATE, method = RequestMethod.POST, consumes = {
    MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Create New Brand Authorisation", description = "Create New Brand Authorisation")
  public GdnRestSingleResponse<BrandAuthCreateWebResponse> create(
    @RequestBody BrandAuthCreateWebRequest webRequest) throws Exception {
    String requestId = this.clientParameterHelper.getRequestId();
    String storeId = this.clientParameterHelper.getStoreId();
    String username = this.clientParameterHelper.getUsername();
    log.info("Adding brand auth for seller {} and brand code {} ", webRequest.getSellerCode(),
      webRequest.getBrandCode());
    RequestHelper.validateBrandAuthRequest(webRequest, false, numberOfYears);
    BrandAuthCreateRequest brandAuthRequest = ConverterUtil.toUpdateBrandRequest(webRequest);
    BrandAuthCreateWebResponse response =
      brandAuthorisationService.create(storeId, username, requestId, brandAuthRequest);
    return new GdnRestSingleResponse<>(null, null, true, response, requestId);
  }

  @RequestMapping(value = BrandAuthoriseApiPath.UPDATE, method = RequestMethod.POST, consumes = {
      MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Update Existing Brand Authorisation", description = "Update Existing "
    + "Brand Authorisation")
  public GdnBaseRestResponse update(@RequestBody BrandAuthCreateWebRequest webRequest) throws Exception {
    String requestId = this.clientParameterHelper.getRequestId();
    String storeId = this.clientParameterHelper.getStoreId();
    String username = this.clientParameterHelper.getUsername();
    log.info("Updating brand auth for seller {} and brand code {} ", webRequest.getSellerCode(),
        webRequest.getBrandCode());
    RequestHelper.validateBrandAuthRequest(webRequest, false, 0);
    BrandAuthUpdateRequest brandAuthRequest = ConverterUtil.toBrandAuthUpdateRequest(webRequest);
    brandAuthorisationService.update(storeId, username, requestId, brandAuthRequest);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @Operation(summary = "Fetch Brand Authorisation History", description = "Fetch Brand Authorisation History")
  @PostMapping(value = BrandAuthoriseApiPath.HISTORY_SUMMARY, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<BrandAuthHistoryResponse> getBrandWipHistory(
    @RequestBody BrandAuthHistoryWebRequest brandAuthHistoryWebRequest,
    @RequestParam(defaultValue = "0") int page, @RequestParam(defaultValue = "10") int size) {
    String requestId = this.clientParameterHelper.getRequestId();
    String storeId = this.clientParameterHelper.getStoreId();
    String username = this.clientParameterHelper.getUsername();
    log.info("Fetching brand auth history details for brand {} :", brandAuthHistoryWebRequest);
    //converting web request to feign call
    BrandAuthHistoryRequest brandAuthHistoryRequest =
      ConverterUtil.toBrandAuthHistoryRequest(brandAuthHistoryWebRequest);
    //getting response from PCB feign
    GdnRestListResponse<BrandAuthHistoryResponse> response = brandAuthorisationService
      .getBrandAuthHistory(storeId, requestId, username, brandAuthHistoryRequest, page, size);
    return new ListBaseResponse<>(null, null,
      response.isSuccess(), response.getRequestId(), response.getContent(),
      new Metadata(page, size, response.getPageMetaData().getTotalRecords()));
  }

  @Operation(summary = "Upload Brand Auth Doc")
  @PostMapping(value = BrandAuthoriseApiPath.UPLOAD_BRAND_AUTH_DOC, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse brandAuthDocUpload(@RequestParam MultipartFile multipartFile,
    @RequestParam @Valid @NotEmpty(message = ErrorMessages.FILE_NAME_EMPTY_ERROR) String documentFileName,
    @RequestParam @Valid @NotEmpty(message = ErrorMessages.EMPTY_BRAND_CODE_ERROR) String brandCode,
    @RequestParam @Valid @NotEmpty(message = ErrorMessages.EMPTY_SELLER_CODE_ERROR) String sellerCode)
    throws Exception {
    String requestId = clientParameterHelper.getRequestId();
    log.info("Uploading doc : {} for brand : {} with seller : {} ", documentFileName, brandCode,
      sellerCode);
    try {
      brandAuthorisationService.uploadBrandAuthDoc(documentFileName, brandCode, sellerCode,
        multipartFile.getBytes());
    } catch (IOException e) {
      log.error("Error when transferring file for file :{} seller Code : {} brand code : {} "
        + "and error ", documentFileName, sellerCode, brandCode, e);
      return new BaseResponse("Error when transferring file " + documentFileName + e.getMessage(),
        null, false, requestId);
    }
    return new BaseResponse(null, null, true, requestId);
  }

  @Operation(summary = "Bulk Brand Authorization upload")
  @PostMapping(value = BrandAuthoriseApiPath.BRAND_AUTH_BULK_UPLOAD, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse bulkBrandAuthUpload(@PathVariable("processType")
  @Valid @NotBlank(message = ErrorMessages.BRAND_AUTH_BULK_UPLOAD_EMPTY) String processType,
      @RequestParam MultipartFile request) throws Exception {
    String requestId = clientParameterHelper.getRequestId();
    GdnPreconditions.checkArgument(BRAND_AUTH_ADD.equals(processType) || BRAND_AUTH_DELETE.equals(processType),
        ErrorMessages.INVALID_RESTRICTED_KEYWORD_TYPE);
    log.info("Bulk brand authorization upload for type {} and requestId {}", processType, requestId);
    try {
      brandAuthorisationService.saveBulkBrandAuthFile(request, processType, requestId,
          clientParameterHelper.getStoreId(), clientParameterHelper.getUsername());
    } catch (IOException e) {
      return new BaseResponse("Error when transferring file " + request.getOriginalFilename() + e.getMessage(), null,
          false, requestId);
    }
    return new BaseResponse(null, null, true, requestId);
  }

  @Operation(summary = "Api to download all brand auths", description = "Api to download all brand auths")
  @PostMapping(value = BrandAuthoriseApiPath.BRAND_AUTH_DOWNLOAD_ALL, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse downloadAllBrandAuths(@RequestBody BrandAuthWebRequest brandAuthFilterRequest) {
    String requestId = this.clientParameterHelper.getRequestId();
    String username = this.clientParameterHelper.getUsername();
    log.info("Invoking download all brand auth : {} ", brandAuthFilterRequest);
    brandAuthorisationService.brandAuthDownloadAll(username, brandAuthFilterRequest);
    return new BaseResponse(null, null, true, requestId);
  }
}

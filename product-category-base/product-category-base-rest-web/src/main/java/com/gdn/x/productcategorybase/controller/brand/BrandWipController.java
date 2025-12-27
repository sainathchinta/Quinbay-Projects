package com.gdn.x.productcategorybase.controller.brand;

import com.gdn.x.productcategorybase.dto.BrandInReviewResponse;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.x.productcategorybase.BrandWipControllerPath;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.controller.util.ConverterUtil;
import com.gdn.x.productcategorybase.dto.brand.BrandApproveRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandRejectRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandRejectionInfoResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandWipHistoryResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandWipHistorySummaryRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandWipResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandWipSummaryRequest;
import com.gdn.x.productcategorybase.dto.brand.CreateBrandResponse;
import com.gdn.x.productcategorybase.dto.brand.CreateBrandWipRequest;
import com.gdn.x.productcategorybase.dto.brand.CreateBrandWipResponse;
import com.gdn.x.productcategorybase.entity.brand.BrandWip;
import com.gdn.x.productcategorybase.service.brand.BrandServiceWrapper;
import com.gdn.x.productcategorybase.service.brand.BrandWipService;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

@Slf4j
@RestController
@RequestMapping(value = BrandWipControllerPath.BASE_PATH)
@Tag(name = "Brand-WIP", description = "Brand WIP Service API")
public class BrandWipController {

  @Autowired
  private BrandWipService brandWipService;

  @Autowired
  private BrandServiceWrapper brandServiceWrapper;

  @RequestMapping(value = BrandWipControllerPath.CREATE, method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE)
  
  public GdnRestSingleResponse<CreateBrandWipResponse> create(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestBody CreateBrandWipRequest createBrandWipRequest) {
    log.info("Creating brand with name :{}", createBrandWipRequest.getBrandName());
    String errorMessage;
    String errorCode;
    CreateBrandWipResponse brandWipResponse = new CreateBrandWipResponse();
    BrandWip brandWip = ConverterUtil
        .generateCreateBrandWipRequestToBrandWip(createBrandWipRequest, createBrandWipRequest.getBusinessPartnerCode(),
            createBrandWipRequest.getBusinessPartnerName());
    try {
      String brandCode = this.brandServiceWrapper.createBrand(storeId, brandWip);
      brandWipResponse = ConverterUtil.generateCreateBrandWipResponse(brandCode);
      return new GdnRestSingleResponse<CreateBrandWipResponse>(null, null, true, brandWipResponse, requestId);
    } catch (ApplicationRuntimeException e) {
      log.error("Error creating brand " + e.getMessage(), e);
      errorCode = ErrorCategory.UNSPECIFIED.getCode();
      errorMessage = e.getMessage();
      return new GdnRestSingleResponse<>(errorMessage, errorCode, false, brandWipResponse, requestId);
    } catch (Exception e) {
      log.error("Error creating brand " + e.getMessage(), e);
      errorCode = ErrorCategory.UNSPECIFIED.getCode();
      errorMessage = e.getMessage();
      return new GdnRestSingleResponse<>(errorMessage, errorCode, false, brandWipResponse, requestId);
    }
  }

  @RequestMapping(value = BrandWipControllerPath.DETAIL, method = RequestMethod.GET,
      produces = MediaType.APPLICATION_JSON_VALUE)
  
  public GdnRestSingleResponse<BrandWipResponse> getBrandWipDetail(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam String brandRequestCode) {
    log.info("Fetching details of Brand Wip with brand request code :{}", brandRequestCode);
    String errorMessage;
    String errorCode;
    try {
      return new GdnRestSingleResponse<BrandWipResponse>(null, null, true,
          this.brandWipService.getBrandWipDetail(storeId, brandRequestCode), requestId);
    } catch (ApplicationRuntimeException e) {
      log.error("Error getting details of brand wip" + e.getMessage(), e);
      return new GdnRestSingleResponse<>(e.getErrorMessage(), e.getErrorCodes().getCode(), false, null, requestId);
    } catch (Exception e) {
      log.error("Error getting details of brand wip" + e.getMessage(), e);
      errorCode = ErrorCategory.UNSPECIFIED.getCode();
      errorMessage = e.getMessage();
      return new GdnRestSingleResponse<>(errorMessage, errorCode, false, null, requestId);
    }
  }

  @RequestMapping(value = BrandWipControllerPath.REJECTED_REASON, method = RequestMethod.GET,
                  produces = MediaType.APPLICATION_JSON_VALUE)
  
  public GdnRestSingleResponse<BrandRejectionInfoResponse> getBrandWipRejectedReason(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @PathVariable("brandRequestCode") String brandRequestCode) {
    log.info("Get brand rejection reason by brand request code :{}", brandRequestCode);
    try {
      return new GdnRestSingleResponse<>(null, null, true,
          this.brandWipService.getBrandRejectionInfoResponse(storeId, brandRequestCode), requestId);
    } catch (ApplicationRuntimeException e) {
      log.error(ErrorMessage.BRAND_WIP_NOT_FOUND.getMessage() + e.getMessage(), e);
      return new GdnRestSingleResponse<>(e.getErrorMessage(), e.getErrorCodes().getCode(), false, null, requestId);
    }
    catch (Exception e) {
      log.error("Error getting rejection reason of brand" + e.getMessage(), e);
      return new GdnRestSingleResponse<>(ErrorCategory.UNSPECIFIED.getMessage(), ErrorCategory.UNSPECIFIED.getCode(),
          false, null, requestId);
    }
  }

  @RequestMapping(value = BrandWipControllerPath.HISTORY_FILTER_SUMMARY, method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Api to fetch history of a brand")
  
  public GdnRestListResponse<BrandWipHistoryResponse> getBrandWipHistory(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username,
      @RequestBody BrandWipHistorySummaryRequest brandWipHistorySummaryRequest,
      @RequestParam(defaultValue = "0") int page, @RequestParam(defaultValue = "10") int size) {
    log.info("Fetching brand wip history for {}", brandWipHistorySummaryRequest);
    try {
      Page<BrandWipHistoryResponse> brandWipHistoryResponsePage =
          this.brandWipService.getBrandWipHistory(brandWipHistorySummaryRequest, page, size);
      return new GdnRestListResponse<>(null, null, true, brandWipHistoryResponsePage.getContent(),
          new PageMetaData(size, page, brandWipHistoryResponsePage.getTotalElements()), requestId);
    } catch (ApplicationRuntimeException e) {
      return new GdnRestListResponse<>(e.getErrorMessage(), e.getErrorCodes().getMessage(), false, null, null,
          requestId);
    } catch (Exception e) {
      return new GdnRestListResponse<>(e.getMessage(), null, false, null, null, requestId);
    }
  }

  @RequestMapping(value = BrandWipControllerPath.FILTER_SUMMARY, method = RequestMethod.POST,
      consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Api to get all the brands according to given name and state of brand")
  
  public GdnRestListResponse<BrandWipResponse> getBrandWipList(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestBody BrandWipSummaryRequest brandWipSummaryRequest,
      @RequestParam(defaultValue = "0") int page, @RequestParam(defaultValue = "10") int size) {
    log.info("Retrieving brands of name and state : {}", brandWipSummaryRequest.getBrandName(),
        brandWipSummaryRequest.getState());
    String errorCode;
    String errorMessage;
    try {
      Page<BrandWipResponse> response = this.brandWipService.getBrandWipList(brandWipSummaryRequest, page, size);
      return new GdnRestListResponse<>(response.getContent(), new PageMetaData(size, page, response.getTotalElements()),
          requestId);
    } catch (ApplicationRuntimeException e) {
      log.error("Error getting details of brand wip" + e.getMessage(), e);
      return new GdnRestListResponse<>(e.getErrorMessage(), e.getErrorCodes().getCode(), false, null, null, requestId);
    } catch (Exception e) {
      log.error("Error getting details of brand wip" + e.getMessage(), e);
      return new GdnRestListResponse<>(ErrorCategory.UNSPECIFIED.getCode(), e.getMessage(), false, null, null,
          requestId);
    }
  }


  @RequestMapping(value = BrandWipControllerPath.UPDATE, method = RequestMethod.POST,
      consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Update a brand wip")
  
  public GdnBaseRestResponse updateBrandWip(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestBody BrandApproveRequest brandApproveRequest) {
    log.info("Updating brand wip of brand request code : {} ", brandApproveRequest.getBrandRequestCode());
    try {
      this.brandServiceWrapper.updateBrand(storeId, brandApproveRequest);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (ApplicationRuntimeException e) {
      return new GdnBaseRestResponse(e.getErrorCodes().getMessage(), e.getErrorMessage(), false, requestId);
    } catch (Exception e) {
      return new GdnBaseRestResponse(ErrorCategory.UNSPECIFIED.getMessage(), e.getMessage(), false, requestId);
    }
  }

  @RequestMapping(value = BrandWipControllerPath.APPROVE, method = RequestMethod.POST,
      consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Approve Brand", description = "Approve Brand")
  
  public GdnRestSingleResponse<CreateBrandResponse> approveBrand(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestBody BrandApproveRequest request) {
    try {
      log.info("Trying to approve brand : {}", request.getBrandRequestCode());
      CreateBrandResponse response = brandServiceWrapper.approveBrand(request);
      return new GdnRestSingleResponse<>(null, null, true, response, requestId);
    } catch (ApplicationRuntimeException e) {
      return new GdnRestSingleResponse<>(e.getErrorMessage(), e.getErrorCodes().getCode(), false, null, requestId);
    } catch (Exception e) {
      log.error("Error while approving brand : {} ", request.getBrandRequestCode(), e);
      return new GdnRestSingleResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, null, requestId);
    }
  }

  @RequestMapping(value = BrandWipControllerPath.REJECT, method = RequestMethod.POST,
      consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Reject Brand", description = "Reject Brand")
  
  public GdnRestSingleResponse<BrandWipResponse> rejectBrand(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestBody BrandRejectRequest request) {
    try {
      log.info("Trying to reject brand : {}", request.getBrandRequestCode());
      BrandWipResponse brandWipResponse = brandServiceWrapper.rejectBrand(request);
      return new GdnRestSingleResponse<>(null, null, true, brandWipResponse, requestId);
    } catch (Exception e) {
      log.error("Error while rejecting brand : {} ", request.getBrandRequestCode());
      return new GdnRestSingleResponse<>(e.getMessage(), null, false, null, requestId);
    }
  }

  @RequestMapping(value = BrandWipControllerPath.FILTER_BRAND_REQUEST_CODE, method = RequestMethod.GET,
  produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Returns BrandWipResponse using brandRequestCode")
  
  public GdnRestSingleResponse<BrandWipResponse> filterByBrandRequestCode(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam String brandRequestCode) {
    try{
      log.info("Fetching brand wip for brand request code : {}", brandRequestCode);
      BrandWipResponse brandWipResponse = this.brandWipService.filterByBrandRequestCode(brandRequestCode);
      return new GdnRestSingleResponse<>(null, null, true, brandWipResponse, requestId);
    } catch(Exception e) {
      log.error("Error while rejecting brand : {} ", brandRequestCode);
      return new GdnRestSingleResponse<>(e.getMessage(), null, false, null, requestId);
    }
  }

  @RequestMapping(value = BrandWipControllerPath.FILTER_BRAND_NAME, method = RequestMethod.GET,
      produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Retrieves brand wip by name of brand and business partner code")
  
  public GdnRestSingleResponse<BrandWipResponse> findByBrandNameAndBusinessPartnerCode(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username,  @RequestParam String brandName, @RequestParam String businessPartnerCode) {
    try{
      log.info("Fetching brand wip with name :{}", brandName);
      BrandWipResponse response = brandWipService.findByBrandNameAndBusinessPartnerCode(brandName, businessPartnerCode);
      return new GdnRestSingleResponse<>(null, null, true, response, requestId);
    } catch (Exception e) {
      log.error("Error while retrieving brand : {} ", brandName);
      return new GdnRestSingleResponse<>(e.getMessage(), null, false, null, requestId);
    }
    }

  @RequestMapping(value = BrandWipControllerPath.DETAIL_BRAND_CODE, method = RequestMethod.GET, produces = MediaType
      .APPLICATION_JSON_VALUE)
  @Operation(summary = "Get Details of BrandWip by brandCode", description = "Get Details of BrandWip by brandCode")
  
  public GdnRestSingleResponse<BrandWipResponse> getBrandWipDetailByBrandCode(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam String brandCode) {
    try {
      log.info("fetching brandWip details by brandCode : {}", brandCode);
      BrandWipResponse brandWipResponse = brandWipService.getBrandWipDetailByBrandCode(storeId, brandCode);
      return new GdnRestSingleResponse<>(null, null, true, brandWipResponse, requestId);
    } catch (ApplicationRuntimeException e) {
      log.error("Error getting details of brand wip" + e.getMessage(), e);
      return new GdnRestSingleResponse<>(e.getErrorMessage(), e.getErrorCodes().getCode(), false, null, requestId);
    } catch (Exception e) {
      log.error("Error getting details of brand wip" + e.getMessage(), e);
      return new GdnRestSingleResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, null, requestId);
    }
  }

  @RequestMapping(value = BrandWipControllerPath.IN_REVIEW_BRANDS, method = RequestMethod.GET, produces = {
    MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Get all in-review brands", description = "Get all in-review brands")
  
  @Deprecated
  public GdnRestListResponse<BrandInReviewResponse> getAllInReviewBrands(
    @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
    @RequestParam String requestId, @RequestParam String username) throws Exception {
    try {
      log.info("fetching all in review brands.");
      List<BrandInReviewResponse> brandInReviewResponseList =
        brandWipService.getAllInReviewBrands(storeId);
      return new GdnRestListResponse<>(brandInReviewResponseList, null, requestId);
    } catch (ApplicationRuntimeException e) {
      log.error("Error getting list of in review brands " + e.getMessage(), e);
      return new GdnRestListResponse<>(e.getErrorMessage(), e.getErrorCodes().getCode(), false,
        null, null, requestId);
    } catch (Exception e) {
      log.error("Error getting list of in-review brands" + e.getMessage(), e);
      return new GdnRestListResponse<>(ErrorCategory.UNSPECIFIED.getCode(), e.getMessage(), false,
        null, null, requestId);
    }
  }
}
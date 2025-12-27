package com.gdn.x.mta.distributiontask.controller;

import java.util.ArrayList;
import java.util.List;

import com.gdn.x.mta.distributiontask.model.Constants;
import com.gdn.x.mta.distributiontask.model.VendorDefaultFilter;
import com.gdn.x.mta.distributiontask.request.VendorDefaultFilterRequest;
import com.gdn.x.mta.distributiontask.response.VendorDefaultFilterResponse;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Controller;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.*;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.x.mta.distributiontask.model.Vendor;
import com.gdn.x.mta.distributiontask.model.dto.VendorCapacityDTO;
import com.gdn.x.mta.distributiontask.model.dto.VendorTaskInformationDTO;
import com.gdn.x.mta.distributiontask.request.VendorDetailRequest;
import com.gdn.x.mta.distributiontask.response.ProductImageQcFeedbackResponse;
import com.gdn.x.mta.distributiontask.response.VendorDetailResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.VendorCapacityResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.VendorTaskInformationResponse;
import com.gdn.x.mta.distributiontask.service.api.ProductDistributionTaskService;
import com.gdn.x.mta.distributiontask.service.api.ProductImageQcFeedbackService;
import com.gdn.x.mta.distributiontask.service.api.VendorService;
import com.gdn.x.mta.distributiontask.util.CodeGeneratorUtil;
import com.gdn.x.mta.distributiontask.util.GdnRestSimpleResponse;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;

/**
 * Created by Alok on 9/15/16.
 */
@Controller
@RequestMapping(value = VendorController.BASE_PATH)
@Tag(name = "Vendor Controller", description = "Vendor Controller")
@Slf4j
public class VendorController {

  public static final String BASE_PATH = "/vendor";
  public static final String CREATE_VENDOR = "/save-vendor";
  public static final String VENDOR_LIST = "/vendor-list";
  public static final String GET_VENDOR_FOR_CODE = "/get-vendor-for-code";
  public static final String GET_VENDORS_CAPACITY = "/get-vendors-capacity";
  public static final String GET_VENDORS_INFORMATION_TASK = "/get-vendors-task-information";
  public static final String DELETE_VENDOR ="/delete-vendor";
  public static final String GET_CURRENTLY_ASSIGNED_PRODUCT ="/assigned-product-count";
  public static final String GET_IMAGE_FEEDBACK_DETAIL = "/image-feedback/{productCode}";
  public static final String SAVE_DEFAULT_SETTING = "/saveDefaultSettings";
  public static final String VENDOR_CODE_CANNOT_BE_BLANK ="vendorCode cannot be blank";
  public static final String GET_DEFAULT_SETTING = "/getDefaultSetting";

  @Autowired
  private VendorService vendorService;

  @Autowired
  private ProductDistributionTaskService productDistributionTaskService;

  @Autowired
  private ProductImageQcFeedbackService productImageQcFeedbackService;

  @RequestMapping(value = CREATE_VENDOR, method = RequestMethod.POST,
      produces = {MediaType.APPLICATION_JSON_VALUE},
      consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "create  Vendor ", description = "create Vendor") @ResponseBody
  @Deprecated(since = Constants.DEPRECATED_VERSION, forRemoval = true)
  public GdnBaseRestResponse save(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
      @RequestBody VendorDetailRequest vendorDetailRequest) throws Exception {
    try {
      MandatoryRequestParam mandatoryRequestParam = MandatoryRequestParam
          .generateMandatoryRequestParam(storeId, channelId, clientId, requestId, username, null);
      log.info("Api call to save vendor. MandatoryRequestParam: {}", mandatoryRequestParam);
      Vendor vendor = new Vendor();
      if (StringUtils.isEmpty(vendorDetailRequest.getId())) {
        vendorDetailRequest.setVendorCode(CodeGeneratorUtil.randomString(5));
      }
      BeanUtils.copyProperties(vendorDetailRequest, vendor);
      vendor = vendorService.save(vendor);
      VendorDetailResponse vendorDetailResponse = new VendorDetailResponse();
      BeanUtils.copyProperties(vendor, vendorDetailResponse);
      return new GdnBaseRestResponse(null, null, true, vendor.getVendorCode());
    } catch (Exception e) {
      log.error("Error in VendorController vendor save vendor Code{}", vendorDetailRequest.getVendorCode(), e);
      return new GdnBaseRestResponse(e.getMessage(), null , false, requestId);
    }
  }

  @RequestMapping(value = VENDOR_LIST, method = RequestMethod.GET,
      produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "get Vendor List  Vendor ", description = "get Vendor list") @ResponseBody
  public GdnRestListResponse<VendorDetailResponse> getVendorList(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size) throws Exception {
    try {
      MandatoryRequestParam mandatoryRequestParam = MandatoryRequestParam
          .generateMandatoryRequestParam(storeId, channelId, clientId, requestId, username, null);
      log.info("Api call to get vendor List. MandatoryRequestParam: {} page{} size {}",
          mandatoryRequestParam, page, size);
      Pageable pageable = PageRequest.of(page,size);
      Page<Vendor> vendorList = vendorService.findVendorList(pageable);
      List<VendorDetailResponse> vendorDetailResponseList = new ArrayList<>();
      VendorDetailResponse vendorDetailResponse;
      for (Vendor vendor : vendorList.getContent()) {
        vendorDetailResponse = new VendorDetailResponse();
        BeanUtils.copyProperties(vendor, vendorDetailResponse);
        vendorDetailResponseList.add(vendorDetailResponse);
      }
      return new GdnRestListResponse<VendorDetailResponse>(null, null, true,
          vendorDetailResponseList,
          new PageMetaData(pageable.getPageSize(), pageable.getPageNumber(),
              vendorList.getTotalElements()), requestId);
    } catch (Exception e) {
      log.error("Error  to get vendor List", e);
      return new GdnRestListResponse<VendorDetailResponse>(e.getMessage(), null, false, null,
          new PageMetaData(page, size, 0), requestId);
    }
  }
  
  @RequestMapping(value = GET_VENDORS_CAPACITY, method = RequestMethod.GET,
      produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "get Vendor capacity", description = "get Vendor capacity list")
  @ResponseBody
  @Deprecated(since = Constants.DEPRECATED_VERSION, forRemoval = true)
  public GdnRestListResponse<VendorCapacityResponse> countVendorsCapacity(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username) throws Exception {
    try {
      MandatoryRequestParam mandatoryRequestParam = MandatoryRequestParam
          .generateMandatoryRequestParam(storeId, channelId, clientId, requestId, username, null);
      log.info("Api call countVendorsCapacity list. MandatoryRequestParam: {}",
          mandatoryRequestParam);
      List<VendorCapacityDTO> vendorCapacityDTOs = vendorService.countVendorsCapacity();
      List<VendorCapacityResponse> vendorCapacityResponses = new ArrayList<>();
      for (VendorCapacityDTO vendorCapacityDTO : vendorCapacityDTOs) {
        VendorCapacityResponse vendorCapacityResponse = new VendorCapacityResponse();
        BeanUtils.copyProperties(vendorCapacityDTO, vendorCapacityResponse);
        vendorCapacityResponses.add(vendorCapacityResponse);
      }
      return new GdnRestListResponse<VendorCapacityResponse>(vendorCapacityResponses,
          new PageMetaData(1, 0, vendorCapacityResponses.size()), requestId);
    } catch (Exception e) {
      log.error("Error  to countVendorsCapacity List", e);
      return new GdnRestListResponse<VendorCapacityResponse>(e.getMessage(),
          ErrorCategory.UNSPECIFIED.name(), false, requestId);
    }
  }
  
  @RequestMapping(value = GET_VENDORS_INFORMATION_TASK, method = RequestMethod.GET,
      produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "get Vendor capacity", description = "get Vendor capacity list")
  @ResponseBody
  @Deprecated(since = Constants.DEPRECATED_VERSION, forRemoval = true)
  public GdnRestListResponse<VendorTaskInformationResponse> countVendorsInformationTask(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username) throws Exception {
    try {
      MandatoryRequestParam mandatoryRequestParam = MandatoryRequestParam
          .generateMandatoryRequestParam(storeId, channelId, clientId, requestId, username, null);
      log.info("Api call countVendorsCapacity list. MandatoryRequestParam: {}",
          mandatoryRequestParam);
      List<VendorTaskInformationDTO> vendorTaskInformationDTOs = vendorService.countVendorAssignationAndCapacity();
      List<VendorTaskInformationResponse> vendorTaskInformationResponses = new ArrayList<>();
      for (VendorTaskInformationDTO vendorTaskInformationDTO : vendorTaskInformationDTOs) {
        VendorTaskInformationResponse vendorTaskInformationResponse = new VendorTaskInformationResponse();
        BeanUtils.copyProperties(vendorTaskInformationDTO, vendorTaskInformationResponse);
        vendorTaskInformationResponses.add(vendorTaskInformationResponse);
      }
      return new GdnRestListResponse<VendorTaskInformationResponse>(vendorTaskInformationResponses,
          new PageMetaData(1, 0, vendorTaskInformationResponses.size()), requestId);
    } catch (Exception e) {
      log.error("Error  to countVendorsCapacity List", e);
      return new GdnRestListResponse<VendorTaskInformationResponse>(e.getMessage(),
          ErrorCategory.UNSPECIFIED.name(), false, requestId);
    }
  }

  @RequestMapping(value = GET_VENDOR_FOR_CODE, method = RequestMethod.GET,
      produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "get vendor By vendor code ", description = "get vendor By vendor code")
  @ResponseBody
  public GdnRestSingleResponse<VendorDetailResponse> getVendorByCode(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam String vendorCode) throws Exception {
    try {
      MandatoryRequestParam mandatoryRequestParam = MandatoryRequestParam
          .generateMandatoryRequestParam(storeId, channelId, clientId, requestId, username, null);
      log.info("Api call to get vendor By vendor code. MandatoryRequestParam: {} , vendorCode{}",
          mandatoryRequestParam, vendorCode);
      Vendor vendor = vendorService.findByVendorCode(vendorCode);
      VendorDetailResponse vendorDetailResponse = new VendorDetailResponse();
      BeanUtils.copyProperties(vendor, vendorDetailResponse);
      return new GdnRestSingleResponse<>(null, null, true, vendorDetailResponse, requestId);
    } catch (Exception e) {
      log.info("Error to get vendor By vendor code :{}", vendorCode, e);
      return new GdnRestSingleResponse<>(e.getMessage(), null, false, null, requestId);
    }
  }

  @RequestMapping(value = DELETE_VENDOR, method = RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE}, consumes = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation
  @ResponseBody
  @Deprecated(since = Constants.DEPRECATED_VERSION, forRemoval = true)
  public GdnBaseRestResponse deleteVender(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam String vendorCode) {
    Boolean isSuccess = false;
    String errorMessage = org.apache.commons.lang3.StringUtils.EMPTY;
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(vendorCode), VENDOR_CODE_CANNOT_BE_BLANK);
    try {
      MandatoryRequestParam mandatoryRequestParam = MandatoryRequestParam
          .generateMandatoryRequestParam(storeId, channelId, clientId, requestId, username, null);
      log.info(
          "Api call to get vendor By vendor code and delete them. MandatoryRequestParam: {} , vendorCode{}",
          mandatoryRequestParam, vendorCode);
      Vendor vendor = vendorService.findByVendorCode(vendorCode);
      if (vendor != null) {
        productDistributionTaskService.movingProductsbackToProductDistribution(vendor);
        vendorService.deleteVender(vendor);
        isSuccess = true;
      } else {
        log.error("Vendor cannot be found :{}", vendorCode);
        errorMessage = " Failed to the find vendor   ";
      }
    } catch (Exception e) {
      log.error("error while deleting vendor :{}", vendorCode, e);
      errorMessage = "Failed to delete the vendor ";
    }
    return new GdnBaseRestResponse(errorMessage, null, isSuccess, requestId);
  }

  @RequestMapping(value = GET_CURRENTLY_ASSIGNED_PRODUCT, method = RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @ResponseBody
  @Operation
  @Deprecated(since = Constants.DEPRECATED_VERSION, forRemoval = true)
  public GdnRestSimpleResponse<Integer> getCountOfAssignedProduct(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam String vendorCode) {
    try {
      MandatoryRequestParam mandatoryRequestParam = MandatoryRequestParam
          .generateMandatoryRequestParam(storeId, channelId, clientId, requestId, username, null);
      log.info(
          "Api call to get number of product assign to vendor. MandatoryRequestParam: {} , vendorCode{}",
          mandatoryRequestParam, vendorCode);
      Integer count = vendorService.assignedProductCount(vendorCode);
      return new GdnRestSimpleResponse<>(null, null, true, requestId, count);
    } catch (Exception e) {
      log.error("error while getting count of product assign vendor :{}", vendorCode, e);
      return new GdnRestSimpleResponse<>(e.getMessage(), null, false, requestId, null);
    }
  }

  @RequestMapping(value = GET_IMAGE_FEEDBACK_DETAIL, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  @Operation
  public GdnRestSimpleResponse<ProductImageQcFeedbackResponse> getImageFeedbackDetail(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @PathVariable("productCode") String productCode) {
    try {
      MandatoryRequestParam mandatoryRequestParam =
          MandatoryRequestParam.generateMandatoryRequestParam(storeId, channelId, clientId, requestId, username, null);
      log.info("Api call to get image feedback detail for productCode : {}", productCode);
      ProductImageQcFeedbackResponse response =
          productImageQcFeedbackService.findProductQcFeedbackResponseByProductCode(storeId, productCode);
      return new GdnRestSimpleResponse<>(null, null, true, requestId, response);
    } catch (Exception e) {
      return new GdnRestSimpleResponse<>(e.getMessage(), null, false, requestId, null);
    }
  }

  @PostMapping(value = SAVE_DEFAULT_SETTING, consumes = {MediaType.APPLICATION_JSON_VALUE}, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @ResponseBody
  @Operation
  @Deprecated(since = Constants.DEPRECATED_VERSION, forRemoval = true)
  public GdnBaseRestResponse saveDefaultSetting(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestBody VendorDefaultFilterRequest vendorDefaultFilterRequest) throws Exception {
    try {
      MandatoryRequestParam mandatoryRequestParam = MandatoryRequestParam.generateMandatoryRequestParam(storeId, channelId, clientId, requestId);
      log.info("Api call to save default setting for auto assignment vendorEmail : {} ", vendorDefaultFilterRequest.getVendorEmail());
      VendorDefaultFilter vendorDefaultFilter = vendorService.saveDefaultSettingFilter(vendorDefaultFilterRequest, storeId);
      return new GdnBaseRestResponse(null, null, true, vendorDefaultFilter.getVendorEmail());
    } catch (Exception e) {
      log.error("Error in VendorController to save default setting for auto assignment {} ", vendorDefaultFilterRequest.getVendorEmail(), e);
      return new GdnBaseRestResponse(e.getMessage(), null, false, requestId);
    }
  }

  @RequestMapping(value = GET_DEFAULT_SETTING, method = RequestMethod.GET,
          produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation
  @ResponseBody
  @Deprecated(since = Constants.DEPRECATED_VERSION, forRemoval = true)
  public GdnRestSingleResponse<VendorDefaultFilterResponse> getDefaultSetting(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam String vendorEmail) throws Exception {
    try {
      MandatoryRequestParam mandatoryRequestParam = MandatoryRequestParam
              .generateMandatoryRequestParam(storeId, channelId, clientId, requestId);
      log.info(
              "Api call to get default setting by vendor email.  MandatoryRequestParam: {} , vendorEmail: {} ",
              mandatoryRequestParam, vendorEmail);
      VendorDefaultFilterResponse vendorDefaultFilterResponse = vendorService.getDefaultSettingFilter(storeId, vendorEmail);
      return new GdnRestSingleResponse<>(null, null, true, vendorDefaultFilterResponse, requestId);
    } catch (Exception e) {
      log.info("Error to get default setting by vendor email :{}, error - ", vendorEmail, e);
      return new GdnRestSingleResponse<>(e.getMessage(), null, false, null, requestId);
    }
  }
}

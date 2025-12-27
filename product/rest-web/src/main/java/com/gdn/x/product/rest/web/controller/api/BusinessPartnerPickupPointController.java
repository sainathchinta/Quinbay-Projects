package com.gdn.x.product.rest.web.controller.api;

import java.util.List;

import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.x.product.model.entity.BusinessPartnerPickupPoint;
import com.gdn.x.product.service.util.CommonUtil;
import com.gdn.x.product.service.util.ModelConverter;
import com.gdn.x.product.rest.web.model.BusinessPartnerPickupPointApiPath;
import com.gdn.x.product.rest.web.model.enums.ProductErrorCodesEnum;
import com.gdn.x.product.rest.web.model.request.PickupPointSummaryRequest;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.rest.web.model.response.BusinessPartnerPickupPointResponse;
import com.gdn.x.product.service.api.BusinessPartnerPickupPointService;
import lombok.extern.slf4j.Slf4j;

@Controller
@RequestMapping(value = BusinessPartnerPickupPointApiPath.BASE_PATH)
@Tag(name = "Business Partner Pickup Point Controller", description = "Business Partner Pickup "
  + "Point Controller")
@Slf4j
public class BusinessPartnerPickupPointController {

  @Autowired
  private BusinessPartnerPickupPointService businessPartnerPickupPointService;

  @Autowired
  private ModelConverter modelConverter;

  @Value("${validate.business.partner.code.for.security.enabled}")
  private boolean validateBusinessPartnerCodeForSecurity;

  @RequestMapping(value = BusinessPartnerPickupPointApiPath.GET_PICKUP_POINT_DETAILS_BY_PICKUP_POINT_CODES,
      method = RequestMethod.POST, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  public GdnRestListResponse<BusinessPartnerPickupPointResponse> getBusinessPartnerPickupPointDetails(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestBody SimpleListStringRequest pickupPointCodes,
      @RequestParam(value = "merchantCode", required = false) String merchantCode) {
    try {
      log.info("Fetching business partner pickupPoint details for store ID : {} and pickupPointCodes : {} ",
          storeId, pickupPointCodes);
      List<BusinessPartnerPickupPoint> businessPartnerPickupPointList = businessPartnerPickupPointService
          .getBusinessPartnerPickupPointByPickupPointCodes(storeId, pickupPointCodes.getValue());
      businessPartnerPickupPointList =
          CommonUtil.filterBusinessPartnerPickupPointBasedOnMerchantCode(merchantCode, businessPartnerPickupPointList,
              validateBusinessPartnerCodeForSecurity);
      List<BusinessPartnerPickupPointResponse> response = this.modelConverter
          .convertListToResponse(businessPartnerPickupPointList, BusinessPartnerPickupPointResponse.class);
      return new GdnRestListResponse<>(response, new PageMetaData(response.size(), 0, response.size()), requestId);
    } catch (Exception e) {
      log.error("#getBusinessPartnerPickupPointDetails of list = {} failed with error = {} ",
          pickupPointCodes.getValue(), e.getMessage(), e);
      return new GdnRestListResponse<>(e.getMessage(), ProductErrorCodesEnum.INTERNAL_SERVER.getCode(), false,
          requestId);
    }
  }

  @RequestMapping(value = BusinessPartnerPickupPointApiPath.GET_PICKUP_POINT_DETAILS_SUMMARY, method = RequestMethod.POST, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  public GdnRestListResponse<BusinessPartnerPickupPointResponse> getBusinessPartnerPickupPointSummary(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username, @RequestParam(defaultValue = "0") int page,
      @RequestParam(defaultValue = "50") int size, @RequestBody PickupPointSummaryRequest pickupPointSummaryRequest) {
    try {
      log.info("Fetching business partner pickupPoint summary for store ID : {} and request : {} ", storeId,
          pickupPointSummaryRequest);
      Page<BusinessPartnerPickupPointResponse> businessPartnerPickupPointResponsePage =
          businessPartnerPickupPointService.getBusinessPartnerPickupPointSummary(storeId, page, size,
              pickupPointSummaryRequest);
      return new GdnRestListResponse<>(businessPartnerPickupPointResponsePage.getContent(),
          new PageMetaData(size, page, businessPartnerPickupPointResponsePage.getTotalElements()), requestId);
    } catch (Exception e) {
      log.error("#getBusinessPartnerPickupPointDetails of request = {} failed with error = {} ",
          pickupPointSummaryRequest, e);
      return new GdnRestListResponse<>(e.getMessage(), ProductErrorCodesEnum.INTERNAL_SERVER.getCode(), false,
          requestId);
    }
  }
}
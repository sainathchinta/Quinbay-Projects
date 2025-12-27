package com.gdn.x.product.rest.web.controller.api;

import java.util.List;

import io.swagger.v3.oas.annotations.tags.Tag;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.x.product.model.entity.BusinessPartner;
import com.gdn.x.product.service.util.ModelConverter;
import com.gdn.x.product.rest.web.model.BusinessPartnerApiPath;
import com.gdn.x.product.rest.web.model.enums.ProductErrorCodesEnum;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.rest.web.model.response.BusinessPartnerResponse;
import com.gdn.x.product.service.api.BusinessPartnerService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping(value = BusinessPartnerApiPath.BASE_PATH)
@Tag(name = "Business Partner Controller", description = "Business Partner Service Api")
@Slf4j
public class BusinessPartnerController {

  private static final Logger LOGGER = LoggerFactory.getLogger(BusinessPartnerController.class);

  @Autowired
  private BusinessPartnerService businessPartnerService;

  @Autowired
  private ModelConverter modelConverter;

  @Deprecated
  @RequestMapping(value = {BusinessPartnerApiPath.GET_BUSINESS_PARTNER_DETAIL}, method = {
      RequestMethod.POST}, consumes = {MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  public GdnRestListResponse<BusinessPartnerResponse> getBusinessPartnerDetails(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestBody SimpleListStringRequest businessPartnerCodes) {
    try {
      log.info("Fetching business partner details for store ID : {} and business partner codes : {}", storeId,
          businessPartnerCodes);
      List<BusinessPartner> result =
          this.businessPartnerService.findByStoreIdAndBusinessPartnerCodes(storeId, businessPartnerCodes);
      List<BusinessPartnerResponse> response =
          this.modelConverter.convertListToResponse(result, BusinessPartnerResponse.class);
      return new GdnRestListResponse<>(response, new PageMetaData(response.size(), 0, response.size()), requestId);
    } catch (Exception e) {
      log.error("#getBusinessPartnerDetails of businessPartnerList = {} failed with error = {}",
          businessPartnerCodes.getValue(), e.getMessage(), e);
      return new GdnRestListResponse<>(e.getMessage(), ProductErrorCodesEnum.GET_BUSINESS_PARTNER_DETAILS.getCode(),
          false, requestId);
    }
  }
}

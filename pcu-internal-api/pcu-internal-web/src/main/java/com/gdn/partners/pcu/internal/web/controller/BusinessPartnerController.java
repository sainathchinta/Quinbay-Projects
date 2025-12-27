package com.gdn.partners.pcu.internal.web.controller;


import com.gdn.partners.core.web.dto.SingleBaseResponse;
import com.gdn.partners.pcu.internal.model.ErrorMessages;
import com.gdn.partners.pcu.internal.web.model.response.BusinessPartnerProfileWebResponse;
import jakarta.validation.constraints.NotBlank;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.http.MediaType;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.gdn.partners.core.web.dto.ListBaseResponse;
import com.gdn.partners.core.web.dto.Metadata;
import com.gdn.partners.pcu.internal.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.internal.model.BusinessPartnerApiPath;
import com.gdn.partners.pcu.internal.service.BusinessPartnerService;
import com.gdn.partners.pcu.internal.web.model.request.ProductSuspensionFilterRequest;
import com.gdn.partners.pcu.internal.web.model.request.ReviewProductsFilterRequest;
import com.gdn.partners.pcu.internal.web.model.response.BusinessPartnerWebResponse;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;


@Slf4j
@Tag(name = "Business partners API")
@RestController
@RequestMapping(value = BusinessPartnerApiPath.BASE_PATH)
@Validated
public class BusinessPartnerController {

  @Autowired
  private ClientParameterHelper clientParameterHelper;

  @Autowired
  private BusinessPartnerService businessPartnerService;

  @Operation(summary = "Get business partners by time and status filters")
  @PostMapping(value = BusinessPartnerApiPath.BUSINESS_PARTNER_FILTER, produces = MediaType.APPLICATION_JSON_VALUE,
               consumes = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<BusinessPartnerWebResponse> getBusinessPartnersByTimeAndStatusFilter(
      @RequestBody ReviewProductsFilterRequest request, @RequestParam boolean activated, @RequestParam boolean viewable,
      @RequestParam(defaultValue = "0") int page, @RequestParam(defaultValue = "30") int size) {
    log.info("business partners by filter request: {}, activated :{}, viewable :{}", request, activated, viewable);
    Page<BusinessPartnerWebResponse> partnerWebResponsePage =
        businessPartnerService.getBusinessPartnersByTimeAndStatusFilter(request, activated, viewable, page, size);
    return new ListBaseResponse<>(null, null, true,
        clientParameterHelper.getRequestId(),
        partnerWebResponsePage.getContent(), new Metadata(page, size, partnerWebResponsePage.getTotalElements()));
  }

  @Operation(summary = "Get all the active merchant list")
  @PostMapping(value = BusinessPartnerApiPath.BUSINESS_PARTNER_LIST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<BusinessPartnerWebResponse> getAllActiveMerchantList(
      @RequestBody ProductSuspensionFilterRequest request, @RequestParam(defaultValue = "0") int page,
      @RequestParam(defaultValue = "30") int size) {
    log.info("active merchant list by filter request: {}", request);
    Page<BusinessPartnerWebResponse> partnerWebResponsePage =
        businessPartnerService.getAllActiveMerchantList(request, page, size);
    return new ListBaseResponse<>(null, null, true, clientParameterHelper.getRequestId(),
        partnerWebResponsePage.getContent(), new Metadata(page, size, partnerWebResponsePage.getTotalElements()));
  }

  @Operation(summary = "Get business partner flags")
  @GetMapping(value = BusinessPartnerApiPath.BUSINESS_PARTNER_FLAGS, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<BusinessPartnerProfileWebResponse> getBusinessPartnerFlags(
    @RequestParam @NotBlank(message = ErrorMessages.EMPTY_MERCHANT_CODE) String businessPartnerCode) {
    BusinessPartnerProfileWebResponse businessPartnerFlags =
      businessPartnerService.fetchBusinessPartnerFlags(businessPartnerCode);
    return new SingleBaseResponse<>(null, null, true, this.clientParameterHelper.getRequestId(),
      businessPartnerFlags);
  }
}

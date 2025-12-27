package com.gdn.partners.pcu.external.web.controller;

import java.util.List;
import java.util.Objects;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import com.gdn.partners.core.web.dto.BaseResponse;
import com.gdn.partners.core.web.dto.ListBaseResponse;
import com.gdn.partners.core.web.dto.Metadata;
import com.gdn.partners.core.web.dto.SingleBaseResponse;
import com.gdn.partners.pcu.external.client.helper.MandatoryParameterHelper;
import com.gdn.partners.pcu.external.model.BrandApiPath;
import com.gdn.partners.pcu.external.model.Constants;
import com.gdn.partners.pcu.external.service.BrandService;
import com.gdn.partners.pcu.external.web.model.response.BrandAuthorisationResponse;
import com.gdn.partners.pcu.external.web.model.response.PredefinedAttributeValueWebResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandSummaryRequest;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Tag(name ="Brand API")
@RestController
@RequestMapping(value = BrandApiPath.BASE_PATH)
public class BrandController {

  @Autowired
  private BrandService brandService;

  @Autowired
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Operation(summary ="Get brand suggestions")
  @GetMapping(value = BrandApiPath.GET_BRAND_SUGGESTIONS, produces = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<PredefinedAttributeValueWebResponse> getBrandSuggestions(
      @RequestParam(required = false, defaultValue = StringUtils.EMPTY) String value,
      @RequestParam(defaultValue = "false") boolean isSearch, @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size, @RequestParam(required = false, defaultValue = StringUtils.EMPTY) String clientId) {
    String requestId = mandatoryParameterHelper.getRequestId();
    log.info("Get brand suggestions by value, requestId : {} , value : {}", requestId, value);
    Page<PredefinedAttributeValueWebResponse> response =
        brandService.getBrandSuggestions(value, mandatoryParameterHelper, isSearch, page, size);
    if (CollectionUtils.isEmpty(response.getContent())) {
      List<PredefinedAttributeValueWebResponse> defaultBrands = brandService.getDefaultBrands(mandatoryParameterHelper);
      return new ListBaseResponse<>(null, null, true, requestId, defaultBrands,
          new Metadata(page, size, 0l));
    }
    return new ListBaseResponse<>(null, null, true, requestId, response.getContent(),
        new Metadata(page, size, (long) response.getTotalElements()));
  }

  @Operation(summary ="Fetch all active brands")
  @RequestMapping(value = BrandApiPath.FILTER_SUMMARY, method = RequestMethod.GET,
      produces = {MediaType.APPLICATION_JSON_VALUE})
  @ResponseBody
  public ListBaseResponse<BrandResponse> filterSummary(
      @RequestParam(required = false) String brandName,
      @RequestParam(defaultValue = "false") boolean markForDelete,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size) throws Exception {
    String requestId = mandatoryParameterHelper.getRequestId();
    BrandSummaryRequest request = new BrandSummaryRequest();
    request.setBrandName(brandName);
    request.setMarkForDelete(markForDelete);
    Pageable pageable = PageRequest.of(page, size);
    Page<BrandResponse> brandResponses = brandService.findSummaryByFilter(request, pageable);
    return new ListBaseResponse<>(null, null, true, requestId, brandResponses.getContent(),
        new Metadata(page, size, brandResponses.getTotalElements()));
  }

  @Operation(summary ="Check if brand already exists")
  @GetMapping(value = BrandApiPath.FILTER_BRAND_NAME, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse filterByBrandName(@RequestParam("brandName") String brandName) {
    String requestId = mandatoryParameterHelper.getRequestId();
    log.info("Filter by brand name, requestId : {} , brandName : {}", requestId, brandName);
    BrandResponse brand = brandService.findByBrandName(brandName);
    if(Objects.nonNull(brand)) {
      return new BaseResponse(null, null, true, requestId);
    }
    else {
      return new BaseResponse(null, null, false, requestId);
    }
  }

  @Operation(summary ="Check if brand is authorised for given seller")
  @RequestMapping(value = BrandApiPath.VALIDATE, method = RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @ResponseBody
  public SingleBaseResponse<BrandAuthorisationResponse> validateAuthorisedBrand(@RequestParam String brandCode,
      @RequestParam(required = false) String businessPartnerCode) {
    boolean isExternal = Boolean.valueOf(mandatoryParameterHelper.isExternal());
    if (isExternal) {
      businessPartnerCode = mandatoryParameterHelper.getBusinessPartnerCode();
    }
    String requestId = mandatoryParameterHelper.getRequestId();
    boolean brandResponse = brandService.validateAuthorisedBrand(brandCode, businessPartnerCode);
    return new SingleBaseResponse<>(null, null, true, requestId, new BrandAuthorisationResponse(brandResponse));
  }
}

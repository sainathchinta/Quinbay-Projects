package com.gdn.partners.pcu.external.service.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import com.gdn.partners.pcu.external.client.helper.MandatoryParameterHelper;
import com.gdn.x.productcategorybase.dto.BrandInReviewResponse;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pcu.external.client.feign.PBPFeign;
import com.gdn.partners.pcu.external.client.feign.PCBFeign;
import com.gdn.partners.pcu.external.model.Constants;
import com.gdn.partners.pcu.external.service.BrandService;
import com.gdn.partners.pcu.external.service.impl.helper.ResponseHelper;
import com.gdn.partners.pcu.external.web.model.response.PredefinedAttributeValueWebResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandSummaryRequest;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.SimpleBooleanResponse;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class BrandServiceImpl implements BrandService {

  @Value(value = "${api.brand-max.limit}")
  private int MAX_LIMIT;

  @Value("${no.brand.switch}")
  private boolean noBrandSwitch;

  @Value("${no.brand.android.version}")
  private int noBrandAndroidVersion;

  @Value("${no.brand.ios.version}")
  private int noBrandIosVersion;

  @Autowired
  private PCBFeign pcbFeign;

  @Autowired
  private PBPFeign pbpFeign;

  @Override
  public Page<PredefinedAttributeValueWebResponse> getBrandSuggestions(String value, MandatoryParameterHelper mandatoryParameterHelper,
      boolean isSearch, Integer page, Integer size) {
    String businessPartnerCode = Constants.DEFAULT_BP_CODE;
    boolean isExternal = Boolean.valueOf(mandatoryParameterHelper.isExternal());
    if (isExternal) {
      businessPartnerCode = mandatoryParameterHelper.getBusinessPartnerCode();
    }
    GdnRestListResponse<PredefinedAllowedAttributeValueResponse> response =
        pcbFeign.getBrandSuggestions(value, businessPartnerCode, isSearch, isExternal, page, size);
    ResponseHelper.validateResponse(response);
    return new PageImpl<>(ResponseHelper.toPredefinedAttributeValueWebResponseList(response.getContent()),
        PageRequest.of(page, size), response.getPageMetaData().getTotalRecords());
  }

  @Override
  public List<PredefinedAllowedAttributeValueResponse> getBrandSuggestions(String value, String businessPartnerCode,
      boolean isSearch, boolean isExternal) {
    GdnRestListResponse<PredefinedAllowedAttributeValueResponse> response =
        pcbFeign.getBrandSuggestions(value, businessPartnerCode, isSearch, isExternal, 0, MAX_LIMIT);
    if(Objects.isNull(response) || !response.isSuccess()) {
      log.error("Exception while getting Brand suggestions from PCB businessPartnerCode : {}", businessPartnerCode);
      return new ArrayList<>();
    }
    return response.getContent();
  }

  @Override
  public Page<BrandResponse> findSummaryByFilter(BrandSummaryRequest request, Pageable pageable) {
    GdnRestListResponse<BrandResponse> responses =
        this.pcbFeign.filterSummary(pageable.getPageNumber(), pageable.getPageSize(), request);
    ResponseHelper.validateResponse(responses);
    return new PageImpl<BrandResponse>(responses.getContent(), pageable, responses.getPageMetaData().getTotalRecords());
  }


  @Override
  public BrandResponse findByBrandName(String brandName) {
    GdnRestSingleResponse<BrandResponse> response = this.pcbFeign.filterByBrandName(brandName, true);
    return response.getValue();
  }

  @Override
  public List<PredefinedAllowedAttributeValueResponse> activeBrandsByCategoryId(String categoryId) throws Exception {
    GdnRestListResponse<PredefinedAllowedAttributeValueResponse>
        response =
        this.pbpFeign.activeBrandByCategoryId(categoryId);
    if (!response.isSuccess()) {
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getContent();
  }

  @Override
  public List<PredefinedAttributeValueWebResponse> getDefaultBrands(MandatoryParameterHelper mandatoryParameterHelper) {
    if (noBrandSwitch) {
      if (mandatoryParameterHelper.getClientId().equals(Constants.APP_CLIENT_ID)) {
        if (checkIfRequestFromNewAppVersion(mandatoryParameterHelper)) {
          return new ArrayList<>();
        }
      } else {
        return new ArrayList<>();
      }
    }
    GdnRestListResponse<PredefinedAllowedAttributeValueResponse> response = pcbFeign.getDefaultBrands();
    ResponseHelper.validateResponse(response);
    return ResponseHelper.toPredefinedAttributeValueWebResponseList(response.getContent());
  }

  private boolean checkIfRequestFromNewAppVersion(MandatoryParameterHelper mandatoryParameterHelper) {
    return Constants.APP_CLIENT_ID.equalsIgnoreCase(mandatoryParameterHelper.getAppType())
        && mandatoryParameterHelper.getAppVersion() > noBrandAndroidVersion
        || Constants.IOS_CLIENT_ID.equalsIgnoreCase(mandatoryParameterHelper.getAppType())
        && mandatoryParameterHelper.getAppVersion() > noBrandIosVersion;
  }

  @Override
  public boolean validateAuthorisedBrand(String brandCode, String businessPartnerCode) {
    GdnRestSingleResponse<SimpleBooleanResponse> response =
        pcbFeign.validateAuthorisedBrand(brandCode, businessPartnerCode);
    ResponseHelper.validateResponse(response);
    return response.getValue().getResult();
  }

  @Override
  public List<BrandInReviewResponse> getAllInReviewBrand(String storeId,
    String requestId) {
    GdnRestListResponse<BrandInReviewResponse> response =
      pcbFeign.getAllInReviewBrands(storeId, requestId);
    ResponseHelper.validateResponse(response);
    return response.getContent();
  }
}

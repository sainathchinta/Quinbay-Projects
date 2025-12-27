package com.gdn.partners.pcu.internal.service.impl;

import java.util.Collections;
import java.util.List;
import java.util.Optional;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.partners.pcu.internal.model.ErrorMessages;
import com.gdn.partners.pcu.internal.web.model.response.BusinessPartnerProfileWebResponse;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import jakarta.validation.ValidationException;
import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;

import com.gda.mta.product.dto.ProductBusinessPartnerMapperResponse;
import com.gda.mta.product.dto.SummaryFilterRequest;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.x.businesspartner.v2.dto.businesspartner.BusinessPartnerFilterRequest;
import com.gdn.partners.pcu.internal.client.feign.PBPFeign;
import com.gdn.partners.pcu.internal.client.feign.XBPFeign;
import com.gdn.partners.pcu.internal.service.BusinessPartnerService;
import com.gdn.partners.pcu.internal.service.impl.helper.RequestHelper;
import com.gdn.partners.pcu.internal.service.impl.helper.ResponseHelper;
import com.gdn.partners.pcu.internal.web.model.request.ProductSuspensionFilterRequest;
import com.gdn.partners.pcu.internal.web.model.request.ReviewProductsFilterRequest;
import com.gdn.partners.pcu.internal.web.model.response.BusinessPartnerWebResponse;
import com.gdn.x.businesspartner.dto.ProfileResponse;

@Service
public class BusinessPartnerServiceImpl implements BusinessPartnerService {

  @Autowired
  private PBPFeign pbpFeign;

  @Autowired
  private XBPFeign xbpFeign;

  @Override
  public Page<BusinessPartnerWebResponse> getBusinessPartnersByTimeAndStatusFilter(
      ReviewProductsFilterRequest request, boolean activated, boolean viewable, int page, int size) {
    SummaryFilterRequest summaryFilterRequest = RequestHelper.getSummaryFilterRequest(request);
    GdnRestListResponse<ProductBusinessPartnerMapperResponse> response = pbpFeign
        .getBusinessPartnersByTimeAndStatusFilter(summaryFilterRequest, activated, viewable, page, size);
    ResponseHelper.validateResponse(response);
    List<BusinessPartnerWebResponse> businessPartnerWebResponseList =
        ResponseHelper.toBusinessPartnerWebResponseList(response.getContent());
    return new PageImpl<>(businessPartnerWebResponseList, PageRequest.of(page, size),
        response.getPageMetaData().getTotalRecords());
  }

  @Override
  public Page<BusinessPartnerWebResponse> getAllActiveMerchantList(ProductSuspensionFilterRequest request, int page,
      int size) {
    BusinessPartnerFilterRequest businessPartnerFilterRequest = RequestHelper.getBusinessPartnerFilter(request);
    GdnRestListResponse<ProfileResponse> response =
        xbpFeign.getAllActiveMerchantList(businessPartnerFilterRequest, page, size);
    ResponseHelper.validateResponse(response);
    List<BusinessPartnerWebResponse> businessPartnerWebResponseList =
        ResponseHelper.toBusinessPartnerWebResponseListFromProfileResponse(response.getContent());
    return new PageImpl<>(businessPartnerWebResponseList,  PageRequest.of(page, size),
        response.getPageMetaData().getTotalRecords());
  }

  @Override
  public String getBusinessPartnerNameByCode(String businessPartnerCode){
    GdnRestSingleResponse<ProfileResponse> profileResponse =
      xbpFeign.filterByBusinessPartnerCode(businessPartnerCode);
    ResponseHelper.validateResponse(profileResponse);
    return Optional.of(profileResponse.getValue()).map(ProfileResponse::getCompany)
      .map(CompanyDTO::getBusinessPartnerName).orElseThrow(ApplicationRuntimeException::new);
  }

  @Override
  public BusinessPartnerProfileWebResponse fetchBusinessPartnerFlags(String businessPartnerCode) {
    GdnRestSingleResponse<ProfileResponse> profileResponse =
      xbpFeign.filterByBusinessPartnerCode(businessPartnerCode);
    ResponseHelper.validateResponse(profileResponse);
    BusinessPartnerProfileWebResponse businessPartnerProfileWebResponse =
      new BusinessPartnerProfileWebResponse();
    businessPartnerProfileWebResponse.setBusinessPartnerCode(businessPartnerCode);
    businessPartnerProfileWebResponse.setProductConsequenceLimitation(Boolean.TRUE.equals(
      profileResponse.getValue().getFlags()
        .getOrDefault(Constants.PRODUCT_CONSEQUENCE_LIMITATION, false)));
    return businessPartnerProfileWebResponse;
  }
}

package com.gdn.partners.pcu.external.service.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

import com.gdn.partners.pcu.external.client.model.PickupPointOutboundResponse;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.v2.dto.businesspartner.BusinessPartnerFilterRequest;
import com.gdn.x.product.rest.web.model.response.SimpleMapStringResponse;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;

import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.core.web.dto.BaseResponse;
import com.gdn.partners.pcu.external.client.feign.XBPFeign;
import com.gdn.partners.pcu.external.model.Constants;
import com.gdn.partners.pcu.external.service.BusinessPartnerService;
import com.gdn.partners.pcu.external.service.impl.helper.RequestHelper;
import com.gdn.partners.pcu.external.service.impl.helper.ResponseHelper;
import com.gdn.x.businesspartner.dto.PickupPointResponse;
import com.gdn.x.businesspartner.dto.ProfileRequest;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.businesspartner.v2.dto.pickuppoint.MarkPickupPointAsDefaultRequest;
import com.gdn.x.businesspartner.v2.dto.pickuppoint.PickupPointFilterRequest;

import lombok.extern.slf4j.Slf4j;

/**
 * Created by govind on 12/12/2018 AD.
 */
@Service
@Slf4j
public class BusinessPartnerServiceImpl implements BusinessPartnerService {

  @Autowired
  private XBPFeign xbpFeign;

  @Value("${pickup.point.filter.batch.size}")
  private int pickupPointFilterBatchSize;

  @Value("${set.waiting.deletion.for.delete.pickup.point}")
  private boolean setWaitingDeletionForDeletePickupPoint;

  @Override
  public ProfileResponse getProfileDetailById(String profileId){
    GdnRestSingleResponse<ProfileResponse> response = xbpFeign.getProfileDetailById(profileId);
    ResponseHelper.validateResponse(response);
    return response.getValue();
  }

  @Override
  public Page<PickupPointOutboundResponse> filterBusinessPartner(int page, int size, PickupPointFilterRequest request) {
    GdnRestListResponse<PickupPointOutboundResponse> response = xbpFeign.filter(page, size, request);
    ResponseHelper.validateResponse(response);
    return new PageImpl<>(response.getContent(), PageRequest.of(page, size),
        response.getPageMetaData().getTotalRecords());
  }

  @Override
  public void updateDefaultPickupPointCode(MarkPickupPointAsDefaultRequest request) {
    BaseResponse response = xbpFeign.updateDefaultPickupPointCode(request);
    ResponseHelper.validateResponse(response);
  }

  @Override
  public void updateDefaultConfiguration(ProfileRequest profileRequest) {
    GdnRestSingleResponse<ProfileResponse> response = xbpFeign.updateDefaultConfiguration(Constants.PRODUCT_SETTING, profileRequest);
    ResponseHelper.validateResponse(response);
  }

  @Override
  public ProfileResponse filterByBusinessPartnerCode(String businessPartnerCode) {
    GdnRestSingleResponse<ProfileResponse> response = xbpFeign.filterByBusinessPartnerCode(businessPartnerCode);
    ResponseHelper.validateResponse(response);
    return response.getValue();
  }

  @Override
  public List<PickupPointOutboundResponse> getAllPickupPointsForBusinessPartner(String businessPartnerCode) {
    return getPickupPointsForBusinessPartner(businessPartnerCode, null);
  }

  @Override
  public List<PickupPointOutboundResponse> getPickupPointsForBusinessPartner(String businessPartnerCode,
    Set<String> pickupPointCodes) {
    int page = 0;
    List<PickupPointOutboundResponse> pickupPointResponses = new ArrayList<>();
    PickupPointFilterRequest pickupPointFilterRequest =
        PickupPointFilterRequest.builder().businessPartnerCode(businessPartnerCode).build();
    RequestHelper.setWaitingDeletionFlagForDeletePickupPoint(setWaitingDeletionForDeletePickupPoint,
        pickupPointFilterRequest);
    if (CollectionUtils.isNotEmpty(pickupPointCodes)) {
      pickupPointFilterRequest.setCodes(pickupPointCodes);
    }
    GdnRestListResponse<PickupPointOutboundResponse> response = null;
    do {
      response = xbpFeign.filter(page, pickupPointFilterBatchSize, pickupPointFilterRequest);
      ResponseHelper.validateResponse(response);
      pickupPointResponses.addAll(response.getContent());
      page++;
    } while (page * pickupPointFilterBatchSize < response.getPageMetaData().getTotalRecords());
    return pickupPointResponses;
  }

  @Override
  public PickupPointOutboundResponse getPickupPointByCode(String code) {
    GdnRestSingleResponse<PickupPointOutboundResponse> response = xbpFeign.filterByCode(code);
    ResponseHelper.validateResponse(response);
    return response.getValue();
  }

  @Override
  public SimpleMapStringResponse getBusinessPartnerDetailsByList(int page, int size,
      BusinessPartnerFilterRequest request) {
    GdnRestListResponse<ProfileResponse> response =
        xbpFeign.getBusinessPartnerDetailsByList(page, size, request);
    ResponseHelper.validateResponse(response);
    SimpleMapStringResponse businessPartnerCodeAndNameMap = new SimpleMapStringResponse();
    Map<String, String> codeAndNameMap = new HashMap<>();
    List<ProfileResponse> content = response.getContent();
    content.forEach(profileResponse -> codeAndNameMap.put(profileResponse.getBusinessPartnerCode(),
        Optional.ofNullable(profileResponse.getCompany()).map(CompanyDTO::getBusinessPartnerName)
            .orElse(StringUtils.EMPTY)));
    businessPartnerCodeAndNameMap.setValue(codeAndNameMap);
    return businessPartnerCodeAndNameMap;
  }

}

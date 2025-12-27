package com.gdn.partners.pcu.internal.service.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;

import com.gdn.partners.pcu.internal.model.Constants;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pcu.internal.client.feign.XBPFeign;
import com.gdn.partners.pcu.internal.service.BPService;
import com.gdn.partners.pcu.internal.service.impl.helper.ResponseHelper;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.businesspartner.v2.dto.businesspartner.BusinessPartnerFilterRequest;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class BPServiceImpl implements BPService {

  @Autowired
  private XBPFeign xbpFeign;

  @Value("${internal.business.partner.switch}")
  private boolean internalBusinessPartnerSwitch;

  @Override
  public Map<String, ProfileResponse> getProfileResponseMap(List<String> businessPartnerCodes) {
    Map<String, ProfileResponse> profileResponseMap = new HashMap<>();
    if (internalBusinessPartnerSwitch) {
      businessPartnerCodes.remove(Constants.USER_TYPE_INTERNAL);
    }
    if (CollectionUtils.isNotEmpty(businessPartnerCodes)) {
      BusinessPartnerFilterRequest businessPartnerFilterRequest = new BusinessPartnerFilterRequest();
      businessPartnerFilterRequest.setBusinessPartnerCodes(new HashSet<>(businessPartnerCodes));
      try {
        GdnRestListResponse<ProfileResponse> profileResponses =
            xbpFeign.getAllActiveMerchantList(businessPartnerFilterRequest, 0, businessPartnerCodes.size());
        ResponseHelper.validateResponse(profileResponses);
        if (internalBusinessPartnerSwitch) {
          return Optional.of(profileResponses.getContent()).orElseGet(ArrayList::new).stream()
              .filter(Objects::nonNull).collect(
                  Collectors.toMap(ProfileResponse::getBusinessPartnerCode, Function.identity()));
        } else {
          return Optional.ofNullable(profileResponses.getContent()).orElseGet(ArrayList::new)
              .stream().collect(
                  Collectors.toMap(ProfileResponse::getBusinessPartnerCode, Function.identity()));
        }
      } catch (Exception e) {
        log.error("Exception caught while getting commission type and IS flag businessPartnerCodes:{}",
            businessPartnerCodes, e);
      }
    }
    return profileResponseMap;
  }

  @Override
  public ProfileResponse getProfileResponseByBusinessPartnerCode(String businessPartnerCode) {
    ProfileResponse profileResponse = null;
    if (StringUtils.isNotBlank(businessPartnerCode)) {
      try {
        GdnRestSingleResponse<ProfileResponse> profileResponseSingle =
            xbpFeign.filterByBusinessPartnerCode(businessPartnerCode);
        ResponseHelper.validateResponse(profileResponseSingle);
        profileResponse = profileResponseSingle.getValue();
      } catch (Exception e) {
        log.error("Exception caught while getting commission type and IS flag businessPartnerCode:{}",
            businessPartnerCode, e);
      }
    }
    return profileResponse;
  }
}
